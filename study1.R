
# ============================================================
# 城市×年份面板数据: 身体形象词频预测休息不耐受词频
# ============================================================
library(readxl)
library(dplyr)

bodyimage_ris <- read_excel("bodyimage_ris.xlsx")

# ============================================================
# 1. 数据清洗: 把字符型的词频列转为数值
# ============================================================
# 需要转换的关键变量(都被读成了字符)
char_vars <- c("身体形象", "休息不耐受",
               "消极感受_ris", "社会比较_ris",
               "强迫性思维_ris", "认知偏差_ris",
               "字典覆盖率", "字典覆盖率_ris")

bodyimage_ris <- bodyimage_ris %>%
  mutate(across(all_of(char_vars), ~ as.numeric(.)))

# 重命名核心变量, 便于建模(中文列名在公式里容易出错)
df <- bodyimage_ris %>%
  rename(
    body_image = 身体形象,        # 自变量: 身体形象词频
    rest_intol = 休息不耐受,      # 因变量: 休息不耐受词频
    n_weibo    = 微博数,           # 控制: 微博数量(代表样本量/活跃度)
    n_words    = 词数
  ) %>%
  # 去掉关键变量缺失的行
  filter(!is.na(body_image), !is.na(rest_intol))

cat("城市数:", n_distinct(df$city), "  年份范围:", range(df$year), "\n")
cat("有效观测数:", nrow(df), "\n")

# ============================================================
# 2. 描述与可视化: 先看两者关系长什么样
# ============================================================
# 词频是很小的比例值, 取对数让分布更正态、关系更线性(加小常数防log0)
df <- df %>%
  mutate(
    log_body = log(body_image +0.001),
    log_rest = log(rest_intol + 0.001)
  )

hist(df$log_body)
hist(df$log_rest)
df <- df[-which.max(df$log_rest),]
hist(df$log_rest)

cor.test(df$log_body, df$log_rest)  # 简单相关先看一眼

library(ggplot2)
ggplot(df, aes(x = log_body, y = log_rest)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  labs(x = "身体形象词频(log)", y = "休息不耐受词频(log)",
       title = "城市×年份层面: 身体形象 vs 休息不耐受")

# ============================================================
# 3. 面板回归模型 (plm包)
# 面板结构: 个体维度=city, 时间维度=year
# ============================================================
library(plm)

pdf <- pdata.frame(df, index = c("city", "year"))

# ---- 模型1: 混合OLS (pooled, 作为基准, 但忽略面板结构) ----
m_pool <- plm(log_rest ~ log_body, data = pdf, model = "pooling")

# ---- 模型2: 固定效应 (within, 控制所有不随时间变的城市特征) ----
# 这是面板分析的主力模型: 利用城市内随时间的变异来估计关系
# 自动控制了城市间一切稳定差异(地理、经济基础、方言等混淆)
m_fe <- plm(log_rest ~ log_body, data = pdf, model = "within",
            effect = "twoways")   # twoways: 同时控制城市和年份固定效应

# ---- 模型3: 随机效应 ----
m_re <- plm(log_rest ~ log_body, data = pdf, model = "random")

summary(m_fe)
summary(m_re)
# ---- Hausman检验: 选固定效应还是随机效应 ----
phtest(m_fe, m_re)
# p<0.05 -> 用固定效应; p>0.05 -> 随机效应更有效

# ============================================================
# 4. 稳健标准误 (面板数据残差通常有异方差和序列相关)
# ============================================================
library(lmtest)
# 按城市聚类的稳健标准误
coeftest(m_fe, vcov = vcovHC(m_fe, type = "HC1", cluster = "group"))




# ============================================================
# 补充1: 看标准化效应量(系数对小数值不直观, 标准化后好解读)
# ============================================================
df_std <- df %>%
  mutate(z_body = scale(log_body)[,1],
         z_rest = scale(log_rest)[,1])
pdf_std <- pdata.frame(df_std, index = c("city","year"))
m_std <- plm(z_rest ~ z_body, data = pdf_std,
             model = "within", effect = "twoways")
summary(m_std)
# 标准化系数 = body每升高1个标准差, rest升高几个标准差
# 这个数值能直观告诉你效应到底多小

# ============================================================
# 补充2: 横截面相关(城市间) vs 城市内 —— 两者可能差别很大
# 你前面的简单cor.test看的是混合相关, 可能比within大很多
# 报告这个对比本身就是有意义的发现
# ============================================================
# 城市层面均值的横截面相关(城市间关系)
city_mean <- df %>%
  group_by(city) %>%
  summarise(m_body = mean(log_body), m_rest = mean(log_rest))
cor.test(city_mean$m_body, city_mean$m_rest)
# 若城市间相关 >> 城市内系数, 说明关系主要在"城市间"层面
# 即:整体上身体形象话语多的城市休息不耐受话语也多,
#    但同一城市逐年波动的联动很弱

# ============================================================
# 补充3: 加控制变量后系数是否稳健(稳健性检验)
# ============================================================
df <- df %>% mutate(log_nweibo = log(n_weibo + 1))
pdf <- pdata.frame(df, index = c("city","year"))
m_ctrl <- plm(log_rest ~ log_body + log_nweibo,
              data = pdf, model = "within", effect = "twoways")
coeftest(m_ctrl, vcov = vcovHC(m_ctrl, type = "HC1", cluster = "group"))
# 加控制后 log_body 系数若仍显著且方向不变, 说明结果稳健



library(ggplot2)

# ============================================================
# 进阶版: hexbin密度图 + 回归线(强烈推荐, 大样本最清晰)
# ============================================================
ggplot(df, aes(x = log_body, y = log_rest)) +
  geom_hex(bins = 45) +
  scale_fill_gradient(low = "#DCE6F1", high = "#2C5F8A",
                      name = "Count") +
  geom_smooth(method = "lm", se = TRUE,
              color = "#C44E52", fill = "#C44E52",
              alpha = 0.15, linewidth = 1.2) +
  labs(
    x = "Body Talk word frequency (log)",
    y = "Rest Intolerance word frequency (log)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.4),
    legend.position = "right",
    plot.margin = margin(15, 15, 15, 15)
  )

ggsave("scatter_hex.png", width = 7.5, height = 5.5, dpi = 300, bg = "white")
# hexbin 需要 hexbin 包: install.packages("hexbin")






######Stability test




library(readxl)
library(dplyr)

bodyimage_ris <- read_excel("bodyimage_ris.xlsx")

# ============================================================
# 1. 数据清洗: 把字符型的词频列转为数值
# ============================================================
# 需要转换的关键变量(都被读成了字符)
char_vars <- c("身体形象", "休息不耐受",
               "消极感受_ris", "社会比较_ris",
               "强迫性思维_ris", "认知偏差_ris",
               "字典覆盖率", "字典覆盖率_ris")

bodyimage_ris <- bodyimage_ris %>%
  mutate(across(all_of(char_vars), ~ as.numeric(.)))

# 重命名核心变量, 便于建模(中文列名在公式里容易出错)
df <- bodyimage_ris %>%
  rename(
    body_image = 身体形象,        # 自变量: 身体形象词频
    rest_intol = 休息不耐受,      # 因变量: 休息不耐受词频
    n_weibo    = 微博数,           # 控制: 微博数量(代表样本量/活跃度)
    n_words    = 词数
  ) %>%
  # 去掉关键变量缺失的行
  filter(!is.na(body_image), !is.na(rest_intol))

cat("城市数:", n_distinct(df$city), "  年份范围:", range(df$year), "\n")
cat("有效观测数:", nrow(df), "\n")

# ============================================================
# 2. 描述与可视化: 先看两者关系长什么样
# ============================================================
# 词频是很小的比例值, 取对数让分布更正态、关系更线性(加小常数防log0)
df <- df %>%
  mutate(
    log_body = log(body_image +0.001),
    log_rest = log(rest_intol + 0.001)
  )

hist(df$log_body)
hist(df$log_rest)
#df <- df[-which.max(df$log_rest),]
hist(df$log_rest)

cor.test(df$log_body, df$log_rest)  # 简单相关先看一眼

library(ggplot2)
ggplot(df, aes(x = log_body, y = log_rest)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm") +
  labs(x = "身体形象词频(log)", y = "休息不耐受词频(log)",
       title = "城市×年份层面: 身体形象 vs 休息不耐受")

# ============================================================
# 3. 面板回归模型 (plm包)
# 面板结构: 个体维度=city, 时间维度=year
# ============================================================
library(plm)

pdf <- pdata.frame(df, index = c("city", "year"))

# ---- 模型1: 混合OLS (pooled, 作为基准, 但忽略面板结构) ----
m_pool <- plm(log_rest ~ log_body, data = pdf, model = "pooling")

# ---- 模型2: 固定效应 (within, 控制所有不随时间变的城市特征) ----
# 这是面板分析的主力模型: 利用城市内随时间的变异来估计关系
# 自动控制了城市间一切稳定差异(地理、经济基础、方言等混淆)
m_fe <- plm(log_rest ~ log_body, data = pdf, model = "within",
            effect = "twoways")   # twoways: 同时控制城市和年份固定效应

# ---- 模型3: 随机效应 ----
m_re <- plm(log_rest ~ log_body, data = pdf, model = "random")

summary(m_fe)
summary(m_re)
# ---- Hausman检验: 选固定效应还是随机效应 ----
phtest(m_fe, m_re)
# p<0.05 -> 用固定效应; p>0.05 -> 随机效应更有效

# ============================================================
# 4. 稳健标准误 (面板数据残差通常有异方差和序列相关)
# ============================================================
library(lmtest)
# 按城市聚类的稳健标准误
coeftest(m_fe, vcov = vcovHC(m_fe, type = "HC1", cluster = "group"))








