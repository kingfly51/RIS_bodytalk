library(readxl)
library(dplyr)
library(nlme)      # 用于拟合带自相关结构的混合模型
library(lme4)      # 备选的混合模型包
library(lmerTest)  # 为 lme4 提供 p 值

# ---- 1. 读取数据 ----
ema_data <- read_excel("ema_data.xlsx")

# 确保排序正确（同一被试内按天排序，这是构造滞后项的前提）
ema_data <- ema_data %>%
  arrange(Number, day)

# ============================================================
# 2. 个体内中心化：分离个体内成分与个体间成分
# ============================================================
# 个体间成分 (between-person): 每个被试自变量的均值
# 个体内成分 (within-person): 当天值减去该被试的均值
ema_data <- ema_data %>%
  group_by(Number) %>%
  mutate(
    # 个体间成分：每个被试 ris_total 的个人均值
    ris_between = mean(ris_total, na.rm = TRUE),
    # 个体内成分：person-mean centering（组内中心化）
    ris_within  = ris_total - ris_between,
    bts_between = mean(bts_total, na.rm = TRUE),
    # 个体内成分：person-mean centering（组内中心化）
    bts_within  = bts_total - bts_between,
  ) %>%
  ungroup()

# 把个体间成分再做总体中心化（grand-mean centering），便于解释截距
ema_data <- ema_data %>%
  mutate(ris_between_c = ris_between - mean(ris_between, na.rm = TRUE),
         bts_between_c = bts_between - mean(bts_between, na.rm = TRUE))

# ============================================================
# 3. 构造滞后一天 (lag-1) 的自变量
# ============================================================
# 注意：滞后只能在同一被试内进行，且需检查 day 是否连续
ema_data <- ema_data %>%
  group_by(Number) %>%
  mutate(
    # 前一天的 day 值，用于判断是否真正相邻
    day_prev = dplyr::lag(day),
    # 滞后一天的个体内成分（原始滞后）
    ris_within_lag1_raw = dplyr::lag(ris_within),
    # 只有当 day 真正相差 1 天时才保留滞后值，否则设为 NA
    ris_within_lag1 = ifelse(day - day_prev == 1, ris_within_lag1_raw, NA),
    bts_within_lag1_raw = dplyr::lag(bts_within),
    bts_within_lag1 = ifelse(day - day_prev == 1, bts_within_lag1_raw, NA),
    # 因变量滞后一天（如需控制前一天的 bts，做自回归）
    bts_total_lag1_raw = dplyr::lag(bts_total),
    bts_total_lag1 = ifelse(day - day_prev == 1, bts_total_lag1_raw, NA),
    ris_total_lag1_raw = dplyr::lag(ris_total),
    ris_total_lag1 = ifelse(day - day_prev == 1, ris_total_lag1_raw, NA)
  ) %>%
  ungroup()

# ============================================================
# 4. 模型拟合
# ============================================================

# ---- 模型 1: 随机截距模型（同期效应，无滞后）----
# 个体内成分 ris_within + 个体间成分 ris_between_c

m1_ri <- lmer(
  ris_total ~ age+gender+education+nation+week+bts_within + bts_between_c + (1 | Number),
  data = ema_data,
  REML = TRUE
)
summary(m1_ri)

# ---- 模型 2: 随机斜率模型（同期效应）----
# 允许个体内效应 (ris_within) 的斜率在被试间变化

m2_rs <- lmer(
  ris_total ~ age+gender+education+nation+week+bts_within + bts_between_c + (1 + bts_within | Number),
  data = ema_data,
  REML = TRUE
)
summary(m2_rs)

# ---- 模型 3: 随机截距 + 滞后一天 ----

m3_ri_lag <- lmer(
  ris_total ~ age+gender+education+nation+week+bts_within_lag1 + bts_between_c + (1 | Number),
  data = ema_data,
  REML = TRUE
)
summary(m3_ri_lag)

# ---- 模型 4: 随机斜率 + 滞后一天 ----

m4_rs_lag <- lmer(
  ris_total ~age+gender+education+nation+week+ bts_within_lag1 + bts_between_c + (1 + bts_within_lag1 | Number),
  data = ema_data,
  REML = TRUE
)
summary(m4_rs_lag)


# ---- 模型 5: 完整模型（推荐）----
# 同期个体内效应 + 滞后个体内效应 + 因变量自回归 + 个体间效应
# 随机截距 + 随机斜率
m5_full <- lmer(
  ris_total ~age+gender+education+nation+week+ bts_within + bts_within_lag1 + ris_total_lag1 +
    bts_between_c + (1 + bts_within | Number),
  data = ema_data,
  REML = TRUE
)
summary(m5_full)



library(dplyr)
library(psych)

# ============================================================
# 描述性统计
# ============================================================

# 1. 主要变量的描述统计(均值/SD/范围/偏度峰度)
describe(ema_data[, c("ris_total", "bts_total", "age", "week")])

# 2. 分类变量频数
table(ema_data$gender)
table(ema_data$education)
table(ema_data$nation)

# 3. 被试层面信息: 人数 + 每人有效天数
ema_data %>%
  filter(!is.na(ris_total), !is.na(bts_total)) %>%
  summarise(
    n_subjects = n_distinct(Number),
    n_obs = n(),
    days_per_person = n() / n_distinct(Number)
  )

# 4. 核心变量相关
cor(ema_data[, c("ris_total", "bts_total")], use = "pairwise.complete.obs")

# 5. ICC: 休息羞耻的个体间变异占比(说明用多层模型的依据)
library(lme4)
icc_model <- lmer(ris_total ~ 1 + (1 | Number), data = ema_data)
performance::icc(icc_model)


psych::alpha(data.frame(ema_data$ris1, ema_data$ris2))
alpha(ema_data[, c("bts1", "bts2", "bts3")])
