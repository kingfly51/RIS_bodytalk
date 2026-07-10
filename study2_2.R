# ============================================================
# 时序网络分析 (Temporal Network Analysis)
# 多层向量自回归 mlVAR
# 节点: ris1, ris2 (休息羞耻2条目)
#       bts1, bts2, bts3 (身体谈论3条目)
# 时间: day 1-14, 被试: Number
# ============================================================

library(readxl)
library(dplyr)
library(mlVAR)
library(qgraph)

ema_data <- read_excel("ema_data.xlsx")
# ---- 1. 排序: 同一被试内按天排序(滞后估计的前提) ----
ema_data <- ema_data %>% arrange(Number, day)
ema_data$GR <- ema_data$ris1
ema_data$FTR <- ema_data$ris2
ema_data$NFT <- ema_data$bts1
ema_data$NMT <- ema_data$bts2
ema_data$PBT <- ema_data$bts3
# ---- 2. 定义节点变量 ----
# 请确认这5列在你数据里的确切列名, 必要时先 rename
vars <- c("GR", "FTR", "NFT", "NMT", "PBT")

# 检查数据
ema_data %>% select(Number, day, all_of(vars)) %>% summary()
ema_data <- ema_data %>%
  mutate(Number = as.factor(Number)) %>%
  arrange(Number, day)
# ============================================================
# 3. 估计多层 VAR 模型
# ============================================================
# idvar: 被试编号; beepvar/dayvar: 时间索引(用于正确处理缺失天)

fit_net <- mlVAR(
  data    = as.data.frame(ema_data),
  vars    = vars,
  idvar   = "Number",
  beepvar = "day",
  lags    = 1,
  temporal = "correlated",
  contemporaneous = "correlated",
  estimator = "lmer"
)
summary(fit_net)

# ============================================================
# 4. 提取并绘制三种网络
# ============================================================



# 依次绘制三个图
plot(fit_net, "temporal",
     title = "Temporal Network",
     groups = list("Rest Intolerance" = 1:2, "Negative Body Talk" = 3:5),
     color = c("#E69F00", "#56B4E9"),
     layout = "circle",
     nonsig = "dashed",  
     nodeNames = c("Guilt during Rest",          # 图例全称
                   "Feeling Terrible during Rest",
                   "Negative Fat Talk",
                   "Negative Muscle Talk",
                   "Positive Body Talk"),
     label.cex = 0.8, 
     edge.label.cex = 0.9,
     legend.cex = 0.42, 
     legend = TRUE,
     edge.labels = TRUE)

plot(fit_net, "contemporaneous",
     title = "Contemporaneous Network",
     groups = list("Rest Intolerance" = 1:2, "Negative Body Talk" = 3:5),
     color = c("#E69F00", "#56B4E9"),
     layout = "circle",
     nonsig = "dashed",
     nodeNames = c("Guilt during Rest",          # 图例全称
                   "Feeling Terrible during Rest",
                   "Negative Fat Talk",
                   "Negative Muscle Talk",
                   "Positive Body Talk"),
     label.cex = 0.8, 
     edge.label.cex = 0.9,
     legend.cex = 0.42, 
     legend = TRUE,
     edge.labels = TRUE)

plot(fit_net, "between",
     title = "Between-Subjects Network",
     groups = list("Rest Intolerance" = 1:2, "Negative Body Talk" = 3:5),
     color = c("#E69F00", "#56B4E9"),
     layout = "circle",
     nonsig = "dashed",
     nodeNames = c("Guilt during Rest",          # 图例全称
                   "Feeling Terrible during Rest",
                   "Negative Fat Talk",
                   "Negative Muscle Talk",
                   "Positive Body Talk"),
     label.cex = 0.8, 
     edge.label.cex = 0.9,
     legend.cex = 0.42, 
     legend = TRUE,
     edge.labels = TRUE)

library(networktools)
library(qgraph)

# ============================================================
# 桥接中心性: 分组为 休息羞耻 vs 身体谈论
# ============================================================
# 定义节点所属群组(顺序要和 vars 一致)
# vars <- c("ris1", "ris2", "bts1", "bts2", "bts3")  (或 bts3_r)
groups_list <- list(
  "休息羞耻" = c("GR", "FTR"),
  "身体谈论" = c("NFT", "NMT", "PBT")   # 反向版改成 bts3_r
)

# ---- 时序网络的桥接中心性 ----
temp_mat <- getNet(fit_net, "temporal", nonsig = "hide")
# 给矩阵行列命名(确保和节点对应)
colnames(temp_mat) <- rownames(temp_mat) <- vars

bridge_temp <- bridge(temp_mat,
                      communities = c("休息羞耻","休息羞耻",
                                      "身体谈论","身体谈论","身体谈论"),
                      directed = TRUE)   # 时序网络有方向
print(bridge_temp)
# 关注: Bridge Strength(桥接强度) - 哪个节点跨群组连接最强

# 桥接中心性图
plot(bridge_temp, include = c("Bridge Indegree","Bridge Outdegree"))





































