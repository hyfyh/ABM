##  附加费用率建模

# ----------------------------
# 附加费用率预测模型
# 使用财险公司历史财务数据
# ----------------------------

## 1. 加载必要的包
library(tidyverse)    # 数据处理和可视化
library(caret)        # 机器学习工具包
library(randomForest)  # 随机森林算法
library(glmnet)       # 正则化回归
library(xgboost)      # XGBoost算法
library(ggcorrplot)   # 相关性可视化

## 2. 模拟数据集创建（实际应用中替换为您的真实数据）
# 假设我们有5年的季度数据（20个观测值）
set.seed(123) # 确保结果可重现
#n <- 20

insurance_data <- tibble(
  
  # 时间标识
  year = data$Sgntime, ## 时间年份
  # 公司标识
  index = data$Conm ,
  # 时间标识
  #period = seq.Date(from = as.Date("2019-01-01"), by = "quarter", length.out = 606),
  
  # 核心指标
  earned_premium = data$Compal0103, #  runif(n, 50, 100) * 1000000, # 已赚保费（百万）
  total_expenses = data$Compal0124 + data$Compal0125, # runif(n, 10, 25) * 1000000,  # 总费用（百万）
  
  # 费用构成
  commission_ratio = data$Compal0124/( data$Compal0124 + data$Compal0125), #runif(n, 0.05, 0.15),      # 佣金占比
  admin_expense_ratio = data$Compal0124/( data$Compal0124 + data$Compal0125),   # 管理费占比
  #tech_investment = runif(n, 0.01, 0.05),       # 科技投入占比
 
  # 业务结构
  auto_ratio1 = data$Prtbus1003 , # runif(n, 0.4, 0.7),              # 车险业务占比
  auto_ratio2 = data$Prtbus1004 , # runif(n, 0.4, 0.7),              # 车险业务占比
  auto_ratio3 = data$Prtbus1005 , # runif(n, 0.4, 0.7),              # 车险业务占比
  auto_ratio4 = data$Prtbus1006 , # runif(n, 0.4, 0.7),              # 车险业务占比
  auto_ratio5 = data$Prtbus1007 , # runif(n, 0.4, 0.7),              # 车险业务占比
  auto_ratio6 = data$Prtbus1008 , # runif(n, 0.4, 0.7),              # 车险业务占比
  auto_ratio7 = data$Prtbus1009 , # runif(n, 0.4, 0.7),              # 车险业务占比
  auto_ratio8 = data$Prtbus1010 , # runif(n, 0.4, 0.7),              # 车险业务占比
  auto_ratio9 = data$Prtbus1011 , # runif(n, 0.4, 0.7),              # 车险业务占比
  #commercial_ratio = runif(n, 0.2, 0.4),        # 商业险占比
  #personal_ratio = 1 - auto_ratio - commercial_ratio, # 个人险占比
  
  # 运营效率
  policy_count = data$Combas0236, #总资产   # round(runif(n, 50000, 200000)), # 保单数量
  #avg_premium = runif(n, 1000, 3000),            # 平均保费
  
  # 市场因素
  #market_share = runif(n, 0.02, 0.1),           # 市场份额
  #competition_index = runif(n, 0.5, 0.9)        # 竞争指数（1=高度竞争）
)

insurance_data = na.omit(insurance_data)  ## 去掉含有缺失值的行

# 计算附加费用率（目标变量）
insurance_data$expense_ratio <- insurance_data$total_expenses / insurance_data$earned_premium

# 查看数据结构
glimpse(insurance_data)
head(insurance_data)

## 3. 探索性数据分析（EDA）
# 目标变量分布
ggplot(insurance_data, aes(x = expense_ratio)) +
  geom_histogram(fill = "steelblue", bins = 10) +
  labs(title = "附加费用率分布", x = "附加费用率", y = "频数") +
  theme_minimal()

# 时间趋势分析
ggplot(insurance_data, aes(x = period, y = expense_ratio)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue") +
  labs(title = "附加费用率时间趋势", x = "时间", y = "附加费用率") +
  theme_minimal()

# 变量相关性分析
cor_matrix <- cor(insurance_data[, sapply(insurance_data, is.numeric)])
ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE,
           title = "变量相关性矩阵")

## 4. 数据预处理
# 创建建模数据集（移除时间标识）
model_data <- insurance_data[,-c(1,2)]
model_data <- model_data %>% filter(is.na(expense_ratio) == F)
model_data <- na.omit(model_data)
# 划分训练集和测试集（80%训练，20%测试）
set.seed(123)
train_index <- createDataPartition(model_data$total_expenses, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# 标准化处理（对于线性模型）
preprocess_params <- preProcess(train_data, method = c("center", "scale"))
train_data_scaled <- predict(preprocess_params, train_data)
test_data_scaled <- predict(preprocess_params, test_data)

## 5. 模型构建与比较
### 5.1 线性回归模型
lm_model <- lm(total_expenses ~ ., data = train_data)
summary(lm_model)

# 正则化回归（LASSO）
lasso_model <- cv.glmnet(
  x = as.matrix(train_data_scaled[, -which(names(train_data_scaled) == "total_expenses")]),
  y = train_data_scaled$total_expenses,
  alpha = 1
)

### 5.2 随机森林
rf_model <- randomForest(
  total_expenses ~ .,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

### 5.3 XGBoost模型
xgb_data <- xgb.DMatrix(
  data = as.matrix(train_data[, -which(names(train_data) == "total_expenses")]),
  label = train_data$total_expenses
)

xgb_params <- list(
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  min_child_weight = 1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_data,
  nrounds = 100,
  watchlist = list(train = xgb_data),
  print_every_n = 10
)

## 6. 模型评估
# 创建评估函数
evaluate_model <- function(model, test_data, model_type) {
  if (model_type == "lm") {
    predictions <- predict(model, newdata = test_data)
  } else if (model_type == "lasso") {
    test_matrix <- as.matrix(test_data_scaled[, -which(names(test_data_scaled) == "total_expenses")])
    predictions <- predict(lasso_model, newx = test_matrix, s = "lambda.min")[,1]
  } else if (model_type == "rf") {
    predictions <- predict(model, newdata = test_data)
  } else if (model_type == "xgb") {
    test_matrix <- as.matrix(test_data[, -which(names(test_data) == "total_expenses")])
    predictions <- predict(xgb_model, newdata = test_matrix)
  }
  
  # 计算评估指标
  rmse <- RMSE(predictions, test_data$total_expenses)
  mae <- MAE(predictions, test_data$total_expenses)
  r2 <- R2(predictions, test_data$total_expenses)
  
  data.frame(Model = model_type, RMSE = rmse, MAE = mae, R2 = r2)
}

# 评估所有模型
results <- rbind(
  evaluate_model(lm_model, test_data, "lm"),
  evaluate_model(lasso_model, test_data_scaled, "lasso"),
  evaluate_model(rf_model, test_data, "rf"),
  evaluate_model(xgb_model, test_data, "xgb")
)

print(results)

## 7. 特征重要性分析
# 随机森林特征重要性
varImpPlot(rf_model, main = "随机森林特征重要性")

# XGBoost特征重要性
importance_matrix <- xgb.importance(
  feature_names = colnames(train_data[, -which(names(train_data) == "expense_ratio")]),
  model = xgb_model
)
xgb.plot.importance(importance_matrix)

## 8. 模型解释与业务洞察
# 部分依赖图（以佣金占比为例）
partial_data <- train_data
partial_data$commission_ratio <- seq(min(train_data$commission_ratio), 
                                     max(train_data$commission_ratio), 
                                     length.out = 100)

partial_data$predicted <- predict(rf_model, newdata = partial_data)

ggplot(partial_data, aes(x = commission_ratio, y = predicted)) +
  geom_line(color = "steelblue", size = 1.5) +
  labs(title = "佣金占比对附加费用率的影响",
       x = "佣金占比", y = "预测附加费用率") +
  theme_minimal()

## 9. 模型部署与预测
# 选择最佳模型（这里以随机森林为例）
final_model <- xgb_model

# 新数据预测示例
new_data <- data.frame(
  earned_premium = 75000000,
  total_expenses = 16000000,
  commission_ratio = 0.12,
  admin_expense_ratio = 0.15,
  tech_investment = 0.03,
  auto_ratio = 0.6,
  commercial_ratio = 0.25,
  personal_ratio = 0.15,
  policy_count = 150000,
  avg_premium = 2000,
  market_share = 0.05,
  competition_index = 0.7
)

final_model = xgb_model
new_data <- as.matrix(insurance_data[490:566 ,-c(1,2,4)])

predicted_ratio <- predict(final_model,  newdata = new_data)

cat("预测附加费用率:", round(predicted_ratio * 100, 2), "%\n")


index = unique(insurance_data$index)
beta = c()
for(i in index){
  a = which(insurance_data$index == i)
  new_data <- as.matrix(insurance_data[a , -c(1,2,4)])
  predicted_ratio <- predict(final_model,  newdata = new_data)
  
  ss = (predicted_ratio/insurance_data[a , c(3)])
  ss$earned_premium[ss$earned_premium > 1] = 1
  ss$earned_premium[ss$earned_premium < 0] = 0
  beta = c(beta, mean(ss$earned_premium))
}

pp = tibble(index = index,
            beta0 = beta)








## 10. 模型监控与更新
# 实际应用中应定期重新训练模型
# 示例：监控模型性能随时间变化
# (实际应用中需要记录每次预测结果并与实际值比较)

