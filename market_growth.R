##  市场增长率建模

library(forecast)
library(ggplot2)

# ----------------------------
# 附加费用率预测模型
# 使用财险市场历史财务数据
# ----------------------------


business <- read.csv("财产险公司经营情况表（月）153050113(仅供vip使用)/INS_PropertyManageM.csv",
                     fileEncoding = 'utf-8')
income_month <- business$Incomes_month   ## 财险市场的月度保费收入

# 假设 df$TPC 是总保单件数的时间序列向量
ts_data <- ts(business$Incomes_month, frequency = 12) # 设定为月度数据

# 自动拟合 ARIMA 模型
fit_arima <- auto.arima(ts_data, stepwise = FALSE, approximation = FALSE, seasonal = T)

# 预测并绘图
forecast_arima <- forecast(fit_arima, h = 800) # 预测未来12个月
autoplot(forecast_arima) +
  ggtitle("ARIMA Model Forecast of Total Policy Count")

## 高增长情况
growth_upper = c()
for(i in 0:60){
  ss = sum(forecast_arima$upper[(i*12 + 6):(i*12+6+11)])
  growth_upper = c(growth_upper, ss)
}

## 中速增长情况
growth_mean = c()
for(i in 0:60){
  ss = sum(forecast_arima$mean[(i*12 + 6):(i*12+6+11)])
  growth_mean = c(growth_mean, ss)
}

## 低增长情况
growth_lower = c()
for(i in 0:60){
  ss = sum(forecast_arima$lower[(i*12 + 6):(i*12+6+11)])
  growth_lower = c(growth_lower, ss)
}

plot(growth_upper, type = 'l', col = 'red')
lines(growth_mean, type = 'l', col = 'black')
lines(growth_lower, type = 'l', col = 'blue')
## 
plot(diff(growth_upper)/growth_upper, type = 'l', col = 'red')
lines(diff(growth_mean)/growth_mean, type = 'l', col = 'black')
lines(diff(growth_lower)/growth_lower, type = 'l', col = 'blue')



