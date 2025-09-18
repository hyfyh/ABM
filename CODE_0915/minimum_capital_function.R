
library(tidyverse)
library(rootSolve)
library(forecast)
library(plot3D)
library(rgl)
library(tcltk)
library(beepr)
library(VGAM)
library(mailR)
library(utils)
library(MASS)
library(NlcOptim)
library(pracma) ## 算定积分面积

####
mc_function <- function(yewu = 1000000, 
                        invest,
                        alpha,
                        #lilv = 0.025,
                        x  = 0,
                        rf0_feishou = 0.103){
  
  alpha_zhaiquan =alpha[1] 
  alpha_quanyi = alpha[2]  
  rf0_feishou = 0.103
  mc_feishou = yewu * rf0_feishou
  # mc_shichang = c(mc_债券， mc_股票)
  # ## 债券要算久期
  # # 债券参数  
  # face_value <- 1  # 面值  
  # coupon_rate <- lilv  # 年利率  
  # n_periods <- 10  # 期限（年）  
  # market_rate <- lilv  # 市场利率（用于贴现）  
  # # 计算每年的现金流  
  # coupon_payments <- rep(face_value * coupon_rate, n_periods - 1)  # 利息支付  
  # final_payment <- face_value + coupon_payments[length(coupon_payments)]  # 最后一年的利息加本金  
  # cash_flows <- c(coupon_payments, final_payment)  
  # # 计算每年的现金流现值  
  # discount_factors <- (1 + market_rate)^-(0:(n_periods-1))  
  # present_values <- cash_flows * discount_factors  
  # # 计算久期  
  # duration <- sum((0:(n_periods-1)) * present_values) / sum(present_values)  
  # duration = 2
  # ## 财产保险公司利率风险最低资本
  # if(0 < duration & duration <= 5){
  #   RF0_lilv = duration * (-0.001*duration + 0.0217)
  # }else if( 5 < duration & duration<= 10){
  #   RF0_lilv = duration * (-0.00106*duration + 0.022)
  # }else{
  #   RF0_lilv = duration * 0.0114
  # }
  
  RF0_lilv = 0.0394
  mc_lilv = (invest * alpha_zhaiquan +x) * RF0_lilv
  ## 权益价格风险最低资本
  ## 沪深主板股
  ## 创业版股、科创版股
  RF0_quanyi = 0.35  ## 股票的rf0
  mc_quanyi = (invest * alpha_quanyi - 2*x)  * RF0_quanyi
  xiangliang_shichang = c(mc_lilv, mc_quanyi)
  
  ## 市场风险的最低资本相关系数
  matrix_shichang = matrix(c(1, -0.14, -0.14, 1),
                           ncol = 2)  
  mc_shichang = (t(xiangliang_shichang) %*% matrix_shichang %*% xiangliang_shichang)^ (1/2)
  ## 股票信用风险的rf  0.006
  
  ## 信用风险分为两类  利差风险和交易对手违约风险
  ## 只考虑国债的情况下 利差风险是0
  ## 在流动性资产部分会有交易对手违约风险
  # duration = 2
  # if(0 < duration & duration <= 5){
  #   RF0_licha = duration * (-0.0012*duration + 0.012)
  # }else if( 5 < duration ){
  #   RF0_licha = duration * (-0.001*duration + 0.025)
  # }
  RF0_licha  = 0.0192
  mc_licha  = 0 # (invest * alpha_zhaiquan + x)* RF0_licha
  
  ## 交易对手违约
  
  mc_weiyue = 0.005* invest * (1 - alpha_zhaiquan - alpha_quanyi)
  mc_xinyong = (mc_licha ^2 + 2*0.25 * mc_licha * mc_weiyue + mc_weiyue^2)^(1/2)
  
  
  ##  构建一个方程计算最低资本
  ## 总mc = (mc_非寿险 , mc_市场， mc_信用风险）
  mc = c(mc_feishou, mc_shichang, mc_xinyong)
  
  mc_matrix = matrix(data = c(1, 0.1, 0.15, 0.1, 1, 0.27,
                              0.15, 0.27, 1), ncol = 3)
  
  mc_zuidi = (t(mc)%*%mc_matrix%*%(mc))^(1/2)
  return(mc_zuidi)
}



mc_function_asset <- function(yewu = 1000000, 
                              cash,
                              FV_free,
                              FV_risk,
                              #lilv = 0.025,
                              x= 0,
                              rf0_feishou = 0.103){
  mc_feishou = yewu * rf0_feishou
  
  # mc_shichang = c(mc_债券， mc_股票)
  # ## 债券要算久期
  # # 债券参数  
  # face_value <- 1  # 面值  
  # coupon_rate <- lilv  # 年利率  
  # n_periods <- 10  # 期限（年）  
  # market_rate <- lilv  # 市场利率（用于贴现）  
  # # 计算每年的现金流  
  # coupon_payments <- rep(face_value * coupon_rate, n_periods - 1)  # 利息支付  
  # final_payment <- face_value + coupon_payments[length(coupon_payments)]  # 最后一年的 
  # cash_flows <- c(coupon_payments, final_payment)  
  # # 计算每年的现金流现值  
  # discount_factors <- (1 + market_rate)^-(0:(n_periods-1))  
  # present_values <- cash_flows * discount_factors  
  # # 计算久期  
  # duration <- sum((0:(n_periods-1)) * present_values) / sum(present_values)  
  # ## 财产保险公司利率风险最低资本
  # if(0 < duration & duration <= 5){
  #   RF0_lilv = duration * (-0.001*duration + 0.0217)
  # }else if( 5 < duration & duration<= 10){
  #   RF0_lilv = duration * (-0.00106*duration + 0.022)
  # }else{
  #   RF0_lilv = duration * 0.0114  
  # }
  RF0_lilv = 0.0394
  mc_lilv = (FV_free) * RF0_lilv
  
  ## 权益价格风险最低资本
  ## 沪深主板股
  ## 创业版股、科创版股
  RF0_quanyi = 0.35  ## 股票的rf0
  mc_quanyi = (FV_risk -  x)  *  RF0_quanyi #RxF0_quanyi
  xiangliang_shichang = c(mc_lilv, mc_quanyi)
  
  ## 市场风险的最低资本相关系数
  matrix_shichang = matrix(c(1, -0.14, -0.14, 1),
                           ncol = 2)  
  mc_shichang = (t(xiangliang_shichang) %*% matrix_shichang %*% xiangliang_shichang)^ (1/2)
  ## 股票信用风险的rf  0.006
  ## 信用风险分为两类  利差风险和交易对手违约风险
  ## 这里只考虑交易对手违约风险
  # if(0 < duration & duration <= 5){
  #   RF0_licha = duration * (-0.0012*duration + 0.012)
  # }else if( 5 < duration ){
  #   RF0_licha = duration * (-0.001*duration + 0.025)
  # }
  RF0_licha  = 0.0192
  mc_licha  =  0 #(FV_free + 0.8 * x) * RF0_licha
  
  ## 交易对手违约
  mc_weiyue = 0.005 * cash + 0.8 * x
  mc_xinyong = (mc_licha ^2 + 2*0.25 * mc_licha * mc_weiyue + mc_weiyue^2)^(1/2)
  
  ##  构建一个方程计算最低资本
  ## 总mc = (mc_非寿险 , mc_市场， mc_信用风险）
  mc = c(mc_feishou, mc_shichang, mc_xinyong)
  
  mc_matrix = matrix(data = c(1, 0.1, 0.15, 0.1, 1, 0.27,
                              0.15, 0.27, 1), ncol = 3)
  mc_zuidi = (t(mc)%*%mc_matrix%*%(mc))^(1/2)
  return(mc_zuidi)
}

f_mc = function(Equity,
                yewu = 1000000, 
                cash,
                FV_risk,
                FV_free,
                #lilv = 0.025,
                sl_ratio = 0.5,
                x = 0,
                rf0_feishou = 0.103){
  a = mc_function_asset(yewu = yewu,
                        cash = cash,
                        FV_risk = FV_risk,
                        FV_free = FV_free,
                        #lilv =lilv, 
                        x = x)
  return((Equity - 0.2 * x) / a - sl_ratio)
}



mf_mc_ratio = function(x = 0 ,
                       Equity,
                       yewu = 1000000, 
                       invest, 
                       alpha_zhaiquan,
                       alpha_quanyi,
                       #lilv = 0.025,
                       sl_ratio = 0.5,
                       rf0_feishou = 0.103){
  a = mc_function(yewu = yewu , 
                  invest = invest, 
                  alpha_zhaiquan = alpha[1],
                  alpha_quanyi = alpha[2],
                  #lilv = lilv,
                  x  = x)
  return((Equity - 0.2 * x) / a - sl_ratio)
}


####################################
########################################################
### 算rf0  ############################################
## 首先将业务和rf0基准值对应相乘
setwd("F://newest/newest/data_yewufengxian")

matrix_mc = read.csv('matrix_mc.csv',
                     fileEncoding = 'gbk')
rf0 = tibble(read.csv('rf0.csv', header = T,
                      fileEncoding = 'gbk'))
rf0_reserve = tibble(read.csv('rf0_reserve.csv', header = T,
                              fileEncoding = 'gbk'))
yewu_rate = read.csv('yewu_rate.csv',
                     fileEncoding = 'gbk')

rf0_rate = tibble(index = unique(yewu_rate$X))
for(i in 2:length(rf0)){
  a = as.numeric(yewu_rate[, i] )* as.numeric(rf0[i])
  rf0_rate = cbind(rf0_rate, a)
}
rf0_rate_reserve = tibble(index = unique(yewu_rate$X))

for(i in 2:length(rf0)){
  a = as.numeric(yewu_rate[, i] )* as.numeric(rf0_reserve[i]) * 0.5
  rf0_rate_reserve = cbind(rf0_rate_reserve, a)
}
company = rf0_rate$index
rf0_rate =  sqrt(rf0_rate[,-1]^2 + 2 * 0.5 * rf0_rate[,-1] * rf0_rate_reserve[,-1] + rf0_rate_reserve[,-1]^2)
names(rf0_rate) <- c( names(yewu_rate)[-1])
names(matrix_mc)


# "X"            "车险"         "财产险"       "船货特险"     "责任险"       "农业险"      
# "信用保证保险" "短意险"       "短健险"       "短期寿险"     "其他险" 

rf0_new = tibble(index = company,
                 chexian = rf0_rate$机动车辆保险,
                 caichanxian = rf0_rate$企业财产保险 + rf0_rate$家庭财产保险,
                 chuanhuote = rf0_rate$货物运输保险 + rf0_rate$货物运输保险 + rf0_rate$特殊风险保险,
                 zerenxian = rf0_rate$责任保险,
                 nongyexian = rf0_rate$农业保险,
                 xinyongbaozheng = rf0_rate$信用保险 + rf0_rate$保证保险,
                 duanyixian = rf0_rate$意外伤害保险,
                 duanjianxian = rf0_rate$短期健康保险,
                 duanshouxian = 0,
                 qita = rf0_rate$其他)

mc_rf0_dd = c()
for(t in 1:nrow(rf0_new)){
  mc_rf0_dd = c(mc_rf0_dd, 
                as.matrix(rf0_new[t,-1]) %*% as.matrix(matrix_mc[, -1]) %*% t(as.matrix(rf0_new[t,-1])))
}

mc_rf0_data = tibble(company = company,
                     mc_rf0 = mc_rf0_dd ^ 0.5)



###   资产大类的方差系数矩阵

######################################################################
## 算指数的收益率和波动性
setwd("F://newest/newest/data_zhishu")

data_1 <- read.csv("asset_price_data.csv")
data_1$sgibor = as.numeric( str_remove(data_1$sgibor, ","))
data_1$zhengquan = as.numeric( str_remove(data_1$zhengquan, ","))
data_1$year = year(ymd(data_1$date))
data_1 <- data_1 %>% filter(year > 2016) %>% filter(year < 2023)
data_1$iii = 0
data_1$iii = c(0, diff(data_1$sgibor)/data_1$sgibor[-nrow(data_1)])
data_1 <- data_1[-1,]
uni_year = unique(data_1$year)
#data_1$iii = (1+data_1$iii)^252-1
a = c()
for (t in uni_year) {
  ee = which(data_1$year == t)
  temp = data_1$iii[ee] + 1
  for(yy in 2:length(ee)){
    temp[yy] = temp[yy-1]*temp[yy]
  }
  a = c(a, (temp[length(ee)]^(1/length(ee)))^252-1)
}

data_1$rate_shibor = c(0, diff(data_1$sgibor)/data_1$sgibor[-nrow(data_1)])
data_1$rate_guozhai = c(0, diff(data_1$guozhai)/data_1$guozhai[-nrow(data_1)])
data_1$rate_zhengquan = c(0, diff(data_1$zhengquan)/data_1$zhengquan[-nrow(data_1)])
invest_matrix  <-   matrix(cov(data_1[,c(7:9)])*252, ncol = 3, byrow = T)

      
      









###############################################################

data_1 <- read.csv("shangzheng.csv")
data_1$year = year(ymd(data_1$date))
data_1 <- data_1 %>% filter(year > 2010) %>% filter(year < 2023)
data_1$iii = 0
data_1$iii = c(0, diff(data_1$shangzheng)/data_1$shangzheng[-nrow(data_1)])
data_1$iii = (1+data_1$iii)^52-1
a = c()
uni_year = unique(data_1$year)
for (t in uni_year) {
  a = c(a, mean(data_1$iii[ which(data_1$year == t)]))
}
mean(a)
sd(a)

data_1$a1 <- data_1$iii+1
cum_value <- prod(data_1$a1)
cum_value^(52/length(data_1$a1))-1

data_1 <- read.csv("shibor.csv")
data_1$year = year(ymd(data_1$date))
data_1 <- data_1 %>% filter(year > 2011) %>% filter(year < 2023)
data_1$iii = 0
data_1$iii = c(0, diff(data_1$shibor)/data_1$shibor[-nrow(data_1)])
data_1$iii = (data_1$iii)*252
uni_year = unique(data_1$year)
a = c()
for (t in uni_year) {
  a = c(a, mean(data_1$shibor[ which(data_1$year == t)]))
}
mean(a)

cov(data_1$iii, data_1$iii)











######  算韧性  #################################################
resilience = function(avg_mc = rowSums(mc_noncat)/time,
                      period = cat_per,
                      t_f = t_final,
                      mc = MC_ZUIDI[,1]){
  a = min(which(mc[(period+1): length(mc)] >  avg_mc[(period+1): length(mc)]))+ period - 1
  if(a > t_f){
    a = t_final
  } 
  y1 = avg_mc[period:a]
  y2 = mc[period: a]
  x <- c(1:length(y1))
  
  # 计算定积分
  result1 <- trapz(x, y1)
  result2 <- trapz(x, y2)
  return(result2/result1)
}



resilience_time = function(avg_mc,
                           period = cat_per,
                           t_f = t_final,
                           mc = MC_ZUIDI[,1]){
  a = min(which(mc[(period + 1): length(mc)] >  avg_mc[(period+1): length(mc)])) + period - 1
  if(a > t_f){
    a = t_final
  }
  y1 = avg_mc[(period): a ]
  y2 = mc[(period): a ]
  x <- c(1:length(y1))
  result1 <- trapz(x, y1)
  result2 <- trapz(x, y2)
  re = result2/result1
  s = 0
  for(i in 1:(-1+length(x))){
    s = s + (sum(y1[i:(i+1)]/2)-sum(y2[i:(i+1)]/2))*(0.5+i-1)/(result1-result2)
  }
  return(s)
}


resilience_time(avg_mc = ss_non,
                period = cat_per-1,
                t_f = t_final,
                mc = ss_cat)



##  一个是对于无冲击下的平稳序列
ss_non <- read.csv("F://newest/0321_DATA/data_0902/data_1102/cat_0/NUMBER_BE_0.csv")[,-1]
#ss_non = RHO_SQUARE_0
#ss_non = rowSums(ss)/ncol(ss)

## 一个是测试压力下的序列
ss_cat_1 <- read.csv("F://newest/0321_DATA/data_0902/data_1102/cat_1.1/NUMBER_BE_1.1.csv")[,-1]
ss_cat_2 <- read.csv("F://newest/0321_DATA/data_0902/data_1102/cat_1.2/NUMBER_BE_1.2.csv")[,-1]
ss_cat_3 <- read.csv("F://newest/0321_DATA/data_0902/data_1102/cat_1.3/NUMBER_BE_1.3.csv")[,-1]


plot(rowSums(ss_non)/ncol(ss_non), type = 'l',
     main="资本比率", ylim = range(rowSums(ss_non)/ncol(ss_non),
                               rowSums(ss_cat_1)/ncol(ss_cat_1),
                               rowSums(ss_cat_2)/ncol(ss_cat_2),
                               rowSums(ss_cat_3)/ncol(ss_cat_3)))#, 
                               #ss_cat))
text(t_final/2, ss_non[t_final/2], 
     labels = "无风险")

lines(rowSums(ss_cat)/ncol(ss_cat), type = 'l', col = 'blue')
text(t_final/3,(rowSums(ss_cat)/ncol(ss_cat))[t_final/2], 
     labels = "有风险", col = 'blue')

lines(rowSums(ss_cat_2)/ncol(ss_cat_2), type = 'b', col = 'red')
text(t_final/1.5,(rowSums(ss_cat_1)/ncol(ss_cat_1))[t_final/2], 
     labels = "有风险_risk_aversion = 2", col = 'red')
lines(rowSums(ss_cat_3)/ncol(ss_cat_3), type = 'b', col = 'orange')
text(t_final/1.5,(rowSums(ss_cat_1)/ncol(ss_cat_1))[t_final/2], 
     labels = "有风险_risk_aversion = 2", col = 'orange')


# 
# r <- c()
# for(i in 1:ncol(ss_cat)){
#   if(all(ss_cat[21:t_final, i] < ss[21:t_final,i])){
#     r = c(r, i)
#   } 
# }
# 
# kk = setdiff( which(as.numeric(ss_cat[20,] )< ss[20,]), r)
for(t in c(1, 2, 3)){
  kk = which(as.numeric(get(paste0("ss_cat_", t))[20,] )< ss_non[20,])
  assign(paste0("ww", t),c())
  for(i in kk){
    assign(paste0('ww', t), c(get(paste0('ww', t)), resilience(avg_mc = ss_non[,i],
                          period = cat_per,
                          t_f = t_final,
                          mc = get(paste0("ss_cat_", t))[ , i])))
  }
  
  assign(paste0('ww', t) , get(paste0('ww', t))[is.na(get(paste0('ww', t))) == F])
}



plot(density(ww1), main="风险吸收强度指标：资本比率" , xlim = range(1, ww1, ww2, ww3, 0.4)) ## 概率密度图
for(i in 1:3){
  lines(density(get(paste0('ww', i))), lwd = i*2)
}

for(t in c(1, 2, 3)){
  kk = which(as.numeric(get(paste0("ss_cat_", t))[20,] )< ss_non[20,])
  assign(paste0("tt", t),c())
  for(i in kk){
    assign(paste0('tt', t), c(get(paste0('tt', t)), resilience_time(avg_mc = ss_non[,i],
                                                                    period = cat_per,
                                                                    t_f = t_final,
                                                                    mc = get(paste0("ss_cat_", t))[ , i])))
  }
  
  assign(paste0('tt', t) , get(paste0('tt', t))[is.na(get(paste0('tt', t))) == F])
}
plot(density(tt1), main="风险吸收强度指标：资本比率" , xlim = range(0, tt1, tt2, tt3, 30)) ## 概率密度图
for(i in 1:3){
  lines(density(get(paste0('tt', i))), lwd = i*2)
}













#lines(density(ww), main="风险吸收强度指标：偿付能力", col = 'orange') ## 概率密度图
abline(v = quantile(ww, 0.5))
abline(v = mean(ww), col = 'red')
text(mean(ww), mean(density(ww)$y), 
     labels = paste0("平均值： ",
                     round(mean(ww), digits = 3)))
abline(v = quantile(ww, 0.05))
text(quantile(ww, 0.05), mean(density(ww)$y), 
     labels = paste0("0.05分位数：",
                     round(quantile(ww, 0.05), digits = 3)))
r <- c()
for(i in 1:ncol(ss_cat_1)){
  if(all(ss_cat_1[21:t_final, i] < ss_non[21:t_final])){
    r = c(r, i)
  } 
}

kk = setdiff( which(as.numeric(ss_cat_1[20,] )< ss_non[20]))

ww = c()

for(i in kk){
  ww = c(ww, resilience_time(avg_mc = ss_non[,i],
                        period = cat_per,
                        t_f = t_final,
                        mc = ss_cat[ , i]))
}


ww= ww[is.na(ww) == F]
#ww= ww[-which(ww<0)]
#####  
plot(density(ww), main="风险吸收强度指标：偿付能力",
      type = 'l', col = 'red') ## 概率密度图
abline(v = quantile(ww, 0.5), col = 'red')
text(mean(ww), mean(density(ww)$y + 1), 
     labels = paste0("平均值： ",
                     round(mean(ww), digits = 3)),
     col = 'red')
abline(v = quantile(ww, 0.05),
       col = 'red')
text(quantile(ww, 0.05), mean(density(ww)$y + 1), 
     labels = paste0("0.05分位数：",
                     round(quantile(ww, 0.05), digits = 3)),
     col = 'red')

###############################################################

r <- c()
for(i in 1:ncol(ss_cat)){
  if(all(ss_cat[21:t_final, i] < ss_non[21:t_final])){
    r = c(r, i)
  } 
}

kk = setdiff( which(as.numeric(ss_cat[20,] )< ss_non[20]), r)
ww = c()
for(i in kk){
  ww = c(ww, resilience(avg_mc = ss_non,
                             period = cat_per,
                             t_f = t_final,
                             mc = ss_cat))
}


plot(density(ww), main="风险吸收持续期指标：资本比率") ## 概率密度图
abline(v = quantile(ww, 0.5))
abline(v = mean(ww), col = 'red')
text(mean(ww), mean(density(ww)$y), 
     labels = paste0("平均值： ",
                     round(mean(ww), digits = 2)))
abline(v = quantile(ww, 0.95))
text(quantile(ww, 0.95), mean(density(ww)$y), 
     labels = paste0("0.95分位数：",
                     round(quantile(ww, 0.95), digits = 2)))




## 如果需要进行密度函数对比
r <- c()
for(i in 1:ncol(ss_cat_1)){
  if(all(ss_cat_1[21:t_final, i] < ss_non[21:t_final])){
    r = c(r, i)
  } 
}
kk = setdiff( which(as.numeric(ss_cat_1[20,] )< ss_non[20]), r)
ww = c()
for(i in kk){
  ww = c(ww, resilience_time(avg_mc = ss_non,
                             period = cat_per,
                             t_f = t_final,
                             mc = ss_cat_1[ , i]))
}

lines(density(ww), main="风险吸收持续期指标：偿付能力",
      type = 'l', col = 'red') ## 概率密度图
abline(v = quantile(ww, 0.5), col = 'red')
text(mean(ww), mean(density(ww)$y + 0.1), 
     labels = paste0("平均值： ",
                     round(mean(ww), digits = 3)),
     col = 'red')
abline(v = quantile(ww, 0.95),
       col = 'red')
text(quantile(ww, 0.95), mean(density(ww)$y + 0.1), 
     labels = paste0("0.95分位数：",
                     round(quantile(ww, 0.95), digits = 3)),
     col = 'red')




plot(alpha_cash[,2], type = 'l')
plot(alpha_gu[,2], type = 'l')
plot(alpha_zhai[,2], type = 'l')




for(t in 20:50){
  print(sum(c(alpha_zhai[t, i ], alpha_gu[t, i], alpha_cash[t,i]) * 
              c(rf_rate_mean, 
                mean(rm_rate[(len_rate_history + t-back_year):(len_rate_history + t-1), i]),
                cash_rate_mean)))
}

## 使目标收益率要高于一个值  增加约束
## 是否考虑久期的约束条件


###  分市场份额的比例
## 按照市场份额的排名对保险公司进行排序

ee = read.csv("F://newest/0321_DATA/data_0902/data_1102/new_1/cat_1.1/PRICE_DECLINE_1.1.csv")[,-1]
ee = read.csv("F://newest/0321_DATA/data_0902/data_1102/new_1/cat_1.3/PRICE_DECLINE_1.3.csv")[,-1]

#plot(rowSums(ee)/ncol(ee), type = 'l')
ss = rowSums(ee)/ncol(ee)
for(i in 2:length(ss)){
  ss[i] = (rowSums(ee)/ncol(ee))[i] * ss[i-1]
}
plot(ss, type = 'l')
lines(ss, type = 'b')
