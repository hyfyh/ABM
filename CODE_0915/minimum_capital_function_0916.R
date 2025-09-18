
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

# [1] "deposit"              
# [2] "financial_debt"       
# [3] "company_debt"         
# [4] "10_year_national_debt"
# [5] "20_year_national_debt"
# [6] "stock"                
# [7] "real_estate"          
# [8] "foreign asserts"

####
mc_function <- function(yewu = 1000000, 
                        invest = 1000000,
                        alpha = rep(0.125, 8),
                        x  = 0,
                        RF0_property = 0.103){
  
  #alpha_zhaiquan = alpha[1] 
  #alpha_quanyi = alpha[2]  
  
  rf0_feishou = 0.103
  mc_feishou = yewu * RF0_property
  # mc_shichang = c(mc_债券， mc_股票)
  ## 1、市场风险最低资本
  ## 1)、利率风险最低资本
  ## 财产保险公司利率风险最低资本采用综合因子法计量
  ## 四种债券 金融债 企业债 10年国债 20年国债
  ## 基础因子RF0赋值如下：
  RF0_financial = 0.09 #6.09 * (-0.00106 * 6.09 + 0.022)
  RF0_compnay = 0.07 # 4.09  * (-0.001 * 4.09 + 0.0217)
  RF0_ten = 0.11 #10 * (-0.00106 * 10 + 0.022)
  RF0_twenty = 0.228 # 20*0.0114
  ## 利率风险最低资本
  mc_interest_rate = invest * (alpha[2] * RF0_financial + alpha[3] * RF0_compnay 
                               + alpha[4] * RF0_ten + alpha[5] * RF0_twenty)
  # 2）、权益价格风险、
   ## 权益价格风险最低资本
  ## 沪深主板股
  ## 创业版股、科创版股
  RF0_stock = 0.35  ## 股票的rf0
  mc_stock = invest * alpha[6] * RF0_stock
  # 3）、房 地 产 价 格 风 险、
  RF0_real_estate = 0.15
  mc_real_estate = invest * alpha[7] * RF0_real_estate
  # 4）、境 外 资 产 价 格 风 险 和
  RF0_foreign = 0.08
  mc_foreign = invest * alpha[8] * RF0_foreign
  # 5）、汇 率 风 险 = 0
  mc_exchange_rate = 0
  ## 市场风险向量
  market_vecter = c(mc_interest_rate,mc_stock, mc_real_estate, mc_foreign, mc_exchange_rate )
  market_minimal_matrix <- matrix(c(
    1.00, -0.14, -0.18, -0.16, 0.07,
    -0.14, 1.00, 0.22, 0.50, 0.04,
    -0.18, 0.22, 1.00, 0.19, -0.14,
    -0.16, 0.50, 0.19, 1.00, -0.19,
    0.07, 0.04, -0.14, -0.19, 1.00
  ), nrow = 5, ncol = 5, byrow = TRUE)
  
  mc_market = (t(market_vecter) %*% market_minimal_matrix %*% market_vecter)^ (1/2)
  
  ## 信用风险分为两类  利差风险和交易对手违约风险
  ## 1)、 利差风险
  RF0_financial_licha = 0.03 #6.09*0.001+0.025
  RF0_company_licha = 0.06 #4.09 * (0.0006* 4.09+ 0.012)
  
  mc_licha = invest *(alpha[2] * RF0_financial_licha + alpha[3] * RF0_company_licha)
  
  ## 2)、交易对手违约
  mc_violating = (0.005* alpha[1] + 0.006 * alpha[2] + 0.006 * alpha[3]) *invest
  
  ## 信用风险计算
  mc_credit = (mc_licha ^ 2 + 2*0.25 * mc_licha * mc_violating + mc_violating ^ 2)^(1/2)
  
  ##  构建一个方程计算最低资本
  ## 总mc = (mc_非寿险 , mc_市场， mc_信用风险）
  mc = c(mc_feishou, mc_market, mc_credit)
  
  mc_matrix = matrix(data = c(1, 0.1, 0.15, 0.1, 1, 0.27,
                              0.15, 0.27, 1), ncol = 3)
  mc_zuidi = (t(mc)%*%mc_matrix%*%(mc))^(1/2)
  return(mc_zuidi)
}


# [1] "deposit"              
# [2] "financial_debt"       
# [3] "company_debt"         
# [4] "10_year_national_debt"
# [5] "20_year_national_debt"
# [6] "stock"                
# [7] "real_estate"          
# [8] "foreign_assets"

mc_function_asset <- function(yewu = 1000000, 
                              deposit= 1000000,
                              financial_debt= 1000000,
                              company_debt= 1000000,
                              ten_year_national_debt= 1000000,
                              twenty_year_national_debt= 1000000,
                              stock= 1000000,
                              real_estate= 1000000,
                              foreign_assets= 1000000,
                              #lilv = 0.025,
                              x= 0,
                              RF0_property = 0.103){
  ## 业务风险最低资本
  mc_feishou = yewu *  RF0_property 
  
  ## 1、市场风险最低资本
  ## 1)、利率风险最低资本
  ## 财产保险公司利率风险最低资本采用综合因子法计量
  ## 四种债券 金融债 企业债 10年国债 20年国债
  ## 基础因子RF0赋值如下：
  RF0_financial = 0.09 #6.09 * (-0.00106 * 6.09 + 0.022)
  RF0_compnay = 0.07 # 4.09  * (-0.001 * 4.09 + 0.0217)
  RF0_ten = 0.11 #10 * (-0.00106 * 10 + 0.022)
  RF0_twenty = 0.228 # 20*0.0114
  ## 利率风险最低资本
  mc_interest_rate = financial_debt * RF0_financial +  RF0_compnay * company_debt 
                               + ten_year_national_debt * RF0_ten + twenty_year_national_debt * RF0_twenty
   
  # 2）、权益价格风险、
  ## 权益价格风险最低资本
  ## 沪深主板股
  ## 创业版股、科创版股
  RF0_stock = 0.35  ## 股票的rf0
  mc_stock = stock * RF0_stock
  # 3）、房 地 产 价 格 风 险、
  RF0_real_estate = 0.15
  mc_real_estate = real_estate * RF0_real_estate
  # 4）、境 外 资 产 价 格 风 险 和
  RF0_foreign = 0.08
  mc_foreign = foreign_assets * RF0_foreign
  # 5）、汇 率 风 险 = 0
  mc_exchange_rate = 0
  ## 市场风险向量
  market_vecter = c(mc_interest_rate,mc_stock, mc_real_estate, mc_foreign, mc_exchange_rate )
  market_minimal_matrix <- matrix(c(
    1.00, -0.14, -0.18, -0.16, 0.07,
    -0.14, 1.00, 0.22, 0.50, 0.04,
    -0.18, 0.22, 1.00, 0.19, -0.14,
    -0.16, 0.50, 0.19, 1.00, -0.19,
    0.07, 0.04, -0.14, -0.19, 1.00
  ), nrow = 5, ncol = 5, byrow = TRUE)
  
  mc_market = (t(market_vecter) %*% market_minimal_matrix %*% market_vecter)^ (1/2)
  
  ## 信用风险分为两类  利差风险和交易对手违约风险
  ## 1)、 利差风险
  RF0_financial_licha = 0.03 #6.09*0.001+0.025
  RF0_company_licha = 0.06 #4.09 * (0.0006* 4.09+ 0.012)
  mc_licha = financial_debt * RF0_financial_licha + company_debt * RF0_company_licha
  
  ## 2)、交易对手违约
  mc_violating = 0.005* deposit + 0.006 * financial_debt + 0.006 * company_debt
  
  ## 信用风险计算
  mc_credit = (mc_licha ^ 2 + 2*0.25 * mc_licha * mc_violating + mc_violating ^ 2)^(1/2)
  
  ##  构建一个方程计算最低资本
  ## 总mc = (mc_非寿险 , mc_市场， mc_信用风险）
  mc = c(mc_feishou, mc_market, mc_credit)
  
  mc_matrix = matrix(data = c(1, 0.1, 0.15, 0.1, 1, 0.27,
                              0.15, 0.27, 1), ncol = 3)
  mc_zuidi = (t(mc)%*%mc_matrix%*%(mc))^(1/2)
  return(mc_zuidi)
}

#############################################

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



#####################################################################


## 确定无风险资产比例alpha，无风险资产规模是保险公司进行保单偿付所需流动性的总规模
fr <- function(alpha,
               return_expected = invest_avaliable$return_expected,
               vol_expected = invest_avaliable$vol_expected,
               risk_aversion = risk_aversion_par) {
  return(-sum(c(alpha[1],alpha[2],alpha[3],alpha[4],alpha[5],alpha[6],
                alpha[7],alpha[8]) * return_expected * (1- risk_aversion)) + 
           risk_aversion * (t(as.matrix(c(alpha[1],alpha[2],alpha[3],alpha[4],alpha[5],alpha[6],
                                         alpha[7],alpha[8]) * vol_expected)) %*% 
           invest_cor_matrix  %*% as.matrix(c(alpha[1],alpha[2],alpha[3],alpha[4],alpha[5],alpha[6],
                                              alpha[7],alpha[8]) * vol_expected)) ^ 0.5)
}

confun1 = function(alpha){
  f = NULL
  f = rbind(f, 
            - Equity0[i] / mc_function(yewu =  E[t, i] * P_p[t, i],
                                       invest = invest[t, i],
                                       alpha = alpha,
                                       #lilv = 0.025,
                                       x  = 0,
                                       RF0_property = mc_rf0[i]) + sl_ratio_floor)
  ## 流动性资产 下限 0.1
  #f = rbind(f, -(alpha[1] + alpha[4] + alpha[5])* invest[t, i] + E[t, i] *P_p[t, i]) ## 下限
  ## 
  f = rbind(f, - (alpha[1] + alpha[4] + alpha[5] + 0.85 * alpha[2] + 0.8 * alpha[3] 
                   + 0.5 * alpha[6] + 0.5 * alpha[8]) * invest[t, i] + E[t, i] *P_p[t, i]) ## 下限
  ## 权益类资产上限 30% 
  f = rbind(f, alpha[6] - 0.3) ## 上限
  ## 不动产 上限 30 %
  f = rbind(f, alpha[7] - 0.3)
  ## 其他金融资产 上限 25%
  f = rbind(f, alpha[8] - 0.15)
  
  pp = NULL
  pp = rbind(pp, alpha[1] + alpha[2] + alpha[3] + alpha[4] + alpha[5] +
               alpha[6] +alpha[7] +alpha[8] - 1)
  return(list(ceq=pp,c=f))
}


confun2 = function(alpha){
  f = NULL
  f = rbind(f, 
            - Equity[t-1, i] / mc_function(yewu =  E[t, i] *P_p[t, i],
                                           invest = invest[t, i],
                                           alpha = alpha,
                                           #lilv = 0.025,
                                           x  = 0,
                                           RF0_property = mc_rf0[i] ) + sl_ratio_floor)
  ## 流动性资产 下限 0.1
  #f = rbind(f, -(alpha[1] + alpha[4] + alpha[5])* invest[t, i] + E[t, i] * P_p[t, i]) ## 下限
  ## 
  f = rbind(f, - (alpha[1] + alpha[4] + alpha[5] + 0.85 * alpha[2] + 0.8 * alpha[3] 
                   + 0.5 * alpha[6] + 0.5 * alpha[8]) * invest[t, i] + E[t, i] *P_p[t, i]) ## 下限
  ## 权益类资产上限 30% 
  f = rbind(f, alpha[6] - 0.3) ## 上限
  ## 不动产 上限 30 %
  f = rbind(f, alpha[7] - 0.3)
  ## 其他金融资产 上限 25%
  f = rbind(f, alpha[8] - 0.15)
  
  pp = NULL
  pp = rbind(pp, alpha[1] + alpha[2] + alpha[3] + alpha[4] + alpha[5] +
               alpha[6] +alpha[7] +alpha[8] - 1)
  return(list(ceq=pp,c=f))
}




lll

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





## 确定无风险资产比例alpha，无风险资产规模是保险公司进行保单偿付所需流动性的总规模
fr <- function(alpha,
               return_expected = invest_avaliable$return_expected,
               vol_expected = invest_avaliable$vol_expected,
               risk_aversion = risk_aversion_par) {
  return(-sum(c(alpha[1],alpha[2],alpha[3],alpha[4],alpha[5],alpha[6],
                alpha[7],alpha[8]) * return_expected * (1- risk_aversion)) + 
           risk_aversion * t(as.matrix(c(alpha[1],alpha[2],alpha[3],alpha[4],alpha[5],alpha[6],
                                         alpha[7],alpha[8]) * vol_expected)) %*% 
           invest_cor_matrix  %*% as.matrix(c(alpha[1],alpha[2],alpha[3],alpha[4],alpha[5],alpha[6],
                                              alpha[7],alpha[8]) * vol_expected))
}

confun1 = function(alpha){
  f = NULL
  f = rbind(f, 
            - Equity0[i] / mc_function(yewu =  E[t, i] *P_p[t, i],
                                       invest = invest[t, i],
                                       alpha = alpha,
                                       #lilv = 0.025,
                                       x  = 0,
                                       RF0_property = mc_rf0[i]) + sl_ratio_floor)
  ## 流动性资产 下限 0.1
  f = rbind(f, -alpha[1] + 0.1) ## 下限
  
  ## 
  f = rbind(f, - (alpha[1] + alpha[4] + alpha[5] + 0.85 * alpha[2] + 0.8 * alpha[3] 
                  + 0.5 * alpha[6] + 0.5 * alpha[8]) * invest[t, i] - E[t, i] *P_p[t, i]) ## 下限
  
  ## 权益类资产上限 30% 
  f = rbind(f, alpha[6] - 0.3) ## 上限
  ## 不动产 上限 30 %
  f = rbind(f, alpha[7] - 0.3)
  ## 其他金融资产 上限 25%
  f = rbind(f, alpha[8] - 0.15)
  
  pp = NULL
  pp = rbind(pp, alpha[1] + alpha[2] + alpha[3] + alpha[4] + alpha[5] +
               alpha[6] +alpha[7] +alpha[8] - 1)
  return(list(ceq=pp,c=f))
}


confun2 = function(alpha){
  f = NULL
  f = rbind(f, 
            - Equity[t-1, i] / mc_function(yewu =  E[t, i] *P_p[t, i],
                                                invest = invest[t, i],
                                                alpha = alpha,
                                                #lilv = 0.025,
                                                x  = 0,
                                                RF0_property = mc_rf0[i] ) + sl_ratio_floor)
  ## 流动性资产 下限 0.1
  f = rbind(f, -alpha[1] + 0.1) ## 下限
  ## 
  f = rbind(f, - (alpha[1] + alpha[4] + alpha[5] + 0.85 * alpha[2] + 0.8 * alpha[3] 
                  + 0.5 * alpha[6] + 0.5 * alpha[8]) * invest[t, i] - E[t, i] *P_p[t, i]) ## 下限
  ## 权益类资产上限 30% 
  f = rbind(f, alpha[6] - 0.3) ## 上限
  ## 不动产 上限 30 %
  f = rbind(f, alpha[7] - 0.3)
  ## 其他金融资产 上限 25%
  f = rbind(f, alpha[8] - 0.15)
  
  pp = NULL
  pp = rbind(pp, alpha[1] + alpha[2] + alpha[3] + alpha[4] + alpha[5] +
               alpha[6] +alpha[7] +alpha[8] - 1)
  return(list(ceq=pp,c=f))
}




alpha = c(1, 0 ,0,0,0,0,0,0)
tryCatch( {alpha  = solnl(x0, objfun = fr, confun=confun2, lb =c(0,0,0, 0,0,0,0,0), ub = c(1,1,1,1,1,1,1,1),
                          tolX = 1e-01,
                          tolFun = 1e-02, tolCon = 1e-01, maxnFun = 1e+02, maxIter = 400)$par},
          error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
deposit[t, i] = alpha[1] * invest[t, i] * (1 + rate_deposit[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司的定期存款,矩阵结构             
financial_debt[t, i] = alpha[2] * invest[t, i] * (1 + rate_financial[t]) ### 每个公司金融债,矩阵结构     
company_debt[t, i] = alpha[3] * invest[t, i] * (1 + rate_company[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司企业债,矩阵结构
ten_year_national_debt[t, i] = alpha[4] * invest[t, i] * (1 + rate_ten[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司10年期国债,矩阵结构
twenty_year_national_debt[t, i] = alpha[5] * invest[t, i] * (1 + rate_twenty[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司二十年期国债,矩阵结构
stock[t, i] = alpha[6] * invest[t, i] * (1 + rate_stock[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司股票,矩阵结构              
real_estate[t, i] = alpha[7] * invest[t, i] * (1 + rate_real_estate[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司房地产,矩阵结构          
foreign_assets[t, i] = alpha[8] * invest[t, i] * (1 + rate_foreign[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司国外资产,矩阵结构
## 流动性直接算  不用折算



## 计算当期被接管公司经营情况
if(is.null(n_exit) == FALSE){
  for(i in n_exit) {
    K_begin[t, i] = K[t-1, i] ## 被接管公司期初总资产为0
    E[t, i] = 0 ## 市场份额为0
    L[t, i] = 0
    P_exp[t, i] = 0
    LR[t, i] = 0
    P_p[t, i] = 0
    P_l[t, i] = 0
    P[t, i] = 100000000
    ## 这里的P值无意义，使其最大的目的是防止下一期有保险人流向被接管的保险公司
    #alpha[t, i] = 0
    #FV_free[t, i] = 0
    invest[t, i] = 0
    FV_risk[t, i] =  0
    cash[t, i] = 0
    Equity[t-1, i] = 0
    deposit[t, i] = 0
    financial_debt[t, i] = 0
    company_debt[t, i] = 0
    ten_year_national_debt[t, i] = 0
    twenty_year_national_debt[t, i] = 0
    stock[t, i] = 0
    real_estate[t, i] = 0
    foreign_assets[t, i] = 0
  }
}  



cash[t, ] = deposit[t, ] +  ten_year_national_debt[t, ] + twenty_year_national_debt[t, ] 

for (i in 1:length(rho)) {
  if(cash[t, i] > 0 ){
    if(L[t, i] > cash[t, i]){ ## 如果赔付支出大于公司所能提供的流动性之和
      liq_gap[t, i] = L[t, i] - cash[t, i] ## 流动性缺口规模
      ## 为了公司进行业务赔付，保险公司需要进行第一批次的减价出售，出售股权投资，50%的折价
      ## 如果存在流动性缺口，那么就需要进行减价出售机制
      ## 如果没有缺口那就不进行减价出售
      cash[t, i] = cash[t, i] + liq_gap[t, i]
    }
  }
}

## 减价出售机制设定
sum_lip = sum(liq_gap[t, ])
#FV_risk[t, ] = 0
FV_risk[t, ] = financial_debt[t, ] + company_debt[t, ] + stock[t, ] + real_estate[t, ] + foreign_assets[t, ]
sum_risk = sum(FV_risk[t, ])
pp = 0
p = 1


n_exit <- c()
for(i in 1:length(rho)){
  fire_sum = sum(financial_debt[t, i] + company_debt[t, i] + stock[t, i] +real_estate[t, i]+ foreign_assets[t, i])
  financial_debt[t, i] = p * FV_risk[t, i] * financial_debt[t, i]/fire_sum
  company_debt[t, i] = p * FV_risk[t, i] * company_debt[t, i]/fire_sum
  stock[t, i] = p * FV_risk[t, i] * stock[t, i]/fire_sum
  real_estate[t, i] = p * FV_risk[t, i] * real_estate[t, i]/fire_sum
  foreign_assets[t, i] = p * FV_risk[t, i] * foreign_assets[t, i]/fire_sum
  FV_final[t, i] = p * FV_risk[t, i] + cash[t, i]

  # 经营结果
  pi[t, i] = FV_final[t, i]  - invest[t, i] + #投资利润
    E[t, i] * P_p[t, i] - L[t, i] ## 承保利润  ## 现阶段的利润
  ## 算是否需要进行偿付能力调整
  Equity[t, i] = Equity0[i] + pi[t, i]   ## 经营结束之后的权益量
  
  mc_zuidi[t, i] = mc_function_asset(yewu = E[t, i] * P_p[t, i], 
                                     deposit = deposit[t, i],
                                     financial_debt= financial_debt[t, i],
                                     company_debt= company_debt[t, i],
                                     ten_year_national_debt= ten_year_national_debt[t, i],
                                     twenty_year_national_debt= twenty_year_national_debt[t, i],
                                     stock= stock[t, i],
                                     real_estate= real_estate[t, i],
                                     foreign_assets= foreign_assets[t, i],
                                     x= 0,
                                     RF0_property = mc_rf0[i])
}


while (round(sum_lip) > 0 & p > 0.5 ) {
  # 先计算出售的价格
  #sum_v = sum_lip/p
  p_n = p * exp( - k10 * sum_lip / sum_risk)  ## 价格的下降幅度
  FV_risk[t, ] = (FV_risk[t, ] / p - liq_gap[t, ]/p)*p_n
  liq_gap[t, ] = (p - p_n) * liq_gap[t, ]/p
  sum_lip = sum(liq_gap[t, ])
  sum_risk = sum(FV_risk[t, ])
  pp = pp + 1   ## 减价出售轮数
  p = p_n
}     









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
