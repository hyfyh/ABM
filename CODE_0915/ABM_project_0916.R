

###################
#rm(list = ls())
#gc()
#######################

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

## 对于冲击方向的设定

cat_per = 20

catastrophe = c(1.2 )   ## 是否发生巨灾风险  0：不发生  1：发生

for(i in catastrophe){
  assign(paste0("MC_ZUIDI_", i), c())
  assign(paste0("MC_ZUIDI_lower_", i), c())
  assign(paste0("MC_ZUIDI_higher_", i), c())
  assign(paste0("PRICE_DECLINE_", i), c())
  assign(paste0("RETURN_", i), c())
  assign(paste0("NUMBER_", i), c())
  assign(paste0("NUMBER_BE_", i), c())
  assign(paste0("RHO_SQUARE_", i), c())
  assign(paste0("P_PURE_", i), c())
  assign(paste0("BETA_", i), c())
  assign(paste0("P_GROSS_", i), c())
  assign(paste0("K_SUM_", i), c())
  assign(paste0("EQUITY_RATE_", i), c())
  assign(paste0("PRO_EXIT_", i), c())
  assign(paste0("LR_SUM_", i), c())
  assign(paste0('INDIRECT_LOSS_', i), c())
}

#setwd("F://newest/newest")
risk_aversion_par = 0.5 ## 风险厌恶系数 标准情况下冲击系数为1  我们将它扩大一定的倍数

ppp = 1 ## 关于记录循环次数的代码
time  = 1 ##
t_final = 50 ## 每一期的长度

n_max = 120
rho_m = 0.5  ## 市场份额分组
sl_ratio = 0.5  ## 核心偿付能力充足率标准  
sl_ratio_floor = 1.5  ## 保险公司在投资时的偿付能力要求

# data_raw <- read.csv("F://newest/newest/data_result_0326_final.csv",
#                      fileEncoding = "gbk")

#data_raw = left_join(data_raw, mc_rf0_data)
uni_com <- unique(data_full$Conm)
#mc_rf0 = c()
#for(i in uni_com){
#   mc_rf0 = c(mc_rf0, data_raw$mc_rf0[min(which(data_raw$company == i))])
# }
mc_rf0 <- mc_rf0_data$mc_rf0
mc_rf0[(length(uni_com) + 1): n_max] = quantile(as.numeric(mc_rf0), 0.75)

###
p_basic <- 1000 
beta0 <-  data_full$beta0 # - 0.0332*(log(data_full$Combas0236 )) + 0.576
## beta

#data_raw$beta <- data_raw$load/c(data_raw$premium_revenue - data_raw$load)  

## 使用的是十年期国债的收益率和方差
back_year = 5
len_rate_history = 10

x0=rep(0.125, 8)
## loss ratio
## data_raw$loss_ratio = data_raw$loss/c(data_raw$premium_revenue - data_raw$load)

# leverage
LR_basic = rep(0.99, n_max)#data$loss_ratio##################################这里修改了基础赔付率水平


## 保单的增长率
g <-  as.numeric(1 + diff(growth_mean) /growth_mean)# rep(1.05, t_final)#rep(1.06, t_final) #exp(c(t_final:1)/10/t_final)
K0 <-  data_full$Combas0236 * 1000000  ## 总资产
Equity0 <- data_full$Combas0277 * 1000000 ## 所有者权益

E0 <-  sum(data_full$Compal0117)/p_basic *1000000 #   703043520 * 1 #sum(data$pre_earned - data$load) * 1000000 / p_basic   ## 最初的市场总风险单位数  
################################################
## 参数部分
k1 <- 0.667 #  # 保费对纯保费赔付率的敏感性参数
k3 <- 0.5  # 公司上一期提取的附加保费对这一期的影响
k4 <- 0.5 #0.5  # 分红比率  
k5 <- rep(0.05, t_final) #  保险市场吸引资本进入的利润率阈值#######
k6 <- 20 # 被视作每单位超过阈值水平的利润率对新资本的吸引能力 
k9 <- 0.0001 #0.01   # 价格弹性
k10 <-  0.7   ## 风险资产的价格下降速度

## 固定值部分
LR_sum0 = 1
LR_sum_basic = 0.99 #mean(data$LR_basic)
n_new_max <- 2 ## 新进入保险 公司数量的最大值


normal <- 1 
for(loop_b in 1:2){
  
  ## 投资收益率
  
  rate_deposit = rnorm(100, invest_avaliable$return_expected[1], invest_avaliable$vol_expected[1])
  rate_financial = rnorm(100, invest_avaliable$return_expected[2], invest_avaliable$vol_expected[2])
  rate_company = rnorm(100, invest_avaliable$return_expected[3], invest_avaliable$vol_expected[3])
  rate_ten = rnorm(100, invest_avaliable$return_expected[4], invest_avaliable$vol_expected[4])
  rate_twenty = rnorm(100, invest_avaliable$return_expected[5], invest_avaliable$vol_expected[5])
  rate_stock = rnorm(100, invest_avaliable$return_expected[6], invest_avaliable$vol_expected[6])
  rate_real_estate = rnorm(100, invest_avaliable$return_expected[7], invest_avaliable$vol_expected[7])
  rate_foreign = rnorm(100, invest_avaliable$return_expected[8], invest_avaliable$vol_expected[8])
  
  loop_1 = 1
  
  for(shock_und in catastrophe){
    
    if(normal <= time){
      attempt <- 1
      repeat {
        
        result <- tryCatch({
          
          rho <- c()
          rho = data_full$Compal0117/sum(data_full$Compal0117)
          gamma <- rep(0, n_max)  # 保险人对竞争敏感程度,决定方式另附
          
          ## 最初的市场份额，10家公司，每家0.1 ## sum(rho) == 1 
          ## rho的长度就等于第一期的公司数量
          
          avg <- rep(0, 30)
          ########################################################################################
          ## 保险公司经营部分的数据
          
          E <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司的风险单位数
          L <- matrix(nrow = t_final, ncol = n_max) ## 每个公司的损失赔付
          P_exp  <- matrix(nrow = t_final, ncol = n_max) ## 每个公司的预期纯保费
          LR  <- matrix(nrow = t_final, ncol = n_max) ## 纯保费赔付率
          P_p  <- matrix( nrow = t_final, ncol = n_max)  ## 实际的纯保费
          P_l  <- matrix( nrow = t_final, ncol = n_max) ## 附加保费
          P  <- matrix(nrow = t_final, ncol = n_max) ## 总保费(等于实际的纯保费加附加保费)
          pi  <- matrix(0, nrow = t_final, ncol = n_max)  ## 每个公司的利润，实际数值，不是比率
          K_begin <- matrix(0, nrow = t_final, ncol = n_max)  ## 每一个保险公司期初的资产，为了计算当期收益率简便
          K  <- matrix(0, nrow = t_final, ncol = n_max)  ## 每个公司的期末资产
          beta <- matrix(nrow = t_final, ncol = n_max) ## 每个公司的附加费用比率,矩阵结构
          Fee <- matrix(0,nrow = t_final, ncol = n_max) ## 每个公司管理费用,矩阵结构
          
          
          alpha <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司管理费用,矩阵结构
          ## 投资品类设置
          
          
          
          deposit <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司的定期存款,矩阵结构             
          financial_debt <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司金融债,矩阵结构     
          company_debt <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司企业债,矩阵结构
          ten_year_national_debt <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司10年期国债,矩阵结构
          twenty_year_national_debt <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司二十年期国债,矩阵结构
          stock <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司股票,矩阵结构              
          real_estate <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司房地产,矩阵结构          
          foreign_assets <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司国外资产,矩阵结构
          
          cash <- matrix(0, nrow = t_final, ncol = n_max) ## 保险公司的货币资金账户
          
          
          
          
         # FV_free <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司管理费用,矩阵结构
          FV_risk <- matrix(0,nrow = t_final, ncol = n_max) ## 每个公司管理费用,矩阵结构
          FV_final <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司管理费用,矩阵结构
          
          
          
          liq_gap <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司流动性缺口,矩阵结构
          indirect_loss <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司因减价出售而出现的间接损失
          Equity <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司的所有者权益
          D <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司的股东分配利润
          mc_zuidi <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司核算下来的最低资本
          rho_indentity <- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司的市场份额，仅储存用
          invest <- matrix(0, nrow = t_final, ncol = n_max)  ## 保险公司总投资额
          
          
          pi_uw <- matrix(0, nrow = t_final, ncol = n_max)  ## 承保利润
          pi_iv <- matrix(0, nrow = t_final, ncol = n_max) ## 投资利润
          fire_sale <- matrix(0, nrow = t_final, ncol = n_max) ## 投资利润
          liqgap_com <- double(length = t_final) ## 当期初次流动性缺口公司数量
          w = rep(0, t_final)  ## 每一期减价出售的轮数
          equity_sum <- c() ## 用来储存当期期初总资本
          equity_sum[1] = sum(Equity0)
          K_begin_sum <- double(length = t_final)  ## 用来储存保险公司期初总资产
          liq_gap_sum <- double(length = t_final) ## 整个市场的流动性缺口
          price_decline <- double(length = t_final) ## 风险资产的价格下降程度
          price_decline_final <- rep(1, t_final) ## 每一期价格下降的最终程度
          
          #########  保险市场总的纯保费赔付率，用来确定k2  #############################################
          k2 <- double(length = t_final)
          L_sum <- double(length = t_final)  ## 保险市场总期末损失
          P_p_sum <- double(length = t_final)   ## 保险市场总纯保费
          LR_sum <- c(LR_sum0, double(length = t_final)) ## 保险市场总纯保费赔付率
          E_sum <- c(E0, double(length = t_final-1))
          
          ## 因为计算k2值时，要用到LR_sum[t-1]，所以对于第一期k2,这里设置用0.9的LR_sum来计算
          
          ################################  接管退出机制  ###############################################
          pi_sum <- double(length = t_final)  ## 保险市场总利润
          K_sum <- double(length = t_final)   ## 保险市场总资产
          Return <- double(length = t_final)  ## 整个保险市场的利润率 
          n_new <- integer(length = t_final)  ## 储存的是每一期结束时，新进入的公司数量
          loss <- rep(0, t_final)
          len_rho <- rep(0, t_final)
          fire_par_serise <- rep(0, t_final)
          beta_avg <- c()
          ###########
          
          ## 参数设定部分结束
          ############################################################################################
          ## 第一期经营的步骤
          
          ## 第一期的经营情况####################
          
          t = 1
          
          ## 分组
          
          group <- rep(1, length(rho))  ## 组别重置
          
          for(i in c((length(rho)):2)){
            if ((diff(rho[order(rho)])/rho[2:length(rho)])[i-1] > rho_m){
              group[i-1] = group[i] + 1
            }else{
              group[i-1] = group[i]
            }
          }
          ## 此时尾部的rho值已经非常接近，很难再有20%的差别
          
          group <- group[rank(rho)]  ## 最终组别
          
          ## 要确定组内平均费率水平
          ## 首期的公司都是正常经营的公司，所以不需要进行其他调整
          
          sum_j <- rep(0, 30)
          
          for (i in unique(group)) {
            j = which(group == i)
            for (x in j){
              sum_j[i] = sum_j[i] + beta0[x]
            }
            avg[i] = sum_j[i]/length(j)
          }
          
          ## 计算所有保险公司经营情况
          
          for (i in c(1:length(rho))) { ## length(rho)为t期公司数量
            #shock = cat_per[t, normal] * kkk * equity_sum[t]  * rho[i]
            K_begin[t, i] = K0[i] 
            E[t, i] = E_sum[t] * rho[i] * g[1] ## 风险单位数
            L[t, i] = rpois(1, E[t, i])*rgamma(1, shape = p_basic /2 , scale = 2) #+ shock_und* shock
            P_exp[t, i] = p_basic
            P_p[t, i] = P_exp[t, i] 
            if(shock_und != 0 & cat_per == t){  ## 这里判断是否会发生风险冲击，发生的话赔付比例120%
              L[t, i] = shock_und * E[t, i] * P_p[t, i]
            }
            LR[t, i] = L[t, i] / E[t, i] / P_p[t, i]
            gamma[t] = 1 - exp(-LR_sum[t])
            beta[t, i] = beta0[i]#*(beta0[i] / avg[group[i]]) ^ -(gamma[t]) 
            P_l[t, i] = beta[t, i] * P_p[t, i]  ## 附加保费
            P[t, i] =  P_p[t, i] + P_l[t, i] ## 总保费
            ## 投资业务部分
            invest[t, i] = K_begin[t, i]  ## 第一期设为不变 
            alpha = c(1, 0 ,0,0,0,0,0,0)
            tryCatch( {alpha  = solnl(x0, objfun = fr, confun=confun1, lb =c(0,0,0,0,0,0,0,0), ub = c(1,1,1,1,1,1,1,1),
                                      tolX = 1e-02,
                                      tolFun = 1e-03, tolCon = 1e-02, maxnFun = 1e+03, maxIter = 400)$par},
                      error=function(e){
                        ## 报错时执行的操作(此处留空表示静默跳过)
                        #cat("ERROR :",t, i, conditionMessage(e), "\n")
                        invisible(NULL)
                        })
            
            ## 第一期新公司使用与设定值
            ## 期末无风险现金流:无风险资产比重*总资产*（1+无风险收益率）
            
            deposit[t, i] = alpha[1] * invest[t, i] * (1 + rate_deposit[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司的定期存款,矩阵结构             
            financial_debt[t, i] = alpha[2] * invest[t, i] * (1 + rate_financial[t]) ### 每个公司金融债,矩阵结构     
            company_debt[t, i] = alpha[3] * invest[t, i] * (1 + rate_company[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司企业债,矩阵结构
            ten_year_national_debt[t, i] = alpha[4] * invest[t, i] * (1 + rate_ten[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司10年期国债,矩阵结构
            twenty_year_national_debt[t, i] = alpha[5] * invest[t, i] * (1 + rate_twenty[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司二十年期国债,矩阵结构
            stock[t, i] = alpha[6] * invest[t, i] * (1 + rate_stock[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司股票,矩阵结构              
            real_estate[t, i] = alpha[7] * invest[t, i] * (1 + rate_real_estate[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司房地产,矩阵结构          
            foreign_assets[t, i] = alpha[8] * invest[t, i] * (1 + rate_foreign[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司国外资产,矩阵结构
            ## 流动性直接算  不用折算
            
            #FV_free[t, i] = alpha_zhai[t, i] * invest[t, i] * (1 + rf_rate[t])
            #FV_risk[t, i] = alpha_gu[t, i] * invest[t, i] * (1 + rm_rate[t, i]) #############################
            
            ## 计算出公司的流动性缺口，如果缺口为正就表示当期承保利润为正
          }
          
          
          ### 减价出售机制起点  
          ## 储存减价出售之前的无风险资产和风险资产规模
          # FV_free_before[t, ]= FV_free[t, ]
          # FV_risk_before[t, ]= FV_risk[t, ]
          cash[t, ] = deposit[t, ] +  ten_year_national_debt[t, ] + twenty_year_national_debt[t, ] 
          
          for (i in 1:length(rho)) {
            if(cash[t, i] > 0 & L[t, i] > cash[t, i]){
              liq_gap[t, i] = L[t, i] - cash[t, i] ## 流动性缺口规模
              ## 为了公司进行业务赔付，保险公司需要进行第一批次的减价出售，出售股权投资，50%的折价
              ## 如果存在流动性缺口，那么就需要进行减价出售机制
              ## 如果没有缺口那就不进行减价出售
             # cash[t, i] = cash[t, i] + liq_gap[t, i]
            }
          }
          
          ## 减价出售机制设定
          sum_lip = sum(liq_gap[t, ])
          FV_risk[t, ] = 0
          FV_risk[t, ] = financial_debt[t, ] + company_debt[t, ] + stock[t, ] + real_estate[t, ] + foreign_assets[t, ]
          sum_risk = sum(FV_risk[t, ])
          pp = 0
          p = 1
          
          while (round(sum_lip) > 0 & p > 0.5 ) {
            # 先计算出售的价格
            #sum_v = sum_lip/p
            p_n = p * exp( - k10 * sum_lip / p / sum_risk)  ## 价格的下降幅度
            FV_risk[t, ] = (FV_risk[t, ] / p - liq_gap[t, ]/p)*p_n
            liq_gap[t, ] = (p - p_n) * liq_gap[t, ]/p
            sum_lip = sum(liq_gap[t, ])
            sum_risk = sum(FV_risk[t, ])
            pp = pp + 1   ## 减价出售轮数
            p = p_n
          }   
          
          price_decline[t] = p
          w[t] = pp
          
          n_exit <- c()
          for(i in 1:length(rho)){
            fire_sum = max(1, sum(financial_debt[t, i] + company_debt[t, i] + stock[t, i] +real_estate[t, i]+ foreign_assets[t, i]))
            financial_debt[t, i] = FV_risk[t, i] * financial_debt[t, i]/fire_sum
            company_debt[t, i] = FV_risk[t, i] * company_debt[t, i]/fire_sum
            stock[t, i] = FV_risk[t, i] * stock[t, i]/fire_sum
            real_estate[t, i] = FV_risk[t, i] * real_estate[t, i]/fire_sum
            foreign_assets[t, i] = FV_risk[t, i] * foreign_assets[t, i]/fire_sum
            FV_final[t, i] = FV_risk[t, i] + cash[t, i]
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
            
            s = Equity[t, i] / mc_zuidi[t, i]
            tt = NA
            ## 如果偿付能力达标，并且当期利润为正的话就会进行分红  ## 暂时不设分红
            if(s < sl_ratio){
              ## 当偿付能力不达标的情况下
              ## 资产状况可以调整
              ##  对于不满足偿付能力要求的公司来讲
              n_exit <- append(n_exit, i)
              # if("try-error" %in% class(try(uniroot(f_mc,
              #                                       lower = 0,
              #                                       upper =  FV_risk[t, i],
              #                                       tol = 1,
              #                                       Equity = Equity[t, i],
              #                                       yewu = P_p[t, i]* E[t, i] ,
              #                                       cash = cash[t,i],
              #                                       FV_free = FV_free[t, i],
              #                                       FV_risk = FV_risk[t, i],
              #                                       sl_ratio = sl_ratio,
              #                                       lilv = rate_discount),
              #                               silent = T,
              #                               outFile = getOption("try.outFile", default = stderr())))){
              #   ## 如果偿付能力不足，并且也没有办法通过资产调整的方式进行调整
              #   ## 那么下一期公司就会退出市场
              #   n_exit <- append(n_exit, i)
              # }else{
              #   ## 如果可以通过资产调整的方式对偿付能力进行调整
              #   ## 那么就调整
              #   tt = uniroot(f_mc,
              #                lower = 0,
              #                upper =  FV_risk[t, i],
              #                tol = 1,
              #                Equity = Equity[t, i],
              #                yewu = P[t, i]* E[t, i] ,
              #                cash = cash[t,i],
              #                FV_free = FV_free[t, i],
              #                FV_risk = FV_risk[t, i],
              #                sl_ratio =  sl_ratio,
              #                lilv = rate_discount)$root 
              #   Equity[t, i] =  Equity[t, i]  - tt 
              #   fire_sale[t, i] = tt
              # }
            }else{
              D2 = (Equity[t-1, i] - sl_ratio * mc_zuidi[t, i] + pi[t, i]) * k4
              if(pi[t, i] > 0 ){  ## 如果当期的收益值大于0
                D1 = pi[t, i] * k4
                D[t, i] =  min(D1, D2)
                Equity[t, i] = Equity[t, i] - D[t, i]
              }
            }
            
            n_exit = unique(n_exit)
            pi_uw[t, i] = E[t, i] * P_p[t, i] - L[t, i]   ###  单期的承保利润
            pi_iv[t, i] = FV_final[t, i] - invest[t, i]
            
            
            ## 期末总资产
            K[t, i] = invest[t, i] + pi[t, i] - D[t, i] 
            ## 对于被接管公司来讲，期末总资产就为期初总资产
            if(E[t, i] == 0){
              K[t, i] = K_begin[t, i]
            }
            ## 所有者权益变动,保险公司按照杠杆率进行总资本的补充
            #Equity[t, i] = leverage[i] * K0[i] + pi[t, i] - D[t, i]
            rho_indentity[t, i] = rho[i]
          }
          
          
          ## 计算当期市场利润率Return[t]
          ## 市场收益率计算存在问题
          ## 先计算收益率还是先进行利润分配
          ## 收益率中对于总资产的计算可以进行简化
          
          K_begin_sum[t] =  sum(invest[t,])  ## 保险市场总资产 期初值
          K_sum[t] =  sum(K[t, ]) ## 保险市场总资产（期末值）
          equity_sum[t] = sum(Equity[t,])
          pi_sum[t] = sum(pi[t,])
          Return[t] = 2 * as.double(pi_sum[t] / (K_begin_sum[t] + K_sum[t]))  ## 计算市场的收益率
          
          ## 计算当期LR_sum[t+1],用来计算下一期的k2
          
          for(i in c(1 : length(rho))){
            L_sum[t] = as.double(L_sum[t] + L[t, i])
            P_p_sum[t] = as.double(P_p_sum[t]+P_p[t, i]* E[t, i])
            LR_sum[t+1] = as.double(L_sum[t] / P_p_sum[t])  
          }
          
          loss[t] = sum(L[t, which(is.na(L[t, ]) != T)])
          len_rho[t] = length(rho)
          ## 当期平均beta值
          
          beta_avg = c(beta_avg, as.numeric(quantile(beta[t,][which(E[t,] != 0)], 0.75, na.rm = T)))
          ## 第一期经营结束==================================================
          
          ### 第二期经营开始
          
          t = 2
          
          ## 保户转移调整
          eta <- matrix(nrow = length(rho), ncol = length(rho))
          tau <- matrix(nrow = length(rho), ncol = length(rho))
          E_tau <- matrix(nrow = length(rho), ncol = length(rho))
          eta_rsum <- double(length(rho))
          tau_rsum <- double(length(rho)) 
          E_star <- integer()
          
          for (r in c(1 : length(rho))) {
            for (s in c(1 : length(rho))) {
              if (r != s){
                #eta[r, s] = log(max(1, (P[t-1, r] - P[t-1, s]))) * log(1 + 1/(abs(rho[s]-rho[r] + 0.000000001)))
                eta[r, s] = max(1, (P[t-1, r] - P[t-1, s]))*max(0.01, rho[s])
              }else{
                eta[r, s] = 0
              }
              eta_rsum[r] = sum(eta[r, ])
            }
          }
          
          for (r in c(1 : length(rho))) {
            for (s in c(1 : length(rho))) {
              if(eta_rsum[r] != 0 ){
                tau[r, s] = (1 - exp(-k9 * eta_rsum[r])) * eta[r, s] / eta_rsum[r]
              }else{
                tau[r, s] = 0  ## eta_rsum[r] = 0 时，该怎么设定，此处规定它为0
                
              }
            }
            tau_rsum[r] = sum(tau[r, ])
          }
          
          for(i in c(1 : length(rho))){
            for (r in c(1 : length(rho))) {
              E_tau[i, r] = as.double(E[t-1, r] * tau[r, i])
            }
            E_star[i] = E[t-1, i] * (1 - tau_rsum[i]) + sum(E_tau[i, ])
            rho[i] = E_star[i] / E_sum[t-1]
          } 
          
          ## 被接管调整
          
          rho_sum_exit <- 0 ## 被接管的公司在t-1期的市场份额之和
          
          if(is.numeric(n_exit)){
            for(i in n_exit){                  
              rho_sum_exit = rho_sum_exit + rho[i]   ## 被释放的市场份额之和
              rho[i] = 0  ## 被接管公司的下一期市场份额，设为0
            }
          }
          
          for(i in c(1 : length(rho))){
            rho[i] = rho[i]/(1 - rho_sum_exit)
          }
          
          rho = rho/sum(rho)
          ############################################################################
          ##  公司经营部分
          ## 分组
          
          group <- rep(1, length(rho))  ## 组别重置
          
          if(is.numeric(n_exit)){ 
            for(i in c((length(rho)-length(n_exit)):2)){
              d1 <- diff(rho[order(rho)])[length(n_exit):(length(rho)-1)]
              d2 <- (rho[order(rho)])[(length(n_exit)+1):length(rho)]
              d <- d1/d2
              if (d[i-1]> rho_m) {
                group[i+length(n_exit)-1] = group[i+length(n_exit)] + 1
              }else{
                group[i+length(n_exit)-1] = group[i+length(n_exit)]
              }
            }
          }else{
            for(i in c((length(rho)):2)){
              d1 <- diff(rho[order(rho)])
              d2 <- rho[order(rho)][2:length(rho)]
              d <- d1/d2
              if (d[i-1] > rho_m){
                group[i-1] = group[i] + 1
              }else{
                group[i-1] = group[i]
              }
            }
          }
          
          group <- group[rank(rho)] 
          
          for(i in 1:length(rho)){
            if (rho[i] == 0){
              group[i] = 100
            }
          }   ## 最终组别，将被接管的公司组别设为100
          
          sum_j <- rep(0, 30)
          
          for (i in setdiff(unique(group), 100)) {
            j = which(group == i)
            for (x in j){
              sum_j[i] = sum_j[i] + beta[t-1, x]
            }
            avg[i] = sum_j[i] / length(j)
          }
          
          ## 下一期财险市场总风险单位数的确定
          ##  自然增长率，与市场最大承载量和当前市场风险单位数的差值正相关
          ## 考虑到保户的恐慌程度，变动率与当期被接管公司释放出来的市场份额负相关
          ## 巨灾风险的影响存在长期效应，持续三年，负相关
          
          LR_sum_basic = mean(LR_basic[which(LR_basic != 0 )])
          E_sum[t]= E_sum[t-1] * g[t]
          
          ## 计算当期期初总资本
          equity_sum[t] = sum(Equity[t-1,])
          
          ## 计算所有正常经营的保险公司经营情况
          
          ##  正常经营中----t期有公司被接管
          if(is.numeric(n_exit)){          
            for ( i in c(1:length(rho))[- n_exit]) {
              
              K_begin[t, i] = K[t-1, i] ## 正常经营公司的期初资产为前一期的期末资产
              ## 对于基准赔付率的修改
              E[t, i] = min( E_sum[t] * rho[i], E[t-1, i]*1.2)
              L[t, i] = rpois(1, E[t, i]) * rgamma(1, shape = p_basic/2  , scale = 2) #+ shock_und* shock
              P_exp[t, i] = P_p[t-1, i]   
              P_p[t, i] = P_exp[t, i] *min(1.05,exp(k1 * (LR[t-1, i] - LR_basic[i])))
              if(shock_und == 1 & cat_per == t){  ## 这里判断是否会发生风险冲击，发生的话赔付比例120%
                L[t, i] = shock_und * E[t, i] * P_p[t, i]
              }
              LR[t, i] = L[t, i] / E[t, i] / P_p[t, i]
              gamma[t] = 1 - exp(-LR_sum[t])
              beta[t, i] = (1 - k3) * beta[t-1, i]*(beta[t-1, i] / avg[group[i]]) ^ -(gamma[t]) + k3 * beta[t-1, i]
              #beta[t, i] = (1 - k3) * avg[group[i]] *(beta[t-1, i] / avg[group[i]]) ^ (-gamma[t]) + k3 * beta[t-1, i]
              P_l[t, i] = beta[t, i] * P_p[t, i]  
              P[t, i] = P_p[t, i] + P_l[t, i]
              invest[t, i] = (K_begin[t, i] + E[t, i] * P_p[t, i] - E[t-1, i] * P_p[t-1, i])
              
              alpha = c(1, 0 ,0,0,0,0,0,0)
              tryCatch( {alpha  = solnl(x0, objfun = fr, confun=confun2, lb =c(0,0,0, 0,0,0,0,0), ub = c(1,1,1,1,1,1,1,1),
                                        tolX = 1e-01,
                                        tolFun = 1e-02, tolCon = 1e-01, maxnFun = 1e+02, maxIter = 400)$par},
                        error=function(e){
                          ## 报错时执行的操作(此处留空表示静默跳过)
                          #cat("ERROR :",t, i, conditionMessage(e), "\n")
                          invisible(NULL)
                        })
              
              
              
              deposit[t, i] = alpha[1] * invest[t, i] * (1 + rate_deposit[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司的定期存款,矩阵结构             
              financial_debt[t, i] = alpha[2] * invest[t, i] * (1 + rate_financial[t]) ### 每个公司金融债,矩阵结构     
              company_debt[t, i] = alpha[3] * invest[t, i] * (1 + rate_company[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司企业债,矩阵结构
              ten_year_national_debt[t, i] = alpha[4] * invest[t, i] * (1 + rate_ten[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司10年期国债,矩阵结构
              twenty_year_national_debt[t, i] = alpha[5] * invest[t, i] * (1 + rate_twenty[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司二十年期国债,矩阵结构
              stock[t, i] = alpha[6] * invest[t, i] * (1 + rate_stock[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司股票,矩阵结构              
              real_estate[t, i] = alpha[7] * invest[t, i] * (1 + rate_real_estate[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司房地产,矩阵结构          
              foreign_assets[t, i] = alpha[8] * invest[t, i] * (1 + rate_foreign[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司国外资产,矩阵结构
              ## 流动性直接算  不用折算
              
            }
          }
          
          ## 正常经营中------t期没有公司被接管
          
          if(is.null(n_exit)){         
            for (i in c(1:length(rho))) {  
              #shock = cat_per[t, normal] * shock_und * equity_sum[t] * rho[i]
              #shock = K_sum[t-1] * rho[i]
              K_begin[t, i] = K[t-1, i] ## 正常经营公司的期初资产为前一期的期末资产
              ## 对于基准赔付率的修改
              E[t, i] = min( E_sum[t] * rho[i], E[t-1, i]*1.2)
              L[t, i] = rpois(1, E[t, i]) * rgamma(1, shape = p_basic /2 , scale = 2)
              P_exp[t, i] = P_p[t-1, i]  
              P_p[t, i] = P_exp[t, i] * min(1.05,exp(k1 * (LR[t-1, i] - LR_basic[i])))
              if(shock_und != 0 & cat_per == t){  ## 这里判断是否会发生风险冲击，发生的话赔付比例120%
                L[t, i] = shock_und * E[t, i] * P_p[t, i]
              }
              LR[t, i] = L[t, i] / E[t, i] / P_p[t, i]
              gamma[t] = 1 - exp(-LR_sum[t])
              beta[t, i] = (1 - k3) * beta[t-1, i]  *(beta[t-1, i] / avg[group[i]]) ^ (-gamma[t]) + k3 * beta[t-1, i]
              # beta[t, i] = (1 - k3) * avg[group[i]] *(beta[t-1, i] / avg[group[i]]) ^ (-gamma[t]) + k3 * beta[t-1, i]
              P_l[t, i] = beta[t, i] * P_p[t, i]  
              P[t, i] = P_p[t, i] + P_l[t, i] 
              invest[t, i] = (K_begin[t, i] + E[t, i] * P_p[t, i] - E[t-1, i] * P_p[t-1, i])
              alpha = c(1, 0 ,0,0,0,0,0,0)
              tryCatch( {alpha  = solnl(x0, objfun = fr, confun=confun2, lb =c(0,0,0,0,0,0,0,0), ub = c(1,1,1,1,1,1,1,1),
                                        tolX = 1e-02,
                                        tolFun = 1e-03, tolCon = 1e-02, maxnFun = 1e+03, maxIter = 400)$par},
                        error=function(e){
                          ## 报错时执行的操作(此处留空表示静默跳过)
                          #cat("ERROR :",t, i, conditionMessage(e), "\n")
                          invisible(NULL)
                        })
              deposit[t, i] = alpha[1] * invest[t, i] * (1 + rate_deposit[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司的定期存款,矩阵结构             
              financial_debt[t, i] = alpha[2] * invest[t, i] * (1 + rate_financial[t]) ### 每个公司金融债,矩阵结构     
              company_debt[t, i] = alpha[3] * invest[t, i] * (1 + rate_company[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司企业债,矩阵结构
              ten_year_national_debt[t, i] = alpha[4] * invest[t, i] * (1 + rate_ten[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司10年期国债,矩阵结构
              twenty_year_national_debt[t, i] = alpha[5] * invest[t, i] * (1 + rate_twenty[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司二十年期国债,矩阵结构
              stock[t, i] = alpha[6] * invest[t, i] * (1 + rate_stock[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司股票,矩阵结构              
              real_estate[t, i] = alpha[7] * invest[t, i] * (1 + rate_real_estate[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司房地产,矩阵结构          
              foreign_assets[t, i] = alpha[8] * invest[t, i] * (1 + rate_foreign[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司国外资产,矩阵结构
              ## 流动性直接算  不用折算
            }
          }
          
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
          
          ### 减价出售机制起点  
          ## 储存减价出售之前的无风险资产和风险资产规模
          cash[t, ] = deposit[t, ] +  ten_year_national_debt[t, ] + twenty_year_national_debt[t, ] 
          
          liq_gap[t, ] = 0
          liq_tag = which( cash[t, ] > 0 & L[t, ] > cash[t, ])
          liq_gap[t, liq_tag] = L[t, liq_tag] - cash[t, liq_tag]
          cash[t, liq_tag] = cash[t, liq_tag] + liq_gap[t, liq_tag]
          
          
          # 
          # for (i in 1:length(rho)) {
          #   if(cash[t, i] > 0 & L[t, i] > cash[t, i]){
          #      ## 流动性缺口规模
          #     ## 为了公司进行业务赔付，保险公司需要进行第一批次的减价出售，出售股权投资，50%的折价
          #     ## 如果存在流动性缺口，那么就需要进行减价出售机制
          #     ## 如果没有缺口那就不进行减价出售
          #    cash[t, i] = cash[t, i] + liq_gap[t, i]
          #   }
          # }
          # 
          
          ## 减价出售机制设定
          sum_lip = sum(liq_gap[t, ])
          #FV_risk[t, ] = 0
          FV_risk[t, ] = financial_debt[t, ] + company_debt[t, ] + stock[t, ] + real_estate[t, ] + foreign_assets[t, ]
          sum_risk = sum(FV_risk[t, ])
          pp = 0
          p = 1
          
          while (round(sum_lip) > 0 & p > 0.5 ) {
            # 先计算出售的价格
            #sum_v = sum_lip/p
            p_n = p * exp( - k10 * sum_lip / p / sum_risk)  ## 价格的下降幅度
            FV_risk[t, ] = (FV_risk[t, ] / p - liq_gap[t, ]/p)*p_n
            liq_gap[t, ] = (p - p_n) * liq_gap[t, ]/p
            sum_lip = sum(liq_gap[t, ])
            sum_risk = sum(FV_risk[t, ])
            pp = pp + 1   ## 减价出售轮数
            p = p_n
          }   
          
          price_decline[t] = p
          w[t] = pp
          
          n_exit <- c()
          for(i in 1:length(rho)){
            fire_sum = max(1, sum(financial_debt[t, i] + company_debt[t, i] + stock[t, i] +real_estate[t, i]+ foreign_assets[t, i]))
            financial_debt[t, i] = FV_risk[t, i] * financial_debt[t, i]/fire_sum
            company_debt[t, i] = FV_risk[t, i] * company_debt[t, i]/fire_sum
            stock[t, i] = FV_risk[t, i] * stock[t, i]/fire_sum
            real_estate[t, i] = FV_risk[t, i] * real_estate[t, i]/fire_sum
            foreign_assets[t, i] = FV_risk[t, i] * foreign_assets[t, i]/fire_sum
            FV_final[t, i] = FV_risk[t, i] + cash[t, i]
            
            # 经营结果
            pi[t, i] = FV_final[t, i]  - invest[t, i] + #投资利润
              E[t, i] * P_p[t, i] - L[t, i] ## 承保利润  ## 现阶段的利润
            ## 算是否需要进行偿付能力调整
            Equity[t, i] = Equity[t-1, i] + pi[t, i] ## 经营结束之后的权益量
            
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
            
            s = Equity[t, i] / mc_zuidi[t, i]
            tt = NA
            # 如果偿付能力达标，并且当期利润为正的话就会进行分红  ## 暂时不设分红
            if(s < sl_ratio | E[t, i] == 0){
              #   ##  对于不满足偿付能力要求的公司来讲
              n_exit <- append(n_exit, i)
            }else{
              D2 = (Equity[t-1, i] - sl_ratio * mc_zuidi[t, i] + pi[t, i]) * k4
              if(pi[t, i] > 0 ){  ## 如果当期的收益值大于0
                D1 = pi[t, i] * k4
                D[t, i] =  min(D1, D2)
                Equity[t, i] = Equity[t, i] - D[t, i]
              }
            }
            
            n_exit = unique(n_exit)
            pi_uw[t, i] = E[t, i] * P_p[t, i] - L[t, i]   ###  单期的承保利润
            pi_iv[t, i] = FV_final[t, i] - invest[t, i]
            
            # ## 判断是否需要进行分红
            # if (pi[t, i] > 0 ){
            #   D[t, i] = k4 * pi[t, i]
            # }else{
            #   D[t, i] = 0
            # }  
            ## 期末总资产
            K[t, i] = invest[t, i] + pi[t, i] - D[t, i] 
            ## 对于被接管公司来讲，期末总资产就为期初总资产
            if(E[t, i] == 0){
              K[t, i] = K_begin[t, i]
            }
            ## 所有者权益变动,保险公司按照杠杆率进行总资本的补充
            #Equity[t, i] = leverage[i] * K0[i] + pi[t, i] - D[t, i]
            rho_indentity[t, i] = rho[i]
          }
          
          
          ## 计算市场利润率,计算t+1期新公司数量n_new[t]------------------------------NEW
          ## 期初净资产
          K_begin_sum[t] =  sum(invest[t,]) ## 保险市场总资产 期初值###########################################################################
          if(is.null(n_exit) == F){
            K_begin_sum[t] = K_begin_sum[t] + sum(K_begin[t, n_exit])
          }
          K_sum[t] =  sum(K[t, ]) ## 保险市场总资产（期末值）
          pi_sum[t] = sum(pi[t,]) ## 总利润
          ## 这里的市场收益率并没有包含被接管公司的总资产
          Return[t] = 2 * as.double(pi_sum[t] / (K_begin_sum[t] + K_sum[t]))
          
          ## 新公司数量
          if (Return[t - 1] > k5[t-1] && Return[t] > k5[t]) {
            u_1 = Return[t-1] - k5[t-1]
            u_2 = Return[t] - k5[t]
            u = u_1 +u_2
            n_new[t] = as.integer(k6 * u)
          }else{
            n_new[t] = 0
          }
          
          ## 新公司的数量最大为n_new_max
          ## 所以在计算出来的n_new[t]和n_new_max中取最小值
          n_new[t] = min(n_new[t], n_new_max)  
          
          
          if (length(n_exit) == 0) {  ## 如果下一期没有被接管公司,上述n_exit就是integer(0), 这时要把它设为NULL
            n_exit <- c()
          }   ## n_exit为下一期处于接管状态的公司索引
          
          
          loss[t] = sum(L[t, which(is.na(L[t, ]) != T)])
          
          ## 计算当期LR_sum[t+1],用来计算下一期的k2
          
          for(i in c(1 : length(rho))){
            L_sum[t] = as.double(L_sum[t] + L[t, i])
            P_p_sum[t] = as.double(P_p_sum[t]+P_p[t, i]* E[t, i])
            LR_sum[t+1] = as.double(L_sum[t] / P_p_sum[t])  
          }
          
          len_rho[t] = length(rho)
          
          ## 当期平均beta值
          beta_avg = c(beta_avg, as.numeric(quantile(beta[t,][which(E[t,] != 0)], 0.75, na.rm = T)))
          
          ## 第二期经营结束
          
          #####################################
          ## 第三期经营的步骤 ####################
          
          t = 3
          
          ###### rho值计算部分  ################################
          ## 保户转移调整
          
          eta <- matrix(nrow = length(rho), ncol = length(rho))
          tau <- matrix(nrow = length(rho), ncol = length(rho))
          E_tau <- matrix(nrow = length(rho), ncol = length(rho))
          eta_rsum <- double(length(rho))
          tau_rsum <- double(length(rho)) 
          E_star <- double()
          
          for (r in c(1 : length(rho))) {
            for (s in c(1 : length(rho))) {
              if (r != s){
                #eta[r, s] = log(max(1, (P[t-1, r] - P[t-1, s]))) * log(1 + 1/(abs(rho[s]-rho[r] + 0.000000001)))
                eta[r, s] = max(1, (P[t-1, r] - P[t-1, s]))*max(0.01, rho[s])
              }else{
                eta[r, s] = 0
              }
              eta_rsum[r] = sum(eta[r, ])
            }
          }
          
          for (r in c(1 : length(rho))) {
            for (s in c(1 : length(rho))) {
              if(eta_rsum[r] != 0 ){
                tau[r, s] = (1 - exp(-k9 * eta_rsum[r])) * eta[r, s] / eta_rsum[r]
              }else{
                tau[r, s] = 0
              }
            }
            tau_rsum[r] = sum(tau[r, ])
          }
          
          for(i in c(1 : length(rho))){
            for (r in c(1 : length(rho))) {
              E_tau[i, r] = as.double(E[t-1, r] * tau[r, i])
              E_star[i] = as.double(E[t-1, i] * (1 - tau_rsum[i]) + sum(E_tau[i, ]))
            }
          } 
          
          sum_star = sum(E_star)
          
          for(i in c(1:length(rho))){
            rho[i] = E_star[i]/sum_star
          }
          
          ## 被接管调整
          
          rho_sum_exit <- 0
          
          if(is.null(n_exit) == FALSE){
            for(i in n_exit){                  ## 被接管公司的下一期市场份额，设为0
              rho_sum_exit = rho_sum_exit + rho[i]   ## 被释放的市场份额
              rho[i] = 0
            }
          }
          
          for(i in c(1 : length(rho))){
            rho[i] = rho[i]/(1 - rho_sum_exit)
          } 
          
          
          ## 新公司进入调整-----------------------------------------NEW
          ## 如果有新公司进入，新公司的市场份额为当期保险公司市场份额的下四分位数
          ## 下一期公司数量N = length(rho) - length(n_exit) + n_new[t]
          
          quant_rho = as.numeric(quantile(rho[which(rho != 0)], 0.25))
          if(n_new[t-1] != 0) {
            rho_n = quant_rho ## 下一期中，新进入公司单个的市场份额
            rho <- c(rho, rep(rho_n, n_new[t-1]))
            rho = rho / sum(rho)
          } 
          
          #########################################################################
          ##  公司经营部分
          
          ## 分组
          group<- rep(1, length(rho))  ## 组别重置
          
          if(is.numeric(n_exit)){ 
            for(i in c((length(rho)-length(n_exit)):2)){
              d1 <- diff(rho[order(rho)])[length(n_exit):(length(rho)-1)]
              d2 <- (rho[order(rho)])[(length(n_exit)+1):length(rho)]
              d <- d1/d2
              if (is.na(d[i-1]) == F && d[i-1] > rho_m) {
                group[i+length(n_exit)-1] = group[i+length(n_exit)] + 1
              }else{
                group[i+length(n_exit)-1] = group[i+length(n_exit)]
              }
            }
          }else{
            for(i in c((length(rho)):2)){
              d1 <- diff(rho[order(rho)])
              d2 <- rho[2:length(rho)]
              d <- d1/d2
              if (is.na(d[i-1]) == F && d[i-1] > rho_m){
                group[i-1] = group[i] + 1
              }else{
                group[i-1] = group[i]
              }
            }
          }
          
          group <- group[rank(rho)]  ## 最终组别
          
          for(i in 1:length(rho)){## 将被接管的公司组别设为100,
            if (rho[i] == 0){
              group[i] = 100
            }
          }
          
          ## 这里将新公司和被接管公司beta值引入，这样做是为了方便计算组内平均费率水平，实际上他们不参与计算
          
          if (n_new[t-1] !=  0){
            for (i in length(rho) -c(1:0)) {
              beta[t-1, i] = beta_avg[t-1]
            }
          }
          
          sum_j <- rep(0, 30)
          for (i in setdiff(unique(group), 100)) {
            j = which(group == i)
            for (x in j){
              sum_j[i] = sum_j[i] + beta[t-1, x]
            }
            avg[i] = sum_j[i] / length(j)
          }
          
          ## 保单增长率的设定
          
          LR_sum_basic = mean(LR_basic[which(LR_basic != 0 )])
          E_sum[t]=E_sum[t-1] * g[t]
          
          ## 计算当期期初总资本
          equity_sum[t] = sum(Equity[t-1,])
          
          ## 计算所有正常经营的保险公司经营情况
          
          ## 正常经营中----t期有公司被接管
          if(is.numeric(n_exit)){
            for (i in c(1:(length(rho)-n_new[t-1]))[-n_exit]){
              #shock = cat_per[t, normal] * shock_und * equity_sum[t] * rho[i]
              #shock = K_sum[t-1] * rho[i]
              K_begin[t, i] = K[t-1, i]## 正常经营公司的期初资产为前一期的期末资产
              ## 对于基准赔付率的修改
              LR_basic[i] = LR_basic[i]
              
              E[t, i] = min( E_sum[t] * rho[i], E[t-1, i]*1.5)
              
              L[t, i] = rpois(1, E[t, i]) * rgamma(1, shape = LR_basic[i]* p_basic /2 , scale = 2) 
              P_exp[t, i] = P_p[t-1, i]   
              P_p[t, i] = P_exp[t, i] * min(1.05,exp(k1 * (LR[t-1, i] - LR_basic[i])))
              if(shock_und != 0 & cat_per == t){  ## 这里判断是否会发生风险冲击，发生的话赔付比例120%
                L[t, i] = shock_und * E[t, i] * P_p[t, i]
              }
              LR[t, i] = L[t, i] / E[t, i] / P_p[t, i]
              gamma[t] = 1 - exp(-LR_sum[t])
              beta[t, i] = (1 - k3) * beta[t-1, i]*(beta[t-1, i] / avg[group[i]]) ^ -(gamma[t]) + k3 * beta[t-1, i]
              #beta[t, i] = (1 - k3) * avg[group[i]] *(beta[t-1, i] / avg[group[i]]) ^ (-gamma[t]) + k3 * beta[t-1, i]
              P_l[t, i] = beta[t, i] * P_p[t, i]  
              P[t, i] = P_p[t, i] + P_l[t, i]
              LR[t, i] = L[t, i] / P_p[t, i] / E[t, i]
              invest[t, i] = (K_begin[t, i] + E[t, i] * P_p[t, i] - E[t-1, i] * P_p[t-1, i] )
              alpha = c(1, 0 ,0,0,0,0,0,0)
              tryCatch( {alpha  = solnl(x0, objfun = fr, confun=confun2, lb =c(0,0,0,0,0,0,0,0), ub = c(1,1,1,1,1,1,1,1),
                                        tolX = 1e-02,
                                        tolFun = 1e-03, tolCon = 1e-02, maxnFun = 1e+03, maxIter = 400)$par},
                        error=function(e){
                          ## 报错时执行的操作(此处留空表示静默跳过)
                          #cat("ERROR :",t, i, conditionMessage(e), "\n")
                          invisible(NULL)
                        })
              
              
              deposit[t, i] = alpha[1] * invest[t, i] * (1 + rate_deposit[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司的定期存款,矩阵结构             
              financial_debt[t, i] = alpha[2] * invest[t, i] * (1 + rate_financial[t]) ### 每个公司金融债,矩阵结构     
              company_debt[t, i] = alpha[3] * invest[t, i] * (1 + rate_company[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司企业债,矩阵结构
              ten_year_national_debt[t, i] = alpha[4] * invest[t, i] * (1 + rate_ten[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司10年期国债,矩阵结构
              twenty_year_national_debt[t, i] = alpha[5] * invest[t, i] * (1 + rate_twenty[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司二十年期国债,矩阵结构
              stock[t, i] = alpha[6] * invest[t, i] * (1 + rate_stock[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司股票,矩阵结构              
              real_estate[t, i] = alpha[7] * invest[t, i] * (1 + rate_real_estate[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司房地产,矩阵结构          
              foreign_assets[t, i] = alpha[8] * invest[t, i] * (1 + rate_foreign[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司国外资产,矩阵结构
              ## 流动性直接算  不用折算
            } 
          }
          
          ## 正常经营中------t期没有公司被接管
          if(is.numeric(n_exit) == FALSE){
            for ( i in c(1:(length(rho)-n_new[t-1]))) {
              
              K_begin[t, i] = K[t-1, i] ## 正常经营公司的期初资产为前一期的期末资产
              ## 对于基准赔付率的修改
              LR_basic[i] = LR_basic[i] #+ 10*max(rho[i]- rho_indentity[t-1, i], 0) * (LR_sum_basic - LR_basic[i])
              
              E[t, i] =min( E_sum[t] * rho[i], E[t-1, i]*1.5)
              
              L[t, i] = rpois(1, E[t, i]) * rgamma(1, shape = p_basic /2 , scale = 2) 
              P_exp[t, i] = P_p[t-1, i]   
              P_p[t, i] = P_exp[t, i] * min(1.05,exp(k1 * (LR[t-1, i] - LR_basic[i])))
              if(shock_und != 0 & cat_per == t){  ## 这里判断是否会发生风险冲击，发生的话赔付比例120%
                L[t, i] = shock_und * E[t, i] * P_p[t, i]
              }
              gamma[t] = 1 - exp(-LR_sum[t])
              beta[t, i] = (1 - k3) * beta[t-1, i]*(beta[t-1, i] / avg[group[i]]) ^ -(gamma[t]) + k3 * beta[t-1, i]
              #beta[t, i] = (1 - k3) * avg[group[i]] *(beta[t-1, i] / avg[group[i]]) ^ (-gamma[t]) + k3 * beta[t-1, i]
              P_l[t, i] = beta[t, i] * P_p[t, i]  
              P[t, i] = P_p[t, i] + P_l[t, i]
              LR[t, i] = L[t, i] / P_p[t, i] / E[t, i]
              invest[t, i] = (K_begin[t, i] + E[t, i] * P_p[t, i] - E[t-1, i] * P_p[t-1, i] ) 
              alpha = c(1, 0 ,0,0,0,0,0,0)
              tryCatch( {alpha  = solnl(x0, objfun = fr, confun=confun2, lb =c(0,0,0,0,0,0,0,0), ub = c(1,1,1,1,1,1,1,1),
                                        tolX = 1e-02,
                                        tolFun = 1e-03, tolCon = 1e-02, maxnFun = 1e+03, maxIter = 400)$par},
                        error=function(e){
                          ## 报错时执行的操作(此处留空表示静默跳过)
                          #cat("ERROR :",t, i, conditionMessage(e), "\n")
                          invisible(NULL)
                        })
              
              
              
              deposit[t, i] = alpha[1] * invest[t, i] * (1 + rate_deposit[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司的定期存款,矩阵结构             
              financial_debt[t, i] = alpha[2] * invest[t, i] * (1 + rate_financial[t]) ### 每个公司金融债,矩阵结构     
              company_debt[t, i] = alpha[3] * invest[t, i] * (1 + rate_company[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司企业债,矩阵结构
              ten_year_national_debt[t, i] = alpha[4] * invest[t, i] * (1 + rate_ten[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司10年期国债,矩阵结构
              twenty_year_national_debt[t, i] = alpha[5] * invest[t, i] * (1 + rate_twenty[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司二十年期国债,矩阵结构
              stock[t, i] = alpha[6] * invest[t, i] * (1 + rate_stock[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司股票,矩阵结构              
              real_estate[t, i] = alpha[7] * invest[t, i] * (1 + rate_real_estate[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司房地产,矩阵结构          
              foreign_assets[t, i] = alpha[8] * invest[t, i] * (1 + rate_foreign[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司国外资产,矩阵结构
              ## 流动性直接算  不用折算
            } 
          }
          
          
          ## 所有新加入的保险公司经营情况
          if (n_new[t-1] != 0){
            tag_normal = which(E[t-1,] != 0) ## 上一期经营的公司索引
            for (i in c((length(rho)- n_new[t-1] + 1) : length(rho))){
              #leverage[i] = quantile(leverage[tag_normal], 0.25) ## 这里要多计算一个新公司杠杆率
              K_begin[t, i] = quantile(K[t-1,][tag_normal], 0.25) ## 新公司的期初总资产是前一期经营公司总资产的下四分位数
              Equity[t-1, i] = quantile(Equity0, 0.25) #(leverage[i] * K_begin[t, i]
              E[t, i] = E_sum[t]  * rho[i]
              L[t, i] = rpois(1, E[t, i]) * rgamma(1, shape = LR_basic[i] * p_basic /2 , scale = 2) 
              P_exp[t, i] = p_basic  #ll[i]
              LR_basic[i] = quantile(LR_basic[tag_normal], 0.75) ## 基准赔付率为前一期基准赔付率的下四分位数
              P_p[t, i] = P_exp[t, i]   ## 纯保费基于市场状况调整
              if(shock_und != 0 & cat_per == t){  ## 这里判断是否会发生风险冲击，发生的话赔付比例120%
                L[t, i] = shock_und * E[t, i] * P_p[t, i]
              }
              LR[t, i] = L[t, i] / P_p[t, i] / E[t, i]
              gamma[t] = 1 - exp(-LR_sum[t] )
              #beta[t, i] = beta[t-1, i]*(beta[t-1, i] / avg[group[i]]) ^ -(gamma[t])
              beta[t, i] = quantile(beta[t-1,][tag_normal], 0.75) ## beta值为前一期下四分位数
              P_l[t, i] = beta[t, i] * P_p[t, i]
              P[t, i] = P_p[t, i] + P_l[t, i]
              invest[t, i] = K_begin[t, i] 
              
              alpha = c(1, 0 ,0,0,0,0,0,0)
              tryCatch( {alpha  = solnl(x0, objfun = fr, confun=confun2, lb =c(0,0,0,0,0,0,0,0), ub = c(1,1,1,1,1,1,1,1),
                                        tolX = 1e-02,
                                        tolFun = 1e-03, tolCon = 1e-02, maxnFun = 1e+03, maxIter = 400)$par},
                        error=function(e){
                          ## 报错时执行的操作(此处留空表示静默跳过)
                          #cat("ERROR :",t, i, conditionMessage(e), "\n")
                          invisible(NULL)
                        })
              
              
              deposit[t, i] = alpha[1] * invest[t, i] * (1 + rate_deposit[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司的定期存款,矩阵结构             
              financial_debt[t, i] = alpha[2] * invest[t, i] * (1 + rate_financial[t]) ### 每个公司金融债,矩阵结构     
              company_debt[t, i] = alpha[3] * invest[t, i] * (1 + rate_company[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司企业债,矩阵结构
              ten_year_national_debt[t, i] = alpha[4] * invest[t, i] * (1 + rate_ten[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司10年期国债,矩阵结构
              twenty_year_national_debt[t, i] = alpha[5] * invest[t, i] * (1 + rate_twenty[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司二十年期国债,矩阵结构
              stock[t, i] = alpha[6] * invest[t, i] * (1 + rate_stock[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司股票,矩阵结构              
              real_estate[t, i] = alpha[7] * invest[t, i] * (1 + rate_real_estate[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司房地产,矩阵结构          
              foreign_assets[t, i] = alpha[8] * invest[t, i] * (1 + rate_foreign[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司国外资产,矩阵结构
              ## 流动性直接算  不用折算
              
            }
          }
          
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
              deposit[t, i] = 1
              financial_debt[t, i] = 1
              company_debt[t, i] = 1
              ten_year_national_debt[t, i] = 1
              twenty_year_national_debt[t, i] = 1
              stock[t, i] = 1
              real_estate[t, i] = 1
              foreign_assets[t, i] = 1
            }
          }  
          
          #FV_free_before[t, ]= FV_free[t, ]
          #FV_risk_before[t, ]= FV_risk[t, ]
          
          cash[t, ] = deposit[t, ] +  ten_year_national_debt[t, ] + twenty_year_national_debt[t, ] 
          
          for (i in 1:length(rho)) {
            if(cash[t, i] > 0 & L[t, i] > cash[t, i]){
              liq_gap[t, i] = L[t, i] - cash[t, i] ## 流动性缺口规模
              ## 为了公司进行业务赔付，保险公司需要进行第一批次的减价出售，出售股权投资，50%的折价
              ## 如果存在流动性缺口，那么就需要进行减价出售机制
              ## 如果没有缺口那就不进行减价出售
              cash[t, i] = cash[t, i] + liq_gap[t, i]
            }
          }
          ## 减价出售机制设定
          sum_lip = sum(liq_gap[t, ])
          #FV_risk[t, ] = 0
          FV_risk[t, ] = financial_debt[t, ] + company_debt[t, ] + stock[t, ] + real_estate[t, ] + foreign_assets[t, ]
          sum_risk = sum(FV_risk[t, ])
          pp = 0
          p = 1
          while (round(sum_lip) > 0 & p > 0.5 ) {
            # 先计算出售的价格
            #sum_v = sum_lip/p
            p_n = p * exp( - k10 * sum_lip / p / sum_risk)  ## 价格的下降幅度
            FV_risk[t, ] = (FV_risk[t, ] / p - liq_gap[t, ]/p)*p_n
            liq_gap[t, ] = (p - p_n) * liq_gap[t, ]/p
            sum_lip = sum(liq_gap[t, ])
            sum_risk = sum(FV_risk[t, ])
            pp = pp + 1   ## 减价出售轮数
            p = p_n
          }   
          price_decline[t] = p
          w[t] = pp
          
          
          n_exit <- c()
          for(i in 1:length(rho)){
            fire_sum = max(1, sum(financial_debt[t, i] + company_debt[t, i] + stock[t, i] +real_estate[t, i]+ foreign_assets[t, i]))
            financial_debt[t, i] = FV_risk[t, i] * financial_debt[t, i]/fire_sum
            company_debt[t, i] = FV_risk[t, i] * company_debt[t, i]/fire_sum
            stock[t, i] = FV_risk[t, i] * stock[t, i]/fire_sum
            real_estate[t, i] = FV_risk[t, i] * real_estate[t, i]/fire_sum
            foreign_assets[t, i] = FV_risk[t, i] * foreign_assets[t, i]/fire_sum
            FV_final[t, i] = FV_risk[t, i] + cash[t, i]
            
            # 经营结果
            pi[t, i] = FV_final[t, i]  - invest[t, i] + #投资利润
              E[t, i] * P_p[t, i] - L[t, i] ## 承保利润  ## 现阶段的利润
            ## 算是否需要进行偿付能力调整
            Equity[t, i] = Equity[t-1, i] + pi[t, i]   ## 经营结束之后的权益量
            
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
            
            
            s = Equity[t, i] / mc_zuidi[t, i]
            tt = NA
            ## 如果偿付能力达标，并且当期利润为正的话就会进行分红  ## 暂时不设分红
            if(s < sl_ratio | E[t, i] == 0){
              ## 当偿付能力不达标的情况下
              ## 资产状况可以调整
              ##  对于不满足偿付能力要求的公司来讲
              n_exit <- append(n_exit, i)
              
            } else if(s  == Inf){
              n_exit <- append(n_exit, i)
            } else{
              D2 = (Equity[t-1, i] - sl_ratio * mc_zuidi[t, i] + pi[t, i]) * k4
              if(pi[t, i] > 0 ){  ## 如果当期的收益值大于0
                D1 = pi[t, i] * k4
                D[t, i] =  min(D1, D2)
                Equity[t, i] = Equity[t, i] - D[t, i]              }
            }
            
            n_exit = unique(n_exit)
            pi_uw[t, i] = E[t, i] * P_p[t, i] - L[t, i]   ###  单期的承保利润
            pi_iv[t, i] = FV_final[t, i] - invest[t, i]
            
            ## 期末总资产
            K[t, i] = invest[t, i] + pi[t, i] - D[t, i] 
            ## 对于被接管公司来讲，期末总资产就为期初总资产
            if(E[t, i] == 0){
              K[t, i] = K_begin[t, i]
            }
            ## 所有者权益变动,保险公司按照杠杆率进行总资本的补充
            #Equity[t, i] = leverage[i] * K0[i] + pi[t, i] - D[t, i]
            rho_indentity[t, i] = rho[i]
          }
          ## 计算市场利润率,计算t+1期新公司数量n_new[t]------------------------------NEW
          ## 期初净资产
          K_begin_sum[t] =  sum(invest[t,]) ## 保险市场总资产 期初值
          if(is.null(n_exit) == F){
            K_begin_sum[t] = K_begin_sum[t] + sum(K_begin[t, n_exit])
          }
          K_sum[t] =  sum(K[t,]) ## 保险市场总资产（期末值）
          pi_sum[t] = sum(pi[t,]) ## 总利润
          ## 这里的市场收益率并没有包含被接管公司的总资产
          Return[t] = 2 * as.double(pi_sum[t] / (K_begin_sum[t] + K_sum[t]))
          
          ## 新公司数量
          if (Return[t - 1] > k5[t-1] && Return[t] > k5[t]) {
            u_1 = Return[t-1] - k5[t-1]
            u_2 = Return[t] - k5[t]
            u = u_1 +u_2
            n_new[t] = as.integer(k6 * u)
          }else{
            n_new[t] = 0
          }
          
          if (Return[t-1] > k5[t-1] && Return[t] > k5[t]) {
            u_1 = Return[t-1] - k5[t-1]
            u_2 = Return[t] - k5[t]
            u = u_1 +u_2
            n_new[t] = as.integer(k6 * u)
          }else{
            n_new[t] = 0
          }
          
          n_new[t] = min(n_new[t], n_new_max)  ## n_new为下一期新进入市场的公司数量
          
          ## 计算当期LR_sum[t+1],用来计算下一期的k2
          
          for(i in c(1 : length(rho))){
            L_sum[t] = as.double(L_sum[t] + L[t, i])
            P_p_sum[t] = as.double(P_p_sum[t]+P_p[t, i]* E[t, i])
            LR_sum[t+1] = as.double(L_sum[t] / P_p_sum[t])  
          }
          
          
          if (length(n_exit) == 0) {  ## 如果下一期没有被接管公司,上述n_exit就是integer(0), 这时要把它设为NULL
            n_exit <- c()
          }   ## n_exit为下一期处于接管状态的公司索引
          
          ## 此时的n_exit含有三类公司的索引：
          ## t期处于接管第二期，t+1期恢复业务，该公司E[t, i]==0 && E[t-1, i]==0
          ## t期处于接管第一期，t+1期继续被接管，该公司E[t, i]==0 && E[t-1, i]!=0
          ## t期处于正常经营状态，t+1期处于接管第一期，该公司E[t, i]!= 0
          ## 所以需要剔除掉恢复业务的公司
          
          ## 创建恢复业务公司索引
          
          n_recover <- c() ## 恢复业务的公司索引
          
          for(i in c(1:(length(rho) - n_new[t-1]))){   ## 将新公司数据剔除，不剔除会报错，但是对结果无影响
            if(E[t, i] == 0 && E[t-1, i] == 0){       ## 认为t-1期被接管的公司在t+1期恢复业务
              n_recover <- append(n_recover, i)
            }
          }
          
          ## 将恢复业务公司从原有的n_exit中剔除
          
          n_exit <- setdiff(n_exit, n_recover)
          
          ## 如果t期存在只存在处于第二期接管的公司
          ## 上述n_exit就是integer(0), 这时要把它还原为为NULL
          if (length(n_exit) == 0) {  
            n_exit <- c()
          }   ## n_exit为下一期处于接管状态的公司索引
          
          loss[t] = sum(L[t, which(is.na(L[t, ]) != T)])
          
          len_rho[t] = length(rho)
          
          ## 当期平均beta值
          beta_avg = c(beta_avg, as.numeric(quantile(beta[t,][which(E[t,] != 0)], 0.75, na.rm = T)))
          
          
          ## 第三期经营结束
          #####################################
          ## 第四期经营的步骤
          
          
          rm(t) ## 第三期将t值赋为3，循环之前要将原有t移除
          
          for(t in c(4 : t_final)){    ## t_final >= 4, 整体循环，回括号在最后一行
            
            ###### rho计算部分 ################################
            
            ## 保户转移调整
            
            eta <- matrix(nrow = length(rho), ncol = length(rho))
            tau <- matrix(nrow = length(rho), ncol = length(rho))
            E_tau <- matrix(nrow = length(rho), ncol = length(rho))
            eta_rsum <- double(length(rho))
            tau_rsum <- double(length(rho)) 
            E_star <- integer()
            
            for (r in c(1 : length(rho))) {
              for (s in c(1 : length(rho))) {
                if (r != s){
                  #eta[r, s] = log(max(1, (P[t-1, r] - P[t-1, s]))) * log(1 + 1/(abs(rho[s]-rho[r] + 0.000000001)))
                  eta[r, s] = max(1, (P[t-1, r] - P[t-1, s]))*max(0.01, rho[s])
                }else{
                  eta[r, s] = 0
                }
                eta_rsum[r] = sum(eta[r, ])
              }
            }
            
            for (r in c(1 : length(rho))) {
              for (s in c(1 : length(rho))) {
                if(eta_rsum[r] != 0 ){
                  tau[r, s] = (1 - exp(-k9 * eta_rsum[r])) * eta[r, s] / eta_rsum[r]
                }else{
                  tau[r, s] = 0
                }
              }
              tau_rsum[r] = sum(tau[r, ])
            }
            
            for(i in c(1 : length(rho))){
              for (r in c(1 : length(rho))) {
                E_tau[i, r] = as.double(E[t-1, r] * tau[r, i])
                E_star[i] = as.double(E[t-1, i] * (1 - tau_rsum[i]) + sum(E_tau[i, ]))
              }
            } 
            
            sum_star = sum(E_star)
            
            for(i in c(1:length(rho))){
              rho[i] = E_star[i]/sum_star
            }
            
            ## 被接管调整
            
            rho_sum_exit <- 0 
            
            if(is.null(n_exit) == FALSE){
              for(i in n_exit){                  
                rho_sum_exit = rho_sum_exit + rho[i]   
                rho[i] = 0
              }
            }
            
            for(i in c(1 : length(rho))){
              rho[i] = rho[i]/(1 - rho_sum_exit)
            }  
            
            ## 恢复经营调整------------------------------------------------NEW
            ## 恢复经营公司数量：length(n_recover)
            
            ## 如果t+1期恢复业务，那么t期的rho值为0
            ## 单个恢复业务公司的风险单位数固定为 E_r
            ## rho_r = E_r / (E0 * g ^ t) ## 下一期中，单个恢复业务公司的市场份额
            
            ## 恢复经营调整和新公司调整
            
            ## 如果有新公司进入，单个新公司的市场份额1/N
            ## 下一期公司数量N = length(rho) - length(n_exit) + n_new[t] + length(n_recover)
            ## 按这种方式调整，恢复经营的公司和新公司的市场份额就是相同的，所以还应该继续优化
            
            ## 同时存在恢复经营和新公司时
            quant_rho = as.numeric(quantile(rho[which(rho != 0)], 0.25))
            
            if (is.numeric(n_recover)){
              rho_n = quant_rho ## 下一期中，新进入公司单个的市场份额
              rho_r = quant_rho ## 下一期中，恢复经营公司的市场份额，也用了下四分位数
              rho <- c(rho, rep(rho_n, n_new[t-1]))
              rho[n_recover] = rho_r
              rho = rho / sum(rho)
            } 
            
            ## 不存在恢复经营公司时
            
            if (is.null(n_recover)){ 
              rho_n = quant_rho ## 下一期中，新进入公司单个的市场份额
              rho <- c(rho, rep(rho_n, n_new[t-1]))
              rho = rho / sum(rho)
            } 
            
            
            #######################################################################################3
            ## 公司经营部分
            ## 分组
            
            group<- rep(1, length(rho))  ## 组别重置
            
            if(is.numeric(n_exit)){ 
              for(i in c((length(rho)-length(n_exit)):2)){
                d_1 <- diff(rho[order(rho)])
                d1 <- d_1[length(n_exit):(length(rho)-1)]
                d_2 <- (rho[order(rho)])
                d2 <- d_2[(length(n_exit)+1):length(rho)]
                d <- d1/d2
                if (is.na(d[i-1]) == F && d[i-1] > rho_m) {
                  group[i+length(n_exit)-1] = group[i+length(n_exit)] + 1
                }else{
                  group[i+length(n_exit)-1] = group[i+length(n_exit)]
                }
              }
            }else{
              for(i in c((length(rho)):2)){
                d1 <- diff(rho[order(rho)])
                d2 <- rho[order(rho)][2:length(rho)]
                d <- d1/d2
                if (is.na(d[i-1]) == F && d[i-1] > rho_m){
                  group[i-1] = group[i] + 1
                }else{
                  group[i-1] = group[i]
                }
              }
            }
            
            group <- group[rank(rho)]  ## 最终组别
            
            for(i in 1:length(rho)){## 将被接管的公司组别设为100,
              #rho_indentity[t, i] = rho[i]
              if (rho[i] == 0){
                group[i] = 100
              }
            }
            
            ## 这里将新公司beta值引入，这样做是为了方便计算组内平均费率水平，实际上他们不参与计算
            
            if (n_new[t-1] !=  0){
              for (i in length(rho) - c(1:0)) {
                beta[t-1, i] = beta_avg[t-1]
              }
            }
            
            if (is.numeric(n_recover)){
              for (i in n_recover) {
                beta[t-1, i] = beta[t-3, i]
              }
            }
            sum_j <- rep(0, 100)  ## 这里要注意归零
            
            for (i in setdiff(unique(group), 100)) {
              j = which(group == i)
              for (x in j){
                sum_j[i] = sum_j[i] + beta[t-1, x]
              }
              avg[i] = sum_j[i] / length(j)
            }
            
            LR_sum_basic = mean(LR_basic[which(LR_basic != 0 )])
            #######################
            ## 对于保单增长速度的设定
            E_sum[t]=E_sum[t-1] * g[t]
            
            ## 计算当期期初总资本
            equity_sum[t] = sum(Equity[t-1,])
            
            
            ## 计算所有正常经营的保险公司经营情况
            
            ##  正常经营中----t期有公司被接管
            if(is.numeric(n_exit)){     
              for (i in setdiff(setdiff(c(1:(length(rho)-n_new[t-1])), n_recover), n_exit)) {
                #shock = cat_per[t, normal] * shock_und * equity_sum[t]  * rho[i]
                K_begin[t, i] = K[t-1, i] ## 正常经营公司的期初资产为前一期的期末资产
                E[t, i] = min(E_sum[t] * rho[i], E[t-1, i]*1.1)
                ## 对于基准赔付率的修改
                L[t, i] = rpois(1, E[t, i]) * rgamma(1, shape = LR_basic[i] * p_basic /2 , scale = 2) 
                P_exp[t, i] = P_p[t-1, i]  
                P_p[t, i] = P_exp[t, i] * min(1.05,exp(k1 * (LR[t-1, i] - LR_basic[i])))#P_exp[t, i] * exp(k1 * (LR[t-1, i] - LR_basic[i]))################
                if(shock_und != 0 & cat_per == t){  ## 这里判断是否会发生风险冲击，发生的话赔付比例120%
                  L[t, i] = shock_und * E[t, i] * P_p[t, i]
                }
                gamma[t] = 1 - exp(-LR_sum[t])
                beta[t, i] = (1 - k3) * beta[t-1, i] *(beta[t-1, i] / avg[group[i]]) ^ (-gamma[t]) + k3 * beta[t-1, i]
                P_l[t, i] = beta[t, i] * P_p[t, i]  
                P[t, i] = P_p[t, i] + P_l[t, i]
                LR[t, i] = L[t, i] / P_p[t, i] / E[t, i]
                invest[t, i] = (K_begin[t, i] + E[t, i] * P_p[t, i] - E[t-1, i] * P_p[t-1, i])  
                
                # 最佳投资比例的确定
                # 权益资本的投资比例上限要根据上一期偿付能力要求确定，
                # 参见<中国银保监会办公厅关于优化保险公司权益类资产配置监管有关事项的通知_国务院部门文件_中国政府网>
                #rm_top = min(((Equity[t-1, i] / mc_zuidi[t-1, i]) %/% 0.5)*0.05 + 0.1, 0.45)  
                ## 如果保险公司上季末综合偿付率<100%，将停止新增权益投资
                # if(mc_zuidi[t-1, i] < 1){
                #   rm_top = min(rm_top, alpha_gu[t-1, i])
                # }
                
                alpha = c(1, 0 ,0,0,0,0,0,0)
                tryCatch( {alpha  = solnl(x0, objfun = fr, confun=confun2, lb =c(0,0,0,0,0,0,0,0), ub = c(1,1,1,1,1,1,1,1),
                                          tolX = 1e-02,
                                          tolFun = 1e-03, tolCon = 1e-02, maxnFun = 1e+03, maxIter = 400)$par},
                          error=function(e){
                            ## 报错时执行的操作(此处留空表示静默跳过)
                            #cat("ERROR :",t, i, conditionMessage(e), "\n")
                            invisible(NULL)
                          })
                deposit[t, i] = alpha[1] * invest[t, i] * (1 + rate_deposit[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司的定期存款,矩阵结构             
                financial_debt[t, i] = alpha[2] * invest[t, i] * (1 + rate_financial[t]) ### 每个公司金融债,矩阵结构     
                company_debt[t, i] = alpha[3] * invest[t, i] * (1 + rate_company[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司企业债,矩阵结构
                ten_year_national_debt[t, i] = alpha[4] * invest[t, i] * (1 + rate_ten[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司10年期国债,矩阵结构
                twenty_year_national_debt[t, i] = alpha[5] * invest[t, i] * (1 + rate_twenty[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司二十年期国债,矩阵结构
                stock[t, i] = alpha[6] * invest[t, i] * (1 + rate_stock[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司股票,矩阵结构              
                real_estate[t, i] = alpha[7] * invest[t, i] * (1 + rate_real_estate[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司房地产,矩阵结构          
                foreign_assets[t, i] = alpha[8] * invest[t, i] * (1 + rate_foreign[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司国外资产,矩阵结构
                ## 流动性直接算  不用折算
              } 
            }
            
            ## 正常经营中------t期没有公司被接管,(这里还包括恢复经营的公司, 用setdiff去除)
            if(is.null(n_exit)){   
              for (i in setdiff(c(1:(length(rho)-n_new[t-1])), n_recover)) {
                #shock = cat_per[t, normal] * shock_und * equity_sum[t]  * rho[i]
                K_begin[t, i] = K[t-1, i] ## 正常经营公司的期初资产为前一期的期末资产
                E[t, i] = min( E_sum[t] * rho[i], E[t-1, i]*1.1)
                ## 对于基准赔付率的修改
                LR_basic[i] = LR_basic[i] 
                L[t, i] = rpois(1, E[t, i]) * rgamma(1, shape =LR_basic[i] * p_basic /2 , scale = 2)
                P_exp[t, i] = P_p[t-1, i]   
                P_p[t, i] = P_exp[t, i] * min(1.05,exp(k1 * (LR[t-1, i] - LR_basic[i])))#P_exp[t, i] * exp(k1 * (LR[t-1, i] - LR_basic[i]))
                if(shock_und != 0 & cat_per == t){  ## 这里判断是否会发生风险冲击，发生的话赔付比例120%
                  L[t, i] = shock_und * E[t, i] * P_p[t, i]
                }
                gamma[t] = 1 - exp(-LR_sum[t])
                beta[t, i] = (1 - k3) * beta[t-1, i]  *(beta[t-1, i] / avg[group[i]]) ^ (-gamma[t]) + k3 * beta[t-1, i]
                P_l[t, i] = beta[t, i] * P_p[t, i]  
                P[t, i] = P_p[t, i] + P_l[t, i]
                LR[t, i] = L[t, i] / P_p[t, i] / E[t, i]
                invest[t, i] = (K_begin[t, i] + E[t, i] * P_p[t, i] - E[t-1, i] * P_p[t-1, i])  
                
                # rm_top = min(((Equity[t-1, i] / mc_zuidi[t-1, i]) %/% 0.5)*0.05 + 0.1, 0.45)
                # ## 如果保险公司上季末综合偿付率<100%，将停止新增权益投资
                # if(Equity[t-1, i] / mc_zuidi[t-1, i] < 1){
                #   rm_top = min(rm_top, alpha_gu[t-1, i])
                # }
                
                alpha = c(1, 0 ,0,0,0,0,0,0)
                tryCatch( {alpha  = solnl(x0, objfun = fr, confun=confun2, lb =c(0,0,0,0,0,0,0,0), ub = c(1,1,1,1,1,1,1,1),
                                          tolX = 1e-02,
                                          tolFun = 1e-03, tolCon = 1e-02, maxnFun = 1e+03, maxIter = 400)$par},
                          error=function(e){
                            ## 报错时执行的操作(此处留空表示静默跳过)
                            #cat("ERROR :",t, i, conditionMessage(e), "\n")
                            invisible(NULL)
                          })
                
                deposit[t, i] = alpha[1] * invest[t, i] * (1 + rate_deposit[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司的定期存款,矩阵结构             
                financial_debt[t, i] = alpha[2] * invest[t, i] * (1 + rate_financial[t]) ### 每个公司金融债,矩阵结构     
                company_debt[t, i] = alpha[3] * invest[t, i] * (1 + rate_company[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司企业债,矩阵结构
                ten_year_national_debt[t, i] = alpha[4] * invest[t, i] * (1 + rate_ten[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司10年期国债,矩阵结构
                twenty_year_national_debt[t, i] = alpha[5] * invest[t, i] * (1 + rate_twenty[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司二十年期国债,矩阵结构
                stock[t, i] = alpha[6] * invest[t, i] * (1 + rate_stock[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司股票,矩阵结构              
                real_estate[t, i] = alpha[7] * invest[t, i] * (1 + rate_real_estate[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司房地产,矩阵结构          
                foreign_assets[t, i] = alpha[8] * invest[t, i] * (1 + rate_foreign[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司国外资产,矩阵结构
                ## 流动性直接算  不用折算
                
              } 
            }
            
            
            ## 所有新加入的保险公司经营情况
            if (n_new[t-1] != 0){
              tag_normal = which(E[t-1,] != 0) ## 上一期经营的公司索引
              for (i in c((length(rho)- n_new[t-1] + 1) : length(rho))){
                #shock = cat_per[t, normal] * shock_und * equity_sum[t]  * rho[i]
                #leverage[i] = quantile(leverage[tag_normal], 0.25) ## 这里要多计算一个新公司杠杆率
                K_begin[t, i] = quantile(K[t-1,][tag_normal], 0.25) ## 新公司的期初总资产是前一期经营公司总资产的下四分位数
                Equity[t-1, i] = quantile(Equity0, 0.25)
                E[t, i] = E_sum[t]  * rho[i]
                L[t, i] = rpois(1, E[t, i]) * rgamma(1, shape = LR_basic[i] * p_basic /2 , scale = 2)
                #L[t, i] = sum(rnbinom(E[t, i],  size = 0.005, mu = 0.17) * rlnorm(E[t, i], meanlog = 9.13, sdlog = 0.05))
                P_exp[t, i] = p_basic  #ll[i]
                LR_basic[i] = quantile(LR_basic[tag_normal], 0.75) ## 基准赔付率为前一期基准赔付率的下四分位数
                P_p[t, i] = P_exp[t, i]   ## 纯保费基于市场状况调整
                if(shock_und != 0 & cat_per == t){  ## 这里判断是否会发生风险冲击，发生的话赔付比例120%
                  L[t, i] = shock_und * E[t, i] * P_p[t, i]
                }
                LR[t, i] = L[t, i] / P_p[t, i] / E[t, i]
                gamma[t] = 1 - exp(-LR_sum[t] )
                #beta[t, i] = beta[t-1, i]*(beta[t-1, i] / avg[group[i]]) ^ -(gamma[t])
                beta[t, i] = quantile(beta[t-1,][tag_normal], 0.75) ## beta值为前一期下四分位数
                P_l[t, i] = beta[t, i] * P_p[t, i]
                P[t, i] = P_p[t, i] + P_l[t, i]
                invest[t, i] = (K_begin[t, i] + E[t,i] * P[t,i]) 
                
                alpha = c(1, 0 ,0,0,0,0,0,0)
                tryCatch( {alpha  = solnl(x0, objfun = fr, confun=confun2, lb =c(0,0,0,0,0,0,0,0), ub = c(1,1,1,1,1,1,1,1),
                                          tolX = 1e-02,
                                          tolFun = 1e-03, tolCon = 1e-02, maxnFun = 1e+03, maxIter = 400)$par},
                          error=function(e){
                            ## 报错时执行的操作(此处留空表示静默跳过)
                            #cat("ERROR :",t, i, conditionMessage(e), "\n")
                            invisible(NULL)
                          })
                deposit[t, i] = alpha[1] * invest[t, i] * (1 + rate_deposit[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司的定期存款,矩阵结构             
                financial_debt[t, i] = alpha[2] * invest[t, i] * (1 + rate_financial[t]) ### 每个公司金融债,矩阵结构     
                company_debt[t, i] = alpha[3] * invest[t, i] * (1 + rate_company[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司企业债,矩阵结构
                ten_year_national_debt[t, i] = alpha[4] * invest[t, i] * (1 + rate_ten[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司10年期国债,矩阵结构
                twenty_year_national_debt[t, i] = alpha[5] * invest[t, i] * (1 + rate_twenty[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司二十年期国债,矩阵结构
                stock[t, i] = alpha[6] * invest[t, i] * (1 + rate_stock[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司股票,矩阵结构              
                real_estate[t, i] = alpha[7] * invest[t, i] * (1 + rate_real_estate[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司房地产,矩阵结构          
                foreign_assets[t, i] = alpha[8] * invest[t, i] * (1 + rate_foreign[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司国外资产,矩阵结构
                ## 流动性直接算  不用折算
                
              }
            }
            
            ## 所有恢复业务状态的保险公司-------------------------------------------NEW
            ## 恢复经营业务状态的公司，beta[t,i]-----------------------
            ## 恢复仅经营的公司对于总资资产要进行调整
            if(is.numeric(n_recover)){  ## n_recover有数值的话,说明t期有公司处于恢复业务状态
              for (i in n_recover) {
                #shock = cat_per[t, normal] * shock_und * equity_sum[t]  * rho[i]
                Equity[t-1, i] = abs(5 * mc_zuidi[t-3, i]) #sl_ratio * mc_zuidi[t-3, i] #abs(Equity[t-3, i]) * 3
                K_begin[t, i] = K[t-1, i] + Equity[t-1, i] - Equity[t-3, i] ## 期初总资产调整
                E[t, i] = min(E_sum[t] * rho[i], E[t-3, i] * 1.2)
                L[t, i] = rpois(1, E[t, i]) * rgamma(1, shape = LR_basic[i] * p_basic /2 , scale = 2) 
                #L[t, i] = sum( rnbinom(E[t, i],  size = 0.005, mu = 0.17) * rlnorm(E[t, i], meanlog = 9.13, sdlog = 0.05))
                P_exp[t, i] = p_basic    
                P_p[t, i] = P_exp[t, i] * min(1.02,exp(k1 * (LR[t-3, i] - LR_basic[i])))
                if(shock_und != 0 & cat_per == t){  ## 这里判断是否会发生风险冲击，发生的话赔付比例120%
                  L[t, i] = shock_und * E[t, i] * P_p[t, i]
                }
                LR[t, i] = L[t, i] / E[t, i] / P_p[t, i]
                gamma[t] = 1 -  exp(-LR_sum[t])
                beta[t, i] = beta[t-3, i]*(beta[t-3, i] / avg[group[i]]) ^ -(gamma[t])
                P_l[t, i] = beta[t, i] * P_p[t, i]  
                P[t, i] = P_p[t, i] + P_l[t, i]
                LR[t, i] = L[t, i] / P_p[t, i] / E[t, i]
                invest[t, i] =( K_begin[t, i] + E[t, i] * P_p[t, i] - E[t-1, i] * P_p[t-1, i] )
                alpha = c(1, 0 ,0,0,0,0,0,0)
                tryCatch( {alpha  = solnl(x0, objfun = fr, confun=confun2, lb =c(0,0,0,0,0,0,0,0), ub = c(1,1,1,1,1,1,1,1),
                                          tolX = 1e-02,
                                          tolFun = 1e-03, tolCon = 1e-02, maxnFun = 1e+03, maxIter = 400)$par},
                          error=function(e){
                            ## 报错时执行的操作(此处留空表示静默跳过)
                            #cat("ERROR :",t, i, conditionMessage(e), "\n")
                            invisible(NULL)
                          })
                deposit[t, i] = alpha[1] * invest[t, i] * (1 + rate_deposit[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司的定期存款,矩阵结构             
                financial_debt[t, i] = alpha[2] * invest[t, i] * (1 + rate_financial[t]) ### 每个公司金融债,矩阵结构     
                company_debt[t, i] = alpha[3] * invest[t, i] * (1 + rate_company[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司企业债,矩阵结构
                ten_year_national_debt[t, i] = alpha[4] * invest[t, i] * (1 + rate_ten[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司10年期国债,矩阵结构
                twenty_year_national_debt[t, i] = alpha[5] * invest[t, i] * (1 + rate_twenty[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司二十年期国债,矩阵结构
                stock[t, i] = alpha[6] * invest[t, i] * (1 + rate_stock[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司股票,矩阵结构              
                real_estate[t, i] = alpha[7] * invest[t, i] * (1 + rate_real_estate[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司房地产,矩阵结构          
                foreign_assets[t, i] = alpha[8] * invest[t, i] * (1 + rate_foreign[t]) #<- matrix(0, nrow = t_final, ncol = n_max) ## 每个公司国外资产,矩阵结构
                ## 流动性直接算  不用折算
              } 
            }
            
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
              if(cash[t, i] > 0 & L[t, i] > cash[t, i]){
                liq_gap[t, i] = L[t, i] - cash[t, i] ## 流动性缺口规模
                ## 为了公司进行业务赔付，保险公司需要进行第一批次的减价出售，出售股权投资，50%的折价
                ## 如果存在流动性缺口，那么就需要进行减价出售机制
                ## 如果没有缺口那就不进行减价出售
                cash[t, i] = cash[t, i] + liq_gap[t, i]
              }
            }
            
            ## 减价出售机制设定
            sum_lip = sum(liq_gap[t, ])
            #FV_risk[t, ] = 0
            FV_risk[t, ] = financial_debt[t, ] + company_debt[t, ] + stock[t, ] + real_estate[t, ] + foreign_assets[t, ]
            sum_risk = sum(FV_risk[t, ])
            pp = 0
            p = 1
            
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
            
            
            price_decline[t] = p
            w[t] = pp
            
            
            n_exit <- c()
            for(i in 1:length(rho)){
              fire_sum = max(1, sum(financial_debt[t, i] + company_debt[t, i] + stock[t, i] +real_estate[t, i]+ foreign_assets[t, i]))
              financial_debt[t, i] =  FV_risk[t, i] * financial_debt[t, i]/fire_sum
              company_debt[t, i] =  FV_risk[t, i] * company_debt[t, i]/fire_sum
              stock[t, i] =  FV_risk[t, i] * stock[t, i]/fire_sum
              real_estate[t, i] = FV_risk[t, i] * real_estate[t, i]/fire_sum
              foreign_assets[t, i] = FV_risk[t, i] * foreign_assets[t, i]/fire_sum
              FV_final[t, i] = FV_risk[t, i] + cash[t, i]
              
              # 经营结果
              pi[t, i] = FV_final[t, i]  - invest[t, i] + #投资利润
                E[t, i] * P_p[t, i] - L[t, i] ## 承保利润  ## 现阶段的利润
              ## 算是否需要进行偿付能力调整
              Equity[t, i] = Equity[t-1, i] + pi[t, i]   ## 经营结束之后的权益量
              
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
              
              
              s = Equity[t, i] / mc_zuidi[t, i]
              tt = NA
              ## 如果偿付能力达标，并且当期利润为正的话就会进行分红  ## 暂时不设分红
              if(s < sl_ratio |  E[t, i] == 0 ){
                n_exit <- append(n_exit, i)
              }else if(s  == Inf){
                n_exit <- append(n_exit, i)
              }else{
                #D2 = (Equity[t-1, i] - sl_ratio * mc_zuidi[t, i] + pi[t, i]) * k4 #####  分红比例的设定
                D2 = (Equity[t-1, i] - sl_ratio_floor * mc_zuidi[t, i] + pi[t, i]) * k4
                if(pi[t, i] > 0 ){  ## 如果当期的收益值大于0
                  D1 = pi[t, i] * k4
                  D[t, i] =  min(D1, D2)
                  Equity[t, i] = Equity[t, i] - D[t, i]
                }
              }
              
              pi_uw[t, i] = E[t, i] * P_p[t, i] - L[t, i]   ###  单期的承保利润
              pi_iv[t, i] = FV_final[t, i] - invest[t, i]
              
              ## 期末总资产
              K[t, i] = invest[t, i] + pi[t, i] - D[t, i]  
              ## 对于被接管公司来讲，期末总资产就为期初总资产
              if(E[t, i] == 0){
                K[t, i] = K_begin[t, i]
              }
              ## 所有者权益变动,保险公司按照杠杆率进行总资本的补充
              #Equity[t, i] = leverage[i] * K0[i] + pi[t, i] - D[t, i]
              rho_indentity[t, i] = rho[i]
            }
            n_exit = unique(n_exit)
            
            ## 计算市场利润率,计算t+1期新公司数量n_new[t]------------------------------NEW
            ## 期初净资产
            K_begin_sum[t] =  sum(invest[t,]) ## 保险市场总资产 期初值
            if(is.null(n_exit) == F){
              K_begin_sum[t] = K_begin_sum[t] + sum(K_begin[t, n_exit])
            }
            K_sum[t] =  sum(K[t,]) ## 保险市场总资产（期末值）
            pi_sum[t] = sum(pi[t,]) ## 总利润
            ## 这里的市场收益率并没有包含被接管公司的总资产
            Return[t] = 2 * as.double(pi_sum[t] / (K_begin_sum[t] + K_sum[t]))
            
            ## 新公司数量
            if (Return[t - 1] > k5[t-1] && Return[t] > k5[t]) {
              u_1 = Return[t-1] - k5[t-1]
              u_2 = Return[t] - k5[t]
              u = u_1 +u_2
              n_new[t] = as.integer(k6 * u)
            }else{
              n_new[t] = 0
            }
            
            if (Return[t-1] > k5[t-1] && Return[t] > k5[t] &&Return[t-2] > k5[t]) {#######
              u_1 = Return[t-1] - k5[t-1]
              u_2 = Return[t] - k5[t]
              u_3 = Return[t-2] - k5[t]
              u = mean(u_1,u_2,u_3)
              n_new[t] = as.integer(k6 * u)
            }else{
              n_new[t] = 0
            }
            
            n_new[t] = min(n_new[t], n_new_max)  ## n_new为下一期新进入市场的公司数量
            ## 计算当期LR_sum[t+1],用来计算下一期的k2
            
            for(i in c(1 : length(rho))){
              L_sum[t] = as.double(L_sum[t] + L[t, i])
              P_p_sum[t] = as.double(P_p_sum[t]+P_p[t, i]* E[t, i])
              LR_sum[t+1] = as.double(L_sum[t] / P_p_sum[t])  
            }
            
            
            ## 计算当期LR_sum[t+1],用来计算下一期的k2
            
            for(i in c(1 : length(rho))){
              L_sum[t] = as.double(L_sum[t] + L[t, i])
              P_p_sum[t] = as.double(P_p_sum[t]+P_p[t, i]* E[t, i])
              LR_sum[t+1] = as.double(L_sum[t] / P_p_sum[t])  
            }
            
            ## 记录下一期被接管公司索引n_exit
            
            if (length(n_exit) == 0) {  ## 如果下一期没有被接管公司,上述n_exit就是integer(0), 这时要把它设为NULL
              n_exit <- c()
            }   ## n_exit为下一期处于接管状态的公司索引
            
            
            ## 此时的n_exit含有三类公司的索引：
            ## t期处于接管第二期，t+1期恢复业务，该公司E[t, i]==0 && E[t-1, i]==0 或者K[t, i] == -1 && K[t-1, i] == -1
            ## t期处于接管第一期，t+1期继续被接管，该公司E[t, i]==0 && E[t-1, i]!=0
            ## t期处于正常经营状态，t+1期处于接管第一期，该公司E[t, i]!= 0
            ## 所以需要剔除掉恢复业务的公司
            
            ## 创建下一期恢复业务公司索引
            n_recover <- c() ## 恢复业务的公司索引
            
            for(i in c(1:(length(rho) - n_new[t-1]))){   ## 将新公司数据剔除，不剔除会报错，但是对结果无影响
              if(E[t, i] == 0 && E[t-1, i] == 0){         ## 认为t-1期被接管的公司在t+1期恢复业务
                n_recover <- append(n_recover, i)
              }
            }
            
            ## 将恢复业务公司从原有的n_exit中剔除
            n_exit <- setdiff(n_exit, n_recover)
            if (length(n_exit) == 0) { 
              n_exit <- c()
            } 
            
            ###############################################
            
            loss[t] = sum(L[t, which(is.na(L[t, ]) != T)])
            
            ##  新公司beta值
            beta_avg = c(beta_avg, as.numeric(quantile(beta[t,][which(E[t,] != 0)], 0.75, na.rm = T)))
            
            
          }  ## 整个t_final循环的回括号
          
          ## 检验t_final期运行结果（如果中途退出，就不是t_final期了）
          
          print(paste("该模型结束于第", t , "期, t_final = ", t_final, 
                      'risk_aversion', risk_aversion_par,
                      'ppp=',normal, "是否发生巨灾:", shock_und))
          
          ## 第t_final期经营结束
          ##################################################################################
          # if(any(Return > 0.1)){
          #   stop()
          # }
          # 
          # if(any(K_sum < 0)){
          #   stop()
          # }
          # 
          # n_sum <- rep(0, t_final)  ## 每一期参与经营的公司数量
          # 
          # for (t in 1:t_final) {
          #   for(i in c(1:n_max)){
          #     if ( is.na(E[t, i]) == FALSE && E[t, i] != 0){
          #       n_sum[t] = n_sum[t]+1
          #     }
          #   }
          # }
          # 
          # n_sum_before <- rep(0, t_final)  ## 原有公司经营的公司数量
          # 
          # for (t in 1:t_final) {
          #   for(i in c(1:nrow(data_full))){
          #     if ( is.na(E[t, i]) == FALSE && E[t, i] != 0){
          #       n_sum_before[t] = n_sum_before[t]+1
          #     } 
          #   }
          # }
          
          ############################

          # mean_beta <- c()
          # p_avg <- c()
          # p_pure <- c()
          # 
          # rho_square <- c()
          # for(i in 1:t_final){
          #   rho_square = c(rho_square, sum(rho_indentity[i, ]^2))
          #   s = which(E[i,] != 0)
          #   d = sum(P[i, s] * rho_indentity[i, s])
          #   p_avg = c(p_avg, d)
          #   e = sum(beta[i, s] * rho_indentity[i, s])
          #   mean_beta = c(mean_beta, e)
          #   p_pure = c(p_pure, sum(P_p[i, s] * rho_indentity[i, s]))
          # }
          # 
          # 
          # pro_exit <- c()
          # pro_gap <- c()
          # equity_sum <- c()
          # ## 间接损失
          # beta_avg_wr <- c()
          # 
          # for(q in 1:t_final){
          #   ## 市场中公司总数
          #   a = max(which(K_begin[q,] != 0))
          #   ## 被接管公司数量
          #   b = length(which(P_p[q,] == 0))
          #   ## 当期被接管公司占比
          #   pro_exit = c(pro_exit, b/a)
          #   ## 流动性缺口的公司数量
          #   equity_sum = c(equity_sum, sum(Equity[q, which(K_begin[q,] != 0)]))
          #   normal_rho = which(rho_indentity[q,] != 0)
          #   beta_avg_wr[q] =  sum(beta[q, normal_rho] * rho_indentity[q, normal_rho])
          # }
          # 
          # pi_uw_pro <- rep(0, t_final)
          # for(t in 1:t_final){
          #   pi_uw_pro[t] = sum(pi_uw[t, ])/(sum(pi_uw[t, ])+ sum(pi_iv[t, ]))
          # }
          # 
          # ###  分市场份额的比例
          # ## 按照市场份额的排名对保险公司进行排序
          # 
          # ## 计算最低资本
          # uu <- rep(0, t_final)
          # uu1 = uu
          # uu2 = uu
          # for ( ww in 1:t_final){
          #   ii = 0
          #   pp = 0
          #   ii1 = 0
          #   pp1 = 0
          #   ii2 = 0
          #   pp2 = 0
          #   qq = length(rho_indentity[t,which(rho_indentity[t, ] != 0)])
          #   oo1 = rank(rho_indentity[t,which(rho_indentity[t, ] != 0)])[1:round(quantile(c(1:qq), 0.5))]
          #   for(i in 1:n_max){
          #     s = Equity[ww, i] / mc_function_asset(yewu = E[ww, i] * P_p[ww, i],
          #                                           
          #                                           deposit= deposit[ww, i],
          #                                           financial_debt= financial_debt[ww, i],
          #                                           company_debt= company_debt[ww, i],
          #                                           ten_year_national_debt= ten_year_national_debt[ww, i],
          #                                           twenty_year_national_debt= twenty_year_national_debt[ww, i],
          #                                           stock= stock[ww, i],
          #                                           real_estate= real_estate[ww, i],
          #                                           foreign_assets= foreign_assets[ww, i],
          #                                           #lilv = 0.025,
          #                                           x= 0,
          #                                           RF0_property = mc_rf0[i])
          #     
          #     if (is.na(s) == F & s > 0 & s < 100){
          #       ii = ii + rho_indentity[ww, i]
          #       pp  =  pp + s * rho_indentity[ww, i]
          #       if(i %in% oo1){
          #         ii1 = ii1 + rho_indentity[ww, i]
          #         pp1  =  pp1 + s * rho_indentity[ww, i]
          #         
          #       }else{
          #         ii2 = ii2 + rho_indentity[ww, i]
          #         pp2  =  pp2 + s * rho_indentity[ww, i]
          #       }
          #     }
          #     
          #   }
          #   uu[ww] = uu[ww] + pp/ii
          #   uu1[ww] = uu1[ww] + pp1/ii1
          #   uu2[ww] = uu2[ww] + pp2/ii2
          # }

          # assign(paste0("MC_ZUIDI_", shock_und), cbind(get(paste0("MC_ZUIDI_", shock_und)), uu))
          # assign(paste0("MC_ZUIDI_lower_", shock_und),cbind(get(paste0("MC_ZUIDI_lower_", shock_und)), uu1))
          # assign(paste0("MC_ZUIDI_higher_", shock_und), cbind(get(paste0("MC_ZUIDI_higher_", shock_und)), uu2))
          # assign(paste0("PRICE_DECLINE_", shock_und), cbind(get(paste0("PRICE_DECLINE_", shock_und)), price_decline))
          # assign(paste0("RETURN_", shock_und), cbind(get(paste0("RETURN_", shock_und)), Return))
          # assign(paste0("NUMBER_", shock_und), cbind(get(paste0("NUMBER_", shock_und)), n_sum))
          # assign(paste0("NUMBER_BE_", shock_und), cbind(get(paste0("NUMBER_BE_", shock_und)), n_sum_before))
          # assign(paste0("RHO_SQUARE_", shock_und), cbind(get(paste0("RHO_SQUARE_", shock_und)), rho_square))
          # assign(paste0("P_PURE_", shock_und), cbind(get(paste0("P_PURE_", shock_und)), p_pure))
          # assign(paste0("BETA_", shock_und), cbind(get(paste0("BETA_", shock_und)), beta_avg_wr))
          # assign(paste0("P_GROSS_", shock_und), cbind(get(paste0("P_GROSS_", shock_und)), p_avg))
          # assign(paste0("K_SUM_", shock_und), cbind(get(paste0("K_SUM_", shock_und)), K_sum))
          # assign(paste0("EQUITY_RATE_", shock_und), cbind(get(paste0("EQUITY_RATE_", shock_und)), equity_sum/K_sum))
          # assign(paste0("PRO_EXIT_", shock_und), cbind(get(paste0("PRO_EXIT_", shock_und)), pro_exit))
          # assign(paste0("LR_SUM_", shock_und), cbind(get(paste0("LR_SUM_", shock_und)), LR_sum[-1]))
          # assign(paste0('INDIRECT_LOSS_', shock_und), cbind(get(paste0('INDIRECT_LOSS_', shock_und)), 
          #                                                   rowSums(FV_risk_before - FV_risk)))
          # 
          ## 对于利润率水平要重新导入
          normal = normal + 1
          loop_1 = loop_1 + 1
        }, error=function(e){cat("ERROR :",t, i, conditionMessage(e), "\n")})
        
        if (!is.null(result)) {
          
          break  # 成功则跳出重试循环
        }
      }
    }
  }
  loop_b = normal - 1
  
  
  ppp = ppp + 1
  
  
}

lll

PATH1 = "F:/jianguo/ABM/CODE_0915/"
for (shock_und in catastrophe){
  write.csv(get(paste0("MC_ZUIDI_", shock_und)), file = paste0(PATH1, "cat_", shock_und, "/MC_ZUIDI_", shock_und,".csv"))
  write.csv(get(paste0("MC_ZUIDI_lower_", shock_und)), file = paste(PATH1,"cat_", shock_und,  "/MC_ZUIDI_lower_", shock_und,".csv",sep = ''))
  write.csv(get(paste0("MC_ZUIDI_higher_", shock_und)), file = paste(PATH1,"cat_", shock_und,  "/MC_ZUIDI_higher_", shock_und,".csv",sep = ''))
  write.csv(get(paste0("PRICE_DECLINE_", shock_und)), file = paste(PATH1,"cat_", shock_und,  "/PRICE_DECLINE_", shock_und,".csv",sep = ''))
  write.csv(get(paste0("RETURN_", shock_und)), file = paste(PATH1,"cat_", shock_und,  "/RETURN_", shock_und,".csv",sep = ''))
  write.csv(get(paste0("NUMBER_", shock_und)), file = paste(PATH1,"cat_", shock_und,  "/NUMBER_", shock_und,".csv",sep = ''))
  write.csv(get(paste0("NUMBER_BE_", shock_und)), file =paste(PATH1,"cat_", shock_und,  "/NUMBER_BE_", shock_und,".csv",sep = ''))
  write.csv(get(paste0("RHO_SQUARE_", shock_und)), file =paste(PATH1,"cat_", shock_und,  "/RHO_SQUARE_", shock_und,".csv",sep = ''))
  #write.csv(INV_SQUARE, file =paste(PATH1, "INV_SQUARE_", shock_und,".csv",sep = ''))
  #write.csv(PRICE, file = paste(PATH1, "PRICE_", shock_und,".csv",sep = ''))
  write.csv(get(paste0("P_PURE_", shock_und)), file = paste(PATH1,"cat_", shock_und,  "/P_PURE_", shock_und,".csv",sep = ''))
  write.csv(get(paste0("BETA_", shock_und)), file = paste(PATH1,"cat_", shock_und,  "/BETA_", shock_und,".csv",sep = ''))
  write.csv(get(paste0("P_GROSS_", shock_und)), file = paste(PATH1,"cat_", shock_und,  "/P_GROSS_", shock_und,".csv",sep = ''))
  write.csv(get(paste0("K_SUM_", shock_und)), file = paste(PATH1,"cat_", shock_und,  "/K_SUM_", shock_und,".csv",sep = ''))
  write.csv(get(paste0("EQUITY_RATE_", shock_und)), file = paste(PATH1,"cat_", shock_und,  "/EQUITY_RATE_", shock_und,".csv",sep = ''))
  #write.csv(LIFE_OLD, file = paste(PATH1, "LIFE_OLD_", k14,".csv",sep = ''))
  #write.csv(LIFE_NEW, file = paste(PATH1, "LIFE_NEW_", k14,".csv",sep = ''))
  #write.csv(NUM_NEW, file = paste(PATH1, "NUM_NEW_", k14,".csv",sep = ''))
  #write.csv(SD_RETURN, file = paste(PATH1, "SD_RETURN_", k14,".csv",sep = ''))
  write.csv(get(paste0("INDIRECT_LOSS_", shock_und)), file = paste(PATH1,"cat_", shock_und, "/INDIRECT_LOSS_", shock_und,".csv",sep = ''))
  write.csv(get(paste0("PRO_EXIT_", shock_und)), file = paste(PATH1,"cat_", shock_und,  "/PRO_EXIT_", shock_und,".csv",sep = ''))
  #write.csv(PRO_GAP, file = paste(PATH1, "PRO_GAP_", k14,".csv",sep = ''))
  write.csv(get(paste0("LR_SUM_", shock_und)), file = paste(PATH1,"cat_", shock_und,  "/LR_SUM_", shock_und,".csv",sep = ''))
}


#beep(8) 
#par(mfrow = c(2,1))
## 进程提醒设置 ###########################

# progress <- 1:100 
# #开启进度条  
# pb <- tkProgressBar("进度","已完成 %", 0, 100)  
# for(prog in progress) {  
#   info<- sprintf("已完成 %d%%", round(prog*100/length(progress)))  
#   setTkProgressBar(pb, prog*100/length(progress), sprintf("进度 (%s)", info),info)  
# }     


#关闭进度条  
#close(pb) 
#beep(8)


# 发送邮件
# send.mail(from = "hyf_lit@163.com",
#           to = "hyf_lit@qq.com",
#           body = " shock_undKFFFFJJJJOOOO",
#           smtp = list(host.name="smtp.163.com", # smtp 服务器主机名
#                       port= 456, # 默认端口
#                       user.name="hyf_lit@163.com", # 用户名
#                       passwd="ZRZPHCIQUEVKYQMN", # 密码（授权码）
#                       ssl=TRUE),
#           authenticate = TRUE,
#           send = TRUE,
#           encoding = "utf-8" # 编码
# )
