
library(pracma) ## 算定积分面积
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

data_name = 'BETA'
##  数据存储的路径
PATH_1 = "F://jianguo/newest/data_0420/base/"
PATH_2 = paste0("F://jianguo/newest/data_0420/solvency_1.5/")
PATH_3 = paste0("F://jianguo/newest/data_0420/risk_aver_0.7/")

data_type = c(1,3)
for (i in data_type) {
  for(t in catastrophe){
    data = read.csv(paste0(get(paste0('PATH_', i)), 'cat_', t, '/', data_name, '_', t, '.csv'))[,-1]
    assign(paste0("ss_",i,'_', t), data)
  }
}
## 对于市场指标的构造
data_plot = c()
for(i in data_type){
  data_temp = c()
  for (t in catastrophe) {
    data = get(paste0("ss_",i,'_', t))
    data_temp = cbind(data_temp,rowSums(data)/ncol(data))
  }
  for(p in length(catastrophe)){
    names(data_temp)[i] = paste0('ss_', i)
  }
  data_temp = cbind(data_temp, c(1:t_final))
  data_temp = cbind(data_temp, i)
  data_plot = rbind(data_plot, data_temp)
}
name = c()
for(p in catastrophe){
  name = c(name, paste0('ss_', p))
}
name = c(name, 'year', 'type') 
colnames(data_plot) <- name

data_plot = as_tibble(data_plot)
data_plot$type = as.character(data_plot$type)
ggplot(data_plot) + 
  geom_line(aes(year, ss_0, group = type , col = type),linetype = 1, linewidth = 1)+
  geom_line(aes(year, ss_1.1, group = type, col = type ), linetype = 2, linewidth = 1)+
  geom_line(aes(year, ss_1.2, group = type, col = type  ), linetype = 3, linewidth = 1)+
  geom_line(aes(year, ss_1.3, group = type, col = type  ), linetype = 4, linewidth = 1)+
  theme_bw() 
  
## 计算各个市场类型下的行业韧性
data_plot  = c()
for(i in data_type){
  pp = 1000000
  for(j in str_c('ss_', i, '_', catastrophe)){
    pp = min(pp, ncol(get(j)))
  }
  data_temp  = c()
  for(t in catastrophe){
    #kk = which(as.numeric(get(paste0("ss_",i, "_", t))[20, 1:pp] )< get(paste0("ss_",i, "_", 0))[20, 1:pp])
    kk = 1:pp
    assign(paste0('ww_',i, '_', t),c())
    for(p in kk){
      assign(paste0('ww_',i, '_', t), c(get(paste0('ww_',i, '_', t)), resilience(avg_mc = get(paste0("ss_",i, '_', 0))[,p],
                                                                                 period = cat_per,
                                                                                 t_f = t_final,
                                                                                 mc = get(paste0("ss_",i, '_', t))[ , p])))
    }
    
    assign(paste0('ww_',i, '_', t) , get(paste0('ww_',i, '_', t))[is.na(get(paste0('ww_',i, '_', t))) == F])
    data_temp = cbind(data_temp, get(paste0('ww_',i, '_', t)))
  }
  data_temp = cbind(data_temp, i)
  data_plot = rbind(data_plot, data_temp)
}
data_plot= data_plot
name = c()
for(p in catastrophe){
  name = c(name, paste0('ss_', p))
}
name = c(name,  'type') 
colnames(data_plot) <- name
data_plot = as_tibble(data_plot)

for(i in 2:4){
  data_plot[which(data_plot[,i]> 1),i] = NA
}

plot(density(data_plot$ss_1.1, na.rm = T), ylim = c(0, 10), xlim = c(0.4,1))
lines(density(data_plot$ss_1.2, na.rm = T), col = 'red')
lines(density(data_plot$ss_1.3, na.rm = T), col = 'blue')





for(i in data_type){
  for(t in catastrophe[-1]){
    aa = get(paste0('ww_', i, '_', t))
    aa[which(aa > 1)] = NA
    assign(paste0('ww_',i, '_', t) , aa)
#abline(v = mean(get(paste0('ww_', i, 'i_', t))), bty = '7', col = str_c('gray', i*25),lwd = (t-1)*20 )
#abline(v = quantile(get(paste0('ww_', i, '_', t)), 0.05), col = str_c('gray', i*25), lwd = (t-1)*20)
  }
}


data_plot$type = as.character(data_plot$type)
plot(density(ww_1_1.1, na.rm = T), main=paste0("风险吸收强度指标：", data_name) , 
     xlim = range(1,  0.4),
     ylim = c(0,10)) ## 概率密度图

for(i in c(1, 3)){
  for(t in catastrophe[-1]){
    lines(density(get(paste0('ww_', i, '_', t)),na.rm = T ), lwd = (t-1)*20,, col = str_c('gray', i*25), lty = i*2-1)
    abline(v = mean(get(paste0('ww_', i, '_', t)),na.rm = T ), bty = '7', col = str_c('gray', i*25),lwd = (t-1)*20 )
    abline(v = quantile(get(paste0('ww_', i, '_', t)),na.rm = T , 0.05), col = str_c('gray', i*25), lwd = (t-1)*20)
  }
}

 ############################################################
data_plot  = c()
for(i in data_type){
  pp = 1000000
  for(j in str_c('ss_', i, '_', catastrophe)){
    pp = min(pp, ncol(get(j)))
  }
  
  data_temp  = c()
  for(t in catastrophe[-1]){
    kk = which(as.numeric(get(paste0("ss_",i, "_", t))[20, 1:pp] )< get(paste0("ss_",i, "_", 0))[20, 1:pp])
    #kk = 1:pp
    assign(paste0('ww_',i, '_', t),c())
    for(p in kk){
      assign(paste0('ww_',i, '_', t), c(get(paste0('ww_',i, '_', t)), resilience_time(avg_mc = get(paste0("ss_",i, '_', 0))[,p],
                                                                                 period = cat_per,
                                                                                 t_f = t_final,
                                                                                 mc = get(paste0("ss_",i, '_', t))[ , p])))
    }
    assign(paste0('ww_',i, '_', t) , get(paste0('ww_',i, '_', t))[is.na(get(paste0('ww_',i, '_', t))) == F])
    data_temp = cbind(data_temp, get(paste0('ww_',i, '_', t)))
  }
  data_temp = cbind(data_temp, i)
  data_plot = rbind(data_plot, data_temp)
}
name = c()
for(p in catastrophe[-1]){
  name = c(name, paste0('ss_', p))
}

name = c(name,  'type') 
colnames(data_plot) <- name
data_plot = as_tibble(data_plot)
data_plot$type = as.character(data_plot$type)

plot(density(ww_1_1.1), main=paste0("风险吸收时间指标：", data_name) , xlim = range(0,  30),
     ylim = c(0, 0.3)) ## 概率密度图
for(i in data_type){
  for(t in catastrophe[-1]){
    lines(density(get(paste0('ww_', i, '_', t))), lwd = (t-1)*20, col = str_c('gray', i*25), lty = i*2-1)
    abline(v = mean(get(paste0('ww_', i, '_', t))), bty = '7', col = 'red',lwd = (t-1)*20 )
    #abline(v = quantile(get(paste0('ww_', i, '_', t)), 0.95), col = str_c('gray', i*25), lwd = i*2, lty = i*2-1)
  }
}


data_name = 'PRICE_DECLINE'
##  数据存储的路径
#PATH_1 = paste0("F://newest/0321_DATA/data_1118/")
#PATH_2 = paste0("F://newest/0321_DATA/data_0902/data_1102/solvency_1.5/")
#PATH_3 = paste0("F://newest/0321_DATA/data_0902/data_1102/risk_aversion_0.7/")

for (i in data_type) {
  for(t in catastrophe){
    data = read.csv(paste0(get(paste0('PATH_', i)), 'cat_', t, '/', data_name, '_', t, '.csv'))[,-1]
    for(p in 1:ncol(data)){
      for(q in 2: nrow(data)){
        data[q, p] = data[q, p] * data[q-1, p]
      }
    }
    assign(paste0("ss_",i,'_', t), data)
  }
}

## 对于市场指标的构造
data_plot = c()
for(i in data_type){
  data_temp = c()
  for (t in catastrophe) {
    data = get(paste0("ss_",i,'_', t))
    
    data_temp = cbind(data_temp,rowSums(data)/ncol(data))
  }
  for(p in length(catastrophe)){
    names(data_temp)[i] = paste0('ss_', i)
  }
  data_temp = cbind(data_temp, c(1:t_final))
  data_temp = cbind(data_temp, i)
  data_plot = rbind(data_plot, data_temp)
}
name = c()
for(p in catastrophe){
  name = c(name, paste0('ss_', p))
} 
name = c(name, 'year', 'type') 
colnames(data_plot) <- name

data_plot = as_tibble(data_plot)
data_plot$type = as.character(data_plot$type)
ggplot(data_plot)+ 
  geom_line(aes(year, ss_0, group = type , col = type ),linetype = 1, linewidth = 1)+
  geom_line(aes(year, ss_1.1, group = type, col = type ), linetype = 2, linewidth = 1)+
  geom_line(aes(year, ss_1.2, group = type, col = type ), linetype = 3, linewidth = 1)+
  geom_line(aes(year, ss_1.3, group = type, col = type ), linetype = 4, linewidth = 1)+
  theme_bw()

#plot(c(), type = 'l')
plot(MC_ZUIDI_0)
plot(NUMBER_0)
for(i in catastrophe ){
  lines(get(paste0('NUMBER_', i)), type = 'l')
}
for(i in 2:length(price_decline)){
  price_decline[i] = price_decline[i-1] * price_decline[i]
}
plot(price_decline, type = 'l')


plot(rowSums(FV_risk_before - FV_risk), type = 'l')

plot(RHO_SQUARE, type = 'l')
plot(LR_SUM, type = 'l')
plot(NUMBER_1.3, type = 'l')
plot(EQUITY_RATE, type = 'l')

for(i in 2:length(price_decline)){
  price_decline[i] = price_decline[i-1] * price_decline[i]
}
plot(price_decline, type = 'l')
plot(MC_ZUIDI_1.3, type = 'l')
rowSums(MC_ZUIDI_0)



for(i in 2:length(PRICE_DECLINE_1.3)){
  PRICE_DECLINE_1.3[i] = PRICE_DECLINE_1.3[i] * PRICE_DECLINE_1.3[i-1]
}
plot(PRICE_DECLINE_1.3, type = 'l')

for(i in 2:length(PRICE_DECLINE_0[,1])){
  PRICE_DECLINE_0[,1][i] = PRICE_DECLINE_0[,1][i] * PRICE_DECLINE_0[,1][i-1]
}
plot(PRICE_DECLINE_0[,1], type = 'b', col = 'red')


plot(MC_ZUIDI_0[,1], type = 'l')
lines(MC_ZUIDI_1.3[,1], type = 'l', col = 'red')

plot(RHO_SQUARE_0[,1], type = 'l')
lines(RHO_SQUARE_1.3[,1], type = 'l')


plot(RETURN_0[,1], type = 'l')
lines(RETURN_1.3[,1], type = 'l', col = 'red')

plot(NUMBER_0[,1], type = 'l')
lines(NUMBER_1.3[,1], type = 'l', col = 'red')



plot(-0.0332*(log(100000000:200000)) + 0.576, type = 'l')

