
library(tidyverse)
setwd("F://保险数据国泰安")

# 按下快捷键 Ctrl + Shift + C  批量注释
# write.csv(data$Conm[which(data$Sgntime==2023)],
#           "jjj.csv",
#           fileEncoding = 'gbk')



## 读取2023年公司名单
company <- read.csv('company.csv', fileEncoding = 'gbk')
year <- 2016:2023
## 生成2016:2023年全公司列表 temp 临时
temp <- tibble(Conm = rep(company$Conm, 8),
               Sgntime = rep(year, each = 85))

## 读取资产数据 
asset <- read.csv("各保险公司资产负债表(二)153453816(仅供vip使用)/INS_Combas2.csv")
## 资产数据与公司名单合并
asset <- left_join(temp, asset)
## 去掉总资产为NA的公司值  
##  全公司列表  有一些公司早年间还没有成立
asset <- filter(asset, is.na(Combas0236) == F) 
asset$Combas0236[179] = 1197.55



## 读取损益表数据
profit <- read.csv('各保险公司损益表(二)153605326(仅供vip使用)/INS_Compal2.csv',
                   fileEncoding = 'UTF-8')
names(temp) [1] <- names(profit)[2]
profit = left_join(temp, profit)


##  读取保险公司业务表
operation <- read.csv('各财产保险分公司业务统计表153907139(仅供vip使用)/INS_Prtbus.csv',
                      fileEncoding = 'UTF-8')
names(temp)[1] <- names(operation)[2]
opera <- left_join(temp, operation)


names(profit)[1] <- 'Conm'
data <- left_join(asset, profit)
names(opera)[1] <- 'Conm'
data <- left_join(data, opera)

## 对于保险公司的保费收入
## 如果某一险种的收入是NA,把它赋值为0
cols_to_replace <- c("Prtbus1003", "Prtbus1004", "Prtbus1005", "Prtbus1006", "Prtbus1007",
                     "Prtbus1008", "Prtbus1009", "Prtbus1010", "Prtbus1011")
data[cols_to_replace] <- lapply(data[cols_to_replace], function(x) ifelse(is.na(x), 0, x))



