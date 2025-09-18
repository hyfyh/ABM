
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
asset$Combas0236[179] = 1197.55 ## 这个值 原始数据有错误 修改


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







### 算业务rf0  ############################################
## 首先将业务和rf0基准值对应相
# 设置原始行和列名
#原始名称 <- c("MC车险", "MC财产险", "MC船货特险", "MC责任险", "MC农业险", "MC信用保证险", "MC短意险", "MC短健险", "MC短期寿险", "MC其他险")
# 创建原始相关系数矩阵
underwriting_risk <- matrix(c(
  1,    0,    0.15,  0.3,   0,    0,    0.3,   0.25,  0.2,   0,
  0,    1,    0.4,   0.35,  0.35, 0.05, 0.35,  0,     0,     0,
  0.15, 0.4,  1,     0.25,  0.1,  0.05, 0.3,   0.05,  0,     0,
  0.3,  0.35, 0.25,  1,     0.15, 0,    0.55,  0.15,  0.25,  0,
  0,    0.35, 0.1,   0.15,  1,    0,    0.2,   0.1,   0,     0,
  0,    0.05, 0.05,  0,     0,    1,    0,     0,     0,     0,
  0.3,  0.35, 0.3,   0.55,  0.2,  0,    1,     0.25,  0.5,   0,
  0.25, 0,    0.05,  0.15,  0.1,  0,    0.25,  1,     0.5,   0,
  0.2,  0,    0,     0.25,  0,    0,    0.5,   0.5,   1,     0,
  0,    0,    0,     0,     0,    0,    0,     0,     0,     1
), nrow = 10, byrow = TRUE)

# 定义新顺序的索引：基于调整后的对应关系
新顺序索引 <- c(2, 1, 3, 4, 6, 5, 8, 7, 10)  # 对应新顺序的原始索引 并且去掉了短期寿险

# 重排矩阵
underwriting_risk <- underwriting_risk[新顺序索引, 新顺序索引]


### 导出2023年的财险公司业务数据
underwriting = data %>% filter(Sgntime == 2023) 
underwriting = underwriting[, c('Conm', cols_to_replace)]

## 财险公司的各业务占比
underwriting <- underwriting[rowSums(underwriting[,-1]) > 0, ]
 
Conm = underwriting$Conm
underwriting <- underwriting[, -1] / rowSums(underwriting[,-1])

## 各险种的rf0
RF0_prop = c(0.391, 0.103, 0.232, 0.203, 0.467, 0.326, 0.115, 0.122, 0.098)

RF0_underwriting = c()
for(i in 1:length(RF0_prop)){
  RF0_underwriting = cbind(RF0_underwriting, 
                           underwriting[, i] * RF0_prop[i])
}

RF0_prop_rev = RF0_underwriting * 0.3 ## 准备金
RF0_underwriting = sqrt(RF0_underwriting ^ 2 + 2 * 0.5 * RF0_underwriting * RF0_prop_rev + RF0_prop_rev^2)
mc_rf0_dd = c()
for(t in 1:nrow(RF0_underwriting)){
  mc_rf0_dd = c(mc_rf0_dd, 
                t(as.numeric(RF0_underwriting[t,])) %*% as.matrix(underwriting_risk) %*% (as.numeric(RF0_underwriting[t,])))
} 

mc_rf0_data = tibble(Conm = Conm,
                     mc_rf0 = mc_rf0_dd ^ 0.5)   


##  这里的Conm 是最终的名单
data_raw <- tibble(Conm = Conm,
                    Sgntime = 2023)
data_raw <- left_join(data_raw, data)

data_full <- data_raw %>% dplyr::select(Conm,
                                        Combas0236,## 资产总计
                                          Combas0267, ## 负债合计
                                          Combas0277, ## 所有者权益
                                          Compal0103, ## 已赚保费
                                          Prtbus1002,
                                          Compal0117) ## 保费收入合计

data_full <- left_join(data_full, beta0)
data_full <- na.omit(data_full)                             

