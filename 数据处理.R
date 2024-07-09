
rm(list = ls())
gc()
library(tidyverse)



### 2010
library(readxl)

for (x in 2002:2009){
  setwd(paste("F:/patent_data/",x, sep = ''))
  Xdata <- read_excel(paste(x,".xlsx", sep = ''))
  names(Xdata)[5] <- c("patent")
  names(Xdata)[7] <- c("province")
  names(Xdata)[8] <- c("text")
  Xdata <- Xdata %>% filter(patent  != 3) %>% #filter(province  == 33) %>%
    filter(is.na(text) == F)
  for(i in 1:nrow(Xdata)){
    Xdata[i,8] = gsub(" ", ",", Xdata[i,8])
  }
  write.csv(Xdata, paste(x, "_after.csv", sep = ""), fileEncoding = "gbk")
}

for (x in 2002:2009){
  setwd(paste("F:/patent_data/",x, sep = ''))
  Xdata <- read.csv(paste(x,"_after.csv.", sep = ''), fileEncoding = 'utf-8')[,-1]
  data <- Xdata
  if(nrow(data)%/%4992 != 0){
    for(i in 1:(nrow(data)%/%4992)){
      
      data_sep = data[((i-1)*4992+1):(i*4992),]
      write.csv(data_sep, file = paste(x,"_after_",i,".csv", sep = ""), fileEncoding = "utf-8")
    }
  }
  if (nrow(data)%%4992 != 0 ){
    data_tail = data[((nrow(data)%/%4992)*4992+1):nrow(data),]
    h = nrow(data)%/%4992+1
    write.csv(data_tail, file = paste(x,"_after_",h,".csv", sep = ""), fileEncoding = "utf-8")
  }
}


x = 2002
data <- read.csv("2022_after_train_32.csv", fileEncoding = 'gbk')
data_1 <- read.csv('2022_after_train_47.csv', fileEncoding = 'gbk')
jj <- rbind(data, data_1)
jj


write.csv(jj, 'data_trained_full.csv', fileEncoding = 'gbk')

data_1[69936,]
jj[229680,]


32*4992+90*16


10+19+39+12




x = 2021


setwd('F://patent_data/2021')
data_1 <- read.csv("2021_2_after_train_32.csv", fileEncoding = "gbk")
data_2 <- read.csv("2021_2_after_train_47.csv", fileEncoding = "gbk")

data <- rbind(data_1, data_2)
write.csv(data[,-1], file = '2021_2_after_train_full.csv',
          fileEncoding = 'gbk')



### 2011
library(readxl)
X2011 <- read_excel("2011.xlsx")
names(X2011)[5] <- c("patent")
names(X2011)[7] <- c("province")
names(X2011)[8] <- c("text")
X2011 <- X2011 %>% filter(patent  != 3) %>% #filter(province  == 33) %>%
  filter(is.na(text) == F)
write.csv(X2011, "2011_after.csv", fileEncoding = "gbk")

### 2012
library(readxl)
X2012 <- read_excel("2012.xlsx")
names(X2012)[5] <- c("patent")
names(X2012)[7] <- c("province")
names(X2012)[8] <- c("text")
X2012 <- X2012 %>% filter(patent  != 3) #%>% filter(province  == 33) %>%
  filter(is.na(text) == F)
write.csv(X2012, "2012_after.csv", fileEncoding = "gbk")

### 2013
library(readxl)
X2013 <- read_excel("2013.xlsx")
names(X2013)[5] <- c("patent")
names(X2013)[7] <- c("province")
names(X2013)[8] <- c("text")
X2013 <- X2013 %>% filter(patent  != 3) #%>% filter(province  == 33) %>%
  filter(is.na(text) == F)
write.csv(X2013, "2013_after.csv", fileEncoding = "gbk")


### 2014
library(readxl)
X2014 <- read_excel("2014.xlsx")
names(X2014)[5] <- c("patent")
names(X2014)[7] <- c("province")
names(X2014)[8] <- c("text")
X2014 <- X2014 %>% filter(patent  != 3) %>% filter(province  == 33) %>%
  filter(is.na(text) == F)
write.csv(X2014, "2014_after.csv", fileEncoding = "gbk")

### 2015
library(readxl)
X2015 <- read_excel("2015.xlsx")
names(X2015)[5] <- c("patent")
names(X2015)[7] <- c("province")
names(X2015)[8] <- c("text")
X2015 <- X2015 %>% filter(patent  != 3) %>% filter(province  == 33) %>%
  filter(is.na(text) == F)
write.csv(X2015, "2015_after.csv", fileEncoding = "gbk")


### 2016
library(readxl)
X2016 <- read_excel("2016.xlsx")
names(X2016)[5] <- c("patent")
names(X2016)[7] <- c("province")
names(X2016)[8] <- c("text")
X2016 <- X2016 %>% filter(patent  != 3) %>% filter(province  == 33) %>%
  filter(is.na(text) == F)
write.csv(X2016, "2016_after.csv", fileEncoding = "gbk")

### 2017
library(readxl)
X2017 <- read_excel("2017.xlsx")
names(X2017)[5] <- c("patent")
names(X2017)[7] <- c("province")
names(X2017)[8] <- c("text")
X2017 <- X2017 %>% filter(patent  != 3) %>% filter(province  == 33) %>%
  filter(is.na(text) == F)
write.csv(X2017, "2017_after.csv", fileEncoding = "gbk")

### 2018
library(readxl)
X2018 <- read_excel("2018.xlsx")
names(X2018)[5] <- c("patent")
names(X2018)[7] <- c("province")
names(X2018)[8] <- c("text")
X2018 <- X2018 %>% filter(patent  != 3) %>% filter(province  == 33) %>%
  filter(is.na(text) == F)

write.csv(X2018, "2018_after.csv", fileEncoding = "gbk")


### 2019
library(readxl)
X2019 <- read_excel("2019.xlsx")
names(X2019)[5] <- c("patent")
names(X2019)[7] <- c("province")
names(X2019)[8] <- c("text")
X2019 <- X2019 %>% filter(patent  != 3) %>% filter(province  == 33) %>%
  filter(is.na(text) == F)
write.csv(X2019, "2019_after.csv", fileEncoding = "gbk")


### 2020_1
library(readxl)
X2020_1 <- read_excel("2020_1.xlsx")
names(X2020_1)[5] <- c("patent")
names(X2020_1)[7] <- c("province")
names(X2020_1)[8] <- c("text")
X2018 <- X2020_1 %>% filter(patent  != 3) %>% filter(province  == 33) %>%
  filter(is.na(text) == F)
write.csv(X2020_1, "2020_1_after.csv", fileEncoding = "gbk")


### 2020_2
library(readxl)
X2020_2 <- read_excel("2020_2.xlsx")
names(X2020_2)[5] <- c("patent")
names(X2020_2)[7] <- c("province")
names(X2020_2)[8] <- c("text")
X2020_2 <- X2020_2 %>% filter(patent  != 3) %>% filter(province  == 33) %>%
  filter(is.na(text) == F)
write.csv(X2020_2, "2020_2_after.csv", fileEncoding = "gbk")

### 2021_1
library(readxl)
X2021_1 <- read_excel("2021_1.xlsx")
names(X2021_1)[5] <- c("patent")
names(X2021_1)[7] <- c("province")
names(X2021_1)[8] <- c("text")
X2021_1 <- X2021_1 %>% filter(patent  != 3) %>% filter(province  == 33) %>%
  filter(is.na(text) == F)
write.csv(X2021_1, "2021_1_after.csv", fileEncoding = "gbk")

### 2021_2
library(readxl)
X2021_2 <- read_excel("2021_2.xlsx")
names(X2021_2)[5] <- c("patent")
names(X2021_2)[7] <- c("province")
names(X2021_2)[8] <- c("text")
X2021_2 <- X2021_2 %>% filter(patent  != 3) %>% filter(province  == 33) %>%
  filter(is.na(text) == F)
write.csv(X2021_2, "2021_2_after.csv", fileEncoding = "gbk")

### 2022_1
library(readxl)
X2022_1 <- read_excel("2022_1.xlsx")
names(X2022_1)[7] <- c("patent")
names(X2022_1)[11] <- c("province")
names(X2022_1)[12] <- c("text")
X2022_1 <- X2022_1 %>% filter(patent  != 3) %>% filter(province  == 33) %>%
  filter(is.na(text) == F)
write.csv(X2022_1, "2022_1_after.csv", fileEncoding = "gbk")

### 2022_2
library(readxl)
X2022_2 <- read_excel("2022_2.xlsx")
names(X2022_2)[7] <- c("patent")
names(X2022_2)[11] <- c("province")
names(X2022_2)[12] <- c("text")
X2022_2 <- X2022_2 %>% filter(patent  != 3) %>% filter(province  == 33) %>%
  filter(is.na(text) == F)
write.csv(X2022_2, "2022_2_after.csv", fileEncoding = "gbk")


