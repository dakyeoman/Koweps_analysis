
install.packages("foreign")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readxl")
install.packages("haven") 

library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
library(haven)

raw_welfare <- read.spss("desktop/Koweps_hpc10_2015_beta1.sav", to.data.frame = T)
welfare <- raw_welfare
welfare2 <- as.data.frame(welfare)
class(raw_welfare)

head(welfare)
tail(welfare)
#View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

welfare <-rename(welfare, 
       sex = h10_g3,
       birth = h10_g4, 
       marriage = h10_g10, 
       religion = h10_g11, 
       income = p1002_8aq1, 
       code_job = h10_eco9, 
       code_region = h10_reg7)

#welfare.data <- as.data.frame(welfare)
#x <-data.frame(welfare)
#class(x)
#성별 변수 검토, 전처리
#data %>% dplyr::summarise()#모르겠다!

class(welfare$sex)

#이상치 결측 처리: 
table(welfare$sex)
#welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)
#table(is.na(welfare$sex))


#성별 항목 이름 부여
welfare <- ifelse(welfare$sex == 1, "male", "female")
table(welfare)
qplot(welfare)


#자 난 이걸 dataframe으로 바꿀거야. 
#class(welfare) #문자열
#welfare <-as.data.frame(welfare)
#class(welfare)
#안 됨.

welfare$income
is.recursive(welfare)
class(welfare$income)
welfare["income"]

#(위) 왜 $로 찾을 수가 없지? 월급변수 검토 과정 일단 생략해보자. 
#$ operater invalid오류를 해결하기 위한 몸부림(아래) 

x <- c(1, 2)
x
names(x) <- c("bob", "ed")
x$ed
is.atomic(x) #T
is.recursive(x) #F

x["ed"] #2
getElement(x, "ed")
x <- as.data.frame(x)
x$ed 
#어???
x <- c(1, 2)
x
names(x) <- c("bob", "ed")
x <- as.data.frame(t(x))
x$ed
class(x)

#안 되겠다 기초부터 해야 쓰겠다.

