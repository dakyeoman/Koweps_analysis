
install.packages("foreign")
install.packages("ggplot2")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

install.packages("haven")
library(haven)

raw_welfare <- read.spss("desktop/Koweps_hpc10_2015_beta1.sav", to.data.frame = T)
welfare <- raw_welfare

head(welfare)
tail(welfare)
#View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

welfare <- rename(welfare, 
                  sex = h10_g3, 
                  birth = h10_g4, 
                  marriage = h10_g10, 
                  religion = h10_g11, 
                  income = p1002_8aq1, 
                  code_job = h10_eco9, 
                  code_region = h10_reg7)

#성별 변수 검토, 전처리
data %>% dplyr::summarise()#모르겠다!

class(welfare$sex)

table(welfare$sex)
#이상치 결측 처리: 
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex)

#성별 항목 이름 부여
welfare <- ifelse(welfare$sex == 1, "male", "female")
table(welfare)
qplot(welfare)

is.recursive(welfare)
class(welfare$income)

x <- c(1, 2)
x
names(x) <- c("bob", "ed")
x$ed
is.atomic(x) #T
is.recursive(x) #F

x["ed"] #2

