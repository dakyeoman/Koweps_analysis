#Understanding data frame
#Row or Case : 행(정보) / Column or Variable : 열(속성)  

en <- c(90, 80, 60, 70) 
mth <- c(50, 60, 100, 20)
df_midterm <- data.frame(en, mth)

class <- c(1, 1, 2, 2)
df_midterm <- data.frame(en, mth, class)

mean(df_midterm$en)
mean(df_midterm$mth)

#all in one.
df_midterm <- data.frame(en = c(90, 80, 60, 70),
                         mth = c(50, 60, 100, 20), 
                         class = c(1, 1, 2, 2))

install.packages("readxl")
library(readxl)

#read_excel은 엘셀 파일을 데이터 프레임으로 만든다.
#xl 1st Row should be variable. or you wil lost it
df_exam <-read_excel(file.choose()) #excel_exam.xlsx

mean(df_exam$english)
mean(df_exam$science)

#첫 행부터 데이터가 시작되는 파일의 경우
#"col_names(열 이름을 가져올 것인가?) = F(거짓)"를 추가
#이때 T/F는 논리형 벡터(T/F, TRUE/FALSE)
# >>> 첫번째 행을 데이터로 인식해 불러오고 변수명은 '...숫자'로 자동 지정됨
#
#엑셀파일의 세 번째 시트만 불러오기
df_csv_exam <-read.csv(file.choose())

#p96 9/28
#RDS : R전용 데이터 파일
saveRDS(df_midterm, file = "df_midterm.rds")
rm(df_midterm) #데이터 삭제
df_midterm <- readRDS("df_midterm.rds")

#Modify data
exam <- read.csv(file.choose()) #csv_exam.csv
head(exam) #앞에서부터  6행 출력
head(exam, 10) #첫 10행 출력
tail(exam) #뒤에서부터 6행 출력    
View(exam)
dim(exam) #행/열 출력 
str(exam) #모든 변수들의 속성 파악
summary(exam) #요약 통계량 출력

#install.packages("ggplot2")    
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg) #::로 특정 패키지의 함수, 데이터 저장 
View(mpg)
dim(mpg)
str(mpg) #num:실수, int:정수
summary(mpg)

#Change Variable name 
library(dplyr)
df_raw <- data.frame(var1 = c(1, 2, 1), var2 = c(2, 3, 2))
df_new <- df_raw #make copy
df_new <- rename(df_new, v2 = var2) #rename(df, 새 변수명 = 기존 변수명)

mpg_new <- mpg
mpg_new <- rename(mpg_new, city = cty, highway = hwy)

#Make Derived variable 
# - 변수 조합
df <- data.frame(var1 = c(4, 3, 8), 
                 var2 = c(2, 6, 1))
df$var_sum <- df$var1 + df$var2 

mpg$total <- (mpg$cty + mpg$hwy) / 2
head(mpg)

# - 조건문 활용(기준점): 합격 판정 변수 생성
summary(mpg$total)
hist(mpg$total) 
mpg$test <- ifelse(mpg$total >= 20, "pass", "fail") #20 이상 통과로 가정
head(mpg)

table(mpg$test) # (P/F)빈도표
qplot(mpg$test) # 막대그래프 

#중첩조건문 - 2개 이상 범주 생성
mpg$grade <- ifelse(mpg$total >= 30, "A",
                    ifelse(mpg$total >= 25, "B",
                           ifelse(mpg$total >= 20, "C", "D"))) 
head(mpg, 20) #확인
table(mpg$grade)
qplot(mpg$grade)

#page123 practice
midwest_raw <- as.data.frame(ggplot2::midwest)
midwest <- midwest_raw
summary(midwest)

midwest <- rename(midwest, total = poptotal, asian = popasian)
midwest$asian_percentage <- midwest$asian / midwest$total
hist(midwest$asian_percentage)
summary(midwest$asian_percentage)
mean <- mean(midwest$asian_percentage)

midwest$asian_size <- ifelse(midwest$asian_percentage >= mean, "large", "smaill" )
table(midwest$asian_percentage)
qplot(midwest$asian_percentage)
qplot(midwest$asian_size)

#<6-5. mutate()로 파생변수 추가>
exam %>%
  mutate(total = math + english + science) %>% 
  head

