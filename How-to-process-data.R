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

#p96