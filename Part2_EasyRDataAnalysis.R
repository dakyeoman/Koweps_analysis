#데이터 전처리 Data Preprocessing
library(dplyr)
#----------dplyr Package----------
# - filter():행 추출
# - select():열(변수) 추출
# - arrange():정렬
# - mutate():변수추가
# - summarise():통계치 산출
# - group_by(): 집단별로 나누기
# - left_join():데이터 합치기(열) 
# - bind_join():데이터 합치기(행)

#<6-2.조건에 맞는 데이터만 추출>
library(dplyr)
exam <- read.csv(file.choose()) #csv_exam.csv
exam %>% filter(class == 1) #class가 1인 경우만 출력
exam %>% filter(class != 1) #class가 1이 아닌 경우만 출력
exam %>% filter(math > 50)
exam %>% filter(math > 50 & english > 50) 
exam %>% filter(math > 90 | english > 85 | science >= 90) #'|' = or
exam %>% filter(class %in% c(1, 3, 5)) #%in%: 변수값 조건 지정
class1 <- exam %>% filter(class == 1)
mean(class1$math)

#<p133 practice>
mpgdata <- as.data.frame(ggplot2::mpg) 
mpg <- mpgdata

#배기량-연비 비교
displ_a <- mpg %>% filter(displ <= 4)
displ_b <- mpg %>% filter(displ >= 5)
mean(displ_a$hwy)
mean(displ_b$hwy)

#제조사-도시연비(cty) 비교
dspl_audi <- mpg %>% filter(manufacturer == "audi")
mean(dspl_audi$cty)
dspl_toyota <- mpg %>% filter(manufacturer == "toyota")
mean(dspl_toyota$cty)


#제조사-고속도로 평균 연비(hwy) 
displ_chevrolet <- mean((mpg %>% filter(manufacturer == "chevrolet"))$hwy)
displ_ford <- mean((mpg %>% filter(manufacturer == "ford"))$hwy)
displ_honda <- mean((mpg %>% filter(manufacturer == "honda"))$hwy)

# >>> %in%으로 더 쉽게. 
mnf <- mpg %>% filter(manufacturer %in% c("chevorlet", "ford", "honda"))
mean(mnf$hwy) ##22.85294


#<6-3. select()로 필요한 변수(행)만 추출하기>
exam %>% select(class, math, english)
exam %>% select(-english, -class) #변수 제외

#dplyr 함수 조합 w/ '%>%'
exam %>% 
  filter(class == 1) %>% 
  select(english)
exam %>% 
  select(class, english) %>% 
  head(10)

#practice p138 #추출
mpg_q1 <- mpg %>% select(class, cty)
head(mpg_q1) 

suv <- mpg_q1 %>% filter(class == "suv") 
compact <- mpg_q1 %>% filter(class == "compact") 
mean(suv$cty) > mean(compact$cty) #FALSE

#<6-4.arrange()로 데이터 정렬>
exam %>% arrange(math) #math 변수 오름차순 정렬 
exam %>% arrange(desc(english)) #english 변수 내림차순 정렬 
exam %>% arrange(class, math) 

#p141 practice: audi가 생산한 자동차 중 hwy 상위 5개 제품 추출
mpg %>% filter(manufacturer == "audi") %>% 
  arrange(desc(hwy)) %>% 
  head(5)

#<6-5. mutate()로 파생변수 추가>
exam %>%
  mutate(total = math + english + science, 
         mean = (math + english + science)/3) %>% 
  arrange(desc(total, mean)) %>% 
  head

exam %>%
  mutate(scn_test = ifelse(science >= 60, "pass", "fail"))%>% 
  head

#p144 practice
mpg2 <-mpg
mpg2 %>% 
  mutate(ttl_mileage = cty + hwy, 
         avr_mileage = ttl_mileage/2) %>%
  arrange(desc(avr_mileage, ttl_mileage)) %>%
  head(3)

#<6-6. summarise()로 집단별 요약하기>
exam %>% 
  group_by(class) %>% #class별로 분리 
  summarise(mean_math = mean(math)) #math 평균 산출

exam %>%
  group_by(class) %>%
  summarise(mean_math = mean(math), 
            sum_math = sum(math), 
            median_math = median(math), 
            sd_math = sd(math),
            n = n()) #n(): 데이터의 빈도(몇 행인가)를 구함, sd():표준편차
mpg %>%
  group_by(manufacturer, drv) %>% #회사별, 구동방식별 분리
  summarise(mean_cty = mean(cty)) %>% #cty 평균 산출
  head(5)

#dplyr 조합
mpg %>% 
  group_by(manufacturer) %>%
  filter(class == "suv") %>% #"suv"추출
  mutate(tot = (cty + hwy)/2) %>% #통합 연비 변수 생성
  summarise(totm = mean(tot)) %>% #통합 연비 평균 산출 
  arrange(desc(totm)) %>%
  head(5)

#p150 practice
mpg %>%
  group_by(class) %>%
  summarise(meancty = mean(cty)) 

mpg %>%
  group_by(class) %>%
  summarise(mean_cty = mean(cty)) %>%
  arrange(desc(mean_cty))

mpg %>%
  group_by(manufacturer) %>%
  summarise(mean_hwy = mean(hwy)) %>%
  arrange(desc(mean_hwy)) %>%
  head(3)

#q4
mpg %>%
  group_by(manufacturer) %>%
  filter(class == "compact") %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#<6-7. 행/열 데이터 수합>
#left_join()으로 행(가로)데이터 합치기
mid <- data.frame(id = c(1, 2, 3, 4, 5), 
                  midterm = c(60, 80, 70, 90, 85))
fin <- data.frame(id = c(1, 2, 3, 4, 5), 
                  final = c(70, 83, 65, 95, 80))
total <- left_join(mid, fin, by = "id") #id를 기준으로 합쳐 total에 할당
total   

#다른 데이터 변수 추가
name <- data.frame(class = c(1, 2, 3, 4, 5),
                   teacher = c("Kim", "Choi", "Jung", "Lee", "Ahn")) #name 데이터 생성 
exam_new <- left_join(exam, name, by = "class")
exam_new

#bind_rows()로 열(세로)데이터 수합: 합치는 데이터의 변수명이 같아야 한다 *다를 경우 rename()활용 
group_a <- data.frame(id = c(1, 2, 3, 4, 5), 
                  test = c(60, 80, 70, 90, 85))
group_b <- data.frame(id = c(6, 7, 8, 9, 10), 
                  test = c(70, 83, 65, 95, 80))
group_all <- bind_rows(group_a, group_b)

#p157 practice
fuel <- data.frame(fl = c("c", "d", "e", "p", "r"), 
                   price_fl = c(2.35, 2.38, 2.11, 2.76, 2.22))
new_mpg <- left_join(fuel, mpg, by = "fl") 
new_mpg %>%
  select(model, fl, price_fl) %>%
  head(5)

#p160 practice
midwest_copy <- ggplot2::midwest
midwest <- midwest_copy

#q1-2
midwest %>% 
  mutate(perckids = 100 - popadults / poptotal * 100) %>%
  select(county, perckids) %>%
  arrange(desc(perckids)) %>%
  head(5)

#q3 등급 분류 & 각 등급에 속한 지역의 수 
midwest %>%
  mutate(perckids = 100 - popadults / poptotal * 100) %>%
  mutate(grade = ifelse(perckids >= 40, "large",
                        ifelse(perckids >=30, "middle",
                               ifelse(perckids < 30, "small", NA)))) %>%
  select(county, grade) %>%
  group_by(grade) %>%
  summarise(n = n())

#q4 아시아인 인구 / 전체 인구 백분율 변수 추가, 하위 10개 지역의 state, county의 아시아인 인구 백분률
midwest %>%
  mutate(percasian = popasian / poptotal * 1000) %>%
  select(state, county, percasian) %>%
  arrange(desc(percasian)) %>5
#앗 안 돌아간다. 


#데이터 정제 211019-----------------------------------------------
#7-1 결측치<NA> 정제
df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA)) ;df

#결측치 확인 
is.na(df) 
table(is.na(df)) #결측치 빈도 출력
table(is.na(df$sex))
table(is.na(df$score))
#결측치가 포함된 데이터는 정상적 연산 불가능
mean(df$score) 
sum(df$score) 

#결측치 제거하기;is.na()를 filter()에 적용 -> 결측치가 있는 행을 제거
library(dplyr)

df %>% filter(is.na(score)) #score = NA 인 데이터만 출력
df %>% filter(!is.na(score)) #score 결측치 제거;결측치가 아닌 행만 출력

df_no_miss <- df %>% filter(!is.na(score)) 
mean(df_no_miss$score)
sum(df_no_miss$score)

df_no_miss <- df %>% filter(!is.na(score) & !is.na(sex)) ;df_no_miss
#여러 변수 동시에 결측치 제거된 데이터 추출 

df_no_miss2 <- na.omit(df) ;df_no_miss2 #모든 변수에 결측치 없는 데이터 추출
#na.omit():변수 지정하지 않고 결측치가 있는 행을 한 번에 제거, but 분석에 필요한 행까지 손실될 수 있다. 

#수치 연산 함수의 결측치 제외기능; na.rm 파라미터 = TRUE
mean(df$score, na.rm = T)
sum(df$score, na.rm = T)
exam <- read.csv(file.choose()) #csv_exam.csv
exam[c(3, 8, 15), "math"] <- NA ;exam #[행 위치, 열 위치]:데이터 위치 지칭, / 3, 8, 15행의 math열에 NA 할당
exam %>% summarise(mean_math = mean(math)) #현재 결측치 포함됨 -> NA
exam %>% summarise(mean_math = mean(math, na.rm = T), 
                   sum_math = sum(math, na.rm = T), 
                   median_math = median(math, na.rm = T))

#결측치 대체법Imputation; 데이터 손실로 분석 결과가 왜곡되는 문제 보완 가능
#평균값으로 결측치 대체하기(앞에서 exam데이터의 [c(3, 8, 15), math] = NA)
mean(exam$math, na.rm = T) #1.평균값
exam$math <- ifelse(is.na(exam$math), 55, exam$math) #math=NA면 55로 대체, 그렇지 않으면 원래의 값.
table(is.na(exam$math)) #결측치 빈도표 생성 (*결측치 없음)
exam
mean(exam$math)

#p170문제 해결
mpg <- as.data.frame(ggplot2::mpg)
mpg[c(65, 124, 131, 153, 212), "hwy"] <- NA #NA 할당 

#q1결측치 확인
is.na(mpg$drv) 
table(is.na(mpg$drv))#결측치 없음
is.na(mpg$hwy) 
table(is.na(mpg$hwy))#결측치 5개

#q2 filter()로 hwy변수의 결측치 제외- 어떤 구동 방식의 hwy 평균이 높은지 하나의 dplyr구문으로 구성
mpg %>% 
  filter(!is.na(hwy)) %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy)) #hwy별 평균 구하기


#7-2 이상치 정제하기
outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1), 
                      score = c(5, 4, 3, 4, 2, 6)) ;outlier
#이상치 확인
table(outlier$sex)
table(outlier$score)

#결측처리
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex);outlier
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score);outlier

#분석 시 결측치 제외
outlier %>%
  filter(!is.na(sex) & !is.na(score)) %>%
  group_by(sex) %>% #성별 분리
  summarise(mean_score = mean(score)) #평균 산출

#Box plot 으로 이상치 제거 
boxplot(mpg$hwy)

boxplot(mpg$hwy)$stats 
#아랫쪽 극단치 경계, Q1, median, Q3, 위쪽 극단치 경계(q1, q3 밖 1.5 IQR 내 최댓값)
#이때 [n,]는 매트릭스 데이터 구조의 인덱스 값

mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)
table(is.na(mpg$hwy))
#Q1 - Q3 밖의 값을 결측 처리

mpg %>% 
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy, na.rm = T))
#결측치 제외한 drv별 hwy 평균



#p178 문제해결-------------------------------------------------------------
mpg <- as.data.frame(ggplot2::mpg)
#(이상치 만들기)
mpg[c(10,14,58,93), "drv"] <- "k" #hwy 이상치 할당
mpg[c(29,43,129,203), "cty"] <- c(3, 4, 39, 42) #cty 이상치 할당

#Q1. drv 이상치 확인 - 결측처리 후 재확인 
table(mpg$drv) #1. 이상치 확인
mpg$drv <- ifelse(mpg$drv %in% c("k"), NA, mpg$drv) #drv 중 "k"값을 NA로 변환 
#or mpg$drv <- ifelse(mpg$drv %in% c(4, "f", "r"), mpg$drv, NA) 
table(mpg$drv) #결측처리됨 

#Q2. cty 이상치 확인 
boxplot(mpg$cty)$stats
mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)
boxplot(mpg$cty)

#Q3. 이상치 제외후 drv별 cty 평균 (dplyr구문)
mpg %>%
  filter(!is.na(drv) & !is.na(cty)) %>% #결측치 제외
  group_by(drv) %>% #dpr별 분리
  summarise(mean_cty = mean(cty)) #cty 평균



#Part 3 그래프 211104 ----------------------------------------
#1. 산점도 geom_point()
library(ggplot2) 
ggplot(data = mpg, aes(x = displ, y = hwy)) + #데이터, 축 설정
  geom_point() + #그래프 종류(산점도)
  xlim(3, 6) + #x축 범위 설정 3~6
  ylim(10, 30) #y축 범위 설정 10~30

#p188 문제해결
#Q1.mpg데이터의 cty와 hwy 산점도 그리기 
ggplot(data = mpg, aes(x = cty, y = hwy)) + 
  geom_point()

#Q2.미국 지역별 인구통계 정보 midwest - 전체 인구(poptotal)와 아시아인(popasian) 인구 관계
ggplot(data = midwest, aes(x = poptotal, y = popasian)) + 
  geom_point() + 
  xlim(0, 500000) + 
  ylim(0, 10000)




#2.평균막대그래프(qplot대체) geom_col()
#구동 방식별 평균 고속도로 연비  
library(dplyr)

df_mpg <- mpg %>%
  filter(!is.na(drv) & !is.na(cty)) %>%
  group_by(drv) %>%
  summarise(mean_hwy = mean(hwy));df_mpg

ggplot(data = df_mpg, aes(x = drv, y = mean_hwy)) + geom_col() #그래프 생성
ggplot(data = df_mpg, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col() 
#reorder(x축 변수, -(내림차순)정렬 기준 변수)

#3.빈도막대그래프: geom_bar(x축 연속변수만 지정)
ggplot(data = mpg, aes(x = drv)) + geom_bar() #drv 변수 항목별 빈도 막대 그래프 생성 
ggplot(data = mpg, aes(x = hwy)) + geom_bar()

#***평균막대그래프(geom_col)은 데이터 요약표로 만든 그래프, 빈도막대그래프(geom_bar)는 원자료로 만든 그래프 

#p193 문제해결
#Q1. "suv"차종에서 평균 도시 연비(cty)가 가장 높은 회사 다섯 곳을 막대그래프로 표현해보자 

df_suv <- mpg %>% 
  filter(class == "suv") %>% 
  group_by(manufacturer) %>%
  summarise(cty_mean = mean(cty)) %>%
  arrange(desc(cty_mean)) %>%
  head(5) #상위 5개 
df_suv 

ggplot(data = df_suv, aes(x = reorder(manufacturer, -cty_mean), y = cty_mean)) + geom_col()

#Q2. class 빈도그래프
ggplot(data = mpg, aes(x = class)) + geom_bar()


#4. 시계열그래프(선): x축에 시간, geom_line()
ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line()    
ggplot(data = economics, aes(x = date, y =psavert)) + geom_line()  

#Boxplox 상자 그림
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot() #drv별 hwy

#p198 문제 해결
cty_c <- mpg %>%
  filter(class == compact, subcompact, suv)

ggplot(data = mpg, aes(x = ))




    
  