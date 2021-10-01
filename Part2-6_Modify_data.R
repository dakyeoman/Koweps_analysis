#데이터 전처리 Data Preprocessing

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
displ_und4 <- mpg %>% filter(displ <= 4)
displ_upp5 <- mpg %>% filter(displ >= 5)
mean_und4 <- mean(displ_und4$displ)
mean_upp5 <- mean(displ_upp5$displ)
mean_und4 < mean_upp5 #TRUE

#제조사-도시연비(cty) 비교
dspl_audi <- mpg %>% filter(manufacturer == "audi")
a <- mean(dspl_audi$cty)
dspl_toyota <- mpg %>% filter(manufacturer == "toyota")
b <- mean(dspl_toyota$cty)
a < b #TRUE, 다른 방법은 없나? 

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

#practice p138  
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
mpg %>% filter(manufacturer == "audi") %>% arrange(desc(hwy)) %>% head(5)

