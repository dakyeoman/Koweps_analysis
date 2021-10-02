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

