#install.packages("tidyverse")
#tidyverse_update() 

#install.packages(c("nycflights13", "gapminder", "Lahman"))

library(tidyverse)
library(nycflights13)
library(gapminder)
library(Lahman)

#ggplot2::mpg
mpg 
?mpg 

x#displ(엔진 크기)과 hwy(고속도로 연비)의 관계
##플롯의 심미성(aesthetic) 수준 설정; color, shape, size, alpha(투명도)
## - 서열변수가 아닌 경우 size(순서형 심미성)으로 매핑하는 것은 바람직하지 않다. 

#(aes 내부에서 심미성 자동 설정)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

#(aes 외부에서 외양만 수동 설정)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "red")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), shape = 4) #page11

dim(mpg)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = hwy))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = class, y = drv))
  #이 플롯이 유용하지 않은 이유? class가 범주형 변수, 이건 플롯 필요 없이 표로 충분함. 

#1.3.1 연습문제 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = hwy, y = cty, shape = class, color = class))

#하나의 (이산형*) 변수에 대한 플롯 면분할; facet_wrap(~ formula) 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 3)

#두 변수 조합으로 면분할
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl) #두 개의 변수가 ~로 분리

#1.5.1
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl)) + 
  facet_grid(drv ~ cyl)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ .) #행으로만 면분할

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl) #열로만 면분할

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2) 
#nrow는 행의 개수, ncol은 열의 개수 
#class별로 displ 높아지면 hwy 작아지는 경향 관찰 가능. 


##geom: 데이터를 나타내기 위해 플롯이 사용하는 기하 객체(geometric object).
##다중geom
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE #범례 생략
  )
#위 코드는 로컬 매핑으로 처리되어, 변수가 중복된다. 
##아래와 같이 전역 매핑으로 처리하여 같은 플롯을 만들 수 있다. 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point() + 
  geom_smooth()

#각 레이어에 aes 적용 가능. 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

#레이어마다 다른 데이터 지정 가능. (평활선에 경차만 지정)
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(
    data = filter(mpg, class == "subcompact"),
    se = FALSE 
  )


#아래 두 코드는 같은 플롯이다. 전역/로컬 매핑의 차이.
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth() 

ggplot() + 
  geom_point(
    data = mpg, 
    mapping = aes(x = displ, y = hwy)
  ) + 
  geom_smooth(
    data = mpg, 
    mapping = aes(x = displ, y = hwy)
  )

#1.6.1 코드 작성 
#1.
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(se = FALSE) 

#2. 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, group = drv)) +
  geom_point() + 
  geom_smooth(se = FALSE)

#3. ## NA 어떻게 없애지? filter? 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point() + 
  geom_smooth(
    se = FALSE
  )

#4. 
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point() + 
  geom_smooth(
    
    se = FALSE
  )




