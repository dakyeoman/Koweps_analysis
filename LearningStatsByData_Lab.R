#LEARNING STATS BY DATA - LAB
#LAB W6: 2000 미 대선 플로리다 선거결과
install.packages("UsingR")
library(UsingR)


attach(florida) #프로그램 종료 시 detach(florida)

# bush를 예측변수, buchanan를 반응변수로 regression
result.lm <- lm(BUCHANAN ~ BUSH)

plot(BUSH, BUCHANAN)
abline(result.lm)        
with(florida, identify(BUSH, BUCHANAN, n=2, labels = County))

summary(result.lm)
#Slope가 0인지 여부(두 변수 관련성)에 관심
#Multiple R-squared 0.3887 : 반응변수 변동(분산) 중 39%를 이 회귀모형으로 설명 가능 
#분산(전체 변동): 시그마(yi - y_bar)^2; 
#기울기가 0 -> 예측-반응 변수 관련 없을 때는 반응변수 전체의 평균으로 예측

#outlier

