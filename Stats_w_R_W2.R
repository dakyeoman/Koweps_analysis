#질적 자료 표와 그래프
a <- rep("A", 1520)
b <- rep("B", 770)
c <- rep("C", 510)
x <- c(a, b, c);x

#도수분포표
table(x) 
y <- as.matrix(table(x)) ;y 
#[,1]: 1st column / [1,]: 1st row 
freq <- y[,1]
relative_freq <- freq/sum(y) #propotion
z <- cbind(freq, relative_freq) ;z

#파이차트
x <- c(1520, 770, 510);x
y <- round(x/sum(x) *100, digits = 1) ;y #소숫점 digits =
lab <- c("A", "B", "C") ;lab
w <- paste(lab, "(", y, "%", ")") ;w
# >>>"A ( 54 % )" "B ( 28 % )" "C ( 18 % )"
pie(x, labels = w, main = "Pie Chart")
 , 1, 1, 1, 0, 2, 2, 0, 0, 0, 1, 2, 1, 2, 0, 0, 1, 6, 4, 3, 3, 1, 2, 5, 0)
y <- as.matrix(table(x));y
freq <- y[,1]
rel_freq <- freq/sum(freq) ;rel_freq #상대도수(%) 구하기
csum <- cumsum(freq) ;csum #누적 합
c_rel_freq <- csum/sum(freq) ;c_rel_freq #누적상대도수
z <- cbind(freq, rel_freq, csum, c_rel_freq) ;z


#양적 자료 표와 그래프
#히스토그램
data()
hist(faithful$waiting)
#줄기-잎 그림
stem(faithful$waiting)











