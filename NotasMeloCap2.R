############### Ejercicios impares Cap.2 Montgomery ######################

### Montgomery 2.5 ###
dias<-c(108,124,124,106,115,138,163,159,134,139)
mean(dias)
sd(dias)
tc=(mean(dias)-120)*sqrt(length(dias))/sd(dias)
cuantil=qt(0.99, 9)
t.test(dias, alternative = "greater", mu=120, conf.level = 0.99)
pvarlor=pt(1.77, 9, lower.tail = FALSE)
II=mean(dias)-qt(0.995, 9)*sd(dias)/sqrt(10)
IS=mean(dias)+qt(0.995, 9)*sd(dias)/sqrt(10)
IC=cbind(II,IS)

### Montgomery 2.16 ###
karl<-c(1.186,1.151,1.322,1.339,1.200,1.402,1.365,1.537,1.559)
leh<-c(1.061,0.992,1.063,1.062,1.065,1.178,1.037,1.086,1.052)
shapiro.test(karl); shapiro.test(leh)
qqnorm(karl);qqline(karl)
qqnorm(leh);qqline(leh)
t.test(karl,leh, alternative="two.sided", pired=TRUE)
diferencia=karl-leh
shapiro.test(diferencia) #p-value = 0.3663
library(ggpubr)
library(ggplot2)
ggqqplot(diferencia) 
qqnorm(diferencia);qqline(diferencia)

############################# Ejercicios Melo Cap. 2 ################

### 5.) ###
#qf(0.975, 14,17)
#qt(0.975, 31)
#pvalor=2*pt(0.133, 31, lower.tail = FALSE)

### 7.) ###
y1t1<-c(3,6,5,8,4,7)   ; y2t1<-c(10,18,22,20,16,19)
y1t2<-c(7,9,5,10,10,9) ; y2t2<-c(14,22,19,24,26,18)
t1<-c(y1t1,y2t1)
t2<-c(y1t2,y2t2)
mean(t1); length(t1); sd(t1)^2
mean(t2); length(t2); sd(t2)^2
var.test(t1, t2, alternative = "two.sided")
t.test(t1,t2, alternative="two.sided", var.equal=TRUE) #p-value = 0.3224

### 9.) ###
mañana<-c(89.8,87.3,90.2,87.6,98.1,87.3,91.2,91.8,88.9)
tarde<-c(86.4,90.3,86.4,99.2,93.1,94,89.2,88.7,90.1,83.9)
length(mañana); mean(mañana); sd(mañana)^2
length(tarde); mean(tarde); sd(tarde)^2
wilcox.test(mañana,tarde, alternative="two.sided", paired=FALSE)
#p-value = 0.9024
t.test(mañana,tarde,alternative="two.sided")
#p-value = 0.9499
hist(tarde)
shapiro.test(tarde) #p-value = 0.7196
tarde[tarde==83.9]=11
wilcox.test(mañana,tarde, alternative="two.sided", paired=FALSE)
#p-value = 0.9024
t.test(mañana,tarde,alternative="two.sided")
#p-value = 0.3865 con cambio
hist(tarde,breaks = "FD")
shapiro.test(tarde) #p-value = 4.936e-06 con cambio

### 11.) ###
placebo<-c(105,119,100,97,96,101,94,95,98)
cafeina<-c(96,99,94,89,96,93,88,105,88)
var.test(placebo, cafeina, alternative = "two.sided")
t.test(placebo,cafeina, alternative="two.sided", var.equal=TRUE)
#p-value = 0.06339
wilcox.test(placebo,cafeina,alternative="two.sided", paired=FALSE)
#p-value = 0.05121
hist(placebo, freq=FALSE); lines(density(placebo))
hist(cafeina, freq=FALSE); lines(density(cafeina))
shapiro.test(placebo); shapiro.test(cafeina)
#p-value = 0.01212 ; 0.4274
qqnorm(placebo); qqline(placebo)
qqnorm(cafeina); qqline(cafeina)
library(ggpubr)
ggqqplot(placebo) #un punto se aleja mucho de las bandas
ggqqplot(cafeina)
