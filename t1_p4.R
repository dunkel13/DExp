#### Ejercicios Melo ####

########### 1.) ##########
pe<-c(428,419,458,439,441,456,463,429,438,445,441,463)
pn<-c(462,448,435,465,429,472,453,459,427,468,452,447)
var.test(pe,pn, alternative = "two.sided", conf.level = 0.95) #p-value = 0.8835
t.test(pe,pn,alternative="two.sided", mu=0, conf.level = 0.95, var.equal=TRUE) 
#p-value = 0.1892
# 95 percent confidence interval: (-20.456585   4.289919)
li<-mean(pe)-mean(pn) - qt(0.975,12+12-2)*((11*sd(pe)+11*sd(pn))/(12+12-2))*sqrt((1/12)+(1/12))
ls<-mean(pe)-mean(pn) + qt(0.975,12+12-2)*((11*sd(pe)+11*sd(pn))/(12+12-2))*sqrt((1/12)+(1/12))
c(li,ls)
# No hay diferencia entre las medias de los procesos

###### 2 #####
wilcox.test(pe, pn, alternative="two.sided", paired=FALSE, conf.int = TRUE) 
#p-value = 0.2037
# 95 percent confidence interval: (-22.999948   5.999933)

##### 3 ######
a<-c(28,22,55,45,32,35,40,25,37,20)
d<-c(39,45,67,61,46,58,51,34,48,30)
#wilcox.test(a,d,alternative="less", paired=TRUE, conf.int = TRUE, mu=-10)
wilcox.test(a,d,alternative="greater", paired=TRUE, conf.int = TRUE, mu=-10) #p-value = 0.9929
# H0: mu1-mu2=-10 (mu1 es menor que mu2 por 10) vs 
# H1: mu1-mu2>-10 (con lo cual mu2>mu1, e.d mu1 es menor que mu2 "por más" de 10)

###### 4 #####
xbar1<-50.2; xbar2<-52.9
sd1<-4.8; sd2<-5.4
sp<- ((99*sd1^2)+(99*sd2^2))/198
spq<-sqrt(sp)
tc<-(xbar1-xbar2)/(spq*sqrt(1/100+1/100)) # -3.737047
tq<-qt(p=0.975, df=198) #1.972017
pvalor<-2*pt(q=-3.737047, df=198) #0.00024
# Se R H0=mu1-mu2=0 a un n.s. del 5%
### B ###
LI<-(xbar1-xbar2)-tq*(spq*sqrt(1/100+1/100))
LS<-(xbar1-xbar2)+tq*(spq*sqrt(1/100+1/100))
c(LI,LS) # (-4.124774, -1.275226)
