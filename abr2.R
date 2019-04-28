#########################################################################
# Clase 2 abr - ejemplo 2 especies de árboles
#########################################################################


A<-c(6.4, 6.8,6.9, 6.9,6.9, 7 ,8.3, 8.6, 8.7, 8.7, 9, 9.1, 9.3, 9.9, 10.1,
     10.2, 11.4, 13.7, 14.8, 15.2, 16.2, 16.3, 17.2, 18.4, 20 ,20.1, 20.3, 21.4, 22.8, 22.8)
B<-c(8.2, 9.7, 9.8, 10, 10, 10.1, 10.3, 11.2, 13.2, 13.4, 14.1, 14.2, 14.4,
     14.8, 15.9, 20.2, 20.3, 20.6, 20.9, 23.8, 25.7,30.9,35.5, 38.2, 40,
     40.1, 40.2, 40.5, 41.8, 42.3)

### Medidas Resumen

library(fields) #library(spam)
Res.A<-stats(A)

media.A<- mean(A)#as.vector(Res.A[2,1])
mediana.A<-median(A)
desvEst.A<- sd(A) #as.vector(Res.A[3,1])
cv.A<-(desvEst.A/media.A)*100
min.A<-min(A)
max.A<-max(A)
n.A<- length(A)#as.vector(Res.A[1,1])

Res.B<-stats(B)

media.B<- mean(B)#as.vector(Res.A[2,1])
mediana.B<-median(B)
desvEst.B<- sd(B) #as.vector(Res.A[3,1])
cv.B<-(desvEst.B/media.B)*100
min.B<-min(B)
max.B<-max(B)
n.B<- length(B)#as.vector(Res.A[1,1])

Res.AB<-data.frame(cbind(c(media.A,mediana.A,desvEst.A,cv.A,min.A,max.A),
                  c(media.B,mediana.B,desvEst.B,cv.B,min.B,max.B)), row.names = c("media", "mediana", "desv std", "cv","min","max")
           )
colnames(Res.AB)<-c("A","B")
round(Res.AB,2)

### Prueba de normalidad

library(stats)
ks.test(A, "pnorm",media.A,desvEst.A, alternative = "two.sided", exact=NULL) #p-value = 0.1269
ks.test(B, "pnorm",media.B,desvEst.B, alternative = "two.sided", exact=NULL) #p-value = 0.2157

par(mfrow=c(1,2))
#Diagrama de caja una vez se verifica el supuesto de normalidad
boxplot(A)
boxplot(B)
par(mfrow=c(1,1))

var.test(A, B, ratio = 1, alternative = "two.sided", conf.level = 0.95) #p-value = 5.132e-05
#qf(0.05/2,29,29)

t.test(A, B, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95) #p-value = 0.0004861
(media.A-media.B)/sqrt((desvEst.A^2/n.A)+(desvEst.B^2/n.B))
