##############################################################################
### Diseño Taller 1 
##############################################################################
library(nortest)
s<-c(18.7, 46.3, 21.7, 17.8, 21.5, 19.4, 19.5, 21.3)
c<-c(25.1, 28.4, 23.2, 26.4, 25.7, 20.3, 21.4, 24.7)
m<-c(s,c)
### 1A
ks.test(m,"pnorm") #p-value < 2.2e-16
shapiro.test(m) # p-value = 0.0001653
qqnorm(m)
qqline(m)
### 1B
t.test(s,c, alternative = "two.sided", conf.level = 0.95, paired = F) #p-value = 0.7531
### 1C
t.test(s[s!=46.3],c, alternative = "two.sided", conf.level = 0.95, paired = F) #p-value = 0.001947
### 1D
# El valor de la respuesta no corresponde en su totalidad al efecto de ese nivel (sin fracturas) del factor
# es decir el valor de 46.3 es explicado por otro factor diferente, en este caso tiroides
### 1E
wilcox.test(s,c,alternative = "two.sided", paired = F) #p-value = 0.04988
### 1F
# No presentan la misma conclusión, porque las pruebas no paramétricas no son sensibles a datos atípicos.
### 1G
wilcox.test(s[s!=46.3],c,alternative = "two.sided", paired = F) #p-value = 0.005905
### 1H
# La prueba paramétrica es más sensible, porque al remover el dato atípico cambia la decisión sobre las hipótesis de la prueba
