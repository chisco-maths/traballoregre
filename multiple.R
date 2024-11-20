###--------#--------#--------#--------#--------#--------#--------#--------#--------###
###--------#-------MODELOS--DE--REGRESIÓN--E--ANÁLISE--MULTIVARIANTE------#--------###
###--------#--------#--------#--------#--------#--------#--------#--------#--------###
###--------#--------#------TAREFA--DE--AVALIACIÓN--CONTINUA------#--------#--------###
###--------#--------#-------------------SCRIPT-------------------#--------#--------###
###--------#--------#--------------------------------------------#--------#--------###
###--------#--------#----------CHENLO--ANDRADE--NICOLÁS----------#--------#--------###
###--------#--------#---------ESTÉVEZ--LENGUA--FRANCISCO---------#--------#--------###

# Cargamos en R a base de datos eliminando as observacións incompletas.
base <- na.omit(read.csv("BBDDHIV.csv",sep=";"))
head(base)
attach(base)

# Recollemos na seguinte táboa a explicación das variables:

# Nome       Descrición                                                   Tipo 
# ---------------------------------------------------------------------------------------
# RegCod <-  Código da rexión                                             Categórica
# Reg    <-  Rexión                                                       Categórica
# CouCod <-  Código do país                                               Identificativa
# Cou    <-  País                                                         Identificativa 
# NewHIV <-  Número de novas infeccións por VIH                           Continua 
# PreHIV <-  Prevalencia de VIH entre adultos de 15 a 49 anos (\%)        Continua 
# TotHIV <-  Estimación de persoas (de todas as idades) vivindo con VIH   Continua 
# Pop    <-  Poboación                                                    Continua 
# Rel    <-  Relixión principal do país                                   Categórica 
# Con    <-  Continente                                                   Categórica 

# - As variables "CouCod" e "Cou" son identificadores únicos para cada observación.
# - As variables continuas son "NewHIV" (resposta), "PreHIV", "TotHIV" e "Pop"
#   (explicativas).
# - As variables categóricas son "Rel", "RegCod" e "Reg", esta última reconfigurada na 
#   variable "Con" para indicar continente.


### Capítulo 1: modelo múltiple.


# Propomos o seguinte modelo (mod0):
# Y  <- NewHIV  (resposta)
# X1 <- PreHIV  (explicativa)
# X2 <- TotHIV  (explicativa)
# X3 <- Pop     (explicativa)


# Diagramas de dispersión da variable resposta sobre as variables explicativas (Disp1)
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)

plot1 <- ggplot(data.frame(PreHIV,NewHIV), aes(x = PreHIV, y = NewHIV)) + 
  geom_point(color = "blue") + theme_minimal()

plot2 <- ggplot(data.frame(TotHIV,NewHIV), aes(x = TotHIV, y = NewHIV)) + 
  geom_point(color = "red") + theme_minimal()

plot3 <- ggplot(data.frame(Pop,NewHIV), aes(x = Pop, y = NewHIV)) + 
  geom_point(color = "green") + theme_minimal() +
  xlim(min(Pop) * 0.9, max(Pop) * 1.1)

grid.arrange(plot1, plot2, plot3, nrow = 1,
             top="Dispersión de NewHIV sobre as variables explicativas") 

# - En todos os diagramas apreciamos un comportamento similar:
#   1. Observamos unha relación directa: cando aumenta a variable explicativa
#      (PreHIV, TotHIV ou Pop), aumenta a variable resposta (NewHIV).
#   2. Debido as escalas poboacionais preséntase unha gran variabilidade nos datos.
# - Debido á forma de "cono" dos diagramas conxeturamos que o modelo lineal múltiple
#   proposto non vai verificar a hipótese de homocedasticidade.


# Diagramas de dispersión entre as variables explicativas (Disp2)
plot4 <- ggplot(data.frame(TotHIV,PreHIV), aes(x = TotHIV, y = PreHIV)) + 
  geom_point(color = "purple") + theme_minimal()

plot5 <- ggplot(data.frame(TotHIV,Pop), aes(x = TotHIV, y = Pop)) + 
  geom_point(color = "orange") + theme_minimal()

plot6 <- ggplot(data.frame(PreHIV,Pop), aes(x = PreHIV, y = Pop)) + 
  geom_point(color = "black") + theme_minimal()

grid.arrange(plot4, plot5, plot6, nrow = 1, top="Dispersion entre as variables explicativas") 

# - Repítense as conclusións dos diagramas de dispersión da resposta frente as 
#   explicativas.


# Debido a que os observacións son non negativas e teñen unha distribución asimétrica
# podemos aplicar unha transformación logarítmica a todas as variables. Tras a
# transformación logarítimica obtemos o seguinte modelo (mod1):
#  Y   log(NewHIV)  resposta
#  X1  log(PreHIV)  explicativa
#  X2  log(TotHIV)  explicativa
#  X3  log(Pop)     explicativa


#  Diagramas de dispersión da resposta sobre as explicativas (logarítmicas) (Disp3)
plotlog1 <- ggplot(data.frame(log(PreHIV),log(NewHIV)), aes(x = log(PreHIV), 
  y = log(NewHIV))) + geom_point(color = "cadetblue1") + theme_minimal()

plotlog2 <- ggplot(data.frame(log(TotHIV),log(NewHIV)), aes(x = log(TotHIV), 
  y = log(NewHIV))) + geom_point(color = "indianred1") + theme_minimal()

plotlog3 <- ggplot(data.frame(log(Pop),log(NewHIV)), aes(x = log(Pop), 
  y = log(NewHIV))) + geom_point(color = "palegreen") + theme_minimal()

grid.arrange(plotlog1, plotlog2, plotlog3, nrow = 1,
             top="Dispersion de NewHIV sobre as variables explicativas (logarítmicas)") 

# - No diagrama plotlog1 chama a atención a gran dispersión da nube de puntos. Notamos
#   un comportamento inusual: para determinados valores de log(PreHIV) hai varios
#   varios valores de log(NewHIV) (véxase log(PreHIV) = -2).
# - Nos diagramas plotlog2 e plotlog3 observamos linealidade con relación de 
#   dependencia directa. Non se observa demasiada variabilidade.


#  Diagramas de dispersión entre as variables explicativas (Disp4)
plotlog4 <- ggplot(data.frame(log(PreHIV),log(TotHIV)), aes(x = log(PreHIV), 
  y = log(TotHIV))) + geom_point(color = "gold2") + theme_minimal()

plotlog5 <- ggplot(data.frame(log(Pop),log(TotHIV)), aes(x = log(Pop), 
  y = log(TotHIV))) + geom_point(color = "orchid1") + theme_minimal()

plotlog6 <- ggplot(data.frame(log(PreHIV),log(Pop)), aes(x = log(PreHIV), 
  y = log(Pop))) + geom_point(color = "gray") + theme_minimal()

grid.arrange(plotlog4, plotlog5, plotlog6, nrow = 1,
             top="Dispersion entre as variables explicativas") 

# - No diagrama plotlog4 apreciamos o comportamento inusual similar ao de plotlog1.
# - No diagrama plotlog5 observamos unha relación directa.
# - No diagrama plotlog6 apreciamos unha dispersión maior que nos diagramas plotlog4 e
#   plotlog5.


#  Formulación e análise do modelo proposto (mod1)
mod1 <-  lm(log(NewHIV) ~ log(PreHIV) + log(TotHIV) + log(Pop))
summary(mod1)

# Estimadores dos parámetros do modelo (Est.Mod1)
beta_hat <- coef(mod1)
beta_hat

# - Na análise dos residuos vemos que non hai unha gran variabilidade.
# - Na análise dos estimadores dos parámetros apreciamos o seguinte:
#   1. O intercepto é significativamente non nulo.
#   2. Os estimadores asociados a log(TotHIV) e log(Pop) son moi significativos. O valor
#      crítico asociado ao coeficiente de log(PreHIV) é 0.00314, logo tamén é
#      significativo. 
#   3. Os erro típicos non son excesivamente altos.
# - A estimación da desviación típica vale 0.7429.
# - Podemos tomar un nivel de significación maior que 0.004 para facer inferencia.
# - Dos estimadores dos parámetros deducimos a seguinte expresión:
#   log(NewHIV) = -4.68712 + 0.26564*log(PreHIV) + 0.47702*log(TotHIV) + 0.45011*log(Pop).
# - O intercepto é negativo. A interpretación literal é que se todas as variables
#   explicativas valen 1 (isto é, os logaritmos valen 0), entón log(NewHIV) = -4.6871156, 
#   ie, NewHIV = exp(-4.6871156) = 0.009213222. Porén, esta interpretación non ten moito
#   sentido e non nos centraremos nela.
# - Os coeficientes asociados ás variables explicativas son positivos. Como o logaritmo
#   é unha función crecente, interpretamos que a relación entre as variables explicativas
#   e a variable resposta é de dependencia directa.


# Intervalos de confianza para os estimadores dos parámetros do modelo (Int.mod1)

# Calculamos os intervalos de confianza en diferentes niveis
confint_99 <- confint(mod1, level = 0.99)
confint_95 <- confint(mod1, level = 0.95)
confint_90 <- confint(mod1, level = 0.90)

# Convertimos os intervalos a data frame e agregamos unha columna co nivel de confianza
confint_df_99 <- as.data.frame(confint_99)
confint_df_99$confianza <- "99%"
colnames(confint_df_99) <- c("lower", "upper", "Confianza")
confint_df_95 <- as.data.frame(confint_95)
confint_df_95$confianza <- "95%"
colnames(confint_df_95) <- c("lower", "upper", "Confianza")
confint_df_90 <- as.data.frame(confint_90)
confint_df_90$confianza <- "90%"
colnames(confint_df_90) <- c("lower", "upper", "Confianza")

# Combinamos os data frame
confint_df <- rbind(confint_df_99, confint_df_95, confint_df_90)

# Renomeamos as columnas para facilitar o uso en ggplot
colnames(confint_df) <- c("lower", "upper", "Confianza")

# Engadimos as columnas do parámetro estimado e do valor central (media)
confint_df$parametro <- rownames(confint_99)
confint_df$media <- (confint_df$lower + confint_df$upper) / 2

# Creamos un gráfico con ggplot que amose na mesma escala os intervalos
ints <- ggplot(data=confint_df, aes(x=parametro, y=media, color=Confianza)) + 
  geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2) +
  xlab("Parámetro") + ylab("Estimación do parámetro con intervalo de confianza") +
  ggtitle("Intervalos de confianza para cada parámetro en diferentes niveis") +
  theme_bw() + theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10))
ints

# Creamos outro gráfico con ggplot que amose en diferentes escalas os intervalos
ints_2 <- ggplot(data=confint_df, aes(x=Confianza, y=media, color=Confianza)) + 
  geom_point(size=3) + geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2) +
  xlab("Nivel de confianza") +  ylab("Estimación do parámetro con intervalo de confianza") +
  ggtitle("Intervalos de confianza para todos os parámetros") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10)) +
  facet_wrap(~parametro,scales="free")  
ints_2

# - Ningún dos intervalos contén ao cero.
# - Os extremos dos parámetros asociados ás variables explicativas están entre 0 e 1.


# Diagnose do modelo (Dia.Mod1)

# Observacións con capacidade de influencia (Cap.Dia.Mod1)
leverages1 <- hat(model.matrix(mod1)) 
n1 <- length(log(NewHIV))
p1 <- 4 
indlev1 <- which(leverages1 >= 2*p1/n1)
length(indlev1) # 19

# Representamos graficamente as observacións con capacidade de influencia nos
# diagramas de dispersión de cada variable explicativa.
par(mfrow = c(1, 3), oma = c(0, 0, 2, 0))

plot(log(NewHIV) ~ log(PreHIV), pch=19)
points(log(NewHIV)[indlev1] ~ log(PreHIV)[indlev1], col=4, pch=16)
text(log(PreHIV)[indlev1], log(NewHIV)[indlev1], labels=indlev1, pos=4, col=4, cex=0.8)

plot(log(NewHIV) ~ log(TotHIV), pch=19)
points(log(NewHIV)[indlev1] ~ log(TotHIV)[indlev1], col=4, pch=16)
text(log(TotHIV)[indlev1], log(NewHIV)[indlev1], labels=indlev1, pos=4, col=4, cex=0.8)

plot(log(NewHIV) ~ log(Pop), pch=19)
points(log(NewHIV)[indlev1] ~ log(Pop)[indlev1], col=4, pch=16)
text(log(Pop)[indlev1], log(NewHIV)[indlev1], labels=indlev1, pos=4, col=4, cex=0.8)

mtext("Observacións con capacidade de influencia", side = 3, outer = TRUE, line = -1, 
      cex = 1.5)

# - Hai 19 observacións con capacidade de influencia: 11, 20, 37, 47, 52, 55, 75, 78, 84, 
#   93, 101, 107, 129, 137, 140, 141, 151, 159, 164. Non todas se atopan nos extremos.
# - Varias observacións con capacidade de influencia corresponden a valores baixos
#   das variables.

# Observacións atípicas (Ati.Dia.Mod1)

# Residuos estandarizados e estudentizados
which(abs(rstandard(mod1)) > 1.96)
which(abs(rstudent(mod1)) > 1.96) # Coinciden
indati1 <- which(abs(rstandard(mod1)) > 1.96)
length(indati1) # 8

# Representamos graficamente as observacións atípicas nos diagramas de dispersión
# de cada variable explicativa.
par(mfrow = c(1,3), oma = c(0, 0, 2, 0))

plot(log(NewHIV) ~ log(PreHIV), pch=19)
points(log(NewHIV)[indati1] ~ log(PreHIV)[indati1], col=4, pch=16)
text(log(PreHIV)[indati1], log(NewHIV)[indati1], labels=indati1, pos=4, col=4, cex=0.8)

plot(log(NewHIV) ~ log(TotHIV), pch=19)
points(log(NewHIV)[indati1] ~ log(TotHIV)[indati1], col=4, pch=16)
text(log(TotHIV)[indati1], log(NewHIV)[indati1], labels=indati1, pos=4, col=4, cex=0.8)

plot(log(NewHIV) ~ log(Pop), pch=19)
points(log(NewHIV)[indati1] ~ log(Pop)[indati1], col=4, pch=16)
text(log(Pop)[indati1], log(NewHIV)[indati1], labels=indati1, pos=4, col=4, cex=0.8)

mtext("Observacións atípicas", side = 3, outer = TRUE, line = -1, cex = 1.5)

# - Hai 8 observacións atípicas: 83, 87, 115, 137, 139, 150, 166, 169.

# Observacións influíntes (Inf.Dia.Mod1)

# Distancia de Cook
cooks.distance(mod1)
round(cooks.distance(mod1),3) 
sort(cooks.distance(mod1)) 

# - A observación con maior distancia de Cook é a 137.

# Influíntes
qf1 <- qf(0.5, 2, n1-p1)
which(cooks.distance(mod1) > qf1)

# Graficamente
par(mfrow = c(1,1))
hist(cooks.distance(mod1))
rug(cooks.distance(mod1))  

# - Ningunha observación é influínte.


# Validación do modelo (Val.Mod1)

par(mfrow = c(2, 2))
plot(mod1)

#   1. Residuals vs Fitted. Este gráfico representa os residuos fronte aos valores axustados.
#   É útil para identificar patróns nos residuos. Se o modelo é adecuado espérase que os
#   residuos estean distribuídos aleatoriamente arredor de 0 sen patróns evidentes, que é
#   o caso. Non se observan residuos que produzan efecto panca. Sinálanse as observacións
#   115, 150 e 166.
#  2.	Scale-Location. Este gráfico representa a raíz cadrada dos residuos estandarizados
#   fronte os valores axustados. Permite avaliar se a variabilidade dos residuos é constante. 
#   A hipótese de homocedasticidade non se ve moi comprometida: visualmente,
#   a liña horizontal é relativamente uniforme, sen ten tendencias claras.
#   Sinálanse as observacións 115, 150 e 166.
#  3.	Q-Q Residuals. Este gráfico mostra a distribución dos residuos estandarizados en
#   comparación cunha distribución normal. Serve para verificar se os residuos verifican
#   normalidade. Observamos que os valores máis próximos aos extremos están máis
#   alonxados do comportamento esperado dunha distribución normal. Sinálanse as 
#   observacións 115, 150 e 166.
#  4.	Residuals vs Leverage. Este gráfico axuda a identificar observacións influíntes
#   con alto leverage, é dicir, aquelas observacións que poden tener un impacto
#   significativo nos resultados do modelo. Os puntos influíntes poden detectarse
#   empregando a distancia de Cook. Sinálanse as observacións 107, 137 e 166.


# Normalidade

# Test de Shapiro-Wilk
shapiro.test(rstandard(mod1)) # 0.0008011
shapiro.test(rstudent(mod1)) # 0.000274

# Debido ao baixo nivel crítico podemos non rexeitar a normalidade do modelo proposto.

par(mfrow = c(1,2))
library(carData)
library(car)
library(lmtest)
hist(rstandard(mod1),freq=F,main="Histograma") 
qqPlot(rstandard(mod1),main="Q-Q Plot")


# Homocedasticidade (Hom.Val.Mod1)

# Test de Breusch-Pagan
bptest(mod1) # 0.07627

# Debido ao relativamente baixo nivel crítico podemos non rexeitar a hipótese de
# homocedasticidade do modelo proposto.

# Test de Harrison-McCabe
hmctest(mod1)

# Debido ao baixo nivel crítico non rexeitamos a hipótese de homocedasticidade
# do modelo proposto. Debemos traballar cun nivel maior ao 7.6 %.


# Linealidade (Lin.Val.Mod1)

# Test de Ramsey
resettest(mod1)

# O nivel crítico é alto (0.3018), logo rexeitamos a hipótese de linealidade do modelo.
# Segundo este contraste pode ser mellor un polinomio de grao 2 ou 3.

# Test de Harvey-Collier
harvtest(mod1)

# O nivel crítico é alto (0.5636), o que nos leva a rexeitar a hipótese nula 
# de que o modelo proposto é lineal (é coherente co resultado anterior).
# Segundo este test estamos aceptando que segue un comportamento convexo ou cóncavo.

 
# Test de sm.regression
library(sm) 
sm.regression(log(NewHIV),rstandard(mod1),model="linear")


# Análise da colinealidade (Col.Dia.Mod1)

x <- cbind(log(NewHIV),log(PreHIV),log(TotHIV),log(Pop))
colnames(x) <- c("log(NewHIV)","log(PreHIV)","log(TotHIV)","log(Pop)")
corr_matrix <-round(cor(x),3)
#install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(corr_matrix,hc.order=TRUE,type="lower",lab=TRUE) # Corr1

# Observamos correlacións moi altas entre certas variables.

# Factores de inflación da varianza
VIF <- c()
VIF["log(PreHIV)"] <- 1/(1-(cor(log(PreHIV),fitted(lm(log(PreHIV)~log(TotHIV)+log(Pop)))))^2)
VIF["log(TotHIV)"] <- 1/(1-(cor(log(TotHIV),fitted(lm(log(TotHIV)~log(PreHIV)+log(Pop)))))^2)
VIF["log(Pop)"] <- 1/(1-(cor(log(Pop),fitted(lm(log(Pop)~log(TotHIV)+log(PreHIV)))))^2)
VIF

# log(PreHIV)   log(TotHIV)   log(Pop) 
# 5.192242      12.266579     7.722377 

# Todos os factores de inflación son superiores a 5.


# Conclusións:

# O modelo mod1 ten parámetros significativos. Apreciamos múltiples observacións
# atípicas e con capacidade de influencia pero ningunha influínte. En base aos diversos
# test realizados, podemos non rexeitar as hipóteses de normalidade, homocedasticiade
# e independencia. Porén, hai problemas ca linealidade e ca colinealidade. Como
# solución atopamos dúas alternativas. En primeiro lugar, poderiamos eliminar as
# observacións con capacidade de influencia e atípicas, mais ao non haber observacións
# influíntes pensamos que isto non tería moito efecto no modelo. En segundo lugar,
# podemos tratar de eliminar algunha variable.

step(mod1)

# Notamos que a función step non elimina ningunha variable do modelo. Porén, pola
# propia definición das variables, sabemos que teoricamente PreHIV = TotHIV / Pop,
# logo log(PreHIV) = log(TotHIV) - log(Pop). Deste xeito, veremos se ao eliminar a
# variable log(TotHIV) solucionamos o problema de colinealidade.


# Formulación do modelo tras a eliminación da variable explicativa log(TotHIV) (mod2)

mod2 <-  lm(log(NewHIV) ~ log(PreHIV)  + log(Pop))

# Diagramas de dispersión asociados a mod2
grid.arrange(plotlog1, plotlog3, nrow = 1,
             top="Dispersión de NewHIV sobre as variables explicativas (logarítmicas)") 
grid.arrange(plotlog4, plotlog6, nrow = 1,
             top="Dispersión entre as variables explicativas") 

# Análise do modelo (Ana.mod2)
summary(mod2)

# Estimadores dos parámetros do modelo (Est.mod2)
beta_hat_2 <- coef(mod2)
beta_hat_2

# - Na análise dos residuos vemos que segue sen haber unha gran variabilidade.
#   Os residuos teñen unha mediana en torno ao 0, cun rango de 2.9190 + 2.6710 ~ 6
# - Na análise dos estimadores dos parámetros do modelo apreciamos o seguinte:
#   1. Os tres estimadores son moi significativos. 
#   2. Os erros típicos son baixos. 
# - A estimación da desviación típica é de 0.8132. Aumenta lixeiramente respecto
#   ao modelo anterior.
# - Por agora, podemos tomar un nivel de significación suficientemente
#   pequeno para facer inferencia.
# - As estimacións dos coeficientes teñen o mesmo signo que no modelo anterior, logo
#   a interpretación é análoga.

# Intervalos de confianza para os estimadores dos parámetros do modelo (Int.mod2)

# Calculamos os intervalos de confianza en diferentes niveis
confint_99_2 <- confint(mod2, level = 0.99)
confint_95_2 <- confint(mod2, level = 0.95)
confint_90_2 <- confint(mod2, level = 0.90)

# Convertimos os intervalos a data frame e agregamos unha columna de nivel de confianza
confint_df_99_2 <- as.data.frame(confint_99_2)
confint_df_99_2$confianza <- "99%"
colnames(confint_df_99_2) <- c("lower", "upper", "Confianza")
confint_df_95_2 <- as.data.frame(confint_95_2)
confint_df_95_2$confianza <- "95%"
colnames(confint_df_95_2) <- c("lower", "upper", "Confianza")
confint_df_90_2 <- as.data.frame(confint_90_2)
confint_df_90_2$confianza <- "90%"
colnames(confint_df_90_2) <- c("lower", "upper", "Confianza")

# Combinamos os data frame
confint_df_2 <- rbind(confint_df_99_2, confint_df_95_2, confint_df_90_2)

# Renomeamos as columnas para facilitar o uso en ggplot
colnames(confint_df_2) <- c("lower", "upper", "Confianza")

# Engadimos as columnas do parámetro estimado e o valor central (media)
confint_df_2$parametro <- rownames(confint_99_2)
confint_df_2$media <- (confint_df_2$lower + confint_df_2$upper) / 2

# Creamos un gráfico con ggplot que amose na mesma escala os intervalos
ints_2 <- ggplot(data=confint_df_2, aes(x=parametro, y=media, color=Confianza)) + 
  geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2) +
  xlab("Parámetro") + ylab("Estimación do parámetro con intervalo de confianza") +
  ggtitle("Intervalos de confianza para cada parámetro en diferentes niveis no mod2") +
  theme_bw() + theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10))
ints_2

# Creamos outro gráfico con ggplot que amose en diferentes escalas os intervalos
ints_escala_2 <- ggplot(data=confint_df_2, aes(x=Confianza, y=media, color=Confianza)) + 
  geom_point(size=3) + geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2) +
  xlab("Nivel de confianza") +  ylab("Estimación do parámetro con intervalo de confianza") +
  ggtitle("Intervalos de confianza para os parámetros do mod2") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10)) +
  facet_wrap(~parametro,scales="free")  
ints_escala_2

# - Ningún dos intervalos contén ao cero.
# - Os extremos dos parámetros asociados ás variables explicativas están entre 0.6 e 1.


# Diagnose do modelo (Dia.mod2)

# Observacións con capacidade de influencia (Cap.Dia.mod2)
leverages2 <- hat(model.matrix(mod2)) # Diagonal da matriz hat
n2 <- length(NewHIV)
p2 <- 3
indlev2 <- which(leverages2 >= 2*p2/n2)
length(indlev2) #17

# Representamos graficamente as observacións con capacidade de influencia nos
# diagramas de dispersión de cada variable explicativa.
par(mfrow = c(1,2))

plot(log(NewHIV) ~ log(PreHIV), pch=19)
points(log(NewHIV)[indlev2] ~ log(PreHIV)[indlev2], col=4, pch=16)
text(log(PreHIV)[indlev2], log(NewHIV)[indlev2], labels=indlev2, pos=4, col=4, cex=0.8)

plot(log(NewHIV) ~ log(Pop), pch=19)
points(log(NewHIV)[indlev2] ~ log(Pop)[indlev2], col=4, pch=16)
text(log(Pop)[indlev2], log(NewHIV)[indlev2], labels=indlev2, pos=4, col=4, cex=0.8)

# - Hai 17 observacións con capacidade de influencia: 20, 33, 37, 47, 55, 66, 75, 93, 
#   107, 111, 113, 137, 140, 141, 151, 178 e 179.
# - Non todas as observacións con capacidade de influencia se atopan nos extremos.


# Observacións atípicas (Ati.Dia.mod2)

# Residuos estandarizados o estudentizados
which(abs(rstandard(mod2)) > 1.96)
which(abs(rstudent(mod2)) > 1.96) 
indati2 <- which(abs(rstandard(mod2)) > 1.96)
length(indati2) # 11

# Representamos graficamente as observacións atípicas nos diagramas de dispersión
# de cada variable explicativa.
par(mfrow = c(1,2))

plot(log(NewHIV) ~ log(PreHIV), pch=19)
points(log(NewHIV)[indati2] ~ log(PreHIV)[indati2], col=4, pch=16)
text(log(PreHIV)[indati2], log(NewHIV)[indati2], labels=indati2, pos=4, col=4, cex=0.8)

plot(log(NewHIV) ~ log(Pop), pch=19)
points(log(NewHIV)[indati2] ~ log(Pop)[indati2], col=4, pch=16)
text(log(Pop)[indati2], log(NewHIV)[indati2], labels=indati2, pos=4, col=4, cex=0.8)

# - Hai 11: observacións atípicas: 11, 75, 84, 87, 101, 115, 137, 150, 159, 164 e 166.


# Observacións influíntes (Inf.Dia.mod2)

# Distancia de Cook
cooks.distance(mod2)
round(cooks.distance(mod2),3) 
sort(cooks.distance(mod2)) 

# A observación con maior distancia de Cook segue a ser a mesma do mod1 (a 137).

# Influíntes
qf2 <- qf(0.5, 2, n2-p2)
which(cooks.distance(mod2) > qf2)

# Graficamente
par(mfrow = c(1,1))
hist(cooks.distance(mod2))
rug(cooks.distance(mod2))  

# - Ningunha observación é influínte. Polo tanto, non eliminaremos observacións.


# Validación do modelo (Val.mod2)

par(mfrow = c(2, 2))
plot(mod2)

#  1.	Residuals vs Fitted. Sinálanse as observacións 137, 150 e 166.
#  2.	Scale-Location. Sinálanse as observacións 137, 150 e 166.
#  3.	Q-Q residuals. Sinálanse as observacións 137, 150 e 166. Coma no mod1, as colas
#     dispérsanse un pouco do comportamento esperado dunha distribución normal.
#  4.	Residuals vs Leverage. Como xa viramos, non se observan observacións influíntes.
#     Sinálanse as observacións 75, 137, e 150


# Normalidade (Nor.Val.mod2)

# Test de Shapiro-Wilk
shapiro.test(rstandard(mod2)) # 0.006058
shapiro.test(rstudent(mod2)) # 0.003103

# - Debido ao baixo nivel crítico podemos non rexeitar a normalidade do modelo proposto.

par(mfrow = c(1,2))
#library(car)
#library(carData)
#library(lmtest)
hist(rstandard(mod2),freq=F,main="Histograma") 
qqPlot(rstandard(mod2),main="Q-Q plot")


# Homocedasticidade (Hom.Val.mod2)

# Test de Breusch-Pagan
bptest(mod2) # 0.0766

# - Debido ao relativamente baixo nivel crítico podemos non rexeitar a hipótese de
#   homocedasticidade do modelo proposto.

# Test de Harrison-McCabe
hmctest(mod2) # 0.029

# - Debido ao baixo nivel crítico non rexeitamos a hipótese de homocedasticidade do 
#   modelo proposto. Debemos traballar cun nivel maior ao 7.6 %.


# Linealidade (Lin.Val.mod2)

# Test de Ramsey
resettest(mod2) # 0.6715

# - O nivel crítico é moi alto, logo rexeitamos a hipótese de linealidade do modelo.

# Test de Harvey-Collier
harvtest(mod2) # 0.0003043

# - Como o nivel crítico é moi baixo, non rexeitamos a hipótese de linealidade no modelo.


# Test de sm.regression
library(sm) 
sm.regression(log(NewHIV),rstandard(mod2),model="linear")

# - Observamos que o estimador non paramétrico da regresión enmárcase dentro da rexión de
#   confianza, logo non rexeitaremos a hipótese de linealidade.


# Análise da colinealidade (Col.Dia.mod2)
x <- cbind(log(NewHIV),log(PreHIV),log(Pop))
colnames(x) <- c("log(NewHIV)","log(PreHIV)","log(Pop)")
corr_matrix <-round(cor(x),3)
#install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(corr_matrix,hc.order=TRUE,type="lower",lab=TRUE)

# -A correlación entre as explicativas é moi baixa, logo eliminamos o problema
#  de colinealidade. 
# -Un 80 % da variable resposta pode ser explicada pola variable log(Pop).
# -Un 50 % da variable resposta pode ser explicada pola variable log(PreHIV).


# Factores de inflación da varianza
VIF_2 <- c()
VIF_2["logPreHIV"] <- 1/(1-(cor(log(PreHIV),fitted(lm(log(PreHIV)~log(Pop)))))^2)
VIF_2["logPop"] <- 1/(1-(cor(log(Pop),fitted(lm(log(Pop)~log(PreHIV)))))^2)
VIF_2

# Os VIF coinciden:
# logPreHIV   logPop 
# 1.001115    1.001115 


# Cómpre aclarar que no modelo estamos a supoñer unha independencia teórica 
# entre as observacións que non ten por que ser completamente fiel á realidade.
# En efecto, a natureza dos nosos datos é xeográfica, logo é imposible que non
# exista certa dependencia entre as observacións. Non profundizaremos moito neste 
# aspecto xa que non temos toda a información necesaria.

# Se ben a interpretación completa dos datos sería labor de expertos en epidemioloxía,
# si que podemos supoñer unha diferencia clara da prevalencia entre países debido a
# cuestións sociolóxicas. Deste xeito, sería lóxico que os países máis economicamente
# desenvolvidos teñan maior coñecemento da enfermidade e tomen medidas preventivas,
# o que se traduciría nunha menor incidencia de VIH. Pola contra, países con poucos
# recursos terían maior número de casos.


# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------

X <- model.matrix(mod2)
n <- nrow(X)
p <- ncol(X)
XtXi <- solve(t(X)%*%X)
H <- X %*% XtXi %*% t(X)
LNewHat <- H %*% log(NewHIV)

M <- diag(1,nrow=nrow(H)) - H
res <- M %*% log(NewHIV)
RSS <- t(log(NewHIV)) %*% M %*% log(NewHIV)
sigma2.hat <- RSS/(n-p)
sigma2.hat

covbeta <- as.numeric(sigma2.hat)*XtXi  
round(covbeta,3)

# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
#CÓDIGO RÁPIDO PARA ANALIZAR
#  Formulación en R do novo modelo (Mod2)
mod2 <-  lm(log(NewHIV) ~ log(PreHIV) + log(TotHIV) + log(Pop))
summary(mod2)

#  Observacións con capacidade de influencia (Cap.Dia.mod2)
leverages2 <- hat(model.matrix(mod2)) # Diagonal da matriz hat
n <- length(NewHIV)
p <- 4 
indlev2 <- which(leverages2 >= 2*p/n)
length(indlev2) #19

#  Observacións atípicas (Ati.Dia.mod2)
indati2 <- which(abs(rstandard(mod2)) > 1.96)

#  Observacións influíntes (Inf.Dia.mod2)
med <- qf(0.5, 2, n-p)
which(cooks.distance(mod2) > med)

#  Validación do modelo (Val.mod2)
par(mfrow = c(2, 2))
plot(mod2)

#  Normalidade (Nor.Val.mod2)
shapiro.test(rstandard(mod2)) #meh
shapiro.test(rstudent(mod2)) #meh
par(mfrow = c(1,2))
hist(rstandard(mod2),freq=F,main="Histograma") 
qqPlot(rstandard(mod2),main="qqPlot")

#  Homocedasticidade (Hom.Val.mod2)
bptest(mod2)
hmctest(mod2)

#  Linealidade (Lin.Val.mod2)
resettest(mod2)
harvtest(mod2)
par(mfrow=c(1,1))
sm.regression(log(NewHIV),rstandard(mod2),model="linear")

#  Análise da Colinealidade (Col.Dia.mod2)
x <- cbind(log(NewHIV),log(PreHIV),log(TotHIV),log(Pop))
colnames(x) <- c("log(NewHIV)","log(PreHIV)","log(TotHIV)","log(Pop)")
corr_matrix <-round(cor(x),3)
library(ggcorrplot)
ggcorrplot(corr_matrix,hc.order=TRUE,type="lower",lab=TRUE)

VIF <- c()
VIF["logPreHIV"] <- 1/(1-(cor(log(PreHIV),fitted(lm(log(PreHIV)~log(TotHIV)+log(Pop)))))^2)
VIF["logTotHIV"] <- 1/(1-(cor(log(TotHIV),fitted(lm(log(TotHIV)~log(PreHIV)+log(Pop)))))^2)
VIF["logPop"] <- 1/(1-(cor(log(Pop),fitted(lm(log(Pop)~log(TotHIV)+log(PreHIV)))))^2)
VIF

#A vista do grande VIF para logTotHIV propomos eliminala
