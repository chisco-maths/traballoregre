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


# ---------------------------------------------------------------------------------------


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


### Capítulo 2: modelo ANOVA.


# Variable categórica: rexión (Reg).
# - Temos un total de 6 grupos (6 valores da variable categórica Reg), cun número maior
#   a 10 observacións por grupo.
# - Plantexamos o seguinte modelo ANOVA:
#   log(NewHIV)= mu(Reg_1) + alpha_i + Erro

# Creamos un data frame cos datos de interese
base <- na.omit(read.csv("BBDDHIV.csv",sep=";"))
dataReg=data.frame(CouCod=base$CouCod,AnovaReg=factor(base$Reg),AnovaNew=log(base$NewHIV))

# Representamos os datos
par(mfrow=c(1,1))
plot(dataReg$AnovaReg, dataReg$AnovaNew, main = "Identificación de atípicos", 
     xlab = "AnovaReg", ylab = "AnovaNew")

ggplot(dataReg, aes(x = AnovaReg, y = AnovaNew)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) +
  labs(x = "Rexión", y = "Log(NewHIV)", title = "Diagrama de dispersión de log(NewHIV) por rexión") +
  theme_minimal()

library(ggplot2)
library(dplyr) # Paquete para manipular data frame

# Definimos unha función que detecta atípicos
atipico <- function(x) {
  return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
}

# Creamos unha columna indicando cales son as observacións atípicas
dataReg <- dataReg %>%
  group_by(AnovaReg) %>%
  mutate(outlier = ifelse(atipico(AnovaNew), CouCod, NA))
length(which(dataReg$outlier != "NA")) # 8 atípicos
 
# Gráfico ilustrativo e identificativo
ggplot(dataReg, aes(x = AnovaReg, y = AnovaNew, colour = AnovaReg, shape = AnovaReg)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  geom_text(aes(label=outlier), na.rm=TRUE, hjust=-.5)+
  theme(legend.position="none")

# Páxina de onde se sacou o código da idea:
# https://www.r-bloggers.com/2022/08/how-to-label-outliers-in-boxplots-in-ggplot2/

# Definimos un novo data frame eliminando os atípicos
dataReg2 <- dataReg %>% filter(is.na(outlier))

# Filtramos atípicos
dataReg2 <- dataReg2 %>%
  group_by(AnovaReg) %>%
  mutate(outlier = ifelse(atipico(AnovaNew), CouCod, NA))
length(which(dataReg2$outlier != "NA")) # 1 atípico

# Representamos os datos
ggplot(dataReg2, aes(x = AnovaReg, y = AnovaNew, colour = AnovaReg, shape = AnovaReg)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  geom_text(aes(label=outlier), na.rm=TRUE, hjust=-.5)+
  theme(legend.position="none")

# Definimos un novo data frame eliminando os atípicos
dataReg3 <- dataReg2 %>% filter(is.na(outlier))

# Filtramos atípicos
dataReg3 <- dataReg3 %>%
  group_by(AnovaReg) %>%
  mutate(outlier = ifelse(atipico(AnovaNew), CouCod, NA))
length(which(dataReg3$outlier != "NA")) # 0 atípicos

# Representamos os datos
ggplot(dataReg3, aes(x = AnovaReg, y = AnovaNew, colour = AnovaReg, shape = AnovaReg)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  geom_text(aes(label=outlier), na.rm=TRUE, hjust=-.5)+
  theme(legend.position="none")


# Centrarémonos unicamente no data frame dataReg3, que renomearemos dataAnova por claridade.
dataAnova=dataReg3
rm(dataReg,dataReg2,dataReg3)
attach(dataAnova)

# Definimos o modelo anova
modAnovaReg<- lm(AnovaNew ~ AnovaReg)

# Visualizamos a información básica do modelo
summary(modAnovaReg)

# - A media máis alta é a asociada ao primeiro grupo (África). Como R toma este grupo
#   como grupo referencia, o resto dos estimadores son negativos, xa que representan as
#   desviacións da media de cada grupo respecto á de África.
# - Todas as medias son positivas.
# - A maior parte dos estimadores son significativamente distintos de 0. O que parece ter máis
#   problemas é o asociado a Asia pero segue tendo un p-valor significativo (0.02372).

#Nótese que a estimación da desviación é de 2.075 (relacionada coa suma da varianza intragrupal)

# Test F aplicado ao modelo ANOVA
anova(modAnovaReg) # 4.254e-08

# - O p-valor é moi significativo, logo rexeitamos a hipótese nula de que todas as medias son
#   iguais. Recordamos que co test F comparamos dous modelos: un modelo simplificado (o da
#   hipótese nula) e o modelo complexo onde polo menos hai dúas medias diferentes (o da hipótese
#   alternativa).


# Validación

# Definimos unha serie de obxectos que usaremos nesta sección
nReg=length(AnovaReg) # Tamaño da mostra
IReg=length(levels(AnovaReg)) # Cantidade de grupos
names=levels(AnovaReg) # Nomes dos grupos
mu_localReg <- tapply(AnovaNew, AnovaReg, mean) # Medias locais
n_localReg <- tapply(AnovaNew, AnovaReg, length) # Número total de observacións por grupo

# Test Shapiro-Wilk para os residuos
normalidad_residuos <- dataAnova %>%
  group_by(AnovaReg) %>%
  mutate(residuo = AnovaNew - mu_localReg[AnovaReg] ) %>% # Calcular los residuos
  summarise(p_value = shapiro.test(residuo)$p.value, .groups = "drop")
print(normalidad_residuos)

# - Observamos que a un nivel de 0.05 o grupo de Europa non cómpre
#   a hipótese de normalidade dos residuos.

# Test de Shapiro-Wilk para os datos
resultado_shapiro <- shapiro.test(dataAnova$AnovaNew)
print(resultado_shapiro) # 0.01248

# Eliminamos o grupo de Europa
table(dataAnova$AnovaReg)
dataAnova=dataAnova[!(dataAnova$AnovaReg%in% c("Europe" )),]
dataAnova$AnovaReg <- droplevels(dataAnova$AnovaReg)
table(dataAnova$AnovaReg)

# Repetimos o modelo

# Gráfico ilustrativo e identificativo
ggplot(dataAnova, aes(x = AnovaReg, y = AnovaNew, colour = AnovaReg, shape = AnovaReg)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  geom_text(aes(label=outlier), na.rm=TRUE, hjust=-.5)+
  theme(legend.position="none")

# Definimos o modelo anova

modAnovaReg2<- lm(dataAnova$AnovaNew ~ dataAnova$AnovaReg)

# Visualizamos a información básica do modelo
summary(modAnovaReg2)

# - As conclusións son practicamente idénticas ás do modelo anterior.

# Test F aplicado ao modelo ANOVA
anova(modAnovaReg2) # 0.0005975

# - O p-valor segue a ser moi significativo, logo rexeitamos a hipótese nula de que todas
# as medias son iguais.

# Repetimos a validación do modelo

# Definimos unha serie de obxectos que usaremos nesta sección
attach(dataAnova)
nReg=length(AnovaReg) # Tamaño da mostra
IReg=length(levels(AnovaReg)) # Cantidade de grupos
names=levels(AnovaReg) # Nomes dos grupos
mu_localReg <- tapply(AnovaNew, AnovaReg, mean) # Medias locais
n_localReg <- tapply(AnovaNew, AnovaReg, length) # Número total de observacións por grupo

# Test de Shapiro-Wilk para os residuos
normalidad_residuos <- dataAnova %>%
  group_by(AnovaReg) %>%
  mutate(residuo = AnovaNew - mu_localReg[AnovaReg] ) %>%# Calcular los residuos
  summarise(p_value = shapiro.test(residuo)$p.value, .groups = "drop")
print(normalidad_residuos)

#O total dos datos non segue unha distribución normal !!!!
resultado_shapiro <- shapiro.test(dataAnova$AnovaNew)
print(resultado_shapiro)


library(ggplot2)
ggplot( data.frame(resid = residuals(modAnovaReg2)), aes(sample = resid)) +
  stat_qq() +
  stat_qq_line()


leveneTest(AnovaNew ~ AnovaReg, data = dataAnova)

#Valor p: 0.01208. Este valor es menor que 0.05, lo que sugiere que rechazas la 
#hipótesis nula de que las varianzas son homogéneas entre los grupos.	
#Interpretación: Las varianzas entre los grupos son significativamente diferentes, 
#lo que indica una violación del supuesto de homogeneidad de varianzas.


# Comparacións múltiples

# Método de Bonferroni

# Cálculo dos intervalos de confianza
ni=n_localReg
bar.y=mean(AnovaNew)
bar.yi=mu_localReg
sumres <- sum((AnovaNew - unlist(bar.yi[AnovaReg]))^2)
dt2.bar=sumres/(nReg-IReg)
dt.bar=sqrt(dt2.bar)
k=IReg*(IReg-1)/2
alpha=0.95
qtb=qt(1-alpha/(2*k),nReg-IReg)
Bonferroni=data.frame(center=rep(0,k),left=rep(0,k),right=rep(0,k))
z=1; ww=rep(0,k);
for (i in 1:IReg){
  for (j in i:IReg){
    if (i!=j){
      ww[z]=paste0("(",names[i],"-",names[j],")");
      Bonferroni$center[z]=(bar.yi[i]-bar.yi[j]);
      error=qtb*(dt.bar*sqrt(1/ni[i]+1/ni[j]));
      Bonferroni$left[z]=Bonferroni$center[z]-error;
      Bonferroni$right[z]=Bonferroni$center[z]+error;
      Bonferroni$p.value[z]=round(2*pt(abs(Bonferroni$center[z])/((dt.bar*sqrt(1/ni[j]+1/ni[i]))),nReg-IReg,lower.tail=FALSE),6)
      z=z+1;
    }
  }
}

# Representación dos intervalos de confianza
#install.packages("ggplot2")
library(ggplot2)
rownames(Bonferroni)=ww
ggplot(Bonferroni, aes(y = rownames(Bonferroni), x = center,xmax=max(right)+1)) +
  geom_point(aes(color = "gold4"), size = 3) +  # Puntos azuis
  geom_segment(aes(x = left, xend = right, y = rownames(Bonferroni), color = "slateblue"), size = 1) +  # Barras de intervalo suaves
  geom_point(aes(x = left), shape = 21, size = 3, fill = "white", color = "slateblue2") +  # Puntos extremo esquerdo
  geom_point(aes(x = right), shape = 21, size = 3, fill = "white", color = "slateblue2") +  # Puntos extremo dereito
  geom_text(aes(x = max(right) + 0.5, label = round(p.value,3)), hjust = 0, vjust = 0, size = 3) +  # Texto con p.adjust
  labs(
    title = "Intervalos de confianza axustados por Bonferroni",
    x = "Estimación", 
    y = "Comparacións"
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 0.15) +
  theme_minimal() +
  theme(legend.position = "none" )  # Axustamos as etiquetas do eixo e eliminamos a lenda

# Método de Tukey

# Cálculo dos intervalos de confianza
TukeyHSD(aov(modAnovaReg))
qtt=qtukey(1-alpha,IReg,nReg-IReg)
Tukey=data.frame(center=rep(0,k),left=rep(0,k),right=rep(0,k),p.adjust=rep(0,k))
z=1; ww=rep(0,k)
for (i in 1:IReg){
  for (j in i:IReg){
    if (i!=j){
      ww[z]=paste0("(",names[i],"-",names[j],")");
      Tukey$center[z]=bar.yi[i]-bar.yi[j];
      error=(qtt/sqrt(2))*(dt.bar*sqrt(1/ni[i]+1/ni[j]))
      Tukey$left[z]=Tukey$center[z]-error;
      Tukey$right[z]=Tukey$center[z]+error;
      Tukey$p.adjust[z]=round(ptukey(abs(Tukey$center[z])/((dt.bar*sqrt(1/ni[j]+1/ni[i]))/sqrt(2)),IReg,nReg-IReg,lower.tail=FALSE),6)
      z=z+1;
    }
  }
}

# Representación dos intervalos de confianza
rownames(Tukey)=ww
ggplot(Tukey, aes(y = rownames(Tukey), x = center,xmax=max(right)+1)) +
  geom_point(aes(color = "gold4"), size = 3) +  # Puntos azuis
  geom_segment(aes(x = left, xend = right, y = rownames(Tukey), color = "slateblue"), size = 1) +  # Barras de intervalo suaves
  geom_point(aes(x = left), shape = 21, size = 3, fill = "white", color = "slateblue2") +  # Puntos extremo esquerdo
  geom_point(aes(x = right), shape = 21, size = 3, fill = "white", color = "slateblue2") +  # Puntos extremo dereito
  geom_text(aes(x = max(right) + 0.5, label = round(p.adjust,3)), hjust = 0, vjust = 0, size = 3) +  # Texto con p.adjust
  labs(
    title = "Intervalos de confianza axustados por Tukey",
    x = "Estimación", 
    y = "Comparacións"
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 0.15) +
  theme_minimal() +
  theme(legend.position = "none" )  # Axustamos as etiquetas do eixo e eliminamos a lenda


# ---------------------------------------------------------------------------------------


### Capítulo 3: modelo ANCOVA
