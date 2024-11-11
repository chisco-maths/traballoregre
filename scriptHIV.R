###--------#--------#--------#--------###
###-------MODELOS--DE--REGRESIÓN------###
###----E--ANÁLISE--MULTIVARIANTE------###
###-----------------------------------###
###--------------SCRIPT---------------###
###-----------------------------------###
###--TRABALLO-DE-AVALIACIÓN-CONTINUA--###
###--------#--------#--------#--------###

###--------#--------#--------#--------###
###-----CHENLO--ANDRADE--NICOLÁS------###
###----ESTÉVEZ--LENGUA--FRANCISCO-----###
###--------#--------#--------#--------###


###--------#--------#--------#--------###
###---------LECTURA--DE--DATOS--------###
###--------#--------#--------#--------###

# Cargamos en R a base de datos eliminando as observacións incompletas

base <- na.omit(read.csv("BBDDHIV.csv",sep=";"))
head(base)
attach(base)
# ---------------------------------------------------------------------------------------
#  Variables do estudio 
# ---------------------------------------------------------------------------------------
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
# ---------------------------------------------------------------------------------------
# Comentarios:
# - As variables "CouCod" e "Cou" son identificadores únicos para cada observación.
# - As variables continuas son "NewHIV" (resposta), "PreHIV", "TotHIV" e "Pop"
#   (explicativas).
# - As variables categóricas son "Rel", "RegCod" e "Reg", esta última reconfigurada na 
#   variable "Con" para indicar continente.
# ---------------------------------------------------------------------------------------


###--------#--------#--------#--------###
###-------------CAPÍTULO-1------------###
###--------#--------#--------#--------###
###----------MODELO--MÚLTIPLE---------###
###--------#--------#--------#--------###

# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
#  Modelo proposto (mod0)
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
#  Y   NewHIV  resposta
#  X1  PreHIV  explicativa
#  X2  TotHIV  explicativa
#  X3  Pop     explicativa
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
#  Diagramas de dispersión da variable resposta sobre as variables explicativas (Disp1)
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
# ---------------------------------------------------------------------------------------
# Comentarios:
# - En todos os diagramas apreciamos un comportamento similar:
#   1. Podemos observar unha relación directa: cando aumenta a variable explicativa
#      que consideremos (PreHIV, TotHIV ou Pop), aumenta a variable resposta (NewHIV).
#   2. Debido as escalas poboacionais preséntase unha gran variabilidade nos datos.
#   3. Obtemos moita concentración: para valores baixos de todas as variables 
#      explicativas obtemos (frecuentemente) valores baixos da variable resposta.
# - Conxeturamos que o modelo lineal múltiple proposto non verifica a hipótese de
#   homocedasticidade (nótese a forma de "cono" dos diagramas).
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
#  Diagramas de dispersión entre as variables explicativas (Disp2)
plot4 <- ggplot(data.frame(TotHIV,PreHIV), aes(x = TotHIV, y = PreHIV)) + 
  geom_point(color = "purple") + theme_minimal()

plot5 <- ggplot(data.frame(TotHIV,Pop), aes(x = TotHIV, y = Pop)) + 
  geom_point(color = "orange") + theme_minimal()

grid.arrange(plot4, plot5, nrow = 1, top="Dispersion entre as variables explicativas") 
# ---------------------------------------------------------------------------------------
# Comentarios:
# - Repítense as conclusións dos diagramas de dispersión da resposta frente as 
#   explicativas.
# ---------------------------------------------------------------------------------------

# Debido a que os observacións son non negativas e teñen unha distribución asimétrica
# podemos aplicar unha transformación logarítmica a todas as variables.

# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
#  Modelo proposto tras a transformación logarítmica (mod1)
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
#  Y   log(NewHIV)  resposta
#  X1  log(PreHIV)  explicativa
#  X2  log(TotHIV)  explicativa
#  X3  log(Pop)     explicativa
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
#  Diagramas de Dispersión da resposta sobre as explicativas (logarítmicas) (Disp3)
plotlog1 <- ggplot(data.frame(log(PreHIV),log(NewHIV)), aes(x = log(PreHIV), 
  y = log(NewHIV))) + geom_point(color = "cadetblue1") + theme_minimal()

plotlog2 <- ggplot(data.frame(log(TotHIV),log(NewHIV)), aes(x = log(TotHIV), 
  y = log(NewHIV))) + geom_point(color = "indianred1") + theme_minimal()

plotlog3 <- ggplot(data.frame(log(Pop),log(NewHIV)), aes(x = log(Pop), 
  y = log(NewHIV))) + geom_point(color = "palegreen") + theme_minimal()

grid.arrange(plotlog1, plotlog2, plotlog3, nrow = 1,
             top="Dispersion de NewHIV sobre as variables explicativas (logarítmicas)") 
# ---------------------------------------------------------------------------------------
# Comentarios:
# - A transformación uniformiza as observacións outorgándolle un peso diferente ás mesmas.
# - No diagrama plotlog1 chama a atención a gran dispersión da nube de puntos: parece un
#   rectángulo. Ademais repítense datos facendo liñas (fixémonos cerca do valor -2 na
#   explicativa, vemos que a resposta fai un tipo de liña para ese valor).
# - Nos diagramas plotlog2 e plotlog3 apreciamos unha posible linealidade con relación de 
#   dependencia directa. Non se observs demasiada variabilidade.
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
#  Diagramas de Dispersión entre as variables explicativas (Disp4)
# ---------------------------------------------------------------------------------------
plotlog4 <- ggplot(data.frame(log(PreHIV),log(TotHIV)), aes(x = log(PreHIV), 
  y = log(TotHIV))) + geom_point(color = "gold2") + theme_minimal()

plotlog5 <- ggplot(data.frame(log(Pop),log(TotHIV)), aes(x = log(Pop), 
  y = log(TotHIV))) + geom_point(color = "orchid1") + theme_minimal()

plotlog6 <- ggplot(data.frame(log(PreHIV),log(Pop)), aes(x = log(PreHIV), 
  y = log(Pop))) + geom_point(color = "gray") + theme_minimal()

grid.arrange(plotlog4, plotlog5, plotlog6, nrow = 1,
             top="Dispersion entre as variables explicativas") 
# ---------------------------------------------------------------------------------------
# Comentarios:
# - No diagrama plotlog4 apreciamos un comportamento similar ao de plotlog1: apreciamos
#   "liñas".
# - No diagrama plotlog5 observamos unha relación directa.
# - No diagrama plotlog6 apreciamos unha dispersión maior que nos diagramas plotlog4 e
#   plotlog5.
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
#  Formulación do modelo proposto (mod1)
# ---------------------------------------------------------------------------------------
mod1 <-  lm(log(NewHIV) ~ log(PreHIV) + log(TotHIV) + log(Pop))

# Análise do modelo (Ana.Mod1)
summary(mod1)
# Estimadores dos parámetros do modelo (Est.Mod1)
beta_hat <- coef(mod1)
beta_hat
# ---------------------------------------------------------------------------------------
# Comentarios:
# - Na análise dos residuos vemos que non hai unha gran variabilidade.
# - Na análise dos estimadores dos parámetros apreciamos o seguinte:
#   1. O intercepto é significativamente non nulo.
#   2. Os estimadores asociados a log(TotHIV) e log(Pop) son moi significativos. O valor
#      crítico asociado ao coeficiente de log(PreHIV) é 0.00314, logo tamén é
#      significativo. 
#   3. Nótese que os erro típicos non son moi altos. 
# - A estimación da varianza é de 0.7429, que é pequena.
# - En xeral, podemos tomar un nivel de significación maior que 0.4% para facer inferencia.
# ---------------------------------------------------------------------------------------
# 
# Dos coeficientes deducimos a seguinte ecuación:
# log(NewHIV) = -4.68712 + 0.26564*log(PreHIV) + 0.47702*log(TotHIV) + 0.45011*log(Pop)
# - O intercepto é negativo. Isto significa que se todas as variables explicativas son 
#   nulas (isto é, que inicialmente sexan todas a unidade), que log(NewHIV)=-4.6871156. 
#   Podémolo interpretar coma que NewHIV=e^(-4.6871156)=0.009213222.
#   Non ten moito senso, pero baixo este modelo, se o número de persoas enfermas é 1, se 
#   a tasa de prevalencia é 1 e a poboación dese país e 1, temos 0.009213222 novos casos.
#   Non ten un sentido directo, pero si que nos da a intuición de que cando haxa un 
#   contaxio sempre haberá unha mínima posibilidade de que máis persoas desenrolen
#   a enfermidade.
# - Os coeficientes asociados ás variables explicativas son positivos. In
# ---------------------------------------------------------------------------------------
# 

# Intervalos de confianza para os estimadores dos parámetros do modelo (Int.Mod1)
# Calculamos os intervalos de confianza en diferentes niveis
confint_99 <- confint(mod1, level = 0.99)
confint_95 <- confint(mod1, level = 0.95)
confint_90 <- confint(mod1, level = 0.90)

# Convertimos os intervalos a data frames y agregamos una columna de nivel de confianza
confint_df_99 <- as.data.frame(confint_99)
confint_df_99$confianza <- "99%"
colnames(confint_df_99) <- c("lower", "upper", "confianza")
confint_df_95 <- as.data.frame(confint_95)
confint_df_95$confianza <- "95%"
colnames(confint_df_95) <- c("lower", "upper", "confianza")
confint_df_90 <- as.data.frame(confint_90)
confint_df_90$confianza <- "90%"
colnames(confint_df_90) <- c("lower", "upper", "confianza")

# Combinamos os DataFrames
confint_df <- rbind(confint_df_99, confint_df_95, confint_df_90)

# Renomeamos as columnas para facilitar o uso en ggplot
colnames(confint_df) <- c("lower", "upper", "confianza")

# Engadimos as columnas do parámetro estimado e o valor central (media)
confint_df$parametro <- rownames(confint_99)
confint_df$media <- (confint_df$lower + confint_df$upper) / 2

# Creamos un gráfico con ggplot que amose na mesma escala os intervalos
ints <- ggplot(data=confint_df, aes(x=parametro, y=media, color=confianza)) + 
  geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2) +
  xlab("Parámetro") + ylab("Estimación do parámetro con Intervalo de confianza") +
  ggtitle("Intervalos de confianza para cada parámetro en diferentes niveis") +
  theme_bw() + theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10))
ints

# Creamos outro gráfico con ggplot, que amose en diferentes escalas os intervalos
ints_2 <- ggplot(data=confint_df, aes(x=confianza, y=media, color=confianza)) + 
  geom_point(size=3) + geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2) +
  xlab("Nivel de confianza") +  ylab("Estimación do parámetro con Intervalo de confianza") +
  ggtitle("Intervalos de confianza para todos os parámetros") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10)) +
  facet_wrap(~parametro,scales="free")  
ints_2
# ---------------------------------------------------------------------------------------
# Comentarios:
# - Ningún dos intervalos contén ao cero, pero fixémonos que para todos os parámetros 
#   menos para o intercepto, os extremos están entre 0 e 1.
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
#  Diagnose do modelo (Dia.Mod1)
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
#  Observacións con capacidade de influencia (Cap.Dia.Mod1)

leverages1 <- hat(model.matrix(mod1)) 
n <- length(log(NewHIV))
p <- 4 
indlev1 <- which(leverages1 >= 2*p/n)
length(indlev1) #19

# Graficamente:
par(mfrow = c(1, 3), oma = c(0, 0, 2, 0)) #CapI1
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
# ---------------------------------------------------------------------------------------
# Comentarios:
# - Temos un total de 19 observacións con capacidade de influencia, pero vemos que non 
#   todos se atopan nos extremos.
# - As observacións con capacidade de influencia son 11  20  37  47  52  55  75  78  84 
#   93  101  107  129  137  140  141  151  159  164.
# - Nas gráficas representamos as observacións con capacidade de influencia,
#   amosándoo nos diagramas de dispersión de cada variable explicativa.
# - Observamos que as observacións con capacidade de influencia (que só dependen da 
#   matriz de deseño) acumúlanse varios na en valores baixos 
# ---------------------------------------------------------------------------------------



# ---------------------------------------------------------------------------------------
#  Observacións atípicas (Ati.Dia.Mod1)

# Residuos estandarizados e estudentizados
which(abs(rstandard(mod1)) > 1.96)
which(abs(rstudent(mod1)) > 1.96) #Coinciden
indati1 <- which(abs(rstandard(mod1)) > 1.96)

# Graficamente:
par(mfrow = c(1,3), oma = c(0, 0, 2, 0)) #Ati1
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
# ---------------------------------------------------------------------------------------
# Comentarios:
# - As observacións atípicas son 83  87 115 137 139 150 166 169.
# - Nas gráficas representamos a mesma idea que en (Cap.Dia.Mod1)
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
#  Observacións influíntes (Inf.Dia.Mod1)

# Distancia de Cook:
cooks.distance(mod1)
round(cooks.distance(mod1),3) 
sort(cooks.distance(mod1)) 

# Influíntes:
qf <- qf(0.5, 2, n-p)
which(cooks.distance(mod1) > qf)

# Graficamente:
par(mfrow = c(1,1))
hist(cooks.distance(mod1))
rug(cooks.distance(mod1))  

# ---------------------------------------------------------------------------------------
# Comentarios:
# - Ningunha observación é influínte. 
# - Nótese que a observación con maior distancia de Cook é a 137.
# ---------------------------------------------------------------------------------------



# ---------------------------------------------------------------------------------------
#  Validación do modelo (Val.Mod1)
# ---------------------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(mod1) #Val1

# ---------------------------------------------------------------------------------------
# Comentarios:
#  1.	Residuos vs. Axustados (Residuals vs Fitted): Este gráfico mostra os residuos fronte
#     os valores axustados. É útil para identificar patróns nos residuos. Se o modelo é 
#     adecuado, espérase que os residuos estén distribuidos aleatoriamente arredor de 
#     cero, sen patróns evidentes. Podemos apreciar unha gran cantidade de datos arrexuntados.
#     Non se observan residuos que produzan un efecto panca.
#  - Sinálanse as observacións 115, 150 e 166.
#  2.	Escala-Localización (Scale-Location): Este gráfico representa
#     a raíz cadrada dos residuos estandarizados fronte os valores axustados. 
#     Permite avaliar se a variabilidade dos residuos é constante. 
#     A hipótese de homocedasticidade non se ve moi comprometida, visualmente podemos
#     apreciar que como hai un cambio na variabilidade que se bede na ausencia de datos.
#     Semella coma que os datos á esquerda tiran da liña roxa para arriba.
#  - Sinálanse as observacións 115, 150 e 166.
#  3.	Gráfico Q-Q (Normal Q-Q): Este gráfico mostra a distribución dos residuos 
#     estandarizados en comparación cunha distribución normal. Serve para verificar 
#     se os residuos seguen unha distribución normal. Apreciamos que as colas semellan 
#     dispersarse un pouco do comportamento esperado dunha distribución normal.
#  - Sinálanse as observacións 115, 150 e 166.
#  4.	Residuos vs. Leverage (Residuals vs Leverage): Este gráfico axuda a identificar 
#     observacións influíntes con alto leverage, é dicir, aquelas observacións que 
#     poden tener un impacto significativo nos resultados do modelo. Os puntos 
#     influíntes poden detectarse empregando a distancia de Cook.
#  - Sinálanse as observacións 107, 137, e 166.
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
#  Normalidade (Nor.Val.Mod1)

#Test de Shapiro-Wilk
shapiro.test(rstandard(mod1))
shapiro.test(rstudent(mod1))
# ---------------------------------------------------------------------------------------
# Comentarios:
#  Debido ao baixo nivel crítico, podemos aceptar a normalidade do modelo proposto.
# ---------------------------------------------------------------------------------------
par(mfrow = c(1,2))
#library(carData)
#library(car)
#library(lmtest)
hist(rstandard(mod1),freq=F,main="Histograma") 
qqPlot(rstandard(mod1),main="qqPlot") #Qqplot1

# ---------------------------------------------------------------------------------------
#  Homocedasticidade (Hom.Val.Mod1)

#Test de Breusch-Pagan
bptest(mod1)
# ---------------------------------------------------------------------------------------
# Comentarios:
#  Debido ao baixo nivel crítico (0.07627), podemos non rexeitar a hipótese de
#  homocedasticidade do modelo proposto.
# ---------------------------------------------------------------------------------------

#Test de Harrison-McCabe
hmctest(mod1)
# ---------------------------------------------------------------------------------------
# Comentarios:
#  Debido ao baixo nivel crítico (0.031), non rexeitamos a hipótese de  homocedasticidade do 
#  modelo proposto. Debemos traballar cun nivel maior ao 7,6%.
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
#  Linealidade (Lin.Val.Mod1)

#Test de Ramsey
resettest(mod1) 
# ---------------------------------------------------------------------------------------
# Comentarios:
#  Temos un nivel crítico relativamente alto (0.3018), poderiamos rexeitar a hipótese de 
#  linealidade do modelo proposto. Segundo este contraste, pode ser mellor un polinomio 
#  de grao 2 ou 3.
# ---------------------------------------------------------------------------------------

#Test de Harvey-Collier
harvtest(mod1)

# ---------------------------------------------------------------------------------------
# Comentarios:
#  OLLO! Temos un alto nivel crítico (0.5636), o que nos leva a rexeitar a hipótese nula 
#  de que o modelo proposto é lineal. Segundo este test estamos aceptando que segue un
#  comportamento convexo ou cóncavo.
# ---------------------------------------------------------------------------------------
 
#Test de sm.regression
library(sm) 
sm.regression(log(NewHIV),rstandard(mod1),model="linear")

# ---------------------------------------------------------------------------------------
#  Análise da Colinealidade (Col.Dia.Mod1)
x <- cbind(log(NewHIV),log(PreHIV),log(TotHIV),log(Pop))
colnames(x) <- c("log(NewHIV)","log(PreHIV)","log(TotHIV)","log(Pop)")
corr_matrix <-round(cor(x),3)
#install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(corr_matrix,hc.order=TRUE,type="lower",lab=TRUE)
# ---------------------------------------------------------------------------------------
# Comentarios:
#  OLLO! Temos unhas correlacións bastante altas.
# ---------------------------------------------------------------------------------------


#Calculemos os factores de incremento da varianza
VIF <- c()
VIF["logPreHIV"] <- 1/(1-(cor(log(PreHIV),fitted(lm(log(PreHIV)~log(TotHIV)+log(Pop)))))^2)
VIF["logTotHIV"] <- 1/(1-(cor(log(TotHIV),fitted(lm(log(TotHIV)~log(PreHIV)+log(Pop)))))^2)
VIF["logPop"] <- 1/(1-(cor(log(Pop),fitted(lm(log(Pop)~log(TotHIV)+log(PreHIV)))))^2)
VIF
# ---------------------------------------------------------------------------------------
# Comentarios:
#  OLLO! Todos os factores de incremento son maiores a 5.
# logPreHIV logTotHIV    logPop 
# 5.192242 12.266579  7.722377
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
# Conclusións:
#  Mod1 é un modelo con parámetros significativos, no cal observamos que non hai 
#  observacións influíntes, senón que pola aleatoriedade da matriz de deseño podemos
#  apreciar numerosas observacións con capacidade de influencia e atípicas.
#  Por unha banda, podemos supór as hipóteses de normalidade, homocedasticidade e 
#  independencia. Máis temos un problema na linealidade.
#  Notemos que hai un problema de colinealidade.
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# Opcións:
#  1. Eliminar observacións con capacidade de influencia e atípicos.
#    Observación. Ao non ter observacións influíntes, eliminar estas observacións non
#                 producirá demasiado efecto. Eliminalas fará que esas observacións sexan
#                 menos importantes que o resto, podería ser un recurso innecesario.
#  2. Eliminar a variable log(TotHIV).
#    Observación. Por construcción do modelo, temos a seguinte relación:
#                 PreHIV = TotHIV / Pop  =>  log(PreHIV) = log(TotHIV) - log(Pop)
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# Decisión:
#  2. Eliminar a variable log(TotHIV). Solucionamos o problema de colinealidade? 
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
#  Formulación en R do Modelo tras a eliminación da variable explicativa
# log(TotHIV) (Mod2)
# ---------------------------------------------------------------------------------------
mod2 <-  lm(log(NewHIV) ~ log(PreHIV)  + log(Pop))

# Visualización dos datos para mod2
grid.arrange(plotlog1, plotlog3, nrow = 1,
             top="Dispersion de NewHIV sobre as variables explicativas (logarítmicas)") 
grid.arrange(plotlog4, plotlog6, nrow = 1,
             top="Dispersion entre as variables explicativas") 

# Análise do modelo (Ana.mod2)
summary(mod2)
# Estimadores dos parámetros do modelo (Est.mod2)
beta_hat_2 <- coef(mod2)
beta_hat_2
# ---------------------------------------------------------------------------------------
# Comentarios:
# - Na análise dos residuos vemos que segue sen haber unha gran variabilidade.
#   os residuos teñen unha mediana en torno ao 0, cun rango de 2.9190 + 2.6710 ~ 6
# - Na análise dos estimadores dos parámetros do modelo apreciamos o seguinte:
#   1. Os tres estimadores son significativos. 
#   2. Os erros típicos son baixos. 
# - A estimación da varianza é de 0.8132, que é pequena. Aumenta lixeiramente respecto
#   o modelo anterior.
# - Por agora, podemos tomar un nivel de significación suficientemente
#   pequeno para facer inferencia.
# ---------------------------------------------------------------------------------------
# Interpretación:
# - O intercepto é negativo. Isto significa, que se todas as variables explicativas son 
#   nulas (isto é, que inicialmente sexan todas a unidade), que log(NewHIV)=-6.33670. 
#   Podémolo interpretar coma que NewHIV=e^(-6.33670 )=0.001770134.
#   Non ten moito senso, pero baixo este modelo, se o número de persoas enfermas é 1, se 
#   a tasa de prevalencia é 1 e a poboación dese país e 1, temos 0.001770134 novos casos.
#   Non ten un sentido directo, pero si que nos da a intuición de que cando haxa un 
#   contaxio sempre haberá unha mínima posibilidade de que máis persoas desenrolen a 
#   enfermidade. É unha interpretación similar á do mod1.
# - A interpretación das estimacións asociadas aos parámetros do modelo de regresión 
#   múltiple é a mesma que no mod1.
# ---------------------------------------------------------------------------------------

# Intervalos de confianza para os estimadores dos parámetros do modelo (Int.mod2)
# Calculamos os intervalos de confianza en diferentes niveis
confint_99_2 <- confint(mod2, level = 0.99)
confint_95_2 <- confint(mod2, level = 0.95)
confint_90_2 <- confint(mod2, level = 0.90)

# Convertimos os intervalos a data frames e agregamos unha columna de nivel de confianza
confint_df_99_2 <- as.data.frame(confint_99_2)
confint_df_99_2$confianza <- "99%"
colnames(confint_df_99_2) <- c("lower", "upper", "confianza")
confint_df_95_2 <- as.data.frame(confint_95_2)
confint_df_95_2$confianza <- "95%"
colnames(confint_df_95_2) <- c("lower", "upper", "confianza")
confint_df_90_2 <- as.data.frame(confint_90_2)
confint_df_90_2$confianza <- "90%"
colnames(confint_df_90_2) <- c("lower", "upper", "confianza")

# Combinamos os DataFrames
confint_df_2 <- rbind(confint_df_99_2, confint_df_95_2, confint_df_90_2)

# Renomeamos as columnas para facilitar o uso en ggplot
colnames(confint_df_2) <- c("lower", "upper", "confianza")

# Engadimos as columnas do parámetro estimado e o valor central (media)
confint_df_2$parametro <- rownames(confint_99_2)
confint_df_2$media <- (confint_df_2$lower + confint_df_2$upper) / 2

# Creamos un gráfico con ggplot, que amose na mesma escala os intervalos
ints_2 <- ggplot(data=confint_df_2, aes(x=parametro, y=media, color=confianza)) + 
  geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2) +
  xlab("Parámetro") + ylab("Estimación do parámetro con Intervalo de confianza") +
  ggtitle("Intervalos de confianza para cada parámetro en diferentes niveis no mod2") +
  theme_bw() + theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10))
ints_2

# Creamos outro gráfico con ggplot, que amose en diferentes escalas os intervalos
ints_escala_2 <- ggplot(data=confint_df_2, aes(x=confianza, y=media, color=confianza)) + 
  geom_point(size=3) + geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2) +
  xlab("Nivel de confianza") +  ylab("Estimación do parámetro con Intervalo de confianza") +
  ggtitle("Intervalos de confianza para os parámetros do mod2") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10)) +
  facet_wrap(~parametro,scales="free")  
ints_escala_2
# ---------------------------------------------------------------------------------------
# Comentarios:
# - Ningún dos intervalos contén ao cero, fixémonos que para os estimadores dos
#   parámetros asociados a log(Pop) e log(PreHIV) os extremos están entre 0 e 1.
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
#  Diagnose do modelo (Dia.mod2)
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
#  Observacións con capacidade de influencia (Cap.Dia.mod2)
leverages2 <- hat(model.matrix(mod2)) # Diagonal da matriz hat
n_2 <- length(NewHIV)
p_2 <- 3
indlev2 <- which(leverages2 >= 2*p_2/n_2)
length(indlev2) #17

# Graficamente:
par(mfrow = c(1,2))
plot(log(NewHIV) ~ log(PreHIV), pch=19)
points(log(NewHIV)[indlev2] ~ log(PreHIV)[indlev2], col=4, pch=16)
text(log(PreHIV)[indlev2], log(NewHIV)[indlev2], labels=indlev2, pos=4, col=4, cex=0.8)


plot(log(NewHIV) ~ log(Pop), pch=19)
points(log(NewHIV)[indlev2] ~ log(Pop)[indlev2], col=4, pch=16)
text(log(Pop)[indlev2], log(NewHIV)[indlev2], labels=indlev2, pos=4, col=4, cex=0.8)
# ---------------------------------------------------------------------------------------
# Comentarios:
# - Temos un total de 17 observacións con capacidade de influencia, pero vemos que non 
#   todos se atopan nos extremos.
# - As observacións con capacidade de influencia son 20  33  37  47  55  66  75  93 107 
#   111 113 137 140 141 151 178 179.
# - Nas gráficas representamos as observacións con capacidade de influencia,
#   amosándoo nos diagramas de dispersión de cada variable explicativa.
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
#  Observacións atípicas (Ati.Dia.mod2)

# Residuos estandarizados o estudentizados
which(abs(rstandard(mod2)) > 1.96)
which(abs(rstudent(mod2)) > 1.96) 
indati2 <- which(abs(rstandard(mod2)) > 1.96)

# Graficamente:
par(mfrow = c(1,2))
plot(log(NewHIV) ~ log(PreHIV), pch=19)
points(log(NewHIV)[indati2] ~ log(PreHIV)[indati2], col=4, pch=16)
text(log(PreHIV)[indati2], log(NewHIV)[indati2], labels=indati2, pos=4, col=4, cex=0.8)

plot(log(NewHIV) ~ log(Pop), pch=19)
points(log(NewHIV)[indati2] ~ log(Pop)[indati2], col=4, pch=16)
text(log(Pop)[indati2], log(NewHIV)[indati2], labels=indati2, pos=4, col=4, cex=0.8)
# ---------------------------------------------------------------------------------------
# Comentarios:
# - As observacións atípicas son 11  75  84  87 101 115 137 150 159 164 166.
# - Nas gráficas representamos a mesma idea que en (Cap.Dia.mod2)
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
#  Observacións influíntes (Inf.Dia.mod2)

# Distancia de Cook:
cooks.distance(mod2)
round(cooks.distance(mod2),3) 
sort(cooks.distance(mod2)) 

# Influíntes:
med <- qf(0.5, 2, n-p)
which(cooks.distance(mod2) > med)

# Graficamente:
par(mfrow = c(1,1))
hist(cooks.distance(mod2))
rug(cooks.distance(mod2))  

# ---------------------------------------------------------------------------------------
# Comentarios:
# - Ningunha observación é influínte. 
# - Nótese que a observación con maior distancia de Cook segue a ser a 137 (do mod1).
# ---------------------------------------------------------------------------------------



# ---------------------------------------------------------------------------------------
#  Validación do modelo (Val.mod2)
# ---------------------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(mod2)

# ---------------------------------------------------------------------------------------
# Comentarios:
#  1.	Residuos vs. Axustados (Residuals vs Fitted): 
#  - Sinálanse as observacións 137, 150 e 166.
#  2.	Escala-Localización (Scale-Location):
#  - Sinálanse as observacións 137, 150 e 166.
#  3.	Gráfico Q-Q (Normal Q-Q): As colas seguen a dispersarse un pouco do comportamento
#     esperado dunha distribución normal.
#  - Sinálanse as observacións 137, 150 e 166.
#  4.	Residuos vs. Leverage (Residuals vs Leverage):
#     Non se observan observacións influintes
#  - Sinálanse as observacións 75, 137, e 150
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
#  Normalidade (Nor.Val.mod2)

#Test de Shapiro-Wilk
shapiro.test(rstandard(mod2))
shapiro.test(rstudent(mod2))
# ---------------------------------------------------------------------------------------
# Comentarios:
#  Debido ao baixo nivel crítico, podemos aceptar a normalidade do modelo proposto.
# ---------------------------------------------------------------------------------------
par(mfrow = c(1,2))
#library(car)
#library(carData)
#library(lmtest)
hist(rstandard(mod2),freq=F,main="Histograma") 
qqPlot(rstandard(mod2),main="qqPlot")
# ---------------------------------------------------------------------------------------
# Comentarios:
#  Observamos que nun entorno do 0, os valores representados son lixeiramente máis
#  grandes do que queremos esperar para supór unha normalidade.
#  Daremos por válida a hipótese en base aos test.
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
#  Homocedasticidade (Hom.Val.mod2)

#Test de Breusch-Pagan
bptest(mod2)
# ---------------------------------------------------------------------------------------
# Comentarios:
#  Debido ao baixo nivel crítico (0.0766), podemos non rexeitar a hipótese de
#  homocedasticidade do modelo proposto.
# ---------------------------------------------------------------------------------------

#Test de Harrison-McCabe
hmctest(mod2)
# ---------------------------------------------------------------------------------------
# Comentarios:
#  Debido ao baixo nivel crítico (0.022), non rexeitamos a hipótese de  homocedasticidade do 
#  modelo proposto. Debemos traballar cun nivel maior ao 7,6%.
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
#  Linealidade (Lin.Val.mod2)

#Test de Ramsey
resettest(mod2) 
# ---------------------------------------------------------------------------------------
# Comentarios:
#  Temos un nivel crítico moi alto (0.6715), deberiamos rexeitar a hipótese de 
#  linealidade do modelo proposto en base a este test.
# ---------------------------------------------------------------------------------------

#Test de Harvey-Collier
harvtest(mod2)

# ---------------------------------------------------------------------------------------
# Comentarios:
#  Temos un nivel crítico (0.0003043) moi baixo, logo non poderiamos rexeitar a hipótese  
#  nula de que o modelo proposto é lineal.
# ---------------------------------------------------------------------------------------

#Test de sm.regression
library(sm) 
sm.regression(log(NewHIV),rstandard(mod2),model="linear")
# ---------------------------------------------------------------------------------------
# Comentarios:
#  Observamos que o estimador non paramétrico da regresión enmárcase dentro da rexión de
#  confianza, polo tanto, tamén grazas a que un test non rexeita H0 (mod2 lineal),
#  daremos por suposto que o modelo é lineal.
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
#  Análise da Colinealidade (Col.Dia.mod2)
x <- cbind(log(NewHIV),log(PreHIV),log(Pop))
colnames(x) <- c("log(NewHIV)","log(PreHIV)","log(Pop)")
corr_matrix <-round(cor(x),3)
#install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(corr_matrix,hc.order=TRUE,type="lower",lab=TRUE)

# ---------------------------------------------------------------------------------------
# Comentarios:
#  A correlación entre as explicativas é case nula. 
#  Un 80% da variable resposta pode ser explicada pola variable log(Pop).
#  Un 50% da variable resposta pode ser explicada pola variable log(Pop).
# ---------------------------------------------------------------------------------------


#Calculemos os factores de incremento da varianza
VIF_2 <- c()
VIF_2["logPreHIV"] <- 1/(1-(cor(log(PreHIV),fitted(lm(log(PreHIV)~log(Pop)))))^2)
VIF_2["logPop"] <- 1/(1-(cor(log(Pop),fitted(lm(log(Pop)~log(PreHIV)))))^2)
VIF_2
# ---------------------------------------------------------------------------------------
# Comentarios:
#  AVANCE! Conseguimos correxir a colinealidade. 
#  Dúbida curiosa: cando temos dúas explicativas, coinciden os VIF?
#  Podería ser por simetría... 
# logPreHIV    logPop 
# 1.001115  1.001115 
# ---------------------------------------------------------------------------------------


#Eliminamos observacións atipicas

posibles2=sort(indati2)
base[posibles2,] #Analizamos cales son as posibles
base2=base[-posibles2,]
attach(base2)


# ---------------------------------------------------------------------------------------
#  Formulación en R do Modelo proposto (Mod3)
# ---------------------------------------------------------------------------------------
mod3<-  lm(log(NewHIV) ~ log(PreHIV)  + log(Pop))

# Análise do modelo (Ana.mod3)
summary(mod3)
# Estimadores dos parámetros do modelo (Est.mod3)
beta_hat_3 <- coef(mod3)
beta_hat_3
# ---------------------------------------------------------------------------------------
# Comentarios:
# - Na análise dos residuos vemos que non hai unha gran variabilidade.
# - Na análise dos estimadores dos parámetros do modelo proposto apreciamos o seguinte:
#   1. A pendente é significativamente non nula.
#   2. O estimador do parámetro asociado á variable log(PreHIV) é significativo, pois
#      o valor crítico é 0.00314. O resto de estimadores tamén son significativos. 
#   3. Nótese que os erro típicos non son moi altos. 
# - A estimación da varianza é de 0.7429, que é pequena.
# - En xeral, podemos tomar un nivel de significación maior que 0.4% para facer inferencia.
# ---------------------------------------------------------------------------------------
# Interpretación:
# - O intercepto é negativo. Isto significa, que se todas as variables explicativas son 
#   nulas (isto é, que inicialmente sexan todas a unidade), que log(NewHIV)=-4.6871156. 
#   Podémolo interpretar coma que NewHIV=e^(-4.6871156)=0.009213222.
#   Non ten moito senso, pero baixo este modelo, se o número de persoas enfermas é 1, se 
#   a tasa de prevalencia é 1 e a poboación dese país e 1, temos 0.009213222 novos casos.
#   Non ten un sentido directo, pero si que nos da a intuición de que cando haxa un contaxio
#   sempre haberá unha mínima posibilidade de que máis persoas desenrolen a enfermidade.
# - As estimacións asociados aos parámetros da recta de regresión axustada teñen a seguinte
#   interpretación. Escollemos unha variable en concreto, o parámetro asociado a esta 
#   variable representa o cambio esperado na variable resposta cando esa variable 
#   explicativa aumenta nunha unidade, mantendo constantes todas as demais variables do modelo. 
# ---------------------------------------------------------------------------------------
#   


# Intervalos de confianza para os estimadores dos parámetros do modelo (Int.mod3)
# Calculamos os intervalos de confianza en diferentes niveis
confint_99_3 <- confint(mod3, level = 0.99)
confint_95_3 <- confint(mod3, level = 0.95)
confint_90_3 <- confint(mod3, level = 0.90)

# Convertimos os intervalos a data frames y agregamos una columna de nivel de confianza
confint_df_99_3 <- as.data.frame(confint_99_3)
confint_df_99_3$confianza <- "99%"
colnames(confint_df_99_3) <- c("lower", "upper", "confianza")
confint_df_95_3 <- as.data.frame(confint_95_3)
confint_df_95_3$confianza <- "95%"
colnames(confint_df_95_3) <- c("lower", "upper", "confianza")
confint_df_90_3 <- as.data.frame(confint_90_3)
confint_df_90_3$confianza <- "90%"
colnames(confint_df_90_3) <- c("lower", "upper", "confianza")

# Combinamos os DataFrames
confint_df_3 <- rbind(confint_df_99_3, confint_df_95_3, confint_df_90_3)

# Renomeamos as columnas para facilitar o uso en ggplot
colnames(confint_df_3) <- c("lower", "upper", "confianza")

# Engadimos as columnas do parámetro estimado e o valor central (media)
confint_df_3$parametro <- rownames(confint_99_3)
confint_df_3$media <- (confint_df_3$lower + confint_df_3$upper) / 2

# Creamos un gráfico con ggplot, que amose na mesma escala os intervalos
ints <- ggplot(data=confint_df_3, aes(x=parametro, y=media, color=confianza)) + 
  geom_point() + geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2) +
  xlab("Parámetro") + ylab("Estimación do parámetro con Intervalo de confianza") +
  ggtitle("Intervalos de confianza para cada parámetro en diferentes niveis") +
  theme_bw() + theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10))
ints

# Creamos outro gráfico con ggplot, que amose en diferentes escalas os intervalos
ints_escala <- ggplot(data=confint_df, aes(x=confianza, y=media, color=confianza)) + 
  geom_point(size=3) + geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2) +
  xlab("Nivel de confianza") +  ylab("Estimación do parámetro con Intervalo de confianza") +
  ggtitle("Intervalos de confianza para todos os parámetros") + theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=0.5, size=10)) +
  facet_wrap(~parametro,scales="free")  
ints_escala
# ---------------------------------------------------------------------------------------
# Comentarios:
# - Ningún dos intervalos contén ao cero, pero fixémonos que para todos os parámetros 
#   menos para o intercepto, os extremos están entre 0 e 1.
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
#  Diagnose do modelo (Dia.mod3)
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
#  Observacións con capacidade de influencia (Cap.Dia.mod3)

leverages3 <- hat(model.matrix(mod3)) # Diagonal da matriz hat
n <- length(NewHIV)
p <- 3
indlev3 <- which(leverages3>= 2*p/n)
length(indlev3) #16

# Graficamente:
par(mfrow = c(1,2))
plot(log(NewHIV) ~ log(PreHIV), pch=19)
points(log(NewHIV)[indlev3] ~ log(PreHIV)[indlev3], col=4, pch=16)
text(log(PreHIV)[indlev3], log(NewHIV)[indlev3], labels=indlev3, pos=4, col=4, cex=0.8)


plot(log(NewHIV) ~ log(Pop), pch=19)
points(log(NewHIV)[indlev3] ~ log(Pop)[indlev3], col=4, pch=16)
text(log(Pop)[indlev3], log(NewHIV)[indlev3], labels=indlev3, pos=4, col=4, cex=0.8)
# ---------------------------------------------------------------------------------------
# Comentarios:
# - Temos un total de 19 observacións con capacidade de influencia, pero vemos que 
#   a maioria ou case todas se atopan nos extremos da nube de puntos.
# - As observacións con capacidade de influencia son 11  20  37  47  52  55  75  78  84 
#   93  101  107  129  137  140  141  151  159  164.
# - Algo máis...
# ---------------------------------------------------------------------------------------



# ---------------------------------------------------------------------------------------
#  Observacións atípicas (Ati.Dia.mod3)

# Residuos estandarizados o estudentizados
which(abs(rstandard(mod3)) > 1.96)
which(abs(rstudent(mod3)) > 1.96) 
indati3 <- which(abs(rstandard(mod3)) > 1.96)

# Graficamente:
par(mfrow = c(1,2))
plot(log(NewHIV) ~ log(PreHIV), pch=19)
points(log(NewHIV)[indati3] ~ log(PreHIV)[indati3], col=4, pch=16)
text(log(PreHIV)[indati3], log(NewHIV)[indati3], labels=indati3, pos=4, col=4, cex=0.8)

plot(log(NewHIV) ~ log(Pop), pch=19)
points(log(NewHIV)[indati3] ~ log(Pop)[indati3], col=4, pch=16)
text(log(Pop)[indati3], log(NewHIV)[indati3], labels=indati3, pos=4, col=4, cex=0.8)
# ---------------------------------------------------------------------------------------
# Comentarios:
# - As observacións atípicas son 83  87 115 137 139 150 166 169.
# - Nas gráficas representamos a mesma idea que en (Cap.Dia.mod3)
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
#  Observacións influíntes (Inf.Dia.mod3)

# Distancia de Cook:
cooks.distance(mod3)
round(cooks.distance(mod3),3) 
sort(cooks.distance(mod3)) 

# Influíntes:
med <- qf(0.5, 2, n-p)
which(cooks.distance(mod3) > med)

# Graficamente:
par(mfrow = c(1,1))
hist(cooks.distance(mod3))
rug(cooks.distance(mod3))  

# ---------------------------------------------------------------------------------------
# Comentarios:
# - Ningunha observación é influínte. 
# - Nótese que a observación con maior distancia de Cook é a 137.
# ---------------------------------------------------------------------------------------



# ---------------------------------------------------------------------------------------
#  Validación do modelo (Val.mod3)
# ---------------------------------------------------------------------------------------
par(mfrow = c(2, 2))
plot(mod3)

# ---------------------------------------------------------------------------------------
# Comentarios:
#  1.	Residuos vs. Axustados (Residuals vs Fitted): 
#  - Sinálanse as observacións 115, 150 e 166.
#  2.	Escala-Localización (Scale-Location): 
#  - Sinálanse as observacións 115, 150 e 166.
#  3.	Gráfico Q-Q (Normal Q-Q): 
#  - Sinálanse as observacións 115, 150 e 166.
#  4.	Residuos vs. Leverage (Residuals vs Leverage):
#  - Sinálanse as observacións 107, 137, e 166.
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
#  Normalidade (Nor.Val.mod3)

#Test de Shapiro-Wilk
shapiro.test(rstandard(mod3))
shapiro.test(rstudent(mod3))
# ---------------------------------------------------------------------------------------
# Comentarios:
#  Os elementos en torno ao cero debilitan a normalidade.
#  O problema non está nas colas, senón nun entorno do cero (para os residuos est.)
# ---------------------------------------------------------------------------------------
par(mfrow = c(1,2))
#library(car)
#library(carData)
#library(lmtest)
hist(rstandard(mod3),freq=F,main="Histograma") 
qqPlot(rstandard(mod3),main="qqPlot")


# ---------------------------------------------------------------------------------------
#  Homocedasticidade (Hom.Val.mod3)

#Test de Breusch-Pagan
bptest(mod3)
# ---------------------------------------------------------------------------------------
# Comentarios:
#  Aumenta o nivel crítico (0.07627) con respecto a mod2
# ---------------------------------------------------------------------------------------

#Test de Harrison-McCabe
hmctest(mod3)
# ---------------------------------------------------------------------------------------
# Comentarios:
#  Debido ao alto nivel crítico (0.582), deberiamos rexeitar a hipótese de 
#  homocedasticidade do modelo.
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
#  Linealidade (Lin.Val.mod3)

#Test de Ramsey
resettest(mod3) 
# ---------------------------------------------------------------------------------------
# Comentarios:
#  Melloramos o valor crítico (0.0592).
# ---------------------------------------------------------------------------------------

#Test de Harvey-Collier
harvtest(mod3)

# ---------------------------------------------------------------------------------------
# Comentarios:
#  Melloramos moito este nivel crítico (0.0002619)
# ---------------------------------------------------------------------------------------

#Test de sm.regression
library(sm) 
sm.regression(log(NewHIV),rstandard(mod3),model="linear")

# ---------------------------------------------------------------------------------------
#  Análise da Colinealidade (Col.Dia.mod3)
x <- cbind(log(NewHIV),log(PreHIV),log(Pop))
colnames(x) <- c("log(NewHIV)","log(PreHIV)","log(Pop)")
corr_matrix_3 <-round(cor(x),3)
#install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(corr_matrix,hc.order=TRUE,type="lower",lab=TRUE)
# ---------------------------------------------------------------------------------------
# Comentarios:
# ---------------------------------------------------------------------------------------


#Calculemos os factores de incremento da varianza
VIF_3 <- c()
VIF_3["logPreHIV"] <- 1/(1-(cor(log(PreHIV),fitted(lm(log(PreHIV)~log(Pop)))))^2)
VIF_3["logPop"] <- 1/(1-(cor(log(Pop),fitted(lm(log(Pop)~log(PreHIV)))))^2)
VIF_3
# ---------------------------------------------------------------------------------------
# Comentarios:
#  OLLO! Todos os factores de incremento son maiores a 5.
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
# Comentarios:
#  En base aos resultados acadados, unha vez se depura a base de datos atípicos
# non se mellora a hipóteses de linealidade. De feito produce o efecto de que se poña en 
#  dúbida a hipótese de normalidade, e os tests asociados á linealidade non sexan 
#  mellores que no modelo anterior (mod2).
# Precisamos aclarar que estamos supondo a hipótese de independencia entre as 
# observacións. A natureza dos datos é xeográfica, polo tanto existirá unha dependencia
# xeográfica, mais ao non profundizar nela, suporemos que os paises están aislados e son
# illas e teñen ningún tipo de dependencia.
# Nótese que hai paises que poden ter coñecemento da enfermidade e medidas preventivas 
# (países desenvoltos) e isto produce que haxa unha menor incidencia da enfermidade
# aínda que tamén se mostrará na variable log(PreHIV), pois a prevalencia
# poderianos indicar a enfermidade ao longo dun rango de tempo.
# Isto é, si un pais tivo un boom de VIH, e despois tomou medidas, isto traducirase
# en alta log(PreHIV) pero baixa resposta.
# ---------------------------------------------------------------------------------------



# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
# Selección de observacións
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
# MODELO ANOVA. 
# Variable Categórica Rexión (Reg).
# ---------------------------------------------------------------------------------------
table(base$Reg)

# ---------------------------------------------------------------------------------------
# Comentarios:
#  - Temos un total de 6 grupos (6 valores da variable categórica Reg), cun número maior
#    a 10 observacións por grupo.
# ---------------------------------------------------------------------------------------

#  - Plantexamos o seguinte modelo ANOVA:
# ---------------------------------------------------------------------------------------
# log(NewHIV)= Reg*X + Erro
# ---------------------------------------------------------------------------------------

AnovaReg=levels()

anovareg=lm(log(NewHIV) ~ Reg)
summary(anovareg)
plot(base$Reg,log(base$NewHIV))
ggplot(data.frame(base$Reg,log(base$NewHIV)), aes(x = Reg, y = log(NewHIV))) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) + # Añade un poco de dispersión en el eje x para que los puntos no se superpongan
  labs(x = "Región", y = "Log(NewHIV)", title = "Diagrama de dispersión de log(NewHIV) por Región") +
  theme_minimal()

ggplot(base, aes(x = Reg, y = log.NewHIV)) +
  geom_jitter(width = 0.2, height = 0, color = "blue", alpha = 0.6) +
  labs(title = "Diagrama de dispersión de log(NewHIV) por Región", 
       x = "Región", y = "log(NewHIV)") +
  theme_minimal()

anovarel=lm(NewHIV ~ Rel)
summary(anovarel)

table(base$Rel)
#Para trabajar con las religiones nos quedamos con tres grupos solo










# ---------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------

X <- model.matrix(mod3)
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
#Eliminamos observacións
posibles=indati1
base[posibles,] #Analizamos cales son as posibles
base2=base[-posibles,]
attach(base2)

# ---------------------------------------------------------------------------------------
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
