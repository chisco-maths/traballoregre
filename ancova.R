
# ---------------------------------------------------------------------------------------
# MODELO ANCOVA. 
# Variable Resposta Continua log(NewHIV)
# Variable Explicativa Categórica Rexión (Reg).
# Variable Explicativa Continua log(PreHIV)
# Variable Explicativa Continua log(Pop)

#Analicemos o mellor modelo

# ---------------------------------------------------------------------------------------

#Apliquemos un modelo que combina o multiple e anova para facer o ancova
# ---------------------------------------------------------------------------------------
# Comentarios:
#  - Temos un total de 6 grupos (6 valores da variable categórica Reg), cun número maior
#    a 10 observacións por grupo.
# ---------------------------------------------------------------------------------------


#Creamos un data.frame cos datos de interese
base <- na.omit(read.csv("BBDDHIV.csv",sep=";"))
data.Ancova=data.frame(AncCouCod=base$CouCod,AncovaReg=factor(base$Reg),AncovaNew=log(base$NewHIV),AncovaPre=log(base$PreHIV),AncovaPop=log(base$Pop))
attach(data.Ancova)


ggplot(data.Ancova, aes(x = AncovaPre, y = AncovaNew, colour = AncovaReg, shape = AncovaReg)) + 
  geom_point() +
  ggtitle("Diagrama de dispersión de log(PreHIV) sobre log(NewHIV) por grupos de Reg") +
  theme_minimal()

modAncovaRegA <- lm(AncovaNew~(AncovaPre)*AncovaReg)
summary(modAncovaRegA)
step(modAncovaRegA)

modAncovaRegA2 <- lm(AncovaNew~AncovaPre+AncovaReg)
summary(modAncovaRegA2)


ggplot(data.Ancova, aes(x = AncovaPop, y = AncovaNew, colour = AncovaReg, shape = AncovaReg)) + 
  geom_point() +
  ggtitle("Diagrama de dispersión de log(PreHIV) sobre log(NewHIV) por grupos de Reg") +
  theme_minimal()

#Modelo con interacción
modAncovaRegB <- lm(AncovaNew~(AncovaPop)*AncovaReg)
summary(modAncovaRegB)
#Modelo sen interacción
modAncovaRegB2 <- lm(AncovaNew~AncovaPop+AncovaReg)
summary(modAncovaRegB2)

#Centrarémonos en estudar o modelo sobre AncovaPop

#  - Plantexamos o seguinte modelo ANCOVA:
# Ancova simple con interacción
# ---------------------------------------------------------------------------------------
# log(NewHIV)= mu(Reg_1) + alpha_i + (delta_1+delta_i)*log(Pop) + Erro
# ---------------------------------------------------------------------------------------

# Estimadores dos parámetros do modelo
coeffancovaB <- coef(modAncovaRegB); coeffancovaB
# Coeficientes para cada grupo

ggplot(data.Ancova, aes(x = AncovaPop, y = AncovaNew, colour = AncovaReg,shape=AncovaReg)) +
  geom_point(shape=AncovaReg) +  # Puntos reales
  geom_abline(slope = coeffancovaB["AncovaPop"], intercept = coeffancovaB["(Intercept)"], color = "yellow", size = 1) + 
  geom_abline(slope = coeffancovaB["AncovaPop"] + coeffancovaB["AncovaPop:AncovaRegAmericas"], intercept = coeffancovaB["(Intercept)"] + coeffancovaB["AncovaRegAmericas"], color = "red", size = 1) +  # Recta para el grupo "Americas" de AncovaReg
  geom_abline(slope = coeffancovaB["AncovaPop"] + coeffancovaB["AncovaPop:AncovaRegEastern Mediterranean"], intercept = coeffancovaB["(Intercept)"] + coeffancovaB["AncovaRegEastern Mediterranean"], color = "purple", size = 1) +  # Recta para el grupo "Eastern Mediterranean" de AncovaReg
  geom_abline(slope = coeffancovaB["AncovaPop"] + coeffancovaB["AncovaPop:AncovaRegEurope"], intercept = coeffancovaB["(Intercept)"] + coeffancovaB["AncovaRegEurope"], color = "green", size = 1) +  # Recta para el grupo "Europe" de AncovaReg
  geom_abline(slope = coeffancovaB["AncovaPop"] + coeffancovaB["AncovaPop:AncovaRegSouth-East Asia"], intercept = coeffancovaB["(Intercept)"] + coeffancovaB["AncovaRegSouth-East Asia"], color = "blue", size = 1) +  # Recta para el grupo "South-East Asia" de AncovaReg
  geom_abline(slope = coeffancovaB["AncovaPop"] + coeffancovaB["AncovaPop:AncovaRegWestern Pacific"], intercept = coeffancovaB["(Intercept)"] + coeffancovaB["AncovaRegWestern Pacific"], color = "orange", size = 1) +  # Recta para el grupo "Western Pacific" de AncovaReg
  labs(title = "Modelo ANCOVA con Interacción", x = "AncovaPop", y = "AncovaNew",colour = "Región") +
  scale_colour_manual(values = c(
    "Americas" = "red",  
    "Eastern Mediterranean" = "purple",  
    "Europe" = "green",  
    "South-East Asia" = "blue",  
    "Western Pacific" = "orange",  
    "Africa" = "yellow"  
  )) +
  theme_minimal()


# Ancova simple sen interacción
# ---------------------------------------------------------------------------------------
# log(NewHIV)= mu(Reg_1) + alpha_i + (delta)*log(Pop) + Erro
# ---------------------------------------------------------------------------------------

# Estimadores dos parámetros do modelo
coeffancovaB2 <- coef(modAncovaRegB2); coeffancovaB2
# interpretación dos parámetros

#Intervalos de confianza para os parámetros
confint(modAncovaRegB2)

#Contrastes de significación dos parámetros

#Varianza do modelo



ggplot(data.Ancova, aes(x = AncovaPop, y = AncovaNew, colour = AncovaReg,shape=AncovaReg)) +
  geom_point(shape=AncovaReg) +  # Puntos reales
  geom_abline(slope = coeffancovaB2["AncovaPop"], intercept = coeffancovaB2["(Intercept)"], color = "yellow", size = 1) + 
  geom_abline(slope = coeffancovaB2["AncovaPop"], intercept = coeffancovaB2["(Intercept)"] + coeffancovaB2["AncovaRegAmericas"], color = "red", size = 1) +  # Recta para el grupo "Americas" de AncovaReg
  geom_abline(slope = coeffancovaB2["AncovaPop"], intercept = coeffancovaB2["(Intercept)"] + coeffancovaB2["AncovaRegEastern Mediterranean"], color = "purple", size = 1) +  # Recta para el grupo "Eastern Mediterranean" de AncovaReg
  geom_abline(slope = coeffancovaB2["AncovaPop"], intercept = coeffancovaB2["(Intercept)"] + coeffancovaB2["AncovaRegEurope"], color = "green", size = 1) +  # Recta para el grupo "Europe" de AncovaReg
  geom_abline(slope = coeffancovaB2["AncovaPop"], intercept = coeffancovaB2["(Intercept)"] + coeffancovaB2["AncovaRegSouth-East Asia"], color = "blue", size = 1) +  # Recta para el grupo "South-East Asia" de AncovaReg
  geom_abline(slope = coeffancovaB2["AncovaPop"], intercept = coeffancovaB2["(Intercept)"] + coeffancovaB2["AncovaRegWestern Pacific"], color = "orange", size = 1) +  # Recta para el grupo "Western Pacific" de AncovaReg
  labs(title = "Modelo ANCOVA con Interacción", x = "AncovaPop", y = "AncovaNew",colour = "Región") +
  scale_colour_manual(values = c(
    "Americas" = "red",  
    "Eastern Mediterranean" = "purple",  
    "Europe" = "green",  
    "South-East Asia" = "blue",  
    "Western Pacific" = "orange",  
    "Africa" = "yellow"  
  )) +
  theme_minimal()
par(mfrow=c(2,2))



#Contraste do efecto da interacción
#Primeiro o modelo sinxelo, logo o complexo
anova(modAncovaRegB2,modAncovaRegB) #0.8547 logo é preferible o modelo sen interacción
#Desbotamos o modelo con interaccion

#Contraste do efecto da variable continua
modsencontinua <-lm(AncovaNew~AncovaReg)
anova(modsencontinua,modAncovaRegB2) #2.2e-16 logo hai un efecto da variable continua
#Estamos a comparar un modelo ancova cun modelo anova (sen continua)

#Contraste do efecto da variable categorica
modsencategorica <-lm(AncovaNew~AncovaPop)
anova(modsencategorica,modAncovaRegB2) #2.2e-16 logo hai un efecto da variable categorica
#Estamos a comparar un modelo ancova cun modelo lineal simple (sen categorica)

#Diagnose
plot(modAncovaRegB2) #Non hai influintes, non quitamos ningun.



# Interp


#Validación
#Residuos do modelo
resAncova <-  residuals(modAncovaRegB2)

shapiro.test(resAncova)



res.df <- data.frame(resAncova=resAncova,Reg=AncovaReg)
head(res.df)

#Normalidade por grupos
normalidad.df=data.frame(Reg=c(),p.value=c())
for (i in  levels(res.df$Reg)) {
  normalidad.df[i,1] <- i
  normalidad.df[i,2] <-shapiro.test(res.df[res.df$Reg == i, ]$resAncova)$p.value
}


#Interpretación do modelo final...




