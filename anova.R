# MODELO ANOVA. 
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
#   problemas é o asociado a Asia pero segue tendo un valor crítico significativo (0.02372).

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
# a hipótese de normalidade dos residuos.

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
# MODELO ANOVA. 
# Variable Categórica Relixión (Rel).
# ---------------------------------------------------------------------------------------
dataRel=data.frame(CouCod=base$CouCod,AnovaRel=factor(base$Rel),AnovaNew=log(base$NewHIV))
attach(dataRel)
table(dataRel$AnovaRel)
levels(dataRel$AnovaRel)[levels(dataRel$AnovaRel) %in% c("Buddhism", "Shinto / Buddhism")] <- "Buddhism"
levels(dataRel$AnovaRel)[levels(dataRel$AnovaRel) %in% c("Christianity / Islam", "Christianity")] <- "Christianity"
dataRel=dataRel[!(dataRel$AnovaRel%in% c("Hinduism",  "None (51%)" ,"Judaism" )),]
dataRel$AnovaRel <- droplevels(dataRel$AnovaRel )
table(dataRel$AnovaRel)



#orixinamos unha columna indicando cales son as observacións atípicas
dataRel <- dataRel %>%
  group_by(AnovaRel) %>%
  mutate(outlier = ifelse(atipico(AnovaNew), CouCod, NA))

#gráfico ilustrativo e identificativo
ggplot(dataRel, aes(x = AnovaRel, y = AnovaNew, colour = AnovaRel, shape = AnovaRel)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  geom_text(aes(label=outlier), na.rm=TRUE, hjust=-.5)+
  theme(legend.position="none")

#novo data.frame de datos
dataRel2 <- dataRel %>% filter(is.na(outlier))
#filtramos atípicos
dataRel2 <- dataRel2 %>%
  group_by(AnovaRel) %>%
  mutate(outlier = ifelse(atipico(AnovaNew), CouCod, NA))

#representamos os datos, vemos que volve a haber un atípico.
ggplot(dataRel2, aes(x = AnovaRel, y = AnovaNew, colour = AnovaRel, shape = AnovaRel)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  geom_text(aes(label=outlier), na.rm=TRUE, hjust=-.5)+
  theme(legend.position="none")



#Modelo ANOVA
modAnovaRel=lm(dataRel2$AnovaNew ~ dataRel2$AnovaRel)
summary(modAnovaRel)

#Datos para construir cousas
nRel=length(dataRel$AnovaRel); nRel
IRel=length(levels(dataRel$AnovaRel));IRel
mu_localRel <- tapply(dataRel$AnovaNew, dataRel$AnovaRel, mean);mu_localRel
n_localReg <- tapply(dataRel$AnovaNew, dataRel$AnovaRel, length);n_localReg 


anova(modAnovaRel) #si
TukeyHSD(aov(modAnovaRel)) #Todas son iguais.

step(modAnovaRel) #É mellor quedarse cun modelo sen desviacións por grupos.e

