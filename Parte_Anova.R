
# ---------------------------------------------------------------------------------------
# MODELO ANOVA. 
# Variable Categórica Rexión (Reg).
# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
# Comentarios:
#  - Temos un total de 6 grupos (6 valores da variable categórica Reg), cun número maior
#    a 10 observacións por grupo.
# ---------------------------------------------------------------------------------------

#  - Plantexamos o seguinte modelo ANOVA:
# ---------------------------------------------------------------------------------------
# log(NewHIV)= mu(Reg_1) + alpha_i + Erro
# ---------------------------------------------------------------------------------------

#Creamos un data.frame cos datos de interese
base <- na.omit(read.csv("BBDDHIV.csv",sep=";"))
dataReg=data.frame(CouCod=base$CouCod,AnovaReg=factor(base$Reg),AnovaNew=log(base$NewHIV))
par(mfrow=c(1,1))
plot(dataReg$AnovaReg, dataReg$AnovaNew, main = "Identificación de Atípicos", 
     xlab = "AnovaReg", ylab = "AnovaNew")

ggplot(dataReg, aes(x = AnovaReg, y = AnovaNew)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.6) + # Añade un poco de dispersión en el eje x para que los puntos no se superpongan
  labs(x = "Rexión", y = "Log(NewHIV)", title = "Diagrama de dispersión de log(NewHIV) por Rexión") +
  theme_minimal()

library(ggplot2)
library(dplyr) #paquete para manipular data.frames

#definimos a función que detecta atípicos
atipico <- function(x) {
  return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
}

#orixinamos unha columna indicando cales son as observacións atípicas
dataReg <- dataReg %>%
  group_by(AnovaReg) %>%
  mutate(outlier = ifelse(atipico(AnovaNew), CouCod, NA))
 
#gráfico ilustrativo e identificativo
ggplot(dataReg, aes(x = AnovaReg, y = AnovaNew, colour = AnovaReg, shape = AnovaReg)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  geom_text(aes(label=outlier), na.rm=TRUE, hjust=-.5)+
  theme(legend.position="none")
#páxina de onde se sacou o código da idea
#https://www.r-bloggers.com/2022/08/how-to-label-outliers-in-boxplots-in-ggplot2/

#novo data.frame de datos
dataReg2 <- dataReg %>% filter(is.na(outlier))
#filtramos atípicos
dataReg2 <- dataReg2 %>%
  group_by(AnovaReg) %>%
  mutate(outlier = ifelse(atipico(AnovaNew), CouCod, NA))

#representamos os datos, vemos que volve a haber un atípico.
ggplot(dataReg2, aes(x = AnovaReg, y = AnovaNew, colour = AnovaReg, shape = AnovaReg)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  geom_text(aes(label=outlier), na.rm=TRUE, hjust=-.5)+
  theme(legend.position="none")

#definimos a nova base eliminado os atípicos
dataReg3 <- dataReg2 %>% filter(is.na(outlier))
#novos atípicos
dataReg3 <- dataReg3 %>%
  group_by(AnovaReg) %>%
  mutate(outlier = ifelse(atipico(AnovaNew), CouCod, NA))

#representamos, vemos que non hai atípicos
ggplot(dataReg3, aes(x = AnovaReg, y = AnovaNew, colour = AnovaReg, shape = AnovaReg)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  geom_text(aes(label=outlier), na.rm=TRUE, hjust=-.5)+
  theme(legend.position="none")

#Podemos esquecernos dos data frames base, dataReg e dataReg2. Por claridade chamaremos dataAnova.
dataAnova=dataReg3
rm(dataReg,dataReg2,dataReg3)

attach(dataAnova)


#Definimos o modelo anova
modAnovaReg<- lm(AnovaNew ~ AnovaReg)
#Visualizamos a información básica do modelo
summary(modAnovaReg)


#Comentarios: 
# Todos as medias son positivas e a maior delas é asociada ao primeiro grupo (Africa)
# por eso o resto de estimadores (das desviación respecto do grupo de referencia)
# son negativas.
# Nótese tamén que a meirande parte deles son significativamente distintos de cero.
# A que semella ter máis problemas é a do grupo de Asia, que significativamente
# non difire moito da de Africa (pero cualitativamente si).
#Nótese que a estimación da desviación é de 2.075 (relacionada coa suma da varianza intragrupal)

nReg=length(AnovaReg) #tamaño da mostra
IReg=length(levels(AnovaReg)) #cantidade de grupos
names=levels(AnovaReg) #os nomes dos grupos
mu_localReg <- tapply(AnovaNew, AnovaReg, mean) #calculamos as medias locais
n_localReg <- tapply(AnovaNew, AnovaReg, length) #calculamos o total de observacións por grupo



#Test F aplicado ao modelo ANOVA
anova(modAnovaReg) #si
# Comprobamos que o p-valor é moi pequeno, polo tanto rexeitamos H0 (de que as medias son todas iguais)
#Co test F comparamos dous modelos, na nula temos un modelo simplificado e na alternativa temos 
# o modelo complexo, onde teeriamos que hai duas medias polo menos diferentes (entre elas)

#Validación


# Realizar la prueba de Shapiro-Wilk para cada grupo

normalidad_residuos <- dataAnova %>%
  group_by(AnovaReg) %>%
  mutate(residuo = AnovaNew - mu_localReg[AnovaReg] ) %>%# Calcular los residuos
  summarise(p_value = shapiro.test(residuo)$p.value, .groups = "drop")
# Prueba de Shapiro-Wilk para los residuos

# Ver resultados
print(normalidad_residuos)
#Observamos que a un nivel de 0.05 o grupo de Europa non segue a normalidade.

#O total dos datos non segue unha distribución normal !!!!
resultado_shapiro <- shapiro.test(dataAnova$AnovaNew)

# Ver los resultados
print(resultado_shapiro)

#Eliminamos o grupo de Europa
table(dataAnova$AnovaReg)
dataAnova=dataAnova[!(dataAnova$AnovaReg%in% c("Europe" )),]
dataAnova$AnovaReg <- droplevels(dataAnova$AnovaReg)
table(dataAnova$AnovaReg)

#Repetimos o modelo


#gráfico ilustrativo e identificativo
ggplot(dataAnova, aes(x = AnovaReg, y = AnovaNew, colour = AnovaReg, shape = AnovaReg)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter() +
  geom_text(aes(label=outlier), na.rm=TRUE, hjust=-.5)+
  theme(legend.position="none")

#Definimos o modelo anova

modAnovaReg2<- lm(dataAnova$AnovaNew ~ dataAnova$AnovaReg)
#Visualizamos a información básica do modelo
summary(modAnovaReg2)
attach(dataAnova)

#Comentarios: 
# Todos as medias son positivas e a maior delas é asociada ao primeiro grupo (Africa)
# por eso o resto de estimadores (das desviación respecto do grupo de referencia)
# son negativas.
# Nótese tamén que a meirande parte deles son significativamente distintos de cero.
# A que semella ter máis problemas é a do grupo de Asia, que significativamente
# non difire moito da de Africa (pero cualitativamente si).
#Nótese que a estimación da desviación é de 2.075 (relacionada coa suma da varianza intragrupal)

nReg=length(AnovaReg) #tamaño da mostra
IReg=length(levels(AnovaReg)) #cantidade de grupos
names=levels(AnovaReg) #os nomes dos grupos
mu_localReg <- tapply(AnovaNew, AnovaReg, mean) #calculamos as medias locais
n_localReg <- tapply(AnovaNew, AnovaReg, length) #calculamos o total de observacións por grupo







#Test F aplicado ao modelo ANOVA
anova(modAnovaReg2) #si
# Comprobamos que o p-valor bastante pequeno, polo tanto rexeitamos H0 (de que as medias son todas iguais)
#Co test F comparamos dous modelos, na nula temos un modelo simplificado e na alternativa temos 
# o modelo complexo, onde teeriamos que hai duas medias polo menos diferentes (entre elas)


#Volvemos a validar
#Validación


# Realizar la prueba de Shapiro-Wilk para cada grupo
normalidad_residuos <- dataAnova %>%
  group_by(AnovaReg) %>%
  mutate(residuo = AnovaNew - mu_localReg[AnovaReg] ) %>%# Calcular los residuos
  summarise(p_value = shapiro.test(residuo)$p.value, .groups = "drop")
# Ver resultados
print(normalidad_residuos)
#Observamos que a un nivel de 0.05 o grupo de Europa non segue a normalidade.

#O total dos datos non segue unha distribución normal !!!!
resultado_shapiro <- shapiro.test(dataAnova$AnovaNew)

# Ver los resultados
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




##Comparacións múltiples
#Bonferroni

#install.packages("ggplot2")
library(ggplot2)

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
rownames(Bonferroni)=ww

ggplot(Bonferroni, aes(y = rownames(Bonferroni), x = center,xmax=max(right)+1)) +
  geom_point(aes(color = "gold4"), size = 3) +  # Puntos azules
  geom_segment(aes(x = left, xend = right, y = rownames(Bonferroni), color = "slateblue"), size = 1) +  # Barras de intervalo suaves
  geom_point(aes(x = left), shape = 21, size = 3, fill = "white", color = "slateblue2") +  # Puntos al final izquierdo
  geom_point(aes(x = right), shape = 21, size = 3, fill = "white", color = "slateblue2") +  # Puntos al final derecho
  geom_text(aes(x = max(right) + 0.5, label = round(p.value,3)), hjust = 0, vjust = 0, size = 3) +  # Texto con p.adjust
  labs(
    title = "Intervalos de Confianza Axustados por Bonferroni",
    x = "Estimación", 
    y = "Comparacións"
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 0.15) +
  theme_minimal() +
  theme(legend.position = "none" )  # Ajustamos las etiquetas del eje y y eliminamos la leyenda

##Comparacións múltiples
#Tukey
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
rownames(Tukey)=ww


ggplot(Tukey, aes(y = rownames(Tukey), x = center,xmax=max(right)+1)) +
  geom_point(aes(color = "gold4"), size = 3) +  # Puntos azules
  geom_segment(aes(x = left, xend = right, y = rownames(Tukey), color = "slateblue"), size = 1) +  # Barras de intervalo suaves
  geom_point(aes(x = left), shape = 21, size = 3, fill = "white", color = "slateblue2") +  # Puntos al final izquierdo
  geom_point(aes(x = right), shape = 21, size = 3, fill = "white", color = "slateblue2") +  # Puntos al final derecho
  geom_text(aes(x = max(right) + 0.5, label = round(p.adjust,3)), hjust = 0, vjust = 0, size = 3) +  # Texto con p.adjust
  labs(
    title = "Intervalos de Confianza Axustados por Tukey",
    x = "Estimación", 
    y = "Comparacións"
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", size = 0.15) +
  theme_minimal() +
  theme(legend.position = "none" )  # Ajustamos las etiquetas del eje y y eliminamos la leyenda






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
