
# ---------------------------------------------------------------------------------------
# MODELO ANCOVA. 
# Variable Resposta Continua log(NewHIV)
# Variable Explicativa Categórica Rexión (Reg).
# Variable Explicativa Continua log(PreHIV)
# Variable Explicativa Continua log(Pop)

# ---------------------------------------------------------------------------------------

#Apliquemos un modelo que combina o multiple e anova para facer o ancova
# ---------------------------------------------------------------------------------------
# Comentarios:
#  - Temos un total de 6 grupos (6 valores da variable categórica Reg), cun número maior
#    a 10 observacións por grupo.
# ---------------------------------------------------------------------------------------

#  - Plantexamos o seguinte modelo ANCOVA:
# ---------------------------------------------------------------------------------------
# log(NewHIV)= mu(Reg_1) + alpha_i + (gamma_1+gamma_i)*log(PreHIV)+(delta_1+delta_i)*log(Pop) Erro
# ---------------------------------------------------------------------------------------

#Creamos un data.frame cos datos de interese
base <- na.omit(read.csv("BBDDHIV.csv",sep=";"))
data.Ancova=data.frame(AncCouCod=base$CouCod,AncovaReg=factor(base$Reg),AncovaNew=log(base$NewHIV),AncovaPre=log(base$PreHIV),AncovaPop=log(base$Pop))
attach(data.Ancova)


ggplot(data.Ancova, aes(x = AncovaPre, y = AncovaNew, colour = AncovaReg, shape = AncovaReg)) + 
  geom_point() +
  ggtitle("Intervalos de confianza para todos os parámetros") +
  theme_minimal()

modAncovaReg <- lm(AncovaNew~(AncovaPre+AncovaPop)*AncovaReg)
summary(modAncovaReg)

modAncovaRegfinal <- step(modAncovaReg)
summary(modAncovaRegfinal) 
par(mfrow=c(2,2))
plot(modAncovaRegfinal) #Non hai influintes, non quitamos ningun

library(dplyr)
#install.packages("plotly")
library(plotly)

# Modelo sin interacción (asegúrate de que no haya términos de interacción)
modAncovaRegfinal <- lm(AncovaNew ~ AncovaPre + AncovaPop + AncovaReg, data = data.Ancova)

# Crear un grid para predicción
grid_data <- expand.grid(
  AncovaPre = seq(min(data.Ancova$AncovaPre), max(data.Ancova$AncovaPre), length.out = 50),
  AncovaPop = seq(min(data.Ancova$AncovaPop), max(data.Ancova$AncovaPop), length.out = 50),
  AncovaReg = levels(data.Ancova$AncovaReg)
)

# Agregar predicciones
grid_data$Pred <- predict(modAncovaRegfinal, newdata = grid_data)

# Graficar planos interactivos
plot_ly() %>%
  add_markers(data = data.Ancova, x = ~AncovaPre, y = ~AncovaPop, z = ~AncovaNew, color = ~AncovaReg,
              marker = list(size = 3), name = "Datos originales") %>%
  add_surface(x = unique(grid_data$AncovaPre), 
              y = unique(grid_data$AncovaPop),
              z = matrix(grid_data$Pred, nrow = 50, byrow = TRUE),
              color = ~grid_data$AncovaReg, showscale = FALSE) %>%
  layout(scene = list(
    xaxis = list(title = "AncovaPre (log)"),
    yaxis = list(title = "AncovaPop (log)"),
    zaxis = list(title = "AncovaNew (log)")
  ))

plot_ly() %>%
  add_markers(data = data.Ancova, x = ~AncovaPre, y = ~AncovaPop, z = ~AncovaNew, color = ~AncovaReg,
              marker = list(size = 3), name = "Datos originales") %>%
  add_trace(data = grid_data,
            x = ~AncovaPre, y = ~AncovaPop, z = ~Pred, type = "scatter3d", mode = "lines",
            line = list(width = 2), color = ~AncovaReg, name = "Planos ajustados") %>%
  layout(scene = list(
    xaxis = list(title = "AncovaPre (log)"),
    yaxis = list(title = "AncovaPop (log)"),
    zaxis = list(title = "AncovaNew (log)")
  ))


anova(aov(modAncovaRegfinal))

