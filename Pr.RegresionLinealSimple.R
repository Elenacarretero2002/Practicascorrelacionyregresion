

#EJERCICIO 8
cuentas <- c(100,2,6,98,40,94,31,5,8,10)
distancia <- c(1.1,100.2,90.3,5.4,57.5,6.6,34.7,65.8,57.9,86.1)
modelo <- lm(cuentas ~ distancia)
summary(modelo)

#EJERCICIO 11

x_bar <- mean(distancia)
y_bar <- mean(cuentas)

b_1 <- sum((distancia - x_bar) * (cuentas - y_bar)) / sum((distancia - x_bar)^2)

b_0 <- y_bar - b_1 * x_bar

print(paste("b_0 (intercepto):", b_0))
print(paste("b_1 (pendiente):", b_1))

#EJERCICIO 12

valor_real <- 110  
valor_estimado <- 92.3397   
error <- valor_real - valor_estimado
print(error)

#EJERCICIO 13

cuentas_ <- c(6,98,40,94,31,5,8,10)
predicciones <- c(-6.682842,85.520196,28.938591,84.216973,53.69983,19.924631,28.504183,-2.121561)
modelo <- lm(cuentas_ ~ predicciones)
summary(modelo)
datos <- data.frame(cuentas_,predicciones)

#EJERCICIO 14
shapiro.test(residuals(modelo))
# Gráfico Q-Q
qqnorm(residuals(modelo))
qqline(residuals(modelo), col = "red")

#EJERCICIO 15

set.seed(123) 
indices_entrenamiento <- sample(1:length(cuentas_), 0.7 * length(cuentas_)) 
datos_entrenamiento <- datos[indices_entrenamiento, ]
datos_prueba <- datos[-indices_entrenamiento, ]

#EJERCICIO 16
library(caret)
control <- trainControl(method = "cv", number = 5)
modelo_cv <- train(cuentas_ ~ ., data = datos, method = "lm", trControl = control)
print(modelo_cv)

#EJERCICIO 19

resumen <- summary(modelo)
r_cuadrado <- resumen$r.squared
print(r_cuadrado)
