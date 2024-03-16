
nuevo_dir(C:Practicascorrelacionyregresion)

install.packages("readxl")
library(readxl)
data <- read_excel("C:/data/data.xls")
view(data)
print(data)

#EJERCICIO 1(TEORIA,pdf)
#EJERCICIO 2(TEORIA,pdf)

#EJERCICIO 3

correlacion_datos <-cor(data)
print(correlacion_datos)



#ACTIVIDAD 4


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0 ,1))
  Cor <- abs(cor(x, y)) 
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor*Cor)
}
```


pairs(data,
      upper.panel = panel.cor,
      lower.panel = panel.smooth)


#EJERCICIO 5

library(correlation)
matriz <- correlation(data)
matriz
```

#EJERCICIO 6


library(ggpubr)
library(ggplot2)
ggscatter(data, x = "altura", y = "peso",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "altura piezas (mm)", ylab = "peso piezas (mg)")

#EJERCICIO 7


library(corrplot)
corrplot(cor(data))
```

#EJERCICIO 8

#a)  


distancia <- c( 1.1,100.2,90.3,5.4,57.5,6.6,34.7,65.8,57.9,86.1)
n_piezas <- c(110,2,6,98,40,94,31,5,8,10)
datos_2 <- data.frame(distancia, n_piezas)
print(datos_2)
```
#b) Correlacion


correlacion_datos_2 <- cor(datos_2)
print(correlacion_datos_2)
```

#c) Significancia

significancia_datos_2 <- cor.test(datos_2$distancia, datos_2$n_piezas)$p.value
print(significancia_datos_2)
```

#d) intervalo de confianza


intervaloconfianza_datos_2 <- cor.test(datos_2$distancia, datos_2$n_piezas)$conf.int
print(intervaloconfianza_datos_2)
```
#EJERCICIO 9
#a)realcion lineal
x <- 1:10
plot(x, x^2)  
y_lineal <- 2*x + 1 
plot(x, y_lineal, main = "Relación Lineal", xlab = "X", ylab = "Y", pch = 19, col = "pink")
abline(lm(y_lineal ~ x), col = "green") 
View(plot)

#b)relacion monotona
y_monotona <- log(x)
plot(x, y_monotona, main = "Relación Monótona", xlab = "X", ylab = "Y", pch = 19, col = "blue")
lines(x, y_monotona, col = "red")
