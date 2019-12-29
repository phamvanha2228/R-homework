# setwd("G:\\MESIO\\Computacio\\R\\Practica")
library(readxl)
sea.deep <- read_excel("sea_temp.xlsx")

#EJERCICIO 3.a
graficas_ej3a <- function(anos, profundidades, df) {
  par(mfrow=c(1, length(anos) * length(profundidades) + 1))
  diferencias_matr = matrix( nrow = length(anos) * length(profundidades), ncol = 12)
  diferencias_matr_fila = 1
  for (ano in anos){
    for (profundidad in profundidades){
      datos_ano_prof <- subset(df, df$year==ano & df$depth==profundidad)
      diferencias <- c()
      for (i in 1:11){
        diferencias = c(diferencias, datos_ano_prof[i, 7] - datos_ano_prof[i+1, 7])
      }
      diferencias = c(diferencias, datos_ano_prof[12, 7] - datos_ano_prof[1, 7])
      plot(1:12, diferencias, type = "o",
           main = paste("Diferencia intermensual 
                      (a?o=", ano, ", profundidad=", profundidad, ")"),
           cex.main=1,
           xlab = "relaci?n mensual",
           ylab = "Diferencia de temperatura (?C)",
           col = "purple",
           lwd = 3,
           xlim = c(1, 12),
           ylim = c(-4, 4),
      )
      diferencias_matr[diferencias_matr_fila, ] = as.numeric(diferencias)
      diferencias_matr_fila = diferencias_matr_fila + 1
    }
  }
  diferencias_medias = c() #Almacena las doce medias (una para cada mes) de las diferencias calculadas anteriormente
  n_filas = dim(diferencias_matr)[1]
  n_columnas = dim(diferencias_matr)[2]
  print(diferencias_matr)
  for (columna in 1:n_columnas){
    diferencias_medias = c(diferencias_medias, (sum(diferencias_matr[, columna])/n_filas))
  }
  print(diferencias_medias)
  plot(1:12, diferencias_medias, type = "o",
       main = "medias de las diferencias
       calculadas anteriormente",
       cex.main=1,
       xlab = "relaci?n mensual",
       ylab = "Diferencia de temperatura (?C)",
       col = "red",
       lwd = 3,
       xlim = c(1, 12),
       ylim = c(-4, 4),
  )
}

graficas_ej3b <- function(anos, profundidades, df)
{
  par(mfrow=c(1, length(anos) * length(profundidades) + 1))
  diferencias_matr = matrix( nrow = length(anos) * length(profundidades), ncol = 12) #matriz para almacenar las diferencias de cada (a?o, profundidad) para luego calcular la media
  diferencias_matr_fila = 1
  for (ano in anos){
    for (profundidad in profundidades){
      datos_ano_prof <- subset(df, df$year==ano & df$depth==profundidad)
      diferencias <- c()
      for (i in 1:12){
        diferencias = c(diferencias, datos_ano_prof[i, 7] - datos_ano_prof[i+13, 7])
      }
    }
    plot(1:12, diferencias, type = "o",
         main = paste("Diferencia entre la temperatura 
                        los a?os anteriores (a?o=", ano, ", profundidad=", profundidad, ")"),
         cex.main=1,
         xlab = "relaci?n del mes con el de los a?os anteriores",
         ylab = "Diferencia de temperatura (?C)",
         col = "purple",
         lwd = 3,
         xlim = c(1, 12),
         ylim = c(-4, 4) )
    diferencias_matr[diferencias_matr_fila, ] = as.numeric(diferencias)
    diferencias_matr_fila = diferencias_matr_fila + 1
  }
  diff.mean <- c()
  for (i in unique(df$year))
  {
    dif.year <- c()
    dataset <- subset(df, df$year==i & df$depth==profundidades)
    r <- 1
    while (r<13)
    {
      dif.year = c(dif.year, dataset[r, 7] - dataset[r+13, 7])
      r=r+1
    }
    diff.mean = c(diff.mean,mean(as.numeric(dif.year)))
  }
  plot(as.vector(unique(df$year)), diff.mean, type = "o",
       main = paste("Average difference of all the years" ),
       xlab = "years",
       ylab = "Temperature",
       col = "purple",
       lwd = 3,
       ylim = c(-4, 4))
  
  
}

graficas_ej3b( c(2011), c(0), sea.deep)



