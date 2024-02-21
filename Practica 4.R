nuevo_dir = "C:/Practica 4"
setwd(nuevo_dir)

set.seed(123)

#Ejercicio 1

n_registros = 200

yacimiento = sample(1:10, n_registros, replace = TRUE)
tipo_artefacto = sample(c("Pottery", "Tools", "Jewerly", "Weapons"), n_registros, replace = TRUE) 
numero_artefactos = sample(1:1000, n_registros, replace = TRUE)
contextos = sample(c("Habitacional", "Funerario", "Otros"), n_registros, replace = TRUE)
latitud = runif(n_registros, min=0, max=90)
longitud = runif(n_registros, min = -180, max = 180)

archeological_data = data.frame(
  yacimiento = yacimiento,
  tipo_artefacto = tipo_artefacto,
  numero_artefactos = numero_artefactos,
  contextos = contextos,
  latitud = latitud,
  longitud = longitud
  )
View((archeological_data))

#Ejercicio 2
media_numero_artefactos = mean(numero_artefactos)
print(media_numero_artefactos)



cuartil_numero_artefactos = quantile(numero_artefactos,probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
print(cuartil_numero_artefactos)

#Ejercicio 3 

#Cuando la media (obtenida en el ejercicio 2) y la mediana representada por la linea del grafico coinciden, por lo que es simetrica 

Histograma_numero_artefactos = hist(archeological_data$numero_artefactos, 
     main = "Histograma de Número de Artefactos",
     xlab = "Número de Artefactos",
     ylab = "Frecuencia")

#Ejercicio 4

#

boxplot(archeological_data$numero_artefactos, 
        main = "Gráfico numero artefactos",
        ylab = "Variable")

#Ejercicio 5


Tabla_media_numero = table(media_numero_artefactos)
Tabla_yacimientos = table(yacimiento)

Grafico_barras = barplot(Tabla_media_numero$ Tabla_yacimientos, 
        main = "Número Medio de Artefactos por Yacimiento",
        xlab = "Yacimiento",
        ylab = "Número Medio de Artefactos")

#Ejercicio 6 

Mapa_calor = ggplot(archeological_data, aes(x = longitud, y = latitud)) +
  geom_bind2d() +
  labs(title = "Densidad de artefactos", x = "Longitud", y = "Latitud")


print(Mapa_calor)

#Ejercicio 7
numero_total_artefactos = sum(yacimiento + numero_artefactos)
print(numero_total_artefactos)

#Ejercicio 8
mediana_artefactos_yacimiento = aggregate(numero_artefactos ~yacimiento, data = archeological_data, FUN = median)
print(mediana_artefactos_yacimiento)

#Ejercicio 9 
desviacion_estandar_artefactos_yacimiento = aggregate(numero_artefactos ~yacimiento, data = archeological_data, FUN = sd)
print(desviacion_estandar_artefactos_yacimiento)

#Ejercicio 10 

max_yacimiento <- archeological_data$yacimiento[which.max(archeological_data$numero_artefactos)]
print(max_yacimiento)
max_cantidad <- max(archeological_data$numero_artefactos)
print(max_cantidad)

#Ejercicio 11
Tabla_resumen <- aggregate(numero_artefactos ~ yacimiento, archeological_data, function(numero_artefactos) c(media = mean(numero_artefactos), mediana = median(numero_artefactos), desviacion_estandar = sd(numero_artefactos)))
print(Tabla_resumen)

#Ejercicio 12

boxplot(numero_artefactos ~ yacimiento, data = archeological_data, 
        main = "Distribución de Artefactos por Yacimiento",
        xlab = "Yacimiento", ylab = "Cantidad de Artefactos",
        col = "pink")