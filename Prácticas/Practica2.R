library(dendextend) # librería para graficar el dendograma
library(dplyr) # librería para sacar los primeros n clusters de un dendograma

path = "/Users/pedrovazquezg/OneDrive - Universidad de Guanajuato/Universidad/01 Licenciaturas/Lic. Sistemas de Información Administrativa/01 Materias/Inteligencia de negocios/Datasets/customer_data.csv"
customer_data <- read.csv(path)

# *Calcular promedio de cada característica (income y spend).
income_average <- mean(customer_data$INCOME)
spend_average <- mean(customer_data$SPEND)


# *Mostrar gráfica de dispersión de los datos.
plot(customer_data$INCOME, customer_data$SPEND, asp=1, main="Customer Data", xlab="Income", ylab="Spend")
points(income_average, spend_average, pch=20, col="maroon", bg="lightblue", cex=1.5)


# *Cuartiles
boxplot(customer_data$INCOME ~ customer_data$SPEND, main="Grafica por cuartiles") # Extra


# *Obtener el número optimo de grupos aplicando el método del codo.
set.seed(3456)
# kmeans
km2 <- kmeans(customer_data, 2)
km3 <- kmeans(customer_data, 3)
km4 <- kmeans(customer_data, 4)
km5 <- kmeans(customer_data, 5)
km6 <- kmeans(customer_data, 6)
km7 <- kmeans(customer_data, 7)
km8 <- kmeans(customer_data, 8)
km9 <- kmeans(customer_data, 9)
km10 <- kmeans(customer_data, 10)
optimization <- data.frame(clusters=c(2:10), wss=rep(0,9))
optimization[1,2] = as.numeric(km2$tot.withinss)
optimization[2,2] = as.numeric(km3$tot.withinss)
optimization[3,2] = as.numeric(km4$tot.withinss)
optimization[4,2] = as.numeric(km5$tot.withinss)
optimization[5,2] = as.numeric(km6$tot.withinss)
optimization[6,2] = as.numeric(km7$tot.withinss)
optimization[7,2] = as.numeric(km8$tot.withinss)
optimization[8,2] = as.numeric(km9$tot.withinss)
optimization[9,2] = as.numeric(km10$tot.withinss)
plot(optimization$wss ~ optimization$clusters, type="b", ylim=c(90000,430000), main="Elbow method",
     xlab="Number of clusters", ylab="Within sum of square error", pch=17, col="black")

# hierachical Tree
#dendrograma
hc <- hclust(dist(customer_data, method = "euclidean"), method="ward.D2")
dendrogram <- as.dendrogram(hc)
dend_six_color <- color_branches(dendrogram, k=6)
plot(dend_six_color, leaflab="none", horiz=TRUE, main="Dendogram", xlab="Height")

#grafica del codo
getWSS <- function(hc, n) {
  sumatoria_j <- rep(0, n)

  indices_grupo <- cutree(hc, n)
  etiquetas <- as.data.frame(customer_data %>% group_by(cutree(hc, n)) %>% 
                                summarise(avg_income = median(INCOME), avg_spend = median(SPEND)))

  for (i in 1:length(indices_grupo)) {
    distancia <- customer_data[i, 1:2] - etiquetas[indices_grupo[i], 2:3]
    sumatoria_j[indices_grupo[i]] = sumatoria_j[indices_grupo[i]] + sum(distancia[1]^2 + distancia[2]^2)
  }

  return(sum(sumatoria_j))
}

optimization_hc <- data.frame(clusters=c(2:10), wss=rep(0,9))
optimization_hc[1,2] = as.numeric(getWSS(hc, 2))
optimization_hc[2,2] = as.numeric(getWSS(hc, 3))
optimization_hc[3,2] = as.numeric(getWSS(hc, 4))
optimization_hc[4,2] = as.numeric(getWSS(hc, 5))
optimization_hc[5,2] = as.numeric(getWSS(hc, 6))
optimization_hc[6,2] = as.numeric(getWSS(hc, 7))
optimization_hc[7,2] = as.numeric(getWSS(hc, 8))
optimization_hc[8,2] = as.numeric(getWSS(hc, 9))
optimization_hc[9,2] = as.numeric(getWSS(hc, 10))
plot(optimization_hc$wss ~ optimization_hc$clusters, type="b", ylim=c(90000,450000), main="Elbow method",
     xlab="Number of clusters", ylab="Within sum of square error", pch=17, col="black")


# *Generar graficas de disperción utilizando k=6 y k=7 con el metodo kmeans y hierarchical tree
# kmeans
#k = 6
plot(customer_data$INCOME, customer_data$SPEND, asp=1, col=km6$cluster, main="kmeans k=6", xlab="income", ylab="spend")
points(km6$centers[,1], km6$centers[,2], pch=23, col="maroon", bg="lightblue", cex=2.5)
text(km6$centers[,1], km6$centers[,2], cex=1, col="black", attributes(km6$centers)$dimnames[[1]])
#k = 7
plot(customer_data$INCOME, customer_data$SPEND, asp=1, col=km7$cluster, main="kmeans k=7", xlab="income", ylab="spend")
points(km7$centers[,1], km7$centers[,2], pch=23, col="maroon", bg="lightblue", cex=2.5)
text(km7$centers[,1], km7$centers[,2], cex=1, col="black", attributes(km7$centers)$dimnames[[1]])

# hiearchical tree
#k = 6
etiquetas6 <- as.data.frame(customer_data %>% group_by(cutree(hc, 6)) %>% 
                             summarise(avg_income = median(INCOME), avg_spend = median(SPEND)))
plot(customer_data$INCOME, customer_data$SPEND, col=cutree(hc, 6), main="hierarchicarl tree k=6", 
     xlab="income", ylab="spend")
points(etiquetas6[,2], etiquetas6[,3], pch=23, col="maroon", bg="white", cex=2.5)
text(etiquetas6[,2], etiquetas6[,3], cex=1, col="black", etiquetas6[,1])
#k = 7
etiquetas7 <- as.data.frame(customer_data %>% group_by(cutree(hc, 7)) %>% 
                              summarise(avg_income = median(INCOME), avg_spend = median(SPEND)))
plot(customer_data$INCOME, customer_data$SPEND, col=cutree(hc, 7), main="hierarchicarl tree k=7", 
     xlab="income", ylab="spend")
points(etiquetas7[,2], etiquetas7[,3], pch=23, col="maroon", bg="white", cex=2.5)
text(etiquetas7[,2], etiquetas7[,3], cex=1, col="black", etiquetas7[,1])


# *Conclusiones
#¿Que información destaca de la segmentación generada?
# Observamos que en general los clientes gastan mas o menos lo mismo, sin importar sus ingresos. Unicamente cuando
# los ingresos son algo mayores o menos se forman grupos con gente que gasta mas. Finalmente, se observa que el 
# grupo de los clientes que gana mas gasta lo mismo que el promedio de clientes.

#¿Cree que estos grupos ayuden a generar estrategias de marketing enfocadas a grupos?
# Sí,en base a los grupos que se generaron se puede hacer una segmentación de campañas de marketing.
