# 1. Cargar base de datos mtcars
mtcars


# 2. Mostrar la matriz de gráfica de dispersión
pairs(mtcars)


# 3. De acuerdo a las características observads en la gráfica anterior
mpg_values <- mtcars[, names(mtcars) %in% c("mpg")]

# 3.1 Desarrolle un modelo de regresión lineal simple (usando wt)
regresion_1 <- lm(mpg ~ wt, mtcars) # y = 37.28 + (-5.34)*x

# 3.2 Desarrolle un modelo de regresión lineal múltiple (usando wt, qsec, am)
regresion_2 <- lm(mpg ~ wt + qsec + am, mtcars) # y = 9.61 + (-3.917)x1 + 1.22x2 + 2.93x3

# 3.3 Desarrolle un modelo de regresión lineal múltiple (todos los atributos).
regresion_3 <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, mtcars)


# 4. Con cada modelo de regresión generado, realice las predicciones de todos los autos, ¿qué modelo diría que es el mejor? ¿porque?
# Modelo 1
predict_1 <- predict.lm(regresion_1, mtcars, interval="predict")
predict_1_values <- predict_1[1:dim(predict_1)[1]]

residuo <- 0
for (x in 1:length(mpg_values)) {
  residuo <- residuo + (mpg_values[x] - predict_1_values[x])
}
residuo <- residuo/length(mpg_values) # 8.881784e-15

# Modelo 2
predict_2 <- predict.lm(regresion_2, mtcars, interval="predict")
predict_2_values <- predict_2[1:dim(predict_2)[1]]

residuo <- 0
for (x in 1:length(mpg_values)) {
  residuo <- residuo + (mpg_values[x] - predict_2_values[x])
}
residuo <- residuo/length(mpg_values) # 7.71605e-15

# Modelo 3
predict_3 <- predict.lm(regresion_3, mtcars, interval="predict")
predict_3_values <- predict_3[1:dim(predict_3)[1]]

residuo <- 0
for (x in 1:length(mpg_values)) {
  residuo <- residuo + (mpg_values[x] - predict_3_values[x])
}
residuo <- residuo/length(mpg_values) #  9.880985e-15

# El mejor modelo fue el segundo, en el que se usaron las propiedades 'wt', 'qsec' y 'am' ya que con este
# modelo obtuvimos un residuo promedio de 7.716e-15, el cual fue el menor de los 3 residuos obtenidos
# por los 3 modelos

