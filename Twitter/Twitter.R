if (!"package:RTextTools" %in% search()) { 
  install.packages("RTextTools")
}

if (!"package:e1071" %in% search()) { 
  install.packages("e1071")
}

library(e1071)
library(RTextTools)


setwd("C:/Users/RobertoFrías/Documents/Mi espacio de trabajo de R/Twitter")
happy.train <- readLines("happyTrain.txt")
sad.train <- readLines("sadTrain.txt")
happy.test <- readLines("happyTest.txt")
sad.test <- readLines("sadTest.txt")

tweets.train <- c(happy.train, sad.train)
tweets.test <- c(happy.test, sad.test)
tweets.all <- c(tweets.train, tweets.test)

class.train <- c(rep("happy", length(happy.train)), rep("sad", length(sad.train)))
class.test <- c(rep("happy", length(happy.test)), rep("sad", length(sad.test)))
class.all <- as.factor(c(class.train, class.test))

mat = create_matrix(tweets.all, language = "english", removeStopwords = FALSE,
                    removePunctuation = FALSE,
                    removeNumbers = TRUE, stemWords = FALSE, tm::weightTfIdf)
mat = as.matrix(mat)
dim(mat)
View(mat)


# Creamos un modelo 
model.df <- naiveBayes(mat[1:160,], class.all[1:160])

# Predecimos las clases de los 20 de test
predict(model.df, mat[161:180,])

# Predecimos la probabilidad de cada clase en los 20 de test
predict(model.df, mat[161:180,], type = "raw")

# Obtenemos una matriz de confusion 
pred <- predict(model.df, mat[161:180,])
table(pred, class.all[161:180])



# Matriz 1, con frecuencia de palabra minima de 5, quitando las "stop words", puntuacion, numeros y dejando solo la
# raiz de las palabras, todo a minusculas.

mat1 = create_matrix(tweets.all, language = "english",minDocFreq = 5, removeStopwords = TRUE,
                    removePunctuation = TRUE,
                    removeNumbers = TRUE, stemWords = TRUE, toLower = TRUE, tm::weightTfIdf)
mat1 = as.matrix(mat1)
dim(mat1)
View(mat1)


# Creamos un modelo 
model.df1 <- naiveBayes(mat1[1:160,], class.all[1:160])

# Predecimos las clases de los 20 de test
predict(model.df1, mat1[161:180,])

# Predecimos la probabilidad de cada clase en los 20 de test
predict(model.df1, mat1[161:180,], type = "raw")

# Obtenemos una matriz de confusion 
pred <- predict(model.df1, mat1[161:180,])
table(pred, class.all[161:180])

# Resultados matriz 1: Tenemos una matriz de 641 elementos en las que hay "basura", y con estos parametros hemos 
# conseguido 9 aciertos y 1 fallo en sad que esta cerca del objetivo, aunque no hemos conseguido ni un solo acierto 
# en happy.
# Esta matriz esta lejos de ser la optima para el resultado que se desea.


# Matriz 2, quitamos puntuacion y numeros, todo en minusculas.

mat2 = create_matrix(tweets.all, language = "english", removeStopwords = FALSE,
                     removePunctuation = TRUE,
                     removeNumbers = TRUE, stemWords = FALSE, toLower = TRUE, tm::weightTfIdf)
mat2 = as.matrix(mat2)
dim(mat2)
View(mat2)


# Creamos un modelo 
model.df2 <- naiveBayes(mat2[1:160,], class.all[1:160])

# Predecimos las clases de los 20 de test
predict(model.df2, mat2[161:180,])

# Predecimos la probabilidad de cada clase en los 20 de test
predict(model.df2, mat2[161:180,], type = "raw")

# Obtenemos una matriz de confusion 
pred <- predict(model.df2, mat2[161:180,])
table(pred, class.all[161:180])

# Resultado matriz 2: Tenemos una matriz de 773 elementos, y con estos parametros hemos conseguido 8 aciertos y 
# 2 fallos en sad que esta cerca del objetivo, 5 aciertos y 5 fallos en happy.
# Esta matriz esta cerca de ser una solucion valida.


# Matriz 3, no quitamos ningun parametro.

mat3 = create_matrix(tweets.all, language = "english", removeStopwords = FALSE,
                   removePunctuation = FALSE,
                     removeNumbers = FALSE, stemWords = FALSE, toLower = FALSE, tm::weightTfIdf)
mat3 = as.matrix(mat3)
dim(mat3)
View(mat3)


# Creamos un modelo 
model.df3 <- naiveBayes(mat3[1:160,], class.all[1:160])

# Predecimos las clases de los 20 de test
predict(model.df3, mat3[161:180,])

# Predecimos la probabilidad de cada clase en los 20 de test
predict(model.df3, mat3[161:180,], type = "raw")

# Obtenemos una matriz de confusion 
pred <- predict(model.df3, mat3[161:180,])
table(pred, class.all[161:180])
  
# Resultado matriz 3: Tenemos una matriz de 1011 elementos, y con estos parametros hemos conseguido 7 aciertos y 
# 3 fallos en sad que esta cerca del objetivo, 6 aciertos y 4 fallos en happy.
# Esta matriz esta cerca de ser una solucion valida.


# Matriz 4, aplicamos la frecuencia minima de palabras y una longitud minima de palabra, y eliminamos las "stop words"
# y dejamos solo la raiz de las palabras.

mat4 = create_matrix(tweets.all, language = "english",minDocFreq = 10, minWordLength = 3, removeStopwords = TRUE,
                     removePunctuation = FALSE,
                     removeNumbers = FALSE, stemWords = TRUE, toLower = FALSE, tm::weightTfIdf)
mat4 = as.matrix(mat4)
dim(mat4)
View(mat4)


# Creamos un modelo 
model.df4 <- naiveBayes(mat4[1:160,], class.all[1:160])

# Predecimos las clases de los 20 de test
predict(model.df4, mat4[161:180,])

# Predecimos la probabilidad de cada clase en los 20 de test
predict(model.df4, mat4[161:180,], type = "raw")

# Obtenemos una matriz de confusion 
pred <- predict(model.df4, mat4[161:180,])
table(pred, class.all[161:180])

# Resultado matriz 4: Tenemos una matriz de 881 elementos, y con estos parametros hemos conseguido 9 aciertos y 
# 1 fallo en sad que esta cerca del objetivo, 3 aciertos y 7 fallos en happy.
# Esta matriz esta lejos de ser una solucion valida.


# Matriz 5, aplicamos la frecuencia minima de palabras y una longitud minima de palabra, eliminamos puntuacion y 
# numeros, todo en minusculas.

mat5 = create_matrix(tweets.all, language = "english",minDocFreq = 10, minWordLength = 3, removeStopwords = FALSE,
                     removePunctuation = TRUE,
                     removeNumbers = TRUE, stemWords = FALSE, toLower = TRUE, tm::weightTfIdf)
mat5 = as.matrix(mat5)
dim(mat5)
View(mat5)


# Creamos un modelo 
model.df5 <- naiveBayes(mat5[1:160,], class.all[1:160])

# Predecimos las clases de los 20 de test
predict(model.df5, mat5[161:180,])

# Predecimos la probabilidad de cada clase en los 20 de test
predict(model.df5, mat5[161:180,], type = "raw")

# Obtenemos una matriz de confusion 
pred <- predict(model.df5, mat5[161:180,])
table(pred, class.all[161:180])

# Resultado matriz 5: Tenemos una matriz de 773 elementos, y con estos parametros hemos conseguido 8 aciertos y 
# 2 fallos en sad que esta cerca del objetivo, 5 aciertos y 5 fallos en happy.
# Esta matriz esta lejos de ser una solucion valida.

