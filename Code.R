library(readr)

datak <- read_csv("C:/Users/Usuario/Desktop/Dataset_Student.csv", 
                 col_names = FALSE, skip = 1)

datak <- datak[,-(1:8),drop=FALSE] 
View(datak)
datak1 <- datak[,-(17:19),drop=FALSE]
View(datak1)
datak2 <- datak1[,-(18),drop=FALSE] 
View(datak2)

# Validacion Cruzada
cant_datos = dim(datak2)[1]
train = 0.70
test = 1-train
set.seed(12)
ran = sample(cant_datos)
datak3 = datak2[ran,]
View(datak3)

# Training
datak4 = datak3[1:round((cant_datos*train)),]
View(datak4)
reg = lm(datak4$X28~.,data = datak4)
summary(reg)

# Test
datak5 = datak3[(round((cant_datos*train))+1):(dim(datak2)[1]),]
pred = predict(reg,newdata = datak5)
View(datak5)

table(datak2$X9) #Sirve para poner en una tabla la variable por frecuencia 

# 1: limpiar el fichero csv (todas las variables)
# 2: Agrupar por provincia dentro del mismo R
# 3: acabar la intro

### Random Cross Validation
library(e1071)
library(caTools)

set.seed(2)
split = sample.split(datak2$X28, SplitRatio = .7)
summary(split)

### Training
training_set = subset(datak2, split == TRUE)

### Test
test_set = subset(datak2, split == FALSE)

summary(test_set)

### Entrenar el Clasificador
# Funcion kernel determina la funcion y va aprendiendo la curva
classifier1 = svm(formula = X28~., data = training_set, 
                  type = 'eps-regression', kernel = 'linear')

### Predicciones del clasificador
test_pred1 = predict(classifier1, type = 'response', 
                     newdata = test_set[-17])
summary(test_pred1)


### Matriz Confusion
library(ggplot2)
library(lattice)
library(caret)
cm1 = table(test_set[,17], test_pred1)
con = confusionMatrix(test_pred1,test_set[,17])
accuracy = con$overall[1]
precision = con$byClass[,5]
recall = con$byClass[,6]
f1 = con$byClass[,7]


