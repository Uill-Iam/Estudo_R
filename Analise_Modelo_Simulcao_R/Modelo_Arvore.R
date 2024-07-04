library(readxl)
Cadastral <- read_excel("C:/Users/uilli/Downloads/Cadastral.xlsx")
View(Cadastral)



### 1 Frequencia na vairavel Sexo
frequencia <- table(Cadastral$Sexo)
frequencia


####2 Ordenar pela variavel ID

Ordenar<- Cadastral[order(Cadastral$ID),]
Ordenar

### 3 removas duplicados

A <- Cadastral[!duplicated(Cadastral$ID,formLast = TRUE),]
dim(A)

### 4
Frequencia_A <-  table(A$Sexo)
Frequencia_A


### 5 
A$Data_atual <-  Sys.Date()
View(A)

### 6

is.numeric(A$salario)

### 7
min(A$salario)
max(A$salario)

#### 8


A$fx_salario <- cut(A$salario, breaks = c(1574,3000,5000,7000,135000),
                        labels =  c("A","B","C","D"))
View(A)


########## 9

library(readxl)
B <- read_excel("C:/Users/uilli/Downloads/Transacional.xlsx")
View(B)

unindoAB <- merge(A,B, by = "ID", all.x = T)
View(unindoAB)

##### 10

unindoAB$comprometimento_Renda <- unindoAB$ValorEmprestimo / unindoAB$salario

View(unindoAB)


##### 11

library(summarytools)
library(caret)
library(rpart)
library(rattle)
library(esquisse)

names(unindoAB)

modeloA <-rpart(default ~ 
                 Atraso 
                + QtdaParcelas 
                + comprometimento_Renda 
                 + NumerodeFilhos 
                + Sexo
              # + TempodeServiço
              #  + EstadoCivil
               + TempodeResidencia
                + Conta
                + fx_salario
                + ValorEmprestimo
                , data = unindoAB )

modeloA$variable.importance
modeloA

fancyRpartPlot(modeloA, palettes = c("Greys","Oranges"),cex = 0.9  )


#### validar modelo


modeloA$prod <- predict(modeloA, newdata = modeloA, type = "prob")
modeloA$class <- predict(modeloA, newdata = modeloA, type = "class")

modeloA$variable.importance

View(unindoAB)


