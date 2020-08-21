###############################
# clasificacion de documentos #
###############################

# aprendizaje supervisado
#       training dataset vs validation dataset

# el objetivo es clasificar documentos en categorias

# inputs
#       1. terminos de documentos
#       2. pesos de terminos



######################
# caso 1 > clickbait #
######################

# el clickbait es una tecnica que consiste en manipular al usuario (a traves de su curiosidad) 
# con el fin de que este haga clic en un articulo o post determinado con el fin de generar 
# ingresos publicitarios, especialmente a expensas de la calidad o exactitud de estos

# caracteristicas de clickbait links
#       1. el titular retiene informacion (requiere que el usuario haga clic en el enlace)
#       2. el texto es enganoso o exagerado

# Ejemplos
# "9 cosas increibles que son las mas alucinantes del mundo"
# "Esta es la emocion que sentiras despues de hacer clic"
# "No vas a querer ver esto" 
# "12+1 cosas que solo entenderas tu" 

# objetivo : identificar clickbait posts para removerlos del newsfeed
# la meta del newsfeed de Facebook es "show people the stories most relevant to them"

# 1. crear algoritmo de clasificacion
# 2. obtener el texto (1 = clickbait, 0 = de interes)
# 3. separar la base en en training y validation datasets
# 4. extraer features del training dataset: DTM poderada por TFIDF
# 5. entrenar el modelo usando GLMNet lasso regression
# 6. identicar el top de los terminos que indican clickbait





        
library(tm)
library(Matrix)
library(glmnet)     ## elastic net regression
library(caret)      ## partitioning, categorization and regression training
library(pROC)
library(ggthemes)
library(ggplot2)
library(arm)        ## invlogit
library(stringr)

# funcion personalizada para "limpiar" texto
headline.clean<-function(x){
        x<-tolower(x)
        x<-removeWords(x,stopwords('en'))
        x<-removePunctuation(x)
        #x<-str_replace_all(x, "[[:punct:]]", " ")
        #x<-gsub("[^[:alnum:][:blank:]+?&/\\-]", "", x)
        x<-stripWhitespace(x)
        return(x)
}

# el nuevo documento debe coincidir con los terminos del DTM que se usaron para el entrenamiento
# el algoritmo esperara que el nuevo DTM tenga el mismo número de columnas que la DTM de los datos de entrenamiento.

# arreglar el bug en la funcion create_matrix de la libreria RTextTools
# en la linea 42 se debe cambiar "Acronym" por "acronym"

#library(RTextTools)
#trace("create_matrix",edit=T)

# alternativamente es posible usar la funcion match.matrix
# descartar los terminos en la nueva matriz que no estan en el DTM original
# agregar los terminos en la nueva matriz que estan en el DTM original
# ver el if statement cuando original.matrix no es NULL
match.matrix <- function(text.col, original.matrix=NULL, weighting=weightTf)
{
        control <- list(weighting=weighting)
        training.col <- sapply(as.vector(text.col,mode="character"),iconv, to="UTF8",sub="byte")
        corpus <- VCorpus(VectorSource(training.col))
        matrix <- DocumentTermMatrix(corpus,control=control)
        if (!is.null(original.matrix)) {
                terms <- colnames(original.matrix[, which(!colnames(original.matrix) %in% colnames(matrix))])
                weight <- 0
                if (attr(original.matrix,"weighting")[2]=="tfidf") weight <- 0.000000001
                amat <- matrix(weight,nrow=nrow(matrix), ncol=length(terms))
                colnames(amat) <- terms
                rownames(amat) <- rownames(matrix)
                fixed <- as.DocumentTermMatrix(cbind(matrix[,which(colnames(matrix) %in% colnames(original.matrix))],amat), weighting=weighting)
                matrix <- fixed
        }
        matrix <- matrix[,sort(colnames(matrix))]
        gc()
        return(matrix)
}




setwd("C:/Users/Juan Camilo/Dropbox/USTA/Cursos/text mining/USTA text mining/code")

# seleccionar filas de números aleatorios basados en la distribución de la variable dependiente
headlines<-read.csv('all_3k_headlines.csv')
set.seed(1234)
train<-createDataPartition(headlines$y,p=0.5,list=F)
train.headlines<-headlines[train,]
test.headlines<-headlines[-train,]






# limipar el texto
clean.train<-headline.clean(train.headlines$headline)

# construir el DTM ponderando por tf_idf (no es necesario incluir una "matriz original")
train.dtm <- match.matrix(text.col = clean.train, weighting = tm::weightTfIdf)





# transformar el DTM a matrix para el modelamiento
train.matrix<-as.matrix(train.dtm)
train.matrix<-Matrix(train.matrix, sparse=T)

dim(train.matrix)



# GLMNet models
# GLM : Generalized Linear Model

# ridge : regresion bajo multicolinealidad tipicamente con un numero grande de predictores
#         por medio de un penalty term se introduce un pequeño sesgo en la estimacion de los 
#         parametros con el fin de aumentar la eficiencia de las estimaciones (regularizacion)
#         hace shrinkage a los parametros para mejorar prediccion y asi reducir overfitting
#         no hace seleccion de variables
#         encoge terminos acercando el coeficiente a 0

# https://en.wikipedia.org/wiki/Tikhonov_regularization

# lasso : seleccionar predictores y regularizar para mejorar prediccion e interpretabilidad
#         del modelo    
#         "least absolute shrinkage selection operator"
#         elimina terminos cambiendo el coeficiente a 0

# https://en.wikipedia.org/wiki/Lasso_(statistics)#Elastic_net

# elasticnet
# la mezcla entre lasso y ridge se controla por medio de alpha
# alpha = 1 : lasso
# alpha = 0 : ridge
# 0 < alpha < 1 : ElasticNet
# balance entre simplcidad (lasso) y accuracy

# lidiar con el problema de small n big penalizando terminos 
# evitar asignar coeficientes grandes a palabras raras  

# es smuy importante evitar overfitting porque de nada sirve un modelo perfecto
# en un conjunto de datos dado si el proposito es clasificar







# CV
# type.measure :  deviance, AUC, class, mse, mae
# class elige el penalty term basado en la menor tasa de clasificion errada
cv<-cv.glmnet(train.matrix,
              y = as.factor(train.headlines$y), alpha=1,
              family = 'binomial', nfolds=10, intercept=F,
              type.measure = 'class')


m = glmnet(x = train.matrix, y = as.factor(train.headlines$y), alpha=1, family = 'binomial')

dim(m$beta)





windows()
plot(cv)

# la primera linea punteada muestra el valor del penalty term ue minimiza el criterio
# la segunda representa el valor mas grande del parametro a una desviacion estandar
# este segundo valor incluiria menos terminos comparado con el primer valor

cv$lambda.min

cv$lambda.1se





# clasificar los textos del traning dataset antes de usar datos nuevos para verificar que todo este en orde
# la precision aqui esta inflada porque el modelo ya uso este conjunto de datos
# si algo no funciona bien es buena idea revisar la particion del conjunto de datos,
# el ajuste del modelo
# newx debe tener exactamente las mismas columnas que el training dataset
preds<-predict(object = cv, newx = train.matrix, type = "class", s = cv$lambda.1se)



# cuantificar performance
# auc = 0.5 : el modelo no es mejor que una predccion aleatoria
# auc < 0.5 : el modelo lo hace peor que una prediccion aleatoria
# auc = 1   : el modelo hace una clasificacion perfecta
train.auc<-roc(train.headlines$y,as.numeric(preds))
train.auc





# sensitivity = true positive rate (recall)
# specificity = true negative rate
# tumores vs terroristas
windows()
plot(train.auc)


library(PRROC)
PRROC_obj <- roc.curve(scores.class0 = as.numeric(preds), weights.class0=train.headlines$y, curve=TRUE)

windows()
plot(PRROC_obj)

#https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/



confusion<-table(preds,train.headlines$y)
confusion


# precision general
sum(diag(confusion))/sum(confusion)







# predicciones
# las palabras que el modelo no ha visto, se tienen que eliminar del objeto a predecir
# el objeto a predicir debe tener el mismo formato del training dataset
# las columnas deben coincidir, mas no el numero de observaciones
clean.test<-headline.clean(test.headlines$headline)
test.dtm<-match.matrix(clean.test,
                       weighting=tm::weightTfIdf,
                       original.matrix=train.dtm)                


class(test.dtm)

# tranformar a matriz y luego a sparse matrix   
test.matrix<-as.matrix(test.dtm)
test.matrix<-Matrix(test.matrix)


head(colnames(test.matrix))
head(colnames(train.matrix))

identical(colnames(test.matrix), colnames(train.matrix))





# type ="response" produce las probabildiades
preds<-predict(object = cv, newx = test.matrix, type="class", s=cv$lambda.1se)
headline.preds<-data.frame(doc_row = rownames(test.headlines),class=preds[,1])

head(headline.preds)




# evaluacion
test.auc<-roc(test.headlines$y, as.numeric(preds))
test.auc

# como mejorar el modelo?
# mas datos de entrenamiento
# incluir bigramas o trigramas
# modificar el modelo (cambiar lambda, alpha) https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html


windows()
plot(train.auc,col="blue",main="RED = test, BLUE =
train",adj=0)
plot(test.auc, add=TRUE,col="red", lty=2)

confusion<-table(headline.preds[,2],test.headlines$y)
sum(diag(confusion))/sum(confusion)





# cuales son los terminos mas importantes
# cuales son los terminos que tienen un impacto positivo o negativo en la clasificacion 
# examinar los coeficientes del modelo
# los coeficientes del modelo cuantifican el impacto de un termino dado en el log odds de 
# ser clickbait mientras los demas permanecen constantes
# cuidado: los betas m se interpretan en relacion al log odds 


glmnet.coef<-as.matrix(coef(cv, s='lambda.1se'))

head(glmnet.coef)

glmnet.coef<-data.frame(words= row.names(glmnet.coef), glmnet_coefficients=glmnet.coef[,1])

head(glmnet.coef)


glmnet.coef<-glmnet.coef[order(glmnet.coef$glmnet_coefficients, decreasing=T),]
glmnet.coef$words<-factor(glmnet.coef$words, levels=unique(glmnet.coef$words))

head(glmnet.coef)

tail(glmnet.coef)

# resumen de la distribucion de los beta
summary(glmnet.coef$glmnet_coefficients)

# numero de coef > 0
length(subset(glmnet.coef$glmnet_coefficients, glmnet.coef$glmnet_coefficients>0))

# numero de coef < 0
length(subset(glmnet.coef$glmnet_coefficients, glmnet.coef$glmnet_coefficients<0))

windows()
ggplot(glmnet.coef,
       aes(x=glmnet.coef$glmnet_coefficients)) +
        geom_line(stat='density', color='darkred',
                  size=1) + theme_gdocs()

# usar regularized regression permite identificar los terminos mas relevantes en los extremos





# top de coeficientes
top.coef<-rbind(head(glmnet.coef,10), tail(glmnet.coef,10))
top.coef$impact<-ifelse(top.coef$glmnet_coefficients>0,"Posit","Negat")

top.coef

# visualizacion
windows()
ggplot(top.coef, aes(x=glmnet_coefficients, y=words)) +
        geom_segment(aes(yend=words), xend=0,
                     colour="grey50") +
        geom_point(size=3, aes(colour=impact)) + theme_few()




# transformacion
# threshold = 0 en log odds
# threshold = 0.5 en probabilidad
glmnet.coef$probability<-invlogit(glmnet.coef$glmnet_coefficients)
top.coef$probability<-invlogit(top.coef$glmnet_coefficients)



# visualizacion
windows()
plot(glmnet.coef$probability,glmnet.coef$word, col='blue')
points(top.coef$probability,top.coef$word, col='red', pch=16)
