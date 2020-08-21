###########################
# modelamiento predictivo #
###########################

# aprendizaje supervisado       
# clasificacion y prediccion continua usando texto como input
# output fuera del corpus 



#################
# clasificacion # 
#################

# identificar pacientes en el momento de la salida del hospital que probablemente 
# volveran en un periodo de 30 dias

# entender las razones 
# tomar acciones para beneficiar la institucion y el paciente

# dataset : 8,500 pacientes diabeticos
# https://www.hindawi.com/journals/bmri/2014/781670/cta/
# 136 variables incluyendo genero, raza, peso, junto con "discharge information"





setwd("C:/Users/Juan Camilo/Dropbox/USTA/Cursos/text mining/USTA text mining/code")

library(text2vec)  ## lista de palabras, estadisticas
library(caret)     ## preparacion de la data
library(tm)
library(glmnet)
library(pROC)



# funcion personalizada para limpiar el texto
diagnosis.clean<-function(x){
        x<-removePunctuation(x)
        x<-stripWhitespace(x)
        return(x)
}





# importar data
# ojear el conjunto de datos
diabetes<-read.csv('diabetes_subset_8500.csv')
diabetes$diag.text <- as.character(paste(diabetes$diag_1_desc, diabetes$diag_2_desc, diabetes$diag_3_desc, sep=" "))

# TRUE  = volver
# FALSE = no volver 

# limpiar el texto
diabetes$diag.text<-diagnosis.clean(diabetes$diag.text)

# dividir el conjunto de datos
set.seed(300520)
train<-createDataPartition(diabetes$readmitted,p=.7,list=F)
train.diabetes<-diabetes[train,]
test.diabetes<-diabetes[-train,]

# tokenizacion (iterador -> calcular estadisticas sobre de los documentos)
iter.maker<-itoken(iterable = train.diabetes$diag.text, 
                   preprocess_function = tolower, 
                   tokenizer = word_tokenizer)

# vectorizacion
v <- create_vocabulary(iter.maker,stopwords=stopwords('en'))
vectorizer <- vocab_vectorizer(v)  # function

# DTM
it <- itoken(iterable = train.diabetes$diag.text,
             preprocess_function = tolower,
             tokenizer = word_tokenizer)

dtm <- create_dtm(it, vectorizer)


# guia
# https://cran.r-project.org/web/packages/text2vec/vignettes/text-vectorization.html




# ajustar el modelo usando texto
text.cv<-cv.glmnet(dtm,
                   y=as.factor(train.diabetes$readmitted), 
                   alpha=0.9,
                   family="binomial",
                   type.measure="auc", 
                   nfolds=5, 
                   intercept=F)

# visualizacion
windows()
plot(text.cv)

max(text.cv$cvm)





# ajustar el modelo usando variables dummy
no.text<-as.matrix(train.diabetes[,1:132])

no.text.cv<-cv.glmnet(no.text, 
                      y=as.factor(train.diabetes$readmitted), 
                      alpha=0.9,
                      family='binomial',
                      type.measure='auc', 
                      nfolds=5,
                      intercept=F)

# visualizacion
windows()
plot(no.text.cv)
title("GLMNET No Text")

max(no.text.cv$cvm)






# ajustar el modelo usando toda la data
all.data<-cbind(dtm,no.text)

all.cv<-cv.glmnet(all.data,
                  y=as.factor(train.diabetes$readmitted), 
                  alpha=0.9, 
                  family="binomial",
                  type.measure="auc", 
                  nfolds=5, 
                  intercept=F)

# visualizacion
windows()
plot(all.cv)

max(all.cv$cvm)




# prediccion solo texto (training)
text.preds<-as.logical(predict(text.cv, dtm,type='class', s=text.cv$lambda.min))
text.roc<-roc((train.diabetes$readmitted*1), text.preds*1)





# prediccion no texto (training)
no.text.preds<-as.logical(predict(no.text.cv, no.text,type='class', s=no.text.cv$lambda.min))
no.text.roc<-roc((train.diabetes$readmitted*1), no.text.preds*1)





# prediccion toda la data (training)
all.data.preds<-as.logical(predict(all.cv, all.data,type='class', s=all.cv$lambda.min))
all.data.roc<-roc((train.diabetes$readmitted*1), all.data.preds*1)




# visualizacion
windows()
plot(text.roc,col="blue",main="BLUE = Text, RED = No
Text, GREEN=All",adj=0)
plot(no.text.roc, add=TRUE,col="red", lty=2)
plot(all.data.roc,add=TRUE,col="darkgreen", lty=3)






# metricas adicionales de prediccion
# recall, precision and the F1

confusion<-confusionMatrix(data  = as.factor(all.data.preds), reference = as.factor(train.diabetes$readmitted))

recall <- confusion$byClass['Sensitivity']
precision <- confusion$byClass['Pos Pred Value']
f1.score <- as.numeric(2 * ((precision * recall) / (precision + recall)))

# https://en.wikipedia.org/wiki/Confusion_matrix




# DTM validation dataset
test.it<-itoken(test.diabetes$diag.text,
                preprocess_function = tolower,
                tokenizer = word_tokenizer)
test.dtm <-create_dtm(test.it, vectorizer)

# dado que se esta usando el mismo vectorizes el DTM nuevo coincide con el de entrenamiento
# otra opcion es usar la funcion match.matrix




# conformacion de los predictores (toda la data)
test.no.text<-as.matrix(test.diabetes[,1:132])
new.patients<-cbind(test.dtm,test.no.text)



# prediccion toda la data (validation dataset)
test.preds<-predict(object = all.cv, newx = new.patients, type='class', s=all.cv$lambda.min)



# evaluacion
test.confusion<-confusionMatrix(as.factor(test.preds), as.factor(test.diabetes$readmitted))
test.precision <- test.confusion$byClass['Pos Pred Value']
test.recall <- test.confusion$byClass['Sensitivity']
test.f1 <- as.numeric(2 * ((test.precision * test.recall) / (test.precision + test.recall)))





##############
# prediccion #
##############

# predecir los ingresos de una pelicula en su estreno con base en reviews tempranos

# variable respuesta: ingresos en el fin de semana del estreno

# predictores : reviews, actores, director,..

# dataset : 2000 movies reviwes

# http://www.cs.cmu.edu/~ark/movie$‐data/


library(data.table)   ## organizacion de datos
library(pbapply)      ## organizacion de datos
library(text2vec)     ## organizacion de datos
library(caret)
library(glmnet)
library(qdap)         ## text mining
library(tm)           ## text mining
library(Metrics)      ## metricas para variables continuas
library(tidyr)
library(ggthemes)





# conjunto de datos
movie.data<-fread('2k_movie_reviews.csv')




# limipeza de datos
# stopwords se eliminan luego
review.clean<-function(x){
        x<-replace_contraction(x)
        x<-removePunctuation(x)
        x<-stripWhitespace(x)
        x<-removeNumbers(x)
        x<-tolower(x)
        x<-stemmer(x)
        return(x)
}


# ejemplo
# x <- c("Mr. Jones isn't going.",  
#        "Check it out what's going on.",
#        "He's here but didn't go.",
#        "the robot at t.s. wasn't nice", 
#        "he'd like it if i'd go away")
# replace_contraction(x)

#stemmer(DATA$state)



# limipar texto
clean.text <- review.clean(movie.data$train.movies)




# variable respuesta
y<-movie.data$opening_weekend





# particion del conjunto de datos
train<-createDataPartition(y,p=0.8,list=F)
train.movies<-clean.text[train]
train.y<-y[train]
test.movies<-clean.text[-train]
test.y<-y[-train]





# tokenizacion
iter.maker<-itoken(train.movies, tokenizer = word_tokenizer)
v <- create_vocabulary(iter.maker, stopwords=c(stopwords('SMART'),'movie','movies'))




# eliminar terminos muy poco frecuentes y exageradamente frecuentes
pruned.v<-prune_vocabulary(v, term_count_min = 10, doc_proportion_max = 0.5, doc_proportion_min = 0.001)



# toquenizacion
vectorizer <- vocab_vectorizer(pruned.v)
it <- itoken(train.movies, tokenizer = word_tokenizer)
dtm <- create_dtm(it, vectorizer)




# modelamiento
text.cv<-cv.glmnet(dtm,train.y,alpha=1,family= 'gaussian', type.measure='mse', nfolds=5, intercept=T)

# otras opciones 
# mae
# rmse
# necesa
# family = c("gaussian", "binomial", "poisson", "multinomial",
#           "cox", "mgaussian")


# visualizacion
windows()
plot(text.cv)
title("Movie Reviews predict Revenue")




# evaluacion (training dataset)
text.preds<-predict(text.cv,dtm,s=text.cv$lambda.min)

train.dat<-data.frame(actual=train.y, preds=text.preds[,1])

# metricas
rmse(train.dat$actual,train.dat$preds)
mae(train.dat$actual,train.dat$preds)

# en promedio, las predicciones estan a pm 3M



# visualizacion
windows()
train.tidy<-gather(train.dat)
ggplot(train.tidy, aes(x=key, y=value, fill=key)) + geom_boxplot()+theme_gdocs()



windows()
ggplot(train.dat, aes(x=actual, y=preds)) +
        geom_point(color='darkred',shape=1) +
        stat_smooth(method=lm) + theme_gdocs()

# como mejorar el modelo?
# considerar otros valores de alpha
# tener en cuenta otros preditores (actores, director, ...)






# prediccion (validation data)

# limpiar data
test.text<-review.clean(test.movies)


# tokenizar
test.it<-itoken(test.text, preprocess_function = tolower, tokenizer = word_tokenizer)

test.dtm<-create_dtm(test.it,vectorizer)

# las matrices nuevas y la original deben coincidir en las columnas




# predecir
test.preds<-predict(text.cv,test.dtm, s=text.cv$lambda.min)

# metricas
rmse(test.y,test.preds)

mae(test.y,test.preds)





# visualizacion
windows()
test.dat<-data.frame(actual=test.y, preds=test.preds[,1])
ggplot(test.dat, aes(x=actual, y=preds)) +
        geom_point(color='darkred',shape=1) +
        stat_smooth(method=lm) + theme_gdocs()