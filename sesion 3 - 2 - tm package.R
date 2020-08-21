######################
### otros formatos ###
######################

# formato "tidy"
# - un token por fila (n-grama)
# - util para usar tidy las herramientas de dplyr, tidyr, ggplot2

# CRAN Task View for Natural Language Processing (NLP)
# muchos paquetes para el procesamiento de lenguaje natural no son compatibles con el
# formato tidy

# como interactuan el formato tidy y otros formatos populares de text mining?
# (algunos algoritmos usando matrices como input y no objetos tidy)
# como analizar corpus que no esten formato tidy?
# como manejar corpus con metadata?

# paquetes: "tm", "quanteda"

# https://towardsdatascience.com/r-packages-for-text-analysis-ad8d86684adb      


############################################################
#  Matriz de Documento-Termino (Document-Term Matrix, DTM) #
############################################################

# paquete "tm"
# - cada fila representa un documento
# - cada columna representa un termino
# - los valores de la matriz representan la frecuencia de apariciones de un termino en 
#   un documento

# estas matrices tienden a ser "sparse matrices"
# suelen almacenarse de una forma mas eficiente que como simplemente matrices (igual 
# que las redes)



# ejemplo: Associated Press newspaper articles
suppressMessages(suppressWarnings(library(tm)))
suppressMessages(suppressWarnings(library(topicmodels)))

data("AssociatedPress", package = "topicmodels")

# 2,246 articulos de una agencia de noticias en 1988
AssociatedPress

# cada fila corresponde a un articulo
# cada columna corresponde a una palabra
# 99% de las entradas de la matriz son 0



# terminos
terms <- Terms(AssociatedPress)

head(x = terms, n = 20)

length(terms)



# convertir a fomrato "tidy" (data frame con un token por fila por documento)
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(tidytext)))
suppressMessages(suppressWarnings(library(ggplot2)))


ap_td <- tidy(AssociatedPress)

ap_td

# no hay filas con frecuencia 0



# analisis de sentimiento 
ap_sentiments <- ap_td %>%
        inner_join(get_sentiments("bing"), by = c(term = "word"))



windows()
ap_sentiments %>%
        count(sentiment, term, wt = count) %>%
        ungroup() %>%
        filter(n >= 200) %>%
        mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
        mutate(term = reorder(term, n)) %>%
        ggplot(aes(term, n, fill = sentiment)) +
        geom_bar(stat = "identity") +
        ylab("Contribution to sentiment") +
        coord_flip()




#####################################################################
# Matriz de documento-caracteristica (document-feature matrix, dfm) #
#####################################################################

# paquete "quanteda"
# - estructura similar a la DTM de la libreria tm



# ejemplo: corpus de discursos presidenciales
suppressMessages(suppressWarnings(library(methods)))

data("data_corpus_inaugural", package = "quanteda")

inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)

inaug_dfm

dim(inaug_dfm) 

# convertir a formato tidy
inaug_td <- tidy(inaug_dfm)



# cuales son las palabras mas importantes en cada discurso?
inaug_tf_idf <- inaug_td %>%
        bind_tf_idf(term, document, count) %>%
        arrange(desc(tf_idf))

speeches <- c("1933-Roosevelt", "1861-Lincoln", "1961-Kennedy", "2009-Obama")

windows()
inaug_tf_idf %>%
        filter(document %in% speeches) %>%
        group_by(document) %>%
        top_n(10, tf_idf) %>%
        ungroup() %>%
        mutate(term = reorder_within(term, tf_idf, document)) %>%
        ggplot(aes(term, tf_idf, fill = document)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ document, scales = "free") +
        coord_flip() +
        scale_x_reordered() +
        labs(x = "", y = "tf-idf")



# ejemplo (cont.) 
# extraer el agno de cada discurso y calcular el numero de palabras de cada discurso
suppressMessages(suppressWarnings(library(tidyr)))

year_term_counts <- inaug_td %>%
        extract(document, "year", "(\\d+)", convert = TRUE) %>%
        complete(year, term, fill = list(count = 0)) %>%
        group_by(year) %>%
        mutate(year_total = sum(count))

# se usa la funcion complete() para incluir los ceros (casos donde una palabra no aparece 
# en un documento)



# como cambia la frecuencia de las palabras a traves del tiempo?
windows()
year_term_counts %>%
        filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
        ggplot(aes(year, count / year_total)) +
        geom_point() +
        geom_smooth() +
        facet_wrap(~ term, scales = "free_y") +
        scale_y_continuous(labels = scales::percent_format()) +
        ylab("% frequency of word in inaugural address")



#################################
# Convertir forma tidy a matrix #
#################################

# trasformar a formato dtm (tm) la data de Associated Press newspaper articles 
ap_td %>%
        cast_dtm(document, term, count)



# transformar a formato dfm (quanteda) la data de Associated Press newspaper articles 
ap_td %>%
        cast_dfm(term, document, count)



# transformar a matrix (Matrix)
library(Matrix)

m <- ap_td %>%
        cast_sparse(document, term, count)

class(m)

dim(m)

m <- as.matrix(m)

m[1:3, 1:10]

#######################################
# Manejo de formato tidy con metadata #
#######################################

# metadata: ID, fecha, hora, titulo, idioma, etc.
# informacion relevante del corpus



# ejemplo: 50 articulos del news service Reuters (paquete tm)
data("acq")

acq

# primer documento
acq[[1]]



# convertir a formato tidy
acq_td <- tidy(acq)

acq_td



# tokenizar por palabras 
acq_tokens <- acq_td %>%
        select(-places) %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words, by = "word")



# palabras mas frecuentes
acq_tokens %>%
        count(word, sort = TRUE)


# palabras mas relevantes en cada articulo
acq_tokens %>%
        count(id, word) %>%
        bind_tf_idf(word, id, n) %>%
        arrange(desc(tf_idf))