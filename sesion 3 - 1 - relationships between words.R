#######################################################
# Relaciones entre palabras: N-gramas y correlaciones #
#######################################################

# hasta ahora se han considerado las palabras como entes individuales, y se han
# tratado sus relaciones con sentimientos y colecciones de documentos

# otros analisis estan basados en relaciones entre palabras
# - que palabras tienden a seguir otras?
# - que palabras tienden a co-ocurrir con otras?

# cuantificar y calcular relaciones y conexiones entre palabras

# que tan frecuentemente una palabra X es seguida por una palabra Y?
# tal frecuencia permite construir un modelo de relaciones entre palabras

# el analisis de N-gramas aplican de igual forma que con palabras individuales
# palabras consecutivas dan mas estructura y contexto


#############################
# tokenizacion por N-gramas #
#############################

# se ha usado "unnest_tokens" para tokenizar por palabras individuales
# ahora se quiere tokenizar por secuencias de palabras consecutivas



# bi-gramas: pares de palabras consecutivas
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(tidytext)))
suppressMessages(suppressWarnings(library(janeaustenr)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(igraph)))
suppressMessages(suppressWarnings(library(ggraph)))

austen_bigrams <- austen_books() %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2)

# en este caso cada token es un bigrama



# bigramas mas frecuentes
austen_bigrams %>%
        count(bigram, sort = TRUE)

# hay bigramas que no son interesantes ("of the", por ejemplo)
# esto motiva el uso de stopwords nuevamente



# separar en palabras
bigrams_separated <- austen_bigrams %>%
        separate(bigram, c("word1", "word2"), sep = " ")

# omitir stop words
bigrams_filtered <- bigrams_separated %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word)

# nuevo conteo de bigramas
bigram_counts <- bigrams_filtered %>%
        count(word1, word2, sort = TRUE)

# sobresalen los nombres propios



# re-unificar los bigramas
bigrams_united <- bigrams_filtered %>%
        unite(bigram, word1, word2, sep = " ")



# trigramas
austen_books() %>%
        unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
        separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
        filter(!word1 %in% stop_words$word,
               !word2 %in% stop_words$word,
               !word3 %in% stop_words$word) %>%
        count(word1, word2, word3, sort = TRUE)




########################
# analisis de bigramas #
########################

# "streets" mas comunes
bigrams_filtered %>%
        filter(word2 == "street") %>%
        count(book, word1, sort = TRUE)



# calculo de tf-idf
bigram_tf_idf <- bigrams_united %>%
        count(book, bigram) %>%
        bind_tf_idf(bigram, book, n) %>%
        arrange(desc(tf_idf))



# visualizacion de tf-idf
windows()
bigram_tf_idf %>%
        arrange(desc(tf_idf)) %>%
        group_by(book) %>%
        top_n(12, tf_idf) %>%
        ungroup() %>%
        mutate(bigram = reorder(bigram, tf_idf)) %>%
        ggplot(aes(bigram, tf_idf, fill = book)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ book, ncol = 2, scales = "free") +
        coord_flip() +
        labs(y = "tf-idf of bigram to novel",
             x = "")

# nuevamente expresiones que involucran nombres propios son caracteristicas

# hay ventajas y desventajas analizando tf-idf de bigramas en lugar de palabras
# - pares de palabras pueden capturar mas estructura y dar mas contexto que palabras 
#   individuales
# - los conteos que involucran bigramas son mas "escasos", por lo que se recomienda 
#   usar bigramas con documentos largos



#########################################
# analisis de sentimientos con bigramas #
#########################################

# el analisis de sentimientos clasico con palabras individuales puede tener sesgos por
# que no se capturan estructuras. por ejemplo, "I'm not happy and I don't like it!"
# que palabras son las que mas contribuyen en la direccion equivocada?



# palabras precedidas por "not"
bigrams_separated %>%
        filter(word1 == "not") %>%
        count(word1, word2, sort = TRUE)

# ahora es posible realizar el analisis teniendo en cuenta, por ejemplo, palabras
# positivas precedidas por palabras negativas



# diccionario AFINN (puntaje numerico en direccion del sentimiento)
AFINN <- get_sentiments("afinn")



# palabras mas frecuentes precedidas por "not" asociadas con un sentimiento 
not_words <- bigrams_separated %>%
        filter(word1 == "not") %>%
        inner_join(AFINN, by = c(word2 = "word")) %>%
        count(word2, value, sort = TRUE) %>%
        ungroup()



# por ejemplo, el sentimiento mas comun seguido de "not" fue "like", el cual normalmente
# recibe un puntaje positivo



# que palabras son las que mas contribuyen en la direccion equivocada?
# multiplicar el puntaje por su frecuencia
# el impacto de una pablabra que ocurre 10 veces con un puntaje de +3 es equivalente
# al de una palabra que ocurre 30 veces con un puntaje de +1



# visualizacion
windows()
not_words %>%
        mutate(contribution = n * value) %>%
        arrange(desc(abs(contribution))) %>%
        head(20) %>%
        mutate(word2 = reorder(word2, contribution)) %>%
        ggplot(aes(word2, n * value, fill = n * value > 0)) +
        geom_col(show.legend = FALSE) +
        xlab("Words preceded by \"not\"") +
        ylab("Sentiment score * number of occurrences") +
        coord_flip()

# los bigramas "not like" y "not help" constityen las mayores causas de mala identificacion
# en el sentimiento del documento al hacerlo considerando palabras individuales, haciendo
# que el texto parezca mas positivo que de lo que en realiad es
# en menor medida sucede el caso con trario con "not afraid", por ejemplo



# "not" no es el unico termino que se debe usar
# es posible conformar un un conjunto de palabras que nieguen la subsiguiente
negation_words <- c("not", "no", "never", "without")

# frecuencia de palabras precedidas por las palabras de negacion especificadas
negated_words <- bigrams_separated %>%
        filter(word1 %in% negation_words) %>%
        inner_join(AFINN, by = c(word2 = "word")) %>%
        count(word1, word2, value, sort = TRUE) %>%
        ungroup()



# visualizacion
windows()
negated_words %>%
        mutate(contribution = n * value,
               word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
        group_by(word1) %>%
        top_n(12, abs(contribution)) %>%
        ggplot(aes(word2, contribution, fill = n * value > 0)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ word1, scales = "free") +
        scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
        xlab("Words preceded by negation term") +
        ylab("Sentiment value * # of occurrences") +
        coord_flip()

# hay otras combinaciones que potencialmente pueden causar sesgos
# "no great" o "never loved", por ejemplo



###################################################
# visualizacion de bigramas por medio de networks #
###################################################

# visualizar las relaciones entre palabras simultaneamente

# conteos
bigram_counts



# contruir network a partir de las 20 combinaciones mas comunes
bigram_graph <- bigram_counts %>%
        filter(n > 20) %>%
        graph_from_data_frame()



# visualizacion 1
windows()
set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
        geom_edge_link() +
        geom_node_point() +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# que se puede observare en la red?
# por que es necesaria una semilla aleatoria
# tipos de redes
# estadistica descriptiva para redes



# visualizacion 2
windows()
set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                       arrow = a, end_cap = circle(.07, 'inches')) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void()



# estadisticas de redes

# degree
# identificar nodos altamente conectados
degree(bigram_graph)

length( degree(bigram_graph) )

d <- data.frame(degree = degree(bigram_graph))
windows()
ggplot(data = d, aes(x = degree)) + geom_histogram()

d <- degree(bigram_graph)
head( d[order(d, decreasing = T)], n = 10)

# matriz de adyacencia
A <- get.adjacency(bigram_graph, sparse = F)

dim(A)



# centralidad
# que tan importante es un nodo?        
a <- evcent(bigram_graph)$vector

head(a[order(a, decreasing = T)])


# densidad
# proporcion de conexiones
graph.density(bigram_graph)



# transitividad
# propensidad a formar triangulos
transitivity(bigram_graph)


# reciprocidad (unicamente para redes dirigidas)
# propensidad en la reciprocidad de los links
reciprocity(bigram_graph)


# asortatividad
# propensidad de conexion entre nodos similiares
assortativity.degree(bigram_graph)



# clustering

isSymmetric(A)

B <- A + t(A)

isSymmetric(B)

table(B)

# B[B!=0] <- 1
# B[B!=1] <- 0

g <- graph_from_adjacency_matrix(adjmatrix = B, mode = "undirected")
        
kc <- fastgreedy.community(g)

# tamanos
sizes(kc)

# cantidad de palabras
sum( sizes(kc) )

# cantidad de comunidades
length( sizes(kc) )


# membresias
m <- membership(kc)

m[m == 1]

windows()
plot(kc, bigram_graph)



library(ape)
windows()
dendPlot(kc, mode = "hclust")


windows()
dendPlot(kc, mode = "dendrogram")



windows()
dendPlot(kc)



##################################################################
# funciones para construir y visualizar bigramas automaticamente #
##################################################################

suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(tidytext)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(igraph)))
suppressMessages(suppressWarnings(library(ggraph)))

count_bigrams <- function(dataset) {
        dataset %>%
                unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
                separate(bigram, c("word1", "word2"), sep = " ") %>%
                filter(!word1 %in% stop_words$word,
                       !word2 %in% stop_words$word) %>%
                count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
        set.seed(2016)
        a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
        bigrams %>%
                graph_from_data_frame() %>%
                ggraph(layout = "fr") +
                geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
                geom_node_point(color = "lightblue", size = 5) +
                geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
                theme_void()
}



#################################
# ejemplo: Biblia de King James #
#################################

# libro 10 en Project Gutenberg
suppressMessages(suppressWarnings(library(gutenbergr)))

kjv <- gutenberg_download(10)



# constuir bigramas
suppressMessages(suppressWarnings(library(stringr)))
kjv_bigrams <- kjv %>%
        count_bigrams()



# filtar digitos
windows()
kjv_bigrams %>%
        filter(n > 40,
               !str_detect(word1, "\\d"),
               !str_detect(word2, "\\d")) %>%
        visualize_bigrams()



##################
# correlaciones  #
##################

# identificar palabras que tienden a ocurrir juntas en el mismo documento, aunque no
# sean adyacentes



# palabras secciones de 10 lineas
austen_section_words <- austen_books() %>%
        filter(book == "Pride & Prejudice") %>%
        mutate(section = row_number() %/% 10) %>%
        filter(section > 0) %>%
        unnest_tokens(word, text) %>%
        filter(!word %in% stop_words$word)



# conteo de palabras que ocurren juntas dentro de secciones
suppressMessages(suppressWarnings(library(widyr)))

word_pairs <- austen_section_words %>%
        pairwise_count(word, section, sort = TRUE)

# las palabras que mas tienen a ocurrir conjuntamente involucran los personasjes 
# principales



# cuales palabras ocurren conjuntamente con "Darcy"?
word_pairs %>%
        filter(item1 == "darcy")



# correlacion: que tanto ocurren un par de palabras conjuntamente respecto a que tanto
# ocurren separadamente
# la interpretacion es analoga a la del coef. de correlacion de Pearson usual

# coeficiente "phi" medida de asociacion entre variables binarias
# phi := ( n_{11} * n_{00} - n_{10} * n_{01} ) / sqrt{n_{1.} * n_{0.} * n_{.0} * n_{.1} }
#
#                   Tiene palabra Y   No tiene palabra Y
# Tiene palabra X   n_{11}            n_{10}
# Tiene palabra X   n_{01}            n_{00}

# https://en.wikipedia.org/wiki/Phi_coefficient

# correlacion sobre palabras relativamente comunes
word_cors <- austen_section_words %>%
        group_by(word) %>%
        filter(n() >= 20) %>%
        pairwise_cor(word, section, sort = TRUE)



# palabras mas correlacionadas con "pounds"?
word_cors %>%
        filter(item1 == "pounds")



# visualizacion de palabras mas correlacionadas con un conjunto de palabras dado
windows()
word_cors %>%
        filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
        group_by(item1) %>%
        top_n(6) %>%
        ungroup() %>%
        mutate(item2 = reorder(item2, correlation)) %>%
        ggplot(aes(item2, correlation)) +
        geom_bar(stat = "identity") +
        facet_wrap(~ item1, scales = "free") +
        coord_flip()




# visualizacion de palabras con al menos una correlacion de 15% que aparecen conjuntamente
# en secciones de 10 lineas
windows()
set.seed(2016)
word_cors %>%
        filter(correlation > .15) %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), repel = TRUE) +
        theme_void()

# a diferencia de las redes de bigramas, la rede de correlaciones es simetrica