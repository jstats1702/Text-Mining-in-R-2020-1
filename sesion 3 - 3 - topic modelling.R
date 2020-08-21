####################
### Modelamiento ###
####################

# tecnica de clasificacion no supervisada para definir grupos de documentos
# equivalente a clustering usando datos numericos
# no hay ejemplos de clasificacion predeterminados

# Latent Dirichlet Allocation (LDA)
# - documento = mezcla de topicos
#   cada documento contiene palabras de varios topicos en diferentes proporciones
# - topico    = mezcla de palabras
#   pueden haber palabras compartidas entre topicos a diferencia de metodos de clustering
#   tradicionales
# - aprendizaje automatico para clasificar documentos basado en el contenido del texto
# - simultaneamente encontrar la mezcla de palabras que conforman los topicos, y determinar
#   la mezcla de topicos que conforman los documentos

# topicmodels package
# https://cran.r-project.org/web/packages/topicmodels/index.html

# mallet package es otr alternativa
# https://cran.r-project.org/web/packages/mallet/mallet.pdf

# ejemplo: AssociatedPress dataset
# 2,246 articulos de una agencia de noticias en 1988
suppressMessages(suppressWarnings(library(topicmodels)))

data("AssociatedPress")

AssociatedPress



# modelo LDA con k = 2 topicos
ap_lda <- LDA(x = AssociatedPress, k = 2, control = list(seed = 1234))

ap_lda

# como en la mayoria de metodos de clustering se recomiendo usar una semilla aleatoria 
# para reproducir los resultados
# ajustar el modelo es sencillo, el trabajo verdaderamente importante es la interpretacion



# extrer las probabilidades de pertenencia/generacion (beta) de cada palabra en cada topico 
suppressMessages(suppressWarnings(library(tidytext)))

ap_topics <- tidy(x = ap_lda, matrix = "beta")

ap_topics

# por ejemplo, la palabra "aaron" tiene una probabildiad de 1.69e-12 de ser generada 
# por el topico 1, y una probabildiad de 3.90e- 5 de ser generada por el topico 2



# cuales es el top 10 de palabras mas probables en cada topico?
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(dplyr)))

# tabla
ap_top_terms <- ap_topics %>%
        group_by(topic) %>%
        top_n(10, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)

# visualizacion
windows()
ap_top_terms %>%
        mutate(term = reorder(term, beta)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip()

# cuales son las palabras mas comunes en cada topico?
# como se pueden interpretar los topicos de acuerdo con estas palabras?
# hay palabras en comun?



# cuales son las palabras asociadas con las diferencias mas grandes en probabilidad 
# entre el topico 2 y el topico 1?
# filtar palabras con betas superiores a 1/1000 en al menos un topico
# calcular r = log_2 ( beta_2 / beta_1 )
# que significa que r = 1? que significa que r = -1? por que es conveniente usar log_2

suppressMessages(suppressWarnings(library(tidyr)))

# tabla
beta_spread <- ap_topics %>%
        mutate(topic = paste0("topic", topic)) %>%
        spread(topic, beta) %>%
        filter(topic1 > .001 | topic2 > .001) %>%
        mutate(log_ratio = log2(topic2 / topic1))

beta_spread

# visualizacion
windows()
beta_spread %>%
        group_by(direction = log_ratio > 0) %>%
        top_n(10, abs(log_ratio)) %>%
        ungroup() %>%
        mutate(term = reorder(term, log_ratio)) %>%
        ggplot(aes(term, log_ratio)) +
        geom_col() +
        labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
        coord_flip()

# esto confirma la interpretacion de topicos como politico y financiero, respectivamente



# extrer las probabilidades de pertenencia/generacion (gamma) de cada topico en cada documento
ap_documents <- tidy(ap_lda, matrix = "gamma")

ap_documents

ap_documents[ap_documents$document == 1, ]
ap_documents[ap_documents$document == 2, ]
ap_documents[ap_documents$document == 6, ]

# proporcion estimada de palabras del documento que son generadas por un topico determinado
# por ejemplo, el 24.8% de las palabras en el documento 1 son generadas del topico 1



# que sucede con el documento 6? cuales son las palabras mas comunes en el documento 6?
tidy(AssociatedPress) %>%
        filter(document == 6) %>%
        arrange(desc(count))

# este resultado concuerda con la interpretacion del topico?



####################################
# ejemplo: The Great Library Heist #
####################################

# para probar la validez de un metodo estadistico se acostumbra a testearlo con un dataset 
# para el que se conocen con cierto grado de certeza las tendencias
# esto permite evidenciar donde el metodo acerta y donde se equivoca

# literatura clasica
# - Great Expectations by Charles Dickens
# - The War of the Worlds by H.G. Wells
# - Twenty Thousand Leagues Under the Sea by Jules Verne
# - Pride and Prejudice by Jane Austen

# se conforma una base de datos con los capitulos individuales de los libros
# es posible asignar los capitulos a los libros de los cuales provienen?
# asginar los capitulos a topicos esperando recobrar los libros originales

suppressMessages(suppressWarnings(library(gutenbergr)))
suppressMessages(suppressWarnings(library(stringr)))

titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds", "Pride and Prejudice", "Great Expectations")

# data
books <- gutenberg_works(title %in% titles) %>%
        gutenberg_download(meta_fields = "title")



# dividir en documentos (capitulos)
reg <- regex("^chapter ", ignore_case = TRUE)
by_chapter <- books %>%
        group_by(title) %>%
        mutate(chapter = cumsum(str_detect(text, reg))) %>%
        ungroup() %>%
        filter(chapter > 0) %>%
        unite(document, title, chapter)



# dividir en palabras
by_chapter_word <- by_chapter %>%
        unnest_tokens(word, text)



# calcular frecuencias
word_counts <- by_chapter_word %>%
        anti_join(stop_words) %>%
        count(document, word, sort = TRUE) %>%
        ungroup()



# transformar a formato DocumentTermMatrix para aplicar el modelo
chapters_dtm <- word_counts %>%
        cast_dtm(document, word, n)

chapters_dtm



# ajustar el modelo con 4 topicos (como decidir el numero adecuado de topicos?)
chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))

chapters_lda



# betas
chapter_topics <- tidy(chapters_lda, matrix = "beta")

chapter_topics



# top 5 de palabras en cada termino
top_terms <- chapter_topics %>%
        group_by(topic) %>%
        top_n(5, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
        
top_terms



# visualizacion
windows()
top_terms %>%
        mutate(term = reorder(term, beta)) %>%
        ggplot(aes(term, beta, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        coord_flip()

# estas palabras corresponden efectivamente a los libros!
# hay palabras en comun?



# es posible asignar los capitulos a los libros originales?
chapters_gamma <- tidy(chapters_lda, matrix = "gamma")

chapters_gamma[chapters_gamma$document == "Pride and Prejudice_1", ]

chapters_gamma[chapters_gamma$document == "Great Expectations_1", ]

# cada valor corresponde a la proporcion estimada de palabras del documento que son 
# generadas por un topico determinado
# por ejemplo, el modelo estima que las palabras en el documento Great Expectations_57
# tiene una probabilidad de 0.0000134 de provenir del topico 1



# separar el dombre del documento en title y chapter
chapters_gamma <- chapters_gamma %>%
        separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma



# ordenar por topico y vizualizar gammas
windows()
chapters_gamma %>%
        mutate(title = reorder(title, gamma * topic)) %>%
        ggplot(aes(factor(topic), gamma)) +
        geom_boxplot() +
        facet_wrap(~ title)

# el modelo esta funcionando correctamente?



# hay casos donde el topico mas asociado con un capitulo sea de otro libro?

# identificar el topico mas probable
chapter_classifications <- chapters_gamma %>%
        group_by(title, chapter) %>%
        top_n(1, gamma) %>%
        ungroup()

chapter_classifications



# identificacion del topico de acuerdo con el consenso
book_topics <- chapter_classifications %>%
        count(title, topic) %>%
        group_by(title) %>%
        top_n(1, n) %>%
        ungroup() %>%
        transmute(consensus = title, topic)



# capitulos mal clasificados
chapter_classifications %>%
        inner_join(book_topics, by = "topic") %>%
        filter(title != consensus)

# solo dos capitulos estan mal clasificados!
# este es un excelente resultado para aprendizaje no supervisado



##############
# aumetacion #
##############

# cuales palabras en cada documento fueron asignadas a cada topico?

# cuanto mas palabras de un documento se asignan a un topico, mas peso (gamma) tendra 
# la asignacion del documento a un topico

assignments <- augment(chapters_lda, data = chapters_dtm)

assignments

# augment usa el modelo para añadir mas informacion a cada observacion de la data
# original (.topic)
# usar . es una practica comun para evitar sobre-escribir alguna variable



# cuales palabras fueron asignadas incorrectamente?
assignments <- assignments %>%
        separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
        inner_join(book_topics, by = c(".topic" = "topic"))

assignments



# visualizacion (matriz de confusion)
suppressMessages(suppressWarnings(library(scales)))

windows()
assignments %>%
        count(title, consensus, wt = count) %>%
        group_by(title) %>%
        mutate(percent = n / sum(n)) %>%
        ggplot(aes(consensus, title, fill = percent)) +
        geom_tile() +
        scale_fill_gradient2(high = "red", label = percent_format()) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              panel.grid = element_blank()) +
        labs(x = "Book words were assigned to",
             y = "Book words came from",
             fill = "% of assignments")

# este grafico explica por que dos capitulos fueron mal clasificados



# cuales fueron las palabras mal clasificadas mas comunes?
wrong_words <- assignments %>%
        filter(title != consensus)

wrong_words



#  palabras mal clasificadas mas comunes ordenadas por frecuencia
wrong_words %>%
        count(title, consensus, term, wt = count) %>%
        ungroup() %>%
        arrange(desc(n))

# estas palabras han sido mal clasificadas porque son mas comunes en los libros donde 
# fueron asignadas incorrectamente