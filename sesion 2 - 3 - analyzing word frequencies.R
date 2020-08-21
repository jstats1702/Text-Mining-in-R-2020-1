#############################################
### Analyzing Word and Document Frequency ###
#############################################

# - como cuantificar de que esta compuesto un documento?
# - una lista de "stop words" no es la mejor manera de descartar palabras no importantes

# - que tan importante es una palabra en un documento de una coleccion de documentos?

#   tf  : "term frequency" -> frecuencia 
#   tf = n/total
#   tf  : que tan frecuente ocurre una palabra en un documento 

# la frecuencia por si misma permite explorar el uso del lenguaje
# aplicable a libros, articulos, webpages, etc.

#   idf : "inverse document frequency" -> peso 
#   idf = log(n_{documentos} / n_{documentos con el termino})
#   idf : ponderacion del documento

# el idf sera mas alto para para palabras en pocos documentos y viceversa (por que?)

#   tf * idf : estadistica para cuantificar que tan importante es una palabra en un 
#              documento de un corpus (coleccion de documentos)

# - decrecer el peso para palabras comunes
# - incrementar el peso para palabras raras


###########
# Ejemplo #
###########

suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(janeaustenr)))
suppressMessages(suppressWarnings(library(tidytext)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressMessages(library(stringr)))
suppressMessages(suppressMessages(library(gutenbergr)))

# frecuencia de palabras por documento
book_words <- austen_books() %>%
        unnest_tokens(word, text) %>%
        count(book, word, sort = TRUE) %>%
        ungroup()



# numero total de palabras por documento
total_words <- book_words %>%
        group_by(book) %>%
        summarize(total = sum(n))



# cruce de tablas
book_words <- left_join(book_words, total_words)
book_words



# distribucion de "tf"
windows()
ggplot(book_words, aes(n/total, fill = book)) +
        geom_histogram(show.legend = FALSE) +
        xlim(NA, 0.0009) +
        facet_wrap(~book, ncol = 2, scales = "free_y")

# a que corresponden las colas de la distribucion?
# por que la forma de esta distribucion es natural?
# similar a lo que sucede en redes
# esta distribucion es típica en lenguaje



#############################
# Ley de Zipf (George Zipf) #
#############################

# La ley de Zipf establece que la frecuencia con la que aparece una palabra es inversamente
# proporcional a su rango (ranking respecto a su frecuencia en el documento).

# freq proporcional a 1/rank   ->   freq = k * 1/rank

# potencialmente es posible hacer "predicciones" para ranks/freqs no observados
# comparar autores o documentos


# ley de Zipf
freq_by_rank <- book_words %>%
        group_by(book) %>%
        mutate(rank = row_number(), `term frequency` = n/total)

# la tabla ya estaba ordenada por n



# visualizacion de la ley de Zipf
windows()
freq_by_rank %>%
        ggplot(aes(rank, `term frequency`, color = book)) +
        geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
        scale_x_log10() +
        scale_y_log10()

# en todos los documentos el comportamiento es muy similar
# la relacion es inversa, aunque no es constante
# cuidado! escalas log



# constate de proporcionalidad para la "seccion intermedia"
rank_subset <- freq_by_rank %>%
        filter(rank < 500, rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

# pendiente aprox -1



# visualizacion de la ley de Zipf
windows()
freq_by_rank %>%
        ggplot(aes(rank, `term frequency`, color = book)) +
        geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
        geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
        scale_x_log10() +
        scale_y_log10()



###################################################
# tf-idf : identificacion de palabras importantes #
###################################################

# encontrar las palabras importantes de un documento en una coleccion de documentos
# - decrecer el peso para palabras comunes
# - incrementar el peso para palabras raras

# tf-idf = tf * idf
# tf  = n/total (term freq)
# idf = log(n_{documentos} / n_{documentos con el termino}) (inverse document freq)



book_words <- book_words %>%
        bind_tf_idf(word, book, n)

# funcion bind_tf_idf
# una palabra por fila (token)
# chequear algunos numeros manualmente



# documentos con valores altos de tf-idf
book_words %>%
        select(-total) %>%
        arrange(desc(tf_idf))



# visualizacion de tf-idf
windows()
book_words %>%
        arrange(desc(tf_idf)) %>%
        mutate(word = factor(word, levels = rev(unique(word)))) %>%
        group_by(book) %>%
        top_n(15) %>%
        ungroup %>%
        ggplot(aes(word, tf_idf, fill = book)) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = "tf-idf") +
        facet_wrap(~book, ncol = 2, scales = "free") +
        coord_flip()

# el lenguaje es similar a lo largo de las novelas
# los nombres propios son fundamentales



########################################
# Ejemplo : corpus de textos de fisica #
########################################

# corpus
# - Discourse on Floating Bodies by Galileo Galilei
# - Treatise on Light by Christiaan Huygens
# - Experiments with Alternate Currents of High Potential and High Frequency by Nikola Tesla
# - Sidelights on Relativit by Albert Einstein

# diferentes temas
# momentos de la historia diferente
# escritos en otros idiomas y traducidos al ingles
# son textos no homogeneos



# ID de libros
gutenberg_works(author == "Galilei, Galileo")
gutenberg_works(author == "Huygens, Christiaan")
gutenberg_works(author == "Tesla, Nikola")
gutenberg_works(author == "Einstein, Albert")

# descargar libros
physics <- gutenberg_download(c(37729, 14725, 13476, 7333), meta_fields = "author")




# conteo de palabras
physics_words <- physics %>%
        unnest_tokens(word, text) %>%
        count(author, word, sort = TRUE) %>%
        ungroup()



# calculo de tf-idf
plot_physics <- physics_words %>%
        bind_tf_idf(word, author, n) %>%
        arrange(desc(tf_idf)) %>%
        mutate(word = factor(word, levels = rev(unique(word)))) %>%
        mutate(author = factor(author, levels = c("Galilei, Galileo",
                                                  "Huygens, Christiaan",
                                                  "Tesla, Nikola",
                                                  "Einstein, Albert")))



# visualizacion de tf-idf
windows()
plot_physics %>%
        group_by(author) %>%
        top_n(15, tf_idf) %>%
        ungroup() %>%
        mutate(word = reorder(word, tf_idf)) %>%
        ggplot(aes(word, tf_idf, fill = author)) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = "tf-idf") +
        facet_wrap(~author, ncol = 2, scales = "free") +
        coord_flip()


# "AB", "RC", por ejemplo, son nombres de circulos, angulos, etc.
physics %>%
        filter(str_detect(text, "AK")) %>%
        select(text)



# stopwords personalizadas
mystopwords <- data_frame(word = c("eq", "co", "rc", "ac", "ak", "bn", "fig", "file", "cg", "cb", "cm"))

physics_words <- anti_join(physics_words, mystopwords, by = "word")



# calculo de tf-idf
plot_physics <- physics_words %>%
        bind_tf_idf(word, author, n) %>%
        arrange(desc(tf_idf)) %>%
        mutate(word = factor(word, levels = rev(unique(word)))) %>%
        group_by(author) %>%
        top_n(15, tf_idf) %>%
        ungroup %>%
        mutate(author = factor(author, levels = c("Galilei, Galileo",
                                                  "Huygens, Christiaan",
                                                  "Tesla, Nikola",
                                                  "Einstein, Albert")))



# visualizacion de tf-idf
windows()
ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = "tf-idf") +
        facet_wrap(~author, ncol = 2, scales = "free") +
        coord_flip()

# hay diferencias?