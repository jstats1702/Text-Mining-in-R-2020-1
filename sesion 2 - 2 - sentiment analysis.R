############################################
### opinion mining or sentiment analysis ###
############################################

# * endender actitudes y opiniones en un texto
# * identificar flujos de narrativas a traves de un texto
# * identificar la contribucion de palabras en la expresion de un sentimiento
# * abordar el contenido emocional de un texto programaticamente
# * sentimiento = suma de contenido del sentimiento de las palabras individuales



#   diccionarios
# * basados en unigramas (palabras), no expresiones compuestas (e.g. "no good")
# * a las palabras se les asigna un score (escala; positivo/negativo; emociones)
# * validacion del diccionario vs tipo de texto
# * hay palabras neutras



#   caveats:
# * tener en cuenta sarcasmo y expresiones compuetas
# * la longitud del texto peude tener un efecto en el analisis: encontrar un balance
#   textos muy grandes el efecto del texto suele sumar cero
#   textos muy pequenos puede no haber suficientes palabras para identificar el efecto
# * puede haber sesgos en casos como "I'm not happy and I don't like it!" dado que las
#   que esta compuesta palabras positivas (ver N-gramas)

suppressMessages(suppressWarnings(library(tidytext)))



# AFINN from Finn Arup Nielsen
# http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010
# escala (-5 a 5)

get_sentiments("afinn")



# Bing from Bing Liu and collaborators
# https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
# clasificacion binaria (+/-)

get_sentiments("bing")



# NRC from Saif Mohammad and Peter Turney
# http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
# clasificacion binaria (+/-) y algunas categorias

get_sentiments("nrc")



## comparacion de diccionarios
get_sentiments("nrc") %>%
        filter(sentiment %in% c("positive", "negative")) %>%
        count(sentiment)

get_sentiments("bing") %>%
        count(sentiment)

# ambos diccionarios tienen mas palabras positivas que negativas
# para cual diccionario la razon - a + es mayor?



#######################################################################
# Ejemplo : cuales son las palabras de alegria mas comunes en "Emma"? #
#######################################################################

suppressMessages(suppressWarnings(library(tidytext)))
suppressMessages(suppressWarnings(library(janeaustenr)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(ggplot2)))

## tokenization como tidy data (una palabra por fila)
tidy_books <- austen_books() %>%
        group_by(book) %>%
        mutate(linenumber = row_number(),
               chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
        ungroup() %>%
        unnest_tokens(word, text)



## filtrar palabras de alegria del diccionario NRC
nrcjoy <- get_sentiments("nrc") %>%
        filter(sentiment == "joy")



## contar las palabras de alegria en el libro
tidy_books %>%
        filter(book == "Emma") %>%
        inner_join(nrcjoy) %>%
        count(word, sort = TRUE)



#############################################################
# Ejemplo : como cambia el sentimiento a traves de novelas? #
#############################################################

## contar palabras +/- en secciones de a 80 palabras
janeaustensentiment <- tidy_books %>%
        inner_join(get_sentiments("bing")) %>%
        count(book, index = linenumber %/% 80, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)

# x %/% y es equivalente a floor(x/y))



## graficar
windows()
ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~book, ncol = 2, scales = "free_x")

# se evidencian los cambios de "mood" a lo largo de los textos



################################################################
# Ejemplo : comparacion de diccionarios en Pride and Prejudice #
################################################################

## obtener texto de referencia
pride_prejudice <- tidy_books %>%
        filter(book == "Pride & Prejudice")



## sentimiento usando AFINN (scores)
afinn <- pride_prejudice %>%
        inner_join(get_sentiments("afinn")) %>%
        group_by(index = linenumber %/% 80) %>%
        summarise(sentiment = sum(value)) %>%
        mutate(method = "AFINN")



## sentimiento usando BING y NRC (+/-)
bing_and_nrc <- bind_rows(
        pride_prejudice %>%
                inner_join(get_sentiments("bing")) %>%
                mutate(method = "BING"),
        pride_prejudice %>%
                inner_join(get_sentiments("nrc") %>%
                filter(sentiment %in% c("positive", "negative"))) %>%
                mutate(method = "NRC")) %>%
        count(method, index = linenumber %/% 80, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)



## graficar
windows()
bind_rows(afinn, bing_and_nrc) %>%
        ggplot(aes(index, sentiment, fill = method)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~method, ncol = 1, scales = "free_y")

# en terminos absolutos los resultados son diferentes
# en terminos relativos hay ciertas similitudes en las trayectorias (picos y valles)
# cual diccionario presenta los valores absolutos mas grandes (mator varianza)?
# cual diccionario provee bloques mas prolongados?
# cual diccionario presenta una evaluacion mas positiva?



###########################################################
# Ejemplo : cuanto una palabra contribuye al sentimiento? #
###########################################################

## conteo de palabras
bing_word_counts <- tidy_books %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        ungroup()



## graficar
windows()
bing_word_counts %>%
        group_by(sentiment) %>%
        top_n(10) %>%
        ungroup() %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Contribution to sentiment", x = NULL) +
        coord_flip()

# cuidado con "miss"!
# deberia agregarse como "stop word" en un futuro analisis

custom_stop_words <- bind_rows(data_frame(word = c("miss"), lexicon = c("custom")), stop_words)



###############################
# Ejemplo: libraria wordcloud #
###############################

suppressMessages(suppressWarnings(library(wordcloud)))

## graficar las palabras mas comunes
windows()
tidy_books %>%
        anti_join(stop_words) %>%
        count(word) %>%
        with(wordcloud(word, n, max.words = 100))




suppressMessages(suppressWarnings(library(reshape2)))

## comparar sentimientos
windows()
tidy_books %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>%
        acast(word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("gray20", "gray80"), max.words = 100)

# acast : cambiar el formato de data.frame a matrix
# visualizar palabras mas comunes
# el tamno no es comparable a traves de sentimientos



#####################################################
# Ejemplo : cuales son los capitulos mas negativos? #
#####################################################

## palabras negativas de BING
bingnegative <- get_sentiments("bing") %>%
        filter(sentiment == "negative")



## cantidad de palabras por libro
wordcounts <- tidy_books %>%
        group_by(book, chapter) %>%
        summarize(words = n())



## palabras negativas en cada capitulo y normalizar 
tidy_books %>%
        semi_join(bingnegative) %>%
        group_by(book, chapter) %>%
        summarize(negativewords = n()) %>%
        left_join(wordcounts, by = c("book", "chapter")) %>%
        mutate(ratio = negativewords/words) %>%
        filter(chapter != 0) %>%
        top_n(1) %>%
        ungroup()




##########################################
# Ejemplo : diferentes unidades de texto #
##########################################

# examinar el sentimiento de una frase como un todo

## tokenization con frases
PandP_sentences <- data_frame(text = prideprejudice) %>%
        unnest_tokens(sentence, text, token = "sentences")

# ver por ejemplo
# coreNLP (Arnold and Tilton 2016)
# cleanNLP (Arnold 2016)
# sentimentr (Rinker 2017)
# exposicion?