##############################
### frecuencia de palabras ###
##############################

# * identificar parabras con mayores frecuencias
# * comparar documentos 

## data
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")



## convertir a data frame
suppressMessages(suppressWarnings(library(dplyr)))
text_df <- data_frame(line = 1:4, text = text)

# no tiene "estructura" para analizar
        


## tokenizacion como tidy data
suppressMessages(suppressWarnings(library(tidytext)))
text_df %>%
        unnest_tokens(tbl = ., output = word, input = text)

# output : nombre del output
# input  : nombre de donde proviene la data
# se elimina la puntuacion
# por defecto en minusculas (to_lower)

# este formato permite manipular, procesar y vizualizar la 
# data con dplyr, tidyr, ggplot2,



#####################################
# Ejemplo : Trabajos de Jane Austen #
#####################################

suppressMessages(suppressWarnings(library(janeaustenr)))
suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(tidytext)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(scales)))


# Jane Austen (Steventon, 16 de diciembre de 1775-Winchester, 18 de julio de 1817) fue 
# una novelista britanica que vivio durante la epoca georgiana. 
# La ironia que emplea para dotar de comicidad a sus novelas hace que Jane Austen sea 
# considerada entre los clasicos de la novela inglesa.
# https://es.wikipedia.org/wiki/Jane_Austen

# Trabajos completos (6 novelas) de Jane Austen's
# "Sense and Sensibility", "Pride and Prejudice", "Mansfield Park", "Emma", 
# "Northanger Abbey", and "Persuasion".



## agregar linenumber y chapter
original_books <- austen_books() %>%
        group_by(book) %>%
        mutate(linenumber = row_number(), 
               chapter = cumsum(str_detect(string = text, pattern = regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
        ungroup()

# regex : regular expressions 
#         https://regex101.com/r/3kzOvH/1#         
#         https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/regex
#
#         ^ asserts position at start of a line
#         \d    matches a digit (equal to [0-9])
#         ivxlc matches a single character in the list ivxlc (case insensitive)


 
## tokenization como tidy data (una palabra por fila)
tidy_books <- original_books %>%
        unnest_tokens(tbl = ., output = word, input = text)

# formato de una palabra por linea
# cada token es una palabra



## remover stop words
data(stop_words)

table(stop_words$lexicon)

# http://www.lextek.com/manuals/onix/stopwords1.html
# http://www.jmlr.org/papers/volume5/lewis04a/lewis04a.pdf
# http://snowball.tartarus.org/algorithms/english/stop.txt

tidy_books <- tidy_books %>%
        anti_join(x = ., y = stop_words)



## contar palabras
tidy_books %>%
        count(word, sort = TRUE)



## visualizacion
windows()
tidy_books %>%
        count(word, sort = TRUE) %>%
        filter(n > 600) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n)) +
        geom_col() +
        xlab(NULL) +
        coord_flip()



################################
# Ejemplo : paquete gutenbergr #
################################

# El proyecto Gutenberg es una biblioteca con mas de 60,000 eBooks gratis.
# https://www.gutenberg.org/
# https://ropensci.org/tutorials/gutenbergr_tutorial/

# una tarea comun en DM es mirar frecuencias de palabras, y compararlas a traves de diferentes textos.

suppressMessages(suppressWarnings(library(gutenbergr)))

# Herbert George Wells (Bromley; 21 de septiembre de 1866-Londres, 13 de agosto de 1946),
# fue un escritor, y novelista britanico. 
# Es recordado por sus novelas de ciencia ficcion y es frecuentemente citado como el 
# padre de la ciencia ficcion junto con Julio Verne y Hugo Gernsback.
# https://es.wikipedia.org/wiki/H._G._Wells



## libros
gutenberg_works(author == "Wells, H. G. (Herbert George)")



## Trabajos de H. G. Wells
## The Time Machine, The War of the Worlds, The Invisible Man, and The Island of Doctor Moreau
hgwells <- gutenberg_download(c(35, 36, 5230, 159))
tidy_hgwells <- hgwells %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words)



## palabras mas comunes H. G. Wells
tidy_hgwells %>%
        count(word, sort = TRUE)



## Trabajos de las hermanas Bronte
## Jane Eyre, Wuthering Heights, The Tenant of Wildfell Hall, Villette, and Agnes Grey
bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) 



## palabras mas comunes hermanas Bronte
tidy_bronte %>%
        count(word, sort = TRUE)

# "time", "eyes", y "hand" estan en el top 10 de H.G. Wells y las hermanas Bronte



## comparar las freciencias de los tres autores
frequency <- bind_rows(mutate(.data = tidy_bronte,  author = "Bronte Sisters"),
                       mutate(.data = tidy_hgwells, author = "H.G. Wells"),
                       mutate(.data = tidy_books,   author = "Jane Austen")) %>%
        mutate(word = str_extract(word, "[a-z']+")) %>%
        count(author, word) %>%
        group_by(author) %>%
        mutate(proportion = n / sum(n)) %>%
        select(-n) %>%
        spread(author, proportion) %>%
        gather(author, proportion, `Bronte Sisters`:`H.G. Wells`)

# se usa str_extract() porque hay texto codificado (UTF-8)
# por ejemplo, any es lo mismo que _any_

# UTF-8 (8-bit Unicode Transformation Format) es un formato de codificación de caracteres 
# capaz de codificar todos los code points validos en Unicode

## plot
windows(width = 10, height = 5)
ggplot(frequency, aes(x = proportion, y = `Jane Austen`,
                      color = abs(`Jane Austen` - proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001),
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~author, ncol = 2) +
        theme(legend.position = "none") +
        labs(y = "Jane Austen", x = NULL)

# palabras cerca de la línea tienen frecuencias similares en ambos textos
# por ejemplo, "miss", "time", "day", y "brother"

# Las palabras que están lejos de la línea son palabras que se encuentran más en un 
# conjunto de textos que en otro

# cual grafico es mas denso?
# que diferencias hay entre los graficos en terminos de forma?
# entre que par de textos hay mas similitudes?



## prueba de correlacion
cor.test(data = frequency[frequency$author == "Bronte Sisters",], ~ proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",], ~ proportion + `Jane Austen`)

# entre que par de textos las freciencias estan mas correlacionadas?