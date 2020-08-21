################################################################################
############################### librerias ######################################
################################################################################

library(dplyr)      # manipulacion de datos
library(magrittr)   # pipes
library(ggplot2)    # visualizacion
library(gapminder)  # base de datos

# cargar y adjuntar la base de datos gap
data(gapminder)

# duplicar la base de datos
gap <- gapminder


################################################################################
############################### Boxplots #######################################
################################################################################

# ej.
# boxplot simple
windows()
ggplot(data = gap, mapping = aes(x = " ", y = gdpPercap)) + 
        geom_boxplot() + 
        labs(title = "Distribución PIB per capita", 
             x     = " ",
             y     = "PIB per capita")


# ej. 
# boxplot simple
windows()
ggplot(data = gap, mapping = aes(x = " ", y = gdpPercap)) + 
        geom_boxplot(colour = "red", fill = "mistyrose", width = 0.5, 
                     outlier.colour = "red", outlier.shape = 16, outlier.size = 1, alpha = 0.25) + 
        stat_summary(fun.y = mean, geom = "point", shape = 4, size = 3, colour = "red") +
        labs(title    = "Distribución PIB per capita", 
             subtitle = "Todos los paises",
             caption  = "Source: Gapminder",
             x        = " ",
             y        = "PIB per capita")


# ej.
# boxplot compuesto
windows()
ggplot(data = gap_ap, mapping = aes(x = country, y = gdpPercap)) + 
        geom_boxplot() 


# ej.
# boxplot compuesto
windows()
ggplot(data = gap_ap, mapping = aes(x = country, y = gdpPercap, color = country)) + 
        geom_boxplot() +
        geom_jitter(shape = 16, position=position_jitter(0.1), alpha = 0.25) + 
        labs(title    = "Distribución PIB per capita", 
             subtitle = "Alianza del Pacífico",
             caption  = "Source: Gapminder",
             x        = "País",
             y        = "PIB per capita",
             color    = "País")

################################################################################
################################ Histogramas ###################################
################################################################################

# ej.
# histograma
windows()
ggplot(data = gap, aes(x = gdpPercap)) + 
        geom_histogram()


# ej.
# histograma
windows()
ggplot(data = gap, aes(x = gdpPercap, y = ..density..)) + 
        geom_histogram(bins = 30, color = "darkblue", fill = "lightblue") +
        labs(title    = "Distribución PIB per capita", 
             subtitle = "Todos los paises",
             caption  = "Source: Gapminder",
             x        = "PIB per capita",
             y        = "Densidad")


# ej.
# histograma compuesto
windows()
ggplot(data = gap, aes(x = gdpPercap)) + 
        geom_histogram() + 
        facet_wrap(~continent) +
        labs(title    = "Distribución PIB per capita", 
             subtitle = "Todos los paises por contiente",
             caption  = "Source: Gapminder",
             x        = "PIB per capita",
             y        = "Frecuencia")


# ej.
windows()
ggplot(data = gap, aes(x = log(gdpPercap), colour = continent, fill = continent)) +
        geom_density(alpha = 0.5) +
        labs(title    = "Distribución PIB per capita", 
             subtitle = "Todos los paises por continente",
             caption  = "Source: Gapminder",
             x        = "PIB per capita",
             y        = "Densidad")


################################################################################
############################### Dispersogramas #################################
################################################################################

# ej.
# dispersograma
windows()
qplot(x = gap$lifeExp, y = gap$gdpPercap)


# ej.
# dispersograma
windows()
ggplot(data = gap) +
        geom_point(mapping = aes(x = lifeExp, y = gdpPercap))


# ej.
# dispersograma
windows()
ggplot(data = gap) +
        geom_point(mapping = aes(x = lifeExp, y = gdpPercap, size = pop), colour = "red") +
        labs(title    = "PIB per capita vs. Esperanza de vida", 
             x        = "Esperanza de vida",
             y        = "PIB per capita")


# ej.
# dispersograma
windows()
ggplot(data = gap) +
        geom_point(mapping = aes(x = lifeExp, y = gdpPercap, size = pop, colour = continent), alpha = 0.25) +
        scale_y_continuous(trans = "log") +
        labs(title    = "PIB per capita vs. Esperanza de vida", 
             subtitle = "Todos los paises",
             caption  = "Source: gap",
             x        = "Esperanza de vida",
             y        = "PIB per capita",
             color    = "Continente",
             size     = "Población")

################################################################################
################################################################################
################################################################################