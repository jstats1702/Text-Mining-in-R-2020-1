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

# nombre de las variables
colnames(gap)

# clase de objeto
class(gap)

# ojear algunos registros al azar
sample_n(tbl = gap, size = 10)

# dimension de los datos
dim(gap)

# numero de registros
gap %>% nrow() -> n

# numero de variables
gap %>% ncol() -> p

# numero de registros completos
gap %>% 
        complete.cases() %>%
        sum()

# clase de variable
sapply(X = gap, FUN = class)

# otra manera
gap %>% sapply(class)

# resumen 
gap %>% summary()

################################################################################
#################### GESTION DE DATOS USANDO dplyr #############################
################################################################################

# ej.
# seleccionar variables
gapminder %>% 
        select(country, year, lifeExp, gdpPercap)


# ej.
# filtrar por un continente especifico.
gapminder %>%
        filter(continent == "Americas")


# ej.
# filtrar por pais Colombia
gapminder %>% 
        filter(country == "Colombia")


# ej.
# filtrar por paises Colombia o Venezuela
gapminder %>%
        filter(country == "Colombia" | country == "Venezuela")


#ej. 
# gapminder paises alianza del pacifico
gapminder %>%
        filter(country %in% c("Chile", "Colombia", "Mexico", "Peru")) ->
        gap_ap


# ej.
# Construir una base de datos con las variables `country`, `year`, `gdpPercap`
# y entonces, filtrar por paises de la Alianza del Pacifico y años en el siglo XXI.
gapminder %>%
        select(country, year, gdpPercap) %>%
        filter(country %in% c("Chile", "Colombia", "Mexico", "Peru") & year >= 2000)

# otra forma
gapminder %>%
        select(country, year, pop) %>%
        filter(country %in% c("Colombia", "Venezuela")) %>%
        filter(year == 2007)


# ej.
# agrupar por continente
# y entonces, contar cuantos registros tiene cada continente,
# y entonces, contar cuantos paises distintos hay en cada continente
gapminder %>%
        group_by(continent) %>%
        summarize(n_observaciones = n(), n_paises = n_distinct(country))


# ej.
# construir una base de datos con las siguientes variables: country, year, gdpPercap
# y entonces, filtrar por pais Estados Unidos
# y entonces, construir la variable logaritmo de gdpPercap
gapminder %>%
        select(country, year, gdpPercap) %>%
        filter(country == "United States") %>%
        mutate(log_gdpPercap = log(gdpPercap))


# ej.
# construir una base de datos con las siguientes variables: continent, lifeExp
# y entonces, agrupar por continente
# y entonces, calcular el promedio de lifeExp
gapminder %>%
        select(continent, lifeExp) %>%
        group_by(continent) %>%
        summarise(prom_lifeExp = mean(lifeExp))

gapminder %>%
        select(continent, lifeExp) %>%
        group_by(continent) %>%
        summarise(min_vida = min(lifeExp), max_vida = max(lifeExp)) -> tab
 

# ej.
# filtrar por año 2007
# y entonces, agrupar por continente
# y entonces calcular la mediana de lifeExp
gapminder %>%
        filter(year == 2007) %>%
        group_by(continent) %>%
        summarise(lifeExp = median(lifeExp))


# ej.
# construir una base de datos con las siguientes variables: country, pop
# y entonces, filtrar por paises de Merco Sur
# y entonces, contruir la variable pop en millones
# y entonces, agrupar por pais
# y entonces, calcular el max de popMill
gapminder %>%
        select(country, pop) %>%
        filter(country %in% c("Argentina", "Brasil", "Paraguay", "Uruguay", "Venezuela")) %>%
        mutate(popMill = pop/1000000) %>%
        group_by(country) %>%
        summarise(max_popMill = max(popMill))


# ej.
# construir una base de datos con las siguientes variables: continent, country, year, gdpPercap
# y entonces, filtrar por continente America y año 2007
# y entonces, organizar ascendentemente por gdpPercap
gapminder %>%
        select(continent, country, year, gdpPercap) %>%
        filter(continent == "Americas" & year == 2007) %>%
        arrange(gdpPercap)


# ej.
# construir una base de datos con las siguientes variables: continent, country, year, gdpPercap
# y entonces, filtrar por continente America y año 2007
# y entonces, organizar descendentemente por gdpPercap
gapminder %>%
        select(continent, country, year, gdpPercap) %>%
        filter(continent == "Americas" & year == 2007) %>%
        arrange(desc(gdpPercap))

#################################################################################
#################################################################################