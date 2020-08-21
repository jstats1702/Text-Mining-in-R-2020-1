################################################################################
#################################### Intro #####################################
################################################################################

#-------------------------------------------------------------------------------

# remove every object in the Global Enviroment
rm(list=ls(all=TRUE))

# Install packages if needed
packs <- c("dplyr", "ggplot2", "magrittr", "rattle", "readr")
for (i in packs) if (!(i %in% installed.packages())) install.packages(i)

#-------------------------------------------------------------------------------

# Load packages required for this script.
library(dplyr)     # Data wrangling and glimpse().
library(ggplot2)   # Visualise data.
library(magrittr)  # Pipes %>%, %<>%, %T>%, %$%.
library(rattle)    # The weatherAUS dataset and normVarNames().
library(readr)     # Efficient reading of CSV data.

#-------------------------------------------------------------------------------

# glimpse dataset
glimpse(weatherAUS)

#-------------------------------------------------------------------------------

# quick plot
windows()
qplot(data=weatherAUS, x=MinTemp, y=MaxTemp)

#-------------------------------------------------------------------------------

# simulate and plot data
N <- 2000                                # data size
a <- 0                                   # intercept
b <- 1                                   # slope
sigma <- 0.1                             # standard deviation 
x <- runif(n = N)                        # independent varaible
e <- rnorm(n = N, mean = 0, sd = sigma)  # random error
y <- a + b*x + e 

# scatter plot
windows()
ggplot(mapping=aes(x, y)) + geom_point(alpha=0.5)

#-------------------------------------------------------------------------------

# Select columns from the dataset.
weatherAUS %>%
        select(MinTemp, MaxTemp, Rainfall, Sunshine) %>%
        glimpse()

#-------------------------------------------------------------------------------

# Select colums from the dataset and summarise the result.
weatherAUS %>%
        select(MinTemp, MaxTemp, Rainfall, Sunshine) %>%
        summary()

#-------------------------------------------------------------------------------

# Select specific columns and observations from the dataset.
weatherAUS %>%
        select(MinTemp, MaxTemp, Rainfall, Sunshine) %>%
        filter(Rainfall >= 1)

#-------------------------------------------------------------------------------

# Select columns/observations and save the result.
rainy_days <- 
        weatherAUS %>%
        select(MinTemp, MaxTemp, Rainfall, Sunshine) %>%
        filter(Rainfall >= 1)

# Demonstrate use of the forward assignment operator.
weatherAUS %>%
        select(MinTemp, MaxTemp, Rainfall, Sunshine) %>%
        filter(Rainfall >= 1) ->
        rainy_days

#-------------------------------------------------------------------------------

# Summarise subset of variables for observations with rainfall.
weatherAUS %>%
        select(MinTemp, MaxTemp, Rainfall, Sunshine) %>%
        filter(Rainfall >= 1) %>%
        summary()

# Summarise observations with little or no rainfall.
weatherAUS %>%
        select(MinTemp, MaxTemp, Rainfall, Sunshine) %>%
        filter(Rainfall < 1) %>%
        summary()

# Functional form equivalent to the pipeline above.
summary(filter(select(weatherAUS, MinTemp, MaxTemp, Rainfall, Sunshine), Rainfall < 1))

#-------------------------------------------------------------------------------

# Copy the dataset into the variable ds.
ds <- weatherAUS

# Report on the dimensions of the dataset.
dim(ds)

# Demonstrate an assignment pipeline.
ds %<>%
        filter(Rainfall == 0) %>%
        select(MinTemp, MaxTemp, Sunshine)

# Confirm that the dataset has changed.
dim(ds)

# Functional form equivalent to the pipeline above.
ds <- select(filter(weatherAUS, Rainfall == 0), MinTemp, MaxTemp, Sunshine)

#-------------------------------------------------------------------------------

# Demonstrate usage of a tee-pipe.
weatherAUS %>%
        filter(Rainfall == 0) %T>%
        {head(.) %>% print()} ->
        no_rain

# Alternative to using a tee-pipe for a simple pipeline.
weatherAUS %>%
        filter(Rainfall == 0) ->
        no_rain

head(no_rain)

#-------------------------------------------------------------------------------

# Multiple tee-pipes in a single pipeline.
# rattle::comcat() which simply formats the incoming data to be more easily readable.
weatherAUS %>%
        filter(Rainfall == 0) %T>%
        {dim(.) %>% comcat()} %T>%
        {head(.) %>% print()} ->
        no_rain

#-------------------------------------------------------------------------------

# Multiple tee-pipes in a single pipeline.
weatherAUS %>%
        filter(Rainfall == 0) %T>%
        {dim(.) %>% comcat()} %T>%
        {head(.) %>% print()} %T>%
        {summary(.) %>% print()} ->
        no_rain

#-------------------------------------------------------------------------------

# Identify cities of interest.
cities <- c("Canberra", "Darwin", "Melbourne", "Sydney")

# Build the required dataset and plot it.
weatherAUS %>%
        filter(Location %in% cities) %>%
        filter(Temp3pm %>% is.na() %>% not()) %>%
        ggplot(aes(x=Temp3pm, colour=Location, fill=Location)) +
        geom_density(alpha=0.5) +
        labs(title   ="Distribution of the Daily Temperature at 3pm",
             subtitle="Selective Locations",
             caption ="Source: Australian Bureau of Meteorology",
             x       ="Temperature Recorded at 3pm",
             y       ="Density")

################################################################################
################################################################################
################################################################################