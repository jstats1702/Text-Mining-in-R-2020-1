#-------------------------------------------------------------------------------

suppressMessages(suppressWarnings(library(dplyr)))        # glimpse().
suppressMessages(suppressWarnings(library(ggplot2)))      # Visualise data.
suppressMessages(suppressWarnings(library(magrittr)))     # Data pipelines: %>% %<>% %T>% equals().
suppressMessages(suppressWarnings(library(randomForest))) # na.roughfix() for missing data.
suppressMessages(suppressWarnings(library(rattle)))       # normVarNames().
suppressMessages(suppressWarnings(library(rattle.data)))  # weatherAUS.
suppressMessages(suppressWarnings(library(scales)))       # commas(), percent().
suppressMessages(suppressWarnings(library(stringr)))      # str_replace_all().

#-------------------------------------------------------------------------------

iris %>%
        ggplot(aes(x=Sepal.Length, y=Sepal.Width)) +
        geom_point()

iris %>%
        ggplot(aes(x=Sepal.Length, y=Sepal.Width)) +
        geom_point() +
        geom_line()

iris %>%
        ggplot(aes(x=Sepal.Length, y=Sepal.Width)) +
        geom_point() +
        stat_smooth(method="loess")

iris %>%
        ggplot(aes(x=Sepal.Length, y=Sepal.Width, colour=Species)) +
        geom_point()

# Initialise the dataset as per the template.

dsname <- "weatherAUS"
ds     <- get(dsname)

glimpse(ds)

# Review the variables before normalising their names.

names(ds)

# Capture the original variable names for use in plots.

vnames <- names(ds)

# Normalise the variable names.

names(ds) %<>% normVarNames()

# Confirm the results are as expected.

names(ds)

# Index the original variable names by the new names.

names(vnames) <- names(ds)

vnames

# Note the available variables.

vars <- names(ds) %T>% print()

# Note the target variable.

target <- "rain_tomorrow"

# Place the target variable at the beginning of the vars.

vars <- c(target, vars) %>% unique() %T>% print()

# Note the risk variable which measures the severity of the outcome.

risk <- "risk_mm"

# Note the identifiers.

id <- c("date", "location")

# Initialise ignored variables: identifiers.

ignore <- c(risk, id)

# Remove the variables to ignore.

vars <- setdiff(vars, ignore)

# Identify the input variables for modelling.

inputs <- setdiff(vars, target) %T>% print()

# Also record them by indicies.

inputi <- 
        inputs %>%
        sapply(function(x) which(x == names(ds)), USE.NAMES=FALSE) %T>%
        print()

# Identify the numeric variables by index.

numi <- 
        ds %>%
        sapply(is.numeric) %>%
        which() %>%
        intersect(inputi) %T>%
        print()

# Identify the numeric variables by name.

numc <- 
        ds %>% 
        names() %>% 
        extract(numi) %T>% 
        print()

# Identify the categoric variables by index.

cati <- 
        ds %>%
        sapply(is.factor) %>%
        which() %>%
        intersect(inputi) %T>%
        print()

# Identify the categoric variables by name.

catc <-
        ds %>% 
        names() %>% 
        extract(cati) %T>% 
        print()

# Normalise the levels of all categoric variables.

for (v in catc) 
        levels(ds[[v]]) %<>% normVarNames()


# Count the number of missing values.

ds[vars] %>% is.na() %>% sum()

# Impute missing values.

ds[vars] %<>% na.roughfix()

# Confirm that no missing values remain.

ds[vars] %>% is.na() %>% sum()

glimpse(ds)

ds %>%
        sample_n(1000) %>%
        ggplot(aes(x=min_temp, y=max_temp, colour=rain_tomorrow)) +
        geom_point() +
        scale_colour_brewer(palette="Set2") +
        labs(x      = vnames["min_temp"], 
             y      = vnames["max_temp"], 
             colour = vnames["rain_tomorrow"])

ds %>%
        filter(location=="Canberra") %>%
        ggplot(aes(x=date, y=max_temp)) +
        geom_point(shape=".") +
        geom_smooth(method="gam", formula=y~s(x, bs="cs")) +
        labs(x=vnames["date"], y=vnames["max_temp"])

ds %>%
        ggplot(aes(x=date, y=max_temp)) +
        geom_point(alpha=0.05, shape=".") +
        geom_smooth(method="gam", formula=y~s(x, bs="cs")) +
        facet_wrap(~location) +
        theme(axis.text.x=element_text(angle=45, hjust=1)) +
        labs(x=vnames["date"], y=vnames["max_temp"])

ds %>%
        ggplot(aes(x=date, y=max_temp)) +
        geom_line(alpha=0.1, size=0.05) +
        geom_smooth(method="gam", formula=y~s(x, bs="cs")) +
        facet_wrap(~location) +
        theme(axis.text.x=element_text(angle=45, hjust=1)) +
        labs(x=vnames["date"], y=vnames["max_temp"])

lblr <- function(x) 
{
        x %>%
                str_replace_all("n", "North ") %>%
                str_replace_all("s", "South ") %>%
                str_replace_all("e", "East ") %>%
                str_replace_all("w", "West ") %>%
                str_replace(" $", "")
}

ds %>%
        sample_n(10000) %>%
        ggplot(aes(x=min_temp, y=max_temp, colour=rain_tomorrow)) +
        geom_point(shape=".") +
        geom_smooth(method="gam", formula=y~s(x, bs="cs")) +
        facet_wrap(~wind_dir_3pm, labeller=labeller(wind_dir_3pm=lblr)) +
        labs(x      = vnames["min_temp"], 
             y      = vnames["max_temp"], 
             colour = vnames["rain_tomorrow"])

ds %>%
        group_by(rain_tomorrow) %>%
        count() %>%
        ungroup()  %>% 
        mutate(per=round(`n`/sum(`n`), 2)) %>%
        mutate(label=paste(rain_tomorrow, percent(per))) %>%
        arrange(per) %>%
        ggplot(aes(x=1, y=per, fill=rain_tomorrow)) +
        geom_bar(stat="identity") +
        coord_polar(theta='y') +
        theme_void() +
        theme(legend.position="none") +
        geom_text(aes(x=1, y=cumsum(per)-per/2, label=label), size=8)

ds %>%
        ggplot(aes(x=wind_dir_3pm)) +
        geom_bar() +
        scale_y_continuous(labels=comma) +  
        labs(x=vnames["wind_dir_3pm"], y="Count")

d <- iris        # Full data set
d_bg <- d[, -5]  # Background Data - full without the 5th column (Species)

ggplot(d, aes(x = Sepal.Width, fill = Species)) +
        geom_histogram(data = d_bg, fill = "grey", alpha = .5) +
        geom_histogram(colour = "black") +
        facet_wrap(~ Species) +
        guides(fill = FALSE) +  # to remove the legend
        theme_bw()              # for clean look overall