################################################################################
############################### Web Analytics ##################################
################################################################################

# CKAN : The Comprehensive Knowledge Archive Network.
# https://ckan.org/
# Open Source data portal platform.

#-------------------------------------------------------------------------------

# Load required packages from local library into R.

library(ckanr)     # Access data from CKAN.
library(dplyr)     # Wrangling: group_by().
library(ggplot2)   # Visualise data.
library(magrittr)  # Pipelines for data processing: %>% %T>% %<>%.
library(rattle)    # Wrangling: normVarNames().
library(readr)     # Modern data reader.
library(scales)    # Include commas in numbers in plots.
library(stringi)   # The string concat operator: %s+%.
library(stringr)   # String manpiulation: str_split().
library(tidyr)     # Tidy the dataset: gather().
library(xtable)    # Format R data frames as LaTeX tables.

#-------------------------------------------------------------------------------

# Application Programmer Interfaces (API)

# Since the repository supports the CKAN API we can use ckanr to program our 
# navigation through datasets.



# List some of the known servers.
servers()

# Count the number of servers.
servers() %>% length()

# Current default server.
get_default_url()

# Change the default server.
ckanr_setup(url = "http://data.gov.au")

# Check the new default server.
get_default_url()

# https://www.datos.gov.co/ esta en la lista?

#-------------------------------------------------------------------------------

# Store the list of organisations.
orgs <- organization_list(as="table")

# Report the number of organisations listed.
nrow(orgs)

# Identify metadata available.
names(orgs)

################################################################################
############################## Browser Data ####################################
################################################################################

# Australian Taxation Office (ATO)
# https://www.ato.gov.au/

# "ATO Web Analytics"
pattern <- "title:ato web analytics"

# Search for specific package.
ato_web_pkg <- package_search(pattern)$results[[1]] %T>% print()

# Save the package identifier.
pid <- ato_web_pkg$id %T>% print()

# number of resources (datasets)
ato_web_pkg$num_resources

# list of datasets included within the resources metadata
ato_web_pkg$resources %>% sapply(extract2, 'name')

#-------------------------------------------------------------------------------

# "Browser by month and traffic source - July 2013 to April 2014"
# Save the resource structure for the web analytics dataset.
# describe aspects of the dataset.
bwres <- ato_web_pkg$resources[[1]]

# Available metadata.
names(bwres)

# description.
bwres$description

# location.
bwres$url

# format.
bwres$format

# name.
bwres$name

# https://data.gov.au/data/dataset/ato-web-analytics-july-2013-to-april-2014

#-------------------------------------------------------------------------------

# download data

dspath  <- bwres$url
csvname <- bwres$name %s+% ".csv"
temp    <- tempfile(fileext=".zip")

download.file(dspath, temp)
browsers <- unz(temp, csvname) %>% read_csv()
unlink(temp)

#-------------------------------------------------------------------------------

# preparation

# dataset copy.
dsname <- "browsers"
ds     <- get(dsname)

# take a look at the dataset.
glimpse(ds)

# normalise variables' names
names(ds) %<>% normVarNames() %T>% print()

names(ds)[3] <- "source"

# glimpse again. 
glimpse(ds)

# What is a page view?
# A page view is when a page on your site is loaded by a browser.

# What is a visit?
# A visit is any time a visitor reaches your site from somewhere outside of your 
# website domain. That means the person was on a different site and clicked on a 
# link that took them to your site or entered your website URL directly into their 
# browser. 

#-------------------------------------------------------------------------------

# varibles characteristics
ds %>% sapply(class)

# date: ch with 10 levels
length(unique(ds$month))

unique(ds$month)

# date as factor (other formats can be used)
ds %<>% 
        mutate(month = factor(month, levels=unique(ds$month)))

length(unique(ds$source))

unique(ds$source)

# source as factor
ds %<>% 
        mutate(source=factor(source))

length(unique(ds$browser))

# the idea is to tidy up this variable since there are several cases refering to
# the same OS/Browsers (e.g. Microsoft)

# extract first word
ds %<>% mutate(browser = str_extract(ds$browser, '[A-Za-z]+'))

# Threshold below which a browser is considered as Other.
visits.threshold <- 1e4

# Determine the list of Other browsers.
ds %>%
        group_by(browser) %>%
        summarise(visits=sum(visits)) %>%
        filter(visits < visits.threshold) %>%
        extract2('browser') ->
        other


# Helper function.
remap.other <- function(x) if (x %in% other) "Other" else x

# Remap the browsers noting the opportunity to collapse
# repeated browser/month/source entries. Notice we need to
# remove the grouping to facilitate fuhter processing.
ds %<>%
        mutate(browser=sapply(browser, remap.other)) %>%
        group_by(browser, month, source) %>%
        summarise(views=sum(views), visits=sum(visits)) %>%
        ungroup()

# Check the number of unique browsers we now have.
length(unique(ds$browser))

# Record the browser names ordered by their frequency of visits.
ds %>%
        group_by(browser) %>%
        summarise(visits=sum(visits)) %>%
        arrange(visits) %>%
        extract2("browser") %>%
        as.character() %T>%
        print() ->
        blvls

# Convert browser into a factor with the sorted levels.
ds %<>% 
        mutate(browser=factor(browser, levels=blvls))

# glimpse the data set again.
glimpse(ds)

#-------------------------------------------------------------------------------

# analysis

ds %>% 
        group_by(source) %>%
        summarise(total=sum(visits)) %T>%
        print() ->
        freq

# Internal visits account for just 3% of all visits.
round(100*freq$total[2]/sum(freq$total))

# profiles of browsers used and their changing patterns of usage over time.
# ignoring those less frequently represented browsers.
# external visitors.
windows()
ds %>%
        filter(browser %in% levels(browser)[4:9]) %>%
        filter(source == "External") %>%
        ggplot(aes(month, visits, fill=browser)) +
        geom_bar(stat="identity") +
        facet_wrap(~browser) +
        scale_y_continuous(labels=comma) +
        theme(axis.text.x=element_text(angle=45, hjust=1)) +
        theme(legend.position="none")

# internal.
windows()
ds %>%
        filter(browser %in% levels(browser)[4:9]) %>%
        filter(source == "Internal") %>%
        ggplot(aes(month, visits, fill=browser)) +
        geom_bar(stat="identity") +
        facet_wrap(~browser) +
        scale_y_continuous(labels=comma) +
        theme(axis.text.x=element_text(angle=45, hjust=1)) +
        theme(legend.position="none")

# frequencies
ds %>%
        group_by(browser, source) %>%
        summarise(total=sum(visits)) %>%
        spread(source, total) %>%
        set_names(c("Browser", "External", "Internal"))

################################################################################
################################ Entry Pages ###################################
################################################################################

# description.
ato_web_pkg$resources[[2]]$name

# download dataset. 
url     <- ato_web_pkg$resources[[2]]$url
csvname <- ato_web_pkg$resources[[2]]$name %s+% ".csv"
temp    <- tempfile(fileext=".zip")

download.file(url, temp)
entry <- unz(temp, csvname) %>% read_csv()
unlink(temp)

#-------------------------------------------------------------------------------

# data preparation

# taka a look.
glimpse(entry)

# load dataset.
dsname <- "entry"
ds <- get(dsname)

# normalise variables.
names(ds) <- normVarNames(names(ds)) %T>% print()

vars <- names(ds)

# which variables should be treated as factors and which ones as strings?
ds %>%
        sapply(is.character) %>%
        which() %T>%
        {names(.) %>% print()} %>%
        select(ds, .) %>%
        sapply(function(x) unique(x) %>% length())

# check month.
ds %>% select(month) %>% table()

# something is wrong!
ds %>% select(month) %>% unique()

# record.
ds %>% filter(month == "External") %>% print.data.frame()

# obs. number.
which(ds$source == "4")

# remove observation.
dim(ds)

ds %<>% filter(month != "External")

dim(ds)

# convert month into a factor.
unique(ds$month)

months <- c("Jul-13", "Aug-13", "Sep-13", "Oct-13", "Nov-13", "Dec-13", "Jan-14", "Feb-14", "Mar-14", "Apr-14")

ds %<>% mutate(month=factor(month, levels=months))

# take a look at source.
ds %>% select(source) %>% table()

# convert source into a factor.
ds %<>% mutate(source=factor(source))

#-------------------------------------------------------------------------------

# analysis

# summary.
summary(ds)

# total number of views.
ds$views %>% sum %>% comcat()

# total number of visits.
ds$visits %>% sum %>% comcat()

# visualization.
windows()
ds %>%
        group_by(month) %>%
        summarise(views=sum(views), visits=sum(visits)) %>%
        gather(type, count, -month) %>%
        ggplot(aes(x=month, y=count, fill=type)) +
        geom_bar(stat="identity", position="dodge") +
        scale_y_continuous(labels=comma) +
        labs(fill="Type", x="Month", y="Count") +
        theme(axis.text.x=element_text(angle=45, hjust=1))

# gather example
# https://tidyr.tidyverse.org/reference/gather.html

# visualization by source.
windows()
ds %>%
        group_by(month, source) %>%
        summarise(views=sum(views), visits=sum(visits)) %>%
        gather(type, count, -c(month, source)) %>%
        ggplot(aes(x=month, y=count, fill=type)) +
        geom_bar(stat="identity", position="dodge") +
        scale_y_continuous(labels=comma) +
        labs(fill="Type", x="Month", y="Count") +
        theme(axis.text.x=element_text(angle=45, hjust=1)) +
        facet_wrap(~source)