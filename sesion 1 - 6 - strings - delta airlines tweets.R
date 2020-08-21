# Text mining is the process of distilling actionable insights from text.

# Text mining represents the ability to take large amounts of unstructured language
# and quickly extract useful and novel insights that can affect stakeholder 
# decision-making.

# 1. Define the problem and specific goals.
# 2. Identify the text that needs to be collected.
# 3. Organize the text.
# 4. Extract features.
# 5. Analyze.
# 6. Reach an insight or recommendation.

#-------------------------------------------------------------------------------

library(stringi)
library(stringr)
library(qdap)     # alternative: use library(textclean) 
library(lubridate)
library(tm)

#-------------------------------------------------------------------------------

# understand Delta Airlines' customer service tweets.

# 1. What is the average length of a social customer service reply?
# 2. What links were referenced most often?
# 3. How many people should be on a social media customer service team?
# 4. How many social replies are reasonable for a customer service representative
#    to handle?

text.df <- read.csv("C:/Users/Juan Camilo/Dropbox/USTA/Cursos/text mining/USTA text mining/code/oct_delta.txt")

dim(text.df)

colnames(text.df)

# from factor to character
text.df$text <- as.character(text.df$text)

# number of characters
nchar(head(text.df$text))

nchar(text.df[4, 5])

# mean number of characters
mean(nchar(text.df$text))

# when necessary
subset.doc <- subset(text.df, nchar(text.df$text) > 0)

# substitute text (see help)
sub('thanks', 'thank you', text.df[1,5], ignore.case=T)

sub('pls', 'please', text.df[1:5,5], ignore.case=F)

fake.text <- 'R text mining is good but text mining in python is also'

sub('text mining','tm', fake.text, ignore.case=F)

gsub('text mining','tm', fake.text, ignore.case=F)

gsub('&amp', '', text.df[5,5])

gsub('[[:punct:]]', '', text.df[1:5,5])


fake.text    <- 'R text mining is good but text mining in python is also'
patterns     <- c('good','also','text mining')
replacements <- c('great','just as suitable','tm')
mgsub(patterns, replacements, fake.text)

#-------------------------------------------------------------------------------

# string manipulation

patterns <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

replacements <- seq(1:12)

text.df$month <- mgsub(patterns, replacements, text.df$month)

text.df$combined <- paste(text.df$month, text.df$date, text.df$year, sep='-')

text.df$combined <- mdy(text.df$combined)

#-------------------------------------------------------------------------------

substring('R text mining is great', 18, 22)

agents <- strsplit(text.df$text, '[*]')

last.chars <- function(text, num)  substr(text, nchar(text)-num+1, nchar(text))

last.chars('R text mining is great', 8)

text.df$text[1:2]

last.chars(text.df$text[1:2], 2)

weekdays <- subset(text.df, text.df$combined >= mdy('10- 05-2015') & text.df$combined<= mdy('10-09-2015'))

table(as.factor(last.chars(weekdays$text, 2)))

#-------------------------------------------------------------------------------

# Keyword Scanning

grep('sorry', text.df$text, ignore.case=T, value = F)

text.df$text[413]

sorry <- grepl('sorry', text.df$text, ignore.case=T)

sum(sorry)/nrow(text.df)

grep(c('sorry|apologize'), text.df$text, ignore.case=T)

sum(grepl('http', text.df$text, ignore.case = T))/nrow(text.df)

sum(grepl('[0-9]{3})|[0-9]{4}', text.df$text))/nrow(text.df)

#-------------------------------------------------------------------------------

# String Packages stringr and stringi

stri_count(text.df$text, fixed='http')

str_detect(text.df$text, 'http')

patterns <- with(text.df, str_detect(text.df$text, 'http') & str_detect(text.df$text, 'DM'))

patterns <- with(text.df,str_detect(text.df$text, 'DM'))

text.df[patterns, 5]

#-------------------------------------------------------------------------------

# Preprocessing Steps

tweets <- data.frame(ID = seq(1:nrow(text.df)), text = text.df$text)

# tm functions:

s <- "Starbuck's is from Seattle."

tolower(s)

s <- "Watch out! That coffee is going to spill!"

removePunctuation(s)
        
s <- "I like      coffee."

stripWhitespace(s)
        
s <- "I drank 4 cups of coffee 2 days ago."

removeNumbers(s)

s <- "The coffee house and barista he visited were nice, she said hello."

removeWords(s, words = c("he", "she"))

s <- "Transforming words to do text mining applications is often needed."

stemDocument(s)

# stopwords
custom.stopwords <- c(stopwords('english'), 'lol', 'smh', 'delta')

# Return NA instead of tolower error
tryTolower <- function(x){
        # return NA when there is an error
        y = NA
        # tryCatch error
        try_error = tryCatch(tolower(x), error = function(e) e)
        # if not an error
        if (!inherits(try_error, 'error'))
                y = tolower(x)
        return(y)
}

clean.corpus <- function(corpus) {
        corpus <- tm_map(corpus, content_transformer(tryTolower))
        corpus <- tm_map(corpus, removeWords, custom.stopwords)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, removeNumbers)
        return(corpus)
}

docs <- data.frame(doc_id = tweets$ID, text = tweets$text, stringsAsFactors = F)

corpus <- VCorpus(DataframeSource(docs))

corpus <- clean.corpus(corpus)

tdm <- TermDocumentMatrix(corpus, control = list(weighting = weightTf))

tdm.tweets.m <- as.matrix(tdm)

dim(tdm.tweets.m)

tdm.tweets.m[2250:2255,1340:1342]

term.freq <- rowSums(tdm.tweets.m)

freq.df <- data.frame(word = names(term.freq), frequency = term.freq)
freq.df <- freq.df[order(freq.df[,2], decreasing=T),]
freq.df[1:10,]

