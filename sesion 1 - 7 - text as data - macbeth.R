################################################################################
############################# Text as data #####################################
################################################################################

# natural language processing 
# computational linguistics

# extract meaning algorithmically
# quantifying ideas embedded in the text

# computers are really good at storing text, but not very good at understanding it 
# humans are really good at understanding text, but not very goo at storing it

#-------------------------------------------------------------------------------

library(mdsr)
library(stringr)
library(tidyr)
library(ggplot2)

#-------------------------------------------------------------------------------

macbeth_url <- "http://www.gutenberg.org/cache/epub/1129/pg1129.txt"
Macbeth_raw <- RCurl::getURL(macbeth_url)

# strsplit returns a list: we only want the first element
macbeth <- strsplit(Macbeth_raw, "\r\n")[[1]]

# number of lines
length(macbeth)

#-------------------------------------------------------------------------------

# how many times does the character Macbeth speak in the play?
macbeth_lines <- grep("  MACBETH", macbeth, value = TRUE)

length(macbeth_lines)

head(macbeth_lines)



grepl("  MACBETH", macbeth)

length(grepl("  MACBETH", macbeth))



head(macbeth[grep ("  MACBETH", macbeth, value = F)])

head(macbeth[grepl("  MACBETH", macbeth)])

#-------------------------------------------------------------------------------

# . is a metacharacter that matches any character
grep("MAC.", macbeth, value = TRUE)


grep("MACBETH\\.", macbeth, value = TRUE)


# match MACBETH but not MACALESTER
grep("MAC[B-Z]", macbeth, value = TRUE)


# match any lines that contain either MACB or MACD.
grep("MAC(B|D)", macbeth, value = TRUE)


# anchors : ^ $
# ^ and $ are anchors. ^ anchors to the begining, whereas $ anchors to the end
grep("^  MAC[B-Z]", macbeth, value = TRUE)


# repetitions: ? * +
# ? = zero or one time, * =  zero or more times,  + =  one or more times
# applied to the previous element in the pattern
grep("^ ?MAC[B-Z]", macbeth, value = TRUE)


grep("^ *MAC[B-Z]", macbeth, value = TRUE)


grep("^ +MAC[B-Z]", macbeth, value = TRUE)

#-------------------------------------------------------------------------------

# analyze the speaking patterns

Macbeth     <- grepl("  MACBETH\\.",      macbeth)
LadyMacbeth <- grepl("  LADY MACBETH\\.", macbeth)
Banquo      <- grepl("  BANQUO\\.",       macbeth)
Duncan      <- grepl("  DUNCAN\\.",       macbeth)

speaker_freq <- data.frame(Macbeth, LadyMacbeth, Banquo, Duncan) %>%
        mutate(line = 1:length(macbeth)) %>%
        gather(key = "character", value = "speak", -line) %>%
        mutate(speak = as.numeric(speak)) %>%
        filter(line > 228 & line < 3172)

glimpse(speaker_freq)


# ejemplo usando gather
# mini_iris <- iris[c(1, 51, 101), ]
# gather(mini_iris, key = "flower_att", value = "measurement", -Species)


# contextual information
acts_idx    <- grep("^ACT [I|V]+", macbeth)
acts_labels <- str_extract(macbeth[acts_idx], "^ACT [I|V]+")
acts        <- data.frame(line = acts_idx, labels = acts_labels)

windows()
ggplot(data = speaker_freq, aes(x = line, y = speak)) +
        geom_smooth(aes(color = character), method = "loess", se = 0, span = 0.4) +
        geom_vline(xintercept = acts_idx, color = "darkgray", lty = 3) +
        geom_text(data = acts, aes(y = 0.085, label = labels), hjust = "left", color = "darkgray") + 
        ylim(c(0, NA)) + xlab("Line Number") + 
        ylab("Proportion of Speeches")