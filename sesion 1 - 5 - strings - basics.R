#-------------------------------------------------------------------------------

suppressMessages(suppressWarnings(library(dplyr)))        # Wrangling: mutate().
suppressMessages(suppressWarnings(library(stringi)))      # The string concat operator %s+%.
suppressMessages(suppressWarnings(library(stringr)))      # String manipulation. tidyverse.
suppressMessages(suppressWarnings(library(glue)))         # Format strings.
suppressMessages(suppressWarnings(library(magrittr)))     # Pipelines for data processing: %>% %T>% %<>%.
suppressMessages(suppressWarnings(library(rattle.data)))  # Weather dataset.
suppressMessages(suppressWarnings(library(scales)))       # commas(), percent().

#-------------------------------------------------------------------------------

# %s+% operator

"abc" %s+% "def" %s+% "ghi"

c("abc", "def", "ghi", "jkl") %s+% c("mno")

c("abc", "def", "ghi", "jkl") %s+% c("mno", "pqr")

c("abc", "def", "ghi", "jkl") %s+% c("mno", "pqr", "stu", "vwx")

#-------------------------------------------------------------------------------

# functions to concatenate text

str_c("hello", "world")

str_c("hello", "world", sep=" ")

glue("hello", "world")

cat("hello", "world")

cat("hello", 123, "world")

paste("hello", "world")

#-------------------------------------------------------------------------------

# NULL behavior

"hello" %s+% NULL %s+% "world"

str_c("hello", NULL, "world")

glue("hello", NULL, "world")

cat("hello", NULL, "world")

paste("hello", NULL, "world")

#-------------------------------------------------------------------------------

# NA behavior

"hello" %s+% NA %s+% "world"

str_c("hello", NA, "world")

glue("hello", NA, "world")

cat("hello", NA, "world")

paste("hello", NA, "world")

#-------------------------------------------------------------------------------

# number of characters

str_length("hello world")

str_length(c("hello", "world"))

str_length(NULL)

str_length(NA)

nchar("hello world")

nchar(c("hello", "world"))

nchar(NULL)

nchar(NA)

#-------------------------------------------------------------------------------

# lowercase and uppercase

toupper("String Manipulation")

tolower("String Manipulation")

casefold("String Manipulation", upper=TRUE)

casefold("String Manipulation")

#-------------------------------------------------------------------------------

# string manipulation

s <- "string manipulation"

str_sub(s, start=3, end=6)

str_sub(s, 1, -8)

str_sub(s, 1, -8) <- "stip"

s

s <- "string manipulation"

substr(s, start=3, stop=6)

substr(s, 1, 12) <- "stip"

s

s <- "string manipulation"

substring(s, first=3, last=6)

v <- c("string", "manipulation", "always", "fascinating")

str_sub(v, -4, -1)

str_sub(v, -4, -1) <- "RING"

v

x <- c("abcd", "aabcb", "babcc", "cabcd")

substring(x, 2, 4)

substring(x, 2, 4) <- "AB"

x

#-------------------------------------------------------------------------------

# white space

ws <- c(" abc", "def ", " ghi ")

str_trim(ws)

str_trim(ws, side="left")

str_trim(ws, side="right")

str_trim(ws, side="both")

str_pad("abc", width=7, side="left")

str_pad("abc", width=7, side="right")

str_pad("abc", width=7, side="both", pad="#")

#-------------------------------------------------------------------------------

# phrases manipulation

st <- "All the Worlds a stage, All men are merely players"

cat(str_wrap(st, width=25))

st <- c("The quick brown fox", "jumps on the brown dog")

word(st, start=2, end=3)

word(st, start=1, end=-2)

#-------------------------------------------------------------------------------

# comments

dsname <- "weatherAUS"
nobs   <- nrow(weatherAUS)
starts <- min(weatherAUS$Date)

glue("The {dsname} dataset",
     " has just less than {comma(nobs + 1)} observations,",
     " starting from {format(starts, '%d %B %Y')}.")

glue("
     The {dsname} dataset has just
     less than {comma(nobs + 1)} observations
     starting from {format(starts, '%d %B %Y')}.
     ")

glue("
     The {dsname} dataset has just
         less than {comma(nobs + 1)} observations
     starting from {format(starts, '%d %B %Y')}.
     ",
     dsname = "weather",
     nobs   = nrow(weather),
     starts = min(weather$Date))

weatherAUS %>%
        sample_n(6) %>%
        glue_data("Observation",
                  " {rownames(.) %>% as.integer() %>% comma() %>% sprintf('%7s', .)}",
                  " location {Location %>% sprintf('%-14s', .)}",
                  " max temp {MaxTemp %>% sprintf('%5.1f', .)}")

weatherAUS %>%
        sample_n(6) %>%
        mutate(TempRange = glue("{MinTemp}-{MaxTemp}")) %>%
        glue_data("Observed temperature range at {Location} of {TempRange}")

glue("
    A formatted string \\
    can also be on a \\
    single line
    ")

name <- "Fred"
glue("My name is {name}, not {{name}}.")

one <- "1"
glue("The value of $e^{2\\pi i}$ is $<<one>>$.", .open = "<<", .close = ">>")

#-------------------------------------------------------------------------------

# patterns

s <- c("hands", "data", "on", "data$cience", "handsondata$cience", "handson")

grep(pattern="^data", s, value=TRUE)

grep(pattern="on$", s, value=TRUE)

grep(pattern="\\$", s, value=TRUE)

s <- c("aaab", "abb", "bc", "abbcd", "bbbc", "abab", "caa")

grep(pattern="ab*b", s, value=TRUE)

grep(pattern="abbc?", s, value=TRUE)

grep(pattern="b{2,}?", s, value=TRUE)

s <- c("abc12", "@#$", "345", "ABcd")

grep(pattern="[0-9]+", s, value=TRUE)

grep(pattern="[A-Z]+", s, value=TRUE)

grep(pattern="[^@#$]+", s, value=TRUE)

grep(pattern="[[:alpha:]]", s, value=TRUE)

grep(pattern="[[:upper:]]", s, value=TRUE)

#-------------------------------------------------------------------------------

# random text

stri_rand_lipsum(20)

stri_rand_lipsum(2)

sapply(stri_rand_lipsum(10), nchar, USE.NAMES=FALSE)

sapply(stri_rand_lipsum(10), nchar, USE.NAMES=FALSE)

#-------------------------------------------------------------------------------