install.packages("rio")
install.packages("here")
install.packages("vowels")

library(vowels)
library(here)
library(rio)

install.packages("languageR")
library(languageR)

# Read in NK & NNK data/import data set
nk <- import(here("data", "NK.xls"))
nnk <- import(here("data", "NNK.xls"))

# Putting both datasets together into one (I realized we have to do this to normalize... we can separate the groups back out later...)
head(nk)
head(nnk)
dim(nk)
dim(nnk)
vowels <- rbind(nk, nnk)

# Vowels is now both sets of data
dim(vowels)

# Normalize all vowels (Lobanov method), and put those values into a new dataframe called l.vowels
l.vowels <- norm.lobanov(vowels)

# Vowels is now both sets of data
dim(vowels)

# Normalize all vowels (Lobanov method), and put those values into a new dataframe called l.vowels
l.vowels <- norm.lobanov(vowels)

# Note that this changes the headings
head(l.vowels)


# The star in the headings is weird -- rename F*1 & F*2 columns
l.vowels$F1 <- l.vowels[,4]
l.vowels$F2 <- l.vowels[,5]

# Make Speaker and Vowel into factors
l.vowels$Speaker <- as.factor(l.vowels$Speaker)
l.vowels$Vowel <- as.factor(l.vowels$Vowel)


# How many speakers do we have?
levels(l.vowels$Speaker)

# Now, make a column denoting which country the speaker is from
l.vowels$Country <- NA
