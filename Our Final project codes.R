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

# Same as above for country
l.vowels$Country[l.vowels$Speaker %in% c("NS1", "NS2", "NS3")] <- "Native Korean"
l.vowels$Country[l.vowels$Speaker %in% c("NNS1", "NNS2", "NNS3")] <- "Non native Korean"
l.vowels$Country <- as.factor(l.vowels$Country)
levels(l.vowels$Country)

# Token counts
table(l.vowels$Speaker)

table(l.vowels$Vowel)

table(l.vowels$Speaker, l.vowels$Vowel)

table(l.vowels$Country, l.vowels$Speaker)

install.packages("plyr")
library(plyr)

# To get summary stats by country and by vowel, you can do something like this (though this may be more meaningful to do in raw Hz values, but in order to do that you'd want to split by gender):
summary.c.vowels <- ddply(l.vowels, .(Country, Vowel), summarise, N = length(F1), MeanF1 = mean(F1), SDF1 = sd(F1), SEF1 = sd(F1)/sqrt(length(F1)), MeanF2 = abs(mean(F2)), SDF2 = sd(F2), SEF2 = sd(F2)/sqrt(length(F2)))

summary.c.vowels

# Plot

# Plotting everything at once to look at outliers
vowelplot(l.vowels, color="vowels", label="vowels", xlim=c(4.5,-4.5), ylim=c(4.5,-4.5), leg=NA, size=.75)

# Lots of outliers, so should probably trim


# In order to plot by group, need to convince the program that "country" is really "speaker"
# Make lg.vowels, and put "group2" in the "Speaker" (first) column
lc.vowels <- l.vowels
head(lc.vowels)
lc.vowels$SpeakerNum <- lc.vowels[,1]
head(lc.vowels)
# lc.vowels$Speaker <- lc.vowels[,1]
# head(lc.vowels)
lc.vowels[,1] <- lc.vowels$Country
head(lc.vowels)

# Every individual token, by country
vowelplot(lc.vowels, color="vowels", speaker="Native Korean", label="vowels", xlim=c(4.5,-4.5), ylim=c(4.5,-4.5), title="Native Korean speakers", size=.75)
add.spread.vowelplot(lc.vowels[,1:7], speaker="Native Korean", sd.mult=1, ellipsis=T, color="vowels")

vowelplot(lc.vowels, color="vowels", speaker="Non native Korean", label="vowels", xlim=c(4.5,-4.5), ylim=c(4.5,-4.5), leg=NA, title="Non-native Korean speakers", size=.75)
add.spread.vowelplot(lc.vowels[,1:7], speaker="Non native Korean", sd.mult=1, ellipsis=T, color="vowels")

# Plotting just the means, by country

# Another way to summarize the data
nk.means <- compute.means(lc.vowels[,1:7], speaker="Native Korean")
nnk.means <- compute.means(lc.vowels[,1:7], speaker="Non native Korean")
both.means <- rbind(nk.means, nnk.means)

vowelplot(nk.means, color="vowels", label="vowels", xlim=c(3, -3), ylim=c(3, -3), title="Native Korean speakers", leg=NA, size=1.75)
add.spread.vowelplot(lc.vowels[,1:7], sd.mult=1, ellipsis=T, color="vowels", speaker="Native Korean")

vowelplot(nnk.means, color="vowels", label="vowels", xlim=c(3, -3), ylim=c(3, -3), title="Non-native Korean speakers", leg=NA, size=1.75)
add.spread.vowelplot(lc.vowels[,1:7], sd.mult=1, ellipsis=T, color="vowels", speaker="Non native Korean")

# Plotting means for both countries on one plot
vowelplot(both.means, color="speakers", label="vowels", xlim=c(3,-3), ylim=c(3,-3), title="Both groups of speakers", size=1.75)
add.spread.vowelplot(lc.vowels[,1:7], sd.mult=1, ellipsis=T, color="speakers")



