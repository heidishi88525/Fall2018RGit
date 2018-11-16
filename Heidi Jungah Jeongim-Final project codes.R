###CAM: I removed the code that installs the packages. Although it is more reproducible, I believe there is a faux pas about installing packages onto other people's computers. 

library(vowels)
library(here)
library(rio)
library(languageR)
library(tidyverse)###CAM: I moved the loading of tidyverse up here so all of the packages are together. 


#### [READ ME] ####
##This project aims to compare Korean native and non native vowel productions. In Section 1, we will first employ the required functions in the EDLD 610 final rubic. Afterwards, in Section 2, we will use some linguistic R packages/functions to obtain our results for research purposes.

####[Note]####
##F1 (vowel height) and F2(vowel backness) are formant measurements in phonetics which indicate the vowel articulation status in a speaker's vocal cavity. They are phonetic terminologies and DIFFERENT variables, rather than the same variable labled by different numbers. 

####Section 1. The following functions are generated to meet the requirement of EDLD 610 final peoject (we used sumamrize() above).####

#l.vowels is the main dataframe we will work on, we rename it into df
View(l.vowels) ###CAM: I couldn't seem to find where the l.vowels object was coming from. I would suggest moving the importing of l.vowels above this line so all of the code works from the start.

df <- l.vowels ###CAM: I added spaces around the assignment operator for legibility. I've also added spaces around assignment operators and equal signs in the rest of the document. It is just a stylistic thing. In some cases, there was a space between `filter` and the first paranthesis of the function. I removed those as well. Rstudio has a handy tool under `RStudio > Preferences > Code > Diagnostics > Provide R Style Diagnostics (e.g., Whitespace)`, which will atuomatically flag whitespace issues for you :)
View(df)

# clean column names
df <- df %>%
  janitor::clean_names() ###CAM: This would be a good idea to add; it changes all of the variable names to lowercase. 

# cleaning up df, removed NA & useless columns, call the new one td
td <- df[-3:-7] ###CAM: It might be good to use dplyr::select here. Instead of numerals, I would use the variable names for readability. You could use `select(speaker, vowel)`. 
View(td)



####gather####
gtd <- td %>% 
  gather(forments, value, -c(1:2, 5)) ###CAM: Added a space after the comma for readability. Gathering does not seem to be doing anything here. Since `td` only had two columns, and you are excluding the two columns, the result is the same as the input. 

  
View(gtd)   #works

####separate####
gtd %>% 
  separate(speaker, c("nativity", "number"), -1) ###CAM: Great use of the `sep = -1` argument 

####spread####
gtd %>% 
  tbl_df() %>% 
  group_by(vowel, forments) %>% ###CAM: Where does the `forments` variable come from?
  summarize(mean = mean(value, na.rm = TRUE)) %>% 
  spread(vowel, mean)

####select+ group_by+ summarize####
#check mean vowel height (F1) by each Vowel
f1vmean <- df %>% 
  select(speaker, vowel, f_1) %>% ###CAM: I believe you removed the F1s earlier; I think you should be using DF here instead
  group_by(vowel) %>% 
  summarize(vmeanf1 = mean(f_1)) 

f1vmean

#Plot mean F1 of each vowel
ggplot(f1vmean, aes(x = vowel, y = vmeanf1)) + ###CAM: added columns for readability. 
  geom_bar(stat = "identity", aes(fill = vowel), position = "dodge") +
  xlab("Vowel") + ylab("F1") +
  ggtitle("Mean F1 of Each Vowel") +
  theme_bw()

#check mean vowel backenss (F2) by each Vowel
f2vmean <- df %>%  ###CAM: Again, I think you should be using df here instead of td.
  select(speaker, vowel, f_2) %>% 
  group_by(vowel) %>% 
  summarize(vmeanf2 = mean(f_2))
f2vmean

#Plot mean F2 of each vowel
ggplot(f2vmean, aes(x = vowel, y = vmeanf2)) +
  geom_bar(stat = "identity", aes(fill = vowel), position = "dodge") +
  xlab("Vowel") + ylab("F2") +
  ggtitle("Mean F2 of Each Vowel") +
  theme_bw()

####filter+ group_by+ summarize####
#native F1 mean and sd of each vowel
f1n <- df %>% ###CAM: Again, I think you should be using df here instead of td.
  filter(Country == "Native Korean") %>%  ###CAM: I'm not sure where the `Country` variable came from.
  group_by(vowel) %>% 
  summarize(nmean_f1 = mean(f_1),
            nsd_f1 = sd(f_1))

f1n #native's mean F1 & sd of vowel i

#plot of native speakers' F1
td %>% ###CAM: Again, I think you should be using df here instead of td, but I do not know where `country` is coming from on the next line.
  filter(country == "Native Korean") %>% 
  group_by(vowel) %>% 
  ggplot(aes(x = vowel, y = f_1)) +
    geom_line(aes(group = vowel)) + ###CAM: I added indents here because I think it makes it easier to tell that the ggplot geoms are adding to the initial ggplot function call. 
    geom_point() +
    geom_boxplot() +
    xlab("Vowel") + ylab("F1") +
    ggtitle("F1 of Native Speakers") +
    theme_bw()

#non-native F1 mean and sd of each vowel
f1nn <- td %>% ###CAM: Again, I think you should be using df here instead of td, but I do not know where `country` is coming from on the next line.
  filter(country == "Non native Korean") %>% 
  group_by(vowel) %>% 
  summarize(nnmean_f1 = mean(f_1),
            nnsd_f1 = sd(f_1))

f1nn #native's mean F1 & sd of vowel i

#plot of non-native speakers' F1
td %>% ###CAM: Again, I think you should be using df here instead of td, but I do not know where `country` is coming from on the next line.
  filter(country == "Non native Korean") %>% 
  group_by(vowel) %>% 
  ggplot(aes(x = vowel, y = f_1)) +
  geom_line(aes(vroup = vowel)) +
  geom_point() +
  geom_boxplot() +
  xlab("Vowel") + ylab("F1") +
  ggtitle("F1 of Non-Native Speakers") +
  theme_bw()


####Section 2. (The following functions are for real research purposes) ####

# Read in NK（Native-Korean) & NNK（non-Native-Korean) data/import data set
nk <- import(here("data", "NK.xls"))
nnk <- import(here("data", "NNK.xls"))

# Putting both datasets together into one. To compare the vowel productions, we need to normalize the data, aiming to get rid of influential factors such as speaker body size, gender, etc. Vowel normalization is necessary because it makes the forments of different speaker comparable.  

View(nk) #CAM: I prefer not to use View in my scripts because it moves focus from the script you are working on to the data in a seperate tab. This can make it hard to quickly run through all of your code.
View(nnk)
dim(nk)  #72 rows (speech samples) of 3 native speakers' 3 repetitions of 8 vowels
dim(nnk) #72 rows (speech samples) of 3 non-native speakers' 3 repetitions of 8vowels

# Vowels contains both sets of data now.
vowels <- rbind(nk, nnk)
View(vowels)
dim(vowels)  #144 rows in total

# Normalize all vowels (Lobanov method), and put those values into a new dataframe called l.vowels
l.vowels <- norm.lobanov(vowels)
dim(l.vowels)

# Note that this changes the headings
View(l.vowels)

####Formatting####
#The following codes are based on a template to create Vowel charts, they may seem redundant, but... sorry

# The asterisks in the headings are weird -- rename the F*1 & F*2 columns
#l.vowels$F1 <- l.vowels[,4]
#l.vowels$F2 <- l.vowels[,5]
names(l.vowels) <- gsub("\\*", "", names(l.vowels)) ###CAM: Here is an alternative way of removing the asterisks from the column names. The benefit is that you do not have to create new varaibles. Essentially, gsub takes all matches of a pattern (in this case "*") and replaces it with a string (in this case nothing). The "\\" before the asterisk is so that "*" is not interpreted as a special character.


# Change Speaker and Vowel into factors
l.vowels$Speaker <- as.factor(l.vowels$Speaker)
l.vowels$Vowel <- as.factor(l.vowels$Vowel)

# How many speakers do we have?
levels(l.vowels$Speaker)  #the answer is 6, NS=3, NNS=3

# Now, make a column denoting which country the speaker is from
l.vowels$Country <- NA

# Same as above for country
l.vowels$Country[l.vowels$Speaker %in% c("NS1", "NS2", "NS3")] <- "Native Korean"
l.vowels$Country[l.vowels$Speaker %in% c("NNS1", "NNS2", "NNS3")] <- "Non native Korean"
l.vowels$Country <- as.factor(l.vowels$Country)
levels(l.vowels$Country)

# Token counts
table(l.vowels$Speaker) # each speaker has 3(repetitions)*8(vowels)=24 speech samples

table(l.vowels$Vowel) #each vowel is produced 3(repetitions)*6(speakers)=18 times

table(l.vowels$Speaker, l.vowels$Vowel) #3 repetition for each vowel, each speaker

table(l.vowels$Country, l.vowels$Speaker) #Native and Non-Native both have 3(repetitions)*8(vowels)=24 speech samples

install.packages("plyr")
library(plyr)

# To get summary stats by country and by vowel, [summarize] is used here!
summary.c.vowels <- ddply(l.vowels, .(Country, Vowel), summarise, N = length(F1), MeanF1 = mean(F1), SDF1 = sd(F1), SEF1 = sd(F1)/sqrt(length(F1)), MeanF2 = abs(mean(F2)), SDF2 = sd(F2), SEF2 = sd(F2)/sqrt(length(F2)))

View(summary.c.vowels)

####Vowel Charts####

# Plotting everything at once to look at outliers
vowelplot(l.vowels, color = "vowels", label = "vowels", xlim = c(4.5,-4.5), ylim = c(4.5,-4.5), leg = NA, size = .75)

# Lots of outliers, so should probably trim

####Formatting#####
# In order to plot by group, need to convince the program that "country" is really "speaker"
# Make lg.vowels, and put "group2" in the "Speaker" (first) column

lc.vowels <- l.vowels
lc.vowels$SpeakerNum <- lc.vowels[,1] #adding a speakerNum column at the end
lc.vowels[,1] <- lc.vowels$Country #changing the "Speaker" column to "Country"
View(lc.vowels)

# Each individual's production, grouped by country
vowelplot(lc.vowels, color = "vowels", speaker = "Native Korean", label = "vowels", xlim = c(4.5,-4.5), ylim = c(4.5,-4.5), title = "Native Korean speakers", size = .75)

add.spread.vowelplot(lc.vowels[,1:7], speaker = "Native Korean", sd.mult = 1, ellipsis = T, color = "vowels")

vowelplot(lc.vowels, color = "vowels", speaker = "Non native Korean", label = "vowels", xlim = c(4.5,-4.5), ylim = c(4.5,-4.5), leg = NA, title = "Non-native Korean speakers", size = .75)

add.spread.vowelplot(lc.vowels[,1:7], speaker = "Non native Korean", sd.mult = 1, ellipsis = T, color = "vowels")


# Plotting just the mean values, grouped by country

# Another way to summarize the data
nk.means <- compute.means(lc.vowels[,1:7], speaker = "Native Korean")
nnk.means <- compute.means(lc.vowels[,1:7], speaker = "Non native Korean")
both.means <- rbind(nk.means, nnk.means)

vowelplot(nk.means, color = "vowels", label = "vowels", xlim = c(3, -3), ylim = c(3, -3), title = "Native Korean speakers", leg = NA, size = 1.75)
add.spread.vowelplot(lc.vowels[,1:7], sd.mult = 1, ellipsis = T, color = "vowels", speaker = "Native Korean")

vowelplot(nnk.means, color = "vowels", label = "vowels", xlim = c(3, -3), ylim = c(3, -3), title = "Non-native Korean speakers", leg = NA, size = 1.75)
add.spread.vowelplot(lc.vowels[,1:7], sd.mult = 1, ellipsis = T, color = "vowels", speaker = "Non native Korean")

# Plotting mean values for both countries on one plot
vowelplot(both.means, color = "speakers", label = "vowels", xlim = c(3,-3), ylim = c(3,-3), title = "Both groups of speakers", size = 1.75)
add.spread.vowelplot(lc.vowels[,1:7], sd.mult = 1, ellipsis = T, color = "speakers")


