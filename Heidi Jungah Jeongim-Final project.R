install.packages("rio")
install.packages("here")
install.packages("vowels")
install.packages("languageR")

library(vowels)
library(here)
library(rio)
library(languageR)


#### [READ ME] ####
##This project aims to compare Korean native and non native vowel productions. We will start with [Prep], aiming to prepare the dataframe and all the necessary settings. Afterwards, in [Section 1], we will employ the required functions in the EDLD 610 final rubic. Finally, in Section 2, we will use some linguistic R packages/functions to obtain our results for research purposes.

####[Prep]####

# Import NK（Native-Korean) & NNK（non-Native-Korean) datasets
nk <- import(here("data", "NK.xls"))
nnk <- import(here("data", "NNK.xls"))

# Putting both datasets together into one. To compare the vowel productions, we need to normalize the data, aiming to get rid of influential factors such as speaker body size, gender, etc. Vowel normalization is necessary because it makes the forments of different speaker comparable.  

View(nk)
View(nnk)
dim(nk)  #72 rows (speech samples) of 3 native speakers' 3 repetitions of 8 vowels
dim(nnk) #72 rows (speech samples) of 3 non-native speakers' 3 repetitions of 8vowels

# Vowels contains both sets of data now.
vowels <- rbind(nk, nnk) #IMPORTANT STEP
View(vowels)
dim(vowels)  #144 rows in total

# Normalize all vowels (Lobanov method), and put those values into a new dataframe called l.vowels
l.vowels <- norm.lobanov(vowels) #IMPORTANT STEP
dim(l.vowels)

# Note that this changes the headings
View(l.vowels)

####[Formatting1]
#The following codes are based on a template to create Vowel charts, they may seem redundant, but... sorry, we need this template

# The asterisks in the headings are weird -- rename the F*1 & F*2 columns
l.vowels$F1 <- l.vowels[,4]
l.vowels$F2 <- l.vowels[,5]

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


####Section 1. The following functions are generated to meet the requirement of EDLD 610 final peoject (we used sumamrize() above).####

#l.vowels is the main dataframe we will work on, we rename it into df
library(tidyverse)

View(l.vowels)
df<-l.vowels  #IMPORTANT STEP
View(df)

# Tidyverse df, removed NA & useless columns, call the new one td
td<- df[-3:-7]  #IMPORTANT STEP
View(td)

####gather####
gtd<-td %>% 
  gather(Forments,Value,-c(1:2,5))
View(gtd)   

####separate####
setd<-gtd %>% 
  separate(Speaker, c("nativity", "number"), -1)
View(setd)

####spread####
nsptd<-gtd %>% 
  tbl_df() %>% 
  group_by(Vowel, Forments) %>% 
  filter(Country=="Native Korean") %>% 
  summarize(mean = mean(Value, na.rm = TRUE)) %>% 
  spread(Vowel, mean)
View(nsptd)   #Native Speakers' mean F1&F2 for each vowel

nnsptd<-gtd %>% 
  tbl_df() %>% 
  group_by(Vowel, Forments) %>% 
  filter(Country=="Non native Korean") %>% 
  summarize(mean = mean(Value, na.rm = TRUE)) %>% 
  spread(Vowel, mean)
View(nnsptd)   #Non-native Speakers' mean F1&F2 for each vowel

####select+ group_by+ summarize####
##check mean vowel height (F1) by each Vowel

#Native speakers
n_F1vmean<-td %>% 
  filter(Country=="Native Korean") %>% 
  select(Speaker,Vowel,F1) %>% 
  group_by(Vowel) %>% 
  summarize(n_vmeanF1=mean(F1))
n_F1vmean   #Native Speakers' mean F1 for each vowel

####[Plot 1] NS mean F1 of each vowel####
ggplot(n_F1vmean, aes(x=Vowel,y = n_vmeanF1))+
  geom_bar(stat = "identity", aes(fill = Vowel), position = "dodge") +
  xlab("Vowel") + ylab("F1") +
  ggtitle("Native Speakers' Mean F1 of Each Vowel") +
  theme_bw()

#Non-Native speakers
nn_F1vmean<-td %>% 
  filter(Country=="Non native Korean") %>% 
  select(Speaker,Vowel,F1) %>% 
  group_by(Vowel) %>% 
  summarize(nn_vmeanF1=mean(F1))
nn_F1vmean   #Non-Native Speakers' mean F1 for each vowel

####[Plot 2] NNS mean F1 of each vowel####
ggplot(nn_F1vmean, aes(x=Vowel,y = nn_vmeanF1))+
  geom_bar(stat = "identity", aes(fill = Vowel), position = "dodge") +
  xlab("Vowel") + ylab("F1") +
  ggtitle("Non-Native Speakers' Mean F1 of Each Vowel") +
  theme_bw()

##check mean vowel backenss (F2) by each Vowel
#Native Speakers
n_F2vmean<-td %>% 
  filter(Country=="Native Korean") %>% 
  select(Speaker,Vowel,F2) %>% 
  group_by(Vowel) %>% 
  summarize(n_vmeanF2=mean(F2))
n_F2vmean #Native Speakers' mean F2 for each vowel

####[Plot 3] NS mean F2 of each vowel ####
ggplot(n_F2vmean, aes(x=Vowel,y = n_vmeanF2))+
  geom_bar(stat = "identity", aes(fill = Vowel), position = "dodge") +
  xlab("Vowel") + ylab("F2") +
  ggtitle("Native Speakers' Mean F2 of Each Vowel") +
  theme_bw() 

#Non-Native Speakers
nn_F2vmean<-td %>% 
  filter(Country=="Non native Korean") %>% 
  select(Speaker,Vowel,F2) %>% 
  group_by(Vowel) %>% 
  summarize(nn_vmeanF2=mean(F2))
nn_F2vmean #Non-Native Speakers' mean F2 for each vowel

####[Plot 4] NNS mean F2 of each vowel ####
ggplot(nn_F2vmean, aes(x=Vowel,y = nn_vmeanF2))+
  geom_bar(stat = "identity", aes(fill = Vowel), position = "dodge") +
  xlab("Vowel") + ylab("F2") +
  ggtitle("Non-Native Speakers' Mean F2 of Each Vowel") +
  theme_bw()

####filter+ group_by+ summarize####
#Now we switch to Mean and SD

#Native F1
F1n<-td %>%
  filter (Country=="Native Korean") %>% 
  group_by(Vowel) %>% 
  summarize(nmean_F1 = mean(F1),
            nsd_F1 = sd(F1))

F1n ##native's mean F1 & sd of all vowels

####[Plot5] Boxplot of NS F1 mean and sd ####
td %>% 
  filter (Country=="Native Korean") %>% 
  group_by(Vowel) %>% 
  ggplot(aes(x = Vowel, y = F1)) +
  geom_line(aes(group = Vowel)) +
  geom_point()+
  geom_boxplot()+
  xlab("Vowel") + ylab("F1") +
  ggtitle("F1 of Native Speakers") +
  theme_bw()

#Non-Native F1
F1nn<-td %>%
  filter (Country=="Non native Korean") %>% 
  group_by(Vowel) %>% 
  summarize(nnmean_F1 = mean(F1),
            nnsd_F1 = sd(F1))

F1nn #non-native's mean F1 & sd of all vowels

####[Plot6] Boxplot of NNS F1 mean and sd ####
td %>% 
  filter (Country=="Non native Korean") %>% 
  group_by(Vowel) %>% 
  ggplot(aes(x = Vowel, y = F1)) +
  geom_line(aes(group = Vowel)) +
  geom_point()+
  geom_boxplot()+
  xlab("Vowel") + ylab("F1") +
  ggtitle("F1 of Non-Native Speakers") +
  theme_bw()

#Native F2
F2n<-td %>%
  filter (Country=="Native Korean") %>% 
  group_by(Vowel) %>% 
  summarize(nmean_F2 = mean(F2),
            nsd_F1 = sd(F2))

F2n ##native's mean F1 & sd of all vowels

####[Plot7] Boxplot of NS F2 mean and sd ####
td %>% 
  filter (Country=="Native Korean") %>% 
  group_by(Vowel) %>% 
  ggplot(aes(x = Vowel, y = F1)) +
  geom_line(aes(group = Vowel)) +
  geom_point()+
  geom_boxplot()+
  xlab("Vowel") + ylab("F2") +
  ggtitle("F2 of Native Speakers") +
  theme_bw()

#Non-Native F2
F2nn<-td %>%
  filter (Country=="Non native Korean") %>% 
  group_by(Vowel) %>% 
  summarize(nnmean_F2 = mean(F2),
            nnsd_F2 = sd(F2))

F2nn #non-native's mean F2 & sd of all vowels

####[Plot8] Boxplot of NNS Fs mean and sd ####
td %>% 
  filter (Country=="Non native Korean") %>% 
  group_by(Vowel) %>% 
  ggplot(aes(x = Vowel, y = F2)) +
  geom_line(aes(group = Vowel)) +
  geom_point()+
  geom_boxplot()+
  xlab("Vowel") + ylab("F2") +
  ggtitle("F2 of Non-Native Speakers") +
  theme_bw()

####Section 2. (The following functions are for real research purposes) ####

# To get summary stats by country and by vowel

install.packages("plyr")
library(plyr)

summary.c.vowels <- ddply(l.vowels, .(Country, Vowel), summarise, N = length(F1), MeanF1 = mean(F1), SDF1 = sd(F1), SEF1 = sd(F1)/sqrt(length(F1)), MeanF2 = abs(mean(F2)), SDF2 = sd(F2), SEF2 = sd(F2)/sqrt(length(F2)))

View(summary.c.vowels)

####Vowel Charts####

####[Plot 9]Plotting everything at once to look at outliers####
vowelplot(l.vowels, color="vowels", label="vowels", xlim=c(4.5,-4.5), ylim=c(4.5,-4.5), leg=NA, size=.75)

# Lots of outliers, so should probably trim

####[Formatting 2]
# In order to plot by group, need to convince the program that "country" is really "speaker"
# Make lg.vowels, and put "group2" in the "Speaker" (first) column

lc.vowels <- l.vowels
lc.vowels$SpeakerNum <- lc.vowels[,1] #adding a speakerNum column at the end
lc.vowels[,1] <- lc.vowels$Country #changing the "Speaker" column to "Country"
View(lc.vowels)

####[Plot 10] Each native's production, grouped by country####
vowelplot(lc.vowels, color="vowels", speaker="Native Korean", label="vowels", xlim=c(4.5,-4.5), ylim=c(4.5,-4.5), title="Native Korean Speakers", size=.75)

add.spread.vowelplot(lc.vowels[,1:7], speaker="Native Korean", sd.mult=1, ellipsis=T, color="vowels")

####[Plot 11] Each non-native's production, grouped by country####
vowelplot(lc.vowels, color="vowels", speaker="Non native Korean", label="vowels", xlim=c(4.5,-4.5), ylim=c(4.5,-4.5), leg=NA, title="Non-Native Korean Speakers", size=.75)

add.spread.vowelplot(lc.vowels[,1:7], speaker="Non native Korean", sd.mult=1, ellipsis=T, color="vowels")


# Plotting just the mean values, grouped by country

# Another way to summarize the data
nk.means <- compute.means(lc.vowels[,1:7], speaker="Native Korean")
nnk.means <- compute.means(lc.vowels[,1:7], speaker="Non native Korean")
both.means <- rbind(nk.means, nnk.means)

####[Plot 12] NS Mean Value Vowel Chart####
vowelplot(nk.means, color="vowels", label="vowels", xlim=c(3, -3), ylim=c(3, -3), title="Native Korean speakers", leg=NA, size=1.75)
add.spread.vowelplot(lc.vowels[,1:7], sd.mult=1, ellipsis=T, color="vowels", speaker="Native Korean")

####[Plot 13] NNS Mean Value Vowel Chart####
vowelplot(nnk.means, color="vowels", label="vowels", xlim=c(3, -3), ylim=c(3, -3), title="Non-Native Korean Speakers", leg=NA, size=1.75)
add.spread.vowelplot(lc.vowels[,1:7], sd.mult=1, ellipsis=T, color="vowels", speaker="Non native Korean")

####[Plot 14] Comparative Mean Value Vowel Chart of both countries####
vowelplot(both.means, color="speakers", label="vowels", xlim=c(3,-3), ylim=c(3,-3), title="Comparison of Two Speaker Groups", size=1.75)
add.spread.vowelplot(lc.vowels[,1:7], sd.mult=1, ellipsis=T, color="speakers")


