
###########################################################################
###########################################################################
######################## The Spotify Tracks Analysis ######################
###########################################################################
###########################################################################

###### Clear environment and load libraries
rm(list = ls())
library(ggplot2)
library(lme4)
library(stringr)
library(dplyr)
library(tidyverse)
library(car)
library(MASS)
library(nnet)
library(RColorBrewer)
library(wesanderson)
library(caret)
names(wes_palettes)

###### Load the data
data <- read.csv("./data.csv",header=TRUE,sep=',') # 169909 obs. of 19 variables
data$explicit <- factor(data$explicit)
data$key <- factor(data$key)
data$mode <- factor(data$mode)
data$acousticness <- data$acousticness*100
data$danceability <- data$danceability*100
data$energy <- data$energy*100
data$instrumentalness <- data$instrumentalness*100
data$speechiness <- data$speechiness*100
data$duration_s <- data$duration_ms/1000
data$loudness <- data$loudness*100
data$year_c <- data$year - 1921
data$year_c_fac <- cut(data$year_c, breaks = c(-Inf,9,19,29,39,49,59,69,79,89,Inf), 
                       labels = c('1921-1929','1930-1939','1940-1949','1950-1959','1960-1969',
                                  '1970-1979','1980-1989','1990-1999','2000-2009','2010-2020'), right = FALSE)
data$popularity_fac <- cut(data$popularity, breaks = c(-Inf, 1, 25, 50, 75, Inf), 
                     labels = c('Not Popular', 'Less Popular', 'Somewhat Popular', 'More Popular', 'Popular'), right = FALSE)
table(data$popularity_fac)

###### View properties of the data  
head(data)
dim(data)
str(data)

data%>% 
  filter_all(any_vars(is.na(.))) # no NA rows

set.seed(1234) 


## Distribution of popularity -- positive skewed
ggplot(data,aes(popularity)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(15),bins=15) + theme(legend.position="none") +
  geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Popularity",y="Popularity") + theme_classic()

# drop rows with 0 popularity (likely due to Not Popular) -- 142552 obs. of 19 variables
data <- data[data$popularity != 0,]


###### Exploratory data analysis #####

## Audio Features ##

# acousticness
# Acousticness: This value describes how acoustic a song is. A score of 1.0 means the song is most likely to be an acoustic one.
# more acousticness, less popular
ggplot(data,aes(x=popularity_fac, y=acousticness, fill=popularity_fac)) +
  geom_boxplot() + scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  coord_flip() + theme_classic()

# Danceability: “Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable”.
# higher danceability score, more popular 
ggplot(data,aes(x=popularity_fac, y=danceability, fill=popularity_fac)) +
  geom_boxplot() + scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  coord_flip() + theme_classic()

# Energy: “(energy) represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy”.
# higher energy score, more popular 
ggplot(data,aes(x=popularity_fac, y=energy, fill=popularity_fac)) +
  geom_boxplot() + scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  coord_flip() + theme_classic()

# Instrumentalness: This value represents the amount of vocals in the song. The closer it is to 1.0, the more instrumental the song is.
# more instrumental, less popular -- interesting but also makes sense?
# higher energy score, more popular 
ggplot(data,aes(x=popularity_fac, y=instrumentalness, fill=popularity_fac)) +
  geom_boxplot() + scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  coord_flip() + theme_classic()

# This value describes the probability that the song was recorded with a live audience. According to the official documentation “a value above 0.8 provides strong likelihood that the track is live”.
# higher liveness score, less popular --> makes sense, usually lower quality
ggplot(data,aes(x=popularity_fac, y=liveness, fill=popularity_fac)) +
  geom_boxplot() + scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  coord_flip() + theme_classic()

ggplot(data,aes(x=speechiness, y=popularity)) +
  geom_point() + geom_smooth(method='lm',col='red3') + theme_classic()
# Speechiness: “Speechiness detects the presence of spoken words in a track”. If the speechiness of a song is above 0.66, it is probably made of spoken words, a score between 0.33 and 0.66 is a song that may contain both music and words, and a score below 0.33 means the song does not have any speech.
# slightly higher popularity for songs with spoken words, but not enough data when speechiness goes over 0.5
ggplot(data,aes(x=popularity_fac, y=speechiness, fill=popularity_fac)) +
  geom_boxplot() + scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  coord_flip() + theme_classic()

ggplot(data,aes(x=valence, y=popularity)) +
  geom_point() + geom_smooth(method='lm',col='red3') + theme_classic()
# A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry)”
# popularity gets lower slightly towards more positive songs but the trend is not distinguishable
ggplot(data,aes(x=popularity_fac, y=speechiness, fill=popularity_fac)) +
  geom_boxplot() + scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  coord_flip() + theme_classic()

### Song Characteristics ###
ggplot(data,aes(x=duration_ms, y=popularity)) +
  geom_point(shape = 20, color = "#E7B800", size = 2) + 
  geom_smooth(method='lm',col='red3') + theme(panel.background = element_rect(fill = "grey15"))
# longer duration, more popular, but also not enough points for longer duration songs
ggplot(data,aes(x=popularity_fac, y=duration_ms, fill=popularity_fac)) +
  geom_boxplot() + scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  coord_flip() + theme_classic()

ggplot(data,aes(x=popularity_fac, y=duration_s, fill=popularity_fac)) +
  geom_boxplot() + scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  coord_flip() + theme_classic()
ggplot(data,aes(x=duration_s, y=popularity)) +
  geom_point(shape = 20, color = "#E7B800", size = 2) + 
  geom_smooth(method='lm',col='red3') + theme(panel.background = element_rect(fill = "grey15"))



ggplot(data,aes(x=key, y=popularity, fill=key)) +
  geom_boxplot()  + theme_classic() + 
  scale_fill_manual(values=c("#F3DF6C", "#CEAB07", "#D5D5D3", "#24281A", "#798E87", "#C27D38", "#CCC591", "#29211F","#446455", "#FDD262", "#D3DDDC", "#C7B19C"))
# All keys on octave encoded as values ranging from 0 to 11, starting on C as 0, C# as 1 and so on…
# no obvious pattern 

ggplot(data,aes(x=popularity_fac, y=loudness, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot()  + theme_classic() + coord_flip()
# Float typically ranging from -60 to 0
# Loudness - LUFS (Loudness Unit Full Scale) -- a way to master average volume rather than peak volume
# peak volume -- loudness is 0dB full scale, average volume - measured over a period of time
# overall louder the average volume, more popular

ggplot(data,aes(x=mode, y=popularity, fill=mode)) +
  scale_fill_manual(values = wes_palette("Darjeeling2")) +
  geom_boxplot()  + theme_classic()
# about the same median and distribution for major and minor songs

ggplot(data,aes(x=popularity_fac, y=tempo, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() + coord_flip()
# Float typically ranging from 50 to 150
# in general, higher popularity for faster tempo songs


### Other Features ###
ggplot(data,aes(x=explicit, y=popularity, fill=explicit)) +
  scale_fill_manual(values = wes_palette("Royal1")) +
  geom_boxplot() + theme_classic()
# explicit --> more popular

ggplot(data,aes(x=year, y=popularity)) +
  geom_point(shape = 20, color = "#E7B800", size = 2) + 
  geom_smooth(method='lm',col='red3') + theme(panel.background = element_rect(fill = "grey15"))
# Ranges from 1921 to 2020
# newer songs more popular, but this could be biased
ggplot(data,aes(x=explicit, y=year_c_fac, fill=explicit)) +
  scale_fill_manual(values = wes_palette("Royal1")) +
  geom_boxplot()  + theme_classic() 



### Interactions ###
ggplot(data,aes(x=acousticness, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() + facet_wrap(~explicit,ncol=1)
ggplot(data,aes(x=acousticness, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() + facet_wrap(~year_fac,ncol=2)

ggplot(data,aes(x=danceability, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() + facet_wrap(~explicit,ncol=1)
ggplot(data,aes(x=danceability, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() + facet_wrap(~year_fac,ncol=2)

ggplot(data,aes(x=energy, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() + facet_wrap(~explicit,ncol=1)
ggplot(data,aes(x=energy, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() + facet_wrap(~year_fac,ncol=2)

ggplot(data,aes(x=duration_ms, y=popularity, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() + facet_wrap(~explicit)

ggplot(data,aes(x=instrumentalness, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() + facet_wrap(~explicit,ncol=1) # makes sense, instrumentalness and explicit kind of the opposite

ggplot(data,aes(x=valence, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() + facet_wrap(~explicit,ncol=1) 

ggplot(data,aes(x=tempo, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() + facet_wrap(~explicit,ncol=1)

ggplot(data,aes(x=liveness, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() + facet_wrap(~explicit,ncol=1)

ggplot(data,aes(x=loudness, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() + facet_wrap(~explicit,ncol=1)

ggplot(data,aes(x=speechiness, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() + facet_wrap(~explicit,ncol=1)

ggplot(data,aes(x=year_c_fac, y=popularity, fill=popularity)) +
  #scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() + facet_wrap(~explicit,ncol=1)

ggplot(data,aes(x=mode, y=popularity, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() + facet_wrap(~explicit,ncol=1)

ggplot(data,aes(x=key, y=popularity, fill=popularity)) +
  geom_boxplot() + theme_classic() + facet_wrap(~explicit,ncol=1)




john <- data %>%
  filter(str_detect(data$artist, "John Legend"))

ggplot(john,aes(popularity)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(15),bins=15) + theme(legend.position="none") +
  geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Popularity",y="Popularity") + theme_classic()

ggplot(john,aes(x=year, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() 

ggplot(john,aes(x=danceability, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic()

ggplot(john,aes(x=acousticness, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic()

ggplot(john,aes(x=energy, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic()

ggplot(john,aes(x=valence, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic()




mj <- data %>%
  filter(str_detect(data$artist, "Michael Jackson"))

ggplot(mj,aes(popularity)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(15),bins=15) + theme(legend.position="none") +
  geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Popularity",y="Popularity") + theme_classic()

ggplot(mj,aes(x=year, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() 

ggplot(mj,aes(x=danceability, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic()

ggplot(mj,aes(x=acousticness, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic()

ggplot(mj,aes(x=energy, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic()

ggplot(mj,aes(x=valence, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic()




beatles <- data %>%
  filter(str_detect(data$artist, "Beatles"))

ggplot(beatles,aes(popularity)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(15),bins=15) + theme(legend.position="none") +
  geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Popularity",y="Popularity") + theme_classic()

ggplot(beatles,aes(x=year, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic() 

ggplot(beatles,aes(x=danceability, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic()

ggplot(beatles,aes(x=acousticness, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic()

ggplot(beatles,aes(x=energy, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic()

ggplot(beatles,aes(x=valence, y=popularity_fac, fill=popularity_fac)) +
  scale_fill_manual(values = wes_palette("IsleofDogs2")) +
  geom_boxplot() + theme_classic()



################### Proportional Odds Model ##################
###### Model fitting
summary(model_polr <- polr(popularity_fac ~ acousticness + danceability + energy + 
                          instrumentalness + speechiness + duration_s + loudness + 
                          year_c_fac, na.action = na.omit, data=data, method="logistic"))
#Residual Deviance: 249307.68; AIC: 249347.68

###### Deviance test
#anova(model_null, model_polr, test = "Chisq") 








################### Multinomial Logistic Model ##################
summary(model_mlm <- multinom(popularity_fac ~ acousticness + danceability + energy + 
                          instrumentalness + speechiness + duration_s + loudness + explicit +
                          year_c_fac, na.action = na.omit, data=data, maxit = 200))
# Residual Deviance: 239806.5  ;AIC:  239950.5 

#confint(model_mlm) 
#exp(confint(model_mlm))



### Model Assessment
predprobs <- fitted(model_mlm) 
head(predprobs)

rawresid1 <- (data$popularity_fac == "Less Popular") -  predprobs[,2] 
rawresid2 <- (data$popularity_fac == "Somewhat Popular") -  predprobs[,3] 
rawresid3 <- (data$popularity_fac == "More Popular") -  predprobs[,4] 
rawresid4 <- (data$popularity_fac == "Popular") -  predprobs[,5] 

## binned plots for continuous variables
#make a 2 by 2 graphical display
par(mfcol = c(2,2))
# binned residuals plots (for overall)
binnedplot(x=predprobs[,2],y=rawresid1,xlab="Pred. Probabilities",
           col.int="darkorange",ylab="Ave. Residuals",main="Binned Residuals Plot - Less Popular",col.pts="dodgerblue")
binnedplot(x=predprobs[,3],y=rawresid2,xlab="Pred. Probabilities",
           col.int="darkorange",ylab="Ave. Residuals",main="Binned Residuals Plot - Somewhat Popular",col.pts="dodgerblue")
binnedplot(x=predprobs[,4],y=rawresid3,xlab="Pred. Probabilities",
           col.int="darkorange",ylab="Ave. Residuals",main="Binned Residuals Plot - More Popular",col.pts="dodgerblue")
binnedplot(x=predprobs[,5],y=rawresid4,xlab="Pred. Probabilities",
           col.int="darkorange",ylab="Ave. Residuals",main="Binned Residuals Plot - Popular",col.pts="dodgerblue")



## Accuracy
pred_classes <- predict(model_mlm)
Conf_mat <- confusionMatrix(as.factor(pred_classes),as.factor(data$popularity_fac))
Conf_mat$table # 4 x 4 matrix because we are predicting 4 levels, only accurate on the diagonal

Conf_mat$overall["Accuracy"]; #only 69% because it's hard to do across multiple groups
kable(Conf_mat$byClass[,c("Sensitivity","Specificity")])

## Individual ROC curves for the different levels
#here we basically treat each level as a standalone level (i.e. whether or not it's level 1 vs not level 1 ...etc.)
par(mfcol = c(2,2))
roc((data$popularity_fac == "Less Popular"),predprobs[,2],plot=T,print.thres="best",legacy.axes=T,print.auc =T,
    col="red3",percent=T,main="Less Popular")
roc((data$popularity_fac == "Somewhat Popular"),predprobs[,3],plot=T,print.thres="best",legacy.axes=T,print.auc =T,
    col="gray3",percent=T,main="Somewhat Popular")
roc((data$popularity_fac == "More Popular"),predprobs[,4],plot=T,print.thres="best",legacy.axes=T,print.auc =T,
    col="green3",percent=T,main="More Popular")
roc((data$popularity_fac == "Popular"),predprobs[,5],plot=T,print.thres="best",legacy.axes=T,print.auc =T,
    col="blue3",percent=T,main="Popular")




### Model Interpretation
tidy(model_mlm, exponentiate = FALSE, conf.int = TRUE) %>%
  kable(digits = 4, format = "markdown")
#duration_s is not significant at 2 levels














