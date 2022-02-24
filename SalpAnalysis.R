##For Salp morphology comparison: Based on ANCOVA discussion at:
###http://r-eco-evo.blogspot.com/2011/08/comparing-two-regression-slopes-by.html

#Load required packages
library(tidyverse)
library(readr)
install.packages("wesanderson")
library("wesanderson")

#Import data (this works if the spreadsheet is a csv saved in the the Rproject)
SalpDummy <- read_csv("SalpDummy.csv")
#have a peek
head(SalpDummy)

#Make the model (read this as DropTime as a function of Number, with Morpholophy as categories)
model1 <- aov(Droptime~Number*Morphology, data=SalpDummy)
summary(model1)

# alternate model
model2 <- aov(Droptime~Number+Morphology, data=SalpDummy)
summary(model2)

#Compare the models
anova(model1,model2)
##interpretation notes- the two models are NOT significantly different, so the more 
##'parsimonious' explanation is that Morphology is not that important (ie, number has an effect on Droptime, 
##'# slope of that effect is similar for wheels and chains)
##'
#Make some graphs
wheels <- subset(SalpDummy, Morphology=="Wheel")
chains <- subset(SalpDummy, Morphology=="Chain")

#Separate regression models for plotting
regress1 <- lm(Droptime~Number, data=wheels); summary(regress1)
regress2 <- lm(Droptime~Number, data=chains); summary(regress2)

#Making the plots in base R
plot(Droptime~Number, data=SalpDummy, type='n')
points(wheels$Number, wheels$Droptime, pch=20)
points(chains$Number, chains$Droptime, pch=1)
abline(regress1, lty=1)
abline(regress2, lty=2)
legend("bottomright", c("Wheel","Chain"), lty=c(1,2), pch=c(20,1))


##I like ggplots better, so this is my attempt at that.
ggplt <- ggplot(SalpDummy,aes(x=Number,y=Droptime,shape=Morphology))+
  geom_point()
  #theme_classic()

ggplt
ggplt+geom_smooth(method=lm,se=TRUE,fullrange=TRUE,
                  aes(color=Morphology))+
ggtitle("Descent time as a function of salp number \nfor different aggregate morphologies") +
  xlab("Number of individuals") + ylab("Descent time (s)")+
  scale_color_manual(values= wes_palette("Darjeeling1", n = 3))

##Dang! That was WAY easier.

#Lets make an individual estimate!
individuals <- rep("Individual(est)", 10)
individuals
indNum <- c(1:10)
indDrop <- c(12.5*indNum)
Indiv.df <- as_tibble(individuals, indNum,indDrop)
Indiv.df