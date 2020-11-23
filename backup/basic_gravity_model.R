#Just following along with this vignette: https://rpubs.com/adam_dennett/257231

library(sp)
library(MASS)
library(reshape2)
library(geojsonio)
library(rgdal)
library(downloader)
library(maptools)
library(broom) 
library(stplanr)
library(leaflet)
library(tidyverse)

#Grab district level boundaries for London
EW <- geojson_read("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson", what = "sp")

#  taking a look at the data portion
head(EW@data)

#Pulling out just London
London <- EW[grep("^E09",EW@data$lad15cd),] #"grep()" nabs the indices of regex matches
plot(London) #taking a look
summary(London) #looking at the data

#Calculating the distance matrix
BNG = "+init=epsg:27700" #new projection - British National Grid
LondonBNG <- spTransform(London, BNG) #transforming data

LondonBNG <- LondonBNG[order(LondonBNG$lad15cd),] #order by Borough code

dist <- spDists(LondonBNG) #make the distance matrix - essentially the cost of movement between locations!
distPair <- melt(dist) #changing the df... similar to pivot!

#Bringing in commute data for London to test our model
#  these will help as models for the 'mass' of a given location - the attractiveness/emissiveness of the location
cdata <- read.csv("https://www.dropbox.com/s/7c1fi1txbvhdqby/LondonCommuting2001.csv?raw=1") #commutes
CodeLookup <- read.csv("https://www.dropbox.com/s/h8mpvnepdkwa1ac/CodeLookup.csv?raw=1") #lookup table for boroughs
popincome <- read.csv("https://www.dropbox.com/s/84z22a4wo3x2p86/popincome.csv?raw=1") #income data

#  merging the data
cdata$OrigCodeNew <- CodeLookup$NewCode[match(cdata$OrigCode, CodeLookup$OldCode)]
cdata$DestCodeNew <- CodeLookup$NewCode[match(cdata$DestCode, CodeLookup$OldCode)]
cdata$vi1_origpop <- popincome$pop[match(cdata$OrigCodeNew, popincome$code)]
cdata$vi2_origsal <- popincome$med_income[match(cdata$OrigCodeNew, popincome$code)]
cdata$wj1_destpop <- popincome$pop[match(cdata$DestCodeNew, popincome$code)]
cdata$wj2_destsal <- popincome$med_income[match(cdata$DestCodeNew, popincome$code)]

cdata <- arrange(cdata, OrigCodeNew, DestCodeNew) #ordering by borough code again

#  getting rid of intra-borough flows
cdata$TotalNoIntra <- ifelse(cdata$OrigCode == cdata$DestCode,0,cdata$Total)
cdata$offset <- ifelse(cdata$OrigCode == cdata$DestCode,0.0000000001,1) #slightly offsetting from 0 - important for model?

#  adding in dist data from earlier
cdata$dist <- distPair$value
View(cdata)

#Simplifying the data set down
toMatch<-c("00AA", "00AB", "00AC", "00AD", "00AE", "00AF", "00AG") #borough codes to keep

cdatasub <- cdata[grep(paste(toMatch,collapse = "|"), cdata$OrigCode),] #filtering out the boroughs that we want - origins
cdatasub <- cdatasub[grep(paste(toMatch,collapse = "|"), cdata$DestCode),] #now for destinations

cdatasub <- cdatasub[cdatasub$OrigCode!=cdatasub$DestCode,] #pulling out the intra-borough flows
cdatasub <- cdatasub[1:42,] #getting rid of empty cells

cdatasub <- dplyr::select(cdatasub, OrigCodeNew, DestCodeNew, Total, everything()) #reorder vars

#  pivoting out into a graph matrix kinda thing
cdatasubmat <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "Total", margins=c("Orig", "Dest"))
cdatasubmat

#Putting together the basic gravity model
#  check out description/explanation of model in the vignette - well explained!
mu <- 1 #picking arbitrary vals for the model params at first
alpha <- 1
beta <- -2
k <- 1
T2 <- sum(cdatasub$Total)

vi1_mu <- cdatasub$vi1_origpop^mu #estimating flows by applying the params to the vars
wj2_alpha <- cdatasub$wj2_destsal^alpha
dist_beta <- cdatasub$dist^beta
T1 <- vi1_mu*wj2_alpha*dist_beta
k <- T2/sum(T1)

#  applying the estimates - the model dictates that we should mult together for each i,j in the OD matrix
cdatasub$unconstrainedEst1 <- round(k*vi1_mu*wj2_alpha*dist_beta,0)
sum(cdatasub$unconstrainedEst1) #checking that total flows seems reasonable - should be similar to T2

#  the estimated OD matrix
cdatasubmat1 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "unconstrainedEst1", margins=c("Orig", "Dest"))
cdatasubmat1

#Defining goodness-of-fit measures
#  R-squared between cols not matrices...
CalcRSquared <- function(observed,estimated){ 
  r <- cor(observed,estimated)
  R2 <- r^2
  R2
}

#  root mean squared error - sqrt of mean of squared error
CalcRMSE <- function(observed,estimated){
  res <- (observed - estimated)^2
  RMSE <- sqrt(mean(res))
  RMSE
}

#Checking goodness of fit of initial model
CalcRSquared(cdatasub$Total,cdatasub$unconstrainedEst1) #want to be closer to 1
CalcRMSE(cdatasub$Total,cdatasub$unconstrainedEst1) #want to be closer to 0

#Estimating params rather than just picking them to improve performance
#  basically, we will estimate a Poisson reg model - a GLM
uncosim <- glm(Total ~ log(vi1_origpop)+log(wj2_destsal)+log(dist), #the actual model
               na.action = na.exclude, family = poisson(link = "log"), data = cdatasub)
summary(uncosim)

cdatasub$fitted <- fitted(uncosim) #grabbing the fitted vals from the model and putting back into df
cdatasub$fitted <- round(cdatasub$fitted,0) #rounding off
sum(cdatasub$fitted) #checking total flows

#  turning into an OD matrix one more time
cdatasubmat2 <- dcast(cdatasub, Orig ~ Dest, sum, value.var = "fitted", margins=c("Orig", "Dest"))
cdatasubmat2

#  checking goodness-of-fit... definitely does better at estimating flows than original model!
CalcRSquared(cdatasub$Total,cdatasub$fitted) 
CalcRMSE(cdatasub$Total,cdatasub$fitted)

