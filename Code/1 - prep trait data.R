#STEP 1: CALCULATE PROPORTIONAL ABUNDANCES FOR EACH TRAIT


#load necessary packages
library(car)
library(lattice)
library(mgcv)
library(RColorBrewer)
library(dplyr)

#load traits time series
load("Data/traits_time_series_9fam.rda")
traits_time_series <- traits_time_series_9fam

#load tsa lagged 4 year with all covariates
load("Data/tsalag4_update_covariates.rda")


##########################################################################################################################################
################################################################## SIZE ##################################################################

#fix factors 
traits_time_series$year <- as.factor(traits_time_series$year)
traits_time_series$Reef <- as.factor(traits_time_series$Reef)
traits_time_series$count <- as.numeric(traits_time_series$count)
tsalag4$year <- as.factor(tsalag4$year)

#organise data so that you have abundance of each association class per transect per year 
size <- traits_time_series %>% group_by(Size, year, Reef) %>%
  summarize(abundance = sum(count, na.rm = TRUE))
year <- traits_time_series[,c(4,5,7)]
year <- unique(year[,c('year','Reef', "time_since_closure")])
size <- merge.data.frame(size, year)


### merge tsalag4 and size
size <- merge.data.frame(size, tsalag4, by = c("year", "Reef"))

#remove rows where size is na
size <- filter(size, !is.na(Size))

#get relative abundance of each size group per site
total.ab <- size %>% group_by(year, Reef) %>% summarize(total.ab = sum(abundance))
size <- merge.data.frame(size, total.ab)
size$relative.ab <- size$abundance/size$total.ab

#Fix column labels
size$time_since_closure.y <- NULL
size<- size %>% rename(time_since_closure = time_since_closure.x)

#fix levels
size$Size <- factor(size$Size, levels = c(1,2,3,4,5,6))

#transform
size$logit <- logit(size$relative.ab)

#only keep malindi
size <- filter(size, Reef2 != "Malindi")



##########################################################################################################################################
################################################################## DIET ##################################################################
#organise data so that you have abundance of each association class per transect per year 
diet <- traits_time_series %>% group_by(Diet, year, Reef) %>%
  summarize(abundance = sum(count, na.rm = TRUE))
year <- traits_time_series[,c(4,5,7)]
year <- unique(year[,c('year','Reef', "time_since_closure")])
diet <- merge.data.frame(diet, year)


### merge tsalag4 and diet
diet <- merge.data.frame(diet, tsalag4, by = c("year", "Reef"))

#remove rows where diet is na
diet <- filter(diet, !is.na(Diet))

#get relative abundance of each diet group per site
total.ab <- diet %>% group_by(year, Reef) %>% summarize(total.ab = sum(abundance))
diet <- merge.data.frame(diet, total.ab)
diet$relative.ab <- diet$abundance/diet$total.ab

#Fix column labels
diet$time_since_closure.y <- NULL
diet<- diet %>% rename(time_since_closure = time_since_closure.x)

#fix levels
diet$Diet <- factor(diet$Diet, ordered = FALSE)

#transform
diet$logit <- logit(diet$relative.ab)

#only keep malindi
diet <- filter(diet, Reef2 != "Malindi")




##########################################################################################################################################
######################################################### POSITION IN THE WATER COLUMN ###########################################################
#organise data so that you have abundance of each association class per transect per year 
position <- traits_time_series %>% group_by(Position, year, Reef) %>%
  summarize(abundance = sum(count, na.rm = TRUE))
year <- traits_time_series[,c(4,5,7)]
year <- unique(year[,c('year','Reef', "time_since_closure")])
position <- merge.data.frame(position, year)


### merge tsalag4 and position
position <- merge.data.frame(position, tsalag4, by = c("year", "Reef"))

#remove rows where position is na
position <- filter(position, !is.na(Position))

#get relative abundance of each position group per site
total.ab <- position %>% group_by(year, Reef) %>% summarize(total.ab = sum(abundance))
position <- merge.data.frame(position, total.ab)
position$relative.ab <- position$abundance/position$total.ab

#Fix column labels
position$time_since_closure.y <- NULL
position<- position %>% rename(time_since_closure = time_since_closure.x)

#fix levels
position$Position <- factor(position$Position, ordered = FALSE)

#transform
position$logit <- logit(position$relative.ab)

#only keep malindi
position <- filter(position, Reef2 != "Malindi")



##########################################################################################################################################
################################################################## SCHOOLING ################################################################
#organise data so that you have abundance of each association class per transect per year 
schooling <- traits_time_series %>% group_by(Schooling, year, Reef) %>%
  summarize(abundance = sum(count, na.rm = TRUE))
year <- traits_time_series[,c(4,5,7)]
year <- unique(year[,c('year','Reef', "time_since_closure")])
schooling <- merge.data.frame(schooling, year)


### merge tsalag4 and schooling
schooling <- merge.data.frame(schooling, tsalag4, by = c("year", "Reef"))

#remove rows where schooling is na
schooling <- filter(schooling, !is.na(Schooling))

#get relative abundance of each schooling group per site
total.ab <- schooling %>% group_by(year, Reef) %>% summarize(total.ab = sum(abundance))
schooling <- merge.data.frame(schooling, total.ab)
schooling$relative.ab <- schooling$abundance/schooling$total.ab

#Fix column labels
schooling$time_since_closure.y <- NULL
schooling<- schooling %>% rename(time_since_closure = time_since_closure.x)

#fix levels
schooling$Schooling <- factor(schooling$Schooling, levels = c(1,2,3,4,5))

#transform
schooling$logit <- logit(schooling$relative.ab)

#only keep malindi
schooling <- filter(schooling, Reef2 != "Malindi")



##########################################################################################################################################
########################################################### REEF ASSOCIATION #############################################################


#organise data so that you have abundance of each association class per transect per year 
association <- traits_time_series %>% group_by(Reef.Association, year, Reef) %>%
                     summarize(abundance = sum(count, na.rm = TRUE))
year <- traits_time_series[,c(4,5,7)]
year <- unique(year[,c('year','Reef', "time_since_closure")])
association <- merge.data.frame(association, year)


### merge tsalag4 and association
association <- merge.data.frame(association, tsalag4, by = c("year", "Reef"))

#remove rows where association is na
association <- filter(association, !is.na(Reef.Association))

#get relative abundance of each association group per site
total.ab <- association %>% group_by(year, Reef) %>% summarize(total.ab = sum(abundance))
association <- merge.data.frame(association, total.ab)
association$relative.ab <- association$abundance/association$total.ab

#Fix column labels
association$time_since_closure.y <- NULL
association<- association %>% rename(time_since_closure = time_since_closure.x)

#fix levels
association$Reef.Association <- factor(association$Reef.Association, levels = c("Low", "Med", "High"))

#transform
association$logit <- logit(association$relative.ab)

#only keep malindi
association <- filter(association, Reef2 != "Malindi")




##########################################################################################################################################
######################################################### LENGTH AT MATURITY #############################################################
load("Data/tsalag4_with_chl.rda")


traits_time_series <- traits_time_series[!is.na(traits_time_series$count),]
lm <- traits_time_series %>% group_by(year, Reef) %>% summarize(mean.lm = weighted.mean(Lm, count))


year <- traits_time_series[,c(4,5,7)]
year <- unique(year[,c('year','Reef', "time_since_closure")])
lm <- merge.data.frame(lm, year)


### merge tsalag4 andlm
lm <- merge.data.frame(lm, tsalag4, by = c("year", "Reef"))


#remove rows where lm is na
lm <- filter(lm, !is.na(mean.lm))

#remove duplicated rows
lm <- lm %>% distinct()



