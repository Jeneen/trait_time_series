# load tidy packages and set ggplot theme
library(tidyverse); theme_set(theme_bw()); library(gridExtra); library(vegan); 
library(cluster); library(ape);
library(splitstackshape); library(FD); library(ggrepel)
library(ggrepel); library(FD); library(fuzzySim)


#functions
'%!in%' <- function(x,y)!('%in%'(x,y))


#load data
load("Data/annual_tsa_mpa_max.rda") 
load(file = "Data/kenya.rda")
load("Data/traits_time_series_9fam.rda")
load("Data/abundance_av.rda")
abundance_av <- filter(abundance_m, Reef2 != "Malindi")


#fix data names
traits_time_series_9fam$Family.y <- NULL

#add in thermal stress anomaly
traits_tsa <- merge.data.frame(traits_time_series_9fam, tsa2, by.x = c("Reef2", "year"), by.y = c("Reef2", "year"), all = TRUE)

#get average counts of species at sites per year
traits_tsa$year <- as.factor(traits_tsa$year)
traits_tsa_na <- filter(traits_tsa, !is.na(traits_tsa$count))
traits_tsa_na$count <- as.numeric(traits_tsa_na$count)

average.counts <- aggregate(count ~ Species + year + Reef, FUN=mean, data=traits_tsa_na)
names(average.counts) <- c("Species", "year", "Reef", "av_count")


#remove transect column (keep spp, year, traits, tsa) from df and delete duplicate rows
# keep all traits
traits_tsa <- traits_tsa[,c(1:3,6,10,13,20:26,30)]
traits_tsa <- traits_tsa[!duplicated(traits_tsa), ]



#merge with average counts
traits_tsa_av <- merge.data.frame(average.counts, traits_tsa, by = c("Reef", "year", "Species"), 
                                  by.y = c("Reef", "year", "Species") )

##remove malindi
traits_tsa_av <- filter(traits_tsa_av, Reef2 != "Malindi")


## now make a matrix of traits to species
trait_matrix <- traits_tsa_av[,c(3,7,8,9,11:14 )]
trait_matrix$Size <- factor(trait_matrix$Size, ordered = TRUE)
trait_matrix$Diet <- as.factor(trait_matrix$Diet)
trait_matrix$Position <- factor(trait_matrix$Position, ordered = TRUE)
trait_matrix$Schooling <- factor(trait_matrix$Schooling, ordered = TRUE)
trait_matrix$Reef.Association <- factor(trait_matrix$Reef.Association, levels = c("Low", "Med", "High"), ordered=TRUE)

trait_matrix <- unique(trait_matrix) 


#make a matrix for traits
trait_matrix2 <- trait_matrix

rownames(trait_matrix2) <- trait_matrix2[,1]
trait_matrix2 <- trait_matrix2[,-1]

#order them alphabetically
trait_matrix2 <- trait_matrix2[order(rownames(trait_matrix2)),]



## find any NAs and check why
trait_matrix2_na <- trait_matrix2 [ !complete.cases(trait_matrix2), ]


# create weights file for number of species per year
kenya2 <- kenya[,-c(2)] ## fix this 
kenya2$year <- as.factor(kenya2$year)
time_since_closure <- kenya2[,c(3,4,5,6,11)]
kenya2$Species <- as.factor(kenya2$Species)
kenya2$count <- as.numeric(kenya2$count)


#average counts of spp per year
weights_traits <- aggregate(count ~ Species + year + Reef, FUN=mean, data=kenya2) # takin mean because there's different numb of transects

#pcoa
trait_matrix2$Size <- factor(trait_matrix2$Size, ordered = TRUE)
trait_matrix2$Schooling<- factor(trait_matrix2$Schooling, ordered = TRUE)
trait_matrix2$Position <- factor(trait_matrix2$Position, ordered = TRUE)
trait_matrix2$Diet <- factor(trait_matrix2$Diet, ordered = FALSE)
trait_matrix2$Reef.Association <- factor(trait_matrix2$Reef.Association, ordered = TRUE, levels = c("Low", "Med", "High"))


##remove mobility
trait_matrix2 <-subset(trait_matrix2, select = -c(Mobility))


#remove na rows
trait_matrix2 <- trait_matrix2 [ complete.cases(trait_matrix2), ]



mat_dissim<-daisy(trait_matrix2, metric="gower")
mat_pcoa<-pcoa(mat_dissim)
head(mat_pcoa$values$Relative_eig) 
summary(mat_pcoa)


vf <- envfit(mat_pcoa$vectors, trait_matrix2, perm = 999)
biplot(mat_pcoa)
plot(vf) 


trait_labels <- read.csv(file = "Data/trait_scores_labels.csv", header = TRUE)#run in code
categories <- as.data.frame(vf$factors$centroids)
vector <- as.data.frame(vf$vectors$arrows)

#remove vector from the trait lables
category_lables <- trait_labels[ !(trait_labels$traits %in% c("Lm")), ]

#merge with categories
categories <- merge.data.frame(category_lables, categories, 
                               by.x= "traits", by.y="row.names")

#merge vector label
vector_lables <- trait_labels[ (trait_labels$traits %in% c("Lm")), ]
vector <- merge.data.frame(vector_lables, vector, 
                           by.x= "traits", by.y="row.names")


#plot 
plot_pcoa = ggplot(categories, aes(x = Axis.1, y = Axis.2)) +
  geom_point(alpha = 0.5, size = 5) + 
  geom_text_repel(label = categories$labels, size = 6)
#geom_text_repel(label = labels, aes(Axis.1,Axis.2))

plot_pcoa + geom_segment(data = vector, aes(x = 0, xend = Axis.1-0.3, y=0, yend = Axis.2),
                         arrow = arrow(length = unit(0.25, "cm")), colour = "black")+
  geom_label(data = vector, aes(x = 0, y=0, label = labels), nudge_x = c(0.15), 
             nudge_y = c(-0.4), size = 6) +
  theme_bw(base_size = 22) +
  xlab("PCoA 1 (44%)") + ylab("PCoA 2 (31%)")


##make biplot with spp on it
spp <- as.data.frame(mat_pcoa$vectors)

spp$spcode <- spCodes(rownames(spp),
                      nchar.gen = 5, nchar.sp = 10, nchar.ssp = 1, sep.spcode = ".")


spp_pcoa <- ggplot(spp, aes(x = Axis.1, y = Axis.2)) +
  # geom_point(alpha = 0.5, size = 5) + 
  geom_text_repel(label = rownames(spp), size = 3)+
  geom_text_repel(data = categories, label = categories$labels, size = 6,  colour = "red")+
  geom_segment(data = vector, aes(x = 0, xend = Axis.1-0.3, y=0, yend = Axis.2+0.3),
               arrow = arrow(length = unit(0.25, "cm")), colour = "red")+
  geom_label(data = vector, aes(x = 0, y=0, label = labels), nudge_x = c(0.22), 
             nudge_y = c(-0.4), size = 6, colour = "red") +
  theme_bw(base_size = 22) +
  xlab("PCoA 1 (44%)") + ylab("PCoA 2 (31%)")+
  theme(axis.text.x = element_text(angle = 50,  hjust=1, size=28), panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#get species and counts and covert into a wide matrix
weights <- traits_tsa_av[,c("Species", "av_count", "year", "Reef")]


#### match reef to reef2 so can filter out malindi
reef2 <- select(abundance_av, c("Reef", "Reef2"))

weights <- merge.data.frame(weights, reef2, by = "Reef")
weights <- unique(weights)

#remove malindi
weights <- filter(weights, Reef2 != "Malindi")

#create yearsite
weights$yearsite <- paste(weights$year,weights$Reef, sep="_")
weights <- weights[,c(2,3,6)]
weights<- weights[!duplicated(weights), ]


#match species in weights to species from mat_pcoa
a <- mat_pcoa$vectors
weights <- filter(weights, weights$Species %in% rownames(a))
weights<- weights[!duplicated(weights), ]

weights_wide <- spread(weights, Species, av_count)

rownames(weights_wide) <- weights_wide[,1]
weights_wide <- weights_wide[,-1]
weights_wide <- as.matrix(weights_wide)


cwm_assem_m <- functcomp(mat_pcoa$vectors, weights_wide, CWM.type = "all") ### abundance weighted means for each site/year
cwm_traits_m <- functcomp(trait_matrix2, weights_wide, CWM.type = "all") ### abundance weighted means for traits


### make year seperate column
cwm_assem_m  <- as.data.frame(cwm_assem_m)
cwm_assem_m <- cwm_assem_m  %>% rownames_to_column("site_year")


#select first 4 axes
cwm_assem_m <- cwm_assem_m[,1:5]

#fix up site / year
cwm_assem_m <- cSplit(cwm_assem_m, "site_year", "_")
cwm_assem_m <- cwm_assem_m[,c(5:7, 1:4)]
colnames(cwm_assem_m )[colnames(cwm_assem_m )=="site_year_1"] <- "year"

# merge site names
cwm_assem_m$site <- paste(cwm_assem_m$site_year_2,cwm_assem_m$site_year_3, sep="_")

cwm_assem_m <- cwm_assem_m[,c(1,4:8)]
cwm_assem_m <- cwm_assem_m[,c(1,6,2:5)]


#remove _NA from site
cwm_assem_m$site <- str_remove(cwm_assem_m$site, "_NA")
