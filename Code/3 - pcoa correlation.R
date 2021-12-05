
library(tidyverse)
library(ggplot2)
library(patchwork)

source("Code/2 - PCoA.R")


### fix cwm_assem_m so year_site combined into row names
cwm_assem_m$yearsite <- paste(cwm_assem_m$year, cwm_assem_m$site, sep="_")
cwm_assem_m <- cwm_assem_m[,c(7, 3:6)]
cwm_assem_m <- cwm_assem_m %>% remove_rownames %>% column_to_rownames(var="yearsite")


# run a pearson correlation

output <- data.frame(pearson_axis1 = NA, traits = colnames(cwm_traits_m))

for (i in 1:length(colnames(cwm_traits_m))){
  output[i, 1]<- cor(cwm_assem_m$Axis.1, cwm_traits_m[[i]], method = "pearson")
}

output_axis2 <- data.frame(pearson_axis2 = NA, traits = colnames(cwm_traits_m))

for (i in 1:length(colnames(cwm_traits_m))){
  output_axis2[i, 1]<- cor(cwm_assem_m$Axis.2, cwm_traits_m[[i]], method = "pearson")
}


pearson_cor <- merge.data.frame(output_axis2, output)
pearson_cor <- pearson_cor %>% arrange(desc(pearson_axis1))



#plot the correlation as a forest plot
trait_labels <- read.csv(file = "trait_scores_labels.csv", header = TRUE)


#fix trait names in pearson - remove _
pearson_cor$traits <- as.character(pearson_cor$traits)
pearson_cor$traits <- str_replace_all(string=pearson_cor$traits, pattern="_", repl="")


pearson_cor <- merge.data.frame(trait_labels, pearson_cor)


p1 = ggplot(data=pearson_cor, aes(x = reorder(labels, pearson_axis1), y = pearson_axis1))+
  geom_point(aes(size = 3)) + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Traits") + ylab("Pearson correlation") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none")+
  ggtitle("PCoA 1")

p2 = ggplot(data=pearson_cor, aes(x = reorder(labels, pearson_axis2), y = pearson_axis2))+
  geom_point(aes(size = 3)) + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Traits") + ylab("Pearson correlation") +
  theme_bw(base_size = 22) +
  theme(legend.position = "none")+
  ggtitle("PCoA 2")
p1|p2


