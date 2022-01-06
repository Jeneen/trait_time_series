#plot trait models
library(ggplot2)
library(RColorBrewer)

##color palette
palette <- brewer.pal(6, "Dark2")


#load trait models from source
source("Code/8 - run trait models.R")

###########################################################SIZE ########################################################################


size.1 <- filter(size, Size == "1")
size.2 <- filter(size, Size == "2")
size.3 <- filter(size, Size == "3")
size.4 <- filter(size, Size == "4")
size.5 <- filter(size, Size == "5")
size.6 <- filter(size, Size == "6")




newdata.1<-data.frame(time_since_closure_scaled= size.1$time_since_closure_scaled, 
                      mean_biomass_sc = mean(size.1$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(size.1$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(size.1$ben_pc1_sc),
                      Reef2 = 'Kisite', Size = "1")
pred.1 <- predict(size_mod, se.fit=T, newdata = newdata.1, re.form=NA, type = "response") 
size.1 <- cbind(size.1, pred.1)



newdata.2<-data.frame(time_since_closure_scaled= size.2$time_since_closure_scaled, 
                      mean_biomass_sc = mean(size.2$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(size.2$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(size.2$ben_pc1_sc),
                      Reef2 = 'Kisite', Size = "2")
pred.2 <- predict(size_mod, se.fit=T, newdata = newdata.2, re.form=NA, type = "response") 
size.2 <- cbind(size.2, pred.2)


newdata.3<-data.frame(time_since_closure_scaled= size.3$time_since_closure_scaled,  
                      mean_biomass_sc = mean(size.3$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(size.3$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(size.3$ben_pc1_sc),
                      Reef2 = 'Kisite', Size = "3")
pred.3 <- predict(size_mod, se.fit=T, newdata = newdata.3, re.form=NA, type = "response") 
size.3 <- cbind(size.3, pred.3)


newdata.4<-data.frame(time_since_closure_scaled= size.4$time_since_closure_scaled,
                      mean_biomass_sc = mean(size.4$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(size.4$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(size.4$ben_pc1_sc),
                      Reef2 = 'Kisite', Size = "4")
pred.4 <- predict(size_mod, se.fit=T, newdata = newdata.4, re.form=NA, type = "response") 
size.4 <- cbind(size.4, pred.4)



newdata.5<-data.frame(time_since_closure_scaled= size.5$time_since_closure_scaled, 
                      mean_biomass_sc = mean(size.5$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(size.5$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(size.5$ben_pc1_sc),
                      Reef2 = 'Kisite', Size = "5")
pred.5 <- predict(size_mod, se.fit=T, newdata = newdata.5, re.form=NA, type = "response") 
size.5 <- cbind(size.5, pred.5)


newdata.6<-data.frame(time_since_closure_scaled= size.6$time_since_closure_scaled, 
                      mean_biomass_sc = mean(size.6$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(size.6$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(size.6$ben_pc1_sc),
                      Reef2 = 'Kisite', Size = "6")
pred.6 <- predict(size_mod, se.fit=T, newdata = newdata.6, re.form=NA, type = "response") 
size.6 <- cbind(size.6, pred.6)




#get confidence bands manually
size.1$upper <- size.1$fit + 1.96*size.1$se.fit
size.1$lower <- size.1$fit - 1.96*size.1$se.fit

size.2$upper <- size.2$fit + 1.96*size.2$se.fit
size.2$lower <- size.2$fit - 1.96*size.2$se.fit

size.3$upper <- size.3$fit + 1.96*size.3$se.fit
size.3$lower <- size.3$fit - 1.96*size.3$se.fit

size.4$upper <- size.4$fit + 1.96*size.4$se.fit
size.4$lower <- size.4$fit - 1.96*size.4$se.fit


size.5$upper <- size.5$fit + 1.96*size.5$se.fit
size.5$lower <- size.5$fit - 1.96*size.5$se.fit


size.6$upper <- size.6$fit + 1.96*size.6$se.fit
size.6$lower <- size.6$fit - 1.96*size.6$se.fit

#values=c("1"="red", "2"="green", "3"="blue"

sizeNA <- rbind(size.1, size.2, size.3, size.4, size.5, size.6)
###


p.size <- ggplot(sizeNA, aes(x = time_since_closure, y = logit, color = Size)) + 
  #geom_point(size = 3)+
  geom_vline(xintercept=c(10,20,30,40), linetype="dashed", color = "grey", size = 1)+
  geom_ribbon(aes(ymin=lower, ymax= upper, fill=Size), colour = NA,
              linetype=1, alpha=0.5)+
  xlab("Time since closure (years)") +  ylab(expression(paste("Proportional abundance \n               (logit)"))) +
  geom_line(aes(x=time_since_closure, y = fit, color = Size), size = 1) +
  scale_color_manual(limits=c("1", "2", "3", "4", "5", "6"), name = "Size", 
                     values = c("1" = palette[1], "2" = palette[2], "3" = palette[3],
                                "4" = palette[4], "5" = palette[5], "6" = palette[6]),
                     labels = c("0-7cm", "7-15cm", "15-30cm", "30-50cm","50-80cm", ">80cm"))+
  scale_fill_manual( "Size",
                     values = c("1" = palette[1], "2" = palette[2], "3" = palette[3],
                                "4" = palette[4], "5" = palette[5], "6" = palette[6]),
                     labels = c("0-7cm", "7-15cm", "15-30cm", "30-50cm","50-80cm", ">80cm"))+
  
  theme_bw(base_size = 22)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = margin(1, 3, 1, 1, "cm"), plot.background = element_blank())+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))




p.size

###########################################################   DIET   ##################################################################

diet.fc <- filter(diet, Diet == "FC")
diet.hd <- filter(diet, Diet == "HD")
diet.hm <- filter(diet, Diet == "HM")
diet.im <- filter(diet, Diet == "IM")
diet.is <- filter(diet, Diet == "IS")
diet.om <- filter(diet, Diet == "OM")
diet.pk <- filter(diet, Diet == "PK")



newdata.1<-data.frame(time_since_closure_scaled= diet.fc$time_since_closure_scaled, 
                      mean_biomass_sc = mean(diet.fc$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(diet.fc$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(diet.fc$ben_pc1_sc),
                      Reef2 = 'Kisite', Diet = "FC")
pred.1 <- predict(diet_mod, se.fit=T, newdata = newdata.1, re.form=NA, type = "response") 
diet.fc <- cbind(diet.fc, pred.1)



newdata.2<-data.frame(time_since_closure_scaled= diet.hd$time_since_closure_scaled, 
                      mean_biomass_sc = mean(diet.hd$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(diet.hd$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(diet.hd$ben_pc1_sc),
                      Reef2 = 'Kisite', Diet = "HD")
pred.2 <- predict(diet_mod, se.fit=T, newdata = newdata.2, re.form=NA, type = "response")
diet.hd <- cbind(diet.hd, pred.2)


newdata.3<-data.frame(time_since_closure_scaled= diet.hm$time_since_closure_scaled,  
                      mean_biomass_sc = mean(diet.hm$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(diet.hm$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(diet.hm$ben_pc1_sc),
                      Reef2 = 'Kisite', Diet = "HM")
pred.3 <- predict(diet_mod, se.fit=T, newdata = newdata.3, re.form=NA, type = "response") 
diet.hm <- cbind(diet.hm, pred.3)


newdata.4<-data.frame(time_since_closure_scaled= diet.im$time_since_closure_scaled,
                      mean_biomass_sc = mean(diet.im$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(diet.im$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(diet.im$ben_pc1_sc),
                      Reef2 = 'Kisite', Diet = "IM")
pred.4 <- predict(diet_mod, se.fit=T, newdata = newdata.4, re.form=NA, type = "response") 
diet.im <- cbind(diet.im, pred.4)



newdata.5<-data.frame(time_since_closure_scaled= diet.is$time_since_closure_scaled, 
                      mean_biomass_sc = mean(diet.is$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(diet.is$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(diet.is$ben_pc1_sc),
                      Reef2 = 'Kisite', Diet = "IS")
pred.5 <- predict(diet_mod, se.fit=T, newdata = newdata.5, re.form=NA, type = "response") 
diet.is <- cbind(diet.is, pred.5)


newdata.6<-data.frame(time_since_closure_scaled= diet.om$time_since_closure_scaled, 
                      mean_biomass_sc = mean(diet.om$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(diet.om$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(diet.om$ben_pc1_sc),
                      Reef2 = 'Kisite', Diet = "OM")
pred.6 <- predict(diet_mod, se.fit=T, newdata = newdata.6, re.form=NA, type = "response") 
diet.om <- cbind(diet.om, pred.6)


newdata.7<-data.frame(time_since_closure_scaled= diet.om$time_since_closure_scaled, 
                      mean_biomass_sc = mean(diet.om$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(diet.om$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(diet.om$ben_pc1_sc),
                      Reef2 = 'Kisite', Diet = "PK")
pred.7 <- predict(diet_mod, se.fit=T, newdata = newdata.7, re.form=NA, type = "response") 
diet.pk <- cbind(diet.pk, pred.7)




diet.fc$upper <- diet.fc$fit + 1.96*diet.fc$se.fit
diet.fc$lower <- diet.fc$fit - 1.96*diet.fc$se.fit

diet.hd$upper <- diet.hd$fit + 1.96*diet.hd$se.fit
diet.hd$lower <- diet.hd$fit - 1.96*diet.hd$se.fit

diet.hm$upper <- diet.hm$fit + 1.96*diet.hm$se.fit
diet.hm$lower <- diet.hm$fit - 1.96*diet.hm$se.fit

diet.im$upper <- diet.im$fit + 1.96*diet.im$se.fit
diet.im$lower <- diet.im$fit - 1.96*diet.im$se.fit


diet.is$upper <- diet.is$fit + 1.96*diet.is$se.fit
diet.is$lower <- diet.is$fit - 1.96*diet.is$se.fit


diet.om$upper <- diet.om$fit + 1.96*diet.om$se.fit
diet.om$lower <- diet.om$fit - 1.96*diet.om$se.fit

diet.pk$upper <- diet.pk$fit + 1.96*diet.pk$se.fit
diet.pk$lower <- diet.pk$fit - 1.96*diet.pk$se.fit


dietNA <- rbind(diet.fc, diet.hd, diet.hm, diet.im, diet.is, diet.om, diet.pk)
###



diet.fc <- filter(diet, Diet == "FC")
diet.hd <- filter(diet, Diet == "HD")
diet.hm <- filter(diet, Diet == "HM")
diet.im <- filter(diet, Diet == "IM")
diet.is <- filter(diet, Diet == "IS")
diet.om <- filter(diet, Diet == "OM")
diet.pk <- filter(diet, Diet == "PK")


p.diet <- ggplot(dietNA, aes(x = time_since_closure, y = logit, color = Diet)) + 
  # geom_point(alpha = 0.5, size = 0.3)+
  geom_vline(xintercept=c(10,20,30,40), linetype="dashed", color = "grey", size = 1)+
  geom_ribbon(aes(ymin=lower, ymax= upper, fill=Diet), colour = NA,
              linetype=1, alpha=0.5)+
  xlab("Time since closure (years)") + ylab(expression(paste("Proportional abundance \n               (logit)"))) +
  geom_line(aes(x=time_since_closure, y = fit, color = Diet), size = 1) +
  scale_color_manual( name = "Diet", 
                      values = c("FC" = palette[1], "HD" = palette[2], "HM" = palette[3],
                                 "IM" = palette[4], "IS" = palette[5], "OM" = palette[6], 
                                 "PK" = palette[7]),
                      labels = c("Piscivorous", "Herbivorous-detritus", "Herbivorous-macroalgae", 
                                 "Mobile invertebrate feeders","Sessile invertebrate feeders", 
                                 "Omnivorous", "Planktivorous"))+
  scale_fill_manual( "Diet",
                     values = c("FC" = palette[1], "HD" = palette[2], "HM" = palette[3],
                                "IM" = palette[4], "IS" = palette[5], "OM" = palette[6], 
                                "PK" = palette[7]),
                     labels = c("Piscivorous", "Herbivorous-detritus", "Herbivorous-macroalgae", 
                                "Mobile invertebrate feeders","Sessile invertebrate feeders", 
                                "Omnivorous", "Planktivorous"))+
  theme_bw(base_size = 22)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = margin(1, 0, 1, 1, "cm"))+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

p.diet

###########################################################   POSITION   ##################################################################
position.1 <- filter(position, Position == "1")
position.2 <- filter(position, Position == "2")
position.3 <- filter(position, Position == "3")






newdata.1<-data.frame(time_since_closure_scaled= position.1$time_since_closure_scaled, 
                      mean_biomass_sc = mean(position.1$mean_biomass_sc), 
                      rugosity_sc = mean(position.1$rugosity_sc),
                      tsa_max.lag.sc =mean(position.1$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(position.1$ben_pc1_sc),
                      Reef2 = 'Kisite', Position = "1")
pred.1 <- predict(position_mod, se.fit=T, newdata = newdata.1, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
position.1 <- cbind(position.1, pred.1)



newdata.2<-data.frame(time_since_closure_scaled= position.2$time_since_closure_scaled, 
                      mean_biomass_sc = mean(position.2$mean_biomass_sc), 
                      rugosity_sc = mean(position.2$rugosity_sc),
                      tsa_max.lag.sc =mean(position.2$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(position.2$ben_pc1_sc),
                      Reef2 = 'Kisite', Position = "2")
pred.2 <- predict(position_mod, se.fit=T, newdata = newdata.2, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
position.2 <- cbind(position.2, pred.2)


newdata.3<-data.frame(time_since_closure_scaled= position.3$time_since_closure_scaled,  
                      mean_biomass_sc = mean(position.3$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(position.3$tsa_max.lag.sc), 
                      rugosity_sc = mean(position.3$rugosity_sc),
                      ben_pc1_sc = mean(position.3$ben_pc1_sc),
                      Reef2 = 'Kisite', Position = "3")
pred.3 <- predict(position_mod, se.fit=T, newdata = newdata.3, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
position.3 <- cbind(position.3, pred.3)






#get confidence bands manually
position.1$upper <- position.1$fit + 1.96*position.1$se.fit
position.1$lower <- position.1$fit - 1.96*position.1$se.fit

position.2$upper <- position.2$fit + 1.96*position.2$se.fit
position.2$lower <- position.2$fit - 1.96*position.2$se.fit

position.3$upper <- position.3$fit + 1.96*position.3$se.fit
position.3$lower <- position.3$fit - 1.96*position.3$se.fit


#values=c("1"="red", "2"="green", "3"="blue"

positionNA <- rbind(position.1, position.2, position.3)


p.position <- ggplot(positionNA, aes(x = time_since_closure, y = logit, color = Position)) + 
  geom_vline(xintercept=c(10,20,30,40), linetype="dashed", color = "grey", size = 1)+
  #geom_point(size = 3)+
  geom_ribbon(aes(ymin=lower, ymax= upper, fill=Position), colour = NA,
              linetype=1, alpha=0.5)+
  xlab("Time since closure (years)") + ylab(expression(paste("Proportional abundance \n               (logit)"))) +
  geom_line(aes(x=time_since_closure, y = fit, color = Position), size = 1) +
  scale_color_manual(limits=c("1", "2", "3"), name = "Position in water\ncolumn", 
                     values = c("1" = palette[1], "2" = palette[2], "3" = palette[3]),
                     labels = c("Bottom-dweller", "Low in water column", "Pelagic"))+
  scale_fill_manual( "Position in water\ncolumn",
                     values = c("1" = palette[1], "2" = palette[2], "3" = palette[3]),
                     labels = c("Bottom-dweller", "Low in water column", "Pelagic"))+
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01))+
  
  theme_bw(base_size = 22)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = margin(1, 0, 1, 1, "cm"))

p.position



###################################################   REEF ASSOCIATION     ##################################################################
association.1 <- filter(association, Reef.Association == "Low")
association.2 <- filter(association, Reef.Association == "Med")
association.3 <- filter(association, Reef.Association == "High")

newdata.1<-data.frame(time_since_closure_scaled= association.1$time_since_closure_scaled, 
                      mean_biomass_sc = mean(association.1$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(association.1$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(association.1$ben_pc1_sc),
                      Reef2 = 'Kisite', Reef.Association = "Low")
pred.1 <- predict(association_mod, se.fit=T, newdata = newdata.1, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
association.1 <- cbind(association.1, pred.1)



newdata.2<-data.frame(time_since_closure_scaled= association.2$time_since_closure_scaled, 
                      mean_biomass_sc = mean(association.2$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(association.2$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(association.2$ben_pc1_sc),
                      Reef2 = 'Kisite', Reef.Association = "Med")
pred.2 <- predict(association_mod, se.fit=T, newdata = newdata.2, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
association.2 <- cbind(association.2, pred.2)


newdata.3<-data.frame(time_since_closure_scaled= association.3$time_since_closure_scaled,  
                      mean_biomass_sc = mean(association.3$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(association.3$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(association.3$ben_pc1_sc),
                      Reef2 = 'Kisite', Reef.Association = "High")
pred.3 <- predict(association_mod, se.fit=T, newdata = newdata.3, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
association.3 <- cbind(association.3, pred.3)

#get confidence bands manually
association.1$upper <- association.1$fit + 1.96*association.1$se.fit
association.1$lower <- association.1$fit - 1.96*association.1$se.fit

association.2$upper <- association.2$fit + 1.96*association.2$se.fit
association.2$lower <- association.2$fit - 1.96*association.2$se.fit

association.3$upper <- association.3$fit + 1.96*association.3$se.fit
association.3$lower <- association.3$fit - 1.96*association.3$se.fit


#values=c("1"="red", "2"="green", "3"="blue"

associationNA <- rbind(association.1, association.2, association.3)



p.association <- ggplot(associationNA, aes(x = time_since_closure, y = logit, color = Reef.Association)) + 
  #geom_point(size = 3)+
  geom_vline(xintercept=c(10,20,30,40), linetype="dashed", color = "grey", size = 1)+
  geom_ribbon(aes(ymin=lower, ymax= upper, fill=Reef.Association), colour = NA,
              linetype=1, alpha=0.5)+
  xlab("Time since closure (years)") +ylab(expression(paste("Proportional abundance \n               (logit)")))  +
  geom_line(aes(x=time_since_closure, y = fit, color = Reef.Association), size = 1) +
  scale_color_manual(limits=c("Low", "Med", "High"), name = "Level of reef\nassociation", 
                     values = c("Low" = palette[1], "Med" = palette[2], "High" = palette[3]),
                     labels = c("Low", "Med", "High"))+
  scale_fill_manual(limits=c("Low", "Med", "High"), name = "Level of reef\nassociation",
                    values = c("Low" = palette[1], "Med" = palette[2], "High" = palette[3]),
                    labels = c("Low", "Med", "High"))+
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01))+
  theme_bw(base_size = 22)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = margin(1, 2, 1, 1, "cm"))



p.association


########################################################### SCHOOLING ##################################################################
schooling.1 <- filter(schooling, Schooling == "1")
schooling.2 <- filter(schooling, Schooling == "2")
schooling.3 <- filter(schooling, Schooling == "3")
schooling.4 <- filter(schooling, Schooling == "4")
schooling.5 <- filter(schooling, Schooling == "5")





newdata.1<-data.frame(time_since_closure_scaled= schooling.1$time_since_closure_scaled, 
                      mean_biomass_sc = mean(schooling.1$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(schooling.1$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(schooling.1$ben_pc1_sc),
                      Reef2 = 'Kisite', Schooling = "1")
pred.1 <- predict(schooling_mod, se.fit=T, newdata = newdata.1, re.form=NA, type = "response") 
schooling.1 <- cbind(schooling.1, pred.1)



newdata.2<-data.frame(time_since_closure_scaled= schooling.2$time_since_closure_scaled, 
                      mean_biomass_sc = mean(schooling.2$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(schooling.2$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(schooling.2$ben_pc1_sc),
                      Reef2 = 'Kisite', Schooling = "2")
pred.2 <- predict(schooling_mod, se.fit=T, newdata = newdata.2, re.form=NA, type = "response") 
schooling.2 <- cbind(schooling.2, pred.2)


newdata.3<-data.frame(time_since_closure_scaled= schooling.3$time_since_closure_scaled,  
                      mean_biomass_sc = mean(schooling.3$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(schooling.3$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(schooling.3$ben_pc1_sc),
                      Reef2 = 'Kisite', Schooling = "3")
pred.3 <- predict(schooling_mod, se.fit=T, newdata = newdata.3, re.form=NA, type = "response") 
schooling.3 <- cbind(schooling.3, pred.3)


newdata.4<-data.frame(time_since_closure_scaled= schooling.4$time_since_closure_scaled,
                      mean_biomass_sc = mean(schooling.4$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(schooling.4$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(schooling.4$ben_pc1_sc),
                      Reef2 = 'Kisite', Schooling = "4")
pred.4 <- predict(schooling_mod, se.fit=T, newdata = newdata.4, re.form=NA, type = "response") 
schooling.4 <- cbind(schooling.4, pred.4)



newdata.5<-data.frame(time_since_closure_scaled= schooling.5$time_since_closure_scaled, 
                      mean_biomass_sc = mean(schooling.5$mean_biomass_sc), 
                      tsa_max.lag.sc =mean(schooling.5$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(schooling.5$ben_pc1_sc),
                      Reef2 = 'Kisite', Schooling = "5")
pred.5 <- predict(schooling_mod, se.fit=T, newdata = newdata.5, re.form=NA, type = "response") 
schooling.5 <- cbind(schooling.5, pred.5)





#get confidence bands manually
schooling.1$upper <- schooling.1$fit + 1.96*schooling.1$se.fit
schooling.1$lower <- schooling.1$fit - 1.96*schooling.1$se.fit

schooling.2$upper <- schooling.2$fit + 1.96*schooling.2$se.fit
schooling.2$lower <- schooling.2$fit - 1.96*schooling.2$se.fit

schooling.3$upper <- schooling.3$fit + 1.96*schooling.3$se.fit
schooling.3$lower <- schooling.3$fit - 1.96*schooling.3$se.fit

schooling.4$upper <- schooling.4$fit + 1.96*schooling.4$se.fit
schooling.4$lower <- schooling.4$fit - 1.96*schooling.4$se.fit


schooling.5$upper <- schooling.5$fit + 1.96*schooling.5$se.fit
schooling.5$lower <- schooling.5$fit - 1.96*schooling.5$se.fit



#values=c("1"="red", "2"="green", "3"="blue"

schoolingNA <- rbind(schooling.1, schooling.2, schooling.3, schooling.4, schooling.5)



p.schooling <- ggplot(schoolingNA, aes(x = time_since_closure, y = logit, color = Schooling)) + 
  #geom_point(size = 3)+
  geom_vline(xintercept=c(10,20,30,40), linetype="dashed", color = "grey", size = 1)+
  geom_ribbon(aes(ymin=lower, ymax= upper, fill=Schooling), colour = NA,
              linetype=1, alpha=0.5)+
  xlab("Time since closure (years)") +ylab(expression(paste("Proportional abundance \n               (logit)")))  +
  geom_line(aes(x=time_since_closure, y = fit, color = Schooling), size = 1) +
  scale_color_manual(limits=c("1", "2", "3", "4", "5"), name = "Schooling", 
                     values = c("1" = palette[1], "2" = palette[2], "3" = palette[3],
                                "4" = palette[4], "5" = palette[5]),
                     labels = c("Solitary", "Pairing", "Small group", "Medium group","Large group"))+
  scale_fill_manual( "Schooling",
                     values = c("1" = palette[1], "2" = palette[2], "3" = palette[3],
                                "4" = palette[4], "5" = palette[5]),
                     labels = c("Solitary", "Pairing", "Small group", "Medium group","Large group"))+
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01))+
  
  theme_bw(base_size = 22)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = margin(1, 3.5, 1, 1, "cm"))

p.schooling


################################################# LENGTH AT MATURITY ################################################################
lm.1 <- lm
newdata.1<-data.frame(time_since_closure_scaled= lm.1$time_since_closure_scaled, 
                      mean_biomass_sc = mean(lm.1$mean_biomass_sc), chl_sc = 0,
                      tsa_max.lag.sc =mean(lm.1$tsa_max.lag.sc), 
                      ben_pc1_sc = mean(lm.1$ben_pc1_sc),
                      rugosity_sc =mean(lm.1$rugosity_sc),
                      Reef2 = 'Mombasa')
pred.1 <- predict(lm_mod, se.fit=T, newdata = newdata.1, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val


#get partial residuals
resid <- residuals(lm_mod)  + pred.1$fit



lm.1 <- cbind(lm.1, pred.1, resid)



#get confidence bands manually
lm.1$upper <- lm.1$fit + 1.96*lm.1$se.fit
lm.1$lower <- lm.1$fit - 1.96*lm.1$se.fit




p.lm <- ggplot(lm.1, aes(x = time_since_closure.x, y = mean.lm)) + 
  # geom_point(size = 3)+
  geom_ribbon(aes(ymin=lower, ymax= upper), colour = NA,
              linetype=1, alpha=0.3)+
  xlab("Time since closure (years)") + ylab("Mean length at maturity \n (log cm)")  +
  geom_line(aes(x=time_since_closure.x, y = fit), size = 1) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01))+
  theme_bw(base_size = 22)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin = margin(1, 10, 1, 0.4, "cm"))


p.lm
