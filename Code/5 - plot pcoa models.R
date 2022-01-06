#load package
library(RColorBrewer)

#run previous code
source("Code/4 - run PCoA models.r")

#load data
load("Data/tsalag4_with_chl.rda") 

#set color palette
palette <-brewer.pal(n = 3, name = "Dark2")


#clean data
tsalag4 <- unique(tsalag4)

#remove malindi
tsalag4 <- filter(tsalag4, Reef2 !="Malindi")

#remove NAs
tsalag4_na <- filter(tsalag4, !is.na(Axis.1))
tsalag4_na <- filter(tsalag4, !is.na(Axis.2))
tsalag4_na <- filter(tsalag4_na, !is.na(time_since_closure))
tsalag4_na <- filter(tsalag4_na, !is.na(Reef2))
tsalag4_na <- filter(tsalag4_na, !is.na(tsa_max.lag.sc))

#remove watamu outlier
#tsalag4_na <-filter(tsalag4_na, Reef2 != "Watamu"| time_since_closure != 20)

mombasa <- filter(tsalag4_na, Reef2 == "Mombasa")
kisite <- filter(tsalag4_na, Reef2 == "Kisite")
watamu <- filter(tsalag4_na, Reef2 == "Watamu")


######################################################## PCOA 1 ##########################################################################



newdata.mom<-data.frame(time_since_closure_scaled= mombasa$time_since_closure_scaled, 
                        tsa_max.lag.sc = mean(mombasa$tsa_max.lag.sc), rugosity_sc =0,
                        ratio_ma_coral_scaled =0, ratio_ca_ma_scaled =0, mean.biomass.scaled = 0,npp_mean_sc = 0,
                        ben_pc1_sc =0,
                        Reef2 = "Mombasa") 
pred.mom <- predict(pcoa1_mod_reef, se.fit=T, newdata = newdata.mom, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
mombasa <- cbind(mombasa, pred.mom)




newdata.kis<-data.frame(time_since_closure_scaled= kisite$time_since_closure_scaled, 
                        tsa_max.lag.sc = mean(kisite$tsa_max.lag.sc),rugosity_sc =0,
                        ratio_ma_coral_scaled =0, ratio_ca_ma_scaled =0, mean.biomass.scaled = 0,npp_mean_sc = 0,
                        ben_pc1_sc =0,
                        Reef2 = "Kisite") 
pred.kis <- predict(pcoa1_mod_reef , se.fit=T, newdata = newdata.kis, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
kisite <- cbind(kisite, pred.kis)




newdata.wat<-data.frame(time_since_closure_scaled= watamu$time_since_closure_scaled, 
                        tsa_max.lag.sc = mean(watamu$tsa_max.lag.sc),rugosity_sc =0,
                        ratio_ma_coral_scaled =0, ratio_ca_ma_scaled =0, mean.biomass.scaled = 0,npp_mean_sc = 0,
                        ben_pc1_sc =0,
                        Reef2 = "Watamu") 
pred.wat <- predict(pcoa1_mod_reef , se.fit=T, newdata = newdata.wat, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
watamu <- cbind(watamu, pred.wat)





#get confidence bands manually
mombasa$upper <- mombasa$fit + 1.96*mombasa$se.fit
mombasa$lower <- mombasa$fit - 1.96*mombasa$se.fit

kisite$upper <- kisite$fit + 1.96*kisite$se.fit
kisite$lower <- kisite$fit - 1.96*kisite$se.fit

watamu$upper <- watamu$fit + 1.96*watamu$se.fit
watamu$lower <- watamu$fit - 1.96*watamu$se.fit


axis1_lag4 <- rbind(mombasa, kisite, watamu)


#get partial residuals
resid <- residuals(pcoa1_mod_reef )
resid <- resid[!is.na(resid)] + axis1_lag4$fit
axis1_lag4$fit <- axis1_lag4$fit[!is.na(axis1_lag4$fit)]
axis1_lag4$se.fit <- axis1_lag4$se.fit[!is.na(axis1_lag4$se.fit)]
#resid <- unique(resid)
axis1_lag4 <- cbind(axis1_lag4, resid)


#dataframes with model predictions
#filter for each mpa for graphs with partial resid
mombasa_axis1 <- filter(axis1_lag4, Reef2 == "Mombasa")
watamu_axis1 <- filter(axis1_lag4, Reef2 == "Watamu")
kisite_axis1 <- filter(axis1_lag4, Reef2 == "Kisite")


#pcoa 1
pcoa1 <- ggplot()+
  guides(color=guide_legend("Marine park"))+
  scale_color_manual( values = c("Mombasa" = palette[1], "Kisite" = palette[2],
                                 "Watamu" = palette[3]))+
  xlab("Time since closure (years)") + ylab("PCoA 1") +
  ggtitle("a. PCoA 1")+
  theme_bw(base_size = 22)+
  theme(text = element_text(size = 25),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  geom_line(data = mombasa_axis1, aes(x=time_since_closure, y = fit, color = Reef2), size =2, alpha = 0.5) +
  geom_ribbon(data = mombasa_axis1, aes(x=time_since_closure, y = fit, ymin=lower, ymax= upper,fill = Reef2 ), linetype=1, alpha=0.2)+
  geom_point(data = mombasa_axis1, alpha = 2, size = 3, aes(x=time_since_closure, y = resid, color = Reef2))+
  
  geom_line(data = kisite_axis1, aes(x=time_since_closure, y = fit, color = Reef2), size =2, alpha = 0.5) +
  geom_ribbon(data = kisite_axis1, aes(x=time_since_closure, y = fit, ymin=lower, ymax= upper, fill = Reef2), linetype=1, alpha=0.2)+
  geom_point(data = kisite_axis1, alpha = 2, size = 3, aes(x=time_since_closure, y = resid, color = Reef2))+
  
  geom_line(data = watamu_axis1, aes(x=time_since_closure, y = fit, color = Reef2), size =2, alpha = 0.5)+
  geom_point(data = watamu_axis1, alpha = 2, size = 3, aes(x=time_since_closure, y = resid, color = Reef2))+
  geom_ribbon(data = watamu_axis1, aes(x=time_since_closure, y = fit, ymin=lower, ymax= upper, fill = Reef2), linetype=1, alpha=0.2)


pcoa1



######################################################## PCOA 2 #########################################################################

mombasa <- filter(tsalag4_na, Reef2 == "Mombasa")
kisite <- filter(tsalag4_na, Reef2 == "Kisite")
watamu <- filter(tsalag4_na, Reef2 == "Watamu")



newdata.mom<-data.frame(time_since_closure_scaled= mombasa$time_since_closure_scaled, 
                        tsa_max.lag.sc = mean(mombasa$tsa_max.lag.sc), rugosity_sc =0,
                        ratio_ma_coral_scaled =0, ratio_ca_ma_scaled =0, mean.biomass.scaled = 0,npp_mean_sc = 0,
                        ben_pc1_sc =0,
                        Reef2 = "Mombasa") 
pred.mom <- predict(pcoa2_mod_reef, se.fit=T, newdata = newdata.mom, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
mombasa <- cbind(mombasa, pred.mom)




newdata.kis<-data.frame(time_since_closure_scaled= kisite$time_since_closure_scaled, 
                        tsa_max.lag.sc = mean(kisite$tsa_max.lag.sc),rugosity_sc =0,
                        ratio_ma_coral_scaled =0, ratio_ca_ma_scaled =0, mean.biomass.scaled = 0,npp_mean_sc = 0,
                        ben_pc1_sc =0,
                        Reef2 = "Kisite") 
pred.kis <- predict(pcoa2_mod_reef , se.fit=T, newdata = newdata.kis, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
kisite <- cbind(kisite, pred.kis)




newdata.wat<-data.frame(time_since_closure_scaled= watamu$time_since_closure_scaled, 
                        tsa_max.lag.sc = mean(watamu$tsa_max.lag.sc),rugosity_sc =0,
                        ratio_ma_coral_scaled =0, ratio_ca_ma_scaled =0, mean.biomass.scaled = 0,npp_mean_sc = 0,
                        ben_pc1_sc =0,
                        Reef2 = "Watamu") 
pred.wat <- predict(pcoa2_mod_reef , se.fit=T, newdata = newdata.wat, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
watamu <- cbind(watamu, pred.wat)





#get confidence bands manually
mombasa$upper <- mombasa$fit + 1.96*mombasa$se.fit
mombasa$lower <- mombasa$fit - 1.96*mombasa$se.fit

kisite$upper <- kisite$fit + 1.96*kisite$se.fit
kisite$lower <- kisite$fit - 1.96*kisite$se.fit

watamu$upper <- watamu$fit + 1.96*watamu$se.fit
watamu$lower <- watamu$fit - 1.96*watamu$se.fit


axis2_lag4 <- rbind(mombasa, kisite, watamu)


#get partial residuals
resid <- residuals(pcoa2_mod_reef )
resid <- resid[!is.na(resid)] + axis2_lag4$fit
axis2_lag4$fit <- axis2_lag4$fit[!is.na(axis2_lag4$fit)]
axis2_lag4$se.fit <- axis2_lag4$se.fit[!is.na(axis2_lag4$se.fit)]
#resid <- unique(resid)
axis2_lag4 <- cbind(axis2_lag4, resid)


#dataframes with model predictions
#filter for each mpa for graphs with partial resid
mombasa_axis2 <- filter(axis2_lag4, Reef2 == "Mombasa")
watamu_axis2 <- filter(axis2_lag4, Reef2 == "Watamu")
kisite_axis2 <- filter(axis2_lag4, Reef2 == "Kisite")



#pcoa 2
pcoa2 <- ggplot()+
  guides(color=guide_legend("Marine park"))+
  scale_color_manual( values = c("Mombasa" = palette[1], "Kisite" = palette[2],
                                 "Watamu" = palette[3]))+
  xlab("Time since closure (years)") + ylab("PCoA 2") +
  ggtitle("b. PCoA 2")+
  theme_bw(base_size = 22)+
  theme(text = element_text(size = 25),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  geom_line(data = mombasa_axis2, aes(x=time_since_closure, y = fit, color = Reef2), size =2, alpha = 0.5) +
  geom_ribbon(data = mombasa_axis2, aes(x=time_since_closure, y = fit, ymin=lower, ymax= upper,fill = Reef2 ), linetype=1, alpha=0.2)+
  geom_point(data = mombasa_axis2, alpha = 2, size = 3, aes(x=time_since_closure, y = resid, color = Reef2))+
  
  geom_line(data = kisite_axis2, aes(x=time_since_closure, y = fit, color = Reef2), size =2, alpha = 0.5) +
  geom_ribbon(data = kisite_axis2, aes(x=time_since_closure, y = fit, ymin=lower, ymax= upper, fill = Reef2), linetype=1, alpha=0.2)+
  geom_point(data = kisite_axis2, alpha = 2, size = 3, aes(x=time_since_closure, y = resid, color = Reef2))+
  
  geom_line(data = watamu_axis2, aes(x=time_since_closure, y = fit, color = Reef2), size =2, alpha = 0.5)+
  geom_point(data = watamu_axis2, alpha = 2, size = 3, aes(x=time_since_closure, y = resid, color = Reef2))+
  geom_ribbon(data = watamu_axis2, aes(x=time_since_closure, y = fit, ymin=lower, ymax= upper, fill = Reef2), linetype=1, alpha=0.2)


pcoa2






