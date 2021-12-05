library(ggplot2)
library(RColorBrewer)
library(dplyr)


#plot biomass and abundance
source("Code/6 - run biomass and abundance models.R")

############################################## BIOMASS #############################################################
load("Data/biomass_av.rda")
biomass_m <- filter(biomass_av, Reef2 != "Malindi")
biomass_m$log.biomass.ha <- log10(biomass_m$mean.biomass.ha)

mombasa <- filter(biomass_m, Reef2 == "Mombasa")
kisite <- filter(biomass_m, Reef2 == "Kisite")
watamu <- filter(biomass_m, Reef2 == "Watamu")


newdata.mom<-data.frame(time_since_closure_sc= mombasa$time_since_closure_sc, 
                        tsa_max.lag.sc = mean(mombasa$tsa_max.lag.sc), rugosity_sc =0,
                        ratio_ma_coral_scaled =0, ratio_ca_ma_scaled =0, mean.biomass.scaled = 0,npp_mean_sc = 0,
                        ben_pc1_sc =0, chl_sc =0, 
                        Reef2 = "Mombasa") 
pred.mom <- predict(biomass_mod_reef$gam, se.fit=T, newdata = newdata.mom, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
mombasa <- cbind(mombasa, pred.mom)




newdata.kis<-data.frame(time_since_closure_sc= kisite$time_since_closure_s, 
                        tsa_max.lag.sc = mean(kisite$tsa_max.lag.sc),rugosity_sc =0,
                        ratio_ma_coral_scaled =0, ratio_ca_ma_scaled =0, mean.biomass.scaled = 0,npp_mean_sc = 0,
                        ben_pc1_sc =0,chl_sc =0, 
                        Reef2 = "Kisite") 
pred.kis <- predict(biomass_mod_reef$gam, se.fit=T, newdata = newdata.kis, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
kisite <- cbind(kisite, pred.kis)


newdata.wat<-data.frame(time_since_closure_sc= watamu$time_since_closure_sc, 
                        tsa_max.lag.sc = mean(watamu$tsa_max.lag.sc),rugosity_sc =0,
                        ratio_ma_coral_scaled =0, ratio_ca_ma_scaled =0, mean.biomass.scaled = 0,npp_mean_sc = 0,
                        ben_pc1_sc =0,chl_sc =0, 
                        Reef2 = "Watamu") 
pred.wat <- predict(biomass_mod_reef$gam, se.fit=T, newdata = newdata.wat, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
watamu <- cbind(watamu, pred.wat)





#get confidence bands manually
mombasa$upper <- mombasa$fit + 1.96*mombasa$se.fit
mombasa$lower <- mombasa$fit - 1.96*mombasa$se.fit

kisite$upper <- kisite$fit + 1.96*kisite$se.fit
kisite$lower <- kisite$fit - 1.96*kisite$se.fit

watamu$upper <- watamu$fit + 1.96*watamu$se.fit
watamu$lower <- watamu$fit - 1.96*watamu$se.fit


biomass_reef <- rbind(mombasa, kisite, watamu)

#get partial residuals
resid <- residuals(biomass_mod_reef$gam)
resid <- resid[!is.na(resid)] + biomass_reef$fit
biomass_reef$fit <- biomass_reef$fit[!is.na(biomass_reef$fit)]
biomass_reef$se.fit <- biomass_reef$se.fit[!is.na(biomass_reef$se.fit)]
biomass_reef<- cbind(biomass_reef, resid)



#get partial residuals
mombasa_biomass <-filter(biomass_reef, Reef2 == "Mombasa")
watamu_biomass <-filter(biomass_reef, Reef2 == "Watamu")
kisite_biomass <-filter(biomass_reef, Reef2 == "Kisite")




biomass_fig <- ggplot()+
  guides(color=guide_legend("Marine park"))+
  scale_color_manual( values = c("Mombasa" = palette[1], "Kisite" = palette[2],
                                 "Watamu" = palette[3]))+
  xlab("Time since closure (years)") + ylab("Biomass") +
  ggtitle("a. Biomass")+
  theme_bw(base_size = 22)+
  theme(text = element_text(size = 25),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  geom_line(data = mombasa_biomass, aes(x=time_since_closure, y = fit, color = Reef2), size =2, alpha = 0.5) +
  geom_ribbon(data = mombasa_biomass, aes(x=time_since_closure, y = fit, ymin=lower, ymax= upper,fill = Reef2 ), linetype=1, alpha=0.2)+
  geom_point(data = mombasa_biomass, alpha = 2, size = 3, aes(x=time_since_closure, y = resid, color = Reef2))+
  
  geom_line(data = kisite_biomass, aes(x=time_since_closure, y = fit, color = Reef2), size =2, alpha = 0.5) +
  geom_ribbon(data = kisite_biomass, aes(x=time_since_closure, y = fit, ymin=lower, ymax= upper, fill = Reef2), linetype=1, alpha=0.2)+
  geom_point(data = kisite_biomass, alpha = 2, size = 3, aes(x=time_since_closure, y = resid, color = Reef2))+
  
  geom_line(data = watamu_biomass, aes(x=time_since_closure, y = fit, color = Reef2), size =2, alpha = 0.5)+
  geom_ribbon(data = watamu_biomass, aes(x=time_since_closure, y = fit, ymin=lower, ymax= upper, fill = Reef2), linetype=1, alpha=0.2)+
  geom_point(data = watamu_biomass, alpha = 2, size = 3, aes(x=time_since_closure, y = resid, color = Reef2))

biomass_fig

############################################## ABUNDANCE #############################################################
load("Data/abundance_av.rda")
abundance_m <- filter(abundance_av, Reef2 != "Malindi")
abundance_m$log_abun.ha <- log10(abundance_m$mean.abundance.ha)
#add chl
load(file = "Data/tsalag4_with_chl.rda")
chl <- select(tsalag4, c("Reef2", "Reef", "year", "chl", "chl_sc"))
chl <- unique(chl)
chl <- filter(chl, !is.na(Reef))
chl <- filter(chl, Reef2 != "Malindi")
chl$year <- as.integer(chl$year)
abundance_m <- left_join(abundance_m, chl)

mombasa <- filter(abundance_m, Reef2 == "Mombasa")
kisite <- filter(abundance_m, Reef2 == "Kisite")
watamu <- filter(abundance_m, Reef2 == "Watamu")


newdata.mom<-data.frame(time_since_closure_sc= mombasa$time_since_closure_sc, 
                        tsa_max.lag.sc = mean(mombasa$tsa_max.lag.sc), rugosity_sc =0,
                        ratio_ma_coral_scaled =0, ratio_ca_ma_scaled =0, mean.biomass.scaled = 0,npp_mean_sc = 0,
                        ben_pc1_sc =0, chl_sc =0, 
                        Reef2 = "Mombasa") 
pred.mom <- predict(abundance_mod_reef$gam, se.fit=T, newdata = newdata.mom, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
mombasa <- cbind(mombasa, pred.mom)


newdata.kis<-data.frame(time_since_closure_sc= kisite$time_since_closure_s, 
                        tsa_max.lag.sc = mean(kisite$tsa_max.lag.sc),rugosity_sc =0,
                        ratio_ma_coral_scaled =0, ratio_ca_ma_scaled =0, mean.biomass.scaled = 0,npp_mean_sc = 0,
                        ben_pc1_sc =0,chl_sc =0, 
                        Reef2 = "Kisite") 
pred.kis <- predict(abundance_mod_reef$gam, se.fit=T, newdata = newdata.kis, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
kisite <- cbind(kisite, pred.kis)




newdata.wat<-data.frame(time_since_closure_sc= watamu$time_since_closure_sc, 
                        tsa_max.lag.sc = mean(watamu$tsa_max.lag.sc),rugosity_sc =0,
                        ratio_ma_coral_scaled =0, ratio_ca_ma_scaled =0, mean.biomass.scaled = 0,npp_mean_sc = 0,
                        ben_pc1_sc =0,chl_sc =0, 
                        Reef2 = "Watamu") 
pred.wat <- predict(abundance_mod_reef$gam, se.fit=T, newdata = newdata.wat, re.form=NA, type = "response") # make sure that re.form = NA and without gives diff val
watamu <- cbind(watamu, pred.wat)





#get confidence bands manually
mombasa$upper <- mombasa$fit + 1.96*mombasa$se.fit
mombasa$lower <- mombasa$fit - 1.96*mombasa$se.fit

kisite$upper <- kisite$fit + 1.96*kisite$se.fit
kisite$lower <- kisite$fit - 1.96*kisite$se.fit

watamu$upper <- watamu$fit + 1.96*watamu$se.fit
watamu$lower <- watamu$fit - 1.96*watamu$se.fit


abundance_reef <- rbind(mombasa, kisite, watamu)

#get partial residuals
resid <- residuals(abundance_mod_reef$gam)
resid <- resid[!is.na(resid)] + abundance_reef$fit
abundance_reef$fit <- abundance_reef$fit[!is.na(abundance_reef$fit)]
abundance_reef$se.fit <- abundance_reef$se.fit[!is.na(abundance_reef$se.fit)]
abundance_reef<- cbind(abundance_reef, resid)


mombasa_abundance <-filter(abundance_reef, Reef2 == "Mombasa")
watamu_abundance <-filter(abundance_reef, Reef2 == "Watamu")
kisite_abundance <-filter(abundance_reef, Reef2 == "Kisite")



abundance_fig <- ggplot()+
  guides(color=guide_legend("Marine park"))+
  scale_color_manual( values = c("Mombasa" = palette[1], "Kisite" = palette[2],
                                 "Watamu" = palette[3]))+
  xlab("Time since closure (years)") + ylab("Abundance") +
  ggtitle("b. Abundance")+
  theme_bw(base_size = 22)+
  theme(text = element_text(size = 25),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none") +
  geom_line(data = mombasa_abundance, aes(x=time_since_closure, y = fit, color = Reef2), size =2, alpha = 0.5) +
  geom_ribbon(data = mombasa_abundance, aes(x=time_since_closure, y = fit, ymin=lower, ymax= upper,fill = Reef2 ), linetype=1, alpha=0.2)+
  geom_point(data = mombasa_abundance, alpha = 2, size = 3, aes(x=time_since_closure, y = resid, color = Reef2))+
  geom_line(data = kisite_abundance, aes(x=time_since_closure, y = fit, color = Reef2), size =2, alpha = 0.5) +
  geom_ribbon(data = kisite_abundance, aes(x=time_since_closure, y = fit, ymin=lower, ymax= upper, fill = Reef2), linetype=1, alpha=0.2)+
  geom_point(data = kisite_abundance, alpha = 2, size = 3, aes(x=time_since_closure, y = resid, color = Reef2))+
  geom_line(data = watamu_abundance, aes(x=time_since_closure, y = fit, color = Reef2), size =2, alpha = 0.5)+
  geom_ribbon(data = watamu_abundance, aes(x=time_since_closure, y = fit, ymin=lower, ymax= upper, fill = Reef2), linetype=1, alpha=0.2)+
  geom_point(data = watamu_abundance, alpha = 2, size = 3, aes(x=time_since_closure, y = resid, color = Reef2))


abundance_fig
