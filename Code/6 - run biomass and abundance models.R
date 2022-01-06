#load package
library(mgcv)

# biomass and abundance models

#load data
load("Data/abundance_av.rda")
abundance_m <- filter(abundance_m, Reef2 != "Malindi")
abundance_m$log_abun.ha <- log10(abundance_m$mean.abundance.ha)
#add chl
load(file = "Data/tsalag4_with_chl.rda")
chl <- select(tsalag4, c("Reef2", "Reef", "year", "chl", "chl_sc"))
chl <- unique(chl)
chl <- filter(chl, !is.na(Reef))
chl <- filter(chl, Reef2 != "Malindi")
chl$year <- as.integer(chl$year)
abundance_m <- left_join(abundance_m, chl)
load("Data/biomass_av.rda")
biomass_m <- filter(biomass_av, Reef2 != "Malindi")
biomass_m$log.biomass.ha <- log10(biomass_m$mean.biomass.ha)


#models
abundance_mod_reef <- gamm(log_abun.ha ~ s(time_since_closure_sc, k=4, bs = "cr", by = Reef2) +
                  s(chl_sc, bs = "cr")+
                  s(tsa_max.lag.sc, bs = "cr"),
                correlation = corAR1(form =~ year | Reef),
                # weights = varPower(form = ~ c(time_since_closure_sc)),
                family=gaussian(), method = "REML", 
                data = abundance_m)
summary(abundance_mod_reef$gam)
gam.check(abundance_mod_reef$gam)


biomass_m$log.biomass.ha <- log10(biomass_m$mean.biomass.ha)
biomass_mod_reef <- gamm(log.biomass.ha ~ s(time_since_closure_sc, k=4, bs = "cr", by = Reef2),
                #s(rugosity_sc, bs = "cr")+
                #s(npp_mean_sc, k = 3, bs = "cr")+
                #s(tsa_max.lag.sc, bs = "cr")+
                #s(ben_pc1_sc, bs = "cr"),
                weights = varPower(form = ~ c(time_since_closure_sc)),
                correlation = corAR1(form =~ year | Reef),
                family=gaussian(), method = "REML", 
                data = biomass_m)
summary(biomass_mod_reef$gam)
gam.check(biomass_mod_reef$gam)

