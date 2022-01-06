#RUN TRAIT MODELS

#load package
library(mgcv)

#load data from source
source("Code/1 - prep trait data.R")

#size
size_mod <- gam(logit ~ Size + s(time_since_closure_scaled, by = Size, k=4, bs = "cr") +
      s(Reef2, bs = "re" ),
    #s(rugosity_sc, bs = "cr")+
    # s(npp_mean_sc, k = 3, bs = "cr")+
    #s(tsa_max.lag.sc, bs = "cr")+
    #s(ben_pc1_sc, bs = "cr"),
    correlation = corAR1(form =~ year | Reef),
    weights = total.ab,
    method="REML", family= gaussian(),
    data = size)
gam.check(size_mod)

#diet
diet_mod <- gam(logit ~ Diet + s(time_since_closure_scaled, by = Diet, k=4, bs = "cr") +
            s(Reef2, bs = "re" ) + 
            #s(rugosity_sc, bs = "cr")+
            #s(npp_mean_sc, k = 3, bs = "cr")+
            s(tsa_max.lag.sc, bs = "cr"),
          #s(ben_pc1_sc, bs = "cr"),
          correlation = corAR1(form =~ year | Reef),
          weights = total.ab,
          method="REML", family= gaussian(),
          data = diet)
gam.check(diet_mod)

#position in water column
position_mod <- gam(logit ~ Position + s(time_since_closure_scaled, by = Position, k=4, bs = "cr") +
            s(Reef2, bs = "re" ) + 
            s(rugosity_sc, bs = "cr"),
          #s(npp_mean_sc, k = 3, bs = "cr")+
          #s(tsa_max.lag.sc, bs = "cr")+
          #s(ben_pc1_sc, bs = "cr"),
          correlation = corAR1(form =~ year | Reef),
          weights = total.ab,
          method="REML", family= gaussian(),
          data = position)
gam.check(position_mod)


#schooling
schooling_mod <- gam(logit ~ Schooling + s(time_since_closure_scaled, by = Schooling, k=4, bs = "cr") +
                       s(Reef2, bs = "re" ) + 
                       #s(rugosity_sc, bs = "cr")+
                       #s(npp_mean_sc, k = 3, bs = "cr")+
                       #s(tsa_max.lag.sc, bs = "cr")+
                       s(ben_pc1_sc, bs = "cr"),
                     correlation = corAR1(form =~ year | Reef),
                     weights = total.ab,
                     method="REML", family= gaussian(),
                     data = schooling)
gam.check(schooling_mod)

#reef association 
association_mod <-  gam(logit ~ Reef.Association + s(time_since_closure_scaled, by = Reef.Association, k=4, bs = "cr") +
                          s(Reef2, bs = "re" ),
                        # s(rugosity_sc, bs = "cr"),
                        #s(npp_mean_sc, k = 3, bs = "cr")+
                        #s(tsa_max.lag.sc, bs = "cr")+
                        #s(ben_pc1_sc, bs = "cr"),
                        correlation = corAR1(form =~ year | Reef),
                        weights = total.ab,
                        method="REML", family= gaussian(),
                        data = association)
gam.check(association_mod)


#length at maturity
lm_mod <- gam(mean.lm ~ 
            s(time_since_closure_scaled, k=4, bs = "cr") + 
            s(Reef2, bs = "re" ) + 
            s(rugosity_sc, bs = "cr")+
            s(chl_sc, bs = "cr")+
            s(tsa_max.lag.sc, bs = "cr")+
            s(ben_pc1_sc, bs = "cr"),
          correlation = corAR1(form =~ year | Reef),
          method="REML", family = Gamma(link = "log"),
          data = lm)
gam.check(lm_mod)
