library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(mgcv)

#load data
load("Data/tsalag4_with_chl.rda") 
#remove malindi
tsalag4 <- filter(tsalag4, Reef2 !="Malindi")


pcoa1_mod_reef <- gam(Axis.1 ~ s(time_since_closure_scaled, k=4, by=Reef2, bs = "cr") +  
                        #s(rugosity_sc, bs = "cr")+
                        #s(npp_mean_sc,  k=3, bs = "cr")+
                        s(tsa_max.lag.sc, bs = "cr"),
                      #s(ben_pc1_sc, bs = "cr"),
                      correlation = corAR1(form =~ year | Reef),
                      family=gaussian(link = "identity"), method = "REML", 
                      data = tsalag4)
pcoa2_mod_reef <- gam(Axis.2 ~ s(time_since_closure_scaled, k=4, by=Reef2, bs = "cr"),
                      #s(rugosity_sc, bs = "cr")+
                      # s(npp_mean_sc,  k=3, bs = "cr")+
                      # s(tsa_max.lag.sc, bs = "cr"),
                      #s(ben_pc1_sc, bs = "cr"),
                      correlation = corAR1(form =~ year | Reef),
                      family=gaussian(link = "identity"), method = "REML", 
                      data = tsalag4)

