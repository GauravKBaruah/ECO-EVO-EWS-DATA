# I would not recommend running this script as it takes a day or more to run the whole script. But instead I provide the 
# false positive data set for variable times series with the paper. It takes a long time due to the function that needs to create a parallel 500 time series per 1 replicate time series.
# Apart from that from those timseries, one needs to estimate A kendall's tau value which is another problem in the back! Thus almost more than 12 hrs or so. I dont exactly remember how long it took in a normall PC.




rm(list=ls())


library("earlywarnings")
source('~/Dropbox/Zurich PhD Research/2_Chapter_2/J Animal Ecology codes/Functions_J_A_Ecology.R')
source('~/Dropbox/Zurich PhD Research/2_Chapter_2/J Animal Ecology codes/surrogate_functions_variable_length.R')


extent.decline<-function(data){
  n0<-data$N[500]
  percent.decline = 0.30
  n.final<-n0*(1-percent.decline)  
  
  time.point <- 500+which(round(data$N[500:800],1) < round(n.final,1))[1]
 
  return(time.point)
}
load("EWS.genvar0.05.RData") 
load("EWS.genvar0.1.RData")  
load("EWS.genvar0.2.RData") 
load("EWS.genvar0.3.RData")
load("EWS.genvar0.4.RData")
load("EWS.genvar0.5.RData")





second.op<-EWS.trait.genvar.0.05
third.op<- EWS.trait.genvar.0.1 
fourth.op<- EWS.trait.genvar.0.2
fifth.op<-EWS.trait.genvar.0.3  
six.op<- EWS.trait.genvar.0.4
sev.op<-EWS.trait.genvar.0.5

x11<-numeric();x22<-numeric();x33<-numeric();x44<-numeric();x55<-numeric();x66<-numeric();x77<-numeric();x88<-numeric()

for ( i in 1:100){
  x22[i]<- extent.decline(second.op[[i]])
  x33[i]<-extent.decline(third.op[[i]])
  x44[i]<-extent.decline(fourth.op[[i]])
  x55[i]<-extent.decline(fifth.op[[i]])
  x66[i]<-extent.decline(six.op[[i]])
  x77[i]<-extent.decline(sev.op[[i]])
}

EWS.gen.1<-list()
EWS.gen.2<-list()
EWS.gen.3<-list()
EWS.gen.4<-list()
EWS.gen.5<-list()
EWS.gen.6<-list()
EWS.gen.7<-list()
EWS.gen.8<-list()

Tau.traitg.SD1<-numeric(); Tau.traitg.SD2<-numeric(); Tau.traitg.SD3<-numeric(); Tau.traitg.SD4<-numeric(); Tau.traitg.SD5<-numeric(); Tau.traitg.SD6<-numeric();
Tau.traitg.SD7<-numeric(); Tau.traitg.SD8<-numeric()
Tau.traitg.AR1<-numeric(); Tau.traitg.AR2<-numeric(); Tau.traitg.AR8<-numeric(); Tau.traitg.AR3<-numeric(); Tau.traitg.AR4<-numeric(); Tau.traitg.AR5<-numeric(); 
Tau.traitg.AR6<-numeric(); Tau.traitg.AR7<-numeric();
Tau.traitg.AR.SD1<-numeric(); Tau.traitg.AR.SD2<-numeric(); Tau.traitg.AR.SD3<-numeric(); Tau.traitg.AR.SD4<-numeric(); Tau.traitg.AR.SD5<-numeric(); Tau.traitg.AR.SD6<-numeric(); 
Tau.traitg.AR.SD7<-numeric(); Tau.traitg.AR.SD8<-Tau.trait2<- Tau.trait3<-Tau.trait4<-Tau.trait5<-Tau.trait6<-Tau.trait7<-Tau.trait8<-numeric();

Tau.AR.SD1<-numeric(); Tau.AR.SD2<-numeric(); Tau.AR.SD3<-numeric(); Tau.AR.SD4<-numeric(); Tau.AR.SD5<-numeric(); Tau.AR.SD6<-numeric();
Tau.AR.SD7<-numeric();  Tau.AR.SD8<-Tau.N2<-Tau.N3<-Tau.N4<-Tau.N5<-Tau.N6<-Tau.N7<-Tau.N8<-numeric()
TauSD_1<-numeric()
TauAR_1<-numeric()
TauAR_2<-numeric()
TauSD_2<-numeric()
TauSD_3<-numeric()
TauAR_3<-numeric()
TauAR_4<-numeric()
TauSD_4<-numeric()
TauAR_5<-numeric()
TauSD_5<-numeric()
TauAR_6<-numeric()
TauSD_6<-numeric()
TauAR_7<-numeric()
TauSD_7<-numeric()
TauAR_8<-numeric()
TauSD_8<-numeric()


for(i in 1:100){  tryCatch({
  Tau.traitg.AR.SD2[i]<-surrogate_ews_trait_ar1_sd(data = second.op[[i]], final.timepoint=round(mean(x22),0),
                                                   timeseries_length = "variable",winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}
for(i in 1:100){  tryCatch({
  Tau.traitg.SD2[i]<-surrogate_ews_trait_sd(data = second.op[[i]], final.timepoint=round(mean(x22),0),
                                            timeseries_length = "variable",winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}
for(i in 1:100){  tryCatch({
  Tau.traitg.AR2[i]<-surrogate_ews_trait_ar1(data = second.op[[i]], final.timepoint=round(mean(x22),0),
                                             timeseries_length = "variable",winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}
# for(i in 1:100){  tryCatch({
#   TauSD_2[i]<-surrogate_ews_m( second.op[[i]]$N[500:round(mean(x22),0)], winsize = 50, indicator= "sd",detrending = "gaussian", boots =500)$false_p
# }, error=function(e){})}
# for(i in 1:100){  tryCatch({
#   TauAR_2[i]<-surrogate_ews_m( second.op[[i]]$N[500:round(mean(x22),0)], winsize = 50, indicator = "ar1",detrending = "gaussian", boots =500)$false_p
# }, error=function(e){})}
for(i in 1:100){  tryCatch({
  Tau.AR.SD2[i]<-surrogate_ews_sd_ar1(data = second.op[[i]], final.timepoint=round(mean(x22),0),
                                      timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}
#Tau.trait2[i]<-mean(EWS.gen.2[[i]]$Tau.Trait)
#Tau.N2[i]<-mean(EWS.gen.2[[i]]$Tau.N)

for(i in 1:100){  tryCatch({
  Tau.traitg.AR.SD3[i]<-surrogate_ews_trait_ar1_sd(data = third.op[[i]],final.timepoint=round(mean(x33),0),
                                                   timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}
for(i in 1:100){  tryCatch({
  Tau.traitg.AR3[i]<-surrogate_ews_trait_ar1(data = third.op[[i]],final.timepoint=round(mean(x33),0),
                                             timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}
for(i in 1:100){  tryCatch({
  Tau.traitg.SD3[i]<-surrogate_ews_trait_sd(data = third.op[[i]],final.timepoint=round(mean(x33),0),
                                            timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}
# for(i in 1:100){  tryCatch({
#   TauSD_3[i]<-surrogate_ews_m( third.op[[i]]$N[500:round(mean(x33),0)], winsize = 50, indicator= "sd",detrending = "gaussian", boots =500)$false_p
# }, error=function(e){})}
# for(i in 1:100){  tryCatch({
#   TauAR_3[i]<-surrogate_ews_m(third.op[[i]]$N[500:round(mean(x33),0)], winsize = 50, indicator = "ar1",detrending = "gaussian", boots =500)$false_p
# }, error=function(e){})}

for(i in 1:100){  tryCatch({
  Tau.AR.SD3[i]<-surrogate_ews_sd_ar1(data = third.op[[i]],final.timepoint=round(mean(x33),0),
                                      timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}


#Tau.trait3[i]<-mean(EWS.gen.2[[i]]$Tau.Trait)
#Tau.N3[i]<-mean(EWS.gen.2[[i]]$Tau.N)


for(i in 1:100){  tryCatch({
  Tau.traitg.AR.SD4[i]<-surrogate_ews_trait_ar1_sd(data = fourth.op[[i]],final.timepoint=round(mean(x44),0),
                                                   timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p 
}, error=function(e){})}
for(i in 1:100){  tryCatch({
  Tau.traitg.AR4[i]<-surrogate_ews_trait_ar1(data = fourth.op[[i]],final.timepoint=round(mean(x44),0),
                                             timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}
for(i in 1:100){  tryCatch({
  Tau.traitg.SD4[i]<-surrogate_ews_trait_sd(data = fourth.op[[i]], final.timepoint=round(mean(x44),0),
                                            timeseries_length = "variable",winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}
# for(i in 1:100){  tryCatch({
#   TauSD_4[i]<-surrogate_ews_m(fourth.op[[i]]$N[500:round(mean(x44),0)], winsize = 50, indicator= "sd",detrending = "gaussian", boots =500)$false_p
# }, error=function(e){})}
# for(i in 1:100){  tryCatch({
#   TauAR_4[i]<-surrogate_ews_m(fourth.op[[i]]$N[500:round(mean(x44),0)], winsize = 50, indicator = "ar1",detrending = "gaussian", boots =500)$false_p
# }, error=function(e){})}
for(i in 1:100){  tryCatch({
  Tau.AR.SD4[i]<-surrogate_ews_sd_ar1(data = fourth.op[[i]], final.timepoint=round(mean(x44),0),
                                      timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}

#Tau.trait4[i]<-mean(EWS.gen.2[[i]]$Tau.Trait)
#Tau.N4[i]<-mean(EWS.gen.2[[i]]$Tau.N)
for(i in 1:100){  tryCatch({
  Tau.traitg.AR.SD5[i]<-surrogate_ews_trait_ar1_sd(data = fifth.op[[i]],final.timepoint=round(mean(x55),0),
                                                   timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}
for(i in 1:100){  tryCatch({
  Tau.traitg.AR5[i]<-surrogate_ews_trait_ar1(data = fifth.op[[i]], final.timepoint=round(mean(x55),0),
                                             timeseries_length = "variable",winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}
for(i in 1:100){  tryCatch({
  Tau.traitg.SD5[i]<-surrogate_ews_trait_sd(data = fifth.op[[i]],final.timepoint=round(mean(x55),0),
                                            timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}
# for(i in 1:100){  tryCatch({
#   TauSD_5[i]<-surrogate_ews_m(fifth.op[[i]]$N[500:round(mean(x55),0)], winsize = 50, indicator= "sd",detrending = "gaussian", boots =500)$false_p
# }, error=function(e){})}
# for(i in 1:100){  tryCatch({
#   TauAR_5[i]<-surrogate_ews_m(fifth.op[[i]]$N[500:round(mean(x55),0)], winsize = 50, indicator = "ar1",detrending = "gaussian", boots =500)$false_p
# }, error=function(e){})}
for(i in 1:100){  tryCatch({
  Tau.AR.SD5[i]<-surrogate_ews_sd_ar1(data = fifth.op[[i]],final.timepoint=round(mean(x55),0),
                                      timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}

#Tau.trait5[i]<-mean(EWS.gen.2[[i]]$Tau.Trait)
#Tau.N5[i]<-mean(EWS.gen.2[[i]]$Tau.N)


for(i in 1:100){  tryCatch({
  Tau.traitg.AR.SD6[i]<-surrogate_ews_trait_ar1_sd(data = six.op[[i]],final.timepoint=round(mean(x66),0),
                                                   timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}
for(i in 1:100){  tryCatch({
  Tau.traitg.AR6[i]<-surrogate_ews_trait_ar1(data = six.op[[i]],final.timepoint=round(mean(x66),0),
                                             timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}
for(i in 1:100){  tryCatch({
  Tau.traitg.SD6[i]<-surrogate_ews_trait_sd(data = six.op[[i]], final.timepoint=round(mean(x66),0),
                                            timeseries_length = "variable",winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}
# for(i in 1:100){  tryCatch({
#   TauSD_6[i]<-surrogate_ews_m(six.op[[i]]$N[500:round(mean(x66),0)], winsize = 50, indicator= "sd",detrending = "gaussian", boots =500)$false_p
# }, error=function(e){})}
# for(i in 1:100){  tryCatch({
#   TauAR_6[i]<-surrogate_ews_m(six.op[[i]]$N[500:round(mean(x66),0)], winsize = 50, indicator = "ar1",detrending = "gaussian", boots =500)$false_p
# }, error=function(e){})}
for(i in 1:100){  tryCatch({
  Tau.AR.SD6[i]<-surrogate_ews_sd_ar1(data = six.op[[i]],final.timepoint=round(mean(x66),0),
                                      timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
}, error=function(e){})}


for(i in 1:100){  
  tryCatch({
    Tau.traitg.AR.SD7[i]<-surrogate_ews_trait_ar1_sd(data = sev.op[[i]], final.timepoint=round(mean(x77),0),
                                                     timeseries_length = "variable",winsize = 50, detrending = "gaussian", boots =500)$false_p 
  }, error=function(e){})}
for(i in 1:100){  
  tryCatch({
    Tau.traitg.AR7[i]<-surrogate_ews_trait_sd(data = sev.op[[i]], final.timepoint=round(mean(x77),0),
                                              timeseries_length = "variable",winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}
for(i in 1:100){  
  tryCatch({
    Tau.traitg.SD7[i]<-surrogate_ews_trait_ar1(data = sev.op[[i]], final.timepoint=round(mean(x77),0),
                                               timeseries_length = "variable",winsize = 50, detrending = "gaussian", boots =500)$false_p
  print(i)}, error=function(e){})}
for(i in 1:100){
  tryCatch({
    TauSD_7[i]<-surrogate_ews_m(sev.op[[i]]$N[500:round(mean(x77),0)], winsize = 50, indicator= "sd",detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}
for(i in 1:100){
  tryCatch({
    TauAR_7[i]<-surrogate_ews_m(sev.op[[i]]$N[500:round(mean(x77),0)], winsize = 50, indicator = "ar1",detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}
for(i in 1:100){
  tryCatch({
    Tau.AR.SD7[i]<-surrogate_ews_sd_ar1(data = sev.op[[i]], final.timepoint=round(mean(x77),0),
                                        timeseries_length = "variable",winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}



library(wesanderson)

best_color_paletter<- c(wes_palettes$Darjeeling1, wes_palettes$Rushmore1)


# 
# a
var.0.05<-data.frame(Kendall.tau=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitg.AR2,Tau.traitg.SD2,Tau.traitg.AR.SD2),
                     value=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitg.AR2,Tau.traitg.SD2,Tau.traitg.AR.SD2),
                     variation=factor(c(rep("AR1",each=length(TauAR_2)),
                                        rep("SD",each=length(TauSD_2)),
                                        rep("AR1+SD",each=length(Tau.AR.SD2)),
                                        rep("AR1+Tr",each=length(Tau.traitg.AR2)),
                                        rep("SD+Tr",each=length(Tau.traitg.SD2)),
                                        rep("AR1+SD+Tr",each=length(Tau.traitg.AR.SD2)))))

var.0.05$variation<-factor(var.0.05$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )
# a
var.0.1<-data.frame(Kendall.tau=c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitg.AR3,Tau.traitg.SD3,Tau.traitg.AR.SD3),
                    value=c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitg.AR3,Tau.traitg.SD3,Tau.traitg.AR.SD3),
                    variation=factor(c(rep("AR1",each=length(TauAR_3)),
                                       rep("SD",each=length(TauSD_3)),
                                       rep("AR1+SD",each=length(Tau.AR.SD3)),
                                       rep("AR1+Tr",each=length(Tau.traitg.AR3)),
                                       rep("SD+Tr",each=length(Tau.traitg.SD3)),
                                       rep("AR1+SD+Tr",each=length(Tau.traitg.AR.SD3)))))
var.0.1$variation<-factor(var.0.1$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
var.0.2<-data.frame(Kendall.tau=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitg.AR4,Tau.traitg.SD4,Tau.traitg.AR.SD4),
                    value=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitg.AR4,Tau.traitg.SD4,Tau.traitg.AR.SD4),
                    variation=factor(c(rep("AR1",each=length(TauAR_4)),
                                       rep("SD",each=length(TauSD_4)),
                                       rep("AR1+SD",each=length(Tau.AR.SD4)),
                                       rep("AR1+Tr",each=length(Tau.traitg.AR4)),
                                       rep("SD+Tr",each=length(Tau.traitg.SD4)),
                                       rep("AR1+SD+Tr",each=length(Tau.traitg.AR.SD4)))))
var.0.2$variation<-factor(var.0.2$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
var.0.3<-data.frame(Kendall.tau=c(TauAR_5,TauSD_5,Tau.AR.SD5,Tau.traitg.AR5,Tau.traitg.SD5,Tau.traitg.AR.SD5),
                    value=c(TauAR_5,TauSD_5,Tau.AR.SD5,Tau.traitg.AR5,Tau.traitg.SD5,Tau.traitg.AR.SD5),
                    variation=factor(c(rep("AR1",each=length(TauAR_5)),
                                       rep("SD",each=length(TauSD_5)),
                                       rep("AR1+SD",each=length(Tau.AR.SD5)),
                                       rep("AR1+Tr",each=length(Tau.traitg.AR5)),
                                       rep("SD+Tr",each=length(Tau.traitg.SD5)),
                                       rep("AR1+SD+Tr",each=length(Tau.traitg.AR.SD5)))))
var.0.3$variation<-factor(var.0.3$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
var.0.4<-data.frame(Kendall.tau=c(TauAR_6,TauSD_6,Tau.AR.SD6,Tau.traitg.AR6,Tau.traitg.SD6,Tau.traitg.AR.SD6),
                    value=c(TauAR_6,TauSD_6,Tau.AR.SD6,Tau.traitg.AR6,Tau.traitg.SD6,Tau.traitg.AR.SD6),
                    variation=factor(c(rep("AR1",each=length(TauAR_6)),
                                       rep("SD",each=length(TauSD_6)),
                                       rep("AR1+SD",each=length(Tau.AR.SD6)),
                                       rep("AR1+Tr",each=length(Tau.traitg.AR6)),
                                       rep("SD+Tr",each=length(Tau.traitg.SD6)),
                                       rep("AR1+SD+Tr",each=length(Tau.traitg.AR.SD6)))))
var.0.4$variation<-factor(var.0.4$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
var.0.5<-data.frame(Kendall.tau=c(TauAR_7,TauSD_7,Tau.AR.SD7,Tau.traitg.AR7,Tau.traitg.SD7,Tau.traitg.AR.SD7),
                    value=c(TauAR_7,TauSD_7,Tau.AR.SD7,Tau.traitg.AR7,Tau.traitg.SD7,Tau.traitg.AR.SD7),
                    variation=factor(c(rep("AR1",each=length(TauAR_7)),
                                       rep("SD",each=length(TauSD_7)),
                                       rep("AR1+SD",each=length(Tau.AR.SD7)),
                                       rep("AR1+Tr",each=length(Tau.traitg.AR7)),
                                       rep("SD+Tr",each=length(Tau.traitg.SD7)),
                                       rep("AR1+SD+Tr",each=length(Tau.traitg.AR.SD7)))))
var.0.5$variation<-factor(var.0.5$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )


#a



data.g.variable.timeseries.FP<-data.frame(rbind(var.0.05,var.0.1, var.0.2, var.0.3,var.0.4,var.0.5),
                      Genetic_variation =c(rep("0.05",each=nrow(var.0.05)), rep("0.1", each=nrow(var.0.1)),
                                           rep("0.2",each=nrow(var.0.2)), rep("0.3",each=nrow(var.0.3)), 
                                           rep("0.4",each=nrow(var.0.4)),rep( "0.5",each=nrow(var.0.5))) )

save(data.g.variable.timeseries.FP, file="FP_variable_timeseries_genvar.RData")
best_color_paletter<-c("#A42820", "#5F5647",  "#E58601",  "#00A08A" ,
                       "#4E2A1E","#000066", "#0C1707", "#000066")

g1.fp.time<-ggplot(data= data.g.variable.timeseries.FP, aes(y = Kendall.tau,x = variation, color= variation )) +geom_boxplot(alpha=0)+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ggtitle("Genetic variation")+ylim(c(0,1))+
  ylab("Rate of False positives")+
  theme_classic()+theme(plot.title = element_text(size = 14,   face = "bold"),text = element_text(size = 12 ),
                        axis.title = element_text(face="bold"),axis.text.x=element_blank(),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="EWS metric")+
  facet_wrap(.~Genetic_variation)+xlab("")
g1.fp.time

#plotting false positives for Genetic variation





################################  Reproduction  rate data #################

#rm(list=ls())
load("EWS.reproduction1.1.RData")
load("EWS.reproduction1.2.RData") 
load("EWS.reproduction1.3.RData")  
load("EWS.reproduction1.4.RData")                       
load("EWS.reproduction1.5.RData")

library("earlywarnings")



first.R0<-EWS.trait.reproduction_1.1
second.R0<-EWS.trait.reproduction_1.2
third.R0<- EWS.trait.reproduction_1.3 
fourth.R0<- EWS.trait.reproduction_1.4
fifth.R0<-EWS.trait.reproduction_1.5

reps1<-length(first.R0)
x1<-numeric();x2<-numeric();x3<-numeric();x4<-numeric();x5<-numeric();x6<-numeric();x7<-numeric();x8<-numeric()
# getting the bifurcation time out

for ( i in 1:100){
  
  x1[i]<-extent.decline(first.R0[[i]])
  x2[i]<-extent.decline(second.R0[[i]])
  x3[i]<-extent.decline(third.R0[[i]])
  x4[i]<-extent.decline(fourth.R0[[i]])
  x5[i]<-extent.decline(fifth.R0[[i]])
  
  
}

EWSR0.1<-list()
EWSR0.2<-list()
EWSR0.3<-list()
EWSR0.4<-list()
EWSR0.5<-list()
Tau.traitR.SD1<-numeric(); Tau.traitR.SD2<-numeric(); Tau.traitR.SD3<-numeric(); Tau.traitR.SD4<-numeric(); Tau.traitR.SD5<-numeric(); Tau.traitR.SD6<-numeric();
Tau.traitR.SD7<-numeric(); Tau.traitR.SD8<-numeric()
Tau.traitR.AR1<-numeric(); Tau.traitR.AR2<-numeric(); Tau.traitR.AR8<-numeric(); Tau.traitR.AR3<-numeric(); Tau.traitR.AR4<-numeric(); Tau.traitR.AR5<-numeric(); 
Tau.traitR.AR6<-numeric(); Tau.traitR.AR7<-numeric();
Tau.traitR.AR.SD1<-numeric(); Tau.traitR.AR.SD2<-numeric(); Tau.traitR.AR.SD3<-numeric(); Tau.traitR.AR.SD4<-numeric(); Tau.traitR.AR.SD5<-numeric(); Tau.traitR.AR.SD6<-numeric(); 
Tau.traitR.AR.SD7<-numeric(); Tau.traitR.AR.SD8<-numeric();

Tau.AR.SD1<-numeric(); Tau.AR.SD2<-numeric(); Tau.AR.SD3<-numeric(); Tau.AR.SD4<-numeric(); Tau.AR.SD5<-numeric(); Tau.AR.SD6<-numeric();
Tau.AR.SD7<-numeric(); Tau.AR.SD8<-numeric()

TauSD_1<-numeric(); Sk1<-numeric();Sk2<-numeric();sk3<-numeric();sk3<-numeric();sk4<-numeric();sk5<-numeric();
TauAR_1<-numeric();rr1<-numeric();rr2<-numeric();rr3<-numeric();rr4<-numeric();rr5<-numeric();
TauAR_2<-numeric();dr1<-numeric();dr2<-numeric();dr3<-numeric();dr4<-numeric();dr5<-numeric();
TauSD_2<-numeric();
TauSD_3<-numeric()
TauAR_3<-numeric()
TauAR_4<-numeric()
TauSD_4<-numeric()
TauAR_5<-numeric()
TauSD_5<-numeric()

for ( i in 1:100){
  tryCatch({
    Tau.traitR.AR.SD1[i]<-surrogate_ews_trait_ar1_sd(data = first.R0[[i]],final.timepoint=round(mean(x1),0),
                                                     timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){}) 
  print(i)}

for(i in 1: 100 ){  
  tryCatch({
    Tau.traitR.SD1[i]<-surrogate_ews_trait_sd(data = first.R0[[i]],final.timepoint=round(mean(x1),0),
                                              timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
    
  }, error=function(e){}) }

for(i in 1:100){ 
  tryCatch({
    Tau.traitR.AR1[i]<-surrogate_ews_trait_ar1(data = first.R0[[i]],final.timepoint=round(mean(x1),0),
                                               timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p 
  }, error=function(e){}) 
}
for(i in 1:100){  
  tryCatch({
    TauSD_1[i]<-surrogate_ews_m( first.R0[[i]]$N[500:round(mean(x1),0)], "sd",winsize = 50, detrending = "gaussian", boots =500)$false_p 
  }, error=function(e){}) }
for(i in 1:100){
  tryCatch({
    TauAR_1[i]<-surrogate_ews_m( first.R0[[i]]$N[500:round(mean(x1),0)],"ar1", winsize = 50, detrending = "gaussian", boots =500)$false_p 
  }, error=function(e){}) }
for ( i in 1:100){
  tryCatch({
    Tau.AR.SD1[i]<-surrogate_ews_sd_ar1(data = first.R0[[i]],final.timepoint=round(mean(x1),0),
                                        timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
  print(i)}

#dr2[i]<-mean(EWSR0.2[[i]]$KTauDR)



# dr1[i]<-mean(EWSR0.1[[i]]$KTauDR)
for ( i in 1:100){
  tryCatch({
    Tau.traitR.AR.SD2[i]<-surrogate_ews_trait_ar1_sd(data = second.R0[[i]],final.timepoint=round(mean(x2),0),
                                                     timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){}) 
  print(i)}

for(i in 1: 100 ){  
  tryCatch({
    Tau.traitR.SD2[i]<-surrogate_ews_trait_sd(data = second.R0[[i]],final.timepoint=round(mean(x2),0),
                                              timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
    
  }, error=function(e){}) }

for(i in 1:100){ 
  tryCatch({
    Tau.traitR.AR2[i]<-surrogate_ews_trait_ar1(data = second.R0[[i]],final.timepoint=round(mean(x2),0),
                                               timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p 
  }, error=function(e){}) 
}
for(i in 1:100){  
  tryCatch({
    TauSD_2[i]<-surrogate_ews_m( second.R0[[i]]$N[500:round(mean(x2),0)], "sd",winsize = 50, detrending = "gaussian", boots =500)$false_p 
  }, error=function(e){}) }
for(i in 1:100){
  tryCatch({
    TauAR_2[i]<-surrogate_ews_m( second.R0[[i]]$N[500:round(mean(x2),0)],"ar1", winsize = 50, detrending = "gaussian", boots =500)$false_p 
  }, error=function(e){}) }
for ( i in 1:100){
  tryCatch({
    Tau.AR.SD2[i]<-surrogate_ews_sd_ar1(data = second.R0[[i]],final.timepoint=round(mean(x2),0),
                                        timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
  print(i)}

#dr2[i]<-mean(EWSR0.2[[i]]$KTauDR)

for ( i in 1:100){
  tryCatch({
    Tau.traitR.AR.SD3[i]<-surrogate_ews_trait_ar1_sd(data = third.R0[[i]],final.timepoint=round(mean(x3),0),
                                                     timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}

for ( i in 1:100){  
  tryCatch({
    Tau.traitR.AR3[i]<-surrogate_ews_trait_ar1(data = third.R0[[i]],final.timepoint=round(mean(x3),0),
                                               timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}
for ( i in 1:100){
  tryCatch({
    Tau.traitR.SD3[i]<-surrogate_ews_trait_sd(data = third.R0[[i]],final.timepoint=round(mean(x3),0),
                                              timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}
for ( i in 1:100){
  tryCatch({
    TauSD_3[i]<-surrogate_ews_m(third.R0[[i]]$N[500:round(mean(x3),0)],"sd", winsize = 50, detrending = "gaussian", boots =500)$false_p 
  }, error=function(e){})}
for ( i in 1:100){
  tryCatch({
    TauAR_3[i]<-surrogate_ews_m( third.R0[[i]]$N[500:round(mean(x3),0)],"ar1", winsize = 50, detrending = "gaussian", boots =500)$false_p }, error=function(e){})}
for ( i in 1:100){
  tryCatch({
    Tau.AR.SD3[i]<-surrogate_ews_sd_ar1( third.R0[[i]], winsize = 50, final.timepoint=round(mean(x2),0),
                                         timeseries_length = "variable",detrending = "gaussian", boots =500)$false_p }, error=function(e){})}
#dr3[i]<-mean(EWSR0.3[[i]]$KTauDR)

for ( i in 1:100){
  tryCatch({
    Tau.traitR.AR.SD4[i]<-surrogate_ews_trait_ar1_sd(data = fourth.R0[[i]], final.timepoint=round(mean(x4),0),
                                                     timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p }, error=function(e){})}
for ( i in 1:100){
  tryCatch({
    Tau.traitR.AR4[i]<-surrogate_ews_trait_ar1(data = fourth.R0[[i]], final.timepoint=round(mean(x4),0),
                                               timeseries_length = "variable",winsize = 50, detrending = "gaussian", boots =500)$false_p }, error=function(e){})}
for ( i in 1:100){
  tryCatch({
    Tau.traitR.SD4[i]<-surrogate_ews_trait_sd(data = fourth.R0[[i]],final.timepoint=round(mean(x4),0),
                                              timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p }, error=function(e){})}
for ( i in 1:100){
  tryCatch({
    TauSD_4[i]<-surrogate_ews_m(fourth.R0[[i]]$N[500:round(mean(x4),0)],"sd", winsize = 50, detrending = "gaussian", boots =500)$false_p }, error=function(e){})}
for ( i in 1:100){
  tryCatch({
    TauAR_4[i]<-surrogate_ews_m( fourth.R0[[i]]$N[500:round(mean(x4),0)],"ar1", winsize = 50, detrending = "gaussian", boots =500)$false_p }, error=function(e){})}
for ( i in 1:100){
  tryCatch({
    Tau.AR.SD4[i]<-surrogate_ews_sd_ar1( fourth.R0[[i]], winsize = 50, final.timepoint=round(mean(x4),0),
                                         timeseries_length = "variable",detrending = "gaussian", boots =500)$false_p }, error=function(e){})}
#dr3[i]<-mean(EWSR0.3[[i]]$KTauDR)

#dr4[i]<-mean(EWSR0.4[[i]]$KTauDR)
for ( i in 1:100){
  tryCatch({
    Tau.traitR.AR.SD5[i]<-surrogate_ews_trait_ar1_sd(data = fifth.R0[[i]], final.timepoint=round(mean(x5),0),
                                                     timeseries_length = "variable",winsize = 50, detrending = "gaussian", boots =500)$false_p }, error=function(e){})}
for ( i in 1:100){
  tryCatch({
    Tau.traitR.AR5[i]<-surrogate_ews_trait_ar1(data = fifth.R0[[i]], final.timepoint=round(mean(x5),0),
                                               timeseries_length = "variable",winsize = 50, detrending = "gaussian", boots =500)$false_p }, error=function(e){})}
for ( i in 1:100){
  tryCatch({
    Tau.traitR.SD5[i]<-surrogate_ews_trait_sd(data = fifth.R0[[i]], final.timepoint=round(mean(x5),0),
                                              timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p }, error=function(e){})}
for ( i in 1:100){
  tryCatch({
    TauSD_5[i]<-surrogate_ews_m(fifth.R0[[i]]$N[500:x5[i]],"sd", winsize = 50, detrending = "gaussian", boots =500)$false_p }, error=function(e){})}
for ( i in 1:100){
  tryCatch({
    TauAR_5[i]<-surrogate_ews_m( fifth.R0[[i]]$N[500:x5[i]],"ar1", winsize = 50, detrending = "gaussian", boots =500)$false_p }, error=function(e){})}
for ( i in 1:100){
  tryCatch({
    Tau.AR.SD5[i]<-surrogate_ews_sd_ar1( fifth.R0[[i]],final.timepoint=round(mean(x5),0),
                                         timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p }, error=function(e){})
  print(i)}

#dr5[i]<-mean(EWSR0.5[[i]]$KTauDR)


#Tau.trait5[i]<-mean(EWSR0.5[[i]]$Tau.Trait)
#Tau.N5[i]<-mean(EWSR0.5[[i]]$Tau.N)


library(wesanderson)


best_color_paletter<- c(wes_palettes$Darjeeling1, wes_palettes$Rushmore1)


R1<-data.frame(Kendall.tau=c(TauAR_1,TauSD_1,Tau.AR.SD1,Tau.traitR.AR1,Tau.traitR.SD1,Tau.traitR.AR.SD1),
               value=c(TauAR_1,TauSD_1,Tau.AR.SD1,Tau.traitR.AR1,Tau.traitR.SD1,Tau.traitR.AR.SD1),
               variation=factor(c(rep("AR1", each= length(TauAR_1)),rep("SD",each=length(TauSD_1)),
                                  rep("AR1+SD", each=length(Tau.AR.SD1)),rep("AR1+Tr",each=length(Tau.traitR.AR1)),
                                  rep("SD+Tr",each=length(Tau.traitR.SD1)),rep("AR1+SD+Tr", each= length(Tau.traitR.AR.SD1)))))
R1$variation<-factor(R1$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
R2<-data.frame(Kendall.tau=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitR.AR2,Tau.traitR.SD2,Tau.traitR.AR.SD2),
               value=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitR.AR2,Tau.traitR.SD2,Tau.traitR.AR.SD2),
               variation=factor(c(rep("AR1", each= length(TauAR_2)),rep("SD",each=length(TauSD_2)),
                                  rep("AR1+SD", each=length(Tau.AR.SD2)),rep("AR1+Tr",each=length(Tau.traitR.AR2)),
                                  rep("SD+Tr",each=length(Tau.traitR.SD2)),rep("AR1+SD+Tr", each= length(Tau.traitR.AR.SD2)))))
R2$variation<-factor(R2$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )



# a
R3<-data.frame(Kendall.tau=c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitR.AR3,Tau.traitR.SD3,Tau.traitR.AR.SD3),
               value=c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitR.AR3,Tau.traitR.SD3,Tau.traitR.AR.SD3),
               variation=factor(c(rep("AR1", each= length(TauAR_3)),rep("SD",each=length(TauSD_3)),
                                  rep("AR1+SD", each=length(Tau.AR.SD3)),rep("AR1+Tr",each=length(Tau.traitR.AR3)),
                                  rep("SD+Tr",each=length(Tau.traitR.SD3)),rep("AR1+SD+Tr", each= length(Tau.traitR.AR.SD3)))))
R3$variation<-factor(R3$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
R4<-data.frame(Kendall.tau=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitR.AR4,Tau.traitR.SD4,Tau.traitR.AR.SD4),
               value=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitR.AR4,Tau.traitR.SD4,Tau.traitR.AR.SD4),
               variation=factor(c(rep("AR1", each= length(TauAR_4)),rep("SD",each=length(TauSD_4)),
                                  rep("AR1+SD", each=length(Tau.AR.SD4)),rep("AR1+Tr",each=length(Tau.traitR.AR4)),
                                  rep("SD+Tr",each=length(Tau.traitR.SD4)),rep("AR1+SD+Tr", each= length(Tau.traitR.AR.SD4)))))
R4$variation<-factor(R4$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
R5<-data.frame(Kendall.tau=c(TauAR_5,TauSD_5,Tau.AR.SD5,Tau.traitR.AR5,Tau.traitR.SD5,Tau.traitR.AR.SD5),
               value=c(TauAR_5,TauSD_5,Tau.AR.SD5,Tau.traitR.AR5,Tau.traitR.SD5,Tau.traitR.AR.SD5),
               variation=factor(c(rep("AR1", each= length(TauAR_5)),rep("SD",each=length(TauSD_5)),
                                  rep("AR1+SD", each=length(Tau.AR.SD5)),rep("AR1+Tr",each=length(Tau.traitR.AR5)),
                                  rep("SD+Tr",each=length(Tau.traitR.SD5)),rep("AR1+SD+Tr", each= length(Tau.traitR.AR.SD5)))))

R5$variation<-factor(R5$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )




data.R.falsepositives_variabletime<-data.frame(rbind(R1,R2, R3, R4,R5),
                                  R0 =c(rep("1.1",each=nrow(R1)), rep("1.2", each=nrow(R2)),rep("1.3",each=nrow(R3)), rep("1.4",each=nrow(R4)), 
                                        rep("1.5",each=nrow(R5))) )


save(data.R.falsepositives_variabletime, file = "False.positives.variable.timeseries.R0.RData")

best_color_paletter<-c("#A42820", "#5F5647",  "#E58601",  "#00A08A" ,
                       "#4E2A1E","#000066", "#0C1707", "#000066")

r1<-ggplot(data= data.R.falsepositives_variabletime, aes(y = Kendall.tau,x = variation, color= variation )) +geom_boxplot(alpha=0)+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ggtitle("Net Reproductive rate")+ylim(c(0,1))+
  ylab("Rate of false positives")+
  theme_classic()+theme(plot.title = element_text(size = 14,   face = "bold"),text = element_text(size = 12 ),
                        axis.title = element_text(face="bold"),axis.text.x=element_blank(),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="EWS metric")+
  facet_wrap(.~R0)+xlab("")
r1




############################## Plasticity Data ##########################


load("EWS.plasticity.0.05.RData") 
load("EWS.plasticity.0.1.RData")  
load("EWS.plasticity.0.2.RData")      
load("EWS.plasticity.0.3.RData") 
load("EWS.plasticity.0.4.RData")
load("EWS.plasticity.0.5.RData")
load("EWS.plasticity.0.8.RData")

library("earlywarnings")



second.p<-EWS.trait.plasticity0.05
third.p<- EWS.trait.plasticity0.1
fourth.p<- EWS.trait.plasticity0.2
fifth.p<-EWS.trait.plasticity0.3
six.p<- EWS.trait.plasticity0.4
sev.p<-EWS.trait.plasticity0.5
e.p<-EWS.trait.plasticity0.8
reps1<-length(second.p)
# 
xp1<-xp2<-xp3<-xp4<-xp5<-xp6<-xp7<-xp8<-numeric()
for ( i in 1:100){
  xp1[i]<- extent.decline(second.op[[i]])
  xp2[i]<-extent.decline(third.op[[i]])
  xp3[i]<-extent.decline(fourth.op[[i]])
  xp4[i]<-extent.decline(fifth.op[[i]])
  xp5[i]<-extent.decline(six.op[[i]])
  xp6[i]<-extent.decline(sev.op[[i]])
  xp7[i]<-extent.decline(e.p[[i]])
}
EWS.p.1<-list()
EWS.p.2<-list()
EWS.p.3<-list()
EWS.p.4<-list()
EWS.p.5<-list()
EWS.p.6<-list()
EWS.p.7<-list()
EWS.p.8<-list()
Tau.traitP.SD1<-numeric(); Tau.traitP.SD2<-numeric(); Tau.traitP.SD3<-numeric(); Tau.traitP.SD4<-numeric(); Tau.traitP.SD5<-numeric(); Tau.traitP.SD6<-numeric();
Tau.traitP.SD7<-numeric(); Tau.traitP.SD8<-numeric()
Tau.traitP.AR1<-numeric(); Tau.traitP.AR2<-numeric(); Tau.traitP.AR8<-numeric(); Tau.traitP.AR3<-numeric(); Tau.traitP.AR4<-numeric(); Tau.traitP.AR5<-numeric(); 
Tau.traitP.AR6<-numeric(); Tau.traitP.AR7<-numeric();
Tau.traitP.AR.SD1<-numeric(); Tau.traitP.AR.SD2<-numeric(); Tau.traitP.AR.SD3<-numeric(); Tau.traitP.AR.SD4<-numeric(); Tau.traitP.AR.SD5<-numeric(); Tau.traitP.AR.SD6<-numeric(); 
Tau.traitP.AR.SD7<-numeric(); Tau.traitP.AR.SD8<-numeric();

Tau.AR.SD1<-numeric(); Tau.AR.SD2<-numeric(); Tau.AR.SD3<-numeric(); Tau.AR.SD4<-numeric(); Tau.AR.SD5<-numeric(); Tau.AR.SD6<-numeric();
Tau.AR.SD7<-numeric(); Tau.AR.SD8<-numeric()

TauSD_1<-numeric()
TauAR_1<-numeric()
TauAR_2<-numeric()
TauSD_2<-numeric()
TauSD_3<-numeric()
TauAR_3<-numeric()
TauAR_4<-numeric()
TauSD_4<-numeric()
TauAR_5<-numeric()
TauSD_5<-numeric()
TauAR_6<-numeric()
TauSD_6<-numeric()
TauAR_7<-numeric()
TauSD_7<-numeric()




# dr1[i]<-mean(EWS.p.1[[i]]$KTauDR)
for(i in 1:100){
  tryCatch({ 
    Tau.traitP.AR.SD2[i]<-surrogate_ews_trait_ar1_sd(data = second.p[[i]],final.timepoint=round(mean(xp1),0),
                                                     timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}

for(i in 1:100){
  tryCatch({ 
    Tau.traitP.SD2[i]<-surrogate_ews_trait_sd(data = second.p[[i]], final.timepoint=round(mean(xp1),0),
                                              timeseries_length = "variable",winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}

for(i in 1:100){
  tryCatch({ 
    Tau.traitP.AR2[i]<-surrogate_ews_trait_ar1(data = second.p[[i]],final.timepoint=round(mean(xp1),0),
                                               timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}

for(i in 1:100){
  tryCatch({ 
    TauSD_2[i]<-surrogate_ews_m(second.p[[i]]$N[500:round(mean(xp1),0)],"sd", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}
for(i in 1:100){
  tryCatch({ 
    TauAR_2[i]<-surrogate_ews_m( second.p[[i]]$N[500:round(mean(xp1),0)],"ar1", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}
for(i in 1:100){
  tryCatch({ Tau.AR.SD2[i]<-surrogate_ews_sd_ar1(data = second.p[[i]],final.timepoint=round(mean(xp1),0),
                                                 timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}

#dr2[i]<-mean(EWS.p.2[[i]]$KTauDR)
for(i in 1:100){
  tryCatch({ 
    Tau.traitP.AR.SD3[i]<-surrogate_ews_trait_ar1_sd(data = third.p[[i]], final.timepoint=round(mean(xp2),0),
                                                     timeseries_length = "variable",winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}
for(i in 1:100){
  tryCatch({ Tau.traitP.AR3[i]<-surrogate_ews_trait_sd(data = third.p[[i]],final.timepoint=round(mean(xp2),0),
                                                       timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}

for(i in 1:100){
  tryCatch({ Tau.traitP.SD3[i]<-surrogate_ews_trait_ar1(data = third.p[[i]],final.timepoint=round(mean(xp2),0),
                                                        timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}

for(i in 1:100){
  tryCatch({ 
    TauSD_3[i]<-surrogate_ews_m(third.p[[i]]$N[500:round(mean(xp2),0)],'sd', winsize = 50, detrending = "gaussian", boots =500)$false_p 
  }, error=function(e){})}
for(i in 1:100){
  tryCatch({ TauAR_3[i]<-surrogate_ews_m( third.p[[i]]$N[500:round(mean(xp2),0)], 'ar1',winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}
for(i in 1:100){
  tryCatch({ 
    Tau.AR.SD3[i]<-surrogate_ews_sd_ar1(data = third.p[[i]],final.timepoint=round(mean(xp2),0),
                                        timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}


for(i in 1:100){
  tryCatch({ 
    Tau.traitP.AR.SD4[i]<-surrogate_ews_trait_ar1_sd(data = fourth.p[[i]],final.timepoint=round(mean(xp3),0),
                                                     timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}
for(i in 1:100){
  tryCatch({ 
    Tau.traitP.AR4[i]<-surrogate_ews_trait_sd(data = fourth.p[[i]],final.timepoint=round(mean(xp3),0),
                                              timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}
for(i in 1:100){
  tryCatch({ 
    Tau.traitP.SD4[i]<-surrogate_ews_trait_ar1(data = fourth.p[[i]],final.timepoint=round(mean(xp3),0),
                                               timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}
for(i in 1:100){
  tryCatch({ 
    TauSD_4[i]<-surrogate_ews_m(fourth.p[[i]]$N[500:round(mean(xp3),0)],'sd', winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}
for(i in 1:100){
  tryCatch({ 
    TauAR_4[i]<-surrogate_ews_m(fourth.p[[i]]$N[500:round(mean(xp3),0)], 'ar1',winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})}
for(i in 1:100){
  tryCatch({ 
    Tau.AR.SD4[i]<-surrogate_ews_sd_ar1(data = fourth.p[[i]],final.timepoint=round(mean(xp3),0),
                                        timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p 
  }, error=function(e){})}

for(i in 1:100){
  tryCatch({ 
    Tau.traitP.AR.SD5[i]<-surrogate_ews_trait_ar1_sd(data = fifth.p[[i]],final.timepoint=round(mean(xp4),0),
                                                     timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){}) 
}
for(i in 1:100){
  tryCatch({ 
    Tau.traitP.AR5[i]<-surrogate_ews_trait_sd(data = fifth.p[[i]],final.timepoint=round(mean(xp4),0),
                                              timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
}

for(i in 1:100){
  tryCatch({ 
    Tau.traitP.SD5[i]<-surrogate_ews_trait_ar1(data = fifth.p[[i]],final.timepoint=round(mean(xp4),0),
                                               timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
}
for(i in 1:100){
  tryCatch({ 
    TauSD_5[i]<-surrogate_ews_m(fifth.p[[i]]$N[500:round(mean(xp4),0)],'sd', winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
}

for(i in 1:100){
  tryCatch({ 
    TauAR_5[i]<-surrogate_ews_m(fifth.p[[i]]$N[500:round(mean(xp4),0)], 'ar1',winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
}

for(i in 1:100){
  tryCatch({ 
    Tau.AR.SD5[i]<-surrogate_ews_sd_ar1(fifth.p[[i]],final.timepoint=round(mean(xp4),0),
                                        timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
}

for(i in 1:100){
  tryCatch({ 
    Tau.traitP.AR.SD6[i]<-surrogate_ews_trait_ar1_sd(data = six.p[[i]],final.timepoint=round(mean(xp5),0),
                                                     timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
}
for(i in 1:100){
  tryCatch({ 
    Tau.traitP.AR6[i]<-surrogate_ews_trait_sd(data = six.p[[i]],final.timepoint=round(mean(xp5),0),
                                              timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
}
for(i in 1:100){
  tryCatch({ 
    Tau.traitP.SD6[i]<-surrogate_ews_trait_ar1(data = six.p[[i]],final.timepoint=round(mean(xp5),0),
                                               timeseries_length = "variable",  winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
}
for(i in 1:100){
  tryCatch({ 
    TauSD_6[i]<-surrogate_ews_m(six.p[[i]]$N[500:round(mean(xp5),0)],'sd', winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
}
for(i in 1:100){
  tryCatch({ 
    TauAR_6[i]<-surrogate_ews_m(six.p[[i]]$N[500:round(mean(xp5),0)],'ar1', winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
}
for(i in 1:100){
  tryCatch({ 
    Tau.AR.SD6[i]<-surrogate_ews_sd_ar1(data = six.p[[i]],final.timepoint=round(mean(xp5),0),
                                        timeseries_length = "variable",  winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
}

# for(i in 1:100){
#   tryCatch({ 
#     Tau.traitP.AR.SD7[i]<-surrogate_ews_trait_ar1_sd(data = sev.p[[i]],final.timepoint=round(mean(xp6),0),
#                                                      timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p 
#   }, error=function(e){})
# }
# 
# for(i in 1:100){
#   tryCatch({ 
#     Tau.traitP.AR7[i]<-surrogate_ews_trait_sd(data = sev.p[[i]], final.timepoint=round(mean(xp6),0),
#                                               timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
#   }, error=function(e){})
# }
# for(i in 1:100){
#   tryCatch({ 
#     Tau.traitP.SD7[i]<-surrogate_ews_trait_ar1(data = sev.p[[i]],final.timepoint=round(mean(xp6),0),
#                                                timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p 
#   }, error=function(e){})
# }
# for(i in 1:100){
#   tryCatch({ 
#     TauSD_7[i]<-surrogate_ews_m(sev.p[[i]]$N[500:round(mean(xp6),0)],'sd', winsize = 50, detrending = "gaussian", boots =500)$false_p
#   }, error=function(e){})
# }
# for(i in 1:100){
#   tryCatch({ 
#     TauAR_7[i]<-surrogate_ews_m(sev.p[[i]]$N[500:round(mean(xp6),0)],'ar1', winsize = 50, detrending = "gaussian", boots =500)$false_p 
#   }, error=function(e){})
# }
# for(i in 1:100){
#   tryCatch({ 
#     Tau.AR.SD7[i]<-surrogate_ews_sd_ar1(data = sev.p[[i]],final.timepoint=round(mean(xp6),0),
#                                         timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p 
#   }, error=function(e){})
# }

for(i in 1:100){
  tryCatch({ 
    Tau.traitP.AR.SD1[i]<-surrogate_ews_trait_ar1_sd(data = e.p[[i]],final.timepoint=round(mean(xp7),0),
                                                     timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
}


for(i in 1:100){
  tryCatch({ 
    TauSD_1[i]<-surrogate_ews_m(e.p[[i]]$N[500:round(mean(xp7),0)],'sd', winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
}
for(i in 1:100){
  tryCatch({ 
    TauAR_1[i]<-surrogate_ews_m(e.p[[i]]$N[500:round(mean(xp7),0)],'ar1', winsize = 50, detrending = "gaussian", boots =500)$false_p 
  }, error=function(e){})
}

for(i in 1:100){
  tryCatch({ 
    Tau.traitP.SD1[i]<-surrogate_ews_trait_sd(data = e.p[[i]],final.timepoint=round(mean(xp7),0),
                                              timeseries_length = "variable", winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
}


for(i in 1:100){
  tryCatch({ 
    Tau.traitP.AR1[i]<-surrogate_ews_trait_ar1(data = e.p[[i]],final.timepoint=round(mean(xp7),0),
                                               timeseries_length = "variable",  winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
  print(Tau.traitP.AR1[i])
}

for(i in 1:100){
  tryCatch({ 
    Tau.AR.SD1[i]<-surrogate_ews_sd_ar1(data = e.p[[i]],final.timepoint=round(mean(xp7),0),
                                        timeseries_length = "variable",  winsize = 50, detrending = "gaussian", boots =500)$false_p
  }, error=function(e){})
  print(i) }



library(wesanderson)

best_color_paletter<- c(wes_palettes$Darjeeling1, wes_palettes$Rushmore1)


P2<-data.frame(Kendall.tau=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitP.AR2,Tau.traitP.SD2,Tau.traitP.AR.SD2),
               value=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitP.AR2,Tau.traitP.SD2,Tau.traitP.AR.SD2),
               variation=factor(c(rep("AR1",each=length(TauAR_2)),
                                  rep("SD", each = length(TauSD_2)),
                                  rep("AR1+SD", each=length(Tau.AR.SD2)),
                                  rep("AR1+Tr", each= length(Tau.traitP.AR2)),
                                  rep("SD+Tr", each=length(Tau.traitP.SD2)),
                                  rep("AR1+SD+Tr",each=length(Tau.traitP.AR.SD2)))))

P2$variation<-factor(P2$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
P3<-data.frame(Kendall.tau=c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitP.AR3,Tau.traitP.SD3,Tau.traitP.AR.SD3),
               value=c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitP.AR3,Tau.traitP.SD3,Tau.traitP.AR.SD3),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_3))))

P3$variation<-factor(P3$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
P4<-data.frame(Kendall.tau=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitP.AR4,Tau.traitP.SD4,Tau.traitP.AR.SD4),
               value=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitP.AR4,Tau.traitP.SD4,Tau.traitP.AR.SD4),
               variation=factor(c(rep("AR1",each=length(TauAR_4)),
                                  rep("SD", each = length(TauSD_4)),
                                  rep("AR1+SD", each=length(Tau.AR.SD4)),
                                  rep("AR1+Tr", each= length(Tau.traitP.AR4)),
                                  rep("SD+Tr", each=length(Tau.traitP.SD4)),
                                  rep("AR1+SD+Tr",each=length(Tau.traitP.AR.SD4)))))

P4$variation<-factor(P4$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
P5<-data.frame(Kendall.tau=c(TauAR_5,TauSD_5,Tau.AR.SD5,Tau.traitP.AR5,Tau.traitP.SD5,Tau.traitP.AR.SD5),value=c(TauAR_5,TauSD_5,Tau.AR.SD5,Tau.traitP.AR5,Tau.traitP.SD5,Tau.traitP.AR.SD5),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_5))))
P5$variation<-factor(P5$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )


#                   Kendall_Tau=c(dr1, dr2,dr3,dr4,dr5),value=c(dr1,dr2,dr3,dr4,dr5))
P6<-data.frame(Kendall.tau=c(TauAR_6,TauSD_6,Tau.AR.SD6,Tau.traitP.AR6,Tau.traitP.SD6,Tau.traitP.AR.SD6),value=c(TauAR_6,TauSD_6,Tau.AR.SD6,Tau.traitP.AR6,Tau.traitP.SD6,Tau.traitP.AR.SD6),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_6))))

P6$variation<-factor(P6$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )


P8<-data.frame(Kendall.tau=c(TauAR_1,TauSD_1,Tau.AR.SD1,Tau.traitP.AR1,Tau.traitP.SD1,Tau.traitP.AR.SD1),value=c(TauAR_1,TauSD_1,Tau.AR.SD1,Tau.traitP.AR1,Tau.traitP.SD1,Tau.traitP.AR.SD1),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_1))))
P8$variation<-factor(P8$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a

data.p.plasticity_fp_variable<-data.frame(rbind(P2,P3,P4, P5,P6,P8),
                                 Plasticity =c(rep("0.05",each=nrow(P2)), rep("0.1", each=nrow(P3)),rep("0.2",each=nrow(P4)),
                                               rep("0.3",each=nrow(P5)), 
                                               rep("0.4",each=nrow(P6)),rep( "0.8",each=nrow(P8))) )
save(data.p.plasticity_fp_variable, file = "False.positives_variable_timeseries_plasticity.RData")



best_color_paletter<-c("#A42820", "#5F5647",  "#E58601",  "#00A08A" ,
                       "#4E2A1E","#000066", "#0C1707", "#000066")

p1<-ggplot(data= data.p.plasticity_fp_variable, aes(y = Kendall.tau,x = variation, color= variation )) +geom_boxplot(alpha=0)+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ggtitle("Strength of plasticity")+ylim(c(0,1))+
  ylab("Rate of False positives")+
  theme_classic()+theme(plot.title = element_text(size = 14,   face = "bold"),text = element_text(size = 12 ),
                        axis.title = element_text(face="bold"),axis.text.x=element_blank(),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="EWS metric")+
  facet_wrap(.~Plasticity)+xlab("")
p1
