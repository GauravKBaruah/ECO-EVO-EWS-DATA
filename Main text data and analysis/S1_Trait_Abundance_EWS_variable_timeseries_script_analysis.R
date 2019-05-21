rm(list=ls())



extent.decline<-function(data){
  n0<-data$N[500]
  percent.decline = 0.30
  n.final<-n0*(1-percent.decline)  
  
  time.point <- 500+which(round(data$N[500:800],1) < round(n.final,1))[1]
  # temp.dat<- data$N[time.point]
  # seq.dat<-head(temp.dat, 10)
  # if(seq.dat[10] < n.final){
  #   for(i in 1:length(head(temp.dat, 10))){
  #     #i=1
  #     if(seq.dat[length(head(temp.dat, 10))-i+1] >= n.final){break()}
  #   }
  # final.timepoint <- which(data$N[time.point]== seq.dat[length(head(temp.dat, 10))-i+1])}else {
  #   final.timepoint<- NA }
  #   
  
  return(time.point)
}





load("EWS.genvar0.05.RData") 
load("EWS.genvar0.1.RData")  
load("EWS.genvar0.2.RData") 
load("EWS.genvar0.3.RData")
load("EWS.genvar0.4.RData")
load("EWS.genvar0.5.RData")






library("earlywarnings")
source('~/Dropbox/Zurich PhD Research/1_Chapter_1/rolling_GAMs_methods.R', echo=FALSE)




second.op<-EWS.trait.genvar.0.05
third.op<- EWS.trait.genvar.0.1 
fourth.op<- EWS.trait.genvar.0.2
fifth.op<-EWS.trait.genvar.0.3  
six.op<- EWS.trait.genvar.0.4
sev.op<-EWS.trait.genvar.0.5


reps1<-length(first.op)

t<-seq(1,699);lamda<-numeric();#x1<-array();
x11<-numeric();x22<-numeric();x33<-numeric();x44<-numeric();x55<-numeric();x66<-numeric();x77<-numeric();x88<-numeric()
## getting the bifurcation time out 
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

for(i in 1:100){
  
  
  
  EWS.gen.2[i]<- list(Composite.trait.ews((genericEWS(second.op[[i]]$N[500:round(mean(x22),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),
                                          (genericEWS(second.op[[i]]$N[500:round(mean(x22),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),
                                          (genericEWS(second.op[[i]]$phenotype[500:round(mean(x22),0)],winsize = 50,detrending = "no")$ddata),
                                          (genericEWS(second.op[[i]]$N[500:round(mean(x22),0)],winsize = 50,detrending = "no")$ddata)))
  EWS.gen.3[i]<- list(Composite.trait.ews((genericEWS(third.op[[i]]$N[500:round(mean(x33),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),
                                          (genericEWS(third.op[[i]]$N[500:round(mean(x33),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),
                                          (genericEWS(third.op[[i]]$phenotype[500:round(mean(x33),0)],winsize = 50,detrending = "no")$ddata),
                                          (genericEWS(third.op[[i]]$N[500:round(mean(x33),0)],winsize = 50,detrending = "no")$ddata)))
  EWS.gen.4[i]<- list(Composite.trait.ews((genericEWS(fourth.op[[i]]$N[500:round(mean(x44),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),
                                          (genericEWS(fourth.op[[i]]$N[500:round(mean(x44),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),
                                          (genericEWS(fourth.op[[i]]$phenotype[500:round(mean(x44),0)],winsize = 50,detrending = "no")$ddata),
                                          (genericEWS(fourth.op[[i]]$N[500:round(mean(x44),0)],winsize = 50,detrending = "no")$ddata)))
  EWS.gen.5[i]<- list(Composite.trait.ews((genericEWS(fifth.op[[i]]$N[500:round(mean(x55),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),
                                          (genericEWS(fifth.op[[i]]$N[500:round(mean(x55),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),
                                          (genericEWS(fifth.op[[i]]$phenotype[500:round(mean(x55),0)],winsize = 50,detrending = "no")$ddata),
                                          (genericEWS(fourth.op[[i]]$N[500:round(mean(x55),0)],winsize = 50,detrending = "no")$ddata)))
  EWS.gen.6[i]<- list(Composite.trait.ews((genericEWS(six.op[[i]]$N[500:round(mean(x66),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),
                                          (genericEWS(six.op[[i]]$N[500:round(mean(x66),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),
                                          (genericEWS(six.op[[i]]$phenotype[500:round(mean(x66),0)],winsize = 50,detrending = "no")$ddata),
                                          (genericEWS(fourth.op[[i]]$N[500:round(mean(x66),0)],winsize = 50,detrending = "no")$ddata)))
  EWS.gen.7[i]<- list(Composite.trait.ews((genericEWS(sev.op[[i]]$N[500:round(mean(x77),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),
                                          (genericEWS(sev.op[[i]]$N[500:round(mean(x77),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),
                                          (genericEWS(sev.op[[i]]$phenotype[500:round(mean(x77),0)],winsize = 50,detrending = "no")$ddata),
                                          (genericEWS(fourth.op[[i]]$N[500:round(mean(x77),0)],winsize = 50,detrending = "no")$ddata)))

  
    Tau.traitg.AR.SD2[i]<-mean(EWS.gen.2[[i]]$Tau.Trait.SD.AR)
  Tau.traitg.SD2[i]<-mean(EWS.gen.2[[i]]$Tau.Trait.SD)
  Tau.traitg.AR2[i]<-mean(EWS.gen.2[[i]]$Tau.Trait.AR)
  TauSD_2[i]<-mean(EWS.gen.2[[i]]$Tau.SD)
  TauAR_2[i]<-mean(EWS.gen.2[[i]]$Tau.AR)
  Tau.AR.SD2[i]<-mean(EWS.gen.2[[i]]$Tau.composite)
  
  Tau.trait2[i]<-mean(EWS.gen.2[[i]]$Tau.Trait)
  Tau.N2[i]<-mean(EWS.gen.2[[i]]$Tau.N)
  
  
  Tau.traitg.AR.SD3[i]<-mean(EWS.gen.3[[i]]$Tau.Trait.SD.AR)
  Tau.traitg.AR3[i]<-mean(EWS.gen.3[[i]]$Tau.Trait.AR)
  Tau.traitg.SD3[i]<-mean(EWS.gen.3[[i]]$Tau.Trait.SD)
  TauSD_3[i]<-mean(EWS.gen.3[[i]]$Tau.SD)
  TauAR_3[i]<-mean(EWS.gen.3[[i]]$Tau.AR)
  Tau.AR.SD3[i]<-mean(EWS.gen.3[[i]]$Tau.composite)
  
  Tau.trait3[i]<-mean(EWS.gen.2[[i]]$Tau.Trait)
  Tau.N3[i]<-mean(EWS.gen.2[[i]]$Tau.N)
  
  
  
  Tau.traitg.AR.SD4[i]<-mean(EWS.gen.4[[i]]$Tau.Trait.SD.AR)
  Tau.traitg.AR4[i]<-mean(EWS.gen.4[[i]]$Tau.Trait.AR)
  Tau.traitg.SD4[i]<-mean(EWS.gen.4[[i]]$Tau.Trait.SD)
  TauSD_4[i]<-mean(EWS.gen.4[[i]]$Tau.SD)
  TauAR_4[i]<-mean(EWS.gen.4[[i]]$Tau.AR)
  Tau.AR.SD4[i]<-mean(EWS.gen.4[[i]]$Tau.composite)
  
  Tau.trait4[i]<-mean(EWS.gen.2[[i]]$Tau.Trait)
  Tau.N4[i]<-mean(EWS.gen.2[[i]]$Tau.N)
  
  Tau.traitg.AR.SD5[i]<-mean(EWS.gen.5[[i]]$Tau.Trait.SD.AR)
  Tau.traitg.AR5[i]<-mean(EWS.gen.5[[i]]$Tau.Trait.AR)
  Tau.traitg.SD5[i]<-mean(EWS.gen.5[[i]]$Tau.Trait.SD)
  TauSD_5[i]<-mean(EWS.gen.5[[i]]$Tau.SD)
  TauAR_5[i]<-mean(EWS.gen.5[[i]]$Tau.AR)
  Tau.AR.SD5[i]<-mean(EWS.gen.5[[i]]$Tau.composite)
  
  Tau.trait5[i]<-mean(EWS.gen.2[[i]]$Tau.Trait)
  Tau.N5[i]<-mean(EWS.gen.2[[i]]$Tau.N)
  
  
  
  Tau.traitg.AR.SD6[i]<-mean(EWS.gen.6[[i]]$Tau.Trait.SD.AR)
  Tau.traitg.AR6[i]<-mean(EWS.gen.6[[i]]$Tau.Trait.AR)
  Tau.traitg.SD6[i]<-mean(EWS.gen.6[[i]]$Tau.Trait.SD)
  TauSD_6[i]<-mean(EWS.gen.6[[i]]$Tau.SD)
  TauAR_6[i]<-mean(EWS.gen.6[[i]]$Tau.AR)
  Tau.AR.SD6[i]<-mean(EWS.gen.6[[i]]$Tau.composite)
  
  Tau.trait6[i]<-mean(EWS.gen.2[[i]]$Tau.Trait)
  Tau.N6[i]<-mean(EWS.gen.2[[i]]$Tau.N)
  
  Tau.traitg.AR.SD7[i]<-mean(EWS.gen.7[[i]]$Tau.Trait.SD.AR)
  Tau.traitg.AR7[i]<-mean(EWS.gen.7[[i]]$Tau.Trait.AR)
  Tau.traitg.SD7[i]<-mean(EWS.gen.7[[i]]$Tau.Trait.SD)
  TauSD_7[i]<-mean(EWS.gen.7[[i]]$Tau.SD)
  TauAR_7[i]<-mean(EWS.gen.7[[i]]$Tau.AR)
  Tau.AR.SD7[i]<-mean(EWS.gen.7[[i]]$Tau.composite)
  
  Tau.trait7[i]<-mean(EWS.gen.2[[i]]$Tau.Trait)
  Tau.N7[i]<-mean(EWS.gen.2[[i]]$Tau.N)
  
  
  print(i)
}

library(wesanderson)

best_color_paletter<- c(wes_palettes$Darjeeling1, wes_palettes$Rushmore1)


# 
# a
var.0.05<-data.frame(Kendall.tau=
                       c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitg.AR2,Tau.traitg.SD2,Tau.traitg.AR.SD2),
                     value=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitg.AR2,Tau.traitg.SD2,Tau.traitg.AR.SD2),
                     variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_2))))

var.0.05$variation<-factor(var.0.05$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )
# a
var.0.1<-data.frame(Kendall.tau=c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitg.AR3,Tau.traitg.SD3,Tau.traitg.AR.SD3),
                    value=c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitg.AR3,Tau.traitg.SD3,Tau.traitg.AR.SD3),
                    variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_3))))
var.0.1$variation<-factor(var.0.1$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
var.0.2<-data.frame(Kendall.tau=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitg.AR4,Tau.traitg.SD4,Tau.traitg.AR.SD4),
                    value=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitg.AR4,Tau.traitg.SD4,Tau.traitg.AR.SD4),
                    variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_4))))
var.0.2$variation<-factor(var.0.2$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
var.0.3<-data.frame(Kendall.tau=c(TauAR_5,TauSD_5,Tau.AR.SD5,Tau.traitg.AR5,Tau.traitg.SD5,Tau.traitg.AR.SD5),
                    value=c(TauAR_5,TauSD_5,Tau.AR.SD5,Tau.traitg.AR5,Tau.traitg.SD5,Tau.traitg.AR.SD5),
                    variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_5))))
var.0.3$variation<-factor(var.0.3$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
var.0.4<-data.frame(Kendall.tau=c(TauAR_6,TauSD_6,Tau.AR.SD6,Tau.traitg.AR6,Tau.traitg.SD6,Tau.traitg.AR.SD6),
                    value=c(TauAR_6,TauSD_6,Tau.AR.SD6,Tau.traitg.AR6,Tau.traitg.SD6,Tau.traitg.AR.SD6),
                    variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_6))))
var.0.4$variation<-factor(var.0.4$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
var.0.5<-data.frame(Kendall.tau=c(TauAR_7,TauSD_7,Tau.AR.SD7,Tau.traitg.AR7,Tau.traitg.SD7,Tau.traitg.AR.SD7),
                    value=c(TauAR_7,TauSD_7,Tau.AR.SD7,Tau.traitg.AR7,Tau.traitg.SD7,Tau.traitg.AR.SD7),
                    variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_7))))
var.0.5$variation<-factor(var.0.5$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )


# a
data.g.timeseries<-data.frame(rbind(var.0.05,var.0.1, var.0.2, var.0.3,var.0.4,var.0.5),
                   Genetic_variation =c(rep("0.05",each=nrow(var.0.05)), rep("0.1", each=nrow(var.0.1)),
                                        rep("0.2",each=nrow(var.0.2)), rep("0.3",each=nrow(var.0.3)), 
                                        rep("0.4",each=nrow(var.0.4)),rep( "0.5",each=nrow(var.0.5))) )

#save(data.g.timeseries,file="genvar_variable_timeseries.RData")

#ar1
## plotting genetic variation


g1.variable<-ggplot(data= data.g.timeseries, aes(y = Kendall.tau,x = Genetic_variation, color= Genetic_variation )) +geom_boxplot(alpha=0)+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ylim(c(-1,1))+
  ylab("Kendall's Tau")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Genetic variation")+
  facet_wrap(.~variation)+xlab("")

#pdf(file = "Residuals_noise_genvar.pdf", width = 8, height = 8)
g1.variable


################################  Reproduction  rate data #################

#rm(list=ls())
load("EWS.reproduction1.1.RData")
load("EWS.reproduction1.2.RData") 
load("EWS.reproduction1.3.RData")  
load("EWS.reproduction1.4.RData")                       
load("EWS.reproduction1.5.RData")

library("earlywarnings")



first.R0<-EWS.trait.reproduction_1.1
second.R0<-EWS.trait.reproduction_1.2#low genvariation low plasticity
third.R0<- EWS.trait.reproduction_1.3 #high genvariation low plasticity
fourth.R0<- EWS.trait.reproduction_1.4# low genvariation high plasticity
fifth.R0<-EWS.trait.reproduction_1.5

reps1<-length(first.R0)

x1<-numeric();x2<-numeric();x3<-numeric();x4<-numeric();x5<-numeric();x6<-numeric();x7<-numeric();x8<-numeric()
# getting the bifurcation time out
t<-seq(500:900)
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

for(i in 1:100){
  
  
  EWSR0.1[i]<-list(Composite.trait.ews((genericEWS(first.R0[[i]]$N[500:round(mean(x1),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),
                                       (genericEWS(first.R0[[i]]$N[500:round(mean(x1),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),
                                       (genericEWS(first.R0[[i]]$phenotype[500:round(mean(x1),0)],winsize = 50,detrending = "no")$ddata),
                                       (genericEWS(first.R0[[i]]$N[500:round(mean(x1),0)],winsize = 50,detrending = "no")$ddata)))
  EWSR0.2[i]<- list(Composite.trait.ews((genericEWS(second.R0[[i]]$N[500:round(mean(x2),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),
                                        (genericEWS(second.R0[[i]]$N[500:round(mean(x2),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),
                                        (genericEWS(second.R0[[i]]$phenotype[500:round(mean(x2),0)],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(second.R0[[i]]$N[500:round(mean(x2),0)],winsize = 50,detrending = "no")$ddata)                  ))
  EWSR0.3[i]<- list(Composite.trait.ews((genericEWS(third.R0[[i]]$N[500:round(mean(x3),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),
                                        (genericEWS(third.R0[[i]]$N[500:round(mean(x3),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),
                                        (genericEWS(third.R0[[i]]$phenotype[500:round(mean(x3),0)],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(third.R0[[i]]$N[500:round(mean(x3),0)],winsize = 50,detrending = "no")$ddata)))
  EWSR0.4[i]<- list(Composite.trait.ews((genericEWS(fourth.R0[[i]]$N[500:round(mean(x4),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),
                                        (genericEWS(fourth.R0[[i]]$N[500:round(mean(x4),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),
                                        (genericEWS(fourth.R0[[i]]$phenotype[500:round(mean(x4),0)],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(fourth.R0[[i]]$N[500:round(mean(x4),0)],winsize = 50,detrending = "no")$ddata)))
  EWSR0.5[i]<-list(Composite.trait.ews((genericEWS(fifth.R0[[i]]$N[500:round(mean(x5),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),
                                       (genericEWS(fifth.R0[[i]]$N[500:round(mean(x5),0)],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),
                                       (genericEWS(fifth.R0[[i]]$phenotype[500:round(mean(x5),0)],winsize = 50,detrending = "no")$ddata),
                                       (genericEWS(fifth.R0[[i]]$N[500:round(mean(x5),0)],winsize = 50,detrending = "no")$ddata)))
  # dr1[i]<-mean(EWSR0.1[[i]]$KTauDR)
  
  Tau.traitR.AR.SD1[i]<-mean(EWSR0.1[[i]]$Tau.Trait.SD.AR)
  Tau.traitR.SD1[i]<-mean(EWSR0.1[[i]]$Tau.Trait.SD)
  Tau.traitR.AR1[i]<-mean(EWSR0.1[[i]]$Tau.Trait.AR)
  TauSD_1[i]<-mean(EWSR0.1[[i]]$Tau.SD)
  TauAR_1[i]<-mean(EWSR0.1[[i]]$Tau.AR)
  Tau.AR.SD1[i]<-mean(EWSR0.1[[i]]$Tau.composite)
  
  
  Tau.trait2[i]<-mean(EWSR0.1[[i]]$Tau.Trait)
  Tau.N2[i]<-mean(EWSR0.1[[i]]$Tau.N)
  
  
  
  # dr1[i]<-mean(EWSR0.1[[i]]$KTauDR)
  Tau.traitR.AR.SD2[i]<-mean(EWSR0.2[[i]]$Tau.Trait.SD.AR)
  Tau.traitR.SD2[i]<-mean(EWSR0.2[[i]]$Tau.Trait.SD)
  Tau.traitR.AR2[i]<-mean(EWSR0.2[[i]]$Tau.Trait.AR)
  TauSD_2[i]<-mean(EWSR0.2[[i]]$Tau.SD)
  TauAR_2[i]<-mean(EWSR0.2[[i]]$Tau.AR)
  Tau.AR.SD2[i]<-mean(EWSR0.2[[i]]$Tau.composite)
  #dr2[i]<-mean(EWSR0.2[[i]]$KTauDR)
  Tau.traitR.AR.SD3[i]<-mean(EWSR0.3[[i]]$Tau.Trait.SD.AR)
  Tau.traitR.AR3[i]<-mean(EWSR0.3[[i]]$Tau.Trait.AR)
  Tau.traitR.SD3[i]<-mean(EWSR0.3[[i]]$Tau.Trait.SD)
  TauSD_3[i]<-mean(EWSR0.3[[i]]$Tau.SD)
  TauAR_3[i]<-mean(EWSR0.3[[i]]$Tau.AR)
  Tau.AR.SD3[i]<-mean(EWSR0.3[[i]]$Tau.composite)
  #dr3[i]<-mean(EWSR0.3[[i]]$KTauDR)
  
  
  Tau.traitR.AR.SD4[i]<-mean(EWSR0.4[[i]]$Tau.Trait.SD.AR)
  Tau.traitR.AR4[i]<-mean(EWSR0.4[[i]]$Tau.Trait.AR)
  Tau.traitR.SD4[i]<-mean(EWSR0.4[[i]]$Tau.Trait.SD)
  TauSD_4[i]<-mean(EWSR0.4[[i]]$Tau.SD)
  TauAR_4[i]<-mean(EWSR0.4[[i]]$Tau.AR)
  Tau.AR.SD4[i]<-mean(EWSR0.4[[i]]$Tau.composite)
  #dr4[i]<-mean(EWSR0.4[[i]]$KTauDR)
  Tau.traitR.AR.SD5[i]<-mean(EWSR0.5[[i]]$Tau.Trait.SD.AR)
  Tau.traitR.AR5[i]<-mean(EWSR0.5[[i]]$Tau.Trait.AR)
  Tau.traitR.SD5[i]<-mean(EWSR0.5[[i]]$Tau.Trait.SD)
  TauSD_5[i]<-mean(EWSR0.5[[i]]$Tau.SD)
  TauAR_5[i]<-mean(EWSR0.5[[i]]$Tau.AR)
  Tau.AR.SD5[i]<-mean(EWSR0.5[[i]]$Tau.composite)
  #dr5[i]<-mean(EWSR0.5[[i]]$KTauDR)
  
  
  Tau.trait5[i]<-mean(EWSR0.5[[i]]$Tau.Trait)
  Tau.N5[i]<-mean(EWSR0.5[[i]]$Tau.N)
  
  print(i)
}

library(wesanderson)


best_color_paletter<- c(wes_palettes$Darjeeling1, wes_palettes$Rushmore1)


R1<-data.frame(Kendall.tau=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitR.AR2,Tau.traitR.SD2,Tau.traitR.AR.SD2),value=c(TauAR_1,TauSD_1,Tau.AR.SD1,Tau.traitR.AR1,Tau.traitR.SD1,Tau.traitR.AR.SD1),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_1))))
R1$variation<-factor(R1$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
R2<-data.frame(Kendall.tau=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitR.AR2,Tau.traitR.SD2,Tau.traitR.AR.SD2),value=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitR.AR2,Tau.traitR.SD2,Tau.traitR.AR.SD2),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_2))))

R2$variation<-factor(R2$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
R3<-data.frame(Kendall.tau=c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitR.AR3,Tau.traitR.SD3,Tau.traitR.AR.SD3),value=c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitR.AR3,Tau.traitR.SD3,Tau.traitR.AR.SD3),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_3))))

R3$variation<-factor(R3$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
R4<-data.frame(Kendall.tau=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitR.AR4,Tau.traitR.SD4,Tau.traitR.AR.SD4),value=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitR.AR4,Tau.traitR.SD4,Tau.traitR.AR.SD4),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_4))))

R4$variation<-factor(R4$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
R5<-data.frame(Kendall.tau=c(TauAR_5,TauSD_5,Tau.AR.SD5,Tau.traitR.AR5,Tau.traitR.SD5,Tau.traitR.AR.SD5),value=c(TauAR_5,TauSD_5,Tau.AR.SD5,Tau.traitR.AR5,Tau.traitR.SD5,Tau.traitR.AR.SD5),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_5))))
#                   Kendall_Tau=c(dr1, dr2,dr3,dr4,dr5),value=c(dr1,dr2,dr3,dr4,dr5))

R5$variation<-factor(R5$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )


data.R.timeseries<-data.frame(rbind(R1,R2, R3, R4,R5),
                   R0 =c(rep("1.1",each=nrow(R1)), rep("1.2", each=nrow(R2)),rep("1.3",each=nrow(R3)), rep("1.4",each=nrow(R4)), 
                         rep("1.5",each=nrow(R5))) )

#save(data.R.timeseries,file="reproductive_rate_variable_timeseries.RData")



R1.variable<-ggplot(data= data.R.timeseries, aes(y = Kendall.tau,x = R0, color= R0 )) +geom_boxplot(alpha=0)+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ylim(c(-1,1))+
  ylab("Kendall's Tau")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Net reproductive rate")+
  facet_wrap(.~variation)+xlab("")

#pdf(file = "Residuals_noise_genvar.pdf", width = 8, height = 8)
R1.variable





############################## Plasticity Data ##########################


load("EWS.plasticity.0.05.RData") #low plasticity low genvariation
load("EWS.plasticity.0.1.RData")  #low plasticity high genvariation
load("EWS.plasticity.0.2.RData")      
load("EWS.plasticity.0.3.RData")  #high plasticity low genvariation
load("EWS.plasticity.0.4.RData")
load("EWS.plasticity.0.5.RData")
load("EWS.plasticity.0.8.RData")

library("earlywarnings")




second.p<-EWS.trait.plasticity0.05#low genvariation low plasticity
third.p<- EWS.trait.plasticity0.1 #high genvariation low plasticity
fourth.p<- EWS.trait.plasticity0.2# low genvariation high plasticity
fifth.p<-EWS.trait.plasticity0.3
six.p<- EWS.trait.plasticity0.4
sev.p<-EWS.trait.plasticity0.5#high genvariation high plasticity 
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

for(i in 1:100){
  
  EWS.p.2[i]<- list(Composite.trait.ews((genericEWS(second.p[[i]]$N[500:round(mean(xp1),0)],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),(genericEWS(second.p[[i]]$N[500:round(mean(xp1),0)],winsize=50,detrending = "gaussian",bandwidth =50 )$sd),(genericEWS(second.p[[i]]$phenotype[500:round(mean(xp1),0)],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(second.p[[i]]$N[500:round(mean(xp1),0)],winsize = 50,detrending = "no")$ddata)))
  EWS.p.3[i]<- list(Composite.trait.ews((genericEWS(third.p[[i]]$N[500:round(mean(xp2),0)],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),
                                        (genericEWS(third.p[[i]]$N[500:round(mean(xp2),0)],winsize=50,detrending = "gaussian",bandwidth =50 )$sd),(
                                          genericEWS(third.p[[i]]$phenotype[500:round(mean(xp2),0)],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(third.p[[i]]$N[500:round(mean(xp2),0)],winsize = 50,detrending = "no")$ddata)))
  EWS.p.4[i]<- list(Composite.trait.ews((genericEWS(fourth.p[[i]]$N[500:round(mean(xp3),0)],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),
                                        (genericEWS(fourth.p[[i]]$N[500:round(mean(xp3),0)],winsize=50,detrending = "gaussian",bandwidth =50 )$sd),
                                        (genericEWS(fourth.p[[i]]$phenotype[500:round(mean(xp3),0)],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(fourth.p[[i]]$N[500:round(mean(xp3),0)],winsize = 50,detrending = "no")$ddata)))
  EWS.p.5[i]<- list(Composite.trait.ews((genericEWS(fifth.p[[i]]$N[500:round(mean(xp4),0)],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),
                                        (genericEWS(fifth.p[[i]]$N[500:round(mean(xp4),0)],winsize=50,detrending = "gaussian",bandwidth =50 )$sd),
                                        (genericEWS(fifth.p[[i]]$phenotype[500:round(mean(xp4),0)],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(fifth.p[[i]]$N[500:round(mean(xp4),0)],winsize = 50,detrending = "no")$ddata) ))
  EWS.p.6[i]<- list(Composite.trait.ews((genericEWS(six.p[[i]]$N[500:round(mean(xp5),0)],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),
                                        (genericEWS(six.p[[i]]$N[500:round(mean(xp5),0)],winsize=50,detrending = "gaussian",bandwidth =50 )$sd),
                                        (genericEWS(six.p[[i]]$phenotype[500:round(mean(xp5),0)],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(six.p[[i]]$N[500:round(mean(xp5),0)],winsize = 50,detrending = "no")$ddata)))
  EWS.p.7[i]<- list(Composite.trait.ews((genericEWS(sev.p[[i]]$N[500:round(mean(xp6),0)],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),
                                        (genericEWS(sev.p[[i]]$N[500:round(mean(xp6),0)],winsize=50,detrending = "gaussian",bandwidth =50 )$sd),
                                        (genericEWS(sev.p[[i]]$phenotype[500:round(mean(xp6),0)],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(sev.p[[i]]$N[500:round(mean(xp6),0)],winsize = 50,detrending = "no")$ddata)))
  EWS.p.8[i]<-list(Composite.trait.ews((genericEWS(e.p[[i]]$N[500:round(mean(xp7),0)],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),
                                       (genericEWS(e.p[[i]]$N[500:round(mean(xp7),0)],winsize=50,detrending = "gaussian",bandwidth =50 )$sd),
                                       (genericEWS(e.p[[i]]$phenotype[500:round(mean(xp7),0)],winsize = 50,detrending = "no")$ddata),
                                       (genericEWS(e.p[[i]]$N[500:round(mean(xp7),0)],winsize = 50,detrending = "no")$ddata)   ))
  
  #EWS.p.7[i]<- list(genericEWS(sev.p[[i]]$N[400:570],winsize=50))
  
  
  
  # dr1[i]<-mean(EWS.p.1[[i]]$KTauDR)
  Tau.traitP.AR.SD2[i]<-mean(EWS.p.2[[i]]$Tau.Trait.SD.AR)
  Tau.traitP.SD2[i]<-mean(EWS.p.2[[i]]$Tau.Trait.SD)
  Tau.traitP.AR2[i]<-mean(EWS.p.2[[i]]$Tau.Trait.AR)
  TauSD_2[i]<-mean(EWS.p.2[[i]]$Tau.SD)
  TauAR_2[i]<-mean(EWS.p.2[[i]]$Tau.AR)
  Tau.AR.SD2[i]<-mean(EWS.p.2[[i]]$Tau.composite)
  #dr2[i]<-mean(EWS.p.2[[i]]$KTauDR)
  Tau.traitP.AR.SD3[i]<-mean(EWS.p.3[[i]]$Tau.Trait.SD.AR)
  Tau.traitP.AR3[i]<-mean(EWS.p.3[[i]]$Tau.Trait.AR)
  Tau.traitP.SD3[i]<-mean(EWS.p.3[[i]]$Tau.Trait.SD)
  TauSD_3[i]<-mean(EWS.p.3[[i]]$Tau.SD)
  TauAR_3[i]<-mean(EWS.p.3[[i]]$Tau.AR)
  Tau.AR.SD3[i]<-mean(EWS.p.3[[i]]$Tau.composite)
  
  Tau.traitP.AR.SD4[i]<-mean(EWS.p.4[[i]]$Tau.Trait.SD.AR)
  Tau.traitP.AR4[i]<-mean(EWS.p.4[[i]]$Tau.Trait.AR)
  Tau.traitP.SD4[i]<-mean(EWS.p.4[[i]]$Tau.Trait.SD)
  TauSD_4[i]<-mean(EWS.p.4[[i]]$Tau.SD)
  TauAR_4[i]<-mean(EWS.p.4[[i]]$Tau.AR)
  Tau.AR.SD4[i]<-mean(EWS.p.4[[i]]$Tau.composite)
  
  
  Tau.traitP.AR.SD5[i]<-mean(EWS.p.5[[i]]$Tau.Trait.SD.AR)
  Tau.traitP.AR5[i]<-mean(EWS.p.5[[i]]$Tau.Trait.AR)
  Tau.traitP.SD5[i]<-mean(EWS.p.5[[i]]$Tau.Trait.SD)
  TauSD_5[i]<-mean(EWS.p.5[[i]]$Tau.SD)
  TauAR_5[i]<-mean(EWS.p.5[[i]]$Tau.AR)
  Tau.AR.SD5[i]<-mean(EWS.p.5[[i]]$Tau.composite)
  
  
  Tau.traitP.AR.SD6[i]<-mean(EWS.p.6[[i]]$Tau.Trait.SD.AR)
  Tau.traitP.AR6[i]<-mean(EWS.p.6[[i]]$Tau.Trait.AR)
  Tau.traitP.SD6[i]<-mean(EWS.p.6[[i]]$Tau.Trait.SD)
  TauSD_6[i]<-mean(EWS.p.6[[i]]$Tau.SD)
  TauAR_6[i]<-mean(EWS.p.6[[i]]$Tau.AR)
  Tau.AR.SD6[i]<-mean(EWS.p.6[[i]]$Tau.composite)
  
  Tau.traitP.AR.SD7[i]<-mean(EWS.p.7[[i]]$Tau.Trait.SD.AR)
  Tau.traitP.AR7[i]<-mean(EWS.p.7[[i]]$Tau.Trait.AR)
  Tau.traitP.SD7[i]<-mean(EWS.p.7[[i]]$Tau.Trait.SD)
  TauSD_7[i]<-mean(EWS.p.7[[i]]$Tau.SD)
  TauAR_7[i]<-mean(EWS.p.7[[i]]$Tau.AR)
  Tau.AR.SD7[i]<-mean(EWS.p.7[[i]]$Tau.composite)
  
  Tau.traitP.AR.SD1[i]<-mean(EWS.p.8[[i]]$Tau.Trait.SD.AR)
  Tau.traitP.SD1[i]<-mean(EWS.p.8[[i]]$Tau.Trait.SD)
  Tau.traitP.AR1[i]<-mean(EWS.p.8[[i]]$Tau.Trait.AR)
  TauSD_1[i]<-mean(EWS.p.8[[i]]$Tau.SD)
  TauAR_1[i]<-mean(EWS.p.8[[i]]$Tau.AR)
  Tau.AR.SD1[i]<-mean(EWS.p.8[[i]]$Tau.composite)
  print(i)
  
  
}

library(wesanderson)

best_color_paletter<- c(wes_palettes$Darjeeling1, wes_palettes$Rushmore1)


P2<-data.frame(Kendall.tau=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitP.AR2,Tau.traitP.SD2,Tau.traitP.AR.SD2),value=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitP.AR2,Tau.traitP.SD2,Tau.traitP.AR.SD2),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_2))))

P2$variation<-factor(P2$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
P3<-data.frame(Kendall.tau=c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitP.AR3,Tau.traitP.SD3,Tau.traitP.AR.SD3),value=c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitP.AR3,Tau.traitP.SD3,Tau.traitP.AR.SD3),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_3))))

P3$variation<-factor(P3$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
P4<-data.frame(Kendall.tau=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitP.AR4,Tau.traitP.SD4,Tau.traitP.AR.SD4),value=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitP.AR4,Tau.traitP.SD4,Tau.traitP.AR.SD4),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_4))))

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


data.p.timesires<-data.frame(rbind(P2,P3,P4, P5,P6,P8),Plasticity =c(rep("0.05",each=600), rep("0.1", each=600),
                                                           rep("0.2",each=600), rep("0.3",each=600), 
                                                           rep("0.5",each=600),rep( "0.8",each=600)) )

#save(data.p.timesires,file="plasticity_data_variable_timeseries.RData")


p.variable<-ggplot(data= data.p.timesires, aes(y = Kendall.tau,x = Plasticity, color= Plasticity )) +geom_boxplot(alpha=0)+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ylim(c(-1,1))+
  ylab("Kendall's Tau")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Plasticity strength")+
  facet_wrap(.~variation)+xlab("")

#pdf(file = "Residuals_noise_genvar.pdf", width = 8, height = 8)
p.variable




# creating the dataframes
