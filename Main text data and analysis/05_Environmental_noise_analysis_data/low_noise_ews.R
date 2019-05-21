rm(list=ls())

# this script does the anlysis for low noise dataset also included. One needs to make sure when functions are sourced, the files are in the exact location.


load("EWS.lownoise.genvar.0.05.RData") 
load("EWS.lownoise.genvar.0.1.RData")  
load("EWS.lownoise.genvar.0.2.RData") 
load("EWS.lownoise.genvar.0.3.RData") 
load("EWS.lownoise.genvar.0.4.RData")
load("EWS.lownoise.genvar.0.5.RData")



library("earlywarnings")
source('~/Dropbox/Zurich PhD Research/2_Chapter_2/J Animal Ecology codes/Functions_J_A_Ecology.R')



second.op<-lownoise.trait.genvar.0.05
third.op<- lownoise.trait.genvar.0.1 
fourth.op<- lownoise.trait.genvar.0.2
fifth.op<-lownoise.trait.genvar.0.3
six.op<- lownoise.trait.genvar.0.4
sev.op<-lownoise.trait.genvar.0.5

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
  
  
  
  EWS.gen.2[i]<- list(Composite.trait.ews((genericEWS(second.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(second.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),(genericEWS(second.op[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                          (genericEWS(second.op[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata)))
  EWS.gen.3[i]<- list(Composite.trait.ews((genericEWS(third.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(third.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),(genericEWS(third.op[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                          (genericEWS(third.op[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata)))
  EWS.gen.4[i]<- list(Composite.trait.ews((genericEWS(fourth.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(fourth.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),(genericEWS(fourth.op[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                          (genericEWS(fourth.op[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata)))
  EWS.gen.5[i]<- list(Composite.trait.ews((genericEWS(fifth.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(fifth.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),(genericEWS(fifth.op[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                          (genericEWS(fourth.op[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata)))
  EWS.gen.6[i]<- list(Composite.trait.ews((genericEWS(six.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(six.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),(genericEWS(six.op[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                          (genericEWS(fourth.op[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata)))
  EWS.gen.7[i]<- list(Composite.trait.ews((genericEWS(sev.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(sev.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),(genericEWS(sev.op[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                          (genericEWS(fourth.op[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata)))

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
var.0.06.low<-data.frame(Kendall.tau=
                       c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitg.AR2,Tau.traitg.SD2,Tau.traitg.AR.SD2),
                       value=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitg.AR2,Tau.traitg.SD2,Tau.traitg.AR.SD2),
                     variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_2))))

var.0.06.low$variation<-factor(var.0.06.low$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )
# a
var.0.1.low<-data.frame(Kendall.tau=c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitg.AR3,Tau.traitg.SD3,Tau.traitg.AR.SD3),
                       value= c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitg.AR3,Tau.traitg.SD3,Tau.traitg.AR.SD3),
                    variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_3))))
var.0.1.low$variation<-factor(var.0.1.low$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
var.0.2.low<-data.frame(Kendall.tau=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitg.AR4,Tau.traitg.SD4,Tau.traitg.AR.SD4),
                        value=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitg.AR4,Tau.traitg.SD4,Tau.traitg.AR.SD4),
                    variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_4))))
var.0.2.low$variation<-factor(var.0.2.low$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
var.0.3.low<-data.frame(Kendall.tau=c(TauAR_5,TauSD_5,Tau.AR.SD5,Tau.traitg.AR5,Tau.traitg.SD5,Tau.traitg.AR.SD5),
                        value=c(TauAR_5,TauSD_5,Tau.AR.SD5,Tau.traitg.AR5,Tau.traitg.SD5,Tau.traitg.AR.SD5),
                    variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_5))))
var.0.3.low$variation<-factor(var.0.3.low$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
var.0.4.low<-data.frame(Kendall.tau=c(TauAR_6,TauSD_6,Tau.AR.SD6,Tau.traitg.AR6,Tau.traitg.SD6,Tau.traitg.AR.SD6),
                        value=c(TauAR_6,TauSD_6,Tau.AR.SD6,Tau.traitg.AR6,Tau.traitg.SD6,Tau.traitg.AR.SD6),
                    variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_6))))
var.0.4.low$variation<-factor(var.0.4.low$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
var.0.5.low<-data.frame(Kendall.tau=c(TauAR_7,TauSD_7,Tau.AR.SD7,Tau.traitg.AR7,Tau.traitg.SD7,Tau.traitg.AR.SD7),
                        value=c(TauAR_7,TauSD_7,Tau.AR.SD7,Tau.traitg.AR7,Tau.traitg.SD7,Tau.traitg.AR.SD7),
                    variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_7))))
var.0.5.low$variation<-factor(var.0.5.low$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )


# a



#ar1
G.trait.ar.sd2<-ggplot(var.0.06.low,aes(variation,Kendall.tau))+geom_boxplot(alpha=0)+ggtitle("AR1")+geom_boxplot(data=var.0.06.low[var.0.06.low$variation=="AR1+Tr",],aes(x=variation,y=Kendall.tau),fill="tomato",alpha=0.2)+
  geom_boxplot(data=var.0.06.low[var.0.06.low$variation=="AR1+SD+Tr",],aes(x=variation,y=Kendall.tau),fill="cyan",alpha=0.2)+
  geom_boxplot(data=var.0.06.low[var.0.06.low$variation=="SD+Tr",],aes(x=variation,y=Kendall.tau),alpha=0.2,fill="blue")+geom_jitter(alpha=0.2,width = 0.2)+ggtitle("Var= 0.05")+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Metrics")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 11),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")

G.trait.ar.sd3<-ggplot(var.0.1.low,aes(variation,Kendall.tau))+geom_boxplot(alpha=0)+ggtitle("AR1")+geom_boxplot(data=var.0.1.low[var.0.1.low$variation=="AR1+Tr",],aes(x=variation,y=Kendall.tau),fill="tomato",alpha=0.2)+
  geom_boxplot(data=var.0.1.low[var.0.1.low$variation=="AR1+SD+Tr",],aes(x=variation,y=Kendall.tau),fill="cyan",alpha=0.2)+geom_boxplot(data=var.0.1.low[var.0.1.low$variation=="SD+Tr",],aes(x=variation,y=Kendall.tau),alpha=0.2,fill="blue")+
  geom_jitter(alpha=0.2,width = 0.2)+ggtitle("Var= 0.1")+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Metrics")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 11),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")
G.trait.ar.sd4<-ggplot(var.0.2.low,aes(variation,Kendall.tau))+geom_boxplot(alpha=0)+ggtitle("AR1")+geom_boxplot(data=var.0.2.low[var.0.2.low$variation=="AR1+Tr",],aes(x=variation,y=Kendall.tau),fill="tomato",alpha=0.2)+
  geom_boxplot(data=var.0.2.low[var.0.2.low$variation=="AR1+SD+Tr",],aes(x=variation,y=Kendall.tau),fill="cyan",alpha=0.2)+
  geom_boxplot(data=var.0.2.low[var.0.2.low$variation=="SD+Tr",],aes(x=variation,y=Kendall.tau),alpha=0.2,fill="blue")+geom_jitter(alpha=0.2,width = 0.2)+ggtitle("Var= 0.2")+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Metrics")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 11),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")
G.trait.ar.sd5<-ggplot(var.0.3.low,aes(variation,Kendall.tau))+geom_boxplot(alpha=0)+ggtitle("AR1")+geom_boxplot(data=var.0.3.low[var.0.3.low$variation=="AR1+Tr",],aes(x=variation,y=Kendall.tau),fill="tomato",alpha=0.2)+
  geom_boxplot(data=var.0.3.low[var.0.3.low$variation=="AR1+SD+Tr",],aes(x=variation,y=Kendall.tau),fill="cyan",alpha=0.2)+geom_boxplot(data=var.0.3.low[var.0.3.low$variation=="SD+Tr",],aes(x=variation,y=Kendall.tau),alpha=0.2,fill="blue")+
  geom_jitter(alpha=0.2,width = 0.2)+ggtitle("Var= 0.3")+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Metrics")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 11),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")
G.trait.ar.sd6<-ggplot(var.0.4.low,aes(variation,Kendall.tau))+geom_boxplot(alpha=0)+ggtitle("AR1")+geom_boxplot(data=var.0.4.low[var.0.4.low$variation=="AR1+Tr",],aes(x=variation,y=Kendall.tau),fill="tomato",alpha=0.2)+
  geom_boxplot(data=var.0.4.low[var.0.4.low$variation=="AR1+SD+Tr",],aes(x=variation,y=Kendall.tau),fill="cyan",alpha=0.2)+geom_boxplot(data=var.0.4.low[var.0.4.low$variation=="SD+Tr",],aes(x=variation,y=Kendall.tau),alpha=0.2,fill="blue")+
  geom_jitter(alpha=0.2,width = 0.2)+ggtitle("Var= 0.4")+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Metrics")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 11),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")
G.trait.ar.sd7<-ggplot(var.0.5.low,aes(variation,Kendall.tau))+geom_boxplot(alpha=0)+ggtitle("AR1")+geom_boxplot(data=var.0.5.low[var.0.5.low$variation=="AR1+Tr",],aes(x=variation,y=Kendall.tau),fill="tomato",alpha=0.2)+
  geom_boxplot(data=var.0.5.low[var.0.5.low$variation=="AR1+SD+Tr",],aes(x=variation,y=Kendall.tau),fill="cyan",alpha=0.2)+geom_boxplot(data=var.0.5.low[var.0.5.low$variation=="SD+Tr",],aes(x=variation,y=Kendall.tau),alpha=0.2,fill="blue")+geom_jitter(alpha=0.2,width = 0.2)+ggtitle("Var= 0.5")+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Metrics")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 11),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")


multiplot(G.trait.ar.sd2,G.trait.ar.sd3,G.trait.ar.sd4,G.trait.ar.sd5,G.trait.ar.sd6,G.trait.ar.sd7,cols=2)






################################  Reproduction  rate data #################

#rm(list=ls())
load("EWS.lownoise.reproduction1.1.RData")
load("EWS.lownoise.reproduction1.2.RData") 
load("EWS.lownoise.reproduction1.3.RData")  
load("EWS.lownoise.reproduction1.4.RData")                       
load("EWS.lownoise.reproduction1.5.RData")

library("earlywarnings")



first.R0<-lownoise.trait.reproduction_1.1
second.R0<-lownoise.trait.reproduction_1.2
third.R0<- lownoise.trait.reproduction_1.3
fourth.R0<- lownoise.trait.reproduction_1.4
fifth.R0<-lownoise.trait.reproduction_1.5

reps1<-length(first.R0)


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
  
  
  EWSR0.1[i]<-list(Composite.trait.ews((genericEWS(first.R0[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(first.R0[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth =50)$sd),(genericEWS(first.R0[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                       (genericEWS(first.R0[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata)))
  EWSR0.2[i]<- list(Composite.trait.ews((genericEWS(second.R0[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(second.R0[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),(genericEWS(second.R0[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(second.R0[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata)                  ))
  EWSR0.3[i]<- list(Composite.trait.ews((genericEWS(third.R0[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(third.R0[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),(genericEWS(third.R0[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(third.R0[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata)))
  EWSR0.4[i]<- list(Composite.trait.ews((genericEWS(fourth.R0[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(fourth.R0[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),(genericEWS(fourth.R0[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(fourth.R0[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata)))
  EWSR0.5[i]<-list(Composite.trait.ews((genericEWS(fifth.R0[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(fifth.R0[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd),(genericEWS(fifth.R0[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                       (genericEWS(fifth.R0[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata)))

  
  Tau.traitR.AR.SD1[i]<-mean(EWSR0.1[[i]]$Tau.Trait.SD.AR)
  Tau.traitR.SD1[i]<-mean(EWSR0.1[[i]]$Tau.Trait.SD)
  Tau.traitR.AR1[i]<-mean(EWSR0.1[[i]]$Tau.Trait.AR)
  TauSD_1[i]<-mean(EWSR0.1[[i]]$Tau.SD)
  TauAR_1[i]<-mean(EWSR0.1[[i]]$Tau.AR)
  Tau.AR.SD1[i]<-mean(EWSR0.1[[i]]$Tau.composite)
  
  
  Tau.trait2[i]<-mean(EWSR0.1[[i]]$Tau.Trait)
  Tau.N2[i]<-mean(EWSR0.1[[i]]$Tau.N)
  
  
  

  Tau.traitR.AR.SD2[i]<-mean(EWSR0.2[[i]]$Tau.Trait.SD.AR)
  Tau.traitR.SD2[i]<-mean(EWSR0.2[[i]]$Tau.Trait.SD)
  Tau.traitR.AR2[i]<-mean(EWSR0.2[[i]]$Tau.Trait.AR)
  TauSD_2[i]<-mean(EWSR0.2[[i]]$Tau.SD)
  TauAR_2[i]<-mean(EWSR0.2[[i]]$Tau.AR)
  Tau.AR.SD2[i]<-mean(EWSR0.2[[i]]$Tau.composite)

  Tau.traitR.AR.SD3[i]<-mean(EWSR0.3[[i]]$Tau.Trait.SD.AR)
  Tau.traitR.AR3[i]<-mean(EWSR0.3[[i]]$Tau.Trait.AR)
  Tau.traitR.SD3[i]<-mean(EWSR0.3[[i]]$Tau.Trait.SD)
  TauSD_3[i]<-mean(EWSR0.3[[i]]$Tau.SD)
  TauAR_3[i]<-mean(EWSR0.3[[i]]$Tau.AR)
  Tau.AR.SD3[i]<-mean(EWSR0.3[[i]]$Tau.composite)

  
  
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


R1.low<-data.frame(Kendall.tau=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitR.AR2,Tau.traitR.SD2,Tau.traitR.AR.SD2),value=c(TauAR_1,TauSD_1,Tau.AR.SD1,Tau.traitR.AR1,Tau.traitR.SD1,Tau.traitR.AR.SD1),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_1))))
R1.low$variation<-factor(R1.low$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
R2.low<-data.frame(Kendall.tau=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitR.AR2,Tau.traitR.SD2,Tau.traitR.AR.SD2),value=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitR.AR2,Tau.traitR.SD2,Tau.traitR.AR.SD2),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_2))))

R2.low$variation<-factor(R2.low$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
R3.low<-data.frame(Kendall.tau=c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitR.AR3,Tau.traitR.SD3,Tau.traitR.AR.SD3),value=c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitR.AR3,Tau.traitR.SD3,Tau.traitR.AR.SD3),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_3))))

R3.low$variation<-factor(R3.low$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
R4.low<-data.frame(Kendall.tau=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitR.AR4,Tau.traitR.SD4,Tau.traitR.AR.SD4),value=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitR.AR4,Tau.traitR.SD4,Tau.traitR.AR.SD4),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_4))))

R4.low$variation<-factor(R4.low$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
R5.low<-data.frame(Kendall.tau=c(TauAR_5,TauSD_5,Tau.AR.SD5,Tau.traitR.AR5,Tau.traitR.SD5,Tau.traitR.AR.SD5),value=c(TauAR_5,TauSD_5,Tau.AR.SD5,Tau.traitR.AR5,Tau.traitR.SD5,Tau.traitR.AR.SD5),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_5))))
#                   Kendall_Tau=c(dr1, dr2,dr3,dr4,dr5),value=c(dr1,dr2,dr3,dr4,dr5))

R5.low$variation<-factor(R5.low$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )



R.trait.ar.sd2<-ggplot(R1.low,aes(variation,Kendall.tau))+geom_boxplot(alpha=0)+ggtitle("AR1")+geom_boxplot(data=R1.low[R1.low$variation=="AR1+Tr",],aes(x=variation,y=Kendall.tau),fill="tomato",alpha=0.2)+
  geom_boxplot(data=R1.low[R1.low$variation=="AR1+SD+Tr",],aes(x=variation,y=Kendall.tau),fill="cyan",alpha=0.2)+geom_boxplot(data=R1.low[R1.low$variation=="SD+Tr",],aes(x=variation,y=Kendall.tau),alpha=0.2,fill="blue")+geom_jitter(alpha=0.2,width = 0.2)+labs(title=(expression(paste("",R[0], " = 1.1"))))+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Metrics")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 8),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")
R.trait.ar.sd3<-ggplot(R2.low,aes(variation,Kendall.tau))+geom_boxplot(alpha=0)+ggtitle("AR1")+geom_boxplot(data=R2.low[R2.low$variation=="AR1+Tr",],aes(x=variation,y=Kendall.tau),fill="tomato",alpha=0.2)+
  geom_boxplot(data=R2.low[R2.low$variation=="AR1+SD+Tr",],aes(x=variation,y=Kendall.tau),fill="cyan",alpha=0.2)+geom_boxplot(data=R2.low[R2.low$variation=="SD+Tr",],aes(x=variation,y=Kendall.tau),alpha=0.2,fill="blue")+geom_jitter(alpha=0.2,width = 0.2)+labs(title=(expression(paste("",R[0], " = 1.2"))))+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Metrics")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 8),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")
R.trait.ar.sd4<-ggplot(R3.low,aes(variation,Kendall.tau))+geom_boxplot(alpha=0)+ggtitle("AR1")+geom_boxplot(data=R3.low[R3.low$variation=="AR1+Tr",],aes(x=variation,y=Kendall.tau),fill="tomato",alpha=0.2)+
  geom_boxplot(data=R3.low[R3.low$variation=="AR1+SD+Tr",],aes(x=variation,y=Kendall.tau),fill="cyan",alpha=0.2)+geom_boxplot(data=R3.low[R3.low$variation=="SD+Tr",],aes(x=variation,y=Kendall.tau),alpha=0.2,fill="blue")+geom_jitter(alpha=0.2,width = 0.2)+labs(title=(expression(paste("",R[0], " = 1.3"))))+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Metrics")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 8),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")
R.trait.ar.sd5<-ggplot(R4.low,aes(variation,Kendall.tau))+geom_boxplot(alpha=0)+ggtitle("AR1")+geom_boxplot(data=R4.low[R4.low$variation=="AR1+Tr",],aes(x=variation,y=Kendall.tau),fill="tomato",alpha=0.2)+
  geom_boxplot(data=R4.low[R4.low$variation=="AR1+SD+Tr",],aes(x=variation,y=Kendall.tau),fill="cyan",alpha=0.2)+geom_boxplot(data=R4.low[R4.low$variation=="SD+Tr",],aes(x=variation,y=Kendall.tau),alpha=0.2,fill="blue")+geom_jitter(alpha=0.2,width = 0.2)+labs(title=(expression(paste("",R[0], " = 1.4")))) +ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Metrics")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 8),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")
R.trait.ar.sd6<-ggplot(R5.low,aes(variation,Kendall.tau))+geom_boxplot(alpha=0)+ggtitle("AR1")+geom_boxplot(data=R5.low[R5.low$variation=="AR1+Tr",],aes(x=variation,y=Kendall.tau),fill="tomato",alpha=0.2)+
  geom_boxplot(data=R5.low[R5.low$variation=="AR1+SD+Tr",],aes(x=variation,y=Kendall.tau),fill="cyan",alpha=0.2)+geom_boxplot(data=R5.low[R5.low$variation=="SD+Tr",],aes(x=variation,y=Kendall.tau),alpha=0.2,fill="blue")+geom_jitter(alpha=0.2,width = 0.2)+labs(title=(expression(paste("",R[0], "= 1.5"))))+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Metrics")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 8),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")


multiplot(R.trait.ar.sd2,R.trait.ar.sd3,R.trait.ar.sd4,R.trait.ar.sd5,R.trait.ar.sd6,cols=2)






############################## Plasticity Data ##########################


load("EWS.lownoise.plasticity.0.05.RData") 
load("EWS.lownoise.plasticity.0.1.RData")
load("EWS.lownoise.plasticity.0.2.RData")      
load("EWS.lownoise.plasticity.0.3.RData") 
load("EWS.lownoise.plasticity.0.4.RData")
load("EWS.lownoise.plasticity.0.5.RData")
load("EWS.lownoise.plasticity.0.8.RData")

library("earlywarnings")



second.p<-lownoise.trait.plasticity0.05
third.p<- lownoise.trait.plasticity0.1 
fourth.p<- lownoise.trait.plasticity0.2
fifth.p<-lownoise.trait.plasticity0.3
six.p<- lownoise.trait.plasticity0.4
sev.p<-lownoise.trait.plasticity0.5
e.p<-lownoise.trait.plasticity0.8
reps1<-length(second.p)
# 
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
  
  EWS.p.2[i]<- list(Composite.trait.ews((genericEWS(second.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),(genericEWS(second.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd),(genericEWS(second.p[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(second.p[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata)))
  EWS.p.3[i]<- list(Composite.trait.ews((genericEWS(third.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),(genericEWS(third.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd),(genericEWS(third.p[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(third.p[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata)))
  EWS.p.4[i]<- list(Composite.trait.ews((genericEWS(fourth.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),(genericEWS(fourth.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd),(genericEWS(fourth.p[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(fourth.p[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata)))
  EWS.p.5[i]<- list(Composite.trait.ews((genericEWS(fifth.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),(genericEWS(fifth.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd),(genericEWS(fifth.p[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(fifth.p[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata) ))
  EWS.p.6[i]<- list(Composite.trait.ews((genericEWS(six.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),(genericEWS(six.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd),(genericEWS(six.p[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(six.p[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata)))
  EWS.p.7[i]<- list(Composite.trait.ews((genericEWS(sev.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),(genericEWS(sev.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd),(genericEWS(sev.p[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                        (genericEWS(sev.p[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata)))
  EWS.p.8[i]<-list(Composite.trait.ews((genericEWS(e.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),(genericEWS(e.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd),(genericEWS(e.p[[i]]$phenotype[500:530],winsize = 50,detrending = "no")$ddata),
                                       (genericEWS(e.p[[i]]$N[500:530],winsize = 50,detrending = "no")$ddata)   ))
  
  
  
  
  
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


P2.low<-data.frame(Kendall.tau=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitP.AR2,Tau.traitP.SD2,Tau.traitP.AR.SD2),value=c(TauAR_2,TauSD_2,Tau.AR.SD2,Tau.traitP.AR2,Tau.traitP.SD2,Tau.traitP.AR.SD2),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_2))))

P2.low$variation<-factor(P2.low$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
P3.low<-data.frame(Kendall.tau=c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitP.AR3,Tau.traitP.SD3,Tau.traitP.AR.SD3),value=c(TauAR_3,TauSD_3,Tau.AR.SD3,Tau.traitP.AR3,Tau.traitP.SD3,Tau.traitP.AR.SD3),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_3))))

P3.low$variation<-factor(P3.low$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
P4.low<-data.frame(Kendall.tau=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitP.AR4,Tau.traitP.SD4,Tau.traitP.AR.SD4),value=c(TauAR_4,TauSD_4,Tau.AR.SD4,Tau.traitP.AR4,Tau.traitP.SD4,Tau.traitP.AR.SD4),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_4))))

P4.low$variation<-factor(P4.low$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a
P5.low<-data.frame(Kendall.tau=c(TauAR_5,TauSD_5,Tau.AR.SD5,Tau.traitP.AR5,Tau.traitP.SD5,Tau.traitP.AR.SD5),value=c(TauAR_5,TauSD_5,Tau.AR.SD5,Tau.traitP.AR5,Tau.traitP.SD5,Tau.traitP.AR.SD5),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_5))))
P5.low$variation<-factor(P5.low$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )


#                   Kendall_Tau=c(dr1, dr2,dr3,dr4,dr5),value=c(dr1,dr2,dr3,dr4,dr5))
P6.low<-data.frame(Kendall.tau=c(TauAR_6,TauSD_6,Tau.AR.SD6,Tau.traitP.AR6,Tau.traitP.SD6,Tau.traitP.AR.SD6),value=c(TauAR_6,TauSD_6,Tau.AR.SD6,Tau.traitP.AR6,Tau.traitP.SD6,Tau.traitP.AR.SD6),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_6))))

P6.low$variation<-factor(P6.low$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )


P8.low<-data.frame(Kendall.tau=c(TauAR_1,TauSD_1,Tau.AR.SD1,Tau.traitP.AR1,Tau.traitP.SD1,Tau.traitP.AR.SD1),value=c(TauAR_1,TauSD_1,Tau.AR.SD1,Tau.traitP.AR1,Tau.traitP.SD1,Tau.traitP.AR.SD1),
               variation=factor(rep(c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"),each=length(TauAR_1))))
P8.low$variation<-factor(P8.low$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )

# a

P.trait.ar.sd2<-ggplot(P2.low,aes(variation,Kendall.tau))+geom_boxplot(alpha=0)+ggtitle("AR1")+geom_boxplot(data=P2.low[P2.low$variation=="AR1+Tr",],aes(x=variation,y=Kendall.tau),fill="tomato",alpha=0.2)+
  geom_boxplot(data=P2.low[P2.low$variation=="AR1+SD+Tr",],aes(x=variation,y=Kendall.tau),fill="cyan",alpha=0.2)+geom_boxplot(data=P2.low[P2.low$variation=="SD+Tr",],aes(x=variation,y=Kendall.tau),alpha=0.2,fill="blue")+geom_jitter(alpha=0.2,width = 0.2)+labs(title=(expression(paste("",b, "= 0.05"))))+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Metrics")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 8),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")
P.trait.ar.sd3<-ggplot(P3.low,aes(variation,Kendall.tau))+geom_boxplot(alpha=0)+ggtitle("AR1")+geom_boxplot(data=P3.low[P3.low$variation=="AR1+Tr",],aes(x=variation,y=Kendall.tau),fill="tomato",alpha=0.2)+
  geom_boxplot(data=P3.low[P3.low$variation=="AR1+SD+Tr",],aes(x=variation,y=Kendall.tau),fill="cyan",alpha=0.2)+geom_boxplot(data=P3.low[P3.low$variation=="SD+Tr",],aes(x=variation,y=Kendall.tau),alpha=0.2,fill="blue")+geom_jitter(alpha=0.2,width = 0.2)+labs(title=(expression(paste("",b, "= 0.1"))))+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Metrics")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 8),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")

P.trait.ar.sd5<-ggplot(P5.low,aes(variation,Kendall.tau))+geom_boxplot(alpha=0)+ggtitle("AR1")+geom_boxplot(data=P5.low[P5.low$variation=="AR1+Tr",],aes(x=variation,y=Kendall.tau),fill="tomato",alpha=0.2)+
  geom_boxplot(data=P5.low[P5.low$variation=="AR1+SD+Tr",],aes(x=variation,y=Kendall.tau),fill="cyan",alpha=0.2)+geom_boxplot(data=P5.low[P5.low$variation=="SD+Tr",],aes(x=variation,y=Kendall.tau),alpha=0.2,fill="blue")+geom_jitter(alpha=0.2,width = 0.2)+labs(title=(expression(paste("",b, "= 0.4"))))+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Metrics")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 8),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")
P.trait.ar.sd6<-ggplot(P6.low,aes(variation,Kendall.tau))+geom_boxplot(alpha=0)+ggtitle("AR1")+geom_boxplot(data=P6.low[P6.low$variation=="AR1+Tr",],aes(x=variation,y=Kendall.tau),fill="tomato",alpha=0.2)+
  geom_boxplot(data=P6.low[P6.low$variation=="AR1+SD+Tr",],aes(x=variation,y=Kendall.tau),fill="cyan",alpha=0.2)+geom_boxplot(data=P6.low[P6.low$variation=="SD+Tr",],aes(x=variation,y=Kendall.tau),alpha=0.2,fill="blue")+geom_jitter(alpha=0.2,width = 0.2)+labs(title=(expression(paste("",b, "= 0.5"))))+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Metrics")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 8),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")
#e<-ggplot(Ktau_DR,aes(Variation,Kendall_Tau))+geom_boxplot()+geom_jitter(width=0.2)+ggtitle("SD")+ylim(c(-1,1))+ylab("Kendall Tau")
#e

P.trait.ar.sd8<-ggplot(P8.low,aes(variation,Kendall.tau))+geom_boxplot(alpha=0)+ggtitle("AR1")+geom_boxplot(data=P8.low[P8.low$variation=="AR1+Tr",],aes(x=variation,y=Kendall.tau),fill="tomato",alpha=0.2)+
  geom_boxplot(data=P8.low[P8.low$variation=="AR1+SD+Tr",],aes(x=variation,y=Kendall.tau),fill="cyan",alpha=0.2)+geom_boxplot(data=P8.low[P8.low$variation=="SD+Tr",],aes(x=variation,y=Kendall.tau),alpha=0.2,fill="blue")+geom_jitter(alpha=0.2,width = 0.2)+labs(title=(expression(paste("",b, "= 0.8"))))+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Metrics")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 8),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")

#pdf("TRAIT-based-EWS-1-plasticity.pdf",width = 10,height = 8)
multiplot(P.trait.ar.sd2,P.trait.ar.sd3,P.trait.ar.sd5,P.trait.ar.sd6,P.trait.ar.sd8,cols=2)
#dev.off()

#genetic variation ddata



data.g.lownoise<-data.frame(rbind(var.0.05.low,var.0.1.low, var.0.2.low, var.0.3.low,var.0.4.low,var.0.5.low),
                            Genetic_variation =c(rep("0.05",each=600), rep("0.1", each=600),rep("0.2",each=600), rep("0.3",each=600), 
                                                 rep("0.4",each=600),rep( "0.5",each=600)) )
# reproductive rate data
data.r.lownoise<-data.frame(rbind(R1.low,R2.low, R3.low, R4.low,R5.low),
                            R0 =c(rep("1.1",each=600), rep("1.2", each=600),rep("1.3",each=600), rep("1.4",each=600), 
                                                 rep("1.5",each=600)))

# plasticity data

data.p.lownoise<-data.frame(rbind(P2.low,P3.low, P4.low, P5.low,P6.low,P8.low),
                            Plasticity =c(rep("0.05",each=600), rep("0.1", each=600),rep("0.2",each=600), rep("0.3",each=600), 
                                                 rep("0.4",each=600),rep( "0.8",each=600)) )

