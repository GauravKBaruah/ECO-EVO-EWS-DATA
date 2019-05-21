

#this particular script will reproduce figure 1 and figure S3, S4 and S5.
rm(list=ls())

#libraries needed
library("earlywarnings")
require(grid)
require(gridExtra)
require(ggplot2)
source('~/Dropbox/Zurich PhD Research/2_Chapter_2/J Animal Ecology codes/Functions_J_A_Ecology.R')


# loading the genetic variation data
load("EWS.genvar0.05.RData") 
load("EWS.genvar0.1.RData")  
load("EWS.genvar0.2.RData") 
load("EWS.genvar0.3.RData")
load("EWS.genvar0.4.RData")
load("EWS.genvar0.5.RData")


#assigning the data
second.op<-EWS.trait.genvar.0.05
third.op<- EWS.trait.genvar.0.1 
fourth.op<- EWS.trait.genvar.0.2
fifth.op<-EWS.trait.genvar.0.3  
six.op<- EWS.trait.genvar.0.4
sev.op<-EWS.trait.genvar.0.5

#initializing empty lists
EWS.gen.1<-list()
EWS.gen.2<-list()
EWS.gen.3<-list()
EWS.gen.4<-list()
EWS.gen.5<-list()
EWS.gen.6<-list()
EWS.gen.7<-list()
EWS.gen.8<-list()

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

#reps of 100 replicates analysis

for(i in 1:100){
  
  # calling a function genericEWS within Composite.ews() from the Functions_J_A_Ecology script. Note the genericEWS function is a modified script from the library earlywarnings
  EWS.gen.2[i]<- list(Composite.ews((genericEWS(second.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(second.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd)))
  EWS.gen.3[i]<- list(Composite.ews((genericEWS(third.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(third.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd)))
  EWS.gen.4[i]<- list(Composite.ews((genericEWS(fourth.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(fourth.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd)))
  EWS.gen.5[i]<- list(Composite.ews((genericEWS(fifth.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(fifth.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd)))
  EWS.gen.6[i]<- list(Composite.ews((genericEWS(six.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(six.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd)))
  EWS.gen.7[i]<- list(Composite.ews((genericEWS(sev.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(sev.op[[i]]$N[500:530],winsize = 50,detrending = "gaussian",bandwidth = 50)$sd)))
   
 # EWS.gen.2[[i]]$Tau.SD gives a list of Kendall Tau values of SD with all having the same Kendall Tau value. 
  TauSD_2[i]<-mean(EWS.gen.2[[i]]$Tau.SD)
  TauAR_2[i]<-mean(EWS.gen.2[[i]]$Tau.AR)


 TauSD_3[i]<-mean(EWS.gen.3[[i]]$Tau.SD)
 TauAR_3[i]<-mean(EWS.gen.3[[i]]$Tau.AR)


 TauSD_4[i]<-mean(EWS.gen.4[[i]]$Tau.SD)
 TauAR_4[i]<-mean(EWS.gen.4[[i]]$Tau.AR)

 TauSD_5[i]<-mean(EWS.gen.5[[i]]$Tau.SD)
 TauAR_5[i]<-mean(EWS.gen.5[[i]]$Tau.AR)

 TauSD_6[i]<-mean(EWS.gen.6[[i]]$Tau.SD)
 TauAR_6[i]<-mean(EWS.gen.6[[i]]$Tau.AR)

 TauSD_7[i]<-mean(EWS.gen.7[[i]]$Tau.SD)
 TauAR_7[i]<-mean(EWS.gen.7[[i]]$Tau.AR)


 print(i)
 }

# data frame
Gtau_AR1<-data.frame(Variation=factor(rep(c("0.05","0.1","0.2","0.3","0.4","0.5"),each=length(TauAR_2))),
                     Kendall_Tau=c( TauAR_2,TauAR_3,TauAR_4,TauAR_5,TauAR_6,TauAR_7),value=c( TauAR_2,TauAR_3,TauAR_4,TauAR_5,TauAR_6,TauAR_7))


Gtau_SD1<-data.frame(Variation=factor(rep(c("0.05","0.1","0.2","0.3","0.4","0.5"),each=length(TauSD_2))),
                     Kendall_Tau=c(TauSD_2,TauSD_3,TauSD_4,TauSD_5,TauSD_6,TauSD_7),value=c(TauSD_2,TauSD_3,TauSD_4,TauSD_5,TauSD_6,TauSD_7))


Gar1<-ggplot(Gtau_AR1,aes(Variation,Kendall_Tau))+geom_boxplot(alpha=0)+geom_jitter(alpha=0.3,width = 0.1)+ggtitle("AR1")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
        text = element_text(size = 12 ),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "right") +
  scale_fill_brewer(palette = "Accent")+
    ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Genetic variation")


Gsd1<-ggplot(Gtau_SD1,aes(Variation,Kendall_Tau))+geom_boxplot(alpha=0.1)+geom_jitter(alpha=0.3,width=0.1)+ggtitle("SD")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
        text = element_text(size = 12 ),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "right") +
  scale_fill_brewer(palette = "Accent")+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Genetic variation")




################################  Reproduction  rate data #################

# calling reproductive rate data
load("EWS.reproduction1.1.RData")
load("EWS.reproduction1.2.RData") 
load("EWS.reproduction1.3.RData")  
load("EWS.reproduction1.4.RData")                        
load("EWS.reproduction1.5.RData")





first.R0<-EWS.trait.reproduction_1.1
second.R0<-EWS.trait.reproduction_1.2
third.R0<- EWS.trait.reproduction_1.3
fourth.R0<- EWS.trait.reproduction_1.4
fifth.R0<-EWS.trait.reproduction_1.5


EWS.pl.1<-list()
EWS.pl.2<-list()
EWS.pl.3<-list()
EWS.pl.4<-list()
EWS.pl.5<-list()

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


#similar stuff like the one above for genetic variation
for(i in 1:100){
  EWS.pl.1[i]<-list(Composite.ews( (genericEWS(first.R0[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),(genericEWS(first.R0[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd)))
  EWS.pl.2[i]<- list(Composite.ews( (genericEWS(second.R0[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(second.R0[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd)))
  EWS.pl.3[i]<- list(Composite.ews( (genericEWS(third.R0[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(third.R0[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd)))
  EWS.pl.4[i]<- list(Composite.ews( (genericEWS(fourth.R0[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(fourth.R0[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd)))
  EWS.pl.5[i]<- list(Composite.ews( (genericEWS(fifth.R0[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth = 50)$ar1),(genericEWS(fifth.R0[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd)))
  
  
  TauSD_1[i]<-mean(EWS.pl.1[[i]]$Tau.SD)
  TauAR_1[i]<-mean(EWS.pl.1[[i]]$Tau.AR)

  
 TauSD_2[i]<-mean(EWS.pl.2[[i]]$Tau.SD)
  TauAR_2[i]<-mean(EWS.pl.2[[i]]$Tau.AR)

  
  TauSD_3[i]<-mean(EWS.pl.3[[i]]$Tau.SD)
  TauAR_3[i]<-mean(EWS.pl.3[[i]]$Tau.AR)

  
  
 TauSD_4[i]<-mean(EWS.pl.4[[i]]$Tau.SD)
TauAR_4[i]<-mean(EWS.pl.4[[i]]$Tau.AR)

  
  TauSD_5[i]<-mean(EWS.pl.5[[i]]$Tau.SD)
  TauAR_5[i]<-mean(EWS.pl.5[[i]]$Tau.AR)

  
  
  print(i)
}

Rtau_AR1<-data.frame(Variation=factor(rep(c("1.1","1.2", "1.3","1.4","1.5"),each=length(TauAR_1))),
                     Kendall_Tau=c(TauAR_1, TauAR_2,TauAR_3,TauAR_4,TauAR_5),value=c(TauAR_1, TauAR_2,TauAR_3,TauAR_4,TauAR_5))

Rtau_SD1<-data.frame(Variation=factor(rep(c("1.1","1.2", "1.3","1.4","1.5"),each=length(TauSD_1))),
                     Kendall_Tau=c(TauSD_1, TauSD_2,TauSD_3,TauSD_4,TauSD_5),value=c(TauSD_1 ,TauSD_2,TauSD_3,TauSD_4,TauSD_5))


R.ar1<-ggplot(Rtau_AR1,aes(Variation,Kendall_Tau))+geom_boxplot(alpha=0.1)+geom_jitter(alpha=0.3,width=0.1)+ggtitle("AR1")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
        text = element_text(size = 12 ),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "right") +
  scale_fill_brewer(palette = "Accent")+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Reproductive Rate")


R.sd1<-ggplot(Rtau_SD1,aes(Variation,Kendall_Tau))+geom_boxplot(alpha=0.1)+geom_jitter(alpha=0.3,width=0.1)+ggtitle("SD")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
        text = element_text(size = 12 ),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        legend.position = "right") +
  scale_fill_brewer(palette = "Accent")+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Reproductive rate")



############################## Plasticity Data ##########################


load("EWS.plasticity.0.05.RData") 
load("EWS.plasticity.0.1.RData")  
load("EWS.plasticity.0.2.RData")      
load("EWS.plasticity.0.3.RData")  
load("EWS.plasticity.0.4.RData")
load("EWS.plasticity.0.5.RData")
load("EWS.plasticity.0.8.RData")





second.p<-EWS.trait.plasticity0.05
third.p<- EWS.trait.plasticity0.1 
fourth.p<- EWS.trait.plasticity0.2
fifth.p<-EWS.trait.plasticity0.3
six.p<- EWS.trait.plasticity0.4
sev.p<-EWS.trait.plasticity0.5
e.p<-EWS.trait.plasticity0.8

# 

EWS.p.1<-list()
EWS.p.2<-list()
EWS.p.3<-list()
EWS.p.4<-list()
EWS.p.5<-list()
EWS.p.6<-list()
EWS.p.7<-EWS.p.8<-list()

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
  EWS.p.2[i]<- list(Composite.ews((genericEWS(second.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),(genericEWS(second.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd)))
  EWS.p.3[i]<- list(Composite.ews((genericEWS(third.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),(genericEWS(third.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd)))
  EWS.p.4[i]<- list(Composite.ews((genericEWS(fourth.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),(genericEWS(fourth.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd)))
  EWS.p.5[i]<- list(Composite.ews((genericEWS(fifth.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),(genericEWS(fifth.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd)))
  EWS.p.6[i]<- list(Composite.ews((genericEWS(six.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),(genericEWS(six.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd)))
  EWS.p.7[i]<- list(Composite.ews((genericEWS(sev.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),(genericEWS(sev.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd)))
  EWS.p.8[i]<-list(Composite.ews((genericEWS(e.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$ar1),(genericEWS(e.p[[i]]$N[500:530],winsize=50,detrending = "gaussian",bandwidth =50 )$sd)))
  
  
  
  
  TauSD_2[i]<-mean(EWS.p.2[[i]]$Tau.SD)
  TauAR_2[i]<-mean(EWS.p.2[[i]]$Tau.AR)
  
  
  TauSD_3[i]<-mean(EWS.p.3[[i]]$Tau.SD)
  TauAR_3[i]<-mean(EWS.p.3[[i]]$Tau.AR)
  
  
  TauSD_4[i]<-mean(EWS.p.4[[i]]$Tau.SD)
  TauAR_4[i]<-mean(EWS.p.4[[i]]$Tau.AR)
  
  TauSD_5[i]<-mean(EWS.p.5[[i]]$Tau.SD)
  TauAR_5[i]<-mean(EWS.p.5[[i]]$Tau.AR)
  
  TauSD_6[i]<-mean(EWS.p.6[[i]]$Tau.SD)
  TauAR_6[i]<-mean(EWS.p.6[[i]]$Tau.AR)
  
  TauSD_7[i]<-mean(EWS.p.7[[i]]$Tau.SD)
  TauAR_7[i]<-mean(EWS.p.7[[i]]$Tau.AR)
  # 
  TauSD_1[i]<-mean(EWS.p.8[[i]]$Tau.SD)
   TauAR_1[i]<-mean(EWS.p.8[[i]]$Tau.AR)
  # 
  print(i)
  
  
}

Ptau_AR1<-data.frame(Variation=factor(rep(c("0.05", "0.1","0.3","0.3", "0.4","0.8"),each=length(TauAR_2))),
                     Kendall_Tau=c(TauAR_2,TauAR_3,TauAR_5,TauAR_6,TauAR_7,TauAR_1),value=c(TauAR_2,TauAR_3,TauAR_5,TauAR_6,TauAR_7,TauAR_1))

Ptau_SD1<-data.frame(Variation=factor(rep(c("0.05", "0.1","0.3","0.3", "0.4","0.8"),each=length(TauSD_2))),
                     Kendall_Tau=c( TauSD_2,TauSD_3,TauSD_5, TauSD_6,TauSD_7,TauSD_1),value=c( TauSD_2,TauSD_3,TauSD_5, TauSD_6,TauSD_7,TauSD_1))


Par1<-ggplot(Ptau_AR1,aes(Variation,Kendall_Tau))+geom_boxplot(alpha=0.1)+geom_jitter(alpha=0.3,width=0.1)+ggtitle("AR1")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 11),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Strength of plasticity")



Psd<-ggplot(Ptau_SD1,aes(Variation,Kendall_Tau))+geom_boxplot(alpha=0.1)+geom_jitter(alpha=0.3,width=0.1)+ggtitle("SD")+
  theme_bw()+theme(plot.title = element_text(size = 14,   face = "bold"),
                   text = element_text(size = 12 ),
                   axis.title = element_text(face="bold"),
                   axis.text.x=element_text(size = 11),
                   legend.position = "right") +
  scale_fill_brewer(palette = "Accent")+ylim(c(-1,1))+ylab("Kendall Tau")+xlab("Strength of plasticity")



#pdf("03_Fig_abundanceEWS-all-factors-30datapoints.pdf", width=9, height =7)
multiplot(Psd,Gsd1,R.sd1,Par1, Gar1,R.ar1, cols=2)
#dev.off()





######### Plotting population dynamics of genetic variation  #############
######### Plotting population dynamics of genetic variation #############

r2 = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
#pdf("short_genetic-variation-population-collapse_short.pdf")
par(mfrow=c(3,2),mar=c(1,4,1,1)+0.9)


t<-seq(500,550)
plot(0,0,xlim = c(500,550),ylim = c(0,80),type = "n",xlab="Time",ylab = "Population size",main = "var = 0.06")
for (i in 1:100){
  lines(t[1:31],second.op[[i]]$N[500:530],col=r2[i])
  lines(t[31:50],second.op[[i]]$N[531:550],col='grey')
  
}
abline(v=530,col='red',lwd=2,lty=2);abline(v=500,col='steelblue',lwd=2,lty=2)


t<-seq(500,550)
plot(0,0,xlim = c(500,550),ylim = c(0,80),type = "n",xlab="Time",ylab = "Population size",main = "var = 0.1")
for (i in 1:100){
  lines(t[1:31],third.op[[i]]$N[500:530],col=r2[i])
  lines(t[31:50],third.op[[i]]$N[531:550],col='grey')
  
}
abline(v=530,col='red',lwd=2, lty=2);abline(v=500,col='steelblue',lwd=2,lty=2)


t<-seq(500,550)
plot(0,0,xlim = c(500,550),ylim = c(0,80),type = "n",xlab="Time",ylab = "Population size",main = "var = 0.2")
for (i in 1:100){
  lines(t[1:31],fourth.op[[i]]$N[500:530],col=r2[i])
  lines(t[31:50],fourth.op[[i]]$N[531:550],col='grey')
  
}
abline(v=530,col='red',lwd=2,lty=2);abline(v=500,col='steelblue',lwd=2,lty=2)


t<-seq(500,550)
plot(0,0,xlim = c(500,550),ylim = c(0,80),type = "n",xlab="Time",ylab = "Population size",main = "var = 0.3")
for (i in 1:100){
  lines(t[1:31],fifth.op[[i]]$N[500:530],col=r2[i])
  lines(t[31:50],fifth.op[[i]]$N[531:550],col='grey')
}
abline(v=530,col='red',lwd=2,lty=2);abline(v=500,col='steelblue',lwd=2,lty=2)

t<-seq(500,550)
plot(0,0,xlim = c(500,550),ylim = c(0,80),type = "n",xlab="Time",ylab = "Population size",main = "var = 0.4")
for (i in 1:100){
  lines(t[1:31],six.op[[i]]$N[500:530],col=r2[i])
  lines(t[31:50],six.op[[i]]$N[531:550],col='grey')
}
abline(v=530,col='red',lwd=2,lty=2);abline(v=500,col='steelblue',lwd=2,lty=2)


#dev.off()



############################## plotting population dynamics Reproduction plots ####################################################

############################## plotting population dynamics Reproduction plots ####################################################
#pdf("Short_Reproduction_plots_collapse_short.pdf")
par(mfrow=c(3,2),mar=c(1,4,1,1)+0.9)


t<-seq(500,550)
plot(0,0,xlim = c(500,550),ylim = c(0,80),type = "n",xlab="Time",ylab = "Population size",main = "R0 = 1.1")

for (i in 1:100){
  lines(t[1:31],first.R0[[i]]$N[500:530],col=r2[i])
  lines(t[31:50],first.R0[[i]]$N[531:550],col='grey')
  
}
abline(v=530,col='red',lwd=2,lty=2);abline(v=500,col='steelblue',lwd=2,lty=2)


t<-seq(500,550)
plot(0,0,xlim = c(500,550),ylim = c(0,80),type = "n",xlab="Time",ylab = "Population size",main = "R0 = 1.2")
for (i in 1:100){
  lines(t[1:31],second.R0[[i]]$N[500:530],col=r2[i])
  lines(t[31:50],second.R0[[i]]$N[531:550],col='grey')
  
}
abline(v=530,col='red',lwd=2,lty=2);abline(v=500,col='steelblue',lwd=2,lty=2)


t<-seq(500,550)
plot(0,0,xlim = c(500,550),ylim = c(0,80),type = "n",xlab="Time",ylab = "Population size",main = "R0 = 1.3")
for (i in 1:100){
  lines(t[1:31],third.R0[[i]]$N[500:530],col=r2[i])
  lines(t[31:50],third.R0[[i]]$N[531:550],col='grey')
  
}
abline(v=530,col='red',lwd=2,lty=2);abline(v=500,col='steelblue',lwd=2,lty=2)


t<-seq(500,550)
plot(0,0,xlim = c(500,550),ylim = c(0,80),type = "n",xlab="Time",ylab = "Population size",main = "R0 = 1.4")
for (i in 1:100){
  lines(t[1:31],fourth.R0[[i]]$N[500:530],col=r2[i])
  lines(t[31:50],fourth.R0[[i]]$N[531:550],col='grey')
  
}
abline(v=530,col='red',lwd=2,lty=2);abline(v=500,col='steelblue',lwd=2,lty=2)


t<-seq(500,550)
plot(0,0,xlim = c(500,550),ylim = c(0,80),type = "n",xlab="Time",ylab = "Population size",main = "R0 = 1.5")
for (i in 1:100){
  lines(t[1:31],fifth.R0[[i]]$N[500:530],col=r2[i])
  lines(t[31:50],fifth.R0[[i]]$N[531:550],col='grey')
  
}
abline(v=530,col='red',lwd=2,lty=2);abline(v=500,col='steelblue',lwd=2,lty=2)
#dev.off()


############################################## plotting population dynamics plasticity plots#############################################
#######################################################################################################
#pdf("short_plasticity-population-collapse_short.pdf")
par(mfrow=c(3,2),mar=c(1,4,1,1)+0.9)



t<-seq(500,550)
plot(0,0,xlim = c(500,550),ylim = c(0,80),type = "n",xlab="Time",ylab = "Population size",main = "b = 0.05")
for (i in 1:100){
  lines(t[1:31],second.p[[i]]$N[500:530],col=r2[i])
  lines(t[31:50],second.p[[i]]$N[531:550],col='grey')
  
}
abline(v=530,col='red',lwd=2,lty=2);abline(v=500,col='steelblue',lwd=2,lty=2)



t<-seq(500,550)
plot(0,0,xlim = c(500,550),ylim = c(0,80),type = "n",xlab="Time",ylab = "Population size",main = "b = 0.1")
for (i in 1:100){
  lines(t[1:31],third.p[[i]]$N[500:530],col=r2[i])
  lines(t[31:50],third.p[[i]]$N[531:550],col='grey')
}
abline(v=530,col='red',lwd=2,lty=2);abline(v=500,col='steelblue',lwd=2,lty=2)




t<-seq(500,550)
plot(0,0,xlim = c(500,550),ylim = c(0,80),type = "n",xlab="Time",ylab = "Population size",main = "b = 0.3")
for (i in 1:100){
  lines(t[1:31],fifth.p[[i]]$N[500:530],col=r2[i])
  lines(t[31:50],fifth.p[[i]]$N[531:550],col='grey')
  
}
abline(v=530,col='red',lwd=2,lty=2);abline(v=500,col='steelblue',lwd=2,lty=2)

t<-seq(500,550)
plot(0,0,xlim = c(500,550),ylim = c(0,80),type = "n",xlab="Time",ylab = "Population size",main = "b = 0.4")
for (i in 1:100){
  lines(t[1:31],six.p[[i]]$N[500:530],col=r2[i])
  lines(t[31:50],six.p[[i]]$N[531:550],col='grey')
  
}
abline(v=530,col='red',lwd=2,lty=2);abline(v=500,col='steelblue',lwd=2,lty=2)


t<-seq(500,550)
plot(0,0,xlim = c(500,550),ylim = c(0,80),type = "n",xlab="Time",ylab = "Population size",main = "b = 0.5")

for (i in 1:100){
  lines(t[1:31],sev.p[[i]]$N[500:530],col=r2[i])
  lines(t[31:50],sev.p[[i]]$N[531:550],col='grey')
  
}
abline(v=530,col='red',lwd=2,lty=2);abline(v=500,col='steelblue',lwd=2,lty=2)

t<-seq(500,550)
plot(0,0,xlim = c(500,550),ylim = c(0,80),type = "n",xlab="Time",ylab = "Population size",main = "b = 0.8")
for (i in 1:100){
  lines(t[1:31],e.p[[i]]$N[500:530],col=r2[i])
  lines(t[31:50],e.p[[i]]$N[531:550],col='grey')
  # lines(t,fifth.op[[i]]$N,col=r3[i])
  
}
abline(v=530,col='red',lwd=2,lty=2);abline(v=500,col='steelblue',lwd=2,lty=2)

#dev.off()





################### Recovery time ####

rm(list=ls())
load("Genetic.var_0.05.recovery.RData")
load("Genetic.var.0.5_recovery.RData") #low plasticity low genvariation

first.op<- EWS.trait.recovery.genvar.0.05
second.op<-EWS.trait.recovery.genvar.0.5
library("earlywarnings")
source('~/Dropbox/Zurich PhD Research/Trait Dyanamics chapter 1/rolling_GAMs_methods.R', echo=TRUE)

# Lamdba vales for evolving gentic var
t.recover<-function(N){
  
  
  lowN<-numeric(); lowN<-min(N)
  K_max<-max(N[505:1000])
  t.rec<-match(K_max,N[501:1000])
  t.min<-match(lowN,N); time.rec<-t.rec+505-t.min
  return(time.rec)
}

t.recovery<-matrix(NA,ncol=2,nrow=100)

for (i in 1:100){
  
  t.recovery[i,1]<-t.recover(first.op[[i]]$N)
  t.recovery[i,2]<-t.recover(second.op[[i]]$N)
  
}

pdf("Genetic-variation-rescue.pdf")
boxplot(t.recovery, ylab="Time to recover to carrying capacity",names=c("Genetic variation=0.05", "Genetic variation =0.5"), col=c("grey","white"))
dev.off()
