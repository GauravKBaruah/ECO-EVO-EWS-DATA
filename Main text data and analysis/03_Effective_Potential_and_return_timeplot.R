# this script will reproduce figure 2 of the main text and Figure S1 of the appendix


rm(list=ls())



########
library(png); 
library(grid);
source('~/Dropbox/Zurich PhD Research/2_Chapter_2/J Animal Ecology codes/Functions_J_A_Ecology.R')
best_color_paletter<-c("#A42820", "#5F5647",  "#E58601",  "#00A08A" ,
                       "#4E2A1E","#000066", "#0C1707", "#000066")


# assembling a data frame of ball figures for the effective potential plot
dd<-data.frame(images=list.files("~/Dropbox/Zurich PhD Research/2_Chapter_2/EWS-Trait/Potential_plot/", full.names = T), stringsAsFactors = F)
dd$names = gsub("[a-zA-Z]|[[:punct:]]","",dd$images)
dd$values = sample(1:100, size=nrow(dd))
img = readPNG(dd$images[1])
g =  rasterGrob(img, interpolate=TRUE)

#variables needed for the effective potential of genetic variation
R0=1.2; K=73; k=log(K); del=0; omega=20; # all the constants for the effective potential barring genetic variatioin
r =log(R0);
sigma_z=c(0.02,0.6,1); # this sequence is for genetic variatioin

N <-seq(8900,0.1,-2); n<-log(N);

V1<-numeric();V2<-numeric(); V3<-numeric();V4<-numeric();V5<-numeric();V6<-numeric()
#Potential well plots- Potential function for different levels of genetic variation from equation 8 in the main-text
V1<-(r/k)*n^2/2 + (0.5*(1/R0)*del^2*r*n)/(sigma_z[1]+omega) - log(sqrt(omega/(sigma_z[1]+omega)))*n -r*n

V2<-(r/k)*n^2/2 + (0.5*(1/R0)*del^2*r*n)/(sigma_z[2]+omega) - log(sqrt(omega/(sigma_z[2]+omega)))*n -r*n

V3<-(r/k)*n^2/2 + (0.5*(1/R0)*del^2*r*n)/(sigma_z[3]+omega) - log(sqrt(omega/(sigma_z[3]+omega)))*n -r*n



# creating the data frame of V1 (effective potential) for different levels of genetic variatioin
genvar.potential<-data.frame( Abundance=log(N),values = c(V1,V2,V3),Genetic_variation = factor(c(rep("0.02",each=length(V1)),
                                                                       rep("0.6", each= length(V2)),
                                                                       rep("1", each= length(V3)))))


fp.gc<-ggplot(data= genvar.potential, aes(y = values,x =Abundance , color = Genetic_variation )) +geom_line()+
  ylab("Effective potential V(N)")+xlab("Population size (log-transformed)")+ggtitle("A")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Genetic variation")


g = list()
KK<-c(n[which(V1==min(V1))],n[which(V2==min(V2))],
      n[which(V3==min(V3))]) # this assembles the vector of carrying capacity or the point at which the population has the lowest effective potential value.


VV<-c(min(V1),min(V2),min(V3)) # this vector assembles the lowest value of effective potential

# this for low below manages to put the .png pics of the ball at the lowest point in the effective potential curve
for(i in 1:(nrow(dd)-2)){
  img = readPNG(dd$images[i])
  g[[i]] =  rasterGrob(img, interpolate=TRUE)
  
  fp.gc = fp.gc +
    annotation_custom(grob=g[[i]], xmin=KK[i], xmax=KK[i]+.5, ymin=VV[i], ymax=VV[i]+0.03)
}

fp.gc



######### Reproductive rate R0 ##########

R0=c(1.1,1.2,1.3,1.4,1.5); K=73; k=log(K); del=0; sigma_z=0.25;omega=20;
r =log(R0);

N <-seq(8900,0.1,-2); n<-log(N);
#Potential well plots- Potential function for different levels of R0 from equation 8 in the main-text
VR1<-(r[1]/k)*n^2/2 + (0.5*(1/R0[1])*del^2*r[1]*n)/(sigma_z+omega) - log(sqrt(omega/(sigma_z+omega)))*n -r[1]*n
VR2<-(r[2]/k)*n^2/2 + (0.5*(1/R0[2])*del^2*r[2]*n)/(sigma_z+omega) - log(sqrt(omega/(sigma_z+omega)))*n -r[2]*n
VR3<-(r[3]/k)*n^2/2 + (0.5*(1/R0[3])*del^2*r[3]*n)/(sigma_z+omega) - log(sqrt(omega/(sigma_z+omega)))*n -r[3]*n
VR4<-(r[4]/k)*n^2/2 + (0.5*(1/R0[4])*del^2*r[4]*n)/(sigma_z+omega) - log(sqrt(omega/(sigma_z+omega)))*n -r[4]*n
VR5<-(r[5]/k)*n^2/2 + (0.5*(1/R0[5])*del^2*r[5]*n)/(sigma_z+omega) - log(sqrt(omega/(sigma_z+omega)))*n -r[5]*n


# creating the data frame of V1 (effective potential) for different levels of genetic variatioin
R0.potential<-data.frame( Abundance=log(N),values = 
                            c(VR1,VR2,VR3,VR4,VR5),
                          R0 = factor(c(rep("1.1",each=length(VR1)),
                                                       rep("1.2", each= length(VR2)),
                                                       rep("1.3", each= length(VR3)),
                                                       rep("1.4", each= length(VR4)),
                                                       rep("1.5", each= length(VR5)))))

fp.R<-ggplot(data= R0.potential, aes(y = values,x =Abundance , color = R0 )) +geom_line()+
  ylab("Effective potential V(N)")+xlab("Population size (log-transformed)")+ggtitle("B")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Net reproductive rate")


gR = list()
KKR<-c(n[which(VR1==min(VR1))],n[which(VR2==min(VR2))],
      n[which(VR3==min(VR3))],n[which(VR4==min(VR4))],
      n[which(VR5==min(VR5))])#this assembles the vector of carrying capacity or the point at which the population has the lowest effective potential value.

VVR<-c(min(VR1),min(VR2),min(VR3),min(VR4),min(VR5))# this vector assembles the lowest value of effective potential
for(i in 1:(nrow(dd))){
  img = readPNG(dd$images[i])
  gR[[i]] =  rasterGrob(img, interpolate=TRUE)
  
  fp.R = fp.R +
    annotation_custom(grob=gR[[i]], xmin=KKR[i]-0.25, xmax=KKR[i]+0.25, ymin=VVR[i], ymax=VVR[i]+0.045)
}

fp.R
 


############## plasticity ##############

R0=1.2; K=73; k=log(K); 
sigma_z=0.25;omega=20; del=c(0,(1-2),(0.1-2))
r =log(R0);

N <-seq(8900,0.1,-2); n<-log(N);



#Potential well plots- Potential function for different levels of plasticity from equation 8 in the main-text
Vp1<-(r/k)*n^2/2 + (1*(1/R0)*del[1]^2*r*n)/(sigma_z+omega) - log(sqrt(omega/(sigma_z+omega)))*n -r*n
Vp2<-(r/k)*n^2/2 + (1*(1/R0)*del[2]^2*r*n)/(sigma_z+omega) - log(sqrt(omega/(sigma_z+omega)))*n -r*n
Vp3<-(r/k)*n^2/2 + (1*(1/R0)*del[3]^2*r*n)/(sigma_z+omega) - log(sqrt(omega/(sigma_z+omega)))*n -r*n



P.potential<-data.frame( Abundance=log(N),values = 
                           c(Vp1,Vp2,Vp3),
                         plasticity = factor(c(rep("1",each=length(Vp1)),
                                       rep("0.5", each= length(Vp2)),
                                       rep("0", each= length(Vp3)))))

fp.P<-ggplot(data= P.potential, aes(y = values,x =Abundance , color = plasticity )) +geom_line()+
  ylab("Effective potential V(N)")+xlab("Population size (log-transformed)")+ggtitle("C")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Plasticity strength")


gP = list()
KKP<-c(n[which(Vp1==min(Vp1))],n[which(Vp2==min(Vp2))],
       n[which(Vp3==min(Vp3))])
VVP<-c(min(Vp1),min(Vp2),min(Vp3))
for(i in 1:(nrow(dd)-2)){
  img = readPNG(dd$images[i])
  gP[[i]] =  rasterGrob(img, interpolate=TRUE)
  
  fp.P = fp.P +
    annotation_custom(grob=gP[[i]], xmin=KKP[i], xmax=KKP[i]+.45, ymin=VVP[i], ymax=VVP[i]+0.03)
}

fp.P

###### Plotting all

multiplot(fp.gc,fp.P,fp.R, cols = 2)


#### this above figure will generate figure 2 of the main-text.



# CHARACTERISTIC RETURN TIME TO EQUILIBRIUM : equation from appendix 2


R0=1.2; K=73; k=log(K);omega=20; del=0 ; # del here signifies no evolutionary lag
r =log(R0);
sigma<-c(0,0.001,0.02,0.08,0.2,0.4,0.6, 1)
beta  = log(sqrt(omega)/(sqrt(omega+sigma)))  
gamma = del/(R0*(omega+sigma))

N <-seq(8900,0,-2); n<-log(N);

# Genetic variation 
del=0; omega<-25
R0=1.2
sigma<-c(0,0.001,0.02,0.08,0.2,0.4,0.6, 1)
gt1 <- - 1/ (1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma[1]))) - del^2/(R0*(omega+sigma[1])) )*log(R0)  )
gt2 <-  -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma[2]))) - del^2/(R0*(omega+sigma[2])) )*log(R0) )
gt3 <-  -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma[3]))) - del^2/(R0*(omega+sigma[3])) )*log(R0) )
gt4 <-  -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma[4]))) - del^2/(R0*(omega+sigma[4])) )*log(R0))
gt5 <-  -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma[5]))) - del^2/(R0*(omega+sigma[5])) )*log(R0))
gt6 <-  -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma[6]))) - del^2/(R0*(omega+sigma[6])) )*log(R0))
gt7 <-  -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma[7]))) - del^2/(R0*(omega+sigma[7])) )*log(R0))
gt8 <-  -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma[8]))) - del^2/(R0*(omega+sigma[8])) )*log(R0))

sigmaz<-sigma
Gt<-c(gt1,gt2,gt3,gt4,gt5,gt6,gt7,gt8)
plot(sigmaz,Gt, col="cornflowerblue", xlab="Genetic variation",ylab="Return Time",lwd=2)

# Net reproductive rate,

del=0; omega<-20; sigma<-0.25
R0=c(1.0,1.15,1.2,1.25,1.3,1.35,1.4)
Rt1 <-  -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma))) - del^2/(R0[1]*(omega+sigma)) )*log(R0[1])  )
Rt2 <-  -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma))) - del^2/(R0[2]*(omega+sigma)) )*log(R0[2]) )
Rt3 <-  -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma))) - del^2/(R0[3]*(omega+sigma)) )*log(R0[3]))
Rt4 <- -1/  (1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma))) - del^2/(R0[4]*(omega+sigma)) )*log(R0[4]))
Rt5 <- -1/  (1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma))) - del^2/(R0[5]*(omega+sigma)) )*log(R0[5]))
Rt6 <-  -1/ ( 1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma))) - del^2/(R0[6]*(omega+sigma)) )*log(R0[6]))
Rt7 <- -1/  (1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma))) - del^2/(R0[7]*(omega+sigma)) )*log(R0[7]))

Rt<-c(Rt1,Rt2,Rt3,Rt4,Rt5,Rt6,Rt7)
R01<-R0
plot(R01,Rt, col="cornflowerblue", xlab="Net Reproductive rate",ylab="Return Time",lwd=2)


# adaptive plasticity
b<-c(0,0.05,0.1,0.2,0.3,0.5,0.6,0.8,1)
del=c( (0+2*b[1]-3), (0+2*b[2]-3),(0+2*b[3]-3),(0+2*b[4]-3),(0+2*b[5]-3),(0+2*b[6]-3),(0+2*b[7]-3)
       ,(0+2*b[8]-3),(0+2*b[9]-3))

omega<-20; sigma<-0.25; R0<-1.2

bt1 <-  -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma))) - del[1]^2/(R0*(omega+sigma)) )*log(R0)  )
bt2 <-  -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma))) - del[2]^2/(R0*(omega+sigma)) )*log(R0) )
bt3 <-  -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma))) - del[3]^2/(R0*(omega+sigma)) )*log(R0))
bt4 <-  -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma))) - del[4]^2/(R0*(omega+sigma)) )*log(R0))
bt5 <-  -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma))) - del[5]^2/(R0*(omega+sigma)) )*log(R0))
bt6 <-  -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma))) - del[6]^2/(R0*(omega+sigma)) )*log(R0))
bt7 <-   -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma))) - del[7]^2/(R0*(omega+sigma)) )*log(R0))
bt8 <-   -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma))) - del[8]^2/(R0*(omega+sigma)) )*log(R0))
bt9 <-   -1/(1 - (1+  log(sqrt(omega)/(sqrt(omega+sigma))) - del[9]^2/(R0*(omega+sigma)) )*log(R0))


Bt<-c(bt1,bt2,bt3,bt4,bt5,bt6,bt7,bt8,bt9)
b1<-b

#pdf("Return_time_Vs_factors.pdf")
par(mfrow=c(2,2))
plot(b1,Bt, col="cornflowerblue", xlab="Strength in plasticity",ylab="Return Time",lwd=2)
plot(R01,Rt, col="gray", xlab="Net Reproductive rate",ylab="Return Time",lwd=2)
plot(sigmaz,Gt, col="darkgoldenrod2", xlab="Genetic variation",ylab="Return Time",lwd=2)

#dev.off()

