rm(list=ls())
#loading the data
library(wesanderson)
load("genvar_residuals.RData")
load("reproductive_rate_residuals.RData")
load("plasticity_residuals.RData")


load("~/Dropbox/Zurich PhD Research/2_Chapter_2/J Animal Ecology codes/plasticity.RData")
load("~/Dropbox/Zurich PhD Research/2_Chapter_2/J Animal Ecology codes/reproductive_rate.RData")
load("~/Dropbox/Zurich PhD Research/2_Chapter_2/J Animal Ecology codes/genvar.RData")

best_color_paletter<- c(wes_palettes$Darjeeling1, wes_palettes$Rushmore1)
#R0 combined data

Net.repro.dat<-data.frame(rbind(data.R, data.R.resids), Phenotypic_residual_noise =factor(c(rep("0", each=3000), rep("0.25",each=3000)) ))

#plasticity combined data

Net.p.dat<-data.frame(rbind(data.p, data.p.plasticity_resids), Phenotypic_residual_noise =factor(c(rep("0", each=3600), rep("0.25",each=3600)) ))

#genvariation combined data
Net.g.dat<-data.frame(rbind(data.g, data.g.resids), Phenotypic_residual_noise =factor(c(rep("0", each=3600), rep("0.25",each=3600)) ))


# plotting R0



R01<-ggplot(data= Net.repro.dat, aes(y = Kendall.tau,x = variation, color= Phenotypic_residual_noise )) +geom_boxplot(alpha=0)+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ylim(c(-1,1))+
  ylab("Kendall's Tau")+
  theme_classic()+theme(plot.title = element_text(size = 8.5,   face = "bold"),text = element_text(size = 8.5 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Residual noise")+
  facet_wrap(.~R0)+xlab("")+ggtitle("Net Reproductive rate")
#pdf(file = "Residuals_noise_net_R0.pdf", width = 8, height = 8)
R01
#dev.off()
## plotting plasticity



pl1<-ggplot(data= Net.p.dat, aes(y = Kendall.tau,x = variation, color= Phenotypic_residual_noise )) +geom_boxplot(alpha=0)+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ylim(c(-1,1))+
  ylab("Kendall's Tau")+
  theme_classic()+theme(plot.title = element_text(size = 9,   face = "bold"),text = element_text(size = 9 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Residual noise")+
  facet_wrap(.~Plasticity)+xlab("")+ggtitle("Strength of plasticity")


#pdf(file = "Residuals_noise_plasticity.pdf", width = 8, height = 8)
pl1
#dev.off()



## plotting genetic variation


gg1<-ggplot(data= Net.g.dat, aes(y = Kendall.tau,x = variation, color= Phenotypic_residual_noise )) +geom_boxplot(alpha=0)+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ylim(c(-1,1))+
  ylab("Kendall's Tau")+
  theme_classic()+theme(plot.title = element_text(size = 8.5,   face = "bold"),text = element_text(size = 8.5 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Residual noise")+
  facet_wrap(.~Genetic_variation)+xlab("")+ggtitle("Genetic variation")

#pdf(file = "Residuals_noise_genvar.pdf", width = 8, height = 8)
gg1
#dev.off()

