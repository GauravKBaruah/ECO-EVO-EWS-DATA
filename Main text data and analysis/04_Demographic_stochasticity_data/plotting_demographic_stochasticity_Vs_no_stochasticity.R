rm(list=ls())

load("~/Dropbox/Zurich PhD Research/2_Chapter_2/J Animal Ecology codes/reproductive_rate.RData")
load("~/Dropbox/Zurich PhD Research/2_Chapter_2/J Animal Ecology codes/genvar.RData")
load("~/Dropbox/Zurich PhD Research/2_Chapter_2/J Animal Ecology codes/plasticity.RData")

load("Demographic_stochasticity_plasticity.RData")
load("Demographic_stochasticity_genetic_variation.RData")
load("Demographic_stochasticity_R0.RData")

best_color_paletter<- c(wes_palettes$Darjeeling1, wes_palettes$Rushmore1)
#R0 combined data

Net.repro.dat<-data.frame(rbind(data.R, data.R.demog), Demographic_stochasticity =factor(c(rep("No", each=3000), rep("Yes",each=3000)) ))

#plasticity combined data

Net.p.dat<-data.frame(rbind(data.p, data.p.plasticity_demog), Demographic_stochasticity =factor(c(rep("No", each=3600), rep("Yes",each=3600)) ))

#genvariation combined data
Net.g.dat<-data.frame(rbind(data.g, data.g.demog), Demographic_stochasticity =factor(c(rep("No", each=3600), rep("Yes",each=3600)) ))


# plotting R0



R01<-ggplot(data= Net.repro.dat, aes(y = Kendall.tau,x = R0, color= Demographic_stochasticity )) +geom_boxplot(alpha=0)+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ylim(c(-1,1))+
  ylab("Kendall's Tau")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Demographic stochasticity?")+
  facet_wrap(.~variation)+xlab("Net reproductive rate")+ggtitle("EWS metrics")
#pdf(file = "constant_timeseries_vs_variable_timeseris__R0.pdf", width = 8, height = 8)
R01 
#dev.off()
## plotting plasticity



pl1<-ggplot(data= Net.p.dat, aes(y = Kendall.tau,x = Plasticity, color= Demographic_stochasticity )) +geom_boxplot(alpha=0)+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ylim(c(-1,1))+
  ylab("Kendall's Tau")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Demographic stochasticity?")+
  facet_wrap(.~variation)+xlab("Plasticity strength")+ggtitle("EWS metrics")


#pdf(file = "constant_timeseries_vs_variable_timeseris_plasticity.pdf", width = 8, height = 8)
pl1
#dev.off()



## plotting genetic variation


gg1<-ggplot(data= Net.g.dat, aes(y = Kendall.tau,x = Genetic_variation, color= Demographic_stochasticity )) +geom_boxplot(alpha=0)+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ylim(c(-1,1))+
  ylab("Kendall's Tau")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Demographic stochasticity?")+
  facet_wrap(.~variation)+xlab("Genetic variation")+ggtitle("EWS metrics")

#pdf(file = "constant_timeseries_vs_variable_timeseris_genetic_variation.pdf", width = 8, height = 8)
gg1
#dev.off()

