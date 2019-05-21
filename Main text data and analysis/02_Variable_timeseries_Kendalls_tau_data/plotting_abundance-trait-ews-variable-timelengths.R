rm(list=ls())
#loading the data
library(wesanderson)
load("reproductive_rate_variable_timeseries.RData")
load("plasticity_data_variable_timeseries.RData")
load("genvar_variable_timeseries.RData")


load("plasticity.RData")
load("reproductive_rate.RData")
load("genvar.RData")

best_color_paletter<- c(wes_palettes$Darjeeling1, wes_palettes$Rushmore1)
#R0 combined data

Net.repro.dat<-data.frame(rbind(data.R, data.R.timeseries), Variable_timeseries =factor(c(rep("No", each=3000), rep("Yes",each=3000)) ))

#plasticity combined data

Net.p.dat<-data.frame(rbind(data.p, data.p.timesires), Variable_timeseries =factor(c(rep("No", each=3600), rep("Yes",each=3600)) ))

#genvariation combined data
Net.g.dat<-data.frame(rbind(data.g, data.g.timeseries), Variable_timeseries =factor(c(rep("No", each=3600), rep("Yes",each=3600)) ))


# plotting R0



R01<-ggplot(data= Net.repro.dat, aes(y = Kendall.tau,x = R0, color= Variable_timeseries )) +geom_boxplot(alpha=0)+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ylim(c(-1,1))+
  ylab("Kendall's Tau")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Variable time series length?")+
  facet_wrap(.~variation)+xlab("")+ggtitle("Net Reproductive rate")
#pdf(file = "constant_timeseries_vs_variable_timeseris__R0.pdf", width = 8, height = 8)
R01 
#dev.off()
## plotting plasticity



pl1<-ggplot(data= Net.p.dat, aes(y = Kendall.tau,x = Plasticity, color= Variable_timeseries )) +geom_boxplot(alpha=0)+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ylim(c(-1,1))+
  ylab("Kendall's Tau")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Variable time series length?")+
  facet_wrap(.~variation)+xlab("")+ggtitle("Strength of plasticity")


#pdf(file = "constant_timeseries_vs_variable_timeseris_plasticity.pdf", width = 8, height = 8)
pl1
#dev.off()



## plotting genetic variation


gg1<-ggplot(data= Net.g.dat, aes(y = Kendall.tau,x = Genetic_variation, color= Variable_timeseries )) +geom_boxplot(alpha=0)+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ylim(c(-1,1))+
  ylab("Kendall's Tau")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Variable time series length?")+
  facet_wrap(.~variation)+xlab("")+ggtitle("Genetic variation")

#pdf(file = "constant_timeseries_vs_variable_timeseris_genetic_variation.pdf", width = 8, height = 8)
gg1
#dev.off()

