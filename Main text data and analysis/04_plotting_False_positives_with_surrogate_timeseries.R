 rm(list=ls())
library(grid)
library(gridExtra)
 source('~/Dropbox/Zurich PhD Research/2_Chapter_2/J Animal Ecology codes/Functions_J_A_Ecology.R')
load("False_positives_constant_timeseries_genvar.RData")
load("False_positives_constant_timeseries_plasticity.RData")
load("False_positives_constant_timeseries_R0.RData")





fp.Pc<-ggplot(data= data.p.plasticity_fp, aes(y = Kendall.tau,x =Plasticity , color = variation )) +geom_boxplot(alpha=0)+
  geom_point(alpha=0.1, position = position_jitterdodge())+ggtitle("A")+ylim(c(0,1))+
  ylab("Rate of false positives")+xlab("Plasticity strength")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="EWS metric")
fp.Pc

fp.rc<-ggplot(data= data.R.falsepositives, aes(y = Kendall.tau,x =R0 , color = variation )) +geom_boxplot(alpha=0)+
  geom_point(alpha=0.1, position = position_jitterdodge())+ggtitle("B")+ylim(c(0,1))+
  ylab("Rate of false positives")+xlab("Net reproductive rate")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="EWS metric")
fp.rc

fp.gc<-ggplot(data= data.g.FP, aes(y = Kendall.tau,x =Genetic_variation , color = variation )) +geom_boxplot(alpha=0)+
  geom_point(alpha=0.1, position = position_jitterdodge())+ggtitle("C")+ylim(c(0,1))+
  ylab("Rate of false positives")+xlab("Genetic variation")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="EWS metric")
fp.gc

grid_arrange_shared_legend(fp.Pc,fp.rc,fp.gc, ncol = 2, nrow = 2)





#




best_color_paletter<-c("#A42820", "#5F5647",  "#E58601",  "#00A08A" ,
                       "#4E2A1E","#000066", "#0C1707", "#000066")

plot_gen.R0<-ggplot(data= R0_fp, aes(y = Kendall.tau,x =R0 , color = timeseries_length )) +geom_boxplot(alpha=0)+
  geom_point(alpha=0.2, position = position_jitterdodge())+ggtitle("EWS metrics")+ylim(c(0,1))+
  ylab("Rate of false positives")+xlab("Net reproductive rate")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Timeseries length?")+
  facet_wrap(.~variation)
plot_gen.R0





plot_plastic.fp<-ggplot(data= plasticity_dat_fp, aes(y = Kendall.tau,x =Plasticity , color = timeseries_length )) +geom_boxplot(alpha=0)+
  geom_point(alpha=0.2, position = position_jitterdodge())+ggtitle("EWS metrics")+ylim(c(0,1))+
  ylab("Rate of false positives")+xlab("Plasticity Strength")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Timeseries length?")+
  facet_wrap(.~variation)
plot_plastic.fp






# variable timeseries

load("False.positives_variable_timeseries_plasticity.RData")
load("False.positives.variable.timeseries.R0.RData")
load("FP_variable_timeseries_genvar.RData")


#genetic variation plot

gen.var.fp.data<-data.frame(rbind(data.g.FP,data.g.variable.timeseries.FP), 
           timeseries_length=factor(c(rep("Constant", each=nrow(data.g.FP)), rep("Variable", each=
                                                                                   nrow(data.g.variable.timeseries.FP)))))



best_color_paletter<-c("#A42820", "#5F5647",  "#E58601",  "#00A08A" ,
                       "#4E2A1E","#000066", "#0C1707", "#000066")

plot_gen.fp<-ggplot(data= gen.var.fp.data, aes(y = Kendall.tau,x =Genetic_variation , color = timeseries_length )) +geom_boxplot(alpha=0)+
  geom_point(alpha=0.2, position = position_jitterdodge())+ggtitle("EWS metrics")+ylim(c(0,1))+
  ylab("Rate of false positives")+xlab("Genetic variation")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Timeseries length?")+
  facet_wrap(.~variation)
plot_gen.fp


# R0 plots
R0_fp<-data.frame(rbind(data.R.falsepositives,data.R.falsepositives_variabletime), 
                            timeseries_length=factor(c(rep("Constant", each=nrow(data.R.falsepositives)),
                                                       rep("Variable", each= nrow(data.R.falsepositives_variabletime)))))



best_color_paletter<-c("#A42820", "#5F5647",  "#E58601",  "#00A08A" ,
                       "#4E2A1E","#000066", "#0C1707", "#000066")

plot_gen.R0<-ggplot(data= R0_fp, aes(y = Kendall.tau,x =R0 , color = timeseries_length )) +geom_boxplot(alpha=0)+
  geom_point(alpha=0.2, position = position_jitterdodge())+ggtitle("EWS metrics")+ylim(c(0,1))+
  ylab("Rate of false positives")+xlab("Net reproductive rate")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Timeseries length?")+
  facet_wrap(.~variation)
plot_gen.R0


#

plasticity_dat_fp<-data.frame(rbind(data.p.plasticity_fp,data.p.plasticity_fp_variable), 
                  timeseries_length=factor(c(rep("Constant", each=nrow(data.p.plasticity_fp)),
                                             rep("Variable", each= nrow(data.p.plasticity_fp_variable)))))



best_color_paletter<-c("#A42820", "#5F5647",  "#E58601",  "#00A08A" ,
                       "#4E2A1E","#000066", "#0C1707", "#000066")

plot_plastic.fp<-ggplot(data= plasticity_dat_fp, aes(y = Kendall.tau,x =Plasticity , color = timeseries_length )) +geom_boxplot(alpha=0)+
  geom_point(alpha=0.2, position = position_jitterdodge())+ggtitle("EWS metrics")+ylim(c(0,1))+
  ylab("Rate of false positives")+xlab("Plasticity Strength")+
  theme_classic()+theme(plot.title = element_text(size = 10,   face = "bold"),text = element_text(size = 10 ),
                        axis.title = element_text(face="bold"),legend.position = "right") +
  scale_color_manual(values=best_color_paletter )+ labs(color="Timeseries length?")+
  facet_wrap(.~variation)
plot_plastic.fp
