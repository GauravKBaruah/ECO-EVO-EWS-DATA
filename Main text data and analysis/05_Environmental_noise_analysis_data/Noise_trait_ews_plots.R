rm(list=ls())

# this scripts plots the kendalls tau value for three different noise levels. Sorry that its all .csv files. I made sure for the next environmental sstochasticity plots, those are not .csv files/


Plasticity.data <- read.csv(file = 'ALL_NOISE_LEVELS_PLASTICITY_Var.csv')
Plasticity.data<-Plasticity.data[,-1]
Plasticity.data$variation<-factor(Plasticity.data$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )
Plasticity.data$Noise_levels<-factor(Plasticity.data$Noise_levels,levels =c("High_noise",  "Medium_noise" ,"Low_noise" ), ordered=T )

Plasticity.data$Plasticity<-factor(Plasticity.data$Plasticity,levels =c("0.05",  "0.1" ,"0.2","0.3","0.4","0.8" ), ordered=T )


genvar.data<-read.csv(file='ALL_NOISE_LEVELS_GENETIC_Var.csv')
genvar.data<-genvar.data[,-1]
genvar.data$variation<-factor(genvar.data$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )
genvar.data$Noise_levels<-factor(genvar.data$Noise_levels,levels =c("High_noise",  "Medium_noise" ,"Low_noise" ), ordered=T )


reproduction.data<-read.csv(file='ALL_NOISE_LEVELS_R0_Var.csv')
reproduction.data<-reproduction.data[,-1]
reproduction.data$variation<-factor(reproduction.data$variation,levels =c("AR1","SD","AR1+SD","AR1+Tr","SD+Tr","AR1+SD+Tr"), ordered=T )
reproduction.data$Noise_levels<-factor(reproduction.data$Noise_levels,levels =c("High_noise",  "Medium_noise" ,"Low_noise" ), ordered=T )
reproduction.data$R0 <-factor(reproduction.data$R0 ,levels =c("1.1",  "1.2" ,"1.3","1.4","1.5"), ordered=T )


#plots of plasicity data all trait EWS +EWS for all noise levels
library(wesanderson)


best_color_paletter<-c("#9986A5", "#79402E", "#CCBA72", "#0F0D0E", "#D9D0D3" ,"#8D8680","#FF0000" ,"#DC863B","#35274A","#5BBCD6",
                       "#E1BD6D", "#C93312", "#EABE94", "#0B775E", 
                       "#899DA4", "#F2300F", "#FF0000",  "#00A08A" ,"#F2AD00" ,"#F98400"  )#c(wes_palettes$Darjeeling1,wes_palettes$Rushmore1 )

p1<-ggplot(data= Plasticity.data, aes(y = Kendall.tau,x = Plasticity, color= Noise_levels )) +geom_boxplot()+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ggtitle("Strength of Plasticity")+ylim(c(-1,1))+ylab("Kendall's Tau")+xlab("Metrics")+
 theme_classic()+theme(plot.title = element_text(size = 14,   face = "bold"),text = element_text(size = 12 ),
                      axis.title = element_text(face="bold"),axis.text.x=element_text(size = 8),legend.position = "right") +
  scale_color_manual(values=wes_palettes$Darjeeling1)+  
  facet_wrap(.~variation)



g1<-ggplot(data= genvar.data, aes(y = Kendall.tau,x = variation, color= Noise_levels )) +geom_boxplot(alpha=0)+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ggtitle("Genetic variation")+ylim(c(-1,1))+ylab("Kendall's Tau")+xlab("Metrics")+
  theme_classic()+theme(plot.title = element_text(size = 14,   face = "bold"),text = element_text(size = 12 ),
                        axis.title = element_text(face="bold"),axis.text.x=element_text(size = 8),legend.position = "right") +
  scale_color_manual(values=wes_palettes$Darjeeling1)+  geom_hline(yintercept=0, linetype="dashed")+
  facet_wrap(.~Genetic_variation)


r1<-ggplot(data= reproduction.data, aes(y = Kendall.tau,x = R0, color= Noise_levels )) +geom_boxplot(alpha=0)+
  geom_point(pch = 21,alpha=0.2, position = position_jitterdodge())+ggtitle("Net Reproductive rate")+ylim(c(-1,1))+ylab("Kendall's Tau")+xlab("Metrics")+
  theme_classic()+theme(plot.title = element_text(size = 14,   face = "bold"),text = element_text(size = 12 ),
                        axis.title = element_text(face="bold"),axis.text.x=element_text(size = 8),legend.position = "right") +
  scale_color_manual(values=wes_palettes$Darjeeling1)+  geom_hline(yintercept=0, linetype="dashed")+
  facet_wrap(.~variation)


