#(lmtest) 
require(ggpubr)
require(ggplot2)
#library(plyr)
#library(qpcR)
#library(scales)
library(car)
library(here)

#select data by macrogroup
#modified

FQA_EIA_1_2018<-read.csv(here("data", "FQA_EIA_1_2018.csv"))
FQA.data <- FQA_EIA_1_2018



### Box PLOTS all types cwmean c


#clean data, get rid of blanks,ensure EIA_RANK is coded as factor 
FQA.data_rank<-subset(FQA.data,EIA_RANK_NOSIZE!="")
FQA.data_rank$EIA_RANK_NOSIZE<-as.factor(as.character(FQA.data_rank$EIA_RANK_NOSIZE))

#get sample sizes using table function for cross-tab
my.sizes<-table(FQA.data_rank$EIA_RANK_NOSIZE)

#new metric cwCOC, ANOVA and posthoc groupings
AOV_avCOC <- aov(FQA.data_rank$cwCOC ~ FQA.data_rank$EIA_RANK_NOSIZE)
my.anova<-(Anova(AOV_avCOC, type="III"))
summary(AOV_avCOC )
TukeyHSD(AOV_avCOC )

#table of means, in case needed 
model.tables(AOV_avCOC , type="means")

#ANOVA and post-hoc test to get groupings for old metric 
AOV_OLDavCOC <- aov(FQA.data_rank$OLDcwCOC ~ FQA.data_rank$EIA_RANK_NOSIZE)
my.anova<-(Anova(AOV_OLDavCOC, type="III"))
summary(AOV_OLDavCOC )
TukeyHSD(AOV_OLDavCOC )
model.tables(AOV_OLDavCOC, type="means")



##code below saves boxplots as png file 
## manualy codes in labels for significant groupings (a, b, c, etc)- better if grabbed from file
png(here("figures/MEboxplot_all_CWmeanC_pub2_2021.png"), width=1300, height=1000, res=216)
newMeanC.plot<-ggboxplot(FQA.data_rank, x = "EIA_RANK_NOSIZE", y = "cwCOC", size=1, color="black" )+ 
  ylim(1,8)+
  ggtitle("New")+
  labs(x = "EIA Rank", y="cw-Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=7, label="a", size=7)+
  geom_text(x=2, y=7, label="a", size=7)+
  geom_text(x=3, y=7, label="b", size=7)+
  geom_text(x=1, y=1, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=1, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=1, label=paste("N=", my.sizes[3], sep=""),size=5)+
  theme(plot.title = element_text(hjust= 0.9, margin = margin(t = 10, b = -20), size=16, face = "bold"),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

oldMeanC.plot<-ggboxplot(FQA.data_rank, x = "EIA_RANK_NOSIZE", y = "OLDcwCOC", size=1, color="darkgray" )+ 
  ylim(1,8)+
  ggtitle("Old")+
  labs(x = "EIA Rank", y="cw-Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=7, label="a", size=7)+
  geom_text(x=2, y=7, label="a", size=7)+
  geom_text(x=3, y=7, label="b", size=7)+
  geom_text(x=1, y=1, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=1, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=1, label=paste("N=", my.sizes[3], sep=""),size=5)+
  theme(plot.title =element_text(hjust= 0.9, margin = margin(t = 10, b = -20), size=16, face = "bold"),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

ggarrange(oldMeanC.plot,newMeanC.plot, ncol=2)
dev.off()





























