
require(lmtest) 
require(AICcmodavg)
require(ggpubr)
require(ggplot2)
library(plyr)
library(qpcR)
library(stringr)
library(scales)

#select data by macrogroup

FQA_EIA_1_2018<-read.csv("C:/data/Maine_NH/VT_plots_withFQA_EORank_hierarchy3_final with peatland.csv")

FQA.data <- FQA_EIA_1_2018

library(plyr)
FQA.data$CONDITION_RATING_CD[FQA.data$CONDITION_RATING_CD=="D"]<-"C"
FQA.data$CONDITION_RATING_CD <-as.factor(as.character(FQA.data$CONDITION_RATING_CD))
FQA.data$OVERALL_EO_RANK[FQA.data$OVERALL_EO_RANK=="D"]<-"C"
FQA.data$OVERALL_EO_RANK<-as.factor(as.character(FQA.data$OVERALL_EO_RANK))
FQA.data$OVERALL_EO_RANK<-FQA.data$CONDITION_RATING_CD

FQA.data$EO_Score <- revalue(FQA.data$OVERALL_EO_RANK,
                             c("A"="4", "AB"="3.5", "B"="3", "B?"="3", "BC"="2.5", "C"="2.0", "D"="1")
                             )
FQA.data$EO_Score<-as.numeric(as.character(FQA.data$EO_Score))

#Avereage COC
avCOC_regression_avCOC <- lm(FQA.data$MeanC ~ FQA.data$EO_Score)

summary(avCOC_regression_avCOC)


OLDavCOC_regression_avCOC <- lm(FQA.data$MeanC.OLD.CoC~ FQA.data$EO_Score)

summary(OLDavCOC_regression_avCOC)
new.summary<-summary(avCOC_regression_avCOC)
new.summary$r.squared
new.summary$coefficients[2,4]

old.summary<-summary(OLDavCOC_regression_avCOC)
old.summary$r.squared
old.summary$coefficients[2,4]


str(old.summary)
AIC(OLDavCOC_regression_avCOC, avCOC_regression_avCOC)


png('C:/data/VT_MeanC_regression.png', width=1200, height=1100, res=216)
par(mar=c(5,5,4,2))
plot(FQA.data$MeanC ~ jitter(FQA.data$EO_Score),  xlab = "EIA score", ylab = "Mean C", xlim=c(1, 4.1), ylim=c(1,11), pch=21, col="black", bg='red', cex=1.2, cex.lab=1.5, font.lab=2, font.axis=2)
abline(avCOC_regression_avCOC, lwd=2, col ='red')

points(FQA.data$MeanC.OLD.CoC ~ jitter(FQA.data$EO_Score), pch=21, cex=1.2, col="black", bg='blue')
abline(OLDavCOC_regression_avCOC, lty=5, lwd=2, col = 'blue')

legend("topleft", title=expression(underline("All Types")), legend=c(expression(paste("New: ", "R"^"2"," = 0.01, p < 0.01")), expression(paste("Old: ", "R"^"2"," = 0.03, p < 0.001"))), bty="n",
       col= c('black', 'black'), pt.bg=c('red','blue'), pch=c(21, 21), cex=1, text.font= c(2,1,1))
dev.off()

#community weighted COC
cwCOC_regression_cwCOC <- lm(FQA.data$CWMeanC ~ FQA.data$EO_Score)

summary(cwCOC_regression_cwCOC)
OLDcwCOC_regression_cwCOC <- lm(FQA.data$CWMeanC.OLD.CoC ~ FQA.data$EO_Score)

summary(OLDcwCOC_regression_cwCOC)
new.summary<-summary(cwCOC_regression_cwCOC)
new.summary$r.squared
new.summary$coefficients[2,4]

old.summary<-summary(OLDcwCOC_regression_cwCOC)
old.summary$r.squared
old.summary$coefficients[2,4]

png('C:/data/VT_CWmeanC_Regression.png', width=1200, height=1100, res=216)
par(mar=c(5,5,4,2))
plot(FQA.data$CWMeanC ~ jitter(FQA.data$EO_Score), 
     xlab = "EIA score", ylab = "CW-Mean C", xlim=c(1, 4.1), ylim=c(1,11), pch=21, col='black', bg='red', cex=1.2, cex.lab=1.5, font.lab=2, font.axis=2)
abline(cwCOC_regression_cwCOC, lwd=2, col='red')

points(FQA.data$CWMeanC.OLD.CoC ~ jitter(FQA.data$EO_Score), pch=21, cex=1.2, col="black", bg='blue')
abline(OLDcwCOC_regression_cwCOC, lty=5, lwd=2, col ='blue')

legend("topleft", title=expression(underline("All Types")), legend=c(expression(paste("New: ", "R"^"2"," = 0.003, NS")), expression(paste("Old: ", "R"^"2"," = 0.008, p = 0.035"))), bty="n",
       col= c('black', 'black'), pt.bg=c('red','blue'), pch=c(21, 21), cex=1, text.font= c(2,1,1))
dev.off()


###by floodplain Mean C

FQA.flood<-subset(FQA.data, Formation == "Floodplain Forests")


avCOC_regression_avCOC <- lm(FQA.flood$MeanC ~ FQA.flood$EO_Score)
summary(avCOC_regression_avCOC)
OLDavCOC_regression_avCOC <- lm(FQA.flood$MeanC.OLD.CoC ~ FQA.flood$EO_Score)

summary(OLDavCOC_regression_avCOC)
new.summary<-summary(avCOC_regression_avCOC)
new.summary$r.squared
new.summary$coefficients[2,4]

old.summary<-summary(OLDavCOC_regression_avCOC)
old.summary$r.squared
old.summary$coefficients[2,4]
old.r2<-signif(old.summary$r.squared,2)
old.p<-format(signif(old.summary$coefficients[2,4],2), scientific=F)
?round


signif(0.123450000, 2)

round_anystr(old.summary)
AIC(OLDavCOC_regression_avCOC, avCOC_regression_avCOC)
akaike.weights(c(236.2884, 264.4035))
evidence(236.2884, 264.4035)
.99/.00000078

png('C:/data/VT_MeanC_regression_Flood.png', width=1200, height=1100, res=216)
par(mar=c(5,5,4,2))
plot(FQA.flood$MeanC ~ jitter(FQA.flood$EO_Score),  xlab = "EIA score", ylab = "Mean C", xlim=c(1, 4.1), ylim=c(1,11), pch=21, col="black", bg='red', cex=1.2, cex.lab=1.5, font.lab=2, font.axis=2)
abline(avCOC_regression_avCOC, lwd=2, col ='red')

points(FQA.flood$MeanC.OLD.CoC ~ jitter(FQA.flood$EO_Score), pch=21, cex=1.2, col="black", bg='blue')
abline(OLDavCOC_regression_avCOC, lty=5, lwd=2, col = 'blue')

legend("topleft", title=expression(underline("Floodplain Forests")), legend=c(expression(paste("New: ", "R"^"2"," = 0.01, p = 0.831")), expression(paste("Old: ", "R"^"2"," = 0.02, p = 0.71"))), bty="n",
       col= c('black', 'black'), pt.bg=c('red','blue'), pch=c(21, 21), cex=1, text.font= c(2,1,1))
dev.off()


###by floodplain Mean CW

FQA.flood<-subset(FQA.data, Formation == "Floodplain Forests")

avCOC_regression_avCOC <- lm(FQA.flood$CWMeanC ~ FQA.flood$EO_Score)
summary(avCOC_regression_avCOC)
OLDavCOC_regression_avCOC <- lm(FQA.flood$CWMeanC.OLD.CoC ~ FQA.flood$EO_Score)

summary(OLDavCOC_regression_avCOC)
new.summary<-summary(avCOC_regression_avCOC)
new.summary$r.squared
new.summary$coefficients[2,4]

old.summary<-summary(OLDavCOC_regression_avCOC)
old.summary$r.squared
old.summary$coefficients[2,4]
old.r2<-signif(old.summary$r.squared,2)
old.p<-format(signif(old.summary$coefficients[2,4],2), scientific=F)

round_anystr(old.summary)
AIC(OLDavCOC_regression_avCOC, avCOC_regression_avCOC)

png('C:/data/VT_CWMeanC_regression_Flood.png', width=1200, height=1100, res=216)
par(mar=c(5,5,4,2))
plot(FQA.flood$CWMeanC ~ jitter(FQA.flood$EO_Score),  xlab = "EIA score", ylab = "CW-Mean C", xlim=c(1, 4.1), ylim=c(1,11), pch=21, col="black", bg='red', cex=1.2, cex.lab=1.5, font.lab=2, font.axis=2)
abline(avCOC_regression_avCOC, lwd=2, col ='red')

points(FQA.flood$CWMeanC.OLD.CoC ~ jitter(FQA.flood$EO_Score), pch=21, cex=1.2, col="black", bg='blue')
abline(OLDavCOC_regression_avCOC, lty=5, lwd=2, col = 'blue')

legend("topleft", title=expression(underline("Floodplain Forests")), legend=c(expression(paste("New: ", "R"^"2"," = 0.17, p = 0.26")), expression(paste("Old: ", "R"^"2"," = 0.04, p = 0.59"))), bty="n",
       col= c('black', 'black'), pt.bg=c('red','blue'), pch=c(21, 21), cex=1, text.font= c(2,1,1))
dev.off()


###by swamp Mean C

FQA.swamp<-subset(FQA.data, Formation == "Hardwood Swamps"| Formation == "Softwood Swamps")


avCOC_regression_avCOC <- lm(FQA.swamp$MeanC ~ FQA.swamp$EO_Score)
summary(avCOC_regression_avCOC)
OLDavCOC_regression_avCOC <- lm(FQA.swamp$MeanC.OLD.CoC ~ FQA.swamp$EO_Score)

summary(OLDavCOC_regression_avCOC)
new.summary<-summary(avCOC_regression_avCOC)
new.summary$r.squared
new.summary$coefficients[2,4]

old.summary<-summary(OLDavCOC_regression_avCOC)
old.summary$r.squared
old.summary$coefficients[2,4]

signif(0.123450000, 2)

round_anystr(old.summary)
AIC(OLDavCOC_regression_avCOC, avCOC_regression_avCOC)
akaike.weights(c(236.2884, 264.4035))
evidence(236.2884, 264.4035)
.99/.00000078

png('C:/data/VT_MeanC_regression_swamp.png', width=1200, height=1100, res=216)
par(mar=c(5,5,4,2))
plot(FQA.swamp$MeanC ~ jitter(FQA.swamp$EO_Score),  xlab = "EIA score", ylab = "Mean C", xlim=c(1, 4.1), ylim=c(1,11), pch=21, col="black", bg='red', cex=1.2, cex.lab=1.5, font.lab=2, font.axis=2)
abline(avCOC_regression_avCOC, lwd=2, col ='red')

points(FQA.swamp$MeanC.OLD.CoC ~ jitter(FQA.swamp$EO_Score), pch=21, cex=1.2, col="black", bg='blue')
abline(OLDavCOC_regression_avCOC, lty=5, lwd=2, col = 'blue')

legend("topleft", title=expression(underline("Wooded Swamps")), legend=c(expression(paste("New: ", "R"^"2"," = 0.008, p = 0.2")), expression(paste("Old: ", "R"^"2"," = 0.04, p < 0.01"))), bty="n",
       col= c('black', 'black'), pt.bg=c('red','blue'), pch=c(21, 21), cex=1, text.font= c(2,1,1))
dev.off()



###by swamp CW-Mean C

FQA.swamp<-subset(FQA.data, Formation == "Hardwood Swamps"| Formation == "Softwood Swamps")


avCOC_regression_avCOC <- lm(FQA.swamp$CWMeanC ~ FQA.swamp$EO_Score)
summary(avCOC_regression_avCOC)
OLDavCOC_regression_avCOC <- lm(FQA.swamp$CWMeanC.OLD.CoC ~ FQA.swamp$EO_Score)

summary(OLDavCOC_regression_avCOC)
new.summary<-summary(avCOC_regression_avCOC)
new.summary$r.squared
new.summary$coefficients[2,4]

old.summary<-summary(OLDavCOC_regression_avCOC)
old.summary$r.squared
old.summary$coefficients[2,4]


png('C:/data/VT_CW_MeanC_regression_swamp.png', width=1200, height=1100, res=216)
par(mar=c(5,5,4,2))
plot(FQA.swamp$CWMeanC ~ jitter(FQA.swamp$EO_Score),  xlab = "EIA score", ylab = "CW-Mean C", xlim=c(1, 4.1), ylim=c(1,11), pch=21, col="black", bg='red', cex=1.2, cex.lab=1.5, font.lab=2, font.axis=2)
abline(avCOC_regression_avCOC, lwd=2, col ='red')

points(FQA.swamp$CWMeanC.OLD.CoC ~ jitter(FQA.swamp$EO_Score), pch=21, cex=1.2, col="black", bg='blue')
abline(OLDavCOC_regression_avCOC, lty=5, lwd=2, col = 'blue')

legend("topleft", title=expression(underline("Wooded Swamps")), legend=c(expression(paste("New: ", "R"^"2"," < 0.001, p = 0.99")), expression(paste("Old: ", "R"^"2"," = 0.004, p = 0.37"))), bty="n",
       col= c('black', 'black'), pt.bg=c('red','blue'), pch=c(21, 21), cex=1, text.font= c(2,1,1))
dev.off()





### Box PLOTS Meanc  -condition Rank

FQA.data_rank<-subset(FQA.data,EO_Score!="")
FQA.data_rank<-subset(FQA.data_rank,OVERALL_EO_RANK!=0)
FQA.data_rank$OVERALL_EO_RANK<-as.factor(as.character(FQA.data_rank$OVERALL_EO_RANK))
FQA.data_rank2<-FQA.data_rank # placehold to subset from by vegtype

my.sizes<-table(FQA.data_rank$OVERALL_EO_RANK)
AOV_avCOC <- aov(FQA.data_rank$MeanC ~ FQA.data_rank$OVERALL_EO_RANK)
summary(AOV_avCOC )
TukeyHSD(AOV_avCOC )
AOV_OLDavCOC <- aov(FQA.data_rank$MeanC.OLD.CoC  ~ FQA.data_rank$OVERALL_EO_RANK)
summary(AOV_OLDavCOC )
TukeyHSD(AOV_OLDavCOC )


png('C:/data/VTboxplot_2panel_alltypesConrank.png', width=1300, height=1000, res=216)
newMeanC.plot<-ggboxplot(FQA.data_rank, x = "OVERALL_EO_RANK", y = "MeanC", size=1, color="Red" )+ 
  ylim(1,10)+
  ggtitle("All Types (new C)")+
  labs(x = "Condition Rank", y="Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=9, label="a", size=7)+
  geom_text(x=2, y=8.5, label="b", size=7)+
  geom_text(x=3, y=8, label="b", size=7)+
  geom_text(x=1, y=1, label=paste("N=", my.sizes[1], sep=""),size=4)+
  geom_text(x=2, y=1, label=paste("N=", my.sizes[2], sep=""),size=4)+
  geom_text(x=3, y=1, label=paste("N=", my.sizes[3], sep=""),size=4)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

oldMeanC.plot<-ggboxplot(FQA.data_rank, x = "OVERALL_EO_RANK", y = "MeanC.OLD.CoC", size=1, color="blue" )+ 
  ylim(1,10)+
  ggtitle("All Types (old C)")+
  labs(x = "Condition Rank", y="Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=9, label="a", size=7)+
  geom_text(x=2, y=8.5, label="b", size=7)+
  geom_text(x=3, y=8, label="b", size=7)+
  geom_text(x=1, y=1, label=paste("N=", my.sizes[1], sep=""),size=4)+
  geom_text(x=2, y=1, label=paste("N=", my.sizes[2], sep=""),size=4)+
  geom_text(x=3, y=1, label=paste("N=", my.sizes[3], sep=""),size=4)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")
ggarrange( oldMeanC.plot,newMeanC.plot, ncol=2)
dev.off()

### Box PLOTS CWMeanc  -condition Rank VERMONT REVISIONS
FQA.data_rank<-subset(FQA.data,EO_Score!="")
FQA.data_rank<-subset(FQA.data_rank,OVERALL_EO_RANK!=0)
FQA.data_rank$OVERALL_EO_RANK<-as.factor(as.character(FQA.data_rank$OVERALL_EO_RANK))
FQA.data_rank2<-FQA.data_rank # placehold to subset from by vegtype

my.sizes<-table(FQA.data_rank$OVERALL_EO_RANK)
AOV_avCOC <- aov(FQA.data_rank$CWMeanC ~ FQA.data_rank$OVERALL_EO_RANK)
model.tables(AOV_avCOC, type="means")

summary(AOV_avCOC )
TukeyHSD(AOV_avCOC )
AOV_OLDavCOC <- aov(FQA.data_rank$CWMeanC.OLD.CoC  ~ FQA.data_rank$OVERALL_EO_RANK)
model.tables(AOV_OLDavCOC, type="means")
summary(AOV_OLDavCOC )
TukeyHSD(AOV_OLDavCOC )

AIC(AOV_OLDavCOC, AOV_avCOC)

anova(AOV_OLDavCOC)

akaike.weights(c(1236.025, 1118.0485))
evidence(1236.025, 1118.0485)
.99/.00000078

sqrt(1.291) 
sqrt(0.98)
png('C:/data/VTboxplot_2panel_for_pub.png', width=1300, height=1000, res=216)
newVTCWMeanC.plot<-ggboxplot(FQA.data_rank, x = "OVERALL_EO_RANK", y = "CWMeanC", size=1, color="black" )+ 
  ylim(1,10)+
  ggtitle("New")+
  labs(x = "Condition Rank", y="CW-Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=9.5, label="a", size=7)+
  geom_text(x=2, y=9.3, label="b", size=7)+
  geom_text(x=3, y=8, label="b", size=7)+
  geom_text(x=1, y=1, label=paste("N=", my.sizes[1], sep=""),size=4)+
  geom_text(x=2, y=1, label=paste("N=", my.sizes[2], sep=""),size=4)+
  geom_text(x=3, y=1, label=paste("N=", my.sizes[3], sep=""),size=4)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust= 0.9, margin = margin(t = 10, b = -20), size=16, face = "bold"),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

oldVTCWMeanC.plot<-ggboxplot(FQA.data_rank, x = "OVERALL_EO_RANK", y = "CWMeanC.OLD.CoC", size=1, color="darkgray" )+ 
  ylim(1,10)+
  ggtitle("Old")+
  labs(x = "Condition Rank", y="CW-Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=9.5, label="a", size=7)+
  geom_text(x=2, y=9.3, label="b", size=7)+
  geom_text(x=3, y=8, label="b", size=7)+
  geom_text(x=1, y=1, label=paste("N=", my.sizes[1], sep=""),size=4)+
  geom_text(x=2, y=1, label=paste("N=", my.sizes[2], sep=""),size=4)+
  geom_text(x=3, y=1, label=paste("N=", my.sizes[3], sep=""),size=4)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust= 0.9, margin = margin(t = 10, b = -20), size=16, face = "bold"),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")
ggarrange(oldVTCWMeanC.plot,newVTCWMeanC.plot, ncol=2)
dev.off()

png('C:/data/combo_boxplots.png', width=1300, height=1000, res=216)
ggarrange(oldVTCWMeanC.plot,newVTCWMeanC.plot, oldVTCWMeanC.plot,newVTCWMeanC.plot,oldVTCWMeanC.plot,newVTCWMeanC.plot,nrow=2, ncol=4)
dev.off()





#Boxplots softwood swamp meanC
str(FQA.data_rank)
FQA.data_rank<-subset(FQA.data_rank2, Formation == "Softwood Swamps")

AOV_avCOC <- aov(FQA.data_rank$MeanC ~ FQA.data_rank$OVERALL_EO_RANK)
summary(AOV_avCOC )
TukeyHSD(AOV_avCOC )
AOV_OLDavCOC <- aov(FQA.data_rank$MeanC.OLD.CoC  ~ FQA.data_rank$OVERALL_EO_RANK)
summary(AOV_OLDavCOC )
TukeyHSD(AOV_OLDavCOC )
my.sizes<-table(FQA.data_rank$OVERALL_EO_RANK)

png('C:/data/VTboxplot_soft_swamp_MeanC.png', width=1300, height=1000, res=216)
newMeanC.plot<-ggboxplot(FQA.data_rank, x = "OVERALL_EO_RANK", y = "MeanC", size=1, color="Red" )+ 
  ylim(2,9)+
  ggtitle("Softwood Swamps (new C)")+
  labs(x = "EIA Rank", y="Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=7, label="a", size=7)+
  geom_text(x=2, y=6.5, label="b", size=7)+
  geom_text(x=3, y=6.5, label="b", size=7)+
  geom_text(x=1, y=2, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=2, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=2, label=paste("N=", my.sizes[3], sep=""),size=5)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

oldMeanC.plot<-ggboxplot(FQA.data_rank, x = "OVERALL_EO_RANK", y = "MeanC.OLD.CoC", size=1, color="blue" )+ 
  ylim(2,9)+
  ggtitle("Softwood Swamps (old C)")+
  labs(x = "EIA Rank", y="Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=7, label="a", size=7)+
  geom_text(x=2, y=6.5, label="b", size=7)+
  geom_text(x=3, y=6.5, label="b", size=7)+
  geom_text(x=1, y=2, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=2, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=2, label=paste("N=", my.sizes[3], sep=""),size=5)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")
ggarrange( oldMeanC.plot,newMeanC.plot, ncol=2)
dev.off()

#Boxplots softwood swamp CWmeanC
str(FQA.data_rank)
FQA.data_rank<-subset(FQA.data_rank2, Formation == "Softwood Swamps")

AOV_avCOC <- aov(FQA.data_rank$CWMeanC ~ FQA.data_rank$OVERALL_EO_RANK)
summary(AOV_avCOC )
TukeyHSD(AOV_avCOC )
AOV_OLDavCOC <- aov(FQA.data_rank$CWMeanC.OLD.CoC  ~ FQA.data_rank$OVERALL_EO_RANK)
summary(AOV_OLDavCOC )
TukeyHSD(AOV_OLDavCOC )
my.sizes<-table(FQA.data_rank$OVERALL_EO_RANK)



png('C:/data/VTboxplot_soft_swamp_CWMeanC.png', width=1300, height=1000, res=216)
newMeanC.plot<-ggboxplot(FQA.data_rank, x = "OVERALL_EO_RANK", y = "CWMeanC", size=1, color="Red" )+ 
  ylim(2,9)+
  ggtitle("Softwood Swamps (new C)")+
  labs(x = "EIA Rank", y="CW-Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=7.5, label="a", size=7)+
  geom_text(x=2, y=7.5, label="a", size=7)+
  geom_text(x=3, y=7.5, label="a", size=7)+
  geom_text(x=1, y=2, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=2, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=2, label=paste("N=", my.sizes[3], sep=""),size=5)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

oldMeanC.plot<-ggboxplot(FQA.data_rank, x = "OVERALL_EO_RANK", y = "CWMeanC.OLD.CoC", size=1, color="blue" )+ 
  ylim(2,9)+
  ggtitle("Softwood Swamps (old C)")+
  labs(x = "EIA Rank", y="CW-Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=7.5, label="a", size=7)+
  geom_text(x=2, y=7.5, label="a", size=7)+
  geom_text(x=3, y=7.5, label="a", size=7)+
  geom_text(x=1, y=2, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=2, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=2, label=paste("N=", my.sizes[3], sep=""),size=5)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")
ggarrange( oldMeanC.plot,newMeanC.plot, ncol=2)
dev.off()


#Boxplots hardwood swamp meanC
str(FQA.data_rank)
FQA.data_rank<-subset(FQA.data_rank2, Formation == "Hardwood Swamps")

AOV_avCOC <- aov(FQA.data_rank$MeanC ~ FQA.data_rank$OVERALL_EO_RANK)
summary(AOV_avCOC )
TukeyHSD(AOV_avCOC )
AOV_OLDavCOC <- aov(FQA.data_rank$MeanC.OLD.CoC  ~ FQA.data_rank$OVERALL_EO_RANK)
summary(AOV_OLDavCOC )
TukeyHSD(AOV_OLDavCOC )
my.sizes<-table(FQA.data_rank$OVERALL_EO_RANK)

png('C:/data/VTboxplot_hard_swamp_MeanC.png', width=1300, height=1000, res=216)
newMeanC.plot<-ggboxplot(FQA.data_rank, x = "OVERALL_EO_RANK", y = "MeanC", size=1, color="Red" )+ 
  ylim(2,9)+
  ggtitle("Hardwood Swamps (new C)")+
  labs(x = "EIA Rank", y="Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=7, label="a", size=7)+
  geom_text(x=2, y=7, label="a", size=7)+
  geom_text(x=3, y=7, label="b", size=7)+
  geom_text(x=1, y=2, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=2, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=2, label=paste("N=", my.sizes[3], sep=""),size=5)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

oldMeanC.plot<-ggboxplot(FQA.data_rank, x = "OVERALL_EO_RANK", y = "MeanC.OLD.CoC", size=1, color="blue" )+ 
  ylim(2,9)+
  ggtitle("Hardwood Swamps (old C)")+
  labs(x = "EIA Rank", y="Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=7, label="a", size=7)+
  geom_text(x=2, y=7, label="a", size=7)+
  geom_text(x=3, y=7, label="a", size=7)+
  geom_text(x=1, y=2, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=2, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=2, label=paste("N=", my.sizes[3], sep=""),size=5)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")
ggarrange( oldMeanC.plot,newMeanC.plot, ncol=2)
dev.off()


#Boxplots hardwood swamp CWmeanC
str(FQA.data_rank)
FQA.data_rank<-subset(FQA.data_rank2, Formation == "Hardwood Swamps")

AOV_avCOC <- aov(FQA.data_rank$CWMeanC ~ FQA.data_rank$OVERALL_EO_RANK)
summary(AOV_avCOC )
TukeyHSD(AOV_avCOC )
AOV_OLDavCOC <- aov(FQA.data_rank$CWMeanC.OLD.CoC  ~ FQA.data_rank$OVERALL_EO_RANK)
summary(AOV_OLDavCOC )
TukeyHSD(AOV_OLDavCOC )
my.sizes<-table(FQA.data_rank$OVERALL_EO_RANK)



png('C:/data/VTboxplot_hard_swamp_CWMeanC.png', width=1300, height=1000, res=216)
newMeanC.plot<-ggboxplot(FQA.data_rank, x = "OVERALL_EO_RANK", y = "CWMeanC", size=1, color="Red" )+ 
  ylim(2,9)+
  ggtitle("Hardwood Swamps (new C)")+
  labs(x = "EIA Rank", y="CW-Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=7.5, label="a", size=7)+
  geom_text(x=2, y=7.5, label="a", size=7)+
  geom_text(x=3, y=7.5, label="a", size=7)+
  geom_text(x=1, y=2, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=2, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=2, label=paste("N=", my.sizes[3], sep=""),size=5)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

oldMeanC.plot<-ggboxplot(FQA.data_rank, x = "OVERALL_EO_RANK", y = "CWMeanC.OLD.CoC", size=1, color="blue" )+ 
  ylim(2,9)+
  ggtitle("Hardwood Swamps (old C)")+
  labs(x = "EIA Rank", y="CW-Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=7.5, label="a", size=7)+
  geom_text(x=2, y=7.5, label="a", size=7)+
  geom_text(x=3, y=7.5, label="a", size=7)+
  geom_text(x=1, y=2, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=2, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=2, label=paste("N=", my.sizes[3], sep=""),size=5)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")
ggarrange( oldMeanC.plot,newMeanC.plot, ncol=2)
dev.off()





#Boxplots formation meanC


FQA.data_rank<-subset(FQA.data_rank2, Formation%in%c("Hardwood Swamps", "Softwood Swamps" , "Open Alkaline Peatlands",  "Open Acidic Peatlands" ,"Oak-Pine-Northern Hardwood Forest"))
FQA.data_rank$Formation<-as.factor(as.character(FQA.data_rank$Formation))
FQA.data_rank<-subset(FQA.data_rank,OVERALL_EO_RANK=="A")
FQA.data_rank$Formation2 <- factor(FQA.data_rank$Formation, c("Hardwood Swamps", "Softwood Swamps" , "Open Alkaline Peatlands",  "Open Acidic Peatlands" ,"Oak-Pine-Northern Hardwood Forest"))

AOV_avCOC <- aov(FQA.data_rank$MeanC ~ FQA.data_rank$Formation2)
summary(AOV_avCOC )
TukeyHSD(AOV_avCOC )
AOV_OLDavCOC <- aov(FQA.data_rank$MeanC.OLD.CoC  ~ FQA.data_rank$Formation2)
summary(AOV_OLDavCOC )
TukeyHSD(AOV_OLDavCOC )
my.sizes<-table(FQA.data_rank$Formation2)

png('C:/data/VTboxplot_Formation_MeanC.png', width=1300, height=1000, res=216)
newMeanC.plot<-ggboxplot(FQA.data_rank, x = "Formation2", y = "MeanC", size=.8, color="Red" )+ 
  ylim(2,9)+
  ggtitle("Formations (new C)")+
  labs(y="Mean C")+
  font("xylab", face="bold")+
  font("xy.text", size = 12, color = "black", face = "bold")+
  geom_text(x=1, y=7, label="a", size=5)+
  geom_text(x=2, y=7.5, label="b", size=5)+
  geom_text(x=3, y=8.5, label="c", size=5)+
  geom_text(x=4, y=8.5, label="d", size=5)+
  geom_text(x=5, y=7, label="a", size=5)+
  geom_text(x=1, y=2, label=paste("N=", my.sizes[1], sep=""),size=3)+
  geom_text(x=2, y=2, label=paste("N=", my.sizes[2], sep=""),size=3)+
  geom_text(x=3, y=2, label=paste("N=", my.sizes[3], sep=""),size=3)+
  geom_text(x=4, y=2, label=paste("N=", my.sizes[4], sep=""),size=3)+
  geom_text(x=5, y=2, label=paste("N=", my.sizes[5], sep=""),size=3)+
  scale_x_discrete(labels = wrap_format(10))+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), 
        legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1, size=6.5), axis.title.x=element_blank())

oldMeanC.plot<-ggboxplot(FQA.data_rank, x = "Formation2", y = "MeanC.OLD.CoC", size=.8, color="blue" )+ 
  ylim(2,9)+
  ggtitle("Formations (old C)")+
  labs(y="Mean C")+
  font("xylab", face="bold")+
  font("xy.text", size = 12, color = "black", face = "bold")+
  geom_text(x=1, y=7, label="a", size=5)+
  geom_text(x=2, y=7, label="a", size=5)+
  geom_text(x=3, y=8.5, label="b", size=5)+
  geom_text(x=4, y=8.5, label="c", size=5)+
  geom_text(x=5, y=7, label="a", size=5)+
  geom_text(x=1, y=2, label=paste("N=", my.sizes[1], sep=""),size=3)+
  geom_text(x=2, y=2, label=paste("N=", my.sizes[2], sep=""),size=3)+
  geom_text(x=3, y=2, label=paste("N=", my.sizes[3], sep=""),size=3)+
  geom_text(x=4, y=2, label=paste("N=", my.sizes[4], sep=""),size=3)+
  geom_text(x=5, y=2, label=paste("N=", my.sizes[5], sep=""),size=3)+
  scale_x_discrete(labels = wrap_format(10))+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), 
        legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1, size=6.5), axis.title.x=element_blank())
ggarrange(oldMeanC.plot, newMeanC.plot, ncol=2, nrow=1)
dev.off()


#Boxplots formation cwmeanC


FQA.data_rank<-subset(FQA.data_rank2, Formation%in%c("Hardwood Swamps", "Softwood Swamps" , "Open Alkaline Peatlands",  "Open Acidic Peatlands" ,"Oak-Pine-Northern Hardwood Forest"))
FQA.data_rank$Formation<-as.factor(as.character(FQA.data_rank$Formation))
FQA.data_rank<-subset(FQA.data_rank,OVERALL_EO_RANK=="A")
FQA.data_rank$Formation2 <- factor(FQA.data_rank$Formation, c("Hardwood Swamps", "Softwood Swamps" , "Open Alkaline Peatlands",  "Open Acidic Peatlands" ,"Oak-Pine-Northern Hardwood Forest"))

AOV_avCOC <- aov(FQA.data_rank$CWMeanC ~ FQA.data_rank$Formation2)
summary(AOV_avCOC )
TukeyHSD(AOV_avCOC )
AOV_OLDavCOC <- aov(FQA.data_rank$CWMeanC.OLD.CoC  ~ FQA.data_rank$Formation2)
summary(AOV_OLDavCOC )
TukeyHSD(AOV_OLDavCOC )
my.sizes<-table(FQA.data_rank$Formation2)

png('C:/data/VTboxplot_Formation_CWMeanC.png', width=1300, height=1000, res=216)
newMeanC.plot<-ggboxplot(FQA.data_rank, x = "Formation2", y = "CWMeanC", size=.8, color="Red" )+ 
  ylim(2,9)+
  ggtitle("Formations (new C)")+
  labs(y="CW-Mean C")+
  font("xylab", face="bold")+
  font("xy.text", size = 12, color = "black", face = "bold")+
  geom_text(x=1, y=7, label="a", size=5)+
  geom_text(x=2, y=7.5, label="b", size=5)+
  geom_text(x=3, y=8.7, label="c", size=5)+
  geom_text(x=4, y=8.7, label="c", size=5)+
  geom_text(x=5, y=7, label="a", size=5)+
  geom_text(x=1, y=2, label=paste("N=", my.sizes[1], sep=""),size=3)+
  geom_text(x=2, y=2, label=paste("N=", my.sizes[2], sep=""),size=3)+
  geom_text(x=3, y=2, label=paste("N=", my.sizes[3], sep=""),size=3)+
  geom_text(x=4, y=2, label=paste("N=", my.sizes[4], sep=""),size=3)+
  geom_text(x=5, y=2, label=paste("N=", my.sizes[5], sep=""),size=3)+
  scale_x_discrete(labels = wrap_format(10))+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), 
        legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1, size=6.5), axis.title.x=element_blank())

oldMeanC.plot<-ggboxplot(FQA.data_rank, x = "Formation2", y = "CWMeanC.OLD.CoC", size=.8, color="blue" )+ 
  ylim(2,9)+
  ggtitle("Formations (old C)")+
  labs(y="CW-Mean C")+
  font("xylab", face="bold")+
  font("xy.text", size = 12, color = "black", face = "bold")+
  geom_text(x=1, y=7, label="a", size=5)+
  geom_text(x=2, y=7.5, label="a", size=5)+
  geom_text(x=3, y=8.5, label="b", size=5)+
  geom_text(x=4, y=8.5, label="b", size=5)+
  geom_text(x=5, y=7, label="a", size=5)+
  geom_text(x=1, y=2, label=paste("N=", my.sizes[1], sep=""),size=3)+
  geom_text(x=2, y=2, label=paste("N=", my.sizes[2], sep=""),size=3)+
  geom_text(x=3, y=2, label=paste("N=", my.sizes[3], sep=""),size=3)+
  geom_text(x=4, y=2, label=paste("N=", my.sizes[4], sep=""),size=3)+
  geom_text(x=5, y=2, label=paste("N=", my.sizes[5], sep=""),size=3)+
  scale_x_discrete(labels = wrap_format(10))+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), 
        legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1, size=6.5), axis.title.x=element_blank())
ggarrange(oldMeanC.plot, newMeanC.plot, ncol=2, nrow=1)
dev.off()


scale_x_discrete(labels = wrap_format(10))




### Box PLOTS Floodplain Forests
FQA.data_rank2$Formation
FQA.data_rank<-subset(FQA.data_rank2, Formation == "Floodplain Forests")

AOV_avCOC <- aov(FQA.data_rank$MeanC ~ FQA.data_rank$OVERALL_EO_RANK)
summary(AOV_avCOC )
TukeyHSD(AOV_avCOC )
AOV_OLDavCOC <- aov(FQA.data_rank$MeanC.OLD.CoC  ~ FQA.data_rank$OVERALL_EO_RANK)
summary(AOV_OLDavCOC )
TukeyHSD(AOV_OLDavCOC )
my.sizes<-table(FQA.data_rank$OVERALL_EO_RANK)

png('C:/data/VTboxplot_2panel_floodplain.png', width=1300, height=1000, res=216)
newMeanC.plot<-ggboxplot(FQA.data_rank, x = "OVERALL_EO_RANK", y = "MeanC", size=1, color="Red" )+ 
  ylim(2,9)+
  ggtitle("Floodplain Forests (new C)")+
  labs(x = "EIA Rank", y="Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=7, label="a", size=7)+
  geom_text(x=2, y=6.5, label="b", size=7)+
  geom_text(x=3, y=6.5, label="b", size=7)+
  geom_text(x=1, y=2, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=2, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=2, label=paste("N=", my.sizes[3], sep=""),size=5)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

oldMeanC.plot<-ggboxplot(FQA.data_rank, x = "OVERALL_EO_RANK", y = "MeanC.OLD.CoC", size=1, color="blue" )+ 
  ylim(2,9)+
  ggtitle("Floodplain Forests (old C)")+
  labs(x = "EIA Rank", y="Mean-C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  #geom_text(x=1, y=7, label="a", size=7)+
  #geom_text(x=2, y=6, label="b", size=7)+
  #geom_text(x=3, y=5, label="c", size=7)+
  geom_text(x=1, y=2, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=2, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=2, label=paste("N=", my.sizes[3], sep=""),size=5)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")
ggarrange( oldMeanC.plot,newMeanC.plot, ncol=2)
dev.off()



png('C:/data/boxplot1.png', width=1200, height=1100, res=216)
ggboxplot(FQA.data_rank, x = "EIA_RANK_NOSIZE", y = "avCOC", size=1, color="colors")+ 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  stat_compare_means(comparisons = my_comparisons, label="p.signif", size=1)+ 
  #stat_compare_means(method = "anova", label.y=9, size = 5, color="white")+
  ggtitle("All Types")+
  labs(x = "EIA Rank", y="Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  #geom_text(x=1, y=9, label="Anova: p <0.001", size=5)+
  theme(plot.title = element_text(hjust=0.5),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")
dev.off()
?geom_text

  # Add global p-value
?stat_compare_means
  my_comparisons <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )
  ggboxplot(ToothGrowth, x = "dose", y = "len")+
  stat_compare_means(method = "anova", label.y=45)+
  stat_compare_means(comparisons = my_comparisons)

compare_means(avCOC~EIA_RANK_NOSIZE,  data = FQA.data_rank, method="anova")
my_comparisons <- list( c("A", "B"), c("B", "C"), c("A", "C") )
ggbarplot(FQA.data_rank, x = "EIA_RANK_NOSIZE", y = "avCOC",add="mean_se") +
  stat_compare_means(comparisons = my_comparisons, label="p.signif")+ # Add pairwise comparisons p-value
  stat_compare_means(method = "anova", label.y=9) +
  ggtitle("All Types")+
  labs(x = "EIA Rank", y="Mean C")+
  font("xlab", size = 12, color = "blue")
  theme(plot.title = element_text(hjust=0.5))



ggbarplot(FQA.data_rank, x = "EIA_RANK_NOSIZE", y = "avCOC")+
  stat_compare_means(comparisons = my_comparisons, label="p.signif")+
  stat_compare_means(label.y = 9)  # Add pairwise comparisons p-value
str(ToothGrowth)
str(FQA.data_rank)

boxplot(FQA.data_rank$cwCOC ~ FQA.data_rank$EIA_RANK_NOSIZE, main = "All Types",
        xlab = "EIA rank", ylab = "CW-C")

AOV_cwCOC <- aov(FQA.data_rank$cwCOC ~ FQA.data_rank$EIA_RANK_NOSIZE)
summary(AOV_cwCOC )
TukeyHSD(AOV_cwCOC )





large_river_floodplain <- subset(FQA.data, Macrogroup == "Large river floodplain")
northern_swamp <- subset(FQA.data, Macrogroup == "Northern swamp")


##FQA AOV, compared among ecoregions
boxplot(FQA.data$avCOC ~ FQA.data$Region, main = "All plots",
        xlab = "ecoregion", ylab = "avCOC")
EIA_floodplain_AOV_avCOC <- aov(FQA.data$avCOC ~ FQA.data$Region)
summary(EIA_floodplain_AOV_avCOC)
TukeyHSD(EIA_floodplain_AOV_avCOC)

#FQA AOV, floodplains compared among ecoregion
boxplot(large_river_floodplain$avCOC ~ large_river_floodplain$Region, main = "Large river floodplain",
        xlab = "ecoregion", ylab = "avCOC")
EIA_floodplain_AOV_avCOC <- aov(large_river_floodplain$avCOC ~ large_river_floodplain$Region)
summary(EIA_floodplain_AOV_avCOC)
TukeyHSD(EIA_floodplain_AOV_avCOC)

#FQA AOV, northern swamp compared among ecoregion
boxplot(northern_swamp$avCOC ~ northern_swamp$Region, main = "Northern swamp",
        xlab = "ecoregion", ylab = "avCOC")
EIA_northern_swamp_AOV_avCOC <- aov(northern_swamp$avCOC ~ northern_swamp$Region)
summary(EIA_northern_swamp_AOV_avCOC)
TukeyHSD(EIA_northern_swamp_AOV_avCOC)


#################
#EIA regressions, floodplains, avCOC, NEW COCs
plot(large_river_floodplain$avCOC ~ large_river_floodplain$EIA_score_NOSIZE, main = "Large river floodplain",
     xlab = "EIA score", ylab = "avCOC")
EIA_floodplain_regression_avCOC <- lm(large_river_floodplain$avCOC 
                                      ~ large_river_floodplain$EIA_score_NOSIZE)
abline(EIA_floodplain_regression_avCOC)
summary(EIA_floodplain_regression_avCOC)

#EIA regressions, floodplains, avCOC, OLD COCs
plot(large_river_floodplain$OLDavCOC ~ large_river_floodplain$EIA_score_NOSIZE, main = "Large river floodplain",
     xlab = "EIA score", ylab = "avCOC")
EIA_floodplain_regression_avCOC_OLD <- lm(large_river_floodplain$OLDavCOC 
                                      ~ large_river_floodplain$EIA_score_NOSIZE)
abline(EIA_floodplain_regression_avCOC_OLD)
summary(EIA_floodplain_regression_avCOC_OLD)

#################
#EIA AOV, floodplains, avCOC, NEW COCs
boxplot(large_river_floodplain$avCOC ~ large_river_floodplain$EIA_RANK_NOSIZE, main = "Large river floodplain",
        xlab = "EIA rank", ylab = "avCOC")
EIA_floodplain_AOV_avCOC <- aov(large_river_floodplain$avCOC ~ large_river_floodplain$EIA_RANK_NOSIZE)
summary(EIA_floodplain_AOV_avCOC)
TukeyHSD(EIA_floodplain_AOV_avCOC)

#EIA AOV, floodplains, avCOC, OLD COCs
boxplot(large_river_floodplain$OLDavCOC ~ large_river_floodplain$EIA_RANK_NOSIZE, main = "Large river floodplain",
        xlab = "EIA rank", ylab = "avCOC")
EIA_floodplain_AOV_avCOC_OLD <- aov(large_river_floodplain$OLDavCOC ~ large_river_floodplain$EIA_RANK_NOSIZE)
summary(EIA_floodplain_AOV_avCOC_OLD)
TukeyHSD(EIA_floodplain_AOV_avCOC_OLD)

#################
#EIA regressions, floodplains, cwCOC, NEW COCs
plot(large_river_floodplain$cwCOC ~ large_river_floodplain$EIA_score_NOSIZE, main = "Large river floodplain",
     xlab = "EIA score", ylab = "cwCOC")
EIA_floodplain_regression_cwCOC <- lm(large_river_floodplain$cwCOC 
                                      ~ large_river_floodplain$EIA_score_NOSIZE)
abline(EIA_floodplain_regression_cwCOC)
summary(EIA_floodplain_regression_cwCOC)

#EIA regressions, floodplains, cwCOC, OLD COCs
plot(large_river_floodplain$OLDcwCOC ~ large_river_floodplain$EIA_score_NOSIZE, main = "Large river floodplain",
     xlab = "EIA score", ylab = "cwCOC")
EIA_floodplain_regression_cwCOC_OLD <- lm(large_river_floodplain$OLDcwCOC 
                                      ~ large_river_floodplain$EIA_score_NOSIZE)
abline(EIA_floodplain_regression_cwCOC_OLD)
summary(EIA_floodplain_regression_cwCOC_OLD)


################
#EIA AOV, floodplains, cwCOC, NEW COCs
boxplot(large_river_floodplain$cwCOC ~ large_river_floodplain$EIA_RANK_NOSIZE, main = "Large river floodplain",
        xlab = "EIA rank", ylab = "cwCOC")
EIA_floodplain_AOV_cwCOC <- aov(large_river_floodplain$cwCOC ~ large_river_floodplain$EIA_RANK_NOSIZE)
summary(EIA_floodplain_AOV_cwCOC)
TukeyHSD(EIA_floodplain_AOV_cwCOC)

#EIA AOV, floodplains, cwCOC, OLD COCs
boxplot(large_river_floodplain$OLDcwCOC ~ large_river_floodplain$EIA_RANK_NOSIZE, main = "Large river floodplain",
        xlab = "EIA rank", ylab = "cwCOC")
EIA_floodplain_AOV_cwCOC_OLD <- aov(large_river_floodplain$OLDcwCOC ~ large_river_floodplain$EIA_RANK_NOSIZE)
summary(EIA_floodplain_AOV_cwCOC_OLD)
TukeyHSD(EIA_floodplain_AOV_cwCOC_OLD)





######################################################################################################

#################
#EIA regressions, northern_swamps, avCOC, NEW COCs
plot(northern_swamp$avCOC ~ northern_swamp$EIA_score_NOSIZE, main = "Northern swamp",
     xlab = "EIA score", ylab = "avCOC")
EIA_northern_swamp_regression_avCOC <- lm(northern_swamp$avCOC 
                                          ~ northern_swamp$EIA_score_NOSIZE)
abline(EIA_northern_swamp_regression_avCOC)
summary(EIA_northern_swamp_regression_avCOC)

#EIA regressions, northern_swamps, avCOC, OLD COCs
plot(northern_swamp$OLDavCOC ~ northern_swamp$EIA_score_NOSIZE, main = "Northern swamp",
     xlab = "EIA score", ylab = "avCOC")
EIA_northern_swamp_regression_avCOC_OLD <- lm(northern_swamp$OLDavCOC 
                                              ~ northern_swamp$EIA_score_NOSIZE)
abline(EIA_northern_swamp_regression_avCOC_OLD)
summary(EIA_northern_swamp_regression_avCOC_OLD)

#################
#EIA AOV, northern_swamps, avCOC, NEW COCs
boxplot(northern_swamp$avCOC ~ northern_swamp$EIA_RANK_NOSIZE, main = "Northern swamp",
        xlab = "EIA rank", ylab = "avCOC")
EIA_northern_swamp_AOV_avCOC <- aov(northern_swamp$avCOC ~ northern_swamp$EIA_RANK_NOSIZE)
summary(EIA_northern_swamp_AOV_avCOC)
TukeyHSD(EIA_northern_swamp_AOV_avCOC)

#EIA AOV, northern_swamps, avCOC, OLD COCs
boxplot(northern_swamp$OLDavCOC ~ northern_swamp$EIA_RANK_NOSIZE, main = "Northern swamp",
        xlab = "EIA rank", ylab = "avCOC")
EIA_northern_swamp_AOV_avCOC_OLD <- aov(northern_swamp$OLDavCOC ~ northern_swamp$EIA_RANK_NOSIZE)
summary(EIA_northern_swamp_AOV_avCOC_OLD)
TukeyHSD(EIA_northern_swamp_AOV_avCOC_OLD)

#################
#EIA regressions, northern_swamps, cwCOC, NEW COCs
plot(northern_swamp$cwCOC ~ northern_swamp$EIA_score_NOSIZE, main = "Northern swamp",
     xlab = "EIA score", ylab = "cwCOC")
EIA_northern_swamp_regression_cwCOC <- lm(northern_swamp$cwCOC 
                                          ~ northern_swamp$EIA_score_NOSIZE)
abline(EIA_northern_swamp_regression_cwCOC)
summary(EIA_northern_swamp_regression_cwCOC)

#EIA regressions, northern_swamps, cwCOC, OLD COCs
plot(northern_swamp$OLDcwCOC ~ northern_swamp$EIA_score_NOSIZE, main = "Northern swamp",
     xlab = "EIA score", ylab = "cwCOC")
EIA_northern_swamp_regression_cwCOC_OLD <- lm(northern_swamp$OLDcwCOC 
                                              ~ northern_swamp$EIA_score_NOSIZE)
abline(EIA_northern_swamp_regression_cwCOC_OLD)
summary(EIA_northern_swamp_regression_cwCOC_OLD)


################
#EIA AOV, northern_swamps, cwCOC, NEW COCs
boxplot(northern_swamp$cwCOC ~ northern_swamp$EIA_RANK_NOSIZE, main = "Northern swamp",
        xlab = "EIA rank", ylab = "cwCOC")
EIA_northern_swamp_AOV_cwCOC <- aov(northern_swamp$cwCOC ~ northern_swamp$EIA_RANK_NOSIZE)
summary(EIA_northern_swamp_AOV_cwCOC)
TukeyHSD(EIA_northern_swamp_AOV_cwCOC)

#EIA AOV, northern_swamps, cwCOC, OLD COCs
boxplot(northern_swamp$OLDcwCOC ~ northern_swamp$EIA_RANK_NOSIZE, main = "Northern swamp",
        xlab = "EIA rank", ylab = "cwCOC")
EIA_northern_swamp_AOV_cwCOC_OLD <- aov(northern_swamp$OLDcwCOC ~ northern_swamp$EIA_RANK_NOSIZE)
summary(EIA_northern_swamp_AOV_cwCOC_OLD)
TukeyHSD(EIA_northern_swamp_AOV_cwCOC_OLD)
