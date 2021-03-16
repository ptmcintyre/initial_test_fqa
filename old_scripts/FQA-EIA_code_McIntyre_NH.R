require(lmtest) 
require(AICcmodavg)
require(ggpubr)
require(ggplot2)
library(plyr)
library(qpcR)
library(scales)


#select data by macrogroup

FQA_EIA_1_2018<-read.csv("C:/data/Maine_NH/FQA_EIA_1_2018.csv")
FQA.data <- FQA_EIA_1_2018

#Avereage COC
avCOC_regression_avCOC <- lm(FQA.data$avCOC ~ FQA.data$EIA_score_NOSIZE)
summary(avCOC_regression_avCOC)
OLDavCOC_regression_avCOC <- lm(FQA.data$OLDavCOC~ FQA.data$EIA_score_NOSIZE)

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

my.weights<

png('C:/data/NH_MeanC_regression_plot.png', width=1200, height=1100, res=216)
par(mar=c(5,5,4,2))
plot(FQA.data$avCOC ~ FQA.data$EIA_score_NOSIZE, 
     xlab = "EIA score", ylab = "Mean C", xlim=c(1, 4.1), ylim=c(1,7.5), pch=21, col="black", bg='red', cex=1.2, cex.lab=1.5, font.lab=2, font.axis=2)
abline(avCOC_regression_avCOC, lwd=2, col ='red')

points(FQA.data$OLDavCOC ~ FQA.data$EIA_score_NOSIZE, pch=21, cex=1.2, col="black", bg='blue')
abline(OLDavCOC_regression_avCOC, lty=5, lwd=2, col = 'blue')

legend("topleft", title=expression(underline("All Types")), legend=c(expression(paste("New: ", "R"^"2"," = 0.31, p < 0.001")), expression(paste("Old: ", "R"^"2"," = 0.28, p < 0.001"))), bty="n",
       col= c('black', 'black'), pt.bg=c('red','blue'), pch=c(21, 21), cex=1, text.font= c(2,1,1))

dev.off()


#community weighted COC
cwCOC_regression_cwCOC <- lm(FQA.data$cwCOC ~ FQA.data$EIA_score_NOSIZE)

summary(cwCOC_regression_cwCOC)


OLDcwCOC_regression_cwCOC <- lm(FQA.data$OLDcwCOC ~ FQA.data$EIA_score_NOSIZE)

summary(OLDcwCOC_regression_cwCOC)
newCW.summary<-summary(cwCOC_regression_cwCOC)
newCW.summary$r.squared
newCW.summary$coefficients[2,4]


oldCW.summary<-summary(OLDcwCOC_regression_cwCOC)
oldCW.summary$r.squared
oldCW.summary$coefficients[2,4]
oldCWr2<-signif(oldCW.summary$r.squared, 2)
my.aic<-AIC(OLDcwCOC_regression_cwCOC , cwCOC_regression_cwCOC)
akaike.weights(c(my.aic[2,2], my.aic[1,2]))
evidence(236.2884, 264.4035)


png('C:/data/NH_CWmeanC_RegressionPlot2.png', width=1200, height=1100, res=216)
par(mar=c(5,5,4,2))
plot(FQA.data$cwCOC ~ FQA.data$EIA_score_NOSIZE, 
     xlab = "EIA score", ylab = "CW-Mean C", xlim=c(1, 4.1), ylim=c(1,7.5), pch=21, col='black', bg='red', cex=1.2, cex.lab=1.5, font.lab=2, font.axis=2)
abline(cwCOC_regression_cwCOC, lwd=2, col='red')

points(FQA.data$OLDcwCOC ~ FQA.data$EIA_score_NOSIZE, pch=21, cex=1.2, col="black", bg='blue')
abline(OLDcwCOC_regression_cwCOC, lty=5, lwd=2, col ='blue')

legend("topleft", title=expression(underline("All Types")), legend=c(expression(paste("New: ", "R"^"2"," =0.26, p < 0.001")), expression(paste("Old: ", "R"^"2"," = 0.19, p < 0.001"))), bty="n",
       col= c('black', 'black'), pt.bg=c('red','blue'), pch=c(21, 21), cex=1, text.font= c(2,1,1))
dev.off()

###by floodplain Mean C

FQA.flood<-subset(FQA.data, Macrogroup == "Large river floodplain")


avCOC_regression_avCOC <- lm(FQA.flood$avCOC ~ FQA.flood$EIA_score_NOSIZE)
summary(avCOC_regression_avCOC)
OLDavCOC_regression_avCOC <- lm(FQA.flood$OLDavCOC~ FQA.flood$EIA_score_NOSIZE)

summary(OLDavCOC_regression_avCOC)
new.summary<-summary(avCOC_regression_avCOC)
new.summary$r.squared
new.summary$coefficients[2,4]

old.summary<-summary(OLDavCOC_regression_avCOC)
old.summary$r.squared
old.summary$coefficients[2,4]
old.r2<-signif(old.summary$r.squared,2)
old.p<-format(signif(old.summary$coefficients[2,4],2), scientific=F)


my.aic<-AIC(OLDavCOC_regression_avCOC , avCOC_regression_avCOC)
akaike.weights(c(my.aic[2,2], my.aic[1,2]))

png('C:/data/NH_MeanC_regression_plot_floodplain.png', width=1200, height=1100, res=216)
par(mar=c(5,5,4,2))
plot(FQA.flood$avCOC ~ FQA.flood$EIA_score_NOSIZE, 
     xlab = "EIA score", ylab = "Mean C", xlim=c(1, 4.1), ylim=c(1,7.5), pch=21, col="black", bg='red', cex=1.2, cex.lab=1.5, font.lab=2, font.axis=2)
abline(avCOC_regression_avCOC, lwd=2, col ='red')

points(FQA.flood$OLDavCOC ~ FQA.flood$EIA_score_NOSIZE, pch=21, cex=1.2, col="black", bg='blue')
abline(OLDavCOC_regression_avCOC, lty=5, lwd=2, col = 'blue')

legend("topleft", title=expression(underline("Large River Floodplain")), legend=c(expression(paste("New: ", "R"^"2"," = 0.19, p < 0.001")), expression(paste("Old: ", "R"^"2"," = 0.12, p < 0.001"))), bty="n",
       col= c('black', 'black'), pt.bg=c('red','blue'), pch=c(21, 21), cex=1, text.font= c(2,1,1))

dev.off()

#Floodplain community weighted COC
cwCOC_regression_cwCOC <- lm(FQA.flood$cwCOC ~ FQA.flood$EIA_score_NOSIZE)

summary(cwCOC_regression_cwCOC)


OLDcwCOC_regression_cwCOC <- lm(FQA.flood$OLDcwCOC ~ FQA.flood$EIA_score_NOSIZE)

summary(OLDcwCOC_regression_cwCOC)
newCW.summary<-summary(cwCOC_regression_cwCOC)
newCW.summary$r.squared
newCW.summary$coefficients[2,4]


oldCW.summary<-summary(OLDcwCOC_regression_cwCOC)
oldCW.summary$r.squared
oldCW.summary$coefficients[2,4]
oldCWr2<-signif(oldCW.summary$r.squared, 2)

my.aic<-AIC(OLDcwCOC_regression_cwCOC  , cwCOC_regression_cwCOC)
akaike.weights(c(my.aic[2,2], my.aic[1,2]))

png('C:/data/NH_flood_CWmeanC_Regression.png', width=1200, height=1100, res=216)
par(mar=c(5,5,4,2))
plot(FQA.flood$cwCOC ~ FQA.flood$EIA_score_NOSIZE, 
     xlab = "EIA score", ylab = "CW-Mean C", xlim=c(1, 4.1), ylim=c(1,7.5), pch=21, col='black', bg='red', cex=1.2, cex.lab=1.5, font.lab=2, font.axis=2)
abline(cwCOC_regression_cwCOC, lwd=2, col='red')

points(FQA.flood$OLDcwCOC ~ FQA.flood$EIA_score_NOSIZE, pch=21, cex=1.2, col="black", bg='blue')
abline(OLDcwCOC_regression_cwCOC, lty=5, lwd=2, col ='blue')

legend("topleft", title=expression(underline("Large River Floodplain")), legend=c(expression(paste("New: ", "R"^"2"," =0.17, p < 0.001")), expression(paste("Old: ", "R"^"2"," = 0.11, p < 0.001"))), bty="n",
       col= c('black', 'black'), pt.bg=c('red','blue'), pch=c(21, 21), cex=1, text.font= c(2,1,1))
dev.off()


###by swamp mean C

FQA.swamp<-subset(FQA.data, Macrogroup == "Northern swamp")


avCOC_regression_avCOC <- lm(FQA.swamp$avCOC ~ FQA.swamp$EIA_score_NOSIZE)
summary(avCOC_regression_avCOC)
OLDavCOC_regression_avCOC <- lm(FQA.swamp$OLDavCOC~ FQA.swamp$EIA_score_NOSIZE)

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
my.aic<-AIC(OLDavCOC_regression_avCOC  , avCOC_regression_avCOC)
akaike.weights(c(my.aic[2,2], my.aic[1,2]))


png('C:/data/NH_MeanC_regression_plot_swamp.png', width=1200, height=1100, res=216)
par(mar=c(5,5,4,2))
plot(FQA.swamp$avCOC ~ FQA.swamp$EIA_score_NOSIZE, 
     xlab = "EIA score", ylab = "Mean C", xlim=c(1, 4.1), ylim=c(1,7.5), pch=21, col="black", bg='red', cex=1.2, cex.lab=1.5, font.lab=2, font.axis=2)
abline(avCOC_regression_avCOC, lwd=2, col ='red')

points(FQA.swamp$OLDavCOC ~ FQA.swamp$EIA_score_NOSIZE, pch=21, cex=1.2, col="black", bg='blue')
abline(OLDavCOC_regression_avCOC, lty=5, lwd=2, col = 'blue')

legend("topleft", title=expression(underline("Northern Swamp")), legend=c(expression(paste("New: ", "R"^"2"," = 0.55, p < 0.001")), expression(paste("Old: ", "R"^"2"," = 0.6, p < 0.001"))), bty="n",
       col= c('black', 'black'), pt.bg=c('red','blue'), pch=c(21, 21), cex=1, text.font= c(2,1,1))

dev.off()

#swamp community weighted COC
cwCOC_regression_cwCOC <- lm(FQA.swamp$cwCOC ~ FQA.swamp$EIA_score_NOSIZE)

summary(cwCOC_regression_cwCOC)


OLDcwCOC_regression_cwCOC <- lm(FQA.swamp$OLDcwCOC ~ FQA.swamp$EIA_score_NOSIZE)

summary(OLDcwCOC_regression_cwCOC)
newCW.summary<-summary(cwCOC_regression_cwCOC)
newCW.summary$r.squared
newCW.summary$coefficients[2,4]


oldCW.summary<-summary(OLDcwCOC_regression_cwCOC)
oldCW.summary$r.squared
oldCW.summary$coefficients[2,4]
oldCWr2<-signif(oldCW.summary$r.squared, 2)

my.aic<-AIC(OLDcwCOC_regression_cwCOC  , cwCOC_regression_cwCOC)
akaike.weights(c(my.aic[2,2], my.aic[1,2]))


png('C:/data/NH_swamp_CWmeanC_Regression.png', width=1200, height=1100, res=216)
par(mar=c(5,5,4,2))
plot(FQA.swamp$cwCOC ~ FQA.swamp$EIA_score_NOSIZE, 
     xlab = "EIA score", ylab = "CW-Mean C", xlim=c(1, 4.1), ylim=c(1,7.5), pch=21, col='black', bg='red', cex=1.2, cex.lab=1.5, font.lab=2, font.axis=2)
abline(cwCOC_regression_cwCOC, lwd=2, col='red')

points(FQA.swamp$OLDcwCOC ~ FQA.swamp$EIA_score_NOSIZE, pch=21, cex=1.2, col="black", bg='blue')
abline(OLDcwCOC_regression_cwCOC, lty=5, lwd=2, col ='blue')

legend("topleft", title=expression(underline("Northern Swamp")), legend=c(expression(paste("New: ", "R"^"2"," =0.49, p < 0.001")), expression(paste("Old: ", "R"^"2"," = 0.52, p < 0.001"))), bty="n",
       col= c('black', 'black'), pt.bg=c('red','blue'), pch=c(21, 21), cex=1, text.font= c(2,1,1))
dev.off()


### Box PLOTS all types mean c #revisions mean C

FQA.data_rank<-subset(FQA.data,EIA_RANK_NOSIZE!="")
FQA.data_rank$EIA_RANK_NOSIZE<-as.character(FQA.data_rank$EIA_RANK_NOSIZE)
FQA.data_rank$EIA_RANK_NOSIZE<-as.factor(FQA.data_rank$EIA_RANK_NOSIZE)
boxplot(FQA.data_rank$avCOC ~ FQA.data_rank$EIA_RANK_NOSIZE, main = "All Types",
        xlab = "EIA rank", ylab = "Mean C")

AOV_avCOC <- aov(FQA.data_rank$avCOC ~ FQA.data_rank$EIA_RANK_NOSIZE)

model.tables(AOV_avCOC, type="means")
summary(AOV_avCOC )
TukeyHSD(AOV_avCOC )
AOV_OLDavCOC <- aov(FQA.data_rank$OLDavCOC ~ FQA.data_rank$EIA_RANK_NOSIZE)
summary(AOV_OLDavCOC )
TukeyHSD(AOV_OLDavCOC )
model.tables(AOV_OLDavCOC, type="means")

FQA.data_rank$colors[FQA.data_rank$EIA_RANK_NOSIZE=="A"]<-"gray40"
FQA.data_rank$colors[FQA.data_rank$EIA_RANK_NOSIZE=="B"]<-"orange2"
FQA.data_rank$colors[FQA.data_rank$EIA_RANK_NOSIZE=="C"]<-"midnightblue"

compare_means(avCOC~EIA_RANK_NOSIZE,  data = FQA.data_rank, method="anova")
my_comparisons <- list( c("A", "B"), c("B", "C"), c("A", "C") )
AIC(AOV_OLDavCOC,AOV_avCOC )
akaike.weights(c(225.8572, 258.2585))
evidence(225.8572, 258.2585)
.99/.0000009207

sqrt(0.260)
sqrt(0.324)

png('C:/data/MEboxplot_2panel_alltypes.png', width=1300, height=1000, res=216)
newMeanC.plot<-ggboxplot(FQA.data_rank, x = "EIA_RANK_NOSIZE", y = "avCOC", size=1, color="Red" )+ 
  ylim(2,9)+
  ggtitle("All Types (new C)")+
  labs(x = "EIA Rank", y="Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=8, label="a", size=7)+
  geom_text(x=2, y=7, label="b", size=7)+
  geom_text(x=3, y=6, label="c", size=7)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

oldMeanC.plot<-ggboxplot(FQA.data_rank, x = "EIA_RANK_NOSIZE", y = "OLDavCOC", size=1, color="blue" )+ 
  ylim(2,9)+
  ggtitle("All Types (old C)")+
  labs(x = "EIA Rank", y="Mean-C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=7, label="a", size=7)+
  geom_text(x=2, y=6, label="b", size=7)+
  geom_text(x=3, y=5, label="c", size=7)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")


ggarrange(oldMeanC.plot,newMeanC.plot, ncol=2)

dev.off()

###Boxplot_flood mean c

FQA.flood_rank <- subset(FQA.data_rank, Macrogroup == "Large river floodplain")

AOV_avCOC <- aov(FQA.flood_rank$avCOC ~ FQA.flood_rank$EIA_RANK_NOSIZE)
summary(AOV_avCOC )
TukeyHSD(AOV_avCOC )
AOV_OLDavCOC <- aov(FQA.flood_rank$OLDavCOC ~ FQA.flood_rank$EIA_RANK_NOSIZE)
summary(AOV_OLDavCOC )
TukeyHSD(AOV_OLDavCOC )
png('C:/data/MEboxplot_2panel_flood.png', width=1300, height=1000, res=216)
newMeanC.plot<-ggboxplot(FQA.flood_rank, x = "EIA_RANK_NOSIZE", y = "avCOC", size=1, color="Red" )+ 
  ylim(2,8)+
  ggtitle("Large River Floodplain (new C)")+
  labs(x = "EIA Rank", y="Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=6.5, label="ab", size=7)+
  geom_text(x=2, y=6, label="ab", size=7)+
  geom_text(x=3, y=5.5, label="c", size=7)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

oldMeanC.plot<-ggboxplot(FQA.flood_rank, x = "EIA_RANK_NOSIZE", y = "OLDavCOC", size=1, color="blue" )+ 
  ylim(2,8)+
  ggtitle("Large River Floodplain (old C)")+
  labs(x = "EIA Rank", y="Mean-C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=6.5, label="ab", size=7)+
  geom_text(x=2, y=6, label="ab", size=7)+
  geom_text(x=3, y=5.5, label="c", size=7)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")


ggarrange( oldMeanC.plot, newMeanC.plot,ncol=2)

dev.off()

#boxplot swamp mean c

FQA.swamp_rank <- subset(FQA.data_rank, Macrogroup == "Northern swamp")

AOV_avCOC <- aov(FQA.swamp_rank$avCOC ~ FQA.swamp_rank$EIA_RANK_NOSIZE)
summary(AOV_avCOC )
TukeyHSD(AOV_avCOC )
AOV_OLDavCOC <- aov(FQA.swamp_rank$OLDavCOC ~ FQA.swamp_rank$EIA_RANK_NOSIZE)
summary(AOV_OLDavCOC )
TukeyHSD(AOV_OLDavCOC )
png('C:/data/MEboxplot_2panel_swamp.png', width=1300, height=1000, res=216)
newMeanC.plot<-ggboxplot(FQA.swamp_rank, x = "EIA_RANK_NOSIZE", y = "avCOC", size=1, color="Red" )+ 
  ylim(2,8)+
  ggtitle("Northern Swamp (new C)")+
  labs(x = "EIA Rank", y="Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=6.5, label="ab", size=7)+
  geom_text(x=2, y=6, label="abc", size=7)+
  geom_text(x=3, y=5.5, label="c", size=7)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

oldMeanC.plot<-ggboxplot(FQA.swamp_rank, x = "EIA_RANK_NOSIZE", y = "OLDavCOC", size=1, color="blue" )+ 
  ylim(2,8)+
  ggtitle("Northern Swamp (old C)")+
  labs(x = "EIA Rank", y="Mean-C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=6.5, label="ab", size=7)+
  geom_text(x=2, y=6, label="ab", size=7)+
  geom_text(x=3, y=5.5, label="c", size=7)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")


ggarrange(oldMeanC.plot,newMeanC.plot, ncol=2)

dev.off()


###Boxplot_flood mean c

FQA.flood_rank <- subset(FQA.data_rank, Macrogroup == "Large river floodplain")

AOV_avCOC <- aov(FQA.flood_rank$avCOC ~ FQA.flood_rank$EIA_RANK_NOSIZE)
summary(AOV_avCOC )
TukeyHSD(AOV_avCOC )
AOV_OLDavCOC <- aov(FQA.flood_rank$OLDavCOC ~ FQA.flood_rank$EIA_RANK_NOSIZE)
summary(AOV_OLDavCOC )
TukeyHSD(AOV_OLDavCOC )
png('C:/data/MEboxplot_2panel_flood.png', width=1300, height=1000, res=216)
newMeanC.plot<-ggboxplot(FQA.flood_rank, x = "EIA_RANK_NOSIZE", y = "avCOC", size=1, color="Red" )+ 
  ylim(2,8)+
  ggtitle("Large River Floodplain (new C)")+
  labs(x = "EIA Rank", y="Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=6.5, label="ab", size=7)+
  geom_text(x=2, y=6, label="ab", size=7)+
  geom_text(x=3, y=5.5, label="c", size=7)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

oldMeanC.plot<-ggboxplot(FQA.flood_rank, x = "EIA_RANK_NOSIZE", y = "OLDavCOC", size=1, color="blue" )+ 
  ylim(2,8)+
  ggtitle("Large River Floodplain (old C)")+
  labs(x = "EIA Rank", y="Mean-C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=6.5, label="ab", size=7)+
  geom_text(x=2, y=6, label="ab", size=7)+
  geom_text(x=3, y=5.5, label="c", size=7)+
  # geom_text(data = label.df, label = "***")+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")


ggarrange( oldMeanC.plot, newMeanC.plot,ncol=2)

dev.off()

#boxplot swamp cwMean C

FQA.swamp_rank <- subset(FQA.data_rank, Macrogroup == "Northern swamp")

AOV_avCOC <- aov(FQA.swamp_rank$cwCOC ~ FQA.swamp_rank$EIA_RANK_NOSIZE)
summary(AOV_avCOC )
TukeyHSD(AOV_avCOC )
AOV_OLDavCOC <- aov(FQA.swamp_rank$OLDcwCOC ~ FQA.swamp_rank$EIA_RANK_NOSIZE)
summary(AOV_OLDavCOC )
TukeyHSD(AOV_OLDavCOC )

my.sizes<-table(FQA.swamp_rank$EIA_RANK_NOSIZE)


png('C:/data/MEboxplot_swamp_CWmeanC.png', width=1300, height=1000, res=216)
newMeanC.plot<-ggboxplot(FQA.swamp_rank, x = "EIA_RANK_NOSIZE", y = "cwCOC", size=1, color="Red" )+ 
  ylim(1,8)+
  ggtitle("Northern Swamp (new C)")+
  labs(x = "EIA Rank", y="cw-Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=6.5, label="a", size=7)+
  geom_text(x=2, y=6, label="ab", size=7)+
  geom_text(x=3, y=5.5, label="b", size=7)+
  geom_text(x=1, y=1, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=1, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=1, label=paste("N=", my.sizes[3], sep=""),size=5)+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

oldMeanC.plot<-ggboxplot(FQA.swamp_rank, x = "EIA_RANK_NOSIZE", y = "OLDcwCOC", size=1, color="blue" )+ 
  ylim(1,8)+
  ggtitle("Northern Swamp (old C)")+
  labs(x = "EIA Rank", y="cw-Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=6.5, label="a", size=7)+
  geom_text(x=2, y=6, label="a", size=7)+
  geom_text(x=3, y=5.5, label="b", size=7)+
  geom_text(x=1, y=1, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=1, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=1, label=paste("N=", my.sizes[3], sep=""),size=5)+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

ggarrange(oldMeanC.plot,newMeanC.plot, ncol=2)
dev.off()

###Boxplot_flood mean c

FQA.flood_rank <- subset(FQA.data_rank, Macrogroup == "Large river floodplain")

AOV_avCOC <- aov(FQA.flood_rank$cwCOC ~ FQA.flood_rank$EIA_RANK_NOSIZE)
summary(AOV_avCOC )
TukeyHSD(AOV_avCOC )
AOV_OLDavCOC <- aov(FQA.flood_rank$OLDcwCOC ~ FQA.flood_rank$EIA_RANK_NOSIZE)
summary(AOV_OLDavCOC )
TukeyHSD(AOV_OLDavCOC )

my.sizes<-table(FQA.flood_rank$EIA_RANK_NOSIZE)

png('C:/data/MEboxplot_flood_CWmeanC.png', width=1300, height=1000, res=216)
newMeanC.plot<-ggboxplot(FQA.flood_rank, x = "EIA_RANK_NOSIZE", y = "cwCOC", size=1, color="Red" )+ 
  ylim(1,8)+
  ggtitle("Large River Floodplain (new C)")+
  labs(x = "EIA Rank", y="cw-Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=6.5, label="a", size=7)+
  geom_text(x=2, y=6.5, label="a", size=7)+
  geom_text(x=3, y=6.5, label="b", size=7)+
  geom_text(x=1, y=1, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=1, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=1, label=paste("N=", my.sizes[3], sep=""),size=5)+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

oldMeanC.plot<-ggboxplot(FQA.flood_rank, x = "EIA_RANK_NOSIZE", y = "OLDcwCOC", size=1, color="blue" )+ 
  ylim(1,8)+
  ggtitle("Large River Floodplain (old C)")+
  labs(x = "EIA Rank", y="cw-Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=6.5, label="ab", size=7)+
  geom_text(x=2, y=6, label="a", size=7)+
  geom_text(x=3, y=5.5, label="bc", size=7)+
  geom_text(x=1, y=1, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=1, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=1, label=paste("N=", my.sizes[3], sep=""),size=5)+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

ggarrange(oldMeanC.plot,newMeanC.plot, ncol=2)
dev.off()

### Box PLOTS all types cwmean c

FQA.data_rank<-subset(FQA.data,EIA_RANK_NOSIZE!="")
FQA.data_rank$EIA_RANK_NOSIZE<-as.character(FQA.data_rank$EIA_RANK_NOSIZE)
FQA.data_rank$EIA_RANK_NOSIZE<-as.factor(FQA.data_rank$EIA_RANK_NOSIZE)

AOV_avCOC <- aov(FQA.data_rank$cwCOC ~ FQA.data_rank$EIA_RANK_NOSIZE)
summary(AOV_avCOC )
TukeyHSD(AOV_avCOC )

model.tables(AOV_avCOC , type="means")

AOV_OLDavCOC <- aov(FQA.data_rank$OLDcwCOC ~ FQA.data_rank$EIA_RANK_NOSIZE)
summary(AOV_OLDavCOC )
TukeyHSD(AOV_OLDavCOC )
model.tables(AOV_OLDavCOC, type="means")
my.sizes<-table(FQA.data_rank$EIA_RANK_NOSIZE)


png('C:/data/MEboxplot_all_CWmeanC.png', width=1300, height=1000, res=216)
newMeanC.plot<-ggboxplot(FQA.data_rank, x = "EIA_RANK_NOSIZE", y = "cwCOC", size=1, color="Red" )+ 
  ylim(1,8)+
  ggtitle("All Types (new C)")+
  labs(x = "EIA Rank", y="cw-Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=7, label="a", size=7)+
  geom_text(x=2, y=7, label="a", size=7)+
  geom_text(x=3, y=7, label="b", size=7)+
  geom_text(x=1, y=1, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=1, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=1, label=paste("N=", my.sizes[3], sep=""),size=5)+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

oldMeanC.plot<-ggboxplot(FQA.data_rank, x = "EIA_RANK_NOSIZE", y = "OLDcwCOC", size=1, color="blue" )+ 
  ylim(1,8)+
  ggtitle("All Types (old C)")+
  labs(x = "EIA Rank", y="cw-Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  geom_text(x=1, y=7, label="a", size=7)+
  geom_text(x=2, y=7, label="a", size=7)+
  geom_text(x=3, y=7, label="b", size=7)+
  geom_text(x=1, y=1, label=paste("N=", my.sizes[1], sep=""),size=5)+
  geom_text(x=2, y=1, label=paste("N=", my.sizes[2], sep=""),size=5)+
  geom_text(x=3, y=1, label=paste("N=", my.sizes[3], sep=""),size=5)+
  theme(plot.title = element_text(hjust=0.5, size=13),  panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="none")

ggarrange(oldMeanC.plot,newMeanC.plot, ncol=2)
dev.off()


### comparing types like MN


FQA.swamp_flood <- subset(FQA.data_rank, Macrogroup == "Northern swamp"| Macrogroup == "Large river floodplain")
FQA.swamp_flood$Macrogroup<-as.factor(as.character(FQA.swamp_flood$Macrogroup))
FQA.swamp_flood<- subset(FQA.swamp_flood, EIA_RANK_NOSIZE !="B")
FQA.swamp_flood$EIA_RANK_NOSIZE<-as.factor(as.character(FQA.swamp_flood$EIA_RANK_NOSIZE))
FQA.swamp_floodA <- subset(FQA.swamp_flood, EIA_RANK_NOSIZE == "A")
FQA.swamp_floodC <- subset(FQA.swamp_flood, EIA_RANK_NOSIZE == "C")


formation_A <- aov(FQA.swamp_floodA$cwCOC ~ FQA.swamp_floodA$Macrogroup)
summary(formation_A)
TukeyHSD(formation_A)
formation_C<- aov(FQA.swamp_floodC$cwCOC ~ FQA.swamp_floodC$Macrogroup)
summary(formation_C)
TukeyHSD(formation_C)


group.colors <- c("Northern swamp" = "orange1", "Large river floodplain" = "darkorchid2")

png('C:/data/NHboxplot_formations_meanCW.png', width=1300, height=1000, res=216)
formation.plotA<-ggplot(data=FQA.swamp_flood, aes(x = EIA_RANK_NOSIZE, y=cwCOC))+
  geom_boxplot(aes(color = Macrogroup), fill=NA, position=position_dodge(width=0.035), size=1)+
  ylim(2,8)+

  scale_color_manual(values=group.colors)+

  labs(x = "EIA Rank", y="CW-Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  theme(plot.title = element_text(hjust=0.5, size=13),   axis.text.x = element_text(size=12, face="bold"), panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), legend.key = element_rect(colour = NA, fill = NA))
formation.plotA
dev.off()


group.colors <- c("A" = "chartreuse4", "C" = "orange1")

png('C:/data/NHboxplot_formations_meanCW2.png', width=1300, height=1000, res=216)
formation.plotA<-ggplot(data=FQA.swamp_flood, aes(x = Macrogroup, y=cwCOC))+
  geom_boxplot(aes(color = EIA_RANK_NOSIZE), fill=NA, position=position_dodge(width=0.035), size=1)+
  ylim(2,8)+
  
  scale_color_manual(values=group.colors)+
  
  labs(x = "EIA Rank", y="CW-Mean C")+
  guides(color=guide_legend(title="Rank"))+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  theme(plot.title = element_text(hjust=0.5, size=13),   axis.text.x = element_text(size=12, face="bold"), panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="right",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), legend.key = element_rect(colour = NA, fill = NA),axis.title.x=element_blank())
formation.plotA
dev.off()





formation.plotA<-ggplot(data=FQA.swamp_flood, aes(x = EIA_RANK_NOSIZE, y=cwCOC))+
  geom_boxplot(aes(color = Macrogroup), fill=NA, position=position_dodge(width=0.035), size=1)+
  ylim(2,8)+
  labs(x = "EIA Rank", y="CW Mean C")+
  font("xylab", size = 16, face="bold")+
  font("xy.text", size = 14, color = "black", face = "bold")+
  theme(plot.title = element_text(hjust=0.5, size=13),   axis.text.x = element_text(size=10, face="bold"), panel.border = element_rect(colour = "black", fill=NA, size=1.5), legend.position="right",
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), legend.key = element_rect(colour = NA, fill = NA))


geom_boxplot(color="red", fill="orange", alpha=0.2)+ 
 
  ylim(2,8)+



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





large_river_floodplain <- subset(FQA.data_rank, Macrogroup == "Large river floodplain")
northern_swamp <- subset(FQA.data_rank, Macrogroup == "Northern swamp")


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

hist(large_river_floodplain$OLDcwCOC)
large_river_floodplain$transformed<-sqrt(max(large_river_floodplain$OLDcwCOC+1)-large_river_floodplain$OLDcwCOC)

boxplot(large_river_floodplain$transformed ~ large_river_floodplain$EIA_RANK_NOSIZE, main = "Large river floodplain",
        xlab = "EIA rank", ylab = "cwCOC")
EIA_floodplain_AOV_cwCOC_OLD <- aov(large_river_floodplain$transformed ~ large_river_floodplain$EIA_RANK_NOSIZE)
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


