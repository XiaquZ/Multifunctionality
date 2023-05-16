#Set working directory
setwd(choose.dir())

#Load libraries (first install them of course)
library(xlsx)
library(FD)
library(vegan)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(plyr)

#################################################################
#####Import data

#Import sample matrix (plots in rows, species in columns). First column are plot names, without column header.
sample<-read.xlsx(file=file.choose(),sheetName = "BA",header=TRUE, row.names=1)

#Import trait matrix (species in rows, traits in columns). First column are species names, without column header.
traits<-read.xlsx(file=file.choose(),sheetName = "ES",header=TRUE, row.names=1)

sample<-as.matrix(sample)
traits<-as.matrix(traits)

str(traits)
str(sample)
head(sample)
head(traits)

#################################################################
#####Single MF

#Calculate relative abundance
sample_stand<-decostand(sample, "total", MARGIN=1)

#Calculate CWM and SR
CWM<-sample_stand%*%traits
SR<-rowSums(sample != 0)
SR<-as.data.frame(SR)
write.xlsx2(CWM, col.names=TRUE, row.names=TRUE,"CWM.xlsx")

#prepare data for plotting
l<-cbind(SR, CWM[,17:24])
l<-as.data.frame(l)
plotsingle<-melt(l,id="SR")
psFits <- dlply(plotsingle, .(variable), function(x) lm(value ~ SR, data=x))
psLabels <- data.frame(variable=levels(plotsingle$variable), SR=10, value=c(1.5,2.5,0.7,2.5,1.5,2.5,5.5,2.5), lab=paste(c(1:8), ")", sep=""), r2 = sapply(psFits, function(x) summary(x)$r.squared),p = sapply(psFits, function(x) anova(x) [1,5]))
psLabels$labels <- with(psLabels, paste(lab, "p =", round(p,3),expression(R^2), "=", round(r2,2)," ", sep=" "))
psLabels$labels <- gsub("p = 0 ", "p < 0.001 ", psLabels$labels)

#plot functions
ggplot(aes(x=SR, y=value),data=plotsingle) +
  geom_point(size=2.5)+
  facet_wrap(~variable, scales="free", nrow=2) +
  theme_bw(base_size=15)+
  stat_smooth(method="lm", colour="black", size=1.5) +
  xlab("\nSpecies Richness") +
  ylab("Value of Function\n") +
  scale_x_continuous(breaks=c(2,4,6,8))+
  geom_text(data=psLabels,
            aes(label=labels), nudge_x =-5) +
  theme(panel.grid = element_blank())

#################################################################
#####Average MF

#Z-standardization following Byrnes et al., 2014
CWM_stand<-decostand(CWM, "max", MARGIN=2)

z<-cbind(SR, CWM_stand)
z<-as.data.frame(z)
plotaverage<-melt(z,id=c("SR","LU"))

#plot standardized functions on common scale
ggplot(aes(x=SR, y=value),data=plotaverage)+geom_point(size=2.5)+
  facet_grid(~variable) +
  theme_bw(base_size=15) +
  stat_smooth(method="lm", colour="black", size=1.5) +
  xlab("\nSpecies richness") + ylab("Average value of standardized functions\n")

#Calculate MFav and species richness
MFav<-rowMeans(CWM_stand, na.rm = TRUE)
MFav<-as.data.frame(MFav)
MF<-cbind(SR,LU, MFav)
write.xlsx2(MF, col.names=TRUE, row.names=TRUE,"MF.xlsx")

#Statistical fit
lm<-lm(MFav~SR, data=MF)
lm2<-lm(MFav~SR+LU, data=MF)
lm3<-lm(MFav~SR+LU+SR*LU, data=MF)

#plot average MF
p2<-anova(lm(MFav ~ SR, data=MF))[1,5]
r22<-summary(lm(MFav ~ SR, data=MF))$r.squared
labels<-paste("p=",round(p2,3), "R^2=",round(r22,2),sep=" ")
labels2<-as.data.frame(labels)

ggplot(MF, aes(x=SR, y=MFav, shape = LU))+geom_point(size=2.5)+
  theme_bw(base_size=15)+
  stat_smooth(method="lm", size=1.5, se=FALSE, aes(colour = LU)) +
  scale_x_continuous(breaks=c(2,4,6,8))+
  geom_text(data=labels2, aes(8,0.385,label=labels)) +
  xlab("\nSpecies richness") + ylab("Average value of standardized functions\n")

#Export figure as high resolution tiff
tiff(file = "SR~MFav.tiff", width = 2700, height = 2000, units = "px", res = 300)
ggplot(MF, aes(x=SR, y=MFav))+geom_point(aes(shape = factor(LU)), size=2.5)+
  theme_bw(base_size=15)+
  stat_smooth(method="lm", size=1.5, colour="black") +
  scale_x_continuous(breaks=c(2,4,6,8))+
  geom_text(data=labels2, aes(8,0.385,label=labels)) +
  xlab("\nSpecies richness") + ylab("Average value of standardized functions\n")
dev.off()

#################################################################
#####Single threshold-based MF

MFthres<-SR
m<-as.numeric(dim(sample)[[1]])
t<-as.numeric(dim(traits)[[2]])
maxf <-apply(CWM, 2,max, na.rm = TRUE)
maxf<-as.matrix(t(maxf))
threshmin<-0.01
threshmax<-0.75
threshstep<-0.01
threshold <- seq(threshmin, threshmax,threshstep)
thresholds<-as.matrix(threshold)

for (i in 1:(t-1)){
  thresholds<-cbind(thresholds,thresholds[,1])
}

compare<-sweep(thresholds,MARGIN=2,maxf,'*')
colnames(compare)<-colnames(CWM)
rownames(compare)<-threshold

for (i in 1:((threshmax-threshmin+0.01)*100)) {
  y<-adply(CWM,1, function(x) sum(x>=compare[i,], na.rm=TRUE))
  MFthres<-cbind(MFthres,y[,2])
}

colnames(MFthres)<-c("SR",rownames(compare))
u<-melt(MFthres, id="SR")
u$percent<-as.numeric(as.character(u$variable))

#Fit single thresholds
glm<-glm(value ~ SR, data=subset(u, u$percent==0.80), family=quasipoisson(link="identity"))

#plot 20,40,60 and 80%
qw <- function(...) {
  sapply(match.call()[-1], deparse)
}
gcPlot<-subset(u, u$variable %in% qw(0.2, 0.4, 0.6, 0.8))
gcPlot$percentage<-paste(100*gcPlot$percent, "%", sep="")

qplot(SR, value, data=gcPlot, facets=~percentage) +
  stat_smooth(method="glm", method.args=list(family=quasipoisson(link="identity")), colour="red", lwd=1.2) +
  ylab(expression("Number of functions >= threshold")) +
  xlab("Species richness") +
  theme_bw(base_size=14) +
  geom_text(data=data.frame(percentage = unique(gcPlot$percentage), lab = paste(letters[1:4], ")", sep=""),
                            SR=9, value=16), mapping=aes(x=SR, y=value, label=lab))

#################################################################
#####Multiple threshold-based MF

#plot MFthreshold
ggplot(aes(x=SR, y=value, group=variable),data=u) +
  stat_smooth(method="glm", method.args=list(family=quasipoisson(link="identity")), lwd=0.8,fill=NA, aes(colour=percent)) +
  ylab(expression("Number of functions >= threshold")) +
  xlab("Species richness") +
  theme_bw(base_size=14) +
  scale_color_gradient(name="Percent of \nmaximum", low="blue", high="red")+
  scale_x_continuous(breaks=c(2,4,6,8))

#Plot slopes of all thresholds
Slopes<-ddply(u, .(percent), function(x) {
              t<-glm(x, formula= value ~ SR,family=quasipoisson(link="identity"))
              data.frame(Estimate=coefficients(t)[2],Std.Error=coef(summary(t))[1,2])})

ggplot(Slopes, aes(x=percent)) +
  geom_ribbon(fill="grey50", aes(x=percent*100, ymin=Estimate-
                                   1.96*Slopes[["Std.Error"]],
                                 ymax=Estimate+1.96*Slopes[["Std.Error"]])) +
  geom_point(aes(x=percent*100, y=Estimate)) +
  ylab("Change in Number of Functions per Addition of 1 Species\n") +
  xlab("\nThreshold (%)") +
  geom_abline(intercept=0, slope=0, lwd=1, linetype=2) +
  theme_bw(base_size=14)


