#Based on the description of the design it seems that you could combine the two groups to gain more information when modeling the correlation structure for the repeated measurements over time (i.e., you could make the assumption that the random-effects structure is the same in the two groups). However, in the fixed effects you should consider the two groups separately because of the difference in the time periods
#Fixed different groups
#Random same

require(lme4)
require(ggplot2)
require(ggpubr)
require(magrittr)
require(tidyverse)
require(report)
setwd("~/Downloads/Hand Sanitiser")
df<-read.csv("Data/gerba.data.20200302.csv",header=TRUE)


#Mixed effects model

ContactTime   <- df$time
TreatmentContrast <- df$time * (1-2*(df$treatment == 'Treated')) #Makes dummy variables (-1,1)*NumberContacts
data <- cbind(df, as.factor(ContactTime), TreatmentContrast)
m    <- lmer(log10(conc) ~ group + ContactTime + TreatmentContrast+# treatment+#Contactscontrast +#+ log10(Inoculum)
                (treatment+ContactTime+TreatmentContrast | id)  , #+ Contactscontrast
             data=subset(data,time>0), weights = log10(data$conc))

m    <- lmer(log10(conc) ~  time:treatment +#Contactscontrast +#+ log10(Inoculum)
               (time + treatment| id)  , #+ Contactscontrast
             data=df, REML=FALSE, weights = log10(df$conc))

plot(m)
summary(m)
report(m)
#Linear models


linMod<-lm(data=subset(df,time>0),log10(conc)~time*treatment)
report(linMod)
summary(linMod)
plot(linMod)


r <- report(m)

text_long(r)



# https://stats.stackexchange.com/questions/33013/what-test-can-i-use-to-compare-slopes-from-two-or-more-regression-models
#gls(log10(conc) ~ treatment*time, data=df,weights=varIdent(form=~1|treatment),method = "ML")
library(lsmeans)
m.interaction <- lm(log10(conc) ~ treatment*time, data=df)
anova(m.interaction)
# Obtain slopes
m.interaction$coefficients
m.lst <- lstrends(m.interaction, "treatment", var="time")

# Compare slopes
pairs(m.lst)


######

a=aggregate(data=df,conc~treatment+time,mean)
#Find the difference between treated and untreated individuals over time
b=(a$conc[a$treatment=="Untreated"]-a$conc[a$treatment=="Treated"])
#Add time back to the frame
d=cbind(b,a$time)%>%set_colnames(c("conc","time"))%>%as.data.frame()
#lienar model
plot(lm(data=d,log10(conc)~time))




#Makes a baseline for both groups

df$X<-NULL
df$id<-as.factor(df$id)
mBefore=cbind((df$conc[df$treatment=="Untreated" & df$time==0]),as.numeric(rep(-0.1,NROW((df$conc[df$treatment=="Untreated"& df$time==0])))),rep(1,NROW((df$conc[df$treatment=="Untreated"& df$time==0]))),rep("Treated",NROW((df$conc[df$treatment=="Untreated"& df$time==0]))),rep("Group 1",NROW((df$conc[df$treatment=="Untreated"& df$time==0]))))
mBefore=mBefore%>%as_tibble()%>%set_colnames(colnames(df))
mBefore$conc<-as.integer(mBefore$conc)
mBefore$time<-as.numeric(mBefore$time)
A<-rbind(mBefore,df)

mBefore=cbind((df$conc[df$treatment=="Untreated" & df$time==0]),as.numeric(rep(-0.1,NROW((df$conc[df$treatment=="Untreated" & df$time==0])))),rep(1,NROW((df$conc[df$treatment=="Untreated" & df$time==0]))),rep("Untreated",NROW((df$conc[df$treatment=="Untreated" & df$time==0]))),rep("Group 1",NROW((df$conc[df$treatment=="Untreated"& df$time==0]))))
mBefore=mBefore%>%as_tibble()%>%set_colnames(colnames(df))
mBefore$conc<-as.integer(mBefore$conc)
mBefore$time<-as.numeric(mBefore$time)

df$time<-as.numeric(df$time)
A<-rbind(mBefore,A)

#This is a plot of all groups
ggplot(A,aes(x=time,y=conc,colour=treatment))+
  geom_point(colour="black",alpha=0.3)+
  stat_summary(fun.data = "mean_cl_boot", size = 0.8)+ # position = position_dodge(width=0.75)
  #geom_smooth(method="lm")+
  #annotate("text", x = 3, y = 10E4, label = "Baseline")+
  annotate(
    geom = "curve", x = 4, y = 15E4, xend = -1, yend = 3E4, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 4.1, y = 15E4, label = "Baseline", hjust = "left")+
  scale_colour_manual(values = c("blue", "red"),name="Treatment")+
  scale_x_continuous(name="Time (h) after sanitising", scales::pretty_breaks(n = 12))+
  scale_y_continuous("Total aerobic colony count",trans="log10")+
  facet_grid(~.,scales = "free_y")+ 
  theme(axis.text.y   = element_text(colour="black",size=12),
                                          axis.text.x   = element_text(colour="black",size=12),
                                          axis.title.y  = element_text(colour="black",size=12),
                                          axis.title.x  = element_text(colour="black",size=12),
                                          panel.background = element_blank(),
                                          panel.grid.major = element_blank(), 
                                          panel.grid.minor = element_blank(),
                                          axis.line = element_line(colour = "black"),
                                          panel.border = element_rect(colour = "black", fill=NA, size=1),
                                          strip.background = element_rect(fill = "black"),
                                          strip.text = element_text(color = "white"),
                                          text=element_text(size=12,  family="sans")
  )
ggsave("Figure 1.pdf",width = 21,height=15,units="cm")


# Kruskal wallis at time 0
kruskal.test(A$conc[A$time==0]~A$treatment[A$time==0])
kruskal.test(A$conc[A$time==24]~A$treatment[A$time==24])

#Rate of recontamination
#Calculate the difference between each person's contamination
r=A %>%
  group_by(treatment,id) %>%
  mutate(Diff = conc - lag(conc))
#plot the lag between the contamination
ggplot(r,aes(x=time,y=Diff,colour=treatment))+geom_point()+
  scale_y_continuous(name="ACC difference",trans="log2")+
  geom_smooth(method="lm")+
  facet_grid(.~treatment,scales="free_y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


summary(lm(data=subset(A,time>2), log10(conc)~time*treatment)) 

r=aggregate(data=A, conc~time+treatment,mean) %>%as_tibble()
aT=cbind(diff(r$conc[r$treatment=="Treated"])/c(1,2,2,2,10,2,2,2,2),c("Just After Treatment","0-2","2-4","4-6","6-16","16-18","18-20","20-22","22-24"))%>%as_tibble()
aU=cbind(diff(r$conc[r$treatment=="Untreated"])/c(1,2,2,2,10,2,2,2,2),c("Baseline-0","0-2","2-4","4-6","6-16","16-18","18-20","20-22","22-24"))%>%as_tibble()
aU$treatment=rep('Untreated')
aT$treatment=rep('Treated')
aTU<-rbind(aT,aU)%>%set_colnames(c("ConcDiff","time","treatment"))
aTU$ConcDiff<-as.numeric(aTU$ConcDiff)
aTU$time <- factor(aTU$time, levels = c("Baseline-0","Just After Treatment", "0-2","2-4","4-6" ,"6-16","16-18","18-20","20-22","22-24"))            

ggplot(aTU,aes(x=time,y=ConcDiff,colour=treatment))+geom_point()+
  scale_y_continuous(name="ACC difference",trans="log2")+
  geom_smooth(method="lm")+
  facet_grid(.~treatment,scales="free_y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary(lm(data=r,r$conc[r$treatment=="Treated"]~c(0,2,2,2,2,10,2,2,2,2))) 

summary(lm(data=subset(r,treatment=="Treated" & time >0),log10(conc)~time))
summary(lm(data=subset(r,treatment=="Untreated" & time >0),log10(conc)~time))


R<-data.frame(C=c(3.1,2.9,2.2,1.1),
              time=c(0,1,3,6),
              s=c(0.9,1.2,0.9,1))
plot(diff(R$C))              
              
              
              
              
              
#######
#read in the data
df<-read.csv("Data/gerba_data_summary.csv",header=T)
df%>%
filter(time>6)%>%
ggplot(aes(x=time,y=mean,colour=treatment))+
geom_point()+
  geom_smooth(method="lm")+
  scale_x_continuous(name="Time (h) after sanitising", scales::pretty_breaks(n = 12))+
  scale_y_continuous("Total aerobic colony count",trans="log")

fit_lm<-lm(log(mean)~seq(0,8,2),data=df%>%filter(treatment=="Untreated" & time >6))
fit_lm

fit_lm<-lm(log(mean)~seq(0,8,2),data=df%>%filter(treatment=="Treated" & time >6))
fit_lm

#fit an exponential to the data
