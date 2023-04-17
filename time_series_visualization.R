# visualizing time series data (eye tracking) with ggplot2
rm(list=ls(all=T))
library(dplyr)
library(ggplot2)
setwd("~/Dropbox/Portfolio/time_series_visualization")
exp1data <- read.csv("exp1.data.csv", header=T)
ds <- group_by(exp1data, subject, age.group, condition, timefromnoun) %>% summarise(targetlook = mean(targetlook))
se <- function(x) sqrt(var(x)/length(x))
pd <- group_by(ds, age.group, condition, timefromnoun) %>% summarise(
  targetlook.mean = mean(targetlook),
  targetlook.se = se(targetlook)
)
pd$upper <- pd$targetlook.mean + pd$targetlook.se
pd$lower <- pd$targetlook.mean - pd$targetlook.se
colnames(pd)[1:4] <- c("age","condition","time","targetlook")
pd$condition <- ifelse(pd$condition=="critical", "Predictive Sentences", "Neutral Sentences")
ggplot(data=pd, aes(x=time, y=targetlook)) +
  facet_wrap(~age, nrow=1) +
  # display permutation analysis results with shading
  geom_rect(data=pd[pd$age=="Adults",],aes(xmin=-600, xmax=800, ymin=0, ymax=1), fill="grey", alpha=0.01) +
  geom_rect(data=pd[pd$age=="Children",],aes(xmin=-300, xmax=1000, ymin=0, ymax=1), fill="grey", alpha=0.01) +
  scale_color_manual(values=c("#000000", "#0099FF")) +
  scale_fill_manual(values=c("#000000", "#0099FF")) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=condition), alpha = .5) +
  geom_line(aes(color=condition), linewidth = 1) +
  scale_x_continuous(limits=c(-1000,1000), breaks=seq(-1000,1000,1000), name="Time from Noun Onset (ms)") +
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,.5), name="Proportion of Target Looks") +
  #geom_vline(aes(xintercept=0-mean(exp1audio$time.between)), linetype='dashed', alpha=1) + # annotate prediction onset
  theme_bw(base_family = "Times", base_size=12) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_rect(linewidth = 1, color='black'),
        legend.position = 'bottom',
        legend.title = element_blank()
        )