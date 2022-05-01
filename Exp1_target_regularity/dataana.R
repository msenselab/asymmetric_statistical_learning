library(R.matlab)
library(tidyverse)
library(ez)
library(data.table)
source('functions.R')
library( RColorBrewer)
library(cowplot)

subfiles<-c("pilot exp SY.mat", "02_PP.mat", "03_LB.mat", "04_MJ.mat", "05_TS.mat", "06_YC.mat", "07_MI.mat", "08_JP.mat",
            "09_NI.mat", "10_WNH.mat", "11_AA.mat", "12_BC.mat", "13_MM.mat", "14_PP.mat",
            "15_ZJP.mat", "16_BEC.mat", "17_KR.mat", "18_XZ.mat", "19_AS.mat", "20_WP.mat", "21_YZ.mat",
            "22_CS.mat", "23_NK.mat", "24_JK.mat")

N <- 24
seqno <- seq(1,N)
dyndirections <- as.numeric((seqno+1)%%4 < 2)*2 - 1

raw <- data.table(do.call('rbind',lapply(subfiles, readData)))

raw <- within(raw,{
  tar_pos_cond = factor(tar_pos_cond, levels=c(0,1,2), labels=c("Random",'Infreq. neighbour','Freq. neighbour'))
  q1 = factor(q1, levels=c(1,2), labels=c("Pattern", "Random"))
})

raw <- mutate(raw, 
              correct_q2_response = ifelse(dyndirections[group] > 0, 2, 3), 
              correct_q2 = ifelse(q2==correct_q2_response, 1, 0), 
              awareness=ifelse(q1=="Pattern" & correct_q2==1, "Aware", "Unaware"))

# Mean RTs
sdata <- group_by(raw, tar_pos_cond, sub) %>% filter(!outlier, correct) %>% summarize(rt=mean(rt)*1000)
ssdata <- sdata %>% summarize(mrt=mean(rt)*1000, sert=sd(rt)*1000/sqrt(n()-1))

Rtime<-ggplot(ssdata, aes(x=tar_pos_cond, y=mrt,fill= tar_pos_cond)) + 
  geom_bar(stat="identity") + 
  scale_fill_grey(start = 0.4,end = 0.8)+
  geom_errorbar(aes(ymin=mrt-sert, ymax=mrt+sert), width=0.2) + theme_classic() + 
  labs(x="Target position", y="Mean response time (ms)") + 
  coord_cartesian(ylim=c(700,1300)) +
  theme(legend.position = "none") 

##change the name of the labels##
Rtime<-Rtime+scale_x_discrete(labels=c("Random"="Random","Infreq. neighbour"="Infrequent","Freq. neighbour"= "Frequent"))
Rtime

#SAVE PLOT
ggsave('./Rtime.png', Rtime, width = 4, height = 3.5)

ezANOVA(sdata, dv=rt, wid=sub, within=tar_pos_cond)



# Mean ERs
edata <- group_by(raw, tar_pos_cond, sub) %>%
  summarize(err=1-mean(correct))
sedata <- edata  %>% summarize(merr=mean(err), se_err=sd(err)/sqrt(n()-1))

Erate<-ggplot(sedata, aes(x=tar_pos_cond, y=merr,fill= tar_pos_cond)) + 
  geom_bar(stat="identity") + 
  scale_fill_grey(start = 0.4,end = 0.8)+
  geom_errorbar(aes(ymin=merr-se_err, ymax=merr+se_err), width=0.2) + 
  theme_classic() + 
  labs(x="Target position", y="Error rate") 

#change the name of the labels
Erate<-Erate+scale_x_discrete(labels=c("Random"="Random","Infreq. neighbour"="Infrequent","Freq. neighbour"= "Frequent"))+
  theme(legend.position = "None")

#SAVE PLOT FOR ER
ggsave('./Erate.png', Erate, width = 4, height = 3.5)

ezANOVA(edata, dv=err, wid=sub, within=tar_pos_cond)


###########Mean outlier###############
raw %>% group_by(sub)%>%summarize(mean(outlier))
mean(raw$outlier)
##############Mean incorrect################
mean(raw$correct)
mean(sdata$rt)


dists <- c(1,2,3, 4,-3,-2,-1,0,1,2,3,4,-3,-2,-1)
tar_pos_inttrial <- raw %>% mutate(t_t_dist = dists[direction*(tar_pos - lag(tar_pos))+8]) %>%
  filter(correct==1, !outlier, tno %% 60 > 1) %>% 
  group_by(t_t_dist, tar_pos_cond, sub) %>% summarize(rt=mean(rt)*1000)
tar_pos_inttrial %>% summarize(mRT=mean(rt), se_rt=sd(rt)/sqrt(n()-1)) %>%
  ggplot(aes(x=t_t_dist, shape=tar_pos_cond, y=mRT, group=1)) + geom_point(size=3) + geom_line() +
  geom_errorbar(aes(ymin=mRT-se_rt, ymax=mRT+se_rt), width=0.2) + theme_classic() + 
  labs(x="Target position inter-trial change", y="Mean response time (ms)")


#############################inter-trial learning five 0，1，2，3，4########################################

dists <- c(1,2,3, 4,-3,-2,-1,0,1,2,3,4,-3,-2,-1)
tar_pos_inttrial2 <- raw %>% mutate(dyn_dir=dyndirections[group], 
                                    t_t_dist = dists[dyn_dir*(tar_pos - lag(tar_pos))+8])%>%
  filter(correct==1, !outlier, tno %% 60 > 1) %>%  
  mutate(t_t_dist = abs(t_t_dist)) %>%
  group_by(t_t_dist, sub) %>% 
  summarize(rt=mean(rt)*1000)
#pj <- position_dodge(width = 0.4)

tar_pos_inttrial_plot3<- tar_pos_inttrial2 %>% group_by(t_t_dist)%>% summarize(mRT=mean(rt), se_rt = sd(rt)/sqrt(n()-1)) %>% 
  ggplot(aes(t_t_dist, y=mRT, group=1)) + 
  geom_point(position=pj, size=3) + geom_line(position=pj) + theme_classic() + 
  geom_errorbar(aes(ymin=mRT-se_rt, ymax=mRT+se_rt), width=0.4, position=pj) +
  labs(x="Target position inter-trial change", y="Mean response time (ms)")+
  scale_shape_discrete(name= "tar_pos_cond",labels = c("Random","Infrequent","Frequent"))+    #Edit legend title and text labels
  geom_point(data=filter(ssdata, tar_pos_cond!="Random"),aes(x=1, shape=tar_pos_cond, color=tar_pos_cond, y=mrt), size=4) +
  geom_errorbar(data=filter(ssdata, tar_pos_cond!="Random"), aes(x=1, y=mrt,color=tar_pos_cond,ymin=mrt-sert, ymax=mrt+sert),width=0.4)+
  theme(legend.position = "top")+
  theme(legend.title=element_blank())                                   #theme(legend.position = "none") #ggplot with no legend

tar_pos_inttrial_plot3<- tar_pos_inttrial_plot3+ scale_shape_manual(values = c(15,17),breaks=c("Infreq. neighbour","Freq. neighbour"),labels=c("Infrequent","Frequent"))+
  scale_color_discrete(breaks=c("Infreq. neighbour","Freq. neighbour"),labels=c("Infrequent","Frequent"))



tar_pos_inttrial_plot4<- tar_pos_inttrial_plot3 + scale_x_continuous(breaks=c(0,1,2,3,4),labels=c("0","1","2", "3","4"))

tar_pos_inttrial_plot4


ggsave('./tar_pos_inttrial_plot4.png', tar_pos_inttrial_plot4, width = 4, height = 3.5)

tart_pos_inttrial3 <- tar_pos_inttrial2 %>% ungroup()%>%  spread(t_t_dist, rt)


# new combine three plots
fig_exp1 = plot_grid (Rtime, Erate,tar_pos_inttrial_plot4,nrow = 1, labels = c('a','b','c'))
fig_exp1

ggsave('./fig_exp1.png', fig_exp1, width = 12, height = 3.5)



