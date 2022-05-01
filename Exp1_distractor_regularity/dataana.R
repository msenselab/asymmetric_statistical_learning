library(R.matlab)
library(tidyverse)
library(ez)
library(data.table)
library(RColorBrewer)
library(ggpubr)
library(ggplot2)
library(cowplot)

source('functions.R')

subfiles<-c("01_NS.mat", "02_MT.mat", "03_LB.mat", "04_KR.mat", "05_LA.mat", "06_AS.mat", "07_AL.mat",
            "08_NK.mat", "09_SY.mat", "10_BK.mat", "11_PP.mat", "12_MG.mat", "13_PC.mat", "14_XZ.mat",
            "15_QFFD.mat", "16_CS.mat", "17_YYL.mat", "18_GV.mat", "19_HB.mat",  "20_JM.mat",  "21_NS.mat", 
            "22_GV.mat", "23_SZ.mat", "24_EG.mat")

N <- 24
seqno <- seq(1,N)
dyndirections <- as.numeric((seqno+1)%%4 < 2)*2 - 1

raw <- data.table(do.call('rbind',lapply(subfiles, readData)))

raw <- within(raw,{
  dist_pos_cond = factor(dist_pos_cond, levels=c(0,1,2,3), labels=c("Absent", "Random",'Infreq. neighbour','Freq. neighbour'))
  q1 = factor(q1, levels=c(1,2), labels=c("Pattern", "Random"))
})

raw <- mutate(raw, 
              correct_q2_response = ifelse(dyndirections[group] > 0, 2, 3), 
              correct_q2 = ifelse(q2==correct_q2_response, 1, 0), 
              awareness=ifelse(q1=="Pattern" & correct_q2==1, "Aware", "Unaware"))

# Mean RTs 
sdata <- group_by(raw, dist_pos_cond,  sub) %>% filter(!outlier, correct) %>% summarize(rt=mean(rt))
ssdata <- sdata %>% summarize(mrt=mean(rt)*1000, sert=sd(rt)*1000/sqrt(n()-1))

Rtime1<-ggplot(ssdata, aes(x=dist_pos_cond, y=mrt,fill= dist_pos_cond)) + 
  geom_bar(stat="identity") + 
  scale_fill_grey(start = 0,end = 0.8)+
  geom_errorbar(aes(ymin=mrt-sert, ymax=mrt+sert), width=0.2) + theme_classic() + 
  labs(x="Distractor position", y="Mean response time (ms)")+
  coord_cartesian(ylim=c(700,1200))+
  theme(legend.position = "none") 

#change the name of the labels
Rtime1<-Rtime1+ scale_x_discrete(labels=c("Absent" = "Absent","Random"="Random","Infreq. neighbour"="Infrequent","Freq. neighbour"= "Frequent")) 
Rtime1

#SAVE PLOT FOR RT
ggsave('./Rtime1.png', Rtime1, width = 4, height = 3.5)

ezANOVA(filter(sdata, dist_pos_cond!="Absent"), dv=rt, wid=sub, within=dist_pos_cond)

t.test(filter(sdata, dist_pos_cond=="Freq. neighbour")$rt, 
       filter(sdata, dist_pos_cond=="Infreq. neighbour")$rt, paired=T)


# Mean error rates 
edata <- group_by(raw, dist_pos_cond, sub) %>%
  summarize(err=1-mean(correct))
sedata <- edata  %>% summarize(merr=mean(err), se_err=sd(err)/sqrt(n()-1))

Error1<-ggplot(sedata, aes(x=dist_pos_cond, y=merr,fill=dist_pos_cond)) + 
  geom_bar(stat="identity") + 
  scale_fill_grey(start = 0,end = 0.8)+
  geom_errorbar(aes(ymin=merr-se_err, ymax=merr+se_err), width=0.2) + theme_classic() + 
  labs(x="Distractor position", y="Error rate")

#change the name of the labels
Error1<-Error1+scale_x_discrete(labels=c("Absent" = "Absent","Random"="Random","Infreq. neighbour"="Infrequent","Freq. neighbour"= "Frequent")) +
  theme(legend.position = "None")

#SAVE PLOT FOR Error rate
ggsave('./Error1.png', Error1, width = 4, height = 3.5)

ezANOVA(filter(edata, dist_pos_cond!="Absent"), dv=err, wid=sub, within=dist_pos_cond)


##inverse efficiency score##
ies_data <- full_join(edata, sdata, by = c("sub", "dist_pos_cond")) %>% mutate(ies = rt/(1-err)) 

sies_data <- ies_data %>% summarize(mies=mean(ies), se_ies=sd(ies)/sqrt(n()-1))

ggplot(sies_data, aes(x=dist_pos_cond, y=mies)) + geom_bar(stat="identity", fill="darkgreen", color="black") + 
  geom_errorbar(aes(ymin=mies-se_ies, ymax=mies+se_ies), width=0.2) + theme_classic() + 
  labs(x="Distractor position", y="Inverse efficiency score") 


ezANOVA(filter(ies_data, dist_pos_cond!="Absent"), dv=err, wid=sub, within=dist_pos_cond)


#Mean outlier#
raw %>% group_by(sub)%>%summarize(mean(outlier))
mean(raw$outlier)
#Mean incorrect#
mean(raw$correct)
mean(sdata$rt)

#################### Target location effect2#################
tar_pos_data <- raw %>% group_by(tar_pos_cond, sub) %>%
  filter(correct==1, rt<2.5, rt>0.2, dist_pos_cond == "Random", lag(dist_pos_cond != "Absent")) %>%
  summarize(rt=mean(rt)*1000)
tar_pos_data$tar_pos_cond <- factor(tar_pos_data$tar_pos_cond, levels=1:3,
                                    labels=c("Random dist. loc.", "Infreq. dist. loc.",
                                             "Freq. dist. loc."))
s_tar_pos_data <- tar_pos_data %>% summarize(mrt=mean(rt), se_rt=sd(rt)/sqrt(n()-1))
s_tar_pos_data %>% ggplot(aes(x=tar_pos_cond, y=mrt, group=1)) + geom_bar(stat="identity") +
  theme_classic() + geom_errorbar(aes(ymin=mrt-se_rt, ymax=mrt+se_rt), width=0.2) +
  labs(x="Target position", y = "Mean response time (ms)") + coord_cartesian(ylim=c(700,1100))
tar_pos_data %>% ezANOVA(dv=rt, wid=participant, within=tar_pos_type)



#############################inter-trial learning five 0，1，2，3，4 ########################################

dists <- c(-1, 1,2,3, 4,-3,-2,-1,0,1,2,3,4,-3,-2,-1)
dist_pos_inttrial2 <- raw %>% mutate(d_d_dist = dists[direction*(dist_pos - lag(dist_pos))+9])%>%
  filter(correct==1, !outlier, tno%% 60 > 1, dist_pos_cond!="Absent",  lag(dist_pos_cond)!="Absent") %>%  
  mutate(d_d_dist = abs(d_d_dist)) %>%
  group_by(d_d_dist, sub) %>% 
  summarize(rt=mean(rt)*1000) 
pj <- position_dodge(width = 0.4)

dist_pos_inttrial_plot3<- dist_pos_inttrial2 %>% 
  group_by(d_d_dist)%>% summarize(mRT=mean(rt), se_rt = sd(rt)/sqrt(n()-1)) %>% 
  ggplot(aes(d_d_dist, y=mRT, group=1)) + 
  geom_point(position=pj, size=4) + 
  geom_line(position=pj) + 
  theme_classic() + 
  geom_errorbar(aes(ymin=mRT-se_rt, ymax=mRT+se_rt), width=0.4, position=pj) +
  labs(x="Distractor position inter-trial change", y="Mean response time (ms)")+
  geom_point(data=filter(ssdata, dist_pos_cond!="Random",dist_pos_cond!="Absent"),aes(x=1, shape=dist_pos_cond, color=dist_pos_cond, y=mrt), size=4) +
  geom_errorbar(data=filter(ssdata, dist_pos_cond!="Random",dist_pos_cond!="Absent"), aes(x=1, y=mrt,color=dist_pos_cond,ymin=mrt-sert, ymax=mrt+sert),width=0.4)+
  theme(legend.position = "top")+
  theme(legend.title=element_blank())
#theme(legend.position = "none") #ggplot with no legend
#scale_shape_discrete(name= "dist_pos_cond",labels = c("Random","Infrequent","Frequent"))  #Edit legend title and text labels

dist_pos_inttrial_plot3 <-dist_pos_inttrial_plot3 + scale_shape_manual(values = c(15,17),breaks=c("Infreq. neighbour","Freq. neighbour"),labels=c("Infrequent","Frequent"))+
  scale_color_discrete(breaks=c("Infreq. neighbour","Freq. neighbour"),labels=c("Infrequent","Frequent"))

dist_pos_inttrial_plot3<-  dist_pos_inttrial_plot3 + scale_x_continuous(breaks=c(0,1,2,3,4),labels=c("0","1","2", "3","4"))

dist_pos_inttrial_plot3

ggsave('./dist_pos_inttrial_plot3.png', dist_pos_inttrial_plot3, width = 4, height = 3.5)


dist_pos_inttrial3 <- dist_pos_inttrial2 %>% ungroup()%>%  spread(d_d_dist, rt) 


# new combine three plots
fig_exp1 = plot_grid (Rtime1, Error1,dist_pos_inttrial_plot3,nrow = 1, labels = c('a','b','c'))
fig_exp1

ggsave('./fig_exp1.png', fig_exp1, width = 12, height = 3.5)


########## Distractor position inter-trial transitions###########

dists <- c(-1, 1,2,3, 4,-3,-2,-1,0,1,2,3,4,-3,-2,-1)
dist_pos_inttrial <- raw %>% mutate(d_d_dist = dists[direction*(dist_pos - lag(dist_pos))+9]) %>%
  filter(correct==1, !outlier, tno %% 60 > 1, dist_pos_cond!="Absent", 
         lag(dist_pos_cond)!="Absent") %>% 
  group_by(d_d_dist, dist_pos_cond, sub) %>% summarize(rt=mean(rt)*1000)
dist_pos_inttrial %>% summarize(mRT=mean(rt), se_rt=sd(rt)/sqrt(n()-1)) %>%
  ggplot(aes(x=d_d_dist, shape=dist_pos_cond, y=mRT, group=1)) + geom_point(size=3) + geom_line() +
  geom_errorbar(aes(ymin=mRT-se_rt, ymax=mRT+se_rt), width=0.2) + theme_classic() + 
  labs(x="Distractor position inter-trial change", y="Mean response time (ms)")

#########Target position inter-trial transitions###########

dists <- c(1,2,3, 4,-3,-2,-1,0,1,2,3,4,-3,-2,-1)
tar_pos_inttrial <- raw %>% mutate(dyn_dir=dyndirections[group], 
                                   t_t_dist = dists[dyn_dir*(tar_pos - lag(tar_pos))+8]) %>%
  filter(correct==1, !outlier, tno %% 60 > 1) %>% 
  group_by(t_t_dist, sub) %>% summarize(rt=mean(rt)*1000)
tar_pos_inttrial %>% summarize(mRT=mean(rt), se_rt=sd(rt)/sqrt(n()-1)) %>% ggplot(aes(x=t_t_dist, y=mRT)) + 
  geom_point() + geom_errorbar(aes(ymin=mRT-se_rt, ymax=mRT+se_rt), width=0.2) +
  geom_line() + theme_classic() + labs(x="Target position inter-trial change", y="Mean response time (ms)")



