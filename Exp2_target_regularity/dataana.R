library(R.matlab)
library(tidyverse)
library(ez)
source('functions.R') 
library(data.table)
library(RColorBrewer)
library(ggpubr)
library(ggplot2)
library(cowplot)

datafiles <- c('A_01_LCC_dyntargetnodist_2021-01-02_20h06.19.770.csv',
               'A_02_JM_dyntargetnodist_2021-01-31_14h02.37.375.csv',
               'A_03_MS_dyntargetnodist_2021-01-06_21h05.28.758.csv',
               'A_04_CS_dyntargetnodist_2021-01-17_17h17.58.178.csv',
               'A_05_CB_dyntargetnodist_2021-01-26_10h27.38.148.csv',
               'A_06_MUA_dyntargetnodist_2021-02-07_08h59.41.367.csv',
               'A_07_MI_dyntargetnodist_2021-01-26_15h53.40.164.csv',
               'A_08_AW_dyntargetnodist_2021-02-04_12h04.23.778.csv',
               'A_09_KM_dyntargetnodist_2021-02-07_11h35.10.119.csv',
               'A_10_YFL_dyntargetnodist_2021-02-07_21h22.07.549.csv',
               'A_11_FT_dyntargetnodist_2021-02-08_17h59.17.461.csv',
               'A_12_MT_dyntargetnodist_2021-01-26_21h02.33.645.csv',
               'A_13_SS_dyntargetnodist_2021-01-28_09h21.03.221.csv',
               'A_14_AB_dyntargetnodist_2021-02-04_21h07.40.924.csv',
               'A_15_MVAT_dyntargetnodist_2021-02-02_16h25.04.598.csv',
               'A_16_JF_dyntargetnodist_2021-02-04_13h33.28.757.csv', 
               'A_17_IS_dyntargetnodist_2021-03-27_13h39.06.922.csv',
               'A_18_FE_dyntargetnodist_2021-03-05_17h19.50.264.csv',
               'A_19_LF_dyntargetnodist_2021-03-09_16h21.35.475.csv',
               'A_20_AW_dyntargetnodist_2021-03-27_14h40.21.017.csv',
               'A_21_LEO_dyntargetnodist_2021-03-06_07h47.06.159.csv',
               'A_22_LK_dyntargetnodist_2021-03-07_14h33.34.460.csv',
               'A_23_MMB_dyntargetnodist_2021-03-07_09h02.02.765.csv',
               'A_24_HHJ_dyntargetnodist_2021-03-08_10h47.02.933.csv')

data <- do.call('rbind',lapply(datafiles, readData))
qdata <- filter(data, is.na(trials.thisTrialN)) # Questionnaire

### Summary of questionnaire responses

# Proportion who said they noticed a pattern
prop_noticed <- mean(qdata$key_pq1.keys=='y' | qdata$key_pq1.keys=='Pattern')

seqno <- seq(1,24)
dyndirections <- as.numeric((seqno+1)%%4 < 2)*2 - 1
qdata <- mutate(qdata, q1 = ifelse(qdata$key_pq1.keys=='y' | qdata$key_pq1.keys=='Pattern', 
                                        "Pattern", "Random"), 
                correct_response = ifelse(dyndirections[group] > 0, 2, 3), 
       correct = ifelse(key_pq2.keys==correct_response, 1, 0),
       awareness=ifelse(q1=="Pattern" & correct==1, "Aware", "Unaware"))

data <- full_join(data, qdata %>% select(awareness, participant)) 
ndata <- filter(data, !is.na(trials.thisTrialN)) # Data expect for questionnaire
aware_subs <- unique(filter(qdata, awareness == "Aware")$participant)

# Proportion of correct identification of pattern
prop_correct <- mean(qdata$correct) 

# Proportion that correctly guessed that target was rotating (but not necessarily the correct 
# direction of rotation)
prop_corr_type <- mean(qdata$key_pq2.keys %in% c(2,3)) 

############## Mean RTs##################
sdata <- group_by(ndata, tar_pos_type, participant) %>% filter(key_resp.corr==1, 
                                                               key_resp.rt<2.5, 
                                                               key_resp.rt>0.2) %>%
  summarize(rt=mean(key_resp.rt)*1000)


ssdata <- sdata %>% summarize(mrt=mean(rt), sert=sd(rt)/sqrt(n()))

sdata_awareness <- group_by(ndata, tar_pos_type, awareness, participant) %>% 
  filter(key_resp.corr==1, key_resp.rt<2.5,  key_resp.rt>0.2, tar_pos_type!="Random") %>%
  summarize(rt=mean(key_resp.rt)*1000)

sdata_awareness %>% ezANOVA(dv=rt, wid=participant, within=tar_pos_type, between=awareness)

Rtime<-ggplot(ssdata, aes(x=tar_pos_type, y=mrt,fill= tar_pos_type)) + 
  geom_bar(stat="identity") + 
  scale_fill_grey(start = 0.4,end = 0.8)+
  geom_errorbar(aes(ymin=mrt-sert, ymax=mrt+sert), width=0.2) + theme_classic() + 
  labs(x="Target position", y="Mean response time (ms)") + 
  coord_cartesian(ylim=c(700,1100))+
  theme(legend.position = "none")

Rtime<-Rtime+scale_x_discrete(labels=c("Random"="Random","Infreq. neighbour"="Infrequent","Freq. neighbour"= "Frequent"))
Rtime

ggsave('./Rtime.png', Rtime, width = 4, height = 3.5)


######### violin plot with dot plot##########

sidata <- group_by(ndata, tar_pos_type, awareness,participant) %>% filter(!outlier,key_resp.corr==1) %>% summarize(rt=mean(key_resp.rt)*1000) %>%
  spread(tar_pos_type,rt) %>% mutate( pc = `Infreq. neighbour` - `Freq. neighbour`)

sidata1 <- sidata %>% group_by(awareness) %>% 
  ggplot(aes(x=awareness,y=pc))+
  geom_violin()+
  geom_point(data= sidata,aes(y=pc))+
  theme(legend.position = "none")+
  theme_classic()+
  labs(x="Awareness", y="Mean probability cueing effect (ms)")

ggsave('./sidata1.png', sidata1, width = 4, height = 3)
sidata1




# Mean error rates 
edata <- group_by(ndata, tar_pos_type, participant) %>%
  summarize(err=1-mean(key_resp.corr))
sedata <- edata  %>% summarize(merr=mean(err), se_err=sd(err)/sqrt(n()))

Errorrate<-ggplot(sedata, aes(x=tar_pos_type, y=merr,fill= tar_pos_type)) + 
  geom_bar(stat="identity") +
  scale_fill_grey(start = 0.4,end = 0.8)+
  geom_errorbar(aes(ymin=merr-se_err, ymax=merr+se_err), width=0.2) + theme_classic() + 
  labs(x="Target position", y="Error rate")+
  theme(legend.position = "none")
#change the name of the labels
Errorrate<-Errorrate+scale_x_discrete(labels=c("Random"="Random","Infreq. neighbour"="Infrequent","Freq. neighbour"= "Frequent")) +
  theme(legend.position = "None")

ggsave('./Errorrate.png', Errorrate, width = 4, height = 3.5)


####Mean outlier#####
ndata %>% group_by(participant)%>%summarize(mean(outlier))
mean(ndata$outlier)
######Mean incorrect####
mean(edata$err)





# Test for difference between frequent and infrequent direction
t.test(filter(sdata, tar_pos_type=="Freq. neighbour")$rt, 
       filter(sdata, tar_pos_type=="Infreq. neighbour")$rt, paired=T)
ezANOVA(sdata,dv=rt,wid = participant,within = .(tar_pos_type))
ezANOVA(edata,dv=err,wid = participant,within = .(tar_pos_type))

# Test for difference between frequent and infrequent direction
t.test(filter(sdata, tar_pos_type=="Freq. neighbour")$rt, 
       filter(sdata, tar_pos_type=="Infreq. neighbour")$rt, paired=T)

t.test(filter(edata, tar_pos_type=="Freq. neighbour")$err, 
       filter(edata, tar_pos_type=="Infreq. neighbour")$err, paired=T)

t.test(filter(edata, tar_pos_type=="Freq. neighbour")$err, 
       filter(edata, tar_pos_type=="Random")$err, paired=T)

t.test(filter(edata, tar_pos_type=="Infreq. neighbour")$err, 
       filter(edata, tar_pos_type=="Random")$err, paired=T)

#23 02 2021 t-Test
t.test(filter(sdata, tar_pos_type=="Freq. neighbour")$rt, 
       filter(sdata, tar_pos_type=="Random")$rt, paired=T)
t.test(filter(sdata, tar_pos_type=="Infreq. neighbour")$rt, 
       filter(sdata, tar_pos_type=="Random")$rt, paired=T)

# ---- Target position inter-trial transitions ----

dists <- c(1,2,3, 4,-3,-2,-1,0,1,2,3,4,-3,-2,-1)
tar_pos_inttrial <- data %>% mutate(dyn_dir=dyndirections[group], 
                t_t_dist = dists[dyn_dir*(tar_pos - lag(tar_pos))+8]) %>%
  filter(key_resp.corr==1, key_resp.rt<2.5, key_resp.rt>0.2, trials.thisTrialN %% 84 > 0) %>% 
  group_by(t_t_dist, tar_pos_type, participant) %>% 
  summarize(rt=mean(key_resp.rt)*1000)
pj <- position_dodge(width = 0.4)
tar_pos_inttrial %>% summarize(mRT=mean(rt), se_rt = sd(rt)/sqrt(n()-1)) %>% 
  ggplot(aes(x=t_t_dist, shape=tar_pos_type, y=mRT, group=1)) + 
  geom_point(position=pj, size=3) + geom_line(position=pj) + theme_classic() + 
  geom_errorbar(aes(ymin=mRT-se_rt, ymax=mRT+se_rt), width=0.4, position=pj) +
  labs(x="Target position inter-trial change", y="Mean response time (ms)")

#############################short time learning  inter trial change five 0，1，2，3，4########################################

dists <- c(1,2,3, 4,-3,-2,-1,0,1,2,3,4,-3,-2,-1)
tar_pos_inttrial3 <- data %>% mutate(dyn_dir=dyndirections[group], 
                                     t_t_dist = dists[dyn_dir*(tar_pos - lag(tar_pos))+8])%>%
  filter(key_resp.corr==1, key_resp.rt<2.5, key_resp.rt>0.2, trials.thisTrialN %% 84 > 0) %>% 
  mutate(t_t_dist = abs(t_t_dist)) %>%
  group_by(t_t_dist, participant) %>% 
  summarize(rt=mean(key_resp.rt)*1000)
pj <- position_dodge(width = 0.4)


tar_pos_inttrial_plot3<- tar_pos_inttrial3 %>% group_by(t_t_dist)%>% summarize(mRT=mean(rt), se_rt = sd(rt)/sqrt(n()-1)) %>% 
  ggplot(aes(t_t_dist, y=mRT, group=1)) + 
  geom_point(position=pj, size=4) + geom_line(position=pj) + 
  theme_classic() + 
  geom_errorbar(aes(ymin=mRT-se_rt, ymax=mRT+se_rt), width=0.4, position=pj) +
  labs(x="Target position inter-trial change", y="Mean response time (ms)")+
  geom_point(data=filter(ssdata, tar_pos_type!="Random"),aes(x=1, shape=tar_pos_type, color=tar_pos_type, y=mrt), size=4) +
  geom_errorbar(data=filter(ssdata, tar_pos_type!="Random"), aes(x=1, y=mrt,color=tar_pos_type,ymin=mrt-sert, ymax=mrt+sert),width=0.4)+
  theme(legend.position = "top")+
  theme(legend.title=element_blank())


tar_pos_inttrial_plot3<- tar_pos_inttrial_plot3+ scale_shape_manual(values = c(15,17),breaks=c("Infreq. neighbour","Freq. neighbour"),labels=c("Infrequent","Frequent"))+
  scale_color_discrete(breaks=c("Infreq. neighbour","Freq. neighbour"),labels=c("Infrequent","Frequent"))



#change the name of the labels#
#tar_pos_inttrial_plot2<- tar_pos_inttrial_plot2 + scale_x_discrete(labels=c("Infrequent","Repeat","Frequent", "Random"))

tar_pos_inttrial_plot3<- tar_pos_inttrial_plot3 + scale_x_continuous(breaks=c(0,1,2,3,4),labels=c("0","1","2", "3","4"))

tar_pos_inttrial_plot3

ggsave('./tar_pos_inttrial_plot3.png', tar_pos_inttrial_plot3, width = 4, height = 3.5)



tart_pos_inttrial4 <- tar_pos_inttrial3 %>% ungroup()%>%  spread(t_t_dist, rt) 



# new combine three plots
fig_exp1 = plot_grid (Rtime,Errorrate,tar_pos_inttrial_plot3, nrow = 1, labels = c('a','b','c'))
fig_exp1

ggsave('./fig_exp1.png', fig_exp1, width = 12, height = 3.5)






