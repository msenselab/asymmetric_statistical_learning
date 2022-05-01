library(R.matlab)
library(tidyverse)
library(ez)
library(data.table)
source('functions.R') 
library( RColorBrewer)
library(ggplot2)
library(cowplot)

datafiles <- c('B_01_AA_dyndist_2021_Jun_15_1638.csv', 'B_02_CS_dyndist_2021_Jun_15_1501.csv', 
               'B_03_LN_dyndist_2021_Jun_14_0909.csv', 'B_04_KR_dyndist_2021_Jun_12_1237.csv',
               'B_05_LC_dyndist_2021_Jun_12_1027.csv', 'B_06_NS_dyndist_2021_Jun_11_1318.csv',
               'B_07_AJ_dyndist_2021_Jun_11_1041.csv', 'B_08_DS_dyndist_2021_Jun_10_1925.csv',
               'B_09_MK_dyndist_2021_Jun_10_1830.csv', 'B_10_RLW_dyndist_2021_Jun_10_1131.csv',
               'B_11_TS_dyndist_2021_Jun_10_1004.csv', 'B_12_ST_dyndist_2021_Jun_09_1618.csv',
               'B_13_CYC_dyndist_2021_Jun_07_1511.csv', 'B_14_SY_dyndist_2021_Jun_04_1419.csv',
               'B_15_CB_dyndist_2021_Jun_04_1303.csv', 'B_16_JP_dyndist_2021_Jun_04_1111.csv',
               'B_17_ZXF_dyndist_2021_Jun_04_1014.csv', 'B_18_AL_dyndist_2021_Jun_04_0907.csv',
               'B_19_ZJP_dyndist_2021_Jun_02_1905.csv', 'B_20_YWZ_dyndist_2021_Jun_02_1510.csv',
               'B_21_KJ_dyndist_2021_Jun_02_1159.csv', 'B_22_AM_dyndist_2021_May_31_1704.csv',
               'B_23_SY_dyndist_2021_May_27_1728.csv', 'B_24_LB_dyndist_2021_May_25_1620_1.csv')

data <- do.call('rbind',lapply(datafiles, readData_e2))

seqno <- seq(1,24)
dyndirections <- as.numeric((seqno+1)%%4 < 2)*2 - 1

q1data <- filter(data, !is.na(key_pq1.keys)) # Questionnaire
q2data <- filter(data, !is.na(key_pq2.keys)) # Questionnaire
qdata <- cbind(q2data %>% select(-key_pq1.keys), q1data %>% 
                 select(key_pq1.keys)) %>%
  mutate( correct_response = ifelse(dyndirections[group] > 0, 2, 3))
qdata <- mutate(qdata, awareness=ifelse(key_pq1.keys=="Pattern" &
                         key_pq2.keys==correct_response, 
                         "Aware", "Unaware"))
ndata <- filter(data, !is.na(trials.thisTrialN)) # Data expect for questionnaire
data <- full_join(data, qdata %>% select(participant, awareness))

### Summary of questionnaire responses

# Proportion who said they noticed a pattern
prop_noticed <- mean(qdata$key_pq1.keys=='Pattern', na.rm=T)

# Proportion of correct identification of pattern
prop_correct <- mean(qdata$correct, na.rm=T) 

# Proportion that correctly guessed that target was rotating (but not necessarily the correct 
# direction of rotation)
prop_corr_type <- mean(qdata$key_pq2.keys %in% c(2,3)) 

# Mean RTs
sdata <- group_by(ndata, dist_pos_type, participant) %>% filter(key_resp.corr==1, 
                                                                key_resp.rt<2.5, 
                                                                key_resp.rt>0.2,
                                                                trials.thisTrialN %% 60 > 0) %>%
  summarize(rt=mean(key_resp.rt)*1000)


ssdata <- sdata %>% summarize(mrt=mean(rt), sert=sd(rt)/sqrt(n()))


### Mean RTs### 
Rtime1<-ggplot(ssdata, aes(x=dist_pos_type, y=mrt,fill= dist_pos_type)) + 
  geom_bar(stat="identity") + 
  scale_fill_grey(start = 0,end = 0.8)+
  geom_errorbar(aes(ymin=mrt-sert, ymax=mrt+sert), width=0.2) + theme_classic() + 
  labs(x="Distractor position", y="Mean response time (ms)") +
  coord_cartesian(ylim=c(700,1100))+
  theme(legend.position = "none") 

Rtime1<-Rtime1+scale_x_discrete(labels=c("Absent" = "Absent","Random"="Random","Infreq. neighbour"="Infrequent","Freq. neighbour"= "Frequent")) 
Rtime1

ggsave('./Rtime1.png', Rtime1, width = 4, height = 3.5)

#### Mean ERs ###
edata <- group_by(ndata, dist_pos_type, participant) %>%
  summarize(err=1-mean(key_resp.corr))
sedata <- edata  %>% summarize(merr=mean(err), se_err=sd(err)/sqrt(n()))

rror1<-ggplot(sedata, aes(x=dist_pos_type, y=merr,fill=dist_pos_type)) + 
  geom_bar(stat="identity") + 
  scale_fill_grey(start = 0,end = 0.8)+
  geom_errorbar(aes(ymin=merr-se_err, ymax=merr+se_err), width=0.2) +
  theme_classic() + 
  labs(x="Distractor position", y="Error rate") +
  theme(legend.position = "none") 

#change the name of the labels
Error1<-Error1+scale_x_discrete(labels=c("Absent" = "Absent","Random"="Random","Infreq. neighbour"="Infrequent","Freq. neighbour"= "Frequent"))+
  theme(legend.position = "None") 
ggsave('./Error1.png', Error1, width = 4, height = 3.5)

#Mean outlier#
ndata %>% group_by(participant)%>%summarize(mean(outlier))
mean(ndata$outlier)
#Mean incorrect#
mean(edata$err)


###################target location effect################

tar_pos_data <- data %>% group_by(tar_pos_type, participant) %>%
  filter(key_resp.corr==1, key_resp.rt<2.5, key_resp.rt>0.2, dist_pos_type == "Random", lag(dist_pos_type != "Absent")) %>%
  summarize(rt=mean(key_resp.rt)*1000)
tar_pos_data$tar_pos_type <- factor(tar_pos_data$tar_pos_type, levels=1:3,
                                    labels=c("Random dist. loc.", "Infreq. dist. loc.",
                                             "Freq. dist. loc."))
s_tar_pos_data <- tar_pos_data %>% summarize(mrt=mean(rt), se_rt=sd(rt)/sqrt(n()-1))
s_tar_pos_data %>% ggplot(aes(x=tar_pos_type, y=mrt, group=1)) + geom_bar(stat="identity") +
  theme_classic() + geom_errorbar(aes(ymin=mrt-se_rt, ymax=mrt+se_rt), width=0.2) +
  labs(x="Target position", y = "Mean response time (ms)") + coord_cartesian(ylim=c(700,1100))
tar_pos_data %>% ezANOVA(dv=rt, wid=participant, within=tar_pos_type)


################ Test for difference between frequent and infrequent direction##########
t.test(filter(sdata, dist_pos_type=="Freq. neighbour")$rt, 
       filter(sdata, dist_pos_type=="Infreq. neighbour")$rt, paired=T)
ezANOVA(sdata,dv=rt,wid = participant,within = .(dist_pos_type))
ezANOVA(edata,dv=err,wid = participant,within = .(dist_pos_type))

# ---- Target position inter-trial transitions ----

dists <- c(1,2,3, 4,-3,-2,-1,0,1,2,3,4,-3,-2,-1)
tar_pos_inttrial <- data %>% mutate(dyn_dir=dyndirections[group], 
                                    t_t_dist = dists[dyn_dir*(tar_pos - lag(tar_pos))+8]) %>%
  filter(key_resp.corr==1, key_resp.rt<2.5, key_resp.rt>0.2, trials.thisTrialN %% 60 > 0) %>% 
  group_by(t_t_dist, participant) %>% summarize(rt=mean(key_resp.rt)*1000)
tar_pos_inttrial %>% summarize(mRT=mean(rt), se_rt=sd(rt)/sqrt(n()-1)) %>% ggplot(aes(x=t_t_dist, y=mRT)) + 
  geom_point() + geom_errorbar(aes(ymin=mRT-se_rt, ymax=mRT+se_rt), width=0.2) +
  geom_line() + theme_classic() + labs(x="Target position inter-trial change", y="Mean response time (ms)")

# ---- Distractor position inter-trial transitions ----

ddists <- c(NA,1,2,3, 4,-3,-2,-1,0,1,2,3,4,-3,-2,-1, NA)
dist_pos_inttrial <- data %>% group_by(participant) %>% mutate(dyn_dir=dyndirections[group], pdp=lag(dist_pos),
                                    d_d_dist = ifelse(dist_pos==-1 | pdp==-1, NA, 
                                                      ddists[dyn_dir*(dist_pos - pdp)+9])) %>%
  filter(key_resp.corr==1, key_resp.rt<2.5, key_resp.rt>0.2, dist_pos!=-1, trials.thisTrialN %% 60 > 0) %>% 
  group_by(d_d_dist, dist_pos_type, participant) %>% summarize(rt=mean(key_resp.rt)*1000)
dist_pos_inttrial %>% summarize(mRT=mean(rt), se_rt=sd(rt)/sqrt(n()-1)) %>%
  ggplot(aes(x=d_d_dist, shape = dist_pos_type, y=mRT, group=1)) +
  geom_point(size=3) + geom_errorbar(aes(ymin=mRT-se_rt, ymax=mRT+se_rt), width=0.2) +
  geom_line() + theme_classic() + labs(x="Dist. position inter-trial change", y="Mean response time (ms)")

# ---- Distractor - target position inter-trial transitions

ddists <- c(NA,1,2,3, 4,-3,-2,-1,0,1,2,3,4,-3,-2,-1, NA)
dist_tar_pos_inttrial <- data %>% group_by(participant) %>% mutate(dyn_dir=dyndirections[group], pdp=lag(dist_pos),
                                                               d_t_dist = ifelse(dist_pos==-1 | pdp==-1, NA, 
                                                                                 ddists[dyn_dir*(tar_pos - pdp)+9])) %>%
  filter(key_resp.corr==1, key_resp.rt<2.5, key_resp.rt>0.2, dist_pos!=-1, trials.thisTrialN %% 60 > 0) %>% 
  group_by(d_t_dist, participant) %>% summarize(rt=mean(key_resp.rt)*1000)
dist_tar_pos_inttrial %>% summarize(mRT=mean(rt), se_rt=sd(rt)/sqrt(n()-1)) %>% ggplot(aes(x=d_t_dist, y=mRT)) + 
  geom_point() + geom_errorbar(aes(ymin=mRT-se_rt, ymax=mRT+se_rt), width=0.2) +
  geom_line() + theme_classic() + labs(x="Dist. - Target  position inter-trial change", y="Mean response time (ms)")

# ---- Target - Distractor  position inter-trial transitions

ddists <- c(NA,1,2,3, 4,-3,-2,-1,0,1,2,3,4,-3,-2,-1, NA)
tar_dist_pos_inttrial <- data %>% group_by(participant) %>% mutate(dyn_dir=dyndirections[group], ptp=lag(tar_pos),
                                                               t_d_dist = ifelse(dist_pos==-1 | ptp==-1, NA, 
                                                                                 ddists[dyn_dir*(dist_pos - ptp)+9])) %>%
  filter(key_resp.corr==1, key_resp.rt<2.5, key_resp.rt>0.2, dist_pos!=-1, trials.thisTrialN %% 60 > 0) %>% 
  group_by(t_d_dist, participant) %>% summarize(rt=mean(key_resp.rt)*1000)
tar_dist_pos_inttrial %>% summarize(mRT=mean(rt), se_rt=sd(rt)/sqrt(n()-1)) %>% ggplot(aes(x=t_d_dist, y=mRT)) + 
  geom_point() + geom_errorbar(aes(ymin=mRT-se_rt, ymax=mRT+se_rt), width=0.2) +
  geom_line() + theme_classic() + labs(x="Target - Dist. position inter-trial change", y="Mean response time (ms)")


#############################  inter trial learning  five 0，1，2，3，4########################################

ddists <- c(NA,1,2,3, 4,-3,-2,-1,0,1,2,3,4,-3,-2,-1,NA)
dist_pos_inttrial2 <- data %>% mutate(dyn_dir=dyndirections[group], pdp=lag(dist_pos),d_d_dist = ddists[dyn_dir*(dist_pos - pdp)+9])%>%
  filter(key_resp.corr==1, key_resp.rt<2.5, key_resp.rt>0.2,dist_pos!=-1, trials.thisTrialN %% 60 > 0)  %>% 
  mutate(d_d_dist = abs(d_d_dist)) %>%
  group_by( d_d_dist, participant) %>% 
  summarize(rt=mean(key_resp.rt)*1000)
pj <- position_dodge(width = 0.4)

dist_pos_inttrial2_plot2 <- dist_pos_inttrial2 %>% group_by(d_d_dist)%>% summarize(mRT=mean(rt), se_rt = sd(rt)/sqrt(n()-1)) %>% 
  ggplot(aes(d_d_dist, y=mRT, group=1)) + 
  geom_point(position=pj, size=4) + geom_line(position=pj) + 
  theme_classic() + 
  geom_errorbar(aes(ymin=mRT-se_rt, ymax=mRT+se_rt), width=0.4, position=pj) +           
  labs(x="Distractor position inter-trial change", y="Mean response time (ms)")+                          #theme(legend.position = "none") #ggplot with no legend+
  scale_shape_discrete(name= "dist_pos_type",labels = c("Absent,Random","Infrequent","Frequent"))+                      #Edit legend title and text labels
  geom_point(data=filter(ssdata, dist_pos_type!="Random",dist_pos_type!="Absent"),aes(x=1, shape=dist_pos_type, color=dist_pos_type, y=mrt), size=4) +
  geom_errorbar(data=filter(ssdata, dist_pos_type!="Random",dist_pos_type!="Absent"), aes(x=1, y=mrt,color=dist_pos_type,ymin=mrt-sert, ymax=mrt+sert),width=0.4)+
  theme(legend.position = "top")+
  theme(legend.title=element_blank())

dist_pos_inttrial2_plot2<- dist_pos_inttrial2_plot2+ scale_shape_manual(values = c(15,17),breaks=c("Infreq. neighbour","Freq. neighbour"),labels=c("Infrequent","Frequent"))+
  scale_color_discrete(breaks=c("Infreq. neighbour","Freq. neighbour"),labels=c("Infrequent","Frequent"))


dist_pos_inttrial2_plot3<- dist_pos_inttrial2_plot2 + scale_x_continuous(breaks=c(0,1,2,3,4),labels=c("0","1","2", "3","4"))
dist_pos_inttrial2_plot3 

ggsave('./dist_pos_inttrial2_plot3.png', dist_pos_inttrial2_plot3, width = 4, height = 3.5)


dis_pos_inttrial3 <- dist_pos_inttrial2 %>% ungroup()%>%  spread( d_d_dist, rt) 



# new combine three plots
fig_exp1 = plot_grid (Rtime1, Error1, dist_pos_inttrial2_plot3, nrow = 1, labels = c('a','b','c'))
fig_exp1

ggsave('./fig_exp1.png', fig_exp1, width = 12, height = 3.5)


