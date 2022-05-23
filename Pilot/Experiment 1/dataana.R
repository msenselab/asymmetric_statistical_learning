source('funs.R')
library(BayesFactor)

subfiles<-c("S3.mat", "S4.mat", "S5.mat", "S6.mat", "S7.mat", "S8.mat", "S9.mat", "S10.mat",
            "S11.mat", "S12.mat", "S13.mat", "S14.mat", "S15.mat", "S16.mat")

raw <- data.table(do.call('rbind',lapply(subfiles, readData)))
raw <- mutate(raw, dist_cond=if_else(dist_pos==0, "Absent", if_else(dist_pos==freq_pos, "Freq", "Rare")),
              tar_cond=if_else(tar_pos==freq_pos, "Freq", "Rare"))
raw$dist_cond <- factor(raw$dist_cond, levels=c("Absent", "Rare", "Freq"))
raw$tar_cond <- factor(raw$tar_cond, levels=c("Rare", "Freq"))

distance <- c(0, 1, 2, 3, 4, 3, 2, 1)
raw <- mutate(raw, d_dist = if_else(dist_cond!="Absent", distance[abs(dist_pos-freq_pos)+1], -1)) 
raw <- mutate(raw, t_dist = distance[abs(tar_pos-freq_pos)+1]) 

raw$d_dist <- factor(raw$d_dist)
raw$t_dist <- factor(raw$t_dist)


N <- length(unique(raw$sub))


# ---- Target position inter-trial transitions ----

summary_rt_data <- group_by(raw, sub, blkNo) %>% filter(correct, !outlier) %>% 
  group_by(dist_cond, sub) %>% summarize(rt=mean(rt)*1000) %>% 
  summarize(mRT=mean(rt), seRT=sd(rt)/sqrt(N-1)) 

levels(summary_rt_data$dist_cond) <- c("Absent", "Random", "Frequent")

write_rds(summary_rt_data, "E1_rt_data.rds")

E1_rt_plot <- summary_rt_data %>% 
  ggplot(aes(x=dist_cond, fill=dist_cond, y=mRT)) + geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mRT-seRT, ymax=mRT+seRT),width=0.2) + 
  theme_bw() + labs(x="Distractor position", y="Mean response time (ms)") + 
  scale_fill_grey(start = 0.4,end = 0.8) +  coord_cartesian(ylim=c(900,1300)) + 
  theme(legend.position = "None")

summary_err_data <- group_by(raw, sub, blkNo) %>%
  group_by(dist_cond, sub) %>% summarize(err=1-mean(correct)) %>% 
  summarize(merr=mean(err), se_err=sd(err)/sqrt(N-1)) 

levels(summary_err_data$dist_cond) <- c("Absent", "Random", "Frequent")

write_rds(summary_err_data, "E1_err_data.rds")

E1_err_plot <-  summary_err_data %>% 
  ggplot(aes(x=dist_cond, fill=dist_cond, y=merr)) + geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=merr-se_err, ymax=merr+se_err),width=0.2) + 
  theme_bw() + labs(x="Distractor position", y="Error rate") + 
  scale_fill_grey(start = 0.4,end = 0.8) + theme(legend.position = "None")

