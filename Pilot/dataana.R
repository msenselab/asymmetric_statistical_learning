library(tidyverse)
library(cowplot)

E1_rt_data <- readRDS('Experiment 1/E1_rt_data.rds')
E1_err_data <- readRDS('Experiment 1/E1_err_data.rds')

E2_rt_data <- readRDS('Experiment 2/E2_rt_data.rds')
E2_err_data <- readRDS('Experiment 2/E2_err_data.rds')


E1_rt_plot <- E1_rt_data %>% 
  ggplot(aes(x=dist_cond, fill=dist_cond, y=mRT)) + geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mRT-seRT, ymax=mRT+seRT),width=0.2) + 
  theme_bw() + labs(x="Distractor position", y="Mean response time (ms)") + 
  scale_fill_grey(start = 0.4,end = 0.8) +  coord_cartesian(ylim=c(900,1300)) + 
  theme(legend.position = "None")

E1_err_plot <- E1_err_data %>% 
  ggplot(aes(x=dist_cond, fill=dist_cond, y=merr)) + geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=merr-se_err, ymax=merr+se_err),width=0.2) + 
  theme_bw() + labs(x="Distractor position", y="Error rate") + 
  scale_fill_grey(start = 0.4,end = 0.8) + theme(legend.position = "None")


E2_rt_plot <- E2_rt_data %>% 
  ggplot(aes(x=dist_cond, fill=dist_cond, y=mRT)) + geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mRT-seRT, ymax=mRT+seRT),width=0.2) + 
  theme_bw() + labs(x="Distractor position", y="Mean response time (ms)") + 
  scale_fill_grey(start = 0.4,end = 0.8) +  coord_cartesian(ylim=c(700,1100)) + 
  theme(legend.position = "None")

E2_err_plot <- E2_err_data %>% 
  ggplot(aes(x=dist_cond, fill=dist_cond, y=merr)) + geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=merr-se_err, ymax=merr+se_err),width=0.2) + 
  theme_bw() + labs(x="Distractor position", y="Error rate") + 
  scale_fill_grey(start = 0.4,end = 0.8)  + theme(legend.position = "None")

results_figure <- plot_grid(E1_rt_plot, E2_rt_plot, E1_err_plot, E2_err_plot, rel_heights = c(0.65, 0.35), 
          labels=c('a', 'b', 'c', 'd'))

ggsave("Figures/results_figure.png", results_figure, width=7, height=4.8)
