# --------------------------------------------------------------------
# EXPLANATORY PLOTS
# TASK  01: DRAW GRAPHS FOR GUMBELL FRECHET AND WEIBULL
# TASK  02: GRAPHS OF INDIVIDUAL NODES
# TASK  03: PLOT THE ETS RESIDUALS OF THE NODES
# --------------------------------------------------------------------


# --------------------------------------------------------------------
# TASK  01: DRAW GRAPHS FOR GUMBELL FRECHET AND WEIBULL
# --------------------------------------------------------------------
#  https://dk81.github.io/dkmathstats_site/rvisual-cont-prob-dists.html
library(ggplot2)
library(evd)
library(gridExtra)

# R Doc. Template for Weibull: dweibull(x, shape, scale = 1, log = FALSE)

x_lower_wei <- -5
x_upper_wei <- 5

# Excluded ylimits this time.

g1 <- ggplot(data.frame(x = c(x_lower_wei , x_upper_wei)), aes(x = x)) +
  xlim(c(x_lower_wei , x_upper_wei)) +
  stat_function(fun = dgev, args = list(shape = -0.5, scale =1),  aes(colour = "1")) +
  stat_function(fun = dgev, args = list(shape = -0.8, scale = 2),  aes(colour = "2")) +
  stat_function(fun = dgev, args = list(shape = -1, scale = 1),  aes(colour = "3")) +
  scale_color_manual("Shape & Scale \n Parameters", values = c("blue", "darkgreen", "red")) +
  labs(x = "\n x", y = "f(x) \n",
       title = "Weibull") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        legend.position = "none")

g1

g2 <- ggplot(data.frame(x = c(x_lower_pareto, x_upper_pareto)), aes(x = x)) +
  xlim(c(x_lower_pareto, x_upper_pareto)) +
  stat_function(fun = dgev, args = list(shape = 1, scale = 3), aes(colour = "1")) +
  stat_function(fun = dgev, args = list(shape = 3, scale = 2),  aes(colour = "2")) +
  stat_function(fun = dgev, args = list(shape = 5, scale = 1), aes(colour = "3")) +
  scale_color_manual("Shape and scale values", values = c("darkgreen", "blue", "red")) +
  labs(x = "x", y = "",
       title = "Frechet") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        legend.position = "none")



g2

g3 <- ggplot(data.frame(x = c(x_lower_pareto, x_upper_pareto)), aes(x = x)) +
  xlim(c(x_lower_pareto, x_upper_pareto)) +
  stat_function(fun = dgev, args = list(shape = 0, scale = 1), aes(colour = "1")) +
  stat_function(fun = dgev, args = list(shape = 0, scale = 2),  aes(colour = "2")) +
  stat_function(fun = dgev, args = list(shape = 0, scale = 3), aes(colour = "3")) +
  scale_color_manual("Shape and scale values", values = c("darkgreen", "blue", "red")) +
  labs(x = "x", y = "",
       title = "Gumbel") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        legend.position = "none")


grid.arrange(g1, g2, g3, nrow = 1)

# x <- seq(-10, 10, by=0.1)
# Gumbel_density <- exp(-x-exp(-x))
# Frechet_density <- dgev(x, xi=0.8, mu=0)
# Weibull_density <- dgev(x, xi=-0.3, mu=0)
#
# plot(c(x,x,x), c(Gumbel_density,Frechet_density, Weibull_density),
#      type='n', xlab="x", ylab=" ",las=1)
# lines(x, Gumbel_density, type='l', lty=1, col='green')
# lines(x, Weibull_density, type='l', lty=2, col='blue')
# lines(x, Frechet_density, type='l', lty=3, col='red')
# legend('topright', legend=c('Gumbel','Weibull','Frechet'), lty=c(1,2,3), col=c('green','blue','red'))


# --------------------------------------------------------------------
# TASK  02: GRAPHS OF INDIVIDUAL NODES
# --------------------------------------------------------------------

library(tidyverse)
library(ggplot2)


dat2 <- read_csv('Data_Output/To_Make_Public/dat_2019.csv')
dat3 <- dat2 %>% mutate(day = ceiling( (time - st_time)/(60*60*24) ) )
nodes <- unique(dat3$node)

nodeset <- c(2,3,4,5,151,211,301,307)
nodeids <- nodes[nodeset]
datnode <- dat3 %>%
  filter(node %in% nodeids) %>%
  group_by(day, node) %>%
  summarize(count = n())

ggplot(datnode, aes(day, count)) +
  geom_point() +
  geom_line() +
  facet_wrap(~node, scales = 'free', nrow = 2) +
  xlab("Day") +
  ylab('Daily Aggregated ARP Calls')


# --------------------------------------------------------------------
# TASK  3 : PLOT THE ETS RESIDUALS OF THE NODES
# --------------------------------------------------------------------


library(plyr)
library(ggplot2)
library(tidyverse)

library(hts)
library(fable)
library(fpp3)
library(lookout)


# ETS MODEL
fore_res <- readRDS("Data_Output/Mahdi/fore_res.rds")

nodeset <- c(2, 5, 211, 301)

fore_res %>%
  filter(.model == 'ets_bu' & nodes == 151 ) %>%
  mutate(forecast = ifelse(fore_res < 0, 0, fore_res)) %>%
  select(time, forecast) %>%
  autoplot()


fore_res2 <- fore_res %>%
  filter(.model == 'ets_bu' & nodes == 2) %>%
  mutate(forecast = ifelse(fore_res < 0, 0, fore_res))
fore_res3 <- fore_res %>%
  filter(.model == 'ets_bu' & nodes == 5) %>%
  mutate(forecast = ifelse(fore_res < 0, 0, fore_res))
fore_res4 <- fore_res %>%
  filter(.model == 'ets_bu' & nodes == 211) %>%
  mutate(forecast = ifelse(fore_res < 0, 0, fore_res))
fore_res5 <- fore_res %>%
  filter(.model == 'ets_bu' & nodes == 301) %>%
  mutate(forecast = ifelse(fore_res < 0, 0, fore_res))

fore_res_4nodes <- bind_rows(fore_res2, fore_res3, fore_res4, fore_res5)
fore_res_4nodes <- fore_res_4nodes %>%
  mutate(node = paste('N',str_pad(fore_res_4nodes$nodes, width=3, pad="0"), sep=""))

ggplot(fore_res_4nodes, aes(time, forecast)) +
  geom_path() +
  facet_wrap(~node, scales = "free", nrow = 1) +
  xlab('Time (hours)') +
  ylab('Hourly Residuals')





# ------------------------------
# TO CONSIDER LATER

# ETS MODEL
fore_res_ets <- readRDS("Data_Output/Mahdi/fore_res.rds")
head(fore_res_ets)
unique(fore_res_ets$nodes)


tslm_res <- readRDS("Data_Output/Mahdi/fore_tslm_res.RDS")
hist(tslm_res$fore_res)

week_hours <- data.frame(hour = (24*7)*10:49 )

tslm_res_gg <- tslm_res %>%
  filter(.model == 'tslm_bu') %>%
  filter(abs(fore_res) > 10) %>%
  mutate(model = "TSLM") %>%
  select(time, model, fore_res)

fore_res_gg <- fore_res_ets %>%
  filter(.model == 'ets_bu') %>%
  filter(abs(fore_res) > 10) %>%
  mutate(model = 'ETS') %>%
  select(time, model, fore_res)

fore_gg <-  bind_rows(as_tibble(fore_res_gg), as_tibble(tslm_res_gg))
fore_gg %>% filter(time==1514)

ggplot(fore_res_gg, aes(time, fore_res)) +
  geom_line() +
  geom_vline(xintercept = week_hours$hour, linetype = "dotted" ) +
  xlab('Hours') +
  ylab('ETS Residuals')

ggplot(fore_gg, aes(time, fore_res)) +
  geom_line() +
  facet_wrap(~model) +
  geom_vline(xintercept = week_hours$hour, linetype = "dotted" ) +
  xlab('Hours') +
  ylab('Residuals')

