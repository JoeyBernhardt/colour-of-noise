### mod tag plot


library(tidyverse)
library(cowplot)

theme_set(theme_cowplot())

events <- read_csv("data-raw/xy-tag.csv") %>% 
	gather(key = variable, value = time) %>% 
	group_by(variable) %>% 
	mutate(lag = time - lag(time)) %>% 
	mutate(event = 0)

events_wide <- read_csv("data-raw/xy-tag.csv") %>% 
	mutate(lag = x -y) %>% 
	mutate(variable = "x,y")

events_wide %>% 
	ggplot(aes(x = y, y = x)) + geom_point(size =3) + geom_abline(intercept = 0, slope = 1)
ggsave("figures/events-xy-corr.pdf", width = 6, height = 4)

events %>% 
	filter(variable == "x") %>% 
	ggplot(aes(x = time, y = event, color = variable)) + geom_point(shape = 124, size = 10) +
	ylim(0, 0) +
	theme(
		axis.text.x = element_blank(),
		axis.text.y = element_blank(),
		axis.ticks = element_blank()) +
	scale_color_manual(values = c("turquoise")) +
	theme(legend.position = "none") + 
	xlim(0, 120)
ggsave("figures/events-x.pdf", width = 6, height = 4)

events %>% 
	filter(variable == "y") %>% 
	ggplot(aes(x = time, y = event, color = variable)) + geom_point(shape = 124, size = 10) +
	ylim(0, 0) +
	theme(
		axis.text.x = element_blank(),
		axis.text.y = element_blank(),
		axis.ticks = element_blank()) +
	scale_color_manual(values = c("darkgrey")) +
	theme(legend.position = "none") + 
	xlim(0, 120)
ggsave("figures/events-y.pdf", width = 6, height = 4)

events %>% 
	filter(variable %in% c("x", "y")) %>% 
	ggplot(aes(x = time, y = event, color = variable)) + geom_point(shape = 124, size = 10) +
	ylim(0, 0) +
	theme(
		axis.text.x = element_blank(),
		axis.text.y = element_blank(),
		axis.ticks = element_blank()) +
	scale_color_manual(values = c("turquoise", "darkgrey")) +
	# scale_color_manual(values = c("darkgrey")) +
	theme(legend.position = "none") + 
	xlim(0, 120)
ggsave("figures/events-xy.pdf", width = 6, height = 4)

events_all <- bind_rows(events, events_wide)


events_all %>% 
	ungroup() %>% 
	# mutate(variable = factor(variable)) %>% 
	mutate(variable = factor(variable, levels = c("x", "y", "x,y"))) %>%
	select(-event) %>% 
	rename(event = variable) %>% 
	ggplot(aes(x = lag, color = event, fill = event)) + geom_density() +
	facet_wrap( ~ event, nrow = 3, ncol = 1) +
	scale_fill_manual(values = c("turquoise", "darkgrey", "black")) +
	scale_color_manual(values = c("turquoise", "darkgrey", "black")) +
	ylab("Frequency") + xlab("Time lag between events") + 
	theme(legend.position = "none") +
	theme(
		strip.background = element_blank(),
		strip.text.x = element_blank()
	)
ggsave("figures/prob-xy.pdf", width = 2.8, height = 3.5)

	

