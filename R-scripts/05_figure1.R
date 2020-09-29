

library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())

s <- read_csv("data-raw/time-series-cartoon.csv")

s2 <- s %>% 
	mutate(response_x = x + 1)
s %>% 
	ggplot(aes(x = x, y = y)) + geom_line()

s3 <- read_csv("data-raw/time-series-cartoon-2.csv")

x3 <- seq(12,120,length.out=1000) -5 

y3 <- .15*sin(x3*(2*pi/32)) + 0.4

df3 <- data.frame(x = x3, y = y3) %>% 
	mutate(y3 = ifelse(x3 > 103, 0.25, y3)) %>% 
	mutate(y3 = ifelse(x3 <7, 0.25, y3))

plotb <- s3 %>% 
	ggplot(aes(x = x, y = y2)) + geom_line(size = 1, color = "darkgrey") +
	# # geom_line(aes(x = x, y = y3), color = "cadetblue", size = 1) +
	# # geom_line(aes(x = x, y = y4), color = "pink", size = 1) +
	# geom_line(aes(x = x, y = y5), color = "orange", size = 1) +
	geom_line(aes(x = x3, y = y3), color = "black", size = 1, data = df3) + xlim(0, 120) +
	theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
	theme(
		  axis.text=element_blank(),
		  axis.ticks=element_blank()) + xlab("Time") + ylab("") +
	ggtitle("B) Filtering \n (e.g. integration)") +
	theme(plot.title = element_text(size = 12))

plota <- s3 %>% 
	ggplot(aes(x = x, y = y2)) + geom_line(size = 1, color = "darkgrey") +
	# # geom_line(aes(x = x, y = y3), color = "cadetblue", size = 1) +
	# # geom_line(aes(x = x, y = y4), color = "pink", size = 1) +
	# geom_line(aes(x = x, y = y5), color = "orange", size = 1) +
	# geom_line(aes(x = x3, y = y3), color = "lightblue", size = 1, data = df3) + xlim(0, 120) +
	theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
	theme(
		  axis.text=element_blank(),
		  axis.ticks=element_blank()) + xlab("Time") + ylab("") +
	ggtitle("A) Environmental variable \n (e.g. temperature)") +
	theme(plot.title = element_text(size = 12))

plotc <- s3 %>% 
	ggplot(aes(x = x, y = y2)) + geom_line(size = 1, color = "darkgrey") +
	# # geom_line(aes(x = x, y = y3), color = "cadetblue", size = 1) +
	geom_line(aes(x = x, y = y4), color = "black", size = 1) +
	# geom_line(aes(x = x, y = y5), color = "black", size = 1) +
	# geom_line(aes(x = x3, y = y3), color = "lightblue", size = 1, data = df3) + xlim(0, 120) +
	theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
	theme(
		  axis.text=element_blank(),
		  axis.ticks=element_blank()) + xlab("Time") + ylab("") +
	ggtitle("C) Feedback with some delay") +
	theme(plot.title = element_text(size = 12))
	

plotd <- s3 %>% 
	ggplot(aes(x = x, y = y2)) +
	geom_rect(aes(xmin = 95, xmax = 120, ymin = 0.25, ymax = 1),
			  fill = "lightblue", alpha = 0.05) +
	geom_line(size = 1, color = "darkgrey") +
	# geom_line(aes(x = x, y = y3), color = "cadetblue", size = 1) +
	# geom_line(aes(x = x, y = y4), color = "black", size = 1) +
	geom_line(aes(x = x, y = y5), color = "black", size = 1) +
	geom_point(aes(x = x, y = y6 + 0.3), shape = 8, size = 3) +
	# geom_line(aes(x = x3, y = y3), color = "lightblue", size = 1, data = df3) + xlim(0, 120) +
	theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
	theme(
		  axis.text=element_blank(),
		  axis.ticks=element_blank()) + xlab("Time") + ylab("") +
	ggtitle("D) Feedforward from cue (*) \n (e.g. photoperiod)") +
	theme(plot.title = element_text(size = 12))

library(patchwork)

plota / (plotb | plotc | plotd)
plota + plotb + plotc + plotd
ggsave("figures/figure1-time-series-filtering.png", width = 6, height = 4)
