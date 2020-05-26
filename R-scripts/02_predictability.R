

### predictability

library(tidyverse)
library(cowplot)

theme_set(theme_cowplot())


xes <- runif(n = 10, min = 0, max = 50)
yes <- xes + runif(n = 10, min = 0, max = 1)


df <- data.frame(x = xes, y = yes) %>% 
	arrange(desc(-x)) %>% 
	mutate(lag_x = x - lag(x)) %>% 
	mutate(lag_y = y - lag(y)) %>%
	mutate(lag_x_y = y - x)

df %>% 
	filter(lag_y > 0) %>% 
	ggplot(aes(x = lag_y)) + geom_histogram(binwidth = 10) 


df %>% 
	filter(lag_y > 0) %>% 
	ggplot(aes(x = lag_x)) + geom_histogram(binwidth = 10) 

df %>% 
	filter(lag_y > 0) %>% 
	ggplot(aes(x = lag_x_y)) + geom_histogram(binwidth = 0.01) 

df %>% 
	ggplot(aes(x = xes, y = 0)) + geom_point(shape = 124, size = 10) +
	geom_point(aes(x = yes, y = 0), data = df, color = "turquoise", shape = 124, size = 10) +
	ylim(0, 0)


### let's try this again



xes <- runif(n = 12, min = 0, max = 100)
yes <- xes + 2
