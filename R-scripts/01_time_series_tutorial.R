


library(tidyverse)
library(cowplot)
library(lubridate)
theme_set(theme_cowplot())

temps <- read_csv("data-raw/daily_temps_2000.csv") %>% 
	filter(lat == -74.875, lon == 164.625)

mspect <- spectrum(temps$sst, spans=c(2,2), plot=FALSE)
delta <- 1/365
specx <- mspect$freq/delta
specy <- 2*mspect$spec
# plot(log(specx), log(mspect$spec), xlab="Period (years)", ylab="Spectral Density", type="l")

spectral <- data.frame(specx = mspect$freq, specy = specy)

spectral %>% 
ggplot(aes(x = specx, y = specy)) + geom_line() +
	scale_y_log10() + scale_x_log10() + geom_smooth(method = "lm")

lm(log(specy) ~ log(specx), data = spectral) %>% summary()


oisst <- read_csv("data-raw/OISST_data.csv")

place1 <- oisst %>% 
	filter(isolate.code == 358)

oisst %>% 
	mutate(month = month(time)) %>% 
	mutate(year = year(time)) %>% 
	mutate(day = day(time)) %>%
	mutate(year_day = yday(time)) %>%
	# mutate(month_day = mday(time)) %>% View
	# unite(month_day, month, day, sep = "-", remove = FALSE) %>% 
	# mutate(month_day = mday(month_day)) %>% View
	group_by(year, isolate.code) %>% 
	top_n(n = 1, wt = sst) %>% 
	ggplot(aes(x = year, y =year_day, color = abs(lat), group = isolate.code)) +
	# geom_point() +
	ylab("Julian date of yearly max temperature") +
	xlab("Year") +
	# geom_line() +
	scale_color_viridis_c() + geom_smooth(method = "lm", se = FALSE)

ggsave("figures/max-temp-julian-date-all-lats.png", width = 8, height = 6)
	

place2_sst <- runif(n = 10411, min = min(place1$sst), max = max(place1$sst))
place2 <- data.frame(time = place1$time, sst = place2_sst)

place1_sub <- place1 %>% 
	mutate(month = month(time)) %>% 
	mutate(year = year(time)) %>% 
	mutate(day = day(time)) %>% 
	filter(year != "2002") %>% 
	filter(year == 1986) 
1/365

### correlogram
sst_acf <- acf(place1$sst, lag.max = 365)
plot(sst_acf)

correlation <- data.frame(correlation = sst_acf$acf, lag = sst_acf$lag)

correlation %>% 
	ggplot(aes(x = lag, y = correlation)) + geom_line() +
	geom_hline(yintercept = 0) + ylab("Autocorrelation") + xlab("Lag (days)")
ggsave("figures/sst-correlogram.pdf", width = 6, height = 3)

mspect <- spectrum(place1$sst, spans=c(2,2), plot=FALSE)



delta <- 1
specx <- mspect$freq/delta
specy <- 2*mspect$spec
plot(log(specx), log(mspect$spec), xlab="Period (years)", ylab="Spectral Density", type="l")

spectral <- data.frame(specx = mspect$freq/delta, specy = specy)


spectral %>% 
	ggplot(aes(x = specx, y = specy)) + geom_line() +
	scale_y_log10(breaks = c(1,10, 100)) +
	scale_x_log10(breaks = c(0.001,0.01, 0.1, 0.5)) +
	geom_vline(xintercept = 1/365, color = "cadetblue", size = 1) +
	geom_vline(xintercept = 1/182.5, color = "green", size = 1) +
	geom_vline(xintercept = 1/30, color = "turquoise", size = 1) +
	geom_vline(xintercept = 1/(365*10), color = "purple", size = 1) +
	geom_vline(xintercept = 1/7, color = "pink", size = 1) +
	geom_vline(xintercept = 1/(2), color = "orange", size = 1) +
	geom_line() +
	geom_smooth(method = "lm", se = FALSE, color = "grey") +
	ylab("Spectral density") +
	xlab("Frequency (1/days)")
ggsave("figures/sst-spectral-slope-normalred-noise.png", width = 6, height = 3)
ggsave("figures/sst-spectral-slope-normalred-noise.pdf", width = 5.6, height = 2.9)


library(broom)
lm(log(specy) ~ log(specx), data = spectral) %>% tidy(., conf.int = TRUE)

x <- seq(0,365,length.out=1000)
y <- 9263*sin(x*(2*pi/(365)))
df <- data.frame(x = x, y = y)

x2 <- seq(0,365,length.out=1000)
y2 <- 2000*sin(x2*(2*pi/20))
df2 <- data.frame(x = x2, y = y2)

x3 <- seq(0,365,length.out=1000)
y3 <- 100*sin(x3*(2*pi/7))
df3 <- data.frame(x = x3, y = y3)

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
p + geom_line(aes(x = x, y = y), data = df, color = "cadetblue",size = 1) +
	geom_line(aes(x = x, y = y), data = df2, color = "turquoise", size = 1) +
	geom_line(aes(x = x, y = y), data = df3, color = "pink", size = 1) +
	xlim(0, 365) + ylab("Temperature (°C)") + xlab("Time (days)") +
	theme(
		axis.text.y = element_blank(),
		axis.ticks.y = element_blank()) +
	theme(text = element_text(size=14),
		  axis.text.x = element_text(size = 14),
		  axis.text.y = element_blank())

ggsave("figures/sine-waves.png", width = 6, height = 3)
ggsave("figures/sine-waves.pdf", width = 5.7, height = 3)

df2 <- data.frame(x = x, y = 100*y)




	geom_line(aes(x = x, y = y), data = df2)




place1 %>% 
	ggplot(aes(x = time, y = sst)) + geom_line() +
	ylab("Temperature (°C)") + xlab("Date") +
	theme(text = element_text(size=14),
		  axis.text.x = element_text(size = 14),
		  axis.text.y = element_text(size = 14))
ggsave("figures/temp-time-series.png", width = 6, height = 3)
ggsave("figures/temp-time-series.pdf", width = 6, height = 3)

place2 %>% 
	ggplot(aes(x = time, y = sst)) + geom_line() +
	ylab("Temperature (°C)") + xlab("Date")

library(lubridate)

place1 %>% 
	mutate(month = month(time)) %>% 
	mutate(year = year(time)) %>% 
	mutate(day = day(time)) %>% 
	filter(year != "2002") %>% 
	filter(year == 1986) %>% 
	ggplot(aes(x = time, y = sst)) + geom_line() +
	geom_point() +
	facet_wrap( ~ year, scales = "free") +
	ylab("Temperature (°C)") + xlab("Date")
