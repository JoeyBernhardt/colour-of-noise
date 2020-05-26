

#### How to plot a spectral density plot -- for Maggie

library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())



#### this is a sea surface temperature dataset from NOAA that has a bunch of time series for different locations around the world
### isolate.code is just an identifier for each location
### time is the date
### lat is latitude
### lon is longitude


### Read in the data
oisst <- read_csv("data-raw/OISST_data.csv") 


### filter out one location - I think this is somewhere near Norway? (you can play around with this)
place1 <- oisst %>% 
	filter(isolate.code == 358)

### plot the time series
place1 %>% 
	ggplot(aes(x = time, y = sst)) + geom_line() +
	ylab("Temperature (°C)") + xlab("Date") +
	theme(text = element_text(size=14),
		  axis.text.x = element_text(size = 14),
		  axis.text.y = element_text(size = 14))
# ggsave("figures/temp-time-series.png", width = 6, height = 3)


### plot the correlogram
sst_acf <- acf(place1$sst, lag.max = 365)

correlation <- data.frame(correlation = sst_acf$acf, lag = sst_acf$lag)

correlation %>% 
	ggplot(aes(x = lag, y = correlation)) + geom_line() +
	geom_hline(yintercept = 0) + ylab("Autocorrelation") + xlab("Lag (days)")
ggsave("figures/sst-correlogram.pdf", width = 6, height = 3)


### estimate the spectral density
mspect <- spectrum(place1$sst, spans=c(2,2), plot=FALSE)

delta <- 1
specx <- mspect$freq/delta
specy <- 2*mspect$spec

spectral <- data.frame(specx = mspect$freq/delta, specy = specy)

#### plot
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
# ggsave("figures/sst-spectral-slope-normalred-noise.png", width = 6, height = 3)


### mess around and plot some of the component sine waves
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

# ggsave("figures/sine-waves.png", width = 6, height = 3)