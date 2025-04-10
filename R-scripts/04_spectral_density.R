

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


place1b <- place1 %>% 
	select(time, sst) %>% 
	rename(temperature = sst)
write_csv(place1b, "data-processed/place1.csv")


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




spectral %>% 
	ggplot(aes(x = specx, y = specy)) + geom_line() +
	# scale_y_log10(breaks = c(1,10, 100)) +
	# scale_x_log10(breaks = c(0.001,0.01, 0.1, 0.5)) +
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



# try a low latitude location ---------------------------------------------

place2 <- oisst %>% 
	mutate(abs_lat = abs(lat)) %>% 
	filter(isolate.code == 365)




### plot the time series
place2 %>% 
	ggplot(aes(x = time, y = sst)) + geom_line() +
	ylab("Temperature (°C)") + xlab("Date") +
	theme(text = element_text(size=14),
		  axis.text.x = element_text(size = 14),
		  axis.text.y = element_text(size = 14))
ggsave("figures/temp-time-series-low-lat.png", width = 6, height = 3)


### plot the correlogram
sst_acf <- acf(place2$sst, lag.max = 365)

correlation <- data.frame(correlation = sst_acf$acf, lag = sst_acf$lag)

correlation %>% 
	ggplot(aes(x = lag, y = correlation)) + geom_line() +
	geom_hline(yintercept = 0) + ylab("Autocorrelation") + xlab("Lag (days)")
ggsave("figures/sst-correlogram.pdf", width = 6, height = 3)


### estimate the spectral density
mspect2 <- spectrum(place2$sst, spans=c(2,2), plot=FALSE)

delta <- 1
specx2 <- mspect2$freq/delta
specy2 <- 2*mspect2$spec

spectral2 <- data.frame(specx = mspect2$freq/delta, specy = specy2)



#### plot
spectral2 %>% 
	mutate(inverse_freq = 1/specx) %>% 
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
ggsave("figures/sst-spectral-slope-tropical-noise.png", width = 6, height = 3)

mod2 <- lm(log(specy) ~ log(specx), data = spectral2)
summary(mod2)


# place 3 -----------------------------------------------------------------
place3 <- oisst %>% 
	mutate(abs_lat = abs(lat)) %>% 
	filter(isolate.code == 363)




### plot the time series
place3 %>% 
	ggplot(aes(x = time, y = sst)) + geom_line() +
	ylab("Temperature (°C)") + xlab("Date") +
	theme(text = element_text(size=14),
		  axis.text.x = element_text(size = 14),
		  axis.text.y = element_text(size = 14))
ggsave("figures/temp-time-series-high-lat.png", width = 6, height = 3)




### estimate the spectral density
mspect3 <- spectrum(place3$sst, spans=c(2,2), plot=FALSE)

delta <- 1
specx3 <- mspect3$freq/delta
specy3 <- 2*mspect3$spec

spectral3 <- data.frame(specx = mspect3$freq/delta, specy = specy3)


mod1 <- lm(log(specy) ~ log(specx), data = spectral3)
summary(mod1)


# place 3 -----------------------------------------------------------------
place4 <- oisst %>% 
	mutate(abs_lat = abs(lat)) %>%
	filter(isolate.code == 570)




### plot the time series
place4 %>% 
	ggplot(aes(x = time, y = sst)) + geom_line() +
	ylab("Temperature (°C)") + xlab("Date") +
	theme(text = element_text(size=14),
		  axis.text.x = element_text(size = 14),
		  axis.text.y = element_text(size = 14))
ggsave("figures/temp-time-series-mid-lat.png", width = 6, height = 3)




### estimate the spectral density
mspect4 <- spectrum(place4$sst, spans=c(2,2), plot=FALSE)

delta <- 1
specx4 <- mspect4$freq/delta
specy4 <- 2*mspect4$spec

spectral4 <- data.frame(specx = mspect4$freq/delta, specy = specy4)


mod4 <- lm(log(specy) ~ log(specx), data = spectral4)
summary(mod4)




#### plot
spectral4 %>% 
	mutate(inverse_freq = 1/specx) %>% 
	mutate(freq_year = specx*365) %>% 
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
ggsave("figures/sst-spectral-slope-mid-lat.png", width = 6, height = 3)




spec3 <- spectral3 %>% 
	mutate(location = "high latitude")


spec2 <- spectral2 %>% 
	mutate(location = "low latitude")

spec4 <- spectral4 %>% 
	mutate(location = "mid latitude")

all_spec <- bind_rows(spec3, spec2, spec4)

all_spec %>% 
	mutate(inverse_freq = 1/specx) %>% 
	mutate(freq_year = specx*365) %>% 
	ggplot(aes(x = specx, y = specy, group = location, color = location)) + geom_line(alpha = 0.7) +
	scale_y_log10(breaks = c(1,10, 100)) +
	scale_x_log10(breaks = c(0.001,0.01, 0.1, 0.5)) +
	# geom_vline(xintercept = 1/365, color = "cadetblue", size = 1) +
	# geom_vline(xintercept = 1/182.5, color = "green", size = 1) +
	# geom_vline(xintercept = 1/30, color = "turquoise", size = 1) +
	# geom_vline(xintercept = 1/(365*10), color = "purple", size = 1) +
	# geom_vline(xintercept = 1/7, color = "pink", size = 1) +
	# geom_vline(xintercept = 1/(2), color = "orange", size = 1) +
	# geom_smooth(method = "lm", se = FALSE) +
	ylab("Spectral density") +
	xlab("Frequency (1/days)")
ggsave("figures/sst-spectral-slope-high-low-lat-mid-no-lm.png", width = 8, height = 6)



# help from chatgpt -------------------------------------------------------

# Load libraries
library(tidyverse)
library(lubridate)
library(zoo)  # for rollmean

# Load and prepare the data
df <- read_csv("data-processed/place1.csv") %>% 
	mutate(time = ymd(time)) %>%
	arrange(time)

# Ensure daily regular time series
df_daily <- df %>%
	complete(time = seq.Date(as.Date(min(time)), as.Date(max(time)), by = "day")) %>% 
	fill(temperature, .direction = "downup")

# Detrend the temperature data
temperature <- df_daily$temperature
temperature_detrended <- temperature - mean(temperature, na.rm = TRUE)

# FFT
n <- length(temperature_detrended)
fft_result <- fft(temperature_detrended)
amplitude <- Mod(fft_result) / n
freq <- (0:(n - 1)) / n
freq_year <- freq * 365.25

# Keep only positive frequencies
half_n <- floor(n / 2)
freq_year <- freq_year[1:half_n]
amplitude <- amplitude[1:half_n]

# Smooth the amplitude spectrum using moving average
window_size <- 15
amplitude_smoothed <- rollmean(amplitude, k = window_size, fill = NA, align = "center")

# Plot original and smoothed spectrum
plot(freq_year, amplitude, type = "l", col = "lightblue", lwd = 1,
	 xlab = "Frequency (cycles per year)", ylab = "Amplitude",
	 main = "Smoothed Frequency Spectrum of Temperature Data")
lines(freq_year, amplitude_smoothed, col = "blue", lwd = 2)
abline(v = 1, col = "red", lty = 2)
abline(v = 2, col = "orange", lty = 2)
legend("topright",
	   legend = c("Original", "Smoothed", "1/year", "2/year"),
	   col = c("lightblue", "blue", "red", "orange"),
	   lty = c(1, 1, 2, 2),
	   lwd = c(1, 2, 1, 1),
	   bty = "n")
grid()



new_df <- data.frame(power = amplitude, frequency = freq_year)


new_df %>% 
	ggplot(aes(x = freq_year, y = power)) + geom_line() + 
	scale_y_log10()






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




# try again ---------------------------------------------------------------

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(signal)

# Load the data
df <- read_csv("data-processed/place1.csv")

# Convert time column to Date format
# df <- df %>%
# 	mutate(time = ymd(time)) %>%
# 	arrange(time)

# Ensure daily regular time series
df_daily <- df %>%
	complete(time = seq.Date(as.Date(min(time)), as.Date(max(time)), by = "day")) %>% 
	fill(temperature, .direction = "downup")

# Detrend the temperature data
temperature <- df_daily$temperature
temperature_detrended <- temperature - mean(temperature, na.rm = TRUE)

# Compute FFT
n <- length(temperature_detrended)
fft_result <- fft(temperature_detrended)
amplitude <- Mod(fft_result) / n
freq <- (0:(n - 1)) / n
freq_year <- freq * 365.25
# freq_year <- freq

# Keep only the positive frequencies
half_n <- floor(n / 2)
freq_year <- freq_year[1:half_n]
amplitude <- amplitude[1:half_n]

# Plot with log scale on y-axis
plot(freq_year, amplitude, type = "l", col = "blue", lwd = 2,
	 xlab = "Frequency (cycles per year)", ylab = "Amplitude (log scale)",
	 main = "Log-Scale Frequency Spectrum of Temperature Data",
	 log = "y")  # <- This sets the y-axis to log scale

abline(v = 1, col = "red", lty = 2)
abline(v = 2, col = "orange", lty = 2)
legend("topright", legend = c("1 cycle/year", "2 cycles/year"),
	   col = c("red", "orange"), lty = 2, bty = "n")
grid()

