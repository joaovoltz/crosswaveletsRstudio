# Load the WaveletComp library
library(Wavecomp)

# Data to be analyzed
data <- read.csv("C:/Users/joão/Desktop/micro/crosswavelet/csv_crosswavelet_final.csv", header = TRUE)

# In this case, I had separate columns for DATE and TIME
datetime <- as.POSIXct(paste(data$DATE, data$TIME), format = "%Y-%m-%d %H:%M:%S")

# Defining the date range to be analyzed for October to December 2015
start_date <- as.POSIXct("2015-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
end_date <- as.POSIXct("2015-12-31 23:59:59", format = "%Y-%m-%d %H:%M:%S")

# Creating a dataframe with formatted data within the desired date range
my.data <- data.frame(datetime = datetime, x = data$NEE_uStar_f, y = data$Tair_f)

# Filtering the dataframe to include only dates within the specified range
my.data <- subset(my.data, datetime >= start_date & datetime <= end_date)

# Checking if there are data in the desired range
if (nrow(my.data) > 0) {
  # Perform coherency analysis on the filtered data
  my.wc <- analyze.coherency(my.data, my.pair = c("x", "y"),
                             loess.span = 0,
                             dt = 1/12, dj = 1/250,
                             lowerPeriod = 1/2,
                             upperPeriod = 32,
                             make.pval = TRUE, n.sim = 10)
  
  # Plot of coherence
  wc.image(my.wc, n.levels = 250,
           legend.params = list(lab = "Coherence Spectrum"),
           show.date = TRUE, date.format = "%Y-%m-%d",
           timelab = "Year", periodlab = "Period")
  
  # Plot of wavelet spectrum of x
  wt.image(my.wc, my.series = "x",
           legend.params = list(lab = "Wavelet Spectrum of x"),
           show.date = TRUE, date.format = "%Y-%m-%d",
           timelab = "Year", periodlab = "Period")
  
  # Average Coherence
  wc.avg(my.wc, sigpch = 20)
  
} else {
  # If there are no data within the specified date range (October to December 2015)
  print("There are no data within the specified date range (October to December 2015).")
}
