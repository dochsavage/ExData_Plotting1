#
# Student:    Jeff Murdoch
# Date:       2020-10-07
# Course:     Exploratory Graphs
# Assignment: Week #1 Course Project
#
# Summary:  This project analyzes the electric power consumption in one
#           household with a 1-minute sampling rate over a period of 2-days.
#           The original dataset contains almost 4-years of data.
#
###############################################################################
loadData <- function(directory = "exdata_data_household_power_consumption",
                     filename = "household_power_consumption.txt",
                     dateList = c("1/2/2007","2/2/2007"),
                     debug = FALSE) 
{
  if( debug ) print("loadData: Begin.")
  # Libraries
  library(readr)
  library(dplyr)
  
  # Constants
  if( debug ) print(paste("loadData: directory=",directory))
  dirData <- paste0(directory,"/")
  fileData <- paste0(dirData,filename)
  
  # Load Data Files
  #  Note: separators are semicolons.
  #  Note: The character "?" indicates missing data.
  if( debug ) print(paste("loadData: fileData=",fileData))
  dt <- read.table(file = fileData, sep = ";", na.strings = "?", header = TRUE)
  if( debug ) print(paste("loadData: dates=",dateList))
  #
  # Filter the list for the rows corresponding to the date vector parameter.
  #
  df_filtered <- dt[dt[,1] %in% dateList,]
  #
  # Add new column "timestamp" as a Date column.
  #
  df_filtered <- mutate(df_filtered, timestamp = strptime(paste(df_filtered$Date,df_filtered$Time), "%d/%m/%Y %H:%M:%S"))
  if( debug ) print("loadData: End.")
  df_filtered
}

#
# Constructs and writes plot to PNG file.
#
plot3 <- function(data, filename = "plot3.png")
{
  png(file = filename)
  plot(data$timestamp, data$Sub_metering_1,type = "l",
                                               ylab = "Energy Sub Metering",
                                               xlab = "",
                                               col = "green")
  points(data$timestamp, data$Sub_metering_2,type = "l", col = "red")
  points(data$timestamp, data$Sub_metering_3,type = "l", col = "purple")
  legend("topright",lty = 1, col = c("green","red","purple"), legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  dev.off()
}

data <- loadData(debug = TRUE)
plot3(data)