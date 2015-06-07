## course project for exploratory data
## Alex Bergin
## created: 6/7/2015

##functions:
## plot[N] - plots the [N]th graph
## getstarted(N) - Downloads file, does data cleanup

## THE n=1 argument of getstarted will produce the different plots (1-4)


getstarted <- function(n = 4)
{

  ## set the URL & Filename
  URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  filename <- "data.zip"
  
  ## file download/unzip
  if(!file.exists(filename))
  {
    download.file(URL, filename)
    ## unzip the data
    unzip(filename)
  }
  files <- unzip(filename, list = TRUE)
  
  ## read data into table
  data <- read.csv2(files$Name, stringsAsFactors = FALSE)
  
  
  ## create data2 using dplyr package
  library(dplyr)
  data2 <- tbl_df(data)
  rm(data)
  
  ## filter data for timeperiod 
  ## into data3
  x <- mutate(data2, BetterDate = as.Date(Date, format = "%d/%m/%Y"))
  date1 <- as.Date("2007-02-01", format = "%Y-%m-%d")
  date2 <- as.Date("2007-02-02", format = "%Y-%m-%d")
  
  data3 <- filter(x, 
                  BetterDate >= date1,
                  BetterDate <= date2)
  rm(data2)
  #Plot using functions below:
  
  makeplot(n, data3)

}

makeplot <- function(n, data3)
{
  if(n == 1)
  {
    outfile <- "plot1.png"
    png(outfile, width = 480, height = 480)
    plot1(data3)
  }
  if(n == 2)
  {
    outfile <- "plot2.png"
    png(outfile, width = 480, height = 480)
    plot2(data3)
  }
  if(n == 3)
  {
    outfile <- "plot3.png"
    png(outfile, width = 480, height = 480)
    plot3(data3)
  }
  if(n == 4)
  {
    outfile <- "plot4.png"
    png(outfile, width = 480, height = 480)
    plot4(data3)
  }
  
  dev.off()
  
}
  

plot1 <- function(data3)
{
  ## global active power; convert from factor to numeric
  x <- select(data3, Global_active_power)
  x2 <- as.numeric(x[[1]])
  rm(x)
  
  hist(x2, col = "red", 
       xlab = "Global Active Power (kilowatts)", 
       ylab = "Frequency",
       main = "Global Active Power")
  
}

plot2 <- function(data3)
{
  ## global active power; convert from factor to numeric
  
  x <- select(data3, Global_active_power)
  x2 <- as.numeric(x[[1]])
  
  
  
  ## plot using base plot
  plot(x2 ,
       xlab = "", 
       ylab = "Global Active Power (kilowatts)",
       type = "l",
       xaxt = 'n')
  axis(side = 1,
       at = c(1, length(x2) / 2, length(x2)),
       labels = c("Thu", "Fri","Sat"))
  
}

plot3 <- function(data3)
{
  ## Energy Sub Metering
  
  x <- select(data3, Sub_metering_1,Sub_metering_2,Sub_metering_3)
  x2 <- mutate(x, 
         Sub_metering_1 <- as.numeric(Sub_metering_1),
         Sub_metering_2 <- as.numeric(Sub_metering_2),
         Sub_metering_3 <- as.numeric(Sub_metering_3))
  
  ## plot using base plot
  plot(x2[[1]] ,
       xlab = "", 
       ylab = "Energy sub metering",
       type = "l",
       xaxt = 'n',
       col = "black")
  lines(x2[[2]] ,
       col = "red")
  lines(x2[[3]] ,
        col = "blue")
  axis(side = 1,
       at = c(1, length(x2[[1]]) / 2, length(x2[[1]])),
       labels = c("Thu", "Fri","Sat"))
  legend("topright",
         legend = c("Sub_Metering_1","Sub_Metering_2","Sub_Metering_3"),
         lty = c(1,1),
         lwd = c(1.5,1.5),
         col = c("black", "red", "blue"))
}

subplot4a <- function(data3)
  {
    ## global active power; convert from factor to numeric
    
    x <- select(data3, Voltage)
    x2 <- as.numeric(x[[1]])
    
    
    
    ## plot using base plot
    plot(x2 ,
         xlab = "datetime", 
         ylab = "Voltage",
         type = "l",
         xaxt = 'n')
    axis(side = 1,
         at = c(1, length(x2) / 2, length(x2)),
         labels = c("Thu", "Fri","Sat"))
    
  }
subplot4b <- function(data3)
  {
    ## global active power; convert from factor to numeric
    
    x <- select(data3, Global_reactive_power)
    x2 <- as.numeric(x[[1]])
    
    
    
    ## plot using base plot
    plot(x2 ,
         xlab = "datetime", 
         ylab = "Global_reactive_power",
         type = "l",
         xaxt = 'n')
    axis(side = 1,
         at = c(1, length(x2) / 2, length(x2)),
         labels = c("Thu", "Fri","Sat"))
    
  }
plot4 <- function(data3)
{
  par(mfrow = c(2,2))
  plot2(data3)
  subplot4a(data3)
  plot3(data3)
  subplot4b(data3)  
}
