
x <- read.table("household_power_consumption.txt", header=TRUE, 
                sep=";", na.strings = "?", 
                colClasses = c('character','character','numeric','numeric',
                               'numeric','numeric','numeric','numeric','numeric'))

x$Date <- as.Date(x$Date, "%d/%m/%Y")

# Filtered data
x <- subset(x,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
x <- x[complete.cases(x),]

dateTime <- paste(x$Date, x$Time)
dateTime <- setNames(dateTime, "DateTime")

# date and time columns
x <- x[ ,!(names(x) %in% c("Date","Time"))]
x <- cbind(dateTime, x)
x$dateTime <- as.POSIXct(dateTime)

# histogram
hist(x$Global_active_power, main="Global Active Power", 
     xlab = "Global Active Power", col="purple")

# Plot 2
plot(x$Global_active_power~x$dateTime, type="l", 
     ylab="Global Active Power", xlab="")

# Plot 3
with(x, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})


# Plot 4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(x, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power", xlab="")
  lines(Sub_metering_2~dateTime,col='purple')
  lines(Sub_metering_3~dateTime,col='green')
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power",xlab=" ")
})
