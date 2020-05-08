
x <- read.table("household_power_consumption.txt", header=TRUE, 
                sep=";", na.strings = "?", 
                colClasses = c('character','character','numeric','numeric',
                               'numeric','numeric','numeric','numeric','numeric'))

x$Date <- as.Date(x$Date, "%d/%m/%Y")

# Filtered data
x <- subset(x,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

x <- x[complete.cases(x),]

# Combine Date and Time column
dateTime <- paste(x$Date, x$Time)

dateTime <- setNames(dateTime, "DateTime")

# date and time columns
x <- x[ ,!(names(x) %in% c("Date","Time"))]
x <- cbind(dateTime, x)
x$dateTime <- as.POSIXct(dateTime)

# histogram
hist(x$Global_active_power, main="Global Active Power", 
     xlab = "Global Active Power (kilowatts)", col="purple")

# Plot 2
plot(x$Global_active_power~x$dateTime, type="l", 
     ylab="Global Active Power (kilowatts)", xlab="")

# Plot 3
with(x, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("pink", "green", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))


# Plot 4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(x, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='blue')
  lines(Sub_metering_3~dateTime,col='green')
  legend("topright", col=c("pink", "black", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab=" ")
})