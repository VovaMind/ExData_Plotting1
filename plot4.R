# Check if questions exists in column
hasQuestions <- function(columnValues) {
        temp <- rep(1,length(columnValues))
        temp2 <- temp[as.character(columnValues) == "?"]
        sum(temp2) > 0
}

#build graphics for sub_metering
buildChart<- function(dateTimes, values, color) {
        valuesNum <- as.numeric(as.character(values))
        lines(dateTimes, 
              valuesNum, 
              type="l",
              col = color)
        0
}

#build graphocs for Global ACtive Power, Voltage, Global_reactive_power
buildSimpleChart <- function(dateTimes, values, labelX, labelY) {
        valuesNum <- as.numeric(as.character(values))
        plot(dateTimes, 
             valuesNum, 
             main = "",
             type = "l",
             xlab = labelX,
             ylab = labelY)
        0
}

# set working directory and read input data
#setwd("C:\\R_projects\\ExploratoryAnalysis")
inputData <- read.table("household_power_consumption.txt", header = TRUE, sep = ";")

# check question marks in date and time columns
print("? in all Dates:")
print(hasQuestions(inputData$Date))
print("? in all Times:")
print(hasQuestions(inputData$Time))

# after check we can transform to date and time safely
inputData$DateStr <- as.character(inputData$Date)
inputData$Date <- as.Date(inputData$DateStr, "%d/%m/%Y")

# filter inputData by date. Stay date from  [2007-02-01, 2007-02-02]
dateBegin <- as.Date("2007-02-01", "%Y-%m-%d")
dateEnd <- as.Date("2007-02-02", "%Y-%m-%d")
filteredData <- inputData[(dateBegin <= inputData$Date) & (inputData$Date <= dateEnd),]

# no questions in Sub_metering's, so we can work safely
# build DateTime from Date and Time
filteredData$TimeStr <- as.character(filteredData$Time)
filteredData$DateTimeStr <- paste(filteredData$DateStr, filteredData$TimeStr)
filteredData$DateTime <- strptime(filteredData$DateTimeStr, "%d/%m/%Y %H:%M:%S")

# plot init
png(filename = "plot4.png", width = 480, height = 480)

# check question marks in Voltage
print("? in filtered Voltage:")
print(hasQuestions(filteredData$Voltage))
# check question marks in Global_reactive_power
print("? in filtered Global_reactive_power:")
print(hasQuestions(filteredData$Global_reactive_power))

# draw init
Sys.setlocale("LC_TIME", "English")
par(mfrow = c(2,2))

############################################################
# DateTime -> Global Active power
buildSimpleChart(filteredData$DateTime, 
                filteredData$Global_active_power, 
                '', 
                "Global Active Power")

############################################################
# chart DateTime -> Voltage
buildSimpleChart(filteredData$DateTime, 
                 filteredData$Voltage, 
                 'datetime', 
                 'Voltage')

############################################################
# submetring chart
# draw init canvas
allDates <- append(filteredData$DateTime, filteredData$DateTime)
allDates <- append(allDates, filteredData$DateTime)

valuesNum <- as.numeric(as.character(filteredData$Sub_metering_1))
valuesNum <- append(valuesNum, as.numeric(as.character(filteredData$Sub_metering_2)))
valuesNum <- append(valuesNum, as.numeric(as.character(filteredData$Sub_metering_3)))

plot(allDates, 
     valuesNum, 
     type="n",
     main="",
     xlab="",
     ylab="Energy sub metering")

# add graphics
buildChart(filteredData$DateTime, filteredData$Sub_metering_1, "black")
buildChart(filteredData$DateTime, filteredData$Sub_metering_2, "red")
buildChart(filteredData$DateTime, filteredData$Sub_metering_3, "blue")

# add legend and finish
legend("topright", 
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
       col=c("black", "red", "blue"),
       lty = c(1,1,1))

############################################################
# chart DateTime -> Global_reactive_power
buildSimpleChart(filteredData$DateTime, 
                 filteredData$Global_reactive_power, 
                 'datetime', 
                 'Global_reactive_power')

dev.off()
