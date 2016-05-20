# Check if questions exists in column
hasQuestions <- function(columnValues) {
        temp <- rep(1,length(columnValues))
        temp2 <- temp[as.character(columnValues) == "?"]
        sum(temp2) > 0
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

# check question marks in Global_active_power
print("? in filtered Global ActivePower:")
print(hasQuestions(filteredData$Global_active_power))

# no questions in Global_active_power, so we can work safely
# build DateTime from Date and Time
filteredData$TimeStr <- as.character(filteredData$Time)
filteredData$DateTimeStr <- paste(filteredData$DateStr, filteredData$TimeStr)
filteredData$DateTime <- strptime(filteredData$DateTimeStr, "%d/%m/%Y %H:%M:%S")

# plot result
png(filename = "plot2.png", width = 480, height = 480)
Sys.setlocale("LC_TIME", "English")
globalActivePower <- as.numeric(as.character(filteredData$Global_active_power))
plot(filteredData$DateTime, 
     globalActivePower, 
     type="l",
     main="",
     xlab="",
     ylab="Global Active Power (kilowatts)")
dev.off()
