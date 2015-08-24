plot2 <- function () {
    require(plyr)
    require(dplyr)
    require(reshape2)
    
    if (!file.exists("data.zip")){
        url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        download.file(url, "data.zip")
    }
    
    if (!file.exists("summarySCC_PM25.rds") | !file.exists("Source_Classification_Code.rds")){
        unzip("data.zip")
    }
    
    SCC <- readRDS("summarySCC_PM25.rds")
    classification <- readRDS("Source_Classification_Code.rds")
    
    melted <- melt(SCC, id.vars = c("fips", "SCC", "Pollutant", "type", "year"), measure.vars = "Emissions")
    
    png("plot2.png")
    sumSCC <- dcast(
        melted,
        formula = year ~ variable,
        subset = .(fips == "24510"),
        fun.aggregate = sum)
    plot(
        x = sumSCC$year,
        y = sumSCC$Emissions / 1000,
        main = "Baltimore City PM2.5 Emissions From All Sources",
        xlab = "Year",
        ylab = "Emissions (thousands of tons)",
        pch = 16,
        xaxt = "n")
    axis(
        1,
        at = c(1999, 2002, 2005, 2008),
        las = 2)
    abline(
        with(sumSCC, lm(Emissions / 1000 ~ year)),
        col = rgb(0.05, 0.95, 0.05, 0.3),
        lwd = 5)
    dev.off()
}