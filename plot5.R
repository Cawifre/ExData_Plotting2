plot5 <- function () {
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
    
    png("plot5.png")
    vehicles <- classification[grepl("vehicles", classification$EI.Sector, ignore.case = TRUE),]
    vehicleSCC <- merge(SCC, vehicles, by = "SCC")
    melted <- melt(vehicleSCC, id.vars = c("fips", "SCC", "Pollutant", "type", "year"), measure.vars = "Emissions")
    sumSCC <- dcast(
        melted,
        formula = year ~ variable,
        subset = .(fips == "24510"),
        fun.aggregate = sum)
    plot(
        x = sumSCC$year,
        y = sumSCC$Emissions / 100,
        main = "Baltimore City PM2.5 Emissions From Vehicles",
        xlab = "Year",
        ylab = "Emissions (hundreds of tons)",
        pch = 16,
        xaxt = "n")
    axis(
        1,
        at = c(1999, 2002, 2005, 2008),
        las = 2)
    abline(
        with(sumSCC, lm(Emissions / 100 ~ year)),
        col = rgb(0.05, 0.95, 0.05, 0.3),
        lwd = 5)
    dev.off()
}