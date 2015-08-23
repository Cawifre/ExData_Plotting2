plot2 <- function () {
    require(plyr)
    require(dplyr)
    require(reshape2)
    require(ggplot2)
    require(data.table)
    
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
    
    png("plot1.png")
    sumSCC <- dcast(
        melted,
        formula = year ~ variable,
        fun.aggregate = sum)
    plot(
        x = sumSCC$year,
        y = sumSCC$Emissions / 1000000,
        main = "PM2.5 Emissions From All Sources",
        xlab = "Year",
        ylab = "Emissions (millions of tons)",
        pch = 16,
        xaxt = "n")
    axis(
        1,
        at = c(1999, 2002, 2005, 2008),
        las = 2)
    abline(
        with(sumSCC, lm(Emissions / 1000000 ~ year)),
        col = rgb(0.05, 0.95, 0.05, 0.3),
        lwd = 5)
    dev.off()
    
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
    
    sumSCC <- dcast(
        melted,
        formula = year + type ~ variable,
        subset = .(fips == "24510"),
        fun.aggregate = sum)
    ggplot(sumSCC, aes(x = year,
                       y = Emissions / 1000,
                       group = type,
                       color = type)) +
        geom_point() +
        geom_smooth(method = "lm") +
        facet_wrap(~type) +
        labs(
            x = "Year",
            y = "Emissions (thousands of tons)",
            color = "Type",
            title = "Baltimore City PM2.5 Emissions From All Sources By Type")
    ggsave("plot3.png")
    
    png("plot4.png")
    coals <- classification[grepl("coal", classification$EI.Sector, ignore.case = TRUE),]
    coalSCC <- merge(SCC, coals, by = "SCC")
    melted <- melt(coalSCC, id.vars = c("fips", "SCC", "Pollutant", "type", "year"), measure.vars = "Emissions")
    sumSCC <- dcast(
        melted,
        formula = year ~ variable,
        fun.aggregate = sum)
    plot(
        x = sumSCC$year,
        y = sumSCC$Emissions / 1000,
        main = "PM2.5 Emissions From Coal Sources",
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
    
    bcSumSCC <- dcast(
        melted,
        formula = year ~ variable,
        subset = .(fips == "24510"),
        fun.aggregate = sum)
    bcSumSCC$City <- "Baltimore City"
    laSumSCC <- dcast(
        melted,
        formula = year ~ variable,
        subset = .(fips == "06037"),
        fun.aggregate = sum)
    laSumSCC$City <- "Los Angeles"
    sumSCC <- rbind( bcSumSCC, laSumSCC)
    ggplot(sumSCC, aes(x = year,
                       y = Emissions / 1000,
                       group = City,
                       color = City)) +
        geom_point() +
        geom_smooth(method = "lm") +
        facet_wrap(~City) +
        labs(
            x = "Year",
            y = "Emissions (thousands of tons)",
            color = "City",
            title = "PM2.5 Emissions From All Sources By City")
    ggsave("plot6.png")
}