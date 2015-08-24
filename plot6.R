plot6 <- function () {
    require(plyr)
    require(dplyr)
    require(reshape2)
    require(ggplot2)
    
    if (!file.exists("data.zip")){
        url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        download.file(url, "data.zip")
    }
    
    if (!file.exists("summarySCC_PM25.rds") | !file.exists("Source_Classification_Code.rds")){
        unzip("data.zip")
    }
    
    SCC <- readRDS("summarySCC_PM25.rds")
    classification <- readRDS("Source_Classification_Code.rds")
    
    vehicles <- classification[grepl("vehicles", classification$EI.Sector, ignore.case = TRUE),]
    vehicleSCC <- merge(SCC, vehicles, by = "SCC")
    melted <- melt(vehicleSCC, id.vars = c("fips", "SCC", "Pollutant", "type", "year"), measure.vars = "Emissions")
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
    ggsave(
        "plot6.png",
        width = 8,
        height = 4,
        dpi = 96)
}