plot3 <- function () {
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
    
    melted <- melt(SCC, id.vars = c("fips", "SCC", "Pollutant", "type", "year"), measure.vars = "Emissions")
    
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
    ggsave(
        "plot3.png",
        width = 8,
        height = 8,
        dpi = 96)
}