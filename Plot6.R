#Plot6.R
#read the files in the downloaded zip package from the link on the course webpage
#Read into data-set names "NEI" and "SCC" as per class instructions

#Because of the long latency to read the data files check to see  if the objects
#NEI and SCC exist in the current environment. Read in the one that does not exist/

#Caution: If your program redefines either NEI or SCC then you must re-read
#the data files to recreate NEI and SCC for use in a subsequent program that uses them.
#Alternately, you can use rm(NEI) and rm(SCC) to delete the objects from the environment

library(ggplot2)
library(dplyr)

if(!exists("NEI")){
  NEI <- readRDS("summarySCC_PM25.rds") 
}

if(!exists("SCC")) {
  SCC <- readRDS("Source_Classification_Code.rds")
}


#since this is exploratory data analysis, the SCC.Level.Two and SCC.Level.Three
#fields in the SCC data-frame are explored for rows that 
#contain the expression "vehicle" Note: SCC.Level.One, SCC.Level.Three, and SCC.Level.Four were found
#not to contribute to the number of relevant rows to be considered so they are
#not included.


tre <- SCC[grepl("mobile",SCC$EI.Sector, ignore.case=TRUE), ]

SCC_vehicle <- tre[grep("vehicle", tre$SCC.Level.Two, ignore.case=TRUE), ]$SCC

NEIBA <- NEI[NEI$fips =="24510"|NEI$fips == "06037", ]

#use these values of SCC stored in SCC_vehicle and the fips variable to filter dataframe

NEIT <- NEIBA[NEIBA$SCC %in% SCC_vehicle, ]

#modify the names of the factor levels to make the labels on plots readable


proj2_plot6 <- with(NEIT, aggregate(Emissions, by = list(year = year, fips = fips), 
                                    FUN = function(x) sum(x, na.rm=TRUE)))
colnames(proj2_plot6)[3] <- "Emissions"

proj2_plot6$fips = factor(proj2_plot6$fips, levels = c("24510","06037"))
levels(proj2_plot6$fips) <- list("Baltimore City" = "24510", 
                          "Los Angeles County" = "06037")

#select a color set suitable for the data set to be plotted using base R
#colrs_set <- 2:(length(proj2_plot3[,2]) + 1)   #in R the first color is black

png(filename = "plot6.png",width = 480, height = 480)

g <- ggplot(proj2_plot6,aes(factor(year),Emissions/10^2)) +
  geom_bar(stat="identity", fill = "steelblue", width = 0.75) +
  facet_grid(facets = .~fips,scales = "free", space = "free") +
  labs(x="year", y="PM2.5 Emissions (10^2 Tons)" ) + 
  labs(title="PM2.5 Vehicle Source Emissions, Baltimore City v/s LA County")

print(g)
dev.off()