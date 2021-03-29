#Plot3.R
#read the files in the downloaded zip package from the link on the course webpage
#Read into data-set names "NEI" and "SCC" as per class instructions

#Because of the long latency to read the data files check to see  if the objects
#NEI and SCC exist in the current environment. Read in the one that does not exist/

#Caution: If your program redefines either NEI or SCC then you must re-read
#the data files to recreate NEI and SCC for use in a subsequent program that uses them.
#Alternately, you can use rm(NEI) and rm(SCC) to delete the objects from the environment

library(ggplot2)

if(!exists("NEI")){
  NEI <- readRDS("summarySCC_PM25.rds") 
}

if(!exists("SCC")) {
  SCC <- readRDS("Source_Classification_Code.rds")
}
#subset NEI to contain only those rows that contain data for the source type
NEIF <- subset(NEI, fips =="24510", select = c(Emissions, year,type))

proj2_plot3 <- with(NEIF, aggregate(Emissions, by = list(year = year, type=type), 
                                    FUN = function(x) sum(x, na.rm=TRUE)))

colnames(proj2_plot3)[3] <- "Emissions"

png("Plot3.png", width = 720, height = 640)

g <- ggplot(NEIF,aes(factor(year),Emissions,fill=type)) +
       geom_bar(stat="identity") +
       facet_grid(.~type, scales = "free", space="free") + 
       labs(x="year", y="PM2.5 Emissions (10^2 Tons)" ) + 
       labs(title="PM2.5 Emissions- Baltimore City 1999-2008",
            subtitle = "by Source Type")
print(g)

dev.off()