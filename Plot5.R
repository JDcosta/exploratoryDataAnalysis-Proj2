#Plot5.R
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

#Because the objective is an exploratory data analysis, a first order approximation
#of the answer is sought. Examine the EI.Sector field in the SCC dataset to short-list possible rows
# and then use The SCC.Level.Two and SCC.Level.Three fields in the same dataset are used to search in data-frame subsetted using the
#short-listed rows using the search expression "vehicle" 
#Note: SCC.Level.One and SCC.Level.Four were found not to substantially meet 
#the search criteria and are not included in the search without loss of generality
#FInally subset NEI using the SCC results and proceed with the analysis

tre <- SCC[grepl("mobile",SCC$EI.Sector, ignore.case=TRUE), ]

SCC_vehicle <- tre[grep("vehicle", tre$SCC.Level.Two, ignore.case=TRUE), ]$SCC

NEIBA <- NEI[NEI$fips =="24510", ]  #fips 24510 corresponds to Baltimore City, MD

#use these values of SCC stored in SCC_vehicle and the fips variable to filter dataframe

NEIT <- NEIBA[NEIBA$SCC %in% SCC_vehicle, ]


proj2_plot5 <- with(NEIT, aggregate(Emissions, by = list(year = year), 
                                    FUN = function(x) sum(x, na.rm=TRUE)))
colnames(proj2_plot5)[2] <- "Emissions"

png(filename = "plot5.png",width = 480, height = 480)

g <- ggplot(proj2_plot5,aes(factor(year),Emissions/10^2)) +
  geom_bar(stat="identity", fill = "steelblue", width = 0.75) +
  labs(x="year", y="PM2.5 Emissions (10^2 Tons)" ) + 
  labs(title="PM2.5 Emissions, Baltimore City", subtitle="from Automobile sources")

print(g)
dev.off()