#Plot2.R
#read the files in the downloaded zip package from the link on the course webpage
#Read into data-set names "NEI" and "SCC" as per class instructions

#Because of the long latency to read the data files check to see  if the objects
#NEI and SCC exist in the current environment. Read in the one that does not exist/

#Caution: If your program redefines either NEI or SCC then you must re-read
#the data files to recreate NEI and SCC for use in a subsequent program that uses them.
#Alternately, you can use rm(NEI) and rm(SCC) to delete the objects from the environment

if(!exists("NEI")){
  NEI <- readRDS("summarySCC_PM25.rds") 
}

if(!exists("SCC")) {
  SCC <- readRDS("Source_Classification_Code.rds")
}


NEIF <- subset(NEI, fips =="24510", select = c(Emissions, year))
proj2_plot2 <- with(NEIF, aggregate(Emissions, by = list(year = year), 
                                   FUN = function(x) sum(x, na.rm=TRUE)))

colnames(proj2_plot2)[2] <- "Emissions"

#select a color set suitable for the data set to be plotted using base R
colrs_set <- 2:(length(proj2_plot2[,2]) + 1)   #in R the first color is black

with(proj2_plot2, 
     barplot(height=Emissions/10^2, names.arg = year, col = colrs_set, 
             xlab="Year",
             ylab="PM2.5 Emissions (10^2 Tons)",
             ylim=c(0, 35),
             main="PM2.5 Emissions from 1999 to 2008 in Baltimore City, Maryland"))

dev.copy(png, file="Plot2.png", height=480, width=480)

dev.off()
