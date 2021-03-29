#Plot1.R
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

proj2_plot1 <- with(NEI, aggregate(Emissions, by = list(year = year), 
                    FUN = function(x) sum(x, na.rm=TRUE)))

colnames(proj2_plot1)[2] <- "Emissions"

#select a color set that is suitable for plotting the data set using base R
colrs_set <- 2:(length(proj2_plot1[,2]) + 1)   #in R the first color is black

#Note we scale Emissions by a factor of 100 for better visuals

with(proj2_plot1, 
     barplot(height=Emissions/10^2, names.arg = year, col = colrs_set, 
             xlab="Year",
             ylab="PM2.5 Emissions (10^2 Tons)",
             main="Total PM2.5 Emissions From All US Sources"))
             
dev.copy(png, file="Plot1.png", height=480, width=480)

dev.off()

