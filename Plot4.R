#Plot4.R
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

#since this is an exploratory data analysis, the Short.name field in the SCC dataframe
#is interrogated for rows that have "coal" AND "Comb"and the SCC codes corresponding 
#to the rows thus obtained are used to get a first order approximation 
#of the answer to the question

tre <- grep("coal", SCC$Short.Name, ignore.case=TRUE)
fre <- lapply(SCC$Short.Name[tre], 
              FUN = function(x) grepl("comb", x, ignore.case=TRUE))

#the SCC$SCC rows that correspond to the selected SCC$Short.Name field rows
#are the rows of interest for this analysis

SCC_coal_comb <- SCC$SCC[tre[unlist(fre)]]

#verify
#head(SCC_coal_comb)
#[1] 10100101 10100102 10100201 10100202 10100203 10100204
#these are values in the rows of the SCC column of NEI

#use these values of SCC stored in SCC_coal_comb to subset NEI

NEIF <- subset(NEI, SCC %in% SCC_coal_comb, select = c(Emissions, year))

proj2_plot4 <- with(NEIF, aggregate(Emissions, by = list(year = year), 
                    FUN = function(x) sum(x, na.rm=TRUE)))

colnames(proj2_plot4)[2] <- "Emissions"

png(filename = "plot4.png",width = 480, height = 480)
    
g <- ggplot(NEIF,aes(factor(year),Emissions/10^2)) +
  geom_bar(stat="identity", fill = "steelblue", width = 0.75) +
  labs(x="year", y="PM2.5 Emissions (10^2 Tons)" ) + 
  labs(title="PM2.5 Emissions across the US in the period 1999-2008",
       subtitle ="produced by coal combustion" )

print(g)

dev.off()