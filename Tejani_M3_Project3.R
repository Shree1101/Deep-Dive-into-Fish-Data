print("Shree Tejani")  # print author name
#importing the libraries
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")
install.packages("plyr")
install.packages("tidyverse")
# use installed libraries
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(plotrix)
library(ggplot2)
library(moments)
library(Hmisc)


#importing dataset from FSA library 
tBio <- read.csv("D:\\MPS_Quater 1\\ALY6000_Intro to Analytics\\inchBio.csv", header=TRUE, stringsAsFactors=FALSE)
tBio
str(tBio)   #structure of tBio
summary(tBio)   #summary of tBio

head(tBio,5) # head values for tBio
tail(tBio,5) # tail values for tBio

cts <- table(tBio$species) #obj 'CTS' to displays count 
cts
class(cts)

names(cts)  # names of species 

#temp1
temp1 <- subset(tBio, select = c("species"))
temp1 <- table(temp1)
temp1

#temp2
temp2 <- head(temp1,5)
temp2

#table t
t <- table(tBio[3])
t
class(t)

#transform t table to DF
df <- data.frame(t)
df
class(df)

# Q10 
freq <- df$Freq
freq

#Q11
# Creating a new table named tSpec containing the species attribute of tBio
tSpec <- table(tBio$species)
tSpec
class(tSpec)

#Q12
tSpecPct <- (t/676)*100 
tSpecPct
class(tSpecPct)

#Q13
dfSP <- as.data.frame(tSpecPct)  
dfSP
class(tSpecPct)


#Q14 bar plot for tSpec
polt1 <- barplot(tSpec,main="Plot 1: Fish Species Counts",ylim = c(0,250),
        ylab = "Counts",col="grey",las=2,cex.axis=0.58) 

#Q15
polt2 <- barplot(tSpecPct,main="Plot 2: Fish Relative Frequency",ylim = c(0,35),
                 ylab = "%",col="cyan",las=2,cex.axis=0.58) 

#Q16
data <- dfSP[order(-dfSP$Freq),]
data

#Q17
colnames(data) <- c("Species", "RelFreq")
data

#Q18
# Calculate the cumulative relative frequency
data$cumFreq <- cumsum(data$RelFreq)
# Add a column to data with the count of each species
data$cts <- data$RelFreq*676 
data$cumCts <- cumsum(cts)
data


#Q19
varPar <- colnames(data)
varPar

#Q20
pc <- barplot(data$cts, width = 1, space = .1, border = NA,
              axes = FALSE, ylim = c(0, 3.05*max(data$cts, na.rm=TRUE)),
              ylab = "Cumulative Counts", names.arg = data$Species,col = "lightgreen", 
              main = "Plot 3: Fish Species Pareto", las = 2, cex.names = 0.58)

#Q21
pc <- barplot(data$cts, width = 1, space = .1, border = NA,
              axes = FALSE, ylim = c(0, 3.05*max(data$cts, na.rm=T)),
              ylab = "Cumulative Counts", names.arg = data$Species,col = "lightgreen", 
              main = "Plot 3B: Fish Species Pareto", las = 2, cex.names = 0.58)
lines(pc,data$cts,type = "b", cex = 0.75, pch = 20, col = "black")
box(col = "grey62") #Q22

#Q23
pc <- barplot(data$cts, width = 1, space = .1, border = NA,
              axes = FALSE, ylim = c(0, 3.05*max(data$cts, na.rm=TRUE)),
              ylab = "Cumulative Counts", names.arg = data$Species, 
              main = "Plot 3D: Fish Species Pareto", las = 2, cex.names = 0.58, cex.axis = 0.75)
lines(pc,data$cts,type = "b", cex = 0.75, pch = 20, col = "black")
box(col = "grey62") #Q22
axis(side = 2, at = data$cumCts, col.ticks = "grey62",
      col = "grey62", cex.axis = 0.75,las = 2)

#Q24
pc <- barplot(data$cts, width = 1, space = .1, border = NA,
              axes = FALSE, ylim = c(0, 3.05*max(data$cts, na.rm=T)),
              ylab = "Cumulative Counts", names.arg = data$Species,
              main = "Plot 3D: Fish Species Pareto", las = 2, cex.names = 0.58)
lines(pc,data$cts,type = "b", cex = 0.75, pch = 20, col = "black")
box(col = "grey62") #Q22
axis(side = 4, at = c(0,data$cumCts), labels = paste(c(0,round(data$cumFreq * 100)),"%",sep =""),
     col.lab = "cyan4", col.axis = "cyan4",
     cex.axis = 0.75, las = 2) 

#Q25
pc <- barplot(data$cts, width = 1, space = .1, border = NA,
              axes = FALSE, ylim = c(0, 3.05*max(data$cts, na.rm=T)),
              ylab = "Cumulative Counts by Shree Tejani", names.arg = data$Species, 
              main = "Plot 3B: Fish Species Pareto", las = 2, cex.names = 0.58)
lines(pc,data$cts,type = "b", cex = 0.75, pch = 20, col = "black")
box(col = "grey62") #Q22
axis(side = 2, at = data$cumCts, labels = data$cumCts,
     col.axis = "grey62", col.lab = "grey62", cex.axis = 0.75,las = 2)
axis(side = 4, at = c(0,data$cumCts), labels = paste(c(0,round(data$cumFreq * 100)),"%",sep =""),
     col.lab = "cyan4", col.axis = "cyan4",
     cex = 0.75, las = 2)
