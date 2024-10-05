print("Plotting Basics: Shree Tejani")  # print author name
#importing the libraries
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")
# use installed libraries
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(moments)
library(Hmisc)


#load the required dataset in df
library(FSA)
data(BullTroutRML2)  # import BullTroutRML2 dataset
BullTroutRML2
str(BullTroutRML2)  # view structure of the BullTroutRML2 dataset
dim(BullTroutRML2)  # view number of rows and cols of BullTroutRML2 dataset

head(BullTroutRML2,3) # View the first 3 rows of the dataset
tail(BullTroutRML2,n=3) # View the last 3 rows of the dataset

#remove records apart from harrison lake only
harrisonLake <- filter(BullTroutRML2,lake == "Harrison")
harrisonLake
str(harrisonLake) # view structure of filtered dataset i.e HarrisonLake
summary(harrisonLake)  # view summary of filtered dataset i.e HarrisonLake

head(harrisonLake,5) # View the first 5 rows of the filtered dataset i.e Harrison lake
tail(harrisonLake,n=5) # View the last 5 rows of the filtered dataset i.e Harrison lake

# create scatter plot for age and fl for harrison lake dataset
plot(harrisonLake$fl, harrisonLake$age, 
     xlim = c(0,500), ylim = c(0,15), 
     xlab = "Fork Length (mm)", ylab = "Age (yrs)",
     pch=18, col="red",
     main = "Plot 1: Harrison Lake Trout")

# create histogram for age and frequency for harrison lake dataset
hist(harrisonLake$age, main ="Plot 2: Harrison Fish Age Distribution",
    xlab="Age (yrs)", ylab="Frequency",col="yellow", col.main="blue",labels = TRUE)

# create scatter Plot 3: Harrison Density Shaded by Era
plot(data = harrisonLake, age~fl, main ="Plot 3: Harrison Density Shaded by Era", xlim =c(0,500),ylim= c(0,15),
    xlab="Fork Length (mm)", ylab="Age (yrs)", pch=18,
    colramp = colorRampPalette(c('lightgreen','white')),col = 'green')

#Entering the first and the last five records of the BULLTORNT data in the new object "tmp"
tmp <- bind_rows(head(harrisonLake,5),tail(harrisonLake,5))
tmp

#Displaying the era values in the temp object
tmp_Era <- C(tmp$era)
tmp_Era

#Create a pchs vector with the argument values for + and x
pch <-as.vector(harrisonLake$era)
pchs <- c("+","X")
pchs

#> Create a cols vector with the two elements “blue” and “red”
col<-as.vector(harrisonLake$era)
col
cols<-c("blue","red")
cols

#convert temp$era into numeric values
tmp$era = as.numeric(tmp_Era)
tmp$era

#intialize cols vector with temp era values
#plot 4: Symbol & Color by Era
plot(data=harrisonLake,age~fl, xlab="Fork length(mm)", ylab ="Age (yrs)",xlim = c(0,500), ylim = c(0,15),
     pch=pchs, col=cols, main="Plot 4: Symbol & Color by Era")

# regression overlay 
plot(data=harrisonLake,age~fl, xlab="Fork Length(mm)", ylab = "Age (yrs)", xlim = c(0,500), ylim = c(0,15),
     pch=pchs,col=cols, main="Plot 5: Regression Overlay")
abline(lm(age~fl,data = harrisonLake))

# legend
plot(data=harrisonLake,age~fl, xlab="Fork Length(mm)", ylab = "Age (yrs)", xlim = c(0,500), ylim = c(0,15),
     pch=pchs,col=cols, main="Plot 6: Legend Overlay")
legend(x="topleft", legend = paste(levels(harrisonLake$era)),pch = pchs,col=cols)
abline(lm(age~fl,data = harrisonLake))
