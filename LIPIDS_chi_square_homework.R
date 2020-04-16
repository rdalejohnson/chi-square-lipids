
#Sources:
# adding new columns and recoding them, 2 approaches:
#    https://stackoverflow.com/questions/31150764/how-to-create-a-new-column-of-data-in-r-with-if-statements


library(plyr)
library(tidyverse)
library(MASS)
library(Hmisc)
library(PerformanceAnalytics)



lipids = read.csv("LIPIDS.csv",sep=",", na.strings=c("","NA")) 

nrow(lipids)
nrow(na.omit(lipids))
ncol(lipids)

for (i in colnames(lipids)){
  print(class(lipids[[i]]))
}

########### TRIGLYCERIDES
summary(lipids$Triglycerides)

thirdQuantile <- quantile(lipids$Triglycerides)[4]
firstQuantile <- quantile(lipids$Triglycerides)[2]
interqurtRange <- IQR(lipids$Triglycerides)*1.5
outliersAbove <- unname(thirdQuantile + interqurtRange)
outliersBelow <- unname(firstQuantile - interqurtRange)

outliersListTriglycerides <-  lipids$Triglycerides[lipids$Triglycerides < outliersBelow | lipids$Triglycerides > outliersAbove] 

outliersListTriglycerides

#Get a histogram, mean, standard deviation
histogramTriglycerides=hist(lipids$Triglycerides, main="A histogram of the Triglycerides values")
meanTriglycerides <- mean(lipids$Triglycerides,na.rm=TRUE)
sdTriglycerides <- sd(lipids$Triglycerides,na.rm=TRUE)
print (paste("Triglycerides Mean: ", meanTriglycerides))
print (paste("Triglycerides St. Dev: ", sdTriglycerides))
print (paste("Interquartile Range: ", IQR(lipids$Triglycerides)))
print (paste("Outliers upper (1.5*IQR) boundary: ", outliersAbove))
print (paste("Outliers lower (1.5*IQR) boundary: ", outliersBelow))


### BOX PLOT; note the outliers;
triglyceridesBoxPlot <-
  boxplot(lipids$Triglycerides,
          main = "Triglycerides Boxplot",
          xlab = "x",
          col = "orange",
          border = "brown",
          horizontal = TRUE,
          notch = TRUE
  )

triglyceridesBoxPlot

qqnorm(lipids$Triglycerides)
qqline(lipids$Triglycerides)

########## Cholesterol
summary(lipids$Cholesterol)

thirdQuantile <- quantile(lipids$Cholesterol)[4]
firstQuantile <- quantile(lipids$Cholesterol)[2]
interqurtRange <- IQR(lipids$Cholesterol)*1.5
outliersAbove <- unname(thirdQuantile + interqurtRange)
outliersBelow <- unname(firstQuantile - interqurtRange)

outliersListCholesterol <-  lipids$Cholesterol[lipids$Cholesterol < outliersBelow | lipids$Cholesterol > outliersAbove] 

outliersListCholesterol

#Get a histogram, mean, standard deviation
histogramCholesterol=hist(lipids$Cholesterol, main="A histogram of the Cholesterol values")
meanCholesterol <- mean(lipids$Cholesterol,na.rm=TRUE)
sdCholesterol <- sd(lipids$Cholesterol,na.rm=TRUE)
print (paste("Cholesterol Mean: ", meanCholesterol))
print (paste("Cholesterol St. Dev: ", sdCholesterol))
print (paste("Interquartile Range: ", IQR(lipids$Cholesterol)))
print (paste("Outliers upper (1.5*IQR) boundary: ", outliersAbove))
print (paste("Outliers lower (1.5*IQR) boundary: ", outliersBelow))


### BOX PLOT; note the outliers;
CholesterolBoxPlot <-
  boxplot(lipids$Cholesterol,
          main = "Cholesterol Boxplot",
          xlab = "x",
          col = "orange",
          border = "brown",
          horizontal = TRUE,
          notch = TRUE
  )

CholesterolBoxPlot

qqnorm(lipids$Cholesterol)
qqline(lipids$Cholesterol)



########## LDL
summary(lipids$LDL)

thirdQuantile <- quantile(lipids$LDL)[4]
firstQuantile <- quantile(lipids$LDL)[2]
interqurtRange <- IQR(lipids$LDL)*1.5
outliersAbove <- unname(thirdQuantile + interqurtRange)
outliersBelow <- unname(firstQuantile - interqurtRange)

outliersListLDL <-  lipids$LDL[lipids$LDL < outliersBelow | lipids$LDL > outliersAbove] 

outliersListLDL

#Get a histogram, mean, standard deviation
histogramLDL=hist(lipids$LDL, main="A histogram of the LDL values")
meanLDL <- mean(lipids$LDL,na.rm=TRUE)
sdLDL <- sd(lipids$LDL,na.rm=TRUE)
print (paste("LDL Mean: ", meanLDL))
print (paste("LDL St. Dev: ", sdLDL))
print (paste("Interquartile Range: ", IQR(lipids$LDL)))
print (paste("Outliers upper (1.5*IQR) boundary: ", outliersAbove))
print (paste("Outliers lower (1.5*IQR) boundary: ", outliersBelow))


### BOX PLOT; note the outliers;
LDLBoxPlot <-
  boxplot(lipids$LDL,
          main = "LDL Boxplot",
          xlab = "x",
          col = "orange",
          border = "brown",
          horizontal = TRUE,
          notch = TRUE
  )

LDLBoxPlot

qqnorm(lipids$LDL)
qqline(lipids$LDL)



########## HDL
summary(lipids$HDL)

thirdQuantile <- quantile(lipids$HDL)[4]
firstQuantile <- quantile(lipids$HDL)[2]
interqurtRange <- IQR(lipids$HDL)*1.5
outliersAbove <- unname(thirdQuantile + interqurtRange)
outliersBelow <- unname(firstQuantile - interqurtRange)

outliersListHDL <-  lipids$HDL[lipids$HDL < outliersBelow | lipids$HDL > outliersAbove] 

outliersListHDL

#Get a histogram, mean, standard deviation
histogramHDL=hist(lipids$HDL, main="A histogram of the HDL values")
meanHDL <- mean(lipids$HDL,na.rm=TRUE)
sdHDL <- sd(lipids$HDL,na.rm=TRUE)
print (paste("HDL Mean: ", meanHDL))
print (paste("HDL St. Dev: ", sdHDL))
print (paste("Interquartile Range: ", IQR(lipids$HDL)))
print (paste("Outliers upper (1.5*IQR) boundary: ", outliersAbove))
print (paste("Outliers lower (1.5*IQR) boundary: ", outliersBelow))


### BOX PLOT; note the outliers;
HDLBoxPlot <-
  boxplot(lipids$HDL,
          main = "HDL Boxplot",
          xlab = "x",
          col = "orange",
          border = "brown",
          horizontal = TRUE,
          notch = TRUE
  )

HDLBoxPlot

qqnorm(lipids$HDL)
qqline(lipids$HDL)

########### BIG SUMMARY ##############
summary(lipids)


lipids = read.csv("LIPIDS.csv",sep=",", na.strings=c("","NA")) 


 
lipids$triglycerides_national_guidelines <- ifelse(lipids$Triglycerides<150, "0-Optimal", "1-Not Optimal")
         

lipids$triglycerides_tertiles <- ifelse(lipids$Triglycerides<=113, "1-Low Level [0-113]", 
                                                    ifelse(lipids$Triglycerides<=183, "2-Medium Level [114-183]",
                                                           ifelse(lipids$Triglycerides<=706, "3-High Level [184-706]", NA)))


         





