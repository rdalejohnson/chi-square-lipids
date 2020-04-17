set.seed (3214)
#lab=read.table("Risk_Behaviors_Recoded.csv", sep=",", header=T, na.strings=c("","NA"))

lab = read.csv("Risk_Behaviors_Recoded.csv",sep=",", na.strings=c("","NA")) 


#### activ.religion

religion.freq=table(lab$activ.religion)
religion.prop=prop.table(religion.freq)
religion.table=cbind(religion.freq,religion.prop)
colnames(religion.table)=c("Frequency","Percent")
print(religion.table)


#### Recode.marijuana

mj.freq=table(lab$Recode.marijuana)
mj.prop=prop.table(mj.freq)
mj.table=cbind(mj.freq,mj.prop)
colnames(mj.table)=c("Frequency","Percent")
print(mj.table)


#### Recode.cigarettes

cigarette.freq=table(lab$Recode.cigarettes)
cigarette.prop=prop.table(cigarette.freq)
cigarette.table=cbind(cigarette.freq,cigarette.prop)
colnames(cigarette.table)=c("Frequency","Percent")
print(cigarette.table)


#### number_of_sexual_partners

lab$number_of_sexual_partners = gsub(0, "0 partners",lab$number_of_sexual_partners)
lab$number_of_sexual_partners = gsub(1, "1 partner",lab$number_of_sexual_partners)
lab$number_of_sexual_partners = gsub("2 or more", "2+ partner",lab$number_of_sexual_partners)

partner.freq=table(lab$number_of_sexual_partners)
partner.prop=prop.table(partner.freq)
partner.table=cbind(partner.freq,partner.prop)
colnames(partner.table)=c("Frequency","Percent")
print(partner.table)



### Step 2:  Check Assumptions
#### 1) Expected Frequencies Need to Be ??? 5
#### Produced when conduct the chi-square analyses have a violation; otherwise, no violation.

### Step 3:  Run Statistical Test:  Chi-Square
#### Crosstabulation of "Activ:religion" (IV) with  1) "Recode:Cigarettes", 2)"Recode:Marijuana", and 3) "# of Sexual Partners"
#### You may install R package "MASS"

#### Run Statistical Test:  Phi & Cramer's V.  
#### Cramer's V (and possibly phi coefficient)
#### You may install R package "vcd"


### religion & marijuana

table1= table(lab$activ.religion,lab$Recode.marijuana)
table1.1= addmargins (table1)
print(table1.1)

table1.2=prop.table(table1, 1)
print(table1.2)
library(MASS)
Xsq <- chisq.test(table1)
library(vcd)
assocstats(table1)

Xsq$expected



### religion & cigarettes


table2= table(lab$activ.religion,lab$Recode.cigarettes)
table2.1= addmargins (table2)
print(table2.1)

table2.2=prop.table(table2, 1)
print(table2.2)
library(MASS)
chisq.test(table2)
library(vcd)
assocstats(table2)



### religion & partners


table3= table(lab$activ.religion,lab$number_of_sexual_partners)
table3.1= addmargins (table3)
print(table3.1)

table3.2=prop.table(table3)
print(table3.2)
library(MASS)
chisq.test(table3)
library(vcd)
assocstats(table3)

### Step 4:  State a conclusion 

#### Two of the three risky behaviors were statistically significant, indicating that the pattern of cell frequencies observed is unlikely to have occurred by chance alone.  Smoking cigarettes and marijuana do appear to be dependent upon the students' perception of religion's importance [??2 (6, n=1,378) = 54.11, p<.0001 for cigarettes and ??2 (3, n=1,378) = 72.85, p<.0001 for marijuana].  There was no significant effect on the number of sexual partners.


