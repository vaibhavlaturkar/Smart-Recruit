fintro <- read.csv("G:/R/Fintro/Train.csv")
test <- read.csv("G:/R/Fintro/test.csv")


fintro1 <- fintro[,-23]
fintro_replace <- rbind(fintro1,test)

fintro_replace$Manager_Business <- abs(fintro_replace$Manager_Business)


summary(fintro_replace)
### Function
Replace_Median <- function(data){
  median <- median(data,na.rm = TRUE)
  index <- which(is.na(data))
  data[index] <- median
  return(data)
}


fintro_replace$Manager_Grade <- Replace_Median(fintro_replace$Manager_Grade)
fintro_replace[,17] <- Replace_Median(fintro_replace[,17])
fintro_replace[,18] <- Replace_Median(fintro_replace[,18])
fintro_replace[,19] <- Replace_Median(fintro_replace[,19])
fintro_replace[,20] <- Replace_Median(fintro_replace[,20])
fintro_replace[,21] <- Replace_Median(fintro_replace[,21])
fintro_replace[,22] <- Replace_Median(fintro_replace[,22])
fintro_replace$Applicant_City_PIN <- Replace_Median(fintro_replace$Applicant_City_PIN)


summary(fintro_replace)



## Replace Blank from gender to M 
fintro_replace$Applicant_Gender <- ifelse(fintro_replace$Applicant_Gender == "","M",ifelse(fintro_replace$Applicant_Gender == "F","F","M"))
fintro_replace$Applicant_Gender <- as.factor(fintro_replace$Applicant_Gender)


## Replace Blank from Applicant_Marital_Status to M 

fintro_replace$Applicant_Marital_Status <- as.character(fintro_replace$Applicant_Marital_Status)
ms <- which(fintro_replace$Applicant_Marital_Status == "")
fintro_replace$Applicant_Marital_Status[ms] <- "M"
fintro_replace$Applicant_Marital_Status <- as.factor(fintro_replace$Applicant_Marital_Status)

## Occuption

fintro_replace$Applicant_Occupation <- as.character(fintro_replace$Applicant_Occupation)
oc <- which(fintro_replace$Applicant_Occupation == "")
fintro_replace$Applicant_Occupation[oc] <- "Salaried"
fintro_replace$Applicant_Occupation <- as.factor(fintro_replace$Applicant_Occupation)


## Qualification

fintro_replace$Applicant_Qualification <- as.character(fintro_replace$Applicant_Qualification)
q <- which(fintro_replace$Applicant_Qualification == "")
fintro_replace$Applicant_Qualification[q] <- "Class XII"
fintro_replace$Applicant_Qualification<- as.factor(fintro_replace$Applicant_Qualification)




## Joining_Designation

fintro_replace$Manager_Joining_Designation <- as.character(fintro_replace$Manager_Joining_Designation)
m <- which(fintro_replace$Manager_Joining_Designation == "")
fintro_replace$Manager_Joining_Designation[m] <- "Level 2"
fintro_replace$Manager_Joining_Designation<- as.factor(fintro_replace$Manager_Joining_Designation)



## current_Designation

fintro_replace$Manager_Current_Designation <- as.character(fintro_replace$Manager_Current_Designation)
c <- which(fintro_replace$Manager_Current_Designation == "")
fintro_replace$Manager_Current_Designation[c] <- "Level 1"
fintro_replace$Manager_Current_Designation<- as.factor(fintro_replace$Manager_Current_Designation)


## Manager Gender

fintro_replace$Manager_Gender <- as.character(fintro_replace$Manager_Gender)
gen <- which(fintro_replace$Manager_Gender == "")
fintro_replace$Manager_Gender[gen] <- "M"
fintro_replace$Manager_Gender<- as.factor(fintro_replace$Manager_Gender)



## Manager_Status

fintro_replace$Manager_Status <- as.character(fintro_replace$Manager_Status)
st <- which(fintro_replace$Manager_Status == "")
fintro_replace$Manager_Status[st] <- "Confirmation"
fintro_replace$Manager_Status<- as.factor(fintro_replace$Manager_Status)

levels(fintro_replace$Manager_Status)



summary(fintro_replace)


# Converting Application_Reciept_Date --- > Joining_date,Applicant_BirthDate, Manager_DOJ,Manger_DoB into Dateformat 

library(lubridate)

fintro_replace$Application_Receipt_Date <- mdy(fintro_replace$Application_Receipt_Date)
fintro_replace$Applicant_BirthDate <- mdy(fintro_replace$Applicant_BirthDate)
fintro_replace$Manager_DOJ <- mdy(fintro_replace$Manager_DOJ)
fintro_replace$Manager_DoB <- mdy(fintro_replace$Manager_DoB)



library(dplyr)
fintro_replace <- mutate(fintro_replace, Applicant_Age = as.numeric((Application_Receipt_Date- Applicant_BirthDate)/365))

fintro_replace <- mutate(fintro_replace, Manager_Age_Joining = as.numeric((fintro_replace$Manager_DOJ - fintro_replace$Manager_DoB)/365) )

fintro_replace <- mutate(fintro_replace, Manager_Age_TimeApproving = as.numeric((fintro_replace$Application_Receipt_Date-fintro_replace$Manager_DoB)/365))# Manager Age at time of Approving a particular Job

fintro_replace <- mutate(fintro_replace,Manager_Experience = Manager_Age_TimeApproving - Manager_Age_Joining)


fintro_replace<- mutate(fintro_replace,Age_Difference = Manager_Age_TimeApproving - Applicant_Age)


summary(fintro_replace)

colnames(fintro_replace)


fintro_replace[,23] <- Replace_Median(fintro_replace[,23])
fintro_replace[,24] <- Replace_Median(fintro_replace[,24])
fintro_replace[,25] <- Replace_Median(fintro_replace[,25])
fintro_replace[,26] <- Replace_Median(fintro_replace[,26])
fintro_replace[,27] <- Replace_Median(fintro_replace[,27])

summary(fintro_replace)


#### Adviser

fintro_replace <- mutate(fintro_replace,Advisor_Business = Manager_Business - Manager_Business2)

fintro_replace <- mutate(fintro_replace,Advisor_Products_Sold = Manager_Num_Products - Manager_Num_Products2)


summary(fintro_replace)


