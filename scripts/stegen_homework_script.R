# Multivariable Analysis Module
# Zagreb 2017
# 


# Packages required
required_packages <- c("foreign", "Hmisc", "epiDisplay", "epiR")

install.packages(required_packages)

for (i in seq(along = required_packages))
  library(required_packages[i], character.only = TRUE)



# Functions required
big.table <- function(data, useNA = "no") {
  count <- table(data, useNA = useNA)
  prop <- round(prop.table(count)*100, digits = 2)
  cumulative <- cumsum(prop)
  rbind(count,
        prop,
        cumulative) 
}


attack.rate <- function(table) {
  prop <- round(prop.table(table,1),digits = 2)
  denominator <- rowSums(table) 
  output <- cbind(Ill = table[,2], N = denominator, Proportions = prop[,2])
  return(output)
}



# Case-study: An outbreak of gastroenteritis in Stegen, Germany, 1998
#####Part 1 (homework)#####
#####Q3####

setwd("N:/MED/IMED-VIE/INFE/Public/CC-INFE-Schmid/EPIET/Learning R/R Case studies/MVA module 2016/Homework")
tira.data <- read.dta("tirav12.dta", convert.factors = FALSE)


# View structure of your data set
str(tira.data)
summary(tira.data)
describe(tira.data) # function from the Hmisc package that gives similar output to Codebook in Stata


# The Describe command showed that the variables salmon, pork and horseradish have few records with a value of 9.
# Recode these values to NA        
tira.data$pork[tira.data$pork == 9] <- NA
tira.data$salmon[tira.data$salmon == 9] <- NA
tira.data$horseradish[tira.data$horseradish == 9] <- NA


# Create summary tables with counts and proportions 
# Can do the following for each variable to obtain a table with counts, proportions, cumulative sum
sex <- table(tira.data$sex)
prop <- round(prop.table(sex)*100, digits = 2)
cumul <- cumsum(prop)
sextable <- rbind(sex,prop,cumul)
sextable


# Alternatively, you can use the big.table function which does all of the above in one line
big.table(tira.data$sex)

big.table(tira.data$beer)

#Make box plot for age
describe(tira.data$age)
boxplot(tira.data$age)

describe(tira.data$age[tira.data$sex == 0])
describe(tira.data$age[tira.data$sex == 1])

big.table(tira.data$ill)

#To create more than one table at a time
vars <- c("ill", "tira", "beer", "pork", "salmon")

output <- list() # create an empty list to hold the output of your loop
for (var in vars) {
  total <- big.table(tira.data[,var])
  output[[var]] <- total # assign the value of your tables to the output list
}

output


#Create a histogram of the age of those who attended the party and of those who fell ill
age_hist_cases <- hist(tira.data$age[tira.data$ill == 1],
     xlab = "Age",
     ylab = "No. of cases",
     main = "Histogram of the age of the cases ")


age_hist_all <- hist(tira.data$age,
     xlab = "Age",
     ylab = "No. of attendees of party",
     main = "Histogram of the age of those who attended the party")


#You can save the graph by stating where it should be saved first:
jpeg(filename = "N:/MED/IMED-VIE/INFE/Public/CC-INFE-Schmid/EPIET/Fellows portfolios/2015_Patrick Keating/R scripts/MVA/pre_exercise/graphs/age_all.jpeg")

age_hist_all <- hist(tira.data$age,
                     xlab = "Age",
                     ylab = "No. of attendees of party",
                     main = "Histogram of the age of those who attended the party")
dev.off()


# If you believe that two age groups are identifiable
# you may want to create a new variable with two age classes
# (< 30 years and above). The following shows one way to do it:   

# by using ifelse
tira.data$agegroup <- ifelse(tira.data$age >= 30, 1, 0)

# Alternatively
# by using findInterval ( levels start at 1, so we have to subtract 1)
tira.data$agegroup <- findInterval(tira.data$age, c(0,30,150)) - 1
# by using cut
tira.data$agegroup <- cut(tira.data$age, c(0,30,150), labels = FALSE) - 1



##### Q4 ########
# Describe the outbreak in terms of person and place
# Table 1: create a table for sex
big.table(tira.data$sex)

# Table 2: create a table for age
big.table(tira.data$agegroup)

summary(tira.data$age) # to obtain the mean, median age and range

# Table 3: create a table for attack rate
big.table(tira.data$ill)

# Table 4: create a table for date of onset of illness

big.table(tira.data$dateonset)


##### Q5 ########
# percentage of cases exposed
# Example: percentage of cases exposed to tira
count <- table(tira.data$tira,tira.data$ill)# the first element will be rows and the 2nd will be columns
prop <- round(prop.table(count,1),digits = 2) # # Here we select row % by including ,1 in the prop.table section
denominator <- rowSums(count) # total of rows in table
tira <- cbind(Ill = count[,2], N = denominator, Proportions = prop[,2]) #take the count and proportion date only where ill==1)


# Or you can do this using a loop: Percentage of cases exposed to each of the key exposure variables
# Below are two different ways that you can loop over your key exposure variables

# 1. List all of your variables and loop over them

vars <- c("tira", "tportion", "wmousse", "dmousse", "mousse", "mportion", "beer", "redjelly", "fruitsalad", "tomato", "mince", "salmon", "horseradish", "chickenwin", "roastbeef", "pork","agegroup")


# 2. List the column numbers and loop over them

vars <- colnames(tira.data[,6:22])

output2 <- list()
for (var in vars) {
  count <- table(tira.data[,var], tira.data$ill) # The exposure will be in the rows and the outcome in the column
  prop <- round(prop.table(count,1)*100,digits = 2) # Here we select row % by including ,1 in the prop.table section
  denominator <- rowSums(count) # Obtain the total number of each of the rows
  output2[[var]] <- cbind(Ill = count[,2], N = denominator, Proportions = prop[,2])
}

output2


### Dose response of tportion###
count <- table(tira.data$tportion,tira.data$ill, deparse.level = 2)
prop <- round(prop.table(count,1),digits = 2) 
denominator <- rowSums(count) 
tportion <- cbind(Ill = count[,2], N = denominator, Proportions = prop[,2]) 
tportion

# Recode 3 portions of tportion as 2 portions
tira.data$tportion2 <- tira.data$tportion
tira.data$tportion2[tira.data$tportion2 == 3] <- 2

# Calculate counts, proportions and sum of recoded tportion2
count <- table(tira.data$tportion2,tira.data$ill, deparse.level = 2)
prop <- round(prop.table(count,1),digits = 2) 
denominator <- rowSums(count) 
tportion2 <- cbind(Ill = count[,2], N = denominator, Proportions = prop[,2]) 
tportion2


##Interpret the results and identify the outbreak vehicle
## Univariable analysis
# A straightforward approach to obtaining RRs for each exposure variable is to use a user-written command called single variable analysis.v.02 (developed by Daniel Gardiner Cohort 2015)

source("single.variable.analysis.v0.2.R") # open this package to create output similar to cstable in Stata

# specify your exposures of interest i.e. tira-pork
vars <- c("tira", "wmousse", "dmousse", "mousse", "beer", "redjelly", "fruitsalad", "tomato", "mince", "salmon", "horseradish", "chickenwin", "roastbeef", "pork")


#NB. click on "sva" in your global environment to view Dan's source code and read his explanations

a <- sva(tira.data, outcome = "ill", exposures = c(vars), measure = "rr", verbose = TRUE)

a

##### Q6 ########
## Stratified analysis

# Outcome and exposure variables of interest need to be factor/categorical variables prior to performing stratified analysis

vars <- colnames(tira.data[,c(2,6,8:10,12:21)])

for (var in vars) {
  tira.data[,var] <- factor(tira.data[,var],levels = c(1,0)) # here we reorder the levels of the variable so that 1 comes first and 0 second
}


# Stratify by exposure to tiramisu
# Select your variables of interest by referring to the column name
# below, we select wmousse to mousse and beer to pork)
vars <- colnames(tira.data[,c(8:10,12:21)]) 

output3 <- list()

# 3 way table with variable of interest, outcome and stratifiying variable in that order
for (var in vars) {
  a <- table(tira.data[,var], tira.data$ill, tira.data$tira)
  # Use the epi.2by2 function to calculate RR  
  mh <- epi.2by2(a, method = "cohort.count")
  # Identify the elements of the mh table of interest and append together 
  resultstable <- rbind(mh$massoc$RR.crude.wald, 
                          mh$massoc$RR.strata.wald, 
                          mh$massoc$RR.mh.wald)
  # Create labels for each row of the results table
  rownames(resultstable) <- c("Crude", "Strata 1", "Strata 0", "MH")
  output3[[var]] <- resultstable
}

output3 # Gives the main output including crude, stratum-specific and adjusted RR


# #######Appendix stuff#######
# 
# ## Creating labels:
# ##  To have better understandable tables, you can attach labels to data. Labels are displayed instead of codes in tables.
# tira.data$agegroup <- factor(tira.data$agegroup, levels = c(0,1), labels = c("<30", ">=30"))
# 
# 
# ## You can label more than one variable at a time using a loop :
# vars <- c("ill", "tira", "wmousse", "dmousse")
# 
# for (var in vars) {
#   tira.data[[var]] <- factor(tira.data[[var]], levels = c(0,1), labels = c("No", "Yes"))
# }
# 
# 
# 
# 
# # to save individual objects
# saveRDS(tira.data, "tiradata.rds")
# # to save possibly the whole workspace
# save(list = "tira.data", file = "tiradata.sv")
# 
# 
# 
# # Making tables for manuscripts in R
# install.packages("ReporteRs")
# library(ReporteRs)
# 
# label_table <- function(X){
#   setFlexTableBorders(X,inner.vertical = borderProperties(style = "none"),inner.horizontal = borderProperties(style = "none"),outer.vertical = borderProperties(style = "none"),outer.horizontal = borderProperties(width = 2),body = T,header = T)
# }
# 
# label_footer <- function(X){
#   setFlexTableBorders(X,inner.vertical = borderProperties(style = "none"),inner.horizontal = borderProperties(style = "none"),outer.vertical = borderProperties(style = "none"),outer.horizontal = borderProperties(style = "none"),footer = T)
# }
# 
# # Making exportable tables
# # Table 1: create a table for sex
# sextable <- tab1(tira.data$sex) # requires epiDisplay
# sextable <- as.data.frame(sextable) # convert sextable object to a dataframe
# sextable <- sextable[,c(1:3)] # select columns of interest
# sextable$first.line <- c("Female", "Male", "Total")  # apply appropriate variable labels
# 
# table1 <- FlexTable(sextable,header.columns = F)
# table1 <- addHeaderRow(table1, text.properties = textBold(), value = c("Sex","N","%"), colspan = c(1,1,1)) # Add a new row, specify the text for each column and over how many columns the text should be repeated and that the text should be bold
# table1 <- label_footer(table1) #  removed the label around the footer
# table1 <- label_table(table1) # format the table so that only the top and lower parts are neatly formatted
# table1
# 
# 
# # Produce similar output to Stata for tables
# install.packages("gmodels")
# library(gmodels)
# 
# a <- CrossTable(tira.data$ill,tira.data$sex, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE) # you can decide which elements to include in the table
# ##End of Appendix stuff#####
