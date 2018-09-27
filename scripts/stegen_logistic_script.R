# Multivariable Analysis Module
# Zagreb 2017
#

# Packages required
required_packages <- c("foreign", "epiR", "multcomp", "broom")

install.packages(required_packages)

for (i in seq(along = required_packages))
  library(required_packages[i], character.only = TRUE)

#Case-study: An outbreak of gastroenteritis in Stegen, Germany, 1998

#### Part 2 MVA ####
##### Q8 #####

setwd("N:/MED/IMED-VIE/INFE/Public/CC-INFE-Schmid/EPIET/Learning R/R Case studies/MVA module 2016/Logistic Regression")

tira.data <- read.dta("tiraclean.dta", convert.factors = FALSE)


# For regression it is important that your variables are ordered 0,1 - i.e. not as (1,0) 
# Logistic regression using tiramisu as dichotomous variable:
# Using the glm command with a logit link, you obtain the regression coefficients

model1 <- glm(ill~tira,
              data = tira.data,
              family = binomial(link = "logit"))

model1op <- tidy(model1, exponentiate = TRUE, conf.int = TRUE)
model1op


# Logistic regression using tportion as categorical variable:
# For categorical variables you can create a dummy variable for each level of the variable. 
# Below we see that there are 4 levels of tportion, from 0 to 3 portions
    #The rownames of the output saying "factor" is because glm believes the var name is "factor(tportion)"
    #i.e. treating it as a categorical variable (similar to "i." in STATA)
table(tira.data$tportion)

model2 <- glm(ill~factor(tportion), 
              data = tira.data,
              family = binomial(link = "logit"))

model2op <- tidy(model2, exponentiate = TRUE, conf.int = TRUE)
model2op


# Change the reference level to 3 portions of tiramisu, which is the 4th level of the variable ( !: reference designates the index and not the value)
tira.data$tportion2 <- relevel(factor(tira.data$tportion), ref = 4)

model3 <- glm(ill ~ tportion2, 
              data = tira.data,
              family = binomial(link = "logit"))

model3op <- tidy(model3, exponentiate = TRUE)
model3op

table(tira.data$tportion2)


# Repeat the regression without specifying that tportion is categorical
model4 <- glm(ill ~ tportion,
              data = tira.data,
              family = binomial(link = "logit"))

model4op <- tidy(model4, exponentiate = TRUE, conf.int = TRUE)
model4op



# Adding a second variable to the model
model5 <- glm(ill ~ tira + beer,
              data = tira.data,
              family = binomial(link = "logit"))

model5op <- tidy(model5, exponentiate = TRUE, conf.int = TRUE)
model5op


# Adding a third variable to the model
# Can update model 5 with the new variable only
model6 <- update(model5,
                 formula = ill ~ tira + beer + mousse)

model6op <- tidy(model6, exponentiate = TRUE, conf.int = TRUE)
model6op




# Drop observations with missing data 
# List all the variables for which we want to drop the missing values
vars <- c("ill", "tira", "age", "dmousse", "wmousse", "beer", "fruitsalad", "redjelly", "tportion", "mportion", "salmon", "mince", "tomato", "horseradish", "chickenwin", "roastbeef", "pork")

# Create a new data set and assign the original data set to it!
tira.data.new <- tira.data


for (var in vars) {
  tira.data.new <- tira.data.new[!is.na(tira.data.new[,var]),] #Only keep the values of tira.data.new that are NOT equal to NA
}
# Should have 239 observations after this loop


## Stepwise Multivariable ####

# Only one independent variable

model7 <- glm(ill~tira,
                 data = tira.data.new,
                 family = binomial(link = "logit"))
                 

#  Then do a second model with one additional variable (beer) 
model8 <- update(model7, 
                    formula = ill ~ tira +beer)

#  Then test for the difference in the 2 models using anova test
anova(model7, model8, test = "Chisq")

# Add a third independent variable
model9 <- update(model8,
                    formula = ill ~ tira + beer + mousse)

#  Then test for the difference in the 2 models using anova test
anova(model8, model9, test = "Chisq")

# Add a fourth independent variable
model10 <- update(model9, 
                    formula = ill ~ tira + beer + mousse + sex)

#  Then test for the difference in the 2 models using anova test
anova(model9, model10, test = "Chisq")


###Assessing the fit of each model using the AIC ####
#It is possible to get AIC values from glm - however for a nice comparative overview:
AIC(model7, model8, model9, model10)


##### Q9 ########

# Stratified analysis (using logistic regression) to check for interactions. 
# Reminder of what we saw in the stratified analysis: i.e stratifying exposure to beer by tira

a <-  table(tira.data.new$beer, tira.data.new$ill, tira.data.new$tira)
mh <- epi.2by2(a, method = "case.control")

# To obtain the stratum specific OR estimates, we do the following
mh$massoc$OR.strata.wald


# You can obtain the same ORs using logistic regression:
# No exposure to Tiramisu
tira0 <- glm(ill ~ beer, 
             data = tira.data.new[tira.data.new$tira == 0,],
             family = binomial(link = "logit"))

tira0op <- tidy(tira0, exponentiate = TRUE, conf.int = TRUE)
tira0op 


# Exposure to Tiramisu
tira1 <- glm(ill ~ beer, 
             data = tira.data.new[tira.data.new$tira == 1,],
             family = binomial(link = "logit"))

tira1op <- tidy(tira1, exponentiate = TRUE, conf.int = TRUE)
tira1op


# Check for interaction between beer and tira
tirabeer <- glm(ill ~ beer*tira, 
                data = tira.data.new,
                family = binomial(link = "logit"))

tirabeerop <- tidy(tirabeer, exponentiate = TRUE, conf.int = TRUE)
tirabeerop




## Use the function glht from the package multcomp to obtain the same results as lincom ####

names(coef(tirabeer)) #Extract the coefficient names 

# linfct specifies the required combination: In this case we want beer and tira and beer:tira=0
# effect of tira and beer in those who didn't consume either

summ10 <- summary(glht(tirabeer, linfct = c("beer + tira + beer:tira = 0")))

ci <- confint(summ10) 

# Put together the table with the exponent of the coefficients and CI, and p value
table10 <- round(cbind(OR = exp(coef(summ10)),
                       Interval = exp(ci$confint),
                       Pvalue = summ10$test$pvalues),
                       digits = 3)

table10

# check if the interaction improves the model 
nointeract <- glm(ill~ tira + beer, 
                  data = tira.data.new,
                  family = binomial(link = "logit"))


interact <- glm(ill~ tira*beer, 
                data = tira.data.new,
                family = binomial(link = "logit"))


anova(nointeract, interact, test = "Chisq")

# Assessing the fit of each model
AIC(nointeract, interact)


##### Optional Q10 ########

# Binomial regression with one independent variable
bin1 <- glm(ill ~ tira, 
            data = tira.data.new,
            family = binomial(link = "log"))

bin1op <- tidy(bin1, exponentiate = TRUE, conf.int = TRUE)
bin1op 



# With two independent variables
bin2 <- update(bin1,
               formula = ill ~ tira + beer)

bin2op <- tidy(bin2, exponentiate = TRUE, conf.int = TRUE)
bin2op 



#  Then test for the difference in the 2 models using anova test
anova(bin1, bin2, test = "Chisq")

# With three independent variables
# The standard approach would be to add the 3rd variable as normal
bin3 <- glm(ill ~ tira + beer + mousse,
            data = tira.data.new,
            family = binomial(link = "log"))



# if the model does not converge and requires starting points, then you need to provide a starting point for the intercept and each independent variable to be added
# These can be obtained from the simpler model as below
coefini <- coef(glm(ill ~ tira + beer,
                   data = tira.data.new,
                   family = binomial(link = "log")))

bin3 <- glm(ill ~ tira + beer + mousse,
           data = tira.data.new,
           family = binomial(link = "log"),
           start = c(coefini,0))

bin3op <- tidy(bin3, exponentiate = TRUE, conf.int = TRUE)
bin3op


# Assessing the fit of each model
AIC(bin1,bin2,bin3)


# check if the interaction improves the model 
nointeract_binom <- glm(ill~ tira + beer, 
                        data = tira.data.new,
                        family = binomial(link = "log"))


interact_binom <- glm(ill~ tira*beer, 
                      data = tira.data.new,
                      family = binomial(link = "log"))
                      # start = c(-2.9136, 2.7694, -0.2425, 0))


anova(nointeract_binom, interact_binom, test = "Chisq")

# Assessing the fit of each model
AIC(nointeract_binom, interact_binom)
