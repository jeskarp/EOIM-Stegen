//Multivariable Analysis Module
//Vienna 2016

//Case-study: An outbreak of gastroenteritis in Stegen, Germany, 1998
******************
****Part 2 MVA****
******************

**********
****Q8****
**********

cd "C:\type your folder\"
capture log close
log using tira_Q8, replace
use tiraclean.dta, replace

//Logistic regression using tiramisu as dichotomous variable:
//Using the logit command, you obtain the regression coefficient
logit ill tira, nolog

//The OR for tiramisu is:
di exp(4.364143)
 
//The logit command with the or option or the logistic command gives you the ORs.
logistic ill tira

//Logistic regression using tportion as categorical variable:
logistic ill i.tportion

//to change the reference level 
char tportion[omit] 3
xi: logistic ill i.tportion

//to change the reference level back
char tportion[omit]

 
//tportion without indicating that it is categorical
logistic ill tportion

//Adding a second variable to the model
logistic ill tira beer

//Adding a third variable to the model
logistic ill tira beer mousse


//Adding variables in a step-by-step fashion
//using the likelihood ratio test lrtest to compare different models

//making sure all the variables has the same number of observations
//manually
*drop if ill == .
*drop if tira == .
//...
*drop if mportion == .


//or using the ado-file dropmissing that you may need to download
dropmissing ill tira age dmousse wmousse beer fruitsalad redjelly tportion mportion salmon mince tomato horseradish chickenwin roastbeef pork
save tiranomissing, replace

//only one independent variable - model 1
logistic ill tira
estimates store m1
               //(will store estimates in the model m1)

//Then do a second model with one additional variable (beer) - model 2
logistic ill tira beer
estimates store m2
               // (will store estimates in the model m2)

//Then test for the difference in log likelihood
lrtest m2 m1

//model 3
//you would have to repeat model 2 for storing the estimates
logistic ill tira beer
estimates store m2
logistic ill tira beer mousse
estimates store m3
lrtest m2 m3

//model 4
//you would have to repeat model 3 for storing the estimates
logistic ill tira beer mousse
estimates store m3
logistic ill tira beer mousse sex
estimates store m4
lrtest m3 m4

//Assessing the fit of each model
glm ill tira, link(logit) family(binomial) eform 
estat ic
glm ill tira beer, link(logit) family(binomial) eform
estat ic


**********
****Q9****
**********

cd "C:\type your folder\"
capture log close
log using tira_Q9, replace
use tiranomissing.dta, replace

//stratified analysis (using logistic regression) to check for interactions.

//Fisrt lets remember what we saw in the stratified analysis
*ccinter ill beer, by(tira)

//You can obtain the same ORs using logistic regression:
logistic ill beer if tira==0
logistic ill beer if tira==1

//generate a dummy variable 
*gen tira_beer=tira*beer
*logistic ill tira beer tira_beer

logistic ill tira##beer 
di 0.32*125.13*0.99

lincom 1.tira + 1.beer + tira_beer

//check if the interaction improves the model 
logistic ill tira beer
estimates store model0
logistic ill tira##beer
estimates store model1
lrtest model0 model1

//Assessing the fit of each model
glm ill tira beer, link(logit) eform nolog
estat ic
glm ill tira##beer, link(logit) eform nolog
estat ic


********************
****optional Q10****
********************

use tiranomissing.dta, replace
glm ill tira, family(binomial) link(log) eform 
estimates store model0
glm ill tira beer, family(binomial) link(log) eform 
estimates store model1
lrtest model0 model1

glm ill tira beer, family(binomial) link(log) eform 
estimates store model2
glm ill tira beer mousse, family(binomial) link(log) eform 
estimates store model3
lrtest model2 model3

//glm ill tira##beer, family(binomial) link(log) eform


