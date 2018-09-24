//Multivariable Analysis Module
//Vienna 2015

//Case-study: An outbreak of gastroenteritis in Stegen, Germany, 1998
*************************
****Part 1 (homework)****
*************************

**********
****Q3****
**********

cd "C:\type your folder\"
capture log close
log using tira_P1Q3, replace
use tira.dta, replace

//Codebook will display type, range, unit, unique values, missing
//and a tabulation of each variable

codebook
codebook ill
codebook salmon horseradish pork

//Codebook of the variables salmon, pork and horseradish show that a few records have the value 9.
//As stated in Q3, recode these values to missing.

recode salmon horseradish pork (9=.)

tab sex
tab sex , nolabel
tab beer, missing
tab beer 
summarize age, d
sort sex
by sex : summarize age
tab ill


//To show more than one table at a time
tab1 tira pork salmon


//Density plot is default for histogram.
//For plotting number of cases, use discrete and frequency options.

histogram age, discrete frequency

//To have nicer title:
histogram age, discrete frequency ytitle(Number of cases)

//You can save the graph using the graph export command:
graph export "Age distribution.png" , replace

//If you believe that two age groups are identifiable
//you may want to create a new variable with two age classes
//(< 30 years and above). The following shows one way to do it:   

recode age (0/30=0) (30/100=1), gen(agegroup)
tab agegroup

// alternatively
// egen agegroup = cut(age) , at(0,30,100) label

// or:
// egen agegroup = cut(age) , at(0,30,100) icodes

//the icodes option specifies that the codes 0, 1, 2, 3 etc., 
//be used instead of the left-hand ends of intervals (default).
//The option label specifies that the codes 0, 1, 2, 3 etc. are labelled with the left-hand ends of intervals).
 
//Creating labels:
//To have better understandable tables, you can attach label to data. Labels are displayed instead of codes in tables.

label define yesno 0 "No" 1 "Yes"
label value ill yesno
label value tira yesno
label value wmousse yesno
label value dmousse yesno

//You can label more than one variable at a time :
label value mousse beer redjelly fruitsalad tomato mince salmon horseradish chickenwin roastbeef pork yesno

label define cat 0 "None" 1 "One portion" 2 "Two portion" 3 "Three portion"
label value tportion cat
label value mportion cat

save tiraclean, replace
log close

**********
****Q4****
**********

//cd "C:\type your folder\"
capture log close
log using tira_P1Q4, replace
use tiraclean.dta, replace

use tiraclean.dta, clear
tab sex, missing
sum age, detail
tab agegroup, missing
tab ill, missing
tab dateonset if ill==1, missing

log close

**********
****Q5****
**********

//cd "C:\type your folder\"
capture log close
log using tira_P1Q5, replace
use tiraclean.dta, replace

//percentage of cases exposed 
tab ill sex, row
tab ill agegroup, row
tab ill tira, row
tab ill tportion, row
tab2 ill tira-pork , row firstonly

//dose response
tab tportion ill, row
recode tportion (0 = 0) (1 = 1) (2 = 2) (3 = 2) , generate (T2portion)
label define cat2 0 "None" 1 "One portion" 2 "Two or more portion"
label value T2portion cat
tab T2portion ill, row

//Interpret the results and identify the outbreak vehicle
cstable ill tira *mousse beer-pork, rr


log close

tab ill beer if tira==1

tab ill beer if tira==0


**********
****Q6****
**********

//cd "C:\type your folder\"
capture log close
log using tira_P1Q6, replace
use tiraclean.dta, replace

foreach val of varlist *mousse beer-pork {
 csinter ill `val', by(tira)
 } 
 
  *
 //to better see the CI for beer if tira ==1
 cs ill beer, by(tira)
  
 csinter ill beer if portion==0
 