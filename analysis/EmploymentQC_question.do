* Project: diversification
* Created on: spring 2022
* Created by: amf
* Edited by: alj
* Last edit: august 2022 
* Stata v.17.0 / 16.1

* does
	* explores: 
	*** QUESTION: Why do respondents never have unique employment sectors within their households?

* assumes
	* see below: need to insert appropriate file paths 

* TO DO:
	* complete
	
* **********************************************************************
* **********************************************************************

* USE INDIVIDUAL EMPLOYMENT DATA FOR MALAWI ROUND 5
	use				"INSERT FILE PATH HERE /sect6_Employment_Other_r5", clear

* generate individual employment sector variables based on Section 6 Question 6_1
	gen 			wage_inc_ind = 1 if s6q6_1 == 4 | s6q6_1 == 5
	replace 		wage_inc_ind = 0 if s6q6_1 < 4	
	gen 			farm_inc_ind = 1 if s6q6_1 == 3
	replace 		farm_inc_ind = 0 if s6q6_1 != 3 & s6q6_1 < .
	gen 			bus_inc_ind = 1 if s6q6_1 < 3
	replace 		bus_inc_ind = 0 if s6q6_1 >= 3 & s6q6_1 < .
	
* collapse to household level 
	collapse 		(max) *_inc_ind, by(HHID)
	
* merge respondent employment for round 5
	merge 			1:1 HHID using "INSERT FILE PATH HERE /sect6a_Employment2_r5.dta", assert(3) nogen

* generate respondent employment sector variables based on Section 6 Question 6
	gen 			wage_inc_resp = 1 if s6q6 == 4 | s6q6 == 5
	replace 		wage_inc_resp = 0 if s6q6 < 4	
	gen 			farm_inc_resp = 1 if s6q6 == 3
	replace 		farm_inc_resp = 0 if s6q6 != 3 & s6q6 < .
	gen 			bus_inc_resp = 1 if s6q6 < 3
	replace 		bus_inc_resp = 0 if s6q6 >= 3 & s6q6 < .
	
* combine employment variables for respondents and other/ind household members
	gen 			wage_inc = 1 if wage_inc_resp == 1 | wage_inc_ind == 1
	gen 			farm_inc = 1 if farm_inc_resp == 1 | farm_inc_ind == 1
	gen 			bus_inc = 1 if bus_inc_resp == 1 | bus_inc_ind == 1
	
* summarize income variables 	
	tab 			wage_inc wage_inc_ind
	tab 			farm_inc farm_inc_ind			
	tab 			bus_inc bus_inc_ind		
	
/* The number of yes responses in the household income variable (respondent and other individuals combined) is exactly the same as the individual data. Therefore, respondent data never adds any additional income information for the household (i.e., respondents never have a unique sector of employment/income source among members in their household). The same occurs in other waves with both individual and respondent employment data such as Nigeria rounds 5 and 10. Why is this? */

* **********************************************************************
* **********************************************************************	