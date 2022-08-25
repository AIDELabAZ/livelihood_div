* Project: diversification
* Created on: spring 2022
* Created by: amf
* Edited by: alj
* Last edit: august 2022 
* Stata v.17.0 / 16.1

* does
	* explores: 
	***  Why do so many Ethiopia households report zero income sources when other countries do not?

* assumes
	* see below: need to insert appropriate file paths 

* TO DO:
	* complete
	
* **********************************************************************
* **********************************************************************

* USE ETHIOPIA ROUND 1 PHONE SURVEY MICRO DATA *
	use 			"INSERT FILE PATH HERE /200610_WB_LSMS_HFPM_HH_Survey-Round1_Clean-Public_Microdata ", clear	

* rename 10 income variables from section 6	
	rename			lc1_farm farm_inc
	rename			lc1_bus bus_inc
	rename			lc1_we wage_inc
	rename			lc1_rem_dom rem_dom
	rename			lc1_rem_for rem_for
	rename			lc1_isp isp_inc
	rename			lc1_pen pen_inc
	rename			lc1_gov gov_inc
	rename			lc1_ngo ngo_inc
	rename			lc1_other oth_inc

* replace negative values with missing (represent refused, don't know, etc.)
	foreach 		var in farm_inc bus_inc wage_inc rem_dom rem_for ///
						isp_inc pen_inc gov_inc ngo_inc oth_inc {
		replace 		`var' = . if `var' < 0
	}
	
* generate count of income sources	
	egen 			inc_count = rowtotal(farm_inc bus_inc wage_inc rem_dom rem_for ///
						isp_inc pen_inc gov_inc ngo_inc oth_inc) 
	
* replace income count with missing if any income variables are missing
	replace 		inc_count = . if farm_inc >= . | bus_inc >= . | ///
						wage_inc >= .| rem_dom >= . | rem_for >= . | ///
						isp_inc >= . | pen_inc >= . | gov_inc >= . | ///
						ngo_inc >= . | oth_inc >= .

* summarize income count 						
	tab 			inc_count 	
	
/*  Results in 311 households reporting 0 income sources
	Other Ethiopia rounds also report high frequences of 0 responses: 
		Round 2: 545 out of 3104 responses
		Round 3: 542 out of 3056 responses
		Round 4: 375 out of 2877 responses
		Round 5: 424 out of 2768 responses
		Round 6: 362 out of 2691 responses
*/


* Other countries have no or very few households that report 0 income sources	

* USE MALAWI ROUND 1 PHONE SURVEY DATA *
	use 			"INSERT FILE PATH HERE /sect7_Income_Loss_r1", clear	
	drop 			income_source_os
	
*reshape data
	reshape 		wide s7q1 s7q2, i(y4_hhid HHID) j(income_source)	
	
* rename 11 income variables from section 7 (additional income source option is assistance inc)
	rename 			s7q11 farm_inc
	rename 			s7q12 bus_inc
	rename 			s7q13 wage_inc
	rename 			s7q14 rem_for
	rename 			s7q15 rem_dom
	rename 			s7q16 asst_inc
	rename 			s7q17 isp_inc
	rename 			s7q18 pen_inc
	rename 			s7q19 gov_inc
	rename 			s7q110 ngo_inc
	rename 			s7q196 oth_inc
	
* replace "no" responses with 0 rather than 2
	foreach 		var in farm_inc bus_inc wage_inc rem_dom rem_for ///
						isp_inc pen_inc gov_inc ngo_inc oth_inc asst_inc {
		replace 		`var' = 0 if `var' == 2
	}
	
* generate count of income sources	
	egen 			inc_count = rowtotal(farm_inc bus_inc wage_inc rem_dom rem_for ///
						isp_inc pen_inc gov_inc ngo_inc oth_inc asst_inc) 
	
* replace income count with missing if income variables are missing
	replace 		inc_count = . if farm_inc >= . | bus_inc >= . | ///
						wage_inc >= .| rem_dom >= . | rem_for >= . | ///
						isp_inc >= . | pen_inc >= . | gov_inc >= . | ///
						ngo_inc >= . | oth_inc >= . | asst_inc >= .

* summarize income count 							
	tab 			inc_count 	
	
/* no 0 responses in this round or any other */	


* **********************************************************************
* **********************************************************************