* Project: WB COVID
* Created on: June 2021
* Created by: amf
* Edited by: amf
* Last edit: June 2021 
* Stata v.16.1

* does
	* reads in baseline Ethiopia data
	* builds data for LD 
	* outputs HH income dataset

* assumes
	* raw Ethiopia data

* TO DO:
	* complete

	
* **********************************************************************
* 0 - setup
* **********************************************************************

* define 
	global	root	=	"$data/ethiopia/raw"
	global	export	=	"$data/ethiopia/refined"
	global	logout	=	"$data/ethiopia/logs"
	global  fies 	= 	"$data/analysis/raw/Ethiopia"

* open log
	cap log 		close
	log using		"$logout/eth_build", append

* set local wave number & file number
	local			w = 0
	
* make wave folder within refined folder if it does not already exist 
	capture mkdir 	"$export/wave_0`w'" 

	
* ***********************************************************************
*  household data
* ***********************************************************************

* load data
	use 			"$root/wave_0`w'/HH/sect1_hh_w4", clear

* rename other variables 
	rename 			household_id hhid_eth
	rename 			individual_id ind_id 
	rename 			s1q04 new_mem
	rename 			s1q05 curr_mem
	replace 		curr_mem = 1 if new_mem == 1
	replace 		curr_mem = 1 if curr_mem == .
	rename 			s1q02 sex_mem
	rename 			s1q03a age_mem
	rename 			s1q03b age_month_mem
	rename 			s1q01 relat_mem
	
	foreach 		var in new_mem curr_mem {
		replace 		`var' = 0 if `var' == 2 
	}
						
* generate counting variables
	gen				hhsize = 1 if curr_mem == 1
	gen 			hhsize_adult = 1 if curr_mem == 1 & age_mem > 18 & age_mem < .
	gen				hhsize_child = 1 if curr_mem == 1 & age_mem < 19 & age_mem != . 
	gen 			hhsize_schchild = 1 if curr_mem == 1 & age_mem > 4 & age_mem < 19  
	
* create hh head gender
	gen 			sexhh = . 
	replace			sexhh = sex_mem if relat_mem == 1
	label var 		sexhh "Sex of household head"
	
* collapse data
	collapse		(sum) hhsize hhsize_adult hhsize_child hhsize_schchild new_mem ///
					(max) sexhh, by(hhid_eth)
	replace 		new_mem = 1 if new_mem > 0 & new_mem < .
	lab var			hhsize "Household size"
	lab var 		hhsize_adult "Household size - only adults"
	lab var 		hhsize_child "Household size - children 0 - 18"
	lab var 		hhsize_schchild "Household size - school-age children 5 - 18"

* save tempfile 
	tempfile 		temp0
	save 			`temp0'

	
* ***********************************************************************
*  labor & time use  
* ***********************************************************************

* load data
	use 			"$root/wave_0`w'/HH/sect4_hh_w4", clear
	drop 			if s4q00 == 2
	
* rename variables 	
	rename 			household_id hhid_eth 
	rename 			s4q05 farm_emp 
	rename 			s4q10 casual_emp
	rename 			s4q12 wage_emp
	
	foreach 		var in farm casual wage {
	    replace 	`var'_emp = 0 if `var'_emp == 2
	}
	
* collapse to hh level
	collapse 		(max) farm_emp casual_emp wage_emp, by(hhid)
	
* save tempfile 
	tempfile 		temp1
	save 			`temp1'

	
* ***********************************************************************
*  NFE income
* ***********************************************************************

* load data
	use 			"$root/wave_0`w'/HH/sect12b1_hh_w4", clear
	rename 			household_id hhid_eth
	
* rename variables 
	rename 			s12bq12 bus_months_op
	rename 			s12bq13 bus_days_op
	rename 			s12bq16 bus_avg_sales
	rename 			s12bq24 bus_perc_hh_inc
/*	
	keep bus_*

THIS DATA DOES NOT MAKE SENSE - include sales and review
make sure to consider months in operation when calculating income based on avg monthly sales 
asdfd
*/

* generate number of NFEs and indicator if any
	gen 			num_bus = 1 
	collapse 		(sum) num_bus, by(hhid_eth)
	gen 			bus_inc = 1 if num_bus > 0 
	
* save tempfile 
	tempfile 		temp2
	save 			`temp2'	
	
	
* ***********************************************************************
*  other income  
* ***********************************************************************
	
* load data
	use 			"$root/wave_0`w'/HH/sect13_hh_w4", clear	

* rename variables
	rename 			household_id hhid_eth
	rename 			s13q01 inc_
	replace 		source_cd = source_cd - 100
	keep 			hhid inc_ source_cd
	
* reshape data and rename/generate inc vars
	reshape 		wide inc_, i(hhid) j(source_cd)
	
	rename 			inc_1 remit_inc
	replace 		remit_inc = 0 if remit_inc == 2
	gen 			asst_inc = 0 if inc_2 == 2 | inc_3 == 2
	replace			asst_inc = 1 if inc_2 == 1 | inc_3 == 1
	rename 			inc_5 pen_inc
	replace 		pen_inc = 0 if pen_inc == 2
	gen 			isp_inc = 0 if inc_4 == 2 | inc_6 ==2 | inc_7 == 2 | ///
						inc_8 == 2 | inc_9 == 2 | inc_10 == 2
	replace			isp_inc = 1 if inc_4 == 1 | inc_6 == 1 | inc_7 == 1 | ///
						inc_8 == 1 | inc_9 == 1 | inc_10 == 1
	gen 			oth_inc = 0 if inc_11 == 2 | inc_12 == 2 | inc_13 == 2 | ///
						inc_14 == 2 
	replace			oth_inc = 1 if inc_11 == 1 | inc_12 == 1 | inc_13 == 1 | ///
						inc_14 == 1
	
	keep 			hhid *_inc
	
* save tempfile 
	tempfile 		temp3
	save 			`temp3'
		
	
* ***********************************************************************
* merge  
* ***********************************************************************	
	
* combine dataset 
	use 			`temp0', clear
	merge 			1:1 hhid using `temp1', assert(3) nogen
	merge 			1:1 hhid using `temp2', nogen
	merge 			1:1 hhid using `temp3', assert(3) nogen
	lab def 		yesno 1 "Yes" 0 "No"
	ds *_inc *_emp
	foreach 		var in `r(varlist)' {
		lab val 	`var' yesno
	}
	
* add country & wave 
	gen 			wave = 0
	gen 			country = 1
	
* save round file
	save			"$export/wave_0`w'/r`w'", replace

/* END */	