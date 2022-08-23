* Project: WB COVID
* Created on: June 2021
* Created by: amf
* Edited by: amf
* Last edit: June 2021 
* Stata v.16.1

* does
	* reads in baseline Uganda data
	* builds data for LD 
	* outputs HH income dataset

* assumes
	* raw Uganda data

* TO DO:
	* complete
	
	
* **********************************************************************
* 0 - setup
* **********************************************************************

* define
	global	root	=	"$data/uganda/raw"
	global	fies	=	"$data/analysis/raw/Uganda"
	global	export	=	"$data/uganda/refined"
	global	logout	=	"$data/uganda/logs"

* open log
	cap log 		close
	log using		"$logout/uga_build", append
	
* set local wave number & file number
	local			w = 0
	
* make wave folder within refined folder if it does not already exist 
	capture mkdir "$export/wave_0`w'" 	
	
	
* ***********************************************************************
*  household data
* ***********************************************************************
	
* load data
	use 			"$root/wave_0`w'/Household/GSEC2", clear

* rename other variables 
	rename 			hhid baseline_hhid
	rename 			PID ind_id 
	
	rename 			h2q7 curr_mem
	replace 		curr_mem = 1 if curr_mem < 5
	replace 		curr_mem = 0 if curr_mem > 4 & curr_mem < . 
	rename 			h2q8 age_mem
	rename 			h2q3 sex_mem
	rename 			h2q4 relat_mem
	replace 		curr_mem  = 0 if curr_mem  == 2 
						
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
	collapse		(sum) hhsize hhsize_adult hhsize_child hhsize_schchild ///
					(max) sexhh, by(baseline_hhid)
	lab var			hhsize "Household size"
	lab var 		hhsize_adult "Household size - only adults"
	lab var 		hhsize_child "Household size - children 0 - 18"
	lab var 		hhsize_schchild "Household size - school-age children 5 - 18"

	drop 			if hhsize == 0
	
* save tempfile 
	tempfile 		temp0
	save 			`temp0'	
	
	
* ***********************************************************************
*  labor & time use  
* ***********************************************************************
		
* load data
	use 			"$root/wave_0`w'/Household/GSEC8", clear
	
* rename other variables 
	rename 			hhid baseline_hhid
	rename 			s8q04 wage_emp
	rename 			s8q06 bus_emp 
	replace 		bus_emp = 1 if s8q08 == 1
	rename 			s8q12 farm_emp
	
	foreach 		var in farm bus wage {
	    replace 	`var'_emp = 0 if `var'_emp == 2
	}	
	
* collapse to hh level
	collapse 		(max) farm_emp bus_emp wage_emp, by(baseline_hhid)
	
* save tempfile 
	tempfile 		temp1
	save 			`temp1'
		
	
* ***********************************************************************
*  other income  
* ***********************************************************************
		
* load data
	use 			"$root/wave_0`w'/Household/GSEC7_1", clear	

* rename/generate inc vars	
	rename 			hhid baseline_hhid
	gen 			isp_inc = 0 if s11q04__0 == 0 | s11q04__1 == 0 | ///
						s11q04__2 == 0 | s11q04__3 == 0 | s11q04__4 == 0 | ///
						s11q04__5 == 0 | s11q04__6 == 0 | s11q04__7 == 0 | ///
						s11q04__8 == 0
	replace 		isp_inc = 1 if s11q04__0 == 1 | s11q04__1 == 1 | ///
						s11q04__2 == 1 | s11q04__3 == 1 | s11q04__4 == 1 | ///
						s11q04__5 == 1 | s11q04__6 == 1 | s11q04__7 == 1 | ///
						s11q04__8 == 1	
	rename 			s11q04__9 pen_inc
	rename 			s11q04__10 remit_inc 
	replace 		remit_inc = 1 if s11q04__11 == 1
	rename 			s11q04__12 oth_inc
	replace 		oth_inc = 1 if s11q04__13 == 1
	
	keep 			baseline_hhid *_inc	
	
* save tempfile 
	tempfile 		temp2
	save 			`temp2'
		
	
* ***********************************************************************
* merge  
* ***********************************************************************	
	
* combine dataset 
	use 			`temp0', clear
	merge 			1:1 baseline_hhid using `temp1', nogen
	merge 			1:1 baseline_hhid using `temp2', nogen
	lab def 		yesno 1 "Yes" 0 "No"
	ds *_inc *_emp
	foreach 		var in `r(varlist)' {
		lab val 	`var' yesno
	}

* add country & wave 
	gen 			wave = 0
	gen 			country = 4
	gen 			hhid_uga = _n //temp hhid, not sure how to match
	
* save round file
	save			"$export/wave_0`w'/r`w'", replace

/* END */			