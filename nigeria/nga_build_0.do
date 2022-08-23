* Project: WB COVID
* Created on: June 2021
* Created by: amf
* Edited by: amf
* Last edited: June 2021 
* Stata v.16.1

* does
	* reads in baseline Nigeria data
	* builds data for LD 
	* outputs HH income dataset

* assumes
	* raw Nigeria data

* TO DO:
	* complete


* **********************************************************************
* 0 - setup
* **********************************************************************

* define 
	global	root	=	"$data/nigeria/raw"
	global	export	=	"$data/nigeria/refined"
	global	logout	=	"$data/nigeria/logs"
	global  fies 	= 	"$data/analysis/raw/Nigeria"

* open log
	cap log 		close
	log using		"$logout/nga_reshape", append

* set local wave number & file number
	local			w = 0
	
* make wave folder within refined folder if it does not already exist 
	capture mkdir "$export/wave_0`w'" 
		
		
* ***********************************************************************
*  household data
* ***********************************************************************

* load data
	use 			"$root/wave_0`w'/sect1_harvestw4", clear

* rename other variables 
	rename 			hhid hhid_nga
	rename 			indiv ind_id 
	rename 			s1q2 sex_mem
	rename 			s1q3 relat_mem
	rename 			s1q4 age_mem
	rename 			s1q4a curr_mem
	replace 		curr_mem = 0 if curr_mem == 2 
	gen 			new_mem = 1 if curr_mem == . & sex_mem != .
			
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
	collapse		(sum) new_mem hhsize hhsize_adult hhsize_child hhsize_schchild ///
					(max) sexhh, by(hhid_nga)
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
	use 			"$root/wave_0`w'/sect3a_harvestw4", clear

* rename variables 
	rename 			hhid hhid_nga
	rename 			s3q4 wage_emp
	rename 			s3q5 farm_emp
	rename 			s3q6 bus_emp
	
	foreach 		var in farm bus wage {
	    replace 	`var'_emp = 0 if `var'_emp == 2
	}
	
* collapse to hh level
	collapse 		(max) farm_emp bus_emp wage_emp, by(hhid)
	
* save tempfile 
	tempfile 		temp1
	save 			`temp1'
	

* ***********************************************************************
*  other income  
* ***********************************************************************

* load data
	use 			"$root/wave_0`w'/sect6_harvestw4", clear
	
* rename variables	
	rename 			hhid hhid_nga
	rename 			s6q1__1 remit_inc 
	replace 		remit_inc = 1 if s6q1__3 == 1
	rename 			s6q1__2 asst_inc
	replace 		asst_inc = 1 if s6q1__4 == 1

	collapse 		(max) *_inc, by(hhid_nga)
	
* save tempfile 
	tempfile 		temp2a
	save 			`temp2a'

* load data
	use 			"$root/wave_0`w'/sect13_harvestw4", clear
	
* rename variables
	rename 			hhid hhid_nga
	rename 			s13q1 inc_
	replace 		source_cd = source_cd - 100
	keep 			hhid inc_ source_cd
	
* reshape data and rename/generate inc vars
	reshape 		wide inc_, i(hhid) j(source_cd)
	
	gen 			isp_inc = 0 if inc_1 == 2 | inc_2 == 2
	replace 		isp_inc = 1 if inc_1 == 1 | inc_2 == 1
	rename 			inc_3 pen_inc 
	replace 		pen_inc = 0 if pen_inc == 2
	rename 			inc_4 oth_inc 
	replace 		oth_inc = 0 if oth_inc == 2

	keep 			hhid *_inc
	
* save tempfile 
	tempfile 		temp2b
	save 			`temp2b'

	
* ***********************************************************************
* merge  
* ***********************************************************************	
	
* combine dataset 
	use 			`temp0', clear
	merge 			1:1 hhid using `temp1', assert(3) nogen
	merge 			1:1 hhid using `temp2a', nogen
	merge 			1:1 hhid using `temp2b', nogen
	lab def 		yesno 1 "Yes" 0 "No"
	ds *_inc *_emp
	foreach 		var in `r(varlist)' {
		lab val 	`var' yesno
	}
	
* add country & wave 
	gen 			wave = 0
	gen 			country = 3
	
* save round file
	save			"$export/wave_0`w'/r`w'", replace

/* END */		