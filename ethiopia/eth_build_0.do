* Project: diversification
* Created on: June 2021
* Created by: amf
* Edited by: alj
* Last edit: 22 March 2022
* Stata v.17.0

* does
	* reads in baseline Ethiopia data
	* builds data for LD 
	* outputs HH income dataset

* assumes
	* raw Ethiopia data

* TO DO:
	* complete

************************************************************************
**# - setup
************************************************************************

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

	
*************************************************************************
**#  household data
*************************************************************************

* load data
	use 			"$root/wave_0`w'/HH/sect1_hh_w4", clear

* rename other variables 
	rename 			household_id hhid_eth
	rename 			individual_id ind_id 
	rename			saq01 region
	rename			saq02 zone
	rename			saq03 woreda
	rename 			saq06 kebele
	rename 			ea_id ea
	rename 			pw_w4 baseline_weight
	
	* make regions consistent with COVID rounds
	replace 		region = 8 if region == 12
	replace 		region = 9 if region == 13
	replace 		region = 10 if region == 14
	replace 		region = 11 if region == 15
	replace 		region = region + 1000
	
	gen				sector = 2 if saq14 == 2
	replace			sector = 1 if saq14 == 1
	
	gen				curr_mem = 0 if s1q05 == 2
	replace 		curr_mem = 1 if curr_mem == . 
	gen				new_mem = 0 if s1q04 == 2
	gen				mem_left = 0 if s1q05 == 1
	replace			mem_left = 1 if s1q05 == 2
	replace 		curr_mem = 1 if new_mem == 1
	rename 			s1q02 sex_mem
	rename 			s1q03a age_mem
	rename 			s1q03b age_month_mem
	rename 			s1q01 relat_mem
	
	foreach 		var in new_mem curr_mem sex_mem {
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
						(max) sexhh, by(hhid_eth region zone woreda kebele ea sector saq14 baseline_weight)
	replace 		new_mem = 1 if new_mem > 0 & new_mem < .
	lab var			hhsize "Household size"
	lab var 		hhsize_adult "Household size - only adults"
	lab var 		hhsize_child "Household size - children 0 - 18"
	lab var 		hhsize_schchild "Household size - school-age children 5 - 18"

* check for unique identifier
	isid 			hhid_eth
	
* save tempfile 
	tempfile 		temp0
	save 			`temp0'

	
*************************************************************************
**#  labor & time use  
*************************************************************************

* load data
	use 				"$root/wave_0`w'/HH/sect4_hh_w4", clear
	drop 				if s4q00 == 2

* rename merge variables
	rename 				household_id hhid_eth
	rename				individual_id ind_id

* wage payments
	* rename indicator vars
		rename 			s4q33b wage_emp_0
		replace 		wage_emp_0 = 0 if wage_emp_0 == 2
		
	* gen conversion variables
		gen				days_per_month	= 365/12
		gen				weeks_per_month = 365/7/12
	
	* rename & reformat variables
		rename			s4q37 main_months
		rename			s4q38 main_weeks_per_month
		rename			s4q39 main_hrs_per_week
		rename			s4q40 main_pay
		rename			s4q41 main_pay_unit
	
	* estimate number of days worked per week (assuming 8 hour days)
		replace 		main_hrs_per_week = 0 if main_hrs_per_week == .
		gen				main_days_per_week = cond(main_hrs_per_week <= 8 & main_hrs_per_week > 0,1, ///
							cond(main_hrs_per_week > 8 & main_hrs_per_week <= 16,2, ///
							cond(main_hrs_per_week > 16 & main_hrs_per_week <= 24, 3, ///
							cond(main_hrs_per_week > 24 & main_hrs_per_week <= 32, 4, ///
							cond(main_hrs_per_week > 32 & main_hrs_per_week <= 40, 5, ///
							cond(main_hrs_per_week > 40 & main_hrs_per_week <= 48, 6, ///
							cond(main_hrs_per_week > 48, 7,.))))))) 

	* convert day, hour, & fortnight to weekly
		gen 			main_pay_per_week = main_pay * main_days_per_week ///
							if main_pay_unit == 2								//days
		replace 		main_pay_per_week = main_pay if main_pay_unit == 3 		//weeks
		replace 		main_pay_per_week = main_pay/2 if main_pay_unit == 4 	//fns
		replace 		main_pay_per_week = main_pay * main_hrs_per_week if ///
							main_pay_unit == 1									//hrs
		* if report working all weeks make 165/7/12 instead of 4
		replace			main_weeks_per_month = weeks_per_month if ///
							main_weeks_per_month == 4
						
	* convert weekly (including day, hr, fortnight) income to monthly
		gen				main_pay_per_month = main_pay if main_pay_unit == 5 //months
		replace			main_pay_per_month = main_pay_per_week * ///
							main_weeks_per_month if main_pay_per_week != . //dy,wk,fn,hr
							
	* convert all main income to annual
		gen				main_pay_annual = main_pay_per_month * main_months
		replace			main_pay_annual = main_pay if main_pay_unit == 8
		replace			main_pay_annual = main_pay * 2 if main_pay_unit == 7
		replace			main_pay_annual = main_pay * 4 if main_pay_unit == 6

* in-kind payments ('annuities and allowances')
	* format in-kind payment variables
		rename			s4q43 main_pay_kind
		rename			s4q44 main_pay_kind_unit
	
	* convert kind day, hour & fortnight to weekly
		gen				main_pay_kind_per_week = main_pay_kind * main_days_per_week ///
						if main_pay_kind_unit == 2								//days
		replace 		main_pay_kind_per_week = main_pay_kind if ///
						main_pay_kind_unit == 3 								//weeks
		replace			main_pay_kind_per_week = main_pay_kind / 2 ///
						if main_pay_kind_unit == 4								//fns

	* convert weekly (including day, hr, fortnight) income to monthly
		gen				main_pay_kind_per_month = main_pay_kind if ///
							main_pay_kind == 5									//months
		replace			main_pay_kind_per_month = main_pay_kind_per_week * ///
							main_weeks_per_month if main_pay_kind_per_week != . //dy,wk,fn,hr
	
	* convert all main in-kind income to annual
		gen				main_pay_kind_annual = main_pay_kind_per_month * main_months
		replace			main_pay_kind_annual = main_pay_kind if main_pay_kind_unit == 8
		replace			main_pay_kind_annual = main_pay_kind * 2 ///
							if main_pay_kind_unit == 7
		replace			main_pay_kind_annual = main_pay_kind * 4 ///
							if main_pay_kind_unit == 6

* calculate annual income from food/cash (temp) for labor
	* format variables
		gen				temp_emp_0 = 1 if s4q45 == 1
		replace			temp_emp_0 = 0 if temp_emp_0 != 1
		rename			s4q46 temp_days
		rename			s4q47 temp_pay
		replace			temp_emp_0 = 0 if temp_emp_0 != 1 | temp_emp_0 >= .
	
	* calculate annual income from temp pay (assuming s4q47 is total annaul temp pay)
		gen				temp_emp_amnt_0 = temp_pay
		replace			temp_emp_amnt_0 = 0 if temp_emp_amnt_0 >= .
		
	
	/* calculate annual income from temp pay (assuming s4q47 is days wage)
		gen				temp_pay_annual_alt = temp_pay * temp_days */

* calculate annual income from casual/other temporary labor
	* format vars
		gen 			casual_emp_0 = 1 if s4q48 == 1
		replace			casual_emp_0 = 0 if casual_emp_0 != 1
		rename			s4q49 cas_days
		rename			s4q50 cas_pay

	* calculate annual income from casual labor (assuming s4q50 is total annual temp pay)
		gen 			casual_emp_amnt_0 = cas_pay if cas_pay > 0 & cas_pay < .
		replace			casual_emp_amnt_0 = 0 if cas_pay >= .
		
	
	/* calculate annual income from temp pay (assuming s4q50 is days wage)
		gen				cas_pay_annual_alt = cas_pay * cas_days */
	
* add wage, in-kind, food/cash, 
	replace				main_pay_annual = 0 if main_pay_annual >= .
	gen					wage_emp_amnt_0 = main_pay_annual
	replace				wage_emp_amnt_0 = main_pay_annual + main_pay_kind_annual if ///
							main_pay_kind_annual != .
	
* collapse to hh level
	collapse 		(sum) wage_emp_amnt_0 casual_emp_amnt_0 temp_emp_amnt_0 (max) ///
						wage_emp_0 casual_emp_0 temp_emp_0, by(hhid_eth)

* check for unique identifier
	isid 			hhid_eth
	
* save tempfile 
	tempfile 		temp1
	save 			`temp1'

	
*************************************************************************
**#  NFE income
*************************************************************************

* load data
	use 			"$root/wave_0`w'/HH/sect12b1_hh_w4", clear
	rename 			household_id hhid_eth

* rename variables 
	rename 			s12bq12 bus_months_op
	rename 			s12bq13 bus_days_op
	rename 			s12bq16 bus_avg_sales
	rename 			s12bq24 bus_perc_hh_inc

* generate annual business income
	gen				nfe_inc_amnt_0 = bus_months_op * bus_avg_sales if ///
						bus_months_op < 13
	*** 4 missing values generated			
	*** there are four observations < 13 (16, 24, 29, 60)
	*** big pool of observations = 12, but this makes sense 
	collapse		(sum) nfe_inc_amnt_0, by(hhid_eth)
	gen				nfe_inc_0 = 1 if nfe_inc_amnt_0 > 0 & nfe_inc_amnt_0 < .
	*** 119 missing values generated 
	*** 1545 observations = 1 
	replace			nfe_inc_0 = 0 if nfe_inc_amnt_0 == 0 | nfe_inc_amnt_0 >= .
	*** 119 changes made 
	*** nfe_inc_0: 119 = 0 (7.15%), 1545 = 1 (92.85%)
	
	isid 			hhid_eth 
	
* save tempfile 
	tempfile 		temp2
	save 			`temp2'	

*************************************************************************
**#  other income  
*************************************************************************
	
* load data
	use 			"$root/wave_0`w'/HH/sect13_hh_w4", clear	

* rename variables
	rename 			household_id hhid_eth
	rename 			s13q01 inc_
	rename			s13q02 inc_amnt_
	replace 		source_cd = source_cd - 100
	keep 			hhid inc_ source_cd inc_amnt_
	
* reshape data and rename/generate inc vars
	reshape 		wide inc_ inc_amnt_, i(hhid) j(source_cd)
	lab def			yesno 0 "No" 1 "Yes", replace
	ds inc_*
	foreach			var in `r(varlist)' {
		replace			`var' = 0 if `var' == 2
		lab val			`var' yesno
	}
	
* rename vars
	
	rename			inc_1 cash_trans_0
	rename			inc_2 food_trans_0
	rename			inc_3 kind_trans_0
	rename			inc_4 save_inc_0
	rename			inc_5 pen_inc_0
	rename			inc_6 rent_veh_0
	rename			inc_7 rent_0
	rename			inc_8 asset_ag_0
	rename			inc_9 rent_ag_0
	rename			inc_10 sales_re_0
	rename			inc_11 asset_nonag_0
	rename			inc_12 asset_ag_fish_sales_0
	rename			inc_13 asset_nonag_oth_0
	rename			inc_14 other_inc_0

	rename			inc_amnt_1 cash_trans_amnt_0
	rename			inc_amnt_2 food_trans_amnt_0
	rename			inc_amnt_3 kind_trans_amnt_0
	rename			inc_amnt_4 save_inc_amnt_0
	rename			inc_amnt_5 pen_inc_amnt_0
	rename			inc_amnt_6 rent_veh_amnt_0
	rename			inc_amnt_7 rent_amnt_0
	rename			inc_amnt_8 asset_ag_amnt_0
	rename			inc_amnt_9 rent_ag_amnt_0
	rename			inc_amnt_10 sales_re_amnt_0
	rename			inc_amnt_11 asset_nonag_amnt_0
	rename			inc_amnt_12 asset_ag_fish_sales_amnt_0
	rename			inc_amnt_13 asset_nonag_oth_amnt_0
	rename			inc_amnt_14 other_inc_amnt_0
	
	keep 			hhid_eth *_0
	
* check for unique identifier
	isid 			hhid_eth
	
* save tempfile 
	tempfile 		temp3
	save 			`temp3'


*************************************************************************
**#  assistance/safety nets
*************************************************************************

* load data
	use				"$root/wave_0`w'/HH/sect14_hh_w4", clear
	rename 			household_id hhid_eth

* rename variables
	rename			s14q03 asst_cash_amnt_0
	rename			s14q05 asst_food_amnt_0
	rename			s14q06 asst_kind_amnt_0
	
* generate indicator variables for assistance type
	gen				asst_cash_0 = 1 if asst_cash_amnt_0 > 0
	gen				asst_food_0 = 1 if asst_food_amnt_0 > 0 
	gen 			asst_kind_0 = 1 if asst_kind_amnt_0 > 0 
	
	replace			asst_cash_0 = 0 if asst_cash_amnt_0 >= . |  ///
						asst_cash_amnt_0 == 0
	replace			asst_food_0 = 0 if asst_food_amnt_0 >= . |  ///
						asst_food_amnt_0 == 0
	replace			asst_kind_0 = 0 if asst_kind_amnt_0 >= . |  ///
						asst_kind_amnt_0 == 0
						
	lab define		yesno 0 "No" 1 "Yes", replace
	foreach 		var in asst_cash_0 asst_food_0 asst_kind_0 {
		lab val			`var' yesno
	}

* collapse by hhid
	collapse		(max) asst_cash_0 asst_food_0 asst_kind_0 ///
						(sum) asst_cash_amnt_0 asst_food_amnt_0 asst_kind_amnt_0, ///
						by(hhid_eth)
	
* check for unique identifier
	isid 			hhid_eth
	
* save tempfile
	tempfile		temp4
	save			`temp4'
	

*************************************************************************
**# crop & tree income   
*************************************************************************	

* load conversion factor
	use				"$root/wave_0`w'/PH/Crop_CF_Wave4"
		
	* rename key variables
		rename			unit_cd crop_unit
		rename			crop_code cropcode
		*** at this point, we have 1175 observations 	
	
	* average conversion units with multiple entries
		collapse (mean) mean_*, by (cropcode crop_unit)
		*** now we have 1174 observations - so only 1 collapses 
		
	* reshape file
		*reshape long mean_, i(cropcode crop_unit) j(region_conv) string  // (other method of merge)
	
	* save tempfile of conversion code to merage with crop disposition section (harvest)
		tempfile		conv
		save			`conv'
	
	* rename variables for consumption conversion from crop disposition
		rename			crop_unit cons_unit
	
	* save tempfile of consumption conversion
		tempfile		conv_cons
		save			`conv_cons'
	
	* rename variables for other uses conversion from crop disposition
		rename			cons_unit other_unit
		
	* save tempfile
		tempfile		conv_other
		save			`conv_other'
	
	* rename vars for reimbursement conversion
		rename			other_unit reim_unit
		
	* save tempfile
		tempfile		conv_reim
		save			`conv_reim'
	
	* rename vars for lost harvest conversion units
		rename			reim_unit lost_unit
		
	* save tempfile
		tempfile		conv_lost
		save			`conv_lost'

* load crop disposition data
	use				"$root/wave_0`w'/PH/sect11_ph_w4", clear
	*** observations = 9307 
	
	* rename variables
		rename			saq01 region
		rename			saq02 zone
		rename			saq03 woreda
		rename 			saq06 kebele
		rename 			ea_id ea
		rename			household_id hhid_eth
		
		rename 			s11q01 cropcode
		rename			s11q03a1 crop_quant
		rename			s11q03a2 crop_unit
		rename			s11q11a crop_sold_quant
		rename			s11q11b crop_sold_unit
		rename			s11q12 crop_sold_val
		
		rename			s11q18a1 cons_quant
		rename 			s11q18a2 cons_unit
		rename			s11q19a1 reim_quant
		rename			s11q19a2 reim_unit
		rename			s11q20a1 other_quant
		rename			s11q20a2 other_unit
		rename			s11q21a1 lost_quant
		rename			s11q21a2 lost_unit
		
		gen				pct_sales = s11q27a / 100
		gen				pct_cons = s11q27b / 100
		gen				pct_other = s11q27d / 100
		*** observations = 9307 

	* merge with conversion code to get conversion units for sold crops
		merge 			m:1 cropcode crop_unit using `conv'
		*** 7880 matched
		*** 2026 not matched from master, 599 not matched from using 
		drop			if crop_quant == . | crop_quant == 0 | _m == 1
		*** only keeping merged observations 
		*** observations = 7880 
	/* NOTE: this drops 15% of the data, this is because we do not have 
			specific conversion factors for the select combinations of crop and quantity */

	* gen crop conversion variable and populate with specific conversion for region
		gen				crop_conv = mean_cf1 if region == 1
		
		replace			crop_conv = mean_cf2 if crop_conv == . & region == 2
		replace         crop_conv = mean_cf3 if crop_conv == . & region == 3
		replace			crop_conv = mean_cf4 if crop_conv == . & region == 4
		replace			crop_conv = mean_cf6 if crop_conv == . & region == 6
		replace			crop_conv = mean_cf7 if crop_conv == . & region == 7
		replace			crop_conv = mean_cf12 if crop_conv == . & region == 12
		replace			crop_conv = mean_cf99 if crop_conv == .
	
	* drop _merge for future merge
		drop _m*
		
	* merge consumption conversion units and drop _m*
		merge			m:1 cropcode cons_unit using `conv_cons'
		*** matched 6124
		*** not matched 1666 from master, 659 from using 
		drop 			if crop_quant == . | crop_quant == 0 
		*** dropping if there is not crop quantity or if it is zero 
		*** 7880 observations
		drop			_m*
		
	* gen consumption conversion variable and populate with specific conversion for region
		gen				cons_conv = mean_cf1 if region == 1
		
		replace			cons_conv = mean_cf2 if cons_conv == . & region == 2
		replace         cons_conv = mean_cf3 if cons_conv == . & region == 3
		replace			cons_conv = mean_cf4 if cons_conv == . & region == 4
		replace			cons_conv = mean_cf6 if cons_conv == . & region == 6
		replace			cons_conv = mean_cf7 if cons_conv == . & region == 7
		replace			cons_conv = mean_cf12 if cons_conv == . & region == 12
		replace			cons_conv = mean_cf99 if cons_conv == .
		
	* merge in other uses conversion codes
		merge			m:1 cropcode other_unit using `conv_other'
		*** 1425 matched 
		*** 6455 not matched from master, 932 not matched from using 
		*** dropping if there is not crop quantity or if it is zero 
		drop 			if crop_quant == . | crop_quant == 0 
		drop			_m*
		
	* gen other conversion variable and populate with conversion value for region
		gen				other_conv = mean_cf1 if region == 1
		
		replace			other_conv = mean_cf2 if other_conv == . & region == 2
		replace         other_conv = mean_cf3 if other_conv == . & region == 3
		replace			other_conv = mean_cf4 if other_conv == . & region == 4
		replace			other_conv = mean_cf6 if other_conv == . & region == 6
		replace			other_conv = mean_cf7 if other_conv == . & region == 7
		replace			other_conv = mean_cf12 if other_conv == . & region == 12
		replace			other_conv = mean_cf99 if other_conv == .
		
	* merge in reim uses conversion codes
		merge			m:1 cropcode reim_unit using `conv_reim'
		*** matched = 430
		*** not matched from master 7450, not matched from using 1044 
		*** dropping if there is not crop quantity or if it is zero 
		drop 			if crop_quant == . | crop_quant == 0 
		drop			_m*
		
	* gen reim conversion variable and populate with conversion value for region
		gen				reim_conv = mean_cf1 if region == 1
		
		replace			reim_conv = mean_cf2 if reim_conv == . & region == 2
		replace         reim_conv = mean_cf3 if reim_conv == . & region == 3
		replace			reim_conv = mean_cf4 if reim_conv == . & region == 4
		replace			reim_conv = mean_cf6 if reim_conv == . & region == 6
		replace			reim_conv = mean_cf7 if reim_conv == . & region == 7
		replace			reim_conv = mean_cf12 if reim_conv == . & region == 12
		replace			reim_conv = mean_cf99 if reim_conv == .
	
	* merge in lost uses conversion codes
		merge			m:1 cropcode lost_unit using `conv_lost'
		*** matched 421
		*** not matched: 7459 from master, 1038 from using 
		*** dropping if there is not crop quantity or if it is zero 
		drop 			if crop_quant == . | crop_quant == 0 
		drop			_m*
		
	* gen lost conversion variable and populate with conversion value for region
		gen				lost_conv = mean_cf1 if region == 1
		
		replace			lost_conv = mean_cf2 if lost_conv == . & region == 2
		replace         lost_conv = mean_cf3 if lost_conv == . & region == 3
		replace			lost_conv = mean_cf4 if lost_conv == . & region == 4
		replace			lost_conv = mean_cf6 if lost_conv == . & region == 6
		replace			lost_conv = mean_cf7 if lost_conv == . & region == 7
		replace			lost_conv = mean_cf12 if lost_conv == . & region == 12
		replace			lost_conv = mean_cf99 if lost_conv == .
	
	* drop conversion factors named mean_cf`i'
		drop			mean_*
		
	* calculate price per kg
		gen				crop_kg = crop_quant * crop_conv
		gen				price_per_kg = crop_sold_val / crop_kg
		
* get median price per kg by smallest geo area
	egen 			ea_grp = group(cropcode region zone woreda kebele ea)
	egen 			keb_grp = group(cropcode region zone woreda kebele)
	egen			wor_grp = group(cropcode region zone woreda)
	egen			zn_grp = group(cropcode region zone)
	egen			reg_grp = group(cropcode region)
	egen			ctry_grp = group(cropcode)
		
	foreach 		l in ea keb wor zn reg ctry {
		bysort			`l'_grp: egen `l'_grp_count = count(`l'_grp) ///
								if price_per_kg != .
		bysort			`l'_grp: egen `l'_med = median(price_per_kg) ///
								if `l'_grp_count >= 10
	}
	
	gen				med_pr_per_kg = ctry_med
	replace			med_pr_per_kg = reg_med if reg_med != .
	replace			med_pr_per_kg = zn_med if zn_med != .
	replace			med_pr_per_kg = wor_med if wor_med != .
	replace			med_pr_per_kg = keb_med if keb_med != .
	replace			med_pr_per_kg = ea_med if ea_med != .
	
* convert uses into kgs & calculate value for each use then total value (not lost crops)
	foreach			i in cons reim other {
		gen			`i'_kg = `i'_quant * `i'_conv
		gen			`i'_val = `i'_kg * med_pr_per_kg
		replace		`i'_val = 0 if `i'_val >= .
	}
	
	* aggregate value
		egen			crop_inc_amnt_0 = rowtotal(*_val)
		gen				crop_inc_0 = cond(crop_inc_amnt_0 > 0, 1,0)
		
		*** at this point, before collapse we have 7880 observations 
		
		collapse		(sum) crop_inc_amnt_0 (max) crop_inc_0, by(hhid_eth)
		*** 2069 observations 
		
* check for unique identifier
	isid 			hhid_eth
	*** 2069 observations 
	*** 65 observations are 0 in both crop_inc_0 and crop_inc_amnt_0
	
* save tempfile
	tempfile		temp5
	save			`temp5'

	
*************************************************************************
**# livestock products 
*************************************************************************		

* load and format dung sales data
	use				"$root/wave_0`w'/LS/sect8_3_ls_w4", clear 
	*** 16044 observations 
	*** note only 15 households sold dung
	rename			ls_s8_3q28 dung
	rename			household_id hhid_eth
	replace			dung = 0 if dung == .
	collapse		(sum) dung, by(hhid_eth)
	*** 2637 observations 
	
	tempfile		dung
	save			`dung'
	
* load and format milk, egg, power, production
	use				"$root/wave_0`w'/LS/sect8_4_ls_w4", clear
	*** 42784 observations 
	
	rename			household_id hhid_eth
	rename			saq01 region
	rename			saq02 zone
	rename			saq03 woreda
	rename			saq06 kebele
	rename 			ea_id ea
	
	* rename & format milk variables
		rename			ls_s8_4q07 cons_ltrs_per_wk
		replace			cons_ltrs_per_wk = 0 if cons_ltrs_per_wk >= .
		rename			ls_s8_4q09 sold_ltrs_per_wk
		replace			sold_ltrs_per_wk = 0 if sold_ltrs_per_wk >= .
		rename			ls_s8_4q10 revs_per_wk
		rename			ls_s8_4q12 proc_revs_per_wk
		
		rename			ls_s8_4q01 milked
		replace			milked = 0 if milked != 1
		
		rename			ls_s8_4q11 milkproc_sold
		replace			milkproc_sold = 0 if milkproc_sold != 1
		
		gen				milkproc = 1 if ls_s8_4q06 == 3
		replace			milkproc = 0 if milkproc != 1
		
		gen				pr_per_ltr = revs_per_wk / sold_ltrs_per_wk
		gen				wks_per_year = 365/7
		gen				wks_per_month = wks_per_year / 12
		gen				wks_milked = ls_s8_4q03 * wks_per_month
		
		gen				ltrs_per_wk = ls_s8_4q04 * 7
		gen				proc_sold_ltrs_per_wk = ltrs_per_wk - cons_ltrs_per_wk ///
							- sold_ltrs_per_wk
		replace			proc_sold_ltrs_per_wk = 0 if proc_sold_ltrs_per_wk < 0
		replace			proc_sold_ltrs_per_wk = 0 if milkproc_sold == 0
		
		gen				proc_pr_per_ltr = proc_revs_per_wk / proc_sold_ltrs_per_wk
		/// note: discerning what amount of milk not sold or consumed was used for processing and consumption may not be possible. this is an estimate and only 257 households used milk for processing ///
		
		gen				proc_cons_ltrs_per_wk = ltrs_per_wk - sold_ltrs_per_wk				
		replace			proc_cons_ltrs_per_wk = 0 if ls_s8_4q06 !=3
		replace			proc_cons_ltrs_per_wk = 0 if proc_cons_ltrs_per_wk < 0
		
	* calc median price per liter to use for consumption value
		egen			ea_grp = group(ls_code region woreda zone kebele ea)
		egen			keb_grp = group(ls_code region woreda zone  kebele)
		egen			wor_grp = group(ls_code region woreda zone)
		egen			zn_grp = group(ls_code region zone)
		egen			reg_grp = group(ls_code region)
		egen			ctry_grp = group(ls_code)
		
		foreach 		l in ea keb wor zn reg ctry {
			bysort 			`l'_grp: egen `l'_grp_count = count(`l'_grp) ///
							if revs_per_wk != .
			bysort 			`l'_grp: egen `l'_med = median(pr_per_ltr) ///
							if `l'_grp_count >= 10
	}					
		foreach 		l in ea keb wor zn reg ctry {						
			bysort 			`l'_grp: egen proc_`l'_grp_count = count(`l'_grp) ///
							if proc_revs_per_wk != .					
			bysort 			`l'_grp: egen proc_`l'_med = ///
							median(proc_pr_per_ltr) if proc_`l'_grp_count >= 10
	}
	
		*** 42784 observations 
		
		gen				med_pr_per_ltr = ctry_med
		replace			med_pr_per_ltr = reg_med if reg_med != .
		replace			med_pr_per_ltr = zn_med if zn_med != .
		replace			med_pr_per_ltr = wor_med if wor_med != .
		replace			med_pr_per_ltr = keb_med if keb_med != .
		replace			med_pr_per_ltr = ea_med if ea_med != .
		
		gen				med_proc_pr_per_ltr = proc_ctry_med if proc_ctry_med != .
		replace			med_proc_pr_per_ltr = proc_reg_med if proc_reg_med != .
		replace			med_proc_pr_per_ltr = proc_zn_med if proc_zn_med != .
		replace			med_proc_pr_per_ltr = proc_wor_med if proc_wor_med != .
		replace			med_proc_pr_per_ltr = proc_keb_med if proc_keb_med != .
		replace			med_proc_pr_per_ltr = proc_ea_med if proc_ea_med != .
		
	* calc total value of milk 
		gen 			cons_val = med_pr_per_ltr * cons_ltrs_per_wk
		replace			cons_val = 0 if cons_val >= .
		gen				proc_cons_val = med_proc_pr_per_ltr * proc_cons_ltrs_per_wk
		replace			proc_cons_val = 0 if proc_cons_val >= .
		
		replace			revs_per_wk = 0 if revs_per_wk >= .
		replace			proc_revs_per_wk = 0 if proc_revs_per_wk >= .
		
		gen 			milk = cons_val + revs_per_wk + proc_cons_val ///
							+ proc_revs_per_wk
		replace			milk = milk * wks_milked
		*** 42784 observations 

	* rename and format egg variables
		rename			ls_s8_4q14 clutch_pds_yr
		rename			ls_s8_4q15 egg_per_clutch
		rename			ls_s8_4q16 amnt_hens_3m
		rename			ls_s8_4q18 egg_sales_3m
		rename			ls_s8_4q19 egg_rev_3m
		
	* get egg sales
		gen				rev_per_egg = egg_sales_3m / egg_rev_3m
		gen				egg_per_yr = clutch_pds_yr * egg_per_clutch * amnt_hens_3m 
		*** assuming the last 3 months has the average amount of hens a year
		
		gen				eggs = rev_per_egg * egg_per_yr
		*** unsure if supposed to calc income for non-sold eggs will do below 
		*** above line is only revenue for sold eggs
		
	* calc median egg revenue
		foreach 		l in ea keb wor zn reg ctry {						
			bysort 			`l'_grp: egen egg_`l'_grp_count = count(`l'_grp) ///
							if rev_per_egg != .					
			bysort 			`l'_grp: egen egg_`l'_med = ///
							median(rev_per_egg) if egg_`l'_grp_count >= 10
	}
		
		gen				med_pr_per_egg = egg_ctry_med
		replace			med_pr_per_egg = egg_reg_med if egg_reg_med != .
		replace			med_pr_per_egg = egg_zn_med if egg_zn_med != .
		replace			med_pr_per_egg = egg_wor_med if egg_wor_med != .
		replace			med_pr_per_egg = egg_keb_med if egg_keb_med != .
		replace			med_pr_per_egg = egg_ea_med if egg_ea_med != .
		
		replace			eggs = med_pr_per_egg * egg_per_yr if eggs == .
		*** the above code gives income of eggs for consumption and sold eggs
		*** if this is not wanted we can revert to the original eggs var
		
	* calculate animal power income
		rename			ls_s8_4aq23 power
	
	* collapse and save tempfile for milk, eggs, power
		collapse (sum) milk eggs power, by(hhid_eth)
		*** 2637 observations 
		*** no duplicates 
	
		tempfile		milkeggspower
		save			`milkeggspower'
		
* load and format meat sales data
	use				"$root/wave_0`w'/LS/sect8_2_ls_w4", clear
	*** 42784 observations 
	
	rename			household_id hhid_eth
	rename			saq01 region
	rename			saq02 zone
	rename			saq03 woreda
	rename			saq06 kebele
	rename 			ea_id ea
	
	rename			ls_s8_2q13 num_sold
	rename			ls_s8_2q14 tot_rev_sold
	gen				pr_per_sale = tot_rev_sold / num_sold 
	
	gen				num_cons = ls_s8_2q16 if ls_s8_2q17 != 1
	
	* we are estimating the income from consumed animals using live animal sales
	egen			ea_grp = group(ls_code region woreda zone kebele ea)
	egen			keb_grp = group(ls_code region woreda zone kebele)
	egen			wor_grp = group(ls_code region woreda zone)
	egen			zn_grp = group(ls_code region zone )
	egen			reg_grp = group(ls_code region)
	egen			ctry_grp = group(ls_code)
		
	foreach 		l in ea keb wor zn reg ctry {
		bysort 			`l'_grp: egen `l'_grp_count = count(`l'_grp) ///
							if tot_rev_sold != .
		bysort 			`l'_grp: egen `l'_med = median(pr_per_sale) ///
							if `l'_grp_count >= 0 
	} 
	*** there are too few observations n = 20
	
	gen				med_pr_per_sl = ctry_med
	replace			med_pr_per_sl = reg_med if reg_med != .
	replace			med_pr_per_sl = zn_med if zn_med != .
	replace			med_pr_per_sl = wor_med if wor_med != .
	replace			med_pr_per_sl = keb_med if keb_med != .
	replace			med_pr_per_sl = ea_med if ea_med != .      
	
	gen				meat = num_cons * med_pr_per_sl
		
* save data and collapse for merge with dung milkeggspower 
	collapse 		(sum) meat, by(hhid_eth)
	*** 2637 observations 
	
* merge temp files, save for final merge
	merge			1:1 hhid_eth using `dung', nogen
	merge			1:1 hhid_eth using `milkeggspower', nogen
	*** all matched - 2637 observations in both merges 
	
	gen				live_prod_0 = 0
	foreach			var in meat milk eggs dung power {
		replace			`var' = 0 if `var' == .
		replace			live_prod_0 = 1 if `var' >0
	}

	gen				live_prod_amnt_0 = meat + milk + eggs + dung + power
	drop			meat milk eggs dung power
	
* check for unique identifier
	isid 			hhid_eth
	*** 2637 observations 
	*** 666 observations which are 0 in both live_prod_0 and live_prod_amnt_0
	
* save tempfile
	tempfile		temp6
	save			`temp6'
	
	
* ***********************************************************************
**# livestock sales  
* ***********************************************************************
	
* load & format data
	use 			"$root/wave_0`w'/LS/sect8_2_ls_w4", clear
	rename			household_id hhid_eth
	
	rename 			ls_s8_2q14	live_inc_amnt_0
	replace			live_inc_amnt_0 = 0 if live_inc_amnt_0 >= .
	
* collapse to hh level
	collapse		(sum) live_inc_amnt_0, by(hhid_eth)

* gen indicator 
	gen				live_inc_0 = cond(live_inc_amnt_0 > 0, 1, 0)

* check for unique identifier
	isid 			hhid_eth
	
* save temp file
	tempfile		temp7
	save			`temp7'

* ***********************************************************************
**# ag rental income
* ***********************************************************************	

* load and format plot rental
	use			"$root/wave_0`w'/PP/sect2_pp_w4", clear
	*** 8174 observations 
	
	rename		household_id hhid_eth
	
	* plot rental data
		rename 			s2q15a plot
		rename			s2q15b kind_pay
		
		replace			plot = 0 if plot >= .
		replace			kind_pay = 0 if kind_pay >= .
		replace			plot = plot + kind_pay
		
		gen				plot_0 = 1 if plot > 0 & plot < .
		replace			plot_0 = 0 if plot >= .
		
		collapse 		(sum) plot (max) plot_0, by(hhid_eth)
		*** 2889 observations 

* check for unique identifier
	isid 			hhid_eth
	*** 2889 observations
	*** 2822 observations have 0 in plot and . in plot_0
	
	* save tempfile
		tempfile		temp8
		save			`temp8'
	
	
*************************************************************************
**# merge  
*************************************************************************	
	
* combine dataset 
	use 			`temp0', clear
	
* temp 1
	merge 			1:1 hhid_eth using `temp1'
	*** all matched - 6770
* replace missing values with 0s
	quietly: ds		*_0
	foreach			var in `r(varlist)' {
		replace			`var' = 0 if `var' >=.
	}
	*** no replacements made 
	drop _merge 
	
* temp 2 
	merge 			1:1 hhid_eth using `temp2'
	*** only 1664 matched
	*** 5106 not matched from master 
	*** total still 6770
* replace missing values with 0s
	quietly: ds		*_0
	foreach			var in `r(varlist)' {
		replace			`var' = 0 if `var' >=.
	}
	*** 5106 changes made - the unmatched from master  	
	drop _merge 
	
* temp 3 
	merge 			1:1 hhid_eth using `temp3'
	*** all matched 6770
* replace missing values with 0s
	quietly: ds		*_0
	foreach			var in `r(varlist)' {
		replace			`var' = 0 if `var' >=.
	}
	*** changes made - varying amounts 	
	drop _merge 	
	
* temp 4 
	merge 			1:1 hhid_eth using `temp4'
	*** all matched 6770
* replace missing values with 0s
	quietly: ds		*_0
	foreach			var in `r(varlist)' {
		replace			`var' = 0 if `var' >=.
	}
	*** 0 changes made  	
	drop _merge 	
	
* temp 5
	merge 			1:1 hhid_eth using `temp5'
	*** 4758 not matched from master
	*** 57 not matched from using
	*** 2012 matched 
	*** no longer have 6770 - have 6827 - which is 57 more
* drop if not matched from using
	drop 			if _merge == 2 
* replace missing values with 0s
	quietly: ds		*_0
	foreach			var in `r(varlist)' {
		replace			`var' = 0 if `var' >=.
	}
	*** various changes 4758 changes made  	
	drop _merge 	
		
* temp 6
	merge 			1:1 hhid_eth using `temp6'
	*** 4237 not matched from master
	*** 104 not  matched from using
	*** 2533 matched 
	*** no longer have 6770 - have 6874 - which is 104 more
* drop if not matched from using
	drop 			if _merge == 2 
* replace missing values with 0s
	quietly: ds		*_0
	foreach			var in `r(varlist)' {
		replace			`var' = 0 if `var' >=.
	}
	*** various changes 4237 changes made  	
	drop _merge 		
	
* temp 7
	merge 			1:1 hhid_eth using `temp7'
	*** 4237 not matched from master
	*** 104 not  matched from using
	*** 2533 matched 
	*** no longer have 6770 - have 6874 - which is 104 more
* drop if not matched from using
	drop 			if _merge == 2 
* replace missing values with 0s
	quietly: ds		*_0
	foreach			var in `r(varlist)' {
		replace			`var' = 0 if `var' >=.
	}
	*** various changes 4237 changes made  	
	drop _merge 		
	
* temp 8
	merge 			1:1 hhid_eth using `temp8'
	*** 4002 not matched from master
	*** 121 not  matched from using
	*** 2768 matched 
	*** no longer have 6770 - have 6891 - which is 121 more
* drop if not matched from using
	drop 			if _merge == 2 
* replace missing values with 0s
	quietly: ds		*_0
	foreach			var in `r(varlist)' {
		replace			`var' = 0 if `var' >=.
	}
	*** various changes 6721 changes made  	
	drop _merge 			
	
* it is possible to merge everything together using a loop
* due to issues with merge, this is not done, but appropriate code is below	
/*	
	forval			x = 1/8 {
		merge			1:1 hhid_eth using `temp`x'', nogen 
	}
*/ 

************************************************************************
**# combine similar variables with few observations, format, label
************************************************************************	

* rental income
	gen				rent_inc_0 = cond((rent_veh_0 == 1 | rent_0 == 1 | ///
						asset_ag_0 == 1 | rent_ag_0 == 1 | sales_re_0 == 1 | plot_0 == 1), 1, 0)
	
	egen			rent_inc_amnt_0 = rowtotal(rent_veh_amnt_0 rent_amnt_0 ///
						asset_ag_amnt_0 rent_ag_amnt_0 sales_re_amnt_0 plot)
	
	drop			rent_veh_0 rent_0 asset_ag_0 rent_ag_0 sales_re_0 plot_0 ///
						rent_veh_amnt_0 rent_amnt_0 asset_ag_amnt_0 rent_ag_amnt_0 ///
						sales_re_amnt_0 plot
* asset sales
	gen				asset_0 = cond(asset_nonag_0 == 1 | asset_ag_fish_sales_0 == 1, ///
						1, 0)
	gen				asset_amnt_0 = asset_nonag_amnt_0 + asset_ag_fish_sales_amnt_0
	
	drop			asset_nonag_amnt_0 asset_ag_fish_* asset_nonag_0
	
* other 
	gen				oth_inc_0 = cond(other_inc_0 == 1 | asset_nonag_oth_0 == 1, ///
						1, 0)
	gen				oth_inc_amnt_0 = other_inc_amnt_0 + asset_nonag_oth_amnt_0	
	drop			 asset_nonag_oth_amnt_0 asset_nonag_oth_0 other_inc_*
	
* label indicator variables
	lab define		yesno 0 "No" 1 "Yes", replace
	foreach			var in cash_trans_0 food_trans_0 kind_trans_0 save_inc_0 ///
						rent_inc_0 asset_0 oth_inc_0 crop_inc_0 live_inc_0 ///
						live_prod_0 nfe_inc_0 casual_emp_0 wage_emp_0 temp_emp_0 ///
						asst_cash_0 asst_food_0 asst_kind_0 {
		lab val			`var' yesno
	}
						
* label variables
	lab var 		cash_trans_0 "Cash Transfers/Gifts"
	lab var 		food_trans_0 "Food Transfers/Gifts"
	lab var 		kind_trans_0 "In-Kind Transfers/Gifts"
	lab var 		save_inc_0 "Savings, Interest, Investment"	
	lab var 		pen_inc_0 "Pension"
	lab var 		rent_inc_0 "Rental Income"
	lab var 		asset_0 "Asset Sales"
	lab var			oth_inc_0 "Other"
	lab var 		asst_food_0 "Free Food"
	lab var 		asst_cash_0 "Cash Transfers from Govt and NGOs"
	lab var			asst_kind_0 "In-kind Transfers from Govt and NGOs"
	lab var 		casual_emp_0 "Casual Employment Wages"
	lab var			temp_emp_0 "Temporary Employment Wages"
	lab var 		wage_emp_0 "Wages"
	lab var 		crop_inc_0 "Crop Income"
	lab var 		live_inc_0 "Livestock Sales"
	lab var 		live_prod_0 "Livestock Product Income"
	lab var 		nfe_inc_0 "Non-Farm Enterprises"

	lab var 		cash_trans_amnt_0 "Cash Transfers/Gifts from Individuals"
	lab var 		food_trans_amnt_0 "Food Transfers/Gifts from Individuals"
	lab var 		kind_trans_amnt_0 "Non-Food In-Kind Transfers/Gifts from Individuals"
	lab var 		save_inc_amnt_0 "Savings, Interest or Other Investment Income"
	lab var 		pen_inc_amnt_0 "Pension"
	lab var 		rent_inc_amnt_0 "Rental Income"
	lab var 		asset_amnt_0 "Asset Sales"
	lab var			oth_inc_amnt_0 "Other"
	lab var 		asst_food_amnt_0 "Free Food"
	lab var 		asst_cash_amnt_0 "Cash Transfers from Govt and NGOs"
	lab var			asst_kind_amnt_0 "In-Kind Transfers from Govt and NGOs"
	lab var 		casual_emp_amnt_0 "Wages from casual employment"
	lab var			temp_emp_amnt_0 "Wages from temporary employment"
	lab var 		wage_emp_amnt_0 "Wages"
	lab var 		crop_inc_amnt_0 "Income from crop income"
	lab var 		live_inc_amnt_0 "Income from livestock sales"
	lab var 		live_prod_amnt_0 "Income from livestock products"
	lab var			nfe_inc_amnt_0 "Income from non-farm enterprises"


************************************************************************
**# remove outliers in income amounts 
************************************************************************

* replace outliers with missing (outlier if > 2sds)	
	quietly: 			ds *_amnt_0
	foreach 			var in `r(varlist)' {
		quietly: 		sum `var' if `var' != 0, det
		local			med_sdx2 = `r(p50)' + (`r(sd)' * 2)
		replace 		`var' = . if `var' >= `med_sdx2'
	}

* impute missing values 
 	mi set 				wide 	
	mi xtset, 			clear // clear any xtset that may have had in place previously

* impute each variable in local
	ds 					*amnt_0
	foreach 			var in `r(varlist)' {
		mi register			imputed `var' // identify variable to be imputed
		sort				hhid, stable // sort to ensure reproducability of results
		mi impute 			pmm `var' i.region if `var' != 0, add(1) rseed(245780) ///
								noisily dots force knn(5) bootstrap
	}						
	mi 					unset
	drop 				mi_miss

* replace missing values with imputed values 
	ds 					*amnt_0
	local 				count = 0
	foreach 			var in `r(varlist)' {
		local 				count = `count' + 1
		replace 			`var' = `var'_`count'_ if `var' == .
		drop 				`var'_*
	}

	
************************************************************************
**# generate country-specific index
************************************************************************
* count/fraction index	
	preserve 
		drop 			*amnt_0 
		keep 			hhid_eth *_0 region zone woreda kebele ea
		
		* count number of possible income sources
		ds 				*_0
		gen 			tot_count = `c(k)' - 6 //number of columns minus 6 (hhid region zone woreda kebele ea)

		* generate count of income sources and total number of observations by each geographic level
		gen 			one = 1
		foreach 		l in region zone woreda kebele ea {
			foreach v 		of varlist *_0 {
				egen 			`v'_count_`l' = max(`v'), by(`l')
			}
			egen			`l'_count = rowtotal(*_count_`l')
			egen 			`l'_obs = total(one), by(`l')
		}
	
		* generate combined var using the smallest geographic area with at least 10 obs
		gen 	 		geo_count = region_count if region_obs >= 10
		replace 		geo_count = zone_count if zone_obs >= 10
		replace 		geo_count = woreda_count if woreda_obs >= 10
		replace 		geo_count = kebele_count if kebele_obs >= 10
		replace 		geo_count = ea_count if ea_obs >= 10

		
		* count number of hh income sources
		egen 			hh_count = rowtotal(*_0)
		
		* generate index
		gen 			eth_pre_index_all = 1- (hh_count / tot_count)
		lab var 		eth_pre_index_all "1- LD index with all 16 income sources in eth as denominator"
		gen				eth_pre_index_geo = 1- (hh_count / geo_count)
		lab var 		eth_pre_index_geo "1- LD index with denominator as number of income sources at lowest geo level with 10 obs"
		keep 			hhid eth_pre_index* 
		
		* tempfile 
		tempfile 		ind
		save 			`ind'
		
	restore
	
	merge 				1:1 hhid_eth using `ind', assert(3) nogen

	gen 				eth_pre_index_all_phhm = eth_pre_index_all / hhsize
	lab var 			eth_pre_index_all_phhm "eth_pre_index_all divided by hh size"
	gen 				eth_pre_index_geo_phhm = eth_pre_index_geo / hhsize
	lab var 			eth_pre_index_geo_phhm "eth_pre_index_geo divided by hh size"
	
* weighted by amount (Herfindahl-Hirschman Index)
	egen 				tot_inc = rowtotal(*amnt_0)
	ds 					*_amnt_0 
	foreach 			var in `r(varlist)' {
		gen 			`var'_persq = ((`var' / tot_inc) * 100)^2
	}
	
	egen 				eth_pre_index_hhi = rowtotal(*_amnt_0_persq)
	drop 				*_persq 
		
	lab var 			eth_pre_index_hhi "Herfindahl-Hirschman index of LD"

	
************************************************************************
**# add educational engagement variable
************************************************************************	
preserve
	
* load data
	use 				"$root/wave_0`w'/HH/sect2_hh_w4", clear

* merge with roster data to get age	
	merge 				1:1 household_id individual_id using "$root/wave_0`w'/HH/sect1_hh_w4"

* keep children age 5-18 and format variable 
 /// note: age is specified in survey
	rename				household_id hhid_eth
	rename				s1q03a age
	rename				s2q00 agerange
	keep				if age >= 5 & age <= 18
	rename				s2q07 edu_act
	replace				edu_act = 0 if edu_act == 2
	
* drop if never attended school because too young
	drop 			if s2q05 == 1

* replace other reasons for never attending schoo with "no"
	replace			edu_act = 0 if edu_act == . & s2q04 == 2 

* collapse to hh level
	collapse		(max) edu_act, by (hhid_eth)
	
* save tempfile
	tempfile		edu
	save			`edu'

restore	

	merge 			1:1 hhid_eth using `edu', nogen
	
	
************************************************************************
**# close matters
************************************************************************

* have 6770 observations 

* change geographic indicators to numeric
	foreach 		var in ea zone woreda kebele {
		rename 			`var' `var'_99
		egen 			`var' = group(`var'_99)
		drop 			`var'_99
	}
			
* add country & wave 
	gen 			wave = 0
	gen 			country = 1
	
* save round file
	save			"$export/wave_0`w'/r`w'", replace
	
	log close

/* END */	