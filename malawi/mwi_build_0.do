* Project: diversification
* Created on: June 2021
* Created by: amf
* Edited by: jdm, amf, alj
* Last edited: Nov 2021
* Stata v.17.0

* does
	* reads in baseline Malawi data
	* builds data for LD 
	* outputs HH income indices dataset

* assumes
	* raw malawi data 

* TO DO:
	* complete


* **********************************************************************
**# setup
* **********************************************************************

* define
	global	root	=	"$data/malawi/raw"
	global	export	=	"$data/malawi/refined"
	global	logout	=	"$data/malawi/logs"

* open log
	cap log 		close
	log using		"$logout/mal_build", append
	
* set local wave number & file number
	local			w = 0
	
* make wave folder within refined folder if it does not already exist 
	capture mkdir "$export/wave_0`w'" 	

	
* ***********************************************************************
**# household data
* ***********************************************************************

* load data
	use 			"$root/wave_0`w'/hh_mod_b_19", clear

* rename other variables 
	rename 			PID ind_id 
	rename 			hh_b03 sex_mem
	rename 			hh_b05a age_mem
	rename 			hh_b04 relat_mem	
	gen 			curr_mem = 0 if hh_b06_2 == 3 | hh_b06_2 == 4
	replace 		curr_mem = 1 if hh_b06_2 < 3
	replace 		curr_mem = 1 if hh_b06_2 == .
	gen 			new_mem = 0 if hh_b06_2 != 2 & hh_b06_2 < .
	replace 		new_mem = 1 if hh_b06_2 == 2
	gen 			mem_left = 0 if hh_b06_2  != 3 & hh_b06_2  < .
	replace 		mem_left = 1 if hh_b06_2 == 3
	
* generate counting variables
	drop 			hhsize 
	gen				hhsize = 1 if curr_mem == 1
	gen 			hhsize_adult = 1 if curr_mem == 1 & age_mem > 18 & age_mem < .
	gen				hhsize_child = 1 if curr_mem == 1 & age_mem < 19 & age_mem != . 
	gen 			hhsize_schchild = 1 if curr_mem == 1 & age_mem > 4 & age_mem < 19	

* create hh head gender
	gen 			sexhh = . 
	replace			sexhh = sex_mem if relat_mem == 1
	label var 		sexhh "Sex of household head"
	
* collapse data to hh level and merge in why vars
	collapse		(sum) hhsize hhsize_adult hhsize_child hhsize_schchild new_mem ///
					mem_left (max) sexhh, by(y4)	

* save tempfile 
	tempfile 		temp0
	save 			`temp0'

	
* ***********************************************************************
**# other income  
* ***********************************************************************
		
* load data
	use 			"$root/wave_0`w'/hh_mod_p_19", clear	

* rename variables
	replace 		hh_p0a = hh_p0a - 100
	egen 			temp = rowtotal(hh_p03a-hh_p03c) if hh_p02 == . & hh_p01 == 1
	replace 		hh_p02 = temp if hh_p02 == . & temp < .
	rename 			hh_p01 inc_
	rename 			hh_p02 amnt_
	keep 			y4 inc_ hh_p0a amnt_
	
* reshape data and rename vars
	reshape 		wide inc_ amnt_, i(y4) j(hh_p0a)
	lab	def			yesno 0 "No" 1 "Yes", replace
	
	ds inc_* 
	foreach 		var in `r(varlist)' {
		replace 		`var' = 0 if `var' == 2
		lab val 		`var' yesno
	}
	ds amnt_* 
	foreach 		var in `r(varlist)' {
		replace 		`var' = 0 if `var' == . 
	}

	rename 			inc_1 cash_trans_0
	rename 			amnt_1 cash_trans_amnt_0
	rename 			inc_2 food_trans_0
	rename 			amnt_2 food_trans_amnt_0
	rename 			inc_3 kind_trans_0
	rename 			amnt_3 kind_trans_amnt_0
	rename 			inc_4 save_inc_0
	rename 			amnt_4 save_inc_amnt_0
	rename 			inc_5 pen_pub_0
	rename 			amnt_5 pen_pub_amnt_0
	rename 			inc_6 rent_nonag_0
	rename 			amnt_6 rent_nonag_amnt_0
	rename 			inc_7 rent_0
	rename 			amnt_7 rent_amnt_0
	rename 			inc_8 rent_shop_0
	rename 			amnt_8 rent_shop_amnt_0
	rename 			inc_9 rent_veh_0
	rename 			amnt_9 rent_veh_amnt_0
	rename 			inc_10 sales_re_0
	rename 			amnt_10 sales_re_amnt_0
	rename 			inc_11 asset_nonag_0
	rename 			amnt_11 asset_nonag_amnt_0
	rename 			inc_12 asset_ag_0
	rename 			amnt_12 asset_ag_amnt_0
	rename 			inc_13 inherit_0
	rename 			amnt_13 inherit_amnt_0
	rename 			inc_14 gamb_0
	rename 			amnt_14 gamb_amnt_0	
	rename 			inc_15 oth_inc1_0
	rename 			amnt_15 oth_inc1_amnt_0
	rename 			inc_16 pen_priv_0
	rename 			amnt_16 pen_priv_amnt_0	

* save tempfile 
	tempfile 		temp1
	save 			`temp1'	
	
	
* ***********************************************************************
**# transfers from children
* ***********************************************************************	
	
* load data
	use 			"$root/wave_0`w'/hh_mod_o_19", clear
	
* rename vars
	rename 			hh_o11 cash_child_0
	replace 		cash_child_0 = 0 if cash_child_0 == 2
	rename 			hh_o14 cash_child_amnt_0
	rename 			hh_o15 kind_child_0
	replace 		kind_child_0 = 0 if kind_child_0 == 2
	rename 			hh_o17 kind_child_amnt_0
	replace 		cash_child_0 = 0 if hh_o0a == 2
	replace 		kind_child_0 = 0 if hh_o0a == 2
	
* collapse vars to hh level
	collapse 		(max) cash_child_0  kind_child_0 ///
						(sum) cash_child_amnt_0 kind_child_amnt_0, by(y4)
						
* save tempfile 
	tempfile 		temp2
	save 			`temp2'	
	

* ***********************************************************************
**# labor & time use  
* ***********************************************************************	
	
* load data
	use 			"$root/wave_0`w'/hh_mod_e_19", clear

* rename indicator vars	
	rename 			hh_e06_4 wage_emp_0
	rename 			hh_e06_6 casual_emp_0	
	foreach 		var in wage casual {
		replace 		`var'_emp_0 = 0 if `var'_emp_0 == 2	| `var'_emp_0 >= .
	}

* calc wage income	
	* generate conversion variables
		gen 			days_per_month = 365/12
		gen 			weeks_per_month = 365/7/12
		
	* rename main wage job variables	
		rename 			hh_e22 main_months
		rename 			hh_e25 main_pay
		rename 			hh_e26a main_pay_period
		rename 			hh_e26b main_pay_unit
		rename 			hh_e27 main_pay_kind
		rename 			hh_e28a main_pay_kind_period
		rename 			hh_e28b main_pay_kind_unit
		rename 			hh_e31 main_cost
		
	* convert all main income to monthly 
		* salary payments
		gen 			main_pay_per_month = (main_pay / main_pay_period) ///
							if main_pay_unit == 5
		replace 		main_pay_per_month = (main_pay / main_pay_period) * ///
							days_per_month if main_pay_unit == 3
		replace 		main_pay_per_month = (main_pay / main_pay_period) * ///
							weeks_per_month if main_pay_unit == 4
		* in-kind payments
		gen 			main_pay_kind_per_month = (main_pay_kind / main_pay_kind_period) ///
							if main_pay_kind_unit == 5
		replace 		main_pay_kind_per_month = (main_pay_kind / main_pay_kind_period) * ///
							days_per_month if main_pay_kind_unit == 3
		replace 		main_pay_kind_per_month = (main_pay_kind / main_pay_kind_period) * ///
							weeks_per_month if main_pay_kind_unit == 4		
		*combine salary and in-kind
		replace 		main_pay_per_month = main_pay_per_month + main_pay_kind_per_month ///
							if main_pay_kind_per_month != .
		
	* calc annual main income (subtract $ paid for apprenticeships)
		gen				main_pay_annual = (main_pay_per_month * main_months) 
		replace 		main_pay_annual = main_pay_annual - main_cost if main_cost !=.

	/* NOTE: the respondents who reported salaries on a weekly basis have significantly lower annual salaries than 
		other respondents (less than half) - this seems strange but is left as-is becuase we see no obvious errors
		sum main_pay_annual if main_pay_unit == 5
		sum main_pay_annual if main_pay_unit == 4
		sum main_pay_annual if main_pay_unit == 3
	*/

	* rename seocndary wage job variables	
		rename 			hh_e36 sec_months
		rename 			hh_e39 sec_pay
		rename 			hh_e40a sec_pay_period
		destring 		sec_pay_period, replace
		rename 			hh_e40b sec_pay_unit
		rename 			hh_e41 sec_pay_kind
		destring 		sec_pay_kind, replace
		rename 			hh_e42a sec_pay_kind_period
		rename 			hh_e42b sec_pay_kind_unit
		rename 			hh_e45 sec_cost
		
	* convert all incomes to monthly (already month in this case)
		* salary payments
		gen 			sec_pay_per_month = (sec_pay / sec_pay_period) if sec_pay_unit == 5
		* in-kind payments
		gen 			sec_pay_kind_per_month = (sec_pay_kind / sec_pay_kind_period) ///
							if sec_pay_kind_unit == 5
		*combine salary and in-kind
		replace 		sec_pay_per_month = sec_pay_per_month + sec_pay_kind_per_month ///
							if sec_pay_kind_per_month != .
		
	* calc annual income (subtract $ paid for apprenticeships)
		gen				sec_pay_annual = (sec_pay_per_month * sec_months) 
		replace 		sec_pay_annual = sec_pay_annual - sec_cost if sec_cost !=.

	* combine main and secondary job incomes
		gen 			wage_emp_amnt_0 = main_pay_annual + sec_pay_annual if sec_pay_annual != .
		replace 		wage_emp_amnt_0 = main_pay_annual if wage_emp_amnt_0 == .

* calc income from casual labor
	* rename variables 
		rename 			hh_e56 cas_months_per_year
		rename 			hh_e57 cas_wks_per_month
		rename 			hh_e58 cas_days_per_wk
		rename 			hh_e59 cas_pay_per_day
		
	* calc annual casual salary
		gen 			casual_emp_amnt_0 = cas_pay_per_day * cas_days_per_wk * ///
							cas_wks_per_month * cas_months_per_year
							
* collapse to hh level (note that this makes missing values 0)
	collapse 			(sum) *amnt* (max) wage_emp_0 casual_emp_0, by (y4)

* save tempfile 
	tempfile 		temp3
	save 			`temp3'	


* ***********************************************************************
**# crop & tree income
* ***********************************************************************	

* format datasets to merge
	* format region data
		use 			"$root/wave_0`w'/hh_mod_a_filt_19", clear
		
		gen 			sector = 1 if reside == 2 
		replace 		sector = 2 if reside == 1
		keep 			y4 region district ta_code sector
		replace 		region = region / 100
		tempfile 		region
		save 			`region'
		
	* format conversion data
		use 			"$root/wave_0`w'/ihs_seasonalcropconversion_factor_2020", clear
		drop 			if strpos(unit_code, "31") == 1 //drop 9 string vars that do not match sec g
		destring 		unit_code, replace
		tempfile 		convert
		save 			`convert'
	
	* format conversion data for consumption (consumption does not provide condition so use mean of unshelled/shelled)
		use 			`convert', clear
		collapse		(mean) conversion, by(region crop_code unit_code)
		rename 			conversion mean_conversion 
		tempfile 		mean_convert
		save 			`mean_convert'
		
*RAINY
* get amount of harvest sold and prices per kg of each crop
	* load & format rainy data
		use 			"$root/wave_0`w'/ag_mod_i_19", clear
		
		rename 			ag_i02a sold_harv
		rename 			ag_i02b unit_code
		rename 			ag_i02c condition
		rename 			ag_i03 sold_price

		keep 			if ag_i01 == 1
		keep 			y4 crop_code sold_harv unit_code condition sold_price 
		
	* merge region	
		merge 			m:1 y4 using `region' 
		drop 			if _m == 2
		drop 			_m 
		
	* merge with conversion factor
		merge 			m:1 crop_code unit_code condition region using `convert'
		drop 			if _m == 2
		drop 			_m 
		
		replace 		conversion = 1 if unit_code == 1 //1 for kgs
		drop 			collectionround
		* 169 obs with no conversion factor (~8%)
	
	* convert to kgs
		gen 			sold_harv_kg = sold_harv * conversion	
		gen 			pr_per_kg = sold_price / sold_harv_kg
	
	* calc price using median at lowest geographic level with at least 10 obs
		egen			ta_grp = group(crop_code region district ta_code)
		egen			dist_grp = group(crop_code region district)
		egen			reg_grp = group(crop_code region)
		egen			ctry_grp = group(crop_code)
		
		foreach 		l in ta dist reg ctry {
			bysort 			`l'_grp: egen `l'_grp_count = count(`l'_grp) ///
								if pr_per_kg != .
			bysort 			`l'_grp: egen `l'_med = median(pr_per_kg) ///
								if `l'_grp_count >= 10
		}

		gen 			med_pr_per_kg = ctry_med
		replace 		med_pr_per_kg = reg_med if reg_med != .
		replace 		med_pr_per_kg = dist_med if dist_med != .
		replace 		med_pr_per_kg = ta_med if ta_med != .
		* 122 missing
		
		preserve
			keep			y4 crop region district ta_code *harv_kg
			tempfile 		sold_harv
			save			`sold_harv'
		restore
		
		collapse 			(max) med_pr_per_kg, by(crop_code region district ta_code)
		
		tempfile 		prices
		save			`prices'
		
* get kgs to alternative uses
	* load & format rainy data
		use 			"$root/wave_0`w'/ag_mod_i_19", clear
		
		rename 			ag_i30a cons_harv 
		rename 			ag_i30b cons_unit
		rename 			ag_i31a gift_harv 
		rename 			ag_i31b gift_unit 
		rename 			ag_i31c gift_cond 
		rename 			ag_i32a reim_harv
		rename 			ag_i32b reim_unit
		rename 			ag_i32c reim_cond
		rename 			ag_i33a anim_harv 
		rename 			ag_i33b anim_unit
		rename 			ag_i33c anim_cond 
		rename 			ag_i34a inp_harv 
		rename 			ag_i34b inp_unit 
		rename 			ag_i34c inp_cond 
		rename 			ag_i35a seed_harv
		rename 			ag_i35b seed_unit
		rename 			ag_i35c seed_cond
		rename 			ag_i36a loss_harv 
		rename 			ag_i36b loss_unit 
		rename 			ag_i36c loss_cond 
		rename 			ag_i40a stor_harv
		rename 			ag_i40b stor_unit
		rename 			ag_i40c stor_cond
	
		keep 			y4 crop *_harv *_unit *_cond 
		replace 		stor_harv = 0 if stor_harv >= .
	
	* merge region	
		merge 			m:1 y4 using `region' 
		drop 			if _m == 2
		drop 			_m 	
		
	* merge with conversion factor
		foreach 		use in gift reim anim inp seed loss stor {
			preserve 
				keep 			y4 crop_code region `use'_*
				rename 			`use'_unit unit_code 
				rename 			`use'_cond condition
				merge 			m:1 crop_code unit_code condition region using `convert'
				drop 			if _m == 2
				drop 			_m 
				replace 		conversion = 1 if unit_code == 1 //1 for kgs
				drop 			collectionround unit_name
				rename 			unit_code `use'_unit
				rename 			condition `use'_cond
				rename 			conversion `use'_conv
				gen 			`use'_harv_kg = `use'_harv * `use'_conv
				tempfile 		`use'
				save 			``use''
			restore
		}
		
		* merge consumption with mean conversion (no condition provided)
			preserve 
				keep 			y4 crop_code region cons_*
				rename 			cons_unit unit_code 
				merge 			m:1 crop_code unit_code region using `mean_convert'
				drop 			if _m == 2
				drop 			_m 
				replace 		mean_conversion = 1 if unit_code == 1 //1 for kgs
				rename 			unit_code cons_unit
				rename 			mean_conversion cons_conv
				gen 			cons_harv_kg = cons_harv * cons_conv
				tempfile 		cons
				save 			`cons'
			restore
		
		foreach 		use in gift reim anim inp seed loss stor {
			merge 			1:1 y4 crop `use'_harv `use'_unit `use'_cond region ///
								using ``use'', assert(3) nogen
			replace 		`use'_harv_kg = 0 if `use'_harv == 0 //missing values now due to missing conversion units 
		}
		merge 			1:1 y4 crop cons_harv cons_unit region using `cons', assert(3) nogen
		replace 		cons_harv_kg = 0 if cons_harv == 0
		
	* format	
		keep 			y4 crop_code region district ta_code *_kg
		drop 			if crop_code == .
	
* merge with sold harvest and prices
	merge 				1:1 y4 crop_code region district ta_code using `sold_harv', nogen 
	merge 				m:1 crop_code region district ta_code using `prices', nogen 
	
	replace 			sold_harv_kg = 0 if sold_harv_kg >= .
	
	* fill in missing values of price when there were no sold observations 
	egen 				dist_pr = median(med_pr_per_kg), by(crop_code district)
	egen 				reg_pr = median(med_pr_per_kg), by(crop_code region)
	egen 				country_pr = median(med_pr_per_kg), by(crop_code)

	rename 				med_pr_per_kg temp
	rename 				country_pr med_pr_per_kg
	replace 			med_pr_per_kg = reg_pr if reg_pr != .
	replace 			med_pr_per_kg = dist_pr if dist_pr != .
	replace 			med_pr_per_kg = temp if temp != .
	
	replace 			med_pr_per_kg = 0 if med_pr_per_kg >= .
	
	drop 				temp dist_pr reg_pr
	
	* calculate value of harvest from each use
	ds 					*_harv_kg
	foreach 			var in `r(varlist)' {
		replace 			`var' = 0 if `var' >= .
		gen 				`var'_value = `var' * med_pr_per_kg
	}
	rename 				*harv_kg_value *value
	
	* get income value for hhs (include crops sold, consumed, or used as inputs, animal feed, or seed)
	gen 				rainy_inc_amnt = sold_value + cons_value + anim_value + inp_value + seed_value
	collapse 			(sum) rainy_inc_amnt*, by(y4)

	* save
	tempfile 			rainy_inc
	save 				`rainy_inc'
	
* DIMBA	
* get amount of harvest sold and prices per kg of each crop
	* load & format dimba data
		use 			"$root/wave_0`w'/ag_mod_o_19", clear
		
		rename 			ag_o02a sold_harv
		rename 			ag_o02b unit_code
		rename 			ag_o02c condition
		rename 			ag_o03 sold_price

		keep 			if ag_o01 == 1
		keep 			y4 crop_code sold_harv unit_code condition sold_price 
		
	* merge region	
		merge 			m:1 y4 using `region' 
		drop 			if _m == 2
		drop 			_m 
		
	* merge with conversion factor
		merge 			m:1 crop_code unit_code condition region using `convert'
		drop 			if _m == 2
		drop 			_m 
		
		replace 		conversion = 1 if unit_code == 1 //1 for kgs
		drop 			collectionround
	
	* convert to kgs
		gen 			sold_harv_kg = sold_harv * conversion	
		gen 			pr_per_kg = sold_price / sold_harv_kg
	
	* calc median price per kg at lowest geographic level with at least 10 obs
		egen			ta_grp = group(crop_code region district ta_code)
		egen			dist_grp = group(crop_code region district)
		egen			reg_grp = group(crop_code region)
		egen			ctry_grp = group(crop_code)
		
		foreach 		l in ta dist reg ctry {
			bysort 			`l'_grp: egen `l'_grp_count = count(`l'_grp) ///
								if pr_per_kg != .
			bysort 			`l'_grp: egen `l'_med = median(pr_per_kg) ///
								if `l'_grp_count >= 10
		}
		
		gen 			med_pr_per_kg = ctry_med
		replace 		med_pr_per_kg = reg_med if reg_med != .
		replace 		med_pr_per_kg = dist_med if dist_med != .
		replace 		med_pr_per_kg = ta_med if ta_med != .
		
		preserve
			keep			y4 crop region district ta_code *harv_kg
			tempfile 		dsold_harv
			save			`dsold_harv'
		restore
		
		collapse 			(max) med_pr_per_kg, by(crop_code region district ta_code)
		
		tempfile 		dprices
		save			`dprices'

* get kgs to alternative uses
	* load & format dimba data
		use 			"$root/wave_0`w'/ag_mod_o_19", clear

		rename 			ag_o31a gift_harv 
		rename 			ag_o31b gift_unit 
		rename 			ag_o31c gift_cond 
		rename 			ag_o32a reim_harv
		rename 			ag_o32b reim_unit
		rename 			ag_o32c reim_cond
		rename 			ag_o33a anim_harv 
		rename 			ag_o33b anim_unit
		rename 			ag_o33c anim_cond 
		rename 			ag_o34a inp_harv 
		rename 			ag_o34b inp_unit 
		rename 			ag_o34c inp_cond 
		rename 			ag_o35a seed_harv
		rename 			ag_o35b seed_unit
		rename 			ag_o35c seed_cond
		rename 			ag_o36a loss_harv 
		rename 			ag_o36b loss_unit 
		rename 			ag_o36c loss_cond 
		rename 			ag_o40a stor_harv
		rename 			ag_o40b stor_unit
		rename 			ag_o40c stor_cond
	
		keep 			y4 crop *_harv *_unit *_cond 
		replace 		stor_harv = 0 if stor_harv >= .
	
	* merge region	
		merge 			m:1 y4 using `region' 
		drop 			if _m == 2
		drop 			_m 	
		
	* merge with conversion factor
		foreach 		use in gift reim anim inp seed loss stor {
			preserve 
				keep 			y4 crop_code region `use'_*
				rename 			`use'_unit unit_code 
				rename 			`use'_cond condition
				merge 			m:1 crop_code unit_code condition region using `convert'
				drop 			if _m == 2
				drop 			_m 
				replace 		conversion = 1 if unit_code == 1 //1 for kgs
				drop 			collectionround unit_name
				rename 			unit_code `use'_unit
				rename 			condition `use'_cond
				rename 			conversion `use'_conv
				gen 			`use'_harv_kg = `use'_harv * `use'_conv
				tempfile 		`use'
				save 			``use''
			restore
		}
		
		foreach 		use in gift reim anim inp seed loss stor {
			merge 			1:1 y4 crop `use'_harv `use'_unit `use'_cond region using ``use'', assert(3) nogen
			replace 		`use'_harv_kg = 0 if `use'_harv == 0 //missing values now due to missing conversion units 
		}
		
	* format	
		keep 			y4 crop_code region district ta_code *_kg
		drop 			if crop_code == .
	
* merge with sold harvest and prices
	merge 				1:1 y4 crop_code region district ta_code using `dsold_harv', nogen 
	merge 				m:1 crop_code region district ta_code using `dprices', nogen 
	
	replace 			sold_harv_kg = 0 if sold_harv_kg >= .
	
	* fill in missing values of price when there were no sold observations 
	egen 				dist_pr = median(med_pr_per_kg), by(crop_code district)
	egen 				reg_pr = median(med_pr_per_kg), by(crop_code region)
	egen 				country_pr = median(med_pr_per_kg), by(crop_code)

	rename 				med_pr_per_kg temp
	rename 				country_pr med_pr_per_kg
	replace 			med_pr_per_kg = reg_pr if reg_pr != .
	replace 			med_pr_per_kg = dist_pr if dist_pr != .
	replace 			med_pr_per_kg = temp if temp != .
	
	replace 			med_pr_per_kg = 0 if med_pr_per_kg >= .
	
	drop 				temp dist_pr reg_pr
	
	* calculate value of harvest from each use
	ds 					*_harv_kg
	foreach 			var in `r(varlist)' {
		replace 			`var' = 0 if `var' >= .
		gen 				`var'_value = `var' * med_pr_per_kg
	}
	rename 				*harv_kg_value *value
	
	* get income value for hhs (include crops sold or used as inputs, animal feed, or seed - no consumption data for dimba)
	gen 				dimba_inc_amnt = sold_value + anim_value + inp_value + seed_value
	collapse 			(sum) dimba_inc_amnt*, by(y4)

* merge dimba and rainy
	merge 				1:1 y4 using `rainy_inc'
	
	replace 			dimba = 0 if dimba == .
	replace 			rainy = 0 if rainy == .	
	gen 				crop_inc_amnt_0 = rainy + dimba
	keep 				y4 crop
	gen 				crop_inc_0 = cond(crop_inc_amnt_0 > 0, 1,0)
	
	tempfile 		tempc
	save 			`tempc'		
				
* tree crops	
	* load & format tree data
		use 			"$root/wave_0`w'/ag_mod_q_19", clear	

		rename 			ag_q01 tree_inc_0
		replace 		tree_inc_0 = 0 if tree_inc_0 == 2
		rename 			ag_q03 tree_inc_amnt_0
		collapse 		(sum) tree_inc_amnt_0 (max) tree_inc_0, by(y4)

* merge crop & tree 	
	merge 			1:1 y4 using `tempc', nogen
	
	ds 				*inc*
	foreach 		var in `r(varlist)' {
		replace 		`var' = 0 if `var' >= .
	}
	
* save tempfile 
	tempfile 		temp4
	save 			`temp4'	

	
* ***********************************************************************
**# safety nets/assistance
* ***********************************************************************	

* load data
	use 			"$root/wave_0`w'/hh_mod_r_19", clear

*format and reshape 
	rename 			hh_r01 inc_
	replace 		hh_r02a = hh_r02a + hh_r02b if hh_r02a != . & hh_r02b != .
	replace 		hh_r02a = hh_r02b if hh_r02a == . & hh_r02b != .
	replace 		hh_r02a = hh_r02c if hh_r02a == . 
	rename 			hh_r02a amnt_
	replace 		inc_ = 0 if inc_ == 2
	collapse 		(sum) amnt_ (max) inc_, by(y4 hh_r0a) //collapse duplicate "other" values
	keep 			inc_ amnt_ y4 hh_r0a	
	reshape 		wide inc_ amnt_, i(y4) j(hh_r0a)		
	drop 			inc_105 inc_108 inc_1091 amnt_105 inc_106  amnt_106 ///
						inc_107 amnt_107 amnt_108 amnt_1091

* rename variables 
	rename 			inc_101 asst_maize_0
	rename 			amnt_101 kg_maize_0
	rename 			inc_102 asst_food_0
	rename 			amnt_102 asst_food_amnt_0
	rename 			inc_104 input_for_wrk_0
	rename 			amnt_104 input_for_wrk_amnt_0
	rename 			inc_111 cash_gov_0
	rename 			amnt_111 cash_gov_amnt_0
	rename 			inc_112 cash_ngo_0
	rename 			amnt_112 cash_ngo_amnt_0
	rename 			inc_113 oth_inc2_0
	rename 			amnt_113 oth_inc2_amnt_0
	rename 			inc_1031 masaf_0
	rename 			amnt_1031 masaf_amnt_0
	rename 			inc_1032 cash_for_wrk_0
	rename 			amnt_1032 cash_for_wrk_amnt_0

* convert kg maize to value using prices
	preserve 
		use 			`prices', clear //using rainy season prices
		keep 			if crop_code < 5 //keep maize codes
		drop 			if med_pr == .
		collapse 		(mean) med_pr, by(region district ta_code) //take mean of price of maize types
		sum				med_pr, det
		local 			med_pr_2 = `r(p50)' //take median across all areas to fill in missing values
		tempfile 		temp maize_pr
		save 			`maize_pr' 
	restore 
	
	* merge region data
	merge 				1:1 y4 using `region', assert(3) nogen
	
	* merge with maize price data
	merge 				m:1 region district ta_code using `maize_pr', nogen

	* gen variable & format
	gen 				asst_maize_amnt_0 = kg_maize_0 * med_pr_per_kg
	replace 			asst_maize_amnt_0 = kg_maize_0 * `med_pr_2' if asst_maize_amnt_0 == .
	drop 				kg_maize_0 med_pr_per_kg
	order 				y4 region district ta_code asst_maize* 
	
* save tempfile 
	tempfile 		temp5
	save 			`temp5'	
	
	
* ***********************************************************************
**# livestock 
* ***********************************************************************	

* load & format livestock data
	use 			"$root/wave_0`w'/ag_mod_r1_19", clear	
	
	rename 			ag_r17 live_inc_amnt_0 
	collapse		(sum) live_inc_amnt_0, by(y4)
	gen 			live_inc_0 = cond(live_inc_amnt_0 > 0, 1,0)

* save tempfile 
	tempfile 		temp6
	save 			`temp6'	


* ***********************************************************************
**# livestock products
* ***********************************************************************	

* load & format livestock product data
	use 			"$root/wave_0`w'/ag_mod_s_19", clear

	* rename variables
	rename 			ag_s0a product
	rename 			ag_s05a sales_quant
	rename 			ag_s05b sales_unit
	rename 			ag_s06 sales_rev
	rename 			ag_s09a cons_quant
	rename 			ag_s09b cons_unit
	
	* get price per liter - milk
	gen 			pr_per_ltr_temp = sales_rev / sales_quant if ///
						sales_unit == 1
	egen 			pr_per_ltr = median(pr_per_ltr_temp)
	
	* get price per piece - eggs
	gen 			pr_per_egg_temp = sales_rev / sales_quant if ///
						sales_unit == 3
	egen 			pr_per_egg = median(pr_per_egg_temp)
	
	* get price per kg - meat
	gen 			pr_per_kg_temp = sales_rev / sales_quant if ///
						sales_unit == 2
	egen 			pr_per_kg = median(pr_per_kg_temp)
	
	* get price per 50kg - manure 
	gen 			quant_50kg = sales_quant / 50 if product == 407 & ///
						sales_unit == 2
	replace 		quant_50kg = sales_quant if product == 407 & ///
						ag_s05b_oth == "50 KG BAG"
	gen 			pr_per_50kg_temp = sales_rev / quant_50kg if /// 
						quant_50kg != .
	egen 			pr_per_50kg = median(pr_per_50kg_temp)
	
	* consumption 
	gen 			milk_cons = cons_quant * pr_per_ltr if cons_unit == 1
	gen 			egg_cons = cons_quant * pr_per_egg if cons_unit == 3
	gen 			meat_cons = cons_quant * pr_per_kg if cons_unit == 2
	gen 			man_cons = cons_quant * pr_per_50kg if quant_50kg != .
	
	* sum values 
	foreach 		var in sales_rev milk_cons egg_cons meat_cons man_cons {
		replace 		`var' = 0 if `var' >= .
	}
	egen 			live_prod_amnt_0 = rowtotal(sales_rev *_cons)
	collapse 		(sum) live_prod_amnt_0, by (y4) 
	gen 			live_prod_0  = cond(live_prod_amnt_0 > 0, 1, 0) 
	
* save tempfile 
	tempfile 		temp7
	save 			`temp7'	
	
	
* ***********************************************************************
**# NFE income 
* ***********************************************************************	

* load & format amount data
	use 			"$root/wave_0`w'/hh_mod_n2_19", clear
	
	foreach 		x in a b c d e f g h i j k l m n o p {
		gen 			low`x' = 1 if hh_n25`x' == 1
		gen 			avg`x' = 1 if hh_n25`x' == 2
		gen 			high`x' = 1 if hh_n25`x' == 3
	}
	
	egen 			low_count = rowtotal(low*)
	egen 			avg_count = rowtotal(avg*)
	egen 			high_count = rowtotal(high*)
	
	gen 			avg_sales = hh_n34 
	replace 		avg_sales = hh_n39 if avg_sales == .
	replace 		avg_sales = hh_n32 if avg_sales == .
	
	gen 			low_sales = hh_n36 
	replace 		low_sales = hh_n38 if low_sales == .
	replace 		low_sales = hh_n32 if low_sales == .
	
	gen 			high_sales = hh_n35 
	replace 		high_sales = hh_n37 if high_sales == .
	replace 		high_sales = hh_n32 if high_sales == .
	
	foreach 		x in low avg high {
		gen 			`x' = `x'_count * `x'_sales
	}
	
	egen 			nfe_inc_amnt_0 = rowtotal(low avg high)

	gen 			num_bus = 1
	collapse 		(sum) num_bus nfe_inc_amnt_0, by(y4)
	
	tempfile 		tempnfe
	save 			`tempnfe'
	
* load & format indicator data
	use 			"$root/wave_0`w'/hh_mod_n1_19", clear
	gen 	 		nfe_inc_0 = 0
	replace 		nfe_inc_0 = 1 if hh_n01 == 1 | hh_n02 == 1 | hh_n03 == 1 | ///
						hh_n04 == 1 | hh_n05 == 1 | hh_n06 == 1 | hh_n07 == 1 | ///
						hh_n08 == 1 
	keep 			y4 nfe_inc_0

	merge 			1:1 y4 using `tempnfe', nogen 
	replace 		num_bus = 0 if num_bus >= .
	replace 		nfe_inc_amnt = 0 if nfe_inc_amnt >= .
	
* save tempfile 
		tempfile 		temp8
		save 			`temp8'		

* ***********************************************************************
**# plot rental
* ***********************************************************************	

* load & format dimba data (note: this question asks how much did you receive
	* from rent and sums cash & inkind values)
	use 			"$root/wave_0`w'/ag_mod_i2_19", clear

	gen 			rent_plot_amnt_0 = ag_i217a +ag_i217b
	replace 		rent_plot_amnt_0 = 0 if rent_plot_amnt_0 >= .
	collapse 		(sum) rent_plot_amnt_0, by(y4) 
	gen 			rent_plot_0 = cond(rent_plot_amnt_0 > 0, 1, 0)

	tempfile 		tempx
	save 			`tempx'
	
	
* load & format rainy data (note: this question asks how much did you receive 
	* & how much are you still waiting to receive from rent and asks about 
	* cash & inkind for both options. This var sums across the 4 categories)
	use 			"$root/wave_0`w'/ag_mod_b2_19", clear
	
	egen 			rent_plot_amnt_0 = rowtotal(ag_b219*)
	collapse 		(sum) rent_plot_amnt_0, by(y4) 
	gen 			rent_plot_0 = cond(rent_plot_amnt_0 > 0, 1, 0)

* merge dimba & rainy
	append 			using `tempx'
	collapse 		(sum) rent_plot_amnt_0 (max) rent_plot_0, by(y4)
	
* save tempfile 
	tempfile 		temp9
	save 			`temp9'	
	
	
* ***********************************************************************
**# merge  
* ***********************************************************************	
	
* combine dataset 
	use 			`temp0', clear
	forval 			x = 1/9 {
		merge 			1:1 y4 using `temp`x'', nogen
	}
	
* replace missing values with 0s	
	quietly: ds, 	has(type numeric) 
	foreach 		var in `r(varlist)' {
		replace 		`var' = 0 if `var' >= .
	}

	
************************************************************************
**# combine similar variables with few observations, format, label
************************************************************************	

* pension
	gen 			pen_inc_0 = cond(pen_pub_0 == 1 | pen_priv_0 == 1, 1, 0)
	gen 			pen_inc_amnt_0 = pen_pub_amnt_0 + pen_priv_amnt_0
	drop 			pen_pub* pen_priv*
	
* rental income
	gen 			rent_inc_0 = cond((rent_nonag_0 == 1 | rent_0 == 1 | ///
						rent_shop_0 == 1 | rent_veh_0 == 1 | sales_re_0 == 1 | ///
						rent_plot_0 == 1), 1, 0)
						
	egen 			rent_inc_amnt_0 = rowtotal(rent_nonag_amnt_0 rent_amnt_0 ///
						rent_shop_amnt_0 rent_veh_amnt_0 sales_re_amnt_0 rent_plot_amnt_0)	
						
	drop 			rent_nonag_amnt_0 rent_amnt_0 rent_shop_amnt_0 rent_veh_amnt_0 ///
						sales_re_amnt_0 rent_plot_amnt_0 rent_nonag_0 rent_0 ///
						rent_shop_0 rent_veh_0 sales_re_0 rent_plot_0

* asset sales
	gen 			asset_0 = cond(asset_nonag_0 == 1 | asset_ag_0 == 1, 1, 0)
	gen 			asset_amnt_0 = asset_nonag_amnt_0 + asset_ag_amnt_0
	drop 			asset*ag*

* other
	gen 			oth_inc_0 = cond(oth_inc1_0 == 1 | oth_inc2_0 == 1 | ///
						inherit_0 == 1 | gamb_0 == 1, 1, 0)
	
	egen 			oth_inc_amnt_0 = rowtotal(oth_inc1_amnt_0 oth_inc2_amnt_0 ///
						inherit_amnt_0 gamb_amnt_0)
						
	drop 			oth_inc1* oth_inc2* inherit* gamb_*	

* food/input for work	
	gen 			for_wrk_0 = cond(cash_for_wrk_0 == 1 | input_for_wrk_0 == 1, 1, 0)
	gen 			for_wrk_amnt_0 = cash_for_wrk_amnt_0 + input_for_wrk_amnt_0
	drop 			input_for* cash_for* 
	
* cash assistance 
	gen 			asst_cash_0 = cond(cash_gov_0 == 1 | cash_ngo_0 == 1, 1, 0)
	gen 			asst_cash_amnt_0 = cash_gov_amnt_0  + cash_ngo_amnt_0
	drop 			cash_ngo_* cash_gov_*
	
* food assistance 
	replace 		asst_food_0 = 1 if asst_maize_0 == 1 
	replace			asst_food_amnt_0 = asst_food_amnt_0  + asst_maize_amnt_0
	drop 			asst_maize_* 
	
* label indicator variables
	lab	def			yesno 0 "No" 1 "Yes", replace
	foreach 		var in cash_child_0 kind_child_0 asst_food_0 for_wrk_0 ///
						asst_cash_0  masaf_0 asset_0 pen_inc_0 wage_emp_0 ///
						casual_emp_0 tree_inc_0 crop_inc_0 live_inc_0 live_prod_0 ///
						nfe_inc_0 oth_inc_0 rent_inc_0 {
	lab val 			`var' yesno
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
	lab var 		for_wrk_0 "Cash or Inputs for Work"
	lab var 		cash_child_0 "Cash from Children"
	lab var 		kind_child_0 "In-Kind Transfers from children"
	lab var 		asst_food_0 "Free Food"
	lab var 		masaf_0 "MASAF Public Works Program"
	lab var 		asst_cash_0 "Cash Transfers from Govt and NGOs"
	lab var 		casual_emp_0 "Casual Employment Wages"
	lab var 		wage_emp_0 "Wages"
	lab var 		crop_inc_0 "Crop Income"
	lab var 		tree_inc_0 "Tree Crop Sales"
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
	lab var 		for_wrk_amnt_0 "Cash or Inputs for Work"
	lab var 		cash_child_amnt_0 "Cash from children"
	lab var 		kind_child_amnt_0 "Inkind transfers from children"
	lab var 		asst_food_amnt_0 "Free Food"
	lab var 		masaf_amnt_0 "MASAF - Public Works Programme"
	lab var 		asst_cash_amnt_0 "Cash Transfers from Govt and NGOs"
	lab var 		casual_emp_amnt_0 "Wages from casual employment"
	lab var 		wage_emp_amnt_0 "Wages"
	lab var 		crop_inc_amnt_0 "Income from crop income"
	lab var 		tree_inc_amnt_0 "Income from tree crop sales"
	lab var 		live_inc_amnt_0 "Income from livestock sales"
	lab var 		live_prod_amnt_0 "Income from livestock products"
	lab var 		nfe_inc_amnt_0 "Income from non-farm enterprises"

* merge with region 
	merge 			m:1 y4 using `region'
	drop 			if _m == 2
	drop 			_m
	
	
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
		sort				y4, stable // sort to ensure reproducability of results
		mi impute 			pmm `var' i.district if `var' != 0, add(1) rseed(245780) ///
								noisily dots force knn(5) bootstrap
	}						
	mi 					unset
	drop 				mi_miss
	
* replace missing values with imputed values 
	ds 					*amnt_0
	local 				count = 0
	foreach 			var in `r(varlist)' {
		local 				count = `count' + 1
		replace 				`var' = `var'_`count'_ if `var' == .
		drop 					`var'_*
	}
		
		
************************************************************************
**# generate country-specific index
************************************************************************

* count/fraction index	
	preserve 
		drop 			*amnt_0 
		keep 			y4 *_0 region district ta_code
		
		* count number of possible income sources
		ds 				*_0
		gen 			tot_count = `c(k)' -4 //number of columns minus 4 (y4 region, district, and ta_code)
		
		* generate count of income sources and total number of observations by each geographic level
		gen 			one = 1
		foreach 		l in district region ta_code {
			foreach v 		of varlist *_0 {
				egen 			`v'_count_`l' = max(`v'), by(`l')
			}
			egen			`l'_count = rowtotal(*_count_`l')
			egen 			`l'_obs = total(one), by(`l')
		}
	
		* generate combined variable that uses the smallest geographic area with at least 10 observations
		gen 			geo_count = region_count if region_obs >= 10
		replace 		geo_count = district_count if district_obs >= 10
		replace 		geo_count = ta_code_count if ta_code_obs >= 10
		
		* count number of hh income sources
		egen 			hh_count = rowtotal(*_0)
		
		* generate index
		gen 			mwi_pre_index_all = 1- (hh_count / tot_count)
		lab var 		mwi_pre_index_all "1- LD index with all 21 income sources in MWI as denominator"
		gen				mwi_pre_index_geo = 1- (hh_count / geo_count)
		lab var 		mwi_pre_index_geo "1- LD index with denominator as number of income sources at lowest geo level with 10 obs"
		keep 			y4 mwi_pre_index* 
		
		* tempfile 
		tempfile 		ind
		save 			`ind'
		
	restore
	
	merge 				1:1 y4 using `ind', assert(3) nogen

	gen 				mwi_pre_index_all_phhm = mwi_pre_index_all / hhsize
	lab var 			mwi_pre_index_all_phhm "mwi_pre_index_all divided by hh size"
	gen 				mwi_pre_index_geo_phhm = mwi_pre_index_geo / hhsize
	lab var 			mwi_pre_index_geo_phhm "mwi_pre_index_geo divided by hh size"
		
* weighted by amount (Herfindahl-Hirschman Index)
	egen 			tot_inc = rowtotal(*amnt_0)
	ds 				*_amnt_0 
	foreach 		var in `r(varlist)' {
		gen 		`var'_persq = ((`var' / tot_inc) * 100)^2
	}
	
	egen 			mwi_pre_index_hhi = rowtotal(*_amnt_0_persq)
	drop 			*_persq 
	
	lab var 		mwi_pre_index_hhi "Herfindahl-Hirschman index of LD"
	
	
************************************************************************
**# add educational engagement variable
************************************************************************

preserve 

* load data 
	use 			"$root/wave_0`w'/hh_mod_c_19", clear
	
* merge with roster data to get age 
	merge 			1:1 y4 PID using "$root/wave_0`w'/hh_mod_b_19"

* keep children age 5-18 and format variable
	rename 			hh_b05a age
	keep 			if age >=5 & age <= 18
	rename 			hh_c13 edu_act
	replace 		edu_act = 0 if edu_act == 2

* drop if never attended school because still too young
	drop 			if hh_c06 == 2 & hh_c07a == 1
	
* replace other reasons for never attending school with "no"
	replace 		edu_act = 0 if edu_act == . & hh_c06 == 2
	
* collapse to hh level 
	collapse 		(max) edu_act, by (y4)

* save tempfile and merge 
	tempfile 		edu
	save 			`edu'

restore 
	
	merge 			1:1 y4 using `edu', nogen
	
	
************************************************************************
**# close matters
************************************************************************
	
* add country & wave variables 
	gen 			wave = 0
	gen 			country = 2
	rename 			y4_hhid hhid_mwi
	order 			country wave hhid 
	
* rename region for consistency with post rounds
	drop 				region
	rename 				district region
	replace 			region = region + 2000	
	
* save round file
	save			"$export/wave_0`w'/r`w'", replace
	
	log close

/* END */		