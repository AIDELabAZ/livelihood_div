* Project: diversification
* Created on: Jan 2022
* Created by: amf
* Edited by: amf
* Last edit: Jan 2022
* Stata v.17.0

* does
	* reads in baseline Nigeria data
	* builds data for LD 
	* outputs HH income dataset

* assumes
	* raw Nigeria data

* TO DO:
	* complete
	

* **********************************************************************
**# setup
* **********************************************************************

* define 
	global	root	=	"$data/nigeria/raw"
	global	export	=	"$data/nigeria/refined"
	global	logout	=	"$data/nigeria/logs"

* open log
	cap log 		close
	log using		"$logout/nga_build", append

* set local wave number & file number
	local			w = 0
	
* make wave folder within refined folder if it does not already exist 
	capture mkdir "$export/wave_0`w'" 
		
		
* ***********************************************************************
**# household data
* ***********************************************************************

* load data
	use 			"$root/wave_0`w'/sect1_harvestw4", clear

* rename other variables 
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
					(max) sexhh, by(hhid zone state lga sector)
	lab var			hhsize "Household size"
	lab var 		hhsize_adult "Household size - only adults"
	lab var 		hhsize_child "Household size - children 0 - 18"
	lab var 		hhsize_schchild "Household size - school-age children 5 - 18"

* save tempfile 
	tempfile 		temp0
	save 			`temp0'

	
* ***********************************************************************
**# remittances
* ***********************************************************************
	
* load data
	use 			"$root/wave_0`w'/sect6_harvestw4", clear	
	
* convert currencies 
	* only 23 observations report in foreign currencies, replace with missing
	replace 		s6q4a = . if s6q4b != 4 & s6q4b < .
	replace 		s6q8a = . if s6q8b != 4 & s6q8b < .
	
* rename variables
	rename 			s6q1__1 cash_for_0 
	rename 			s6q4a cash_for_amnt_0
	rename 			s6q1__2 kind_for_0 
	rename 			s6q8a kind_for_amnt_0
	rename 			s6q1__3 cash_dom_0
	rename 			s6q12 cash_dom_amnt_0
	rename 			s6q1__4 kind_dom_0
	rename 			s6q17 kind_dom_amnt_0

* replace missing values with zero
	foreach 		var in cash_for_amnt_0 kind_for_amnt_0 ///	
						cash_dom_amnt_0 kind_dom_amnt_0 {
		replace 		`var' = 0 if `var' >= .
	}
	
* collapse to hh level 
	collapse (sum) cash_for_amnt_0 kind_for_amnt_0 cash_dom_amnt_0 ///
	kind_dom_amnt_0 (max) cash_for_0 kind_for_0 cash_dom_0 kind_dom_0, by(hhid)
	
* save tempfile 
	tempfile 		temp1
	save 			`temp1'


* ***********************************************************************
**# other income  
* ***********************************************************************

* load data
	use 			"$root/wave_0`w'/sect13_harvestw4", clear

* rename variables
	rename 			s13q1 inc_
	rename 			s13q2 amnt_
	replace 		source_cd = source_cd - 100
	keep 			hhid inc_ amnt_ source_cd
	
* reshape data and rename/generate inc vars
	reshape 		wide inc_ amnt_, i(hhid) j(source_cd)

* rename variables 
	rename 			inc_1 save_inc_0
	rename 			amnt_1 save_inc_amnt_0
	rename 			inc_2 rent_nonag_0
	rename 			amnt_2 rent_nonag_amnt_0
	rename 			inc_3 pen_inc_0
	rename 			amnt_3 pen_inc_amnt_0
	rename 			inc_4 oth_inc_0
	rename 			amnt_4 oth_inc_amnt_0
	
	foreach 		var in save_inc_0 rent_nonag_0 pen_inc_0 oth_inc_0 {
		replace 	`var' = 0 if `var' == 2
	}
	
* save tempfile 
	tempfile 		temp2
	save 			`temp2'
	
	
* ***********************************************************************
**#  labor & time use  
* ***********************************************************************

* load data
	use 			"$root/wave_0`w'/sect3a_harvestw4", clear

* cash payments	
	* rename indicator vars
		rename 			s3q4 wage_emp_0
		replace 		wage_emp_0 = 0 if wage_emp_0 == 2

	* generate conversion variables
		gen 			days_per_month = 365/12
		gen 			weeks_per_month = 365/7/12
	
	* rename & format variables
		egen 			main_months = rowtotal(s3q16a__1-s3q16a__23)
		replace 		main_months = 12 if s3q16a__0 == 1
		
		rename 			s3q17 main_weeks_per_month
		rename 			s3q18 main_hrs_per_week
		
		rename 			s3q21a main_pay
		rename 			s3q21b main_pay_unit
	
	* estimate number of days worked per week (assume 8 hour days)
		replace 		main_hrs_per_week = 0 if main_hrs_per_week == .
		gen 			main_days_per_week = cond(main_hrs_per_week <= 8 & main_hrs_per_week > 0,1, ///
							cond(main_hrs_per_week > 8 & main_hrs_per_week <= 16,2, ///
							cond(main_hrs_per_week > 16 & main_hrs_per_week <= 24, 3, ///
							cond(main_hrs_per_week > 24 & main_hrs_per_week <= 32, 4, ///
							cond(main_hrs_per_week > 32 & main_hrs_per_week <= 40, 5, ///
							cond(main_hrs_per_week > 40 & main_hrs_per_week <= 48, 6, ///
							cond(main_hrs_per_week > 48, 7,.)))))))
	
	* convert day, hour, & fortnight to weekly
		gen 			main_pay_per_week = main_pay * main_days_per_week ///
							if main_pay_unit == 2							//days
		replace 		main_pay_per_week = main_pay if main_pay_unit == 3 //weeks
		replace 		main_pay_per_week = main_pay/2 if main_pay_unit == 4 //fns
		replace 		main_pay_per_week = main_pay * main_hrs_per_week if ///
							main_pay_unit == 1								//hrs
		* if report working all weeks make 165/7/12 instead of 4
		replace 		main_weeks_per_month = weeks_per_month if ///
							main_weeks_per_month == 4
	
	* convert weekly (including day, hr, fortnight) income to monthly 	
		gen 			main_pay_per_month = main_pay if main_pay_unit == 5 //months
		replace 		main_pay_per_month = main_pay_per_week * ///
							main_weeks_per_month if main_pay_per_week != . //dy, wk,fn,hr
								
	* convert all main income to annual
		gen 			main_pay_annual = main_pay_per_month * main_months
		replace 		main_pay_annual = main_pay if main_pay_unit == 8
		replace 		main_pay_annual = main_pay * 2 if main_pay_unit == 7
		replace 		main_pay_annual = main_pay * 4 if main_pay_unit == 6
	
* in-kind payments 
	* format in-kind payment variables
		rename 			s3q24a main_pay_kind
		rename 			s3q24b main_pay_kind_unit
		
	* convert kind day, hour, & fortnight to weekly
		gen 			main_pay_kind_per_week = main_pay_kind * main_days_per_week ///
							if main_pay_kind_unit == 2							//days
		replace 		main_pay_kind_per_week = main_pay_kind if ///
							main_pay_kind_unit == 3   							//weeks
		replace 		main_pay_kind_per_week = main_pay_kind / 2 ///
							if main_pay_kind_unit == 4							 //fns
							
	* convert weekly (including day, hr, fortnight) income to monthly 	
		gen 			main_pay_kind_per_month = main_pay_kind if ///
							main_pay_kind_unit == 5   							//months
		replace 		main_pay_kind_per_month = main_pay_kind_per_week * ///
							main_weeks_per_month if main_pay_kind_per_week != . //dy, wk,fn,hr
								
	* convert all main in-kind income to annual
		gen 			main_pay_kind_annual = main_pay_kind_per_month * main_months
		replace 		main_pay_kind_annual = main_pay_kind if main_pay_kind_unit == 8
		replace 		main_pay_kind_annual = main_pay_kind * 2 ///
							if main_pay_kind_unit == 7
		replace 		main_pay_kind_annual = main_pay_kind * 4 ///
							if main_pay_kind_unit == 6	

* add cash & in-kind payments 
	replace 			main_pay_annual = 0 if main_pay_annual >= .
	gen 	 			wage_emp_amnt_0 = main_pay_annual
	replace 			wage_emp_amnt_0 = main_pay_annual + main_pay_kind_annual if ///
							main_pay_kind_annual != .
							
* collapse to household 	
	collapse 			(sum) wage_emp_amnt_0 (max) wage_emp_0, by(hhid)
						
* save tempfile 
	tempfile 		temp3
	save 			`temp3'


* ***********************************************************************
**# safety nets 
* ***********************************************************************	

* load data
	use 			"$root/wave_0`w'/sect14b_harvestw4", clear
	
* drop supplemental feeding program 
	drop 			if snet_cd == 5
	
* format variables 
	replace 		s14q1c__1 = 1 if s14q1c__4 == 1
	foreach 		var in s14q1c__1 s14q1c__2 s14q1c__3 s14q2a ///
						s14q2d s14q2f {
		replace 		`var' = 0 if `var' >= .
	}
	
* generate variables for government and ngo assistance
	egen 			gov_asst_0 = rowmax(s14q1c__1 s14q1c__2 s14q1c__3) ///
						if snet > 6 & snet < 10
	replace 		gov_asst_0 = 0 if gov_asst_0 == .
	egen 			gov_asst_amnt_0 = rowtotal (s14q2a s14q2d s14q2f) ///
						if snet > 6 & snet < 10
	replace 		gov_asst_amnt_0 = 0 if gov_asst_amnt_0 == .		
	
	egen 			ngo_asst_0 = rowmax(s14q1c__1 s14q1c__2 s14q1c__3) ///
						if snet == 12
	replace 		ngo_asst_0 = 0 if ngo_asst_0 == .
	egen 			ngo_asst_amnt_0 = rowtotal (s14q2a s14q2d s14q2f) ///
						if snet == 12
	replace 		ngo_asst_amnt_0 = 0 if ngo_asst_amnt_0 == .

* replace asst variable with 0 if gov or ngo (captured above)
	foreach 		var in s14q1c__1 s14q1c__2 s14q1c__3 s14q2a ///
						s14q2d s14q2f {
		replace 		`var' = 0 if snet == 7 | snet == 8 | snet == 9 ///
							| snet == 12
	} 		

* format other safety net variables by type
	rename 			s14q1c__1 cash_asst_0	
	rename 			s14q1c__2 food_asst_0
	rename 			s14q1c__3 kind_asst_0
	rename 			s14q2a cash_asst_amnt_0	
	rename 			s14q2d food_asst_amnt_0
	rename 			s14q2f kind_asst_amnt_0
	
* collapse by hhid
	collapse 		(max) *asst_0 (sum) *asst_amnt_0, by (hhid)

* save tempfile 
	tempfile 		temp4
	save 			`temp4'

	
* ***********************************************************************
**# non-farm enterprise  
* ***********************************************************************	
	
* load data
	use 			"$root/wave_0`w'/sect9b_harvestw4", clear
	
* months operational
	egen 			bus_months = rowtotal (s9q10__1-s9q10__18)
	replace 		bus_months = 12 if bus_months == 13
	replace 		bus_months = 12 if s9q10__0 == 1
	
	tempfile 		months_op
	save 			`months_op'
	
* load data 
	use 			"$root/wave_0`w'/sect9b_harvestw4", clear

* format and merge with months to calc sales, collapse by hhid
	rename 			s9q27 monthly_sales	
	merge 			1:1 hhid ent_id using `months_op', assert(3) nogen 	
	gen 			nfe_inc_amnt_0 = monthly_sales * bus_months	
	collapse 		(sum) nfe_inc_amnt_0, by(hhid)
	gen 			nfe_inc_0 = 1 if nfe_inc_amnt_0 > 0 & nfe_inc_amnt_0 < .
	
* save tempfile 
	tempfile 		temp5
	save 			`temp5'
	
	
* ***********************************************************************
**# tree crop sales   
* ***********************************************************************	
	
* load data
	use 			"$root/wave_0`w'/secta3iii_harvestw4", clear

* format variables
	rename 			sa3iiiq7 tree_inc_0
	replace 		tree_inc_0 = 0 if tree_inc_0 == 2
	rename 			sa3iiiq14 tree_inc_amnt_0
	replace 		tree_inc_amnt_0 = 0 if tree_inc_amnt_0 >= .
	
* collapse to hhid
	collapse 		(max) tree_inc_0 (sum) tree_inc_amnt_0, by(hhid)

* save tempfile 
	tempfile 		temp6
	save 			`temp6'
	
	
* ***********************************************************************
**# crop sales   
* ***********************************************************************	

* load unit data (use data missing condition and unit so merged with this)
	use 			"$root/wave_0`w'/secta3i_harvestw4", clear

	* rename variables 
	rename 			sa3iq6i crop_quant
	rename 			sa3iq6ii crop_unit
	rename 			sa3iq6_4 crop_size
	rename 			sa3iq6_2 crop_cond
	rename 			sa3iq6_conv crop_conv
	
	* collapse to hhid and crop code
	collapse 		(sum) crop_quant, by(hhid cropcode crop_unit crop_size ///
						crop_cond crop_conv)
	drop 			if crop_q == 0
	duplicates 		tag hhid cropcode, gen(dups)
	drop 			if dups >= 1 // drop if report in 2+ different units
						// because surveys asks those ppl in next section 

	* save tempfile 
	tempfile 		units
	save 			`units'
	
* load sales and uses data
	use 			"$root/wave_0`w'/secta3ii_harvestw4", clear	

	* merge with total harvest to get conversion units
	merge 			1:1 hhid cropc using `units'		

	* rename/format variables 	
	rename 			sa3iiq5a unproc_quant
	rename 			sa3iiq6 unproc_val

	replace 		crop_conv = sa3iiq1_conv if crop_conv == .
	replace 		crop_unit = sa3iiq1c if crop_unit == .
	replace 		crop_size = sa3iiq1d if crop_size == .
	replace 		crop_cond = sa3iiq1b if crop_cond == .
	
	replace 		crop_size = -1 if crop_size == .
	replace 		crop_cond = -1 if crop_cond == .
	// these are often missing, this makes the missing obs 
		// group together rather than all be missing 
			
	rename 			sa3iiq11aa fut_quant
	rename 			sa3iiq12a anim_quant
	rename 			sa3iiq13a cons_quant
	rename 			sa3iiq14a lab_quant
	rename 			sa3iiq15a nlab_quant
	rename 			sa3iiq16a reim_quant
	rename 			sa3iiq17a gift_quant
	rename 			sa3iiq18a lost_quant
	rename 			sa3iiq18aa proc_quant
	
	* caculate price per kg
	gen 			unproc_kg = unproc_quant * crop_conv
	gen 			price_per_kg = unproc_val/unproc_kg
	
* get median price per kg by smallest geo area
	egen			lga_grp = group(cropcode zone state lga)
	egen			st_grp = group(cropcode zone state)
	egen			zn_grp = group(cropcode zone)
	egen			ctry_grp = group(cropcode)

	foreach 		l in lga st zn ctry {
		bysort 			`l'_grp: egen `l'_grp_count = count(`l'_grp) ///
							if price_per_kg != .
		bysort 			`l'_grp: egen `l'_med = median(price_per_kg) ///
							if `l'_grp_count >= 10
	}

	gen 			med_pr_per_kg = ctry_med
	replace 		med_pr_per_kg = st_med if st_med != .
	replace 		med_pr_per_kg = zn_med if zn_med != .
	replace 		med_pr_per_kg = lga_med if lga_med != .
		
* convert uses into kgs & calculate value to each use, then total value
	foreach 		i in fut anim cons lab nlab reim gift proc {
		* note: not including lost crops
		gen 			`i'_kg = `i'_quant * crop_conv
		gen 			`i'_val = `i'_kg * med_pr_per_kg
		replace 		`i'_val = 0 if `i'_val >= .
	}
	replace 		unproc_val = 0 if unproc_val >= .
	
	* drop value of lost crops & replace proccessed sales with reported values
	replace 		proc_val = sa3iiq21 if sa3iiq21 != . 
	
	* aggregate value
	egen 			crop_inc_amnt_0 = rowtotal(*_val)	
	gen 			crop_inc_0 = cond(crop_inc_amnt_0 > 0, 1,0)	
	
	collapse 		(sum) crop_inc_amnt_0 (max) crop_inc_0, by(hhid)
	
* save tempfile 
	tempfile 		temp7
	save 			`temp7'		

	
* ***********************************************************************
**# livestock products  
* ***********************************************************************

* load & format dung sales and animal power data
	use 			"$root/wave_0`w'/sect11k1_plantingw4", clear
	
	rename 			s11k1q4 dung
	replace 		dung = 0 if dung == .
	rename 			s11k1q2 power
	replace 		power = 0 if power == .
	collapse 		(sum) dung power, by(hhid)
	
	tempfile 		dungpower
	save 			`dungpower'
	
* load & format milk data	
	use 			"$root/wave_0`w'/sect11k2_plantingw4", clear
	
	* rename & format variables
	rename 			s11k2q8 cons_ltrs_per_wk
	rename 			s11k2q10 sold_ltrs_per_wk
	rename 			s11k2q11 revs_per_wk
	rename 			s11k2q12 proc_cons_ltrs_per_wk
	rename 			s11k2q13 proc_sold_ltrs_per_wk
	rename 			s11k2q14 proc_revs_per_wk
	
	gen 			pr_per_ltr = revs_per_wk / sold_ltrs_per_wk
	gen 			proc_pr_per_ltr = proc_revs_per_wk / proc_sold_ltrs_per_wk
	
	* calc median price per liter to use for consumption value
	egen			lga_grp = group(animal_cd zone state lga)
	egen			st_grp = group(animal_cd zone state)
	egen			zn_grp = group(animal_cd zone)
	egen			ctry_grp = group(animal_cd)
	
	foreach 		l in lga st zn ctry {
		bysort 			`l'_grp: egen `l'_grp_count = count(`l'_grp) ///
							if revs_per_wk != .
		bysort 			`l'_grp: egen `l'_med = median(pr_per_ltr) ///
							if `l'_grp_count >= 10
	}					
	foreach 		l in lga st zn ctry {						
		bysort 			`l'_grp: egen proc_`l'_grp_count = count(`l'_grp) ///
							if proc_revs_per_wk != .					
		bysort 			`l'_grp: egen proc_`l'_med = ///
							median(proc_pr_per_ltr) if proc_`l'_grp_count >= 10
	}
		
	gen 			med_pr_per_ltr = ctry_med
	replace 		med_pr_per_ltr = zn_med if zn_med != .
	replace 		med_pr_per_ltr = st_med if st_med != .
	replace 		med_pr_per_ltr = lga_med if lga_med != .
		
	gen 			med_proc_pr_per_ltr = proc_ctry_med
	replace 		med_proc_pr_per_ltr = proc_zn_med if proc_zn_med != .
	replace 		med_proc_pr_per_ltr = proc_st_med if proc_st_med != .
	replace 		med_proc_pr_per_ltr = proc_lga_med if proc_lga_med != .
			
	* calc total value of milk
	gen 			wks_per_yr = 365 / 7
	
	gen 			cons_val = med_pr_per_ltr * cons_ltrs_per_wk 
	replace 		cons_val = 0 if cons_val >= .
	gen 			proc_cons_val = med_proc_pr_per_ltr * proc_cons_ltrs_per_wk 
	replace 		proc_cons_val = 0 if proc_cons_val >= .
	
	replace 		revs_per_wk = 0 if revs_per_wk >= .
	replace 		proc_revs_per_wk = 0 if proc_revs_per_wk >= .
	gen 			milk = cons_val + revs_per_wk + proc_cons_val + proc_revs_per_wk
	replace 		milk = milk * wks_per_yr
	collapse		(sum) milk, by(hhid)

	tempfile 		milk
	save 			`milk'	
	
* load & format eggs data
	use 			"$root/wave_0`w'/sect11k3_plantingw4", clear

	rename 			s11k3q8 egg_sales_3m
	rename 			s11k3q2 lay_months
	
	* get egg sales per month
	gen 			egg_sales_1m = egg_sales_3m /3
	gen 			eggs = egg_sales_1m * lay_months
	
	* collapse to hhid
	replace 		eggs = 0 if eggs >= . 
	collapse 		(sum) eggs, by(hhid)
	
	tempfile 		eggs
	save 			`eggs'
	
* load & format meat data
	use 			"$root/wave_0`w'/sect11i_plantingw4", clear
	
	rename 			s11iq19a num_sl
	rename 			s11iq19a1 tot_rev_sl
	gen 			pr_per_sl = tot_rev_sl / num_sl
	rename 			s11iq19b num_cons
	
	* calc median price per slaughtered animal to use for consumption value
	egen			lga_grp = group(animal_cd zone state lga)
	egen			st_grp = group(animal_cd zone state)
	egen			zn_grp = group(animal_cd zone)
	egen			ctry_grp = group(animal_cd)
	
	foreach 		l in lga st zn ctry {
		bysort 			`l'_grp: egen `l'_grp_count = count(`l'_grp) ///
							if tot_rev_sl != .
		bysort 			`l'_grp: egen `l'_med = median(pr_per_sl) ///
							if `l'_grp_count >= 10
	}					

	gen 			med_pr_per_sl = ctry_med
	replace 		med_pr_per_sl = zn_med if zn_med != .
	replace 		med_pr_per_sl = st_med if st_med != .
	replace 		med_pr_per_sl = lga_med if lga_med != .
	
	* calculate consupmtion value 
	gen 			cons_val = num_cons * med_pr_per_sl
	
	* add consumption and sales value
	replace 		tot_rev_sl = 0 if tot_rev_sl >= .	
	replace 		cons_val = 0 if cons_val >= .
	
	gen 			meat = tot_rev_sl + cons_val
	
	* multipy by 4 if 3 month recall perdiod
	replace 		meat = meat * 4 if recall_period == "3 months"

	* collapse to hh level
	collapse 		(sum) meat, by(hhid)
	
* combine datasets 
	merge 			1:1 hhid using `dungpower', nogen
	merge 			1:1 hhid using `milk', nogen
	merge 			1:1 hhid using `eggs', nogen
	
	gen 			live_prod_0 = 0	
	foreach 		var in meat milk eggs dung power {
		replace 		`var' = 0 if `var' == .
		replace 		live_prod_0 = 1 if `var' > 0
	}
	gen 			live_prod_amnt_0 = meat + milk + eggs + dung + power
	drop 			meat milk eggs dung power

* save tempfile 
	tempfile 		temp8
	save 			`temp8'
	
	
* ***********************************************************************
**# livestock sales  (slaughtered with live prods)
* ***********************************************************************

* load & format data
	use 			"$root/wave_0`w'/sect11i_plantingw4", clear
	
	rename 			s11iq17 live_inc_amnt_0 
	replace 		live_inc_amnt_0 = live_inc_amnt_0 * 4 ///
						if recall_period == "3 months"
	replace 		live_inc_amnt_0 = 0 if live_inc_amnt_0 >= .
	
	* collapse to hh level 
	collapse 		(sum) live_inc_amnt_0, by(hhid)
	
	* gen indicator 
	gen 			live_inc_0 = cond(live_inc_amnt_0 > 0, 1, 0)

* save tempfile 
	tempfile 		temp9
	save 			`temp9'
	

* ***********************************************************************
**# ag rental income
* ***********************************************************************

* load & format ag equipment data
	use 			"$root/wave_0`w'/secta4_harvestw4", clear
	
	rename 			sa4q7 equip
	replace 		equip = 0 if equip >= .
	
	collapse 		(sum) equip, by (hhid)

	tempfile 		equip
	save 			`equip'
	
* load & format plot rental data
	use 			"$root/wave_0`w'/sect11b1_plantingw4", clear
	
	rename 			s11b1q31 cash_pay
	rename 			s11b1q32 cash_period
	rename 			s11b1q33 kind_pay
	rename 			s11b1q34 kind_period
	
	gen 			plot = cash_pay if cash_period != 5
	replace 		plot = cash_pay / 6 if s11b1q32_os == "6 YEARS" | ///
						s11b1q32_os == "SIX YEARS" 
	replace 		plot = cash_pay / 3 if s11b1q32_os == "THREE YEARS"
	replace 		plot = cash_pay / 2 if s11b1q32_os == "TWO YEAR"
	
	replace 		plot = 0 if plot >= .
	replace 		kind_pay = 0 if kind_pay >= .
	replace 		plot = plot + kind_pay
	
	collapse 		(sum) plot, by(hhid)
	
* merge with equipment and format
	merge 			1:1 hhid using `equip', nogen
	replace 		plot = 0 if plot == .
	replace 		equip = 0 if equip == .
	gen 			ag_rent_amnt_0 = plot + equip
	gen 			ag_rent_0 = cond(ag_rent_amnt_0 > 0, 1, 0)
	drop 			equip plot 
	
* save tempfile 
	tempfile 		temp10
	save 			`temp10'	

	
* ***********************************************************************
**# merge  
* ***********************************************************************	
	
* combine dataset 
	use 			`temp0', clear
	forval 			x = 1/10  {
		merge 			1:1 hhid using `temp`x'', nogen
	}
	
* replace missing values with 0s	
	quietly: ds		*_0
	foreach 		var in `r(varlist)' {
		replace 		`var' = 0 if `var' >= .
	}


************************************************************************
**# combine similar variables with few observations, format, label
************************************************************************

* assistance
	gen 			asst_inc_0 = 0
	replace 		asst_inc_0 = 1 if cash_asst_0 == 1 | ///
						food_asst_0 == 1 | kind_asst_0 == 1  | ///
						gov_asst_0 == 1 | ngo_asst_0 == 1
	gen 			asst_inc_amnt_0 = cash_asst_amnt_0 + food_asst_amnt_0 ///
						+ kind_asst_amnt_0 + gov_asst_amnt_0 + ngo_asst_amnt_0
	drop 			cash_asst_* food_asst_* kind_asst_* gov_asst_* ngo_asst_*
	
* in-kind remittances
	gen 			kind_inc_0 = cond(kind_for_0 == 1, 1, ///
						cond(kind_dom_0 == 1, 1, 0))
	gen 			kind_inc_amnt_0 = kind_for_amnt_0 + kind_dom_amnt_0
	drop 			kind_for* kind_dom*

* label variables
	lab var 		cash_for_0 "Foreign Remittances"
	lab var 		cash_dom_0 "Domestic Remittances"
	lab var 		kind_inc_0 "In-Kind Remittances"
	lab var 		save_inc_0 "Savings, Interest, Investment"	
	lab var 		rent_nonag_0 "Rental Income (Non-Ag)"
	lab var 		pen_inc_0 "Pension"
	lab var			oth_inc_0 "Other"
	lab var 		nfe_inc_0 "Non-Farm Enterprises"
	lab var 		tree_inc_0 "Tree Crop Sales"
	lab var 		crop_inc_0 "Crop Income"
	lab var 		live_prod_0 "Livestock Product Income"
	lab var 		live_inc_0 "Livestock Sales"
	lab var 		ag_rent_0 "Rental Income (Ag)"
	lab var 		asst_inc_0 "Cash, Food, or In-kind Assistance"


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
		mi impute 			pmm `var' i.state if `var' != 0, add(1) rseed(245780) ///
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
		keep 			hhid *_0 zone state lga
		
		* count number of possible income sources
		ds 				*_0
		gen 			tot_count = `c(k)' -4 //number of columns minus 4 (hhid zone state lga)

		* generate count of income sources and total number of observations by each geographic level
		gen 			one = 1
		foreach 		l in zone state lga {
			foreach v 		of varlist *_0 {
				egen 			`v'_count_`l' = max(`v'), by(`l')
			}
			egen			`l'_count = rowtotal(*_count_`l')
			egen 			`l'_obs = total(one), by(`l')
		}
	
		* generate combined var using the smallest geographic area with at least 10 obs
		gen 	 		geo_count = zone_count if zone_obs >= 10
		replace 		geo_count = state_count if state_obs >= 10
		replace 		geo_count = lga_count if lga_obs >= 10

		
		* count number of hh income sources
		egen 			hh_count = rowtotal(*_0)
		
		* generate index
		gen 			nga_pre_index_all = 1- (hh_count / tot_count)
		lab var 		nga_pre_index_all "1- LD index with all 15 income sources in NGA as denominator"
		gen				nga_pre_index_geo = 1- (hh_count / geo_count)
		lab var 		nga_pre_index_geo "1- LD index with denominator as number of income sources at lowest geo level with 10 obs"
		keep 			hhid nga_pre_index* 
		
		* tempfile 
		tempfile 		ind
		save 			`ind'
		
	restore
	
	merge 				1:1 hhid using `ind', assert(3) nogen

	gen 				nga_pre_index_all_phhm = nga_pre_index_all / hhsize
	lab var 			nga_pre_index_all_phhm "nga_pre_index_all divided by hh size"
	gen 				nga_pre_index_geo_phhm = nga_pre_index_geo / hhsize
	lab var 			nga_pre_index_geo_phhm "nga_pre_index_geo divided by hh size"
	
* weighted by amount (Herfindahl-Hirschman Index)
	egen 				tot_inc = rowtotal(*amnt_0)
	ds 					*_amnt_0 
	foreach 			var in `r(varlist)' {
		gen 			`var'_persq = ((`var' / tot_inc) * 100)^2
	}
	
	egen 				nga_pre_index_hhi = rowtotal(*_amnt_0_persq)
	drop 				*_persq 
		
	lab var 			nga_pre_index_hhi "Herfindahl-Hirschman index of LD"


************************************************************************
**# add educational engagement variable
************************************************************************

preserve 

* load data 
	use 			"$root/wave_0`w'/sect2_harvestw4", clear
	
* merge with roster data to get age 
	merge 			1:1 hhid indiv using "$root/wave_0`w'/sect1_harvestw4", nogen 

* keep children age 5-18 and format variable
	rename 			s1q4 age
	keep 			if age >= 3 & age <= 18
	rename 			s2aq13a edu_act
	replace 		edu_act = 0 if edu_act == 2

* drop if never attended school because still too young
	drop 			if s2aq6 == 2 & s2aq7 == 1
	
* replace other reasons for never attending school with "no"
	replace 		edu_act = 0 if edu_act == . & s2aq6 == 2
	
* collapse to hh level 
	collapse 		(max) edu_act, by (hhid)

* save tempfile and merge 
	tempfile 		edu
	save 			`edu'

restore 
	
	merge 			1:1 hhid using `edu', nogen
	
	
************************************************************************
**# close matters
************************************************************************
	
* add country & wave variables 
	gen 			wave = 0
	gen 			country = 3
	rename 			hhid hhid_nga
	order 			country wave hhid 
	
* save round file
	save			"$export/wave_0`w'/r`w'", replace

	log close
	
/* END */		