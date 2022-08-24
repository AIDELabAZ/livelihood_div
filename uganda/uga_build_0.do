* Project: diversification
* Created on: June 2021
* Created by: amf
* Edited by: amf
* Last edit: June 2021 
* Stata v.17.0

* does
	* reads in baseline Uganda data
	* builds data for LD 
	* outputs HH income dataset

* assumes
	* raw Uganda data

* TO DO:
	* complete
	
	
* **********************************************************************
**# setup
* **********************************************************************

* define
	global	root	=	"$data/uganda/raw"
	global	export	=	"$data/uganda/refined"
	global	logout	=	"$data/uganda/logs"
	global	fies	=	"$data/analysis/raw/Uganda"
	
* open log
	cap log 		close
	log using		"$logout/uga_build", append
	
* set local wave number & file number
	local			w = 0
	
* make wave folder within refined folder if it does not already exist 
	capture mkdir "$export/wave_0`w'" 	
	
	
* ***********************************************************************
**#  household data
* ***********************************************************************
	
* load data
	use 			"$root/wave_0`w'/Household/GSEC2", clear

* rename other variables 
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
					(max) sexhh, by(hhid)
	lab var			hhsize "Household size"
	lab var 		hhsize_adult "Household size - only adults"
	lab var 		hhsize_child "Household size - children 0 - 18"
	lab var 		hhsize_schchild "Household size - school-age children 5 - 18"

	drop 			if hhsize == 0
	
* save tempfile 
	tempfile 		temp0
	save 			`temp0'	
	

* ***********************************************************************
**# other income
* ***********************************************************************

* load and format indicator data
	use 			"$root/wave_0`w'/Household/GSEC7_2", clear	

	ds 				s11q*
	foreach 		var in `r(varlist)' {
		replace 		`var' = 0 if `var' >= .
	}
	
* rename indicator variables
	rename 			s11q04__0 rent_nonag_0
	rename 			s11q04__1 rent_inc_0
	rename 			s11q04__2 royal_0
	rename 			s11q04__3 curr_acc_0
	rename 			s11q04__4 oth_acc_0
	rename 			s11q04__5 interest_0
	rename 			s11q04__6 dividends_0
	rename 			s11q04__7 bonds_0
	rename 			s11q04__8 treas_0
	rename 			s11q04__9 pen_inc_0
	drop 			s11q04__10 s11q04__11 //these are split out later by kind/cash
	rename 			s11q04__12 asset_0
	rename 			s11q04__13 oth_inc_0
	
	keep 			hhid *_0

* save tempfile 
	tempfile 		inc_ind
	save 			`inc_ind'
	
* load and format amount data	
	use 			"$root/wave_0`w'/Household/GSEC7_3", clear	

	keep 			hhid income s11q05 s11q06
	reshape 		wide s11q05 s11q06, i(hhid) j(income)
	ds 				s11q*
	foreach 		var in `r(varlist)' {
		replace 		`var' = 0 if `var' >= .
	}
	
* rename amount variables	
	rename 			s11q050 rent_nonag_amnt_0
	replace 		rent_nonag_amnt_0 = rent_nonag_amnt_0 + s11q060 
	rename 			s11q051 rent_inc_amnt_0
	replace 		rent_inc_amnt_0 = rent_inc_amnt_0 + s11q061 
	rename 			s11q054 oth_acc_amnt_0
	replace 		oth_acc_amnt_0 = oth_acc_amnt_0 + s11q064 
	rename 			s11q055 interest_amnt_0
	replace 		interest_amnt_0 = interest_amnt_0 + s11q065 
	rename 			s11q056 dividends_amnt_0
	replace 		dividends_amnt_0 = dividends_amnt_0 + s11q066 
	rename 			s11q059 pen_inc_amnt_0
	replace 		pen_inc_amnt_0 = pen_inc_amnt_0 + s11q069 
	rename 			s11q0510 cash_dom_amnt_0
	rename 			s11q0610 kind_dom_amnt_0 
	rename 			s11q0511 cash_for_amnt_0
	rename 			s11q0611 kind_for_amnt_0
	rename 			s11q0512 asset_amnt_0
	replace 		asset_amnt_0 = asset_amnt_0 + s11q0612 
	rename 			s11q0513 oth_inc_amnt_0
	replace 		oth_inc_amnt_0 = oth_inc_amnt_0 + s11q0613 
	
	drop 			*48 s11q06*
	
* save tempfile 
	tempfile 		inc_amnt
	save 			`inc_amnt'
	
* merge with indicator data 
	use 			`inc_ind', clear
	merge 			1:1 hhid using `inc_amnt'
	ds 				*_amnt_0
	foreach 		var in `r(varlist)' {
		replace 		`var' = 0 if `var' >=. & _m == 1
	}
	drop 			_m

	gen 			cash_dom_0 = cond(cash_dom_amnt_0 > 0,1,0)
	gen 			kind_dom_0 = cond(kind_dom_amnt_0 > 0,1,0)
	gen 			cash_for_0 = cond(cash_for_amnt_0 > 0,1,0)
	gen 			kind_for_0 = cond(kind_for_amnt_0 > 0,1,0)
	
* drop if no observations
	drop 			royal* curr_acc* bonds* treas*

* save tempfile 
	tempfile 		temp1
	save 			`temp1'

	
************************************************************************
**# labor & time use  
************************************************************************		
	
* MAIN JOB
	* load data
		use 			"$root/wave_0`w'/Household/GSEC8", clear

	* get set wages in terms of months
		* get hours worked per week
			egen 		wrk_hours_per_week = rowtotal(s8q36*)	
			
		* weeks per month
		 * NOTE: have to assume last 7 days is representative of whole month 
		  * bc not given weeks per month
			gen 		weeks_per_month = 365/7/12
			
		* hours worked per month
			gen 		wrk_hours_per_month = wrk_hours_per_week * weeks_per_month
			
		* days worked per month
			foreach 	x in a b c d e f g {
				gen 		day_`x' = cond(s8q36`x' > 0 & s8q36`x' < ., 1, 0)
			}
			egen 		wrk_days_per_week = rowtotal(s8q36*) 
			drop 		day_*
			gen 		wrk_days_per_month = wrk_days_per_week * weeks_per_month

	* monthly set wage 
		gen 			wage_per_month = s8q31a if s8q31c == 4 //months
		replace 		wage_per_month = s8q31a * weeks_per_month if s8q31c == 3 //weeks
		replace 		wage_per_month = s8q31a * wrk_days_per_month if s8q31c == 2 //days
		replace 		wage_per_month = s8q31a * wrk_hours_per_month if s8q31c == 1 //hours
		replace 		wage_per_month = 0 if wage_per_month >= .
		
	* add monthly commissions 
		replace 		s8q78 = 0 if s8q78 >= .
		replace 		wage_per_month = wage_per_month + s8q78
		
	* months worked per year
		gen 			wrk_months = s8q30 
		replace 		wrk_months = 0 if wrk_months >=.
		mean 			wrk_months if wrk_months != 0
		local 			mean_wrk_months = e(b)[1,1] //used as estimated months for sec job

	* annual wages
		gen 			wage_emp_amnt_0 = wage_per_month * wrk_months

	* format & save
		keep 			hhid PID wage_emp*
		tempfile 		temp_set
		save 			`temp_set'
		
* SECONDARY JOB

	* load data
		use 			"$root/wave_0`w'/Household/GSEC8", clear
	
	* get set wages in terms of months
		* get hours worked per week
			egen 		wrk_hours_per_week = rowtotal(s8q43*)	
			
		* weeks per month
		 * NOTE: have to assume last 7 days is representative of whole month 
		  * bc not given weeks per month
			gen 		weeks_per_month = 365/7/12
			
		* hours worked per month
			gen 		wrk_hours_per_month = wrk_hours_per_week * weeks_per_month
			
		* days worked per month
			foreach 	x in a b c d e f g {
				gen 		day_`x' = cond(s8q43`x' > 0 & s8q43`x' < ., 1, 0)
			}
			egen 		wrk_days_per_week = rowtotal(s8q43*) 
			drop 		day_*
			gen 		wrk_days_per_month = wrk_days_per_week * weeks_per_month

	* monthly set wage 
		gen 			wage_per_month = s8q45b if s8q45c == 4 //months
		replace 		wage_per_month = s8q45b * weeks_per_month if s8q45c == 3 //weeks
		replace 		wage_per_month = s8q45b * wrk_days_per_month if s8q45c == 2 //days
		replace 		wage_per_month = s8q45b * wrk_hours_per_month if s8q45c == 1 //hours
		replace 		wage_per_month = 0 if wage_per_month >= .
		
	* add monthly commissions 
		replace 		s8q80 = 0 if s8q80 >= .
		replace 		wage_per_month = wage_per_month + s8q80
		
	* NOTE: months worked per year at secondary job not given. Use average months worked at primary job
		
	* annual household wages
		gen 			sec_wage_emp_amnt_0 = wage_per_month * `mean_wrk_months'

		keep 			hhid PID sec_wage_emp_amnt_0
		merge 			1:1 hhid PID using `temp_set', assert(3) nogen 
		* note almost all workers with secondary job work on farm as primary job
		replace 		wage_emp_amnt_0 = wage_emp_amnt_0 + sec_wage_emp_amnt_0
		collapse 		(sum) wage_emp_amnt_0, by(hhid)
		gen 			wage_emp_0 = cond(wage_emp_amnt_0 > 0 , 1, 0)

* save tempfile 
	tempfile 			temp2
	save 				`temp2'
	

* ***********************************************************************
**# crop income
* ***********************************************************************	

* run over 2 seasons
	forval					 q = 1/2 {
		if 					`q' == 1 {
			local 				z = "a"
		}
		else {
			local 				z = "b"
		}

	* load & format data
		use 			"$root/wave_0`w'/Agriculture/AGSEC5`z'", clear
		
		if 				`q' == 1 {
			rename 			crpid cropID
		}	
		
		foreach 		x in 1 2 {
			
			rename 			s5`z'q06a_`x' tot_harv`x'
			rename 			s5`z'q06b_`x' tot_unit_code`x'
			rename 			s5`z'q06c_`x' tot_condition`x'
			rename 			harvestKG_`x' tot_harv_kg`x'
			rename 			s5`z'q06d_`x' farm_rep_conv`x'
			
			rename 			s5`z'q07a_`x' sold_harv`x'
			rename 			s5`z'q07c_`x' sold_unit_code`x'
			rename 			s5`z'q07b_`x' sold_condition`x'
			rename 			s5`z'q08_`x' sold_price`x'
			rename 			salesKG_`x' sold_harv_kg`x'
			
			rename 			s5`z'q13_`x' cons_quant`x'
			rename 			s5`z'q14a_`x' prod_quant`x'
			rename 			s5`z'q14b_`x' anim_quant`x'
			rename 			s5`z'q15_`x' seed_quant`x'
			
			replace 		conversionFactor_sales_`x' = 1 if sold_unit_code`x' == 1
			replace 		conversionFactor_`x' = 1 if tot_unit_code`x' == 1
			replace 		sold_harv_kg`x' = sold_harv`x' if sold_unit_code`x' == 1 
			replace 		tot_harv_kg`x' = tot_harv`x' if tot_unit_code`x' == 1 
							//kg conversion to kg was 0 rather than 1 for some crops
		}
		drop 				if tot_harv1 >= .
	* generate dataset with just secondary unit reporting and append to main data
		* this means duplicate crops withing households, will collapse later
		preserve 
			keep 			if tot_harv2 != .
			keep 			hhid cropID tot_harv2 tot_unit_code2 tot_condition2 tot_harv_kg2 ///
								 sold_harv2 sold_unit_code2 sold_condition2 sold_price2 sold_harv_kg2 ///
								farm_rep_conv2 cons_quant2 prod_quant2 anim_quant2 seed_quant2 ///
								conversionFactor_2 conversionFactor_sales_2
			rename 			*2 *1
			tempfile 		harv2
			save 			`harv2'
		restore
		append 				using `harv2'
		keep 				hhid cropID tot_*1 sold_*1 farm_rep_conv1 cons*1 prod*1 anim*1 seed*1 ///
								conversionFactor*1*
		rename 				*1 *
	
	* fill in missing conversion data with farm-reported values	to calc kgs to other uses 		
		/* Questionnaires do not ask for units or conditions for other uses (cons, seed, etc.)
		We assume the units are the same as those reported in the total harvest. But total
		harvest units are often missing, so we use farm-reported data to fill in missing 
		observations. We aggregate the estimates at the smallest geographic area with at 
		least 10 observations, taking the median */		
	
		* merge with regions				
			preserve 
				use 			"$root/wave_0`w'/Household/GSEC1", clear
				
				replace 		urban = urban + 1
				rename 			urban sector 
				rename 			dc_2018 dist_code
				rename			cc_2018 county_code
				rename 			sc_2018 town_code
				rename 			pc_2018 ward_code
				rename 			s1aq05b ea_code
				
				egen 			district = group(dist_code)
				egen 			county = group(dist_code county_code)
				egen 			town = group(dist_code county_code town_code)
				egen 			ward = group(dist_code county_code town_code ward_code)
				egen 			ea = group(dist_code county_code town_code ward_code ea_code)
				
				keep 			hhid district county town ward ea sector
			
				tempfile 		region
				save 			`region'
			restore 
				
			merge 				m:1 hhid using `region'
			drop 				if _m == 2
			drop 				_m 
			
		* get median farm-reported conversion factor by smallest geo area
			egen			ea_grp = group(cropID tot_unit_code tot_condition district county town ward ea)
			egen			ward_grp = group(cropID tot_unit_code tot_condition district county town ward)
			egen			town_grp = group(cropID tot_unit_code tot_condition district county town)
			egen			cnty_grp = group(cropID tot_unit_code tot_condition district county)
			egen			dist_grp = group(cropID tot_unit_code tot_condition district)
			egen			ctry_grp = group(cropID tot_unit_code tot_condition)

			foreach 		l in ctry dist cnty town ward ea {
				bysort 			`l'_grp: egen `l'_grp_count = count(`l'_grp) ///
									if farm_rep_conv != .
				bysort 			`l'_grp: egen `l'_med = median(farm_rep_conv) ///
									if `l'_grp_count >= 10
			}
		
			gen 			med_farm_rep_conv = ctry_med
			replace 		med_farm_rep_conv = dist_med if dist_med != .
			replace 		med_farm_rep_conv = town_med if town_med != .
			replace 		med_farm_rep_conv = ward_med if ward_med != .
			replace 		med_farm_rep_conv = ea_med if ea_med != .
				
			replace 		conversionFactor_1 = med_farm_rep_conv if conversionFactor_1 >= . | ///
								conversionFactor_1 == 0
			
			
			replace 		conversionFactor_sales_1 = med_farm_rep_conv if (tot_unit_code == sold_unit_code ///
								& tot_condition == sold_condition) & (conversionFactor_sales_1 >= . | ///
								conversionFactor_sales_1 == 0)
									
			drop 			*count *_med *_grp med_farm 
		
	* calc kgs using total harvest conversion factors (assume other sources reported in same units as total)
		replace 			tot_harv_kg = tot_harv * conversionFactor_1 if tot_harv_kg >= .
		replace 			sold_harv_kg = sold_harv * conversionFactor_sales_1 if sold_harv_kg >= .
		gen 				cons_kg = cons_quant * conversionFactor_1
		gen 				prod_kg = prod_quant * conversionFactor_1
		gen 				anim_kg = anim_quant * conversionFactor_1
		gen 				seed_kg = seed_quant * conversionFactor_1
		
		preserve 
			collapse 		(sum) *kg, by(hhid cropID district county town ward ea)
			tempfile 		kgs
			save 			`kgs'
		restore
		
	* calc median price per kg at lowest geographic level with at least 10 obs
		gen 				pr_per_kg = sold_price / sold_harv_kg
		
		egen				ea_grp = group(cropID district county town ward ea)
		egen				ward_grp = group(cropID district county town ward)
		egen				town_grp = group(cropID district county town)
		egen				cnty_grp = group(cropID district county)
		egen				dist_grp = group(cropID district)
		egen				ctry_grp = group(cropID)
	
		foreach 			l in ctry dist cnty town ward ea {
			bysort 				`l'_grp: egen `l'_grp_count = count(`l'_grp) ///
									if pr_per_kg != .
			bysort 				`l'_grp: egen `l'_med = median(pr_per_kg) ///
									if `l'_grp_count >= 10
		}
		
		gen 				med_pr_per_kg = ctry_med
		replace 			med_pr_per_kg = dist_med if dist_med != .
		replace 			med_pr_per_kg = town_med if town_med != .
		replace 			med_pr_per_kg = ward_med if ward_med != .
		replace 			med_pr_per_kg = ea_med if ea_med != .
		
		collapse 			(max) med_pr_per_kg, by(cropID district county town ward ea)
		
		tempfile 			pr
		save 				`pr'
	
	* merge kgs and prices, calc value
		use 				`kgs', clear
		merge 				m:1 cropID district county town ward ea using `pr', assert(3) nogen
		drop 				tot_harv
		foreach 			x in sold_harv cons prod anim seed {
			gen 				`x'_val = `x'_kg * med_pr_per_kg
			replace 			`x'_val = 0 if `x'_val >= .
		}
		egen 				crop_inc_amnt_s`z' = rowtotal(*val)
		
		collapse			(sum) crop_inc_amnt_s`z', by(hhid)
		
		tempfile 			s`z'
		save 				`s`z'' 
	}

* merge 2 seasons together 
	use 					`sa', clear
	merge 					1:1 hhid using `sb', nogen
	
	replace 				crop_inc_amnt_sa = 0 if crop_inc_amnt_sa >= .
	replace 				crop_inc_amnt_sb = 0 if crop_inc_amnt_sb >= .
	
	egen 					crop_inc_amnt_0 = rowtotal(crop_inc*)
	gen 					crop_inc_0 = cond(crop_inc_amnt_0 > 0, 1,0)
	
	drop 					*sa *sb 
	
 * save tempfile 
	tempfile 		temp3
	save 			`temp3'
	
		
* ***********************************************************************
**# livestock sales
* ***********************************************************************	
	
* load & format data large animals
	use 			"$root/wave_0`w'/Agriculture/AGSEC6A", clear
	
	gen 			live_inc_amnt_0 = s6aq14a * s6aq14b 
	replace 		live_inc_amnt_0 = 0 if live_inc_amnt_0  == .
	gen 			live_inc_0 = cond(live_inc_amnt_0 > 0, 1,0)
	collapse 		(sum) live_inc_amnt_0 (max) live_inc_0, by(hhid)

* load & format data small animals
preserve
	use 			"$root/wave_0`w'/Agriculture/AGSEC6B", clear
	replace 		s6bq14a = s6bq14a * 2 //asks about 6 months, mult by 2 for 1 yr
	gen 			slive_amnt_0 = s6bq14a * s6bq14b 
	replace 		slive_amnt_0 = 0 if slive_amnt_0  == .
	gen 			slive_0 = cond(slive_amnt_0 > 0, 1,0)
	collapse 		(sum) slive_amnt_0 (max) slive_0, by(hhid)
	tempfile 		small
	save 			`small'
restore 

* load & format data poultry
preserve
	use 			"$root/wave_0`w'/Agriculture/AGSEC6C", clear
	replace 		s6cq14a = s6cq14a * 4 //asks about 3 months, mult by 4 for 1 yr
	gen 			plive_amnt_0 = s6cq14a * s6cq14b 
	replace 		plive_amnt_0 = 0 if plive_amnt_0  == .
	gen 			plive_0 = cond(plive_amnt_0 > 0, 1,0)
	collapse 		(sum) plive_amnt_0 (max) plive_0, by(hhid)
	tempfile 		poultry
	save 			`poultry'
restore

* combine 
	merge 			1:1 hhid using `small', nogen 
	merge 			1:1 hhid using `poultry', nogen
	replace 		live_inc_amnt_0 = 0 if live_inc_amnt_0  == .
	replace 		live_inc_0 = 0 if live_inc_0 == .

	foreach 		x in s p {
		replace			`x'live_0 = 0 if `x'live_0 == .
		replace			`x'live_amnt_0 = 0 if `x'live_amnt_0 == .
		replace 		live_inc_0 = 1 if `x'live_0 == 1
		replace 		live_inc_amnt_0 = live_inc_amnt_0 + `x'live_amnt_0
		drop 			`x'live*
	}
	
* save tempfile 
	tempfile 		temp4
	save 			`temp4'
	

* ***********************************************************************
**# livestock products
* ***********************************************************************	

* load & format meat data 
	use 			"$root/wave_0`w'/Agriculture/AGSEC8A", clear

	rename 			s8aq05 meat
	replace 		meat = meat * 2 if s8a_roster == 102 | s8a_roster == 106
	replace 		meat = meat * 4 if s8a_roster == 103 | s8a_roster == 107
	replace 		meat = 0 if meat == .
	collapse 		(sum) meat, by(hhid)
	
	tempfile 		meat
	save 			`meat'
	
* load & format milk data 
	use 			"$root/wave_0`w'/Agriculture/AGSEC8B", clear

	rename 			s8bq05 cons_ltrs_per_day
	rename 			s8bq05a sold_ltrs_per_day
	rename 			s8bq09 revs_per_yr
	gen 			cons_ltrs_per_yr = cons_ltrs_per_day * 365
	gen 			sold_ltrs_per_yr = sold_ltrs_per_day * 365
	gen 			pr_per_ltr = revs_per_yr / sold_ltrs_per_yr
	
	*merge with region data
		merge 		m:1 hhid using `region'
		drop 		if _m == 2
		drop 		_m 
		
	* calc median price per liter to use for consumption value
		egen				ea_grp = group(AGroup district county town ward ea)
		egen				ward_grp = group(AGroup district county town ward)
		egen				town_grp = group(AGroup district county town)
		egen				cnty_grp = group(AGroup district county)
		egen				dist_grp = group(AGroup district)
		egen				ctry_grp = group(AGroup)
	
		foreach 			l in ctry dist cnty town ward ea {
			bysort 				`l'_grp: egen `l'_grp_count = count(`l'_grp) ///
									if pr_per_ltr != .
			bysort 				`l'_grp: egen `l'_med = median(pr_per_ltr) ///
									if `l'_grp_count >= 10
		}
			
		gen 				med_pr_per_ltr = ctry_med
		replace 			med_pr_per_ltr = dist_med if dist_med != .
		replace 			med_pr_per_ltr = town_med if town_med != .
		replace 			med_pr_per_ltr = ward_med if ward_med != .
		replace 			med_pr_per_ltr = ea_med if ea_med != .
		
	* calc total value of milk
		gen 				cons_val = med_pr_per_ltr * cons_ltrs_per_yr
		replace 			cons_val = 0 if cons_val >= .
		replace 			revs_per_yr = 0 if revs_per_yr >= .
		gen 				milk = cons_val + revs_per_yr
		collapse			(sum) milk, by(hhid)

	tempfile 		milk
	save 			`milk'
	
* load & format egg data 
	use 			"$root/wave_0`w'/Agriculture/AGSEC8c", clear
		
	rename 			s8cq05 eggs 
	replace 		eggs = eggs * 4
	replace 		eggs = 0 if eggs == .
	collapse 		(sum) eggs, by (hhid)

	tempfile 		eggs
	save 			`eggs'
	
* load & format animal power & dung data 
	use 			"$root/wave_0`w'/Agriculture/AGSEC11", clear
	
	rename 			s11q01c dung
	replace 		dung = 0 if dung == .
	rename 			s11q05a power
	replace 		power = 0 if power == .
	collapse 		(sum) dung power, by(hhid)
	
	tempfile 		dungpower
	save 			`dungpower'
	
* combine datasets	
	use 			`meat', clear
	merge 			1:1 hhid using `milk', nogen
	merge 			1:1 hhid using `eggs', nogen
	merge 			1:1 hhid using `dungpower', nogen
	
	gen 			live_prod_0 = 0	
	foreach 		var in meat milk eggs dung power {
		replace 		`var' = 0 if `var' == .
		replace 		live_prod_0 = 1 if `var' > 0
	}
	gen 			live_prod_amnt_0 = milk + eggs + meat + dung + power
	drop 			meat milk eggs dung power
	drop 			if hhid == ""
	
* save tempfile 
	tempfile 		temp5
	save 			`temp5'
	
		
* ***********************************************************************
**# business income
* ***********************************************************************	

* load & format data
	use 			"$root/wave_0`w'/Household/GSEC12_2", clear
	
	replace 		N09 = 12 if N09 == 1200000
	gen 			nfe_inc_amnt_0 = N09 * N10
	replace 		nfe_inc_amnt_0 = 0 if nfe_inc_amnt_0 >= .
	collapse 		(sum) nfe_inc_amnt_0, by(hhid)
	gen 			nfe_inc_0 = cond(nfe_inc_amnt_0 > 0, 1,0)

* save tempfile 
	tempfile 		temp6
	save 			`temp6'
	

* ***********************************************************************
**# SAGE assistance
* ***********************************************************************	

* load & format data
	use 			"$root/wave_0`w'/Household/GSEC2C", clear

	gen 			sage_amnt_0 = s2cq4 * 12 if s2cq6 == 1
	replace 		sage_amnt_0 = s2cq4 * 24 if s2cq6 == 2
	replace 		sage_amnt_0 = s2cq4 * 4 if s2cq6 == 3
	replace 		sage_amnt_0 = s2cq4 if s2cq6 == 4
	
	replace 		sage_amnt_0 = 0 if sage_amnt_0 == .
	gen 			sage_0 = cond(sage_amnt_0 > 0 ,1,0)
	
	collapse 		(sum) sage_amnt_0 (max) sage_0, by(hhid)
	keep 			hhid sage* 
	
* save tempfile 
	tempfile 		temp7
	save 			`temp7'
	

* ***********************************************************************
* merge  
* ***********************************************************************	
	
* combine dataset 
	use 				`temp0', clear
	forval 				x = 1/7 {
		merge 				1:1 hhid using `temp`x'', nogen
	}
	
* merge with geographic data
	merge 				1:1 hhid using `region'
	drop 				if district == .
	drop 				if _m == 2
	drop 				_m 
	
* replace missing values with 0s	
	quietly: ds, 		has(type numeric) 
	foreach 			var in `r(varlist)' {
		replace 			`var' = 0 if `var' >= .
	}
	replace 			sexhh = . if sexhh == 0
	
************************************************************************
**# combine similar variables with few observations, format, label
************************************************************************	

*rent 
	replace 			rent_inc_0 = 1 if rent_nonag_0 == 1
	replace 			rent_inc_amnt_0 = rent_inc_amnt_0 + rent_nonag_amnt_0
	drop 				rent_nonag* 
	
* interest
	replace 			interest_0 = 1 if oth_acc_0 == 1 | dividends_0 == 1
	replace 			interest_amnt_0 = interest_amnt_0 + oth_acc_amnt_0 ///
							+ dividends_amnt_0
	drop 				oth_acc* dividends*
	
* other
	replace 			oth_inc_0 = 1 if asset_0 == 1
	replace 			oth_inc_amnt_0 = oth_inc_amnt_0 + asset_amnt_0
	drop 				asset*

* label variables
	lab var 		rent_inc_0 "Rental Income"
	lab var 		interest_0 "Interest and Investments"	
	lab var 		pen_inc_0 "Pension"
	lab var 		cash_dom_0 "Domestic Remittances"
	lab var 		cash_for_0 "Foreign Remittances"
	lab var 		kind_dom_0 "Domestic In-Kind Transfers"
	lab var 		kind_for_0 "Foreign In-Kind Transfers"
	lab var			oth_inc_0 "Other"
	lab var 		wage_emp_0 "Wages"
	lab var 		crop_inc_0 "Crop Income"
	lab var 		nfe_inc_0 "Non-Farm Enterprises"
	lab var 		live_inc_0 "Livestock Sales"
	lab var 		live_prod_0 "Livestock Product Income"
	lab var 		sage_0 "SAGE assistance"
	
	
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
		replace 			`var' = `var'_`count'_ if `var' == .
		drop 				`var'_*
	}
		
		
************************************************************************
**# generate country-specific index
************************************************************************

* count/fraction index	
	preserve 
		drop 			*amnt_0 
		keep 			hhid *_0 district county town ward ea
		
		* count number of possible income sources
		ds 				*_0
		gen 			tot_count = `c(k)' -6 //number of columns minus 6 (hhid district county town ward ea)

		* generate count of income sources and total number of observations by each geographic level
		gen 			one = 1
		foreach 		l in district county town ward ea {
			foreach v 		of varlist *_0 {
				egen 			`v'_count_`l' = max(`v'), by(`l')
			}
			egen			`l'_count = rowtotal(*_count_`l')
			egen 			`l'_obs = total(one), by(`l')
		}
	
		* generate combined variable that uses the smallest geographic area with at least 10 observations
		gen 	 		geo_count = district_count if district_obs >= 10
		replace 		geo_count = county_count if county_obs >= 10
		replace 		geo_count = town_count if town_obs >= 10
		replace 		geo_count = ward_count if ward_obs >= 10
		replace 		geo_count = ea_count if ea_obs >= 10
		
		* count number of hh income sources
		egen 			hh_count = rowtotal(*_0)
		
		* generate index
		gen 			uga_pre_index_all = 1- (hh_count / tot_count)
		lab var 		uga_pre_index_all "1- LD index with all 14 income sources in UGA as denominator"
		gen				uga_pre_index_geo = 1- (hh_count / geo_count)
		lab var 		uga_pre_index_geo "1- LD index with denominator as number of income sources at lowest geo level with 10 obs"
		keep 			hhid uga_pre_index* 
		
		* tempfile 
		tempfile 		ind
		save 			`ind'
		
	restore
	
	merge 				1:1 hhid using `ind', assert(3) nogen

	gen 				uga_pre_index_all_phhm = uga_pre_index_all / hhsize
	lab var 			uga_pre_index_all_phhm "uga_pre_index_all divided by hh size"
	gen 				uga_pre_index_geo_phhm = uga_pre_index_geo / hhsize
	lab var 			uga_pre_index_geo_phhm "uga_pre_index_geo divided by hh size"
	
* weighted by amount (Herfindahl-Hirschman Index)
	egen 				tot_inc = rowtotal(*amnt_0)
	ds 					*_amnt_0 
	foreach 			var in `r(varlist)' {
		gen 			`var'_persq = ((`var' / tot_inc) * 100)^2
	}
	
	egen 				uga_pre_index_hhi = rowtotal(*_amnt_0_persq)
	drop 				*_persq 
		
	lab var 			uga_pre_index_hhi "Herfindahl-Hirschman index of LD"

	
************************************************************************
**# add educational engagement variable
************************************************************************

preserve 

* load data
	use 				"$root/wave_0`w'/Household/GSEC4", clear

* merge with roster data to get age 
	merge 				1:1 hhid PID using "$root/wave_0`w'/Household/GSEC2"

* keep children age 5-18 and format variable
	rename 				h2q8 age		
	keep 				if age >= 3 & age <= 18
	
* format variable
	rename 				s4q05 edu_act
	replace 			edu_act = 0 if edu_act < 3
	replace 			edu_act = 1 if edu_act == 3
	
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
	
* add country & wave 
	gen 				wave = 0
	gen 				country = 4
	rename 				hhid baseline_hhid 
	order 				country wave baseline_hhid 
	
* save round file
	save				"$export/wave_0`w'/r`w'", replace
	
	log close

/* END */			