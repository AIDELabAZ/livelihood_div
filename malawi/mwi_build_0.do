* Project: WB COVID
* Created on: June 2021
* Created by: amf
* Edited by: jdm, amf
* Last edited: Nov 2020
* Stata v.16.1

* does
	* reads in baseline Malawi data
	* builds data for LD 
	* outputs HH income dataset

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
	global  fies 	= 	"$data/analysis/raw/Malawi"

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
**# transfers form children
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
	drop 			inc_105 inc_108 inc_1091 amnt_105 amnt_106 amnt_107 amnt_108 amnt_1091

* rename variables 
	rename 			inc_101 asst_maize_0
	rename 			amnt_101 kg_maize_0
	rename 			inc_102 asst_food_0
	rename 			amnt_102 asst_food_amnt_0
	rename 			inc_104 input_for_wrk_0
	rename 			amnt_104 input_for_wrk_amnt_0
	rename 			inc_106 tnp_0 
	rename 			inc_107 supp_feed_0
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

* save tempfile 
	tempfile 		temp3
	save 			`temp3'	
	
	
* ***********************************************************************
**# labor & time use  
* ***********************************************************************	
	
* load data
	use 			"$root/wave_0`w'/hh_mod_e_19", clear

* rename indicator vars	
	rename 			hh_e06_4 wage_emp_0
	rename 			hh_e06_6 casual_emp_0	
	foreach 		var in wage casual {
		replace 		`var'_emp_0 = 0 if `var'_emp_0 == 2	
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
		gen 			main_pay_per_month = (main_pay / main_pay_period) if main_pay_unit == 5
		replace 		main_pay_per_month = (main_pay / main_pay_period) * days_per_month if main_pay_unit == 3
		replace 		main_pay_per_month = (main_pay / main_pay_period) * weeks_per_month if main_pay_unit == 4
		* in-kind payments
		gen 			main_pay_kind_per_month = (main_pay_kind / main_pay_kind_period) if main_pay_kind_unit == 5
		replace 		main_pay_kind_per_month = (main_pay_kind / main_pay_kind_period) * days_per_month if main_pay_kind_unit == 3
		replace 		main_pay_kind_per_month = (main_pay_kind / main_pay_kind_period) * weeks_per_month if main_pay_kind_unit == 4		
		*combine salary and in-kind
		replace 		main_pay_per_month = main_pay_per_month + main_pay_kind_per_month if main_pay_kind_per_month != .
		
	* calc annual mian income (subtract $ paid for apprenticeships)
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
		gen 			sec_pay_kind_per_month = (sec_pay_kind / sec_pay_kind_period) if sec_pay_kind_unit == 5
		*combine salary and in-kind
		replace 		sec_pay_per_month = sec_pay_per_month + sec_pay_kind_per_month if sec_pay_kind_per_month != .
		
	* calc annual income (subtract $ paid for apprenticeships)
		gen				sec_pay_annual = (sec_pay_per_month * sec_months) 
		replace 		sec_pay_annual = sec_pay_annual - sec_cost if sec_cost !=.

	* combine main and secondary job incomes
		gen 			wage_emp_amnt_0 = main_pay_annual + sec_pay_annual if sec_pay_annual != .
		replace 		wage_emp_amnt_0 = main_pay_annual if wage_emp_amnt_0 == .

	* NOTE: significant outliers that should probably be dropped 

* calc income from casual labor
	* rename variables 
		rename 			hh_e56 cas_months_per_year
		rename 			hh_e57 cas_wks_per_month
		rename 			hh_e58 cas_days_per_wk
		rename 			hh_e59 cas_pay_per_day
		
	* calc annual casual salary
		gen 			casual_emp_amnt_0 = cas_pay_per_day * cas_days_per_wk * ///
							cas_wks_per_month * cas_months_per_year

* drop irrelevant 	
	keep 				y4 *emp*

* collapse to hh level (note that this makes missing values 0)
	collapse 			(sum) *amnt* (max) wage_emp_0 casual_emp_0, by (y4)

* save tempfile 
	tempfile 		temp4
	save 			`temp4'	


* ***********************************************************************
**# crop & tree income
* ***********************************************************************	

* load & format rainy data
	use 			"$root/wave_0`w'/ag_mod_i_19", clear

	rename 			ag_i01 rainy
	replace 		rainy = 0 if rainy == 2
	collapse 		(sum) ag_i03 (max) rainy, by(y4)
	rename 			ag_i03 rainy_sales

preserve 

* load & format dimba data
	use 			"$root/wave_0`w'/ag_mod_o_19", clear

	rename 			ag_o01 dimba
	replace 		dimba = 0 if dimba == 2
	collapse 		(sum) ag_o03 (max) dimba, by(y4)
	rename 			ag_o03 dimba_sales	
	
	* save tempfile 
		tempfile 		tempd
		save 			`tempd'	
	
* combine rainy & dimba crop sales 
restore
	
	merge 			1:1 y4 using `tempd', nogen
	
	gen 			crop_inc_amnt_0 = rainy_sales
	replace 		crop_inc_amnt_0 = rainy_sales + dimba_sales if dimba_sales != .
	gen 			crop_inc_0 = 0 
	replace 		crop_inc_0 = 1 if rainy == 1 | dimba == 1
	keep 			y4 crop_* 

	* save tempfile 
		tempfile 		tempc
		save 			`tempc'	
		
* load & format tree data
	use 			"$root/wave_0`w'/ag_mod_q_19", clear	

	rename 			ag_q01 tree_inc_0
	replace 		tree_inc_0 = 0 if tree_inc_0 == 2
	rename 			ag_q03 tree_inc_amnt_0
	collapse 		(sum) tree_inc_amnt_0 (max) tree_inc_0, by(y4)

* merge crop & tree 	
	merge 			1:1 y4 using `tempc', nogen
	
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
	
	rename 			ag_s04 live_prod_0
	replace 		live_prod_0 = 0 if live_prod_0 == 2
	rename 			ag_s06 live_prod_amnt_0
	collapse 		(sum) live_prod_amnt_0 (max) live_prod_0, by(y4)
	replace 		live_prod_0 = 0 if live_prod_0 == .

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
**# fishery income
* ***********************************************************************	

* load & format fishery data - high season fishers

	use 			"$root/wave_0`w'/fs_mod_e1_19", clear	

	rename 			fs_e08a fish_high1
	rename 			fs_e08f fish_high_price1
	rename 			fs_e08g fish_high2
	rename 			fs_e08l fish_high_price2
	
	replace 		fish_high2 = 0 if fish_high2 >= .
	replace 		fish_high_price2 = 0 if fish_high_price2 >= .
	
	rename 			fs_e09 fish_high_weeks
	gen 			fish_inc_high = ((fish_high1 * fish_high_price1) + (fish_high2 * fish_high_price2)) * fish_high_weeks
	
	collapse 		(sum) fish_inc_high, by(y4)
	
	* save tempfile 
		tempfile 		tempf1
		save 			`tempf1'	
	
* NOTE: many instances where provide amnt and price but then say they fished for 0 weeks, which zeros out the whole line
* 		large outliers

* load & format fishery data - high season fish traders
	* get weeks in rish trading 
		* NOTE: question asked by HH member, take max per HH
	use 			"$root/wave_0`w'/fs_mod_c_19", clear	

	rename 			fs_c04a trade_high_weeks	
	keep 			y4 PID trade_high_weeks
	
	collapse 		(max) trade_high_weeks, by(y4)
	
	tempfile 		tempwk1
	save 			`tempwk1'

	* fish trade high data 
		* NOTE: by fish species, collapsed to HH level
	use 			"$root/wave_0`w'/fs_mod_f1_19", clear	

	rename 			fs_f03a trade_high1
	rename 			fs_f03f trade_high_price1
	rename 			fs_f03g trade_high2
	rename 			fs_f03m trade_high_price2
	
	replace 		trade_high2 = 0 if trade_high2 >= .
	replace 		trade_high_price2 = 0 if trade_high_price2 >= .

	gen 			trade_inc_high = ((trade_high1 * trade_high_price1) + (trade_high2 * trade_high_price2)) 
	replace 		trade_inc_high = 0 if trade_inc_high  >= .
	
	collapse 		(sum) trade_inc_high, by(y4)
	
	merge 			1:1 y4 using `tempwk1'
	replace 		trade_inc_high = trade_inc_high * trade_high_weeks
	
	keep 			y4 trade_inc_high
	
	* save tempfile 
		tempfile 		tempt1
		save 			`tempt1'
	
* load & format fishery data - low season fishers
	use 			"$root/wave_0`w'/fs_mod_i1_19", clear	

	rename 			fs_i08a fish_low1
	rename 			fs_i08f fish_low_price1
	rename 			fs_i08g fish_low2
	rename 			fs_i08l fish_low_price2
	
	replace 		fish_low2 = 0 if fish_low2 >= .
	replace 		fish_low_price2 = 0 if fish_low_price2 >= .
	
	rename 			fs_i09 fish_low_weeks
	gen 			fish_inc_low = ((fish_low1 * fish_low_price1) + (fish_low2 * fish_low_price2)) * fish_low_weeks
	
	collapse 		(sum) fish_inc_low, by(y4)

	* save tempfile 
		tempfile 		tempf2
		save 			`tempf2'
		
* load & format fishery data - high season fish traders
	* get weeks in rish trading 
		* NOTE: question asked by HH member, take max per HH
	use 			"$root/wave_0`w'/fs_mod_g_19", clear	
	
	rename 			fs_g04a trade_low_weeks	
	keep 			y4 PID trade_low_weeks
	
	collapse 		(max) trade_low_weeks, by(y4)
	
	tempfile 		tempwk2
	save 			`tempwk2'

	* fish trade high data 
		* NOTE: by fish species, collapsed to HH level
	use 			"$root/wave_0`w'/fs_mod_j1_19", clear	

	rename 			fs_j03a trade_low1
	rename 			fs_j03f trade_low_price1
	rename 			fs_j03g trade_low2
	rename 			fs_j03l trade_low_price2
	
	replace 		trade_low2 = 0 if trade_low2 >= .
	replace 		trade_low_price2 = 0 if trade_low_price2 >= .

	gen 			trade_inc_low = ((trade_low1 * trade_low_price1) + (trade_low2 * trade_low_price2)) 
	replace 		trade_inc_low = 0 if trade_inc_low  >= .
	
	collapse 		(sum) trade_inc_low, by(y4)
	
	merge 			1:1 y4 using `tempwk2'
	replace 		trade_inc_low = trade_inc_low * trade_low_weeks
	
	keep 			y4 trade_inc_low
	
	* save tempfile 
		tempfile 		tempt2
		save 			`tempt2'

* merge all files together
	use 			`tempf1', clear
	merge 			1:1 y4 using `tempf2', nogen
	merge 			1:1 y4 using `tempt1', nogen
	merge 			1:1 y4 using `tempt2', nogen

	egen 			fish_inc_amnt_0 = rowtotal(fish* trade*)
	gen 			fish_inc_0 = cond(fish_inc_amnt_0 > 0 & fish_inc_amnt_0 <., 1,0)
	keep 			y4 fish_inc_0 fish_inc_amnt_0
	
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
	
* label indicator variables
	lab	def			yesno 0 "No" 1 "Yes", replace
	foreach 		var in cash_child_0 kind_child_0 asst_maize_0 asst_food_0 input_for_wrk_0 ///
						tnp_0 supp_feed_0 cash_gov_0 cash_ngo_0 oth_inc2_0 masaf_0 cash_for_wrk_0 ///
						wage_emp_0 casual_emp_0 tree_inc_0 crop_inc_0 live_inc_0 live_prod_0 ///
						nfe_inc_0 fish_inc_0 {
	lab val 			`var' yesno
	}
	
* add country & wave variables 
	gen 			wave = 0
	gen 			country = 2
	rename 			y4_hhid hhid_mwi
	order 			country wave hhid 
	
* save round file
	save			"$export/wave_0`w'/r`w'", replace

/* END */		