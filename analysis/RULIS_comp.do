* Project: diversification
* Created on: Jan 2022
* Created by: amf
* Edited by: jdm, amf, alj
* Last edited: August 2022
* Stata v.17.0 / 16.1

* does
	* RULIS comparisons 

* assumes
	
* TO DO:

* **********************************************************************
**# format RULIS data for merge
* **********************************************************************

* import & format RULIS 
	use 			"G:\My Drive\wb_covid\data\RuLIS\Ethiopia\Datasets\hh_final.dta", clear
	rename 			hhid hhid_eth 
	tempfile 		temp_eth 
	save 			`temp_eth'
	/*
	use 			"G:\My Drive\wb_covid\data\RuLIS\Malawi\Datasets\hh_final.dta", clear
	rename 			hhid hhid_mwi
	tempfile 		temp_mwi
	save 			`temp_mwi'
	*/
	use 			"G:\My Drive\wb_covid\data\RuLIS\Nigeria\Datasets\hh_final.dta", clear
	destring		hhid, replace
	rename 			hhid hhid_nga
	tempfile 		temp_nga
	save 			`temp_nga'
	
	use 			"G:\My Drive\wb_covid\data\RuLIS\Uganda\Uganda 2019-20\Datasets\hh_final.dta", clear
	rename 			hhid baseline_hhid
	tempfile 		temp_uga
	save 			`temp_uga'
	
	clear 
	use 			`temp_eth', clear
	//append 			using `temp_mwi'
	append 			using `temp_nga'
	append 			using `temp_uga'
	tempfile 		rulis
	save 			`rulis'
	
	
* **********************************************************************
**# merge RULIS with panel 
* **********************************************************************

* load panel data
	use 					"$data/analysis/diversification/ld_pnl", clear
	keep 					hhid* *_0 wave_orig country baseline_hhid
	keep 					if wave_orig == 0 & country != 2
	merge 					1:1 hhid_eth hhid_nga baseline_hhid using `rulis'
	
	keep 					if _m == 3 // our data drops if not in COVID rounds, keep only matches
	drop 					_m
	
	* convert to USD
	foreach 			var in nonagr_wge agr_wge crop cropown cropsold cropgift cropseed cropfeed  ///
								croppay livstvp selfemp int_rem priv_trans soc_ass soc_ins {
		replace 			`var' = `var' * 0.0343 if country == 1
		replace 			`var' = `var' * 0.0014 if country == 2
		replace 			`var' = `var' * 0.0028 if country == 3
		replace 			`var' = `var' *  0.0003 if country == 4
	}
	
	
* **********************************************************************
**# compare income categories
* **********************************************************************
	set 			scheme s1color
	pause 			on

* wages 
	gen 			wage_ua = wage_emp_amnt_0
	replace 		wage_ua = wage_ua + casual_emp_amnt_0 if casual_emp_amnt_0 != .
	replace 		wage_ua = wage_ua + temp_emp_amnt_0 if temp_emp_amnt_0 != .
	replace 		nonagr_wge = 0 if nonagr_wge == .
	replace 		agr_wge = 0 if agr_wge == .
	gen 			wage_rulis = nonagr_wge + agr_wge
	sepscatter 		wage_ua wage_rulis, separate(country) legend(pos(1) col(3))
	sepscatter 		wage_ua wage_rulis if wage_ua < 20000, separate(country) legend(pos(1) col(3)) 
	
* crop 
	gen 			crop_ua = crop_inc_amnt_0 
	foreach 		var in cropown cropsold cropgift cropseed cropfeed croppay {
		replace 		`var' = 0 if `var' == .
	}
	gen 			crop_rulis = cropown + cropsold + cropgift + cropseed + cropfeed + croppay
	sepscatter 		crop_ua crop_rulis, separate(country) legend(pos(1) col(3)) 
	sepscatter 		crop_ua crop_rulis if crop_rulis <5000, separate(country) legend(pos(1) col(3)) 
	
* livestock 
	gen 			live_ua = live_prod_amnt_0 + live_inc_amnt_0
	sepscatter 		live_ua livstvp, separate(country) legend(pos(1) col(3))
	pause 
	sepscatter 		live_ua livstvp if live_ua < 6000 & livstvp < 6000, separate(country) legend(pos(1) col(3))
	
* nfe
	gen 			nfe_ua = nfe_inc_amnt_0 
	replace 		selfemp = 0 if selfemp == .
	sepscatter 		nfe_ua selfemp, separate(country) legend(pos(1) col(3))
	
* transfers
	foreach 		var in cash_trans_amnt_0 food_trans_amnt_0 kind_trans_amnt_0 ///
						kind_child_amnt_0 cash_child_amnt_0 cash_dom_amnt_0  ///
						kind_dom_amnt_0 cash_for_amnt_0 kind_for_amnt_0 {
		replace 			`var' = 0 if `var' == .
	}
	gen 			trans_ua = cash_trans_amnt_0 + food_trans_amnt_0 + kind_trans_amnt_0 + ///
						kind_child_amnt_0 + cash_child_amnt_0 + cash_dom_amnt_0 + ///
						kind_dom_amnt_0 + cash_for_amnt_0 + kind_for_amnt_0
	replace 		int_rem  = 0 if int_rem == .
	replace 		priv_trans = 0 if priv_trans == .
	gen 			trans_rulis = int_rem + priv_trans		
	sepscatter 		trans_ua trans_rulis, separate(country) legend(pos(1) col(3))
	
	
* assistance 
	foreach 		var in asst_cash_amnt_0 asst_food_amnt_0 asst_kind_amnt_0 ///
						asst_inc_amnt_0 sage_amnt_0 {
		replace 		`var' = 0 if `var' == .
	}
	gen 			asst_ua = asst_cash_amnt_0 + asst_food_amnt_0 + asst_kind_amnt_0 + ///
						asst_inc_amnt_0 + sage_0
	sepscatter 		asst_ua soc_ass, separate(country) legend(pos(1) col(3))
	
* pension
	sepscatter 		pen_inc_amnt_0 soc_ins, separate(country) legend(pos(1) col(3))
	
	
* **********************************************************************
**# outliers to look into 
* **********************************************************************	
	gen dif = crop_ua - crop_rulis if country == 1 & crop_rulis < 5000
	sum dif,det
	twoway scatter dif crop_ua	
	
* **********************************************************************	
* **********************************************************************	