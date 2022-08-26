* Project: diversification
* Created on: Jan 2022
* Created by: amf
* Edited by: jdm, amf, alj
* Last edited: August 2022
* Stata v.17.0 / 16.1

* does
	* RULIS regs 

* assumes

* TO DO:

* **********************************************************************
**# setup 
* **********************************************************************
	
	global	export	=		"$data/analysis/diversification"

* import, format & merge RULIS 
	use 			"G:\My Drive\wb_covid\data\RuLIS\Ethiopia\Datasets\hh_final.dta", clear
	rename 			hhid hhid_eth 
	tempfile 		temp_eth 
	save 			`temp_eth'

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
	append 			using `temp_nga'
	append 			using `temp_uga'

* add country & wave variables 
	gen 				country = 1 if hhid_eth != ""
	replace				country = 3 if hhid_nga != .
	replace 			country = 4 if baseline_hhid != ""
	gen 				wave_orig = 0 
	
* format variables for consistency 
	foreach var 		in cropsold cropown cropfeed cropstore cropbyprod ///
							cropgift livstvp nonagr_wge agr_wge soc_ins ///
							int_rem priv_trans selfemp soc_ass otherinc_tot {
		replace 			`var' = 0 if `var' == .
		replace 			`var' = 0 if `var' < 0 // only selfemp
		* convert to USD 
		replace 			`var' = `var' * 0.0343 if country == 1
		replace 			`var' = `var' * 0.0014 if country == 2
		replace 			`var' = `var' * 0.0028 if country == 3
		replace 			`var' = `var' * 0.0003 if country == 4
	}

* **********************************************************************
**# generate income variables 
* **********************************************************************
	
* farm 
	gen 				farm_rulis = cropsold + cropown + cropfeed + cropstore + ///
							cropbyprod + cropgift + livstvp

* wage
	gen 				wage_rulis = nonagr_wge + agr_wge
	
* pension
	gen 				pen_rulis = soc_ins 
	
* remittances
	gen 				rem_rulis = int_rem + priv_trans 
	
* nfe
	gen 				nfe_rulis = selfemp

* assistance
	gen 				asst_rulis = soc_ass

* other (including savings & rental)
	gen 				oth_rulis = otherinc_tot 

	
* **********************************************************************
**# calculate HHI
* **********************************************************************

	egen 				rulis_total_inc = rowtotal(*_rulis)
	ds 					*_rulis
	foreach 			var in `r(varlist)' {
		gen 				`var'_persq = (`var' / rulis_total_inc)^2
	}
	
	egen 				hhi_rulis = rowtotal(*_rulis_persq)
	drop 				*rulis_persq 

	
* **********************************************************************
**# save RULIS 
* **********************************************************************
	
	keep 				hhid* baseline_hhid hhi_rulis wave *rulis livstvp ///
							rulis* 
	tempfile 			rulis 
	save 				`rulis'
	
	
* **********************************************************************
**# combine RULIS with panel data 
* **********************************************************************

* load data
	use 				"$data/analysis/diversification/ld_pnl", replace
	drop 				if country == 2
	merge 				1:1 hhid_eth hhid_nga baseline_hhid wave_orig using `rulis'	
	drop 				if _m == 2 // drop households not in post waves 
				
* prep panel 
	sort 					hhid wave_orig
	xtset 					hhid wave_orig
	
	* gen y0 fs and education and xfill by hhid
		foreach 			f in mild mod sev std {
			gen 				y0_`f' = `f'_fs if wave == 0
			xfill 				y0_`f', i(hhid)
		}
		gen 				y0_edu_act = edu_act if wave == 0
		xfill 				y0_edu_act, i(hhid)
		
	* xfill diversification by hhid
		xfill 				hhi_rulis, i(hhid)
		
	* generate and xfill outcome vars for each wave 
		forval 					x = 0/11 {
			foreach 				fs in mild mod sev {
				gen 					y`x'_fs_`fs' = `fs'_fs if wave_orig == `x'
				xfill 					y`x'_fs_`fs', i(hhid)
			}
		}	
		
		
* **********************************************************************
**# scatter plot comparison
* **********************************************************************

	* farm 
		gen 					ua_farm_amnt_3 = crop_inc_amnt_0 + crop_inc_amnt_0 + tree_inc_amnt_0 + ///
									live_inc_amnt_0 + live_prod_amnt_0 if country == 3
		gen 					ua_live_amnt_3 = live_inc_amnt_0  + live_prod_amnt_0
		twoway scatter 			ua_farm_amnt_3 farm_rulis if country == 3
		
	* wage 
		twoway scatter 			wage_emp_amnt_0 wage_rulis if country == 3
		
	* pen 
		twoway scatter 			pen_inc_amnt_0 pen_rulis if country == 3
		
	* rem 
		gen 					ua_rem_amnt_3 =  cash_for_amnt_0 + kind_inc_amnt_0  + cash_dom_amnt_0 if country == 3
		twoway scatter 			ua_rem_amnt_3 rem_rulis if country == 3
		// only use cash_for_amnt_0 in rulis??
		
	* nfe 
		twoway scatter 			nfe_inc_amnt_0 nfe_rulis if country == 3 
		
	* asst 
		twoway scatter 			asst_inc_amnt_0 asst_rulis if country == 3


* **********************************************************************
**# ANCOVA & DID regs
* **********************************************************************
	
* food security
	* regressions
	preserve 
	drop 						if wave == -1
	foreach 					c in 1 3 {
		if 						`c' == 1 {
			local 					country = "Ethiopia"
		}
		else 					if `c' == 3 {
			local 					country = "Nigeria"
		}
		foreach 					ind in hhi_rulis {	
			* ANCOVA	
			foreach 				f in mild mod sev std {
				reg 					`f'_fs `ind' y0_`f' ib(1).wave c.wave#i.region ///
											[aweight = weight] if wave != 0 & country == `c', vce(cluster hhid) 
				eststo					`f'_an_`ind'`c'
			}
			* DID
			foreach 				f in mild mod sev std {
				reg 					`f'_fs c.`ind'##i.post ib(1).wave c.wave#i.region ///
											[aweight = weight] if country == `c', vce(cluster hhid) 	
				eststo					`f'_dd_`ind'`c'
			}	
		}
		
		* graphics generation
		foreach 					ind in hhi_rulis {
			* coefplot
				coefplot			mild_an_`ind'`c' mod_an_`ind'`c' sev_an_`ind'`c' std_an_`ind'`c', ///
										drop(*.wave y0* _cons *post) xline(0, lcolor(maroon)) ///
										xtitle("Effect on Food Insecurity", size(small)) title("`country'") ///
										levels(95) coeflabels(`ind' = " ", notick) xlabel(-1(.2)1, labs(small)) ///
										legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
										label(6 "Severe") label(8 "Index")) name(anc_rulis_`c', replace)
				
				coefplot			mild_dd_`ind'`c' mod_dd_`ind'`c' sev_dd_`ind'`c' std_dd_`ind'`c', ///
										drop(*.wave y0* _cons *post `ind') ///
										xline(0, lcolor(maroon)) xlabel(-1(.2)1, labs(small))  ///
										xtitle("Effect on Food Insecurity") title("`country'") ///
										levels(95) coeflabels(1.post#c.`ind' = " ", notick) ///
										legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
										label(6 "Severe") label(8 "Index")) name(did_rulis_`c', replace)		
		} 
		
	}
	
	* combined graphics 
	foreach 						r in anc did {
		grc1leg2  						`r'_rulis_1 `r'_rulis_3, col(2) commonscheme title("RULIS") 
		graph export					"$export/figures/reg_results/`r'_rulis.png", as(png) replace
	}
	restore 

	
* education
	* DID & ANCOVA regressions	
	foreach 				c in 1 3 4 {
		if 						`c' == 1 {
			local 					country = "Ethiopia"
			local 					xt = " "
		}
		else 					if `c' == 3 {
			local 					country = "Nigeria"
			local 					xt = "Effect on Educational Engagement"
		}
		else 					if `c' == 4 {
			local 					country = "Uganda"
			local 					xt = "Effect on Educational Engagement"
		}
		
		* regressions 
		foreach 				ind in hhi_rulis {	
			* ANCOVA	
				reg 				edu_act `ind' y0_edu_act ib(1).wave c.wave#i.region ///
										[aweight = weight] if wave != 0 & country == `c', vce(cluster hhid) 
				eststo				edu_anc_`ind'`c'
			* DID
				reg 				edu_act c.`ind'##i.post ib(1).wave c.wave#i.region ///
										[aweight = weight] if country == `c', vce(cluster hhid) 	
				eststo				edu_did_`ind'`c'	
		}
	}	
	
	* plot coefficients by index for each country
		* DID
		coefplot			edu_did_hhi_rulis1 edu_did_hhi_rulis3 edu_did_hhi_rulis4, ///
								drop(*.wave _cons *post hhi_rulis*) xtitle("`xt'") ///
								xline(0, lcolor(maroon)) xlabel(-1(.2)1, labs(med)) ///
								levels(95) xlabel(-1(.2)1, labs(small)) title("DID") ///
								coeflabels(1.post#c.hhi_rulis = " ", notick) ///
								legend(off) name(edu_did_rulis, replace)
		* ANCOVA
		coefplot			edu_anc_hhi_rulis1 edu_anc_hhi_rulis3 edu_anc_hhi_rulis4, ///
								drop(*.wave _cons *post hhi_rulis*) xtitle("`xt'") ///
								xline(0, lcolor(maroon)) xlabel(-1(.2)1, labs(med)) ///
								levels(95) xlabel(-1(.2)1, labs(small)) title("ANCOVA") ///
								coeflabels(y0_edu_act = " ", notick) ///
								legend(off) name(edu_anc_rulis, replace)
	
	
	* graph export 
	gr combine 				edu_did_rulis edu_anc_rulis, commonscheme
	graph export			"$export/figures/reg_results/edu_rulis.png", as(png) replace

* **********************************************************************
* **********************************************************************