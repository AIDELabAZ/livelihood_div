* Project: diversification
* Created on: Feb 2024
* Created by: jdm
* Edited by: jdm
* Last edited: Feb 27 2024
* Stata v.18.0

* does
	* address reviewer comments for JAAEA

* assumes
	* clean fies data
	* clean diversification indices data
	* coefplot

* TO DO:
	* inc_sum table: issues with multicolumn footer - eliminated for now 


***********************************************************************
**# 0. setup
***********************************************************************

* define
	global	export	=		"$data/analysis/diversification"
	global	logout	=		"$data/analysis/logs"
	global  fies 	= 		"$data/analysis/food_security"

* open log
	cap log 				close
	log using				"$logout/revisions", append

* local countries
	local 					countries = "1 2 3 4"	

* load panel data
	use 					"$data/analysis/diversification/ld_pnl", replace	
	
* clear memory 
	graph 					drop _all
	eststo 					clear

***********************************************************************
**# 1. tables
***********************************************************************	

* replace total loss with reduced
	replace					tot_inc_chg = -1 if tot_inc_chg == 3 | tot_inc_chg == 4
	replace					tot_inc_chg = 0 if tot_inc_chg == 2
	lab 					define tot_inc_chg 1 "Increased" 0 "Stayed the same" ///
								-1 "Decreased"
	lab 					values tot_inc_chg tot_inc_chg


* local countries
	local 					countries = "1 2 3 4"	
	
	foreach 				ind in std_pp_index {
	if 						"`ind'" == "std_pp_index" {
		local 				t = "Fractional Index"
	}

	foreach 				c in 1 2 {
		foreach 				fs in mild mod sev std {
			* balance panel with lagged variables
			preserve
			keep 					if country == `c'
			drop 					if `fs'_fs == . // can never use obs without dependent var
			egen 					temp = total(inrange(wave_orig, 0, 11)), by(hhid)
			drop 					if temp < 6 & country == 1 // 3,317 of 17,759 dropped (19%)
			drop 					if temp < 10 & country == 2 // 2,812 of 16,102 dropped (17%)
			drop 					if temp < 4 & country == 3 // 1,050 of 7,606 dropped (14%)
			sort 					hhid wave_, stable 
			bysort 					hhid (wave_orig): gen `ind'_lag = `ind'[_n-1]
			bysort 					hhid (wave_orig): gen `fs'_fs_lag = `fs'_fs[_n-1]
			egen 					wave_temp =  group(country wave_orig)
			egen 					max = max(wave_temp), by(hhid)
			drop 					if max == 4 & country == 1 // drops 8
			* dynamic panel regression
			xtset 					hhid wave_temp 
			xtreg 					`fs'_fs c.`fs'_fs_lag##c.`ind'_lag tot_inc_chg i.wave_temp i.region#c.wave_temp ///
										[aweight = weight], fe vce(cluster hhid)
			eststo					`ind'_`fs'_dyn_`c'
			restore
		}
	}

	* generate graphics 
	coefplot				`ind'_mild_dyn_1 `ind'_mod_dyn_1 `ind'_sev_dyn_1 `ind'_std_dyn_1, ///
								drop(*.wave_temp *.country std_pp_index_lag mild_fs_lag mod_fs_lag ///
								sev_fs_lag std_fs_lag _cons tot_inc_chg) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Ethiopia") ///
								levels(95) coeflabels(c.* = " ", notick) xlabel(-1(.2)1, labs(small)) ///
								legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index")) name(inc_fs_dyn_eth, replace)
								
	coefplot				`ind'_mild_dyn_2 `ind'_mod_dyn_2 `ind'_sev_dyn_2 `ind'_std_dyn_2, ///
								drop(*.wave_temp *.country std_pp_index_lag mild_fs_lag mod_fs_lag ///
								sev_fs_lag std_fs_lag _cons tot_inc_chg) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Malawi") ///
								levels(95) coeflabels(c.* = " ", notick) xlabel(-1(.2)1, labs(small)) ///
								legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index")) name(inc_fs_dyn_mwi, replace)
	}
