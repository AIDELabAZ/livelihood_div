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
**# 1. include income change
***********************************************************************	

* replace total loss with reduced
	replace					tot_inc_chg = 0 if tot_inc_chg == 2
	replace					tot_inc_chg = 2 if tot_inc_chg == 3 | tot_inc_chg == 4
	lab 					define tot_inc_chg 1 "Increased" 0 "Stayed the same" ///
								2 "Decreased"
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
			xtreg 					`fs'_fs c.`fs'_fs_lag##c.`ind'_lag i.tot_inc_chg i.wave_temp i.region#c.wave_temp ///
										geo_control [aweight = weight], fe vce(cluster hhid)
			eststo					`ind'_`fs'_dyn_`c'
			restore
		}
	}

	* generate graphics 
	coefplot				`ind'_mild_dyn_1 `ind'_mod_dyn_1 `ind'_sev_dyn_1 `ind'_std_dyn_1, ///
								drop(*.wave_temp *.country std_pp_index_lag mild_fs_lag mod_fs_lag ///
								sev_fs_lag std_fs_lag _cons) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Ethiopia") ///
								levels(95) coeflabels(c.* = " " tot* = " ", notick) xlabel(-1(.2)1, labs(small)) ///
								legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index")) name(inc_fs_dyn_eth, replace)
								
	coefplot				`ind'_mild_dyn_2 `ind'_mod_dyn_2 `ind'_sev_dyn_2 `ind'_std_dyn_2, ///
								drop(*.wave_temp *.country std_pp_index_lag mild_fs_lag mod_fs_lag ///
								sev_fs_lag std_fs_lag _cons ) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Malawi") ///
								levels(95) coeflabels(c.* = " " tot* = " ", notick) xlabel(-1(.2)1, labs(small)) ///
								legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index")) name(inc_fs_dyn_mwi, replace)
	}
	
	grc1leg2  				inc_fs_dyn_eth inc_fs_dyn_mwi, col(2) commonscheme title("Fractional Index") 
	graph export			"$export/figures/inc_fs_dyn.pdf", as(pdf) replace
	
***********************************************************************
**# 2. standardize denominator
***********************************************************************	

	foreach 				ind in std_pp_indexa {
	if 						"`ind'" == "std_pp_indexa" {
		local 				t = "Fractional Index"
	}

	foreach 				c in 1 2 3 {
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
			xtreg 					`fs'_fs c.`fs'_fs_lag##c.`ind'_lag i.wave_temp i.region#c.wave_temp geo_control ///
										[aweight = weight], fe vce(cluster hhid)
			eststo					`ind'_`fs'_dyn_`c'
			restore
		}
	}
	
	* generate graphics 
	coefplot				`ind'_mild_dyn_1 `ind'_mod_dyn_1 `ind'_sev_dyn_1 `ind'_std_dyn_1, ///
								drop(*.wave_temp *.country std_pp_indexa_lag mild_fs_lag mod_fs_lag ///
								sev_fs_lag std_fs_lag _cons) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Ethiopia") ///
								levels(95) coeflabels(c.* = " ", notick) xlabel(-1(.2)1, labs(small)) ///
								legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index")) name(`ind'_fs_dyn_eth, replace)
								
	coefplot				`ind'_mild_dyn_2 `ind'_mod_dyn_2 `ind'_sev_dyn_2 `ind'_std_dyn_2, ///
								drop(*.wave_temp *.country std_pp_indexa_lag mild_fs_lag mod_fs_lag ///
								sev_fs_lag std_fs_lag _cons) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Malawi") ///
								levels(95) coeflabels(c.* = " ", notick) xlabel(-1(.2)1, labs(small)) ///
								legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index")) name(`ind'_fs_dyn_mwi, replace)
								
	coefplot				`ind'_mild_dyn_3 `ind'_mod_dyn_3 `ind'_sev_dyn_3 `ind'_std_dyn_3, ///
								drop(*.wave_temp *.country std_pp_indexa_lag mild_fs_lag mod_fs_lag ///
								sev_fs_lag std_fs_lag _cons) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Nigeria") ///
								levels(95) coeflabels(c.* = " ", notick) xlabel(-1(.2)1, labs(small)) ///
								legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index")) name(`ind'_fs_dyn_nga, replace)
}	

	grc1leg2  				std_pp_indexa_fs_dyn_eth std_pp_indexa_fs_dyn_mwi ///
								 std_pp_indexa_fs_dyn_nga, col(3) commonscheme title("Fractional Index") 
	graph export			"$export/figures/dyn_fs_index1a.pdf", as(pdf) replace

***********************************************************************
**# 3. coping strategies
***********************************************************************	
	
tab cope_1 if std_pp_index != .
tab cope_2 if std_pp_index != .
tab cope_3 if std_pp_index != .
tab cope_4 if std_pp_index != .
tab cope_5 if std_pp_index != .
tab cope_6 if std_pp_index != .
tab cope_7 if std_pp_index != .
tab cope_8 if std_pp_index != .
tab cope_9 if std_pp_index != .
tab cope_10 if std_pp_index != .
tab cope_11 if std_pp_index != .
tab cope_12 if std_pp_index != .
tab cope_13 if std_pp_index != .
tab cope_14 if std_pp_index != .
tab cope_15 if std_pp_index != .
sum std_pp_index pp_index if cope_2 == 0
bys cope_2: sum std_pp_index pp_index
bys cope_3: sum std_pp_index pp_index
bys cope_4: sum std_pp_index pp_index
bys cope_5: sum std_pp_index pp_index
bys cope_6: sum std_pp_index pp_index
bys cope_7: sum std_pp_index pp_index
bys cope_8: sum std_pp_index pp_index
bys cope_9: sum std_pp_index pp_index
bys cope_10: sum std_pp_index pp_index
bys cope_11: sum std_pp_index pp_index
bys cope_12: sum std_pp_index pp_index
bys cope_13: sum std_pp_index pp_index
bys cope_14: sum std_pp_index pp_index
bys cope_15: sum std_pp_index pp_index
