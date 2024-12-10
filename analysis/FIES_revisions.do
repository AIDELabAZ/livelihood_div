* Project: diversification
* Created on: Dec 2024
* Created by: jdm
* Edited by: jdm
* Last edited: 3 Dec 2024
* Stata v.18.0

* does
	* address reviewer comments for FP
	* produces data that is output for using in FIES shiny app
	* inputs FIES shiny app output for further analysis
	* THIS MEANS ANALYSIS IS NOT FULLY REPRODUCIBLE WITH THIS CODE

* assumes
	* clean fies data
	* clean diversification indices data
	* coefplot

* TO DO:
	* everything


***********************************************************************
**# 0. setup
***********************************************************************

* define
	global	export	=		"$data/analysis/diversification"
	global	logout	=		"$data/analysis/logs"
	global  fies 	= 		"$data/analysis/food_security"

* open log
	cap log 				close
	log using				"$logout/fies_revisions", append

* clear memory 
	graph 					drop _all
	eststo 					clear
	
	
***********************************************************************
**# 1. prepare data
***********************************************************************

* load panel data
	use 					"$data/analysis/diversification/ld_pnl", replace	
	
* keep what we need
	keep if					country < 4
	keep					country wave hhid sector region fs6 fs7 fs8 ///
								fs1 fs2 fs3 fs4 fs5 weight
								
	order					fs1 fs2 fs3 fs4 fs5 fs6 fs7 fs8 weight sector ///
								region country wave hhid
			
* rename variables to match app needs
	rename					fs1 Worried
	rename					fs2 Healthy
	rename					fs3 Fewfood
	rename					fs4 Skipped
	rename					fs5 Ateless
	rename					fs6 Runout
	rename					fs7 Hungry
	rename					fs8 Whlday
	rename					weight WT
	rename					region Region
	
* generate person weight
	gen						wtperson = "NA"
	order					wtperson, after(WT)
	
* generate new urban variable
	gen						Urban = 1 if sector == 0
	replace					Urban = 0 if sector == 1
	drop 					sector
	order					Urban, before(Region)
	
	
************************************************************************
**# 2. export file for use in shiny
************************************************************************	

* export as csv
	export 					delimited using "$fies\fies_shiny.csv", nolabel replace

	
************************************************************************
**# 3. import file from shiny
************************************************************************	

* impost csv
	import 					delimited "$fies\Respondent_2024-12-03.csv", clear 

* destring variables
	loc fies			rawscore rawscorepar rawscoreparerr probmod_sev probsev
	foreach 			var of varlist `fies' {
		replace			`var' = "" if `var' == "NA"
		destring 		`var', replace
	}
	
* merge back in to panel data
	merge 1:1			hhid wave using "$data/analysis/diversification/ld_pnl"

* order variables
	order				wave_orig, after(wave)
	order				rawscore rawscorepar rawscoreparerr probmod_sev probsev, ///
							after(std_fs)
	drop				wt
	label var			country "Country"
	label var			wave "Month of Survey"
	label var			hhid "Unique HH ID"
	
	drop				_merge

* replace missing q3 in malawi baseline
	
* replace malawi fs9 for fs 6
	replace					fs6 = fs9 if country == 2 & wave == 0
	
* generate new variables to measure types of food insecurity
	gen						qual = 1 if fs2 == 1 | fs3 == 1
	replace					qual = 0 if fs2 == 0 & fs3 == 0 & country != 2
	replace					qual = 0 if fs2 == 0 & country == 2
	lab var					qual "Poor food quality"
	
	gen						reduce = 1 if fs4 == 1 | fs5 == 1
	replace					reduce = 0 if fs4 == 0 & fs5 == 0
	lab var					reduce "Reduce food quantity"
	
	gen						hungry = 1 if fs6 == 1 | fs7 == 1
	replace					hungry = 0 if fs6 == 0 & fs7 == 0
	lab var					hungry "Went without eating"
	
	
************************************************************************
**# 4. new fies analysis
************************************************************************	
	
************************************************************************
**# 4.1 new fies graphs
************************************************************************	

**# FIES
	* Ethiopia
	preserve
	keep 					if country == 1
	replace 				wave = 4 if wave == 0
	drop 					if wave > 10	
	
	collapse (mean) 		probmod_sev probsev mild_fs mod_fs sev_fs qual reduce hungry, by(country wave)
	
	twoway 					(line mild_fs wave, lcolor(cranberry*.4) clp(solid) fc(cranberry%25) alw(none)) ///
								(line mod_fs wave, lcolor(cranberry*.8) clp(solid) fc(cranberry%25) alw(none)) ///
								(line sev_fs wave, lcolor(cranberry*1.6) clp(solid) fc(cranberry%25) alw(none)) ///
								(line probmod_sev wave, lcolor(eltblue*.8) clp(solid) fc(eltblue%25) alw(none)) ///
								(line probsev wave, lcolor(eltblue*1.6) clp(solid) fc(eltblue%25) alw(none)) ///
								(line qual wave, lcolor(emerald*.4) clp(solid) fc(emerald%25) alw(none)) ///
								(line reduce wave, lcolor(emerald*.8) clp(solid) fc(emerald%25) alw(none)) ///
								(line hungry wave, lcolor(emerald*1.6) clp(solid) fc(emerald%25) alw(none) ///
								title("Ethiopia", size(large)) ylabel(0 "0" .2 "20" .4 "40" .6 "60" .8 "80" 1 "100", nogrid labs(small)) ///
								ytitle("Prevalence of Moderate or Severe Food Insecurity", size(small)) ///
								xlabel(4 "Jun/Jul19" 5 "May20" 6 "Jun20" 8 "Aug20" 9 "Sep20" 10 "Oct20", ///
								nogrid angle(45) labs(small)) xtitle(" ")), legend(label (1 "Mild Food Insecurity") ///
								label (2 "Moderate Food Insecurity") label (3 "Severe Food Insecurity") ///
								label (4 "Moderate or Severe Food Insecurity") label (5 "Severe Food Insecurity") ///
								label (6 "Consummed Poor Quality Food") label (7 "Reduced Quantity of Food") ///
								label (8 "Went Hungry for Less than a Day") pos(6) col(1) size(small) margin(-1.5 0 0 0) ) ///
								name(eth_fies_prob, replace)
							
	restore 		
	

	* Malawi
	preserve
	keep 					if country == 2
	replace 				wave = 5 if wave == 0
	replace 				wave = 17 if wave == 18
	
	collapse (mean) 		probmod_sev probsev mild_fs mod_fs sev_fs qual reduce hungry, by(country wave)
	
	twoway 					(line mild_fs wave, lcolor(cranberry*.4) clp(solid) fc(cranberry%25) alw(none)) ///
								(line mod_fs wave, lcolor(cranberry*.8) clp(solid) fc(cranberry%25) alw(none)) ///
								(line sev_fs wave, lcolor(cranberry*1.6) clp(solid) fc(cranberry%25) alw(none)) ///
								(line probmod_sev wave, lcolor(eltblue*.8) clp(solid) fc(eltblue%25) alw(none)) ///
								(line probsev wave, lcolor(eltblue*1.6) clp(solid) fc(eltblue%25) alw(none)) ///
								(line qual wave, lcolor(emerald*.4) clp(solid) fc(emerald%25) alw(none)) ///
								(line reduce wave, lcolor(emerald*.8) clp(solid) fc(emerald%25) alw(none)) ///
								(line hungry wave, lcolor(emerald*1.6) clp(solid) fc(emerald%25) alw(none) ///
								title("Malawi", size(large)) ylabel(0 "0" .2 "20" .4 "40" .6 "60" .8 "80" 1 "100", nogrid labs(small)) ///
								ytitle(" ", size(small)) ///
								xlabel(5 "2019" 6 "Jun20" 7 "Jul20" 8 "Aug20" 11 "Nov20" ///
								12 "Dec20" 13 "Jan21" 15 "Mar21" 16 "Apr21" 17 "May21", ///
								nogrid angle(45) labs(small)) xtitle(" ")), legend(label (1 "Mild Food Insecurity") ///
								label (2 "Moderate Food Insecurity") label (3 "Severe Food Insecurity") ///
								label (4 "Moderate or Severe Food Insecurity") label (5 "Severe Food Insecurity") ///
								pos(6) col(1) size(small) margin(-1.5 0 0 0) ) ///
								name(mwi_fies_prob, replace)
							
	restore
	
	* Nigeria
	preserve
	keep 					if country == 3
	replace 				wave = 4 if wave == -1
	replace 				wave = 5 if wave == 0
	drop 					if wave > 11
	
	collapse (mean) 		probmod_sev probsev mild_fs mod_fs sev_fs qual reduce hungry, by(country wave)
	
	twoway 					(line mild_fs wave, lcolor(cranberry*.4) clp(solid) fc(cranberry%25) alw(none)) ///
								(line mod_fs wave, lcolor(cranberry*.8) clp(solid) fc(cranberry%25) alw(none)) ///
								(line sev_fs wave, lcolor(cranberry*1.6) clp(solid) fc(cranberry%25) alw(none)) ///
								(line probmod_sev wave, lcolor(eltblue*.8) clp(solid) fc(eltblue%25) alw(none)) ///
								(line probsev wave, lcolor(eltblue*1.6) clp(solid) fc(eltblue%25) alw(none)) ///
								(line qual wave, lcolor(emerald*.4) clp(solid) fc(emerald%25) alw(none)) ///
								(line reduce wave, lcolor(emerald*.8) clp(solid) fc(emerald%25) alw(none)) ///
								(line hungry wave, lcolor(emerald*1.6) clp(solid) fc(emerald%25) alw(none) ///
								title("Nigeria", size(large)) ylabel(0 "0" .2 "20" .4 "40" .6 "60" .8 "80" 1 "100", nogrid labs(small)) ///
								ytitle("Prevalence of Moderate or Severe Food Insecurity", size(small)) ///
								xlabel(4 "Aug/Sep18" 5 "Jan/Feb19" 6 "Jun20" 8 "Aug20" 11 "Nov20", ///
								nogrid angle(45) labs(small)) xtitle(" ")), legend(label (1 "Mild Food Insecurity") ///
								label (2 "Moderate Food Insecurity") label (3 "Severe Food Insecurity") ///
								label (4 "Moderate or Severe Food Insecurity") label (5 "Severe Food Insecurity") ///
								pos(6) col(1) size(small) margin(-1.5 0 0 0) ) ///
								name(nga_fies_prob, replace)
							
	restore
	
	grc1leg2 				eth_fies_prob mwi_fies_prob nga_fies_prob, col(2) iscale(.5) ///
								ring(0) pos(4) holes(4) commonscheme		
	graph export 		"$export/figures/fies_line_prob.pdf", as(pdf) replace

	
************************************************************************
**## 4.1 new fies dynamic panel
************************************************************************	
			
* prep panel 
	sort 					hhid wave_orig
	xtset 					hhid wave_orig
	
	
	* gen y0 fs and xfill by hhid
		foreach 			f in mild_fs mod_fs sev_fs std_fs fs1 fs2 fs3 fs4 fs5 fs6 fs7 fs8 {
			gen 				y0_`f' = `f' if wave == 0
			xfill 				y0_`f', i(hhid)
		}
		
	* xfill diversification by hhid
		* pre-covid indices
		ds 					*pre_index*
		foreach 			ind in `r(varlist)' {
			xfill 				`ind', i(hhid)
		}
		
	* generate and xfill outcome vars for each wave 
		forval 					x = 0/11 {
			foreach 				fs in mild_fs mod_fs sev_fs std_fs fs1 fs2 fs3 fs4 fs5 fs6 fs7 fs8 {
				gen 					y`x'_fs_`fs' = `fs' if wave_orig == `x'
				xfill 					y`x'_fs_`fs', i(hhid)
			}
		}		
	
* dynamic panel

	foreach 				ind in std_pp_index {
	if 						"`ind'" == "std_pp_index" {
		local 				t = "Fractional Index"
	}

	foreach 				c in 1 3 {
		foreach 				fs in mild_fs mod_fs sev_fs std_fs fs1 fs2 fs3 fs4 fs5 fs6 fs7 fs8 {
			* balance panel with lagged variables
			preserve
			keep 					if country == `c'
			drop 					if `fs' == . // can never use obs without dependent var
			egen 					temp = total(inrange(wave_orig, 0, 11)), by(hhid)
			drop 					if temp < 6 & country == 1 // 3,317 of 17,759 dropped (19%)
			drop 					if temp < 4 & country == 3 // 1,050 of 7,606 dropped (14%)
			sort 					hhid wave_, stable 
			bysort 					hhid (wave_orig): gen `ind'_lag = `ind'[_n-1]
			bysort 					hhid (wave_orig): gen fs_lag = `fs'[_n-1]
			gen						fs =  `fs'
			egen 					wave_temp =  group(country wave_orig)
			egen 					max = max(wave_temp), by(hhid)
			drop 					if max == 4 & country == 1 // drops 8
			* dynamic panel regression
			xtset 					hhid wave_temp 
			xtreg 					fs c.fs_lag##c.`ind'_lag i.wave_temp i.region#c.wave_temp ///
										[aweight = weight], fe vce(cluster hhid)					
			eststo					`ind'_`fs'_dyn_`c'
			restore
		}
	}
	foreach 				c in 2 {
		foreach 				fs in mild_fs mod_fs sev_fs std_fs fs1 fs2 fs4 fs5 fs6 fs7 {
			* balance panel with lagged variables
			preserve
			keep 					if country == `c'
			drop 					if `fs' == . // can never use obs without dependent var
			egen 					temp = total(inrange(wave_orig, 0, 11)), by(hhid)
			drop 					if temp < 10 & country == 2 // 2,812 of 16,102 dropped (17%)
			sort 					hhid wave_, stable 
			bysort 					hhid (wave_orig): gen `ind'_lag = `ind'[_n-1]
			bysort 					hhid (wave_orig): gen fs_lag = `fs'[_n-1]
			gen						fs =  `fs'
			egen 					wave_temp =  group(country wave_orig)
			egen 					max = max(wave_temp), by(hhid)
			drop 					if max == 4 & country == 1 // drops 8
			* dynamic panel regression
			xtset 					hhid wave_temp 
			xtreg 					fs c.fs_lag##c.`ind'_lag i.wave_temp i.region#c.wave_temp ///
										[aweight = weight], fe vce(cluster hhid)					
			eststo					`ind'_`fs'_dyn_`c'
			restore
		}
	}
}
	foreach 				ind in std_pp_index {
	if 						"`ind'" == "std_pp_index" {
		local 				t = "Fractional Index"
	}	
	* generate graphics 
	coefplot				`ind'_mild_fs_dyn_1 `ind'_mod_fs_dyn_1 `ind'_sev_fs_dyn_1 `ind'_std_fs_dyn_1 ///
								`ind'_fs1_dyn_1 `ind'_fs2_dyn_1 `ind'_fs3_dyn_1 `ind'_fs4_dyn_1 ///
								`ind'_fs5_dyn_1 `ind'_fs6_dyn_1 `ind'_fs7_dyn_1 `ind'_fs8_dyn_1, ///
								drop(*.wave_temp *.country std_pp_index_lag fs_lag _cons) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Ethiopia") ///
								levels(95) coeflabels(c.* = " ", notick) xlabel(-1(.2)1, labs(small)) ///
								legend(col(1) pos(6) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index") label(10 "FS1: Worried") ///
								label(12 "FS2: Healthy Foods") label(14 "FS3: Few Foods") label(16 "FS4: Skipped") ///
								label(18 "FS5: Ate Less") label(20 "FS6: Ran Out") label(22 "FS7: Hungry") ///
								label(24 "FS8: Whole Day") ) name(`ind'_fsQ_dyn_eth, replace)
								
	coefplot				`ind'_mild_fs_dyn_2 `ind'_mod_fs_dyn_2 `ind'_sev_fs_dyn_2 `ind'_std_fs_dyn_2 ///
								`ind'_fs1_dyn_2 `ind'_fs2_dyn_2 `ind'_fs4_dyn_2 ///
								`ind'_fs5_dyn_2 `ind'_fs6_dyn_2 `ind'_fs7_dyn_2, ///
								drop(*.wave_temp *.country std_pp_index_lag fs_lag _cons) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Malawi") ///
								levels(95) coeflabels(c.* = " ", notick) xlabel(-1(.2)1, labs(small)) ///
								legend(col(1) pos(6) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index") label(10 "FS1: Worried") ///
								label(12 "FS2: Healthy Foods") label(14 "FS4: Skipped") label(16 "FS5: Ate Less") ///
								label(18 "FS6: Ran Out") label(20 "FS7: Hungry") ) name(`ind'_fsQ_dyn_mwi, replace)
								
	coefplot				`ind'_mild_fs_dyn_3 `ind'_mod_fs_dyn_3 `ind'_sev_fs_dyn_3 `ind'_std_fs_dyn_3 ///
								`ind'_fs1_dyn_3 `ind'_fs2_dyn_3 `ind'_fs3_dyn_3 `ind'_fs4_dyn_3 ///
								`ind'_fs5_dyn_3 `ind'_fs6_dyn_3 `ind'_fs7_dyn_3 `ind'_fs8_dyn_3, ///
								drop(*.wave_temp *.country std_pp_index_lag fs_lag _cons) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Nigeria") ///
								levels(95) coeflabels(c.* = " ", notick) xlabel(-1(.2)1, labs(small)) ///
								legend(col(1) pos(6) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index") label(10 "FS1: Worried") ///
								label(12 "FS2: Healthy Foods") label(14 "FS3: Few Foods") label(16 "FS4: Skipped") ///
								label(18 "FS5: Ate Less") label(20 "FS6: Ran Out") label(22 "FS7: Hungry") ///
								label(24 "FS8: Whole Day") ) name(`ind'_fsQ_dyn_nga, replace)
}	

	grc1leg2  				std_pp_index_fsQ_dyn_eth std_pp_index_fsQ_dyn_mwi ///
								 std_pp_index_fsQ_dyn_nga, col(2) iscale(.5) ///
								ring(0) pos(4) holes(4) commonscheme	
	graph export			"$export/figures/dyn_fsQ.pdf", as(pdf) replace

	
************************************************************************
**## 4.2 new fies ancova
************************************************************************	

graph 					drop _all
eststo 					clear

* ANCOVA regressions
	foreach 					c in 1 3 {
		foreach 					f in mild_fs mod_fs sev_fs std_fs fs1 fs2 fs3 fs4 fs5 fs6 fs7 fs8 {
			preserve
			keep						if country == `c'
			drop 						if wave == -1
			bysort 						hhid (wave_orig): gen fs_y0 = y0_`f'
			
			* ANCOVA
			reg 						`f' c.std_pre_index_hhi##c.fs_y0 ib(1).wave c.wave#i.region ///
												[aweight = weight] if wave != 0, vce(cluster hhid) 
			eststo						`f'_an_hhi_`c'
			restore
		}
	}

	foreach 					c in 2 {
		foreach 					f in mild_fs mod_fs sev_fs std_fs fs1 fs2 fs4 fs5 fs6 fs7 {
			preserve
			keep						if country == `c'
			drop 						if wave == -1
			bysort 						hhid (wave_orig): gen fs_y0 = y0_`f'
			
			* ANCOVA
			reg 						`f' c.std_pre_index_hhi##c.fs_y0 ib(1).wave c.wave#i.region ///
												[aweight = weight] if wave != 0, vce(cluster hhid) 
			eststo						`f'_an_hhi_`c'
			restore
		}
	}	
	* generate ANCOVA graphics 
	coefplot				mild_fs_an_hhi_1 mod_fs_an_hhi_1 sev_fs_an_hhi_1 std_fs_an_hhi_1 ///
								fs1_an_hhi_1 fs2_an_hhi_1 fs3_an_hhi_1 fs4_an_hhi_1 ///
								fs5_an_hhi_1 fs6_an_hhi_1 fs7_an_hhi_1 fs8_an_hhi_1, ///
								keep(std_pre_index_hhi) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Ethiopia") ///
								levels(95) coeflabels(std_pre_index_hhi = " ", notick) xlabel(-.4(.1).4, labs(small)) ///
								legend(col(1) pos(6) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index") label(10 "FS1: Worried") ///
								label(12 "FS2: Healthy Foods") label(14 "FS3: Few Foods") label(16 "FS4: Skipped") ///
								label(18 "FS5: Ate Less") label(20 "FS6: Ran Out") label(22 "FS7: Hungry") ///
								label(24 "FS8: Whole Day") ) name(anc_hhi_fs_eth, replace)
	
	coefplot				mild_fs_an_hhi_2 mod_fs_an_hhi_2 sev_fs_an_hhi_2 std_fs_an_hhi_2 ///
								fs1_an_hhi_2 fs2_an_hhi_2 fs4_an_hhi_2 ///
								fs5_an_hhi_2 fs6_an_hhi_2 fs7_an_hhi_2, ///
								keep(std_pre_index_hhi) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Malawi") ///
								levels(95) coeflabels(std_pre_index_hhi = " ", notick) xlabel(-.4(.1).4, labs(small)) ///
								legend(col(1) pos(6) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index") label(10 "FS1: Worried") ///
								label(12 "FS2: Healthy Foods") label(14 "FS3: Few Foods") label(16 "FS4: Skipped") ///
								label(18 "FS5: Ate Less") label(20 "FS6: Ran Out") label(22 "FS7: Hungry") ///
								label(24 "FS8: Whole Day") ) name(anc_hhi_fs_mwi, replace)
	
	coefplot				mild_fs_an_hhi_3 mod_fs_an_hhi_3 sev_fs_an_hhi_3 std_fs_an_hhi_3 ///
								fs1_an_hhi_3 fs2_an_hhi_3 fs3_an_hhi_3 fs4_an_hhi_3 ///
								fs5_an_hhi_3 fs6_an_hhi_3 fs7_an_hhi_3 fs8_an_hhi_3, ///
								keep(std_pre_index_hhi) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Nigeria") ///
								levels(95) coeflabels(std_pre_index_hhi = " ", notick) xlabel(-.4(.1).4, labs(small)) ///
								legend(col(1) pos(6) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index") label(10 "FS1: Worried") ///
								label(12 "FS2: Healthy Foods") label(14 "FS3: Few Foods") label(16 "FS4: Skipped") ///
								label(18 "FS5: Ate Less") label(20 "FS6: Ran Out") label(22 "FS7: Hungry") ///
								label(24 "FS8: Whole Day") ) name(anc_hhi_fs_nga, replace)
	
	grc1leg2  				anc_hhi_fs_eth anc_hhi_fs_mwi anc_hhi_fs_nga, col(2) iscale(.5) ///
								ring(0) pos(4) holes(4) commonscheme	
	graph export			"$export/figures/fsQ_anc.pdf", as(pdf) replace


	
* close log
	log 			close 
	
/* END */
	