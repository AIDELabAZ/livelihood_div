* Project: diversification
* Created on: Jan 2022
* Created by: amf
* Edited by: jdm
* Last edited: 8 February 2023
* Stata v.17.0

* does
	* runs livelihood diversification regression

* assumes
	* clean fies data
	* clean panel data
	* coefplot

* TO DO:
	* changes made through line 727 - no changes in section on heterogeneous effects
	* should consider: relabeling graphs and figures (away from index 1, 2, etc.)

* **********************************************************************
**# setup
* **********************************************************************

* define
	global	export	=		"$data/analysis/diversification"
	global	logout	=		"$data/analysis/logs"
	global  fies 	= 		"$data/analysis/food_security"

* open log
	cap log 				close
	log using				"$logout/ld_regs", append

* clear memory 
	graph 					drop _all
	eststo 					clear
	
* **********************************************************************
**# prepare data
* **********************************************************************

* load data
	use 					"$data/analysis/diversification/ld_pnl", replace
				
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
		* pre-covid indices
		ds 					*pre_index*
		foreach 			ind in `r(varlist)' {
			xfill 				`ind', i(hhid)
		}
		
	* generate and xfill outcome vars for each wave 
		forval 					x = 0/11 {
			foreach 				fs in mild mod sev {
				gen 					y`x'_fs_`fs' = `fs'_fs if wave_orig == `x'
				xfill 					y`x'_fs_`fs', i(hhid)
			}
		}		
	
	
* **********************************************************************
**# dynamic panel model (index 1)
* **********************************************************************


* **********************************************************************
**## food security index 1
* **********************************************************************			

	foreach 				ind in std_pp_index {
	if 						"`ind'" == "std_pp_index" {
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
			xtreg 					`fs'_fs c.`fs'_fs_lag##c.`ind'_lag i.wave_temp i.region#c.wave_temp ///
										[aweight = weight], fe vce(cluster hhid)
			eststo					`ind'_`fs'_dyn_`c'
			restore
		}
	}
	* generate graphics 
	coefplot				`ind'_mild_dyn_1 `ind'_mod_dyn_1 `ind'_sev_dyn_1 `ind'_std_dyn_1, ///
								drop(*.wave_temp *.country std_pp_index_lag mild_fs_lag mod_fs_lag ///
								sev_fs_lag std_fs_lag _cons) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Ethiopia") ///
								levels(95) coeflabels(c.* = " ", notick) xlabel(-1(.2)1, labs(small)) ///
								legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index")) name(`ind'_fs_dyn_eth, replace)
								
	coefplot				`ind'_mild_dyn_2 `ind'_mod_dyn_2 `ind'_sev_dyn_2 `ind'_std_dyn_2, ///
								drop(*.wave_temp *.country std_pp_index_lag mild_fs_lag mod_fs_lag ///
								sev_fs_lag std_fs_lag _cons) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Malawi") ///
								levels(95) coeflabels(c.* = " ", notick) xlabel(-1(.2)1, labs(small)) ///
								legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index")) name(`ind'_fs_dyn_mwi, replace)
								
	coefplot				`ind'_mild_dyn_3 `ind'_mod_dyn_3 `ind'_sev_dyn_3 `ind'_std_dyn_3, ///
								drop(*.wave_temp *.country std_pp_index_lag mild_fs_lag mod_fs_lag ///
								sev_fs_lag std_fs_lag _cons) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Nigeria") ///
								levels(95) coeflabels(c.* = " ", notick) xlabel(-1(.2)1, labs(small)) ///
								legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index")) name(`ind'_fs_dyn_nga, replace)
}	

	grc1leg2  				std_pp_index_fs_dyn_eth std_pp_index_fs_dyn_mwi ///
								 std_pp_index_fs_dyn_nga, col(3) commonscheme title("Fractional Index") 
	graph export			"$export/figures/dyn_fs_index1.pdf", as(pdf) replace


	* generate table
	esttab 					std_pp_index_mild_dyn_1 std_pp_index_mod_dyn_1 std_pp_index_sev_dyn_1 std_pp_index_std_dyn_1 ///
								std_pp_index_mild_dyn_2 std_pp_index_mod_dyn_2 std_pp_index_sev_dyn_2 std_pp_index_std_dyn_2 ///
								std_pp_index_mild_dyn_3 std_pp_index_mod_dyn_3 std_pp_index_sev_dyn_3 std_pp_index_std_dyn_3 ///
								using "$export/tables/dyn_fs.tex", b(3) se(3) replace drop(*wave* _cons) noobs ///
								booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
								stat(N, labels("Observations") fmt(%9.0fc)) ///
								fragment label prehead("\begin{tabular}{l*{12}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] " ///
								"& \multicolumn{4}{c}{Ethiopia} & \multicolumn{4}{c}{Malawi} & " ///
								"\multicolumn{4}{c}{Nigeria} \\ & \multicolumn{1}{c}{Mild} & \multicolumn{1}{c}{Moderate} & " ///
								"\multicolumn{1}{c}{Severe} & \multicolumn{1}{c}{FS Index} & \multicolumn{1}{c}{Mild} " ///
								"& \multicolumn{1}{c}{Moderate} & \multicolumn{1}{c}{Severe} & \multicolumn{1}{c}{FS Index} " ///
								"& \multicolumn{1}{c}{Mild} & \multicolumn{1}{c}{Moderate} & \multicolumn{1}{c}{Severe} " ///
								"& \multicolumn{1}{c}{FS Index} \\ \midrule ") coeflabels(std_pp_index_lag "Lagged FI" ///
								mild_fs_lag "Lagged Mild" c.mild_fs_lag#c.std_pp_index_lag "Lagged Mild $\times$ Lagged FI" ///
								mod_fs_lag "Lagged Moderate" c.mod_fs_lag#c.std_pp_index_lag "Lagged Moderate $\times$ Lagged FI" ///
								sev_fs_lag "Lagged Severe" c.sev_fs_lag#c.std_pp_index_lag "Lagged Severe $\times$ Lagged FI" ///
								std_fs_lag "Lagged FS Index" c.std_fs_lag#c.std_pp_index_lag "Lagged FS Index $\times$ Lagged FI") ///	
								order(std_pp_index_lag mild_fs_lag c.mild_fs_lag#c.std_pp_index_lag mod_fs_lag ///
								c.mod_fs_lag#c.std_pp_index_lag sev_fs_lag c.sev_fs_lag#c.std_pp_index_lag ///
								std_fs_lag c.std_fs_lag#c.std_pp_index_lag) ///
								postfoot("\hline \hline \\[-1.8ex] " ///
								"\multicolumn{13}{p{760pt}}{\small \noindent \textit{Note}: The table displays regression results " ///
								"from our dynamic panel specification with household fixed effects, round dummies, and region-time trends " ///
								"(see Equation \ref{eq:dyn}). FI stands for Fractional Index while FS stands for our standardized index of " ///
								"food insecurity. Standard errors, clustered at the household, are reported in parentheses " ///
								"(*** p$<$0.001, ** p$<$0.01, * p$<$0.05). Results correspond with coefficients presented in Figure~\ref{fig:dyn_fs}.}  \end{tabular}")
	

* **********************************************************************
**## education index 1 
* **********************************************************************		

	foreach 				ind in std_pp_index {
	if 						"`ind'" == "std_pp_index" {
		local 				t = "Fractional Index"
	}
	
	foreach 				c in 1 2 3 4 {
		* balance panel with lagged variables 
		preserve
		keep 					if country == `c'
		drop 					if edu_act == . // can never use obs without dependent var
		egen 					temp = total(inrange(wave_orig, 0, 11)), by(hhid)
		drop 					if temp < 7 & country == 1 // 5,561 of 11,854 dropped (47%)
		drop 					if temp < 5 & country == 2 // 1,349 of 6,039 dropped (22%)
		drop 					if temp < 9 & country == 3 // 3,772 of 12,997 dropped (29%)
		drop 					if temp < 7 & country == 4 // 1,704 of 10,380 dropped (16%)
		sort 					hhid wave_, stable 
		bysort 					hhid (wave_): 	gen `ind'_lag = `ind'[_n-1]
		bysort					hhid (wave_): 	gen edu_act_lag_`c' = edu_act[_n-1] if country == `c'
		egen 					wave_temp =  group(country wave_orig)
		* dynamic panel regression
		xtset 					hhid wave_temp
		xtreg 					edu_act i.edu_act_lag_`c'##c.`ind'_lag i.wave_temp i.region#c.wave_temp ///
									[aweight = weight], fe vce(cluster hhid)	
		eststo					`ind'_edu_dyn_`c'
		restore
		}
	}
	
	* generate figure
	coefplot			std_pp_index_edu_dyn_1 std_pp_index_edu_dyn_2 std_pp_index_edu_dyn_3 std_pp_index_edu_dyn_4, ///
								drop(*.wave_temp *.region#c.wave_temp _cons *.edu_act_lag_1 *.edu_act_lag_2 ///
								*.edu_act_lag_3 *.edu_act_lag_4 std_pp_index_lag) ///
								xline(0, lcolor(maroon)) xlabel(-.8(.2).8, labs(small))  title("`t'") ///				
								coeflabels(1.edu_act_lag_1#c.std_pp_index_lag = "Ethiopia" ///
								1.edu_act_lag_2#c.std_pp_index_lag = "Malawi" ///
								1.edu_act_lag_3#c.std_pp_index_lag = "Nigeria" ///
								1.edu_act_lag_4#c.std_pp_index_lag= "Uganda") ///
								xtitle("Effect on Educational Engagement") levels(95) ///
								legend(off) name(dyn_edu_index1, replace)
								
		graph export			"$export/figures/dyn_edu_index1.pdf", as(pdf) replace
		
	foreach 				ind in std_pp_index {
	if 						"`ind'" == "std_pp_index" {
		local 				t = "Fractional Index"
	}
	
	foreach 				c in 1 2 3 4 {
		* balance panel with lagged variables 
		preserve
		keep 					if country == `c'
		drop 					if edu_act == . // can never use obs without dependent var
		egen 					temp = total(inrange(wave_orig, 0, 11)), by(hhid)
		drop 					if temp < 7 & country == 1 // 5,561 of 11,854 dropped (47%)
		drop 					if temp < 5 & country == 2 // 1,349 of 6,039 dropped (22%)
		drop 					if temp < 9 & country == 3 // 3,772 of 12,997 dropped (29%)
		drop 					if temp < 7 & country == 4 // 1,704 of 10,380 dropped (16%)
		sort 					hhid wave_, stable 
		bysort 					hhid (wave_): 	gen `ind'_lag = `ind'[_n-1]
		bysort					hhid (wave_): 	gen edu_act_lag = edu_act[_n-1]
		egen 					wave_temp =  group(country wave_orig)
		* dynamic panel regression
		xtset 					hhid wave_temp
		xtreg 					edu_act i.edu_act_lag##c.`ind'_lag i.wave_temp i.region#c.wave_temp ///
									[aweight = weight], fe vce(cluster hhid)	
		eststo					`ind'_edu_dyn_`c'
		restore
		}
	}	
	* generate variables for labels 
	gen 					std_pp_index_lag = .
	lab var 				std_pp_index_lag "Lagged Index"
	gen 					std_pre_index_hhi_lag = . 
	lab var 				std_pre_index_hhi_lag "Lagged Index 2"
	gen 					edu_act_lag = .
	lab var 				edu_act_lag "Lagged Education"
	
	* generate table
	esttab 					std_pp_index_edu_dyn_1 std_pp_index_edu_dyn_2 std_pp_index_edu_dyn_3 std_pp_index_edu_dyn_4 ///
								using "$export/tables/dyn_edu.tex", b(3) se(3) replace ///
								prehead("\begin{tabular}{l*{4}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] " ///
								"& \multicolumn{1}{c}{Ethiopia} & \multicolumn{1}{c}{Malawi} & \multicolumn{1}{c}{Nigeria} & " ///
								"\multicolumn{1}{c}{Uganda} \\ ") drop(*wave* _cons) noobs ///
								booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
								stat(N, labels("Observations") fmt(%9.0fc)) coeflabels(1.edu_act_lag "Lagged Education" ///
								std_pp_index_lag "Lagged FI" 1.edu_act_lag#c.std_pp_index_lag "Lagged Education $\times$ Lagged FI") ///
								fragment label postfoot("\hline \hline \\[-1.8ex] \multicolumn{5}{J{13.5cm}}{\small " ///
								"\noindent \textit{Note}: The table displays regression results " ///
								"from our dynamic panel specification with household fixed effects, round dummies, and region-time trends " ///
								"(see Equation \ref{eq:dyn}). FI stands for Fractional Index. Standard errors, clustered at the household, " ///
								"are reported in parentheses (*** p$<$0.001, ** p$<$0.01, * p$<$0.05). Results correspond with coefficients presented in Figure~\ref{fig:dyn_edu}.}  \end{tabular}") 
	
	drop 					*_lag

	
	
* **********************************************************************
**# dynamic panel model and stringency index (index 1)
* **********************************************************************

graph 					drop _all
eststo 					clear


* **********************************************************************
**## food security index 1
* **********************************************************************			

	foreach 				ind in std_pp_index {
	if 						"`ind'" == "std_pp_index" {
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
			* dynamic panel regression with interactions
			xtset 					hhid wave_temp
			xtreg 					`fs'_fs c.stringency_index##c.`fs'_fs_lag##c.`ind'_lag i.wave_temp ///
											i.region#c.wave_temp [aweight = weight] if country == `c', fe vce(cluster hhid)
			eststo					`ind'_`fs'_inter_`c' 
			restore
		}
	}
	* generate graphics 
	coefplot				`ind'_mild_inter_1 `ind'_mod_inter_1 `ind'_sev_inter_1 `ind'_std_inter_1, ///
								keep(c.stringency_index#c.mild_fs_lag#c.std_pp_index_lag ///
								c.stringency_index#c.mod_fs_lag#c.std_pp_index_lag ///
								c.stringency_index#c.sev_fs_lag#c.std_pp_index_lag ///
								c.stringency_index#c.std_fs_lag#c.std_pp_index_lag) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Ethiopia") ///
								levels(95) coeflabels(c.* = " ", notick) xlabel(-.4(.1).4, labs(small)) ///
								legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index")) name(`ind'_fs_inter_eth, replace)
					
	coefplot				`ind'_mild_inter_2 `ind'_mod_inter_2 `ind'_sev_inter_2 `ind'_std_inter_2, ///
								keep(c.stringency_index#c.mild_fs_lag#c.std_pp_index_lag ///
								c.stringency_index#c.mod_fs_lag#c.std_pp_index_lag ///
								c.stringency_index#c.sev_fs_lag#c.std_pp_index_lag ///
								c.stringency_index#c.std_fs_lag#c.std_pp_index_lag) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Malawi") ///
								levels(95) coeflabels(c.* = " ", notick) xlabel(-.1(.02).1, labs(small)) ///
								legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index")) name(`ind'_fs_inter_mwi, replace)
								
	coefplot				`ind'_mild_inter_3 `ind'_mod_inter_3 `ind'_sev_inter_3 `ind'_std_inter_3, ///
								keep(c.stringency_index#c.mild_fs_lag#c.std_pp_index_lag ///
								c.stringency_index#c.mod_fs_lag#c.std_pp_index_lag ///
								c.stringency_index#c.sev_fs_lag#c.std_pp_index_lag ///
								c.stringency_index#c.std_fs_lag#c.std_pp_index_lag) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Nigeria") ///
								levels(95) coeflabels(c.* = " ", notick) xlabel(-.1(.02).1, labs(small)) ///
								legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index")) name(`ind'_fs_inter_nga, replace)
}		
		
	grc1leg2  				std_pp_index_fs_inter_eth std_pp_index_fs_inter_mwi ///
								 std_pp_index_fs_inter_nga, col(3) commonscheme title("Fractional Index") 
	graph export			"$export/figures/inter_fs_index1.pdf", as(pdf) replace

	* generate table
	esttab 					std_pp_index_mild_inter_1 std_pp_index_mod_inter_1 std_pp_index_sev_inter_1 std_pp_index_std_inter_1 ///
								std_pp_index_mild_inter_2 std_pp_index_mod_inter_2 std_pp_index_sev_inter_2 std_pp_index_std_inter_2 ///
								std_pp_index_mild_inter_3 std_pp_index_mod_inter_3 std_pp_index_sev_inter_3 std_pp_index_std_inter_3 ///
								using "$export/tables/inter_fs.tex", b(3) se(3) replace drop(*wave* _cons) noobs ///
								booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
								stat(N, labels("Observations") fmt(%9.0fc)) ///
								fragment label prehead("\begin{tabular}{l*{12}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] " ///
								"& \multicolumn{4}{c}{Ethiopia} & \multicolumn{4}{c}{Malawi} & " ///
								"\multicolumn{4}{c}{Nigeria} \\ & \multicolumn{1}{c}{Mild} & \multicolumn{1}{c}{Moderate} & " ///
								"\multicolumn{1}{c}{Severe} & \multicolumn{1}{c}{FS Index} & \multicolumn{1}{c}{Mild} " ///
								"& \multicolumn{1}{c}{Moderate} & \multicolumn{1}{c}{Severe} & \multicolumn{1}{c}{FS Index} " ///
								"& \multicolumn{1}{c}{Mild} & \multicolumn{1}{c}{Moderate} & \multicolumn{1}{c}{Severe} " ///
								"& \multicolumn{1}{c}{FS Index} \\ \midrule ") coeflabels(stringency_index "COVID-19 Stringency" ///
								std_pp_index_lag "Lagged FI" c.stringency_index#c.std_pp_index_lag "COVID-19 Stringency $\times$ Lagged FI" ///
								mild_fs_lag "Lagged Mild" c.stringency_index#c.mild_fs_lag "Lagged Mild $\times$ COVID-19 Stringency" ///
								c.mild_fs_lag#c.std_pp_index_lag "Lagged Mild $\times$ Lagged FI" ///
								c.stringency_index#c.mild_fs_lag#c.std_pp_index_lag "Lagged Mild $\times$ COVID-19 Stringency $\times$ Lagged FI" ///
								mod_fs_lag "Lagged Moderate" c.stringency_index#c.mod_fs_lag "Lagged Moderate $\times$ COVID-19 Stringency" ///
								c.mod_fs_lag#c.std_pp_index_lag "Lagged Moderate $\times$ Lagged FI" ///
								c.stringency_index#c.mod_fs_lag#c.std_pp_index_lag "Lagged Moderate $\times$ COVID-19 Stringency $\times$ Lagged FI" ///
								sev_fs_lag "Lagged Severe" c.stringency_index#c.sev_fs_lag "Lagged Severe $\times$ COVID-19 Stringency" ///
								c.sev_fs_lag#c.std_pp_index_lag "Lagged Severe $\times$ Lagged FI" ///
								c.stringency_index#c.sev_fs_lag#c.std_pp_index_lag "Lagged Severe $\times$ COVID-19 Stringency $\times$ Lagged FI" ///
								std_fs_lag "Lagged FS Index" c.stringency_index#c.std_fs_lag "Lagged FS Index $\times$ COVID-19 Stringency" ///
								c.std_fs_lag#c.std_pp_index_lag "Lagged FS Index $\times$ Lagged FI" ///
								c.stringency_index#c.std_fs_lag#c.std_pp_index_lag "Lagged FS Index $\times$ COVID-19 Stringency $\times$ Lagged FI" ) ///
								order(stringency_index std_pp_index_lag c.stringency_index#c.std_pp_index_lag ///
								mild_fs_lag c.stringency_index#c.mild_fs_lag c.mild_fs_lag#c.std_pp_index_lag ///
								c.stringency_index#c.mild_fs_lag#c.std_pp_index_lag ///
								mod_fs_lag  c.stringency_index#c.mod_fs_lag  c.mod_fs_lag#c.std_pp_index_lag  ///
								c.stringency_index#c.mod_fs_lag#c.std_pp_index_lag  ///
								sev_fs_lag  c.stringency_index#c.sev_fs_lag  c.sev_fs_lag#c.std_pp_index_lag  ///
								c.stringency_index#c.sev_fs_lag#c.std_pp_index_lag  ///
								std_fs_lag  c.stringency_index#c.std_fs_lag c.std_fs_lag#c.std_pp_index_lag  ///
								c.stringency_index#c.std_fs_lag#c.std_pp_index_lag) ///
								postfoot("\hline \hline \\[-1.8ex] " ///
								"\multicolumn{13}{p{900pt}}{\small \noindent \textit{Note}: The table displays regression results " ///
								"from our dynamic panel specification with household fixed effects, round dummies, and region-time trends " ///
								"(see Equation \ref{eq:dynint}). FI stands for Fractional Index while FS stands for our standardized index of " ///
								"food insecurity. Standard errors, clustered at the household, are reported in parentheses " ///
								"(*** p$<$0.001, ** p$<$0.01, * p$<$0.05). Results correspond with coefficients presented in Figure~\ref{fig:inter_fs}.}  \end{tabular}")
	
	
* **********************************************************************
**## education index 1 
* **********************************************************************		

	foreach 				ind in std_pp_index {
	if 						"`ind'" == "std_pp_index" {
		local 				t = "Fractional Index"
	}
	
	foreach 				c in 1 2 3 4 {
		* balance panel with lagged variables 
		preserve
		keep 					if country == `c'
		drop 					if edu_act == . // can never use obs without dependent var
		egen 					temp = total(inrange(wave_orig, 0, 11)), by(hhid)
		drop 					if temp < 7 & country == 1 // 5,561 of 11,854 dropped (47%)
		drop 					if temp < 5 & country == 2 // 1,349 of 6,039 dropped (22%)
		drop 					if temp < 9 & country == 3 // 3,772 of 12,997 dropped (29%)
		drop 					if temp < 7 & country == 4 // 1,704 of 10,380 dropped (16%)
		sort 					hhid wave_, stable 
		bysort 					hhid (wave_): 	gen `ind'_lag = `ind'[_n-1]
		bysort					hhid (wave_): 	gen edu_act_lag_`c' = edu_act[_n-1] if country == `c'
		egen 					wave_temp =  group(country wave_orig)
		* dynamic panel regression
		xtset 					hhid wave_temp
		xtreg 					edu_act c.stringency_index##i.edu_act_lag_`c'##c.`ind'_lag i.wave_temp i.region#c.wave_temp ///
									[aweight = weight], fe vce(cluster hhid)	
		eststo					`ind'_edu_inter_`c'
		restore
		}
	}
	
	* generate figure
	coefplot			std_pp_index_edu_inter_1 std_pp_index_edu_inter_2 std_pp_index_edu_inter_3 std_pp_index_edu_inter_4, ///
								keep(1.edu_act_lag_1#c.stringency_index#c.std_pp_index_lag ///
								1.edu_act_lag_2#c.stringency_index#c.std_pp_index_lag ///
								1.edu_act_lag_3#c.stringency_index#c.std_pp_index_lag ///
								1.edu_act_lag_4#c.stringency_index#c.std_pp_index_lag) ///
								xline(0, lcolor(maroon)) xlabel(-.06(.02).06, labs(small))  title("`t'") ///				
								coeflabels(1.edu_act_lag_1#c.stringency_index#c.std_pp_index_lag = "Ethiopia" ///
								1.edu_act_lag_2#c.stringency_index#c.std_pp_index_lag = "Malawi" ///
								1.edu_act_lag_3#c.stringency_index#c.std_pp_index_lag = "Nigeria" ///
								1.edu_act_lag_4#c.stringency_index#c.std_pp_index_lag = "Uganda") ///
								xtitle("Effect on Educational Engagement") levels(95) ///
								legend(off) name(inter_edu_index1, replace)
								
		graph export			"$export/figures/inter_edu_index1.pdf", as(pdf) replace
		
		
	foreach 				ind in std_pp_index {
	if 						"`ind'" == "std_pp_index" {
		local 				t = "Fractional Index"
	}

	foreach 				c in 1 2 3 4 {
		* balance panel with lagged variables 
		preserve
		keep 					if country == `c'
		drop 					if edu_act == . // can never use obs without dependent var
		egen 					temp = total(inrange(wave_orig, 0, 11)), by(hhid)
		drop 					if temp < 7 & country == 1 // 5,561 of 11,854 dropped (47%)
		drop 					if temp < 5 & country == 2 // 1,349 of 6,039 dropped (22%)
		drop 					if temp < 9 & country == 3 // 3,772 of 12,997 dropped (29%)
		drop 					if temp < 7 & country == 4 // 1,704 of 10,380 dropped (16%)
		sort 					hhid wave_, stable 
		bysort 					hhid (wave_): 	gen `ind'_lag = `ind'[_n-1]
		bysort					hhid (wave_): 	gen edu_act_lag = edu_act[_n-1]
		egen 					wave_temp =  group(country wave_orig)
		* dynamic panel regression
		xtset 					hhid wave_temp
		xtreg 					edu_act c.stringency_index##i.edu_act_lag##c.`ind'_lag i.wave_temp i.region#c.wave_temp ///
									[aweight = weight], fe vce(cluster hhid)	
		eststo					`ind'_edu_inter_`c'
		restore
		}
	}
	
	* generate variables for labels 
	gen 					std_pp_index_lag = .
	lab var 				std_pp_index_lag "Lagged Index 1"
	gen 					edu_act_lag = .
	lab var 				edu_act_lag "Lagged Education"
	
	* generate table
	esttab 					std_pp_index_edu_inter_1 std_pp_index_edu_inter_2 std_pp_index_edu_inter_3 std_pp_index_edu_inter_4 ///
								using "$export/tables/inter_edu.tex", b(3) se(3) replace ///
								prehead("\begin{tabular}{l*{4}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] " ///
								"& \multicolumn{1}{c}{Ethiopia} & \multicolumn{1}{c}{Malawi} & \multicolumn{1}{c}{Nigeria} & " ///
								"\multicolumn{1}{c}{Uganda} \\ ") drop(*wave* _cons) noobs ///
								booktabs nonum nomtitle collabels(none) nobaselevels ////
								stat(N, labels("Observations") fmt(%9.0fc)) nogaps ///
								fragment label postfoot("\hline \hline \\[-1.8ex] " ///
								"\multicolumn{5}{J{\linewidth}}{\small \noindent \textit{Note}:  The table displays regression results " ///
								"from our dynamic panel specification with household fixed effects, round dummies, and region-time trends " ///
								"(see Equation \ref{eq:dynint}). FI stands for Fractional Index. Standard errors, clustered at the household, " ///
								"are reported in parentheses (*** p$<$0.001, ** p$<$0.01, * p$<$0.05). Results correspond with coefficients presented in Figure~\ref{fig:inter_edu}.} \end{tabular}") 

	drop 					*_lag
		

* **********************************************************************
**# appendix: dynamic panel model (index 2)
* **********************************************************************

* **********************************************************************
**## food security index 2 
* **********************************************************************		
/*
	foreach 				ind in std_pp_index pp_index {
	if 						"`ind'" == "std_pp_index" {
		local 				t = "Index 1"
	}
	else 				if "`ind'" == "pp_index" {
		local 				t = "Index 2"
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
			sort 					hhid wave_, stable 
			bysort 					hhid (wave_orig): gen `ind'_lag = `ind'[_n-1]
			bysort 					hhid (wave_orig): gen `fs'_fs_lag = `fs'_fs[_n-1]
			egen 					wave_temp =  group(country wave_orig)
			egen 					max = max(wave_temp), by(hhid)
			drop 					if max == 4 & country == 1 // drops 8
			* dynamic panel regression
			xtset 					hhid wave_temp 
			xtreg 					`fs'_fs c.`fs'_fs_lag##c.`ind'_lag i.wave_temp i.region#c.wave_temp ///
										[aweight = weight], fe vce(cluster hhid)
			eststo					`ind'_`fs'_dyn_`c'
			restore
		}
	}
	* generate graphics 
	coefplot				`ind'_mild_dyn_1 `ind'_mod_dyn_1 `ind'_sev_dyn_1 `ind'_std_dyn_1, ///
								drop(*.wave_temp *.country std_pp_index_lag mild_fs_lag mod_fs_lag ///
								sev_fs_lag std_fs_lag pp_index_lag  _cons) xline(0, lcolor(maroon)) ///
								xtitle(" ", size(small)) title("Ethiopia `t'") ///
								levels(95) coeflabels(c.* = " ", notick) xlabel(-1(.2)1, labs(small)) ///
								legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index")) name(`ind'_fs_dyn_eth, replace)
								
	coefplot				`ind'_mild_dyn_2 `ind'_mod_dyn_2 `ind'_sev_dyn_2 `ind'_std_dyn_2, ///
								drop(*.wave_temp *.country std_pp_index_lag mild_fs_lag mod_fs_lag ///
								sev_fs_lag std_fs_lag pp_index_lag _cons) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Malawi `t'") ///
								levels(95) coeflabels(c.* = " ", notick) xlabel(-1(.2)1, labs(small)) ///
								legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index")) name(`ind'_fs_dyn_mwi, replace)
}	

	grc1leg2  				std_pp_index_fs_dyn_eth pp_index_fs_dyn_eth std_pp_index_fs_dyn_mwi pp_index_fs_dyn_mwi, ///
								col(2) commonscheme
	graph export			"$export/figures/reg_results/dyn_fs_index1_2.png", as(png) replace
*/


* **********************************************************************
**## education index 2 
* **********************************************************************		
/*
	foreach 				ind in std_pp_index pp_index {
	if 						"`ind'" == "std_pp_index" {
		local 				t = "Index 1"
	}
	else 				if "`ind'" == "pp_index" {
		local 				t = "Index 2"
	}
	foreach 				c in 1 2 3 4 {
		* balance panel with lagged variables 
		preserve
		keep 					if country == `c'
		drop 					if edu_act == . // can never use obs without dependent var
		egen 					temp = total(inrange(wave_orig, 0, 11)), by(hhid)
		drop 					if temp < 7 & country == 1 // 5,561 of 11,854 dropped (47%)
		drop 					if temp < 5 & country == 2 // 1,349 of 6,039 dropped (22%)
		drop 					if temp < 9 & country == 3 // 3,772 of 12,997 dropped (29%)
		drop 					if temp < 6 & country == 4 // 1,704 of 10,380 dropped (16%)
		sort 					hhid wave_, stable 
		bysort 					hhid (wave_): 	gen `ind'_lag = `ind'[_n-1]
		bysort					hhid (wave_): 	gen edu_act_lag = edu_act[_n-1]
		egen 					wave_temp =  group(country wave_orig)
		* dynamic panel regression
		xtset 					hhid wave_temp
		xtreg 					edu_act i.edu_act_lag##c.`ind'_lag i.wave_temp i.region#c.wave_temp ///
									[aweight = weight], fe vce(cluster hhid)	
		eststo					`ind'_edu_dyn_`c'
		restore
		}
	}
	
	* generate variables for labels 
	gen 					std_pp_index_lag = .
	lab var 				std_pp_index_lag "Lagged Index 1"
	gen 					pp_index_lag = . 
	lab var 				pp_index_lag "Lagged Index 2"
	gen 					edu_act_lag = .
	lab var 				edu_act_lag "Lagged Education"
	
	* generate table
	esttab 					std_pp_index_edu_dyn_1 std_pp_index_edu_dyn_2 std_pp_index_edu_dyn_3 std_pp_index_edu_dyn_4 ///
								using "$export/tables/dyn_edu.tex", b(3) se(3) replace ///
								prehead("\begin{tabular}{l*{4}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] & " ///
								"\multicolumn{4}{c}{\textbf{Index 1}} \\ " ///
								"& \multicolumn{1}{c}{Ethiopia} & \multicolumn{1}{c}{Malawi} & \multicolumn{1}{c}{Nigeria} & " ///
								"\multicolumn{1}{c}{Uganda} \\ ") drop(*wave* _cons) noobs ///
								booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
								fragment label postfoot("\hline \\[-1.8ex]") 	
	
	esttab 					pp_index_edu_dyn_1 pp_index_edu_dyn_2 pp_index_edu_dyn_3 pp_index_edu_dyn_4 ///
								using "$export/tables/dyn_edu.tex", b(3) se(3) append ///
								prehead("& \multicolumn{4}{c}{\textbf{Index 2}} \\ ") drop(*wave* _cons) ///
								booktabs nonum nomtitle collabels(none) nobaselevels ///
								stat(N, labels("Observations") fmt(%9.0fc)) nogaps ///
								fragment label postfoot("\hline \hline \\[-1.8ex] " ///
								"\multicolumn{5}{J{13.5cm}}{\small \noindent \textit{Note}: The table displays regression results from our dynamic panel empirical specification with region and round controls and standard errors clustered at the household level (see Equation \ref{eq:dyn}). Columns represent the four countries of interest and we display results for Indices 1 and 2. Cells report coefficients and standard errors are reported in parentheses (*** p$<$0.001, ** p$<$0.01, * p$<$0.05).}  \end{tabular}") 
	
	drop 					*_lag
*/
	
* **********************************************************************
**# appendix: dynamic panel model and stringency interactions (index 2)
* **********************************************************************

*	graph 					drop _all
*	eststo 					clear

* **********************************************************************
**## food security index 2
* **********************************************************************		
/*
	foreach 				c in 1 2 {
		if 						`c' == 1 {
			local 					country = "Ethiopia"
		}
		else 					if 	`c' == 2 {
			local 					country = "Malawi"
		}
		foreach 				ind in std_pp_index pp_index {
		if 						"`ind'" == "std_pp_index" {
			local 				t = "Index 1"
		}
		else 				if "`ind'" == "pp_index" {
			local 				t = "Index 2"
		}
			foreach 				fs in mild mod sev std {
				* balance panel with lagged variables 
				preserve
				keep 					if country == `c'
				drop 					if `fs'_fs == . // can never use obs without dependent var
				egen 					temp = total(inrange(wave_orig, 0, 11)), by(hhid)
				drop 					if temp < 6 & country == 1 // 3,317 of 17,759 dropped (19%)
			drop 					if temp < 10 & country == 2 // 2,812 of 16,102 dropped (17%)
				sort 					hhid wave_, stable 
				bysort hhid (wave_): 	gen `ind'_lag = `ind'[_n-1]
				drop 					if `ind'_lag == . // can never use obs without lagged index
				bysort hhid (wave_): 	gen `fs'_fs_lag = `fs'_fs[_n-1]
				egen 					wave_temp =  group(country wave_orig)
				* dynamic panel regression with interactions
				xtset 					hhid wave_temp
				xtreg 					`fs'_fs c.stringency_index##c.`fs'_fs_lag##c.`ind'_lag i.wave_temp ///
											i.region#c.wave_temp [aweight = weight] if country == `c', fe vce(cluster hhid)
				eststo					`ind'_`fs'_inter_`c' 
				restore
			}
				coefplot				`ind'_mild_inter_`c' `ind'_mod_inter_`c' `ind'_sev_inter_`c' `ind'_std_inter_`c', ///
											drop(stringency_index `ind'_lag stringency_index#c.`ind'_lag ///
											c.*_fs_lag#c.`ind'_lag  *_fs_lag *.wave_temp _cons) /// 
											xline(0, lcolor(maroon)) coeflabels(* = " ", notick) ///
											xtitle(" ", size(small)) title("`country' `t'") ///
											levels(95)  xlabel(-1(.2)1, labs(small)) ///
											legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
											label(6 "Severe") label(8 "Index")) name(`ind'_fs_inter_`c', replace)	
		}	
	}
	
	* combine & export graphics 
	grc1leg2  				std_pp_index_fs_inter_1 pp_index_fs_inter_1 std_pp_index_fs_inter_2 pp_index_fs_inter_2, ///
								col(2) commonscheme
	graph export			"$export/figures/reg_results/inter_fs_index1_2.png", as(png) replace
*/

* **********************************************************************
**## education index 2
* **********************************************************************		
/*
	foreach 				ind in std_pp_index pp_index {
	if 						"`ind'" == "std_pp_index" {
		local 				t = "Index 1"
	}
	else 				if "`ind'" == "pp_index" {
		local 				t = "Index 2"
	}
	foreach 				c in 1 2 3 4 {
		* balance panel with lagged variables 
		preserve
		keep 					if country == `c'
		drop 					if edu_act == . // can never use obs without dependent var
		egen 					temp = total(inrange(wave_orig, 0, 11)), by(hhid)
		drop 					if temp < 7 & country == 1 // 5,561 of 11,854 dropped (47%)
		drop 					if temp < 5 & country == 2 // 1,349 of 6,039 dropped (22%)
		drop 					if temp < 9 & country == 3 // 3,772 of 12,997 dropped (29%)
		drop 					if temp < 6 & country == 4 // 1,704 of 10,380 dropped (16%)
		sort 					hhid wave_, stable 
		bysort 					hhid (wave_): 	gen `ind'_lag = `ind'[_n-1]
		bysort					hhid (wave_): 	gen edu_act_lag = edu_act[_n-1]
		egen 					wave_temp =  group(country wave_orig)
		* dynamic panel regression
		xtset 					hhid wave_temp
		xtreg 					edu_act c.stringency_index##i.edu_act_lag##c.`ind'_lag i.wave_temp i.region#c.wave_temp ///
									[aweight = weight], fe vce(cluster hhid)	
		eststo					`ind'_edu_inter_`c'
		restore
		}
	}
	
	* generate variables for labels 
	gen 					std_pp_index_lag = .
	lab var 				std_pp_index_lag "Lagged Index 1"
	gen 					pp_index_lag = . 
	lab var 				pp_index_lag "Lagged Index 2"
	gen 					edu_act_lag = .
	lab var 				edu_act_lag "Lagged Education"
	
	* generate table
	esttab 					std_pp_index_edu_inter_1 std_pp_index_edu_inter_2 std_pp_index_edu_inter_3 std_pp_index_edu_inter_4 ///
								using "$export/tables/inter_edu.tex", b(3) se(3) replace ///
								prehead("\begin{tabular}{l*{4}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] & " ///
								"\multicolumn{4}{c}{\textbf{Index 1}} \\ " ///
								"& \multicolumn{1}{c}{Ethiopia} & \multicolumn{1}{c}{Malawi} & \multicolumn{1}{c}{Nigeria} & " ///
								"\multicolumn{1}{c}{Uganda} \\ ") drop(*wave* _cons) noobs ///
								booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
								fragment label postfoot("\hline \\[-1.8ex]") 	
	
	esttab 					pp_index_edu_inter_1 pp_index_edu_inter_2 pp_index_edu_inter_3 pp_index_edu_inter_4 ///
								using "$export/tables/inter_edu.tex", b(3) se(3) append ///
								prehead("& \multicolumn{4}{c}{\textbf{Index 2}} \\ ") drop(*wave* _cons) ///
								booktabs nonum nomtitle collabels(none) nobaselevels ///
								stat(N, labels("Observations") fmt(%9.0fc)) nogaps ///
								fragment label postfoot("\hline \hline \\[-1.8ex] " ///
								"\multicolumn{5}{J{\linewidth}}{\small \noindent \textit{Note}: The table displays regression results from our dynamic panel empirical specification with stringency score interactions (see Equation \ref{eq:dynint}). For these regressions, we include region and round controls and standard errors are clustered at the household level. Columns represent our four countries of interest and we display results for Indices 1 and 2. Cells report coefficients and standard errors are reported in parentheses (*** p$<$0.001, ** p$<$0.01, * p$<$0.05).} \end{tabular}") 
	
	drop 					*_lag
*/

* **********************************************************************
**# ANOCOVA & DID (index 4) 
* **********************************************************************

graph 					drop _all
eststo 					clear


* **********************************************************************
**## food security index 4
* **********************************************************************	

* DID & ANCOVA regressions	
	preserve 
	drop 						if wave == -1
	foreach 					c in 1 2 3 {
		if 						`c' == 1 {
			local 					country = "Ethiopia"
			gen						std_pre_index_hhi_`c' = std_pre_index_hhi
		}
		else 					if 	`c' == 2 {
			local 					country = "Malawi"
			gen						std_pre_index_hhi_`c' = std_pre_index_hhi
		}
		else 					if `c' == 3 {
			local 					country = "Nigeria"
			gen						std_pre_index_hhi_`c' = std_pre_index_hhi
		}
			* ANCOVA	
			foreach 					f in mild mod sev std {
				reg 						`f'_fs std_pre_index_hhi_`c' y0_`f' ib(1).wave c.wave#i.region ///
												[aweight = weight] if wave != 0 & country == `c', vce(cluster hhid) 
				eststo						`f'_an_std_pre_index_hhi_`c'
			}
	/*		* DID
			foreach 				f in mild mod sev std {
				reg 					`f'_fs c.std_pre_index_hhi_`c'##i.post ib(1).wave c.wave#i.region ///
											[aweight = weight] if country == `c', vce(cluster hhid) 	
				eststo					`f'_dd_`ind'`c' 
		} 
		*/

		} 
	restore
	
		* generate graphics 
	coefplot				mild_an_std_pre_index_hhi_1 mod_an_std_pre_index_hhi_1 sev_an_std_pre_index_hhi_1 std_an_std_pre_index_hhi_1, ///
								drop(*.wave_temp *.country std_pp_index_lag mild_fs_lag mod_fs_lag ///
								sev_fs_lag std_fs_lag _cons) xline(0, lcolor(maroon)) ///
								xtitle("Effect on Food Insecurity", size(small)) title("Ethiopia") ///
								levels(95) coeflabels(c.* = " ", notick) xlabel(-1(.2)1, labs(small)) ///
								legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
								label(6 "Severe") label(8 "Index")) name(std_pre_index_hhi_fs_dyn_eth, replace)
	
	* graphics generation
			* coefplot
				coefplot			mild_an_std_pre_index_hhi_`c' mod_an_std_pre_index_hhi_`c' sev_an_std_pre_index_hhi_`c' std_an_std_pre_index_hhi_`c', ///
										drop(*.wave y0* _cons *post) xline(0, lcolor(maroon)) ///
										xtitle("Effect on Food Insecurity", size(small)) title("`country'") ///
										levels(95) coeflabels(std_pre_index_hhi_`c' = " ", notick) xlabel(-0.5(.1)0.5, labs(small)) ///
										legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
										label(6 "Severe") label(8 "Index")) name(anc_std_pre_index_hhi_`c', replace)
				
			/*	coefplot			mild_dd_`ind'`c' mod_dd_`ind'`c' sev_dd_`ind'`c' std_dd_`ind'`c', ///
										drop(*.wave y0* _cons *post `ind') ///
										xline(0, lcolor(maroon)) xlabel(-1(.2)1, labs(small))  ///
										xtitle("Effect on Food Insecurity") title("`country'") ///
										levels(95) coeflabels(1.post#c.`ind' = " ", notick) ///
										legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
										label(6 "Severe") label(8 "Index")) name(did_`ind'_`c', replace)	*/	
										
	* combined graphics 
	foreach 						r in anc did {
		grc1leg2  						`r'_std_pre_index_frac_1 `r'_std_pre_index_frac_2 `r'_std_pre_index_frac_3, ///
											col(2) commonscheme title("Index 3") name(`r'_std_frac)
		graph export					"$export/figures/reg_results/`r'_index3.png", as(png) replace
		
		grc1leg2  						`r'_std_pre_index_hhi_1 `r'_std_pre_index_hhi_2 `r'_std_pre_index_hhi_3, ///
											col(2) commonscheme title("HHI") name(`r'_std_hhi)
		graph export					"$export/figures/reg_results/`r'_index4.png", as(png) replace
		
		grc1leg2  						`r'_pre_index_frac_1 `r'_pre_index_frac_2 `r'_pre_index_frac_3, ///
											col(2) commonscheme title("Index 5") name(`r'_pre_frac)
		graph export					"$export/figures/reg_results/`r'_index5.png", as(png) replace

		grc1leg2  						`r'_pre_index_hhi_1 `r'_pre_index_hhi_2 `r'_pre_index_hhi_3, ///
											col(2) commonscheme title("Index 6") name(`r'_pre_hhi)
		graph export					"$export/figures/reg_results/`r'_index6.png", as(png) replace
	}
	restore
	
* **********************************************************************
**## education index 4
* **********************************************************************	

* DID & ANCOVA regressions	
	foreach 				c in 1 2 3 4 {
		if 						`c' == 1 {
			local 					country = "Ethiopia"
			local 					xt = " "
			gen						std_pre_index_hhi_`c' = std_pre_index_hhi
		}
		else 					if 	`c' == 2 {
			local 					country = "Malawi"
			local 					xt = " "
			gen						std_pre_index_hhi_`c' = std_pre_index_hhi
		}
		else 					if `c' == 3 {
			local 					country = "Nigeria"
			local 					xt = "Effect on Educational Engagement"
			gen						std_pre_index_hhi_`c' = std_pre_index_hhi
		}
		else 					if `c' == 4 {
			local 					country = "Uganda"
			local 					xt = "Effect on Educational Engagement"
			gen						std_pre_index_hhi_`c' = std_pre_index_hhi
		}		
		
		* regressions 
			* ANCOVA	
				reg 				edu_act std_pre_index_hhi_`c' y0_edu_act ib(1).wave c.wave#i.region ///
										[aweight = weight] if wave != 0 & country == `c', vce(cluster hhid) 
				eststo				edu_anc_std_pre_index_hhi`c'
	/*		* DID
				reg 				edu_act c.std_pre_index_hhi##i.post ib(1).wave c.wave#i.region ///
										[aweight = weight] if country == `c', vce(cluster hhid) 	
				eststo				edu_did_std_pre_index_hhi`c'	

	
		* plot coefficients by index for each country
		* DID
		coefplot			edu_did_std_pre_index_hhi`c' ///
								drop(*.wave _cons std_pre_index) ///
								xline(0, lcolor(maroon)) xlabel(-1(.2)1, labs(med)) ///
								xtitle("`xt'") title("`country'") ///
								levels(95) coeflabels( ///
								1.post#c.std_pre_index_hhi = "HHI", notick) xlabel(-1(.2)1, labs(small))  ///
								legend(off) name(edu_did_`c', replace)
		* ANCOVA 
		coefplot			edu_anc_std_pre_index_hhi`c' ///
								drop(*.wave y0* _cons) ///
								xline(0, lcolor(maroon)) xlabel(-1(.2)1, labs(med)) ///
								xtitle("`xt'") title("`country'") ///
								levels(95) coeflabels(std_pre_index_hhi = "HHI", notick) xlabel(-1(.2)1, labs(small))  ///
								legend(off) name(edu_anc_`c', replace) 
								*/
	}

		coefplot			edu_anc_std_pre_index_hhi1 edu_anc_std_pre_index_hhi2 edu_anc_std_pre_index_hhi3 edu_anc_std_pre_index_hhi4, ///
								drop(*.wave y0_edu_act _cons) xline(0, lcolor(maroon)) xlabel(-.2(.05).2, labs(small))  ///
								xtitle("Effect on Educational Engagement") title("HHI") levels(95) ///
								coeflabels(std_pre_index_hhi_1 = "Ethiopia" std_pre_index_hhi_2 = "Malawi" ///
								std_pre_index_hhi_3 = "Nigeria" std_pre_index_hhi_4 = "Uganda") ///
								legend(off) name(edu_anc_cty, replace)

		graph export			"$export/figures/edu_anc.pdf", as(pdf) replace
	

* **********************************************************************
**# appendix: ANOCOVA & DID (indices 3, 5, 6)
* **********************************************************************


* **********************************************************************
**## food security index 3, 5, 6
* **********************************************************************	

	* regressions
	preserve 
	drop 						if wave == -1
	foreach 					c in 1 2 3 {
		if 						`c' == 1 {
			local 					country = "Ethiopia"
		}
		else 					if 	`c' == 2 {
			local 					country = "Malawi"
		}
		else 					if `c' == 3 {
			local 					country = "Nigeria"
		}
		foreach 					ind in std_pre_index_frac pre_index_frac pre_index_hhi {	
			* ANCOVA	
			foreach 					f in mild mod sev std {
				reg 						`f'_fs `ind' y0_`f' ib(1).wave c.wave#i.region ///
												[aweight = weight] if wave != 0 & country == `c', vce(cluster hhid) 
				eststo						`f'_an_`ind'`c'
			}
			* DID
			foreach 				f in mild mod sev std {
				reg 					`f'_fs c.`ind'##i.post ib(1).wave c.wave#i.region ///
											[aweight = weight] if country == `c', vce(cluster hhid) 	
				eststo					`f'_dd_`ind'`c'
			}	
		}
		
		* graphics generation
		foreach 					ind in std_pre_index_frac pre_index_frac pre_index_hhi {
			* coefplot
				coefplot			mild_an_`ind'`c' mod_an_`ind'`c' sev_an_`ind'`c' std_an_`ind'`c', ///
										drop(*.wave y0* _cons *post) xline(0, lcolor(maroon)) ///
										xtitle("Effect on Food Insecurity", size(small)) title("`country'") ///
										levels(95) coeflabels(`ind' = " ", notick) xlabel(-0.5(.1)0.5, labs(small)) ///
										legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
										label(6 "Severe") label(8 "Index")) name(anc_`ind'_`c', replace)
				
				coefplot			mild_dd_`ind'`c' mod_dd_`ind'`c' sev_dd_`ind'`c' std_dd_`ind'`c', ///
										drop(*.wave y0* _cons *post `ind') ///
										xline(0, lcolor(maroon)) xlabel(-1(.2)1, labs(small))  ///
										xtitle("Effect on Food Insecurity") title("`country'") ///
										levels(95) coeflabels(1.post#c.`ind' = " ", notick) ///
										legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
										label(6 "Severe") label(8 "Index")) name(did_`ind'_`c', replace)		
		} 
		
	}
	
	* combined graphics 
	foreach 						r in anc did {
		grc1leg2  						`r'_std_pre_index_frac_1 `r'_std_pre_index_frac_2 `r'_std_pre_index_frac_3, ///
											col(2) commonscheme title("Index 3") name(`r'_std_frac)
		graph export					"$export/figures/reg_results/`r'_index3.png", as(png) replace
		
		grc1leg2  						`r'_std_pre_index_hhi_1 `r'_std_pre_index_hhi_2 `r'_std_pre_index_hhi_3, ///
											col(2) commonscheme title("HHI") name(`r'_std_hhi)
		graph export					"$export/figures/reg_results/`r'_index4.png", as(png) replace
		
		grc1leg2  						`r'_pre_index_frac_1 `r'_pre_index_frac_2 `r'_pre_index_frac_3, ///
											col(2) commonscheme title("Index 5") name(`r'_pre_frac)
		graph export					"$export/figures/reg_results/`r'_index5.png", as(png) replace

		grc1leg2  						`r'_pre_index_hhi_1 `r'_pre_index_hhi_2 `r'_pre_index_hhi_3, ///
											col(2) commonscheme title("Index 6") name(`r'_pre_hhi)
		graph export					"$export/figures/reg_results/`r'_index6.png", as(png) replace
	}


* **********************************************************************
**## education index 3, 5, 6
* **********************************************************************	
	
* DID & ANCOVA regressions - all indexes
	foreach 				c in 1 2 3 4 {
		if 						`c' == 1 {
			local 					country = "Ethiopia"
			local 					xt = " "
		}
		else 					if 	`c' == 2 {
			local 					country = "Malawi"
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
		foreach 				ind in std_pre_index_frac pre_index_frac pre_index_hhi {	
			* ANCOVA	
				reg 				edu_act `ind' y0_edu_act ib(1).wave c.wave#i.region ///
										[aweight = weight] if wave != 0 & country == `c', vce(cluster hhid) 
				eststo				edu_anc_`ind'`c'
			* DID
				reg 				edu_act c.`ind'##i.post ib(1).wave c.wave#i.region ///
										[aweight = weight] if country == `c', vce(cluster hhid) 	
				eststo				edu_did_`ind'`c'	
		}
	
		* plot coefficients by index for each country
		* DID
		coefplot			edu_did_std_pre_index_frac`c' edu_did_std_pre_index_hhi`c' ///
								edu_did_pre_index_frac`c' edu_did_pre_index_hhi`c', ///
								drop(*.wave _cons *post std_pre_index* pre_index*) ///
								xline(0, lcolor(maroon)) xlabel(-1(.2)1, labs(med)) ///
								xtitle("`xt'") title("`country'") ///
								levels(95) coeflabels(1.post#c.std_pre_index_frac = "Index 3" ///
								1.post#c.std_pre_index_hhi = "HHI" 1.post#c.pre_index_frac = "Index 5" ///
								1.post#c.pre_index_hhi = "Index 6", notick) xlabel(-1(.2)1, labs(small))  ///
								legend(off) name(edu_did_`c', replace)
		* ANCOVA
		coefplot			edu_anc_std_pre_index_frac`c' edu_anc_std_pre_index_hhi`c' ///
								edu_anc_pre_index_frac`c' edu_anc_pre_index_hhi`c', ///
								drop(*.wave y0* _cons) ///
								xline(0, lcolor(maroon)) xlabel(-1(.2)1, labs(med)) ///
								xtitle("`xt'") title("`country'") ///
								levels(95) coeflabels(std_pre_index_frac = "Index 3" ///
								std_pre_index_hhi = "HHI" pre_index_frac = "Index 5" ///
								pre_index_hhi = "Index 6", notick) xlabel(-1(.2)1, labs(small))  ///
								legend(off) name(edu_anc_`c', replace)
	}
	
	
	* graph export - all 
	gr combine 				edu_did_1 edu_did_2 edu_did_3 edu_did_4, commonscheme
	graph export			"$export/figures/reg_results/edu_did.png", as(png) replace
	
	gr combine 				edu_anc_1 edu_anc_2 edu_anc_3 edu_anc_4, commonscheme
	graph export			"$export/figures/reg_results/edu_anc.png", as(png) replace

	
	restore 
	

* **********************************************************************
**# ANOCOVA & DID (indices 3-6) - heterogeneous effects (robust - app)
* **********************************************************************

graph 					drop _all
eststo 					clear	
* food security
	* regressions
	preserve 
	rename 						std_pre_index_frac std_pre_frac // names were too long for stored results
	rename 						std_pre_index_hhi std_pre_hhi
	rename 						pre_index_frac pre_frac
	rename 						pre_index_hhi pre_hhi
	drop 						if wave == -1
	foreach 					c in 1 2 3 {
		if 						`c' == 1 {
			local 					country = "Ethiopia"
		}
		else 					if 	`c' == 2 {
			local 					country = "Malawi"
		}
		else 					if `c' == 3 {
			local 					country = "Nigeria"
		}
		foreach 					ind in std_pre_frac std_pre_hhi pre_frac pre_hhi {	
			foreach 					het in sexhh sector {
				* ANCOVA	
				foreach 					f in mild mod sev std {
					reg 						`f'_fs c.`ind'##i.`het' y0_`f' ib(1).wave c.wave#i.region ///
													[aweight = weight] if wave != 0 & country == `c', vce(cluster hhid) 
					eststo						`f'_a_`ind'_`het'`c'
				}
				* DID
				foreach 				f in mild mod sev std {
					reg 					`f'_fs c.`ind'##i.post##i.`het' ib(1).wave c.wave#i.region ///
												[aweight = weight] if country == `c', vce(cluster hhid) 	
					eststo					`f'_d_`ind'_`het'`c'
				}	
			}
		}
		
		* graphics generation - sexhh 
		foreach 					ind in std_pre_frac pre_frac std_pre_hhi pre_hhi {
			* coefplot
				* ancova
				coefplot			mild_a_`ind'_sexhh`c' mod_a_`ind'_sexhh`c' sev_a_`ind'_sexhh`c' std_a_`ind'_sexhh`c', ///
										drop(*.wave y0* _cons *post 2.sexhh `ind') xline(0, lcolor(maroon)) ///
										xtitle("Effect on Food Insecurity", size(small)) title("`country'") ///
										levels(95) coeflabels(2.sexhh#c.`ind' = " ", notick) xlabel(-1(.2)1, labs(small)) ///
										legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
										label(6 "Severe") label(8 "Index")) name(anc_`ind'_sexhh_`c', replace)
				* did
				coefplot			mild_d_`ind'_sexhh`c' mod_d_`ind'_sexhh`c' sev_d_`ind'_sexhh`c' std_d_`ind'_sexhh`c', ///
										drop(*.wave y0* _cons *post 2.sexhh* 1.post#2.sexhh `ind' 1.post#c.`ind') ///
										xline(0, lcolor(maroon)) xlabel(-1(.2)1, labs(small))  ///
										xtitle("Effect on Food Insecurity") title("`country'") ///
										levels(95) coeflabels(1.post#2.sexhh#c.`ind' = " ", notick) ///
										legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
										label(6 "Severe") label(8 "Index")) name(did_`ind'_sexhh_`c', replace)		
		} 
	
		* graphics generation - sector 
		foreach 					ind in std_pre_frac pre_frac std_pre_hhi pre_hhi {
			* coefplot
				* ancova
				coefplot			mild_a_`ind'_sector`c' mod_a_`ind'_sector`c' sev_a_`ind'_sector`c' std_a_`ind'_sector`c', ///
										drop(*.wave y0* _cons *post 2.sector `ind') xline(0, lcolor(maroon)) ///
										xtitle("Effect on Food Insecurity", size(small)) title("`country'") ///
										levels(95) coeflabels(2.sector#c.`ind' = " ", notick) xlabel(-1(.2)1, labs(small)) ///
										legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
										label(6 "Severe") label(8 "Index")) name(anc_`ind'_sector_`c', replace)
				* did
				coefplot			mild_d_`ind'_sector`c' mod_d_`ind'_sector`c' sev_d_`ind'_sector`c' std_d_`ind'_sector`c', ///
										drop(*.wave y0* _cons *post 2.sector* 1.post#2.sector `ind' 1.post#c.`ind') ///
										xline(0, lcolor(maroon)) xlabel(-1(.2)1, labs(small))  ///
										xtitle("Effect on Food Insecurity") title("`country'") ///
										levels(95) coeflabels(1.post#2.sector#c.`ind' = " ", notick) ///
										legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
										label(6 "Severe") label(8 "Index")) name(did_`ind'_sector_`c', replace)		
		} 
	}
	
	* combine graphics 
	* sexhh
		foreach 						r in anc did {
			grc1leg2  						`r'_std_pre_frac_sexhh_1 `r'_std_pre_frac_sexhh_2 `r'_std_pre_frac_sexhh_3, ///
												col(2) commonscheme title("Index 3") name(`r'_std_frac_sex)
			graph export					"$export/figures/reg_results/`r'_sexhh_index3.png", as(png) replace
			
			grc1leg2  						`r'_std_pre_hhi_sexhh_1 `r'_std_pre_hhi_sexhh_2 `r'_std_pre_hhi_sexhh_3, ///
												col(2) commonscheme title("Index 4") name(`r'_std_hhi_sex)
			graph export					"$export/figures/reg_results/`r'_sexhh_index4.png", as(png) replace
			
			grc1leg2  						`r'_pre_frac_sexhh_1 `r'_pre_frac_sexhh_2 `r'_pre_frac_sexhh_3, ///
												col(2) commonscheme title("Index 5") name(`r'_pre_frac_sex)
			graph export					"$export/figures/reg_results/`r'_sexhh_index5.png", as(png) replace

			grc1leg2  						`r'_pre_hhi_sexhh_1 `r'_pre_hhi_sexhh_2 `r'_pre_hhi_sexhh_3, ///
												col(2) commonscheme title("Index 6") name(`r'_pre_hhi_sex)
			graph export					"$export/figures/reg_results/`r'_sexhh_index6.png", as(png) replace
		}

	* sector 
		foreach 						r in anc did {
			grc1leg2  						`r'_std_pre_frac_sector_1 `r'_std_pre_frac_sector_2 `r'_std_pre_frac_sector_3, ///
												col(2) commonscheme title("Index 3") name(`r'_std_frac_sec)
			graph export					"$export/figures/reg_results/`r'_sector_index3.png", as(png) replace
			
			grc1leg2  						`r'_std_pre_hhi_sector_1 `r'_std_pre_hhi_sector_2 `r'_std_pre_hhi_sector_3, ///
												col(2) commonscheme title("Index 4") name(`r'_std_hhi_sec)
			graph export					"$export/figures/reg_results/`r'_sector_index4.png", as(png) replace
			
			grc1leg2  						`r'_pre_frac_sector_1 `r'_pre_frac_sector_2 `r'_pre_frac_sector_3, ///
												col(2) commonscheme title("Index 5") name(`r'_pre_frac_sec)
			graph export					"$export/figures/reg_results/`r'_sector_index5.png", as(png) replace

			grc1leg2  						`r'_pre_hhi_sector_1 `r'_pre_hhi_sector_2 `r'_pre_hhi_sector_3, ///
												col(2) commonscheme title("Index 6") name(`r'_pre_hhi_sec)
			graph export					"$export/figures/reg_results/`r'_sector_index6.png", as(png) replace
		}
	
* education
	* DID & ANCOVA regressions	
	foreach 				c in 1 2 3 4 {
		if 						`c' == 1 {
			local 					country = "Ethiopia"
			local 					xt = " "
		}
		else 					if 	`c' == 2 {
			local 					country = "Malawi"
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
		foreach 				ind in std_pre_frac std_pre_hhi pre_frac pre_hhi {	
			foreach 					het in sexhh sector {
				* ANCOVA	
					reg 				edu_act c.`ind'##i.`het' y0_edu_act ib(1).wave c.wave#i.region ///
											[aweight = weight] if wave != 0 & country == `c', vce(cluster hhid) 
					eststo				edu_a_`ind'_`het'`c'
				* DID
					reg 				edu_act c.`ind'##i.post##i.`het' ib(1).wave c.wave#i.region ///
											[aweight = weight] if country == `c', vce(cluster hhid) 	
					eststo				edu_d_`ind'_`het'`c'	
			}
		}
	
		* plot coefficients for each country
		foreach 			het in sexhh sector { 			
		* DID
		coefplot			edu_d_std_pre_frac_`het'`c' edu_d_std_pre_hhi_`het'`c' ///
								edu_d_pre_frac_`het'`c' edu_d_pre_hhi_`het'`c', ///
								drop(*.wave _cons *post std_pre* pre_* 2.`het'* 1.post#2.`het' 1.post#c.* ) ///
								xline(0, lcolor(maroon)) xlabel(-1(.2)1, labs(med)) ///
								xtitle("`xt'") title("`country'") ///
								levels(95) coeflabels(1.post#2.`het'#c.std_pre_frac = "Index 3" ///
								1.post#2.`het'#c.std_pre_hhi = "Index 4" ///
								1.post#2.`het'#c.pre_frac = "Index 5" ///
								1.post#2.`het'#c.pre_hhi = "Index 6", notick) ///
								xlabel(-1(.2)1, labs(small))  ///
								legend(off) name(edu_d_`het'_`c', replace)
		* ANCOVA
		coefplot			edu_a_std_pre_frac_`het'`c' edu_a_std_pre_hhi_`het'`c' ///
								edu_a_pre_frac_`het'`c' edu_a_pre_hhi_`het'`c', ///
								drop(*.wave y0* 2.`het' std_pre_frac std_pre_hhi pre_frac pre_hhi _cons) ///
								xline(0, lcolor(maroon)) xlabel(-1(.2)1, labs(med)) ///
								xtitle("`xt'") title("`country'") ///
								levels(95) coeflabels(2.`het'#c.std_pre_frac = "Index 3" ///
								2.`het'#c.std_pre_hhi = "Index 4" 2.`het'#c.pre_frac = "Index 5" ///
								2.`het'#c.pre_hhi = "Index 6", notick) xlabel(-1(.2)1, labs(small))  ///
								legend(off) name(edu_a_`het'_`c', replace)
		}
	}
	
		* graph export 
		gr combine 				edu_d_sexhh_1 edu_d_sexhh_2 edu_d_sexhh_3 edu_d_sexhh_4, commonscheme
		graph export			"$export/figures/reg_results/edu_did_sexhh.png", as(png) replace
		
		gr combine 				edu_a_sexhh_1 edu_a_sexhh_2 edu_a_sexhh_3 edu_a_sexhh_4, commonscheme
		graph export			"$export/figures/reg_results/edu_anc_sexhh.png", as(png) replace
		
		gr combine 				edu_d_sector_1 edu_d_sector_2 edu_d_sector_3 edu_d_sector_4, commonscheme
		graph export			"$export/figures/reg_results/edu_did_sector.png", as(png) replace
		
		gr combine 				edu_a_sector_1 edu_a_sector_2 edu_a_sector_3 edu_a_sector_4, commonscheme
		graph export			"$export/figures/reg_results/edu_anc_sector.png", as(png) replace
	
	
	restore 
	
	
/*
* **********************************************************************
**# exploratory analysis
* **********************************************************************								
	* baseline income DID
		preserve
		drop 						if wave_ <0
		xfill 						tot_inc_usd, i(hhid)
		
		pause on
		foreach 					c in 1 2 3 {
			foreach 					f in mild mod sev std {
				reg 						`f'_fs c.tot_inc_usd##i.post ib(1).wave_ c.wave_#i.region ///
													[aweight = weight] if country == `c', vce(cluster hhid) 	
				eststo							`f'_did_inc`c'
			}	
		
	
		coefplot					mild_did_inc`c' mod_did_inc`c' sev_did_inc`c' std_did_inc`c', ///
										drop(*.wave_orig y0* _cons *post `ind') ///
										xline(0, lcolor(maroon))  ///
										xtitle("Effect on Food Insecurity") title("`country'") ///
										levels(95) coeflabels(1.post#c.`ind' = " ", notick) ///
										legend(col(4) pos(3) label(2 "Mild") label(4 "Moderate") ///
										label(6 "Severe") label(8 "Index")) name(did_inc_`c', replace)		
		pause
		}	
		restore 
*/		
		
************************************************************************
**# end matters
************************************************************************	

* close log
	log 			close 
	
/* END */