* Project: diversification
* Created on: Dec 2021
* Created by: amf
* Edited by: jdm, amf, alj
* Last edited: September 1 2022
* Stata v.17.0

* does
	* generates summary statistics tables

* assumes
	* clean fies data
	* clean diversification indices data
	* coefplot

* TO DO:
	* inc_sum table: issues with multicolumn footer - eliminated for now 


* **********************************************************************
**# setup
* **********************************************************************

* define
	global	export	=			"$data/analysis/diversification"
	global	logout	=			"$data/analysis/logs"
	global  fies 	= 			"$data/analysis/food_security"

* open log
	cap log 					close
	log using					"$logout/ld_sum_stats", append

* local countries
	local 						countries = "1 2 3 4"	

* load panel data
	use 						"$data/analysis/diversification/ld_pnl", replace	
	
* clear memory 
	graph 						drop _all
	eststo 						clear
	
	
* **********************************************************************
**# tables
* **********************************************************************	
* Aggregate 
preserve
	gen 				geo = ea if country != 2
	replace 			geo = ta_code if country == 2
	
	local 				list1 = "crop_inc_ live_inc_ live_prod_ wage_emp_ casual_emp_ temp_emp_ nfe_inc_ kind_trans_ cash_trans_ food_trans_ asst_kind_ asst_cash_ asst_food_ pen_inc_ rent_inc_ asset_ save_inc_ oth_inc_"
	local 				list2  "crop_inc_ tree_inc_ live_inc_ live_prod_ wage_emp_ casual_emp_ nfe_inc_	cash_trans_ food_trans_ kind_trans_ cash_child_	kind_child_ asst_food_ asst_cash_ for_wrk_ masaf_ pen_inc_ rent_inc_ asset_	save_inc_ oth_inc_"
	local 				list3 = "crop_inc_ tree_inc_ live_inc_ live_prod_ wage_emp_ nfe_inc_ cash_dom_ cash_for_ kind_inc_ asst_inc_ pen_inc_ rent_nonag_ ag_rent_ save_inc_ oth_inc_"
	local 				list4 = "crop_inc_ live_inc_ live_prod_ wage_emp_ nfe_inc_ cash_dom_ kind_dom_ cash_for_ kind_for_ sage_ pen_inc_ rent_inc_ interest_ oth_inc_"
	

	estpost 			sum wave //need random stored variable for esttabs below to work, even though this is not referenced
	foreach 			c in `countries' {
	if 					`c' == 1 {
		local 				obs1 = "977"
		local 				obs2 = "2270"
		local 				end = " "
	}
	else 				if 	`c' == 2 {
		local 				country = "B: Malawi"
		local 				obs1 = "1092"
		local 				obs2 = "634"
		local 				end = " "
	}
	else 				if `c' == 3 {
		local 				country = "C: Nigeria"
		local 				obs1 = "1195"
		local 				obs2 = "755"
		local 				end = " "
	}
	else 				if `c' == 4 {
		local 				country = "D: Uganda"
		local 				obs1 = "1642"
		local 				obs2 = "583"
		local 				end = "   \end{longtable} "
		}
		
		*"\multicolumn{3}{r}{\small \textit{Note}: The table displays the percent of households engaged in and the mean income earned from each income category. In the LSMS data, income is reported in the local currency. To allow for cross-country comparisons, we convert income values to US dollars using 2019 exchange rates found at \href{https://exchangerates.org}{https://exchangerates.org}. We use LSMS data to generate the table, which covers the pre-COVID-19 period.} 

	
	if 					`c' == 1 {
		esttab 				using "$export/tables/inc_sum.tex", replace ///
								prehead("\begin{longtable}{l l l} " ///
								"\caption{2019 Engagement in and Earnings from Income Sources} \label{incsum} \\ [-1.8ex] \hline \hline & " ///
								"\multicolumn{1}{c}{\textbf{Percent Engaged}} & \multicolumn{1}{c}{\textbf{Mean Income (USD)}} \\  " ///
								"\endfirsthead " ///
								"\hline & \multicolumn{1}{c}{\textbf{Percent Engaged}} & \multicolumn{1}{c}{\textbf{Mean Income (USD)}} \\  " ///
								"\endhead " ///
								"\midrule \multicolumn{3}{c}{{Continued on Next Page\ldots}} " ///
								"\endfoot " ///
								"\endlastfoot " ///
								"\midrule \multicolumn{3}{c}{\textit{Panel A: Ethiopia}} \\ ") ///
								booktabs nonum nomtitle collabels(none) ///
								nogaps fragment label noobs 
	} 
	else 				{
		esttab 				using "$export/tables/inc_sum.tex", append ///
								prehead("\multicolumn{3}{c}{\textit{Panel `country' }} \\ ") ///
								booktabs nonum nomtitle collabels(none) ///
								nogaps fragment label noobs 
	}
	
	foreach 					var in `list`c'' {
		replace 					`var'amnt_0 = . if `var'amnt_0 == 0
		
		quietly: sum 				`var'0 if country == `c' & wave == 0 [aweight = weight]
		local 						temp1 = r(mean) 
		local 						mean1 : display %4.3f `temp1'
		quietly: sum 				`var'amnt_0 if country == `c' & wave == 0  [aweight = weight]
		local 						temp1a = r(mean) 
		local 						mean1a : display %4.0f `temp1a'
		quietly: sum 				`var'0 if country == `c' & wave == 0 [aweight = weight]
		
		local 				label : variable label `var'0
		esttab 				using "$export/tables/inc_sum.tex", append ///
								posthead("`label' & `mean1' & `mean1a' \\ ") ///
								booktabs nonum nomtitle collabels(none) ///
								nogaps fragment label noobs 
	}
	esttab 					using "$export/tables/inc_sum.tex", append ///		
								booktabs nonum nomtitle collabels(none) ///
								nogaps fragment label noobs  ///
								postfoot("\multicolumn{1}{l}{Observations} & " ///
								"\multicolumn{2}{l}{`obs1'} \\ \midrule `end' ")	

	}
	restore 					


* Sector 	
preserve
	gen 				geo = ea if country != 2
	replace 			geo = ta_code if country == 2
	
	local 				list1 = "crop_inc_ live_inc_ live_prod_ wage_emp_ casual_emp_ temp_emp_ nfe_inc_ kind_trans_ cash_trans_ food_trans_ asst_kind_ asst_cash_ asst_food_ pen_inc_ rent_inc_ asset_ save_inc_ oth_inc_"
	local 				list2  "crop_inc_ tree_inc_ live_inc_ live_prod_ wage_emp_ casual_emp_ nfe_inc_	cash_trans_ food_trans_ kind_trans_ cash_child_	kind_child_ asst_food_ asst_cash_ for_wrk_ masaf_ pen_inc_ rent_inc_ asset_	save_inc_ oth_inc_"
	local 				list3 = "crop_inc_ tree_inc_ live_inc_ live_prod_ wage_emp_ nfe_inc_ cash_dom_ cash_for_ kind_inc_ asst_inc_ pen_inc_ rent_nonag_ ag_rent_ save_inc_ oth_inc_"
	local 				list4 = "crop_inc_ live_inc_ live_prod_ wage_emp_ nfe_inc_ cash_dom_ kind_dom_ cash_for_ kind_for_ sage_ pen_inc_ rent_inc_ interest_ oth_inc_"
	

	estpost 			sum wave //need random stored variable for esttabs below to work, even though this is not referenced
	foreach 			c in `countries' {
	if 					`c' == 1 {
		local 				obs1 = "977"
		local 				obs2 = "2270"
		local 				end = " "
	}
	else 				if 	`c' == 2 {
		local 				country = "B: Malawi"
		local 				obs1 = "1092"
		local 				obs2 = "634"
		local 				end = " "
	}
	else 				if `c' == 3 {
		local 				country = "C: Nigeria"
		local 				obs1 = "1195"
		local 				obs2 = "755"
		local 				end = " "
	}
	else 				if `c' == 4 {
		local 				country = "D: Uganda"
		local 				obs1 = "1642"
		local 				obs2 = "583"
		local 				end = "\multicolumn{7}{C{15cm}}{\small \textit{Note}: The table displays the percent of households engaged in (columns 1-3) and the mean income earned from (columns 4-6) each income category used to generate Indices 5 and 6. We present average values by sector. In the LSMS data, income is reported in the local currency. To allow for cross-country comparisons, we convert income values to US dollars using 2019 exchange rates found at \href{https://exchangerates.org}{https://exchangerates.org}. We use LSMS data to generate the table, which covers the pre-COVID-19 period. We calculate statistical significance of urban/rural differences using simple regressions clustered at the region level. Standard errors are reported in parentheses (*** p$<$0.001, ** p$<$0.01, * p$<$0.05). Urban households in Ethiopia were not asked about agricultural engagement in the 2019 LSMS survey.} \end{longtable}"
	}
	if 					`c' == 1 {
		esttab 				using "$export/tables/inc_sec_sum.tex", replace ///
								prehead("\begin{longtable}{l P{1cm} P{1cm} P{1.5cm} P{1cm} P{1cm} P{1.5cm}} " ///
								"\caption{2019 Engagement in and Earnings from Income Sources by Sector} \label{incsecsum} \\ [-1.8ex] \hline \hline & " ///
								"\multicolumn{3}{c}{\textbf{Percent Engaged}} & \multicolumn{3}{c}{\textbf{Mean Income (USD)}} \\ & " ///
								"\multicolumn{1}{c}{Rural} & \multicolumn{1}{c}{Urban} & \multicolumn{1}{c}{Difference} & " ///
								"\multicolumn{1}{c}{Rural} & \multicolumn{1}{c}{Urban} & \multicolumn{1}{c}{Difference} \\ " ///
								"\endfirsthead " ///
								"\hline & \multicolumn{3}{c}{\textbf{Percent Engaged}} & \multicolumn{3}{c}{\textbf{Mean Income (USD)}} \\ & " ///
								"\multicolumn{1}{c}{Rural} & \multicolumn{1}{c}{Urban} & \multicolumn{1}{c}{Difference} & " ///
								"\multicolumn{1}{c}{Rural} & \multicolumn{1}{c}{Urban} & \multicolumn{1}{c}{Difference} \\ \hline " ///
								"\endhead " ///
								"\multicolumn{7}{c}{{Continued on Next Page\ldots}} " ///
								"\endfoot " ///
								"\endlastfoot " ///
								"\midrule \multicolumn{4}{r}{\textit{Panel A: Ethiopia}} \\ ") ///
								booktabs nonum nomtitle collabels(none) ///
								nogaps fragment label noobs 
	} 
	else 				{
		esttab 				using "$export/tables/inc_sec_sum.tex", append ///
								prehead("\multicolumn{4}{r}{\textit{Panel `country' }} \\ ") ///
								booktabs nonum nomtitle collabels(none) ///
								nogaps fragment label noobs drop(sector _cons)
	}
	
	foreach 					var in `list`c'' {
		replace 					`var'amnt_0 = . if `var'amnt_0 == 0
		
		quietly: sum 				`var'0 if country == `c' & wave == 0 & sector == 1 [aweight = weight]
		local 						temp1 = r(mean) 
		local 						mean1 : display %4.3f `temp1'
		quietly: sum 				`var'amnt_0 if country == `c' & wave == 0 & sector == 1 [aweight = weight]
		local 						temp1a = r(mean) 
		local 						mean1a : display %4.0f `temp1a'
		quietly: sum 				`var'0 if country == `c' & wave == 0 & sector == 2 [aweight = weight]
		local 						temp2 = r(mean)
		local 						mean2 : display %4.3f `temp2'
		quietly: sum 				`var'amnt_0 if country == `c' & wave == 0 & sector == 2 [aweight = weight]
		local 						temp2a = r(mean) 
		local 						mean2a : display %4.0f `temp2a'
		local 						temp3 = `temp1' - `temp2'
		local 						dif : display %4.3f `temp3'
		reg 						`var'0 sector if country == `c' & wave == 0 [aweight = weight], cluster(geo)
		local 						pval = r(table)[4, 1]
		if 							`pval' <= .1 {
			local 						star = "*"
		} 
		if 							`pval' <= .05 {
			local 						star = "**" 
		}
		if 							`pval' <= .01 {
			local 						star = "***"
		}
		else if 					`pval' > .1 {
			local 						star = " "
		}
		local 						temp4 = `temp1a' - `temp2a'
		local 						difa : display %4.0f `temp4'
		reg 						`var'amnt_0 sector if country == `c' & wave == 0 [aweight = weight], cluster(geo)
		local 						pvala = r(table)[4, 1]
		if 							`pvala' <= .1 {
			local 						stara = "*"
		} 
		if 							`pvala' <= .05 {
			local 						stara = "**" 
		}
		if 							`pvala' <= .01 {
			local 						stara = "***"
		}
		else if 					`pvala' > .1 {
			local 						stara = " "
		}
		local 				label : variable label `var'0
		esttab 				using "$export/tables/inc_sec_sum.tex", append ///
								posthead("`label' & `mean1' & `mean2' & `dif'\sym{`star'} & `mean1a' & `mean2a' & `difa'\sym{`stara'} \\ ") ///
								booktabs nonum nomtitle collabels(none) ///
								nogaps fragment label noobs drop(sector _cons)
	}
	esttab 					using "$export/tables/inc_sec_sum.tex", append ///		
								booktabs nonum nomtitle collabels(none) ///
								nogaps fragment label noobs drop(sector _cons) ///
								postfoot("\multicolumn{1}{l}{Observations} & " ///
								"`obs1' & `obs2' & & `obs1' & `obs2' & \\ \midrule `end' ")	

	}
	restore 					

* Gender HOH
preserve
	gen 				geo = ea if country != 2
	replace 			geo = ta_code if country == 2
	
	local 				list1 = "crop_inc_ live_inc_ live_prod_ wage_emp_ casual_emp_ temp_emp_ nfe_inc_ kind_trans_ cash_trans_ food_trans_ asst_kind_ asst_cash_ asst_food_ pen_inc_ rent_inc_ asset_ save_inc_ oth_inc_"
	local 				list2  "crop_inc_ tree_inc_ live_inc_ live_prod_ wage_emp_ casual_emp_ nfe_inc_	cash_trans_ food_trans_ kind_trans_ cash_child_	kind_child_ asst_food_ asst_cash_ for_wrk_ masaf_ pen_inc_ rent_inc_ asset_	save_inc_ oth_inc_"
	local 				list3 = "crop_inc_ tree_inc_ live_inc_ live_prod_ wage_emp_ nfe_inc_ cash_dom_ cash_for_ kind_inc_ asst_inc_ pen_inc_ rent_nonag_ ag_rent_ save_inc_ oth_inc_"
	local 				list4 = "crop_inc_ live_inc_ live_prod_ wage_emp_ nfe_inc_ cash_dom_ kind_dom_ cash_for_ kind_for_ sage_ pen_inc_ rent_inc_ interest_ oth_inc_"
	
	foreach 			c in `countries' {
	if 					`c' == 1 {
		local 				obs1 = "2251 "
		local 				obs2 = "996"
		local 				end = " "
	}
	else 				if 	`c' == 2 {
		local 				country = "B: Malawi"
		local 				obs1 = "1343"
		local 				obs2 = "383"
		local 				end = " "
	}
	else 				if `c' == 3 {
		local 				country = "C: Nigeria"
		local 				obs1 = "1578 "
		local 				obs2 = "372"
		local 				end = " "
	}
	else 				if `c' == 4 {
		local 				country = "D: Uganda"
		local 				obs1 = "1494 "
		local 				obs2 = "731"
		local 				end = " \midrule \multicolumn{7}{C{15cm}}{\small \textit{Note}: The table displays the percent of households engaged in (columns 1-3) and the mean income earned from (columns 4-6) each income category used to generate Indices 5 and 6. We present average values by head-of-household-gender. In the LSMS data, income is reported in the local currency. To allow for cross-country comparisons, we convert income values to US dollars using 2019 exchange rates found at \href{https://exchangerates.org}{https://exchangerates.org}. We use LSMS data to generate the table, which covers the pre-COVID-19 period. We calculate statistical significance of male- versus female-headed household differences using simple regressions clustered at the region level. Standard errors are reported in parentheses (*** p$<$0.001, ** p$<$0.01, * p$<$0.05).} \end{longtable}"
	}
	if 					`c' == 1 {
		esttab 				using "$export/tables/inc_sex_sum.tex", replace ///
								prehead("\begin{longtable}{l P{1cm} P{1cm} P{1.5cm} P{1cm} P{1cm} P{1.5cm}} " ///
								"\caption{2019 Engagement in and Earnings from Income Sources by Head of Household Gender} \label{incsexsum} \\ [-1.8ex] \hline \hline & " ///
								"\multicolumn{3}{c}{\textbf{Percent Engaged}} & \multicolumn{3}{c}{\textbf{Mean Income (USD)}} \\ & " ///
								"\multicolumn{1}{c}{Male} & \multicolumn{1}{c}{Female} & \multicolumn{1}{c}{Difference} & " ///
								"\multicolumn{1}{c}{Male} & \multicolumn{1}{c}{Female} & \multicolumn{1}{c}{Difference} \\ " ///
								"\endfirsthead " ///
								"\hline & \multicolumn{3}{c}{\textbf{Percent Engaged}} & \multicolumn{3}{c}{\textbf{Mean Income (USD)}} \\ & " ///
								"\multicolumn{1}{c}{Male} & \multicolumn{1}{c}{Female} & \multicolumn{1}{c}{Difference} & " ///
								"\multicolumn{1}{c}{Male} & \multicolumn{1}{c}{Female} & \multicolumn{1}{c}{Difference} \\ \hline " ///
								"\endhead " ///
								"\multicolumn{7}{c}{{Continued on Next Page\ldots}} " ///
								"\endfoot " ///
								"\endlastfoot " ///
								"\midrule \multicolumn{4}{r}{\textit{Panel A: Ethiopia}} \\ ") ///
								booktabs nonum nomtitle collabels(none) ///
								nogaps fragment label noobs drop(sector _cons)
	} 
	else 				{
		esttab 				using "$export/tables/inc_sex_sum.tex", append ///
								prehead("\multicolumn{4}{r}{\textit{Panel `country' }} \\ ") ///
								booktabs nonum nomtitle collabels(none) ///
								nogaps fragment label noobs drop(sexhh _cons)
	}
	
	foreach 					var in `list`c'' {
		replace 					`var'amnt_0 = . if `var'amnt_0 == 0
		
		quietly: sum 				`var'0 if country == `c' & wave == 0 & sexhh== 1 [aweight = weight]
		local 						temp1 = r(mean) 
		local 						mean1 : display %4.3f `temp1'
		quietly: sum 				`var'amnt_0 if country == `c' & wave == 0 & sexhh == 1 [aweight = weight]
		local 						temp1a = r(mean) 
		local 						mean1a : display %4.0f `temp1a'
		quietly: sum 				`var'0 if country == `c' & wave == 0 & sexhh == 2 [aweight = weight]
		local 						temp2 = r(mean)
		local 						mean2 : display %4.3f `temp2'
		quietly: sum 				`var'amnt_0 if country == `c' & wave == 0 & sexhh == 2 [aweight = weight]
		local 						temp2a = r(mean) 
		local 						mean2a : display %4.0f `temp2a'
		local 						temp3 = `temp1' - `temp2'
		local 						dif : display %4.3f `temp3'
		reg 						`var'0 sexhh if country == `c' & wave == 0 [aweight = weight], cluster(geo)
		local 						pval = r(table)[4, 1]
		if 							`pval' <= .1 {
			local 						star = "*"
		} 
		if 							`pval' <= .05 {
			local 						star = "**" 
		}
		if 							`pval' <= .01 {
			local 						star = "***"
		}
		else if 					`pval' > .1 {
			local 						star = " "
		}
		local 						temp4 = `temp1a' - `temp2a'
		local 						difa : display %4.0f `temp4'
		reg 						`var'amnt_0 sexhh if country == `c' & wave == 0 [aweight = weight], cluster(geo)
		local 						pvala = r(table)[4, 1]
		if 							`pvala' <= .1 {
			local 						stara = "*"
		} 
		if 							`pvala' <= .05 {
			local 						stara = "**" 
		}
		if 							`pvala' <= .01 {
			local 						stara = "***"
		}
		else if 					`pvala' > .1 {
			local 						stara = " "
		}
		local 				label : variable label `var'0
		esttab 				using "$export/tables/inc_sex_sum.tex", append ///
								posthead("`label' & `mean1' & `mean2' & `dif'\sym{`star'} & `mean1a' & `mean2a' & `difa'\sym{`stara'} \\ ") ///
								booktabs nonum nomtitle collabels(none) ///
								nogaps fragment label noobs drop(sexhh _cons)
	}
	esttab 					using "$export/tables/inc_sex_sum.tex", append ///		
								booktabs nonum nomtitle collabels(none) ///
								nogaps fragment label noobs drop(sexhh _cons) ///
								postfoot("\multicolumn{1}{l}{Observations} & " ///
								"`obs1' & `obs2' & & `obs1' & `obs2' & \\ \midrule `end' ")	

	}
	restore 		

	
* **********************************************************************
**# figures
* **********************************************************************	

* stringency index
	preserve 
	drop 				if wave_orig < 1
	foreach 			c in 1 2 3 4 {
		if 					`c' == 1 {
			local 			t = "Ethiopia"
			local 			s = "Stringency Score"
		}
		if 					`c' == 2 {
			local 			t = "Malawi"
			local 			s = " "
		}
		if 					`c' == 3 {
			local 			t = "Nigeria"
			local 			s = "Stringency Score"
		}
		if 					`c' == 4 {
			local 			t = "Uganda"
			local 			s = " "
		}
		graph bar 			(mean) stringency_index [pweight = weight] if country == `c', ///
								over(wave, lab(labs(med) angle(45))) title("`t'", size(large)) ///
								bar(1, color(teal*1.3)) ytitle("`s'", size(med)) ///
								legend(label(1 "Male") label(2 "Female") col(2)) ///
								ylabel(0 "0" 20 "20" 40 "40" 60 "60" 80 "80" 100 "100", labs(med))  ///
								name(stringency_`c', replace)	
	}
	
	gr combine 			stringency_1 stringency_2 stringency_3 stringency_4, commonscheme col(2)
	gr export 			"$export/figures/stringency.png", as(png) replace
	restore 
	
* income sources over time 
	* Ethiopia
	preserve
	keep 					if country == 1
	replace 				wave = 3 if wave == 0
	drop 					if wave > 10
	collapse 				(mean) farm_std_pp wage_std_pp nfe_std_pp rem_std_pp asst_std_pp ///
								save_std_pp pen_std_pp [pweight = weight], by(country wave)
	line 					farm_std_pp wage_std_pp nfe_std_pp rem_std_pp asst_std_pp ///
								save_std_pp pen_std_pp wave, sort(wave) ///
								lcolor(cranberry*1 navy*1 emerald*1 erose*1 khaki*1 magenta*1.5 eltblue*1.2) ///
								lp(solid dash solid dash solid dash solid) ///
								lwidth(thick thick thick thick thick thick thick) title("Ethiopia", size(large)) ///
								ylabel(0 "0" .2 "20" .4 "40" .6 "60" .8 "80" 1 "100", nogrid labs(medlarge)) ///
								ytitle("Percent Engaged", size(medlarge)) ///
								xlabel(3 "2019" 4 "Apr20" 5 "May20" 6 "Jun20" 8 "Aug20" 9 "Sep20" 10 "Oct20", ///
								nogrid angle(45) labs(medlarge)) xtitle(" ") legend(label (1 "Farm") ///
								label (2 "Wages") label (3 "Non-Farm Enterprise") label (4 "Remittances") ///
								label (5 "Assistance and Other") label (6 "Savings and Investments") ///
								label (7 "Pension") pos(6) col(4) size(small) margin(-1.5 0 0 0)) ///
								name(eth_emp_line, replace)
	restore 

	* Malawi
	preserve
	keep 					if country == 2
	replace 				wave = 5 if wave == 0
	drop 					if wave > 18
	collapse 				(mean) farm_std_pp wage_std_pp nfe_std_pp rem_std_pp asst_std_pp ///
								save_std_pp pen_std_pp [pweight = weight], by(country wave)
	line 					farm_std_pp wage_std_pp nfe_std_pp rem_std_pp asst_std_pp ///
								save_std_pp pen_std_pp wave, sort(wave) ///
								lcolor(cranberry*1 navy*1 emerald*1 erose*1 khaki*1 magenta*1.5 eltblue*1.2) ///
								lp(solid dash solid dash solid dash solid) ///
								lwidth(thick thick thick thick thick thick thick) title("Malawi", size(large)) ///
								ylabel(0 "0" .2 "20" .4 "40" .6 "60" .8 "80" 1 "100", nogrid labs(medlarge)) ///
								xlabel(5 "2019" 6 "Jun20" 7 "Jul20" 8 "Aug20" 9 "Sep20" 11 "Nov20" ///
								13 "Jan21" 16 "Apr21" 18 "May21", nogrid angle(45) labs(medlarge)) xtitle(" ") ///
								name(mwi_emp_line, replace)
	restore 
	
	* Nigeria
	preserve
	keep 					if country == 3
	replace 				wave = 4 if wave == 0
	drop 					if wave > 13 | wave < 4
	collapse 				(mean) farm_std_pp wage_std_pp nfe_std_pp rem_std_pp asst_std_pp ///
								save_std_pp pen_std_pp [pweight = weight], by(country wave)
	line 					farm_std_pp wage_std_pp nfe_std_pp rem_std_pp asst_std_pp ///
								save_std_pp pen_std_pp wave, sort(wave) ///
								lcolor(cranberry*1 navy*1 emerald*1 erose*1 khaki*1 magenta*1.5 eltblue*1.2) ///
								lp(solid dash solid dash solid dash solid) ///
								lwidth(thick thick thick thick thick thick thick) title("Nigeria", size(large)) ///
								ylabel(0 "0" .2 "20" .4 "40" .6 "60" .8 "80" 1 "100", nogrid labs(medlarge)) ///
								ytitle("Percent Engaged", size(medlarge)) ///
								xlabel(4 "2019" 5 "May20" 8 "Aug20" 9 "Sep20" 13 "Jan21", nogrid angle(45) ///
								labs(medlarge)) xtitle(" ") name(nga_emp_line, replace)
	restore 
	
	* Uganda 
	preserve
	keep 					if country == 4
	replace 				wave = 5 if wave == 0
	drop 					if wave > 14 
	collapse 				(mean) farm_std_pp wage_std_pp nfe_std_pp rem_std_pp asst_std_pp ///
								save_std_pp pen_std_pp [pweight = weight], by(country wave)
	line 					farm_std_pp wage_std_pp nfe_std_pp rem_std_pp asst_std_pp ///
								save_std_pp pen_std_pp wave, sort(wave) ///
								lcolor(cranberry*1 navy*1 emerald*1 erose*1 khaki*1 magenta*1.5 eltblue*1.2) ///
								lp(solid dash solid dash solid dash solid) ///
								lwidth(thick thick thick thick thick thick thick) title("Uganda", size(large)) ///
								ylabel(0 "0" .2 "20" .4 "40" .6 "60" .8 "80" 1 "100", nogrid labs(medlarge)) ///
								xlabel(5 "2019" 6 "Jun20" 8 "Aug20" 9 "Sep20" 11 "Nov20" 14 "Feb21", nogrid angle(45) ///
								labs(medlarge)) xtitle(" ") name(uga_emp_line, replace)
	restore 
	
	grc1leg2 				eth_emp_line mwi_emp_line nga_emp_line uga_emp_line, col(2) commonscheme iscale(.5)
	graph export 			"$export/figures/ind1_sources_time.png", as(png) replace
	
* Kernel density graphs for each index
	foreach 			c in 1 2 3 4 {
		if `c' == 1 {
			local 			x = "eth"
			local 			t = "Ethiopia"
			local 			a = "Density"
			local 			s = " "
		}
		if `c' == 2 {
			local 			x = "mwi"
			local 			t = "Malawi"
			local 			a = " "
			local 			s = " "
		}
		if `c' == 3 {
			local 			x = "nga"
			local 			t = "Nigeria"
			local 			a = "Density"
			local 			s = "Specialization Index"
		}
		if `c' == 4 {
			local 			x = "uga"
			local 			t = "Uganda"
			local 			a = " "
			local 			s = "Specialization Index"
		}
		* index 1
		kdensity 			 std_pp_index if country == `c' & wave == 0 [aweight = weight], ///
								color(navy%30) recast(area) ///
								xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) ///
								ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4", nogrid labsize(small)) ///
								xlabel(, nogrid labsize(small)) bwidth(.04) name(`x'_std_pp, replace)
		* index 2						
		kdensity 			 `x'_pp_index if country == `c' & wave == 0 [aweight = weight], ///
								color(navy%30) recast(area) ///
								xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) ///
								ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4", nogrid labsize(small)) bwidth(.04) ///
								xlabel(, nogrid labsize(small)) name(`x'_pp, replace)
		* index 3
		kdensity 			 std_pre_index_frac if country == `c' & wave == 0 [aweight = weight], ///
								color(navy%30) recast(area) ///
								xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) ///
								ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4", nogrid labsize(small)) ///
								xlabel(, nogrid labsize(small)) bwidth(.04) name(`x'_std_frac, replace)
		* index 4						
		kdensity 			 std_pre_index_hhi if country == `c' & wave == 0 [aweight = weight], ///
								color(navy%30) recast(area) ///
								xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) ///
								ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4", nogrid labsize(small)) ///
								xlabel(, nogrid labsize(small)) name(`x'_std_hhi, replace)
		* index 5
		kdensity 			`x'_pre_index_geo if wave == 0 [aweight = weight], color(navy%30) recast(area) ///
								xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) ///
								ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4", nogrid labsize(small)) ///
								xlabel(, nogrid labsize(small)) name(`x'_geo, replace)
		* index 6
		kdensity 			`x'_pre_index_hhi if wave == 0 [aweight = weight], color(navy%30) recast(area) ///
								xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) ///
								ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4", nogrid labsize(small)) ///
								xlabel(, nogrid labsize(small)) name(`x'_hhi, replace)
	}	
	
	* export graphics by index type
	gr combine 			eth_std_pp mwi_std_pp nga_std_pp  uga_std_pp, col(2) commonscheme		
	graph export 		"$export/figures/ind1_density.png", as(png) replace
	
	gr combine 			eth_pp mwi_pp nga_pp uga_pp, col(2) commonscheme		
	graph export 		"$export/figures/ind2_density.png", as(png) replace	
	
	gr combine 			eth_std_frac mwi_std_frac nga_std_frac uga_std_frac, col(2) commonscheme		
	graph export 		"$export/figures/ind3_density.png", as(png) replace	
	
	gr combine 			eth_std_hhi mwi_std_hhi nga_std_hhi uga_std_hhi, col(2) commonscheme		
	graph export 		"$export/figures/ind4_density.png", as(png) replace
	
	gr combine 			eth_geo mwi_geo nga_geo uga_geo, col(2) commonscheme		
	graph export 		"$export/figures/ind5_density.png", as(png) replace	
	
	gr combine 			eth_hhi mwi_hhi nga_hhi uga_hhi, col(2) commonscheme		
	graph export 		"$export/figures/ind6_density.png", as(png) replace
	
* Kernel density graphs of HHI by gender and sector
	foreach 			c in 1 2 3 4 {
		if `c' == 1 {
			local 			x = "eth"
			local 			t = "Ethiopia"
			local 			a = "Density"
			local 			s = " "
		}
		if `c' == 2 {
			local 			x = "mwi"
			local 			t = "Malawi"
			local 			a = " "
			local 			s = " "
		}
		if `c' == 3 {
			local 			x = "nga"
			local 			t = "Nigeria"
			local 			a = "Density"
			local 			s = "Specialization Index"
		}
		if `c' == 4 {
			local 			x = "uga"
			local 			t = "Uganda"
			local 			a = " "
			local 			s = "Specialization Index"
		}
		
		* index 1
			* sector
			twoway  			(kdensity std_pp_index if sector == 1 & country == `c' [aweight = weight], ///
									color(gray%30) recast(area) bwidth(.04)) ///
									(kdensity std_pp_index if sector == 2 & country == `c' [aweight = weight], ///
									color(maroon%30) recast(area) ///
									xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) bwidth(.04) ///
									ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5", nogrid labsize(small)) ///
									xlabel(, nogrid labsize(small))), ///
									legend(col(2) label(1 "Rural") label(2 "Urban")) name(`x'_std_pp_sec, replace)
			* sex
			twoway  			(kdensity std_pp_index if sexhh == 1 & country == `c' [aweight = weight], ///
									color(gray%30) recast(area) bwidth(.04)) ///
									(kdensity std_pp_index if sexhh == 2 & country == `c' [aweight = weight], ///
									color(eltblue%30) recast(area) ///
									xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) bwidth(.04) ///
									ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5", nogrid labsize(small)) ///
									xlabel(, nogrid labsize(small))), ///
									legend(col(2) label(1 "Male") label(2 "Female") ) name(`x'_std_pp_sex,replace)	
		* index 2
			* sector
			twoway  			(kdensity `x'_pp_index if sector == 1 & country == `c' [aweight = weight], ///
									color(gray%30) recast(area) bwidth(.04)) ///
									(kdensity `x'_pp_index if sector == 2 & country == `c' [aweight = weight], ///
									color(maroon%30) recast(area) ///
									xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) bwidth(.04) ///
									ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5", nogrid labsize(small)) ///
									xlabel(, nogrid labsize(small))), legend(col(2) label(1 "Rural") label(2 "Urban")) ///
									name(`x'_pp_sec, replace)
			* sex
			twoway  			(kdensity `x'_pp_index if sexhh == 1 & country == `c' [aweight = weight], ///
									color(gray%30) recast(area) bwidth(.04)) ///
									(kdensity `x'_pp_index if sexhh == 2 & country == `c' [aweight = weight], ///
									color(eltblue%30) recast(area) ///
									xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) bwidth(.04) ///
									ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5", nogrid labsize(small)) ///
									xlabel(, nogrid labsize(small))), legend(col(2) label(1 "Male") label(2 "Female")) ///
									name(`x'_pp_sex, replace)	
		* index 3
			* sector
			twoway  			(kdensity std_pre_index_frac if sector == 1 & country == `c' [aweight = weight], ///
									color(gray%30) recast(area) bwidth(.04)) ///
									(kdensity std_pre_index_frac if sector == 2 & country == `c' [aweight = weight], ///
									color(maroon%30) recast(area) ///
									xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) bwidth(.04) ///
									ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5", nogrid labsize(small)) ///
									xlabel(, nogrid labsize(small))), ///
									legend(col(2) label(1 "Rural") label(2 "Urban")) name(`x'_std_frac_sec, replace)
			* sex
			twoway  			(kdensity std_pre_index_frac if sexhh == 1 & country == `c' [aweight = weight], ///
									color(gray%30) recast(area) bwidth(.04)) ///
									(kdensity std_pre_index_frac if sexhh == 2 & country == `c' [aweight = weight], ///
									color(eltblue%30) recast(area) ///
									xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) bwidth(.04) ///
									ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5", nogrid labsize(small)) ///
									xlabel(, nogrid labsize(small))), ///
									legend(col(2) label(1 "Male") label(2 "Female") ) name(`x'_std_frac_sex, replace)	
		* index 4
			* sector
			twoway  			(kdensity std_pre_index_hhi if sector == 1 & country == `c' [aweight = weight], ///
									color(gray%30) recast(area)) ///
									(kdensity std_pre_index_hhi if sector == 2 & country == `c' [aweight = weight], ///
									color(maroon%30) recast(area) ///
									xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) ///
									ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5", nogrid labsize(small)) ///
									xlabel(, nogrid labsize(small))), legend(col(2) label(1 "Rural") label(2 "Urban")) ///
									name(`x'_std_hhi_sec, replace)
			* sex
			twoway  			(kdensity std_pre_index_hhi if sexhh == 1 & country == `c' [aweight = weight], ///
									color(gray%30) recast(area)) ///
									(kdensity std_pre_index_hhi if sexhh == 2 & country == `c' [aweight = weight], ///
									color(eltblue%30) recast(area) ///
									xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) ///
									ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5", nogrid labsize(small)) ///
									xlabel(, nogrid labsize(small))), legend(col(2) label(1 "Male") label(2 "Female")) ///
									name(`x'_std_hhi_sex,replace)		
		* index 5
			* sector
			twoway  			(kdensity `x'_pre_index_geo if sector == 1 [aweight = weight], ///
									color(gray%30) recast(area)) ///
									(kdensity `x'_pre_index_geo if sector == 2 [aweight = weight], ///
									color(maroon%30) recast(area) ///
									xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) ///
									ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5", nogrid labsize(small)) ///
									xlabel(, nogrid labsize(small))), legend(col(2) label(1 "Rural") label(2 "Urban")) ////
									name(`x'_geo_sec, replace)
			* sex
			twoway  			(kdensity `x'_pre_index_geo if sexhh == 1 [aweight = weight], color(gray%30) recast(area)) ///
									(kdensity `x'_pre_index_geo if sexhh == 2 [aweight = weight], color(eltblue%30) recast(area) ///
									xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) ///
									ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5", nogrid labsize(small)) ///
									xlabel(, nogrid labsize(small))), legend(col(2) label(1 "Male") label(2 "Female") ) ///
									name(`x'_geo_sex,replace)
		* index 6
			* sector
			twoway  			(kdensity `x'_pre_index_hhi if sector == 1 [aweight = weight], color(gray%30) recast(area)) ///
									(kdensity `x'_pre_index_hhi if sector == 2 [aweight = weight], color(maroon%30) recast(area) ///
									xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) ///
									ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5", nogrid labsize(small)) ///
									xlabel(, nogrid labsize(small))), legend(col(2) label(1 "Rural") label(2 "Urban")) ///
									name(`x'_hhi_sec, replace)
			* sex
			twoway  			(kdensity `x'_pre_index_hhi if sexhh == 1 [aweight = weight], color(gray%30) recast(area)) ///
									(kdensity `x'_pre_index_hhi if sexhh == 2 [aweight = weight], color(eltblue%30) recast(area) ///
									xtitle("`s'", size(medsmall)) ytitle("`a'") title("`t'", size(medlarge)) ///
									ylabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5", nogrid labsize(small)) ///
									xlabel(, nogrid labsize(small))), legend(col(2) label(1 "Male") label(2 "Female") ) ///
									name(`x'_hhi_sex,replace)
	}
		
	* export graphics for sec/sex by index type
		* index 1
		grc1leg2 				eth_std_pp_sex mwi_std_pp_sex nga_std_pp_sex uga_std_pp_sex, col(2) commonscheme			
		graph export 			"$export/figures/std_pp_density_sex.png", as(png) replace
	
		grc1leg2 				eth_std_pp_sec mwi_std_pp_sec nga_std_pp_sec uga_std_pp_sec, col(2) commonscheme		
		graph export 			"$export/figures/std_pp_density_sec.png", as(png) replace
		
		* index 2
		grc1leg2 				eth_pp_sex mwi_pp_sex nga_pp_sex uga_pp_sex, col(2) commonscheme			
		graph export 			"$export/figures/pp_density_sex.png", as(png) replace
	
		grc1leg2 				eth_pp_sec mwi_pp_sec nga_pp_sec uga_pp_sec, col(2) commonscheme		
		graph export 			"$export/figures/pp_density_sec.png", as(png) replace
			
		* index 3
		grc1leg2 				eth_std_frac_sex mwi_std_frac_sex nga_std_frac_sex uga_std_frac_sex, col(2) commonscheme			
		graph export 			"$export/figures/std_frac_density_sex.png", as(png) replace
	
		grc1leg2 				eth_std_frac_sec mwi_std_frac_sec nga_std_frac_sec uga_std_frac_sec, col(2) commonscheme		
		graph export 			"$export/figures/std_frac_density_sec.png", as(png) replace
		
		* index 4
		grc1leg2 				eth_std_hhi_sex mwi_std_hhi_sex nga_std_hhi_sex uga_std_hhi_sex, col(2) commonscheme			
		graph export 			"$export/figures/std_hhi_density_sex.png", as(png) replace
	
		grc1leg2 				eth_std_hhi_sec mwi_std_hhi_sec nga_std_hhi_sec uga_std_hhi_sec, col(2) commonscheme		
		graph export 			"$export/figures/std_hhi_density_sec.png", as(png) replace

		* index 5
		grc1leg2 				eth_geo_sex mwi_geo_sex nga_geo_sex uga_geo_sex, col(2) commonscheme			
		graph export 			"$export/figures/geo_density_sex.png", as(png) replace
	
		grc1leg2 				eth_geo_sec mwi_geo_sec nga_geo_sec uga_geo_sec, col(2) commonscheme		
		graph export 			"$export/figures/geo_density_sec.png", as(png) replace
		
		* index 6
		grc1leg2 				eth_hhi_sex mwi_hhi_sex nga_hhi_sex uga_hhi_sex, col(2) commonscheme 		
		graph export 			"$export/figures/hhi_density_sex.png", as(png) replace
	
		grc1leg2 				eth_hhi_sec mwi_hhi_sec nga_hhi_sec uga_hhi_sec, col(2) commonscheme 			
		graph export 			"$export/figures/hhi_density_sec.png", as(png) replace

* index 1 over time 	
	foreach 			c in `countries'  {
		if `c' == 1 {
			local 			x = "eth"
			local 			t = "Ethiopia"
			local 			s = "Specialization Index"
		}
		if `c' == 2 {
			local 			x = "mwi"
			local 			t = "Malawi"
			local 			s = " "
		}
		if `c' == 3 {
			local 			x = "nga"
			local 			t = "Nigeria"
			local 			s = "Specialization Index"
		}
		if `c' == 4 {
			local 			x = "uga"
			local 			t = "Uganda"
			local 			s = " "
		}
		preserve
		keep 				if country == `c'
		keep 				if std_pp_index != .
		graph bar 			(mean) std_pp_index [pweight = weight], ///
								over(wave, lab(labs(med) angle(45))) title("`t'", size(large)) ///
								bar(2, color(maroon*1.5)) bar(1, color(gray*1.3)) ///
								ytitle("", margin( 0 -1 -1 10) size(small)) ///
								ylabel(0 "0" .2 "20" .4 "40" .6 "60" .8 "80" 1 "100", labs(med))  ///
								legend(label(1 "Mean of Specialization Index")) ///
								name(std_pp_index_time_`c', replace)	
		restore 
	}
	
	* test for significance 
	foreach 				het in sector sexhh {
		foreach 				c in 1 2 3 4 {
			preserve
			keep 					if country == `c'
			keep 					if std_pp_index != .
			levelsof 				wave_, local(waves)
			foreach 				w in `waves' {
				di 						"country `c' wave `w'"
				reg 					std_pp_index `het' if wave_ == `w' [aweight = weight]
			}
			restore 
		}
	}
	
	* export graphics over time by sector
	grc1leg2 				std_pp_index_time_1 std_pp_index_time_2 std_pp_index_time_3 std_pp_index_time_4, ///
								col(2) commonscheme 
	gr export 				"$export/figures/std_pp_index_time.png", as(png) replace
		
		
* index 1 over time by sector	
	gen 				std_pp_index_sec1 = std_pp_index if sector == 1
	gen 				std_pp_index_sec2 = std_pp_index if sector == 2
	foreach 			c in `countries'  {
		if `c' == 1 {
			local 			x = "eth"
			local 			t = "Ethiopia"
			local 			s = "Specialization Index"
		}
		if `c' == 2 {
			local 			x = "mwi"
			local 			t = "Malawi"
			local 			s = " "
		}
		if `c' == 3 {
			local 			x = "nga"
			local 			t = "Nigeria"
			local 			s = "Specialization Index"
		}
		if `c' == 4 {
			local 			x = "uga"
			local 			t = "Uganda"
			local 			s = " "
		}
		preserve
		keep 				if country == `c'
		keep 				if std_pp_index != .
		graph bar 			(mean) std_pp_index_sec1 std_pp_index_sec2 [pweight = weight], ///
								over(wave, lab(labs(med) angle(45))) title("`t'", size(large)) ///
								bar(2, color(maroon*1.5)) bar(1, color(gray*1.3)) ///
								ytitle("`s'", margin( 0 -1 -1 10) size(small)) ///
								legend(label(1 "Rural") label(2 "Urban") col(2)) ///
								ylabel(0 "0" .2 "20" .4 "40" .6 "60" .8 "80" 1 "100", labs(med))  ///
								name(std_pp_index_time_sec_`c', replace)	
		restore 
	}

	drop 					std_pp_index_sec*
	
	* test for significance 
	foreach 				het in sector sexhh {
		foreach 				c in 1 2 3 4 {
			preserve
			keep 					if country == `c'
			keep 					if std_pp_index != .
			levelsof 				wave_, local(waves)
			foreach 				w in `waves' {
				di 						"country `c' wave `w'"
				reg 					std_pp_index `het' if wave_ == `w' [aweight = weight]
			}
			restore 
		}
	}
	
	* export graphics over time by sector
	grc1leg2 				std_pp_index_time_sec_1 std_pp_index_time_sec_2 std_pp_index_time_sec_3 std_pp_index_time_sec_4, ///
								col(2) commonscheme 
	gr export 				"$export/figures/std_pp_index_time_sector.png", as(png) replace

* index 1 over time by sex	
	gen 				std_pp_index_sex1 = std_pp_index if sexhh == 1
	gen 				std_pp_index_sex2 = std_pp_index if sexhh == 2
	foreach 			c in `countries'  {
		if `c' == 1 {
			local 			x = "eth"
			local 			t = "Ethiopia"
			local 			s = "Specialization Index"
		}
		if `c' == 2 {
			local 			x = "mwi"
			local 			t = "Malawi"
			local 			s = " "
		}
		if `c' == 3 {
			local 			x = "nga"
			local 			t = "Nigeria"
			local 			s = "Specialization Index"
		}
		if `c' == 4 {
			local 			x = "uga"
			local 			t = "Uganda"
			local 			s = " "
		}
		preserve
		keep 				if country == `c'
		keep 				if std_pp_index != .
		graph bar 			(mean) std_pp_index_sex1 std_pp_index_sex2 [pweight = weight], ///
								over(wave, lab(labs(med) angle(45))) title("`t'", size(large)) ///
								bar(2, color(eltblue*1.5)) bar(1, color(gray*1.3)) ///
								ytitle("`s'", margin( 0 -1 -1 10) size(small)) ///
								legend(label(1 "Male") label(2 "Female") col(2)) ///
								ylabel(0 "0" .2 "20" .4 "40" .6 "60" .8 "80" 1 "100", labs(med))  ///
								name(std_pp_index_time_sex_`c', replace)	
		restore 
	}

	drop 					std_pp_index_sex*
	
	* export graphics over time by sex
	grc1leg2 				std_pp_index_time_sex_1 std_pp_index_time_sex_2 std_pp_index_time_sex_3 std_pp_index_time_sex_4, ///
								col(2) commonscheme 
	gr export 				"$export/figures/std_pp_index_time_sex.png", as(png) replace	
	
	
* FIES
	* Ethiopia
	preserve 
	keep 					if country == 1
	foreach 				x in mild mod sev {
		egen 					`x'_fs_mean = mean(`x'_fs), by(country wave)
	}
	replace 				wave = 4 if wave == 0
	drop 					if wave > 10
	line					 mild_fs_mean mod_fs_mean sev_fs_mean wave [pweight = weight], ///
								sort(wave) lcolor(cranberry*.4 cranberry*.8 cranberry*1.6) ///
								lwidth(vthick vthick vthick) title("Ethiopia", size(large)) ///
								ylabel(0 "0" .2 "20" .4 "40" .6 "60" .8 "80" 1 "100", nogrid labs(small)) ///
								ytitle("Percent Reporting Food Insecurity", size(small)) ///
								xlabel(4 "2019" 5 "May20" 6 "Jun20" 7 "Jul20" 8 "Aug20" 9 "Sep20" 10 "Oct20", ///
								nogrid angle(45) labs(small)) xtitle(" ") legend(label (1 "Mild Food Insecurity") ///
								label (2 "Moderate Food Insecurity") label (3 "Severe Food Insecurity") ///
								pos(6) col(3) size(small) margin(-1.5 0 0 0)) name(eth_fies, replace)
	restore 

	* Malawi
	preserve 
	keep 					if country == 2
	foreach 				x in mild mod sev {
		egen 					`x'_fs_mean = mean(`x'_fs), by(country wave)
	}
	replace 				wave = 5 if wave == 0
	replace 				wave = 17 if wave == 18

	line					 mild_fs_mean mod_fs_mean sev_fs_mean wave [pweight = weight], ///
								sort(wave) lcolor(cranberry*.4 cranberry*.8 cranberry*1.6) ///
								lwidth(vthick vthick vthick) title("Malawi", size(large)) ///
								yscale(range(0(.2) 1)) ylabel(0 "0" .2 "20" .4 "40" .6 "60" .8 "80" 1 "100", ///
								nogrid labs(small)) ///
								xlabel(5 "2019" 6 "Jun20" 7 "Jul20" 8 "Aug20" 9 "Sep20" 10 "Oct20" 11 "Nov20" ///
								12 "Dec20" 13 "Jan21" 14 "Feb21" 15 "Mar21" 16 "Apr21" 17 "May21", ///
								nogrid angle(45) labs(small)) xtitle(" ") legend(label (1 "Mild Food Insecurity") ///
								label (2 "Moderate Food Insecurity") label (3 "Severe Food Insecurity") ///
								pos(6) col(3) size(small) margin(-1.5 0 0 0)) name(mwi_fies, replace)
	restore 

	* Nigeria
	preserve 
	keep 					if country == 3
	drop 					if mild_fs >= .
	foreach 				x in mild mod sev {
		egen 					`x'_fs_mean = mean(`x'_fs), by(country wave)
	}
	replace 				wave = 4 if wave == -1
	replace 				wave = 5 if wave == 0

	line					 mild_fs_mean mod_fs_mean sev_fs_mean wave [pweight = weight], ///
								sort(wave) lcolor(cranberry*.4 cranberry*.8 cranberry*1.6) ///
								lwidth(vthick vthick vthick) title("Nigeria", size(large)) ///
								yscale(range(0(.2) 1)) ylabel(0 "0" .2 "20" .4 "40" .6 "60" .8 "80" 1 "100", ///
								nogrid labs(small)) ///
								xlabel(4 "2019pp" 5 "2019ph" 6 "Jun20" 7 "Jul20" 8 "Aug20" ///
								9 "Sep20" 10 "Oct20" 11 "Nov20", nogrid angle(45) labs(small)) ///
								xtitle(" ") legend(label (3 "Mild Food Insecurity") ///
								label (2 "Moderate Food Insecurity") label (1 "Severe Food Insecurity") ///
								pos(6) col(3) size(small) margin(-1.5 0 0 0)) name(nga_fies, replace)
	restore 

	grc1leg2 			eth_fies mwi_fies nga_fies, col(3) commonscheme name(fies_line, replace)			
	graph export 		"$export/figures/fies_line.png", as(png) replace
	
* Education	
	catplot 			edu_act wave country [aweight = weight_child] if country == 1, percent(wave) stack ///
							bar(1, color(maroon*1.5)) bar(2, color(stone*1.5)) ytitle(" ") ///
							var1opts(label(labsize(large))) var3opts(label(angle(90) labsize(large))) ///
							legend(col(2) margin(-1.5 0 0 0)) name(eth_edu, replace)
							
	catplot 			edu_act wave country [aweight = weight_child] if country == 2, percent(wave) stack ///
							bar(1, color(maroon*1.5)) bar(2, color(stone*1.5)) ytitle(" ") ///
							var1opts(label(labsize(large))) var3opts(label(angle(90) labsize(large))) ///
							legend(col(2) margin(-1.5 0 0 0)) name(mwi_edu, replace)
							
	catplot 			edu_act wave country [aweight = weight_child] if country == 3, percent(wave) stack ///
							bar(1, color(maroon*1.5)) bar(2, color(stone*1.5)) ytitle(" ") ///
							var1opts(label(labsize(large))) var3opts(label(angle(90) labsize(large))) ///
							name(nga_edu, replace)	
	
	catplot 			edu_act wave country [aweight = weight_child] if country == 4, percent(wave) stack ///
							bar(1, color(maroon*1.5)) bar(2, color(stone*1.5)) ytitle("Percent", size(small)) ///
							var1opts(label(labsize(large))) var3opts(label(angle(90) labsize(large))) ///
							name(uga_edu, replace) 
	
	grc1leg2 			eth_edu mwi_edu nga_edu uga_edu, col(1) iscale(.5) commonscheme imargin(0 0 0 0)				
	graph export 		"$export/figures/edu_bar.png", as(png) replace 

	
************************************************************************
**# sectors summary wages & NFE
************************************************************************

* ETHIOPIA 
	* NFE
	use 				"$data/ethiopia/raw/wave_00/HH/sect12a_hh_w4.dta", clear
	forval 				x = 1/8 {
		replace 			s12aq01__`x' = 0 if s12aq01__`x' == 2
	} 
	* keep households with nfe income
	egen 				nfe = rowtotal(s12aq01__*)
	replace 			nfe = 1 if nfe > 0
	keep 				if nfe == 1 // 1,664 obs matches data used in eth baseline nfe
	* calc percent from each sector 
	sum 				s12aq01*
	
	* wages 
	use 				"$data/ethiopia/raw/wave_00/HH/sect4_hh_w4.dta", clear
	// NOTE: MAIN JOB
	tab 				s4q34d
	
* MALAWI
	* NFE
	use 				"$data/malawi/raw/wave_00/hh_mod_n1_19.dta", clear
	forval 				x = 1/8 {
		replace 			hh_n0`x' = 0 if hh_n0`x' == 2
	} 
	* keep households with nfe income
	egen 				nfe = rowtotal(hh_n0*)
	replace 			nfe = 1 if nfe > 0
	keep 				if nfe == 1 // 1,347 obs matches data used in mwi baseline nfe
	* calc percent from each sector 
	sum 				hh_n0*
	
	* wages
	* use 				"$data/malawi/raw/wave_00/hh_mod_e_19.dta", clear
	* hh_e20b is the closes var here and has 70 occup. codes that I cant find the key for
	
* NIGERIA
	* NFE
	use 				"$data/nigeria/raw/wave_00/sect9a_harvestw4.dta", clear
	forval 				x = 1/8 {
		replace 			s9q1c__`x' = 0 if s9q1c__`x' == 2
	} 
	* keep households with nfe income
	egen 				nfe = rowtotal(s9q1c__*)
	replace 			nfe = 1 if nfe > 0
	keep 				if nfe == 1 // 2,769 obs matches data used in nga baseline nfe
	* calc percent from each sector 
	sum 				s9q1c__*
	
	* wages
	use 				"$data/nigeria/raw/wave_00/sect3a_harvestw4.dta", clear
	// NOTE: MAIN JOB
	tab 				s3q14
	
* UGANDA
	* NFE - gen this differently than other countries because prior section uses dif qualifier so nums do not match
	use 			"$data/uganda/raw/wave_00//Household/GSEC12_2", clear	
	tab 			N01_1

	* wages
	// no data
	
************************************************************************
**# end matters
************************************************************************	

* close log
	log 			close 
	
/* END */	