* Project: WB COVID
* Created on: June 2021
* Created by: amf
* Stata v.16.1

* does
	* generates income indices 
	* tests index construction as part of PAP

* assumes
	* cleaned panel data

* TO DO:
	* complete
	
	
* **********************************************************************
* 0 - setup
* **********************************************************************
	
	pause on 
	
* define
	global	export	=	"$data/analysis"
	global	logout	=	"$data/analysis/logs"
	
* open log
	cap log 			close
	log using			"$logout/ld", append
		
* run panel cleaning (may take a while)
	//run 				"$code/analysis/pnl_cleaning"
	use 				"$export/lsms_panel", clear

	
* **********************************************************************
* 1 - generate, format, and clean variables
* **********************************************************************	
	
* generate other income 2 (oth, NGO, unemp, asst) 
*	(in baseline this includes sale of assets)
	gen  				oth_inc2 = 0 if oth_inc == 0 | ngo_inc == 0 ///
							| unemp_inc == 0 | asst_food == 0 | asst_cash == 0 ///
							| asst_kind == 0
	replace  			oth_inc2 = 1 if oth_inc == 1 | ngo_inc == 1 ///
							| unemp_inc == 1 | asst_food == 1 | asst_cash == 1 ///
							| asst_kind == 1
							
* generate other income 3 (oth, NGO, unemp, gov_inc, asst)
*	(in baseline this includes sale of assets)
	gen  				oth_inc3 = 0 if oth_inc == 0 | ngo_inc == 0 ///
							| unemp_inc == 0 | gov_inc == 0 | asst_food == 0 ///
							| asst_cash == 0 | asst_kind == 0
	replace  			oth_inc3 = 1 if oth_inc == 1 | ngo_inc == 1 ///
							| unemp_inc == 1 |  gov_inc == 1 | asst_food == 1 ///
							| asst_cash == 1 | asst_kind == 1
							
* generate government, ngo, unemployment
	gen					gov_ngo_inc = 0 if ngo_inc == 0 | unemp_inc == 0 ///
							| gov_inc == 0
	replace				gov_ngo_inc = 1 if ngo_inc == 1 | unemp_inc == 1 ///
							| gov_inc == 1
							
* combine wage and casual
	replace 			wage_inc = 0 if casual_emp == 0  & wage_inc >= .
	replace 			wage_inc = 1 if casual_emp == 1
	
* combine asst_inc and remit_inc
	replace 			remit_inc = 0 if asst_inc == 0 & remit_inc >= .
	replace 			remit_inc = 1 if asst_inc == 1 
	
	
* **********************************************************************
* 2 - uniform indices without secondary variables (excluding BF)
* **********************************************************************

* keep waves with data 
	forval 					c = 1/5 {
		preserve
			keep 			if country == `c'
			gen 			havedata = bus_inc + farm_inc + ///
								isp_inc + pen_inc + remit_inc + wage_inc 
			collapse 		(sum) havedata, by(country wave)
			drop 			if havedata == 0
			replace 		havedata = 1
			tempfile 		temp`c'
			save 			`temp`c''
		restore
	
		preserve 
			merge 				m:1 country wave using `temp`c'' 
			keep 				if havedata == 1

	* generate index for each country (fraction out of 7 income sources)
			egen 				inc_count = rowtotal(bus_inc farm_inc ///
									isp_inc pen_inc remit_inc wage_inc oth_inc3)
			replace 			inc_count = . if bus_inc >= . | farm_inc >= . ///
									| isp_inc >= . | pen_inc >= . | ///
									remit_inc >= . | wage_inc >= . | oth_inc3 >= .
			gen 				uni_index = inc_count/7
			gen 				uni_index_phhm = inc_count/hhsize 
			keep 				country wave hhid uni_index* 
			tempfile 			country`c'
			save 				`country`c''
		restore		
	} 

	* merge uniform index into panel
	preserve 
		clear
		forval 				c = 1/5 {
			append 			using `country`c''
		}
		tempfile 			temp_uni_index
		save 				`temp_uni_index'
	restore
	
	merge 					1:1 country wave hhid using `temp_uni_index', nogen

	
tab uni_index country, missing 	
tab uni_index_phhm country, missing 	

/*
Why so many 0s in Ethiopia? 
pause 	
*/ 

* **********************************************************************
* 3 - uniform indices with secondary variables when possible to 
* 		increase number of waves in each country (same 7 as denominator)	
* **********************************************************************
	
** baseline ** 	
		* baseline
		* secondary wage var
			gen 				wage_inc_sec = wage_emp if wave == 0
		* secondary bus var
			gen 				bus_inc_sec = bus_emp if wave == 0
		* secondary farm var	
			gen 				farm_inc_sec = farm_emp if wave == 0
	
** ETHIOPIA **
	* same as uni_index		
			
** MALAWI ** 
	* add wave 5 - only other wave with wage inc available
	* secondary wage and bus vars (combine emp_stat and ind income data)
		* don't do this for farm because inconsistent with other rounds
			* could be due to seasonal work or bc ag is not main job
		* wage income for respondent 
			replace				wage_inc_sec = 1 if country == 2 & wave == 5 & ///
									(emp_stat == 4 | emp_stat == 5)
			replace 			wage_inc_sec = 0 if country == 2 & wave == 5 & ///
									emp_stat < 4
		* add in individual employment data to wage income
			replace 			wage_inc_sec = 1 if wage_inc_ind == 1
			replace 			wage_inc_sec = 0 if wage_inc_sec == . & ///
									wage_inc_ind == 0
		* bus income for respondent 
			replace				bus_inc_sec = 1 if country == 2 & wave == 5 & ///
									emp_stat < 3 
			replace 			bus_inc_sec = 0 if country == 2 & wave == 5 & ///
									emp_stat >= 3 & emp_stat < .
		* add in individual employment data to bus income
			replace 			bus_inc_sec = 1 if bus_inc_ind == 1
			replace 			bus_inc_sec = 0 if bus_inc_sec == . & ///
									bus_inc_ind == 0
	
		tab wage_inc_sec wave
		tab wage_inc_ind wave
/*	
	THESE END UP BEING EXACTLY THE SAME AS JUST THE IND DATA, WHY?? (same below in NGA)	
	pause
*/
		tab farm_inc_ind wave if country == 2
		tab farm_inc wave if country == 2
	
	* secondary farm var
		replace					farm_inc_sec = ag_crop if country == 2 & wave == 5
		replace 				farm_inc_sec = 1 if ag_live == 1 & country == 2 & wave == 5
		replace 				farm_inc_sec = 0 if farm_inc_sec == . & ag_live == 0 & ///
									country == 2 & wave == 5
	
	* secondary isp var
		gen 					isp_inc_sec = oth_inc_4 if country == 2 & wave == 5
		replace 				isp_inc_sec = 0 if isp_inc_sec == 2
	
	* secondary pension var
		gen 					pen_inc_sec = oth_inc_5 if country == 2 & wave == 5
		replace 				pen_inc_sec = 0 if pen_inc_sec == 2	
	
	* secondary remittance var
		gen 					remit_inc_sec = oth_inc_2 if country == 2 & wave == 5
		replace 				remit_inc_sec = 1 if oth_inc_1 == 1 & country == 2 & ///
									wave == 5
		replace 				remit_inc_sec = 0 if remit_inc_sec == . & oth_inc_1 == 2 ///
									& country == 2 & wave == 5
		replace 				remit_inc_sec = 0 if remit_inc_sec == 2	
	
	* secondary other vars 2 and 3
		* variable generated in mwi_build_5
		replace 				oth_inc2_sec = 1 if oth_inc_3 == 1 & country == 2 ///
									& wave == 5
		replace 				oth_inc2_sec = 0 if oth_inc2_sec == . & oth_inc_3 == 2 ///
									& country == 2 & wave == 5		
		gen 					oth_inc3_sec = oth_inc2_sec
		replace 				oth_inc3_sec = 1 if gov_inc_sec == 1
		replace 				oth_inc3_sec = 0 if oth_inc3_sec == . & gov_inc_sec == 0			

** NIGERIA **
	* add waves 5 
	* secondary wage and bus vars
		* wage income for respondent
			replace				wage_inc_sec = 1 if country == 3 & wave == 5 & ///
									(emp_stat == 4 | emp_stat == 5 | emp_stat == 6)
			replace 			wage_inc_sec = 0 if country == 3 & wave == 5 & ///
									emp_stat < 4
		* add in individual employment data to wage income
			replace 			wage_inc_sec = 1 if wage_inc_ind == 1
			replace 			wage_inc_sec = 0 if wage_inc_sec == . & ///
									wage_inc_ind == 0
		* bus income for respondent 
			replace				bus_inc_sec = 1 if country == 3 & wave == 5 & ///
									emp_stat < 3 
			replace 			bus_inc_sec = 0 if country == 3 & wave == 5 & ///
									emp_stat >= 3 & emp_stat < .
		* add in individual employment data to bus income
			replace 			bus_inc_sec = 1 if bus_inc_ind == 1
			replace 			bus_inc_sec = 0 if bus_inc_sec == . & ///
									bus_inc_ind == 0
							
		* secondary farm income
			replace 			farm_inc_sec = ag_crop if country == 3 & wave == 5
			replace 			farm_inc_sec = 1 if ag_live == 1 & country == 3 & wave == 5
			replace 			farm_inc_sec = 0 if farm_inc_sec == . & ag_live == 0 & ///
									country == 3 & wave == 5	

		* secondary isp var
			replace 			isp_inc_sec = oth_inc_4 if country == 3 & wave == 5
			replace 			isp_inc_sec = 0 if isp_inc_sec == 2
		
		* secondary pension var
			replace				pen_inc_sec = oth_inc_5 if country == 3 & wave == 5
			replace 			pen_inc_sec = 0 if pen_inc_sec == 2	
		
		* secondary remittance var
			replace				remit_inc_sec = oth_inc_2 if country == 3 & wave == 5
			replace 			remit_inc_sec = 1 if oth_inc_1 == 1 & country == 3 & ///
									wave == 5
			replace 			remit_inc_sec = 0 if remit_inc_sec == . & oth_inc_1 == 2 ///
									& country == 3 & wave == 5
			replace 			remit_inc_sec = 0 if remit_inc_sec == 2	
		
		* secondary other var (only includes oth_inc_3, different from MWI)
			replace 			oth_inc2_sec = oth_inc_3 if country == 3 & wave == 5
			replace 			oth_inc2_sec = 0 if oth_inc2_sec == 2
			replace				oth_inc3_sec = oth_inc2_sec // same because no gov inc var here
			
** UGANDA **
	* same as uniform index

** BURKINA FASO **
	* cannot include - no wave has all 7 variables 
	
	
** COMBINE PRIMARY AND SECONDARY VARS & GENERATE INDEX **
	foreach 				i in wage_inc bus_inc farm_inc isp_inc ///
								pen_inc remit_inc oth_inc3 {
		replace 				`i'_sec = `i' if `i'_sec == .
	}	
		
* keep waves with data 
	forval 					c = 1/5 {
		preserve
			keep 			if country == `c'
			gen 			havedata = bus_inc_sec + farm_inc_sec + ///
								isp_inc_sec + pen_inc_sec + remit_inc_sec + wage_inc_sec 
			collapse 		(sum) havedata, by(country wave)
			drop 			if havedata == 0
			replace 		havedata = 1
			tempfile 		temp`c'
			save 			`temp`c''
		restore
	
		preserve 
			merge 				m:1 country wave using `temp`c'' 
			keep 				if havedata == 1

	* generate index for each country (fraction out of 7 income sources)
			egen 				inc_count_sec = rowtotal(bus_inc_sec farm_inc_sec ///
									isp_inc_sec pen_inc_sec remit_inc_sec wage_inc_sec ///
									oth_inc3_sec)
			replace 			inc_count_sec = . if bus_inc_sec >= . | farm_inc_sec >= . | ///
									isp_inc_sec >= . | pen_inc_sec >= . | ///
									remit_inc_sec >= . | wage_inc_sec >= . | oth_inc3_sec >= .
			gen 				sec_index = inc_count_sec/7
			gen 				sec_index_phhm = inc_count/hhsize 
			keep 				country wave hhid sec_index* 
			tempfile 			country`c'
			save 				`country`c''
		restore		
	} 

	* merge uniform index into panel
	preserve 
		clear
		forval 				c = 1/5 {
			append 			using `country`c''
		}
		tempfile 			temp_sec_index
		save 				`temp_sec_index'
	restore
	
	merge 					1:1 country wave hhid using `temp_sec_index', nogen
		
		
* **********************************************************************
* 4 - generate summary statistics and graphs
* **********************************************************************

** UNIFORM INDEX ** 
	* fraction of total sources
		forval 					c = 1/5 {
			sum 					uni_index if country == `c', detail
		}
		* histogram for all countries pooled
		hist 					uni_index, normal xtitle("Income Index") width(.125) ///
									subtitle("Pooled") color(teal*1.5) graphr(color(grey*.1))
		graph export 			"$export/figures/LD/density/uni_index_all.png", ///
									as(png) replace						
		* histograms for each country							
		forval 					c = 1/4 {
			preserve 
				keep 				if country == `c'
				keep 				if uni_index != .
				levelsof 			(wave), local(w)
				hist 				uni_index, normal xtitle("Income Index") width(.125) ///
										subtitle("Country `c', Waves `w'") color(teal*1.5)
				graph export 		"$export/figures/LD/density/uni_index_`c'.png", ///
										as(png) replace
			restore 
		}
	
	* income sources per hh member
		forval 					c = 1/5 {
			sum 					uni_index_phhm if country == `c', detail
		}
		* histogram for all countries pooled
		hist 					uni_index_phhm, normal xtitle("Income Index Per HH Member") width(.125) ///
									subtitle("Pooled") color(teal*1.5) graphr(color(grey*.1))
		graph export 			"$export/figures/LD/density/uni_index_all_phhm.png", ///
									as(png) replace						
		* histograms for each country							
		forval 					c = 1/4 {
			preserve 
				keep 				if country == `c'
				keep 				if uni_index_phhm != .
				levelsof 			(wave), local(w)
				hist 				uni_index_phhm, normal xtitle("Income Index Per HH Member") width(.125) ///
										subtitle("Country `c', Waves `w'") color(teal*1.5)
				graph export 		"$export/figures/LD/density/uni_index_`c'_phhm.png", ///
										as(png) replace
			restore 
		}
	
** SECONDARY INDEX **
	* fraction of total sources
		forval 					c = 1/5 {
			sum 					sec_index if country == `c', detail
		}
		* histogram for all countries pooled
		hist 					sec_index, normal xtitle("Income Index") width(.125) ///
									subtitle("Pooled") color(teal*1.5) graphr(color(grey*.1))
		graph export 			"$export/figures/LD/density/sec_index_all.png", ///
									as(png) replace						
		* histograms for each country							
		forval 					c = 1/4 {
			preserve 
				keep 				if country == `c'
				keep 				if sec_index != .
				levelsof 			(wave), local(w)
				hist 				sec_index, normal xtitle("Income Index") width(.125) ///
										subtitle("Country `c', Waves `w'") color(teal*1.5)
				graph export 		"$export/figures/LD/density/sec_index_`c'.png", ///
										as(png) replace
			restore 
		}
			
	* income sources per hh member	
		forval 					c = 1/5 {
			sum 					sec_index_phhm if country == `c', detail
		}
		* histogram for all countries pooled
		hist 					sec_index_phhm, normal xtitle("Income Index Per HH Member") width(.125) ///
									subtitle("Pooled") color(teal*1.5) graphr(color(grey*.1))
		graph export 			"$export/figures/LD/density/sec_index_all_phhm.png", ///
									as(png) replace						
		* histograms for each country							
		forval 					c = 1/4 {
			preserve 
				keep 				if country == `c'
				keep 				if sec_index_phhm != .
				levelsof 			(wave), local(w)
				hist 				sec_index_phhm, normal xtitle("Income Index Per HH Member") width(.125) ///
										subtitle("Country `c', Waves `w'") color(teal*1.5)
				graph export 		"$export/figures/LD/density/sec_index_`c'_phhm.png", ///
										as(png) replace
			restore 
		}	

/*		
		
* **********************************************************************
* 4 - country indices (allow for dif denominator - as many waves as possible)
* **********************************************************************
** ETHIOPIA **
	* all 5 already included
** MALAWI **
	* wave 6
		* would have to exclude wage and bus 
** NIGERIA **
	
** UGANDA **
** BURKINA FASO **
	* add wave 7 (if we use ind farm data, not great...) & 1 (if we don't use gov in other (use oth_inc_3))
	* secondary wage and bus vars
		* wage income for respondent
			replace				wage_inc_sec = 1 if country == 5 & wave == 7 & ///
									(emp_stat == 4 | emp_stat == 5 | emp_stat == 6)
			replace 			wage_inc_sec = 0 if country == 5 & wave == 7 & ///
									emp_stat < 4
		* add in individual employment data to wage income
			replace 			wage_inc_sec = 1 if wage_inc_ind == 1
			replace 			wage_inc_sec = 0 if wage_inc_sec == . & ///
									wage_inc_ind == 0
		* add wages data for round 1 
			replace 			wage_inc_sec = emp_able_hh if country == 5 & wave == 1
			replace 			wage_inc_sec = 0 if wage_inc_sec == 2	
tab wage_inc_sec wave if country == 5
/* 
* do these seem comparable/reasonable?
*/
pause
	
		* bus income for respondent 
			replace				bus_inc_sec = 1 if country == 5 & wave == 7 & ///
									emp_stat < 3 
			replace 			bus_inc_sec = 0 if country == 5 & wave == 7 & ///
									emp_stat >= 3 & emp_stat < .
		* add in individual employment data to bus income
			replace 			bus_inc_sec = 1 if bus_inc_ind == 1
			replace 			bus_inc_sec = 0 if bus_inc_sec == . & ///
									bus_inc_ind == 0
		* add bus data for round 1 	
			replace 			bus_inc_sec = bus_emp if country == 5 & wave == 1
		
		* secondary farm var
			replace 			farm_inc_sec = farm_emp if country == 5 & wave == 1
		/*		
		* secondary isp var
			replace 			isp_inc_sec = oth_inc_4 if country == 3 & wave == 5
			replace 			isp_inc_sec = 0 if isp_inc_sec == 2
		
		* secondary pension var
			replace				pen_inc_sec = oth_inc_5 if country == 3 & wave == 5
			replace 			pen_inc_sec = 0 if pen_inc_sec == 2	
		
		* secondary remittance var
			replace				remit_inc_sec = oth_inc_2 if country == 3 & wave == 5
			replace 			remit_inc_sec = 1 if oth_inc_1 == 1 & country == 3 & ///
									wave == 5
			replace 			remit_inc_sec = 0 if remit_inc_sec == . & oth_inc_1 == 2 ///
									& country == 3 & wave == 5
			replace 			remit_inc_sec = 0 if remit_inc_sec == 2	
		
		* secondary other var (only includes oth_inc_3, different from MWI)
			replace 			oth_inc_sec = oth_inc_3 if country == 3 & wave == 5
			replace 			oth_inc_sec = 0 if oth_inc_sec == 2
		*/