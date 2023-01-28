* Project: DIVERSIFICATION
* Created on: Sept 2021
* Created by: amf
* Edited by: jdm
* Last edit: 27 jan 2021
* Stata v.17.0


* does
	* merges together all countries
	* renames variables
	* output cleaned panel data

* assumes
	* cleaned country data

* TO DO:
	* add new rounds 
	
	
************************************************************************
**# setup
************************************************************************

* define
	global  root    =	"$data/analysis"
	global	eth		=	"$data/ethiopia/refined/wave_00"
	global	mwi		=	"$data/malawi/refined/wave_00"
	global	nga		=	"$data/nigeria/refined/wave_00"
	global	uga		=	"$data/uganda/refined/wave_00"
	global	bf		=	"$data/burkina_faso/refined/wave_00"
	global	export	=	"$data/analysis/diversification"
	global	logout	=	"$data/analysis/diversification/logs"

* open log
	cap log 			close
	log using			"$logout/pnl_cleaning_div", append

* local countries
	local 				countries = "1 2 3 4"

	
************************************************************************
**# build data set
************************************************************************
	
* read in post data
	use					"$root/lsms_panel", replace
	drop 				if country == 5
	
* append baseline 
	append 				using "$eth/r0"
	append 				using "$mwi/r0"
	append 				using "$nga/r0"
	append 				using "$uga/r0"

* redo hhid
	drop 				hhid
	gen 				hhid_eth1 = "e" + hhid_eth if hhid_eth != ""
	gen					hhid_mwi1 = "m" + hhid_mwi if hhid_mwi != ""
	tostring			hhid_nga, gen(hhid_nga1)
	replace 			hhid_nga1 = "n" + hhid_nga1 if hhid_nga1 != "."
	replace				hhid_nga1 = "" if hhid_nga1 == "."
	
	tostring 			hhid_uga, gen(hhid_uga1) format("%12.0f")
	replace 			hhid_uga1 = baseline_hhid if baseline_hhid != ""
	replace 			hhid_uga1 = "u" + hhid_uga1 if hhid_uga1 != "."
	replace				hhid_uga1 = "" if hhid_uga1 == "."
	
	tostring			hhid_bf, gen(hhid_bf1)
	replace 			hhid_bf1 = "b" + hhid_bf1 if hhid_bf1 != "."
	replace				hhid_bf1 = "" if hhid_bf1 == "."
	
	gen					HHID = hhid_eth1 if hhid_eth1 != ""
	replace				HHID = hhid_mwi1 if hhid_mwi1 != ""
	replace				HHID = hhid_nga1 if hhid_nga1 != ""
	replace				HHID = hhid_uga1 if hhid_uga1 != ""
	replace				HHID = hhid_bf1 if hhid_bf1 != ""
	
	sort				HHID
	egen				hhid = group(HHID)
	drop				HHID hhid_eth1 hhid_mwi1 hhid_nga1 hhid_uga1 hhid_bf1
	lab var				hhid "Unique household ID"
	order 				country wave hhid resp_id hhid*

* set panel
	xtset				, clear
	xtset 				hhid 	
	
	
************************************************************************
**# clean for consistency
************************************************************************

* xfill geographic areas in uga and nigeria
	xfill 				region if country == 4, i(hhid) 
	xfill 				region zone lga if country == 3, i(hhid) 
	xfill 				ea if country == 3, i(hhid)
	drop 				state
	
	xfill 				zone if country == 1, i(hhid)
	replace 			ea = . if country == 1 & wave == 0
	xfill 				ea if country == 1 & wave == 0, i(hhid)
	
	xfill 				ta_code if country == 2, i(hhid)
	
	xfill 				district county town ward ea if country == 4, i(hhid)
	
* fix sector in nga 
	replace 			sector = 100 if sector == 2 & country == 3 	& wave == 0
	replace 			sector = 2 if sector == 1 & country == 3 & wave == 0
	replace 			sector = 1 if sector == 100 & country == 3 & wave == 0
	
* convert incomes into USD 
	ds 					*amnt_0 
	foreach 			var in `r(varlist)' {
		replace 			`var' = `var' * 0.0343 if country == 1
		replace 			`var' = `var' * 0.0014 if country == 2
		replace 			`var' = `var' * 0.0028 if country == 3
		replace 			`var' = `var' *  0.0003 if country == 4
	}
* https://www.exchangerates.org.uk/ETB-USD-spot-exchange-rates-history-2019.html
* https://www.exchangerates.org.uk/MWK-USD-spot-exchange-rates-history-2019.html
* https://www.exchangerates.org.uk/NGN-USD-spot-exchange-rates-history-2019.html
* https://www.exchangerates.org.uk/UGX-USD-spot-exchange-rates-history-2019.html	
	
* combine edu_act with sch_att var in post rounds
	replace 			edu_act = sch_att if edu_act >= . & sch_att != .
	replace 			edu_act = 1 if sch_att == 1

* gender in ethiopia 0 for consistency
	replace 			sexhh = 2 if sexhh == 0 & country == 1 & wave == 0
	
* exclude mwi wave 10 because sample is different (focus on early childhood development)
	drop 					if country == 2 & wave == 10 	
	

************************************************************************
**# generate standardized indices
************************************************************************

* make sure all post vars are coded as 0/1
	foreach 			var in rem_dom rem_for asst_inc oth_inc_1 oth_inc_2 ///
						oth_inc_3 oth_inc_4 oth_inc_5 isp_inc pen_inc gov_inc ///
						ngo_inc oth_inc asst_food asst_cash asst_kind farm_inc ///
						ag_crop ag_live bus_inc bus_inc_ind wage_inc wage_inc_ind {
		replace 			`var' = 0 if `var' == 2
	}
	
* all standard pre-post (index 1)
	* generate variables and fill with post data
	gen 				rem_std_pp = cond(rem_dom == 1 | rem_for == 1 | asst_inc == 1 ///
							| oth_inc_1 == 1 | oth_inc_2 == 1, 1, 0) if rem_dom != . | ///
							rem_for != . | asst_inc != . | oth_inc_1 != . | oth_inc_2 != .
						
	gen 				save_std_pp = cond(isp_inc == 1 | oth_inc_4 == 1, 1, 0) ///
							if isp_inc != . | oth_inc_4 != .
							
	gen 				pen_std_pp = cond(pen_inc == 1 | oth_inc_5 == 1, 1, 0) ///
							if pen_inc != . | oth_inc_5 != .
							
	gen 				asst_std_pp = cond(gov_inc == 1 | ngo_inc == 1 | oth_inc == 1 | ///
							asst_food == 1 | asst_cash == 1 | asst_kind == 1 | oth_inc_3 == 1, ///
							1, 0) if gov_inc != . | ngo_inc != . | oth_inc != . | asst_food != . ///
							| asst_cash != . | asst_kind != . 
	
	gen 				farm_std_pp = cond(farm_inc == 1 | ag_crop == 1 | ag_live == 1, 1, 0) ///
							if farm_inc != . | ag_crop != . | ag_live != . 
	
	gen 				nfe_std_pp = cond(bus_inc == 1 | bus_inc_ind == 1, 1, 0) if bus_inc != . ///
							| bus_inc_ind != .
							
	gen 				wage_std_pp = cond(wage_inc == 1 | wage_inc_ind == 1, 1, 0) if wage_inc ///
							!= . | wage_inc_ind != .
	
	* fill in variables with pre data
	ds 					*_std_pp 
	foreach 			var in `r(varlist)' {
		replace 			`var' = 0 if wave == 0
	}
	
	replace 			rem_std_pp = 1 if cash_trans_0 == 1 | food_trans_0 == 1 | kind_trans_0 == 1 ///
							| cash_child_0 == 1 | kind_child_0 == 1 | cash_for_0 == 1 | cash_dom_0 == 1 ///
							| kind_inc_0 == 1 | kind_dom_0 == 1 | kind_for_0 == 1

	replace				save_std_pp = 1 if rent_inc_0 == 1 | save_inc_0 == 1 | rent_nonag_0 == 1 | ///
							ag_rent_0 == 1 | interest_0 == 1
	
	replace 			pen_std_pp = 1 if pen_inc_0 == 1
	
	replace 			asst_std_pp = 1 if asset_0 == 1 | oth_inc_0 == 1 | asst_cash_0 == 1 | ///
							asst_food_0 == 1 | asst_kind_0 == 1 | masaf_0 == 1 | for_wrk_0 == 1 | ///
							asst_inc_0 == 1 | sage_0 == 1
							
	replace 			farm_std_pp = 1 if crop_inc_0 == 1 | live_inc_0 == 1 | live_prod_0 == 1 | ///
							tree_inc_0 == 1 
		
	replace 			nfe_std_pp = 1 if nfe_inc_0 == 1
	
	replace 			wage_std_pp = 1 if wage_emp_0 == 1 | casual_emp_0 == 1 | temp_emp_0 == 1
	
	* Fraction Standardized across all countries (1)
		* gen smallest geographic level with at leave 10 observations
		gen 			one = 1
		foreach 		l in region district zone woreda kebele county town ward ea ta_code lga {
			foreach v 		of varlist *_std_pp {
				egen 			`v'_count_`l' = max(`v'), by(`l')
			}
			egen			`l'_count = rowtotal(*_count_`l')
			egen 			`l'_obs = total(one), by(`l')
		}
		
		* generate combined variable that uses the smallest geographic area with at least 10 observations
		gen 			geo_count1 = region_count if region_obs >= 10 & country == 1 
		replace 		geo_count = zone_count if zone_obs >= 10 & country == 1 
		replace 		geo_count = woreda_count if woreda_obs >= 10 & country == 1 
		replace 		geo_count = kebele_count if kebele_obs >= 10 & country == 1 
		replace 		geo_count = ea_count if ea_obs >= 10 & country == 1
		
		replace 		geo_count = region_count if region_obs >= 10 & country == 2
		replace 		geo_count = ta_code_count if ta_code_obs >= 10 & country == 2
		
		replace 		geo_count = zone_count if zone_obs >= 10 & country == 3
		replace 		geo_count = region_count if region_obs >= 10 & country == 3 
		replace 		geo_count = lga_count if lga_obs >= 10 & country == 3
		replace 		geo_count = ea_count if ea_obs >= 10 & country == 3
		
		replace	 		geo_count = district_count if district_obs >= 10 & country == 4
		replace 		geo_count = county_count if county_obs >= 10 & country == 4
		replace 		geo_count = town_count if town_obs >= 10 & country == 4
		replace 		geo_count = ward_count if ward_obs >= 10 & country == 4
		replace 		geo_count = ea_count if ea_obs >= 10 & country == 4
		

		* generate index
		egen 				hh_count_pp = rowtotal(*_std_pp)
		gen 				std_pp_index = 1 - (hh_count_pp / geo_count1)
		lab var 			std_pp_index "Index 1"
		ds					*_std_pp 
		foreach 			var in `r(varlist)' {
			replace 			std_pp_index = . if `var' == .
		}
		drop 				*_count* *_obs 
		
		* only keep waves with sufficient data
		ds 					*std_pp 
		foreach 			var in `r(varlist)' {
			replace 			`var' = . if country == 3 & wave == 2 | wave == 3 | wave == 8
		}
		
* by country standard pre-post (index 2)	
	* Ethiopia (waves 1 - 6)
	gen 				rem_std_eth = cond(cash_trans_0 == 1 | kind_trans_0 == 1 | ///
							food_trans_0 == 1 , 1, 0) if country == 1 & wave == 0
	replace 			rem_std_eth = remit_inc if country == 1 & wave != 0 
	
	gen 				save_std_eth = cond(save_inc_0 == 1 | rent_inc_0 == 1, 1, 0) ///
							if country == 1 & wave == 0
	replace 			save_std_eth = isp_inc if country == 1 & wave != 0
	
	gen 				pen_std_eth = pen_inc_0 if country == 1 & wave == 0 
	replace 			pen_std_eth = pen_inc if country ==1 & wave != 0
	
	gen 				oth_std_eth = cond(asset_0 ==1 | oth_inc_0 == 1, 1, 0) ///
							if country == 1 & wave == 0
	replace 			oth_std_eth = oth_inc if country == 1 & wave != 0 
	
	gen 				farm_std_eth = cond(crop_inc_0 == 1 | live_inc_0 ==1 | live_prod_0 == 1, ///
							1, 0) if country == 1 & wave == 0
	replace 			farm_std_eth = farm_inc if country == 1 & wave != 0
	
	gen 				nfe_std_eth = nfe_inc_0 if country == 1 & wave == 0
	replace 			nfe_std_eth = bus_inc if country == 1 & wave != 0
	
	gen 				wage_std_eth = cond(wage_emp_0 == 1 | casual_emp_0 == 1 | temp_emp_0 == 1, ///
							1, 0) if country == 1 & wave == 0
	replace 			wage_std_eth = wage_inc if country == 1 & wave != 0
	
	gen 				food_std_eth = asst_food_0 if country == 1 & wave == 0
	replace 			food_std_eth = asst_food if country == 1 & wave != 0
	
	gen 				cash_std_eth = cond(asst_cash == 1 | gov_inc == 1 | ngo_inc == 1, 1, 0) ///
							if country == 1 & wave != 0
	replace 			cash_std_eth = asst_cash_0 if country == 1 & wave == 0
	
	gen 				kind_std_eth = asst_kind_0 if country == 1 & wave == 0
	replace 			kind_std_eth = asst_kind if country == 1 & wave != 0
	
	* generate ethiopia index 
	* gen smallest geographic level with at leave 10 observations
	foreach 		l in region zone woreda kebele ea {
		foreach v 		of varlist *_std_eth {
			egen 			`v'_count_`l' = max(`v'), by(`l')
		}
		egen			`l'_count = rowtotal(*_count_`l')
		egen 			`l'_obs = total(one), by(`l')
	}
	
	* generate combined variable that uses the smallest geographic area with at least 10 observations
	gen 			geo_count_eth = region_count if region_obs >= 10 & country == 1 
	replace 		geo_count = zone_count if zone_obs >= 10 & country == 1 
	replace 		geo_count = woreda_count if woreda_obs >= 10 & country == 1 
	replace 		geo_count = kebele_count if kebele_obs >= 10 & country == 1 
	replace 		geo_count = ea_count if ea_obs >= 10 & country == 1
	
	* generate index
	egen 				hh_count_eth = rowtotal(*_std_eth)
	gen 				eth_pp_index = 1 - (hh_count_eth / geo_count_eth)
	
	drop 				*_count* *_obs 
	
	ds 					*_std_eth 
	foreach 			var in `r(varlist)' {
		replace 			eth_pp_index = . if `var' == .
	}
		
	* Malawi (waves 1-5, 7, 9, 11)
	gen 				nfe_std_mwi = cond(bus_inc == 1 | bus_inc_ind == 1, 1, 0) ///
							if country == 2 & wave != 0
	replace 			nfe_std_mwi = nfe_inc_0 if country == 2 & wave == 0
	
	gen 				wage_std_mwi = cond(wage_emp_0 == 1 | casual_emp_0 == 1, 1, 0) ///
							if country == 2 & wave == 0
	replace 			wage_std_mwi = wage_inc if country == 2 & wave != 0 
	replace 			wage_std_mwi = wage_inc_ind if country == 2 & wave == 5 
	
	gen 				save_std_mwi = cond(rent_inc_0 == 1 | save_inc_0 == 1, 1, 0) ///
							if country == 2 & wave == 0
	replace 			save_std_mwi = isp_inc if country == 2 & wave != 0
	replace 			save_std_mwi = oth_inc_4 if country == 2 & wave == 5
	
	gen 				pen_std_mwi = cond(pen_inc== 1 | oth_inc_5 == 1, 1, 0) ///
							if country == 2 & wave  != 0
	replace 			pen_std_mwi = pen_inc_0 if country == 2 & wave == 0
	
	gen 				rem_std_mwi = cond(rem_dom == 1 | rem_for == 1 | asst_inc == 1 , 1, 0) ///
							if country == 2 & wave != 0
	replace 			rem_std_mwi = cond(cash_trans_0 == 1 | food_trans_0 == 1 | kind_trans_0  ///
							== 1 | cash_child_0 == 1 | kind_child_0 == 1, 1, 0) if country == 2 ///
							& wave == 0
	
	gen 				farm_std_mwi = cond(farm_inc == 1 | ag_crop == 1 | ag_live == 1, 1, 0) ///
							if country == 2 & wave != 0
	replace 			farm_std_mwi = cond(crop_inc_0 == 1 | tree_inc_0 == 1 | live_inc_0 == 1 ///
							| live_prod_0 == 1, 1, 0) if country == 2 & wave == 0
							
	gen 				oth_std_mwi = cond(gov_inc == 1 | ngo_inc == 1 | oth_inc == 1 | ///
							asst_food == 1 | asst_cash == 1 | asst_kind == 1, 1, 0) ///
							if country == 2 & wave != 0
	replace 			oth_std_mwi = cond(asst_cash_0 == 1 | for_wrk_0 == 1 | masaf_0 == 1 | ///
							asst_food_0 == 1 | oth_inc_0 == 1 | asset_0 == 1, 1, 0) ///
							if country == 2 & wave == 0 
	
	* generate mwi index 
	foreach 		l in region ta_code {
		foreach v 		of varlist *_std_mwi {
			egen 			`v'_count_`l' = max(`v'), by(`l')
		}
		egen			`l'_count = rowtotal(*_count_`l')
		egen 			`l'_obs = total(one), by(`l')
	}
	
	* generate combined variable that uses the smallest geographic area with at least 10 observations
	gen 	 		geo_count_mwi = region_count if region_obs >= 10 & country == 2
	replace 		geo_count = ta_code_count if ta_code_obs >= 10 & country == 2

	* generate index
	egen 				hh_count_mwi = rowtotal(*_std_mwi)
	gen 				mwi_pp_index = 1 - (hh_count_mwi / geo_count_mwi)
	
	drop 				*_count* *_obs 
	
	ds 					*_std_mwi 
	foreach 			var in `r(varlist)' {
		replace 			mwi_pp_index = . if `var' == .
	}

	* Nigeria (waves 1, 4, & 9)
	gen 				farm_std_nga = cond(crop_inc_0 == 1 | tree_inc_0 == 1 | live_inc_0 == 1 | ///
							live_prod_0 == 1, 1, 0) if country == 3 & wave == 0
	replace 			farm_std_nga = farm_inc if country == 3 & wave != 0
	
	gen 				nfe_std_nga = nfe_inc_0 if country == 3 & wave == 0
	replace 			nfe_std_nga = bus_inc if country == 3 & wave != 0
	
	gen 				wage_std_nga = wage_emp_0 if country == 3 & wave == 0
	replace 			wage_std_nga = wage_inc if country == 3 & wave != 0
	
	gen 				oth_std_nga = cond(gov_inc == 1 | ngo_inc == 1 | asst_cash == 1 | ///
							asst_kind == 1 | asst_food == 1 | oth_inc == 1, 1, 0) ///
							if country == 3 & wave != 0
	replace 			oth_std_nga = cond(oth_inc_0 == 1 | asst_inc_0 == 1, 1, 0) ///
							if country == 3 & wave == 0
	
	gen 				rem_std_nga = cond(rem_dom == 1 | rem_for == 1 | asst_inc == 1, 1, 0) ///
							if country == 3 & wave != 0
	replace 			rem_std_nga = cond(cash_for_0 == 1 | cash_dom_0 == 1 | kind_inc_0 == 1, 1, 0) ///
							if country == 3 & wave == 0
							
	gen 				save_std_nga = cond(save_inc_0 == 1 | rent_nonag_0 == 1 | ag_rent_0 == 1, ///
							1, 0) if country == 3 & wave == 0 
	replace 			save_std_nga = isp_inc if country == 3 & wave != 0
	
	gen 				pen_std_nga = pen_inc if country == 3 & wave != 0
	replace 			pen_std_nga = pen_inc_0 if country == 3 & wave == 0
	
	* generate nga index 
	foreach 		l in  zone region lga ea {
		foreach v 		of varlist *_std_nga {
			egen 			`v'_count_`l' = max(`v'), by(`l')
		}
		egen			`l'_count = rowtotal(*_count_`l')
		egen 			`l'_obs = total(one), by(`l')
	}
	
	* generate combined variable that uses the smallest geographic area with at least 10 observations
	gen 			geo_count_nga = zone_count if zone_obs >= 10 & country == 3
	replace 		geo_count = region_count if region_obs >= 10 & country == 3 
	replace 		geo_count = lga_count if lga_obs >= 10 & country == 3
	replace 		geo_count = ea_count if ea_obs >= 10 & country == 3

	* generate index
	egen 				hh_count_nga = rowtotal(*_std_nga)
	gen 				nga_pp_index = 1 - (hh_count_nga / geo_count_nga)
	
	drop 				*_count* *_obs 
	
	ds 					*_std_nga
	foreach 			var in `r(varlist)' {
		replace 			nga_pp_index = . if `var' == .
	}
	
	* Uganda (waves 1-5)
	gen 				save_std_uga = isp_inc if country == 4 & wave != 0
	replace 			save_std_uga = cond(interest_0 == 1 | rent_inc_0 == 1, 1, 0) ///
							if country == 4 & wave == 0
	
	gen 				pen_std_uga = pen_inc if country == 4 & wave != 0
	replace 			pen_std_uga = pen_inc_0 if country == 4 & wave == 0
	
	gen 				rem_std_uga = cond(rem_dom == 1 | rem_for == 1 | asst_inc == 1 , 1, 0) ///
							if country == 4 & wave != 0
	replace 			rem_std_uga = cond(cash_dom_0 == 1 | cash_for_0 == 1 | kind_dom_0 == 1 | ///
							kind_for_0 == 1, 1, 0) if country == 4 & wave == 0
							
	gen 				oth_std_uga = cond(oth_inc == 1 | gov_inc == 1 | ngo_inc == 1, 1, 0) ///
							if country == 4 & wave != 0
	replace 			oth_std_uga = oth_inc_0 if country == 4 & wave == 0
	
	gen 				wage_std_uga = wage_emp_0 if country == 4 & wave == 0
	replace 			wage_std_uga = wage_inc if country == 4 & wave != 0
	
	gen 				farm_std_uga = cond(live_inc_0 == 1 | crop_inc_0 == 1 | live_prod_0 == 1, 1, 0) ///
							if country == 4 & wave == 0
	replace 			farm_std_uga = farm_inc if country == 4 & wave != 0
	
	gen 				nfe_std_uga = nfe_inc_0 if country == 4 & wave == 0
	replace 			nfe_std_uga = bus_inc if country ==4 & wave != 0 
	
	gen 				asst_std_uga = cond(asst_cash == 1 | asst_food == 1 | asst_kind == 1, 1, 0) ///
							if country == 4 & wave != 0
	replace 			asst_std_uga = sage_0 if country == 4 & wave == 0
	
	* generate uga index 
	foreach 		l in district county town ward ea {
		foreach v 		of varlist *_std_uga {
			egen 			`v'_count_`l' = max(`v'), by(`l')
		}
		egen			`l'_count = rowtotal(*_count_`l')
		egen 			`l'_obs = total(one), by(`l')
	}
	
	* generate combined variable that uses the smallest geographic area with at least 10 observations
	gen 	 		geo_count_uga = district_count if district_obs >= 10 & country == 4
	replace 		geo_count = county_count if county_obs >= 10 & country == 4
	replace 		geo_count = town_count if town_obs >= 10 & country == 4
	replace 		geo_count = ward_count if ward_obs >= 10 & country == 4
	replace 		geo_count = ea_count if ea_obs >= 10 & country == 4
	
	* generate uga index 
	egen 				hh_count_uga = rowtotal(*_std_uga)
	gen 				uga_pp_index = 1 - (hh_count_uga / geo_count_uga)

	drop 				*_count* *_obs 
	
	ds 					*_std_uga
	foreach 			var in `r(varlist)' {
		replace 			uga_pp_index = . if `var' == .
	}
	
	* combine countries into one variable
	gen 				pp_index = eth_pp_index if country == 1
	replace 			pp_index = mwi_pp_index if country == 2
	replace 			pp_index = nga_pp_index if country == 3
	replace 			pp_index = uga_pp_index if country == 4
	lab var 			pp_index "Index 2"
	
* all standard pre (indices 3-4)
	* Malawi
	gen 				rem_std_pre = cond(cash_trans_0 == 1, 1, ///
							cond(cash_child_0 == 1, 1, 0)) if country == 2 ///
							& wave == 0
	gen 				rem_std_amnt_pre = cash_trans_amnt_0 + ///
							cash_child_amnt_0 if country == 2 & wave == 0
	
	gen 				kind_std_pre = cond(kind_trans_0 == 1, 1, ///
							cond(kind_child_0 == 1, 1, cond(food_trans_0 == 1, ///
							1, 0))) if country == 2 & wave == 0
	gen 				kind_std_amnt_pre = kind_trans_amnt_0 + kind_child_amnt_0 /// 
							+ food_trans_amnt_0 if country == 2 & wave == 0
							
	gen 				save_std_pre = save_inc_0 if country == 2 & wave == 0
	gen 				save_std_amnt_pre = save_inc_amnt_0 if country == 2 & ///
							wave == 0
	
	gen 				rent_std_pre = rent_inc_0 if country == 2 & wave == 0
	gen 				rent_std_amnt_pre = rent_inc_amnt_0 if country == 2 & ///
							wave == 0
							
	gen 				pen_std_pre = pen_inc_0 if country == 2 & wave == 0
	gen 				pen_std_amnt_pre = pen_inc_amnt_0 if country == 2 & ///
							wave == 0
	
	gen 				nfe_std_pre = nfe_inc_0 if country == 2 & wave == 0
	gen 				nfe_std_amnt_pre = nfe_inc_amnt_0 if country == 2 & ///
							wave == 0
							
	gen 				crop_std_pre = cond(crop_inc_0 == 1, 1, ///
							cond(tree_inc_0 == 1, 1, 0)) if country == 2 & wave == 0
	gen 				crop_std_amnt_pre = crop_inc_amnt_0 + tree_inc_amnt_0 if country == 2 ///
							& wave == 0
							
	gen 				live_std_pre = cond(live_inc_0 == 1, 1, 0) if country == 2 & wave == 0
	gen 				live_std_amnt_pre = live_inc_amnt_0 if country == 2 & wave == 0
		
	gen 				live_prod_std_pre = live_prod_0 if country == 2 & wave == 0
	gen 				live_prod_std_amnt_pre = live_prod_amnt_0 if country == 2 & ///
							wave == 0
							
	gen 				wage_std_pre = cond(casual_emp_0 == 1, 1, ///
							cond(wage_emp_0 == 1, 1, 0)) if country == 2 & wave == 0
	gen 				wage_std_amnt_pre = casual_emp_amnt_0 + wage_emp_amnt_0 ///
							if country == 2 & wave == 0
							
	gen 				asst_std_pre = cond(asst_cash_0 == 1, 1, ///
							cond(masaf_0 == 1, 1, cond(for_wrk_0 == 1, 1, ///
							cond(asst_food_0 == 1, 1, 0)))) if country == 2 & wave == 0
	gen 				asst_std_amnt_pre = asst_cash_amnt_0 + masaf_amnt_0 + for_wrk_amnt_0 ///
							+ asst_food_amnt_0 if country == 2 & wave == 0
							
	gen 				oth_std_pre = cond(asset_0 == 1, 1, cond(oth_inc_0 == 1, 1, 0)) ///
							if country == 2 & wave == 0
	gen 				oth_std_amnt_pre = asset_amnt_0 + oth_inc_amnt_0 if country == 2 & ///
							wave == 0

	* Nigeria 
	replace 			rem_std_pre = cond(cash_for_0 == 1 | cash_dom_0 == 1, 1, 0) ///
							if country == 3 & wave == 0
	replace 			rem_std_amnt_pre = cash_for_amnt_0 + cash_dom_amnt_0 if ///
							country == 3 & wave == 0
							
	replace 			kind_std_pre = kind_inc_0 if country == 3 & wave == 0
	replace 			kind_std_amnt_pre = kind_inc_amnt_0 if country == 3 & wave == 0
	
	replace 			save_std_pre = save_inc_0 if country == 3 & wave == 0
	replace 			save_std_amnt_pre = save_inc_amnt_0 if country == 3 & wave == 0
	
	replace 			rent_std_pre = cond(rent_nonag_0 == 1 | ag_rent_0 == 1, 1, 0) ///
							if country == 3 & wave == 0
	replace 			rent_std_amnt_pre = rent_nonag_amnt_0 + ag_rent_amnt_0 ///
							if country == 3 & wave == 0
	
	replace 			pen_std_pre = pen_inc_0 if country == 3 & wave == 0
	replace 			pen_std_amnt_pre = pen_inc_amnt_0 if country == 3 & wave == 0
	
	replace 			nfe_std_pre = nfe_inc_0 if country == 3 & wave == 0
	replace 			nfe_std_amnt_pre = nfe_inc_amnt_0 if country == 3 & wave == 0
	
	replace 			crop_std_pre = cond(crop_inc_0 == 1 | tree_inc_0 == 1, 1, 0) ///
							if country == 3 & wave == 0
	replace 			crop_std_amnt_pre = crop_inc_amnt_0 + tree_inc_amnt_0 ///
							if country == 3 & wave == 0
	
	replace 			live_std_pre = cond(live_inc_0 == 1, 1, 0) ///
							if country == 3 & wave == 0
	replace 			live_std_amnt_pre = live_inc_amnt_0 if country == 3 & wave == 0
							
	replace 			live_prod_std_pre = live_prod_0 if country == 3 & wave == 0
	replace 			live_prod_std_amnt_pre = live_prod_amnt_0 if country == 3 & wave == 0
	
	replace 			wage_std_pre = wage_emp_0 if country == 3 & wave == 0
	replace 			wage_std_amnt_pre = wage_emp_amnt_0 if country == 3 & wave == 0
	
	replace 			asst_std_pre = asst_inc_0 if country == 3 & wave == 0
	replace 			asst_std_amnt_pre = asst_inc_amnt_0 if country == 3 & wave == 0
	
	replace 			oth_std_pre = oth_inc_0 if country == 3 & wave == 0
	replace 			oth_std_amnt_pre = oth_inc_amnt_0 if country == 3 & wave == 0
	
	* Uganda 
	replace 			rem_std_pre = cond(cash_dom_0 == 1 | cash_for_0 == 1, 1, 0 ) ///
							if country == 4 & wave == 0
	replace 			rem_std_amnt_pre = cash_dom_amnt_0 + cash_for_amnt_0 ///
							if country == 4 & wave == 0
	
	replace 			kind_std_pre = cond(kind_dom_0 == 1 | kind_for_0 == 1, 1, 0) ///
							if country == 4 & wave == 0
	replace 			kind_std_amnt_pre = kind_dom_amnt_0 + kind_for_amnt_0 ///
							if country == 4 & wave == 0
							
	replace 			save_std_pre = interest_0 if country == 4 & wave == 0
	replace 			save_inc_amnt_0 = interest_amnt_0 if country == 4 & wave == 0
	
	replace 			rent_std_pre = rent_inc_0 if country == 4 & wave == 0
	replace 			rent_std_amnt_pre = rent_inc_amnt_0 if country == 4 & wave == 0
	
	replace 			pen_std_pre = pen_inc_0 if country == 4 & wave == 0
	replace 			pen_std_amnt_pre = pen_inc_amnt_0 if country == 4 & wave == 0
	
	replace 			nfe_std_pre = nfe_inc_0 if country == 4 & wave == 0
	replace 			nfe_std_amnt_pre = nfe_inc_amnt_0 if country == 4 & wave == 0
	
	replace 			crop_std_pre = crop_inc_0 if country == 4 & wave == 0
	replace 			crop_std_amnt_pre = crop_inc_amnt_0 if country == 4 & wave == 0
	
	replace 			live_std_pre = live_inc_0 if country == 4 & wave == 0
	replace 			live_std_amnt_pre = live_inc_amnt_0 if country == 4 & wave == 0
	
	replace 			live_prod_std_pre = live_prod_0 if country == 4 & wave == 0
	replace 			live_prod_std_amnt_pre = live_prod_amnt_0 if country == 4 & wave == 0
	
	replace 			wage_std_pre = wage_emp_0 if country == 4 & wave == 0
	replace 			wage_std_amnt_pre = wage_emp_amnt_0 if country == 4 & wave == 0
	
	replace 			asst_std_pre = sage_0 if country == 4 & wave == 0
	replace 			asst_std_amnt_pre = sage_amnt_0 if country == 4 & wave == 0
	
	replace 			oth_std_pre = oth_inc_0 if country == 4 & wave == 0
	replace 			oth_std_amnt_pre = oth_inc_amnt_0 if country == 4 & wave == 0
	
	* Ethiopia 
	replace 			rem_std_pre = cash_trans_0 if country == 1 & wave == 0
	replace 			rem_std_amnt_pre = cash_trans_amnt_0 if country == 1 & wave == 0
	
	replace 			kind_std_pre = cond(kind_trans_0 == 1 | food_trans_0 == 1, 1, 0) ///
							if country == 1 & wave == 0
	replace 			kind_std_amnt_pre = kind_trans_amnt_0 + food_trans_amnt_0 ///
							if country == 1 & wave == 0
	
	replace 			save_std_pre = save_inc_0 if country == 1 & wave == 0
	replace 			save_std_amnt_pre = save_inc_amnt_0 if country == 1 & wave == 0
	
	replace 			rent_std_pre = rent_inc_0 if country == 1 & wave == 0
	replace 			rent_std_amnt_pre = rent_inc_amnt_0 if country == 1 & wave == 0
	
	replace 			pen_std_pre = pen_inc_0 if country == 1 & wave == 0
	replace 			pen_std_amnt_pre = pen_inc_amnt_0 if country == 1 & wave == 0
	
	replace 			nfe_std_pre = nfe_inc_0 if country == 1 & wave == 0
	replace 			nfe_std_amnt_pre = nfe_inc_amnt_0 if country == 1 & wave == 0
	
	replace 			crop_std_pre = crop_inc_0 if country == 1 & wave == 0
	replace 			crop_std_amnt_pre = crop_inc_amnt_0 if country == 1 & wave == 0
	
	replace 			live_std_pre = live_inc_0 if country == 1 & wave == 0
	replace 			live_std_amnt_pre = live_inc_amnt_0 if country == 1 & wave == 0
	
	replace 			live_prod_std_pre = live_prod_0 if country == 1 & wave == 0
	replace 			live_prod_std_amnt_pre = live_prod_amnt_0 if country == 1 & wave == 0
	
	replace 			wage_std_pre = cond(wage_emp_0 == 1 | casual_emp_0 == 1 | temp_emp_0 == 1, ///
							1, 0) if country == 1 & wave == 0
	replace 			wage_std_amnt_pre = wage_emp_amnt_0 + casual_emp_amnt_0 + temp_emp_amnt_0 ///
							if country == 1 & wave == 0
							
	replace 			asst_std_pre = cond(asst_kind_0 == 1 | asst_cash_0 == 1 | asst_food_0 == 1, ///
							1, 0) if country == 1 & wave == 0
	replace 			asst_std_amnt_pre = asst_kind_amnt_0 + asst_cash_amnt_0 + asst_food_amnt_0 ///
							if country == 1 & wave == 0
	
	replace 			oth_std_pre = cond(asset_0 == 1 | oth_inc_0 == 1, 1, 0) ///
							if country == 1 & wave == 0
	replace 			oth_std_amnt_pre = asset_amnt_0 + oth_inc_amnt_0 if country == 1 & wave == 0
	
	* Fraction Index (3)
	* gen smallest geographic level with at leave 10 observations
		foreach 		l in region district zone woreda kebele county town ward ea ta_code lga {
			foreach v 		of varlist *_std_pre {
				egen 			`v'_count_`l' = max(`v'), by(`l')
			}
			egen			`l'_count = rowtotal(*_count_`l')
			egen 			`l'_obs = total(one), by(`l')
		}
		
		* generate combined variable that uses the smallest geographic area with at least 10 observations
		gen 			geo_count3 = region_count if region_obs >= 10 & country == 1 
		replace 		geo_count3 = zone_count if zone_obs >= 10 & country == 1 
		replace 		geo_count3 = woreda_count if woreda_obs >= 10 & country == 1 
		replace 		geo_count3 = kebele_count if kebele_obs >= 10 & country == 1 
		replace 		geo_count3 = ea_count if ea_obs >= 10 & country == 1
		
		replace 		geo_count3 = region_count if region_obs >= 10 & country == 2
		replace 		geo_count3 = ta_code_count if ta_code_obs >= 10 & country == 2
		
		replace 		geo_count3 = zone_count if zone_obs >= 10 & country == 3
		replace 		geo_count3 = region_count if region_obs >= 10 & country == 3 
		replace 		geo_count3 = lga_count if lga_obs >= 10 & country == 3
		replace 		geo_count3 = ea_count if ea_obs >= 10 & country == 3
		
		replace	 		geo_count3 = district_count if district_obs >= 10 & country == 4
		replace 		geo_count3 = county_count if county_obs >= 10 & country == 4
		replace 		geo_count3 = town_count if town_obs >= 10 & country == 4
		replace 		geo_count3 = ward_count if ward_obs >= 10 & country == 4
		replace 		geo_count3 = ea_count if ea_obs >= 10 & country == 4
		

		* generate index
		egen 				hh_count = rowtotal(*_std_pre) if wave == 0
		gen 				std_pre_index_frac = 1 - (hh_count / geo_count3) if wave == 0
		lab var 			std_pre_index_frac "Index 3"
	
		drop 				*_count* *_obs one 
		
	* HHI Index (4) 
	egen 			tot_inc_usd = rowtotal(*_amnt_pre) if wave == 0
	ds 				*_amnt_pre 
	foreach 		var in `r(varlist)' {
		gen 		`var'_persq = ((`var' / tot_inc_usd) * 100)^2
	}
	
	egen 			std_pre_index_hhi = rowtotal(*_amnt_pre_persq) if wave == 0
	drop 			*_persq 
	
	lab var 		std_pre_index_hhi "Index 4"
	
* NOTE: indices 5 and 6 generated in baseline cleaning files 
	gen 			pre_index_frac = eth_pre_index_geo if country == 1
	replace 		pre_index_frac = mwi_pre_index_geo if country == 2
	replace 		pre_index_frac = nga_pre_index_geo if country == 3
	replace 		pre_index_frac = uga_pre_index_geo if country == 4
	lab var 		pre_index_frac "Index 5"
	
	gen 			pre_index_hhi = eth_pre_index_hhi if country == 1
	replace 		pre_index_hhi = mwi_pre_index_hhi if country == 2
	replace 		pre_index_hhi = nga_pre_index_hhi if country == 3
	replace 		pre_index_hhi = uga_pre_index_hhi if country == 4
	lab var 		pre_index_hhi "Index 6"

	
************************************************************************
**# change hhi scale to 0-1
************************************************************************
	* make hhi on scale of 1/n to 1 instead of to 10,000
	ds 					*hhi
	foreach 			var in `r(varlist)' {
		replace 			`var' = `var'/10000
	}
	
		
* **********************************************************************
**# combine fies and ld datasets
* **********************************************************************

* prep fies baseline 
	preserve
		use 			"$data/analysis/food_security/ld_reg_data", clear
		keep 			if wave <= 0 & (country == 1 | country == 2 | country == 3)
		tempfile 		fies
		save 			`fies'
	restore

* merge fies baseline, only keeping households also in post rounds
	preserve 
		keep 			if country == 1 | country == 2 | country == 3
		merge 			1:1 hhid_eth hhid_mwi hhid_nga country wave using `fies'	
		keep 			if _m != 1 // drop if hh not in post rounds
		drop 			_m
		tempfile 		pre
		save 			`pre'
	restore 

* prep fies post rounds
	preserve
		use 			"$data/analysis/food_security/ld_reg_data", clear
		keep			if wave > 0 & (country == 1 | country == 2 | country == 3)
		tempfile		post 
		save 			`post'
	restore

* merge post and append pre rounds
	preserve 
		keep 			if country == 1 | country == 2 | country == 3
		drop 			if wave <= 0
		merge 			1:1 hhid_eth hhid_mwi hhid_nga wave using `post', nogen
		append 			using `pre'
		tempfile 		c
		save 			`c'
	restore  

* replace data with fies merged version 	
	drop 				if country == 1 | country == 2 |country == 3
	append 				using `c'

* drop wave 1 in nigeria and ethiopia because only ask 3 fies qs -> inconsistent
	rename 				std_fsi_wt std_fs
	ds *_fs 
	foreach 			var in `r(varlist)' {
		replace 			`var' = . if (country == 3 | country == 1) & wave == 1
	}
	

* **********************************************************************
**# format & merge covid stringency index
* **********************************************************************

preserve 

* import data from https://ourworldindata.org/covid-stringency-index
	insheet 			using "$data/shocks/raw/covid_stringency_index.csv", clear
	keep 				if location == "Ethiopia" | location == "Malawi" | ///
							location == "Nigeria" | location == "Uganda"
	gen 				country = 1 if location == "Ethiopia"
	replace 			country = 2 if location == "Malawi"
	replace 			country = 3 if location == "Nigeria" 
	replace 			country = 4 if location == "Uganda" 
							
* generate day month year variables
	split 				date, parse("-")
	drop 				date
	destring 			date*, replace
	rename 				date1 year
	rename 				date2 month
	rename 				date3 day
	
* bin data to match waves (from end date of prior wave to end date of current wave)
	* ethiopia 
	gen 				wave = 1 if country == 1 & year == 2020 & ///
							((month == 4 & day >= 22) | (month == 5 & day <= 13))
	replace 			wave = 2 if country == 1 & year == 2020 & ///
							((month == 5 & day > 13) | (month == 6 & day <= 3))
	replace 			wave = 3 if country == 1 & year == 2020 & ///
							((month == 6 & day > 4) | (month == 6 & day <= 26))
	replace 			wave = 4 if country == 1 & year == 2020 & ///
							((month == 6 & day > 26) | month == 7 | ///
							(month == 8 & day <=14))
	replace 			wave = 5 if country == 1 & year == 2020 & ///
							((month == 8 & day > 14) | (month == 9 & day <= 17))
	replace 			wave = 6 if country == 1 & year == 2020 & ///
							((month == 9 & day > 17) | (month == 10 & day <= 13))
	replace 			wave = 7 if country == 1 & year == 2020 & ///
							((month == 10 & day > 13) | (month == 11 & day <= 10))
	replace 			wave = 8 if country == 1 & year == 2020 & ///
							((month == 11 & day > 10) | (month == 12 & day <= 21)) 
	replace 			wave = 9 if country == 1 & ///
							((year == 2020 & month == 12 & day > 21) | ///
							(year == 2021 & month == 1 & day <= 22))
	* malawi 
	replace 			wave = 1 if country == 2 & year == 2020 & ///
							((month == 5 & day >= 26) | (month == 6 & day <= 14))
	replace 			wave = 2 if country == 2 & year == 2020 & ///
							((month == 6 & day > 14 ) | (month == 7 & day <= 31))
	replace 			wave = 3 if country == 2 & year == 2020 & ///
							(month == 8 & day <= 27 ) 
	replace 			wave = 4 if country == 2 & year == 2020 & ///
							((month == 8 & day > 27 ) | month == 9 | ///
							(month == 10 & day <= 1))
	replace 			wave = 5 if country == 2 & year == 2020 & ///
							((month == 10 & day > 1) | (month == 11 & day <= 16))
	replace 			wave = 6 if country == 2 & year == 2020 & ///
							((month == 11 & day > 16) | (month == 12 & day <= 29))
	replace 			wave = 7 if country == 2 & ///
							((year == 2020 & month == 12 & day > 29) | ///
							(year == 2021 & month == 1) | ///
							(year == 2021 & month == 2 & day <= 6))
	replace 			wave = 8 if country == 2 & year == 2021 & ///
							((month == 2 & day > 6) | (month == 3 & day <= 15))
	replace 			wave = 9 if country == 2 & year == 2021 & ///
							((month == 3 & day > 15) | (month == 4 & day <= 23))
	replace 			wave = 10 if country == 2 & year == 2021 & ///
							((month == 4 & day > 23) | (month == 5 & day <= 14))
	replace 			wave = 11 if country == 2 & year == 2021 & ///
							((month == 5 & day > 14) | (month == 6 & day <= 9))
	* nigeria
	replace 			wave = 1 if country == 3 & year == 2020 & ///
							((month == 4 & day >= 20 ) | (month == 5 & day <= 11))		
	replace 			wave = 2 if country == 3 & year == 2020 & ///
							((month == 5 & day > 11 ) | (month == 6 & day <= 16))
	replace 			wave = 3 if country == 3 & year == 2020 & ///
							((month == 6 & day > 16 ) |(month == 7 & day <= 16))
	replace 			wave = 4 if country == 3 & year == 2020 & ///
							((month == 7 & day > 16) | (month == 8 & day <= 24))
	replace 			wave = 5 if country == 3 & year == 2020 & ///
							((month == 8 & day > 24) | (month == 9 & day <= 21))
	replace 			wave = 6 if country == 3 & year == 2020 & ///
							((month == 9 & day > 21) | (month == 10 & day <= 24))
	replace 			wave = 7 if country == 3 & year == 2020 & ///
							((month == 10 & day > 24) | (month == 11 & day <= 23))
	replace 			wave = 8 if country == 3 & year == 2020 & ///
							((month == 11 & day > 23) | (month == 12 & day <= 21))
	replace 			wave = 9 if country == 3 & ///
							((year == 2020 & month == 12 & day > 21) | ///
							(year == 2021 & month == 1 & day <= 25))
	replace 			wave = 10 if country == 3 & year == 2021 & ///
							((month == 1 & day > 25) | (month == 2 & day <= 22))
	replace 			wave = 11 if country == 3 & year == 2021 & ///
							((month == 2 & day > 22) | (month == 3 & day <= 28))
	* uganda 
	replace 			wave = 1 if country == 4 & year == 2020 & ///
							month == 6 & day >= 3 & day <= 20
	replace 			wave = 2 if country == 4 & year == 2020 & ///
							((month == 6 & day > 20) | month == 7 | ///
							(month == 8 & day <= 21))
	replace 			wave = 3 if country == 4 & year == 2020 & ///
							((month == 8 & day > 21) | month == 9 & day <= 30)
	replace 			wave = 4 if country == 4 & year == 2020 & ///
							((month == 10) | month == 11 & day <= 17)	
	replace 			wave = 5 if country == 4 & ///
							((year == 2020 & month == 11 & day > 17) | ///
							(year == 2020 & month == 12) | ///
							(year == 2021 & month == 1)	| ///
							(year == 2021 & month == 2 & day < 21))
				
* take average stringency score by wave
	drop 				if wave == .
	collapse 			(mean) stringency_index, by(country wave)

* save tempfile  
	tempfile 			stringency
	save 				`stringency'
restore 

* merge stringency score with panel 
	merge 				m:1 country wave using `stringency', nogen
	drop 				if hhid == . //one obs dropped (dropped mwi 10 stringency score )
	lab var 			stringency_index "COVID-19 Stringency Index"

	
************************************************************************
**# calculate weights
************************************************************************	

* xfill survey weights from wave 1 in baseline
	gen 				weight = hhw_cs if wave == 1
	xfill 				weight, i(hhid)
	
	* drop households in baseline that are not in post-covid data
	drop 				if weight == . & wave <= 0
	
	egen 				avg_weight = mean(hhw_cs) if wave >= 1, by(hhid)
	replace 			weight = avg_weight if wave > 0
	
* child weight 
	gen 				weight_child = chw_cs if wave == 1
	xfill 				weight_child, i(hhid)
	egen 				avg_weight_child = mean(chw_cs) if wave >= 1, by(hhid)
	replace 			weight_child = avg_weight_child if wave > 0
	
	drop 				avg_weight avg_weight_child 
	
	lab var 			weight "Household weight for diversification panel"
	lab var 			weight_child "Household weight for children in diversification panel"
	
	
************************************************************************
**# rename waves to month numbers
************************************************************************	

	gen 				wave_orig = wave

	replace 			wave = 14 if wave_orig == 10 & country == 1
	replace 			wave = 13 if wave_orig == 9 & country == 1
	replace 			wave = 12 if wave_orig == 8 & country == 1
	replace 			wave = 11 if wave_orig == 7 & country == 1
	replace 			wave = 10 if wave_orig == 6 & country == 1
	replace 			wave = 9 if wave_orig == 5 & country == 1
	replace 			wave = 8 if wave_orig == 4 & country == 1
	replace 			wave = 6 if wave_orig == 3 & country == 1
	replace 			wave = 5 if wave_orig == 2 & country == 1
	replace 			wave = 4 if wave_orig == 1 & country == 1
	
	replace 			wave = 18 if wave_orig == 11 & country == 2
	replace 			wave = 17 if wave_orig == 10 & country == 2
	replace 			wave = 16 if wave_orig == 9 & country == 2
	replace 			wave = 15 if wave_orig == 8 & country == 2
	replace 			wave = 13 if wave_orig == 7 & country == 2
	replace 			wave = 12 if wave_orig == 6 & country == 2
	replace 			wave = 11 if wave_orig == 5 & country == 2
	replace 			wave = 9 if wave_orig == 4 & country == 2
	replace 			wave = 8 if wave_orig == 3 & country == 2 
	replace 			wave = 7 if wave_orig == 2 & country == 2
	replace 			wave = 6 if wave_orig == 1 & country == 2 
	
	replace 			wave = 14 if wave_orig == 10 & country == 3 
	replace 			wave = 13 if wave_orig == 9 & country == 3 
	replace 			wave = 12 if wave_orig == 8 & country == 3 
	replace 			wave = 11 if wave_orig == 7 & country == 3 
	replace 			wave = 10 if wave_orig == 6 & country == 3 
	replace 			wave = 9 if wave_orig == 5 & country == 3 
	replace 			wave = 8 if wave_orig == 4 & country == 3 
	replace 			wave = 7 if wave_orig == 3 & country == 3
	replace 			wave = 6 if wave_orig == 2 & country == 3
	replace 			wave = 5 if wave_orig == 1 & country == 3
	
	replace 			wave = 14 if wave_orig == 5 & country == 4	
	replace 			wave = 11 if wave_orig == 4 & country == 4
	replace 			wave = 9 if wave_orig == 3 & country == 4
	replace 			wave = 8 if wave_orig == 2 & country == 4
	replace 			wave = 6 if wave_orig == 1 & country == 4
	
	replace 			wave = 19 if wave_orig == 10 & country == 5
	replace 			wave = 16 if wave_orig == 9 & country == 5
	replace 			wave = 15 if wave_orig == 8 & country == 5 
	replace 			wave = 14 if wave_orig == 7 & country == 5 
	replace 			wave = 13 if wave_orig == 6 & country == 5 
	replace 			wave = 12 if wave_orig == 5 & country == 5	
	replace 			wave = 11 if wave_orig == 4 & country == 5
	replace 			wave = 10 if wave_orig == 3 & country == 5
	replace 			wave = 8 if wave_orig == 2 & country == 5
	replace 			wave = 6 if wave_orig == 1 & country == 5
	
	lab def 			months -1 "2019" 0 "2019" 4 "Apr20" 5 "May20" 6 "Jun20" 7 "Jul20" 8 "Aug20" ///
							9 "Sep20" 10 "Oct20" 11 "Nov20" 12 "Dec20" 13 "Jan21" 14 "Feb21" ///
							15 "Mar21" 16 "Apr21" 17 "May21" 18 "May21" 19 "Jun21"
	lab val				wave months
	lab var 			wave_orig "Original wave number"
	lab var 			wave "Month"
	
* post variable in uga (in fies data for others)
	replace 			post = cond(wave_orig == 0, 0, 1)
	
	
************************************************************************
**# end matters
************************************************************************	

* format & QC
	compress
	sort hhid wave_orig
	isid hhid wave_orig

* save 
	save 			"$export/ld_pnl", replace
	
* close log
	log 			close 
	
/* END */