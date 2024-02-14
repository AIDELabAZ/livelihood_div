* Project: DIVERSIFICATION
* Created on: Sept 2021
* Created by: amf
* Edited by: alj
* Edited on: 6 September 2022
* Stata v.17.0 

* does
	* establishes an identical workspace between users
	* sets globals that define absolute paths
	* serves as the starting point to find any do-file, dataset or output
	* loads any user written packages needed for analysis

* assumes
	* access to all data and code

* TO DO:
	* update pathways for users 

* **********************************************************************
* 0 - setup
* **********************************************************************

* set $pack to 0 to skip package installation
	global 			pack 	0
		
* Specify Stata version in use
    global stataVersion 17.0    // set Stata version
    version $stataVersion

	
* **********************************************************************
* 0 (a) - Create user specific paths
* **********************************************************************


* Define root folder globals
    if `"`c(username)'"' == "jdmichler" {
        global 		code  	"C:/Users/jdmichler/git/diversification"
		global 		data	"G:/My Drive/wb_covid/data"
		global 		output_f "G:/My Drive/wb_covid/output"
    }

    if `"`c(username)'"' == "annal" {
        global 		code  	"C:/Users/aljosephson/git/livelihood_div"
		global 		data	"C:/Users/aljosephson/OneDrive - University of Arizona/wb_covid/data"
		global 		output_f "C:/Users/aljosephson/OneDrive - University of Arizona/wb_covid/output"
    }

	if `"`c(username)'"' == "annfu" {
		global 		code  	"C:/Users/annfu/git/diversification"
		global 		data	"G:/My Drive/wb_covid/data"
		global 		output_f "G:/My Drive/wb_covid/output"
	}
	
		if `"`c(username)'"' == "lirro" {
		global 		code  	"C:/Users/lirro/Documents/GitHub/diversification"
		global 		data	"G:/My Drive/wb_covid/data"
		global 		output "G:/My Drive/wb_covid/output"
	}
	
	
	
* **********************************************************************
* 0 (b) - Check if any required packages are installed:
* **********************************************************************

* install packages if global is set to 1
if $pack == 0 {
	
	* for packages/commands, make a local containing any required packages
		loc userpack "blindschemes mdesc estout distinct winsor2 palettes catplot grc1leg2 colrspace" 
	
	* install packages that are on ssc	
		foreach package in `userpack' {
			capture : which `package', all
			if (_rc) {
				capture window stopbox rusure "You are missing some packages." "Do you want to install `package'?"
				if _rc == 0 {
					capture ssc install `package', replace
					if (_rc) {
						window stopbox rusure `"This package is not on SSC. Do you want to proceed without it?"'
					}
				}
				else {
					exit 199
				}
			}
		}

	* install -xfill- package
		net install xfill, replace from(https://www.sealedenvelope.com/)

	* update all ado files
		ado update, update

	* set graph and Stata preferences
		set scheme plotplain, perm
		set more off
}

/*
* **********************************************************************
* 1 - run household data cleaning .do file
* **********************************************************************

* run div do files for each country
	run					"$code/ethiopia/eth_build_0"
	run 				"$code/malawi/mwi_build_0"
	run					"$code/nigeria/nga_build_0"
	run					"$code/uganda/uga_build_0"
	//run 				"$code/burkina_faso/"
	
	
* run panel cleaning 
	run 				"$code/analysis/pnl_cleaning_div"
	
	
* **********************************************************************
* 2 - run analysis .do files
* **********************************************************************

*	run 				"$code/analysis/ld_sum_stats"
*	run 				"$code/analysis/ld_regressions"

/* END */