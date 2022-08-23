* Project: DIVERSIFICATION
* Created on: Sept 2021
* Created by: amf
* Edited by: alj
* Edited on: 23 August 2022
* Stata v.17.0 / 16.1

* does
	* establishes an identical workspace between users
	* sets globals that define absolute paths
	* serves as the starting point to find any do-file, dataset or output
	* loads any user written packages needed for analysis

* assumes
	* access to all data and code

* TO DO:
	* add all do-files

* **********************************************************************
* 0 - setup
* **********************************************************************

* set $pack to 0 to skip package installation
	global 			pack 	0
		
* Specify Stata version in use
    *global stataVersion 17.0    // set Stata version
	global stataVersion 16.1
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

    if `"`c(username)'"' == "aljosephson" {
        global 		code  	"C:/Users/aljosephson/git/AIDELabAZ/diversification"
		global 		data	"G:/My Drive/wb_covid/data"
		global 		output_f "G:/My Drive/wb_covid/output"
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
if $pack == 1 {
	
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


* **********************************************************************
* 1 - run household data cleaning .do file
* **********************************************************************

* run div do files for each country
	//run				"$code/ethiopia/"
	//run 				"$code/malawi/mwi_build_0"
	//run				"$code/nigeria/"
	//run				"$code/nigeria/"
	//run 				"$code/burkina_faso/"
	

* run panel cleaning 
	//run 				"$code/diversification/analysis/pnl_cleaning_div"
	
	
* **********************************************************************
* 2 - run analysis .do files
* **********************************************************************



/* END */