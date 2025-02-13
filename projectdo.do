* Project: DIVERSIFICATION
* Created on: Sept 2021
* Created by: amf
* Edited by: jdm
* Edited on: 12 Feb 2025
* Stata v.18.0 

* does
	* establishes an identical workspace between users
	* sets globals that define absolute paths
	* serves as the starting point to find any do-file, dataset or output
	* loads any user written packages needed for analysis

* assumes
	* access to all data and code

* TO DO:
	* done

* **********************************************************************
* 0 - setup
* **********************************************************************

* set $pack to 0 to skip package installation
	global 			pack 	0
		
* Specify Stata version in use
    global stataVersion 18.0    // set Stata version
    version $stataVersion

	
* **********************************************************************
* 0 (a) - Create user specific paths
* **********************************************************************


* Define root folder globals
    if `"`c(username)'"' == "jdmichler" {
        global 		code  	"C:/Users/jdmichler/git/AIDELabAZ/livelihood_div"
		global 		data	"C:/Users/jdmichler/OneDrive - University of Arizona/livelihood_div/data"
		global 		output	"C:/Users/jdmichler/OneDrive - University of Arizona/livelihood_div/output"
    }

    if `"`c(username)'"' == "aljosephson" {
        global 		code  	"C:/Users/aljosephson/git/AIDELabAZ/livelihood_div"
		global 		data	"G:/.shortcut-targets-by-id/1wmQb8xn5Qop-2J14D7Z2rdKxJhcwfP_q/wb_covid/data"
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

	* install -xfill and nwcommands packages
		net install xfill, 	replace from(https://www.sealedenvelope.com/)
	
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
	run				"$code/ethiopia/eth_build_0"
	run 			"$code/malawi/mwi_build_0"
	run				"$code/nigeria/nga_build_0"
	
* run panel cleaning 
	run 			"$code/analysis/pnl_cleaning_div"
	
	
* **********************************************************************
* 2 - run analysis .do files
* **********************************************************************

	run 			"$code/analysis/ld_sum_stats"
	run 			"$code/analysis/ld_regressions"
	run 			"$code/analysis/revisions"
	run 			"$code/analysis/fies_revisions"

/* END */