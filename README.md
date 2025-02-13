# Coping or Hoping? Livelihood Diversification and Food Insecurity in the COVID-19 Pandemic: Replication Code

This README describes the directory structure & Stata packages necessary to replicate all analysis for the paper "Coping or Hoping? Livelihood Diversification and Food Insecurity in the COVID-19 Pandemic" in *Food Policy*. The work relies on the World Bank LSMS and World Bank COVID phone surveys. For more information and to access these surveys, visit the [World Bank Microdata Library][4]. The relevant surveys are available under under the [High-Frequency Phone Survey collection][2] and the [LSMS - Integrated Surveys on Agriculture (ISA) collection][3]. To replicate the analysis, one needs to download the LSMS-ISA data and merge it with the already cleaned phone survey data. The phone survey data was cleaned at part of another project and we provide the cleaned data in this repo. The replication code does the merging. We make no guarantee that variables not used in the analysis are cleaned or accurate. THe analysis is based on a [pre-analysis plan][1] filed with the Open Science Framework (OSF).

Last updated: February 2025. 

For issues or concerns with this repo, please contact Anna Josephson or Jeffrey Michler.

 ## Index
 
 - [Contributors](#contributors)
 - [Data](#data)
 - [Data cleaning](#data-cleaning)
 - [Developing Environment](#developing-environment)

## Contributors

* Ann Furbush (Writing - original draft, Formal Analysis, Data curation)
* Anna Josephson [aljosephson@arizona.edu] (Writing - review & editing, Supervision, Conceptualization)
* Talip Kilic (Resources, Funding acquisition, Conceptualization)
* Jeffrey D. Michler [jdmichler@arizona.edu] (Writing - review & editing, Writing - original draft, Supervision, Project administration, Formal analysis, Conceptualization)

## Data cleaning

### Pre-requisites

The data processing and analysis requires a number of user-written Stata programs:
   * 1. `blindschemes`
   * 2. `mdesc`
   * 3. `estout`
     4. `distinct`
     5. `winsor2`
     6. `palettes`
     7. `catplot`
     8. `grc1leg2`
     9. `colrspace`
     10. `xfill`

The `projectdo.do` file will help you install these.

## Developing Environment

### Step 1

Clone this  repository https://github.com/AIDELabAZ/livelihood_div. The general repo structure looks as follows:<br>

```stata
evolving_impacts_covid_africa
├────README.md
├────projectdo.do
├────LICENSE
├────.gitignore
├────country             /* one dir for each country */
└────analysis            /* overall analysis */
```

### Step 2

Open the projectdo.do file and update the global filepath with your username in Section 0 (a).

   ```
    if `"`c(username)'"' == "USERNAME" {
       	global 		code  	"C:/Users/USERNAME/git/livelihood_div"
		global 		data	"C:/Users/USERNAME/livelihood_div/data"
		global 		output  "C:/Users/USERNAME/livelihood_div/output"
    }
   ```

### Step 3

Set up the file structure on your local machine as outlined below: 

```stata
C:/Users/USERNAME/livelihood_div
├────output
│    ├──logs
│    ├──figures
│    └──tables
└────data
     ├──ethiopia
     │    ├──logs
     │    ├──refined
     │    └──raw
     │         └──wave_00
     ├──malawi
     │    ├──logs
     │    ├──refined
     │    └──raw
     │         └──wave_00
     ├──nigeria
     │    ├──logs
     │    ├──refined
     │    └──raw
     │         └──wave_00
     └──other
```

### Step 4

Download the LSMS-ISA microdata Stata files from World Bank Microdata Library. You will need to create an account with the World Bank if you do not already have one and will be asked to provide a reason for downloading the data. Once data are downloaded, save the data files to the corresponding folders created in Step 3. 

### Step 5

Run the `projectdo.do` file. Output tables and figures will be saved to the relevant subfolders in the `output` folder. 

[1]: https://osf.io/nu593
[2]: http://bit.ly/microdata-hfps
[3]: https://www.worldbank.org/en/programs/lsms/initiatives/lsms-ISA
[4]: https://microdata.worldbank.org/index.php/home
