# Livelihood Diversification project(s)
This README describes the directory structure & should enable users to replicate all tables and figures for work related to the World Bank LSMS and World Bank COVID phone surveys, related to diversification and various projects by the contributors listed below. For more information and to access these phone surveys, visit the World Bank Microdata Library. The relevant surveys are available under under the High-Frequency Phone Survey collection: http://bit.ly/microdata-hfps.  

This repo repairs AIDELabAZ/diversification.

Last update: August 2022. 

 ## Index

 - [Introduction](#introduction)
 - [Data](#data)
 - [Data cleaning](#data-cleaning)
 - [Pre-requisites](#pre-requisites)
 - [Folder structure](#folder-structure)

## Introduction

Contributors:
* Anna Josephson
* Jeffrey D. Michler
* Ann Furbush 
* Talip Kilic 
* Lorin Rudin-Rush

Scripts various go through each step, from cleaning raw data to analysis.

## Data 

The publicly-available data for each survey round is coupled with a basic information document, interview manual, and questionnaire for that round, which can be accessed through: 
 - Ethiopia: http://bit.ly/ethiopia-phonesurvey 
 - Malawi: http://bit.ly/malawi-phonesurvey 
 - Nigeria: http://bit.ly/nigeria-phonesurvey
 - Uganda: http://bit.ly/uganda-phonesurvey 
 
The approach to the phone survey questionnaire design and sampling is comparable across countries. It is informed by the template questionnaire and the phone survey sampling guidelines that have been publicly made available by the World Bank. These can be accessed through: 
 - Template Questionnaire: http://bit.ly/templateqx 
 - Manual: http://bit.ly/interviewermanual
 - Sampling Guidelines: http://bit.ly/samplingguidelines.

## Data cleaning

The code in this repository cleans the raw phone surveys and base survey (00) rounds. This includes updates through August of 2022.

### Pre-requisites

#### Stata reqs

The data processing and analysis requires a number of user-written Stata programs:
   * 1. `blindschemes`
   * 2. `estout`
   * 3. `mdesc`
   * 4. `grc1leg2`
   * 5. `distinct`
   * 6. `winsor2`
   * 7. `palettes`
   * 8. `catplot`
   * 9. `colrspace` 

#### Folder structure

The general repo structure looks as follows:<br>

```stata
wb_covid
├────README.md
├────projectdo.do
├────LICENSE
│    
├────country             /* one dir for each country */
│    ├──household_data
│    │  └──wave          /* one dir for each wave */
│    ├──household_cleaning_code 
│
│────Analysis            /* overall analysis */
│    ├──code
│    └──output
│       ├──tables
│       └──figures
│   
└────config
```
