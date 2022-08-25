/* BEGIN */

* Project: DIVERSIFICATION
* Created on: 30 March 2022
* Created by: alj
* Edited by: alj
* Last edit: 5 April 2022
* Stata v.16.1

* does
	* sample MDE code 

* assumes
	* ssc power 

* TO DO:
	* done 

************************************************************************
**# 0 - example calculations - baraguay and household 
************************************************************************

* example calculations
* examples coming from IRRI work on extension advice 

* more details on code: https://www.stata.com/features/overview/power-and-sample-size/ 
* more details on code: https://dimewiki.worldbank.org/Power_Calculations_in_Stata 

* using twomeans 
* first variable (0.27) is the average 
* k1 and k2 represent the numbers of control and treatment clusters, respectively 
* kratio is ratio of k1/k2 - 1 is default 
* sd is standard deviation - assuming equality between treatment and control groups 
* m1 is the cluster size in the control (could also specify m2)
* mratio is ratio of m1/m2 - 1 is default 
* power - varies (will show a range)
* could also here change sample size 
* rho is ICC assuming equality between treatment and control - this comes from experimental data from IRRI 
*** we can also calculate ICC: https://www.stata.com/features/overview/intraclass-correlation-coefficients/ 

* baragays level 
* baragays = similar to county 
power twomeans .27, k1(52(8)80) kratio(1) sd(.44) m1(5) mratio(1) power(0.7 0.8 0.9) rho(0.31537) graph(y(delta) /// 
	title("") xline(65, lcolor(maroon) lstyle(solid) ) /// 
	legend(pos(6) cols(3))) 

* household level 
power twomeans .27, k1(56) k2(56) sd(.44) m1(1(1)15) mratio(1) power(0.7 0.8 0.9) rho(0.31537) graph(y(delta) /// 
	title("") xline(10, lcolor(maroon) lstyle(solid) ) /// 
	legend(pos(6) cols(3))) 

************************************************************************

* from AF
*** SAMPLE INFORMATION FROM ETHIOPIA 
* sample means 
*** mild_fs = 0.52, mod_fs = 0.25, sev_fs = 0.03, stf_fs = -0.43
*** educ_act = 0.53
* sample sds
*** mild_fs = 0.5, mod_fs = 0.44, sev_fs = 0.17, stf_fs = 0.82
*** educ_act = 0.50 
* obs range from 2077 to 3249 in food security 
* obs range from 1485 to 1919 in educational attainment 

* consider example for mild_fs
power onemean 0.52 0.75, sd(0.5) n(2000(500)3500) graph
power onemean 0.52 0.25, sd(0.5) n(2000(500)3500) graph
*** no real difference if change alt from 0.75 to 0.25

/* END */