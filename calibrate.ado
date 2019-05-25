*************************************************************************
* Programme       : Prediction model calibration                        *
* Programmer      : Jonas Ranstam                                       *
* Programmed date : 08.09.2015                                          *
*************************************************************************

*! calibrate v1.1.0 JRanstam 8sep2015
program define calibrate
   version 14
   syntax varlist(min=2 max=2) [if] [in]
   tokenize `varlist'
   local obs `1'
   local pred `2'
   preserve
   drop if `obs'==. | `pred'==.
   keep if inlist(`obs', 0, 1)
   drop if `pred' > 1 | `pred' < 0

   display
   display as txt "1. Area under the ROC-curve"
   roctab `obs' `pred' `if' `in', binomial
   
   quietly {   
      egen predicted=cut(`pred'), at(0(0.1)1)
      collapse (sum) cases=`obs' (mean) observed=`obs' (sebinomial) seobs=`obs' (count) n=`obs' `if' `in', by(predicted)
      replace predicted=predicted + .05
      gen error=abs(observed-predicted) 
      summarize error
      local mape=r(mean)
      gen lower95=observed-1.96*seobs
      gen upper95=observed+1.96*seobs
      replace lower95=0 if lower95 < 0
      replace upper95=1 if upper95 > 1
      gen row=1
      gen expcases=n*predicted
      gen expnocases=n*(1-predicted)
      gen nocases=n-cases
      gen chi2cases=((cases-expcases)^2)/expcases
      gen chi2nocases=((nocases-expnocases)^2)/expnocases
      summarize chi2cases
      local chi2c=r(sum)
      summarize chi2nocases
      local chi2n=r(sum)
      local chi2=`chi2c'+`chi2n'
      summarize row
      local df=r(sum)-2
      local pval=chi2tail(`df',`chi2')
   }
   display
   display as txt "2. Observed and predicted cases per decile"
   list predicted observed lower95 upper95 cases expcases nocases expnocases n, clean
   display
   display as txt "3. Mean absolute prediction error = " as res `mape' 
   display
   display as txt "4. Hosmer-Lemeshow Chi2(" as res (`df') as txt ") = " as res `chi2' as txt ", p = " as res `pval' 

   twoway (rcap upper95 lower95 predicted, lstyle(ci))                 ///
          (scatter observed predicted, mstyle(p1))                     ///
          (line predicted predicted, lcolor(gray) lpattern(dash)),     ///  
	  xlabel(0(.1)1) ylabel(0(.1)1)                                ///
          ytitle(Observed proportion)                                  ///
          xtitle(Predicted) note(" ") legend(off)

   display
   display as txt "5. Calibration plot saved as `pred'.png" 	  
   quietly: graph export `pred'.png, replace width(3000)
   restore
end

