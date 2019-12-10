#Heat Index Function
#Elena Austin
#December 2019

# heat index function
heat_index <- function(TempF, RH, units = "F") {
  #Based on the heat index equation: 
  #https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
  #Rothfusz approach
  
  datarh  = data.table(TempF, RH)
  
  if (units == "C"){
    datarh[, TempF := (TempF * 9/5) + 32]
  }
  
  datarh[, HI := 0.5 * (TempF + 61.0 + ((TempF-68.0)*1.2) + (RH*0.094))]
  
  datarh[, HIave := rowMeans(.SD), .SDcols = c("HI", "TempF")]
  
  datarh[HIave > 80, HI := -42.379 + 2.04901523*TempF + 10.14333127*RH - .22475541*TempF*RH - 
           .00683783*TempF*TempF - 
           .05481717*RH*RH + .00122874*TempF*TempF*RH + 
           .00085282*TempF*RH*RH - .00000199*TempF*TempF*RH*RH ]
  
  datarh$ADJUSTMENT = 0
  datarh[RH < 13 & (TempF>=80 & TempF <= 112), 
         ADJUSTMENT := -((13-RH)/4)*sqrt((17-abs(TempF-95.))/17)]
  datarh[RH > 85 & (TempF>=80 & TempF <= 87), 
         ADJUSTMENT := ((RH-85)/10) * ((87-TempF)/5)]
  
  datarh = datarh[, HI := HI + ADJUSTMENT]
  datarh$HI
  
}