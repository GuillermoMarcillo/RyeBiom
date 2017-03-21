# The ryebiom_v1 function
#
# This function reads weather inputs, initialize a list of parameters, and
# simulates cover crop growth at a daily time-step. Biomass simulation outputs for a single
# site are produced with a single run over single or multiple growth seasons. As such,
# a simulation begins and stops over seeding and termination in consecutive years of
# a cover crop (i.e., fall seeding and next-year's spring termination)

#The simple model follows a resource capture and transformation under non-limiting conditions,
# and therefore, no stresses of any kind are considered for now.
# No major phenological description is included yet, with development being driven by
# simple thermal time accumulation and leaf expansion routines.
#
# It is meant to work on a dataframe with weather records processed by the weather_rb function.
# Parameters refer to a list of physiologically meaningfulcoefficients driving the dynamic accumulation
# of biomass.


rye_biomV1 <- function(param, weather, sdate, tdate){
  Tbase = param["Tbase"]                            #Define paramaters needed
  RUE= param["RUE"]
  K= param["K"]
  alpha= param["alpha"]
  LAImax= param["LAImax"]
  TTM= param["TTM"]
  TTL= param["TTL"]

  sdate= lubridate::yday(as.character(sdate))      #Transform date to julian days & ensure simulation runs for 2 consecutive years
  tdate= lubridate::yday(as.character(tdate)) + 365

  TT= rep(NA, tdate)                                #Define state variables
  B= rep(NA, tdate)
  LAI= rep(NA, tdate)

  TT[sdate]= 0                                     #Initialize state variables before simulation
  LAI[sdate]= 0.01
  B[sdate]= 1

  for (day in sdate:(tdate-1)){                                        #Update state variables
    dTT= max ((weather$maxt[day] + weather$mint[day])/2 - Tbase, 0)
    if (TT[day] <= TTL){
      dLAI= alpha * dTT * LAI[day] * max(LAImax - LAI[day], 0)
    }else{
      dLAI= 0
    }
    if(TT[day] <= TTM){
      dB= RUE * (1 - exp(-K * LAI[day])) * weather$radn[day]
    }else{
      dB= 0
    }

    TT[day + 1]= TT[day] + dTT
    LAI[day + 1]= LAI[day] + dLAI
    B[day + 1]= B[day] + dB
  }

  return(data.frame( day= sdate:tdate,
                     TT= TT[sdate:tdate],
                     LAI= LAI[sdate:tdate],
                     B= B[sdate:tdate]))
}






