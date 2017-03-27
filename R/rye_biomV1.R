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
  if(length(param)!=7) stop('There are 7 parameters required for this fucntion.') # Quality check
  Tbase = param[["Tbase"]]                            #Define paramaters needed
  RUE= param[["RUE"]]
  K= param[["K"]]
  alpha= param[["alpha"]]
  LAImax= param[["LAImax"]]
  TTM= param[["TTM"]]
  TTL= param[["TTL"]]

  weather<-weather_rb(weather)
  #####################
  weather$Date<-as.Date(paste0(weather[,1],"-",format(strptime(weather[,2], format="%j"),
                                                  format="%m-%d")))
  indx.start<-which(weather$Date==sdate)
  indx.end<-which(weather$Date==tdate)
  if((indx.end-indx.start)!=(as.numeric(tdate-sdate))) stop('There are some missing rows in your weather data for selected time period.') # Quality check
  ########################
  #sdate= lubridate::yday(as.character(sdate))      #Transform date to julian days & ensure simulation runs for 2 consecutive years
  #tdate= lubridate::yday(as.character(tdate)) + 365


  TT= rep(NA, indx.end-indx.start)                                #Define state variables
  B= rep(NA, indx.end-indx.start)
  LAI= rep(NA, indx.end-indx.start)

  TT[1]= 0                                     #Initialize state variables before simulation
  LAI[1]= 0.01
  B[1]= 1

  for (day in indx.start:(indx.end-1)){                                        #Update state variables
     dTT= max ((weather$maxt[day] + weather$mint[day])/2 - Tbase, 0)
  #   cat(day-indx.start+1,"-",TTL,"-",TT[day-indx.start+1],"\n")
     if (TT[day-indx.start+1] <= TTL){
      dLAI= alpha * dTT * LAI[day-indx.start+1] * max(LAImax - LAI[day-indx.start+1], 0)
    }else{
      dLAI= 0
    }

    if(TT[day-indx.start+1] <= TTM){
      dB= RUE * (1 - exp(-K * LAI[day-indx.start+1])) * weather$radn[day]
    }else{
      dB= 0
    }

    TT[day-indx.start+2]= TT[day-indx.start+1] + dTT
    LAI[day-indx.start+2]= LAI[day-indx.start+1] + dLAI
    B[day-indx.start+2]= B[day-indx.start+1] + dB
  }

  return(data.frame( day= weather$Date[indx.start:indx.end],
                     TT= TT,
                     LAI= LAI,
                     B= B))
}






