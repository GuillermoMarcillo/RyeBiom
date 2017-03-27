# The Weather_rb function
#
# This function builds a weather input array in a format such that a rye
# cover crop simulation can be run over a 2-year typical growth season,
# that is, from seeding the cover crop in the Fall to termination in the
# spring of the subsequent year.
#
# It is meant to work on a .csv, or .txt file with weather records, as
# usually downloaded and processed in the format needed by the Agricultural
# production simulator (APSIM v.7.8)

weather_rb<- function(x){
  weather= data.frame(x)
  colnames(weather)= c("year", "day", "radn", "maxt", "mint", "rain")
  return(weather)
}

#not print:
#ames2016<- read.table("Weather_files/Weather_Ames_2016.txt",
 #                     skip=11)

#ames2<- ames2016
#rownames(ames2)<- ames2016$V2
