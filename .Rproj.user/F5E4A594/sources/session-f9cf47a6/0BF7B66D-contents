
to_radians <- function(datetime) {
  decimal_time <- hour(datetime)+ minute(datetime)/60 + second(datetime)/3600
  radian_time <- decimal_time * ((2 * pi)/24)
  #plot(circular(decimal_time), units = "hours", template="clock24")
  #plot(circular(radian_time), units = "radians")
  return(radian_time)
}
