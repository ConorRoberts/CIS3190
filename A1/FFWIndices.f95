module FFWIndices
implicit none

  real :: e = 2.71828182845904523536028747135266249775724709369995

contains
    integer function get_ffmc(temperature,humidity,wind_speed,previous_moisture)
      real :: temperature,previous_moisture
      integer:: humidity,wind_speed

      ! real :: RO

      ! RO = get_rainfall_function(rainfall)

      ! correction = get_correction(previous_ffmc)

      get_ffmc = 101 - get_moisture(previous_moisture,temperature,humidity,wind_speed)
    end function

    real function get_moisture(previous_moisture,temperature,humidity,wind_speed)
      real :: previous_moisture,temperature,rainfall,moisture
      integer :: humidity,wind_speed

      if (rainfall > 0.5) then
        moisture = get_wet_moisture(previous_moisture,temperature,humidity)
      else
        moisture = get_dry_moisture(previous_moisture,temperature,humidity,wind_speed)
      end if

      get_moisture = moisture
    end function

    real function get_log_drying_rate(temperature,humidity,wind_speed)
      real :: log_drying_rate,temperature
      integer :: wind_speed,humidity

      log_drying_rate = 0.424*(1.-(humidity/100.)**1.7)+(0.0694*(wind_speed**0.5))*(1.-(humidity/100.)**8)
      log_drying_rate = log_drying_rate*0.463*(e**(0.0365*temperature))

      get_log_drying_rate = log_drying_rate
    end function

    real function get_wet_moisture(previous_moisture,temperature,humidity)
      real :: EW,previous_moisture,temperature
      integer :: humidity

      EW = get_wet_emc(temperature,humidity)

      get_wet_moisture = EW - ((EW - previous_moisture)/1.9953)
    end function

    real function get_dry_moisture(previous_moisture,temperature,humidity,wind_speed)
      real :: previous_moisture,temperature,k,ED
      integer :: wind_speed,humidity

      ED = get_wet_emc(temperature,humidity)

      k = get_log_drying_rate(temperature,humidity,wind_speed)

      get_dry_moisture = ED + ((previous_moisture - ED) * (10 ** (-k)));
    end function

    real function get_dry_emc(temperature,humidity)
      real :: temperature
      integer :: humidity
      get_dry_emc = 0.942*(humidity**0.679)+(11*(e**((humidity-100)/10))+0.18*(21.1-temperature)*(1-e**(-0.115*humidity)))
    end function

    real function get_wet_emc(temperature,humidity)
      real :: temperature
      integer :: humidity
      get_wet_emc = 0.618*(humidity**0.753)+(10*(e**((humidity-100)/10))+0.18*(21.1-temperature)*(1-e**(-0.115*humidity)))
    end function

    real function get_rainfall_function(rainfall)
      real :: new_rainfall,rainfall

      if (rainfall > 0.5 .AND. rainfall <= 1.45) then
        new_rainfall = 123.85-(55.6*ALOG(rainfall+1.016))
      else if (rainfall > 1.45 .AND. rainfall <= 5.75) then
        new_rainfall = 57.87-(18.2*ALOG(rainfall-1.016))
      else if (rainfall > 5.75) then
        new_rainfall = 40.69-(8.25*ALOG(rainfall-1.905))
      end if

      get_rainfall_function = new_rainfall
    end function

    real function get_correction(previous_ffmc)
      real :: previous_ffmc

      get_correction = 8.73 * (e ** (-0.1117 * previous_ffmc))
    end function

    real function get_fr(previous_rainfall)
      real :: previous_rainfall,previous_ffmc

      get_fr = (previous_ffmc / 100 * get_rainfall_function(previous_rainfall)) + 1 - get_correction(previous_ffmc)
    end function
    
end module FFWIndices