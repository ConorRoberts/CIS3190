module FFWIndices
implicit none

  real :: e = 2.71828182845904523536028747135266249775724709369995
  real :: current_dmc,previous_dmc
  integer :: previous_ffmc
  real :: correction,moisture

contains
    integer function get_ffmc(temperature,humidity,wind_speed,rainfall,ffmc)
      real :: temperature,rainfall
      integer:: humidity,wind_speed,ffmc_after_rain,new_ffmc
      real :: dry_emc,wet_emc,dry_moisture
      integer :: ffmc
      real :: new_rainfall

      previous_ffmc = ffmc

      if (rainfall > 0.5) then
        new_rainfall = get_rainfall(rainfall)
      else
        new_rainfall = rainfall
      end if

      correction = get_correction()

      moisture = get_moisture(new_rainfall)

      dry_emc = get_dry_emc(temperature,humidity)
      
      if (moisture > dry_emc) then
        dry_moisture = get_dry_moisture(temperature,humidity,wind_speed)
      else
        wet_emc = get_wet_emc(temperature,humidity)
        
        if (moisture < wet_emc) then
          dry_moisture = get_wet_moisture(temperature,humidity)
        end if
      end if
      
      if (dry_emc>moisture .AND. moisture>wet_emc) then
        dry_moisture = moisture
      end if
      
      new_ffmc = 101 - dry_moisture

      get_ffmc = new_ffmc
    end function

    subroutine print_header()
      print *,""
      print *,"  DATE  TEMP  RH   WIND  RAIN   FFMC   DMC   DC   ISI   BUI   FWI"
      print *,""
    end subroutine

    real function get_ffmc_after_rain(rainfall)
      real :: rainfall,val

      val=(previous_ffmc/100.)*rainfall+(1.0-correction)
      if (val<0) then
        val=0
      end if

      get_ffmc_after_rain = val
    end function

    real function get_moisture(rainfall)
      real :: rainfall,val

      if (rainfall > 0.5) then
        val = 101 - get_ffmc_after_rain(rainfall)
      else
        val = 101 - previous_ffmc
      end if

      get_moisture = val

    end function

    real function get_log_drying_rate(temperature,humidity,wind_speed)
      real :: log_drying_rate,temperature
      integer :: wind_speed,humidity

      log_drying_rate = 0.424*(1.-(humidity/100.)**1.7)+(0.0694*(wind_speed**0.5))*(1.-(humidity/100.)**8)
      log_drying_rate = log_drying_rate*0.463*(e**(0.0365*temperature))

      get_log_drying_rate = log_drying_rate
    end function

    real function get_wet_moisture(temperature,humidity)
      real :: EW,temperature
      integer :: humidity

      EW = get_wet_emc(temperature,humidity)

      get_wet_moisture = EW - ((EW - moisture)/1.9953)
    end function

    real function get_dry_moisture(temperature,humidity,wind_speed)
      real :: temperature,k,ED
      integer :: wind_speed,humidity

      ED = get_dry_emc(temperature,humidity)

      k = get_log_drying_rate(temperature,humidity,wind_speed)
      get_dry_moisture = ED + ((moisture - ED) * (10 ** (-k)));
    end function

    real function get_dry_emc(temperature,humidity)
      real :: temperature
      integer :: humidity
      get_dry_emc = 0.942*(humidity**0.679)+(11.*EXP((humidity-100.)/10.))+0.18*(21.1-temperature)
    end function

    real function get_wet_emc(temperature,humidity)
      real :: temperature
      integer :: humidity
      get_wet_emc = 0.618*(humidity**0.753)+(10.*EXP((humidity-100.)/10.))+0.18*(21.1-temperature)
    end function

    real function get_rainfall(rainfall)
      real :: new_rainfall,rainfall

      if (rainfall > 0.5 .AND. rainfall <= 1.45) then
        new_rainfall = 123.85-(55.6*log(rainfall+1.016))
      else if (rainfall > 1.45 .AND. rainfall <= 5.75) then
        new_rainfall = 57.87-(18.2*log(rainfall-1.016))
      else if (rainfall > 5.75) then
        new_rainfall = 40.69-(8.25*log(rainfall-1.905))
      end if

      get_rainfall = new_rainfall
    end function

    real function get_correction()
      get_correction = 8.73 * (e ** (-0.1117 * previous_ffmc))
    end function

end module FFWIndices