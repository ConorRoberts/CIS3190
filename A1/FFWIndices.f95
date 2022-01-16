module FFWIndices
implicit none

  real :: previous_dmc,previous_ffmc
  real :: correction,starting_moisture

contains
  !-----------------------------------------------------------------------
  ! Fine Fuel Moisture Code (FFMC)
  ! -----------------------------------------------------------------------
    real function get_ffmc(temperature,humidity,wind_speed,rainfall,ffmc)
      real :: temperature,rainfall
      integer:: humidity,wind_speed
      real :: dry_emc,wet_emc,final_moisture
      real :: ffmc,new_ffmc
      real :: new_rainfall

      previous_ffmc = ffmc

      if (rainfall > 0.5) then
        new_rainfall = get_rainfall(rainfall)
      else
        new_rainfall = rainfall
      end if

      correction = get_correction()

      starting_moisture = get_moisture(new_rainfall)

      dry_emc = get_dry_emc(temperature,humidity)
      
      if (starting_moisture > dry_emc) then
        final_moisture = get_dry_moisture(temperature,humidity,wind_speed)
      else
        wet_emc = get_wet_emc(temperature,humidity)
        
        if (starting_moisture < wet_emc) then
          final_moisture = get_wet_moisture(temperature,humidity)
        end if
      end if
      
      if (dry_emc>starting_moisture .AND. starting_moisture>wet_emc) then
        final_moisture = starting_moisture
      end if
      
      new_ffmc = 101 - final_moisture

      get_ffmc = new_ffmc
    end function

    ! Prints out column headings for the data table
    subroutine print_header()
      print *,""
      print *,"  DATE  TEMP  RH   WIND  RAIN   FFMC   DMC   DC   ISI   BUI   FWI"
      print *,""
    end subroutine

    real function get_ffmc_after_rain(rainfall)
      real :: rainfall,val

      ! Apply fr equation
      val=(previous_ffmc/100.)*rainfall+(1.0-correction)

      ! If the value is less than 0, set it to 0
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
      log_drying_rate = log_drying_rate*(0.463*(EXP(0.0365*temperature)))

      get_log_drying_rate = log_drying_rate
    end function

    real function get_wet_moisture(temperature,humidity)
      real :: EW,temperature
      integer :: humidity

      EW = get_wet_emc(temperature,humidity)

      get_wet_moisture = EW - ((EW - starting_moisture)/1.9953)
    end function

    real function get_dry_moisture(temperature,humidity,wind_speed)
      real :: temperature,k,ED
      integer :: wind_speed,humidity

      ED = get_dry_emc(temperature,humidity)

      k = get_log_drying_rate(temperature,humidity,wind_speed)
      get_dry_moisture = ED + ((starting_moisture - ED) * (10 ** (-k)));
    end function

    real function get_dry_emc(temperature,humidity)
      real :: temperature,val
      integer :: humidity
      val = 0.942*(humidity**0.679) + (11.*EXP((humidity-100.)/10.))
      val = val + 0.18*(21.1-temperature)*(1.-1./EXP(0.115*humidity))

      get_dry_emc = val
    end function

    real function get_wet_emc(temperature,humidity)
      real :: temperature,val
      integer :: humidity
      val = 0.618*(humidity**0.753)+(10.*EXP((humidity-100.)/10.))
      val = val + 0.18*(21.1-temperature)*(1.-1./EXP(0.115*humidity))

      get_wet_emc = val
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
      get_correction = 8.73*EXP(-0.1117*previous_ffmc)
    end function

  !-----------------------------------------------------------------------
  ! Duff Moisture Code (DMC)
  ! -----------------------------------------------------------------------

  real function get_dmc(temperature,humidity,rainfall,dmc,dmc_factor)
    real :: new_dmc, temperature, rainfall,dmc
    integer :: humidity
    real :: dmc_factor,dmc_after_rain,dmc_moisture
    real :: moisture_after_rain,log_drying_rate

    previous_dmc = dmc

    ! Check if we should run rainfall routine or not
    if (rainfall > 1.5) then

      dmc_moisture = get_dmc_moisture()

      moisture_after_rain = get_dmc_moisture_after_rain(dmc_moisture,rainfall)

      dmc_after_rain = get_dmc_after_rain(moisture_after_rain)

      log_drying_rate = get_dmc_log_drying_rate(temperature,humidity,dmc_factor)

      new_dmc = dmc_after_rain + log_drying_rate
    else
      log_drying_rate = get_dmc_log_drying_rate(temperature,humidity,dmc_factor)

      new_dmc = previous_dmc + log_drying_rate
    end if

    get_dmc = new_dmc
  end function

  real function get_dmc_after_rain(moisture_after_rain)
    real :: moisture_after_rain,val

    ! Apply formula
    val = 43.43*(5.6348-ALOG(moisture_after_rain-20.)) 

    ! Force value >=0
    if (val<0) then
      val=0
    end if

    get_dmc_after_rain = val
end function

  real function get_effective_rainfall(rainfall)
    real :: rainfall

    if (rainfall > 1.5) then
      get_effective_rainfall = (0.92*rainfall)-1.27
    else
      get_effective_rainfall = rainfall
    end if

  end function

  real function get_dmc_moisture()
    get_dmc_moisture = 20.0+280./EXP(0.023*previous_dmc)
  end function

  real function get_dmc_rain_effect()
    real :: val

    if (previous_dmc <=33) then
      val = 100./(0.5+0.3*previous_dmc)
    else if (previous_dmc>33 .AND. previous_dmc<=65) then
      val = 14.-1.3*ALOG(previous_dmc)
    else if (previous_dmc>65) then
      val = 6.2*ALOG(previous_dmc)-17.2
    end if

    get_dmc_rain_effect = val
  end function

  real function get_dmc_log_drying_rate(temperature,humidity,dmc_factor)
    real :: temperature,dmc_factor,new_temperature
    integer :: humidity

    ! Temperature cannot be less than -1.1
    if (temperature < -1.1) then
      new_temperature = -1.1
    else
      new_temperature = temperature
    end if

    get_dmc_log_drying_rate=1.894*(new_temperature + 1.1)*(100.-humidity)*(dmc_factor*0.0001)
  end function

  real function get_dmc_moisture_after_rain(dmc_moisture,rainfall)
    real :: effective_rain,function_in_rain_effect,dmc_moisture,rainfall

    effective_rain = get_effective_rainfall(rainfall)
    function_in_rain_effect = get_dmc_rain_effect()

    get_dmc_moisture_after_rain = dmc_moisture+(1000.*effective_rain)/(48.77+function_in_rain_effect*effective_rain)
  end function
end module FFWIndices