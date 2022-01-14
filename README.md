# CIS3190 A1 Information

## Equations & Symbols

### Weather

>These numbers will be given as input to the program.

#### T

Noon temperature, degrees C

#### H

Relative humidity, percent

#### W

>km/h

Wind speed

#### r_0

>mm

Rainfall in open

#### r_e

Relative rainfall, DMC

#### r_d

Effective rainfall, DC

### Fine Fuel Moisture Code (FFMC)

#### m_0

Fine fuel moisture content from previous day

#### m

Fine fuel moisture content after drying

#### E_w

Fine fuel EMC for wetting

```fortran
0.942 * (H ** 0.679) + 11 * (e ** ((H-100)/10)) + 0.18 * (21.1 - T) 
```

#### E_d

Fine fuel EMC for drying

#### k_0

Intermediate step in calculation of [k](#k)

#### k

Log drying rate, FFMC, log_10 m/day

#### f(r_0)

Rainfall function in FFMC

#### C
#### F_0
#### F_r
#### F

### Duff Moisture Code (DMC)

#### M_0
#### M_r
#### M
#### K
#### L_e
#### b
#### P_0
#### P_r
#### P

### Drought Code (DC)

#### Q
#### Q_0
#### Q_r
#### V
#### L_f
#### D_0
#### D_r
#### D

### Fire Weather Index (FWI)

#### f(W)
#### f(F)
#### f(D)
#### R
#### U
#### B
#### S
