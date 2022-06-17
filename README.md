INCLINE Readme
================

# Introduction

# Method

## Sites

## Design

## Treatments

# Data management

## Location

## Naming convention

## Data collection

**Ecosystem fluxes** Net ecosystem exchange (NEE) and ecosystem
respiration (ER) were measured on the field while gross ecosystem
production (GEP) is calculated as GEP = NEE - ER. Carbon fluxes are
measured with a closed loop chamber system connected to an infrared gaz
analyser (INGRA; Li-840A, Li-cor). The plexiglas chamber (25x35x40 cm)
is connected to the INGRA with plastic tubes (inlet and outlet, 3m long
and 4mm diameter each). A filter prevents water droplets and small
particules from entering the INGRA and an air pump ensures a constant
flow of 1L/mn. The chamber is equiped with a fan to mix the air, a
thermocouple (Pt1000, Delta-T) to measure air temperature and a PAR
sensor. In 2020 the air temperature was measured with an iButton.
Airtightness is ensured by laying a heavy chain on the “skirt” of the
chamber (a piece of tarp taped to the lower part of the chamber). NEE is
measured with a transparent chamber. ER is measured with a dark chamber,
in our case the transparent chamber was covered with a thick dark tarp.
The CO<sub>2</sub> concentration was measured every seconds and logged
in a datalogger (Squirrel 2010). The chamber was put on the plot during
three (two in 2020) minutes for each measurements and then aired during
one minute. Since the logger was logging continuesly, the start and end
time of each measurement was noted (it is necessary to precisely
synchronise the logger clock with the watch used on the field and to
regularly check it). The function `match.flux()` is matching the time in
the logger file with the plot being measured at that time (using the
time recorded on the field). It attributes the concentration of
CO<sub>2</sub> measured every seconds to the correct measurement. A
linear regression is fitted to every measurements and the slope is used
to calculated the flux.
<!-- This is the code to keep in case we want to extract a pdf -->
<!-- $$ -->
<!--  \text{flux}=\text{slope}\times \frac{P\times V}{R\times T\times A} -->
<!-- $$ -->
<!-- - flux: the flux of CO~2~ at the surface of the plot ($mmol/m^2/h$) -->
<!-- - slope: slope of linear regression fitting the CO~2~ concentration versus time ($ppm^{-1}$) -->
<!-- - $P$: pressure, assumed 1 atm -->
<!-- - $V$: volume of the chamber and tubing ($L$) -->
<!-- - $R$: gas constant ($0.082057\ L*atm*K^{-1}*mol^{-1}$) -->
<!-- - $T$: chamber air temperature ($K$) -->
<!-- - $A$: area of chamber frame base ($m^2$) -->

<img src="https://render.githubusercontent.com/render/math?math=flux=slope\times \frac{P\times V}{R\times T\times A}">

Where:

-   flux: the flux of CO<sub>2</sub> at the surface of the plot
    (mmol/m<sup>2</sup>/h)
-   slope: slope of linear regression fitting the CO<sub>2</sub>
    concentration versus time (ppm<sup>-1</sup>)
-   P: pressure, assumed 1 atm
-   V: volume of the chamber and tubing (L)
-   R: gas constant (0.082057 L\*atm\*K<sup>-1</sup>\*mol<sup>-1</sup>)
-   T: chamber air temperature (K)
-   A: area of chamber frame base (m<sup>2</sup>)

## Data dictionnaries

### Carbon fluxes

| Variable name | Description                                                                    | Variable type | Variable range or levels                  | Unit                | How measured |
|:--------------|:-------------------------------------------------------------------------------|:--------------|:------------------------------------------|:--------------------|:-------------|
| fluxID        | Unique ID for each flux                                                        | numeric       | 1 - 728                                   | NA                  | defined      |
| p.value       | p value of the linear relation between CO2 concentration and time              | numeric       | 0 - 0.773                                 | NA                  | calculated   |
| r.squared     | R squared of the linear relation between CO2 concentration and time            | numeric       | 0.001 - 0.998                             | NA                  | calculated   |
| adj.r.squared | adjusted R squared of the linear relation between CO2 concentration and time   | numeric       | -0.009 - 0.998                            | NA                  | calculated   |
| nobs          | Number of CO2 concentration measurements for each flux                         | numeric       | 54 - 169                                  | NA                  | measured     |
| PARavg        | PAR value measured every 15 seconds during flux measurement and averaged       | numeric       | 0.119 - 1959.496                          | micromol/s/sqm      | measured     |
| temp_airavg   | Air temperature measured inside the flux chamber every 10 seconds and averaged | numeric       | 277.849 - 299.394                         | Kelvin              | measured     |
| turfID        | ID of each turfs                                                               | categorical   | GUD_1\_3 - ULV_7\_6                       | NA                  | defined      |
| type          | Type of flux measurements                                                      | categorical   | 1 - NEE                                   | NA                  | defined      |
| campaign      | field measurement campaign                                                     | categorical   | 2 - LRC                                   | NA                  | defined      |
| comments      | additional information on the data                                             | categorical   | cliff shade - windy                       | NA                  | defined      |
| datetime      | Date and time of sampling or observation                                       | date_time     | 2020-07-17 12:15:50 - 2020-08-26 14:09:25 | yyyy-mm-dd_hh:mm:ss | defined      |
| replicate     | Replicate measurement of same turf                                             | numeric       | 1 - 4                                     | NA                  | defined      |
| flux          | CO2 flux (postive when emitting to atmosphere negative when vegetation uptake) | numeric       | -40.778 - 136.198                         | mmol/sqm/h          | calculated   |

### NDVI

| Variable name | Description                                                   | Variable type | Variable range or levels                                                                                                                                                                                                                                                                                                                                                                                | Unit       | How measured |
|:--------------|:--------------------------------------------------------------|:--------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------|:-------------|
| plotID        | Unique ID for each plot                                       | NA            | NA                                                                                                                                                                                                                                                                                                                                                                                                      | NA         | defined      |
| NDVI          | NDVI measurement for each plot                                | numeric       | 0.2 - 0.9                                                                                                                                                                                                                                                                                                                                                                                               | NA         | measured     |
| date          | Date of sampling or observation                               | date          | 2019-07-26 - 2021-08-18                                                                                                                                                                                                                                                                                                                                                                                 | yyyy-mm-dd | defined      |
| replicate     | Replicate measurement of same turf                            | NA            | NA                                                                                                                                                                                                                                                                                                                                                                                                      | NA         | defined      |
| tomst         | Presence of TMS microclimate logger in the plot when measuing | NA            | NA                                                                                                                                                                                                                                                                                                                                                                                                      | NA         | defined      |
| OTC           | Presence of OTC in the plot when measuing                     | NA            | NA                                                                                                                                                                                                                                                                                                                                                                                                      | NA         | defined      |
| site          | Unique site ID                                                | NA            | NA                                                                                                                                                                                                                                                                                                                                                                                                      | NA         | defined      |
| date_comment  | Comments specifically about date issue                        | categorical   | Missing date, could have been 12.-13.08.2021 or 16.-18.08.2021, but we usually did it on the last day if we had people and time to spare. I added the 17.08.2021 for most of them, but the 18.08.2021 for the last measurements that we needed to redo. - Missing date, could have been the 06.08.202 or09.08.2021-12.08.2021, but we usually did it on the last day if we had people and time to spare | NA         | defined      |
| comments      | additional information on the data                            | categorical   | Dagmar - tomst logger = 0.35                                                                                                                                                                                                                                                                                                                                                                            | NA         | defined      |

### Microclimate

| Variable name | Description                               | Variable type | Variable range or levels                  | Unit                | How measured |
|:--------------|:------------------------------------------|:--------------|:------------------------------------------|:--------------------|:-------------|
| datetime      | Date and time of sampling or observation  | date_time     | 2019-06-12 00:15:00 - 2021-09-28 23:45:00 | yyyy-mm-dd_hh:mm:ss | defined      |
| loggerID      | Unique ID for each microclimate loggers   | numeric       | 94194607 - 94205760                       | NA                  | defined      |
| plotID        | Unique ID for each plot                   | categorical   | Gud_1\_2 - Ulv_7\_6                       | NA                  | defined      |
| site          | Unique site ID                            | categorical   | Gudmedalen - Ulvehaugen                   | NA                  | defined      |
| OTC           | Presence of OTC in the plot when measuing | categorical   | C - W                                     | NA                  | defined      |
