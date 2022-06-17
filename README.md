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
| turfID        | Unique ID of each turfs                                                        | categorical   | GUD_1\_3 - ULV_7\_6                       | NA                  | defined      |
| type          | Type of flux measurements                                                      | categorical   | 1 - NEE                                   | NA                  | defined      |
| campaign      | field measurement campaign                                                     | categorical   | 2 - LRC                                   | NA                  | defined      |
| comments      | additional information on the data                                             | categorical   | cliff shade - windy                       | NA                  | defined      |
| datetime      | Date and time of sampling or observation                                       | date_time     | 2020-07-17 12:15:50 - 2020-08-26 14:09:25 | yyyy-mm-dd_hh:mm:ss | recorded     |
| replicate     | Replicate measurement of same turf                                             | numeric       | 1 - 4                                     | NA                  | defined      |
| flux          | CO2 flux (postive when emitting to atmosphere negative when vegetation uptake) | numeric       | -40.778 - 136.198                         | mmol/sqm/h          | calculated   |

### NDVI

| Variable name | Description                                      | Variable type | Variable range or levels                                                                                                                                                                                                                                                                                                                                                                                | Unit       | How measured |
|:--------------|:-------------------------------------------------|:--------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------|:-------------|
| plotID        | Unique ID for each plot                          | NA            | NA                                                                                                                                                                                                                                                                                                                                                                                                      | NA         | defined      |
| NDVI          | NDVI measurement for each plot                   | numeric       | 0.2 - 0.9                                                                                                                                                                                                                                                                                                                                                                                               | °C         | measured     |
| date          | Date of sampling or observation                  | date          | 2019-07-26 - 2021-08-18                                                                                                                                                                                                                                                                                                                                                                                 | yyyy-mm-dd | recorded     |
| replicate     | Replicate measurement of same turf               | NA            | NA                                                                                                                                                                                                                                                                                                                                                                                                      | NA         | defined      |
| siteID        | Unique site ID (3 letters, first letter capital) | NA            | NA                                                                                                                                                                                                                                                                                                                                                                                                      | NA         | defined      |
| date_comment  | Comments specifically about date issue           | categorical   | Missing date, could have been 12.-13.08.2021 or 16.-18.08.2021, but we usually did it on the last day if we had people and time to spare. I added the 17.08.2021 for most of them, but the 18.08.2021 for the last measurements that we needed to redo. - Missing date, could have been the 06.08.202 or09.08.2021-12.08.2021, but we usually did it on the last day if we had people and time to spare | NA         | defined      |
| comments      | additional information on the data               | categorical   | Dagmar - tomst logger = 0.35                                                                                                                                                                                                                                                                                                                                                                            | NA         | defined      |

### Microclimate

| Variable name      | Description                                                                                                                    | Variable type | Variable range or levels                              | Unit                | How measured |
|:-------------------|:-------------------------------------------------------------------------------------------------------------------------------|:--------------|:------------------------------------------------------|:--------------------|:-------------|
| datetime           | Date and time of sampling or observation                                                                                       | date_time     | 2019-06-12 00:15:00 - 2021-09-28 23:45:00             | yyyy-mm-dd_hh:mm:ss | recorded     |
| loggerID           | Unique ID for each microclimate loggers                                                                                        | NA            | NA                                                    | NA                  | defined      |
| plotID             | Unique ID for each plot                                                                                                        | NA            | NA                                                    | NA                  | defined      |
| siteID             | Unique site ID (3 letters, first letter capital)                                                                               | NA            | NA                                                    | NA                  | defined      |
| OTC                | Presence of TMS microclimate logger in the plot when measuing                                                                  | NA            | NA                                                    | NA                  | defined      |
| treatment          | Treatments in the experiment (C = control, R = removal, E = transplants with extant traits, N = transplants with novel traits) | NA            | NA                                                    | NA                  | defined      |
| comments           | additional information on the data                                                                                             | categorical   | No read on the 28.09.2021 - No read on the 29.09.2021 | NA                  | defined      |
| soil_moisture      | Soil moisture                                                                                                                  | numeric       | 0 - 0.678                                             | percentage          | measured     |
| ground_temperature | Ground temperature                                                                                                             | numeric       | -16.062 - 38.812                                      | °C                  | measured     |
| soil_temperature   | Soil temperature at 5 cm below ground                                                                                          | numeric       | -4 - 37.5                                             | °C                  | measured     |
| air_temperature    | Air temperature 15 cm above ground                                                                                             | numeric       | -17.875 - 36.25                                       | °C                  | measured     |

### Biomass removal

| Variable name    | Description                                                                                                                    | Variable type | Variable range or levels                                                                                                                                                                                                 | Unit | How measured |
|:-----------------|:-------------------------------------------------------------------------------------------------------------------------------|:--------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----|:-------------|
| year             | Year of data collection                                                                                                        | numeric       | 2019 - 2021                                                                                                                                                                                                              | yyyy | recorded     |
| siteID           | Unique site ID (3 letters, first letter capital)                                                                               | NA            | NA                                                                                                                                                                                                                       | NA   | defined      |
| treatment        | Treatments in the experiment (C = control, R = removal, E = transplants with extant traits, N = transplants with novel traits) | NA            | NA                                                                                                                                                                                                                       | NA   | defined      |
| comments         | additional information on the data                                                                                             | categorical   | 1 Veronica (ny) klippet av, trolig bare toppen - Was going to weight the plot. Only to bags. Looked further up and there was measured for this plot. However the graminoid and the woody bag that I have was measured??? | NA   | defined      |
| plotID           | Unique ID for each plot                                                                                                        | NA            | NA                                                                                                                                                                                                                       | NA   | defined      |
| functional_group | Functional group removed in the removal experiment: graminoid, forb, bryophyte, woody, fern, lichen, litter                    | NA            | NA                                                                                                                                                                                                                       | NA   | defined      |
| value            | registered value of the corresponding trait                                                                                    | numeric       | 0.009 - 68.42                                                                                                                                                                                                            | NA   | measured     |

### Demography

| Variable name     | Description                                                                                                                       | Variable type | Variable range or levels | Unit         | How measured |
|:------------------|:----------------------------------------------------------------------------------------------------------------------------------|:--------------|:-------------------------|:-------------|:-------------|
| siteID            | Unique site ID (3 letters, first letter capital)                                                                                  | NA            | NA                       | NA           | defined      |
| blockID           | Unique ID of each block. A combination of the siteID (Gud, Lav, Skj or Ulv) and the number for the block (1-7) (Ex: Gud_1, Lav_7) | NA            | NA                       | NA           | defined      |
| plotID            | Unique ID for each plot                                                                                                           | NA            | NA                       | NA           | defined      |
| year              | Year of data collection                                                                                                           | numeric       | 2018 - 2021              | yyyy         | recorded     |
| date              | Date of sampling or observation                                                                                                   | date          | 2018-07-31 - 2021-08-19  | yyyy-mm-dd   | recorded     |
| registrator       | Initials or full name of the registror                                                                                            | NA            | NA                       | NA           | recorder     |
| OTC               | Presence of TMS microclimate logger in the plot when measuing                                                                     | NA            | NA                       | NA           | defined      |
| treatment         | Treatments in the experiment (C = control, R = removal, E = transplants with extant traits, N = transplants with novel traits)    | NA            | NA                       | NA           | defined      |
| uniqueID          | Unique ID for each individual.                                                                                                    | NA            | NA                       | NA           | defined      |
| genetID           | Unique ID for genetically similar species. Those we know are clones.                                                              | NA            | NA                       | NA           | defined      |
| mother_shoot      | Unique ID for when we know the mother of the individual (MS = mother shoot)                                                       | NA            | NA                       | NA           | defined      |
| X                 | X coordinates of the individual in the plot measured from the left side of the plot.                                              | numeric       | 0 - 36                   | mm           | measured     |
| Y                 | Y coordinates of the individual int he plot measured from the lower side of the plot                                              | numeric       | -2 - 32.5                | mm           | measured     |
| demographic_trait | Name of trait measured                                                                                                            | NA            | NA                       | NA           | defined      |
| demographic_value | Length/width/height of leaves, leaf stalkes or stems. Or number of leaves/flowers/buds/capsules/aborted capsules/flower stems.    | numeric       | 0 - 80                   | mm or counts | measured     |
| date              | Date of sampling or observation                                                                                                   | date          | 2018-07-30 - 2021-09-16  | yyyy-mm-dd   | recorded     |
| X                 | X coordinates of the individual in the plot measured from the left side of the plot.                                              | numeric       | 0 - 35.5                 | mm           | measured     |
| Y                 | Y coordinates of the individual int he plot measured from the lower side of the plot                                              | numeric       | 0 - 31                   | mm           | measured     |
| demographic_value | Length/width/height of leaves, leaf stalkes or stems. Or number of leaves/flowers/buds/capsules/aborted capsules/flower stems.    | numeric       | -2 - 214                 | mm or counts | measured     |

### Germination alpine

| Variable name | Description                                                                                                                       | Variable type | Variable range or levels  | Unit | How measured |
|:--------------|:----------------------------------------------------------------------------------------------------------------------------------|:--------------|:--------------------------|:-----|:-------------|
| species       | Species abbreviations, three leters of the genus and three leters of the species. Example: Sib_pro                                | NA            | NA                        | NA   | defined      |
| siteID        | Unique site ID (3 letters, first letter capital)                                                                                  | NA            | NA                        | NA   | defined      |
| blockID       | Unique ID of each block. A combination of the siteID (Gud, Lav, Skj or Ulv) and the number for the block (1-7) (Ex: Gud_1, Lav_7) | NA            | NA                        | NA   | defined      |
| OTC           | Presence of TMS microclimate logger in the plot when measuing                                                                     | NA            | NA                        | NA   | defined      |
| uniqueID      | Unique ID for each individual.                                                                                                    | NA            | NA                        | NA   | defined      |
| registrator   | Initials or full name of the registror                                                                                            | NA            | NA                        | NA   | recorder     |
| comments      | additional information on the data                                                                                                | NA            | NA                        | NA   | defined      |
| trait         | trait measured on the individual, either counts og leaves/flowers etc. size measurements, or biomass measurements                 | categorical   | leaf_length - weight_root | NA   | defined      |

### Seedbank

| Variable name        | Description                                                                                                                       | Variable type | Variable range or levels                                                                                        | Unit       | How measured |
|:---------------------|:----------------------------------------------------------------------------------------------------------------------------------|:--------------|:----------------------------------------------------------------------------------------------------------------|:-----------|:-------------|
| siteID               | Unique site ID (3 letters, first letter capital)                                                                                  | NA            | NA                                                                                                              | NA         | defined      |
| blockID              | Unique ID of each block. A combination of the siteID (Gud, Lav, Skj or Ulv) and the number for the block (1-7) (Ex: Gud_1, Lav_7) | NA            | NA                                                                                                              | NA         | defined      |
| plotID               | Unique ID for each plot                                                                                                           | NA            | NA                                                                                                              | NA         | defined      |
| warming              | Wether the plot has an open top chamber or not. C = control, W = warming with open top chamber.                                   | NA            | NA                                                                                                              | NA         | defined      |
| species              | Species abbreviations, three leters of the genus and three leters of the species. Example: Sib_pro                                | NA            | NA                                                                                                              | NA         | defined      |
| uniqueID             | Unique ID for each individual.                                                                                                    | NA            | NA                                                                                                              | NA         | defined      |
| date_in              | Date when seeds were burried to test for survival in the seed bank                                                                | date          | 2019-09-09 - 2019-09-12                                                                                         | yyyy-mm-dd | recorded     |
| date_out             | Date when seeds were taken out to test for survival in the seed bank                                                              | date          | 2020-08-24 - 2020-08-27                                                                                         | yyyy-mm-dd | recorded     |
| survival_in_seedbank | Wether a seed survived in the seed bank or not.                                                                                   | numeric       | 0 - 1                                                                                                           | NA         | recorded     |
| comments             | additional information on the data                                                                                                | categorical   | Germinated before it was moved to the petri dish - Were broken during moving to petri dish                      | NA         | defined      |
| comment_survival     | Comments about how we tested if the seeds were viable or not                                                                      | categorical   | Dead embryo in cuttest after failed germination trail - Viable embryo in cuttest after failed germination trail | NA         | recorded     |

### Seeds per capsule

| Variable name     | Description                                                                                                                    | Variable type | Variable range or levels  | Unit         | How measured |
|:------------------|:-------------------------------------------------------------------------------------------------------------------------------|:--------------|:--------------------------|:-------------|:-------------|
| species           | Species abbreviations, three leters of the genus and three leters of the species. Example: Sib_pro                             | NA            | NA                        | NA           | defined      |
| siteID            | Unique site ID (3 letters, first letter capital)                                                                               | NA            | NA                        | NA           | defined      |
| date              | Date of sampling or observation                                                                                                | date          | 2020-08-23 - 2020-08-27   | yyyy-mm-dd   | recorded     |
| uniqueID          | Unique ID for each individual.                                                                                                 | NA            | NA                        | NA           | defined      |
| comments          | additional information on the data                                                                                             | categorical   | 1 immature - Too immature | NA           | defined      |
| demographic_trait | Name of trait measured                                                                                                         | NA            | NA                        | NA           | defined      |
| demographic_value | Length/width/height of leaves, leaf stalkes or stems. Or number of leaves/flowers/buds/capsules/aborted capsules/flower stems. | numeric       | 1 - 142                   | mm or counts | measured     |
| capsuleID         | unique ID of capsules                                                                                                          | NA            | NA                        | NA           | defined      |
| number_of_seeds   | Number of seeds per capsule                                                                                                    | numeric       | 0 - 65                    | counts       | recorded     |

### Species level biomass allocation

| Variable name  | Description                                                                                                       | Variable type | Variable range or levels                                         | Unit       | How measured |
|:---------------|:------------------------------------------------------------------------------------------------------------------|:--------------|:-----------------------------------------------------------------|:-----------|:-------------|
| species        | Species abbreviations, three leters of the genus and three leters of the species. Example: Sib_pro                | NA            | NA                                                               | NA         | defined      |
| siteID         | Unique site ID (3 letters, first letter capital)                                                                  | NA            | NA                                                               | NA         | defined      |
| species_group  | Grouping of species in the biomass allocation dataset                                                             | NA            | NA                                                               | NA         | defined      |
| date           | Date of sampling or observation                                                                                   | date          | 2019-06-15 - 2019-08-07                                          | yyyy-mm-dd | recorded     |
| weather        | Oberservers impression of weather during analysis                                                                 | categorical   | Cloudy - Sunny/Wind                                              | NA         | recorded     |
| registrator    | Initials or full name of the registror                                                                            | NA            | NA                                                               | NA         | recorder     |
| individual     | Number of the individual,unique within the site and plot                                                          | NA            | NA                                                               | NA         | defined      |
| plot           | Number of the plot, not permanent experimental plot                                                               | NA            | NA                                                               | NA         | defined      |
| sub_plot       | Subplot witin the plot, not permanent experimental plot                                                           | NA            | NA                                                               | NA         | defined      |
| genetID        | Unique ID for genetically similar species. Those we know are clones.                                              | NA            | NA                                                               | NA         | defined      |
| comments_field | comments registered during the field work                                                                         | categorical   | 1 dead leaf - withered flowers on their way to becoming capsules | NA         | defined      |
| comments_lab   | commentes registered during the lab work                                                                          | categorical   | 12 and 14 is 1 ind - was 2 ind                                   | NA         | defined      |
| uniqueID       | Unique ID for each individual.                                                                                    | NA            | NA                                                               | NA         | defined      |
| trait          | trait measured on the individual, either counts og leaves/flowers etc. size measurements, or biomass measurements | NA            | NA                                                               | NA         | defined      |
| value          | registered value of the corresponding trait                                                                       | numeric       | 0 - 756                                                          | NA         | measured     |

<!-- ### -->
<!-- ```{r, echo=FALSE} -->
<!-- knitr::kable() -->
<!-- ``` -->
<!-- ### -->
<!-- ```{r, echo=FALSE} -->
<!-- knitr::kable() -->
<!-- ``` -->
<!-- ### -->
<!-- ```{r, echo=FALSE} -->
<!-- knitr::kable() -->
<!-- ``` -->
<!-- ### -->
<!-- ```{r, echo=FALSE} -->
<!-- knitr::kable() -->
<!-- ``` -->
<!-- ### -->
<!-- ```{r, echo=FALSE} -->
<!-- knitr::kable() -->
<!-- ``` -->
