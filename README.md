
## Sckrabulis _et al_. - Direct onshore wind predicts daily swimmer's itch (avian schistosome) incidence at a Michigan beach

Wind direction predicts swimmer's itch

---

### Authors and maintainers

Jason P. Sckrabulis: jason.sckrabulis@gmail.com

Alan R. Flory: arflory@yahoo.com

Thomas R. Raffel: raffel@oakland.edu

>Conceptualization: TRR, JPS & ARF. Methodology: TRR, JPS & ARF. Analysis: JPS & TRR. Writing - review & editing: JPS & TRR. Funding: ARF. Literature searching: JPS & TRR.

### Issues and suggestions

Please email jason.sckrabulis@gmail.com with any issues or suggestions with the supplemental dataset, or submit via the GitHub issues tab for this [repository](https://github.com/jasonsckrabulis/sckrabulis_etal_wind_predicts_si/issues).
For questions concerning the manuscript, please email the corresponding author at jason.sckrabulis@gmail.com.

### Change log

* Jan 7, 2020: First full commit

### Citation

Please cite work as:
>TBD

---

### Abstract

Swimmer’s itch (SI) is a painful rash caused by skin penetration by free-swimming infectious cercariae of avian schistosomes, snail-borne helminth parasites related to the causative agents of human schistosomiasis. The goal of this study was to determine if commonly collected environmental data could be used to predict daily fluctuations in swimmer’s itch incidence at an inland beach in northwestern Michigan. Lifeguards collected daily data over four summers, including the number of self-reported SI cases, total swimmers, water temperature, wind speed, and wind direction. Mixed-effects binomial regression revealed that wind direction, wind speed, and time of day were the best predictors of daily SI risk. Swimmers entering the water in the morning or on days with direct onshore wind perpendicular to the shoreline had the greatest SI risk. However, there was a negative effect of wind speed after accounting for direction, where SI risk was greatest on days with a gentle breeze originating directly offshore. These results suggest that at this beach, direct onshore winds generate a surface-water current that causes SI cercariae to aggregate in the shallow waters used by swimmers. Data are needed from additional sites to confirm whether onshore wind is a generally important driver of SI incidence.

---

### Repository contents

* README.md
* data  
   Folder of experimental data as .csv files used in data analysis for manuscript  
   * Variable descriptions.txt  
   * CSA data among day variation.csv (dataset without time of day effect)  
   * CSA data within day variation.csv (dataset with time of day effect)

---

### Variable descriptions

**CSA data among day variation.csv**

Variable Name | Description
--- | ---
Date | Date of data collection
CalDay | Calendar day (categorical variable)
SiAM | Number of SI cases reported in the morning session
SiPM | Number of SI cases reported in the afternoon session
SiTotal | Total number of SI cases reported per day (SiAM+SiPM)
PrevSi1/3/5/7 | Average number of SI cases reported in the previous 1/3/5/7 days
WaterTempF | Surface water temperature within the swim area, measured in Farenheit
WaterTempC | Calculated surface water temperature within the swim area in Celsius (from WaterTempF)
AvgPrevTemp1/3/5/7 | Average surface water temperature in the swim area in the previous 1/3/5/7 days
WindDir | Nearest 8-way wind direction (N, NE, E, SE, S, SW, W, NW)
WindDirGroup1 | Nearest 4-way 'paired' wind direction (N+NE, E+SE, S+SW, W+NW)
WindDirGroup2 | Nearest 4-way 'paired' wind direction (NE+E, SE+S, SW+W, NW+N)
WindVel | Wind velocity in miles per hour
SwimAM | Number of swimmers reported in the morning session
SwimPM | Number of swimmers reported in the afternoon session
SwimTotal | Total number of swimmers reported per day (SwimAM+SwimPM)
NWSprecip1 | Precipitation present, where 'trace' was reported as present (binary variable)
NWSprecip2 | Total inches of precipitation per day, where 'trace' was reported as 0 (continuous variable)

**CSA data within day variation.csv**

All variables are the same as the among day variation dataset except for the following differences (Note that the temporal variables are still described as averages of the _previous day_):

Variable Name | Description
--- | ---
TimeOfDay | Time of day (AM: morning session; PM: afternoon session)
SiTotal | Number of SI cases reported _during that session_
SwimTotal | Number of swimmers reported _during that session_
