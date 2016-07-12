# Analysis of the leaf mass loss from the decomposition 

## Metadata

* File created on 6 July 2016

* modified 12 July 2016 -KF- added summaries of the chem data

## Description

This is the code to analyze the effect of crayfish community on the tank chemistry the experiment evaluating the impact of in invasive and native crayfish.

## R Code

### Import data

    ysi <- read.table("./data/tank_YSI.csv", header = T, sep = ",")
    treat <- read.table("./data/tank_map.csv", header = T, sep = ",")

### Merge tank map with YSI data

    ysi <- merge(ysi, treat, by = "tank")

### Summaries of the tank data by time point

#### pH

    tapply(ysi$pH, ysi$sampling, summary)

~~~~
  
  $final
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
6.740   7.512   7.670   7.716   7.860   8.530 

$initial
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
7.510   7.748   7.945   8.036   8.235   9.120 

$second
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
7.320   7.822   8.050   8.108   8.335   9.000 

~~~~
  
#### Percent DO
  
    tapply(ysi$percDO, ysi$sampling, summary)

~~~~

$final
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
79.70   86.15   90.25   91.17   95.55  106.80 

$initial
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
101.0   108.0   113.9   113.4   119.7   125.9 

$second
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
71.90   96.62   99.50   98.72  102.10  108.10 

~~~~
  
#### DO Concentration (mg/L)
  
    tapply(ysi$concDO, ysi$sampling, summary)

~~~~
  
$final
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
6.690   7.188   7.485   7.557   7.847   8.770 

$initial
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
8.090   8.668   9.105   9.136   9.578  10.410 

$second
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
5.830   7.925   8.095   8.067   8.380   9.000 

~~~~
  
#### Temperature (dC)
  
    tapply(ysi$temp, ysi$sampling, summary)

~~~~
  
$final
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
23.80   24.42   24.76   24.61   24.87   25.29 

$initial
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
23.26   26.06   26.51   26.27   26.97   27.58 

$second
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
23.71   25.04   25.60   25.45   25.86   26.97 

~~~~


### Analyze pH by treatment

    plot(pH ~ treatment, data = ysi, subset = sampling == "initial")
    plot(pH ~ treatment, data = ysi, subset = sampling == "second")
    plot(pH ~ treatment, data = ysi, subset = sampling == "final")
    plot(pH ~ sampling, data = ysi)
    anova(lm(pH ~ treatment * sampling, data = ysi))

    plot(temp ~ treatment, data = ysi, subset = sampling == "initial")
    plot(temp ~ treatment, data = ysi, subset = sampling == "second")
    plot(temp ~ treatment, data = ysi, subset = sampling == "final")
    plot(temp ~ sampling, data = ysi)

    plot(percDO ~ treatment, data = ysi, subset = sampling == "initial")
    plot(percDO ~ treatment, data = ysi, subset = sampling == "second")
    plot(percDO ~ treatment, data = ysi, subset = sampling == "final")
    plot(percDO ~ sampling, data = ysi)

    plot(light ~ treatment, data = ysi, subset = sampling == "second", ylim = c(0, 2000))
    plot(light ~ treatment, data = ysi, subset = sampling == "final", ylim = c(0, 2000))
    plot(light ~ sampling, data = ysi)
