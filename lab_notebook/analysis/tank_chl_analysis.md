# Analysis of the tank chl

## Metadata

* File created on 6 July 2016

* modified 7 July 2016 - KF - added final CHL to the analysis

## Description

This is the code to analyze the effect of crayfish community on the tank chlorophyll the experiment evaluating the impact of in invasive and native crayfish.

## R Code

### Import data

    ysi <- read.table("./data/tank_YSI.csv", header = T, sep = ",")
    chl <- read.table("./data/tank_chl.csv", header = T, sep = ",")
    treat <- read.table("./data/tank_map.csv", header = T, sep = ",")

### Merge tank map with chem data

    chl <- merge(chl, treat, by = "tank")

### Analyze Chl by treatment

    plot(Chl ~ treatment, data = chl)
    plot(Chl ~ treatment, data = chl, subset = Date == "2016-06-03")
    plot(Chl ~ treatment, data = chl, subset = Date == "2016-06-16")
    plot(Chl ~ treatment, data = chl, subset = Date == "2016-06-30")

    plot(Phaeo ~ treatment, data = chl)
    plot(Phaeo ~ treatment, data = chl, subset = Date == "2016-06-03")
    plot(Phaeo ~ treatment, data = chl, subset = Date == "2016-06-16")
    plot(Phaeo ~ treatment, data = chl, subset = Date == "2016-06-30")

    plot(Chl ~ Date, data = chl)
    plot(Phaeo ~ Date, data = chl)
    
    plot(Phaeo ~ Chl, data = chl)    

    anova(lm(Chl ~ treatment * Date, data = chl))

    plot(Chl ~ tank, data = chl)
    plot(Phaeo ~ tank, data = chl)

    plot(pH ~ percDO, data = ysi)

