# Analysis of the leaf mass loss from the decomposition 

## Metadata

* File created on 6 July 2016

## Description

This is the code to analyze the effect of crayfish community on the tank chemistry the experiment evaluating the impact of in invasive and native crayfish.

## R Code

### Import data

    ysi <- read.table("./data/tank_YSI.csv", header = T, sep = ",")
    treat <- read.table("./data/tank_map.csv", header = T, sep = ",")

### Merge tank map with YSI data

    ysi <- merge(ysi, treat, by = "tank")

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
