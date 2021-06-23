# Analysis of the mass change in the crayfish for the crayfish-litter-decomp exp

## Metadata

* File Created: 2021-06-17 - KF

### Description

This code describes the analysis of the growth and survival data from the experiment evaluating invasive crayfish species density on the growth, survival and function (leaf litter consumption) of a native crayfish species using mesocosms. The experiment was conducted in 2016 in collaboration with Sujan Henkanaththegedara.  Additonal details on the experimental design and the output of the plots can be found at [https://github.com/KennyPeanuts/crayfish_leaf_decom](https://github.com/KennyPeanuts/crayfish_leaf_decom).

## Load packages

    library("tidyverse") # for data analysis and ggplot graphing
    library("ggpubr") # for plot creation and saving

## Import Data

    cray <- read.table("./data/crayfish_growth_surv.csv", header = T, sep = ",")
    
## Create Variables
    
    Sp.Abundance <- c(rep(4, 24), rep(4, 12), rep(6, 6), rep(2, 6))
    Total.Abundance <- c(rep(8, 6), rep(10, 6), rep(6, 6), rep(4, 6), rep(4, 6), rep(8, 6), rep(10, 6), rep(6, 6))
    Final.Sp.Abundance <- Sp.Abundance * (cray$Survival / 100)
    Final.Total.Abundance <- rep(Final.Sp.Abundance[cray$Species == "Native"] + Final.Sp.Abundance[cray$Species == "Invasive"], 2)
    Final.Invasive.Abundance <- c((Final.Total.Abundance[cray$Species == "Native"] - Final.Sp.Abundance[cray$Species == "Native"]), (Final.Total.Abundance[cray$Species == "Invasive"] - Final.Sp.Abundance[cray$Species == "Invasive"]))
    Invasive.Abundance <- c(rep(4, 6), rep(6, 6), rep(2, 6), rep(0, 6), rep(4, 6), rep(4, 6), rep(6, 6), rep(2, 6))
    
## Add Created Variables to the data.frame
    
    cray <- data.frame(cray, Sp.Abundance, Total.Abundance, Final.Sp.Abundance, Final.Total.Abundance, Invasive.Abundance, Final.Invasive.Abundance)

### Variable Descriptions    
    
* Species = the designation of native or invasive crayfish, where 'Native' = _Cambarus_ _sp C_ and 'Invasive' = _Faxonius virilis_.

* Density = the treatment designation where 'control' = 4 individuals of either the native or the invasive species alone, 'low' = 4 individuals of the native and 2 individuals of the invasive, 'equal' = 4 individuals of both the native and the invasive species, and 'high' = 4 individuals of the native species and 6 individuals of the invasive species.
    
* MassChange = the average change in mass of all of the crayfish of a group (native or invasive) over the course of the experiment (g). Negative numbers indicate an average loss of mass.
    
* LogMassChange = the log (base 10) of MassChange
    
* Survival = the percent of the original number of individuals that were alive a the end of the experiment.
    
* Sp.Abundance = the number of each crayfish of a given species in a treatment at the beginning of the experiment.

* Total.Abundance = the total number of crayfish in a treatment at the beginnig of the experiment.
    
* Final.Sp.Abundance = the number of crayfish of a given species in a treatment at the end of the experiment.

* Final.Total.Abundance = the number of crayfish in a treatment at the end of the experiment.

* Invasive.Abundance = the number of 'Invasive' crayfish in the a treatment at the beginning of the experiment. 

* Final.Invasive.Abundance = the number of 'Invasive' crayfish in the a treatment at the end of the experiment. 
        
    
## Variable Summary
### MassChange
    
    cray %>%
      group_by(Species, Density) %>%
        summarize(mean = mean(MassChange), sd = sd(MassChange), min = min(MassChange), max = max(MassChange))

    ##################################################     
    # Summary of the change is mass of the crayfish 
    
    `summarise()` has grouped output by 'Species'. You can override using the `.groups` argument.
    # A tibble: 8 x 6
    # Groups:   Species [2]
    Species  Density  mean    sd    min   max
    <chr>    <chr>   <dbl> <dbl>  <dbl> <dbl>
    1 Invasive control  5.12 1.53   2.61   6.7 
    2 Invasive equal    3.01 1.59   0.942  5.55
    3 Invasive high     2.67 0.338  2.15   3.1 
    4 Invasive low      1.84 6.14  -7.85  10.6 
    5 Native   control  2.42 0.778  1.72   3.62
    6 Native   equal    1.06 1.41  -1.8    1.94
    7 Native   high     1.53 0.554  1.05   2.33
    8 Native   low      2.86 0.954  1.23   3.75
    
    ################################################## 
    
### Survival
    
    cray %>%
      group_by(Species, Density) %>%
        summarize(mean = mean(Survival), sd = sd(Survival), min = min(Survival), max = max(Survival))

    ##################################################     
    # Summary of the percent crayfish survied to the end of the experiment by species and treatment
    
    `summarise()` has grouped output by 'Species'. You can override using the `.groups` argument.
    # A tibble: 8 x 6
    # Groups:   Species [2]
    Species  Density  mean    sd   min   max
    <chr>    <chr>   <dbl> <dbl> <dbl> <dbl>
    1 Invasive control  83.3  12.9  75     100
    2 Invasive equal    87.5  13.7  75     100
    3 Invasive high     86.1  12.5  66.7   100
    4 Invasive low      75    41.8   0     100
    5 Native   control  91.7  12.9  75     100
    6 Native   equal    79.2  29.2  25     100
    7 Native   high     87.5  20.9  50     100
    8 Native   low      70.8  24.6  25     100
    
    ################################################## 
    
## Exploratory Plots
    
    ggplot(cray, mapping = aes(y = MassChange, x = Density)) +
      facet_wrap(
        ~Species
        ) +
      geom_jitter(
        width = 0.1,
        col = 8
      ) +
      stat_summary(
        fun = mean,
        fun.min = function(x) mean(x) - sd(x),
        fun.max = function(x) mean(x) + sd(x)
      ) +
      theme_classic()

    ggplot(cray, mapping = aes(y = MassChange, x = Final.Total.Abundance, color = Species)) +
      geom_point() +
      geom_smooth(
        method = "lm"
      ) +
      theme_classic()
    
    ggplot(cray, mapping = aes(y = LogMassChange, x = Final.Total.Abundance, color = Species)) +
      geom_point() +
      geom_smooth(
        method = "lm"
      ) +
      theme_classic()
    
    ggplot(cray, mapping = aes(y = MassChange, x = Final.Invasive.Abundance, color = Species)) +
      geom_point() +
      geom_smooth(
        method = "lm"
      ) +
      theme_classic()
    
    ggplot(cray, mapping = aes(y = MassChange, x = Total.Abundance, color = Species)) +
      geom_point() +
      geom_smooth(
        method = "lm"
      ) +
      theme_classic()
    
    ggplot(cray, mapping = aes(y = MassChange, x = Invasive.Abundance, color = Species)) +
      geom_point() +
      geom_smooth(
        method = "lm"
      ) +
      theme_classic()
    
    summary(lm(MassChange ~ Total.Abundance * Species, data = cray))
    summary(lm(MassChange ~ Invasive.Abundance * Species, data = cray))
    anova(lm(log10(MassChange + 10) ~ Density * Species, data = cray))
     