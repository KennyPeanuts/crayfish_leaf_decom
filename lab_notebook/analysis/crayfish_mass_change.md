# Analysis of the mass change in the crayfish for the crayfish-litter-decomp exp

## Metadata

* File Created: 2021-06-17 - KF
* File Modified: 2021-07-07 - KF - based on discussions with SH, I am re-analyzing the data using the raw masses from each crayfish to generate an estimate of individual crayfish mass change, rather than tank mass change. I am also analyzing tank total mass at the end of the exp.

### Description

This code describes the analysis of the growth and survival data from the experiment evaluating invasive crayfish species density on the growth, survival and function (leaf litter consumption) of a native crayfish species using mesocosms. The experiment was conducted in 2016 in collaboration with Sujan Henkanaththegedara.  Additonal details on the experimental design and the output of the plots can be found at [https://github.com/KennyPeanuts/crayfish_leaf_decom](https://github.com/KennyPeanuts/crayfish_leaf_decom).

## Load packages

    library("tidyverse") # for data analysis and ggplot graphing
    library("ggpubr") # for plot creation and saving

## Analysis of Crayfish Growth Rate and Tank Final Mass

### Description

Initially the change in mass was measured as the final mass of the *tank* minus the initial mass of the *tank*. This measured the change in total crayfish mass in the tank and any dead crayfish were not incuded in the final mass (because they has been eaten). The results of this analysis can be seen below in [Analysis of Tank Mass Change](#analysis-of-tank-mass-change). As a result of this way of calculating mass change, tanks that had crayfish deaths could have negative growth.  This way of calculating mass change seemed to conflate the effects of individual crayfish growth and the total mass that a tank could support.

To better distingish between patterns in these two components of the ecology, we are analyzing them separately as the estimated change in mass of an individual crayfish and the total crayfish mass in the tank at the end of the experiment.

## Import Data

    cray.raw <- read.table("./data/crayfish_mass_raw.csv", header = T, sep = ",")
    
## Create variables
    
### Calculate the mean of the stocked mass and harvested mass for species in each tank

This code calculates the mean stocked mass and harvested mass of all the crayfish in each tank and produces a new data.frame with the values. The code also calculates the total mass of each species of crayfish at the end of the experiment.
    
    mean.mass <- cray.raw %>%
      group_by(Year, Type, Tank) %>%
        summarize(mean.Stocked.Mass = mean(Stocked_Mass, na.rm = T), sd.Stocked.Mass = sd(Stocked_Mass), mean.Harvested.Mass = mean(Harvested_Mass, na.rm = T), sd.Harvested.Mass = sd(Harvested_Mass), total.Harvested.Mass = sum(Harvested_Mass, na.rm = T))
    
### Create a data.frame with the crayfish abundance of each tank not replicated for the number of crayfish

This code produces a data.frame with a single abundance value for each tank by using `unique` to select only a single abundance value from the replicate crayfish values.

    tank.abundance <- cray.raw %>%
      group_by(Year, Type, Tank) %>%
      summarize(Abundance = unique(Abundance), Total.Abundance = unique(Total_Abundance), Invasive.Abundance = unique(Invasive_Abundance))

### Create a data.frame with the treatment designation of each tank not replicated for the number of crayfish

This code produces a data.frame with a single treatment value for each tank by using `unique` to select only a single abundance value from the replicate crayfish values.

    tank.treatment <- cray.raw %>%
      group_by(Year, Type, Tank) %>%
      summarize(Treatment = unique(Treatment))
    
### Merge the mean.mass data.frame with the cray.raw data frame
    
    cray.mean <- 
      left_join(mean.mass, tank.abundance) 
    
    cray.mean <-
      left_join(cray.mean, tank.treatment)
    
### Calculate the estimated change in mass of an individual crayfish for each species.
    
This code subtracts the estmated stocked mass from the estimated harvested mass of a single crayfish in each tank.

    ind.delta.mass <- cray.mean$mean.Harvested.Mass - cray.mean$mean.Stocked.Mass
    
## Create data.frame for analyis
    
    cray.mean <- data.frame(cray.mean, ind.delta.mass)

## Variable Descriptions
    
* Year = the year of the study
* Type = the description of whether the crayfish is "Invasive" or "Native". Invasive crayfish were _P. clarkii_ in 2015, and _F. virilis_ in 2016. The Native crayfish were _C. sp. C_ in both years.
* Tank = the unique ID number of the experimental unit (tank).
* mean.Stocked.Mass = the mean mass of the crayfish stocked in the tank of a given treatment and crayfish type. (g)
* sd.Stocked.Mass = the standard deviation of the mean of the mass of the crayfish stocked in the tank of a given treatment and crayfish type. (g)
* mean.Harvested.Mass = the mean mass of the crayfish harvested from the tank of a given treatment and crayfish type at the end of the experiment. Missing (presumed dead) crayfish were not included in the mean. (g)
* sd.Harvested.Mass = the standard deviation of the mean of the mass of the crayfish harvested from the tank of a given treatment and crayfish type at the end of the experiment. (g)
* total.Harvested.Mass = the sum of the mass of the crayfish harvested from the tank of a given treatment and crayfish type at the end of the experiment. Missing (presumed dead) crayfish were not included in the sum. (g)
* Abundance = the number of crayfish of a given type stocked into each tank at the beginning of the experiment.
* Total.Abundance = the total number of crayfish in each tank (native + invasive) stocked into each tank at the beginning of the experiment.
* Invasive.Abundance = the number of invasive crayfish stocked into each tank at the beginning of the experiment.
* Treatment = the treatment level identifier, where "Ctl_Invasive" indicates a tank with only invasive crayfish, "Ctl_Native" indicates a tank with only native crayfish, "Equal" indicates a tank with equal numbers of invasive and native crayfish, "Low" indicates a tank with more native than invasive crayfish, and "High" indicates a tank with more invasive than native crayfish.
* ind.delta.mass = the estimated change in mass of a crayfish in a tank over the course of the experiment measured as the mean harvested mass minus the mean stocked mass. (g)
    
## Data Visualizations
    
    ggplot(subset(cray.mean, Year == "2016"), mapping = aes(y = ind.delta.mass, x = Total.Abundance, color = Type)) +
             geom_point() +
             geom_smooth(
               method = "lm"
             ) +
             theme_classic()
    
    ggplot(subset(cray.mean, Year == "2016"), mapping = aes(y = total.Harvested.Mass, x = Total.Abundance, color = Type)) +
             geom_point() +
             geom_smooth(
               method = "lm"
             ) +
             theme_classic()
    
## Analysis of Tank Mass Change

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
    
### Make a new data.frame without the tanks with negative growth.
    
Three of the tanks had an average growth that was less than 0, indicating that on average, the crayfish lost mass over the course of the experiment. This average loss of mass may indicate that these tanks were not representative of the overall experimental conditions and therefore do not provide a proper test of the treatment (i.e., crayfish density).

    ##################################################
    # Tanks that had negative crayfish growth

    > cray[cray$MassChange < 0, ]
       Species   Density  MassChange  LogMassChange Survival Sp.Abundance Total.Abundance
    6  Native    equal         -1.80            NA        25            4               8
    45 Invasive  low           -7.85            NA         0            2               6
    47 Invasive  low           -1.05            NA       100            2               6
      Final.Sp.Abundance Final.Total.Abundance Invasive.Abundance Final.Invasive.Abundance
    6                  1                     4                  4                        3
    45                 0                     4                  2                        4
    47                 2                     5                  2                        3
    
    ################################################## 

    cray.pos.trunk <- cray[cray$MassChange >= 0, ]
    
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
#### By Species and Density
    
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
    
    cray.pos.trunk %>%
      group_by(Species, Density) %>%
        summarize(mean = mean(MassChange), sd = sd(MassChange), min = min(MassChange), max = max(MassChange))
    
    ################################################## 
    # Summary of the change in mass without the tanks with negative growth
    `summarise()` has grouped output by 'Species'. You can override using the `.groups` argument.
    # A tibble: 8 x 6
    # Groups:   Species [2]
    Species  Density  mean    sd   min   max
    <chr>    <chr>   <dbl> <dbl> <dbl> <dbl>
    1 Invasive control  5.12 1.53  2.61   6.7 
    2 Invasive equal    3.01 1.59  0.942  5.55
    3 Invasive high     2.67 0.338 2.15   3.1 
    4 Invasive low      4.99 3.95  1.45  10.6 
    5 Native   control  2.42 0.778 1.72   3.62
    6 Native   equal    1.63 0.200 1.48   1.94
    7 Native   high     1.53 0.554 1.05   2.33
    8 Native   low      2.86 0.954 1.23   3.75
    
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
    
## Mass Change Analysis
### Comparison of the treatment groups on the change in mass
    
    anova(lm(MassChange ~ Density * Species, data = cray))
   
    ##################################################
    # 2-way ANOVA of the change in mass by treatment and species
    
    Analysis of Variance Table
    
    Response: MassChange
    Df  Sum Sq Mean Sq F value  Pr(>F)  
    Density          3  23.948  7.9826  1.3711 0.26546  
    Species          1  17.062 17.0623  2.9306 0.09466 .
    Density:Species  3  23.291  7.7637  1.3335 0.27703  
    Residuals       40 232.883  5.8221       
    
    ################################################## 
    
    mass_by_treat <-
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
    
    ggexport(mass_by_treat, width = 7, height = 7, filename = "./output/plots/mass_by_treat.pdf")
    
![Plot of mass change by treatment](https://github.com/KennyPeanuts/crayfish_leaf_decom/blob/master/lab_notebook/output/plots/mass_by_treat.pdf)
    
The analysis by treatment group shows no effect of the treatment group and only a marginally significant effect of species.

    cray %>%
      group_by(Species) %>%
        summarize(mean = mean(MassChange), sd = sd(MassChange), min = min(MassChange), max = max(MassChange))
    
    ##################################################
    # Summary of mass change by species
    
    # A tibble: 2 x 5
    Species   mean    sd   min   max
    <chr>    <dbl> <dbl> <dbl> <dbl>
    1 Invasive  3.16  3.29 -7.85 10.6 
    2 Native    1.97  1.16 -1.8   3.75 
    
    ################################################## 
    
Given that the number of crayfish in the tanks could be considered a continuous variable, it may make more sense to think of mass change as a function of total crayfish in the tank.
    
    
    ggplot(cray, mapping = aes(y = MassChange, x = Total.Abundance, color = Species)) +
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
    
    ggplot(cray, mapping = aes(y = LogMassChange, x = Total.Abundance, color = Species)) +
      geom_point() +
      geom_smooth(
        method = "lm"
      ) +
      theme_classic()
    
    ggplot(cray, mapping = aes(y = LogMassChange, x = Invasive.Abundance, color = Species)) +
      geom_point() +
      geom_smooth(
        method = "lm"
      ) +
      theme_classic()
    
    summary(lm(MassChange ~ Total.Abundance * Species, data = cray))
    summary(lm(MassChange ~ Invasive.Abundance * Species, data = cray))
     