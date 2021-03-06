# Analysis of the leaf mass loss from the decomposition 

## Metadata

* File created on 10 June 2016 - KF

* 22 june 2016 - KF - added T2 to the analysis

* 5 July 2016 - KF - added T3 to the analysis

* 12 July 2016 - KF - remade individual time point figures for poster and altered code to fix broken variable names from leaf.AFDM data frame

* 13 July 2016 - KF - added analysis of summary stats for time points

* 1 Dec 2016 - KF - calculated k for each tank and then ran ANOVA

* 15 May 2017 - KF - removed tanks 3 and 4 from the analysis and recalculated the stats. Also, streamlined the function 

* 17 May 2017 - KF - calculated k for each treatment level and made figure 

* 18 May 2017 - KF - updated k figure for manuscript

* 27 May 2021 - KF - cleaned up the script to match the current lab standards and created a new figure for k by treatment with ggplot

## Description

This is the code to calculate the mass loss of the leaf packs in the experiment evaluating the impact of in invasive and native crayfish.

## R Code

### Add required packages

    library("tidyverse")
    library("ggpubr")

### Import data

    leaf.initial <- read.table("./data/initial_leaf_mass.csv", header = T, sep = ",")
    leaf.T1 <- read.table("./data/T1_leaf_mass.csv", header = T, sep = ",")
    leaf.T2 <- read.table("./data/T2_leaf_mass.csv", header = T, sep = ",")
    leaf.T3 <- read.table("./data/T3_leaf_mass.csv", header = T, sep = ",")
    treat <- read.table("./data/tank_map.csv", header = T, sep = ",")

#### Merge treatment information

    leaf.T1 <- merge(leaf.T1, treat, by.x = "BagTank", by.y = "tank")
    leaf.T2 <- merge(leaf.T2, treat, by.x = "BagTank", by.y = "tank")
    leaf.T3 <- merge(leaf.T3, treat, by.x = "BagTank", by.y = "tank")

### Calculate variables
#### Initial AFDM
    
This calculates the AFDM of the leaves before they were incubated in the tanks.

    initialDM <- leaf.initial$CrucLeafDM - leaf.initial$CrucMass
    initialAM <- leaf.initial$CrucAM - leaf.initial$CrucMas
    AFDM <- initialDM - initialAM
    leaf.initial <- data.frame(leaf.initial, AFDM)
    mean.initial.AFDM <- mean(AFDM) 
    
#### T1 AFDM

    T1DM <- leaf.T1$CrucLeafDM - leaf.T1$CrucMass
    T1AM <- leaf.T1$CrucAM - leaf.T1$CrucMass
    AFDM <- T1DM - T1AM
    leaf.T1 <- data.frame(leaf.T1, AFDM)

#### T2 AFDM

    T2DM <- leaf.T2$CrucLeafDM - leaf.T2$CrucMass
    T2AM <- leaf.T2$CrucAM - leaf.T2$CrucMass
    AFDM <- T2DM - T2AM
    leaf.T2 <- data.frame(leaf.T2, AFDM)

#### T3 AFDM

    T3DM <- leaf.T3$CrucLeafDM - leaf.T3$CrucMass
    T3AM <- leaf.T3$CrucAM - leaf.T3$CrucMass
    AFDM <- T3DM - T3AM
    leaf.T3 <- data.frame(leaf.T3, AFDM)

### Analyze Mass Lost with time 

#### Remove tanks with C. longulus in them

Due to an error when stocking, tanks 3 and 4 had C. longulus individuals added to in addition to the target species.  

These tanks were removed from the analysis.

    # create a single data frame with the T1 - T3 AFDM
    leaf.AFDM.raw <- rbind(leaf.T1, leaf.T2, leaf.T3)

    # remove tanks 3 and 4 from the dataset
    leaf.AFDM <- leaf.AFDM.raw[leaf.AFDM.raw$BagTank != 3 & leaf.AFDM.raw$BagTank != 4,]
    
    # convert to percent remaining in the tank
    percAFDM.rem <- (leaf.AFDM$AFDM / mean.initial.AFDM) * 100

    # calculate the time in the tank
    #date <- c(as.Date(leaf.T1$Date), as.Date(leaf.T2$Date), as.Date(leaf.T3$Date)) 
    #time.step <- c(rep("T1", 30), rep("T2", 30), rep("T3", 30))
    # calculate the number of days elapsed
    days.elapsed <- as.Date(leaf.AFDM$Date) - as.Date(unique(leaf.initial$Date))
    days.elapsed <- as.numeric(days.elapsed)

    # add calc variables to data frame
    leaf.AFDM <- data.frame(leaf.AFDM, percAFDM.rem, days.elapsed)

### Calculate K for each tank
#### Create a function that calculates k for each tank

The function below, runs a linear model on the natural log of the percent leaf mass remaining by the number of days in the tank.  The function then extracts only the slope of the line and makes a data frame that matches the slope, tank number, treatment, and block.

Since two of the tanks had a percent mass remiaining of 0 on the final day, 1 was added to the percent mass remaining to avoid taking the natural log of 0.

##### Function designation
    k.tank <- function() {
     k <- numeric(0)
     treat <- character(0)
     tank_name = unique(leaf.AFDM$BagTank)
     index = seq_along(tank_name)
     
     for(i in index) { 
      treat[i] <- as.character(leaf.AFDM$treatment[index == i])[1]
      k[i] <- coef(summary(lm(log(leaf.AFDM$percAFDM.rem + 1)[index == i] ~ leaf.AFDM$days.elapsed[index == i])))[2, 1] # this extracts the slope (col 2, row 1) from the matrix of coefficents produced by the lm
     }

     data.frame(tank_name, treat, k)
    }

    # run the function
    k.list <- k.tank()
    
##### Replace the treatment names in k.list with more logical level names
    k.list$treat <- factor(k.list$treat, levels = c("N", "L", "E", "H", "I"))

#### Print Data 

     k.list

    ##################################################
    > k.list
    tank_name treat           k
    1          1     N -0.03099643
    2          2     L -0.10035310
    3          5     I -0.01454786
    4          6     N -0.04153198
    5          7     E -0.01528752
    6          8     I -0.01706877
    7          9     L -0.02865343
    8         10     H -0.04633552
    9         11     I -0.06030412
    10        12     H -0.04291484
    11        13     N -0.03776685
    12        14     L -0.03969227
    13        15     E -0.10035444
    14        16     E -0.06440327
    15        17     L -0.05480065
    16        18     N -0.07506679
    17        19     H -0.08681979
    18        20     I -0.10375450
    19        21     I -0.23209143
    20        22     E -0.22734437
    21        23     H -0.08669731
    22        24     L -0.09163671
    23        25     N -0.03348355
    24        26     H -0.12604939
    25        27     L -0.02961575
    26        28     N -0.02305021
    27        29     E -0.09646242
    28        30     I -0.01813098
    
    ################################################## 
    
#### Summarize K

    summary(k.list$k)
    sd(k.list$k)

    ################################################## 
    # Summary of the decay coefficient for the leaves in the tank (day^-1)
    
     Min.      1st Qu.   Median     Mean       3rd Qu.     Max.      SD 
    -0.23210   -0.09284  -0.05057   -0.06876   -0.03065    -0.01455  0.05552608

    ################################################## 
    
#### Summarize k by treatment level
    
    tapply(k.list$k, k.list$treat, summary)
    tapply(k.list$k, k.list$treat, sd)

    ################################################## 
    # Summary of k for each treatment level (day^-1)
    
    $E
    Min.      1st Qu.   Median     Mean      3rd Qu.     Max. 
    -0.22730  -0.10040  -0.09646   -0.10080  -0.06440    -0.01529 

    $H
    Min.      1st Qu.   Median     Mean      3rd Qu.     Max. 
    -0.12600  -0.08682  -0.08670   -0.07776  -0.04634   -0.04291 

    $I
    Min.      1st Qu.   Median     Mean      3rd Qu.     Max. 
    -0.23210  -0.09289  -0.03922   -0.07432  -0.01733    -0.01455 

    $L
    Min.      1st Qu.   Median     Mean      3rd Qu.     Max. 
    -0.10040  -0.08243  -0.04725   -0.05746  -0.03213    -0.02865 

    $N
    Min.      1st Qu.   Median     Mean       3rd Qu.     Max. 
    -0.07507  -0.04059  -0.03563   -0.04032   -0.03162    -0.02305 

    # Standard deviation of k by treatment level

         E             H            I            L            N 
         0.07853278    0.03426187   0.08480834   0.03141895   0.01815069 

    ################################################## 
         
#### Analyze k by treatment 

    anova(lm(k ~ treat, data = k.list))

    ################################################## 
    # ANOVA table for the effect of treatment on k

    Analysis of Variance Table

    Response: k
                Df   Sum Sq   Mean Sq    F value   Pr(>F)
    treat        4   0.011335 0.0028336  0.9063    0.4767
    Residuals   23   0.071910 0.0031265     

    ################################################## 

#### Test of the invasive vs native control

    t.test(k.list$k[k.list$treat == "N"], k.list$k[k.list$treat == "I"])

    ################################################## 
    # T test if k is equal between the Native-only and Invasive-only treatments
    Welch Two Sample t-test

    data:  k.list$k[k.list$treat == "N"] and k.list$k[k.list$treat == "I"]
    t = 0.9603, df = 5.457, p-value = 0.3775
    alternative hypothesis: true difference in means is not equal to 0
    95 percent confidence interval:
    -0.0547702  0.1227708
    sample estimates:
    mean of x     mean of y 
    -0.04031597   -0.07431628 
    
    x = N and y = I

    ################################################## 


#### Repeated Measures ANCOVA 
##### Load Packages
    
The `lmerTest` package is required
    
    library("lmerTest")
    
    (k.mod <- lmer(percAFDM.rem ~ 1 + days.elapsed * treatment + (1|BagTank), data = leaf.AFDM))

    ################################################## 
    # Linier Mixed Model of the effects of treatment and time on percent leaf mass remaining where tank (BagTank) is the random variable 
    
    Linear mixed model fit by REML ['merModLmerTest']
    Formula: percAFDM.rem ~ 1 + days.elapsed * treatment + (1 | BagTank)
    Data: leaf.AFDM
    REML criterion at convergence: 659.9343
    Random effects:
    Groups   Name        Std.Dev.
    BagTank  (Intercept)  7.626  
    Residual             13.441  
    Number of obs: 84, groups:  BagTank, 28
    Fixed Effects:
            (Intercept)             days.elapsed               treatmentH  
                97.2299                  -3.1741                  -4.5901  
             treatmentI               treatmentL               treatmentN  
                 5.3940                   3.0902                  -5.5868  
    days.elapsed:treatmentH  days.elapsed:treatmentI  days.elapsed:treatmentL  
                     0.1419                   0.6111                   0.3571  
    days.elapsed:treatmentN  
                     0.9670  

    ################################################## 
                     
##### Test of the Linear Mixed Model 
 
    anova(k.mod)     

    ################################################## 
    # ANOVA Test of Linier Mixed Model of the effects of treatment and time on percent leaf mass remaining where tank (BagTank) is the random variable 
    Analysis of Variance Table of type III  with  Satterthwaite approximation for degrees of freedom
                           SumSq    MeanSq NumDF  DenDF     F.value   Pr(>F)    
    days.elapsed           48338     48338     1   51.001   267.582   <2e-16 ***
    treatment                396        99     4   63.767     0.549   0.7007    
    days.elapsed:treatment   754       188     4   51.001     1.043   0.3944    

    ################################################## 
 
### Summarize AFDM remaining by days of incubation

    tapply(leaf.AFDM$AFDM, leaf.AFDM$days.elapsed, summary)
    tapply(leaf.AFDM$AFDM, leaf.AFDM$days.elapsed, sd)

    ################################################## 
    # Summary of AFDM (g) in the litter bags by number of days elapsed 

    $`3`
    Min.    1st Qu.  Median    Mean    3rd Qu.    Max. 
    0.7647  0.8404   0.8737    0.8881  0.9367     1.0660 

    $`10`
    Min.    1st Qu.  Median    Mean    3rd Qu.    Max. 
    0.2342  0.6420   0.7440    0.7299  0.8073     0.9842 

    $`24`
    Min.     1st Qu.  Median    Mean    3rd Qu.    Max. 
    0.0000   0.1159   0.3178    0.3120  0.4315     0.7865 

    ## SD of the AFDM (g) in the litter bags by number of days elapsed

    3             10            24 
    0.07236243    0.16168339    0.21685885 

    ################################################## 

#### Summarize percent AFDM remaining by days of incubation

    tapply(leaf.AFDM$percAFDM.rem, leaf.AFDM$days.elapsed, summary)
    tapply(leaf.AFDM$percAFDM.rem, leaf.AFDM$days.elapsed, sd)

    ################################################## 
    # Summary of the Percent AFDM remaining in the litter bags by the number of days of incubation
    
    $`3`
    Min.    1st Qu.  Median    Mean    3rd Qu.    Max. 
    75.26   82.71    85.98     87.40   92.18      104.90 

    $`10`
    Min.    1st Qu.  Median    Mean    3rd Qu.    Max. 
    23.05   63.18    73.22     71.83   79.45      96.86 

    $`24`
    Min.    1st Qu.  Median    Mean    3rd Qu.    Max. 
    0.00     11.40    31.27     30.70   42.47      77.40 

    ## SD of the percent AFDM remaining in the litter bags by the number of days of incubation
   
    > tapply(leaf.AFDM$percAFDM.rem, leaf.AFDM$days.elapsed, sd)
    
    3          10          24 
    7.121458   15.911869   21.341893 
    
    ################################################## 
  
### Calculation of k for each group

    N.k <- lm(log(percAFDM.rem + 1) ~ days.elapsed, data = leaf.AFDM, subset = treatment == "N")
    I.k <- lm(log(percAFDM.rem + 1) ~ days.elapsed, data = leaf.AFDM, subset = treatment == "I")
    L.k <- lm(log(percAFDM.rem + 1) ~ days.elapsed, data = leaf.AFDM, subset = treatment == "L")
    E.k <- lm(log(percAFDM.rem + 1) ~ days.elapsed, data = leaf.AFDM, subset = treatment == "E")
    H.k <- lm(log(percAFDM.rem + 1) ~ days.elapsed, data = leaf.AFDM, subset = treatment == "H")

#### Decay Model Summaries
    
##### Summary of the decay model for the Native-only treatment (N)

    summary(N.k)

    ################################################## 
    Call:
    lm(formula = log(percAFDM.rem + 1) ~ days.elapsed, data = leaf.AFDM, subset = treatment == "N")

    Residuals:
    Min        1Q        Median   3Q       Max 
    -0.67002   -0.07375  0.00168  0.15088  0.37404 

    Coefficients:
                  Estimate   Std. Error  t value   Pr(>|t|)    
    (Intercept)   4.60508    0.09354     49.233    < 2e-16 ***
    days.elapsed  -0.04032   0.00619     -6.513    7.15e-06 ***

    Residual standard error: 0.2293 on 16 degrees of freedom
    Multiple R-squared:  0.7261, Adjusted R-squared:  0.709 
    F-statistic: 42.42 on 1 and 16 DF,  p-value: 7.152e-06

    ################################################## 

##### Summary of the decay model for the Invasive-only treatment (I)

    summary(I.k)

    ##################################################
    Call:
    lm(formula = log(percAFDM.rem + 1) ~ days.elapsed, data = leaf.AFDM, subset = treatment == "I")

    Residuals:
     Min      1Q         Median     3Q        Max 
    -3.12379  -0.14828   -0.00439   0.33907   1.23807 

    Coefficients:
                 Estimate   Std. Error  t value   Pr(>|t|)    
    (Intercept)   4.90738    0.39084     12.556    1.06e-09 ***
    days.elapsed  -0.07432   0.02586     -2.873    0.011 *  

    Residual standard error: 0.958 on 16 degrees of freedom
    Multiple R-squared:  0.3404, Adjusted R-squared:  0.2991 
    F-statistic: 8.256 on 1 and 16 DF,  p-value: 0.01104

    ##################################################
 
##### Summary of the decay model for the Low-Density treatment (L)

    summary(L.k)

    ##################################################
    Call:
    lm(formula = log(percAFDM.rem + 1) ~ days.elapsed, data = leaf.AFDM, subset = treatment == "L")

    Residuals:
     Min       1Q        Median    3Q        Max 
    -0.88230   -0.06222  0.06356   0.16757   0.57218 

    Coefficients:
                  Estimate    Std. Error   t value   Pr(>|t|)    
    (Intercept)   4.76914     0.15195      31.387    8.42e-16 ***
    days.elapsed  -0.05746    0.01006      -5.714    3.20e-05 ***

    Residual standard error: 0.3725 on 16 degrees of freedom
    Multiple R-squared:  0.6711, Adjusted R-squared:  0.6506 
    F-statistic: 32.65 on 1 and 16 DF,  p-value: 3.197e-05

    ##################################################

##### Summary of the decay model for the Equal-Density treatment (E)

    summary(E.k)

    ##################################################
    Call:
    lm(formula = log(percAFDM.rem + 1) ~ days.elapsed, data = leaf.AFDM, subset = treatment == "E")

    Residuals:
    Min        1Q         Median     3Q       Max 
    -2.52941   -0.16269   -0.02159   0.29232   1.61763 

    Coefficients:
                  Estimate   Std. Error   t value   Pr(>|t|)    
    (Intercept)   4.94790    0.38963      12.699    1.06e-08 ***
    days.elapsed -0.10077    0.02579      -3.908    0.0018 ** 

    Residual standard error: 0.8719 on 13 degrees of freedom
    Multiple R-squared:  0.5402, Adjusted R-squared:  0.5048 
    F-statistic: 15.27 on 1 and 13 DF,  p-value: 0.001799

    ##################################################

##### Summary of the decay model for the High-Density treatment (H)
 
    summary(H.k)

    ##################################################
    Call:
    lm(formula = log(percAFDM.rem + 1) ~ days.elapsed, data = leaf.AFDM, subset = treatment == "H")

    Residuals:
     Min       1Q        Median    3Q        Max 
    -1.03997   -0.13135  0.00325   0.25734   0.80796 

    Coefficients:
                  Estimate   Std. Error  t value     Pr(>|t|)    
    (Intercept)   4.73341    0.23078     20.511      2.76e-11 ***
    days.elapsed  -0.07776   0.01527     -5.092      0.000207 ***

    Residual standard error: 0.5164 on 13 degrees of freedom
    Multiple R-squared:  0.666, Adjusted R-squared:  0.6403 
    F-statistic: 25.93 on 1 and 13 DF,  p-value: 0.0002067

    ################################################## 

## Plots
  
### Plot of the effect of treatment level on k
  
    #k_by_treat <- 
      ggplot(k.list, mapping = aes(y = k, x = factor(treat))) +
      geom_jitter(
        width = 0.1,
        color = 8
      ) +
      stat_summary(
        fun = mean,
        fun.min = function(x) mean(x) - sd(x),
        fun.max = function(x) mean(x) + sd(x)
      ) +
      labs(
        y = expression(days^{-1}),
        x = " "
      ) +
      scale_x_discrete(
        labels = c("Native Only", "Low Density", "Equal Density", "High Density", "Invasive Only"),
      ) +
      theme_classic()

#### Export plot as pdf

    ggexport(k_by_treat, width = 7, height = 7, filename = "k_by_treat.pdf")
    
