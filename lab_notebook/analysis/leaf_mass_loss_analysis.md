# Analysis of the leaf mass loss from the decomposition 

## Metadata

* File created on 10 June 2016 - KF

* 22 june 2016 - KF - added T2 to the analysis

* 5 July 2016 - KF - added T3 to the analysis

* 12 July 2016 - KF - remade individual time point figures for poster and altered code to fix broken variable names from leaf.AFDM data frame

* 13 July 2016 - KF - added analysis of summary stats for time points

* 1 Dec 2016 - KF - calculated k for each tank and then ran ANOVA

* 15 May 2017 - KF - 

## Description

This is the code to calculate the mass loss of the leaf packs in the experiment evaluating the impact of in invasive and native crayfish.

## R Code

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
    days.elapsed <- as.Date(leaf.AFDM$Date) - as.Date(leaf.initial$Date)
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
     index = seq_along(tank)
     
     for(i in index) { 
      treat[i] <- as.character(leaf.AFDM$treatment[index == i])[1]
      k[i] <- coef(summary(lm(log(leaf.AFDM$percAFDM.rem + 1)[index == i] ~ leaf.AFDM$days.elapsed[index == i])))[2, 1] # this extracts the slope (col 2, row 1) from the matrix of coefficents produced by the lm
     }

     data.frame(tank_name, treat, k)
    }

    # run the function
    k.list <- k.tank()

#### Print Data 

     k.list

~~~~
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
~~~~~

#### Summarize K

    summary(k.list$k)
    sd(k.list$k)

~~~~
 
 Min.  1st Qu.   Median     Mean  3rd Qu.     Max.      SD
-0.23210 -0.09284 -0.05057 -0.06876 -0.03065 -0.01455  0.05552608

~~~~

    tapply(k.list$k, k.list$treat, summary)
    tapply(k.list$k, k.list$treat, sd)

~~~~
 
$E
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.22730 -0.10040 -0.09646 -0.10080 -0.06440 -0.01529 

$H
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.12600 -0.08682 -0.08670 -0.07776 -0.04634 -0.04291 

$I
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.23210 -0.09289 -0.03922 -0.07432 -0.01733 -0.01455 

$L
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.10040 -0.08243 -0.04725 -0.05746 -0.03213 -0.02865 

$N
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-0.07507 -0.04059 -0.03563 -0.04032 -0.03162 -0.02305 

** SD

     E             H            I            L            N 
0.07853278    0.03426187   0.08480834   0.03141895   0.01815069 
~~~~

#### Analyze k by treatment 

    anova(lm(k ~ treat, data = k.list))
    
~~~~
ANOVA table for the effect of treatment on k

Analysis of Variance Table

Response: k
          Df   Sum Sq   Mean Sq F value Pr(>F)
treat      4 0.011335 0.0028336  0.9063 0.4767
Residuals 23 0.071910 0.0031265     
~~~~


    plot(k ~ treat, data = k.list, ylim = c(-0.3, 0), col = "wheat", ylab = "k (1/day)", xlab = "Treatment")
    dev.copy(jpeg, "./output/plots/k_by_treat.jpg")
    dev.off()
     
![k by treatment](../output/plots/k_by_treat.jpg)

FIGURE: k by Treatment

#### Test of the invasive vs native control

    t.test(k.list$k[k.list$treat == "N"], k.list$k[k.list$treat == "I"])

~~~~
 Welch Two Sample t-test

Welch Two Sample t-test

data:  k.list$k[k.list$treat == "N"] and k.list$k[k.list$treat == "I"]
t = 0.9603, df = 5.457, p-value = 0.3775
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.0547702  0.1227708
sample estimates:
  mean of x   mean of y 
-0.04031597 -0.07431628 
~~~~
 
#### Repeated Measures ANCOVA 

The `lmerTest` package is required

    (k.mod <- lmer(percAFDM.rem ~ 1 + days.elapsed * treatment + (1|BagTank), data = leaf.AFDM))

~~~~~
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
~~~~
 
##### Model Test
 
    anova(k.mod)     

~~~~
Analysis of Variance Table of type III  with  Satterthwaite 
approximation for degrees of freedom
                       Sum Sq Mean Sq NumDF  DenDF F.value Pr(>F)    
days.elapsed            48338   48338     1 51.001 267.582 <2e-16 ***
treatment                 396      99     4 63.767   0.549 0.7007    
days.elapsed:treatment    754     188     4 51.001   1.043 0.3944    
~~~~
 
### Summarize AFDM by Day

    tapply(leaf.AFDM$AFDM, leaf.AFDM$days.elapsed, summary)
    tapply(leaf.AFDM$AFDM, leaf.AFDM$days.elapsed, sd)

~~~~
AFDM (g)

$`3`
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.7647  0.8404  0.8737  0.8881  0.9367  1.0660 

$`10`
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.2342  0.6420  0.7440  0.7299  0.8073  0.9842 

$`24`
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 0.0000  0.1159  0.3178  0.3120  0.4315  0.7865 

## SD

       3         10         24 
0.07236243 0.16168339 0.21685885 
~~~~
  
#### Summarize percent AFDM remaining by Day

    tapply(leaf.AFDM$percAFDM.rem, leaf.AFDM$days.elapsed, summary)
    tapply(leaf.AFDM$percAFDM.rem, leaf.AFDM$days.elapsed, sd)

~~~~
Percent AFDM Remaining 
$`3`
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  75.26   82.71   85.98   87.40   92.18  104.90 

$`10`
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  23.05   63.18   73.22   71.83   79.45   96.86 

$`24`
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00   11.40   31.27   30.70   42.47   77.40 

## SD
> tapply(leaf.AFDM$percAFDM.rem, leaf.AFDM$days.elapsed, sd)
        3        10        24 
 7.121458 15.911869 21.341893 
~~~~
  


#### By Treatment over Time
 
    par(las = 1, mar = c(4, 5, 2, 2))
    plot(percAFDM.rem ~ jitter(as.numeric(days.elapsed), 1), data = leaf.AFDM, subset = treatment == "N", xlim = c(0, 30), ylim = c(0, 100), pch = 1, xlab = "Days in Tank", ylab = "Percent Initial Leaf Mass Remaining")
    points(percAFDM.rem ~ jitter(as.numeric(days.elapsed), 1), data = leaf.AFDM, subset = treatment == "I", pch = 19, col = "gray40")
    points(percAFDM.rem ~ jitter(as.numeric(days.elapsed), 1), data = leaf.AFDM, subset = treatment == "E", pch = 19, col = "deepskyblue")
    points(percAFDM.rem ~ jitter(as.numeric(days.elapsed), 1), data = leaf.AFDM, subset = treatment == "H", pch = 19, col = "blue3")
    points(percAFDM.rem ~ jitter(as.numeric(days.elapsed), 1), data = leaf.AFDM, subset = treatment == "L", pch = 19, col = "cadetblue2")
    legend(0, 40, c("Native Only  ", "Invasive Only  ", "Low Invasive  ", "Equal  ", "High Invasive  "), pch = c(1, 19, 19, 19, 19, 19), col = c(1, "gray40", "cadetblue2", "deepskyblue", "blue3"), cex = 0.9)
    dev.copy(jpeg, "./output/plots/percMassRem_by_treat_days.jpg")
    dev.off()

![percent mass remaining by treatment and time](../output/plots/percMassRem_by_treat_days.jpg)


    
### Plot by Tank

    plot(percAFDM.rem ~ BagTank, data = leaf.AFDM, subset = days.elapsed == "3", pch = 19, col = 1, ylim = c(0, 110) )
    points(percAFDM.rem ~ BagTank, data = leaf.AFDM, subset = days.elapsed == "10", pch = 19, col = 2, ylim = c(0, 110) )
    points(percAFDM.rem ~ BagTank, data = leaf.AFDM, subset = days.elapsed == "24", pch = 19, col = 3, ylim = c(0, 110) )


########################## DO NOT USE #################################
#######################################################################
#######################################################################
    #k.tank <- function() {
     # create empty objects
     #k <- numeric(0)
     #tank <- numeric(0)
     #treat <- character(0)
     #block <- character(0)
     
     #for(i in leaf.AFDM$BagTank)
       #k <- c(coef(summary(lm(log(leaf.AFDM$percAFDM.rem + 1)[leaf.AFDM$BagTank == i] ~ leaf.AFDM$days.elapsed[leaf.AFDM$BagTank == i])))[2, 1], k) # this extracts the slope (col 2, row 1) from the matrix of coefficents produced by the lm
     
     #for(i in leaf.AFDM$BagTank)
       #tank <- c(i, tank)

     #for(i in leaf.AFDM$BagTank)
       #treat <- c(as.character(leaf.AFDM$treatment[leaf.AFDM$BagTank == i]), treat)
     
     #for(i in leaf.AFDM$BagTank)
       #block <- c(as.character(leaf.AFDM$block[leaf.AFDM$BagTank == i]), block)
     
     #k.list <- data.frame(tank, treat, block, k) # the "unique" is needed because the tank numbers are repreated for each day in the dataset so the for-loop runs 3X. The unique eliminates the duplicate data
     
     #return(k) #k.list)
    }

    #k.list <- k.tank()
    #names(k.list) <- c("tank", "treat", "block", "k")
#######################################################################
#######################################################################