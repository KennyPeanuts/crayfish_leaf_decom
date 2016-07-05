# Analysis of the leaf mass loss from the decomposition 

## Metadata

* File created on 10 June 2016 - KF

* 22 june 2016 - KF - added T2 to the analysis

* 5 July 2016 - KF - added T3 to the analysis

## Description

This is the code to calculate the mass loss of the leaf packs in the experiment evaluating the impact of in invasive and native crayfish.

## R Code

### Import data

    leaf.initial <- read.table("./data/initial_leaf_mass.csv", header = T, sep = ",")
    leaf.T1 <- read.table("./data/T1_leaf_mass.csv", header = T, sep = ",")
    leaf.T2 <- read.table("./data/T2_leaf_mass.csv", header = T, sep = ",")
    leaf.T3 <- read.table("./data/T3_leaf_mass.csv", header = T, sep = ",")
    treat <- read.table("./data/tank_map.csv", header = T, sep = ",")

### Calculate variables
#### Initial AFDM

    initialDM <- leaf.initial$CrucLeafDM - leaf.initial$CrucMass
    initialAM <- leaf.initial$CrucAM - leaf.initial$CrucMas
    initialAFDM <- initialDM - initialAM
    mean.initial.AFDM <- mean(initialAFDM)
    
#### T1 AFDM

    T1DM <- leaf.T1$CrucLeafDM - leaf.T1$CrucMass
    T1AM <- leaf.T1$CrucAM - leaf.T1$CrucMass
    T1AFDM <- T1DM - T1AM

#### T2 AFDM

    T2DM <- leaf.T2$CrucLeafDM - leaf.T2$CrucMass
    T2AM <- leaf.T2$CrucAM - leaf.T2$CrucMass
    T2AFDM <- T2DM - T2AM


#### T3 AFDM

    T3DM <- leaf.T3$CrucLeafDM - leaf.T3$CrucMass
    T3AM <- leaf.T3$CrucAM - leaf.T3$CrucMass
    T3AFDM <- T3DM - T3AM

### Analyze Mass Lost

    leaf.mass <- c(initialAFDM, T1AFDM, T2AFDM, T3AFDM)
    date <- c(as.Date(leaf.initial$Date), as.Date(leaf.T1$Date), as.Date(leaf.T2$Date), as.Date(leaf.T3$Date)) 
    time.step <- c(rep("T0", 11), rep("T1", 30), rep("T2", 30), rep("T3", 30))
    days.elapsed <- date - date[1]
    plot((leaf.mass) ~ days.elapsed) 
    k <- lm(log(leaf.mass) ~ days.elapsed)
    abline(k)
    
~~~~~

~~~~

    mass.lost.T1 <- mean.initial.AFDM - T1AFDM
    par(las = 1)
    plot(mass.lost.T1 ~ treat$treatment, ylab = "T0 - T1 Mass Loss (g)", xlab = "Treatment", col = 8)
    abline(h = 0)
    dev.copy(jpeg, "./output/plots/T0_T1_mass_loss.jpg")
    dev.off()

![Leaf Mass Loss From T0 to T1 by Treatment](../output/plots/T0_T1_mass_loss.jpg)

    mass.lost.T2 <- mean.initial.AFDM - T2AFDM
    par(las = 1)
    plot(mass.lost.T2 ~ treat$treatment, ylim = c(0, 1), ylab = "T0 - T2 Mass Loss (g)", xlab = "Treatment", col = 8)
    dev.copy(jpeg, "./output/plots/T0_T2_mass_loss.jpg")
    dev.off()

![Leaf Mass Loss From T0 to T2 by Treatment](../output/plots/T0_T2_mass_loss.jpg)


    mass.lost.T3 <- mean.initial.AFDM - T3AFDM
    par(las = 1)
    plot(mass.lost.T3 ~ treat$treatment, ylim = c(0, 1), ylab = "T0 - T3 Mass Loss (g)", xlab = "Treatment", col = 8)
    dev.copy(jpeg, "./output/plots/T0_T3_mass_loss.jpg")
    dev.off()

![Leaf Mass Loss From T0 to T2 by Treatment](../output/plots/T0_T2_mass_loss.jpg)

