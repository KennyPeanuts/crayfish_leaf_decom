# Analysis of the leaf mass loss from the decomposition 

## Metadata

* File created on 10 June 2016 - KF

## Description

This is the code to calculate the mass loss of the leaf packs in the experiment evaluating the impact of in invasive and native crayfish.

## R Code

### Import data

    leaf.initial <- read.table("./data/initial_leaf_mass.csv", header = T, sep = ",")
    leaf.T1 <- read.table("./data/T1_leaf_mass.csv", header = T, sep = ",")
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

### Analyze Mass Lost
    
    leaf.mass <- c(initialAFDM, T1AFDM)
    date <- c(as.Date(leaf.initial$Date), as.Date(leaf.T1$Date)) 
    time.step <- c(rep("T0", 11), rep("T1", 30))
    days.elapsed <- date - date[1]
    plot(leaf.mass ~ days.elapsed) 
    
    mass.lost.T1 <- mean.initial.AFDM - T1AFDM
    par(las = 1)
    plot(mass.lost.T1 ~ treat$treatment, ylab = "T0 - T1 Mass Loss (g)", xlab = "Treatment", col = 8)
    abline(h = 0)
    dev.copy(jpeg, "./output/plots/T0_T1_mass_loss.jpg")
    dev.off()

![Leaf Mass Loss From T0 to T1 by Treatment](../output/plots/T0_T1_mass_loss.jpg)
