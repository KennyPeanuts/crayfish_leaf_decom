# Treatment assignments to tanks

## Metadata

* File created on 10 June 2016

## Description 

This code is to create a file to map the treatments onto the tank numbers for the analyses

## Variables

* tank = the number of the tank

* block = the treatment block in the experimental design

~~~~
                        F
    A [ 1 2 3 4 5]     30
    B [6 7 8 9 10]     29
    C [11 12 13 14 15] 28
    D [16 17 18 19 20] 27
    E [21 22 23 24 25] 26
    
~~~~

* treatment = the crayfish community in the tank
  * N = 4 native crayfish (C. spp C)
  * I = 4 invasive crayfish (O. virilis)
  * L = 4 native and 2 invasive crayfish
  * E = 4 native and 4 invasive crayfish
  * H = 4 native and 6 invasive crayfish
  

## R Code

    tank <- 1:30
    block <- c(rep("A", 5), rep("B", 5), rep("C", 5), rep("D", 5), rep("E", 5), rep("F", 5))
    treatment <- c("N", "L", "E", "H", "I", "N", "E", "I", "L", "H", "I", "H", "N", "L", "E", "E", "L", "N", "H", "I", "I", "E", "H", "L", "N", "H", "L", "N", "E", "I")

    exp <- data.frame(tank, block, treatment)
  
    write.table(exp, "./data/tank_map.csv", row.names = F, quote = F, sep = ",")
