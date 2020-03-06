load("./ecophysio.Rda")

## This dataset contains several biomass and ecophysiological variables measured for 4 species (sp) at
# two altitudes (Piso). We could, for example, calculate RDPI of plant biomass across altitudes
# and compare values across species


rdpi(ecophysio, sp = sp, trait = PB, factor = Piso, verbose = F) 

# this produces a boxplot and the ouput of some statistics to compare rdpi values across species
# but if we want to store the calculated raw RDPI values, we just need to save as an object, ebnsuring that we set verbose = T
      
rdpi_PB <- rdpi(ecophysio,sp = sp, trait = PB, factor = Piso, verbose = T)
