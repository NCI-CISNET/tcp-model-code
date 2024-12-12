## LOOP THROUGH AND GENERATE STATE LEVEL FILES
setwd("/Users/ac3456/Dropbox/state_tcp_tool/LC_code/Darth_TCP_model/source_data/")
allstates <- c("AL", "AK","AZ", "AR", "CA", "CO","CT", "DE", "DC","FL", "GA","HI","ID","IL","IN","IA","KS","KY","LA","ME",
               "MD","MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
               "SC", "SD", "TN", "TX", "UT", "VT","VA", "WA","WV","WI", "WY" )


for (i in c(1:length(allstates))){
  dir.create(allstates[i])
  dir.create(paste0(allstates[i],"/t21"))
  dir.create(paste0(allstates[i],"/t21/deaths"))
  dir.create(paste0(allstates[i],"/t21/lyg"))
  dir.create(paste0(allstates[i],"/t21/results"))
}


# for (i in c(1:length(allstates))){
#   dir.create(allstates[i])
#   dir.create(paste0(allstates[i],"/mla"))
#   dir.create(paste0(allstates[i],"/mla/deaths"))
#   dir.create(paste0(allstates[i],"/mla/lyg"))
#   dir.create(paste0(allstates[i],"/mla/results"))
# }
