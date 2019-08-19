list.of.packages <- c( 
  "tidyverse", 
  "readxl", 
  "e1071",
  "factoextra")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
rm(list.of.packages, new.packages)