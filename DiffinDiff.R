## Source code related to the paper entitled: 
#  A Health Economics Inquiry into Regulatory Constraints on the European Tobacco Market
#  by Salvatore Barbaro, Nathalie Neu-Yanders, and Nina König
##################################################################################################
setwd("path")
# First step: load required libraries
library(rgho)  # who api server 
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(scales)
library(viridis)
library(RColorBrewer)
library(dplyr)
library(reshape2)
library(lubridate)
library(hutils)
library(eurostat)
library(ggmap)
library(maps)
library(mapproj)
library(countrycode)
library(rjson)
library(jsonlite)
library(haven)
library(dplyr)
library(hutils)
library(survey)
library(srvyr)
library(ggplot2)
library(ggpp)
library(countrycode)
library(ggpubr)
library(lubridate)
library(eurostat)
library(rjson)
library(jsonlite)
library(rgho)
library(writexl)
library(readxl)
library(texreg)
library(plm)
library(lmtest)


### Prepare data

volumes <- read_excel("Input_data_DutyTables.xlsx")
volumes$Consumption <- ifelse(volumes$Product == "FC",volumes$Consumption*1000/0.67,volumes$Consumption)
volumes <- filter(volumes,Year >=2010)
volumes <- filter(volumes,Consumption != "NA")
names(volumes) <- make.names(names(volumes), unique=TRUE)
volumes$Consumption <- as.numeric(volumes$Consumption)
volumes_summed <- aggregate(volumes$Consumption,list(volumes$Country.Code,volumes$Year),sum)
colnames(volumes_summed) <- c("Country.Code","Year","Consumption")

#convert to billion sticks
volumes_summed$Consumption = volumes_summed$Consumption / 1000000000

Poland <- filter(volumes_summed,Country.Code == "PL")
Spain <- filter(volumes_summed,Country.Code == "ES")


volumes_summed <- filter(volumes_summed,Country.Code == "PL" | Country.Code == "ES")


volumes_summed$treat_time <- ifelse(volumes_summed$Year >2019,1,0)
volumes_summed$treat_group <- ifelse(volumes_summed$Country.Code == "PL",1,0)
volumes_summed$treat_unit <- volumes_summed$treat_time * volumes_summed$treat_group

### Estimation
#mit  Driscoll-Kraay Standard Errors für clustering und autocorrelation


volumes_summed <- pdata.frame(volumes_summed, index = c("treat_unit", "Year"))
didreg_dk <- plm(Consumption ~ treat_time + treat_group + treat_unit, data = volumes_summed, model = "pooling")
dk_se <- vcovSCC(didreg_dk, type = "HC0", maxlag = 4)
dk_test <- coeftest(didreg_dk, vcov = dk_se)

coefs <- dk_test[, 1]      # coefficients
hac_se <- dk_test[, 2]     # Driscoll-Kraay standard errors
p_values <- dk_test[, 4]   # p-value

tr <- createTexreg(
  coef.names = names(coefs),  
  coef = coefs,               
  se = hac_se,                
  pvalues = p_values,         
  model.name = "Consumption"
)

texreg(list(tr), 
       digits = 4,  
       stars = c(0.01, 0.05, 0.1),
       custom.model.names = c("Consumption [in billion sticks]"),
       caption = "Difference-in-differences regression results",
       label = "DiffinDiff",
       caption.above = TRUE,
       siunitx = TRUE,
       booktabs = TRUE,
       use.packages = FALSE)



