check_gquality <- function(taxonName = "Mammalia", country_code = "AU", dataLimit = 5000){ 
library(rgbif)
library(rgeospatialquality)
library(dplyr)
key <- name_backbone(name = taxonName)
key <- key$usageKey
dat = occ_data(taxonKey = key, country= country_code, limit = dataLimit)
data = dat$data
data_select = data[,c("decimalLatitude","decimalLongitude","countryCode")]

temp = add_flags(data_select[1:50,])$flags
maxLimit = dataLimit %/% 50
for(i in 2:maxLimit)
{
  temp2 = add_flags(data_select[((i-1)*50+1):(50*i),])$flags
  temp = bind_rows(temp, temp2)
}

for(j in 1:dataLimit)
{
  temp[j,"hasScientificName"] = !is.na(data[j,1])
}
returnval <- list("data"=data , "flags"=temp)
return(returnval)
}

data_list <- check_gquality()
View(data_list$flags)
View(data_list$data)

