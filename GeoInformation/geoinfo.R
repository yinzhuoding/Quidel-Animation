### saving all the geoinforamtion for quidel data
rm(list = ls())
RepDir = "~/Dropbox/LEPR02/quidel/"
qdata = readRDS(paste0(RepDir,"Quidel_weekly.Rds"))
library(ggmap)
# nation
geoinfo.nation = suppressMessages(geocode("USA"))
write.csv(geoinfo.nation, file = "geoinfo_nation.csv", row.names = FALSE)
# region
region_list = list( Region1 = c('Maine','New Hampshire','Massachusetts','Rhode Island','Connecticut','Vermont') , 
                    Region2 = c('New York','New Jersey','Puerto Rico'),
                    Region3 = c('Pennsylvania','Delaware','Maryland','West Virginia','Virginia','District of Columbia'),
                    Region4 = c('Kentucky','Tennessee','North Carolina','South Carolina','Georgia','Florida','Alabama','Mississippi'),
                    Region5 = c('Minnesota','Wisconsin','Illinois','Indiana','Michigan','Ohio'),
                    Region6 = c('New Mexico','Texas','Oklahoma','Arkansas','Louisiana'),
                    Region7 = c('Nebraska','Kansas','Iowa','Missouri'),
                    Region8 = c('Utah','Colorado','Wyoming','Montana','South Dakota','North Dakota'),
                    Region9 = c('California','Nevada','Arizona','Hawaii'),
                    Region10 = c('Oregon','Washington','Idaho','Alaska'))
geoinfo.region = NULL
#for (region in region_list) {
  #geoinfo = suppressMessages(geocode(unlist(region)))
  #geoinfo.region = rbind(geoinfo.region, c(mean(geoinfo$lon),mean(geoinfo$lat)))
#}
represent = c('New Hampshire', 'New York','District of Columbia','Georgia','Michigan','Texas','Kansas','Wyoming','California','Oregon')
for (i in 1:10) {
  geoinfo = suppressMessages(geocode(represent[i]))
  geoinfo.region = rbind.data.frame(geoinfo.region,geoinfo)
}
geoinfo.region = cbind(paste("Region",seq(1,10,1),sep = ''), geoinfo.region)
names(geoinfo.region) = c("Region","lon","lat")
write.csv(geoinfo.region, file = "geoinfo_region.csv", row.names = FALSE)

# state
states = qdata$states
states = as.character(abb2state(states))
geoinfo.state = suppressMessages(geocode(states))
geoinfo.state = geoinfo.state[,-1]
geoinfo.state = cbind.data.frame(states,geoinfo.state)
geoinfo.state = cbind(states,geoinfo.state)
write.csv(geoinfo.state, file = "geoinfo_state.csv",row.names = FALSE)
# county
counties = qdata$counties
geoinfo.county = suppressMessages(geocode(paste(counties[,2],counties[,1],sep = ",")))
geoinfo.county = cbind(counties,geoinfo.county)
write.csv(geoinfo.county, file = "geoinfo_county.csv",row.names = FALSE)

# city
cities = qdata$cities
geoinfo.city = suppressMessages(geocode(paste(cities[,2],cities[,1],sep = ",")))
geoinfo.city = cbind(cities,geoinfo.city)
write.csv(geoinfo.city, file = "geoinfo_city.csv",row.names = FALSE)
geoinfo.zip = read.csv("geoinfo_zip.csv")


# zip
zips = qdata$zips
geoinfo.zip = suppressMessages(geocode(paste("USA",zips,sep = ",")))
geoinfo.zip = cbind(zips, geoinfo.zip)
write.csv(geoinfo.zip, file = "geoinfo_zip.csv",row.names = FALSE)
abb2state <- function(name, convert = F, strict = F){
  data(state)
  # state data doesn't include DC
  state = list()
  state[['name']] = c(state.name,"District Of Columbia")
  state[['abb']] = c(state.abb,"DC")
  
  if(convert) state[c(1,2)] = state[c(2,1)]
  
  single.a2s <- function(s){
    if(strict){
      is.in = tolower(state[['abb']]) %in% tolower(s)
      ifelse(any(is.in), state[['name']][is.in], NA)
    }else{
      # To check if input is in state full name or abb
      is.in = rapply(state, function(x) tolower(x) %in% tolower(s), how="list")
      state[['name']][is.in[[ifelse(any(is.in[['name']]), 'name', 'abb')]]]
    }
  }
  sapply(name, single.a2s)
}