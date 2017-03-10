rm(list = ls())
RepDir = "~/Dropbox/LEPR02/quidel/"
qdata = readRDS(paste0(RepDir,"Quidel_weekly.Rds"))
library(tidyr)

MotionChart.state = function(qdata,showyear,level,state.abbr = NULL) {
  week = qdata$week
  year = qdata$year
  n = length(week)
  total.raw = qdata$Total
  split.index = which(year == 2016 & week == 26)
  if(showyear == 2015) {
    year.index = seq(1,split.index,1)
  }
  if(showyear == 2016) {
    year.index = seq(split.index+1,length(week),1)
  }
  
  #### US Nation level ####
  if(level == 'nation') {
    total.nation = qdata$Total$USA[year.index]
    geoinfo.nation = read.csv("geoinfo_nation.csv")
    lon.nation = geoinfo.nation$lon
    lat.nation = geoinfo.nation$lat
    total.nation.std = (total.nation - min(total.nation))/(max(total.nation)-min(total.nation))
    map.total.nation = cbind.data.frame('US-Nation',lon.nation, lat.nation, t(total.nation.std))
    colnames(map.total.nation) = c("nation", "lon", "lat",paste(year[year.index],week[year.index],sep = "-"))
    long.total.nation = gather(map.total.nation,date,value,4:ncol(map.total.nation))
    long.total.nation$date = as.Date(paste(long.total.nation$date,rep(0,nrow(long.total.nation)),sep = '-'), format = "%Y-%W-%w")                                                                                                                                           
    p = gvisMotionChart(long.total.nation,idvar = "nation",timevar = "date")
    plot(p)
  }
  
  #### Region level ####
  if(level == "region") {
    regions = qdata$regions
    total.region = total.raw[year.index,paste("USA",regions,sep = ".")]
    geoinfo.region = read.csv("geoinfo_region.csv")
    lon.region = geoinfo.region$lon
    lat.region = geoinfo.region$lat
    total.region.std = as.data.frame(apply(total.region,2,function(x) {x = (x-min(x))/(max(x)-min(x))}))
    map.total.region = cbind.data.frame(regions,lon.region, lat.region, t(total.region.std))
    colnames(map.total.region) = c("region", "lon", "lat",paste(year[year.index],week[year.index],sep = "-"))
    long.total.region = gather(map.total.region,date,value,4:ncol(map.total.region))
    long.total.region$date = as.Date(paste(long.total.region$date,rep(0,nrow(long.total.region)),sep = '-'), format = "%Y-%W-%w")                                                                                                                                           
    p = gvisMotionChart(long.total.region,idvar = "region",timevar = "date")
  }
  
  #### State level ####
  if(level == "state") {
    states = qdata$states
    var = colnames(total.raw)
    total.state = total.raw[year.index, (substr(var,nchar(var)-2,nchar(var)) %in% paste(".",states,sep = ""))] 
    geoinfo.state = read.csv("geoinfo_state.csv")
    lon.state = geoinfo.state$lon
    lat.state = geoinfo.state$lat
    total.state.std = as.data.frame(apply(total.state,2,function(x) {x = (x-min(x))/(max(x)-min(x))}))
    map.total.state = cbind.data.frame(states,lon.state, lat.state, t(total.state.std))
    colnames(map.total.state) = c("state", "lon", "lat",paste(year[year.index],week[year.index],sep = "-"))
    long.total.state = gather(map.total.state,date,value,4:ncol(map.total.state))
    long.total.state$date = as.Date(paste(long.total.state$date,rep(0,nrow(long.total.state)),sep = '-'), format = "%Y-%W-%w")                                                                                                                                           
    p = gvisMotionChart(long.total.state,idvar = "state",timevar = "date")
  }
  
  #### County level ####
  if(level == "county"){
    var = colnames(total.raw)
    othercounty = c("Baltimore City","Anchorage Borough","Nome Census Area","Parish","NULL")
    county.index = which(grepl('County',var))
    for(county in othercounty) {
      county.index = union(county.index, which(grepl(county,var)))
    }
    total.county = total.raw[year.index,sort(county.index)]
    county.list = strsplit(colnames(total.county),"[.]")
    counties.state = sapply(county.list, function(x) {x[3]})
    counties = paste(as.character(qdata$counties[,2]), counties.state, sep = ",")
    state = state.abbr
    state.index = which(counties.state == state)
    total.county.state = total.county[,state.index]
    total.county.std.state = as.data.frame(apply(total.county.state,2,function(x) {x = (x-min(x))/(max(x)-min(x))}))
    geoinfo.county = read.csv("geoinfo_county.csv")
    lon.county = geoinfo.county$lon[state.index]
    lat.county = geoinfo.county$lat[state.index]
    map.total.county = cbind.data.frame(counties[state.index],lon.county, lat.county, t(total.county.std.state))
    colnames(map.total.county) = c("county", "lon", "lat",paste(year[year.index],week[year.index],sep = "-"))
    long.total.county = gather(map.total.county,date,value,4:ncol(map.total.county))
    long.total.county$date = as.Date(paste(long.total.county$date,rep(0,nrow(long.total.county)),sep = '-'), format = "%Y-%W-%w")                                                                                                                                           
    p = gvisMotionChart(long.total.county,idvar = "county",timevar = "date")
  }
  return(p)
}
plot(MotionChart.state(qdata,showyear = 2016,level = 'county',state.abbr = 'CA'))


