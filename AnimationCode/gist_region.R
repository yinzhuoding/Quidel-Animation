rm(list = ls())
require(googleVis)
require(shiny)
library(tidyr)
## Prepare data to be displayed
## Load presidential election data by state from 1932 - 2012
RepDir = "~/Dropbox/LEPR02/quidel/"
qdata = readRDS(paste0(RepDir,"Quidel_weekly.Rds"))
GeoAnimation.Region = function(qdata,showyear) {
  week = qdata$week
  year = qdata$year
  total.raw = qdata$Total
  regions = qdata$regions
  split.index = which(year == 2016 & week == 26)
  if(showyear == 2015) {
    year.index = seq(1,split.index,1)
  }
  if(showyear == 2016) {
    year.index = seq(split.index+1,length(week),1)
  }
  total.region = total.raw[year.index,paste("USA",regions,sep = ".")]
  states = qdata$states
  var = colnames(total.raw)
  total.state = total.raw[year.index, (substr(var,nchar(var)-2,nchar(var)) %in% paste(".",states,sep = ""))]
  regionOfState = sapply(strsplit(colnames(total.state),'[.]'),function(x){x[[2]]})
  region.name = sapply(strsplit(colnames(total.region),'[.]'),function(x){x[[2]]})
  total.regionbystate = total.state
  for (id in 1:ncol(total.state)) {
    total.regionbystate[,id] = total.region[,which(region.name == regionOfState[id])]
  }
  # normalization
  normalization = function(x) {
    if(max(x) <= 200) {x = -1}
    else{x = (x-min(x))/(max(x)-min(x))}
  }
  total.region.std = as.data.frame(apply(total.regionbystate,2,normalization))
  region.name = paste(rep("Region",10),seq(1,10,1),sep = "")
  #geoinfo.region = read.csv("geoinfo_region.csv")
  #LatLong = paste(geoinfo.region$lat, geoinfo.region$lon,sep = ":")
  total.region.std = cbind.data.frame(states, regionOfState, t(total.region.std))
  colnames(total.region.std) = c("state","region",seq(1,length(year.index),1))
  long.total.region = gather(total.region.std,date,value,3:ncol(total.region.std))

  myColourAxis <- paste("{values: [-1, 0, 0.5, 1.0 ], ",
                        "colors: [\'white\',\'blue\', \'purple\', \'red\']}")
  
  gvisData <- by(long.total.region, list(date=long.total.region$date), function(x){
    date <- x$date[1]	
    g <- gvisGeoChart(x,  locationvar = "state", colorvar = "value", hovervar = "region",
                      options=list(region="US", displayMode="regions",resolution="provinces"),
                      chartid=paste("[", date, "]", sep=""))
    .data <- g$html$chart["jsData"]
    .data <-gsub("function ", "", .data)
    .data <- sub("\\] ()", "\\] = function ", .data)
    return(.data)	
  }
  )
  
  currentYear = showyear
  startDate = 1
  endDate = length(year.index)
  animation <- sprintf("
                       var gvisData = {};
                       var Animation = {};
                       Animation.startDate = %s;
                       Animation.endDate = %s;
                       Animation.currentYear = %s;
                       Animation.currentDate = Animation.startDate;
                       Animation.divCharts = {};
                       
                       Animation.playAnimation = function() {
                       if (Animation.currentDate > Animation.endDate) {
                       return;
                       }
                       document.getElementById('chart-header').innerHTML =
                       'Quidel data in year <b>' + Animation.currentYear + '</b> of week <b>'
                       + Animation.currentDate + '</b> (Regions with no data shown in white)';
                       if (Animation.currentDate > Animation.startDate) {
                       Animation.divCharts[Animation.currentDate-1].style.display = 'none';
                       }
                       Animation.divCharts[Animation.currentDate++].style.visibility = 'visible';
                       setTimeout(Animation.playAnimation, 500);
                       };
                       
                       ",startDate, endDate, currentYear)
  
  
  gvisChart <- sprintf('
                       
                       // jsDrawChart
                       function drawChart() {
                       var chart = {};
                       var options ={};
                       options["region"] = "US";
                       options["displayMode"] = "regions";
                       options["resolution"] = "provinces";
                       options["width"] =   600;
                       options["height"] =  600;
                       options["datalessRegionColor"]="white";
                       options["colorAxis"] = %s;
                       
                       for (var i = Animation.startDate; i<=Animation.endDate; i++) {
                       Animation.divCharts[i] = document.createElement("div");
                       Animation.divCharts[i].className = "pop-chart";
                       document.body.appendChild(Animation.divCharts[i]);
                       chart[i] = new google.visualization.GeoChart(Animation.divCharts[i]);
                       
                       var data = gvisData[(i+1-%s)]();
                       options["title"] = i;
                       chart[i].draw(data,options);
                       }
                       
                       // Animation.playAnimation();
                       setTimeout(Animation.playAnimation, 5000);
                       }
                       
                       
                       // jsDisplayChart 
                       function displayChart() {
                       google.load("visualization", "1", { packages:["geochart"] }); 
                       google.setOnLoadCallback(drawChart);
                       }
                       // jsChart 
                       displayChart()
                       
                       ', myColourAxis, startDate)
  
  
  htmlHead <- '
  
  <html>
  <meta http-equiv="content-type" content="text/html;charset=utf-8" />
  <head>
  <style type="text/css">
  body {
  color: #444444;
  font-family: Arial,Helvetica,sans-serif;
  font-size: 75%;
  }
  a {
  color: #4D87C7;
  text-decoration: none;
  }
  .pop-chart {
  position: absolute;
  top: 50;
  left: 10;
  display: block;
  visibility: hidden;
  }
  </style>
  
  <title>Quidel data Animation</title>
  
  <script type="text/javascript" src="http://www.google.com/jsapi"></script>
  <script type="text/javascript">
  
  '
  htmlFoot <-'
  
  </script>
  </head>
  <body>
  
  <div id="chart-header"></div>
  
  <FORM>
  <INPUT TYPE="button" onClick="history.go(0)" VALUE="Replay">
  </FORM>
  
  </body>
  </html>
  
  '
  Animated.Geo.Chart <- structure(
    list(type="AnimatedGeoChart",
         chartid="Quidel.Animation",
         html=list(
           header=htmlHead,
           chart=c(animation, gvisData, gvisChart),
           caption="",
           footer=htmlFoot)
    ),
    class = c("gvis", "list")
  )
  return(Animated.Geo.Chart)
}
plot(GeoAnimation.Region(qdata,showyear = 2016))
