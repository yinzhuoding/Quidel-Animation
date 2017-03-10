rm(list = ls())
require(googleVis)
require(shiny)
library(tidyr)
## Prepare data to be displayed
## Load presidential election data by state from 1932 - 2012
RepDir = "~/Dropbox/LEPR02/quidel/"
qdata = readRDS(paste0(RepDir,"Quidel_weekly.Rds"))
GeoAnimation.Nation = function(qdata,showyear) {
  week = qdata$week
  year = qdata$year
  total.raw = qdata$Total
  total.nation = qdata$Total$USA
  split.index = which(year == 2016 & week == 26)
  if(showyear == 2015) {
    year.index = seq(1,split.index,1)
  }
  if(showyear == 2016) {
    year.index = seq(split.index+1,length(week),1)
  }
  total.nation = total.nation[year.index]
  nweek = length(year.index)
  # normalization
  total.nation.std = (total.nation-min(total.nation))/(max(total.nation)-min(total.nation))
  total.nation.std = cbind.data.frame(rep("USA",nweek), total.nation.std)
  long.total.nation = cbind(total.nation.std,seq(1,nweek,1))
  names(long.total.nation) = c("location","value","date")
  myColourAxis <- paste("{values: [0, 0.5, 1.0 ], ",
                        "colors: [\'blue\', \'purple\', \'red\']}")
  
  gvisData <- by(long.total.nation, list(date=long.total.nation$date), function(x){
    date <- x$date[1]	
    g <- gvisGeoChart(x,  locationvar = "location", colorvar = "value",
                      options=list(region="US", displayMode="regions"),
                      chartid=paste("[", date, "]", sep=""))
    .data <- g$html$chart["jsData"]
    .data <-gsub("function ", "", .data)
    .data <- sub("\\] ()", "\\] = function ", .data)
    return(.data)	
  }
  )
  
  startDate = 1
  endDate = nweek
  currentYear = showyear
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
                       + Animation.currentDate;
                       if (Animation.currentDate > Animation.startDate) {
                       Animation.divCharts[Animation.currentDate-1].style.display = 'none';
                       }
                       Animation.divCharts[Animation.currentDate++].style.visibility = 'visible';
                       setTimeout(Animation.playAnimation, 500);
                       };
                       
                       ", startDate, endDate, currentYear)
  
  
  gvisChart <- sprintf('
                       
                       // jsDrawChart
                       function drawChart() {
                       var chart = {};
                       var options ={};
                       options["region"] = "US";
                       options["displayMode"] = "regions";
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
plot(GeoAnimation.Nation(qdata,showyear = 2016))
