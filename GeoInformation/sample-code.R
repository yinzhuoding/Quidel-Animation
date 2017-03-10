rm(list = ls())
data = read.csv("data.csv")
myColourAxis <- paste("{values: [0, 0.5, 1.0 ], ",
                      "colors: [\'blue\', \'purple\', \'red\']}")
state = "FL"
gvisData <- by(data, list(date=data$date), function(x){
  date <- x$date[1]	
  g <- gvisGeoChart(x,  locationvar = "region", colorvar = "value",
                    options=list(region=paste("US",state,sep = "-"), displayMode="markers", resolution="metros"),
                    chartid=paste("[", date, "]", sep=""))
  .data <- g$html$chart["jsData"]
  .data <-gsub("function ", "", .data)
  .data <- sub("\\] ()", "\\] = function ", .data)
  return(.data)	
}
)
currentYear = 2016
startDate = 1
endDate = 33

myRegion <- as.character(paste("US",state, sep = "-"))
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
                     var options = {};
                     options["displayMode"] = "markers";
                     options["resolution"] = "metros";
                     options["width"] =   556;
                     options["height"] =  347;
                     options["datalessRegionColor"]="white";
                     options["colorAxis"] = %s;
                     options["region"] = %s;
                     
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
                     
                     ', myColourAxis,myRegion, startDate)


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
plot(Animated.Geo.Chart)