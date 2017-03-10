## Quidel-Visualization
This repository is using 'googleVis' package in R to create map animations based on Quidel data. 
Nation/region/state works well right now, keep on adding county/city/zipcode level animations.
### GeoInformation
This directory saves the geo information -- latitude and longitude for all the levels. These information should be useful when creating animation using 'markers'.  
For the Region level, I choose a 'representative state' and save its geo information to represent the whole region; For other levels, the geo information is exactly where the state/county, etc locates.   
All the information was obtained using the 'geocode' function from 'ggmap'.
### Main function
I've created R files seperately for different levels. The functions are named as 'GeoAnimation.levelname'. At present, there are two parameters in the functions, the first one represents the data, the second one represents the flu year we want to see.  
To run the functions, you can call，for example:  
plot(GeoAnimation.state(qdata,showyear = 2016))  
then there would be an annimation pop up in your browser.  
For nation/state level, we have one version of this animation that we fill the color in corresponding location;  
For region level, we provided two versions: there is another choice that we use the markers to represent each region.
