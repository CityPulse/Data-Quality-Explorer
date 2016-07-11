# QualityExplorer
The QualityExplorer is a web-based tool to get detailed insight about the quality of information the deploy sensors provide. Each sensor is depicted on a map as a dot. The color of the dot indicates the quality of information, ranging from green (good) to red (bad). Additionally bar charts show the number of visible sensors and the corresponding quality of information value.

![Screenshot](https://github.com/CityPulse/QualityExplorer/blob/master/ScreenshotQoiExplorer-CityPulse.png)

## Dependencies

The following applications/libraries have to be installed (Name/License/Link):
R (Version>=3.1)

<table>
	<tr><td>Shiny</td><td>GPL-3</td><td>https://cran.r-project.org/web/packages/shiny/</td></tr>
	<tr><td>Leaflet</td><td>GPL-3</td><td>https://cran.r-project.org/web/packages/leaflet/</td></tr>
	<tr><td>rgeos</td><td>GPL-3</td><td>https://cran.r-project.org/web/packages/rgeos/</td></tr>
	<tr><td>sp</td><td>GPL-3</td><td>https://cran.r-project.org/web/packages/sp/</td></tr>
	<tr><td>maptools</td><td>GPL-3</td><td>https://cran.r-project.org/web/packages/maptools/</td></tr>
	<tr><td>shinyBS</td><td>GPL-3</td><td>https://cran.r-project.org/web/packages/shinyBS/</td></tr>
	<tr><td>ggmap</td><td>GPL-2</td><td>https://cran.r-project.org/web/packages/ggmap/</td></tr>
	<tr><td>rgdal</td><td>GPL-3</td><td>https://cran.r-project.org/web/packages/rgdal/</td></tr>
	<tr><td>cleangeo</td><td>GPL-3</td><td>https://cran.r-project.org/web/packages/cleangeo/</td></tr>
	<tr><td>rjson</td><td>GPL-2</td><td>https://cran.r-project.org/web/packages/rjson/</td></tr>
	<tr><td>ggplot2</td><td>GPL-2</td><td>https://cran.r-project.org/web/packages/ggplot2/</td></tr>
	<tr><td>lattice</td><td>GPL-3</td><td>https://cran.r-project.org/web/packages/lattice/</td></tr>
	<tr><td>scales</td><td>MIT</td><td>https://cran.r-project.org/web/packages/scales/</td></tr>
	<tr><td>RColorBrewer</td><td>Apache 2.0</td><td>https://cran.r-project.org/web/packages/RColorBrewer/</td></tr>
	<tr><td>xtable</td><td>GPL-2</td><td>https://cran.r-project.org/web/packages/xtable/</td></tr>
	<tr><td>knitr</td><td>GPL-2</td><td>https://cran.r-project.org/web/packages/knitr/</td></tr>
	<tr><td>gdata</td><td>GPL-2</td><td>https://cran.r-project.org/web/packages/gdata/</td></tr>
	<tr><td>RCurl</td><td>BSD</td><td>https://cran.r-project.org/web/packages/RCurl/</td></tr>
	<tr><td>dplyr</td><td>MIT</td><td>https://cran.r-project.org/web/packages/dplyr/</td></tr>
	<tr><td>plyr</td><td>MIT</td><td>https://cran.r-project.org/web/packages/plyr/</td></tr>
	<tr><td>GDAL</td><td>X/MIT</td><td>http://www.gdal.org</td></tr>
	<tr><td>GEOS</td><td>LGPL</td><td>http://tsusiatsoftware.net/jts/main.html</td></tr>
</table>


## Installation
Apart from R and the required libraries above, an installation of the QualityExplorer is not required. Simply clone the Github repository.

	git clone https://github.com/CityPulse/QualityExplorer.git


## Starting the component
To start the component change into the 'R-Shiny-Application' folder and execute the following start command:

	R -e "shiny::runApp(host='127.0.0.1', port=10111)"

This starts the server listening on localhost (127.0.0.1) at port 10111. Change the 'host' and 'port' parameter in the start command according to your needs.

## Contributers
The GDI component was developed as part of the EU project CityPulse. The consortium member University of Applied Sciences provided the main contributions for this component.

## Demonstration Video
An introduction video for the QoI Explorer can be found here: https://www.youtube.com/watch?v=edFrlciO8Og

## Link
The code of the Resource Management can be found here: https://github.com/CityPulse/QualityExplorer.git
