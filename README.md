# Gefahrradtour

Project for the HERE Map Data Hackathon 2020

### Introduction

Find the safest route from A to B in Berlin thanks to HERE Data Layers and Berlin traffic accident data.


### The data
The Gefahrradtour App is based on two data sets:

1. Navigable roads from HERE Data Layers
The roads data set used for calculating the route between two points was provided by HERE for the hackathon in a geojson file. It contains 18916 road segments covering a section of Berlin which inlcudes most of its central districts (see Figure 1).

![Figure 1. Area in Berlin covered by HERE navigable roads data set.](https://github.com/Emelieh21/Gefahrradtour/tree/main/assets/berlin_boundingbox_hereroads.jps)



2. Accidents in Berlin 2019
This data set contains all accidents that occured in Berlin in 2019 [1] involving cars, bikers and pedestrians. For each accidents its coordinates, severity (from 1-3) and type are given. In total there where 13390 accidents recorded.


### How routes are computed
To find the route between two coordinates we used the navigable roads dataset in the HERE data layers.
Using the tidygraph library in R, the road data was preprocessed into a graph $G(V,E)$, where $V$ represents its sets of nodes and $E$ its set of edges. All road segments represent egdes and all intersections represent nodes.
The road graph serves as input into the shortest path algorithm, for which we used the Dijkstra's algorithm [2]. This algorithm outputs the shortest path between two nodes in a graph. It takes into account the edges' weights. If the user selects the "very dangerous route" in the shinyapp, these edge weights are simply the length of each road segment. If "very safe route" is selected, we check how many accidents occured on a road segment and add 300 m to this edge's length for each accident that occured on it. We choose 300 m since this produced the most logical routes with a good balance of route safety and route length. We counted accidents with severe injuries as two accidents and those that lead to a death as three accidents.


### Deploying the demo app

The demo app is deployed with [shinyapps.io](https://www.shinyapps.io/admin/). To use shinyapps.io, all you need is an account & the rsconnect package for R (`install.packages("rsconnect")`). Under **Account > Tokens** you can find the command with the credentials that looks something like this:

```R
rsconnect::setAccountInfo(name='<YOUR USERNAME>', 
	token='<YOUR TOKEN>', 
	secret='<YOUR SECRET>')
```
After you run this in Rstudio, a tiny icon should show up next to the "Run App" button (when you have your shiny app script opened). Here you can select **Publish Application**. 

The demo app can be found here: [https://emelieh21.shinyapps.io/danger-ranger-demo/](https://emelieh21.shinyapps.io/danger-ranger-demo/).

<p align="right">
	<img src="assets/logo.jpeg" alt="drawing" width="120"/>
</p>

# References
[1] Amt für Statistik Berlin-Brandenburg (2019). Strassenverkehrsunfälle nach Unfallort in Berlin 2019. mt für Statistik Berlin-Brandenburg (visited November 2020).
[2] West, D.B. (1996). Introduction to Graph Theory. Upper Saddle River, N.J.: Prentice Hall.

