# Gefahrradtour

Project for the HERE Map Data Hackathon 2020

### Introduction

Find the safest route from A to B in Berlin thanks to HERE Data Layers and Berlin traffic accident data.

### Deploying the demo app

The demo app is deployed with [shinyapps.io](https://www.shinyapps.io/admin/). To use shinyapps.io, all you need is an account & the rsconnect package for R (`install.packages("rsconnect")`). Under **Account > Tokens** you can find the command with the credentials that looks something like this:

```R
rsconnect::setAccountInfo(name='<YOUR USERNAME>', 
	token='<YOUR TOKEN>', 
	secret='<YOUR SECRET>')
```
After you run this in Rstudio, a tiny icon should show up next to the "Run App" button (when you have your shiny app script opened). Here you can select **Publish Application**. 

The demo app can be found here: [https://emelieh21.shinyapps.io/Gefahrradtour/](https://emelieh21.shinyapps.io/Gefahrradtour/).

<p align="right">
	<img src="assets/logo.jpeg" alt="drawing" width="120"/>
</p>


