# TAF folder to produce a shiny app for the ICES TC on Intro to Stock Assessment

This app is running at <https://colinpmillar.shinyapps.io/tcisa2024/>

To run this code, install icesTAF
```r
install.packages("icesTAF")
```

then install all dependencies via:
```r
icesTAF::install.deps()
```

next set up the data, and create the shiny app in the `report` folder
```r
icesTAF::taf.boot()
icesTAF::source.all()
```

run the shiny app by:
```r
library(shiny)
runApp("report")
```
