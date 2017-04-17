---
title: "PCA Analysis"
author: "Xiuyang Guan"
date: "4/2/2017"
output:
  pdf_document: default
  html_document: default
---

# All locations 


```r
library(ggplot2)
library(reshape2)
require(gdata)
library(ggmap)
library(leaflet)
library(lattice)
library(knitr)

setwd("~/Google Drive/Xunyang_Fall16Spring17/Hilsa fish research/3.Analysis/Rscript/shinyapp/Analysis/PCA")
load("data/Overview_DATA.RData")
source("function_SVD.R")
PCA_Input <- PCA_Creation(Overview_DATA)
```
**Then, we set the parameters as the following:**
* threhold = 0.8
* similarity_threshold = 0.9


```r
Result <- Get_SVD(PCA_Input, threhold = 0.8, similarity_threshold = 0.9)
```

```
## Error in Get_SVD(PCA_Input, threhold = 0.8, similarity_threshold = 0.9): unused argument (threhold = 0.8)
```

```r
print(Result[[2]])
```

```
##  [1] "Serajgang"   "Sunamganj"   "Bahadurabad" "Sylhet"      "Kanaighat"  
##  [6] "Mymensingh"  "Amalshid"    "Sheola"      "Taraghat"    "Comilla"    
## [11] "Parshuram"
```
