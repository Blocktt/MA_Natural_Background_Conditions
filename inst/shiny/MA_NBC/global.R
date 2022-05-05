# Shiny Global File

# Packages
library(shiny)
library(ggplot2)
library(reshape2)
library(dplyr)
library(utils)
library(tidyr)
library(shinythemes)
library(DT)

# File Size
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 10MB.
options(shiny.maxRequestSize = 25*1024^2)
