# Sophia Tan
# configuration file to be loaded at the start of all scripts
# clears environment and loads packages needed for cleaning and analysis

rm(list=ls())
gc() 

require(tidyverse)
require(lubridate)
require(readr)
require(MatchIt)
require(survival)
require(tableone)

require(patchwork)
require(ggplot2)
require(cowplot)
require(ggh4x)
require(RColorBrewer)