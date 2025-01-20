## common
library(dplyr)

## ggplot2_add
library(ggplot2)
library(ggfortify)
library(cowplot)
library(GGally)
library(reshape2)
library(scales)

invisible(lapply(list.files('/Users/haneo/Documents/GitHub/HyeonRUtils/ggplot2_add/',
                            recursive = T, pattern = '\\.R$', full.names = T),
                 source))
