library(ggplot2)
library(scales)
# Change ggplot theme
if (!require(themejj)) {
  devtools::install_github("janajarecki/themejj")
}
theme_set(themejj())
# Add multiple plots togehter like p1  + p2
if (!require(patchwork)) {
  install.packages('patchwork')
}
library(patchwork)