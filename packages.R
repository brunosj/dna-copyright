
# load packages

# install packages from CRAN
p_needed <- c(
  'rJava',
  'statnet',
  'xergm',
  'igraph',
  'cluster',
  'devtools',
  'rDNA',
  'statnet',
  'network',
  'GGally',
  'geomnet',
  'data.table',
  'kableExtra',
  'gridExtra',
  'qwraps2',
  'xlsx',
  'reshape2',
  'ggplot2',
  'sna',
  'RColorBrewer',
  'ggthemes',
  'summarytools',
  'plyr',
  'paletteer',
  'dplyr', 
  'tidyr', 
  'gapminder',
  'ggplot2',  
  'ggalt',
  'forcats', 
  'R.utils', 
  'png', 
  'grid', 
  'ggpubr', 
  'scales',
  'bbplot'
)
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)
