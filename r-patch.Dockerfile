FROM ghcr.io/learning-games/r:2021-11-23

RUN R -e 'install.packages(c("stringr", "network","sna","ggnetwork","data.table","xtable", "ggplot2", "latex2exp", "patchwork"))'