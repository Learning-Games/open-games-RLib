FROM debian:9-slim@sha256:1a470b92197dd16a46f7aa9cb308fa91f7d0948e0dccd625a03cbbdf2d4516e6
MAINTAINER Chris Done

################################################################################
# Haskell system dependencies (basically never changes)

RUN apt-get update && \
    apt-get install -yq --no-install-suggests --no-install-recommends --force-yes -y -qq \
            dirmngr apt-transport-https ca-certificates software-properties-common gnupg2

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-key 'E19F5F87128899B192B1A2C2AD5F960A256A04AF' && \
   add-apt-repository 'deb https://cloud.r-project.org/bin/linux/debian stretch-cran35/' && \
   apt-get update && \
   apt-get install -y r-base --allow-unauthenticated

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y locales
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales && \
    update-locale LANG=en_US.UTF-8
ENV LANG en_US.UTF-8

RUN R --version && \
    R -e 'install.packages(c("stringr","network","sna","ggnetwork","data.table","xtable", "ggplot2", "latex2exp","patchwork"))'
