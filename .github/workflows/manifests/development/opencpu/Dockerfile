FROM opencpu/base:latest

ARG OCPU_PASS

COPY . /home/opencpu/psychOpenCama
COPY .github/workflows/manifests/development/opencpu/server.conf /etc/opencpu/

#install needed dependenciesp
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    && apt install -y cmake \
    ## clean up
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/ \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

COPY .github/workflows/manifests/development/opencpu/opencpu.conf /etc/apache2/sites-available/
COPY .github/workflows/manifests/development/opencpu/server.conf /etc/opencpu/

RUN cp /etc/apache2/mods-available/rewrite.load /etc/apache2/mods-enabled/ && \
    cp /etc/apache2/mods-available/headers.load /etc/apache2/mods-enabled/ \

RUN R -e "install.packages('devtools')"
RUN R -e "install.packages('metafor')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('GGally')"
RUN R -e "install.packages('metapower')"
RUN R -e "install.packages('meta')"
RUN R -e "install.packages('metaviz')"
RUN R -e "install.packages('netmeta',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('psych')"
RUN R -e "install.packages('labelVector')"
RUN R -e "install.packages('pwr')"
RUN R -e "devtools::install_github('MathiasHarrer/dmetar')"

RUN cd /home/opencpu && \
    R CMD build psychOpenCama && \
    mv psychOpenCama_*.tar.gz psychOpenCama_latest.tar.gz && \
    R CMD INSTALL psychOpenCama_latest.tar.gz -library=/usr/local/lib/opencpu/site-library

# Set opencpu password
RUN \
  echo "opencpu:"$OCPU_PASS | chpasswd






