FROM crzpiddev.azurecr.io/opencpu-adjusted:v1

ARG OCPU_PASS
ARG SSL_KEY


RUN apt update -y &&\
    apt install -y vim

#install needed dependencies
#RUN apt-get update && apt-get install -y \
#    libcurl4-gnutls-dev \
#    && apt install -y cmake \
#    ## clean up
#    && apt-get clean \
#    && rm -rf /var/lib/apt/lists/ \
#    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

#RUN cp /etc/apache2/mods-available/rewrite.load /etc/apache2/mods-enabled/ && \
#    cp /etc/apache2/mods-available/headers.load /etc/apache2/mods-enabled/

#RUN R -e "install.packages('devtools')"
#RUN R -e "install.packages('metafor')"
#RUN R -e "install.packages('ggplot2')"
#RUN R -e "install.packages('GGally')"
#RUN R -e "install.packages('metapower')"
#RUN R -e "install.packages('meta')"
#RUN R -e "install.packages('metaviz')"
#RUN R -e "install.packages('netmeta',dependencies=TRUE, repos='http://cran.rstudio.com/')"
#RUN R -e "install.packages('psych')"
#RUN R -e "install.packages('labelVector')"
#RUN R -e "install.packages('pwr')"
#RUN R -e "devtools::install_github('MathiasHarrer/dmetar')"

RUN apt-get update && \
    apt-get install -y openssl && \
    openssl req -x509 -newkey rsa:4096 -sha256 -days 3650 -nodes \
      -keyout example.key -out example.crt -subj "/CN=opencpu.dev.zpid.org" \
      -addext "subjectAltName=DNS:opencpu.dev.zpid.org,DNS:www.opencpu.dev.zpid.org,IP:12.0.0.1"
#    openssl genrsa -des3 -passout pass:$SSL_KEY -out server.pass.key 2048 && \
#    openssl rsa -passin pass:x -in server.pass.key -out server.key && \
#    rm server.pass.key && \
#    openssl req -new -key server.key -out server.csr \
#        -subj "/C=DE/ST=Rheinland-Pfalz/L=Trier/O=Leibniz Institut für Psychologie/OU=IT Department/CN=opencpu.dev.zpid.org" && \
#    openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt


#copy some config files
COPY . /home/opencpu/psychOpenCama
COPY .github/workflows/manifests/development/opencpu/opencpu.conf /etc/apache2/sites-available/
COPY .github/workflows/manifests/development/opencpu/server.conf /etc/opencpu/
COPY .github/workflows/manifests/development/opencpu/apache2.conf /etc/apache2/
COPY .github/workflows/manifests/development/opencpu/security.conf /etc/apache2/conf-enabled/

RUN cd /home/opencpu && \
    R CMD build psychOpenCama && \
    mv psychOpenCama_*.tar.gz psychOpenCama_latest.tar.gz && \
    R CMD INSTALL psychOpenCama_latest.tar.gz -library=/usr/local/lib/opencpu/site-library

# Set opencpu password
RUN \
  echo "opencpu:"$OCPU_PASS | chpasswd






