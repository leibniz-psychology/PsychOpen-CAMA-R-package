FROM crzpiddev.azurecr.io/opencpu-adjusted:latest-dev

ARG OCPU_PASS
ARG SSL_KEY

RUN apt update -y &&\
    apt install -y vim

RUN cd /home/opencpu && \
    apt-get update && \
    apt-get install -y openssl && \
    openssl req -x509 -newkey rsa:4096 -sha256 -days 3650 -nodes \
      -keyout ocpu_ssl.key -out ocpu_ssl.crt -subj "/CN=opencpu.dev.zpid.org" \
      -addext "subjectAltName=DNS:opencpu.dev.zpid.org,DNS:www.opencpu.dev.zpid.org,IP:127.0.0.1"

RUN cd /home/opencpu && \
    mv /home/opencpu/ocpu_ssl.crt /etc/ssl/certs/ && \
    mv /home/opencpu/ocpu_ssl.key /etc/ssl/private/

#copy some config files
COPY . /home/opencpu/psychOpenCama
COPY .github/workflows/manifests/development/opencpu/opencpu.conf /etc/apache2/sites-available/
COPY .github/workflows/manifests/development/opencpu/default-ssl.conf /etc/apache2/sites-available/
COPY .github/workflows/manifests/development/opencpu/server.conf /etc/opencpu/
COPY .github/workflows/manifests/development/opencpu/apache2.conf /etc/apache2/
COPY .github/workflows/manifests/development/opencpu/security.conf /etc/apache2/conf-enabled/

RUN cd /home/opencpu && \
    R -e "install.packages('remotes')" \
    R CMD build psychOpenCama && \
    mv psychOpenCama_*.tar.gz psychOpenCama_latest.tar.gz && \
    R -e "remotes::install_local('/home/opencpu/psychOpenCama_latest.tar.gz', dependencies=T)"
#    R CMD INSTALL psychOpenCama_latest.tar.gz -library=/usr/local/lib/opencpu/site-library

# Set opencpu password
RUN \
  echo "opencpu:"$OCPU_PASS | chpasswd






