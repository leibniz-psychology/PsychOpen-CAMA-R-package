FROM crzpiddev.azurecr.io/ocpu/ocpu-base-image:latest-dev

ARG OCPU_PASS
ARG SSL_KEY

RUN apt update -y &&\
    apt install -y vim

RUN cd /home/opencpu && \
    apt-get update && \
    apt-get install -y openssl && \
    openssl req -x509 -newkey rsa:4096 -sha256 -days 3650 -nodes \
      -keyout ocpu_ssl.key -out ocpu_ssl.crt -subj "/CN=opencpu.stg.zpid.org" \
      -addext "subjectAltName=DNS:opencpu.stg.zpid.org,DNS:www.opencpu.stg.zpid.org,IP:127.0.0.1"

RUN cd /home/opencpu && \
    mv /home/opencpu/ocpu_ssl.crt /etc/ssl/certs/ && \
    mv /home/opencpu/ocpu_ssl.key /etc/ssl/private/

#copy some config files
COPY . /home/opencpu/psychOpenCama
COPY .github/workflows/manifests/main/opencpu/index.html  /var/www/html/index.html

RUN cd /home/opencpu && \
    R -e "install.packages('remotes')" && \
    R CMD build psychOpenCama && \
    mv psychOpenCama_*.tar.gz psychOpenCama_latest.tar.gz && \
#    R -e "remotes::install_local('/home/opencpu/psychOpenCama_latest.tar.gz')"
    R CMD INSTALL psychOpenCama_latest.tar.gz --library=/usr/local/lib/R/site-library && \
    chmod 755 /var/www/html/index.html
#    touch /etc/ssl/certs/ocpu_ssl.crt && \
#    touch /etc/ssl/private/ocpu_ssl.key && \
#    chmod 644 /etc/ssl/certs/ocpu_ssl.crt && \
#    chmod 400 /etc/ssl/private/ocpu_ssl.key


# Set opencpu password
RUN \
  echo "opencpu:"$OCPU_PASS | chpasswd






