FROM rocker/shiny-verse

# Update the machine
RUN apt update && apt install -y libcurl4-openssl-dev mysql-server git

# Download main and dev branches from covidseq
RUN git clone --single-branch --branch main https://github.com/MicrobialGenomics/covidseq /srv/shiny-server/covidseq && \
    git clone --single-branch --branch dev  https://github.com/MicrobialGenomics/covidseq /srv/shiny-server/covidseq_dev

# Copy covidapp and set permission
ADD ./ /srv/shiny-server/covidapp
RUN chmod -R +r /srv/shiny-server

# Install covidapp and covidseq
RUN R -e "devtools::install('/srv/shiny-server/covidapp')"
RUN R -e "devtools::install('/srv/shiny-server/covidseq_dev')"

# Expose port
EXPOSE 3838
