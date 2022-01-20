FROM microbialgenomics/shiny-geospatial:4.1.2

# Copy covidseq_dev and set permission
RUN rm -rf /srv/shiny-server/*
ADD ./covidapp /srv/shiny-server
RUN chmod -R +r /srv/shiny-server
RUN rm /srv/shiny-server/app_customData.R

# Install deps and package
RUN R -e "devtools::install_deps('/srv/shiny-server')"
RUN R -e "devtools::install('/srv/shiny-server')"

# Expose port
EXPOSE 3838