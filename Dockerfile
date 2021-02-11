FROM francesccatala/shiny-verse:4.0.1

# Copy covidseq_dev and set permission
ADD ./covidseq_dev /srv/shiny-server/covidseq_dev
ADD ./covidseq /srv/shiny-server/covidseq
ADD ./covidapp /srv/shiny-server/covidapp

RUN chmod -R +r /srv/shiny-server

# Install covidapp and covidseq
RUN R -e "devtools::install('/srv/shiny-server/covidapp')"
RUN R -e "devtools::install('/srv/shiny-server/covidseq_dev')"

# Expose port
EXPOSE 3838
