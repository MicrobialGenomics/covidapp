FROM rocker/shiny-verse

RUN apt update && apt install -y libcurl4-openssl-dev mysql-server git

ADD ./ /srv/shiny-server/covidapp
RUN chmod -R +r /srv/shiny-server

RUN R -e "devtools::install('/srv/shiny-server/covidapp')"

EXPOSE 3838
