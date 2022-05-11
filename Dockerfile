FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y libssl-dev && \
  install2.r --error leaflet shiny plotly dplyr reticulate readr RSocrata plyr lubridate httr caret

COPY . .

EXPOSE 3838

CMD R -e 'shiny::runApp("app.R", port = 3838, host = "0.0.0.0")'