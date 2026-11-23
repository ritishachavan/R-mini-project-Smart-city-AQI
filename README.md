Air pollution is a major environmental concern in urban areas because it affects public health,
quality of life and sustainable city development. The Air Quality Index (AQI) provides a numerical representation 
of air quality and helps communicate the potential health effects associated with different levels of pollution.
The project “Smart City Urban Air Quality Forecasting & Health Risk Mapping” uses R programming to analyse AQI observations,
examine the relationship between weather and human activities, compare forecasting models and construct an indicative health-risk surface.
The analysis uses a Kaggle-sourced dataset read by the R script from a file named aqi_dataset.csv.
The variables used in the program include temperature, humidity, wind speed, traffic density, industrial activity and AQI. 
Data preprocessing is performed by identifying physically invalid values, handling missing observations and deriving AQI health categories.
Exploratory data analysis is then carried out using descriptive summaries, distribution plots, category proportions, correlation analysis 
and visual comparisons of AQI with traffic and industrial activity.


The AQI analysis was also deployed as an interactive R Shiny application using shinyapps.io.
The deployment completed successfully, and the published application is available at the following URL:
https://ritishachavan.shinyapps.io/smart_city_urban_air_quality/
The deployed dashboard allows the project outputs to be viewed through a web browser. The application is hosted on shinyapps.io and
uses the AQI project files and required R packages included during deployment.
