# SURGE

Contact Info:

Erin Freeman
425-638-3053
erinef@bu.edu

Michael Silano
203-522-8901
msilano@bu.edu
msilano105@gmail.com

Brenna Stallings
919-649-1404
brennas@bu.edu


Our website is 54.174.19.98/SURGE

OFFICIAL LIST OF MODEL FILES

pull_weather_data.R
pull_hist_weather.R
pull_tide_data.R
pull_hist_tide.R
pull_surge_data.R
pull_hist_surge.R

pres_driven_timeseries.R
MC-Forecast.R
EnKF.R
Diagnostics.R

Cron Jobs:
weather_cron.txt
tide_height_cron.txt
surge_height_cron.txt
driven_cron.txt
MC_cron.txt
EnKF_cron.txt



The cron schedules for tide height data and weather data is everyday at midnight. They run the files named pull_height_data and pull_weather_data. 
The chron schedule for wind and tide data is everyday at midnight. It runs the file named pull_weather_data.
