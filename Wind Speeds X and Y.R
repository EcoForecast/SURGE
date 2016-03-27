
```{r}
Weather.Data = read.csv("WeatherData2015.csv")
windspeed = as.numeric(as.character(Weather.Data$Wind.SpeedMPH))
windspeed[3448] = 0
windspeed
theta = (Weather.Data$WindDirDegrees)

Windspeed.X = (windspeed*cos(theta))
Windspeed.Y = (windspeed*sin(theta))
Windspeed.X
Windspeed.Y
```

