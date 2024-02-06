




### 

library(pacman)

p_load(dplyr, ggplot2, sjPlot)

###

setwd("C:/Users/ÄGAREN/Desktop/krysshobbyrelaterat/ISO_projekt")

df <- read.csv("iso_alla_filer_ny.csv")

df <- df[complete.cases(df),]

df$datetime <- as.POSIXct(df$ModifyDate, format="%Y:%m:%d %H:%M:%S")

# Extract date information for better plotting
df$date <- format(df$datetime, "%Y-%m-%d")
df$time <- format(df$datetime, "%H:%M:%S")
df$jdate <- as.numeric(format(df$datetime, "%j"))
df$hour <- as.numeric(format(df$datetime, "%H"))
df$month <- format(df$date, "%m")

df_clean <- df %>% filter(Flash == 0) %>% filter(Aperture < 10)

head(df_clean)

df_clean$date2 <- as.Date(df_clean$date)
#df_clean <- df_clean %>% filter(as.numeric(format(date2, "%Y")) < 2022)


circular_transform <- function(jdate, hour, shift_date = 0, shift_hour = 0) {
  # Calculate angular variables for day of the year and hour of the day
  jdate_angle <- ((jdate - 1) / 365 + shift_date) * 2 * pi
  hour_angle <- ((hour / 24) + shift_hour) * 2 * pi
  
  # Transform to circular coordinates using sine and cosine
  jdate_sin <- sin(jdate_angle)
  jdate_cos <- cos(jdate_angle)
  
  hour_sin <- sin(hour_angle)
  hour_cos <- cos(hour_angle)
  
  # Return a data frame with the transformed variables
  transformed_data <- data.frame(jdate_sin, jdate_cos, hour_sin, hour_cos)
  
  return(transformed_data)
}

results <- NULL

for(i in seq(from=0, to=1, by=0.05)){
  for(j in seq(from=0, to=1, by=0.05)){
    shift_date_in <- i
    shift_hour_in <- j
    
    new_df <- cbind(df_clean, circular_transform(df_clean$jdate, df_clean$hour,
                                                 shift_date=shift_date_in,
                                                 shift_hour=shift_hour_in))
    
    head(new_df)
    
    #new_df %>% ggplot(aes(x = jdate, y = jdate_sin)) + geom_point()
    #new_df %>% ggplot(aes(x = hour, y = hour_sin)) + geom_point()
    
    df2 <- new_df %>% group_by(jdate, hour, jdate_sin, hour_sin) %>% summarise(mean_ISO = mean(ISO, na.rm=T))
    
    model <- lm(data=df2, log(mean_ISO) ~ jdate_sin * hour_sin)
    
    summary(model)
    
    df2$predicted_iso <- exp(predict(model)) 
    
    ggplot(df2, aes(x = jdate, y = predicted_iso)) +
      geom_point(aes(color=hour_sin)) +
      labs(x = "Day of the year", y = "Predicted ISO") +
      theme_classic()
    
    results <- rbind(results, data.frame(AIC(model), "shift_date"=shift_date_in, "shift_hour" = shift_hour_in))
    
  }
}

results[which.min(results$AIC),]

results %>% ggplot(aes(x=shift_date, y=shift_hour, color=AIC.model.)) + geom_point()

shift_date_in <- 0.45
shift_hour_in <- 0.3

new_df <- cbind(df_clean, circular_transform(df_clean$jdate, df_clean$hour,
                                             shift_date=shift_date_in,
                                             shift_hour=shift_hour_in))

head(new_df)

#new_df %>% ggplot(aes(x = jdate, y = jdate_sin)) + geom_point()
#new_df %>% ggplot(aes(x = hour, y = hour_sin)) + geom_point()

df2 <- new_df %>% group_by(jdate, hour, month, jdate_sin, hour_sin) %>% summarise(mean_ISO = mean(ISO, na.rm=T))

model <- lm(data=df2, log(mean_ISO) ~ jdate_sin * hour_sin)

summary(model)

df2$predicted_iso <- exp(predict(model)) 

ggplot(df2, aes(x = jdate, y = predicted_iso)) +
  geom_point(aes(color=hour_sin)) +
  labs(x = "Day of the year", y = "Predicted ISO") +
  theme_classic()+
  scale_color_gradient(low="skyblue", high="darkblue")+
  theme(legend.position = "none")

df2 %>% ggplot(aes(x = factor(month), y = mean_ISO, group=month)) + 
  geom_boxplot() +
  labs(x="Month", 
       y="ISO\n(means per hour and day)") +
  scale_y_continuous(trans="log10")+
  theme_classic()

df2 %>% ggplot(aes(x=hour, y=hour_sin, color=hour_sin)) + 
  geom_point() +
  labs(x="Hour of the day",
       y="Sin-transformed hour")+
  scale_color_gradient(low="skyblue", high="darkblue")+
  theme_classic()


