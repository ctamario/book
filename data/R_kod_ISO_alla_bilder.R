





library(pacman)

p_load(exifr, stringr, ggplot2)

dirs <- list.files("M:/DCIM/", pattern="ND800", full.names = T)

filer <- list.files(dirs, full.names=T)

read_exif(path=filer[1])$ISO

str(read_exif(path=filer[1]))

read_exif(path=filer[1])$ModifyDate

hm <- function(in_data){
  hej <- data.frame(ISO = in_data$ISO, datum = in_data$ModifyDate)
  hej$date <- as.Date(str_replace_all(str_sub(hej$datum, 1, 10), ":", "-"))
  return(hej)
}

hm(filer[1])

read_exif(filer[1], tags=c("ModifyDate", "ISO"))

urval <- sample(1:17470, 150)

urval2 <- 1:17470
urval2 <- urval2[-urval]
urval3 <- sample(urval2, 500, replace = F)

ut_data <- hm(read_exif(path=filer[urval], tags=c("ModifyDate", "ISO")))

ut_data$month <- format(ut_data$date, "%m")

ggplot(data = ut_data) +
  geom_boxplot(aes(x=month, y=ISO))

ut_data2 <- hm(read_exif(path=filer[urval3], tags=c("ModifyDate", "ISO")))
ut_data2$month <- format(ut_data2$date, "%m")

ut_data3 <- rbind(ut_data, ut_data2)

ggplot(data = ut_data3) +
  geom_boxplot(aes(x=month, y=log(ISO)))

table(ut_data3$month)


urval4 <- sample(urval2[-urval3], 1000)


ut_data4 <- hm(read_exif(path=filer[urval4], tags=c("ModifyDate", "ISO")))


1000*0.45/60


#omegalul <- NA
for(i in 201:230){
  start.time <- Sys.time()
  hm(read_exif(filer[1]))
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  omegalul[i] <- time.taken
}

boxplot(omegalul~rep(c("A","B","C"),c(100,100,30)))

ut_data4$month <- format(ut_data4$date, "%m")

ehh <- rbind(ut_data3, ut_data4)

ggplot(data = ehh) +
  geom_boxplot(aes(x=month, y=ISO))


table(ehh$month)

urval5 <- c(1:17470)[-c(urval4,urval3,urval)]

start.time <- Sys.time()
ut_data5 <- hm(read_exif(path=filer[sample(urval5, 5000, replace=F)], tags=c("ModifyDate", "ISO")))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

ut_data5$month <- format(ut_data5$date, "%m")

ehh <- rbind(ut_data3, ut_data4, ut_data5)

ggplot(data = ehh) +
  geom_boxplot(aes(x=month, y=log(ISO)))

ehh$datetime <- as.POSIXct(ehh$datum, format="%Y:%m:%d %H:%M:%S")

install.packages(c("circular", "lubridate"))
library(circular)
library(lubridate)

df <- ehh[complete.cases(ehh),]

df$circ_time_of_day <- circular(hour(df$datetime) * 2 * pi / 24)
df$circ_time_of_year <- circular(yday(df$datetime) * 2 * pi / 365.25)

model <- lm(log(ISO) ~ circ_time_of_day + circ_time_of_year, data = df)
summary(model)

par(mfrow=c(2, 1))
plot(model, which = 1)  # Diagnostic plot
plot(model, which = 2)  # Residuals vs. Fitted

plot(model)

# Assuming 'your_data' is your dataset and 'model' is your fitted model
df$predicted_iso <- predict(model)

# Plotting predicted vs. actual values
ggplot(df, aes(x = ISO, y = predicted_iso)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Actual ISO", y = "Predicted ISO", title = "Predicted vs Actual ISO") +
  theme_minimal()

# Extract date information for better plotting
df$jdate <- as.numeric(format(df$datetime, "%j"))
df$hour <- as.numeric(format(df$datetime, "%H"))

# Plotting predicted ISO over the course of the year
ggplot(df, aes(x = jdate, y = exp(predicted_iso))) +
  geom_line() +
  labs(x = "Date", y = "Predicted ISO", title = "Predicted ISO Over the Course of the Year") +
  theme_minimal()


plot(data=df, as.numeric(format(df$datetime, "%H"))~as.numeric(circ_time_of_day))

# Assuming 'df' is your dataset with transformed variables
df$predicted_iso <- exp(predict(model))  # Back-transform the predictions

# Plotting linear variable against transformed cyclic variable
ggplot(df, aes(x = circ_time_of_day, y = predicted_iso)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Circular Time of Day", y = "Predicted ISO", title = "Linear vs Transformed Cyclic Variable") +
  theme_minimal()

ggplot(df, aes(x = circ_time_of_year, y = predicted_iso)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Circular Time of Year", y = "Predicted ISO", title = "Linear vs Transformed Cyclic Variable") +
  theme_minimal()

# Assuming 'df' is your dataset with transformed variables
df$predicted_iso <- exp(predict(model))  # Back-transform the predictions

# Encoding circular variables
df$circ_time_of_day <- circular::periodic(hour(df$datetime) / 24)
df$circ_time_of_year <- circular::periodic(yday(df$datetime) / 365.25)

# Plotting linear variable against transformed cyclic variable
ggplot(df, aes(x = circ_time_of_day, y = predicted_iso)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Circular Time of Day", y = "Predicted ISO", title = "Linear vs Transformed Cyclic Variable") +
  theme_minimal()

ggplot(df, aes(x = circ_time_of_year, y = predicted_iso)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Circular Time of Year", y = "Predicted ISO", title = "Linear vs Transformed Cyclic Variable") +
  theme_minimal()



# Assuming 'df' is your dataset with transformed variables
df$predicted_iso <- exp(predict(model))  # Back-transform the predictions

# Encoding circular variables
df$circ_time_of_day <- circular(hour(df$datetime) * 2 * pi / 24)
df$circ_time_of_year <- circular(yday(df$datetime) * 2 * pi / 365.25)

# Assuming 'df' is your dataset with a datetime variable
df$day_of_year <- yday(df$datetime)

# Encode day of the year using sine and cosine
df$cyclic_day_sin <- sin(2 * pi * df$day_of_year / 365)
df$cyclic_day_cos <- cos(2 * pi * df$day_of_year / 365)

df$cyclic_hour_sin <- sin(2 * pi * df$hour / 24)
df$cyclic_hour_cos <- cos(2 * pi * df$hour / 24)

# Plotting cyclic variable
ggplot(df, aes(x = jdate, y = cyclic_day_cos)) +
  geom_point() +
  labs(x = "Cyclic Day (sin)", y = "Cyclic Day (cos)", title = "Cyclic Encoding of Day of the Year") +
  theme_minimal()

ggplot(df, aes(x = hour, y = cyclic_hour_cos)) +
  geom_point() +
  labs(x = "Cyclic Day (sin)", y = "Cyclic Day (cos)", title = "Cyclic Encoding of Day of the Year") +
  theme_minimal()


model <- lm(log(ISO) ~  cyclic_day_cos * cyclic_hour_cos, data = df)
summary(model)

df$predicted_iso <- exp(predict(model)) 

ggplot(df, aes(x = jdate, y = predicted_iso)) +
  geom_point(aes(color=cyclic_hour_cos)) +
  labs(x = "Circular Time of Year", y = "Predicted ISO") +
  theme_minimal()

filer2 <- list.files(list.files("M:/DCIM/It starts over here/", full.names = T), full.names = T)



start.time <- Sys.time()
ut_data6 <- hm(read_exif(path=c(filer,filer2), tags=c("ModifyDate", "ISO")))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


ut_data6$month <- format(ut_data6$date, "%m")

df <- ut_data6

df <- df[complete.cases(df),]

df$datetime <- as.POSIXct(df$datum, format="%Y:%m:%d %H:%M:%S")

# Extract date information for better plotting
df$jdate <- as.numeric(format(df$datetime, "%j"))
df$hour <- as.numeric(format(df$datetime, "%H"))

ggplot(data = df2) +
  geom_boxplot(aes(x=month, y=log(ISO)))

# Encode day of the year using sine and cosine
df$cyclic_day_sin <- sin(2 * pi * df$jdate / 365)
df$cyclic_day_cos <- cos(2 * pi * df$jdate / 365)

df$cyclic_hour_sin <- sin(2 * pi * df$hour / 24)
df$cyclic_hour_cos <- cos(2 * pi * df$hour / 24)

# Plotting cyclic variable
p1 <- ggplot(df, aes(x = jdate, y = cyclic_day_cos)) +
  geom_point() +
  labs(x = "Cyclic Day (sin)", y = "Cyclic Day (cos)", title = "Cyclic Encoding of Day of the Year") +
  theme_classic()

p2 <- df2 %>% group_by(hour, cyclic_hour_cos) %>% 
  summarize(cyclic_hour_cos=mean(cyclic_hour_cos)) %>%
  ggplot(aes(x = hour, y = cyclic_hour_cos, color=cyclic_hour_cos)) +
  geom_point() +
  labs(x = "Hour of the day", y = "Cyclic Hour (cos)") +
  theme_classic()

df2 <- df %>% group_by(jdate, month, hour, cyclic_day_cos, cyclic_hour_cos, 
                       cyclic_day_sin, cyclic_hour_sin) %>% summarize(mean_ISO=mean(ISO, na.rm=T))


model <- lm(log(mean_ISO) ~  cyclic_day_sin + cyclic_hour_cos, data = df2)
summary(model)

model2 <- lm(log(mean_ISO) ~  cyclic_day_sin * cyclic_hour_cos, data = df2)
summary(model2)

anova(model, model2)
AIC(model, model2)

df2$predicted_iso <- exp(predict(model)) 

ggplot(df2, aes(x = jdate, y = predicted_iso)) +
  geom_point(aes(color=cyclic_hour_cos)) +
  labs(x = "Day of the year", y = "Predicted ISO") +
  theme_classic()

#grid.arrange(p2, p3, nrow=1)
#p_load(gridExtra, dplyr)

ggplot(data = df2) +
  geom_boxplot(aes(x=month, y=mean_ISO))+
  scale_y_continuous(trans='log10')+
  theme_classic()

table(df$month)

write.table(file="C:/Users/ÄGAREN/Desktop/krysshobbyrelaterat/iso_alla_filer.csv", df)
