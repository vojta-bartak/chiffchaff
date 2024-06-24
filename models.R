library(tidyverse)
library(car)

load("data.RData")

stepAIC <- function(model){
  mods <- list(model)
  dr <- drop1(model)
  aics <- c(dr[1,]$AIC)
  print(paste("AIC = ", aics[length(aics)]))
  dr_var <- rownames(dr[dr$AIC==min(dr$AIC[rownames(dr)!="<none>"]),])
  dr_vars <- c("None")
  while (length(dr_var) > 0) {
    print(paste("Droping", dr_var))
    model <- update(model, paste("~.-", dr_var, sep=""))
    mods <- append(mods, list(model))
    dr_vars <- c(dr_vars, dr_var)
    (dr <- drop1(model))
    aics <- c(aics, dr[1,]$AIC)
    print(paste("AIC = ", aics[length(aics)]))
    dr_var <- rownames(dr[dr$AIC==min(dr$AIC[rownames(dr)!="<none>"]),] %>% "["(.!="<none>") %>% "["(0))
  }
  plot(aics~c(1:length(aics)), type="l", xlab = "Variable dropped", ylab = "AIC")
  points(aics~c(1:length(aics)))
  points(aics[which(aics==min(aics))]~c(1:length(aics))[which(aics==min(aics))], col = "red")
  axis(1, at=1:length(aics), labels = dr_vars)
  return(mods[[which(aics == min(aics))]])
}

# Multicollineariry check
vif(lm(end_rel ~ ALAN+noise_db+DEN+ temp_sunrise+prec_sunrise+press_sunrise+humid_sunrise+wind_sunrise+cloud+moon, data=df_end))

cor(df_start[,c("temp_")])

# start -------------------------------------------------------------------------------------------------------------------
m.start <- lm(start_rel ~ ALAN+noise_db+DEN+
              temp_sunrise+prec_sunrise+press_sunrise+humid_sunrise+wind_sunrise+cloud+moon+
              ALAN:noise_db + ALAN:DEN + noise_db:DEN, data=df_start)
plot(m.start)
summary(m.start)

summary(stepAIC(m.start))

# end ---------------------------------------------------------------------------------------------------------------------
m.end <- lm(end_rel ~ ALAN+noise_db+DEN+
              temp_sunset+prec_sunset+press_sunset+humid_sunset+wind_sunset+cloud+moon+
              ALAN:noise_db + ALAN:DEN + noise_db:DEN, data=df_end)
plot(m.end)
summary(m.end)

summary(stepAIC(m.end))
summary(step(m.end, trace = F))

# length morning ----------------------------------------------------------------------------------------------------------
m.lmorning <- lm(l_morning ~ ALAN+noise_db+DEN+
                   temp_sunrise+prec_sunrise+press_sunrise+humid_sunrise+wind_sunrise+cloud+moon+
                   ALAN:noise_db + ALAN:DEN + noise_db:DEN, data=df_lmorning)
plot(m.lmorning)
summary(m.lmorning)

summary(stepAIC(m.lmorning))
summary(step(m.lmorning, trace = F))

# count morning -----------------------------------------------------------------------------------------------------------
m.nmorning <- lm(n_morning ~ ALAN+noise_db+DEN+
                   temp_sunrise+prec_sunrise+press_sunrise+humid_sunrise+wind_sunrise+cloud+moon+
                   ALAN:noise_db + ALAN:DEN + noise_db:DEN, data=df_lmorning)
plot(m.nmorning)
summary(m.nmorning)

summary(stepAIC(m.nmorning))
summary(step(m.nmorning, trace = F))

# length evening ----------------------------------------------------------------------------------------------------------
m.levening <- lm(l_evening ~ ALAN+noise_db+DEN+
                   temp_sunset+prec_sunset+press_sunset+humid_sunset+wind_sunset+cloud+moon+
                   ALAN:noise_db + ALAN:DEN + noise_db:DEN, data=df_levening)
plot(m.levening)
summary(m.levening)

summary(stepAIC(m.levening))
summary(step(m.levening, trace = F))

# count evening -----------------------------------------------------------------------------------------------------------
m.nevening <- lm(n_evening ~ ALAN+noise_db+DEN+
                   temp_sunset+prec_sunset+press_sunset+humid_sunset+wind_sunset+cloud+moon+
                   ALAN:noise_db + ALAN:DEN + noise_db:DEN, data=df_levening)
plot(m.nevening)
summary(m.nevening)

summary(stepAIC(m.nevening))
summary(step(m.nevening, trace = F))
