library(tidyverse)
library(readxl)

df <- read.table("budnicek_data_doplnena.csv", header=T, sep=";") %>% 
  filter(Id_lokality != "") %>% 
  mutate(moon = X..moon.illumination,
         period = ifelse(is.na(HOD_ZAPAD), "M","E"),
         noise_db = case_when(hluk.db == "< 50" ~ 50,
                              hluk.db %in% c("50-55", "50 - 55") ~ 55,
                              hluk.db %in% c("55 - 60", "55 - 60 ") ~ 60,
                              hluk.db == "60 - 65" ~ 65,
                              hluk.db == "65 - 70" ~ 70,
                              hluk.db == "70 - 75" ~ 75),
         delka = as.numeric(DELKA_ZPEVU..s.),
         time = as.POSIXct(INTERVAL, format = "%H:%M"))


# df <- read_xlsx("budnicek_data.xlsx", sheet=1) %>% 
#   mutate(moon = get("% moon illumination"),
#          period = ifelse(is.na(HOD_ZAPAD), "M","E"),
#          noise_db = case_when(get("hluk db") == "< 50" ~ 50,
#                               get("hluk db") %in% c("50-55", "50 - 55") ~ 55,
#                               get("hluk db") %in% c("55 - 60", "55 - 60 ") ~ 60,
#                               get("hluk db") == "60 - 65" ~ 65,
#                               get("hluk db") == "65 - 70" ~ 70,
#                               get("hluk db") == "70 - 75" ~ 75))

# start
df_start <- df %>% 
  filter(!is.na(zacatek_zpevu)) %>% 
  mutate(start_rel = východ) %>% 
  filter(!is.na(start_rel))

# end
df_end <- df %>% 
  filter(!is.na(konec_zpevu)) %>% 
  mutate(end_rel = západ) %>% 
  filter(!is.na(end_rel))

# l_morning
df_lmorning <- df %>% 
  filter(time < as.POSIXct("12:00", format="%H:%M")) %>% 
  group_by(Id_lokality) %>% 
  summarize(l_morning = sum(delka),
            n_morning = sum(POCET_ZPEVU)) %>% 
  left_join(df %>% select(Id_lokality,temp_sunrise,prec_sunrise,press_sunrise,humid_sunrise,wind_sunrise,cloud,moon,ALAN,noise_db,DEN) %>% 
              distinct)

# l_evening
df_levening <- df %>% 
  filter(time > as.POSIXct("12:00", format="%H:%M")) %>% 
  group_by(Id_lokality) %>% 
  summarize(l_evening = sum(delka),
            n_evening = sum(POCET_ZPEVU)) %>% 
  left_join(df %>% select(Id_lokality,temp_sunset,prec_sunset,press_sunset,humid_sunset,wind_sunset,cloud,moon,ALAN,noise_db,DEN) %>% 
              distinct)

save(df_start, df_end, df_lmorning, df_levening, file = "data.RData")




