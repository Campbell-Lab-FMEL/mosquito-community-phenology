library(lubridate)
library(tidyverse)
library(rbms)

# for information about installing 'rbms' follow https://rdrr.io/github/RetoSchmucki/rbms/

setwd("")

#data_preprocesing
IL_ymd_mosabund <-  read_csv("IL_00_20_red_spp_LONG.csv")
IL_ymd_mosabund$DATE <- as.Date(IL_ymd_mosabund$date)
IL_ymd_mosabund$SITE_ID <- IL_ymd_mosabund$Loc_ID
IL_ymd_mosabund$species_numeric <- as.numeric(factor(IL_ymd_mosabund$species, levels=unique(IL_ymd_mosabund$species)))
IL_ymd_mosabund$year <- year(IL_ymd_mosabund$date)
IL_ymd_mosabund2 <- IL_ymd_mosabund %>%
  filter(str_detect(species, "genus") != TRUE)
IL_ymd_mosabund2 <- select(IL_ymd_mosabund2, -c(Latitudes.y, Longitudes.y))
IL_ymd_mosabund2 <- rename(IL_ymd_mosabund2, SPECIES = species_numeric)
IL_ymd_mosabund2 <- rename(IL_ymd_mosabund2, COUNT = count)
IL_ymd_mosabund2_lc1 <- subset(IL_ymd_mosabund2, class == "1")
IL_ymd_mosabund2_lc2 <- subset(IL_ymd_mosabund2, class == "2")
IL_ymd_mosabund_count_lc1 <- IL_ymd_mosabund2_lc1 %>% select(SITE_ID,DATE, SPECIES, COUNT, species, Longitudes.x,Latitudes.x, year)
IL_ymd_mosabund_site_lc1 <- IL_ymd_mosabund2_lc1 %>% select(SITE_ID,DATE, Longitudes.x,Latitudes.x)
IL_ymd_mosabund_count_lc2 <- IL_ymd_mosabund2_lc2 %>% select(SITE_ID,DATE, SPECIES, COUNT, species, Longitudes.x,Latitudes.x, year)
IL_ymd_mosabund_site_lc2 <- IL_ymd_mosabund2_lc2 %>% select(SITE_ID,DATE, Longitudes.x,Latitudes.x)
ts_date <- rbms::ts_dwmy_table(InitYear = 2001, LastYear = 2020, WeekDay1 = 'monday')
ts_season <- rbms::ts_monit_season(ts_date, StartMonth = 4, EndMonth = 10, StartDay = 30, EndDay = NULL, CompltSeason = TRUE, Anchor = TRUE, AnchorLength = 2, AnchorLag = 2, TimeUnit = 'd')
ts_season_visit_lc1 <- rbms::ts_monit_site(IL_ymd_mosabund_site_lc1, ts_season)
ts_season_count_lc1 <- rbms::ts_monit_count_site(ts_season_visit_lc1, IL_ymd_mosabund_count_lc1, sp = 26)
ts_flight_curve_lc1 <- rbms::flight_curve(ts_season_count_lc1, NbrSample = 300, MinVisit = 4, MinOccur = 2, MinNbrSite = 4, MaxTrial = 4, GamFamily = 'nb', SpeedGam = FALSE, CompltSeason = FALSE, SelectYear = NULL, TimeUnit = 'd')
ts_season_visit_lc2 <- rbms::ts_monit_site(IL_ymd_mosabund_site_lc2, ts_season)
ts_season_count_lc2 <- rbms::ts_monit_count_site(ts_season_visit_lc2, IL_ymd_mosabund_count_lc2, sp = 26)
ts_flight_curve_lc2 <- rbms::flight_curve(ts_season_count_lc2, NbrSample = 300, MinVisit = 3, MinOccur = 2, MinNbrSite = 2, MaxTrial = 4, GamFamily = 'nb', SpeedGam = FALSE, CompltSeason = FALSE, SelectYear = NULL, TimeUnit = 'd')


## Extract phenology part
pheno <- ts_flight_curve_lc2$pheno
## Filter out some noise from pheno
pheno2 <- pheno %>%
  group_by(YEAR) %>%
  filter(SITE_ID %in% sample(unique(SITE_ID), 1))

## group_by_year and calculate cumulative percent
pheno_cumu<- pheno2 %>%
  group_by(YEAR) %>%
  mutate(cum_perc = cumsum(NM))
phenometrics <- pheno_cumu %>%
  group_by(YEAR) %>%
  summarize(fifteen = trimDAYNO[which.max(cum_perc >= 0.15)],
            peak = trimDAYNO[which.max(NM)],
            fifty = trimDAYNO[which.max(cum_perc >= 0.5)],
            eightyfive = trimDAYNO[which.max(cum_perc >= 0.85)],
            lc_class = rep("lc2"),
            species="Uranotaenia_sapphirina")

write.csv(phenometrics,"phenometrics_Ur_sapphirina.csv",row.names = F)

## following the preceding step for each of the 7 species included in the analyses for lc2 and also for lc1

## assembled phenometrics for all species, years, and land cover types are in mosquitoILphenomets.csv



