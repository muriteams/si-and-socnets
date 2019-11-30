# female: in deg, out deg, simil or homophily

library(dplyr)
library(magrittr)

dat     <- haven::read_sav("data-raw/AllSurveys - FINAL_073018.sav")
group   <- haven::read_sav("data-raw/AllSurveys - FINAL - Group level data_11-9-18.sav")


# Standarized Collective Intelligence Variables in Time 1 and 2
# CIF_T1, CIF_T2
group <- subset(
  group, select = c(Group, CIF_T1, CIF_T2)
) %>%
  mutate(Group = as.integer(Group))

# Merging information
dat$Group <- as.integer(dat$Group)
dat <- left_join(dat, group, by = "Group")

group_ids <- trimws(dat$Group)

# A lot of missigness in GPA
miss <- list(
  GPA        = unique(group_ids[which(is.na(dat$GPAclean))]),
  Female     = unique(group_ids[which(is.na(dat$GenderMvsF))]),
  NoReligion = unique(group_ids[which(is.na(dat$Religion))])
)

# Normalizing soc skill
dat$Empathy_norm <- (dat$Empathy - min(dat$Empathy))/
  diff(range(dat$Empathy))

# Function to create quantiles
quantilizer <- function(x, probs = seq(0, 1, length.out = 5)) {
  
  ran <- quantile(x, na.rm = TRUE, probs = probs)
  ran[c(1, length(ran))] <- ran[c(1, length(ran))] + c(-1, 1)
  as.numeric(cut(x, breaks = ran))
  
}

dat$GPAclean <- na_if(dat$GPAclean, 10)

# Creating categorical versions
table(
  dat$GPAclean,
  quantilizer(dat$GPAclean, seq(0, 1, length.out = 4))
)
dat[["GPAcleanCat"]] <- quantilizer(dat$GPAclean, seq(0, 1, length.out = 4))

for (v in c("Age", "ZRMEscore", "SI3Fac1", "SI3Fac2", "SI3Fac3", "FLAbRel_NoOuts")) 
  dat[[paste0(v, "Cat")]] <- quantilizer(dat[[v]])

dat <- dat %>%
  select(PID, GenderMvsF, racenonwhite,
         Age, GPAclean, SI3Fac1, SI3Fac2, SI3Fac3, FLAbRel_NoOuts,
         AgeCat, GPAcleanCat, ZRMEscoreCat, SI3Fac1Cat, SI3Fac2Cat, SI3Fac3Cat, FLAbRel_NoOutsCat,
         # Non-factor variables
         TIPI_Extraversion, TIPI_Agreeableness, TIPI_Conscientiousness,
         TIPI_EmotionalStability, TIPI_OpennesstoExperiences,
         ZRMEscore, SocialInfoProcessing, SocialSkills, SocialAwareness,
         SocialResponsibility, Empathy, InterpersonalRelationship,
         # Collective Intelligence Variables
         CIF_T1, CIF_T2
         ) %>%
  mutate(
    PID         = PID,
    group       = gsub("[A-Z]+", "", PID),
    participant = gsub("^[0-9]+", "", PID),
    Female      = as.integer(GenderMvsF != 1),
    nonwhite    = as.integer(racenonwhite == 1),
    CIF_T1      = CIF_T1,
    CIF_T2      = CIF_T2,
    # Baseline data
    Age0        = as.integer(Age),
    GPA0        = (GPAclean - 1)*.5 + .25,
    RME0        = ZRMEscore,
    SI3Fac10    = SI3Fac1,
    SI3Fac20    = SI3Fac2,
    SI3Fac30    = SI3Fac3,
    FLAbRel0    = FLAbRel_NoOuts,
    # Re-mapped data
    AgeCat       = AgeCat,
    GPACat      = GPAcleanCat,
    RMECat       = as.integer(ZRMEscoreCat),
    SI3Fac1Cat   = as.integer(SI3Fac1Cat),
    SI3Fac2Cat   = as.integer(SI3Fac2Cat),
    SI3Fac3Cat   = as.integer(SI3Fac3Cat),
    FLAbRelCat   = as.integer(FLAbRel_NoOutsCat)
  ) %>%
  arrange(PID) %>%
  group_by(group) %>%
  mutate(size=n()) %>%
  ungroup

# Replacing categories by their average value
dat <- dat %>% group_by(AgeCat) %>% mutate(Age = mean(Age0)) %>% ungroup
dat <- dat %>% group_by(GPACat) %>% mutate(GPA = mean(GPA0)) %>% ungroup
dat <- dat %>% group_by(RMECat) %>% mutate(RME = mean(RME0)) %>% ungroup
dat <- dat %>% group_by(SI3Fac1Cat) %>% mutate(SI3Fac1 = mean(SI3Fac10)) %>% ungroup
dat <- dat %>% group_by(SI3Fac2Cat) %>% mutate(SI3Fac2 = mean(SI3Fac20)) %>% ungroup
dat <- dat %>% group_by(SI3Fac3Cat) %>% mutate(SI3Fac3 = mean(SI3Fac30)) %>% ungroup
dat <- dat %>% group_by(FLAbRelCat) %>% mutate(FLAbRel = mean(FLAbRel0)) %>% ungroup

saveRDS(dat, file = "data/nodal_data.rds", compress = FALSE)

# Summary statistics -----------------------------------------------------------

# Age
dat %>% group_by(Age) %>%
  summarise(
    Nobs = n(),
    Min  = min(Age0),
    Max  = max(Age0),
    Range = Max - Min
    ) %>% 
  readr::write_csv(path = "data/nodal_data_stats.csv")

dat %>% group_by(GPA) %>%
  summarise(
    Nobs = n(),
    Min  = min(GPA0),
    Max  = max(GPA0),
    Range = Max - Min
  ) %>% 
  readr::write_csv(path = "data/nodal_data_stats.csv", append = TRUE,
                   col_names = TRUE)

dat %>% group_by(RME) %>%
  summarise(
    Nobs = n(),
    Min  = min(RME0),
    Max  = max(RME0),
    Range = Max - Min
  ) %>% 
  readr::write_csv(path = "data/nodal_data_stats.csv", append = TRUE,
                   col_names = TRUE)

dat %>% group_by(SI3Fac1) %>%
  summarize(
    Nobs = n(),
    Min  = min(SI3Fac10),
    Max  = max(SI3Fac10),
    Range = Max - Min
  ) %>% 
  readr::write_csv(path = "data/nodal_data_stats.csv", append = TRUE,
                   col_names = TRUE)

dat %>% group_by(SI3Fac2) %>%
  summarize(
    Nobs = n(),
    Min  = min(SI3Fac20),
    Max  = max(SI3Fac20),
    Range = Max - Min
  ) %>% 
  readr::write_csv(path = "data/nodal_data_stats.csv", append = TRUE,
                   col_names = TRUE)

dat %>% group_by(SI3Fac3) %>%
  summarize(
    Nobs = n(),
    Min  = min(SI3Fac30),
    Max  = max(SI3Fac30),
    Range = Max - Min
  ) %>% 
  readr::write_csv(path = "data/nodal_data_stats.csv", append = TRUE,
                   col_names = TRUE)

dat %>% group_by(FLAbRel) %>%
  summarize(
    Nobs = n(),
    Min  = min(FLAbRel0),
    Max  = max(FLAbRel0),
    Range = Max - Min
  ) %>% 
  readr::write_csv(path = "data/nodal_data_stats.csv", append = TRUE,
                   col_names = TRUE)

