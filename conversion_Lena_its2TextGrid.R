library(rlena) # see package homepage for installation instructions: https://github.com/HomeBankCode/rlena
library(tidyverse)
library(rPraat)
library(lubridate)

# dossier dans lequel le textgrid généré est enregistré
# si NULL, même dossier que le fichier .its
outDir <- NULL
# liste des segments cibles
# si NULL, on les garde tous
cibles <- NULL
#cibles <- c("CHN","CXN","FAN","MAN")

# sélection du fichier .its
fullPathITS <- file.choose()

# lecture du fichier .its
donneeslena <- read_its_file(fullPathITS)

# construction du chemin du fichier .TextGrid
pathITS <- dirname(fullPathITS)
fichierITS <- basename(fullPathITS)
fichierITSnoExt <- str_replace(fichierITS, "\\.its$", "")
if(is.null(outDir)) {
  outDir <- pathITS
}
fullPathTG <- str_c(outDir, .Platform$file.sep, fichierITSnoExt, ".TextGrid")

# enregistrements
recordings <- gather_recordings(donneeslena)
# blocks = conversations + pauses
blocks <- gather_blocks(donneeslena)
# segments (CHN,MAN,FAN etc...)
segments <- gather_segments(donneeslena)
# filtrage des segments cibles
if(!is.null(cibles)) {
  segments <- segments %>% 
    filter(blkType=="Conversation" & spkr %in% cibles)
}

# regroupement par heure ? partir du niveau le plus fin (segments)
segments <- segments %>%
  mutate(
    endTimeDay = lubridate::day(endClockTimeLocal),
    endTimeMonth = lubridate::month(endClockTimeLocal),
    endTimeHour = lubridate::hour(endClockTimeLocal),
    endTimeMinute = lubridate::minute(endClockTimeLocal)
  )
hours <- segments %>%
  group_by(endTimeDay, endTimeMonth, endTimeHour) %>% 
  arrange(endClockTimeLocal) %>% 
  summarise(
    startTime = first(startTime),
    endTime = last(endTime),
    .groups = "drop"
  )
days <- hours %>%
  group_by(endTimeDay, endTimeMonth) %>% 
  arrange(startTime) %>% 
  summarise(
    startTime = first(startTime),
    endTime = last(endTime),
    .groups = "drop"
  ) %>% 
  mutate(
    formattedDate = str_c(endTimeDay, "-", str_pad(endTimeMonth, 2, side = "left", pad = "0"))
  )

minutes <- segments %>%
  group_by(endTimeDay, endTimeMonth, endTimeHour, endTimeMinute) %>% 
  arrange(endClockTimeLocal) %>% 
  summarise(
    startTime = first(startTime),
    endTime = last(endTime),
    .groups = "drop"
  )

# export dans un TextGrid
tMin <- min(recordings %>% pull(startTime))
tMax <- max(recordings %>% pull(endTime))
tg <- tg.createNewTextGrid(tMin, tMax)

# tier segments
tg <- tg.insertNewIntervalTier(tg, newInd = 1, newTierName = "segments")
tg$segments$t1 <- segments %>% pull(startTime)
tg$segments$t2 <- segments %>% pull(endTime)
tg$segments$label <- segments %>% pull(spkr)

# tier blocks
tg <- tg.insertNewIntervalTier(tg, newInd = 2, newTierName = "blocks")
tg$blocks$t1 <- blocks %>% pull(startTime)
tg$blocks$t2 <- blocks %>% pull(endTime)
tg$blocks$label <- blocks %>% pull(blkType)

# tier recordings
tg <- tg.insertNewIntervalTier(tg, newInd = 3, newTierName = "recordings")
tg$recordings$t1 <- recordings %>% pull(startTime)
tg$recordings$t2 <- recordings %>% pull(endTime)
tg$recordings$label <- str_c(recordings %>% pull(startClockTimeLocal), " ", recordings %>% pull(timeZone))

# tier days
tg <- tg.insertNewIntervalTier(tg, newInd = 4, newTierName = "days")
tg$days$t1 <- days %>% pull(startTime)
tg$days$t2 <- days %>% pull(endTime)
tg$days$label <- days %>% pull(formattedDate)

# tier hours
tg <- tg.insertNewIntervalTier(tg, newInd = 5, newTierName = "hours")
tg$hours$t1 <- hours %>% pull(startTime)
tg$hours$t2 <- hours %>% pull(endTime)
tg$hours$label <- as.character(hours %>% pull(endTimeHour))

# tier minutes
tg <- tg.insertNewIntervalTier(tg, newInd = 6, newTierName = "minutes")
tg$minutes$t1 <- minutes %>% pull(startTime)
tg$minutes$t2 <- minutes %>% pull(endTime)
tg$minutes$label <- as.character(minutes %>% pull(endTimeMinute))

# enregistrement textgrid
tg.write(tg, fullPathTG, format = "text")

# bilan des durées des segments
segments <- segments %>% 
  mutate(duree=endTime-startTime)
bilandurees <- segments %>% 
  group_by(blkType,spkr) %>% 
  summarise(
    tpscumule=sum(duree),
    nseg=n(),
    .groups = "drop"
  )
