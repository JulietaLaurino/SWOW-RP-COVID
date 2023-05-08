require(tidyverse)

# Create the targetCues.csv file including all cue types and properties ----
cuesProperties = read.table('../data/SWOW/raw/cuesProperties.txt', 
                            encoding = 'UTF-8', 
                            fill = TRUE, 
                            header = TRUE) %>% 
  rename(cue = 'X.U.FEFF.word')

names <-  c('pandemic', 'emotion', 'routine', 'control')
files <-  paste('../data/SWOW/raw/', names, 'Cues.csv', sep='')
           

for(i in 1:length(files)){
  file <-  read.csv(files[i], encoding = "UTF-8")
  file <-  file %>% 
    distinct() %>% 
    mutate(cue.type = names[i]) 
    assign(names[i] , file)
}


targetCues <- bind_rows(pandemic, emotion, routine, control) 
write.csv(targetCues, '../data/SWOW/raw/targetCues.csv', row.names = F, fileEncoding = 'UTF-8')

# Summary of properties for each cue type ----
## (these are not the final target cues 
## included, after the pre-proccesing step, some cues are discarded because of 
## lack of responses or differences between ARG-URU usage)

cuesPropertiesTargetCues = targetCues %>% left_join(cuesProperties)
cuesPropertiesByType = cuesPropertiesTargetCues %>% 
  group_by(cue.type) %>% 
  summarise(frq = mean(frq, na.rm = TRUE),
            img = mean(imageability, na.rm = TRUE),
            fam = mean(familiarity, na.rm = TRUE),
            con = mean(concreteness, na.rm = TRUE),
            n = n())

write.csv(cuesPropertiesByType, '../data/SWOW/raw/cuesPropertiesByType.csv', row.names = F)

# Final targetCues (after balance participants step) ----

###### Responses for target cues
file.data           = '../data/SWOW/processed/SWOW-RP.R60.targetCues.csv'

# Response frequencies for SWOW-EN R123
response            = 'R123'
SWOWES              = read.csv(file.data, stringsAsFactors = FALSE,encoding = 'UTF-8') %>% 
  as_tibble()

targetCues_final = SWOWES %>% 
  group_by(cue, cue.type) %>% 
  count()

cuesPropertiesByType_final = targetCues_final %>% 
  left_join(cuesProperties) %>% 
  group_by(cue.type) %>% 
  summarise(frq = mean(frq, na.rm = TRUE),
            fam = mean(familiarity, na.rm = TRUE),
            imag = mean(imageability, na.rm = TRUE),
            concr = mean(concreteness, na.rm = TRUE),
            n = n())

write.csv(cuesPropertiesByType_final, '../data/SWOW/raw/cuesPropertiesByType_final.csv', row.names = F)

