# Balance an equal sample of participants 
# TODO: verify sampling rules
#
# Author: Simon De Deyne & Alvaro Cabana
# Last modified 18 October 2021

# Adaptation for SWOW-RP-COVID data: Julieta Laurino, julilaurino@gmail.com

library(tidyverse)
library(hunspell)
source('./settings.R', encoding = 'UTF-8')
source('../functions/preprocessing.R')

file.output = paste0('../data/SWOW/processed/SWOW-RP.R60_covid.csv')
file.output.targetCues = '../data/SWOW/processed/SWOW-RP.R60.targetCues.csv'
file.pp = paste0('../data/SWOW/processed/SWOW-RP.PP_covid.csv')

file.swow = paste0('../data/SWOW/processed/SWOW-RP.spellchecked_covid.csv')
file.targetCues = paste0('../data/SWOW/raw/targetCues.csv')

X = read.csv(file.swow, stringsAsFactors = FALSE,encoding = 'UTF-8') %>% 
  as_tibble()

X.cues = X %>% group_by(cue) %>% tally()

# Convert to long format to facilitate further processing
result = list()
result$X = X %>% ungroup %>% 
  pivot_longer(names_to = 'RPOS',
               cols = c('R1','R2','R3'),
               values_to = 'response')


# Score participants

## Missing and unknown responses
PP = X %>% group_by(participantID,nativeLanguage,gender,age,education) %>%
            summarise(N = n()*3,
                unknown = sum(is.na(R1)*3),
                missingR2 = sum(is.na(R2) & !is.na(R1)),
                missingR3 = sum(is.na(R3) & !is.na(R2)),.groups = 'drop') %>%
        mutate(prop.Unknown = unknown / N, prop.X = (2*missingR2 + missingR3) / N)


## Add Distinct items
PP = left_join(PP,result$X %>% 
                 group_by(participantID) %>%
                  summarise(nTypes = sum(n_distinct(response,na.rm = T))),
               by = 'participantID')

## Count repeated responses: Total number non-missing minus unique responses
PP = PP %>% mutate(prop.Repeat = 
                     ((N - unknown - 2*missingR2 - missingR3)- nTypes)/N)

## Responses in lexicon
# Calculate presence of response in wordlist # OR # Hunspell dictionary
file.lexicon = '../data/dictionaries/wordlist.txt'
X.lexicon  = read.csv(file.lexicon, stringsAsFactors = FALSE, 
                      strip.white = TRUE, encoding = 'UTF-8') %>% as_tibble()
dictionary.file = '../data/dictionaries/Spanish_Rioplatense.dic'
X.dict = dictionary(dictionary.file)
result$X = result$X %>%  mutate(inLexicon = 
                                  ifelse(response %in% X.lexicon$Word,1,
                                         ifelse(is.na(response),NA,0)))
result$X = result$X %>% mutate(inDict = ifelse(inLexicon==0,
                                    hunspell_check(response,dict=X.dict),1)) %>%
                        mutate(inLexicon = inDict) %>% select(-inDict)

# Proportion Rioplatense responses
PP = left_join(PP,result$X %>% group_by(participantID) %>% 
  summarise(prop.Rioplatense = 
              sum(inLexicon,na.rm = TRUE)/ sum(!is.na(response))), 
  by = 'participantID')

# Proportion ngrams (n > 1)
result$X = result$X %>% mutate(nGram  = 
                        ifelse(!is.na(response),str_count(response,' '),NA))

PP = left_join(PP,result$X %>% group_by(participantID) %>% 
  summarise(prop.nGram = sum(nGram>1,na.rm=TRUE) / sum(!is.na(response))),
  by = 'participantID')


PP = PP %>% mutate(prop.X = (unknown + missingR2 + missingR3)/N)


message('Percentage of responses in lexicon: ',  
    round(100*(sum(result$X %>% filter(complete.cases(.)) %>% 
                     pull(inLexicon)) / result$X %>% 
                     filter(complete.cases(.)) %>% nrow()),2), '%')

PP = PP %>% mutate(status = 
              ifelse(prop.X >  criteria.X, 'X',
              ifelse(prop.Rioplatense < criteria.Spanish,'Non-native',
              ifelse(prop.Repeat > criteria.Repeat, 'Perseveration',
              ifelse(prop.nGram > criteria.Ngram, 'Verbose',
              ifelse(age < 16,'Underage','Valid'))))))

## Calculate the breakdown of valid and removed participants
status = PP %>% group_by(status) %>% tally() %>% mutate(prop = n/sum(n))

# Filter valid responses in X
X           = X %>% filter(participantID %in% 
                             PP$participantID[PP$status=='Valid'])

X %>% group_by(cue) %>% tally() %>% filter(n >= 60) %>% nrow()

badPP = PP %>% filter(!status=='Valid')
write.csv(badPP,paste0('../data/SWOW/processed/filteredParticipants_covid.csv'),
          row.names = F)

# Too many non-native participants!
X.nonnative = inner_join(result$X,PP %>% filter(status == 'Non-native') %>% select(participantID), by = 'participantID')
write.csv(X.nonnative,paste0('../data/SWOW/processed/nonnativeData_covid.csv'),
          row.names = F)


# Select 60 responses per row, by considering first: 
# native Uruguayan-Argentinian Rioplatense, then Argentinian other
# next by considering the date (most recent first), but always ordered by 
# participants to include complete response sets at the participant level 
# where possible

# Add a selection variable to favor native speakers
X = X %>% mutate(native = ifelse(nativeLanguage %in% c('URU_R',"ARG_R"), 100,
                                 ifelse(nativeLanguage %in% c('ARG_N','ARG_C'),1,0)),
                 maleOtherGender = ifelse(gender %in% c("X","Ma"),3,1)) %>%
  arrange(participantID)

# (Add a selection variable to favor more male speakers)
X = X %>% mutate(sampleKey = native + participantID)

# Select 120 participants only for target cues (60 precovid and 60 covid). 
targetCues = read.csv(file.targetCues, stringsAsFactors = FALSE,encoding = 'UTF-8') %>% 
  as_tibble()

X.targetCues     = left_join(X, targetCues) %>% 
  filter(complete.cases(cue.type)) %>%
  group_by(cue, cov.time) %>%
  slice_sample(n = 60, weight_by = native+1) 

targetCuesExcluded = X.targetCues %>% 
  group_by(cue) %>% 
  count() %>% 
  filter(n < 120) %>% 
  left_join(targetCues)

X.targetCues = X.targetCues %>% 
  filter(!(cue %in% targetCuesExcluded$cue))

write.csv(X.targetCues,file.output.targetCues, row.names = F, fileEncoding = 'UTF-8')

# This needs to be checked
X = X %>% mutate(cue = as.factor(cue))

#X %>% group_by(cue) %>% tally() %>% filter(n < 60) %>% nrow()
#X %>% group_by(cue) %>% tally() %>% filter(n < 60) %>% nrow()

X.leftCues = X %>%
  filter(!(cue %in% targetCues$cue)) %>% 
  group_by(cue) %>% 
  filter(n() > 60) %>% 
  slice_sample(n = 60, weight_by = native+1) 

X = X.targetCues %>% 
  bind_rows(X.leftCues)

# Write the dataset with 60 responses per cue
write.csv(X,file.output,row.names = F, fileEncoding = 'UTF-8')
write.csv(PP,file.pp,row.names = F)

beepr::beep()
