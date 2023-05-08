# Compute cue-level statistics either on the cue-by-response or
# the cue-by-cue matrix
#
# This script computes:
# 1. % known cues
# 2. % Missing responses (for R2 or R3)
# 2. Total number of cues
# 3. Coverage of cues (i.e. how many of the responses are also cues) based on R1 or R123
# 4. Entropy of the responses given the cues: E(R|C)
#
# Author: Simon De Deyne simon2d@gmail.com
# Date: 2 October 2021

# Adaptation for RP COVID data: Julieta Laurino, julilaurino@gmail.com

require(tidyverse)
require(Matrix)
require(pbapply)
require(entropy)

source('settings.R', encoding = 'UTF-8')
#results = list()
source('../functions/importDataFunctions.R')
source('../functions/networkFunctions.R')

file.swow = paste0('../data/SWOW/processed/SWOW-RP.R60.targetCues.csv')
file.R1  = paste0('../data/SWOW/output/cueStats.SWOW-RP.R1.targetCues.csv')
file.R123 = paste0('../data/SWOW/output/cueStats.SWOW-RP.R123.targetCues.csv')

## SWOW-R1 statistics
X.R1              = importDataSWOW(file.swow,'R1')

X.R1_precovid = X.R1 %>%
  filter(cov.time == 'precovid')
X.R1_covid = X.R1 %>%
  filter(cov.time == 'covid')

# Calculate entropy H
message('Calculating entropy R1')

H.R1_precovid              = getEntropy(X.R1_precovid,'cues')%>%
  mutate(cov.time = 'precovid')
H.R1_covid              = getEntropy(X.R1_covid,'cues') %>%
  mutate(cov.time = 'covid')

H.R1 = bind_rows(H.R1_precovid, H.R1_covid)

#Normalize
H.R1 = H.R1 %>%
  left_join(X.R1) %>%
  select(cue, H, cov.time, cue.type, n) %>%
  group_by(cue, cov.time, cue.type, n) %>%
  summarise(H = mean(H)) %>%
  mutate(H_norm = H / log(n))

#KL divergence
KL.R1 = X.R1 %>% 
  group_by(cue, response, cov.time, cue.type) %>% 
  count() %>% 
  pivot_wider(names_from = cov.time, values_from = n) %>% 
  mutate(covid = ifelse(is.na(covid), 0, covid),
         precovid = ifelse(is.na(precovid), 0, precovid)) %>% 
  pivot_longer(cols = c(covid, precovid), names_to = 'cov.time', values_to = 'n') %>% 
  filter(complete.cases(response)) %>% 
  ungroup() %>% 
  select(-c(cue.type)) %>% 
  pivot_wider(names_from = response, values_from = n)

KL.R1_results <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(KL.R1_results) <- c('cue', 'KL')

library(philentropy)
for (this_cue in unique(KL.R1$cue)) {
  df = filter(KL.R1, cue == this_cue) 
  df = df[ , colSums(is.na(df)) < nrow(df)]
  df = data.frame(df) %>% mutate(cue = paste(cue, cov.time, sep = '_')) %>% 
    select(-cov.time) 
  rownames(df) <- df[,1]
  df = df %>% select(-cue)
  kl = KL(data.matrix(df), unit = 'log', est.prob = "empirical")
  kl_list = list('cue' = this_cue, 'KL' = kl)
  KL.R1_results = rbind(KL.R1_results, kl_list)
  
}

H.R1 = H.R1 %>% 
  select(-c(H, n)) %>% 
  pivot_wider(names_from = cov.time, values_from = H_norm) %>% 
  rename('H_norm_precovid' = precovid, 'H_norm_covid' = covid) %>% 
  left_join(KL.R1_results)

# 
write.csv(H.R1, file = file.R1, row.names = FALSE, fileEncoding = 'UTF-8')
# 
# #H.R1.check = H.R1 %>% 
#  group_by(cue) %>% 
#  count()


# 
# results$H$R1$mean         = mean(H.R1$H)
# results$H$R1$sd           = sd(H.R1$H)
# results$H$R1$min          = min(H.R1$H)
# results$H$R1$max          = max(H.R1$H)
# results$H$R1$min_examples = H.R1 %>% top_n(-10,H)
# results$H$R1$max_examples = H.R1 %>% top_n(10,H)


# Calculate unknown
#xR1               = X.R1 %>% group_by(cue) %>% summarise(unknown = sum(is.na(response)))
#cueStats.R1       = as.data.frame(left_join(coverage.R1,H.R1,by = 'cue') %>% left_join(.,xR1,by = 'cue') %>% left_join(.,Cues.N,by = 'cue'))


## SWOW-R123 statistics
X.R123    = importDataSWOW(file.swow,'R123') 

X.R123_precovid = X.R123 %>% 
  filter(cov.time == 'precovid')
X.R123_covid = X.R123 %>% 
  filter(cov.time == 'covid')

# # Remove cues not in the component
# X.R123      = X.R123 %>% filter(!cue %in% components$R123$removeVertices)


# # Calculate coverage
# Cues.known        = X.R123 %>% filter(complete.cases(response)) %>% group_by(cue) %>% summarise(cue.Tokens = n())
# Cues.N            = X.R123 %>% group_by(cue) %>% summarise(N = n())
# Cues.covered      = X.R123 %>% filter(response %in% cue) %>% group_by(cue) %>% summarise(cue.Covered = n())
# coverage.R123     = left_join(Cues.known,Cues.covered, by = 'cue') %>% mutate(coverage = cue.Covered / cue.Tokens  * 100) %>% select(cue,coverage)
# 
# results$coverage$R123$mean = mean(coverage.R123$coverage)
# results$coverage$R123$median = median(coverage.R123$coverage)
# results$coverage$R123$sd   = sd(coverage.R123$coverage)
# results$coverage$R123$min  = min(coverage.R123$coverage)
# results$coverage$R123$max  = max(coverage.R123$coverage)
# results$coverage$R123min_examples = coverage.R123 %>% top_n(-10,coverage)


# Calculate entropy H
message('Calculating entropy R123')

H.R123_precovid              = getEntropy(X.R123_precovid,'cues') %>% 
  mutate(cov.time = 'precovid')
H.R123_covid              = getEntropy(X.R123_covid,'cues') %>% 
  mutate(cov.time = 'covid')

H.R123 = bind_rows(H.R123_precovid, H.R123_covid)

H.R123 = H.R123 %>% 
  left_join(X.R123) %>% 
  select(cue, H, cov.time, cue.type, n) %>% 
  group_by(cue, cov.time, cue.type, n) %>% 
  summarise(H = mean(H)) %>% 
  mutate(H_norm = H / log(n))


#KL divergence
KL.R123 = X.R123 %>%
  group_by(cue, response, cov.time, cue.type) %>% 
  count() %>% 
  pivot_wider(names_from = cov.time, values_from = n) %>% 
  mutate(covid = ifelse(is.na(covid), 0, covid),
         precovid = ifelse(is.na(precovid), 0, precovid)) %>% 
  pivot_longer(cols = c(covid, precovid), names_to = 'cov.time', values_to = 'n') %>% 
  filter(complete.cases(response)) %>% 
  ungroup() %>% 
  select(-c(cue.type)) %>% 
  pivot_wider(names_from = response, values_from = n)

KL.R123_results <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(KL.R123_results) <- c('cue', 'KL')

library(philentropy)
for (this_cue in unique(KL.R123$cue)) {
    df = filter(KL.R123, cue == this_cue) 
    df = df[ , colSums(is.na(df)) < nrow(df)]
    df = data.frame(df) %>% mutate(cue = paste(cue, cov.time, sep = '_')) %>% 
      select(-cov.time) 
    rownames(df) <- df[,1]
    df = df %>% select(-c(cue))
    kl = KL(data.matrix(df), unit = 'log', est.prob = "empirical")
    kl_list = list('cue' = this_cue, 'KL' = kl)
    KL.R123_results = rbind(KL.R123_results, kl_list)

}

beepr::beep()

H.R123 = H.R123 %>% 
  select(-c(H, n)) %>% 
  pivot_wider(names_from = cov.time, values_from = H_norm) %>% 
  rename('H_norm_precovid' = precovid, 'H_norm_covid' = covid) %>% 
  left_join(KL.R123_results)

write.csv(H.R123, file = file.R123, row.names = FALSE, fileEncoding = 'UTF-8')

# H.R123.check = H.R123 %>%
#   group_by(cue, cov.time) %>%
#   count()

#H.R123$cue       = as.character(H.R123$cue)

# results$H$R123$mean         = mean(H.R123$H)
# results$H$R123$sd           = sd(H.R123$H)
# results$H$R123$min          = min(H.R123$H)
# results$H$R123$max          = max(H.R123$H)
# results$H$R123$min_examples = H.R123 %>% top_n(-10,H)
# results$H$R123$max_examples = H.R123 %>% top_n(10,H)


# Calculate unknown
#xR1   = X.R123 %>% group_by(cue) %>% summarise(unknown = sum(is.na(response[RPOS=='R1'])))

# Calculate missing (R2,R3)
#xR2               = X.R123 %>% group_by(cue) %>% summarise(xR2 = sum(is.na(response[RPOS=='R2'])))
#xR2$xR2           = xR2$xR2 - xR1$unknown
#xR3               = X.R123 %>% group_by(cue) %>% summarise(xR3 = sum(is.na(response[RPOS=='R3'])))
#xR3$xR3           = xR3$xR3 - xR2$xR2 - xR1$unknown

#cueStats.R123       = as.data.frame(left_join(coverage.R123,H.R123,by = 'cue') %>% left_join(.,xR1,by = 'cue') %>%
#                      left_join(.,xR2, by = 'cue') %>% left_join(.,xR3, by = 'cue') %>% left_join(.,Cues.N, by = 'cue'))

#message('Percentage unknown: ', round(mean(cueStats.R1$unknown),1),', range [',min(cueStats.R1$unknown),',', max(cueStats.R1$unknown),']')
#message('Percentage R2 missing: ', round(mean(cueStats.R123$xR2),1),', range [',min(cueStats.R123$xR2),',', max(cueStats.R123$xR2),']')
#message('Percentage R3 missing: ', round(mean(cueStats.R123$xR3),1),', range [',min(cueStats.R123$xR3),',', max(cueStats.R123$xR3),']')

#results$unknown$mean    = mean(cueStats.R1$unknown)
#results$unknown$min     = min(cueStats.R1$unknown)
#results$unknown$max     = max(cueStats.R1$unknown)

#results$R2missing$mean  = mean(cueStats.R123$xR2)
#results$R2missing$min   = min(cueStats.R123$xR2)
#results$R2missing$max   = max(cueStats.R123$xR2)

#results$R3missing$mean  = mean(cueStats.R123$xR3)
#results$R3missing$min   = min(cueStats.R123$xR3)
#results$R3missing$max   = max(cueStats.R123$xR3)

# Write the results to file
#write.csv(cueStats.R1,file = results.file.SWOWES_UY.R1)
#write.csv(cueStats.R123,file = results.file.SWOWES_UY.R123)

# Write a summary of the output to an rds file
#saveRDS(results,report.file,ascii=TRUE)
