# Description:
# Calculate words' dimensions scores by using the Orientation towards Paradigm Words
# extrapolation method. This method predicts a word's dimension using that word’s
# similarity towards certain paradigm words (words commonly used to describe extreme
# values on these dimensions).

#
# Author: Julieta Laurino julilaurino@gmail.com
# Date: Dec 2021

require('tidyverse')
library('tictoc')
source('./semanticSimilarityAnalysisFunctions.R')


tic()
data.file_precovid           = './output/S_precovid_SWOW-RP.RW.A75.R123.csv'
data.file_covid              = './output/S_covid_SWOW-RP.RW.A75.R123.csv'

# Response frequencies for SWOW-EN R123

S_precovid              = read.csv(data.file_precovid, encoding = 'UTF-8')%>%
  rename(cue = X)
S_covid                 = read.csv(data.file_covid, encoding = 'UTF-8') %>%
  rename(cue = X)
targetCues  = read.csv('./targetCues.csv', encoding = 'UTF-8') 

# Orientation towards Paradigm Words method ----
# Create variables that contain the paradigm words

## For cepa
rating_1 = c('vid', 'planta')
rating_7 = c('organismo', 'virus')
cepa = data.frame(rating_1, rating_7)

## For inmunidad
rating_1 = c('parlamentario', 'diplomático')
rating_7 = c('resistencia', 'microorganismo')
inmunidad = data.frame(rating_1, rating_7)

## For ensayo
rating_1 = c('escrito', 'obra')
rating_7 = c('prueba', 'experimento')
ensayo = data.frame(rating_1, rating_7)


dimensions = c('ensayo','cepa', 'inmunidad')


# First approach: A target word’s final score as the sum of a word’s similarity towards
# rating_7 paradigm words minus the sum of its similarity towards rating_1 words.

for (dimension in dimensions) {
  scores_precovid = calculateDimensionScore(S_precovid, dimension) %>%
    mutate(cov.time = 'precovid')
  
  scores_covid = calculateDimensionScore(S_covid, dimension) %>%
    mutate(cov.time = 'covid')
  
  semantic_similarity_analysis = scores_precovid %>%
    bind_rows(scores_covid) %>%
    rename('{dimension}' := S_sum)
  
  assign(paste('semantic_similarity_analysis', dimension, sep = '_'),
         semantic_similarity_analysis)
  
}

# Join all dataframes

semantic_similarity_analysis = targetCues

for (dimension in dimensions) {
  semantic_similarity_analysis = semantic_similarity_analysis %>%
    full_join(get(paste('semantic_similarity_analysis', dimension, sep = '_'))) %>%
    #full_join(get(paste('semantic_similarity_analysis', dimension, 'k',sep = '_'))) %>%
    filter(complete.cases(paradigm_word_rating))
}

write.csv(semantic_similarity_analysis, './output/semanticSimilarityAnalysis_subset.SWOW-RP.csv', row.names = F)


# Permutation test----

# S between dimensions for cepa
d1_1 = cepa$rating_1[1]
d2_1 = cepa$rating_1[2]
d1_7 = cepa$rating_7[1]
d2_7 = cepa$rating_7[2]

s_precovid_1 = S_precovid[S_precovid$cue == d1_1, d2_1]
s_covid_1 = S_covid[S_covid$cue == d1_1, d2_1]
cepa_s_1 = mean(s_precovid_1, s_covid_1)

s_precovid_7 = S_precovid[S_precovid$cue == d1_7, d2_7]
s_covid_7 = S_covid[S_covid$cue == d1_7, d2_7]
cepa_s_7 = mean(s_precovid_7, s_covid_7)

# S between dimensions for inmunidad
d1_1 = inmunidad$rating_1[1]
d2_1 = inmunidad$rating_1[2]
d1_7 = inmunidad$rating_7[1]
d2_7 = inmunidad$rating_7[2]

s_precovid_1 = S_precovid[S_precovid$cue == d1_1, d2_1]
s_covid_1 = S_covid[S_covid$cue == d1_1, d2_1]
inmunidad_s_1 = mean(s_precovid_1, s_covid_1)

s_precovid_7 = S_precovid[S_precovid$cue == d1_7, d2_7]
s_covid_7 = S_covid[S_covid$cue == d1_7, d2_7]
inmunidad_s_7 = mean(s_precovid_7, s_covid_7)

# S between dimensions for ensayo
d1_1 = ensayo$rating_1[1]
d2_1 = ensayo$rating_1[2]
d1_7 = ensayo$rating_7[1]
d2_7 = ensayo$rating_7[2]

s_precovid_1 = S_precovid[S_precovid$cue == d1_1, d2_1]
s_covid_1 = S_covid[S_covid$cue == d1_1, d2_1]
ensayo_s_1 = mean(s_precovid_1, s_covid_1)

s_precovid_7 = S_precovid[S_precovid$cue == d1_7, d2_7]
s_covid_7 = S_covid[S_covid$cue == d1_7, d2_7]
ensayo_s_7 = mean(s_precovid_7, s_covid_7)

sim_cue = list(cepa = c(cepa_s_1, cepa_s_7),
               inmunidad = c(inmunidad_s_1, inmunidad_s_7),
               ensayo = c(ensayo_s_1, ensayo_s_7))


n = 1000
not_include = c(setdiff(S_precovid$cue, S_covid$cue), setdiff(S_covid$cue, S_precovid$cue), dimensions)

tictoc::tic()
set.seed(46)
for (dimension in dimensions){
  random_pairs_rating_1 = createRandomPairs(dimension,
                                            rating = 1,
                                            data = S_precovid,
                                            n,
                                            not_include)
  assign(paste0('random_pairs_', dimension, '_rating_1'), random_pairs_rating_1)
  random_pairs_rating_7 =createRandomPairs(dimension,
                                           rating = 2,
                                           data = S_covid,
                                           n,
                                           not_include)
  assign(paste0('random_pairs_', dimension, '_rating_7'), random_pairs_rating_7)
}

tictoc::toc()

tictoc::tic()
for (dimension in dimensions) {
  permutation_list_1 = get(paste0('random_pairs_', dimension, '_rating_', 1))
  permutation_list_7 = get(paste0('random_pairs_', dimension, '_rating_', 7))
  semantic_similarity_analysis_permutation = data.frame()
  for (i in 1:n){
    rating_1 = permutation_list_1[[i]]
    rating_7 = permutation_list_7[[i]]
    assign(dimension, data.frame(rating_1 = rating_1, rating_7 = rating_7))
    scores_precovid = calculateDimensionScore(S_precovid, dimension) %>%
      mutate(cov.time = 'precovid')
    
    scores_covid = calculateDimensionScore(S_covid, dimension) %>%
      mutate(cov.time = 'covid')
    
    semantic_similarity_analysis = scores_precovid %>%
      bind_rows(scores_covid) %>%
      rename('{dimension}' := S_sum) %>% 
      mutate(permutation_n = i)
    
    semantic_similarity_analysis_permutation = semantic_similarity_analysis_permutation %>% 
      bind_rows(semantic_similarity_analysis)
    print(paste0(i, '-', dimension))
    
  }
  assign(paste('semantic_similarity_analysis_permutation', dimension, sep = '_'),
         semantic_similarity_analysis_permutation)
  
}
tictoc::toc()

# Join all dataframes

semantic_similarity_analysis_permutation = semantic_similarity_analysis_permutation_ensayo %>% 
  full_join(semantic_similarity_analysis_permutation_cepa) %>% 
  full_join(semantic_similarity_analysis_permutation_inmunidad)

write.csv(semantic_similarity_analysis_permutation, './output/semanticSimilarityAnalysis_permutation2_dimensions.SWOW-RP.csv', row.names = F)

# Save random pairs
random_pairs_cepa = list(rating_1 = get('random_pairs_cepa_rating_1'),
                         rating_7 = get('random_pairs_cepa_rating_7'))

random_pairs_ensayo = list(rating_1 = get('random_pairs_ensayo_rating_1'),
                         rating_7 = get('random_pairs_ensayo_rating_7'))

random_pairs_inmunidad = list(rating_1 = get('random_pairs_inmunidad_rating_1'),
                         rating_7 = get('random_pairs_inmunidad_rating_7'))

random_pairs = list(cepa = random_pairs_cepa,
                    ensayo = random_pairs_ensayo,
                    inmunidad = random_pairs_inmunidad)
saveRDS(random_pairs, file="./output/permutation_random_pairs.RData")

# nns_ratio----

k = 20
#dimensions = c('alcohol','cepa', 'inmunidad')

nns_ratio = data.frame(nn = character(),
                       ratio = integer(),
                       dimension = character(),
                       nn_id = character())
for (dimension in dimensions) {
  nn_precovid <- getKNeighbors(S_precovid, dimension, k)
  nn_covid <- getKNeighbors(S_covid, dimension, k)
  union_nns <- union(nn_precovid, nn_covid)
  union_nns <- union_nns[union_nns %in% names(S_precovid) & union_nns %in% names(S_covid)]
  s_precovid <- S_precovid %>% filter(cue == dimension) %>% select(any_of(union_nns))
  s_covid <- S_covid %>% filter(cue == dimension) %>% select(any_of(union_nns))
  this_nns_ratio <- s_covid / s_precovid
  this_nns_ratio <- this_nns_ratio %>%
    pivot_longer(names_to = 'nn',
                 values_to = 'ratio',
                 cols = everything()) %>%
    mutate(dimension = dimension,
           nn_id = as.character(row_number()),
           group = ifelse(nn %in% nn_precovid & nn %in% nn_covid, 'shared',
                          ifelse(nn %in% nn_precovid & !(nn %in% nn_covid), 'precovid',
                                 'covid')))
  nns_ratio <- bind_rows(nns_ratio, this_nns_ratio)
}

toc()

#write.csv(nns_ratio, './output/nns_ratio.SWOWES-UY.csv', row.names = F)
write.csv(nns_ratio, './output/nns_ratio.SWOW-RP.csv', row.names = F)
# 
# 
