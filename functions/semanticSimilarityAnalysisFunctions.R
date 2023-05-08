# permutations <- function(dimensions, n){
#   rating = list()
#   for(i in 1:n){
#     sample = sample(dimensions, 4, FALSE)
#     rating[[i]]= list(rating_1=sample[1:2],
#                       rating_7=sample[3:4])
#   }
#   return(rating)
# }

createRandomPairs <- function(dimension, rating, data, n, not_include){
  random_pairs = list()
  while (length(random_pairs) < n){ 
    row = sample(1:nrow(data),1, replace = FALSE)
    col = sample(2:ncol(data),1, replace = FALSE)
    s <- data[row,col]
    if (s > sim_cue[[dimension]][rating]-0.05 & s < sim_cue[[dimension]][rating]+0.05) {
      w1 <- data[row, 'cue']
      w2 <- names(data)[col]
      if (!(w1 %in% not_include) & !(w1 %in% not_include)){
        random_pairs <- append(random_pairs, list(c(w1,w2)))}
    }
    print(paste0(length(random_pairs), '--- ', dimension, '_rating_', rating))
  }
  return(random_pairs)
}

calculateDimensionScore <- function(similarity_matrix, dimension_name) {
  dimension = get(dimension_name)
  rating_1_k = dimension$rating_1
  rating_7_k = dimension$rating_7
  dimension_scores = similarity_matrix %>%
    filter(cue == dimension_name) %>%
    select(cue, any_of(rating_1_k) | any_of(rating_7_k)) %>%
    pivot_longer(names_to = 'paradigm_word',
                 values_to = 'S',
                 cols = -('cue')) %>%
    filter(!(cue %in% rating_1_k) & !(cue %in% rating_7_k)) %>%
    mutate(paradigm_word_rating = ifelse(paradigm_word %in% rating_1_k, 'rating_1_k', 'rating_7_k')) %>%
    group_by(cue, paradigm_word_rating) %>%
    summarise(S_sum = sum(S)) 
  
  # dimension_scores_paradigm_words = similarity_matrix %>%
  #   select(cue, any_of(rating_1_k) | any_of(rating_7_k)) %>%
  #   pivot_longer(names_to = 'paradigm_word',
  #                values_to = 'S',
  #                cols = -('cue')) %>%
  #   filter(cue %in% rating_1_k | cue %in% rating_7_k) %>%
  #   mutate(paradigm_word_rating = ifelse(paradigm_word %in% rating_1_k, 'rating_1_k', 'rating_7_k')) %>%
  #   group_by(cue, paradigm_word_rating) %>%
  #   summarise(S_sum = sum(S)) 
  # 
  # dimension_scores = dimension_scores %>% 
  #   bind_rows(dimension_scores_paradigm_words)
  
  return(dimension_scores)
}


getKNeighbors <- function(similarity_matrix, paradigm_words, k) {
  # Returns a vector with the k neighbors of the paradigm words.
  data_nb = c()
  for (word in paradigm_words) {
    this_nb = similarity_matrix %>%
      filter(cue == word) %>%
      pivot_longer(names_to = 'neighbor',
                   values_to = 'S',
                   cols = -(cue)) %>%
      filter(cue != neighbor) %>%
      top_n(k, S)
    data_nb = c(data_nb, this_nb$neighbor)
  }
  
  return(data_nb)
  
}



