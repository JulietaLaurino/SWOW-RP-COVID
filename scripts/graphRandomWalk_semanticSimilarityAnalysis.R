## Spreading activation using Katz walks
# The following script computes a random walk similar to the Katz
# centrality procedure (see for example Newman (2010) "Networks An Introduction"
# to provide a simple implementation of spreading activation.
# It depends on a single paramter alpha, which determines the contribution
# of longer paths (values of alpha should be > 0 and < 1).
#
# It will create a path-augmented graph G.rw, which include weighted sum
# of all potential paths. This new graph can then be used to derive
# similarities from.  Pointwise mutual information is used to weigh
# associative response frequency.
# For more information see De Deyne, Navarro, Perfors &  Storms (2018).
#
# Input:
# The input should be an adjacency file formatted as i j f, where i and j
# refer to a cue and response coordinate, and f to the frequency of
# response j to cue i
# In addition, a file with labels should also be provided where the labels
# correspond to the indices i and j in the adjacency file
#
# Typically the graph corresponding to the adjacency matrix is restricted
# to the largest strongly connected component and loops are removed
# Output:
# G.rw: graph with indirect paths, renormalized and ppmi weighted
# S.rw: dense similarity matrix for the graph.
#
# Notes:
# Alpha. Throughout most experiments alpha = .75 performs reasonably well.
# To control degrees of freedom this has been taken as a default.
#
# PPMI. PPMI is known to have a bias for rare events, which does not affect
# typical word associations graphs with n < 12,000 words, but becomes
# a concern for larger graphs (Turney & Pantel, 2010).
# In such cases, weighted PPMI versions can be considered (see for example
# Levy,Goldberg & Dagan, 2015, p 215)
#
# S.rw: calculating the cosine similarity for all possible pairwise combinations
# is memory intensive, only consider doing this when your system has
# sufficient RAM. Otherwise, consider multiplying vectors instead.
#
# Total processing time on an i7 with 32Gb is about 96 seconds.
#
# References:
# De Deyne, S., Navarro, D., Perfors, A., Storms, G. (2016). Structure at
# every scale: A semantic network account of the similarities between
# unrelated concepts. Journal of Experimental Psychology.
# General, 145, 1228-1254.
#
# Levy, O., Goldberg, Y., & Dagan, I. (2015). Improving distributional
# similarity with lessons learned from word embeddings. Transactions of the
# Association for Computational Linguistics, 3, 211-225.
#
# Newman, M. (2010). Networks: an introduction. Oxford university press.
#
# Turney, P. D., & Pantel, P. (2010). From frequency to meaning:
# Vector space models of semantics. Journal of artificial intelligence
# research, 37, 141-188.
#
# Questions / comments:
# Simon De Deyne, simon2d@gmail.com
# Last changed: 12/06/2019
#
# # See creataeRandomWalk.m for a more efficient version
# 
# Adaptation for SWOW-RP-COVID data: Julieta Laurino, julilaurino@gmail.com
# 
rm(list = ls())

library('Matrix')
library('tictoc')
library('tidyverse')
library('igraph')


source('./importDataFunctions.R')
source('./networkFunctions.R')
source('./similarityFunctions.R')


# Construct similarity matrices for SWOW based on the primary (R1) responses
# or choose 'R123' to include all responses

# default value for alpha
alpha = 0.75

# Load the data
dataFile.SWOW       = './SWOW-RP.R60_covid.csv'
SWOW.R123             = importDataSWOW(dataFile.SWOW,'R123')
targetCues       = read.csv('./targetCues.csv', encoding = 'UTF-8') 

SWOW.R123             = SWOW.R123 %>% 
  left_join(targetCues) %>%
  select(cue, response, cov.time, cue.type) 


# For target cues only include the precovid o covid set 
SWOW.R123_precovid    = SWOW.R123 %>% 
  filter(((!is.na(cue.type)) & (cov.time == 'precovid')) |
           is.na(cue.type)) 

SWOW.R123_covid       = SWOW.R123 %>% 
  filter(((!is.na(cue.type)) & (cov.time == 'covid')) |
           is.na(cue.type)) 


# Generate the weighted graphs
G                   = list()
#G$R123$Strength       = weightMatrix(SWOW.R123_precovid,'strength')
#G$R123$PPMI           = weightMatrix(SWOW.R123_precovid,'PPMI')

tic()
G$R123$RW$precovid          = weightMatrix(SWOW.R123_precovid,'RW',alpha)
G$R123$RW$covid             = weightMatrix(SWOW.R123_covid,'RW',alpha)
toc()

write.csv(as.matrix(G$R123$RW$precovid),'./output/G_precovid_SWOW-RP.RW.A75.R123.csv', fileEncoding = 'UTF-8')
write.csv(as.matrix(G$R123$RW$covid),'./output/G_covid_SWOW-RP.RW.A75.R123.csv', fileEncoding = 'UTF-8')

# Compute the cosine similarity matrix 
S_precovid = cosineMatrix(G$R123$RW$precovid)
S_covid = cosineMatrix(G$R123$RW$covid)

write.csv(S_precovid,'./output/S_precovid_SWOW-RP.RW.A75.R123.csv')
write.csv(S_covid,'./output/S_covid_SWOW-RP.RW.A75.R123.csv')
