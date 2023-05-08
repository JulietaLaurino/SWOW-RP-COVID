# Preprocessing pipeline for the English Small World of Words project (SWOWEN-2018)
# Author: Simon De Deyne (simon2d@gmail.com), Alvaro Cabana (almadana@gmail.com)
#
# Each file is self-contained, but the entire pipeline can be executed here
#
# Adaptation for SWOW-RP-COVID data: Julieta Laurino, julilaurino@gmail.com

# Create complete data table from responses.csv and participants.csv
source('importRawData_covid.R')

# Compile a Spanish word list to use for participant language checks
source('createWordlist.R')

# Spelling Check - create word lists to manually verify - spell-check
source('createSpellingLists.R')

# Preprocess the data
source('preprocessData.R')

# Compile the cues used as target (pandemic, emotion, routine and control)
source('./R/createTargetCues.R')

# Sample a balanced dataset
source('balanceParticipants.R')

# Create datafiles with response statistics (types, tokens)
source('createResponseStats.R') 
source('createResponseStats.targetCues.R') # Responses to target cues.

# Create datafiles with cue statistics (# responses, unknown, missing, H)
#source('createCueStats.R') 
source('createCueStats.targetCues.R') # Target cues

# Compute random walk similarity matrix
source('graphRandomWalk_semanticSimilarityAnalysis.R') # S matrix for semantic similarity analysis (S_precovid and S_covid)
source('semanticSimilarityAnalysis.R') # Determine nns_ratio and semantic shifts to certain dimensions
