# SWOW-RP-COVID

The [Small World of Words project (SWOW)](https://smallworldofwords.org/project/) project is a scientific project to map word meaning in various languages. In this repository you will find the analysis pipeline for the Rioplatense Spanish SWOW-COVID study, in which we aimed to explore the collective changes in the mental lexicon as a consequence of the COVID-19 pandemic. The data were obtained in December 2020, and compared with responses previously obtained from the Small World of Words Rioplatense Spanish database ([SWOW-RP, Cabana et al., 2023](https://link.springer.com/article/10.3758/s13428-023-02070-z)). All association data, including the set included in this study, can be downloaded from the [SWOW main research site](https://smallworldofwords.org/es/project/research).

Suggestions are always appreciated, and do not hesitate to get in touch if you have any questions.
op
## Instructions

To reproduce the analyses you should: 
1. Clone [the current github repo](https://github.com/JulietaLaurino/SWOW-RP-COVID).
2. Download the raw data CSV file from [here](https://www.dropbox.com/s/5wia0wtw57cq5oi/SWOW-RP.complete_covid.csv?dl=0) and place it under the 'data/SWOW/raw' folder in the cloned repo.
3. Open the R Project 'SWOW-RP-COVID.Rproj' located in the 'scripts' folder, and open 'analysis.r'. You should be able to run all the code from line 18 (`source('preprocessData.R')`) on.



