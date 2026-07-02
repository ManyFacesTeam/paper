# READ ME

This project contains the files necessary to render the manuscript *Capturing Many Faces: A database and reproducible protocol for collecting face images for research*.

## the manuscript
* index.qmd renders the manuscript file
* the rendered manuscript can be found in docs/index.html

## code folder
* 0_raw_data: compiling of raw data (archive only)
* 1_data_prep: data cleaning 
* 2_agreement: summaries of rater demographics, measures of inter-rater agreement
* custom_functions: functions used in the other code files & manuscript file
* stim_figs: creation of figures including example stimuli
  
## data folder
### raw & cleaned data files:
* manyfaces_ratings_exp.csv & manyfaces_ratings_exp_cleaned.csv (rating data)
* manyfaces_ratings_quest.csv & manyfaces_ratings_quest_cleaned.csv (rater self-report data)
* *note that model self-report data can be accessed with the photo database & cannot be shared here*

### data cleaning steps
* exclusions.csv (rating data exclusions)
* recode_eth_model.csv (model self-reported ethnicity recoding)
* recode_eth_quest.csv (rater self-reported ethnicity recoding)
* recode_gender_model.csv (model self-reported gender recoding)

### other
* project_1136_structure.json (norming/validation study structure)
