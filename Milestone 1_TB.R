install.packages("devtools")
devtools::install_github("UBC-MDS/datateachr")

library(datateachr)
library(tidyverse)

## Task 1: Choose a dataset
# Choosing a data set from 7 semi-tidy datasets from the *datateachr* package by Hayley Boyce and Jordan Bourak

# 1.1: Choosing 4 datasets to explore
# 1. apt_buildings
# 2. cancer_sample
# 3. flow_sample
# 4. vancouver_trees

# 1.2: Exploring the 4 chosen datasets
# 1. apt_buildings
apt_buildings <- data.frame(apt_buildings)
dim(apt_buildings) # 3455 rows, 37 columns
str(apt_buildings) # Dataset consists of numeric and character variables
names(apt_buildings) # Names of columns in dataset
View(apt_buildings) # Getting a visual of the dataset

# 2. cancer_sample
cancer_sample <- data.frame(cancer_sample)
dim(cancer_sample) # 569 rows, 32 columns
str(cancer_sample) # Dataset consists of mostly numeric variables, one character variable
names(cancer_sample) # Names of columns in dataset
View(cancer_sample) # Getting a visual of the dataset

# 3. flow_sample
flow_sample <- data.frame(flow_sample)
dim(flow_sample) # 218 rows, 7 columns
str(flow_sample) # Dataset consists of character and numeric variables
names(flow_sample) # Names of columns in dataset
View(flow_sample) # Getting a visual of the dataset

# 4. vancouver_trees
vancouver_trees <- data.frame(vancouver_trees)
dim(vancouver_trees) # 146611 rows, 20 columns
str(vancouver_trees) # Dataset consists of character and numeric variables
names(vancouver_trees) # Names of columns in dataset
View(vancouver_trees) # Getting a visual of the dataset

# 1.3: Narrow dataset to two choices
# I have narrowed the datasets down to two choices: *apt_buildings* and *vancouver_trees*
# These two datasets not only seem most interesting to me, but also include several numeric variables
# which will serve my quantitative data analyses (found using: str(DATASET)). 

# 1.4: Decide on the dataset
# I have chosen *apt_buildings* as my final dataset for the Mini Data-Analysis project. This data set
# consists of several variables that I can use to answer my research questions on which apartment
# buildings would be most suitable for comfortable living. I intend to examine the number of pet-friendly
# apartments and the amenities in apartments such as bike parking and non-smoking buildings.