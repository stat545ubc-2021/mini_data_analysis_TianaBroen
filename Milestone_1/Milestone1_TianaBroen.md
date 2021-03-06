---
title: "Milestone1_TianaBroen"
author: "Tiana Broen, 16922171"
date: "09/10/2021"
output: github_document
---

```{r echo=FALSE}
devtools::install_github("UBC-MDS/datateachr")
library(datateachr)
library(tidyverse)
library(dplyr)
library(ggplot2)
```

```{r}
# Introduction
#### This file is part 1/3 of the STAT 545A Mini Data-Analysis. I explore four out of seven data sets in the *datateachr* package and determine which dataset I will use for future analyses.
```

```{r}
# Task 1: Choose a dataset
## Choosing a data set from 7 semi-tidy datasets from the *datateachr* package by Hayley Boyce and Jordan Bourak

## 1.1: Choosing 4 datasets to explore. Based on reading the descriptions of the datasets, I have chosen to explore the following four:
### 1. apt_buildings
### 2. cancer_sample
### 3. flow_sample
### 4. vancouver_trees

## 1.2: Exploring the 4 chosen datasets
### 1. apt_buildings
apt_buildings <- data.frame(apt_buildings)
dim(apt_buildings) # 3455 rows, 37 columns
str(apt_buildings) # Dataset consists of numeric and character variables
# names(apt_buildings) # Names of columns in dataset
# View(apt_buildings) # Getting a visual of the dataset; which is a tibble data frame
# glimpse(apt_buildings) # Alternative way to look at the data frame

# 2. cancer_sample
cancer_sample <- data.frame(cancer_sample)
dim(cancer_sample) # 569 rows, 32 columns
str(cancer_sample) # Dataset consists of mostly numeric variables, one character variable
# names(cancer_sample) # Names of columns in dataset
# View(cancer_sample) # Getting a visual of the dataset; which is a tibble data frame

# 3. flow_sample
flow_sample <- data.frame(flow_sample)
dim(flow_sample) # 218 rows, 7 columns
str(flow_sample) # Dataset consists of character and numeric variables
# names(flow_sample) # Names of columns in dataset
# View(flow_sample) # Getting a visual of the dataset; which is a tibble data frame

# 4. vancouver_trees
vancouver_trees <- data.frame(vancouver_trees)
dim(vancouver_trees) # 146611 rows, 20 columns
str(vancouver_trees) # Dataset consists of character and numeric variables
# names(vancouver_trees) # Names of columns in dataset
# View(vancouver_trees) # Getting a visual of the dataset; which is a tibble data frame

# 1.3: Narrow dataset to two choices
# I have narrowed the datasets down to two choices: *apt_buildings* and *vancouver_trees*
# These two datasets not only seem most interesting to me, but also include several numeric variables
# which will serve my quantitative data analyses (found using: str(DATASET)). Both data sets include variables which can be
# easily understood and applied to real-world examples. I think this will be useful when developing my research questions.

# 1.4: Decide on the dataset
# I have chosen *apt_buildings* as my final dataset for the Mini Data-Analysis project. This data set
# consists of several variables that I can use to answer my research questions on which apartment
# buildings would be most suitable for different tenants. I intend to examine the number of pet-friendly
# apartments and the amenities in apartments such as bike parking and non-smoking buildings. I am also interested
# in whether or not the structure and amenities of the buildings have changed over the years. 
```

```{r}
## Task 2: Exploring your dataset
# This is the code to explore the chosen dataset for the Mini Data-Analysis project for STAT545A.
# The chosen dataset is *apt_buildings* from the package *datateachr*. Data manipulation and visualization
# tools will be used to explore the dataset.

# Loading appropriate packages
library(datateachr)
library(tidyverse)
library(dplyr)
library(ggplot2)
# Label dataframe
apt_buildings <- data.frame(apt_buildings)

# 2.1: Completing 4 out of 8 exercises

# 1. Plot the distribution of a numeric variable. I have chosen to plot the distribution of the numeric
# variable: *year_built*. This will allow me to visualize how old the available apartment buildings are.
# The below code produces a histogram to show the approximate distribution of the *years_built* variable.
ggplot(apt_buildings, aes(year_built)) +
  geom_histogram(bins = 15)

# This is the second way I visualized the distribution. It uses kernels to produce the curve
apt_buildings %>%
  ggplot(aes(x=year_built)) +
  geom_density()

# 4. Explore the relationship between 2 variables in a plot. I will explore the relationship between year built and number of storeys. This will
# give me a general idea if these two variables are related (i.e. do older buildings have fewer stories?).
# I have removed values labeled as 'NA'.
apt_buildings %>%
  ggplot(aes(x=no_of_storeys, y=year_built)) + geom_point(alpha=.1) + #'aplha=.1' adjusts the opacity so you can see more data points.
  theme_minimal() + ylab("Year Built") + xlab("Number of Storeys") # 13 rows containing 'NA' values were deleted.

# 5. Filter observations. I am going to filter observations to determine how many apartment buildings
# are pet friendly. This is one of the  requirements I will examine to determine how "appropriate"
# an apartment building is for pet-owners.
pet_friendly_buildings <- apt_buildings %>% filter(pets_allowed == "YES")
count(pet_friendly_buildings) # There are 2764 pet-friendly buildings
percentage_pet_friendly_buildings <- count(pet_friendly_buildings)/count(apt_buildings)
percentage_pet_friendly_buildings # 80% of the total apartment buildings are pet-friendly

# 7. Make a new tibble with a subset of your data, with variables and observations you are interested in exploring. 
# I am creating a tibble to reflect the 'safety' of buildings. Including the variables reflecting: accessible entrances, fire escape, fire alarm,
# and emergency power. Then, I will filter to create a tibble that includes only apartments with a fire escape and fire alarm.
# For exploratory purposes, I will explore how many buildings meet all of the 'safety' requirements of the tibble.

apt_buildings_tibble <- as_tibble(apt_buildings) # Converting my data frame to a tibble format
safety_tibble <- apt_buildings_tibble %>% 
                  select(barrier_free_accessibilty_entr, exterior_fire_escape, fire_alarm, emergency_power) # Selecting the 'safety' columns I am interested in

fire_safe_apts <- safety_tibble %>% #filtering my tibble to only include apartments with both a fire escape and fire alarm
                      filter(exterior_fire_escape=='YES', !is.na(exterior_fire_escape)) %>%
                      filter(fire_alarm=='YES', !is.na(fire_alarm))
count(fire_safe_apts) # There are 575 apartments with an exterior fire escape AND a fire alarm
fire_safe_apts
safe_apts <- fire_safe_apts %>%
                filter(barrier_free_accessibilty_entr=='YES', !is.na(barrier_free_accessibilty_entr)) %>%
                filter(emergency_power=='YES', !is.na(emergency_power))
count(safe_apts) # There are 101 apartments that fit my theoretical safety requirements.
```

```{r}
## Task 3: Write your research questions
# 1. What building type is most likely to allow pets?
# 2. Is there a relationship between the accessibility of apartments and the year they were built?
# 3. What building type would be most appropriate for a family (i.e. including child play area, parking, guest parking, laundry room, and storage)
# 4. Are apartments that allow pets also more likely to offer parking?
```
