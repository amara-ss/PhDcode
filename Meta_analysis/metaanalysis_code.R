#install.packages("metagear")
pacman::p_load("ggplot2","tidyr","dplyr","tidyverse","readxl","pastecs", "metagear")

# Read the dataset downloaded from the Web of Science
data_screening1 <- read_xlsx("Meta_analysis/Metaanalysis_dataset.xlsx")


# First find duplicates. We make sure that R finds titles that are duplicated despite how they are written:
references_cleaned <- data_screening1 %>%
  mutate(Title = str_to_lower(TITLE),  # Convert to lowercase
         Title = str_replace_all(Title, "[[:punct:]]", ""))  # Remove punctuation

# See duplicated titles
duplicates <- references_cleaned %>%
  group_by(Title) %>%
  filter(n() > 1)

#Remove duplicates:
references_cleaned <- references_cleaned %>%  
  distinct(Title, .keep_all = TRUE)

#Proceed to abstract screening: 
effort_distribute(references_cleaned,
                  reviewers = "Amara",
                  initialize = TRUE,
                  save_split = TRUE)

abstract_screener("Effort_Amara.csv", 
                  "Amara",
                  highlightKeywords = c("fire", "burn", "ectomycorrhiza", 
                                        "mycorrhiza", "fungi"))

#Go for a second round of screening of the DISCARDED abstracts:
data_screening2 <- references_cleaned %>%
  filter(INCLUSION=="NO")

#Proceed to abstract screening: 
effort_distribute(data_screening2,
                  reviewers = "Amara",
                  initialize = TRUE,
                  save_split = TRUE)

abstract_screener("Effort_Amara1.csv", 
                  "Amara",
                  highlightKeywords = c("fire", "burn", "ectomycorrhiza", 
                                        "mycorrhiza", "fungi"))

#PRISMA flow-chart:
phases <- c("START_PHASE: 573 of studies identified through database searching",
            "START_PHASE: # of additional studies identified through other sources",
            "564 of studies after duplicates removed",
            "564 of studies with title and abstract screened",
            "EXCLUDE_PHASE: # of studies excluded",
            "# of full-text articles assessed for eligibility",
            "EXCLUDE_PHASE: # of full-text excluded, not fitting eligibility criteria",
            "# of studies included in qualitative synthesis",
            "EXCLUDE_PHASE: # studies excluded, incomplete data reported",
            "final # of studies included in quantitative synthesis (meta-analysis)")
plot_PRISMA(phases)

#Selected papers for retrieving information (#262):
selected_papers1 <- references_cleaned %>%
  filter(INCLUSION2=="YES")
