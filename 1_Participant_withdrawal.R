library(tidyverse)
# Find the intersection of IDs between bd (original dataset) and w73666 (IDs for removal)
common_ids <- intersect(ukb669115$f.eid, w73666_2023.04.25$V1)

# Print the common IDs
print(common_ids)

#Remove the common IDs from bd - goes from 502409 to 502369 participants
bd <- ukb669115[!ukb669115$f.eid %in% common_ids, ]

# Find the intersection of IDs between bd_ICD_DCM_all and w73666
common_ids_ICD_DCM <- intersect(bd_ICD_DCM_all$f.eid, w73666_2023.04.25$V1)

# Print the common IDs
print(common_ids_ICD_DCM)
#No common IDs = no reduction in sample size! :)