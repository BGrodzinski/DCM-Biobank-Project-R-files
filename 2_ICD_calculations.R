library(tidyverse)
library(cowplot)
library(gghighlight)
library(stringr)
library(pwr)
library(dplyr)
library(lubridate)
library(effectsize)
  
#ALL VS MAIN ICD10 -------------------------------------------------------------------------

#Filter to 'main diagnosis' only = 806 DCM hits
 bd_ICD_DCM_main <- bd %>%
   filter_at(vars(contains("f.41202")), any_vars(str_detect(., "M4712|M500|M9931|M9941|M9951"))) 

#Filter to main & secondary diagnoses = 1092 DCM hits
  bd_ICD_DCM_all <- bd %>%
    filter_at(vars(contains("f.41270")), any_vars(str_detect(., "M4712|M500|M9931|M9941|M9951"))) 
  
#Create a table of those in all diagnoses, but not in main diagnoses (ie secondary diagnosis only) = 286 (1092-806)  
  DCM_extras <- anti_join(bd_ICD_DCM_all, bd_ICD_DCM_main, by = "f.eid")
  
#Summarise the main diagnoses for those with DCM as secondary diagnosis, first per-episode and then in aggregate, and export this  
  summary(select(DCM_extras, starts_with("f.41202")))
  
  DCM_extras_41202 <- DCM_extras[, grepl("^f\\.41202", names(DCM_extras))]
  summary(as.factor(unlist(DCM_extras_41202)))

  summary_table <- as.data.frame(sort(table(unlist(DCM_extras_41202)), decreasing = TRUE))
  names(summary_table) <- c("ICD10 Code", "Frequency")
  write.table(summary_table, file = "summary_table.tsv", sep = "\t", quote = FALSE, row.names = FALSE)
  
  #Do the same for all diagnoses when DCM is primary
  summary(select(bd_ICD_DCM_main, starts_with("f.41270")))
  DCM_secondaries <- bd_ICD_DCM_main[, grepl("^f\\.41270", names(bd_ICD_DCM_main))]
  summary(as.factor(unlist(DCM_secondaries)))
  
  DCM_secondaries_table <- as.data.frame(sort(table(unlist(DCM_secondaries)), decreasing = TRUE))
  names(DCM_secondaries_table) <- c("ICD10 Code", "Frequency")
  write.table(DCM_secondaries_table, file = "DCM_secondaries_table.tsv", sep = "\t", quote = FALSE, row.names = FALSE)
  
  #Create table of codes which appear both in the primary field when DCM is secondary, and the secondary field when DCM is primary
  common_table <- inner_join(summary_table, DCM_secondaries_table, by = "ICD10 Code")
  common_table <- common_table %>% arrange(desc(Frequency.x))
  print(common_table)
  
  #Enriched DCM with additional ICD codes (in which DCM is secondary and things like DCM are primary)
  # Recreate the f.41202 DCM dataset (n=806)
  filtered_bd <- bd %>%
    filter_at(vars(starts_with('f.41202')), any_vars(. %in% c('M4712', 'M500', 'M9931', 'M9941', 'M9951')))
  
  # Filter additional rows which have DCM in all diagnoses(f.41270) & other similar conditions (e.g. cervical stenosis) as main diagnosis (f.41202) (n=137)
  additional_rows <- bd %>%
    filter_at(vars(starts_with('f.41270')), any_vars(. %in% c('M4712', 'M500', 'M9931', 'M9941', 'M9951'))) %>%
    filter_at(vars(starts_with('f.41202')), any_vars(. %in% c('G992', 'M4802')))
  
  # Combine the filtered dataset and additional rows (n=943)
  bd_ICD_DCM_enriched <- bind_rows(filtered_bd, additional_rows)
  
   
    
    #POWER CALCULATION---------------------------------------------------------------------------
    pwr.r.test(alternative = "two.sided", n=806, power=0.80)
    
    
    #DCM RECENT OPERATIONS----------------------------------------------------------------
    
    # Function to extract diagnosis dates for DCM patients
    extract_diagnosis_dates <- function(data, diagnosis_cols, date_cols) {
      diagnosis_dates <- map_df(diagnosis_cols, ~ {
        dcm_codes <- c("M500", "M4712", "M9931", "M9941", "M9951")
        diagnosis_col <- sym(.x)
        date_col <- sym(str_replace(.x, "f.41202.", "f.41262."))
        
        data %>%
          filter(across(all_of(diagnosis_col), ~ . %in% dcm_codes)) %>%
          transmute(f.eid = f.eid, diagnosis_date = !!date_col)
      })
      
      diagnosis_dates
    }
    
    # Get columns with diagnosis codes and corresponding date columns
    diagnosis_cols <- bd %>%
      select(starts_with("f.41202.")) %>%
      names()
    
    date_cols <- str_replace(diagnosis_cols, "f.41202.", "f.41262.")
    
    # Extract diagnosis dates for DCM patients, including EID
    diagnosis_dates <- extract_diagnosis_dates(bd, diagnosis_cols, date_cols)
   
    #Make a new table containing only f.eid & operation columns
    op_cols_table <- bd_ICD_DCM_main %>%
      select(matches("f.eid|f.41282"))
    
    #Join these to get a table with diagnosis and operation dates
    diagnosis_and_operation_dates <- left_join(diagnosis_dates, op_cols_table, by = "f.eid")
    
    #Create columns for the range of dates we are looking for an operation in (i.e. 6 months to 2 years preceding diagnosis)
     diagnosis_and_operation_dates_with_range <- diagnosis_and_operation_dates %>%
       mutate(
         diagnosis_date = as.Date(diagnosis_date),  # Convert diagnosis_date to Date format if needed
         date_range_start = diagnosis_date - years(2),
         date_range_end = diagnosis_date - months(6),
       )
    
     #Make a new version of the table which calculates whether each row has had an operation within the date range, and how many
     op_counts <- diagnosis_and_operation_dates_with_range %>%
       rowwise() %>%
       mutate(across(starts_with("f.41282.0."), ~ as.Date(.))) %>%
       mutate(operation_within_range = any(across(starts_with("f.41282.0.")) >= date_range_start & across(starts_with("f.41282.0.")) <= date_range_end, na.rm = TRUE)) %>%
       mutate(num_operations_within_range = sum(across(starts_with("f.41282.0.")) >= date_range_start & across(starts_with("f.41282.0.")) <= date_range_end, na.rm = TRUE)) %>%
       ungroup()
     
     #Summarise results
     summary(op_counts$operation_within_range)
     summary(op_counts$num_operations_within_range)
     
     #Create the histogram plot
     ggplot(op_counts, aes(x = num_operations_within_range)) +
       geom_histogram(binwidth = 1, fill = "skyblue", color = "black") + 
       labs(x = "Number of Operations", y = "Frequency", title = "Number of operations undergone by DCM patients in the two years to six months prior to their diagnosis")
     