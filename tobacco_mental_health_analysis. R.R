# Load packages
library(tidyverse)
library(readr)
# Load data directly
yrbs <- read.csv("C:/Users/Neha/Desktop/YRBS_2023_data.csv")
head(yrbs)
# Check what files are actually on my Desktop
list.files("c:/Users/Neha/Desktop")
  normalizePath("~/Desktop")
  list.files("C:/Users/Neha/OneDrive/Desktop")
  # 1. Set your working directory to the Desktop folder
  setwd("C:/Users/Neha/OneDrive/Desktop")
  
  getwd()
  # 3. See the files (you should see YRBS_2023_data.csv in the list)
  list.files()
  # 4. Load the CSV
  library(readr)
  dat <- read_csv("YRBS_2023_data.csv")
  # See the first few rows
  head(dat)
  # See all column names 
  names(dat)
  # Get a summary
  summary(dat)
  # Age distribution
  # Checking how old students are 
  # Why : Age affects both tobacco use and mental health 
  # Code meaning : q1 = age variable 
  table(dat$q1)
  prop.table(table(dat$q1)) * 100
  # Gender distribution
  # What : Male vs female distribution
  # Why : Gender differences exist in both 
  # code : q2 = sex (1=Female, 2= Male)
  table(dat$q2)
  prop.table(table(dat$q2)) * 100
  
  # Grade Distribution 
  # which grades and with that mental health issue vary
  # Code : q3 = grade (1=9th, 2=10th, 3=11th, 4=12th)
  table(dat$q3)
  prop.table(table(dat$q3)) * 100
  
  # Check queations related to mental health
  # Check q20-q35 range (where these typically are)
  table(dat$q25, useNA = "always")
  # Interpretation guide:
  # 1= yes (felt sad/hopeless), 2 = No
  # NA = missing (student didnt answer)
  
  # Check question 26(Commonly "considered suicide")
  table(dat$q26, useNA = "always")
  prop.table(table(dat$q26)) * 100
  
  # Check question 27 (commonly:"made suicide plan")
  table(dat$q27, useNA = "always")
  prop.table(table(dat$q27)) * 100
  
  # Find cigerette and vaping question 
  # yrbs tobacco questions in q30-q45 range
  
  # pattern question, 1=yes, 2=no
  #"Days used in past 30 days" questions: 1=0 days, 2=1 days, 3=3-5 days, etc
  
  # Check potential cigarette questions
  table(dat$q30, useNA = "always")
  table(dat$q31, useNA = "always")
  
  # Check potential vaping 
  table(dat$q32, useNA = "always")
  table(dat$q33, useNA = "always")
  # Interpretation of the result
  #1 → 18,933 students did NOT smoke in past 30 days  
  #2 → 386 smoked 1–2 days  
  #3 → 122 smoked 3–5 days  
  #4 → 80 smoked 6–9 days  
  #5 → 58 smoked 10–19 days  
  #6 → 22 smoked 20–29 days  
  #7 → 138 smoked daily  
  #<NA> → 364 missing  
  # Examine multiple questions systematically
  # To ensure we don't miss any variable
  # Loop through the question to see their distribution
  for(i in 25:40) {
    col_name <- paste0("q", i)
    cat("\n=== Question", i, "===\n")
    print(table(dat[[col_name]], useNA = "always"))
    cat("\n")
  }
  
  # Converting q25 (depression) from numbers to words
  # "yes/no" is easier to understand than 1/2
  # How: case_when() is like an IF-THEN statement 
  # Assuming q25 = "felt sad or hopeless for 2+ weeks
   dat <- dat %>%
     mutate(depression = case_when(
     q25 == 1 ~ "yes - Depressed",
     q25 == 2 ~ "No - Not Depressed",
     TRUE ~ NA_character_
     ))
  # For NA- missing values stay missing
  # Check what we created 
   table(dat$depression, useNA = "always")
  # Creating used tobacco in past 30 days variable 
  # To keep it simple for analysis, yes/no for current use
  # Assuming q31 = "days smoked cigarettes in past 30 days"
  # 1 = 0 days, 2-7 = 1 or more days 
   
   dat <- dat %>%
     mutate(current_smoker = case_when(
       q31 == 1 ~ "No - Did not smoke",
       q31 >= 2 ~ "Yes - Smoked in past 30 days",
       TRUE ~ NA_character_
     ))
  
   # verify 
   table(dat$current_smoker, useNA = "always")
   
   # Finding what percentage of students reported depression
   # It is the key statistic for the research
   # Count "yes" responses, divide by total answered and multiply by 100
   
   depression_table <- table(dat$depression)
   print(depression_table)
  
   # Calculate the percentage 
   
   depression_prev <- (depression_table["yes - Depressed"] / 
                         sum(depression_table)) * 100
  
   
  cat("prevalence of depression symptopms:", round(depression_prev, 1), "%\n")
  
  # Now finding what % currently use tobacco
  # Same as above reason - count and calculate percentage
  
  smoking_table <- table(dat$current_smoker)
  print(smoking_table)
  
  smoking_prev <- (smoking_table["Yes - Smoked in past 30 days"] / 
                     sum(smoking_table)) * 100
  cat("Prevalence of current smoking:", round(smoking_prev, 1), "%\n")
  
  # Creating professional summary table for my methods and result section
  # Count sample sizes and calculate percentages for key variable
   summary_stats <- data.frame(
     Variable = c("Total Sample",
                  "Female", "Male",
                  "Depression Symptoms",
                  "Current Smoker"),
     N = c(nrow(dat),
           sum(dat$q2 == 1, na.rm = TRUE),
           sum(dat$q2 == 2, na.rm = TRUE),
           sum(dat$depression == "yes - Depressed", na.rm = TRUE),
           sum(dat$current_smoker == "Yes - Smoked in past 30 days", na.rm = TRUE)),
     Percentage = c(100,
                    mean(dat$q2 == 1, na.rm = TRUE) * 100,
                    mean(dat$q2 == 2, na.rm = TRUE) * 100,
                    mean(dat$depression == "yes - Depressed", na.rm = TRUE) * 100,
                    mean(dat$current_smoker == "Yes- Smoked in past 30 days", na.rm = TRUE) * 100)
   )
   
   print(summary_stats)
   
   # Creating a atble showing tobacco use by depression status 
   # It shows how many depressed students smoke and vice versa 
   # Using table() with two variables creating a cross tabulation 
    
   crosstab <- table(dat$depression, dat$current_smoker)
   print(crosstab)
   
   # Converting counts into percentage by row to make it easy to compare
   # prop.table with margin=1 calculates row percentages
   
   crosstab_pct <- prop.table(crosstab, margin = 1) * 100
   print(round(crosstab_pct, 1))
   
   # Doing the chi square test to see if the test are statistically significant
   # Differences could be by chance so test will tell if its "real"
   # chisq.test() compares observed vs. expected frequency
   
   chisq_result <- chisq.test(dat$depression, dat$current_smoker)
   print(chisq_result)
   
   # Interpretation : look at the p- value
   # If p< 0.05- statistically significant- depression and smoking related 
   # If p> 0.05- not significant- could be by chance
   
   cat("\nInterpretation:\n")
   if(chisq_result$p.value < 0.05) {
     cat("There IS a statistically significant relationship between depression and smoking (p < 0.05)\n")
   } else {
       cat("No statistically significant relationship found (p > 0.05)\n")
     }
  
   # Creating a side-by-side bar chart for visual comparision of smoking rates
   # ggplot2 creates graphics
   
   library(ggplot2)
   
   # Prepare data for plotting 
   
   plot_data <- dat %>%
     filter(!is.na(depression), !is.na(current_smoker)) %>%
     group_by(depression, current_smoker) %>%
     summarise(count = n(), .groups = "drop") %>%
     group_by(depression) %>%
     mutate(percentage = count / sum(count) * 100)
  
  # Create the chart
   ggplot(plot_data, aes(x = depression, y = percentage, fill = current_smoker)) +
     geom_bar(stat = "identity", position = "dodge") +
     labs(title = "Smoking Prevalence by Depression status",
          subtitle = "YRBS 2023 Data",
          x = " Depression Status",
          y = "Percentage (%)",
          fill = "Smoking Status") +
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
   # Save the chart
   ggsave("tobacco_depression_comparision.png", width = 10, height = 6, dpi = 300)
  
  # Generating number for results section
  # Every statement needs a statistic to back it up
   
  # Sample description 
   
   cat("SAMPLE CHARACTERISTICS:\n")
   cat("Total sample: n =", nrow(dat), "\n")
   cat("Depression prevalence:", round(depression_prev, 1), "%\n")
   cat("Current smoking prevalence:", round(smoking_prev, 1), "%\n\n")
   
   # Main finding
   cat("MAIN FINDING:\n")
   print(round(crosstab_pct, 1))
   cat("\nchi-square test: p-value =", round(chisq_result$p.value, 4), "\n")
   
   # Check the original tobacco variable you used
   # What question did you use? q30? q31? q32?
   
   # Let's look at all of them:
   cat("=== Q30 ===\n")
   table(dat$q30, useNA = "always")
   
   cat("\n=== Q31 ===\n")
   table(dat$q31, useNA = "always")
   
   cat("\n=== Q32 ===\n")
   table(dat$q32, useNA = "always")
   
   cat("\n=== Q33 ===\n")
   table(dat$q33, useNA = "always")
  
   # CORRECT: Current cigarette smoking (past 30 days)
   dat <- dat %>%
     mutate(current_smoker = case_when(
       q32 == 1 ~ "No - Did not smoke",           # 0 days = not a smoker
       q32 >= 2 ~ "Yes - Smoked in past 30 days", # 1+ days = current smoker
       TRUE ~ NA_character_
     ))
   
   # Check the new variable
   table(dat$current_smoker, useNA = "always")
   
   # Calculate CORRECT prevalence
   smoking_table <- table(dat$current_smoker)
   smoking_prev <- (smoking_table["Yes - Smoked in past 30 days"] / 
                      sum(smoking_table)) * 100
   
   cat("CORRECTED Current smoking prevalence:", round(smoking_prev, 1), "%\n")
  
   # ========================================
   # CORRECTED ANALYSIS
   # ========================================
   
   # 1. Create CORRECT smoking variable (using Q32)
   dat <- dat %>%
     mutate(current_smoker = case_when(
       q32 == 1 ~ "No - Did not smoke",
       q32 >= 2 ~ "Yes - Smoked in past 30 days",
       TRUE ~ NA_character_
     ))
   
   # 2. Calculate corrected prevalence
   smoking_table <- table(dat$current_smoker)
   smoking_prev <- (smoking_table["Yes - Smoked in past 30 days"] / 
                      sum(smoking_table)) * 100
   
   # 3. CORRECTED cross-tabulation
   crosstab <- table(dat$depression, dat$current_smoker)
   print(crosstab)
   
   # 4. CORRECTED percentages
   crosstab_pct <- prop.table(crosstab, margin = 1) * 100
   print(round(crosstab_pct, 1))
   
   # 5. Chi-square test
   chisq_result <- chisq.test(dat$depression, dat$current_smoker)
   
   # 6. CORRECTED SUMMARY
   cat("\n========================================\n")
   cat("CORRECTED RESULTS\n")
   cat("========================================\n")
   cat("Total sample: n =", nrow(dat), "\n")
   cat("Depression prevalence:", round(depression_prev, 1), "%\n")
   cat("Current smoking prevalence:", round(smoking_prev, 1), "%\n\n")
   
   cat("MAIN FINDING:\n")
   print(round(crosstab_pct, 1))
   cat("\nChi-square test: p-value =", round(chisq_result$p.value, 4), "\n")
   
   # 7. Interpretation
   cat("\n========================================\n")
   cat("INTERPRETATION:\n")
   cat("========================================\n")
   
   # Get specific percentages for interpretation
   pct_depressed_smoke <- crosstab_pct["yes - Depressed", "Yes - Smoked in past 30 days"]
   pct_notdepressed_smoke <- crosstab_pct["No - Not Depressed", "Yes - Smoked in past 30 days"]
   
   cat("Among students WITH depression:", round(pct_depressed_smoke, 1), "% smoked in past 30 days\n")
   cat("Among students WITHOUT depression:", round(pct_notdepressed_smoke, 1), "% smoked in past 30 days\n")
   cat("Difference:", round(pct_depressed_smoke - pct_notdepressed_smoke, 1), "percentage points\n")
   
   if(chisq_result$p.value < 0.05) {
     cat("\nConclusion: There IS a statistically significant relationship (p < 0.05)\n")
     cat("Students with depression symptoms have HIGHER rates of cigarette smoking.\n")
   } else {
     cat("\nConclusion: No statistically significant relationship found (p > 0.05)\n")
   }
   
   library(ggplot2)
   
   # Prepare data for visualization
   plot_data <- dat %>%
     filter(!is.na(depression), !is.na(current_smoker)) %>%
     group_by(depression, current_smoker) %>%
     summarise(count = n(), .groups = "drop") %>%
     group_by(depression) %>%
     mutate(percentage = count / sum(count) * 100)
   
   # Create the chart
   ggplot(plot_data, aes(x = depression, y = percentage, fill = current_smoker)) +
     geom_bar(stat = "identity", position = "dodge", width = 0.7) +
     geom_text(aes(label = paste0(round(percentage, 1), "%")), 
               position = position_dodge(width = 0.7), 
               vjust = -0.5, size = 4) +
     scale_fill_manual(values = c("No - Did not smoke" = "#2E7D32", 
                                  "Yes - Smoked in past 30 days" = "#C62828"),
                       name = "Smoking Status") +
     labs(title = "Cigarette Smoking Prevalence by Depression Status",
          subtitle = "Youth Risk Behavior Survey 2023 (n=20,103)",
          x = "Depression Status",
          y = "Percentage (%)",
          caption = "Chi-square test: p < 0.001") +
     theme_minimal() +
     theme(
       plot.title = element_text(size = 14, face = "bold"),
       plot.subtitle = element_text(size = 11),
       axis.title = element_text(size = 12),
       axis.text.x = element_text(size = 11),
       legend.position = "bottom",
       legend.title = element_text(size = 11, face = "bold")
     ) +
     ylim(0, 100)
   
   # Save the chart
   ggsave("depression_smoking_analysis.png", width = 10, height = 7, dpi = 300)
   
   cat("✓ Chart saved as 'depression_smoking_analysis.png'\n")
  
   # ========================================
   # COMPLETE SAVE SCRIPT
   # ========================================
   
   # 1. Save modified dataset
   write.csv(dat, "YRBS_2023_analyzed.csv", row.names = FALSE)
   cat("✓ Dataset saved\n")
   
   # 2. Save results table
   results <- data.frame(
     Depression_Status = c("No - Not Depressed", "Yes - Depressed"),
     Did_Not_Smoke_Pct = round(crosstab_pct[, "No - Did not smoke"], 1),
     Smoked_Pct = round(crosstab_pct[, "Yes - Smoked in past 30 days"], 1)
   )
   write.csv(results, "depression_smoking_results.csv", row.names = FALSE)
   cat("✓ Results table saved\n")
   
   # 3. Save summary statistics
   summary_stats <- data.frame(
     Statistic = c("Sample Size", "Depression Prev", "Smoking Prev", 
                   "Smoking - No Depression", "Smoking - Depression",
                   "Difference", "P-value"),
     Value = c(nrow(dat), 
               round(depression_prev, 1),
               round(smoking_prev, 1),
               14.3, 29.4, 15.1,
               round(chisq_result$p.value, 4))
   )
   write.csv(summary_stats, "analysis_summary.csv", row.names = FALSE)
   cat("✓ Summary saved\n")
   
   # 4. Save chart
   if(exists("chart")) {
     ggsave("depression_smoking_chart.png", plot = chart, 
            width = 10, height = 7, dpi = 300)
     cat("✓ Chart saved\n")
   }
   
   # 5. Save workspace
   save.image("tobacco_mental_health_analysis.RData")
   cat("✓ Workspace saved\n")
   
   # Show where files were saved
   cat("\n========================================\n")
   cat("All files saved to:\n", getwd(), "\n")
   cat("========================================\n")
   
   # List saved files
   cat("\nSaved files:\n")
   list.files(pattern = "YRBS|depression|analysis|tobacco")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  