# Code challenge for R class: make a faceted bar plot of biophysical setting data

# load libraries ----
library(tidyverse)
library(xlsx)

# load data ----
raw_bps_aoi <- read_csv("code_challenge/input_data/bps_aoi_attributes.csv")
raw_bps_model <- read_csv("code_challenge/input_data/bps_model_number_name.csv")
raw_combine <- read_csv("code_challenge/input_data/combine_raw.csv")
raw_LF16 <- read_csv("code_challenge/input_data/LF16_BPS_200.csv")
raw_refcon <- read_csv("code_challenge/input_data/ref_con_modified.csv")
raw_scls_aoi <- read_csv("code_challenge/input_data/scls_aoi_attributes.csv")

# (PAST) refcon cleaning data ----

# filter unecessary water column. 
# but do be careful about over-cleaning! 
# this step caused me some issues in the early days of the assignment
refcon <- select(raw_refcon, -Water) 

# filter non-aoi rows from refcon
filtered_refcon <- refcon %>%
  filter(Model_Code %in% c("13040_32_43_44_49",
                           "13670_32_44", 
                           "15070_44"))

# filter non-aoi rows from all bps 
bps_model <- raw_bps_model %>%
  filter(BpS_Name %in% c("Ozark-Ouachita Shortleaf Pine-Oak Forest and Woodland", 
                         "Ozark-Ouachita Shortleaf Pine-Bluestem Woodland", 
                         "Ozark-Ouachita Dry-Mesic Oak Forest"))

# merge aoi filtered refcon with bps_model names so that refcon has character names
merged_refcon <- left_join(filtered_refcon, bps_model, by = "Model_Code")
merged_refcon <- select(merged_refcon, -Model_Code) 

# (PRESENT) cuurent cleaning data ----

      # This is just horrible, I tried my best
      # filter non-aoi rows from combine
      # filtered_combine <- combine %>%
      # filter(Var1 %in% c("2131", "2141", "2161"))
      # test <- combine %>%  subset(c("2131", "2141", "2161"))
      # everything results in either nothing showing up, or only 2141. weird
      # I wonder if this is a data input issue rather than my code... not going to try to figure this one out

# find a better way to get percentages:
# yeah!

# bring in the character labels
combine <- left_join(raw_combine, raw_bps_aoi %>%
                       dplyr::select(1:4), by = c("Var1" = "VALUE")) %>%
  rename(fraction = Freq.x, whole = Freq.y)

# trim out non aoi data
combine <- combine %>%
  filter(BPS_CODE %in% c("15070", 
                         "13040", 
                         "13670"))

# do maths, get percents
combine <- combine %>%
  mutate(current_percent = as.integer((fraction/whole)*100)) 

# merge datasets ----

# clean up combine
combine <- combine %>%
  select(-fraction, -whole, -"...1.y", -Var1)

# clean up scls
scls_aoi <- raw_scls_aoi %>%
  select(LABEL, VALUE)

# merge over the vars from scls to combine (make the number codes into A B C etc)
merged_combine <- left_join(combine, scls_aoi, by = c("Var2" = "VALUE"))

# change data type... yucky! should be a better way to do this, oh well
merged_combine <- merged_combine %>%
  mutate(BPS_CODE = as.character(BPS_CODE))

# smooth down the model codes to match model... 
# asked ol' Chat G about this one, since I'm not sure how else i'd just extract the first five digits
bps_model$Model_Code <- sub("^([0-9]{5}).*", "\\1", bps_model$Model_Code)

merged_combine <- left_join(merged_combine, bps_model, 
                            by = c("BPS_CODE" = "Model_Code"))
# clean up combine... again
merged_combine <- merged_combine %>% 
  select(-"...1.x", -Var2, -BPS_CODE)

# rename LABEL to Class to match the refcon
merged_combine <- merged_combine %>%
  rename(Class = LABEL)

merged_refcon <- pivot_longer(merged_refcon,
                              cols = -BpS_Name,  
                              names_to = "Class",  
                              values_to = "ref_percent")  

# merge 'em
bps_ref_current <- left_join(merged_combine, 
                             merged_refcon, 
                             by = c("BpS_Name", "Class"))

# looks like UN slipped through the cracks. running out of time at this point, so I'll just sneak the UN row back in in excel, especially since none of the BpS have any "UN" on them.
# messed around in excel
# write.xlsx(bps_ref_current, "bps_ref_current.xlsx")
bps_ref_current <- read.xlsx("code_challenge/input_data/TESTbps_ref_current.xlsx", sheetIndex = 1)

# Order classes. have to put them in reverse order; 
bps_ref_current$class <- factor(bps_ref_current$class, levels = c(
  "Developed", "Agriculture", "UE", "UN", "E", "D", "C", "B", "A"))

# clean up dataset: yes, it can be tidier! it seems like a grouped bar plot is best done with all of the bars in one column, and that column being made up of {category A}, {category B} etc. this might solve my AWFUL issue of it generating a stacked bar plot instead of grouped even though I used position_dodge
bps_ref_current <- pivot_longer(bps_ref_current, 
                                cols = c(current_percent, ref_percent),
                                names_to = "percent_type",
                                values_to = "percent")
# make the chart ----
bps_plot <- ggplot(bps_ref_current, 
               aes(x = class, 
                   y = percent, 
                   fill = factor(percent_type))) + # "fill" adds different colors per categorical variable determined by "factor"
  geom_col(position = position_dodge()) + # makes a bar plot
  facet_wrap(~bps_name, labeller = labeller(bps_name = label_wrap_gen())) +
  coord_flip() + # makes things horizontal
  labs(title = "Succession Classes past and present", 
       subtitle = "Top BpSs selected for illustration. Not all succession classes present in all BpSs",
       caption = "Data from landfire.gov.",
       x = "", 
       y = "Percent") + 
  scale_fill_manual(name = "", 
                    values = c("#3d4740", "#32a852" ), # I did copy colors from hints for consistency
                    labels = c("Present", "Past"))
# scale_fill_manual(name = "", values = c("Present" = "#3d4740", "Past" = "#32a852"))
# have to put values and labels separately! this was what was messing the entire chart up. took me ages to figure this one out

# themes not of major importance here 


bps_plot




