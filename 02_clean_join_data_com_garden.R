###########################################################################
# cleans and formats raw measurement data from imageJ
# for common garden fish
# KS may 2016
###########################################################################


###########################################################################
# libraries
###########################################################################


library("dplyr")
library("ggplot2")
library("tidyr")

###########################################################################
# raw data
###########################################################################


# the brain measurements from imagej (three files, so read and bind)
top_results <- list.files("data", pattern = "top_results", full.names = TRUE)
top_dat_raw <- lapply(top_results, read.table, header = TRUE)
top_dat_raw <- rbind_all(top_dat_raw)

# load sex data
sex_data <- tbl_df(read.csv("data/common_garden_sex.csv", header = TRUE))

# load standard length data
sl_data <- tbl_df(read.csv("data/common_garden_body_size.csv", header = TRUE))

###########################################################################
# clean brain data
###########################################################################

# remove angle data
top_dat_raw  <- top_dat_raw  %>%
  select(Label, Length)

## convert "Label" to pond/family/id

# clean off .jpg/p/trailing 1s
top_dat_raw$Label <- top_dat_raw$Label %>% 
  gsub("\\.jpg|p", "", .) %>%
  gsub("1{1}$", "", .)

# split id string into three columns
top_dat_raw <- top_dat_raw %>%
  separate(Label, sep = "\\.", into = c("pond", "family", "id")) %>%
  rename(length = Length)

# measurements:
# scale, olf_l, olf_w, tele_l, tele_w, optic_l, optic_w, cere_l, cere_w

# check number of measurements per individual
# FALSE = everything as expected
top_dat_raw %>% 
  group_by(pond, family, id) %>%
  summarise(obs_length = length(length) < 9) %>%
  with(any(obs_length))

###########################################################################
# format brain data
###########################################################################

# function for converting imagej measurements to columns
convert_length_to_measurements <- function(imagej_data){
  imagej_data$length %>% t() %>% 
    c(imagej_data$pond[1], imagej_data$family[1], imagej_data$id[1], .)
}

# format the measurements as a tbl_df
top_dat <- top_dat_raw %>%
  group_by(pond, family, id) %>%
  do(new_row = convert_length_to_measurements(.)) %>%
  select(new_row) %>%
  as.list() %>%
  unlist(recursive = FALSE) %>%
  do.call("rbind",.) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(c("pond", "family","id", "scale", "olf_l", "olf_w", "tele_l", "tele_w", "optic_l", "optic_w", "cere_l", "cere_w")) %>%
  tbl_df
  
row.names(top_dat) <- NULL

# convert char columns to numeric (not sure why they got detected as char)
top_dat_new <- data.frame(lapply(4:12,function(x) as.numeric(unlist(top_dat[,x]))))
top_dat <- cbind(as.character(top_dat$pond), as.character(top_dat$family), as.character(top_dat$id), top_dat_new)%>%
  setNames(c("pond", "family", "id", "scale", "olf_l", "olf_w", "tele_l", "tele_w", "optic_l", "optic_w", "cere_l", "cere_w")) %>%
  tbl_df

###########################################################################
# join sl data
###########################################################################

# split sl id column into pond/family/id
sl_data <- sl_data %>%
  separate(id, sep = "\\.", into = c("pond", "family", "id")) %>%
  rename(sl = standard_length)

top_dat <- left_join(top_dat, sl_data)

###########################################################################
# join sex data
###########################################################################

sex_data <- sex_data %>%
  mutate(pond = as.factor(pond)) %>%
  mutate(family = as.factor(family)) %>%
  mutate(id = as.factor(id)) %>%
  mutate(sex = as.character(sex))

top_dat <- left_join(top_dat, sex_data)
  
###########################################################################
# write to file
###########################################################################

write.table(top_dat, file = "data/brain_data_common_garden.txt", row.names = FALSE, quote = FALSE, sep = "\t")
