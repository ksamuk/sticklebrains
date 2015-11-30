###########################################################################
# cleans and formats raw measurement data from imageJ
# KS nov 2015
###########################################################################


# libraries ---------------------------------------------------------------

library("dplyr")
library("ggplot2")
library("tidyr")

# load raw data -----------------------------------------------------------

top_dat_raw <- tbl_df(read.table("data/top_measurements.txt", header = TRUE))
top_dat_raw 

# load diana extra data
diana_data <- tbl_df(read.csv("data/diana_data.csv", header = TRUE))

top_dat_raw  <- top_dat %>%
  select(Label, Length)


# clean data ---------------------------------------------------

## convert "Label" to pond/id

# clean off "_T001" slug, split by "."

pond_id <- top_dat_raw$Label %>% 
  gsub("_T.*|[pP]", "", .) %>%
  strsplit(split = "\\.") 

pond <- unlist(lapply(pond_id, function(x)x[1]))
id <- unlist(lapply(pond_id, function(x)as.numeric(x[2])))

# rebind
top_dat <- tbl_df(data.frame(pond, id, stringsAsFactors = FALSE))

# measurements:
# scale, olf_l, olf_w, tele_l, tele_w, optic_l, optic_w, cere_l, cere_w

# convert "Length" column to named measurements
top_dat_raw <- data.frame(top_dat, length = top_dat_raw$Length)

# check number of measurements per individual
# FALSE = everything as expected
top_dat_raw %>% 
  group_by(pond, id) %>%
  summarise(obs_length = length(length) < 9) %>%
  with(any(obs_length))

# format data -------------------------------------------------------------

# function for converting imagej measurements to columns
convert_length_to_measurements <- function(imagej_data){
  imagej_data$length %>% t() %>% 
    c(imagej_data$pond[1], imagej_data$id[1], .)
}

# format the measurements as a tbl_df
top_dat <- top_dat_raw %>%
  group_by(pond, id) %>%
  do(new_row = convert_length_to_measurements(.)) %>%
  select(new_row) %>%
  as.list() %>%
  unlist(recursive = FALSE) %>%
  do.call("rbind",.) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(c("pond", "id", "scale", "olf_l", "olf_w", "tele_l", "tele_w", "optic_l", "optic_w", "cere_l", "cere_w")) %>%
  tbl_df
  
row.names(top_dat) <- NULL

# convert char columns to numeric (not sure why they got detected as char)
top_dat_new <- data.frame(lapply(3:11,function(x) as.numeric(unlist(top_dat[,x]))))
top_dat<- cbind(as.numeric(top_dat$pond), as.numeric(top_dat$id), top_dat_new)%>%
  setNames(c("pond", "id", "scale", "olf_l", "olf_w", "tele_l", "tele_w", "optic_l", "optic_w", "cere_l", "cere_w")) %>%
  tbl_df

# join in diana data
tmp <- diana_data %>%
  select( Pond, Individual, Treatment, Standard.Length, BD)

names(tmp) <- c("pond", "id", "treatment", "sl", "bd")

top_dat <- left_join(top_dat, tmp)


# write to file -----------------------------------------------------------

write.table(top_dat, file = "data/brain_data_top.txt", row.names = FALSE, quote = FALSE, sep = "\t")
