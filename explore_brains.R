
# load libraries
library("dplyr")
library("ggplot2")
library("tidyr")

# load raw data

top_dat_raw <- tbl_df(read.table("top_measurements.txt", header = TRUE))
top_dat_raw 

top_dat_raw  <- top_dat %>%
  select(Label, Length)

## convert "Label" to pond/id

# clean off "_T001" slug, split by "."

pond_id <- top_dat_raw$Label %>% 
  gsub("_T.*", "", .) %>%
  strsplit(split = "\\.") 

pond <- unlist(lapply(pond_id, function(x)x[1]))
id <- unlist(lapply(pond_id, function(x)as.numeric(x[2])))

top_dat <- tbl_df(data.frame(pond, id, stringsAsFactors = FALSE))
top_dat

# measurements:
# scale, olf_l, olf_w, tele_l, tele_w, optic_l, optic_w, cere_l, cere_w

# convert "Length" column to named measurements
top_dat_raw <- data.frame(top_dat, length = top_dat_raw$Length)

# check number of measurements per individual
top_dat_raw %>% 
  group_by(pond, id) %>%
  summarise(obs_length = length(length))

# function for converting imagej measurements to columns
convert_length_to_measurements <- function(imagej_data){
  imagej_data$length %>% t() %>% 
    c(imagej_data$pond[1], imagej_data$id[1], .)
}



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

top_dat_new <- data.frame(lapply(2:10,function(x) as.numeric(unlist(top_dat[,x]))))

top_dat<- cbind(top_dat$pond, top_dat$id, top_dat_new)%>%
  setNames(c("pond", "id", "scale", "olf_l", "olf_w", "tele_l", "tele_w", "optic_l", "optic_w", "cere_l", "cere_w"))

  
# graphin

top_dat %>%
  mutate(brain_size = (olf_l*olf_w) + (tele_l * tele_w) + (optic_l * optic_w) + (cere_l * cere_w)) %>%
  ggplot(aes(x = pond, y = brain_size)) +
  geom_boxplot()
