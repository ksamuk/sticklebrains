# try to understand why data is different

library("tidyverse")
library("lme4")
library("broom")
library("wesanderson")
library("viridis")
library("plotly")

pond_dat_l <- read.table("data/pond_data_long.txt", h = T)
pond_dat_orig <- read.table("data/pond_data_clean.txt", h = T)

##########################
# are the RAW VALUES the same?
##########################

pond_dat <- pond_dat_orig %>% 
  ungroup %>%
  gather(key = region, value = size, matches("size"))  %>%
  mutate(un_id = paste0(pond, "_", cross, "_", id))

pond_dat_l_small <- pond_dat_l %>%
  mutate(size_l = size) %>%
  select(pond, cross, id, region, treatment, size_l)%>%
  mutate(un_id = paste0(pond, "_", cross, "_", id))

any(!(pond_dat_l_small$un_id %in% pond_dat$un_id))

any(!(pond_dat$un_id %in% pond_dat_l_small$un_id))

tmp <- left_join(pond_dat, pond_dat_l_small, by = c("region", "un_id"))

tmp %>%
ggplot(aes(x = size, y = size_l))+
  geom_point()

tmp %>%
  filter(is.na(size)| is.na(size_l)) %>%
  View

##########################
# residuals for long data
##########################

pond_dat_cor_l <- pond_dat_l %>%
  group_by(region) %>%
  do(augment(lm(.$size ~ .$sl + .$sex), data = .)) %>%
  ungroup %>%
  select(pond, cross, id, treatment, sex, sl, region, size, .resid) %>%
  rename(size_resid = .resid)

##########################
# COMPUTE LM RESIDUALS
##########################

pond_dat <- pond_dat_orig %>% 
  filter(sl  < 60) %>%
  mutate(type = paste0(treatment, "_", cross)) %>%
  filter(!(type == "P_7" & id == "79")) %>% # these individuals are extreme 'allometric outliers' 
  filter(!(type == "NP_1" & id == "56")) %>% # including or excluding them doesn't affect results, but makes plots more readable
  mutate(tmp = paste0(treatment, "_", cross)) %>%
  rowwise %>%
  mutate(total_size = olf_size + tele_size + optic_size + cere_size) %>%
  ungroup %>%
  gather(key = region, value = size, matches("size")) %>%
  select(-tmp)


pond_ls <- split(pond_dat, pond_dat$region)

# function for computing residuals (thanks, augment!)
fit_models <- function(x){
  
  mod1 <- lm(size ~ sl, data = x)
  #mod1 <- lm(size ~ sl + sex + (1|pond) + (1|cross), data = x)
  data.frame(augment(mod1), region = x$region[1])
  
}

# apply augment and bind results
pond_df <- lapply(pond_ls, fit_models)
pond_df <- bind_rows(pond_df)

pond_df <- pond_df %>%
  left_join(pond_dat_orig %>% select(cross, pond, treatment)) %>%
  distinct
