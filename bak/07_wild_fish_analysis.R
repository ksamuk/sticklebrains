# wild fish analysis
# not included in the final manuscript!

library("tidyverse")
library("ggjoy")


# raw data

# body size
body_dat <- read.csv("data/wild_fish_body_size.csv")

# brain size
brain_dat <- read.csv("data/wild_fish_brain_measurements.csv")

# convert brain data to columns

brain_df <- split(brain_dat, brain_dat$id)
brain_df <- lapply(brain_df, function(x) data.frame(x$id[1], t(x$len))  ) %>% bind_rows

names(brain_df) <- c("id", "px_1mm", "olf_x", "olf_y", "tele_x", "tele_y", "optic_x", "optic_y", "cere_x", "cere_y")

# harmonize labels

brain_df <- brain_df %>%
  separate(id, c("lake", "type", "sex", "id"), sep = "_") %>%
  mutate(id = gsub("00[12]\\.jpeg", "", id)) %>%
  arrange(type, sex, id) %>%
  mutate(id = as.numeric(id))

# convert to scale-corrected ellipsoid sizes

brain_df <- brain_df %>%
  rowwise %>%
  mutate(olf_size = olf_x/px_1mm * olf_y/px_1mm * pi) %>%
  mutate(tele_size = tele_x/px_1mm * tele_y/px_1mm * pi) %>%
  mutate(optic_size = optic_x/px_1mm * optic_y/px_1mm * pi) %>%
  mutate(cere_size = cere_x/px_1mm * cere_y/px_1mm * pi) %>%
  ungroup %>%
  select(type, sex, id, matches("_size"))


# join in body size data

wild_brain_df <- body_dat %>%
  select(type, sex, id, std_len) %>%
  left_join(brain_df)

#wild_brain_df %>%
#  gather(key = lobe, value = size, -type, -sex, -id, -std_len) %>%
#  filter(size > 2.2) %>%
#  ggplot(aes(x = std_len, y = size, color = type))+
#  geom_point()+
#  facet_wrap(~lobe, scales = "free_y")



# generate size-corrected residual values 
resids <- wild_brain_df %>%
  filter(olf_size > 2.2) %>%
  gather(key = region, value = size, olf_size, tele_size, optic_size, cere_size) %>%
  group_by(region) %>%
  do(resid = residuals(lm(.$size ~ .$std_len))) %>%
  .$resid %>%
  unlist

# format output
wild_dat_resid <- wild_brain_df %>%
  filter(olf_size > 2.2) %>%
  gather(key = region, value = size, olf_size, tele_size, optic_size, cere_size) %>%
  mutate(resid = resids)

wild_dat_resid %>%
  ggplot(aes(y = resid, x = type)) +
  geom_boxplot() +
  facet_wrap(~region, scale = "free")

#pond cross id treatment sex sl bd region size resid

wild_dat_resid <- wild_dat_resid %>%
  mutate(pond = "wild", cross = type, treatment = "wild", bd = NA, sl = std_len) %>%
  select(pond, cross, id, treatment, sex, sl, bd, region, size, resid)


# write
write.table(wild_dat_resid, "data/wild_data_corrected.txt", row.names = FALSE, quote = FALSE)

### read in other corrected data

wild_dat <- read.table("data/wild_data_corrected.txt", h = T) %>% 
  select(-bd) %>% 
  mutate(type = "wild") 
pond_dat <- read.table("data/pond_data_corrected.txt", h = T) %>% 
  select(-bd) %>% 
  mutate(cross = factor(cross), pond = factor(pond)) %>% 
  mutate(type = "pond") 
gard_dat <- read.table("data/garden_data_corrected.txt", h = T) %>% 
  mutate(cross = factor(family), pond = factor(pond)) %>% 
  mutate(type = "garden") %>%
  select(-family)

all_dat <- list(wild_dat, pond_dat, gard_dat) %>% bind_rows()

write.table(all_dat, "data/merged_data_all.txt", row.names = FALSE, quote = FALSE )


resids <- all_dat %>%
  mutate(sl = ifelse(sl > 20, sl/10, sl)) %>%
  #gather(key = region, value = size, olf_size, tele_size, optic_size, cere_size) %>%
  group_by(region) %>%
  do(resid = residuals(lm(.$size ~ .$sl, na.action = na.exclude))) %>%
  .$resid %>%
  unlist

# format output
all_dat <- all_dat %>%
  mutate(resid2 = resids)


all_dat %>%
  mutate(sl = ifelse(sl > 20, sl/10, sl)) %>%
  mutate(treatment = ifelse(cross %in% c("lim", "ben"), cross, treatment)) %>%
  mutate(treatment = ifelse(type %in% c("garden"), paste0(treatment, "_", type), treatment)) %>%
  ggplot(aes(x = sl, y = resid, color = treatment))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~region, scales = "free_y")


### read in other corrected data

all_dat %>%
  mutate(treatment = ifelse(cross %in% c("lim", "ben"), cross, treatment)) %>%
  mutate(treatment = ifelse(type %in% c("garden"), paste0(treatment, "_", type), treatment)) %>%
  ggplot(aes(y = log(resid2+200), x = treatment)) +
  geom_boxplot()+
  facet_wrap(~region, scales = "free")


all_dat %>%
  filter(!is.na(sex)) %>%
  mutate(treatment = ifelse(cross %in% c("lim", "ben"), cross, treatment)) %>%
  mutate(treatment = ifelse(type %in% c("garden"), paste0(treatment, "_", type), treatment)) %>%
  ggplot(aes(x = resid, y = treatment)) +
  geom_joy()+
  facet_grid(sex~region, scales = "free")

all_dat %>%
  filter(!is.na(sex)) %>%
  mutate(treatment = ifelse(cross %in% c("lim", "ben"), cross, treatment)) %>%
  mutate(treatment = ifelse(type %in% c("garden"), paste0(treatment, "_", type), treatment)) %>%
  ggplot(aes(x = resid, y = treatment)) +
  geom_joy()+
  facet_grid(sex~region, scales = "free")


