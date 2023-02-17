## SET UP


set.seed(2022) # because 2019, 2020 and 2021 were not great
library(tidyverse)
theme_set(theme_light(base_size = 14))
library(sf)
library(cowplot)
library(lubridate)
library(stringi)
library(kableExtra)
library(wesanderson)
library(ggtext)
library(fastai)
library(highcharter)
library(janitor)

# visualise occupancy data
# hack of the unmarked package plot function
plot_unmarked <- function(data) {
  x <- data
  y1 <- getY(x)
  ym <- max(y1, na.rm=TRUE)
  M <- nrow(y1)
  J <- ncol(y1)
  S <- length(x@ylist)
  y1 <- as.data.frame(do.call(rbind,x@ylist))
  colnames(y1) <- paste("obs",1:J)
  y1$site <- rep(1:M,S)
  y1$species <- as.factor(rep(names(x@ylist),each=M))
  y2 <- reshape(y1, idvar=c("site", "species"), varying=list(1:obsNum(x)),
                v.names="value", direction="long")
  y2$variable <- factor(paste("obs", y2$time))
  y2 %>%
    mutate(value = as_factor(value),
           site = as_factor(site),
           variable = fct_reorder(variable, time)) %>%
    ggplot() +
    aes(x = variable, y = site, fill = value) + 
    geom_tile() +
    scale_y_discrete(name = 'site',
                     labels = rownames(x@ylist$lynx)) + 
    facet_wrap(vars(species), ncol = 1) +
    scale_fill_manual(values=c( "#99CCFF","#FF9900"))+
    xlab('time') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


# Get Jura pix
load("dat/metadata_Jura.RData")
# nrow(allfiles)
allfiles %>%
  separate(DateTimeOriginal, c("years", "months", "days", "hours", "mins", "secs")) %>%
  mutate(years = replace(years, str_detect(SourceFile, "!! ATTENTION 2017 AU LIEU DE 2016 !!"), "2017")) %>%
  mutate(years = replace(years, str_detect(years, "2006"), "2016")) %>%
  mutate(years = replace(years, str_detect(years, "2011"), "2016")) %>%
  mutate(years = replace(years, str_detect(years, "2012"), "2016")) %>%
  mutate(years = as.numeric(years), 
         months = as.numeric(months),  
         days = as.numeric(days), 
         hours = as.numeric(hours), 
         mins = as.numeric(mins), 
         secs = as.numeric(secs)) %>% 
  mutate(y = years, 
         m = months,
         d = days) %>% 
  unite(Date, years, months, days, sep="-") %>%
  mutate(h = hours, mi = mins, s = secs) %>% 
  unite(Time, hours, mins, secs, sep=":") %>%
  unite(DateTime, Date, Time, sep=" ") %>%
  mutate(DateTime = ymd_hms(DateTime)) %>%
  select(FileName, Directory, Keywords, DateTime, y, m, d, h , mi, s) -> allpic_Jura
# min(allpic_Jura$DateTime)
# max(allpic_Jura$DateTime)
# Get trap id.
trapic_final_Jura <- allpic_Jura %>% 
  separate(FileName, c("value", "value2", "key"), extra = "merge") %>% 
  unite("n_point",value, value2, sep=".") %>% 
  arrange(desc(as.numeric(n_point))) %>% 
  separate_rows(sep=",", Keywords, convert = TRUE) %>% 
  mutate(especes = Keywords) %>%
  mutate(especes = stri_replace_all_fixed(especes, "vehicule", "humain"),
         especes = stri_replace_all_fixed(especes, "chien", "humain"),
         especes = stri_replace_all_fixed(especes, "cavalier", "humain"),
         especes = stri_replace_all_fixed(especes, "chasseur", "humain"),
         especes = stri_replace_all_fixed(especes, "frequentationhumaine", "humain"),
         especes = stri_replace_all_fixed(especes, "vaches", "humain")) %>%
  filter(especes %in% c('lynx','chevreuil','chamois','chat','renard'))
# Format occupancy data
tocc <- trapic_final_Jura %>%
  mutate(especes = as_factor(especes),
         n_point = as_factor(n_point)) %>%
  mutate(mois = month(DateTime)) %>%
  group_by(mois,n_point,especes,.drop = FALSE) %>%
  count() %>%
  filter(between(mois, 3, 11)) 
# Table of detections and non-detections for lynx. 
dat_lynx <- tocc %>%
  filter(especes == 'lynx') %>% 
  mutate(counts = if_else(n == 0, 0, 1)) %>%
  ungroup() %>%
  select(mois,n_point,counts) %>%
  pivot_wider(names_from = mois, values_from = counts) %>% 
  as.data.frame() 
# Roe deer
dat_chevreuil <- tocc %>%
  filter(especes == 'chevreuil') %>% 
  mutate(counts = if_else(n == 0, 0, 1)) %>%
  ungroup() %>%
  select(mois,n_point,counts) %>%
  pivot_wider(names_from = mois, values_from = counts) %>% 
  as.data.frame() 
# Chamois
dat_chamois <- tocc %>%
  filter(especes == 'chamois') %>% 
  mutate(counts = if_else(n == 0, 0, 1)) %>%
  ungroup() %>%
  select(mois,n_point,counts) %>%
  pivot_wider(names_from = mois, values_from = counts) %>% 
  as.data.frame() 
# Fox
dat_renard <- tocc %>%
  filter(especes == 'renard') %>% 
  mutate(counts = if_else(n == 0, 0, 1)) %>%
  ungroup() %>%
  select(mois,n_point,counts) %>%
  pivot_wider(names_from = mois, values_from = counts) %>% 
  as.data.frame() 
# Cat
dat_chat <- tocc %>%
  filter(especes == 'chat') %>% 
  mutate(counts = if_else(n == 0, 0, 1)) %>%
  ungroup() %>%
  select(mois,n_point,counts) %>%
  pivot_wider(names_from = mois, values_from = counts) %>% 
  as.data.frame() 
# Gather everything for later use
dat_jura <- list(deer = dat_chevreuil,
                 lynx = dat_lynx,
                 chamois = dat_chamois,
                 cat = dat_chat,
                 fox = dat_renard)

# Get Ain data, pictures manually labelled
load('dat/metadata_Ain.RData')
# nrow(allfiles)
allfiles %>%
  mutate(Keywords = observed) %>% # pick manual tags
  separate(DateTimeOriginal, c("years", "months", "days", "hours", "mins", "secs")) %>%
  mutate(years = as.numeric(years), 
         months = as.numeric(months),  
         days = as.numeric(days), 
         hours = as.numeric(hours), 
         mins = as.numeric(mins), 
         secs = as.numeric(secs)) %>% 
  mutate(y = years, 
         m = months,
         d = days) %>% 
  unite(Date, years, months, days, sep="-") %>%
  mutate(h = hours, mi = mins, s = secs) %>% 
  unite(Time, hours, mins, secs, sep=":") %>%
  unite(DateTime, Date, Time, sep=" ") %>%
  mutate(DateTime = ymd_hms(DateTime)) %>%
  mutate(FileName = pix) %>%
  select(FileName, Directory, Keywords, DateTime, y, m, d, h , mi, s) -> allpic_Ain
# min(allpic$DateTime, na.rm = TRUE)
# max(allpic$DateTime, na.rm = TRUE)
# Get trap id
trapic_final_Ain <- allpic_Ain %>% 
  separate(FileName, c("value", "value2", "key"), extra = "merge") %>% 
  unite("n_point",value, value2, sep=".") %>% 
  arrange(desc(as.numeric(n_point))) %>% 
  filter(Keywords %in% c('lynx','chevreuil','chamois','chat','renard')) %>%
  mutate(Keywords = fct_drop(Keywords))
# Format occupancy data
tocc <- trapic_final_Ain %>%
  mutate(especes = as_factor(Keywords),
         n_point = as_factor(n_point)) %>%
  mutate(mois = month(DateTime)) %>%
  group_by(mois,n_point,especes,.drop = FALSE) %>%
  count() %>%
  filter(between(mois, 3, 11)) 
# Table of detections and non-detections for roe deer. 
dat_chevreuil <- tocc %>%
  filter(especes == 'chevreuil') %>% 
  mutate(counts = if_else(n == 0, 0, 1)) %>%
  ungroup() %>%
  select(mois,n_point,counts) %>%
  pivot_wider(names_from = mois, values_from = counts) %>% 
  as.data.frame() 
# lynx
dat_lynx <- tocc %>%
  filter(especes == 'lynx') %>% 
  mutate(counts = if_else(n == 0, 0, 1)) %>%
  ungroup() %>%
  select(mois,n_point,counts) %>%
  pivot_wider(names_from = mois, values_from = counts) %>% 
  as.data.frame() 
# Chamois. 
dat_chamois <- tocc %>%
  filter(especes == 'chamois') %>% 
  mutate(counts = if_else(n == 0, 0, 1)) %>%
  ungroup() %>%
  select(mois,n_point,counts) %>%
  pivot_wider(names_from = mois, values_from = counts) %>% 
  as.data.frame() 
# Fox
dat_renard <- tocc %>%
  filter(especes == 'renard') %>% 
  mutate(counts = if_else(n == 0, 0, 1)) %>%
  ungroup() %>%
  select(mois,n_point,counts) %>%
  pivot_wider(names_from = mois, values_from = counts) %>% 
  as.data.frame() 
# Cat
dat_chat <- tocc %>%
  filter(especes == 'chat') %>% 
  mutate(counts = if_else(n == 0, 0, 1)) %>%
  ungroup() %>%
  select(mois,n_point,counts) %>%
  pivot_wider(names_from = mois, values_from = counts) %>% 
  as.data.frame() 
# Gather data together
dat_ain <- list(deer = dat_chevreuil,
                lynx = dat_lynx,
                chamois = dat_chamois,
                cat = dat_chat,
                fox = dat_renard)

# Get Ain data, pictures automatically labelled
load('dat/metadata_Ain.RData')
# nrow(allfiles)
allfiles %>%
  mutate(Keywords = predicted) %>% # pick automatic tags
  separate(DateTimeOriginal, c("years", "months", "days", "hours", "mins", "secs")) %>%
  mutate(years = as.numeric(years), 
         months = as.numeric(months),  
         days = as.numeric(days), 
         hours = as.numeric(hours), 
         mins = as.numeric(mins), 
         secs = as.numeric(secs)) %>% 
  mutate(y = years, 
         m = months,
         d = days) %>% 
  unite(Date, years, months, days, sep="-") %>%
  mutate(h = hours, mi = mins, s = secs) %>% 
  unite(Time, hours, mins, secs, sep=":") %>%
  unite(DateTime, Date, Time, sep=" ") %>%
  mutate(DateTime = ymd_hms(DateTime)) %>%
  mutate(FileName = pix) %>%
  select(FileName, Directory, Keywords, DateTime, y, m, d, h , mi, s) -> allpic_Ain
# min(allpic$DateTime, na.rm = TRUE)
# max(allpic$DateTime, na.rm = TRUE)
# Get trap id
trapic_final_Ain <- allpic_Ain %>% 
  separate(FileName, c("value", "value2", "key"), extra = "merge") %>% 
  unite("n_point",value, value2, sep=".") %>% 
  arrange(desc(as.numeric(n_point))) %>% 
  filter(Keywords %in% c('lynx','chevreuil','chamois','chat','renard')) %>%
  mutate(Keywords = fct_drop(Keywords))
# Format occupancy data
tocc <- trapic_final_Ain %>%
  mutate(especes = as_factor(Keywords),
         n_point = as_factor(n_point)) %>%
  mutate(mois = month(DateTime)) %>%
  group_by(mois,n_point,especes,.drop = FALSE) %>%
  count() %>%
  filter(between(mois, 3, 11)) 
# Table of detections and non-detections for roe deer. 
dat_chevreuil <- tocc %>%
  filter(especes == 'chevreuil') %>% 
  mutate(counts = if_else(n == 0, 0, 1)) %>%
  ungroup() %>%
  select(mois,n_point,counts) %>%
  pivot_wider(names_from = mois, values_from = counts) %>% 
  as.data.frame() 
# lynx
dat_lynx <- tocc %>%
  filter(especes == 'lynx') %>% 
  mutate(counts = if_else(n == 0, 0, 1)) %>%
  ungroup() %>%
  select(mois,n_point,counts) %>%
  pivot_wider(names_from = mois, values_from = counts) %>% 
  as.data.frame() 
# Chamois. 
dat_chamois <- tocc %>%
  filter(especes == 'chamois') %>% 
  mutate(counts = if_else(n == 0, 0, 1)) %>%
  ungroup() %>%
  select(mois,n_point,counts) %>%
  pivot_wider(names_from = mois, values_from = counts) %>% 
  as.data.frame() 
# Fox
dat_renard <- tocc %>%
  filter(especes == 'renard') %>% 
  mutate(counts = if_else(n == 0, 0, 1)) %>%
  ungroup() %>%
  select(mois,n_point,counts) %>%
  pivot_wider(names_from = mois, values_from = counts) %>% 
  as.data.frame() 
# Cat
dat_chat <- tocc %>%
  filter(especes == 'chat') %>% 
  mutate(counts = if_else(n == 0, 0, 1)) %>%
  ungroup() %>%
  select(mois,n_point,counts) %>%
  pivot_wider(names_from = mois, values_from = counts) %>% 
  as.data.frame() 
# Gather data together (obtained via deeplearning)
dat_ain_dl <- list(deer = dat_chevreuil,
                   lynx = dat_lynx,
                   chamois = dat_chamois,
                   cat = dat_chat,
                   fox = dat_renard)






## WORKFLOW

library(exifr)
pix_folder <- 'pix/pixJura/'
file_list <- list.files(path = pix_folder,
                        recursive = TRUE,
                        pattern = "*.jpg",
                        full.names = TRUE)
labels <-
  read_exif(file_list) %>%
  as_tibble() %>%
  unnest(Keywords, keep_empty = TRUE) %>% # keep_empty = TRUE keeps pix with no labels (empty pix)
  group_by(SourceFile) %>%
  slice_head() %>% # when several labels in a pix, keep first only
  ungroup() %>%
  mutate(Keywords = as_factor(Keywords)) %>%
  mutate(Keywords = fct_na_value_to_level(Keywords, "wo_tag")) %>% # when pix has no tag
  select(SourceFile, FileName, Keywords) %>%
  mutate(Keywords = fct_recode(Keywords,
                               "chat" = "chat forestier",
                               "lievre" = "lièvre",
                               "vehicule" = "véhicule",
                               "ni" = "Non identifié")) %>%
  filter(!(Keywords %in% c("ni", "wo_tag")))



labels %>%
  count(Keywords, sort = TRUE) %>%
  kable(caption = "Species considered, and number of images with these species in them.") %>%
  kable_styling()


# training dataset
pix_train <- labels %>%
  select(SourceFile, FileName, Keywords) %>%
  group_by(Keywords) %>%
  filter(between(row_number(), 1, floor(n()*80/100))) # 80% per category
# validation dataset
pix_valid <- labels %>%
  group_by(Keywords) %>%
  filter(between(row_number(), floor(n()*80/100) + 1, n()))



# create dir train/ and copy pix there, organised by categories
dir.create('pix/train') # create training directory
for (i in levels(fct_drop(pix_train$Keywords))) dir.create(paste0('pix/train/',i)) # create dir for labels
for (i in 1:nrow(pix_train)){
  file.copy(as.character(pix_train$SourceFile[i]),
            paste0('pix/train/', as.character(pix_train$Keywords[i]))) # copy pix in corresp dir
}
# create dir valid/ and copy pix there, organised by categories.
dir.create('pix/valid') # create validation dir
for (i in levels(fct_drop(pix_train$Keywords))) dir.create(paste0('pix/valid/',i)) # create dir for labels
for (i in 1:nrow(pix_valid)){
  file.copy(as.character(pix_valid$SourceFile[i]),
            paste0('pix/valid/', as.character(pix_valid$Keywords[i]))) # copy pix in corresp dir
}
# delete pictures in valid/ directory for which we did not train the model
to_be_deleted <- setdiff(levels(fct_drop(pix_valid$Keywords)), levels(fct_drop(pix_train$Keywords)))
if (!is_empty(to_be_deleted)) {
  for (i in 1:length(to_be_deleted)){
    unlink(paste0('pix/valid/', to_be_deleted[i]))
  }
}


bind_rows("training" = pix_train, "validation" = pix_valid, .id = "dataset") %>%
  group_by(dataset) %>%
  count(Keywords) %>%
  rename(category = Keywords) %>%
  kable(caption = "Sample size (n) for the training and validation datasets.") %>%
  kable_styling()


dls <- ImageDataLoaders_from_folder(
  path = "pix/test/",
  train = "train",
  valid = "valid",
  item_tfms = Resize(size = 460),
  bs = 10,
  batch_tfms = list(aug_transforms(size = 224,
                                   min_scale = 0.75), # transformation
                    Normalize_from_stats( imagenet_stats() )),
  num_workers = 0,
  ImageFile.LOAD_TRUNCATED_IMAGES = TRUE)


learn <- cnn_learner(dls = dls,
                     arch = resnet18(),
                     metrics = list(accuracy, error_rate))


one_cycle <- learn %>%
  fit_one_cycle(2, cbs = SaveModelCallback(every_epoch = TRUE,
                                           fname = 'model'))
one_cycle


learn$load("model_1")
interp <- ClassificationInterpretation_from_learner(learn)
interp$print_classification_report()


interp %>% most_confused()

fls <- list.files(path = "pix/pixAin",
                  full.names = TRUE,
                  recursive = TRUE)


predicted <- character(3)
categories <- interp$vocab %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_trim() %>%
  str_split("   ") %>%
  unlist()
for (i in 1:length(fls)){
  result <- learn %>% predict(fls[i]) # make prediction
  result[[3]] %>%
    str_extract("\\d+") %>%
    as.integer() -> index # extract relevant info
  predicted[i] <- categories[index + 1] # match it with categories
}
data.frame(truth = c("lynx", "roe deer", "wild boar"),
           prediction = predicted) %>%
  kable(caption = "Comparison of the predictions vs. ground truth.") %>%
  kable_styling()


sessionInfo()