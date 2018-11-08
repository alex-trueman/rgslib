library(tidyverse)

samples_2d <- read_csv('data-raw/samples-2d.csv')

usethis::use_data(samples_2d, overwrite = TRUE, compress = 'xz')
