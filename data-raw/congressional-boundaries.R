library(sf)
library(tidyverse)

histcongress <- st_read("data-raw/histcongress-lt20.shp",
                        stringsAsFactors = FALSE) %>%
  st_transform(4326)
colnames(histcongress) <- tolower(colnames(histcongress))
histcongress <- histcongress %>%
  mutate(startcong = as.integer(startcong),
         endcong = as.integer(endcong)) %>%
  filter(startcong <= 20)
devtools::use_data(histcongress, overwrite = TRUE, compress = "xz")
