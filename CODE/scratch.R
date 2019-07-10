suppressMessages(library(here))
library(reshape2)
suppressMessages(library(moderndive))
library(magrittr)
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(library(ggpmisc))
library(ggrepel)


input <- tribble(
  ~raw,	~SS,
  0, 75,
  1,	78,
  2,	80,
  3,	83,
  4,	83,
  5,	83,
  6,	90,
  7,	93,
  8,	95,
  9,	98
)

output <- tribble(
  ~SS, ~raw,
  100, '-',
  99, '-',
  98, '9',
  97, '-',
  96, '-',
  95, '8',
  94, '-',
  93, '7',
  92, '-',
  91, '-',
  90, '6',
  89, '-',
  88, '-',
  87, '-',
  86, '-',
  85, '-',
  84, '-',
  83, '3-5',
  82, '-',
  81, '-',
  80, '2',
  79, '-',
  78, '1',
  77, '-',
  76, '-',
  75, '0',
  74, '-',
  73, '-',
  72, '-',
  71, '-',
  70, '-'
)
