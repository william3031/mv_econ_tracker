library(microbenchmark)
library(data.table)
library(fread)
library(tidyverse)

times <- 50 # Number of times to run each benchmark
file_name <- "app_data/jobs_wages_index.csv"
bm <- microbenchmark(
  rc = read_csv(file_name),
  vr = vroom(file_name),
  fr = fread(file_name),
  times = times
)

system.time(read_csv(file_name))
system.time(vroom(file_name))
system.time(fread(file_name))