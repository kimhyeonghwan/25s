rm(list = ls())

library(tidyverse)

setwd("C:/Users/Lenovo/hwan/homepage/study_25spring")
getwd()

raw_return <- read_csv("data/sp500_return.csv")
raw_id <- read_csv("data/stock_id.csv")
raw_list <- read_csv("data/sp500_list.csv")

# 2022~2024
list <- raw_list %>% 
  select(permno,start,ending) %>% 
  filter(start<=ymd(20220101), ending>=ymd(20241231))

id <- raw_id %>% 
  select(permno, ticker, comnam, nameendt) %>% 
  group_by(permno) %>% 
  arrange(permno, desc(nameendt)) %>% 
  slice(1)

snp500_return <- raw_return %>% 
  filter(date>=ymd(20220101), date<=ymd(20241231)) %>% 
  pivot_longer(.,cols = where(is.numeric), names_to = "permno", values_to = "return") %>% 
  mutate(permno = as.double(permno)) %>% 
  filter(permno %in% list$permno) %>% 
  left_join(.,id, by="permno") %>% 
  select(date,permno,ticker,comnam,return)

is.na(snp500_return) %>% sum()

write_csv(snp500_return, "snp500_return.csv")
