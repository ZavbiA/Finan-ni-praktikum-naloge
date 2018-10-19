library(rvest) 
library(gsubfn) 
library(readr) 
library(dplyr) 
library(tibble)
library(reshape2)
library(gsubfn)
library(tidyr)

#uvoz tabele 2014

tabela2014.tidy <- read.csv("podatki csv/podatki_2014.csv",
                            header = False,
                            locale = locale(encoding = "UTF-8"))


