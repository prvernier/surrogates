library(tidyverse)
library(tidytext)
library(tm)
library(pdftools)

directory <- "references"

pdfs <- paste(directory, "/", list.files(directory, pattern = "*.pdf"), sep = "")
pdf_names <- list.files(directory, pattern = "*.pdf")
pdfs_text <- map(pdfs, pdftools::pdf_text)


my_data <- data_frame(document = pdf_names, text = pdfs_text)

my_data %>% 
  unnest() %>% # pdfs_text is a list
  unnest_tokens(word, text, strip_numeric = TRUE) %>%  # removing all numbers
  group_by(document, word) %>% 
  summarise(count = n())
