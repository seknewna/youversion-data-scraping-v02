rm(list = ls())
library(rvest)
library(tidyverse)
library(openxlsx)
library(stringr)

# all_books <- c("GEN", "EXO", "LEV", "NUM", "DEU", "JOS", "JDG", "RUT", "1SA", 
#                "2SA", "1KI", "2KI", "1CH", "2CH", "EZR", "NEH", "EST", "JOB",
#                "PSA", "PRO", "ECC", "SNG", "ISA", "JER", "LAM", "EZK", "DAN", 
#                "HOS", "JOL", "AMO", "OBA", "JON", "MIC", "NAM", "HAB", "ZEP", 
#                "HAG", "ZEC", "MAL", "MAT", "MRK", "LUK", "JHN", "ACT", "ROM", 
#                "1CO", "2CO", "GAL", "EPH", "PHP", "COL", "1TH", "2TH", "1TI", 
#                "2TI", "TIT", "PHM", "HEB", "JAS", "1PE", "2PE", "1JN", "2JN", 
#                "3JN", "JUD", "REV")

all_books = c("GEN")

# function that will detect the next verse if it exists
next_verse <- function(prev_verse){
  v <- strsplit(prev_verse, split = "\\.")[[1]]
  num <- as.numeric(v[2]) + 1
  next_v <- v
  next_v[2] <- num
  next_v <- paste(next_v, collapse = ".")
  return(next_v)
}


# function to remove cross-references
pattern = "^[0-9]{1,3}\\s*#|^[0-9]{1,3}[a-zA-Z\u0080-\uFFFF]"
remove_crossref <- function(x){
  # x <- gsub("“|‘", "", x)
  x[grepl(pattern, x)]
}

get_verses <- function(bible, bible_num){
  first_verses = first_verses <- as.list(paste0(all_books, ".1.", bible))
  get_all_bible <- lapply(first_verses, function(url){
    verses <- list()
    
    while (TRUE) {
      url_sba <- paste0("https://www.bible.com/bible/", bible_num, "/", url)
      # cat(url, "\n")
      
      links <- read_html(url_sba) %>% html_nodes("a") %>% html_attr("href")
      text <- read_html(url_sba, encoding = "UTF8") %>% html_nodes("span") %>% html_text()
      text <- str_trim(text)
      text <- text[grepl(pattern, text)]
      text2 <- read_html(url_sba, encoding = "UTF8") %>% html_nodes("span.ChapterContent_content__RrUqA") %>% html_text()
      text2 <- str_trim(text2)
      text2 <- text2[text2 != ""]
      
      verses[[url]] <- text
      check_next <- grep(url %>% next_verse(), links)
      
      if (length(check_next) != 0){
        # cat("The next chapter", url %>% next_verse(), "exists. Getting data ...\n")
        url <- url %>% next_verse()
        url_sba <- paste0("https://www.bible.com/bible/", version, "/", url)
      } else {
        # cat("Moving to the next book.\n")
        break
      }
    }
    verses
  })
  
  return(get_all_bible)
}


merge_all <- function(x_list){
  x_list_processed <- lapply(x_list, function(list_element){
    n <- names(list_element)
    to_dataframe <- lapply(n, function(i){
      chp_id <- paste0(strsplit(i, split = "\\.")[[1]][1:2], collapse = ".")
      cat(chp_id, "\n")
      verse_id <- as.numeric(gsub("([0-9]+).*$", "\\1", remove_crossref(list_element[[i]])))
      chapter <- data.frame(ChapterID = chp_id, 
                            VerseID = verse_id,
                            Content = remove_crossref(list_element[[i]]))
    })
  }) %>% bind_rows()
  return(x_list_processed)
}


merge_data <- function(x, y, x_name = "x", y_name = "y", data_name = NULL){
  # Number of verses by chapter
  if (is.null(dim(x))){
    xdata <- merge_all(x)
  } else if (is.data.frame(x)){
    xdata <- x
  } else {
    stop("Check the type of x")
  }
  
  
  if (is.null(dim(y))){
     ydata <- merge_all(y)
  } else {
    ydata <- y
  }
  
  
  # Merge all
  data <- xdata %>% full_join(ydata, by = c("ChapterID", "VerseID")) %>% 
    rename(Content = Content.x, Translation = Content.y)
  
  # View(data)
  write.xlsx(xdata, paste0(x_name, ".xlsx"))
  write.xlsx(ydata, paste0(y_name, ".xlsx"))
  
  if (is.null(data_name)) data_name <- paste0(x_name, y_name, sep = "_")
  write.xlsx(data, paste0(data_name, ".xlsx"))
  return(data)
}
