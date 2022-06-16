# remotes::install_github("njahn82/semscholar")
library(semscholar)
library(tidyverse)
paper_ids <- (s2_authors("3207623"))$papers[[1]]
my_pubs <- s2_papers(paper_ids[,"paper_id"])

my_pubs <- my_pubs %>% select(authors, year, title, venue) 

l <- map(my_pubs$authors, ~ flatten_chr(select(.x, author_name)))

auth_list <- function(ll){
  rev_names <- word(ll,-1)
  inits <- word(ll,start = 1, end = -2)
  inits <- str_remove_all(inits, "[:punct:]")
  inits <- str_remove_all(inits, "[:lowercase:]")
  inits <- str_remove_all(inits, " ")
  full <- map2_chr(rev_names, inits, ~paste0(.x, ", ", .y))
  if (length(full) > 1) {
    full[length(full)] <- paste("&", full[length(full)])
    full[-length(full)] <- map_chr(full[-length(full)], ~ paste0(.x, ", "))
    full <- paste(full, collapse = "")
  } 
  return(full)
}

my_pubs$auth_list <- map_chr(l, auth_list)

my_pubs$journal <- my_pubs$venue