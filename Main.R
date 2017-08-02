### Main

# suppressWarnings globaly
options(warn=-1)

# library(funModeling) # inspiration
library(lubridate)
library(tibble)
library(magrittr)

library(dplyr)
library(tidyr)

get_type_v <- function(x) {
  ## handler for posix object, because class function returns a list in this case
  posix=ifelse(is.POSIXct(x), "POSIXct", "")
  posix=ifelse(is.POSIXlt(x), paste(posix, "POSIXlt", sep="/"), posix)
  posix=ifelse(is.POSIXt(x), paste(posix, "POSIXt", sep="/"), posix)
  
  # ifnot posix..then something else
  if(posix=="")
  {
    cl=class(x)
    return(ifelse(length(cl)>1, paste(cl, collapse = "-"), cl))
  } else {
    return(posix)
  }
}


df_status <- function (data, print_results = TRUE) {
  
  # if (missing(print_results))
  #   print_results = T
  
  df_status_res = data.frame(
    n_zero = sapply(data, function(x) sum(x == 0, na.rm = T)),
    p_zero = paste0(round(100 * sapply(data, function(x) sum(x == 0, na.rm = T)) / nrow(data), 2), "%"),
    n_na = sapply(data, function(x) sum(is.na(x))),
    p_na = paste0(round(100 * sapply(data, function(x) sum(is.na( x ))) / nrow(data), 2), "%"),
    n_inf = sapply(data, function(x) sum(is.infinite(x))),
    p_inf = paste0(round(100 * sapply(data, function(x) sum(is.infinite( x ))) / nrow(data), 2), "%"),
    type = sapply(data, get_type_v),
    n_unique = sapply(data, function(x) sum(!is.na(unique( x ))))
  )
  
  df_status_res$`||variable||` = paste0(rownames(df_status_res), ":")
  
  df_status_res$most_unique = "-"
  df_status_res$most_unique[which(max(df_status_res$n_unique) == df_status_res$n_unique)] = "<-----"
  
  df_status_res$`n_unique=nrow` = "-"
  df_status_res$`n_unique=nrow`[which(df_status_res$n_unique == nrow(data))] = "<-----"
  
  # df_status_res$n_dups = apply( data, 2, function(x) sum(duplicated(x)) ) %>%
  #   as.tibble() %>% select(value) %>% rename(n_dups = value)
  
  dupe_per_col = NULL
  for(i in 1:ncol(data)){
    dupe_per_col[i] = as.character(data[min(which(duplicated(data[,i], incomparables = NA) == TRUE)), i])
  }
  
  df_status_res$dupe_sample_val = dupe_per_col
  
  date_range = select_if(data, function(x) is.Date(x) || is.POSIXct(x)) %>% sapply(function(x) paste0(min(x), " - ", max(x))) %>% as.data.frame(stringsAsFactors = F)
  date_range$`||variable||` = paste0(rownames(date_range),":")
  names(date_range)[1] = "date_range"
  df_status_res = left_join(df_status_res, date_range)
  
  df_status_res$max_nchar = apply( data, 2, function(x) max(nchar(x, type = "width")) ) %>% # <----- TODEBUG: NA returned for some columns, both character and non-character
    as.tibble() %>% select(value) %>% rename(max_nchar = value)

  rownames(df_status_res) = NULL
  
  df_status_res = select(df_status_res, 
                        `||variable||`,
                        type,
                        
                        `n_unique=nrow`,
                        most_unique,
                        
                        n_unique,
                        max_nchar,
                        everything())
  
  l = list(df_status_res)
  
  # if (print_results)
  #   print(df_status_res)
  # else
  #   return(df_status_res)
  
  if (print_results)
    print(l)
  else
    return(l)
  
}


df_status_plus <- function(x, ...) {
  # cat("\n~~~ df_status_plus | bEtA ~~~\n")
  cat("\n # dimensions:\n\n")
  cat(nrow(x), "row(s) by", ncol(x), "column(s)", "\n")
  cat("\n # df_status:\n\n")
  df_status(x)
  # cat(" # head of data:\n\n")
  # print(head(x, 5), justify = "right")
  # cat("\n # tail of data:\n\n")
  # print(tail(x), justify = "right")
  
}

aq = airquality
aq$uid = 1:nrow(aq)
aq$Date = Sys.Date()
aq$text_col = "text"
aq$PDate = as.POSIXct(seq.Date(Sys.Date(), Sys.Date() + 152, by = 1))

max(aq$PDate)

d = funModeling::df_status(aq)

df_status_plus(aq)