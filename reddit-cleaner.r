```{r message=FALSE, warning=FALSE}
#load libraries
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

#Environmental Variables
Sys.setenv(TZ = "America/Chicago")
```
```{r message=FALSE}
#load raw csv file
raw_csv <- file.choose()
if (str_sub(raw_csv, -4, -1) == ".csv") {
    comment_list <- read_csv(raw_csv)
    file_name <- str_sub(basename(raw_csv), 0, -5)
} else {
    print("Not a CSV file.")
}

#clean up columns
#remove spaces and hash in header
colnames(comment_list) = gsub(" ", "_", colnames(comment_list))
colnames(comment_list) = gsub("#", "", colnames(comment_list))

comments <- comment_list %>%
    rename("User_Karma" = 8) %>%
    mutate(date_cst = as.Date(as.POSIXct(UTC_DateTime,
                                        origin = "1970-01-01",
                                        tz = "America/Chicago"))) %>%
    mutate(comment_len = sapply(strsplit(Comment, " "), length)) %>%
    select(-UTC_DateTime)

og_comment <- comments[1, ] #extract original post
og_comment <- og_comment %>%
    select(-comment_len)
top10per <- round(nrow(comment_list) * .1, 0)

# filters by users' karma (whether they are good posters or not),
# checks for comments only longer than 8 words
# sorts by absolute value of post score (should keep negatives)
comments_filtered <- comments %>%
    filter(User_Karma > 10000 & comment_len > 8) %>%
    mutate(abs_score = abs(Score)) %>%
    filter(abs_score / User_Karma <= .25) %>%
    arrange(-abs_score) %>%
    select(-comment_len)
comment_top10 <- comments_filtered %>%
    slice(1:(top10per)) %>%
    select(-abs_score)

#Keep original comment
comment_top10 <- rbind(og_comment, comment_top10)
comment_top10 <- distinct(comment_top10) #checks for original post
#df <- comment_top10[ , c(1, 9, 2, 3, 4, 5, 6, 7, 8)] #reorder
head(comment_top10)

#Generate file name, paste0 removes space that occurs before periods
if (nrow(comment_top10) > 0) {
    file_path <- paste0("C:\\Users\\p40014d\\OneDrive - AholdDelhaize.com\\Documents\\Web Scraping\\Self-Checkout - Reddit\\R-Cleaned\\R Clean ", # nolint
                        file_name, ".csv")
    write_excel_csv(comment_top10, file_path) #saves CSV with UTF-8 encoding
    print("SUCCESS!")
} else {
    print("There are no applicable rows in this dataframe.")
}
```
