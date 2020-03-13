
# S610 HW1
# Name: Shibi He
# Date: 9/8/2019

rm(list=ls())


# create a data frame
df <-setNames(data.frame(matrix(ncol=4, nrow=21)), c("title", "n_words", "n_chars", "n_individuals"))

# create a list of files in folder "books" 
file_list <- list.files(path="books")

library(tidyverse)
library(readr)

for (i in 1:length(file_list)) {
    # Read in the text of each book
    b <- read_file(paste("books/",file_list[i], sep=""))
    
    # Extract the book titles
    df[[i,1]] <- gsub('^.*Title:\\s*|\\s*Author.*$', '', b)
    
    # Count number of words in the book
    df[[i,2]] <- lengths(strsplit(b, " "))
    
    # Count number of characters in the book
    df[[i,3]] <- nchar(b)
    
    # # Count number of individuals in each book
    myreg <- "(Mr|Mrs|Dr|Ms|Miss)[.]?\\s([A-Z].\\s)*([A-Z][a-z]*\\s)+"
    df[[i,4]] <- length(unique(unlist(regmatches(b, gregexpr(myreg, b)))))
}

# Print out this data frame
print(df)


# Print out the title of each book and the names of individuals.
for (i in 1: length(file_list)){
    print(df[[i,1]])
    b <- read_file(paste("books/",file_list[i], sep=""))
    myreg <- "(Mr|Mrs|Dr|Ms|Miss)[.]?\\s([A-Z].\\s)*([A-Z][a-z]*\\s)+"
    Names <- regmatches(b, gregexpr(myreg, b))
    Names_no_space <- gsub(" $","", unlist(Names))
    NameList <- unique(Names_no_space)
    print(NameList)
}




