# # S610 HW2
# # Name: Shibi He
# # Date: 9/12/2019




## 1. Write a function that computes the number of mutations

myList <- list("A","C","T","G")

Nmutation = function(input, output){
    # check sequences are valid
    input_output <- paste(input, output, sep="")
    input_outputV <- strsplit(input_output,"")[[1]]

    for (s in input_outputV) {
        flag <- is.element(s, myList) & (nchar(input) == nchar(output))
        if(!flag) {
            print("There is an error!")
            return()
        }
    }
    
    # compute number of mutations
    k=0
    for (i in 1:nchar(input)){
        match <- strsplit(input, "")[[1]][i] == strsplit(output,"")[[1]][i]
        if (!match){
            k = k+1
        }
    }
    return(k)
}




## 2. Write a function that computes the measure of sequence divergence

myList <- list("A","C","T","G")
L=0

Likelihood = function(input, output, Trans){
    # check sequences are valid
    input_output <- paste(input, output, sep="")
    input_outputV <- strsplit(input_output,"")[[1]]

    for (s in input_outputV) {
        flag1 <- is.element(s, myList) & (nchar(input) == nchar(output))
        if(!flag1) {
            print("There is an error!")
            return()
        }
    }

    # check matrix is valid
    flag2 <- nrow(Trans) == length(myList) & ncol(Trans) == length(myList) & all(Trans>=0) & all(rowSums(Trans) == rep(1,length(myList)))
    if (!flag2){
        print("There is an error!")
        return()
    }
    
    # compute the measure of sequence divergence
    rownames(Trans) <- c("A","G","T","C")
    colnames(Trans) <- c("A","G","T","C")
    
    for (s in 1:nchar(input)){
    i=strsplit(input, "")[[1]][s]
    j=strsplit(output, "")[[1]][s]
    L = L + log(Trans[i, j])
    }
    return(L)
}



Trans1 <- matrix (c(0.93,0.05,0.01,0.01,
                    0.05,0.93,0.01,0.01,
                    0.01,0.01,0.93,0.05,
                    0.01,0.01,0.05,0.93), byrow=TRUE, ncol=4)



Divergence=Likelihood(input="AGGT", output="AACG", Trans1)
print(Divergence)



## 3. Read in the data
germline <- read.delim("germline.txt", header=FALSE)
sequences <- read.csv("sequences.csv", header=TRUE)


## 4. Compute number of mutations and sequence likelihood for each sequence in the data.

## create an empty data frame that contains the measures
A <- rep(NA, 200)
B <- rep(NA, 200)
C <- rep(NA, 200)
df <- data.frame(Nmut=A, Divergence=B, Type=C)


## compute number of mutation and sequence likelihood for each sequence
for (i in 1: length(sequences[[1]])){
    startseq <-toString( germline[[1]])
    endseq <- toString(sequences[[1]][i])
    df[[i, 1]] <- Nmutation(startseq, endseq)
    df[[i, 2]] <- Likelihood(startseq, endseq, Trans1)
    df[[i, 3]] <- toString(sequences[[2]][i])
    
}


## compute mean and standard deviation
library(tidyverse)
summary1=summarize(df, Mean_Nmut=mean(Nmut), SD_Nmut=sd(Nmut), Mean_Divergence=mean(Divergence), SD_Divergence=sd(Divergence))
print(summary1)

### summary by type/different conditions
summary2 = summarize(group_by(df, Type), Mean_Nmut=mean(Nmut), SD_Nmut=sd(Nmut), Mean_Divergence=mean(Divergence), SD_Divergence=sd(Divergence))
print(summary2)


## 5. t-test whether the distribution of the number of mutations differ between the two experiemental conditions

TypeA <- filter(df, Type=="a")
TypeA_Nmut <- TypeA[[1]]
TypeA_Divergence <- TypeA[[2]]

TypeB <- filter(df, Type=="b")
TypeB_Nmut <- TypeB[[1]]
TypeB_Divergence <- TypeB[[2]]

t.test(TypeA_Nmut, TypeB_Nmut)

# p-value = 0.9697, suggesting that we fail to reject the null hypothesis that the average 
# number of mutations between the two experimental conditions are equal. 


## 6. t-test whether the sequence likelihoods differ between the two experimental conditions.
t.test(TypeA_Divergence, TypeB_Divergence)

# p-value = 0.0014, suggesting that we can reject the null hypothesis, and conclude that 
# the mean sequence likelihoods are significantly different between the two experiemental
# condidtions.


## 7. The p-value for the t-test on number of mutations is 0.9697, while the p-value for 
# t-test on sequence likelihoods is 0.0014. This may imply that even though the avereage
# number of mutations are very similar under two conditions, they can have very different
# mutational pattern. Therefore, the number of mutations may not be a good measure to 
# decide whether evolution proceeds differently under the two conditions. 

