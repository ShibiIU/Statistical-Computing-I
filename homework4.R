## Homework 4: Functions to be debugged

## 1. Berhu penalty

berhu <- function(x, delta) {
    if(abs(x) <= delta) { ## the condition should be "<=" rather than "<".
        abs(x)
    } else {
        x^2 / 2 * delta +   ## I moved the plus sign one line above. 
            ## If the plus sign was put in the next line, 
            ## R would think the current line is complete and execute only delta/2. 
            delta / 2  
    }
}

berhu(1,1) ## should be 1
berhu(.5, 1) ## should be .5
berhu(2, 1) ## should be 2.5
xseq = seq(-3, 3, length.out = 200)
plot(sapply(xseq, berhu, 1) ~ xseq, type = 'l')





## 2. Trimmed mean

trimmed_mean <- function(x, trim) {
    qlo = quantile(x, probs =  trim)  # specify the 0.1-quantile 
    qhi = quantile(x, probs = 1 - trim) # specify the 0.9-quantile 
    
    # Deleted these two lines. These two indices have the same length as x, 
    # but the x_trimmed has a length less than x. So it will return a NA.
    
    # lo_idx = x <= qlo    
    # hi_idx = x >= qhi   
    
    idx = (x > qlo & x < qhi) # the idx specifies the elements in x that simultaneously satisfy these two conditions.
    
    #x_trimmed = x  # deleted these three lines
    #x_trimmed = x[!lo_idx]
    #x_trimmed = x_trimmed[!hi_idx]
    
    x_trimmed=x[idx]
    return(mean(x_trimmed))
}

## the following two should give the same results
trimmed_mean(c(-20, 1, 3, 2, 2, 5, 20, 2, 3, 4), trim = .1)
mean(c(-20, 1, 3, 2, 2, 5, 20, 2, 3, 4), trim = .1)





## 3. String processing
process_table = function(str) {
    ## split by line
    lines = strsplit(str, "\n")[[1]] # use strsplit to split lines
    ## remove padding lines
    padding_lines = lines == "|-"
    lines = lines[ !padding_lines]  # use "!" to negate 
    ## remove the class definition line, header line, footer line
    class_line = grep("class=", lines)
    header_line = grep("! Rank !! Overall Ranking", lines)
    footer_line = grep("\\|}", lines)
    t_line = grep("\t\t\t\t\t", lines)  # also remove the "\t\t\t\t\t" line
    lines = lines[-c(class_line, header_line, footer_line, t_line)]
    ## apply the process_data_line function to all of the lines
    plyr::adply(lines, 1, process_data_line)
}

process_data_line = function(str) {
    split = strsplit(str, ' *\\|\\|? +')[[1]]
    ## extract the city
    city_pattern = "\\[\\[.+\\]\\]" # corrected the regular expression for city
    city_idx = grep(city_pattern, split)
    city = regmatches(split[city_idx], regexpr(pattern = city_pattern, split[city_idx]))
    city = gsub("\\[|\\]", "", x = city, perl = TRUE)
    ## extract the rankings and scores
    row = lapply(split[-city_idx], function(x) {
        return(as.numeric(x))
    })
    row = c(row, city)
    row[is.na(row)] = NULL
    names(row) = c("Rank", "Old.Rank", "Score", "City")
    row = data.frame(row)
    return(row)
}

city_rankings = '{| class="wikitable sortable"
|-
! Rank !! Overall Ranking 2017 !! City !! 2010 Score 
|-
| 1 || 1 || {{flagicon|Austria}} [[Vienna]] || 108.6
|-					
| 2 || 2 || {{flagicon|Switzerland}} [[Zürich]] || 108.0 
|-
| 3 || 4 || {{flagicon|Germany}} [[Munich]] || 107.0
|-
| 4 || 6 || {{flagicon|Germany}} [[Düsseldorf]] || 107.2
|-
| 5 || 7 || {{flagicon|Germany}} [[Frankfurt]] || 107.0
|}'
process_table(city_rankings)







## 4. Gradient descent
gradient_descent <- function(fn, deriv, start, step_size, epsilon) {
    x = start
    while(TRUE) {
        # newx equals x minus step_size*deriv(x)
        new_x = x - step_size * deriv(x) 
        # the condition should be the difference between f(xnew) and f(x) <= epsilon
        if(abs(fn(new_x)-fn(x)) <= epsilon){  
            break
        }
        x = new_x   
    }
    
    return(x)
}


## should return something close to 0
gradient_descent(function(x) x^2, function(x) 2 * x , start = 1,
                 step_size = .1, epsilon = 1e-10)





## 5. Line search
backtrack_desc <- function(fn, deriv, start, alpha, beta, epsilon) {
    x = start
    while(TRUE) {
        step_size = backtrack(fn, deriv, x, alpha, beta)
        new_x = x - step_size * deriv(x) # it should be x, not fn(x)
        # the condition should be the difference between f(xnew) and f(x) <= epsilon
        if(abs(fn(new_x)-fn(x)) <= epsilon) {
            break
        }
        x = new_x
    }
    return(x)
}

backtrack <- function(fn, deriv, x, alpha, beta) {
    t = 1
    while(fn(x - t * deriv(x)) > (fn(x) - alpha * t * deriv(x)^2)) {
        t = beta * t
    }
    return(t)  # backtrack function should return the step size t.
}

## should return something close to 0
backtrack_desc(function(x) x^2, function(x) 2 * x, start = 10,
               alpha = .03, beta = .8, epsilon = 1e-10)
backtrack_desc(function(x) x^2, function(x) 2 * x, start = 1,
               alpha = .03, beta = .8, epsilon = 1e-10)




