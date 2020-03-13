

# microbenchmark: check how long it takes to run llr


library(microbenchmark)


# functions
import::here(compute_f_hat, llr, make_predictor_matrix, make_weight_matrix, W,
             .from = 'llr_functions.R')



# Check
# get the data
data(french_fries, package = 'reshape2')
french_fries = na.omit(french_fries)

# input data
x = french_fries$potato
y = french_fries$buttery

# space along which to smooth
z = seq(0, 15, length.out = 100)
omega = 2

# run smoothing
#fits = llr(z = z, x = x, y = y, omega = 2)

time = microbenchmark(
    llr= llr(x, y, z, omega)
)


print(time)


