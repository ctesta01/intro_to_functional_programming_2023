# Introduction to Functional Programming
# Christian Testa
# July 2023

# Functional programming is a technique (or style of coding)
# in which functions are treated just like other variables
# in programming — meaning that they can be passed as arguments
# to other functions, combined together in lists or vectors,
# and repeatedly used in loops.
#
# Not all programming languages support functional programming:
# some programming languages do not allow users to pass functions
# as arguments, for example.  But R is nice and does support
# functional programming.

# Let's begin with some simpler examples that provide some
# foundation that we can build on.


# dependencies ------------------------------------------------------------

library(dplyr)
library(purrr)
library(palmerpenguins)
library(broom)
library(ggplot2)


# review of a function ----------------------------------------------------

# functions can be given a name, i.e, the variable
# they're assigned to.
#
# functions can take arguments, which go in the parentheses
# after the `function` keyword.
#
# the body of a function goes inside the curly brackets {}
#
# in R, the return value of the function is the last
# expression evaluated, or it can be explicitly given using
# `return(return_value_here)`
#
function_name <- function(argument1, argument2) {

  # body of the function
  if (argument1 > 10) {
    print("argument1 was greater than 10")
  } else {
    print("argument1: ", argument1)
    print("argument2: ", argument2)
  }
}

# example of a function:

# Format Estimates and 95% Confidence Intervals in a String
#
# This function takes 3 arguments (an estimate and its 95%
# confidence interval) and it formats them to use one digit
# past the decimal point and forms a string out of them.
#
# This function could be useful for rendering the text to go
# into a scientific manuscript.
#
estimate_and_95ci_formatter <- function(estimate, ci_low, ci_high) {

  # construct our "formatted_string" out of the estimate,
  # ci_low, and ci_high.
  #
  # we use `str_c` from the stringr package to concatenate
  # parts of the string together.
  #
  # the format of the output should be:
  # "estimate (95% CI: ci_low to ci_high)"
  # and we should only have one digit of accuracy on each of
  # estimate, ci_low, and ci_high.
  #
  formatted_string <- stringr::str_c(
    scales::number_format(accuracy = .1)(estimate),
    " ",
    "(95% CI: ",
    scales::number_format(accuracy = .1)(ci_low),
    " to ",
    scales::number_format(accuracy = .1)(ci_high),
    ")"
  )

  # return the formatted_string
  return(formatted_string)
}

# we can use our function by calling it with arguments.
#
# for example, we might have done some statistics to determine
# an effect estimate of scientific interest that we want to report
# is .5 with a 95% confidence interval from .25 to .75.
#
# we could easily generate a string reporting that for a
# scientific manuscript by calling:
estimate_and_95ci_formatter(.5, .25, .75)



# lists of functions ------------------------------------------------------

# realize that many of the things we use in day-to-day R
# programming are functions that have already been written
# for us by others.
#
# some examples that might be familiar to you are functions like
#
# mean()
# sd()
# min()
# max()

# we can even create lists of functions that we might want to use
# as follows:
my_functions <- list(mean, sd, min, max)

# and we could refer to elements in that list just like we refer
# to the elements of any list — only in this case, since the
# elements of this list are functions, we can call them.
my_functions[[1]](c(1,2,3))
my_functions[[2]](c(1,2,3))
my_functions[[3]](c(1,2,3))
my_functions[[4]](c(1,2,3))

# passing functions as arguments ------------------------------------------

# being able to pass functions as arguments to other functions
# is the single most important aspect to functional programming —
# without this, we cannot do functional programming.

# here's an example of a function that can
# take a function as an argument:

apply_function_to_the_iris_dataset <- function(f) {
  f(iris)
}

apply_function_to_the_iris_dataset(dim)
apply_function_to_the_iris_dataset(ncol)
apply_function_to_the_iris_dataset(colnames)



# apply-family functions --------------------------------------------------

# now that you've seen that one can pass functions as
# arguments to other functions, let's meet the apply-family
# functions.

# the apply-family functions allow us to repeatedly apply
# the same function to many different arguments.

# for example, `apply()` let's us apply a function over
# arrays or matrices.
#
# `apply()` usually takes 3 arguments:
#    1. the matrix or array we will use,
#    2. the margin (i.e., 1 for rows or 2 for columns)
#    3. the function to apply
#
# `apply()` will apply the function to each of the rows
# or columns (depending on what is passed for the margin
# argument).

apply(mtcars, 1, mean) # get the mean across rows
apply(mtcars, 2, mean) # get the mean across columns

apply(mtcars, 2, max) # get the max across columns

# a workflow I use from time to time is to define
# a function that counts the number of NAs in a vector
# and to apply that to the different columns of my data.

count_NAs <- function(x) {
  sum(is.na(x))
}

# load an example dataset:  in our case, the penguins
# data from the palmerpenguins package:
library(palmerpenguins)
?penguins
View(penguins)

apply(penguins, 2, count_NAs)


# other members of the apply family include:
# lapply - return the output as a list
# sapply - "simplify" the output, i.e., automatically try to cast it to a vector
# vapply - a version you can tell explicitly what kind of output you want
# mapply - a multivariate version of sapply
#
# but we don't use these very much anymore now that the {purrr}
# package is around.



# intro to purrr::map -----------------------------------------------------

library(purrr)

# map is a more modern replacement for apply

square <- function(x) {
  x^2
}

map(1:10, square)

# what are the advantages of `purrr::map()` compared to
# the apply-family?
#
# 1. map always returns a list (same as lapply)
# 2. tidyverse compatible syntax:
#
#    where the apply-family functions take the function
#    as the first argument, `map()` takes the data as
#    the first argument so it's easier to use in pipe based
#    workflows like this:
#
#    data |>
#      # other operations here ...
#      map()
#
#   here's a lifelike example:

my_directory <- "data/geographic_data/csv_files/"
files_for_reading <- c("us_cities.csv", "us_states.csv", "earthquakes.csv")

stringr::str_c(my_directory, files_for_reading) |>
  map(readr::read_csv, show_col_types=FALSE)


# 3. the ~ syntax for defining functions
#
#    throughout the tidyverse, there are many settings and functions
#    that can be passed single-argument functions defined using the ~ and .
#    syntax (sometimes called the 'formula syntax') rather than having to
#    write out `function(x) { }`
#
#    "~" indicates that the body of the function follows, and the . represents
#    the single argument to the function.
#
#    simple example:
map(penguins, ~ summary(.))
#
#    here's an example where we get the 95% quantile range of the numeric variables:
#
penguins |>
  select(where(is.numeric)) |>
  map(~ quantile(., c(0.025, 0.975), na.rm=TRUE))

# 4. `map()` has a handy syntax for element extraction for nested lists
#
# if you put a number or name into the 2nd argument of `map()`, then `map()` will
# try to extract that item from each element of the list given to it.
#
# examples:
characters <- list(
  harry = list(name = "Harry Potter", gender = "Male", pet = "Hedwig"),
  hermione = list(name = "Hermione Granger", gender = "Female", pet = "Crookshanks"),
  ron = list(name = "Ron Weasley", gender = "Male", pet = list("Scabbers", "Pigwidgeon"))
)

map(characters, 2)
map(characters, "name")
map(characters, "pet")


# 5. the easy use of type-specific variations like map_dbl, map_chr, map_lgl, etc.

# whereas vapply has a syntax that I think most people find tricky to use,
# the type-specific variations of map are intuitive and are guaranteed to
# return the data in the advertised type.
#
# for example, map_dbl always returns a "double" (i.e., double precision numeric
# type) vector.
map_dbl(1:10, square)
typeof(map_dbl(1:10, square))

# similarly, map_int will always return an integer vector:
map_int(1:10, square)
typeof(map_int(1:10, square))

# we could even cast the output to be character type:
map_chr(1:10, ~ as.character(square(.)))

# an example with map_lgl for boolean logical values:
is_even <- function(x) {
  # test if x divided by 2 is remainder 0 to determine if x is even
  x %% 2 == 0
}

map_lgl(1:10, is_even)

# why would we want to use these type-specific versions of map?
# some different reasons might be that:
#
#   - guaranteed output type: we're not comfortable with `sapply()` just guessing
#     when we do/don't want to simplify our data structure.
#
#   - variable input: we don't have strong guarantees on what data type is
#     being passed in but our program will fail if the map_* step doesn't return
#     the right data type. an example might be user-facing web applications.
#
#   - readability: we want people to be able to easily read from our code what
#     type the data is at each step.



# many models -------------------------------------------------------------

# one of the most powerful applications of purrr::map that I often use
# is in fitting stratified statistical models.
#
# that workflow looks something like this:

# fit separate models for each species
penguins_models_by_species <-
  penguins |>
  nest_by(species) |>
  mutate(model = list(lm(flipper_length_mm ~ bill_depth_mm, data = data)))

# now we can use purrr::map on the list column of models
map(penguins_models_by_species$model, broom::tidy) # use broom::tidy to extract coefficients

# maybe I might want to get the AIC of each model
map(penguins_models_by_species$model, AIC)

# learn more about Many Models approaches here:
# https://r4ds.had.co.nz/many-models.html


# many plots --------------------------------------------------------------

# another example using purrr in a tidyverse syntax:
#
# suppose we want to look through the different histograms of each of
# the variables in our dataset. it's not so hard to create lots of plots to look
# through by using purrr::map.

quantitative_variables <- c('bill_length_mm', 'bill_depth_mm', 'flipper_length_mm',
                            'body_mass_g')
quantitative_variables |>
  map(~ ggplot(data = penguins, aes_string(x = .)) +
        geom_histogram(aes(y = ..density..)) +
        geom_density() +
        ggtitle(.))
