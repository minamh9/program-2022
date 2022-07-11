# Hashtags are used for comments in your code 
# Comments do not get run


# Variable assignment -----------------------------------------------------

w <- 0 # Stores the value 0 into the variable w
print(w) # Prints the value stored in w to the console

x <- "a" # Stores the character/letter "a" into the variable x
x # Also prints the value stored to the console

# Change the variable "age" to contain your actual age
age <- 9

# Run the following code to print your age to the console
print(paste("My age is ", age))


y <- c(1, 2, 3, 4, 5) # Stores the vector containing values 1-5 into variable y
y # Prints the vector stored in variable y
length(y) # Computes the length of the vector (i.e. number of elements stored)

z_vector <- 1:5 # Another way to store vectors of consecutive numbers
z_vector

# Elements of a vector can be access by specifying its position in the vector
y[1] # prints the first value stored in y

# Change new_vector to contain a vector with the elements: 4, 6, 8, 10
new_vector <- 0
# Print the 3rd (not 1st) value stored in new_vector
new_vector[1]


# if statements -----------------------------------------------------------

# If statements only run the accompanying code if the condition is true
if (w == 0) {
  print("w equals 0")
}

# If else statements provide different code to be run depending on the condition
if (w > 0) {
  print("w is greater than 0")
} else {
  print("w is less than 0")
}

# Change the if statement to check if age is less than 21.
# If true, print the message "I am younger than 21!"
# If false, print the message "I am 21 or over!"
if (age != 999) { # != means "does not equal"
  print("message 1")
} else {
  print("message 2")
}


# for loops ---------------------------------------------------------------

# for loops iterate over the accompanying code for a given sequence
for (iteration in y) {
  print(iteration) # the value stored in iteration changes with each loop
}

# the sequence to iterate over does not need to be defined outside of the loop
for (i in 3:5) {
  print(i)
}

# for loops can be helpful for accessing individual elements of a vector
for (i in 1:length(new_vector)) { # i will go from 1 to 5 (length of the vector)
  print(paste(
    "The value stored in position", i, "of new_vector is:", new_vector[i]
  ))
}

# Change the loop below to loop over the days of the week and print the day
for (month in c("Jan", "Feb", "Mar", "Apr", "May", "June")) {
  print(paste("The month is", month))
}



# basic plotting ----------------------------------------------------------

# specify x and y axis values
plot(x = 1:3, y = c(5, 0, 2)) # plots the points (1, 5), (2, 0), (3, 2)

# can specify axis labels
plot(x = c(-4, 3, 1), y = c(5, 0, 2), xlab = "location", ylab = "height")

# Make a plot with the following points (1, -3), (-2, 5), (4, 0)

