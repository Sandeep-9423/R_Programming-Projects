#program1
my_function <- function() { # create a function with the name my_function
  print("Hello World!")
}
my_function()

#program2
my_function <- function(fname) {
  paste(fname, "Griffin")
}

my_function("Peter")
my_function("Lois")
my_function("Stewie")

#program3
my_function <- function(fname, lname) {
  paste(fname, lname)
}

my_function("Peter", "Griffin")

#program4
my_function <- function(country = "Norway") {
  paste("I am from", country)
}

my_function("Sweden")
my_function("India")
my_function() # will get the default value, which is Norway
my_function("USA")

#program5
my_function <- function(x) {
  return (5 * x)
}

print(my_function(3))
print(my_function(5))
print(my_function(9))

