
# create bullet points for first page of report describing any restrictions to the data

# create function to generate bullet points

bullet.function = function(x, is.null.text){
  if(is.null(x)){
    bullet = is.null.text
  } else if(!is.null(x)){
    bullet = paste("Restricted to - ", paste(x, collapse = ", "))
  } else {
    bullet = "ERROR"
  }
  bullet
}

# create bullet point for sex

bullet.sex = bullet.function(sex, is.null.text = "Not restricted")

# create bullet point for local authority 

bullet.la = bullet.function(local.authority, is.null.text = "Not restricted")

# create bullet point for laboratory

bullet.lab = bullet.function(laboratory, is.null.text = "Not restricted")

# create bullet point for requesting organisations

bullet.request = bullet.function(requesting.organisation, is.null.text = "Not restricted")

# create bullet point for specimen types 

bullet.spectype = bullet.function(specimen.type, is.null.text = "Not restricted")

# create bullet point for travel status

bullet.travel = bullet.function(travel, is.null.text = "Not restricted")


# create the age bullet point (slightly more complex)

if(is.null(age.lower) & is.null(age.upper)){
  bullet.age = "Not restricted"
} else if(!is.null(age.lower) & is.null(age.upper)){
  bullet.age = paste(">=", age.lower)
} else if(is.null(age.lower) & !is.null(age.upper)){
  bullet.age = paste("<=", age.upper)
} else if(!is.null(age.lower) & !is.null(age.upper)){
  bullet.age = paste(age.lower, "<=", "age", "<=", age.upper)  
} else {
  bullet.age = "ERROR"
}
