

# format column names

colnames(data) = tolower(colnames(data))

# format dates and create new date column

#data$specimen_date = as.Date(as.character(data$specimen_dateamend))
data$specimen_date = as.Date(as.character(data$specimen_dateamend), "%d/%m/%Y")

data$patient_date_of_birth = as.Date(as.character(data$patient_date_of_birth), "%d/%m/%Y")
#data$patient_date_of_birth = as.Date(as.character(data$patient_date_of_birth), "%d/%m/%Y")

data = data.frame(data, get.dates(data$specimen_date))

data$month.day = format(data$specimen_date, "%m%d")

# create age column

data$age = floor(as.numeric((data$specimen_date - data$patient_date_of_birth)/365.25))

# format year column

data$year = as.numeric(as.character(data$year))

# create age group column, convert into factor 

data$age.grp = cut(as.numeric(data$age), breaks = c(0, 6, 16, 26, 46, 66, Inf),
                   include.lowest = T)

age.grp.levels = c(levels(data$age.grp), "Unknown")

data$age.grp = as.character(data$age.grp)

data$age.grp[is.na(data$age.grp)] = "Unknown"

data$age.grp = factor(data$age.grp,
                      levels = age.grp.levels)


# create new sex column, convert into factor

data$sex = firstup(tolower(as.character(data$patient_sex)))

data$sex2 = factor(data$sex, levels = c("Female", "Male", "Unknown"))

# format LA column

data$local_authority_name = as.character(data$local_authority_name)

# format laboratory column

data$lab_name = as.character(data$lab_geography_name_current)

# format travel column 

data$travel_abroad_indicator = as.character(data$travel_abroad_indicator)

data$travel_abroad_indicator[is.na(data$travel_abroad_indicator)] = "Unknown"

data$travel_abroad_indicator[data$travel_abroad_indicator == ""] = "Unknown"

data$travel_abroad_indicator[data$travel_abroad_indicator == "U"] = "Unknown"

data$travel_abroad_indicator[data$travel_abroad_indicator == "N"] = "No"

data$travel_abroad_indicator[data$travel_abroad_indicator == "Y"] = "Yes"


data$travel_abroad_indicator = factor(data$travel_abroad_indicator,
                                      levels = c("Yes", "No", "Unknown"))


# format end reporting date 

end.reporting.date = as.Date(end.reporting.date)

# create a dummy column

data$dummy = "1"

#create a column with year as a factor from histroic year to end reporting year

data$year2 = factor(data$year,
                    levels = historic.year:as.numeric(format(end.reporting.date, "%Y")))


# make column with correct requesting organisation name for subsequent scripts

data$requesting_organisation_type_description = as.character(data$requesting_organisation_type_des)

# convert specimen type to character to avoid excess legends

data$specimen_type_description = as.character(data$specimen_type_description)


# filter on sex

if(is.null(sex)){
  NULL
} else {
  data = data[data$sex2 %in% sex, ]
}

# filter on sex

if(is.null(age.lower) & is.null(age.upper)){
  age.label = "All ages"
} else if (is.null(age.lower) & !is.null(age.upper)){
  data = data[(data$age <= age.upper) | is.na(data$age), ]
  age.label = paste("Only cases aged", age.upper, "and lower")
} else if (!is.null(age.lower) & is.null(age.upper)){
  data = data[data$age >= age.lower | is.na(data$age), ]
  age.label = paste("Only cases aged", age.lower, "and higher")
} else {
  data = data[(data$age >= age.lower & data$age <= age.upper) | is.na(data$age), ]
  age.label = paste("Only cases aged between", age.lower, "and", age.upper)
}


# filter on local authority 

if(is.null(local.authority)){
  NULL
} else {
  data = data[data$local_authority_name %in% local.authority, ]
}

# filter on labority 

if(is.null(laboratory)){
  NULL
} else {
  data = data[data$lab_name %in% laboratory, ]
}


# filter on requesting organisation 

if(is.null(requesting.organisation)){
  NULL
} else {
  data = data[data$requesting_organisation_type_description %in% requesting.organisation, ]
}

# filter on specimen type

if(is.null(specimen.type)){
  NULL
} else {
  data = data[data$specimen_type_description %in% specimen.type, ]
}


# filter on travel 

if(is.null(travel)){
  NULL
} else {
  data = data[data$travel_abroad_indicator %in% travel, ]
}










