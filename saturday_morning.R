# This is my script from Saturday January 22nd 2022 

library(dplyr)
library(readr)

cars2020 <- read_csv("data/cars2020.csv")

manual <- filter(cars2020, transmission == "Manual")

# manual = filter(cars2020, transmission == "Manual")

# manual2<-filter(cars2020,transmission=="Manual")

## How to get all cars with 8 or more cylinders?

df4 <- filter(cars2020, cyl >= 8) 


### All cars with manual transmissions and 8 or more cylinders

df_man_8 <- filter(cars2020, transmission == "Manual", cyl >= 8)

## Alternatively

df_man8_alt <- filter(manual, cyl >= 8)

## How many cars have automatic transmissions and less than 6 gears?

df_aut_l6 <- filter(cars2020, transmission == "Automatic",
                    gears < 6)

hist(cars2020$gears, breaks = seq(-0.5, 10.5, 1))


df <- filter(cars2020, transmission == "Automatic")


### Keeping only the model, transmission, mpg and cyl variables


cars_narrow <- select(cars2020, model, transmission, 
                      cyl, mpg)
# order of selection is reflected in the data frame output

## Using the - to remove columns 

cars <- select(cars2020, -aspiration, -startStop)

### How to sort cars by mpg descending

cars_ranked <- arrange(cars2020, desc(mpg))

## Can sort on multiple variables in order 

cars_ranked_trmpg <- arrange(cars2020, transmission, desc(mpg))



##start with the original data set (all vehicles)
## restrict to CVT transmission vehicles
## remove the sidi, aspiration and startStop columns
## order the data in descending order of mpg
## if cars have the same mpg order them by model alphabetically


df1 <- filter(cars2020, transmission == "CVT")
df2 <- select(df1, -sidi, -aspiration, -startStop)
df3 <- arrange(df2, desc(mpg), model)

## You don't have to create a new data frame for every step

df4 <- filter(cars2020, transmission == "CVT")
df4 <- select(df4, -sidi, -aspiration, -startStop)
df4 <- arrange(df4, desc(mpg), model)

### Using mutate to create new columns


cars30 <- mutate(cars2020, 
                 above30 = ifelse(mpg >= 30, TRUE, FALSE),
                 type = paste(make, model))
cars30 <- relocate(cars30, type, above30)

### Using summarise

## What is the total number of cars and the average miles per gallon

report <- summarise(cars2020, num_cars = n(),
                    avg_mpg = mean(mpg),
                    median_mpg = median(mpg))

## Can do the same for only manual cars

manual_report <- summarise(manual, num_cars = n(),
                           median_mpg = median(mpg))

### using group_by

cars_grouped <- group_by(cars2020, transmission)
new_report <- summarise(cars_grouped, num_cars = n(),
                        median_mpg = median(mpg))


########## Creating pipelines

final_df <- filter(cars2020, transmission == "CVT") |> 
  select(-sidi, -aspiration, -startStop) |> 
  arrange(desc(mpg), model)

## Can also use the pipe to send data to a function to begin: 

grouped_data <- group_by(cars2020, transmission)
report <- summarise(grouped_data)

alt_report <- group_by(cars2020, transmission) |>
  summarise(num_cars = n(), 
            median_mpg = median(mpg))


alt_report2 <- cars2020 |> group_by(transmission) |> 
  summarise(num_cars = n(),
            median_mpg = median(mpg))


hist(cars2020$mpg, main = "Distribution of fuel efficiency", 
     col = "royalblue", breaks = 50)












































