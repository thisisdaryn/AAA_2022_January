library(palmerpenguins)
library(readr)
library(dplyr)


### to load penguins data from the package:
data(penguins)

#penguins <- read_csv("data/penguins.csv")

penguins <- read.csv("data/penguins.csv")

count(penguins, species)

count(penguins, island)

count(penguins, species, island)

hist(penguins$body_mass_g, col = "purple", breaks = 20,
     main = "Distribution of Penguin body mass",
     xlab = "grams")

boxplot(body_mass_g~species, data = penguins)


### scatter plot in base R 
plot(flipper_length_mm~bill_length_mm, data = penguins)

library(ggplot2)

### scatter plot in ggplot2 of flipper length vs bill length


ggplot(data = penguins,
       mapping = aes(x = bill_length_mm, 
                     y = flipper_length_mm)) + 
  geom_point() + theme_minimal()

ggplot(data = penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = flipper_length_mm)) + 
  geom_point() + theme_minimal() + geom_smooth(method = "lm")

### Add the colour to the plot to represent species

ggplot(data = penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = flipper_length_mm,
                     colour = species)) + 
  #geom_smooth(method = "lm") +
  geom_point() + theme_minimal()


### Histogram of body masses


hist(penguins$body_mass_g)

ggplot(data = penguins,
       mapping = aes(x = body_mass_g)) + 
  geom_histogram(fill = "#0471A6",
                 color = "black") + theme_minimal() 


### Using facet_wrap to show subgraphs for different species
ggplot(data = penguins,
       mapping = aes(x = body_mass_g)) + 
  geom_histogram(fill = "#0471A6",
                 color = "black") + theme_minimal() + 
  facet_wrap(~species, ncol = 1) + 
  labs(title = "How much do penguins weigh?",
       subtitle = "Scientists recorded the weights of 344 penguins between 2007 and 2009",
       x = "")

#### Box plot of body masses by species

ggplot(data = penguins,
       mapping = aes(x = body_mass_g,
                     y = species)) + 
  geom_boxplot() + theme_light()


### modifying box plot to show the different sexes within each species
penguins2 <- filter(penguins, !is.na(sex))

ggplot(data = penguins2, 
       aes(x = species, y = bill_length_mm,
           color = sex)) + geom_boxplot()


### 

df <- penguins |> group_by(species) |>
  summarise(number = n())

ggplot(data = df,
       aes(x = species, y = number)) + 
  geom_col(fill = "orange", color = "blue") + 
  geom_text(aes(y = number + 4, label = number)) + 
  theme_minimal()

penguin_plot <- ggplot(data = df) + 
  geom_point(aes(x = number, y = species)) + 
  geom_segment(aes(x = 0, xend = number,
                   y = species, yend = species)) + 
  geom_text(aes(x = number+5, 
                y = species, 
                label = number)) + 
  labs(title = "Number of penguins",
       subtitle = "Scientists went to Antarctica and observed some penguins. Blah blah blah",
       x = "", y = "")

## violin plot
ggplot(data = penguins,
       aes(x = species, y = body_mass_g)) + 
  geom_violin()


#install.packages("ggbeeswarm")
library(ggbeeswarm)
ggplot(data = penguins, 
       aes(x = species, y = body_mass_g)) + 
  geom_beeswarm()



ggplot(data = df,
       aes(x = species, fill = island)) + 
  geom_bar() + 
  geom_text(aes(x = species, y = ))











