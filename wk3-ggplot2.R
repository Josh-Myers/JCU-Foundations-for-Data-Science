# week 3 - ggplot2
library(tidyverse)
data("mpg")
mpg = as_tibble(mpg)

hwy_rescaled = (mpg$hwy-min(mpg$hwy))/(max(mpg$hwy)-min(mpg$hwy)) # Rescaling hwy into [0,1]

displ_rescaled = (mpg$displ-min(mpg$displ))/(max(mpg$displ)-min(mpg$displ)) # Same for displ

ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy, size=displ_rescaled*hwy_rescaled))

ggplot(data = mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, size=displ_rescaled*hwy_rescaled, alpha=cyl))

ggplot(data = mpg) + geom_point(mapping=aes(x=displ, y=hwy, colour = class), size = 4)

ggplot(data = mpg) + geom_point(mapping=aes(x=displ, y=hwy, shape = class), size = 4)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, size=displ_rescaled*hwy_rescaled, alpha=cyl), position="jitter")

ggplot(data = mpg) + geom_text(mapping=aes(x=displ, y=hwy, label = class, colour = cyl))

ggplot(data = mpg) + geom_text(mapping=aes(x=displ, y=hwy, label = cyl, colour = class))

MyGraph <- ggplot(data=mpg) + geom_point(mapping=aes(x=displ,y=hwy), colour="red", shape=15)
MyGraph + facet_grid(class ~ drv)

#Smoothing
MyGraph + geom_smooth(mapping = aes(x=displ,y=hwy))

#Force linearity
MyGraph + geom_smooth(mapping = aes(x=displ,y=hwy), method = "lm", se = FALSE)

#Model for different groups
MyGraph + geom_smooth(mapping = aes(x=displ, y=hwy, group = drv), method = "lm", se = FALSE)

MyGraph + geom_smooth(mapping = aes(x=displ, y=hwy, colour = drv), method="lm", se=FALSE)

ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy, colour=drv), shape=15, size=3) + geom_smooth(mapping=aes(x=displ, y=hwy, colour=drv), method="lm", se=FALSE)

# better to define aes for all up front - save repetition
ggplot(data=mpg, mapping=aes(x=displ, y=hwy, colour=drv)) + geom_point(shape=15, size=3) + geom_smooth(method="lm", se=FALSE)

ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) + geom_point(shape=15, size=3) +
  geom_smooth(method="lm", se=FALSE) + aes(colour = drv)

# can set globally (in first call to ggplot) or locally - below data is called locally in geom_point
ggplot() + geom_point(data = mpg, mapping=aes(x=displ, y=hwy))

library(mdsr)
data("CIACountries")
CIACountries = as_tibble(CIACountries)
ggplot(data = CIACountries) + geom_bar(mapping = aes(x = net_users))

CIACountries_Sample <- sample_n(CIACountries, size = 25)
ordered_countries <- reorder(CIACountries_Sample$country, CIACountries_Sample$pop)

# we set the argument stat to “identity” (its default value in geom_bar is stat = “count”) to force ggplot2 to use the y aesthetic, 
# which we mapped to variable pop (population);
# note also use of coord_flip()
G <- ggplot(data = CIACountries_Sample) + 
  geom_bar(mapping = aes(x = ordered_countries, y = pop), stat = "identity") + coord_flip()
G

# %+%. By using this operator, we keep all the visual properties previously configured for the object, 
# but we change the dataset to which those properties are applied
CIACountries_Sample <- sample_n(CIACountries, size=25) # Another Sample
ordered_countries <- reorder(CIACountries_Sample$country,CIACountries_Sample$pop)

G <- G %+% CIACountries_Sample # Update the data mapped to graph G
G

# Density plots
ggplot(data = CIACountries, mapping = aes(pop)) + geom_density() + scale_x_log10()

# adjust the bandwidth
ggplot(data = CIACountries, mapping = aes(pop)) + geom_density(adjust = 2) + scale_x_log10()
ggplot(data = CIACountries, mapping = aes(pop)) + geom_density(adjust = 0.2) + scale_x_log10()

ggplot(data = diamonds, mapping = aes(x = color, y = carat)) + geom_boxplot()

ggplot(data = diamonds, mapping = aes(x = clarity, y = carat)) + geom_boxplot()
ggplot(data = diamonds, mapping = aes(x = cut, y = carat)) + geom_boxplot()


# topic 1 discussion - mpg data
MyGraph + facet_wrap(c("trans"), ncol = 1)
MyGraph + facet_grid(trans ~ .)

plot = ggplot(data = mpg, aes(x=hwy, y=cty)) + geom_point()
plot + facet_wrap(c('class'))
plot + facet_grid(class ~ .)

# topic 3 exercise - diamonds data
ggplot(data = diamonds, aes(x=clarity, y=carat)) + geom_boxplot()
ggplot(data = diamonds, mapping = aes(x = cut, y = carat)) + geom_boxplot()

ggplot(data = diamonds, mapping = aes(x = clarity, y = carat)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 3) + 
  geom_jitter(width = 0.1, alpha = 0.05, color = "blue")



