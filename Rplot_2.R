#2: Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
#( fips == “24510”) from 1999 to 2008? Use the base plotting system to make a plot
#answering this question.

total_emissions <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year) %>%
  summarise(Emissions = sum(Emissions))

with(total_emissions,
     barplot(height=Emissions/1000, names.arg = year, col = color_range, 
             xlab = "Year", ylab = expression('PM'[2.5]*' in Kilotons'),
             main = expression('Baltimore, Maryland Emissions from 1999 to 2008'))
)