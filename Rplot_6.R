#6: Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles
# County, California ( == “”). Which city has seen greater changes over time in motor vehicle emissions?

SCC_Vehicles <- SCC %>%
  filter(grepl('[Vv]ehicle', SCC.Level.Two)) %>%
  select(SCC, SCC.Level.Two)

Tot_Emi_Two_Locs <- NEI %>%
  filter(fips == "24510" | fips == "06037") %>%
  select(fips, SCC, Emissions, year) %>%
  inner_join(SCC_Vehicles, by = "SCC") %>%
  group_by(fips, year) %>%
  summarise(Total_Emissions = sum(Emissions, na.rm = TRUE)) %>%
  select(Total_Emissions, fips, year)

Tot_Emi_Two_Locs$fips <- gsub("24510", "Baltimore City", Tot_Emi_Two_Locs$fips)
Tot_Emi_Two_Locs$fips <- gsub("06037", "Los Angeles County", Tot_Emi_Two_Locs$fips)

Two_Locs_Plot <- ggplot(Tot_Emi_Two_Locs, aes(x = factor(year), y = Total_Emissions, fill = fips)) +
  geom_bar(stat = "identity", width = 0.7) +
  facet_grid(.~fips) + 
  labs(x = "Year", y = "Emissions (Tons)", title = "Comparison of Motor Vehicle Related Emissions Between Baltimore City and Los Angeles From 1999 - 2008") +
  theme(plot.title = element_text(size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text.x = element_text(size = 12)) +
  theme_dark() + 
  ggsave("plot6.png", width = 30, height = 30, units = "cm")

print(Two_Locs_Plot)