#4: Across the United States, how have emissions from coal combustion-related sources changed from 1999 to 2008?

coal_SCC <- SCC[grep("[Cc][Oo][Aa][Ll]", SCC$EI.Sector), "SCC"]
coal_NEI <- NEI %>% filter(SCC %in% coal_SCC)
coal_summary <- coal_NEI %>% group_by(year) %>% summarise(Emissions = sum(Emissions))

plot <- ggplot(coal_summary, aes(x=year, y=round(Emissions/1000,2), label=round(Emissions/1000,2), fill=year)) +
  geom_bar(stat="identity") + ylab(expression('PM'[2.5]*' Emissions in Kilotons')) + xlab("Year") +
  geom_label(aes(fill = year),colour = "white", fontface = "bold") +
  ggtitle("Coal Combustion Emissions, 1999 to 2008.")
print(plot)