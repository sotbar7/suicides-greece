#########################################
#   EXAMINING SUICIDES IN GREECE & EU   #
#   SOTIRIS BARATSAS                    #
#########################################

library(lattice) 
library(ggplot2)
library(dplyr)
library(tidyverse)
library(shiny)

# Extensive data found on Kaggle: https://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016
df1 <- read.csv(file='suicides.csv', header=TRUE, sep=",", skipNul=T, dec=".")
df1$gdp_for_year.... <- gsub(",", "", df1$gdp_for_year....)
str(df1)
View(df1)

# The input file of the assignment with Economic Data
df2 <- read.csv(file="suicide II OECD.csv.txt", header=TRUE, sep=",", quote="", skipNul=T, fileEncoding="UCS-2LE")
View(df2)
df2$X.LOCATION<-factor(gsub("^\"\"$", "", df2$X.LOCATION))
levels(df2$X.LOCATION)
class(df2$X.LOCATION)


# Country names - Country Codes - Coordinates
df3<- read.csv(file='https://gist.githubusercontent.com/tadast/8827699/raw/7255fdfbf292c592b75cf5f7a19c16ea59735f74/countries_codes_and_coordinates.csv', sep=",")
df3$Alpha.3.code <- gsub(" ", "", df3$Alpha.3.code)
View(df3)
str(df3)

# The input file of the assignment with # of suicides
library("readxl")

df4 <- read_xls("suicide1.xls", skip=2, col_names=T)
str(df4)
df4$`GEO/SEX`<-factor(df4$`GEO/SEX`)
names(df4)[names(df4) == "GEO/SEX"] <- "Country"
str(df4)
# Reshaping the dataset from Wide to Long Format
library(reshape2)
df4 <- melt(df4, id.vars="Country")
names(df4)[names(df4) == "variable"] <- "Year"
names(df4)[names(df4) == "value"] <- "Suicides"
str(df4)
df4$Year<-as.integer(as.character(df4$Year))



# Extracting the full names of the countries
#index1 <- match(df2$X.LOCATION, df3$Alpha.3.code, nomatch=0)
countries <- levels(df4$Country)
years <- c(2011:2015)



keep <- (df1$country %in% countries) & (df1$year %in% years)

# Keeping only the rows with these specific countries and for the years 2011-2015
data <- df1[keep, ]
str(data)
data$country<-factor(data$country)
data$HDI.for.year<-NULL
data$country.year<-NULL

# Making the age column ordinal
data$age <- gsub(" years", "", data$age)
data$age <- factor(data$age, ordered = T, levels = c("5-14", "15-24", "25-34", "35-54", "55-74", "75+"))
View(data)


# Creating aggregates
data2 = aggregate(data$suicides_no,
                by = list(data$country, data$year),
                FUN = sum)




#################
#   P L O T S   #
#################

# ----------------------------------------------
# TOTAL SUICIDES TREND (ALL COUNTRIES) 2011-2015
# ----------------------------------------------

attach(df4)
df4 %>%
  group_by(Year) %>%
  summarize(suicides = sum(Suicides))  %>%
  ggplot(aes(x = Year, y = suicides)) + 
  geom_line(col = "mediumvioletred", size = 1) + 
  geom_point(col = "mediumvioletred", size = 2) + 
  labs(title = "Total Suicides Trend - 2011-2015", x = "Year", y = "Total No. of Suicides") + 
  scale_x_continuous(breaks = seq(2011, 2015, 1)) +
  theme_minimal()

ggsave("total_suicides_trend.jpg", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, dpi = 300, limitsize = TRUE)


# -----------------------
# TOTAL SUICIDES PER 100k
# -----------------------


general <- data %>%
  group_by(year) %>%
  summarize(population = sum(population), 
            suicides = sum(suicides_no), 
            suicides_per_100k = (suicides / population) * 100000)
greekdata <- data %>%
  group_by(year) %>%
  filter(country == "Greece") %>%
  summarize(population = sum(population), 
            suicides = sum(suicides_no), 
            suicides_per_100k = (suicides / population) * 100000)
p<-data.frame("Year"=greekdata$year, "Greece"=greekdata$suicides_per_100k, "Total" = general$suicides_per_100k)

p <- melt(p, id="Year")  # convert to long format
names(p)[names(p) == "variable"] <- "Country"
names(p)[names(p) == "value"] <- "Suicides"
p$Year<-as.integer(as.character(p$Year))


ggplot(data=p, aes(x=Year, y=Suicides, colour=Country)) +
  scale_x_continuous(breaks = seq(2011, 2015, 1)) + 
  scale_y_continuous(breaks = seq(0, 12)) +
  geom_point(size=2) + 
  geom_line(size = 1) +
  labs(title = "Suicides (per 100k)",
       subtitle = "Greece vs All Countries",
       x = "Year",
       y = "Suicides per 100k") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title=element_blank())

ggsave("greece_trend_vs_total.jpg", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, dpi = 300, limitsize = TRUE)


# -----------------------------
# SUICIDES PER 100k PER COUNTRY 
# -----------------------------

# Calculating Avg.Suicides for all countries
total_average <- (sum(as.numeric(data$suicides_no)) / sum(as.numeric(data$population))) * 100000
population = sum(population)
suicides = sum(suicides_no) 
suicides_per_100k = (suicides / population) * 100000
  
# custom.col <- rep(c("#1abc9c", "#2ecc71", "#3498db", "#9b59b6", "#34495e",
                # "#f1c40f", "#e67e22", "#e74c3c", "#c0392b", "#d35400"), length.out=33)

country <- data %>%
  group_by(country) %>%
  summarize(n = n(), 
            suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  arrange(desc(suicide_per_100k))

country$country <- factor(country$country, 
                          ordered = T, 
                          levels = rev(country$country))

ggplot(country, aes(x = country, y = suicide_per_100k, fill=country)) + 
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = total_average, linetype = 1, color = "grey", size = 0.5) +
  labs(title = "Suicides per 100k per Country",
       x = "Country", 
       y = "Suicides per 100k") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 36, 2)) +
  theme_minimal() +
  theme(legend.position="none")

ggsave("per_country.jpg", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, dpi = 300, limitsize = TRUE)



# ------------------
# SUICIDES BY GENDER
# ------------------

general <- data %>%
  group_by(sex) %>%
  summarize(population = sum(population), 
            suicides = sum(suicides_no), 
            suicides_per_100k = (suicides / population) * 100000)
greekdata <- data %>%
  group_by(sex) %>%
  filter(country == "Greece") %>%
  summarize(population = sum(population), 
            suicides = sum(suicides_no), 
            suicides_per_100k = (suicides / population) * 100000)
p<-data.frame("Gender"=greekdata$sex, "Greece"=greekdata$suicides_per_100k, "Total" = general$suicides_per_100k)

p <- melt(p, id="Gender")  # convert to long format
names(p)[names(p) == "variable"] <- "Country"
names(p)[names(p) == "value"] <- "Suicides"

ggplot(data=p, aes(x = Country, y = Suicides, fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Suicides (per 100k) by Gender",
       subtitles = "Greece vs. All European Countries",
       x = "Country", 
       y = "Suicides per 100k", 
       fill = "Gender")

ggsave("By_Gender_Greece_vs_All.jpg", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, dpi = 300, limitsize = TRUE)

# ------------------------------
# SUICIDES BY GENDER PER COUNTRY
# ------------------------------

country_long <- data %>%
  group_by(country, continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  mutate(sex = "OVERALL")

sex_country_long <- data %>%
  group_by(country, sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)

sex_country_wide <- sex_country_long %>%
  spread(sex, suicide_per_100k) %>%
  arrange(male - female)

sex_country_wide$country <- factor(sex_country_wide$country, 
                                   ordered = T, 
                                   levels = sex_country_wide$country)

country_gender_prop <- sex_country_wide %>%
  mutate(Male_Proportion = male / (female + male)) %>%
  arrange(Male_Proportion)

ggplot(country_gender_prop, aes(y = suicides_per_100k, x = country, fill = sex)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportions of suicides that are Male & Female, by Country", 
       x = "Country", 
       y = "Suicides per 100k",
       fill = "Sex") + 
  coord_flip()

sex_country_long$country <- factor(sex_country_long$country, 
                                   ordered = T,
                                   levels = country_gender_prop$country)

ggplot(rev(sex_country_long), aes(y = suicide_per_100k, x = country, fill = sex)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentage of Suicides by Gender", 
       x = "Country", 
       y = "Percentage of Suicides",
       fill = "Gender") + 
  coord_flip() +
  theme_minimal()

ggsave("percentage_suicides_by_gender.jpg", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, dpi = 300, limitsize = TRUE)


# ---------------------
# SUICIDES BY AGE GROUP
# ---------------------

general <- data %>%
  group_by(age) %>%
  summarize(population = sum(population), 
            suicides = sum(suicides_no), 
            suicides_per_100k = (suicides / population) * 100000)
greekdata <- data %>%
  group_by(age) %>%
  filter(country == "Greece") %>%
  summarize(population = sum(population), 
            suicides = sum(suicides_no), 
            suicides_per_100k = (suicides / population) * 100000)
p<-data.frame("Age"=greekdata$age, "Greece"=greekdata$suicides_per_100k, "Total" = general$suicides_per_100k)

p <- melt(p, id="Age")  # convert to long format
names(p)[names(p) == "variable"] <- "Country"
names(p)[names(p) == "value"] <- "Suicides"

ggplot(data=p, aes(x = Country, y = Suicides, fill = Age)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Suicides (per 100k) by Age Group",
       subtitles = "Greece vs. All European Countries",
       x = "Country", 
       y = "Suicides per 100k", 
       fill = "Age") +
  theme_minimal()
  

ggsave("Suicides_by_age_group.jpg", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, dpi = 300, limitsize = TRUE)


# ----------------------------------------
# TRELLIS: SUICIDES OVER TIME BY AGE GROUP
# ----------------------------------------

# We are gonna use the "facets" feature of ggplot in order to produce trellis-like plots for the different age groups

p <- data %>%
  group_by(age, year) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)
  
  ggplot(p, aes(x = year, y = suicide_per_100k, col = factor(age, ordered = F))) + 
  geom_point(size = 2) + 
  geom_line(size = 1) + 
  facet_grid(age ~ ., scales = "free_y") + 
  scale_x_continuous(breaks = seq(2011, 2015, 1)) +
  labs(title = "Suicides per 100k, by Age Group", 
       x = "Year", 
       y = "Suicides per 100k") + 
  theme_light() +
  theme(legend.position = "none")
  
  
ggsave("Suicides_by_age_group_trellis.jpg", plot = last_plot(), device = NULL, path = NULL,
         scale = 1, dpi = 300, limitsize = TRUE)


# ----------------------
# SUICIDES BY GENERATION
# ----------------------


general <- data %>%
  group_by(generation) %>%
  summarize(population = sum(population), 
            suicides = sum(suicides_no), 
            suicides_per_100k = (suicides / population) * 100000)
greekdata <- data %>%
  group_by(generation) %>%
  filter(country == "Greece") %>%
  summarize(population = sum(population), 
            suicides = sum(suicides_no), 
            suicides_per_100k = (suicides / population) * 100000)
p<-data.frame("Generation"=greekdata$generation, "Greece"=greekdata$suicides_per_100k, "Total" = general$suicides_per_100k)

p <- melt(p, id="Generation")  # convert to long format
names(p)[names(p) == "variable"] <- "Country"
names(p)[names(p) == "value"] <- "Suicides"

ggplot(data=p, aes(x = Country, y = Suicides, fill = Generation)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Suicides (per 100k) by Generation",
       subtitles = "Greece vs. All European Countries",
       x = "Country", 
       y = "Suicides per 100k", 
       fill = "Generation") +
  theme_minimal()



# --------------------------------
# SUICIDES BY GENERATION (STACKED)
# --------------------------------


ggplot(data=p, aes(x = Country, y = Suicides, fill = Generation)) + 
  geom_bar(stat = "identity", position = "fill") + 
  labs(title = "Suicides (per 100k) by Generation",
       subtitles = "Greece vs. All European Countries",
       x = "Country", 
       y = "Suicides per 100k", 
       fill = "Generation") +
  theme_minimal()


ggsave("Suicides_by_generation.jpg", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, dpi = 300, limitsize = TRUE)


# --------------------------
# SUICIDES VS GDP PER CAPITA
# --------------------------


p <- data %>%
  group_by(country) %>%
  summarize(suicides_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000, 
            gdp_per_capita = mean(gdp_per_capita....))

ggplot(p, aes(x = gdp_per_capita, y = suicides_per_100k)) + 
  geom_point(col="mediumvioletred") + 
  geom_smooth(method = "lm", aes(group = 1)) + 
  scale_x_continuous(breaks = seq(0, 120000, 10000)) + 
  labs(title = "Correlation between GDP (per capita) and Suicides per 100k", 
       subtitle = "Plot containing every country",
       x = "GDP (per capita) - USD", 
       y = "Suicides per 100k", 
       col = "Country") +
  theme_minimal()

ggsave("suicides_vs_gdp_pc.jpg", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, dpi = 300, limitsize = TRUE)


# ---------------------------------------------
# SUICIDES VS GDP PER CAPITA (GREECE) OVER TIME
# ---------------------------------------------


p <- data %>%
  group_by(country, year) %>%
  filter(country == "Greece") %>%
  summarize(suicides_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000, 
            gdp_per_capita = mean(gdp_per_capita....))
p$year<-as.integer(as.character(p$year))


gg <- ggplot(p, aes(x = year))
gg <- gg + geom_line(aes(y = suicides_per_100k, colour = "Suicides per 100k"))

# adding the gdp per capita data, transformed to match roughly the range of the suicides per 100k data
gg <- gg + geom_line(aes(y = gdp_per_capita/3600, colour = "GDP per Capita"))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
gg <- gg + scale_y_continuous(sec.axis = sec_axis(~.*3600, name = "GDP per capita [scaled 1:3600]"))

# modifying colours and theme options
gg <- gg + scale_colour_manual(values = c("dodgerblue3", "mediumvioletred"))
gg <- gg + labs(title="Suicides per 100k -vs- GDP per Capita (Trendlines)",
                subtitle="Greece",
                y = "Suicides per 100k",
                x = "Years",
                colour="Metric")
gg <- gg + theme_minimal()
gg <- gg + theme(legend.position = c(0.8, 0.9), legend.title=element_blank())
gg

ggsave("greece_trendline_suicides_vs_gdp_pc.jpg", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, dpi = 300, limitsize = TRUE)


# ---------
#  M A P S
# ---------
library(rworldmap)
p <- data %>%
  group_by(country) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)

pp <- joinCountryData2Map(p, joinCode = "NAME", nameJoinColumn = "country")

par(mar=c(0, 0, 0, 0)) # margins

mapCountryData(pp, 
               nameColumnToPlot="suicide_per_100k", 
               mapTitle="", 
               colourPalette = "heat", 
               oceanCol="skyblue1", 
               missingCountryCol="grey91", 
               mapRegion="europe",
               catMethod = "pretty")


# ----------------
# GREECE vs PI(G)S
# ----------------

library(gganimate)
library(gifski)
library(transformr)

p <- data %>%
  group_by(country, year, sex) %>%
  filter(country %in% c("Greece","Spain","Portugal","Ireland")) %>%
  summarize(suicides_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)
p$year<-as.integer(as.character(p$year))

ggplot(p, aes(factor(country), suicides_per_100k)) + 
  geom_boxplot() + 
  transition_states(
    sex,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')


# ----------------
# GREECE ANIMATED
# ----------------

library(gganimate)
library(gifski)
library(transformr)

p <- data %>%
  group_by(country, year, sex, age) %>%
  filter(country =="Greece") %>%
  summarize(suicides_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)
p$year<-as.integer(as.character(p$year))

ggplot(p, aes(factor(age), suicides_per_100k)) + 
  geom_boxplot() + 
  transition_states(
    sex,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') +
  labs(title = "Variability of suicides by gender (2011-2015)", 
       subtitle = "Female - vs- Male",
       x = "Age Groups", 
       y = "Suicides per 100k", 
       col = "Gender") +
  theme_minimal()


# ----------------------
# SUICIDES vs GDP TRENDS
# ----------------------

library(gapminder)

p <- data %>%
  group_by(country, year) %>%
  summarize(population = sum(population),
            suicides_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000,
            gdp_per_capita = mean(gdp_per_capita....))
p$year<-as.integer(as.character(p$year))

ggplot(p, aes(gdp_per_capita, suicides_per_100k, size = population, colour = country)) +
  geom_point(alpha = 0.7, show.legend = F) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'Suicides per 100k') +
  transition_time(year) +
  ease_aes('linear')



##########################
