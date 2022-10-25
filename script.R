# Importar biblioteca
library(tidyverse)

# Ler tabela
data <- read_csv('../datasets/full-covid-data.csv'
                 , col_types=cols(date = col_date("%Y-%m-%d")))

# Examinar tabela
glimpse(data)

colnames(data)

# Filtrar dados que serão utilizados
confirmed_cases_world_2020 <- data %>% select(location, date, total_cases) %>%
  filter(location == "World" &  date <= "2020-03-20")

# Ver tabela filtrada
str(confirmed_cases_world_2020)

head(confirmed_cases_world_2020)

# Remover notação científica
options(scipen=999)

# Gerar gráfico
ggplot(confirmed_cases_world_2020, aes(date, total_cases)) + 
  geom_line() +
  labs(y = "Casos confirmados comulativos"
       , x = "")

# Filtrar dados que serão utilizados
confirmed_cases_china_2020 <- data %>% select(location, date, total_cases) %>%
  filter(location == "China" &  date <= "2020-03-20")

confirmed_cases_world_no_china <- confirmed_cases_world_2020 %>%
  mutate(total_cases = total_cases - confirmed_cases_china_2020$total_cases)

confirmed_cases_world_vs_china <- rbind(confirmed_cases_china_2020, confirmed_cases_world_no_china)

# Gerar gráfico
confirmed_cases_world_vs_china_graph <- ggplot(confirmed_cases_world_vs_china) +
  geom_line(aes(date, total_cases, color=location)) +
  labs(y = "Casos confirmados comulativos"
       , x = "")
confirmed_cases_world_vs_china_graph

who_events <- tribble(
  ~ date, ~ event,
  "2020-01-30", "Saúde global\nEmergência declarada",
  "2020-03-11", "Pandemia declarada",
  "2020-02-13", "Relatório de\nmudança da China"
) %>%
  mutate(date = as.Date(date))

# Using who_events, add vertical dashed lines with an xintercept at date
# and text at date, labeled by event, and at 100000 on the y-axis
confirmed_cases_world_vs_china_graph + 
  geom_vline(aes(xintercept = date), data=who_events, linetype="dashed") +
  geom_text(aes(x=date, label=event)
            , data=who_events, y=1.5e+5, vjust=1, size = 4, angle = 90)


# Filtrar China, de fev 15
china_after_feb15 <- confirmed_cases_world_vs_china %>% 
  filter(location=="China" , date >="2020-02-15");

# Adiciona um linha de tendência usando regressão linear, sem error bars
ggplot(china_after_feb15, aes(date, total_cases)) + 
  geom_line() +
  geom_smooth(method="lm", se=FALSE) +
  ylab("Cumulative confirmed cases")


# Adiciona um linha de tendência usando regressão linear, sem error bars
plt_not_china_trend_lin <- ggplot(confirmed_cases_world_no_china
                                  , aes(date, total_cases)) + 
  geom_line() +
  geom_smooth(method="lm", se=FALSE) +
  labs(y = "Casos confirmados comulativos", x = "")
plt_not_china_trend_lin


# Modifica o gráfico em escala logarítmica no eixo y
plt_not_china_trend_lin + scale_y_log10()


confirmed_cases_top_country <- data %>% 
  select(continent,location, date, total_cases) %>%
  filter(!is.na(continent), location != "China"
         , date <= "2020-03-20") %>%
  mutate(total_cases = if_else(is.na(total_cases)
                               , 0, total_cases)) %>%
  group_by(location) %>%
  summarise(total_cases = max(total_cases)) %>%
  top_n(8, total_cases) %>%
  arrange(-total_cases)

confirmed_cases_top_country


top10 <- c("Italy", "Spain", "United States", "Iran"
           , "Germany", "France", "South Korea"
           , "United Kingdom")
confirmed_cases_top10_outside_china <- data %>% 
  select(location, date, total_cases)  %>% 
  filter(location %in% top10 & date <= "2020-03-20")

# Using confirmed_cases_top7_outside_china, draw a line plot of
# cum_cases vs. date, colored by country
ggplot(confirmed_cases_top10_outside_china) +
  geom_line(aes(date, total_cases, colour=location)) +
  labs(y = "Casos confirmados comulativos", x = "")


# Outras análises

library(data.table)
library(funModeling)
library(Hmisc)
library(ggplot2)

data <- fread("../datasets/full-covid-data.csv")

# View(data)

# ===============================================================
#   1. Checar valores faltando, zeros, tipo de dados e valores únicos
#       (missing values, zeros, data type, unique values) 
# ===============================================================

# total rows, total columns
nrow(data)
ncol(data)

# basic profiling
df_status(data)

# data[, lapply(.SD, function(x) sum(is.na(x)))] count NA
# data[, lapply(.SD, function(x) sum(is.na(x))/ nrow(data))] count percent NA

# profiling categorical variables
freq(data)

# profiling numerical variables
# obs: funciona melhor com arquivos de extensão R
plot_num(data)


# non-graphical way
profiling_num(data[, .(male_smokers, female_smokers)])


# ===============================================================
#   2. Selecionar os dados de interesse
# ===============================================================

# Apenas Brasil
br <- data[location == "Brazil"]

# mostrar nomes das colunas
names(br)


# Selecionar colunas específicas
br <- br[, .(iso_code, location, date, total_vaccinations
             , people_vaccinated_per_hundred
             , people_fully_vaccinated_per_hundred)]

# Remover missing values (NA)
br <- br[!is.na(total_vaccinations)]
# br <- br[complete.cases(br)] outra forma de remover missing values


# ===============================================================
#   3. Visualization
# ===============================================================
br_graph <- ggplot(data=br, aes(x=date, y=people_vaccinated_per_hundred
                                , color=location))
br_graph + geom_line(size=1.2) + scale_y_continuous(breaks=c(0, 20, 40, 60, 80)
                                                    ,labels=scales::comma) +
  labs(titles= "% of vaccinated people in the Brazil",
       x= "Date", y="% vaccinated people")


br_graph <- ggplot(data=data[location %in% c("Netherlands", "France"
                                             , "United Kingdom", "Brazil") & 
                               !is.na(people_vaccinated_per_hundred)]
                   , aes(x=date, y=people_vaccinated_per_hundred
                         , color=location))
br_graph + geom_line(size=1.2) + scale_y_continuous(breaks=c(0, 20, 40, 60, 80)
                                                    ,labels=scales::comma) +
  labs(titles= "% of vaccinated people / some countries comparing Brazil",
       x= "Date", y="% vaccinated people")


data <- read_csv('../datasets/full-covid-data.csv'
                 , col_types=cols(date = col_date("%Y-%m-%d")))

head(data)


confirmed_cases_world_years <- data %>% select(location, date, total_cases) %>%
  filter(location == "World")
tail(confirmed_cases_world_years)


ggplot(data=confirmed_cases_world_years, aes(x = date, y = total_cases)) +
  geom_line() +
  labs(y = "Casos confirmados comulativos", x = "")


confirmed_cases_top <- data %>% 
  select(continent, location, date, total_cases) %>%
  filter(!is.na(continent)) %>%
  mutate(total_cases = if_else(is.na(total_cases)
                               , 0, total_cases)) %>%
  group_by(location) %>%
  summarise(total_case_max = max(total_cases)) %>%
  top_n(10, total_case_max) %>%
  arrange(-total_case_max)
confirmed_cases_top


confirmed_cases_country_years <- data %>% 
  select(location, date, total_cases) %>%
  filter(location %in% c("United States", "India", "France"
                         , "Brazil", "Germany", "South Korea"
                         , "United Kingdom", "Italy", "Japan"
                         , "Russia"))

confirmed_cases_country_years %>% ggplot(aes(x=date, y=total_cases, color=location)) + geom_line()


confirmed_cases_usa <- data %>% 
  select(location, date, total_cases) %>%
  filter(location == "United States")

plt_usa_trend_lin <- ggplot(confirmed_cases_usa
                            , aes(date, total_cases)) + 
  geom_line() +
  geom_smooth(method="lm", se=FALSE) +
  labs(y = "Casos confirmados comulativos", x = "")
plt_usa_trend_lin


total_deaths_top_country <- data %>% 
  select(continent, location, date, total_deaths) %>%
  mutate(total_deaths = if_else(is.na(total_deaths)
                                , 0, total_deaths)) %>%
  filter(!is.na(continent)) %>%
  group_by(location) %>%
  summarise(max_total_deaths = max(total_deaths)) %>%
  top_n(10, max_total_deaths) %>%
  arrange(-max_total_deaths)

total_deaths_top_country


total_deaths_top_country_graph <- data %>% 
  select(continent, location, date, total_deaths) %>%
  filter(location %in% c("United States", "India", "France"
                         , "Brazil", "Indonesia", "Peru"
                         , "United Kingdom", "Italy", "Mexico"
                         , "Russia"))

total_deaths_top_country_graph %>% ggplot(aes(x=date, y=total_deaths, color=location)) + geom_line()


total_vacci_hundred_top_country <- data %>% 
  select(continent, location, date, people_vaccinated) %>%
  mutate(people_vaccinated=if_else(is.na(people_vaccinated), 0, people_vaccinated)) %>%
  filter(!is.na(continent)) %>%
  group_by(location) %>%
  summarise(max_total_vaccinations = max(people_vaccinated)) %>%
  top_n(10, max_total_vaccinations) %>%
  arrange(-max_total_vaccinations)

total_vacci_hundred_top_country


people_vaccinated_top_country_graph <- data %>% 
  select(continent, location, date, people_vaccinated) %>%
  filter(location %in% c("United States", "India", "China"
                         , "Brazil", "Indonesia", "Japan"
                         , "Vietnam", "Pakistan", "Mexico"
                         , "Bangladesh"))

people_vaccinated_top_country_graph %>% ggplot(aes(x=date, y=people_vaccinated, color=location)) + geom_line(size=1.2) + labs(title="10 paises com maior numero absoluto de doses aplicadas") 


