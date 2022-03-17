# Required packages 
# Importing the neccesary library
library(readr) 
library(knitr)
library(kableExtra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MVN)
library(outliers)
library(forecast)

# read the data

happiness <- read.csv("2019.csv",
                      stringsAsFactors = FALSE)

Continents <- read.csv("Countries-Continents.csv",
                       stringsAsFactors = FALSE)


#Displaying head of two datasets
head(happiness)

head(Continents)


#Merging Two Datasets
df <- happiness %>% left_join(Continents, c("Country.or.region" = "Country"))

head(df)


# Understand
# structure of data
str(df)

df$Continent <- factor(df$Continent, 
                       levels = c("Africa","Asia","Europe","North America","Oceania","South America"),
                       labels = c("Africa","Asia","Europe","North America","Oceania","South America"),
                       ordered = FALSE)
levels(df$Continent)

str(df)

# Tidy & Manipulate Data I 
df <- df %>% pivot_longer(names_to = "Criteria", values_to = "Criteria_score", cols = 4:9)

df$Criteria <- as.factor(df$Criteria)
levels(df$Criteria)
str(df)

head(df)

# Tidy & Manipulate Data II
df <- df %>% group_by(Country.or.region) %>% mutate(Dystopia_Residual = Score - sum(Criteria_score))
df

# Scan I
# missing value
sum(is.na(df))

colSums(is.na(df))

df[!complete.cases(df),] %>% select(Country.or.region, Continent) %>% unique()

# handling missing values
df$Continent[df$Country.or.region=="United States"] <- "North America"
df$Continent[df$Country.or.region=="Czech Republic"] <- "Europe"
df$Continent[df$Country.or.region=="Taiwan"] <- "Asia"
df$Continent[df$Country.or.region=="Trinidad & Tobago"] <- "South America"
df$Continent[df$Country.or.region=="Kosovo"] <- "Europe"
df$Continent[df$Country.or.region=="South Korea"] <- "Asia"
df$Continent[df$Country.or.region=="Northern Cyprus"] <- "Europe"
df$Continent[df$Country.or.region=="Russia"] <- "Europe"
df$Continent[df$Country.or.region=="Hong Kong"] <- "Asia"
df$Continent[df$Country.or.region=="North Macedonia"] <- "Europe"
df$Continent[df$Country.or.region=="Congo (Brazzaville)"] <- "Africa"
df$Continent[df$Country.or.region=="Palestinian Territories"] <- "Asia"
df$Continent[df$Country.or.region=="Burkina Faso"] <- "Africa"
df$Continent[df$Country.or.region=="Congo (Kinshasa)"] <- "Africa"
df$Continent[df$Country.or.region=="Myanmar"] <- "Asia"

NA_counts <- function(x)
{
  if (is.numeric(x)) (is.infinite(x) | is.nan(x) | is.na(x))
}
sapply(df, function(x) sum(NA_counts(x)))


# Scan II 
par(mar=c(1,1,1,1))

numeric <- df[, c(3,7)] %>% unique()
numeric

#Score
par(mfrow = c(1,2))

hist(numeric$Score, 
     ylab = "frequency",
     xlab = "Happiness Score",
     main = "Histogram of Happiness Score")

qqnorm(numeric$Score)
qqline(numeric$Score,
       col="red",
       lwd = 1,
       lty = 2)

par(mfrow = c(1,1))

# shapiro-wilk test
shapiro.test(numeric$Score)

z.Score <- numeric$Score %>%  scores(type = "z")
z.Score %>% summary()

which(abs(z.Score) >3 )

# boxplots
ggplot(data = df, mapping = aes(y = Score)) + 
  geom_boxplot() + 
  ggtitle("Boxplot of Happiness Score")

par(mfrow = c(1,2))
hist(numeric$Dystopia_Residual, 
     ylab = "frequency",
     xlab = "Dystopia_Residual",
     main = "Histogram of Dystopia_Residual")

# qq-plot
qqnorm(numeric$Dystopia_Residual)
qqline(numeric$Dystopia_Residual,
       col="red",
       lwd = 1,
       lty = 2)

par(mfrow = c(1,1))

shapiro.test(numeric$Dystopia_Residual)

z.Score <- df$Score %>%  scores(type = "z")
z.Score %>% summary()

# standardised Residuals
which(abs(z.Score) >3 )

ggplot(data = df, mapping = aes(y = Dystopia_Residual)) + 
  geom_boxplot()


#Criteria_score

ggplot(data = df, mapping = aes(x = Criteria, y = Criteria_score)) + 
  geom_boxplot()

#Two-Variable
# Scatterplot
ggplot(data = numeric) + 
  geom_point(mapping = aes(x = Score, y = Dystopia_Residual)) +
  geom_smooth(
    mapping = aes(x = Score, y = Dystopia_Residual)
  )


#Multivariate
  
par(mar=c(1,1,1,1))

# mvn
results <- mvn(data = numeric,
               multivariateOutlierMethod = "quan", 
               showOutliers = TRUE)


# Transform
#Dystopia_Residual
par(mfrow = c(3,2))

boxcox <- BoxCox(numeric$Dystopia_Residual,lambda = "auto") %>% round(3)
hist(boxcox)

log_dystopia <- log10(numeric$Dystopia_Residual)
hist(log_dystopia)

z_df <- scale(numeric$Dystopia_Residual, center = TRUE, scale = TRUE)
hist(z_df)

ln_dystopia <- log(numeric$Dystopia_Residual)
hist(ln_dystopia)

sqrt_dystopia <- sqrt(numeric$Dystopia_Residual)
hist(sqrt_dystopia)

center_df <-scale(numeric$Dystopia_Residual, center = TRUE, scale = FALSE)
hist(center_df)

par(mfrow = c(1,1))


shapiro.test(boxcox)
shapiro.test(log_dystopia)
shapiro.test(z_df)
shapiro.test(ln_dystopia)
shapiro.test(sqrt_dystopia)
shapiro.test(center_df)


#Histogram of Each Criteria
  
par(mfrow = c(3,2))
df_GDP <- df %>% filter(Criteria=="GDP.per.capita")
hist(df_GDP$Criteria_score)

df_social <- df %>% filter(Criteria=="Social.support")
hist(df_social$Criteria_score)

df_healthy <- df %>% filter(Criteria=="Healthy.life.expectancy")
hist(df_healthy$Criteria_score)

df_freedom <- df %>% filter(Criteria=="Freedom.to.make.life.choices")
hist(df_freedom$Criteria_score)

df_geno <- df %>% filter(Criteria=="Generosity")
hist(df_geno$Criteria_score)

df_corrupt <- df %>% filter(Criteria=="Perceptions.of.corruption")
hist(df_corrupt$Criteria_score)

par(mfrow = c(1,1))


# decreasing the skewness
z_df <- scale(df_geno$Criteria_score, center = TRUE, scale = TRUE)
hist(z_df)

sqrt_dystopia <- sqrt(df_geno$Criteria_score)
hist(sqrt_dystopia)

center_df <-scale(df_geno$Criteria_score, center = TRUE, scale = FALSE)
hist(center_df)

par(mfrow = c(1,1))

shapiro.test(z_df)
shapiro.test(sqrt_dystopia)
shapiro.test(center_df)