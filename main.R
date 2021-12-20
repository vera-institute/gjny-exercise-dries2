# =============================================================================
# Program Author: Ellen Dries
# Date: Dec 19, 2021
# Content: Bail Reform & Ethnic Disparities
# Notes: Creates two figures for applicant data task.
# =============================================================================

# =========================== Load Packages ===================================

library(RCurl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(naniar)
library(tidyquant)

# =========================== Load Data ========================================
setwd("~Documents/GitHub/gjny-exercise-dries2")

alb <-
  read.csv(file = "albany_admissions.csv")
nyc <-
  read.csv(file = "nyc_admissions.csv")
all <- rbind(alb, nyc)

# =========================== Create Variables for Graphing =====================

# Create binary variable for whether a case is eligible for bail
all %>% replace_with_na(replace = list(bail_eligible = "Unknown"))
all$bail_eligible_num <-
  ifelse(all$bail_eligible == 'Eligible', 1, 0)

# Create race grouping to focus on black NY residents
all$black <-
  ifelse(all$race == 'Black',
         'Black',
         'White, Asian, Native American or Other')

# Format dates as dates
all$release_date <- as.Date(all$release_date)
all$admission_date <- as.Date(all$admission_date)
all$Year <- format(all$admission_date, format = "%Y")

# Create counter to generate totals
all$counter <- 1

# Create a variable for days held based on admission_date and release_date. 
# Note: I'm not completely sure about what these lengths mean and would like more documentation.
all$days_held <- all$release_date - all$admission_date
all$days_held <- as.numeric(all$days_held)

# Binary variable for if released on a different day than admitted
all$held <- ifelse(all$days_held > 0, 1, 0)

# =========================== Filter Sample =====================
#Focus only on pretrial case observations
all <- dplyr::filter(all, admission_status == "Pretrial")
all <- dplyr::filter(all, bail_eligible = "Unknown")

# =========================== Graphs ===================================

# Line graph of count of bail eligible cases
# Note: add in moving average

df <- all %>%
  group_by(month = lubridate::floor_date(admission_date, "month"), race) %>%
  summarize(total_bail = sum(bail_eligible_num),
            total = sum(counter))

fig1 <- ggplot(df, aes(x = month, y = total_bail, group = race)) +
  #geom_line(aes(color = race)) +
  geom_ma(ma_fun = SMA, n = 3, aes(color = race), linetype = "solid") + 
  scale_color_brewer(palette = "Set1") +
  labs(
  title = "Count of Bail Eligible Cases by Race",
  subtitle = "3 Month Moving Average from NYC and Albany datasets",
  y = "Number of Cases",
  x = "Date",
)
print(fig1)
ggsave(file = "Fig1.jpeg")


# Figure 2: Bar graph displaying bail eligible pretrial cases

df1 <- all %>%
  group_by(race, Year) %>%
  summarize(total = sum(bail_eligible_num),
            percent = paste0("(", sprintf(
              "%0.1f", sum(bail_eligible_num) / sum(counter) * 100
            ), "%)"),  avg_daysheld = sprintf("%0.1f", sum(days_held, na.rm = T) / sum(counter, na.rm = T)),
            total_daysheld = sum(days_held, na.rm = T))


fig2 <-
  ggplot(data = df1,
         aes(x = race, y = avg_daysheld, fill = Year),
         label = percent) +
 # ylim(13.8, 56.2) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(
    aes(label = avg_daysheld),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 4
  ) +
  theme_minimal() +
  labs(
    title = "Average Days Held",
    subtitle = "Difference Between Admissions Date and Release Date",
    y = "Number of Days",
    x = "\n Race \n",
    caption = "From NYC and Albany admissions datasets. \n Cases where release date is not available are excluded."
  ) 
print(fig2)
ggsave(file = "Fig2.jpeg")



# Figure 3: Count of Bail Eligible Pretrial Cases by Charge Severity for Black New Yorkers
df3 <- all %>%
  group_by(race, Year, severity, charge) %>%
  summarize(
    total = sum(bail_eligible_num, na.omit = true),
    percent = paste0("(", sprintf(
      "%0.1f", sum(bail_eligible_num) / sum(counter) * 100
    ), "%)"),
    avg_daysheld = sum(days_held, na.rm = T) / sum(counter, na.rm = T),
    total_daysheld = sum(days_held, na.rm = T)
  )

# Look just a black New York residents
df_black <- dplyr::filter(df3, race == "Black")
df_black <- na.omit(df_black)

fig3 <- ggplot(data = df_black, aes(x = severity, y = total, fill = Year)) +
  ylim(0, 6000) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(
    aes(label = total),
    position = position_dodge(width = 0.9),
    vjust = -2,
    size = 4
  ) +
  geom_text(
    aes(label = percent),
    position = position_dodge(width = 0.9),
    vjust = -0.75,
    size = 3
  ) +
  theme_minimal() +
  labs(
    title = "Count of Bail Eligible Pretrial Cases by Charge Severity \n for Black New Yorkers",
    subtitle = "(Percent of Total Cases)",
    y = "Number of Cases",
    x = "\n Charge Severity \n",
    caption = "From NYC and Albany admissions datasets. \n Cases where bail eligibility is unknown are excluded the total cases."
  )


print(fig3)
ggsave(file = "Fig3.jpeg")




