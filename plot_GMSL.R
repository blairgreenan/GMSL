# Load necessary libraries
library(readr)
library(ggplot2)
library(dplyr)

# Set up the Import Options and import the data
file_path <- "AR6_FGD_assessment_timeseries_GMSL_tide_gauge.csv"
gmsl <- read_csv(file_path, col_names = c("Year", "CentralEstimate", "StructuralUnc1sigma", "InternalUnc1sigma", "TotalUnc1sigma"), skip = 2)

# Rename columns and calculate additional columns
gmsl <- gmsl %>%
  mutate(
    VarName1 = Year,
    ObservedGMSLmean = CentralEstimate,
    ObservedGMSLlower = CentralEstimate - TotalUnc1sigma,
    ObservedGMSLupper = CentralEstimate + TotalUnc1sigma
  )

# Read the text file
filename <- 'GMSL_TPJAOS_5.1_1992_2024.txt'
startRow <- 53

gsat <- read_fwf(
  filename,
  fwf_cols(
    HDR = 3, Glob = 5, alMeanSeaLe = 14,
    VarName5 = 10, VarName6 = 10, VarName7 = 10, VarName8 = 10,
    VarName9 = 10, VarName10 = 10, VarName11 = 10, VarName12 = 10,
    VarName13 = 10
  ),
  skip = startRow - 1
)

# Calculate offsets
ii_gsat <- gsat %>% filter(alMeanSeaLe >= 1993 & alMeanSeaLe <= 2010)
soff <- mean(ii_gsat$VarName9, na.rm = TRUE)

ii_gmsl <- gmsl %>% filter(VarName1 >= 1993 & VarName1 <= 2010)
toff <- mean(ii_gmsl$ObservedGMSLmean, na.rm = TRUE)

# Plotting
ggplot() +
  geom_line(data = gsat, aes(x = alMeanSeaLe, y = VarName9 - soff + toff), color = "red", linewidth = 1.5) +
  geom_line(data = gmsl, aes(x = VarName1, y = ObservedGMSLmean), color = "blue", linewidth = 1.5) +
  geom_ribbon(data = gmsl, aes(x = VarName1, ymin = ObservedGMSLlower, ymax = ObservedGMSLupper), fill = "blue", alpha = 0.25) +
  scale_x_continuous(name = " ", limits = c(1900, 2025), breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  scale_y_continuous(name = expression(paste("Global mean sea level (mm)")), limits = c(-200, 100)) +
#  theme_minimal() +
  theme(text = element_text(size = 14)) +
  annotate("text", label = "Baseline Period 1995-2014", x = 2010, y = -180,
           color = "black", size = 4)


# Save the plot
ggsave("figure_gmslr_2.png", dpi = 1200, width = 10, height = 6, scale = 0.75)
