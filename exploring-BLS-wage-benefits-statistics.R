library(ggplot2)
library(tidyr)
library(blscrapeR)
library(dplyr)

###########
# GET DATA
###########

# Get FIPS codes
areaCodes <- blscrapeR::area_titles
head(areaCodes)

# get industry description codes (NIACS codes)
industryCodes <- blscrapeR::niacs
head(industryCodes)

# get industry size codse 
sizeCodes <- blscrapeR::size_titles
head(sizeCodes)

##############################################################################
### Useful BLS categories
## - OES (Occupational Employment Statistics) includes employment, benefits, and 
##   wages segmented by metro area and occupation.
## - Employer Cost for Employee Compensation
## - National Compensation Survey-Benefits includes survey data of those who have 
##   benefits available and who takes advantage of those benefits.
## - Work Stoppages Data
## - Other wage data can be found in the CPS, CES, and QCEW, which are covered in 
##   the Employment seciton of these vignettes.
##############################################################################

##############################################################################
## OES
## The OES contains similar wage data found in the CPS, but often has more 
## resolution in certain geographic areas. Unlike the CPS, the OES is an annual 
## survey and does not keep time series data.
##############################################################################

## Compare average hourly wage of Computer and Information Systems Managers in 
## Orlando, FL to those in San Jose, CA. Notice, below the survey only returns 
## values for 2015: 
df <- bls_api(c("OEUM003674000000011302103", "OEUM004194000000011302108"))
head(df)
wage_diff <- df[2,4] - df[1,4]
wage_diff

##  Most recent Annual mean wage for All Occupations in All Industries in the 
## United States.
df <- bls_api("OEUN000000000000000000004")
df
usa_annual_mean_wage <- df$value
usa_annual_mean_wage

### Employer Cost for Employee Compensation
## Time series data on how much employers pay for employee benefits as a total
## cost and as a percent of employee wages and salaries.

# The total cost of benefits per hour work and also see what percentage that is 
# of the total compensation:
df <- bls_api(c("CMU1030000000000D", "CMU1030000000000P"))
# inspect
str(df)

# transform period from list to vector
p <- as.factor(unlist(df$period))
class(p)
df$period <- p
# inspect
str(df)

# get mean benefits per quarter
df %>%
  group_by(period) %>%
  summarize(mean = mean(value))

### National Compensation Survey-Benefits
## Data on how many Americans have access to certain benefits.

# Percentage of those who have access to paid vacation days and who have 
# access to Health insurance through their employers:
df <- bls_api(c("NBU10500000000000033030", "NBU11500000000000028178"))

# Spread series ids and rename columns to human readable format.
df.sp <- spread(df, seriesID, value) %>%
    rename("pct_paid_vacation"=NBU10500000000000033030, 
           "pct_health_ins"=NBU11500000000000028178)

# Value data are in whole numbers but represent percentages. 
#    Fix this to avoid confusion.
df.sp$pct_paid_vacation <- df.sp$pct_paid_vacation*0.01
df.sp$pct_health_ins <- df.sp$pct_health_ins*0.01
df.sp

# graph the data
# bar graph, year on x axis, percent on y axis, color filled by year

ggplot(df.sp, aes(x = year, y = pct_health_ins, fill = year)) +
  geom_bar(stat = "identity", position = position_dodge()) + 
  labs(title = "% of Employees with Health Insurance")

# bar graph, year on x axis, percent on axis
ggplot(df.sp, aes(x = year, y = pct_paid_vacation, color = year)) + 
  geom_line() + 
  geom_point()

### QCEW API: Quarterly Census of Employment and Wages 
## (QCEW) is a division of the BLS. Some QCEW time-series data is available via 
## the main BLS API, but the QCEW also houses quarterly data on their own API. 
## The QCEW API returns employment and wage statistics sliced by area, 
## industry type, or industry size.
## For further documentation on the QCEW API, you can refer to the QCEW website.

## The QCEW API is structured differently than the main BLS API. For this reason, the blscrapeR package includes a function for extracting these data.

## IF we wanted to gather the quarterly employment and wage information for the
## Software Publishing industry in Q1 of 2016, we could run the following code:
dat <- qcew_api(year=2016, qtr=1, slice="industry", sliceCode=5112)
head(dat)

## The qcew_api() function accepts various codes in the sliceCode argument.
## The sliceCode that is used will depend on the slice we wish to see. 
## There are three built-in data sets to help with our sliceCode selections.

# Area Description Data (FIPS codes)
areaCodes <- blscrapeR::area_titles
head(areaCodes)


