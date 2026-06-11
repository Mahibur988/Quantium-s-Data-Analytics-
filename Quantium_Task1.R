## The objective of this analysis was to understand customer purchasing behaviour in the chips category and identify customer segments that contribute most to sales.

library(readxl)

# Customer data (CSV)
customer <- read.csv("E:/OneDrive - Loras College/Desktop/Quantam Data Science/QVI_purchase_behaviour.csv")

# Transaction data (Excel)
transaction <- read_excel("E:/OneDrive - Loras College/Desktop/Quantam Data Science/QVI_transaction_data.xlsx")

head(customer)
head(transaction)

dim(customer)
dim(transaction)
## Missing Values Check:

colSums(is.na(transaction))
colSums(is.na(customer))

## Summary(Transaction):
summary(transaction)
summary(customer)

## Product Names Examine:

head(transaction$PROD_NAME)
length(unique(transaction$PROD_NAME))

# I remove Salsa Product becasue its not the part of Quantium Task:

grep("salsa",
     transaction$PROD_NAME,
     ignore.case = TRUE,
     value = TRUE)
transaction <- transaction[
  !grepl("salsa",
         transaction$PROD_NAME,
         ignore.case = TRUE),
]
## Cheacking Quantity Outliers:
boxplot(transaction$PROD_QTY)
transaction[transaction$PROD_QTY > 10, ]

## Convert Date:
head(transaction$DATE)
transaction$DATE <- as.Date(
  transaction$DATE,
  origin = "1899-12-30"
)
head(transaction$DATE)

install.packages("stringr")
library(stringr)
transaction$PACK_SIZE <-
  as.numeric(
    str_extract(
      transaction$PROD_NAME,
      "\\d+"
    )
  )
head(transaction$PACK_SIZE)

## Creating Brand Column
transaction$BRAND <-
  word(transaction$PROD_NAME, 1)
table(transaction$BRAND)
data <- merge(
  transaction,
  customer,
  by = "LYLTY_CARD_NBR"
)
dim(data)
head(data)

##Total Sales

sum(data$TOT_SALES)

## Sales by Life Stage:
sales_lifestage <-
  aggregate(
    TOT_SALES ~ LIFESTAGE,
    data,
    sum
  )

sales_lifestage

## Sales by Premium Type:
sales_premium <-
  aggregate(
    TOT_SALES ~ PREMIUM_CUSTOMER,
    data,
    sum
  )

sales_premium

## Customer Segment Analysis:
segment_sales <-
  aggregate(
    TOT_SALES ~
      LIFESTAGE +
      PREMIUM_CUSTOMER,
    data,
    sum
  )

segment_sales
## Brand Analysis:

brand_sales <-
  aggregate(
    TOT_SALES ~ BRAND,
    data,
    sum
  )

brand_sales[
  order(
    -brand_sales$TOT_SALES
  ),
]

## pack Size Analysis:
pack_sales <-
  aggregate(
    TOT_SALES ~ PACK_SIZE,
    data,
    sum
  )

pack_sales[
  order(
    -pack_sales$TOT_SALES
  ),
]

## Average Spend per Customer:
avg_spend <-
  aggregate(
    TOT_SALES ~
      LIFESTAGE +
      PREMIUM_CUSTOMER,
    data,
    mean
  )

avg_spend

## Creating charts:
install.packages("ggplot2")
library(ggplot2)

## Sales by Life Stage:
ggplot(
  sales_lifestage,
  aes(
    x = LIFESTAGE,
    y = TOT_SALES
  )
) +
  geom_bar(
    stat = "identity"
  )
## Sale by Customer Segment:

ggplot(
  segment_sales,
  aes(
    x = LIFESTAGE,
    y = TOT_SALES,
    fill = PREMIUM_CUSTOMER
  )
) +
  geom_bar(
    stat = "identity"
  )
## Number of Customers by Segment:
customers_segment <- aggregate(
  LYLTY_CARD_NBR ~ LIFESTAGE + PREMIUM_CUSTOMER,
  data,
  function(x) length(unique(x))
)

customers_segment

## Average Chips Bought per Customer:
units_per_customer <- aggregate(
  PROD_QTY ~ LIFESTAGE + PREMIUM_CUSTOMER,
  data,
  mean
)

units_per_customer

## Average Price Per Unit:
data$UNIT_PRICE <- data$TOT_SALES / data$PROD_QTY

price_segment <- aggregate(
  UNIT_PRICE ~ LIFESTAGE + PREMIUM_CUSTOMER,
  data,
  mean
)

price_segment

## Customers by Segment:
library(ggplot2)

ggplot(customers_segment,
       aes(x=LIFESTAGE,
           y=LYLTY_CARD_NBR,
           fill=PREMIUM_CUSTOMER)) +
  geom_bar(stat="identity")

## Units per Customer:
ggplot(units_per_customer,
       aes(x=LIFESTAGE,
           y=PROD_QTY,
           fill=PREMIUM_CUSTOMER)) +
  geom_bar(stat="identity")

##Average Price:
ggplot(price_segment,
       aes(x=LIFESTAGE,
           y=UNIT_PRICE,
           fill=PREMIUM_CUSTOMER)) +
  geom_bar(stat="identity")

## Conclusion:From this analysis, I found that Mainstream customers contribute the most to chip sales, with Older Singles/Couples being the highest spending life-stage group. Kettle was the most popular brand and 175g was the most commonly purchased pack size. Based on these findings, focusing on Mainstream customers and maintaining strong availability of popular brands and pack sizes could help increase future sales and improve overall category performance.

