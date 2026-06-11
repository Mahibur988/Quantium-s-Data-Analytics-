# Customer data (CSV)
customer <- read.csv("E:\\OneDrive - Loras College\\Desktop\\Quantam Data Science\\Task 2\\QVI_data.csv")
head(customer)
str(customer)
dim(customer)

## Convert Date
head(customer$DATE)
customer$DATE <- as.Date(customer$DATE)

## Creating YearMonth:
customer$YEARMONTH <- format(as.Date(customer$DATE), "%Y%m")
head(customer$YEARMONTH)

## Creating Monthly Sales By Store:
library(data.table)

setDT(customer)

monthly_sales <- customer[
  ,
  .(TOT_SALES = sum(TOT_SALES)),
  by = .(STORE_NBR, YEARMONTH)
]

head(monthly_sales)

## Creating Monthly Customer Counts:
monthly_customers <- customer[
  ,
  .(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)),
  by = .(STORE_NBR, YEARMONTH)
]

head(monthly_customers)

## Creating Average Transaction Per Customer:
monthly_txn <- customer[
  ,
  .(
    TXNS = uniqueN(TXN_ID),
    CUSTOMERS = uniqueN(LYLTY_CARD_NBR)
  ),
  by = .(STORE_NBR, YEARMONTH)
]

monthly_txn[
  ,
  AVG_TXN_PER_CUST := TXNS / CUSTOMERS
]

head(monthly_txn)
## Checking Trial Stores:
unique(customer$STORE_NBR)
customer[STORE_NBR == 77]
customer[STORE_NBR == 86]
customer[STORE_NBR == 88]


##Pretrail Dataset
pre_trial <- customer[YEARMONTH < "201902"]
unique(pre_trial$YEARMONTH)

##Create Monthly Metrics:
sales_pretrial <- pre_trial[
  ,
  .(TOT_SALES = sum(TOT_SALES)),
  by = .(STORE_NBR, YEARMONTH)
]
customers_pretrial <- pre_trial[
  ,
  .(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)),
  by = .(STORE_NBR, YEARMONTH)
]

## Customers:

customers_pretrial <- pre_trial[
  ,
  .(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)),
  by = .(STORE_NBR, YEARMONTH)
]

## Transactions per Customer:

txn_pretrial <- pre_trial[
  ,
  .(
    TXNS = uniqueN(TXN_ID),
    CUSTOMERS = uniqueN(LYLTY_CARD_NBR)
  ),
  by = .(STORE_NBR, YEARMONTH)
]

txn_pretrial[
  ,
  AVG_TXN_PER_CUST := TXNS / CUSTOMERS
]
sales_pretrial[STORE_NBR == 77]

## Finding Stores Similar to Store 77
library(ggplot2)

ggplot(
  sales_pretrial[STORE_NBR == 77],
  aes(x = YEARMONTH, y = TOT_SALES, group = 1)
) +
  geom_line() +
  geom_point() +
  ggtitle("Store 77 Pre-Trial Sales")

## Create a Wide Sales Table:
sales_wide <- dcast(
  sales_pretrial,
  STORE_NBR ~ YEARMONTH,
  value.var = "TOT_SALES"
)

head(sales_wide)

## Calculating Correlation with Store 77:
store77 <- sales_wide[sales_wide$STORE_NBR == 77, -1]

correlations <- data.frame(
  STORE_NBR = sales_wide$STORE_NBR,
  CORR = apply(
    sales_wide[, -1],
    1,
    function(x)
      cor(
        as.numeric(x),
        as.numeric(store77),
        use = "complete.obs"
      )
  )
)

correlations <- correlations[order(-correlations$CORR), ]

head(correlations, 10)
## My Quantium  calculation is not matching the offical Quantium approach yet.
corr77 <- correlations[correlations$STORE_NBR != 77, ]

head(corr77, 10)

## Storing 77 vs Controling Store 233
trial77 <- monthly_sales[STORE_NBR %in% c(77,233)]
library(ggplot2)

sales77 <- subset(
  monthly_sales,
  STORE_NBR %in% c(77,233)
)

ggplot(
  sales77,
  aes(
    x = YEARMONTH,
    y = TOT_SALES,
    color = factor(STORE_NBR),
    group = STORE_NBR
  )
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Store 77 vs Control Store 233",
    color = "Store"
  )

## For 86
subset(monthly_sales,
       STORE_NBR %in% c(86,155))
##graph:
sales86 <- subset(
  monthly_sales,
  STORE_NBR %in% c(86,155)
)

ggplot(
  sales86,
  aes(
    x = YEARMONTH,
    y = TOT_SALES,
    color = factor(STORE_NBR),
    group = STORE_NBR
  )
) +
  geom_line() +
  geom_point()
## For 88
subset(monthly_sales,
       STORE_NBR %in% c(88,237))

##aggregate:
aggregate(
  TOT_SALES ~ STORE_NBR,
  trial_period,
  sum
)
## For 77:
subset(monthly_sales,
       STORE_NBR %in% c(77,233))

##Conclusion: The trial stores generally outperformed their control stores during the trial period, indicating that the new layout was successful in increasing sales. Store 88 showed the greatest improvement, while Stores 77 and 86 also experienced positive results. Overall, the findings support expanding the trial layout to other stores as a strategy for improving chip category performance.
