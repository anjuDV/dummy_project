# Loading the data------

#load required libraries
library(dplyr)
library(ggplot2)
#load the data-------

# load the transaction data
transactionData <- "/Users/HP/Documents/QVI_transaction_data.csv"

# load the customer data
customerData <- "/Users/HP/Documents/QVI_purchase_behaviour.csv"


#check the structure of the data
str(chips)

#Data Cleaning and Feature Engineering-------

chips$DATE <- as.Date(chips$DATE, "%d/%m/%Y")
chips$PACK_SIZE <- as.numeric(gsub("[^0-9]", "", chips$PROD_NAME))
chips$BRAND <- gsub("([[:alpha:]]+)(.*)", "\\1", chips$PROD_NAME)
chips$CUSTOMER_SEGMENT <- paste(chips$LIFESTAGE, chips$PREMIUM_CUSTOMER, sep = " - ")



# Exploratory Data Analysis--------

summary(chips)

chips <- chips[chips$TOT_SALES >= 0, ]

#total sale

ggplot(chips, aes(x = TOT_SALES)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(labels = scales::dollar_format(scale = 0.001)) +
  labs(x = "Total Sales", y = "Count", title = "Distribution of Total Sales")


#pack size
ggplot(chips, aes(x = PACK_SIZE)) + geom_histogram(bins = 30)


str(transactionData)
head(transactionData)

#### Convert DATE column to a date format
transactionData[, DATE := as.Date(as.character(DATE), format = "%Y%m%d")]
str(transactionData)

# convert to data.table
library(data.table)
transactionData <- as.data.table(transactionData)
 

# convert DATE to date format
transactionData$DATE <- as.Date(as.character(transactionData$DATE), format = "%Y%m%d")
 
transactionData <- data.frame(transactionData)
summary(transactionData$PROD_NAME)



# Split words in PROD_NAME column
prod_words <- strsplit(as.character(transactionData$PROD_NAME), " ")

# Count frequency of each word
word_counts <- table(unlist(prod_words))

# Sort by frequency and show top 20 words
word_counts <- sort(word_counts, decreasing = TRUE)
head(word_counts, n = 20)


prodName <- transactionData$PROD_NAME

# Load required packages
library(data.table)

# Define transactionData
transactionData <- data.table(
  DATE = c(43831, 43832, 43832, 43832, 43833),
  STORE_NBR = c(1, 2, 3, 4, 5),
  TXN_ID = c(1, 2, 3, 4, 5),
  PROD_NBR = c(1, 2, 3, 4, 5),
  PROD_NAME = c("Smiths Crinkle Cut Chips Chicken 170g",
                "Kettle Tortilla ChpsHny&Jlpno Chili 150g",
                "Smiths Chip Thinly S/Cream&Onion 175g",
                "Kettle Tortilla ChpsFeta&Garlic 150g",
                "Smiths Crinkle Chips Salt & Vinegar 330g")
)

# Convert DATE to date format
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")

# Examine the words in PROD_NAME
productWords <- data.table(unlist(strsplit(unique(transactionData[, PROD_NAME]), " ")))
setnames(productWords, 'words')

# Remove digits and special characters
productWords[, isChips := grepl("^[[:alpha:]]+$", words)]
productWords <- productWords[isChips == TRUE]
productWords[, words := tolower(words)]

# Count frequency of each word
wordFreq <- productWords[, .N, by = words]
setnames(wordFreq, 'N', 'freq')

# Sort words by frequency
wordFreq <- wordFreq[order(-freq)]
head(wordFreq, 10)

string <- "Smiths Crinkle Cut Chips Chicken 170g"
string_without_digits <- gsub("\\d", "", string)
print(string_without_digits)



productWords[, words := gsub("[^[:alpha:]]", "", words)]


# Load required packages
library(data.table)

# Define transactionData
transactionData <- data.table(
  DATE = c(43831, 43832, 43832, 43832, 43833),
  STORE_NBR = c(1, 2, 3, 4, 5),
  TXN_ID = c(1, 2, 3, 4, 5),
  PROD_NBR = c(1, 2, 3, 4, 5),
  PROD_NAME = c("Smiths Crinkle Cut Chips Chicken 170g",
                "Kettle Tortilla ChpsHny&Jlpno Chili 150g",
                "Smiths Chip Thinly S/Cream&Onion 175g",
                "Kettle Tortilla ChpsFeta&Garlic 150g",
                "Smiths Crinkle Chips Salt & Vinegar 330g")
)

# Convert DATE to date format
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")

# Examine the words in PROD_NAME
productWords <- data.table(unlist(strsplit(unique(transactionData[, PROD_NAME]), " ")))
setnames(productWords, 'words')

# Remove digits and special characters
productWords[, isChips := grepl("^[[:alpha:]]+$", words)]
productWords <- productWords[isChips == TRUE]
productWords[, words := tolower(words)]

# Remove salsa products
productWords <- productWords[!grepl("salsa", words)]

# Count frequency of each word
wordFreq <- productWords[, .N, by = words]
setnames(wordFreq, 'N', 'freq')

# Sort words by frequency
wordFreq <- wordFreq[order(-freq)]
head(wordFreq, 10)


#Remove salsa products
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL]

#Check summary statistics
summary(transactionData)




# Define transactionData with PROD_QTY column
transactionData <- data.table(
  DATE = c(43831, 43832, 43832, 43832, 43833),
  STORE_NBR = c(1, 2, 3, 4, 5),
  TXN_ID = c(1, 2, 3, 4, 5),
  PROD_NBR = c(1, 2, 3, 4, 5),
  PROD_NAME = c("Smiths Crinkle Cut Chips Chicken 170g",
                "Kettle Tortilla ChpsHny&Jlpno Chili 150g",
                "Smiths Chip Thinly S/Cream&Onion 175g",
                "Kettle Tortilla ChpsFeta&Garlic 150g",
                "Smiths Crinkle Chips Salt & Vinegar 330g"),
  PROD_QTY = c(2, 3, 1, 2, 200)
)


outlier <- transactionData[PROD_QTY == 200]
outlier

# Convert DATE to date format
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")

# Summarise the data to check for nulls and possible outliers
summary(transactionData)


# Filter the dataset to find transactions by loyalty card number 226000
loyalty_card_226000 <- transactionData[transactionData$LOYALTY_CARD_NBR == "226000",]

# Check if the customer had any other transactions
other_transactions <- transactionData[transactionData$LYLTY_CARD_NBR == loyalty_card_226000$LYLTY_CARD_NBR &
                                        transactionData$TXN_ID != loyalty_card_226000$TXN_ID,]

# Remove the customer's transactions from the dataset
transactionData <- transactionData[transactionData$LOYALTY_CARD_NBR != "226000",]


transactionData <- transactionData[transactionData$LOYALTY_CARD_NBR != "226000", ]


# Count number of transactions by date
transactions_by_date <- table(transactionData$DATE)

# Display the transactions by date
transactions_by_date



library(ggplot2)

# Create sequence of dates
date_seq <- seq(as.Date("2018-07-01"), as.Date("2019-06-30"), by = "day")

# Count number of transactions by date
trans_by_date <- transactionData[, .N, by = DATE]

# Merge with sequence of dates to include missing dates
trans_by_date <- merge(data.frame(DATE = date_seq), trans_by_date, all.x = TRUE)

# Replace missing transaction counts with 0
trans_by_date[is.na(trans_by_date$N), "N"] <- 0

# Create chart of number of transactions over time
ggplot(data = trans_by_date, aes(x = DATE, y = N)) +
  geom_line() +
  labs(title = "Number of Transactions by Date",
       x = "Date",
       y = "Number of Transactions")


# Create a sequence of dates
date_seq <- seq(as.Date("2018-07-01"), as.Date("2019-06-30"), by = "day")

# Count transactions by date
library(dplyr)
transactions_by_date <- transactionData %>%
  group_by(DATE) %>%
  summarise(TXN_COUNT = n())

# Join sequence of dates to transactions by date
library(tidyr)
transactions_by_date <- transactions_by_date %>%
  complete(DATE = date_seq) %>%
  fill(TXN_COUNT, .direction = "down")



#Setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))


#Plot transactions over time
ggplot(transactions_by_date, aes(x = DATE, y = TXN_COUNT)) +
geom_line() +
labs(x = "Date", y = "Number of Transactions", title = "Transactions Over Time") +
scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#Over to you - extract the pack size from PROD_NAME and create a new column "PACK_SIZE" in the transaction data.
library(stringr)

library(stringr)

transactionData <- transactionData %>%
  mutate(PACK_SIZE = as.numeric(str_extract(PROD_NAME, "\\d+")))

head(transactionData)


#Filter to December
dec_transactions <- transactions_by_date %>%
  filter(DATE >= as.Date("2018-12-01") & DATE <= as.Date("2018-12-31"))


# Filter to December and look at individual days
dec_transactions <- transactions_by_date %>%
  filter(DATE >= as.Date("2018-12-01") & DATE <= as.Date("2018-12-31"))

# Create chart of transactions in December
ggplot(dec_transactions, aes(x = DATE, y = TXN_COUNT)) +
  geom_line() +
  labs(title = "Transactions in December 2018",
       x = "Date",
       y = "Number of Transactions") +
  scale_x_date(date_labels = "%d") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))



# Load the tidyverse package
library(tidyverse)

# Read in the data from the CSV file
transactionData <- "/Users/HP/Documents/QVI_transaction_data.csv"

# create PACK_SIZE column
transactionData <- transactionData %>%
  mutate(PACK_SIZE = as.numeric(str_extract(PROD_NAME, "\\d+")))

# check output
transactionData %>%
  group_by(PACK_SIZE) %>%
  summarise(N = n()) %>%
  arrange(PACK_SIZE)

