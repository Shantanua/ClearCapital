func1 <- function(V.Size, N){
# Sort the vector in decreasing order #
V.Size_sorted <- sort(V.Size, decreasing = TRUE);

# create an index variable to order the elements of a list #
index_v <- rep(1:N, length.out = length(V.Size));

# Create the list using index variable #
required_list <- split(V.Size_sorted, index_v);

# Print the list #
print(required_list);
}

# Example #

set.seed(123)
z <- floor(runif(15, min = 0, max = 50))
func1(z,4)

#############################################################################

func2 <- function(V.Ints){
# Determine unique values and their frequency in a data frame format #
output <- data.frame(table(as.factor(V.Ints)));

# Subset to only those with at least 2 occurrence #
output <- output[output[,2] >= 2,]

# Give the correct names to columns #
names(output) <- c("V.Ints", "Number.of.Occurrence");

# Print the required output #
print(output);
}

# Example #

set.seed(123)
z <- floor(runif(15, min = 0, max = 50))
func2(z)

#############################################################################

func3 <- function(V.dates, current_date){
# Parse character as date format #
current_date <- as.POSIXlt(current_date, format="%m/%d/%Y");

# Convert date to required format #
current_date <- format(current_date, "%Y-%m-%d");

# Parse character as date format #
V.dates2 <- as.POSIXlt(V.dates, format="%m/%d/%Y");

# Convert date to required format #
V.dates2 <- format(V.dates2, "%Y-%m-%d");

Original.Date <- V.dates;

# Calculate Time Difference in days. Convert it to absolute numeric value #
Days <- abs(as.numeric(difftime(current_date, V.dates2, tz = "EST", "days")));

# Calculate the quarters #
Quarter <- floor(Days/91);

# Arrange the required information in data frame format #
df <- data.frame(Original.Date, Days, Quarter);

# Print the data frame #
print(df);

}

# Example #

# Input must be standard date format mm/dd/yyyy #

z2 <- c('06/20/2009', '01/20/2009', '06/01/2009', '06/20/2012', '06/20/2007')
func3(z2,'10/27/2010')

##############################################################################

func4 <- function(V.text){

# Create a function which would keep just the dollar amount and date #

dollar_date <- function(x){
	# split the string into chunks of character words #
	splits <- unlist(strsplit(x, split = " "))

	# Determine which of the elements(chunks) are not alphabets #
	indicator <- !(grepl("[[:alpha:]]", splits))

	# Subset the vector into required elements #
	splits <- splits[indicator]

	# Remove Quatation marks if present #
	splits2 <- gsub("'",'',splits)

	# Determine date elements #
	date_v <- splits2[grepl("/", splits2)]

	# Determine dollar amount elements #
	dollar_v <- splits2[!(grepl("/", splits))]

	# Make a list of date and dollar amount #
	list_v <- list(Dates = date_v, Dollar_Amount = dollar_v)
}

# Apply this function to each element of the vector #
required_list <- sapply(V.text, function(V.text) dollar_date(V.text))

# Print the list #

print(required_list)

}

# Example #

x1 <- "Listed on 1/05/2009 for 180000 and sold for $150,250 on 3/1/2009"
x2 <- "For example, assuming present day is '10/27/2010' and an input date of '6/20/2009'"
X <- c(x1,x2)

func4(X)

# Question asks for separate vectors for dates and dollar amounts for a string.
We need to do this for each string in a vector of strings. Hence I made output list 
of a list. Each list has 2 components, dates and dollar amount which capture those part of
a string. You cannot visually see the list of a list #
