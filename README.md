# StatisticalComputing
4 data science projects on pairs trading, baseball, global demographics, spam classification.

# Pairs Trading
Pairs Trading is a stock trading strategy that was employed intensively during the 1980s. We find a pair of stocks that are positively correlated. Then we use the ratio to determine the best time to sell/buy each stock. When one stock is overpriced, we sell it and buy the other. When one stock is underpriced, we buy it and sell the other.
This project seeks to emulate the strategy on stock data from Yahoo! Finance. Next, we utilize a time series to simulate pairs of stocks with various different properties. Finally, we extend the project by finding the hypothetical optimal training fraction.

# Baseball
Sabermetrics is the statistics of baseball. This project seeks to explore some of the possible analysis in this field. We will use SQL queries. The database is Sean Lahman's Baseball SQLite database.
We will collect some cursory information about the database. Then we will dive deeper into discovering the causal relationship between different tables of data. There will be an emphasis on discovering the factors that effect the salaries of the various teams and players.

# Global Demographics
This project seeks to analyze global demographics data. We will use XML sheets from the CIA Factbook to get the infant mortality and population data for different countries. Then we will combine this data with online geolocation data. In order to use these data sets together we will cross-reference ISO 3166 codes.
There will be different ways to categorize the data. First, we will categorize the data by diving it into quantiles. Next, we will take a more complex approach by implementing a k-means clustering algorithm. Using these categories and the geo-location data, we will create maps to better visualize global trends.
Finally, we will extend the project by analyzing individual continents using k-means clustering.

# Spam Classification
Spam e-mail is fraudulent e-mail that is sent out in mass usually for malicious reasons. For user safety, it is important to be able to stop the e-mails from getting into user access. We will program algorithms that attempt to detect spam e-mail. We will use the contents of the e-mail and the e-mail metadata. Spam Assassin has provided message data of both spam and non-spam messages. Spam Assassin refers to non-spam e-mail as ham e-mail, which we will also do throughout this report. We will need to make some decisions on how to categorize the e-mail. For example, is worse to categorize a non-spam e-mail as spam than to categorize a spam e-mail as spam? This is the difference between type 1 & type 2 errors.
Finally, we will extend the project by looking at the e-mail header. The header contains metadata that includes the sender's e-mail address. We will try to see how based on the domain extension and the sender's username we can better observe whether or not the e-mail is spam.
