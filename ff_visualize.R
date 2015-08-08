# Visualize the freefood data

lapply(c("data.table", "lubridate", "ggplot2"), require, character.only=T)

# read in data
df.raw <- as.data.frame(fread("ff_emails.csv"))
df.ff <- data.frame(
  from=df.raw$message_from,
  time=df.raw$message_internaldate,
  date=as.character(as.Date(df.raw$message_internaldate)),
  hour=hour(df.raw$message_internaldate),
  min=minute(df.raw$message_internaldate),
  subj=df.raw$message_subject
)

show(qplot(df.ff$hour) + ggtitle("Free food emails from 11/20/13 to present, hour of day") +
       xlab("Hour of day") + ylab("Frequency"))

require(dplyr)
df.datefreqs <- df.ff %>% select(from, date) %>% group_by(date) %>% summarise(num.emails=n())
df.datefreqs <- as.data.frame(df.datefreqs)
ts.num.emails <- ts(df.datefreqs$num.emails)
# use LOMB package to calculate periodogram for freq
require(lomb)
df.emails <- data.frame(
  sampling.times=df.datefreqs$date, # should be day #
  num.emails=ts.num.emails
)
ts.lsp <- lsp(ts.num.emails, type="period")
# http://stats.stackexchange.com/questions/16117/what-method-can-be-used-to-detect-seasonality-in-data
# see which periods have the largest spectral estimate
df.lsp <- data.frame(
  period=ts.lsp$scanned,
  power=ts.lsp$power
)
df.lsp <- df.lsp[order(df.lsp$power, decreasing=TRUE),]
print("Periodogram analysis detected seasonalities (most important first):")
for (i in 1:5) {
  print(sprintf("%.0f days, with power %.2f", df.lsp$period[i], df.lsp$power[i]))
}
  







