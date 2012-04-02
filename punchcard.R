library("ggplot2")
library("reshape")
library("digest")
library("plyr")
library("scales")

lazy = function(expr) {
 if (!exists("cache")) { assign("cache", new.env(), envir=.GlobalEnv)}

 key = digest(match.call()$expr)
 if (!exists(key, envir=cache)) {
   assign(key, eval(expr), envir=cache)
 }
 get(key, envir=cache)
}

getMatches = function(data) {
 matched_lines = grep('^Date:.*, .*-0\\d+', data, value=T)
 return(gsub('Date:.*, (.*-0\\d+).*', '\\1', matched_lines))
}

# Set to git repo here.
TARGET_REPO = '/home/tierney/repos/libjingle/'

lines=readLines(pipe(paste('cd', TARGET_REPO, '&& git log | grep Date && cd -')))

gitMatches= function(data) {
  matched_lines = grep('^Date: .*[-+]0\\d+', data, value=T)
  return(gsub('Date: *(.*[-+]0\\d+).*', '\\1', matched_lines))
}
matches = gitMatches(lines)

# 'Thu Mar 15 23:39:39 2012 -0400'
gitDateFormat <- '%a %b %d %H:%M:%S %Y %z'
thunderbirdDateFormat <- "%d %b %Y %H:%M:%S %z"
dates = lapply(matches, function(f) {strptime(f, format=gitDateFormat)})

df = ldply(dates, unlist)
#df$wday = df$wday + 1900
df_counts = count(df, vars=c("wday", "hour"))

for (wday in df_counts$wday) {
 m = df_counts$wday == wday
 df_counts$rel_freq[m] = df_counts$freq[m] / sum(df_counts$freq)
}

days_of_the_week = c('Sunday','Monday','Tuesday','Wednesday',
                     'Thursday','Friday','Saturday')

p = ggplot(df_counts, aes(x=hour, y=wday)) +
 scale_y_continuous(name='', limits=c(0,6),
                    labels=factor(seq(0,6,1), labels=days_of_the_week)) +
 scale_x_continuous(name='Hour', limits=c(0,23), breaks=seq(0,23,1)) +
 geom_point(aes(size=rel_freq))

show(p)
