library("ggplot2")
library("reshape")
library("digest")

SOURCE=Sys.glob('/home/power/.thunderbird/*/*/*/*/Sent Mail')

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

lines = lazy(readLines(SOURCE))
matches = lazy(getMatches(lines))
dates = lazy(lapply(matches, function(f) {strptime(f, format="%d %b %Y
%H:%M:%S %z")}))
df = ldply(dates, unlist)
df$year = df$year + 1900
df_counts = count(df, vars=c("year", "hour"))

for (year in df_counts$year) {
 m = df_counts$year == year
 df_counts$freq[m] = df_counts$freq[m] / sum(df_counts$freq[m])
}

p = ggplot(df_counts, aes(x=year, y=hour)) +
 scale_x_continuous(limits=c(2004, 2012)) +
 scale_y_datetime() +
 geom_tile(aes(fill=freq)) +
 scale_fill_gradient()

show(p)
