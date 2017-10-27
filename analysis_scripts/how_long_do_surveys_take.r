source('Z:/Git/caterpillars-count-analysis/cleaning_scripts/data_cleaning.R')


lab = filter(labdata, year >= 2016) %>%
  select(userID:surveyType) %>%
  distinct() %>%
  arrange(date, time) %>%
  mutate(dt = as.POSIXct(strptime(paste(date, time), format = "%Y-%m-%d %H:%M")))

users = c(158, 159, 189, 191, 300, 301, 302)


output = data.frame(userID = NULL, surveyType = NULL, n = NULL, min = NULL)
for (u in users) {
  dat = filter(lab, userID == u)
  
  times = data.frame(time1 = dat$dt[2:nrow(dat)], time2 = dat$dt[1:(nrow(dat)-1)])
  diff = apply(times, 1, function(x) as.numeric(difftime(x[1], x[2], units = 'mins')))
  
  df = data.frame(userID = u, surveyType = dat$surveyType[1:(nrow(dat)-1)],
                  n = 1:(nrow(dat)-1), min = diff)
  output = rbind(output, df)

}
output2 = filter(output, min < 60)


par(mfrow = c(4, 2), mar = c(4, 4, 1, 1))
colors = c('royalblue', 'skyblue', 'darkgreen', 'orange', 'red', 'purple', 'yellow')
i = 1
for (u in users) {
  vis = filter(output2, userID == u, surveyType == 'Visual')
  bs = filter(output2, userID == u, surveyType == 'Beat_Sheet')
  plot(1:100, vis$min[1:100], type = 'l', lwd = 2, col = colors[i], lty = 'solid',
       xlab = 'Survey #', ylab = 'Time (min)', ylim = c(0, 20))
  points(1:100, bs$min[1:100], type = 'l', lwd = 2, col = colors[i], lty = 'dashed')
  legend('topright', legend = u, bty = 'n', cex = 2)
  
  if (i == length(users)) {
    legend('topleft', legend = c('Visual', 'Beat sheet'), lty = c('solid', 'dashed'))
  }
  
  i = i + 1
  
}

