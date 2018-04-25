# Script for analyzing arthropod photo id quiz results

quiz = read.csv('data/quiz_results.csv', header = F, stringsAsFactors = F)
names(quiz) = c('id', 'userID', 'time', 'score', 'possible')

users = read.csv('data/tbl_users.csv', header = F, stringsAsFactors = F)
names(users)[1:2] = c('userID', 'email')

kids = users$userID[grepl('dsdragons', users$email)]

quiz$pct = 100*quiz$score/quiz$possible

quizk = filter(quiz, userID %in% kids)

takes = count(quizk, userID)

kids_gt6 = takes$userID[takes$n > 6]

quizk6 = filter(quizk, userID %in% kids_gt6)

first3 = quizk6 %>% 
  group_by(userID) %>% 
  arrange(userID, time) %>%
  slice(1:3) %>% 
  group_by(userID) %>% 
  summarize(mean = mean(pct))

last3 = quizk6 %>% 
  group_by(userID) %>% 
  arrange(userID, desc(time)) %>%
  slice(1:3) %>% 
  group_by(userID) %>% 
  summarize(mean = mean(pct))




par(mfrow = c(1,1))
plot(c(1,20), c(0, 100), type = 'n', xlab = 'Attempts', ylab = 'Score')
for (user in kids) {
  scores = quizk %>% 
    filter(userID == user) %>%
    arrange(time)
  if (nrow(scores) > 6) {
    points(1:nrow(scores), scores$pct, type = 'l', col = 'gray50', lwd = 2)
  }
}


par(mfrow = c(4,5), mar = c(3, 3, 1, 1), oma = c(4, 4, 0, 0))
for (user in kids) {
  scores = quizk %>% 
    filter(userID == user) %>%
    arrange(time)
  if (nrow(scores) > 6) {
    plot(2:nrow(scores), scores$pct[2:nrow(scores)], type = 'l', col = 'gray50', lwd = 2, 
         xlab = '', ylab = '', main = user, xlim = c(0, 20), ylim = c(0, 100))
  }
}
