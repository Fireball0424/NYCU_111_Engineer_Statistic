#111550130-§õ¼}®x-Ch2-objects
#generate students' score
set.seed(1);
math.score <- sample(0:100, 80, replace = TRUE)

#problem (a), take first thirty students
score.number.1.to.30 <- math.score[1:30]
mean(score.number.1.to.30)
sd(score.number.1.to.30)

#problem (b), filter out pass students
score.greater.than.60 <- which(math.score >= 60, arr.ind = TRUE)
length(score.greater.than.60)
score.greater.than.60

#problem (c) best score and worst score
score.rank <- order(math.score)
best.score <- math.score[score.rank[80]]
worst.score <- math.score[score.rank[1]]

best.score
worst.score
which(math.score == best.score)
which(math.score == worst.score)

#problem (d) top 10 score
sorted.score <- sort(math.score, decreasing = TRUE)
score.top.10 <- sorted.score[1:10]
mean(score.top.10)
sd(score.top.10)

#problem (e) Summary
score.summary <- summary(math.score)
score.summary[2]
