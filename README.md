Install P-Elo in R using:

remotes::install_github("pelointransitivity/PELO")

# Example on Rock Paper Scissor data

```r
library(PELO)

# rock paper scissor - toy dataset (no tie)
set.seed(999)
n <- 200
x <- c("Rock","Paper","Scissors")
p1 <- sample(x, n, TRUE)
p2 <- sample(x, n, TRUE)
winner <- ifelse(p1 == p2, 0.5,
                 ifelse((p1=="Rock"&p2=="Scissors") |
                          (p1=="Paper"&p2=="Rock") |
                          (p1=="Scissors"&p2=="Paper"),
                        1,0))
rps <- data.frame(game = 1:n, p1, p2, winner)
rps_df <- rps[rps$winner!=0.5,]
View(rps_df)

# before model run
colnames(rps_df) <- c('Time','A','B','match_result')
players <- sort(unique(c(rps_df$A, rps_df$B)))
player_id_mapping <- data.frame("Names"=players, "ID"=1:length(players))
initial_rating <- PELO::initialize_ratings(players, 1500)
initial_pair_adv <- PELO::initialize_pair_adv_mat(players, 0)

# find the best K and K2
best_kk2 <- optimize_pelo_kk2(
  rps_df,
  initial_rating,
  initial_pair_adv,
  kk2_init = c(30, 30),
  lower = c(0, -20000),
  upper = c(20000, 20000)
)

# model run
pelo_obj <- PELO::run_pelo(best_kk2, data = rps_df, current_ratings = initial_rating, current_pair_adv_mat = initial_pair_adv, player_id_mapping = player_id_mapping)

# result
View(pelo_obj$ratings)
View(pelo_obj$pair_adv)

# diagnosis

# winning probs for the first game is 0.5 as expected
# game predictions (in terms of accuracy) are perfect afterwards
rps_df[round(pelo_obj$preds,0)!=rps_df$match_result,]
pelo_obj$preds[round(pelo_obj$preds,0)!=rps_df$match_result]
```
