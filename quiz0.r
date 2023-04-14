# question 1 #
hp <- mtcars[ 3, "hp" ]
print(hp)

# question 2 #
six_eight <- mtcars[ mtcars$cyl == 6 | mtcars$cyl == 8, "mpg" ]
avg_mpg <- mean(six_eight)
print(avg_mpg)

# question 3 #
hp <- mtcars$hp
weight <- mtcars$wt
hp_per_weight <- hp / weight
hp_per_weight <- data.frame("horsepower/weight ratio" = hp_per_weight)
mtcars_augment = cbind(mtcars, hp_per_weight)
print(mtcars_augment)

# question 4 #
foreign_workers <- german.credit[ german.credit$Foreign_worker == "yes", ]
num_foreign_workers <- nrow( foreign_workers )

foreign_workers_good <- german.credit[ german.credit$Foreign_worker == "yes" & german.credit$Credit_risk == "GOOD", ]
num_foreign_good_credit <- nrow( foreign_workers_good )

good_credit <- german.credit[ german.credit$Credit_risk == "GOOD", ]
num_good_credit <- nrow(good_credit)

num_workers = nrow(german.credit)

proportions <- c(num_foreign_good_credit / num_foreign_workers, 
                 num_foreign_good_credit / num_good_credit, 
                 num_foreign_workers / num_workers, 
                 num_good_credit / num_workers)

print(proportions)

# /opt/homebrew/Cellar/r/4.2.3/lib/R/library