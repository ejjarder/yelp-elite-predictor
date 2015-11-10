source('parse.R')
library(plyr)
library(lubridate)

options(stringsAsFactors = FALSE)
user.dataset.file <-
    'yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_user.json'

add.compliments.columns <- function(one.row, compliments) {
    if (length(compliments) > 0) {
        compliments.df <- as.data.frame(compliments)
        names(compliments.df) <- paste('compliments', names(compliments.df),
                                       sep = '.')
        one.row <- cbind(one.row, compliments.df)
    }
    one.row
}

get.days.yelping <- function(yelping.since) {
    difftime(today(), parse_date_time(yelping.since, 'ym'))
}

convert.to.row <- function(user.dataset.entry) {
    print(sprintf('Flattening %s', user.dataset.entry$user_id))
    not.lists <- !(names(user.dataset.entry) %in%
                       c('friends', 'elite', 'compliments'))
    one.row <- as.data.frame(user.dataset.entry[not.lists])
    one.row$elite.count <- length(user.dataset.entry$elite)
    one.row$friend.count <- length(user.dataset.entry$friends)
    one.row <- add.compliments.columns(one.row, user.dataset.entry$compliments)
    one.row$days.yelping <- get.days.yelping(one.row$yelping_since)
    one.row
}

get.elite.friends <- function(user.dataset.entry, user.dataset.df) {
    print(sprintf('Getting elite friends... %s', user.dataset.entry$user_id))
    in.friends <- user.dataset.df$user_id %in% user.dataset.entry$friends
    elite.friend.years <- sum(user.dataset.df$elite.count[in.friends])
    print(sprintf('User %s has friends with %s total elite years',
                  user.dataset.entry$user_id, elite.friend.years))
    data.frame(user_id = user.dataset.entry$user_id,
               elite.friend.years = elite.friend.years)
}

read.user.dataset <- function(count = -1L) {
    user.dataset.lines <- readLines(user.dataset.file, n = count)
    user.dataset.list <- basic.parser(user.dataset.lines, 'user_id')
    user.dataset.df.list <- lapply(user.dataset.list, convert.to.row)
    user.dataset.df <- do.call('rbind.fill', user.dataset.df.list)
    user.dataset.elite.friends.df.list <- 
        lapply(user.dataset.list, get.elite.friends, user.dataset.df)
    user.dataset.elite.friends.df <-
        do.call('rbind', user.dataset.elite.friends.df.list)
    merge(user.dataset.df, user.dataset.elite.friends.df)
}

write.csv(read.user.dataset(), file = 'user.data.csv')
