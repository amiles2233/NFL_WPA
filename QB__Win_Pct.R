setwd("~/Analyses/NFL")

library(dplyr)
library(h2o)
library(ggplot2)
library(scales)
library(labeling)


##-------Data Cleaning-------------

#Classify Wins
gm <- read.csv("Data/GAME.csv", stringsAsFactors = FALSE)
gm <- select(gm, gid,v,h,ptsv,ptsh,seas,sprv)
gm$hwin <- ifelse(gm$ptsh>gm$ptsv,1,0)

# Read in Play Level Data & Match to Game Data
play <- read.csv("Data/PLAY.csv", stringsAsFactors = FALSE)
play <- left_join(play, gm, by="gid")

# Gather Cumulative Score
play$hoff <- ifelse(play$off==play$h,1,0)
play$hscore <- ifelse(play$off==play$h,play$ptso,play$ptsd)
play$vscore <- ifelse(play$off==play$v,play$ptso,play$ptsd)

# Score Difference
play$scdiff <- play$hscore-play$vscore

# Time Left
play$timeleft <- ifelse(play$qtr<=4,(4-play$qtr)*15+play$min+play$sec/60,play$min+play$sec/60)

# Home and Visiting Team Timeouts
play$htim <- ifelse(play$off==play$h,play$timo,play$timd)
play$vtim <- ifelse(play$off==play$v,play$timo,play$timd)

# Change Hwin to Factor
play$hwin <- factor(play$hwin, levels=c(1,0), labels=c("Home Win", "Visit Win"))

# Limit Variables
play1 <- select(play, gid, pid, hwin, hoff, hscore, vscore, scdiff, timeleft, htim, vtim,
                qtr, ytg, yfog, sprv)

# ID Predictor Variables
predictors <- names(play1)[4:14]


# Read into H2O
h2o.init(nthreads = -1)

train.hex <- as.h2o(play1)

## Model Win Probability
win.gbm <-h2o.gbm(x=predictors,
                   y="hwin",
                   training_frame = train.hex,
                   nfolds=5)

predict <- as.data.frame(h2o.predict(win.gbm, newdata = train.hex))
predict <- predict$Home.Win

## Attach to Original Data Frame
play <- cbind(play, predict)
play <- play %>%
    group_by(gid) %>%
    mutate(predict_nxt=lead(predict),
           predict_nxt=ifelse(is.na(predict_nxt) &
                                        hwin==1,1,predict_nxt),
           predict_nxt=ifelse(is.na(predict_nxt) &
                                  hwin==0,0,predict_nxt),
           win_chg=predict_nxt-predict)

## Save play with predicted win probs
saveRDS(play, "PlayWinProb.RDS")

## Read in Pass & Player Data
pass <- read.csv("Data/PASS.csv", stringsAsFactors = FALSE)
pass <- select(pass, pid, psr) %>%
    rename(player=psr)
rush <- read.csv("Data/RUSH.csv", stringsAsFactors = FALSE)
rush <- select(rush, pid, bc) %>%
    rename(player=bc)
off <- bind_rows(pass, rush)
player <- read.csv("Data/PLAYER.csv", stringsAsFactors = FALSE)
player <- filter(player, pos1=="QB") %>%
   select(player, player, fname, lname, pname)

off <- left_join(player, off, by=c("player"))

## Join Pass to Play Data
qb_ply_full <- left_join(off, play, by="pid") %>%
    mutate(win_chg=ifelse(hoff==0,-win_chg,win_chg)) %>%
    select(pname, pid, win_chg, seas)
saveRDS(qb_ply_full, "qb_ply_full.RDS")

qb_ply_sum <- qb_ply_full %>%
    group_by(pname, seas) %>%
    summarize(win_chg=sum(win_chg, na.rm=TRUE), plays=n_distinct(pid)) %>%
    filter(plays>100)
saveRDS(qb_ply_sum, "qb_ply_sum.RDS")


## Plots
pass15 <- filter(qb_ply_sum, seas==2015)
ggplot(pass15, aes(x=reorder(pname,win_chg), y=win_chg, label=round(win_chg,2))) +
    geom_bar(stat="identity", fill="dodgerblue3") +
    geom_text(hjust=-.1, vjust=.2) +
    coord_flip() +
    xlab("QB Name") +
    ylab("Win Probability Added") +
    ggtitle("Plot 2: Win Probability Added Rankings - 2015 QB's") +
    theme_minimal()

sf_qb <- filter(qb_ply_full, pname %in% c("B.Gabbert","C.Kaepernick"), seas==2015)
ggplot(sf_qb, aes(x=win_chg, fill=pname)) +
    geom_histogram(position="dodge") +
    xlab("Win Probability Added") +
    ylab("Count") +
    ggtitle("Plot 3: Blaine Gabbert - Colin Kaepernick Comp") +
    theme_minimal()

bgar_comp <- filter(qb_ply_full, pname %in% c("B.Gabbert","T.Brady"), seas==2015)
ggplot(bgar_comp, aes(x=win_chg, fill=pname)) +
    geom_histogram(position="dodge") +
    xlab("Win Probability Added") +
    ylab("Count") +
    ggtitle("Plot 4: Blaine Gabbert - Tom Brady Comp") +
    theme_minimal()

sb <- filter(play, gid==4256) %>%
    mutate(pid=pid-min(pid)) %>%
    ggplot(aes(x=pid,y=predict)) +
    geom_line(size=1.2, color="darkblue") +
    ggtitle("Plot 1: In-Game Win Probability: Super Bowl 50") +
    xlab("Play") +
    ylab("Probability of Home (Bronco) Win") +
    scale_y_continuous(labels=percent, breaks=c(0,.25,.5,.75,1), limits=c(0,1)) +
    theme_minimal() 

