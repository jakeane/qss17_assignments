## Data Visualization (GOVT16-QSS17) Fall 2020
## Intro to R, Part 1 & 2
##
## Name: John Keane
## Date: 9/16/20

# Objects, Vectors, and Matrices

# 1. Objects

# (a)
Harry <- 50
paste("Harry:", Harry)

# (b)
Hermoine <- 47
paste("Hermoine:", Hermoine)

# (c)
Ron <- 44
paste("Ron:", Ron)

# (d)
Jenny <- 40
paste("Jenny:", Jenny)

# (e)
paste("Harry and Jenny greater than Ron and Hermoine:", (Harry + Jenny) > (Ron + Hermoine))

# (f)
Snape <- 49
paste("Snape:", Snape)

# (g)
Voldemort <- 1 / Harry # Is this what he means by inverse?
paste("Voldemort:", Voldemort)

# (h)
paste("Inverses?", Voldemort * Harry == 1)


# 2. Let’s do some basic math in R without creating objects. Order of operations is key.

# (a)
paste("7546 + 9918 =", 7546 + 9918)

# (b)
paste("3467 - 8493 =", 3467 - 8493)

# (c)
paste("37 * 47 + 30 =", 37 * 47 + 30)

# (d)
paste("(3 + 67) * (8 - 29) =", (3 + 67) * (8 - 29))

# (e)
paste("656 + 23/53 =", 656 + 23/53)

# (f)
paste("74^2 =", 74^2)

# (g)
paste("999 %% 77 =", 999 %% 77)


# 3. Vectors (and math operations)

# (a)
wizards1 <- c(Harry, Hermoine)
paste(c("wizards1:", wizards1), collapse=" ")

# (b)
wizards2 <- c(Jenny, Ron)
paste(c("wizards2:", wizards2), collapse=" ")

# (c)
wizards <- c(wizards1, wizards2)
paste(c("wizards:", wizards), collapse=" ")

# (d)
names(wizards) <- c("Harry", "Hermoine", "Jenny", "Ron")
print("wizards:"); wizards

# (e)
remove(Snape)
remove(Voldemort)

# (f) commented out to prevent execution from halting
# paste("Snape:", Snape)
# paste("Voldemort:", Voldemort)

# (g)
wizards["Jenny"]

# (h)
wizards[c("Hermoine", "Ron")]

# (i)
wizards[2:4]

# (j)
print("Scores lower than 45:"); wizards < 45


# 4. Matrices

# (a)
scores <- c(75, 49, 68, 83, 97, 91, 98, 89, 91)
paste(c("scores:", scores), collapse=" ")

# (b)
wiz_scores <- matrix(scores, byrow = TRUE, nrow = 3)
print("wiz_scores:"); wiz_scores

# (c)
rownames(wiz_scores) <- c("Ron", "Harry", "Hermoine")
colnames(wiz_scores) <- c("test1", "test2", "test3")
print("wiz_scores:"); wiz_scores

# (d) its not colMeans right?
wiz_scores2 <- cbind(wiz_scores, avg=rowMeans(wiz_scores))
print("wiz_scores2:"); wiz_scores2

# (e) its not colMeans right?
otherwiz <- matrix(c(81, 80, 78, 92, 87, 84), byrow = TRUE, nrow = 2)
rownames(otherwiz) <- c("Neville", "Jenny")
colnames(otherwiz) <- c("test1", "test2", "test3")
otherwiz <- cbind(otherwiz, avg=rowMeans(otherwiz))
print("otherwiz"); otherwiz

# (f)
all_wiz_scores <- rbind(wiz_scores2, otherwiz)
print("all_wiz_scores:"); all_wiz_scores

# (g)
paste(c("Hermoines 2nd and 3rd scores:", all_wiz_scores["Hermoine", 2:3]), collapse=" ")

# (h) do you mean 'all_wiz_scores2'?
wiz_scores2 <- all_wiz_scores[,c(1,3)]
wiz_scores2 <- cbind(wiz_scores2, newavg=rowMeans(wiz_scores2))
print("wiz_scores2:"); wiz_scores2

# (i)
wiz_scores2["Neville", 2] <- 98
colnames(wiz_scores2)[3] <- "finalavg"
wiz_scores2[,"finalavg"] <- rowMeans(wiz_scores2[,1:2])
print("wiz_scores2:"); wiz_scores2


# Factors, Data Frames, and Lists


# 1. Categorical Variables: Characters & Factors

# (a)
truth <- "Dartmouth is, like, way better than all the other schools. Like, it’s not even close, you guys."
paste("truth:", truth)

# (b)
paste("Class of truth:", class(truth))

# (c)
colors <- c("red", "blue", "green", "red", "blue")
paste(c("colors:", colors), collapse=" ")

# (d)
factor_colors <- factor(colors)
paste(c("factor_colors:", factor_colors), collapse=" ")

# (e)
factor2_colors <- factor(colors, levels = c("red", "blue"))
paste(c("factor2_colors:", factor2_colors), collapse=" ")
# i. The relative value of variables unspecified in 'levels' is unknown, so it will be ignored.

# (f)
print("colors summary:"); summary(colors)
print("factor_colors summary:"); summary(factor_colors)
print("factor2_colors summary:"); summary(factor2_colors)

# (g)
ideology <- c("liberal", "conservative", "very liberal", "very conservative",
              "middle of the road", "slightly conservative", "slightly liberal",
              "liberal", "conservative", "middle of the road")
paste(c("ideology:", ideology), collapse=" ")

# (h) does this need to be reordered?
fact_ideo <- factor(ideology, levels = c("very liberal", "liberal", "slightly liberal",
                                         "middle of the road", "slightly conservative",
                                         "conservative", "very conservative"))
print("fact_ideo:"); fact_ideo

# (i)
respondent <- c("Susie", "Abdul", "Maria", "Fred", "Wilma",
                "Barney", "Dino", "Ajax", "Thor", "Betty")
paste(c("respondent:", respondent), collapse=" ")


# 2. Data Frames

# (a)
income <- c(100000, 75000, 48000, 62000, 31000, 52500, 274000, 88000, 21000, 74000)
paste(c("income:", income), collapse=" ")

# (b)
data1 <- data.frame(ideology, respondent, income)
print("data1:"); data1

# (c)
print("data1 head:"); head(data1)

# (d)
print("data1 tail:"); tail(data1)

# (e)
print("data1 structure"); str(data1)

# (f)
orderdat <- data1[order(-income),]
print("orderdat:"); orderdat

# (g)
print("lowest income respondent:"); data1[order(income)[1],]

# (h)
print("Ajax to Fred:"); data1[which(data1$respondent == "Fred"):which(data1$respondent == "Ajax"),]

# (i)
data1 <- cbind(data1, log_income=log(data1$income))
print("data1 w/log_income"); data1


# 3. Lists

# (a)
survey <- list(ideology, respondent, income)
print("survey structure:"); str(survey)

# (b)
session <- 2
paste("session:", session)

# (c)
weeks <- matrix(1:9, byrow = TRUE, nrow = 3)
print("weeks:"); weeks

# (d)
dv_list <- list(truth=truth, sess1=session, wk=weeks, dat=data1, svy=survey)
print("dv_list:"); dv_list

# (e)
print("middle element from weeks matrix:"); dv_list$wk[2,2]

# (f)
dv_list$dat["income"] <- dv_list$dat["income"] / 2
print("new data1:"); dv_list$dat
