
CorrectStateAbreviation <- function(x) {
  x <- as.character(x)
  usStates <- c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS",
                "CALIFORNIA", "COLORADO", "CONNECTICUT",
                "DELAWARE",
                "FLORIDA",
                "GEORGIA",
                "HAWAII",
                "IDAHO", "ILLINOIS", "INDIANA", "IOWA",
                "KANSAS", "KENTUCKY",
                "LOUISIANA",
                "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", 
                "MINNESOTA", "MISSISSIPI", "MISSOURI", "MONTANA",
                "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY",
                "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA",
                "OHIO", "OKLAHOMA", "OREGON",
                "PENNSYLVANIA",
                "RHODE ISLAND",
                "SOUTH CAROLINA", "SOUTH DAKOTA",
                "TENNESSEE", "TEXAS",
                "UTAH",
                "VERMONT", "VIRGINIA",
                "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")
  usStatesAbr <- c("AL", "AK", "AZ", "AR",
                   "CA", "CO", "CT",
                   "DE",
                   "FL",
                   "GA",
                   "HI",
                   "ID", "IL", "IN", "IA",
                   "KS", "KY",
                   "LA",
                   "ME", "MD", "MA", "MI",
                   "MN", "MS", "MO", "MT",
                   "NE", "NV", "NH", "NJ", 
                   "NM", "NY", "NC", "ND",
                   "OH", "OK", "OR",
                   "PA",
                   "RI",
                   "SC", "SD",
                   "TN", "TX",
                   "UT", 
                   "VT", "VA",
                   "WA", "WV", "WI", "WY")
  commonwealthTerritories <- c("AMERICAN SAMOA", "DISTRICT OF COLUMBIA", 
                               "FEDERATED STATES OF MICRONESIA", "GUAM", 
                               "MARSHALL ISLANDS", "MORTHERN MARIANA ISLANDS", 
                               "PALAU", "PUETO RICO", "VIRGIN ISLANDS")
  commonwealthTerritoriesAbr <- c("AS", "DC", "FM", "GU", "MH", "MP", "PW", "PR", "VI")  
  militaryStates <- c("ARMED FORCES AMERICAS", 
                      "ARMED FORCES AFRICA/CANADA/EUROPE/MIDDLE EAST", 
                      "ARMED FORCES PACIFIC")
  militaryStatesAbr <- c("AA", "AE", "AP")
  allStates <- c(usStates, commonwealthTerritories, militaryStates)
  allStatesAbr <- c(usStatesAbr, commonwealthTerritoriesAbr, militaryStatesAbr)
  kN <- list() ## keyboard neighbors
  kN$Q <- c("W", "A")
  kN$W <- c("Q", "A", "S", "E")
  kN$E <- c("W", "S", "D", "R")
  kN$R <- c("E", "D", "F", "T")
  kN$T <- c("R", "F", "G", "Y")
  kN$Y <- c("T", "G", "H", "U")
  kN$U <- c("Y", "H", "J", "I")
  kN$I <- c("U", "J", "K", "O")
  kN$O <- c("I", "K", "L", "P")
  kN$P <- c("O", "L")
  kN$A <- c("Q", "W", "S", "Z")
  kN$S <- c("W", "E", "A", "Z", "X", "D")
  kN$D <- c("E", "R", "S", "Z", "X", "C", "F")
  kN$F <- c("R", "T", "D", "X", "C", "V", "G")
  kN$G <- c("T", "Y", "F", "C", "V", "B", "H")
  kN$H <- c("Y", "U", "G", "V", "B", "N", "J")
  kN$J <- c("U", "I", "H", "B", "N", "M", "K")
  kN$K <- c("I", "O", "J", "N", "M", "L")
  kN$L <- c("O", "P", "K", "M")
  kN$Z <- c("A", "S", "D", "X")
  kN$X <- c("S", "D", "F", "Z", "C")
  kN$C <- c("D", "F", "G", "X", "V")
  kN$V <- c("F", "G", "H", "C", "B")
  kN$B <- c("G", "H", "J", "V", "N")
  kN$N <- c("H", "J", "K", "B", "M")
  kN$M <- c("J", "K", "L", "N")
  aux <- match(x, allStatesAbr)
  abr <- allStatesAbr[aux]
  state <- allStates[aux]
  if (is.na(aux)) {
    x1 <- substr(x, 1, 1)
    x2 <- substr(x, 2, 2)
    ne1 <- kN[[x1]]
    ne2 <- kN[[x2]]
    aux1 <- match(paste(ne1, x2, sep = ""), allStatesAbr, nomatch = 0)
    abr1 <- allStatesAbr[aux1]
    state1 <- allStates[aux1]
    aux2 <- match(paste(x1, ne2, sep = ""), allStatesAbr, nomatch = 0)
    abr2 <- allStatesAbr[aux2]
    state2 <- allStates[aux2]
    abr <- c(abr1, abr2)
    state <- c(state1, state2)
  }
  list(abr = paste(abr, collapse = ", "), state = paste(state, collapse = ", "))
}


CorrectStates <- function(x) {
  le <- length(x)
  out <- matrix("", le, 2)
  colnames(out) <- c("abreviation", "name")
  for (i in seq(le)) {
    xx <- as.character(x[i])
    if (nchar(xx) == 2) {
      aux <- CorrectStateAbreviation(xx)
      out[i, 1] <- aux$abr
      out[i, 2] <- aux$state
    }
  }
  out
}


#############################################
#############################################
#############################################


require(synapseClient)
synapseLogin()


times <- read.delim(getFileLocation(synGet("syn7404971")), sep = "\t", stringsAsFactors = FALSE)
dim(times)
head(times)

str(times)

table(times[,"statename"])

summary(times$UTC_offset)

hist(times$UTC_offset)

times2 <- na.omit(times)
dim(times2)


table(times2[, "statename"])

table(times2[, "UTC_offset"])


times3 <- times[which(is.na(times$UTC_offset)),]
dim(times3)

tb <- table(times3[, "statename"])
dim(tb)

stb <- sort(tb, decreasing = TRUE)



x <- names(stb)
xx <- CorrectStates(x)



out <- cbind(stb, xx)

write.csv(out, file = "problematic_missing_time_zones.csv")

