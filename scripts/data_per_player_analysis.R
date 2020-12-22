
data <- read.csv(file = "<your folder>", encoding="UTF-8", stringsAsFactors = FALSE)


# This script either plots total points per player against gameweeks or other indices.
# If against is false, total points are plotted against gameweeks, from nLast to latest gameweek.
# You can lower the amount of indices used simply by removing them, as many indices fade
# the effect of important indices, such as threat or influence.
plot_by <- function(team_names, index, index2, index3, index4, index5, nLast, against = FALSE) {
  
  if (index == 'threat' 
      || index == 'influence' 
      || index == 'creativity'
      || index == 'bps') {plot(NA, ylim = c(-2, 100), xlim = c(1, nLast), xlab="gmw", ylab=index)}
  else if (index == 'value') {plot(NA, ylim = c(-2, 130), xlim = c(1, nLast), xlab="gmw", ylab=index)}
  else if (index == 'ict_index') {plot(NA, ylim = c(-2, 40), xlim = c(1, nLast), xlab="gmw", ylab=index)}
  else {plot(NA, ylim = c(-2, 30), xlim = c(1, nLast), xlab="gmw", ylab=index)}
  
  n <- length(team_names)
  
  cols <- colorRampPalette(c("#ffaaff", "#3794bf", "#df8640"))(n)
  
  for (i in 1:length(team_names)) {
    name <- team_names[i]

    p <- data[, index][data$name == name]
    p <- tail(p,nLast)
    ag1 <- data[, index2][data$name == name]
    ag1 <- tail(ag1,nLast)

    ag2 <- data[, index3][data$name == name]
    ag2 <- tail(ag2,nLast)

    ag3 <- data[, index4][data$name == name]
    ag3 <- tail(ag3,nLast)
 
    ag4 <- data[, index5][data$name == name]
    ag4 <- tail(ag4,nLast)

    x <- seq((length(p)-nLast+1),length(p))

    #print(length(p))
    if (against == TRUE) { 
      
      mod <- lm(p ~ ag1 + ag2 + ag3 + ag4) 
    }
    else { 
      mod <- lm(p ~ x) 
    }
    points(p, col = cols[i], lwd=2)
    prediction <- predict(mod, data.frame())
    #cat(name,je)
    lines(prediction, col = cols[i], lwd=3)
    #lines(p, col = cols[i], lwd=1)
    
  }
  legend( x="topleft", legend=team_names, col=cols, pch=15, lty=0)
  
  }
son <- c("Heung-Min Son")
starter_mid <- c("Heung-Min Son", "Mohamed Salah", "Bruno Miguel Borges Fernandes", "Kevin De Bruyne", "James Ward-Prowse")
names_goalies <- c("Alex McCarthy","Hugo Lloris","Sam Johnstone", "Bernd Leno", "Nick Pope", "Edouard Mendy", "Alisson Ramses Becker", "Ederson Santana de Moraes", "Karl Darlow")

# This function shows whether the results have a statistical significance, per player.
index_plus <- function(team_names, index, index2, against = FALSE, print = FALSE) {
  
  n <- length(team_names)
  print("##########")
  cat("# Values for ", index)
  print("")
  print("#")
  for (i in 1:length(team_names)) {
    name <- team_names[i]
    p <- data[, index][data$name == name]
    ag <- data[, index2][data$name == name]
    
    p <- p[1:length(p)]
    x <- seq(1,length(p))
    if (against == TRUE) { mod <- lm(p ~ ag[1:length(p)]) }
    else { mod <- lm(p ~ x[1:length(p)]) }
    cat("# " ,name, summary(mod)$coefficients[1, 4], " \n")
    if (print == TRUE) { print(summary(mod)) }
  }
}

# Here are some hard-coded names, if you wish to analyze the teams separately.

names_goalies <- c("Sam Johnstone", "Bernd Leno", "Nick Pope", "Edouard Mendy", "Alisson Ramses Becker", "Ederson Santana de Moraes", "Karl Darlow")
manu <- c("David de Gea", "Harry Maguire", "Aaron Wan-Bissaka", "Victor Lindelöf", "Bruno Miguel Borges Fernandes", "Marcus Rashford", "Anthony Martial", "Donny van de Beek")
liv <- c("Alisson Ramses Becker", "Andrew Robertson", "Mohamed Salah", "Sadio Mané", "Diogo Jota", "Georginio Wijnaldum", "James Milner", "Roberto Firmino")
bha1 <- c("Mathew Ryan", "Tariq Lamptey", "Adam Webster", "Ben White", "Joël Veltman")
bha2 <- c("Solomon March", "Pascal Groß", "Leandro Trossard", "Yves Bissouma", "Neal Maupay", "Danny Welbeck")
ful <- c("Alphonse Areola","Ola Aina","Antonee Robinson","Ademola Lookman", "Tom Cairney", "André-Frank Zambo Anguissa", "Bobby Decordova-Reid")
lee <- c("Meslier", "Dallas", "Koch", "Ayling", "Alioski", "Klich", "Sousa de Azevedo e Costa", "Bamford")
new <- c("Karl Darlow", "Federico Fernández", "Jeff Hendrick", "Allan Saint-Maximin", "Callum Wilson")
che1 <- c("Edouard Mendy", "Kurt Zouma", "Benjamin Chilwell", "Reece James", "Thiago Thiago", "Jorge Luiz Frello Filho")
che2 <- c("Mason Mount", "Hakim Ziyech", "N'Golo Kanté", "Kai Havertz", "Timo Werner", "Tammy Abraham") 
mci1 <- c("Ederson Santana de Moraes", "Kyle Walker", "João Pedro Cavaco Cancelo", "Rúben Santos Gato Alves Dias", "Benjamin Mendy", "John Stones") 
mci2 <- c("Riyad Mahrez", "Kevin De Bruyne", "Raheem Sterling", "Ferran Torres", "Phil Foden", "Gabriel Fernando de Jesus")
ars <- c("Bernd Leno", "Gabriel Magalhães", "Héctor Bellerín", "Pierre-Emerick Aubameyang", "Willian Borges Da Silva")
stn <- c("Alex McCarthy", "Jannik Vestergaard", "Jan Bednarek", "Kyle Walker-Peters", "Ryan Bertrand", "James Ward-Prowse", "Stuart Armstrong", "Che Adams", "Danny Ings")
spur <- c("Hugo Lloris", "Eric Dier", "Sergio Reguilón", "Toby Alderweireld", "Serge Aurier", "Heung-Min Son", "Pierre-Emile Højbjerg", "Tanguy Ndombele", "Harry Kane")
whu1 <- c("Lukasz Fabianski", "Aaron Cresswell", "Angelo Ogbonna", "Fabián Balbuena", "Arthur Masuaku")
whu2 <- c("Vladimir Coufal", "Jarrod Bowen", "Pablo Fornals", "Tomas Soucek", "Declan Rice", "Sébastien Haller", "Michail Antonio")
wlv1 <- c("Rui Pedro dos Santos Patrício", "Romain Saïss", "Willy Boly", "Max Kilman", "Nélson Cabral Semedo", "Fernando Marçal")
wlv2 <- c("Pedro Lomba Neto", "Daniel Castelo Podence", "Leander Dendoncker", "Fabio Silva")
cry <- c("Vicente Guaita", "Scott Dann", "Wilfried Zaha", "Andros Townsend", "Eberechi Eze", "Cheikhou Kouyaté", "Jordan Ayew")

starter_def <- c("Alex McCarthy", "Héctor Bellerín", "Benjamin Chilwell", "Charlie Taylor")
starter_mid <- c("Heung-Min Son", "Mohamed Salah", "Bruno Miguel Borges Fernandes", "Kevin De Bruyne", "Diogo Jota", "Paul Pogba")
starter_str <- c("Dominic Calvert-Lewin", "Timo Werner", "Che Adams")

par(mfrow=c(1,1))
plot_by(starter_str, 'total_points', 'opponent_team', 'influence','threat', 'creativity',5,TRUE)

index_plus(starter_str, 'total_points', 'opponent_team', against = TRUE, print = TRUE)


