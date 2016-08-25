# Define Space
springBras <- cbind(c(-16,-12),rep(0,2))
rotBras <- cbind(c(-5,-3),rep(0,2))
springMat <- cbind(rep(0,2),c(7,9))
blockBody <- cbind(c(-7,-4),1.3*sqrt(c(-7,-4)+15)-1.5)

# Shape parameters
lengthBras <- 18
widthBras <- .5
lengthMat <- 10
widthMat <- 1
lengthBall <- lengthBras - 0.9
widthBall <- 1

# Mechanical properties
inertiaBras <- 5
stiffnessSpring <- 0.25
ballFriction <- 0.1
dt <- 0.05

# Nice colors (Tango)
Aluminium1 <- "#eeeeec" ; Aluminium2 <- "#d3d7cf" ; Aluminium3 <- "#babdb6" ; Aluminium4 <- "#888a85" ; Aluminium5 <- "#555753" ; Aluminium6 <- "#2e3436"
lightPurple <- "#ad7fa8" ; lightBlue <- "#729fcf" ; lightGreen <- "#8ae234" ; lightChocolate <- "#e9b96e" ; lightRed <- "#ef2929" ; lightOrange <- "#fcaf3e" ; lightButter <- "#fce94f"
mediumPurple <- "#75507b" ; mediumBlue <- "#3465a4" ; mediumGreen <- "#73d216" ; mediumChocolate <- "#c17d11" ; mediumRed <- "#cc0000" ; mediumOrange <- "#f57900" ; mediumButter <- "#edd400"
darkPurple <- "#5c3566" ; darkBlue <-  "#204a87" ; darkGreen <- "#4e9a06" ; darkChocolate <- "#8f5902" ; darkRed <- "#a40000" ; darkOrange <- "#ce5c00" ; darkButter <- "#c4a000"
