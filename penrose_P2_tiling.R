# This script plots a P2 Penrose tiling. 
# It is made of "kite" tiles and "arrow" tiles. It is a periodic
# and very pretty.

# function to add transparancy to a color
add_transparancy <- function(col, alpha) {
    col_rgb = col2rgb(col, alpha = alpha)
    return(rgb(col_rgb[1], 
               col_rgb[2],
               col_rgb[3],
               col_rgb[4], 
               maxColorValue = 255)
    )
}

# kite color
col_k   <- add_transparancy("cornflowerblue", 1) #add_transparancy("white", 1)
# arrows color 
col_a    <- add_transparancy("yellow", 1)#add_transparancy("mistyrose", 1)
# line color
col_line <- "grey"#"ivory"

# number of iterations (more iterations make more tiles)
it_max   <- 8
# line width
lwd <- 0.5

# should a rectangular box be ?
draw_rect <- F
# if yes, give the dimensions of the rectangle (1 x 1 takes the whole tiling)
width_rect <- 0.8
height_rect <- 0.5

# export the image to jpeg ?
export_jpeg <- T
# if yes, export file
export_file <- "im_penrose_kite_star_p2.jpeg"
# jpeg image parameters
width <- 50
height <- 30
res <- 600
pointsize <- 5

# shape at the center of the plot
start <- "star" # either "sun" or "star"

# angle a = pi / 5
a <- pi / 5
# golden ratio
phi <-  0.5 * (1 + sqrt(5))
# function to get the perpendicular vector to a vector u
orth_direct <- function(u) {
    return(c(-u[2], u[1]))
}

# function to get the distance between two points
dist <- function(A, B) {
    return(sqrt((A[1] - B[1]) ^ 2 + (A[2] - B[2]) ^ 2))
}
# function to draw a "kite" tile. 
# S are the coordinate of the summit of the kite (pointy part)
# C are the coordinate of the bottom of the kite (less pointy part)
draw_kite <- function(S, C, 
                      col = "red", 
                      border = NA, 
                      lwd = 0.5) {
    u <- C - S
    v <- orth_direct(u)
    A <- S + cos(a) * u + sin(a) * v
    B <- S + cos(a) * u - sin(a) * v
    
    polygon(c(S[1], A[1], C[1], B[1]), 
            c(S[2], A[2], C[2], B[2]),
            col = col, border = border, lwd = lwd)
}
# function to draw a "arrow" tile
# S are the coordinate of the summit of the arrow (pointy part)
# C are the coordinate of the bottom of the arrow (concave angle)
draw_arrow <- function(S, C, 
                       col = "blue", 
                       border = NA, 
                       lwd = 0.5) {
    u <- C - S
    v <- orth_direct(u)
    A <- S + cos(a) * phi * u + sin(a) * phi * v
    B <- S + cos(a) * phi * u - sin(a) * phi * v
    polygon(c(S[1], A[1], C[1], B[1]), 
            c(S[2], A[2], C[2], B[2]),
            col = col, border = border, lwd = lwd)
}

# function to cut a "kite" tile into 2 "kites" and an "arrow" tile
cut_kite <- function(S, C) {
    u <- C - S
    v <- orth_direct(u)
    S1 <- S + cos(a) * u + sin(a) * v
    S2 <- S + cos(a) * u - sin(a) * v
    C1 <- C2 <- C - 1 / phi ^ 2 * u
    S3 <- S
    C3 <- S + (S2 - S) * (phi - 1) / phi
    return(list(S1 = S1, C1 = C1, S2 = S2, C2 = C2, S3 = S3, C3 = C3))
}

# function to cut a "arrow" tile into a "kite" and a "arrow" tile
cut_arrow <- function(S, C) {
    u <- C - S
    v <- orth_direct(u)
    S_k <- S
    C_k <- C
    S_a  <- S + cos(a) * phi * u + sin(a) * phi * v
    C_a  <- S_a + 1 / phi ^ 2 * (S - S_a)
    
    return(list(S_k = S_k, C_k = C_k, S_a = S_a, C_a = C_a))
}

# initialize
it <- 0
n  <- 5

# define tiles of the central shape
if (start == "sun") {
    n_k <- 5
    n_a  <- 0
    S_k <- C_k <- matrix(0, nrow = 2, ncol = n_k)
    C_k[1, ] <- cos(a * (1 + 2 * 0:4))
    C_k[2, ] <- sin(a * (1 + 2 * 0:4))
    S_a <- C_a <- matrix(nrow = 2, ncol = 0)
} else if (start == "star") {
    n_k <- 0
    n_a  <- 5
    S_a <- C_a <- matrix(0, nrow = 2, ncol = n_a)
    C_a[1, ] <- cos(a * (1 + 2 * 0:4))
    C_a[2, ] <- sin(a * (1 + 2 * 0:4))
    S_k <- C_k <- matrix(nrow = 2, ncol = 0)    
}

# 
while (it <= it_max) {
    it <- it + 1
    
    # total number of tiles
    n_p <- n
    # present number of kite and arrow tiles
    n_k_p <- n_k
    n_a_p  <- n_a
    # present coordinates of kite tiles
    S_k_p <- S_k
    C_k_p <- C_k
    # present coordinates of arrow tiles
    S_a_p <- S_a
    C_a_p <- C_a
    # new increased number of kite and arrow tiles
    n_k <- n_a_p + 2 * n_k_p
    n_a  <- n_a_p + n_k_p
    # new total number of tiles
    n    <- n_k + n_a
    
    # Let's compute the new coordinates of the tiles
    S_k <- C_k <- matrix(0, nrow = 2, ncol = n_k)
    S_a  <- C_a  <- matrix(0, nrow = 2, ncol = n_a)
    if (n_k_p > 0) {
        # cut all kite tiles in 3 (2 kites and 1 arrow)
        for (i in 1:n_k_p) {
            dec <- cut_kite(S_k_p[, i], C_k_p[, i])
            S_k[, 2 * i - 1] <- dec$S1 
            C_k[, 2 * i - 1] <- dec$C1
            S_k[, 2 * i]     <- dec$S2 
            C_k[, 2 * i]     <- dec$C2
            S_a[, i] <- dec$S3 
            C_a[, i] <- dec$C3
        }
    }
    if (n_a_p > 0) {
        # cit all arrow tiles in 2 (1 kite and 1 arrow) 
        for (i in 1:n_a_p) {
            dec <- cut_arrow(S_a_p[, i], C_a_p[, i])
            S_k[, 2 * n_k_p + i] <- dec$S_k
            C_k[, 2 * n_k_p + i] <- dec$C_k
            S_a[, n_k_p + i] <- dec$S_a 
            C_a[, n_k_p + i] <- dec$C_a
        }
    }
    # If it's the end, make a plot.
    if (it == it_max) {
        if (export_jpeg) {
            jpeg(export_file, 
                 width = width, height = height, 
                 res = res, pointsize = pointsize, units = "cm")
        }
        # no margins
        par(mai = c(0, 0, 0, 0))
        # make an empty plot
        plot(NULL, xlim = c(-0.6, 0.6), 
             ylim = c(-0.2, 0.2), 
             xlab = "", ylab = "", asp = 1, 
             xaxt = "n", yaxt = "n", bty = "n")
        # draw all kite tiles one by one.
        for (i in 1:n_k) {
            S <- S_k[, i]
            C <- C_k[, i]
            draw_kite(S, C, col = col_k, border = col_line, lwd = lwd)
        }
        # draw all arrow tiles one by one
        for (i in 1:n_a) {
            S <- S_a[, i]
            C <- C_a[, i]
            draw_arrow(S, C, col_a, border = col_line, lwd = lwd)
        }
        # eventually draw a rectangle
        if (draw_rect) {
            points(width_rect / 2 * c(-1, -1, 1, 1, -1), height_rect / 2 * c(1, -1, -1, 1, 1), type = "l", lwd = 2)
        }
        if (export_jpeg) {
            dev.off()
        }
    }
}