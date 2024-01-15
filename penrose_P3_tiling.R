# This script plots a P3 Penrose tiling. 
# It is made of big and small rhombus tiles. It is a periodic
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

# big rhomb color
col_b   <- add_transparancy("yellow", 0.51) #add_transparancy("white", 1)
# small rhomb color 
col_s    <- add_transparancy("deepskyblue", 0.51)#add_transparancy("mistyrose", 1)
# line color
col_line <- "black"#"ivory"

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
export_file <- "im_penrose_rhomb_p3.jpeg"
# jpeg image parameters
width <- 50
height <- 30
res <- 600
pointsize <- 5


# number of iterations (the higher, the more tiles)
n_iter <- 6

# function to get the perpendicular vector to a vector u
orth_direct <- function(u) {
  return(c(-u[2], u[1]))
}
# function to compute euclidian distance between 2 points
dist <- function(A, B) {
  A = c(A)
  B = c(B)
  return(sqrt((A[1] - B[1]) ^ 2 + (A[2] - B[2]) ^ 2))
}

# golden ratio
phi <- (1 + sqrt(5)) / 2

#function to draw a big rhomb tile
draw_big_rhomb <- function(top, bot,
                           col = "red", 
                           border = "black", 
                           lwd = 0.5) {
  u <- top - bot 
  v <- orth_direct(u)
  
  right <- (bot + top) / 2 - v * sin(pi / 5) / phi
  left  <- (bot + top) / 2 + v * sin(pi / 5) / phi
  pts <- cbind(bot, right, top, left)
  
  polygon(pts[1, ], pts[2, ], 
          col = col, border = border, lwd = lwd)
  
}

# function to draw a small rhomb tile
draw_small_rhomb <- function(top, bot,
                            col = "green", 
                            border = "black", 
                            lwd = 0.5) {
  
  u <- top - bot 
  v <- orth_direct(u)
  
  right <- (bot + top) / 2 - v * tan(pi / 10) / 2
  left  <- (bot + top) / 2 + v * tan(pi / 10) / 2
  pts <- cbind(bot, right, top, left)
  
  polygon(pts[1, ], pts[2, ], 
          col = col, border = border, lwd = lwd)
  
}

# function to cut a big rhomb in 3 big rhombs (1 + 2 half) 
# and 2 (half) small rhombs
cut_big_rhomb <- function(top, bot) {
  u <- top - bot 
  v <- orth_direct(u)
  mid <- (top + bot) / 2
  node <- top - u / (1 + phi)
  
  right <- mid - v * sin(pi / 5) / phi
  left  <- mid + v * sin(pi / 5) / phi
  
  top_big_full <- bot
  bot_big_full <- node
  
  top_big_half_right <- right
  bot_big_half_right <- top
  
  top_big_half_left <- left
  bot_big_half_left <- top
  
  top_small_half_right <- top + (right - top) * phi 
  bot_small_half_right <- node
  
  top_small_half_left <- node
  bot_small_half_left <- top + (left - top) * phi 
  
  return(list(big_full = cbind(top_big_full, bot_big_full), 
              big_half_right = cbind(top_big_half_right, 
                                     bot_big_half_right), 
              big_half_left = cbind(top_big_half_left, 
                                    bot_big_half_left), 
              small_half_right = cbind(top_small_half_right, 
                                       bot_small_half_right), 
              small_half_left = cbind(top_small_half_left, 
                                      bot_small_half_left))
        )
}

# function to cut a small rhomb in two (half) small rhombs 
# and 2 (half) big rhombs
cut_small_rhomb <- function(top, bot) {
  u <- top - bot 
  v <- orth_direct(u)
  
  mid <- (top + bot) / 2
  right <- mid - v * tan(pi / 10) / 2
  left  <- mid + v * tan(pi / 10) / 2
  node_right_low = bot + (right - bot) / phi
  node_right_up = top + (right - top) / phi
  
  top_small_half_low = right + node_right_low - left
  bot_small_half_low = left
  
  top_small_half_up = left
  bot_small_half_up = right + node_right_up - left
  
  top_big_half_low = bot
  bot_big_half_low = left
  
  top_big_half_up = top
  bot_big_half_up = left

  return(list(small_half_low = cbind(top_small_half_low, bot_small_half_low), 
              small_half_up = cbind(top_small_half_up, bot_small_half_up), 
              big_half_low = cbind(top_big_half_low, bot_big_half_low), 
              big_half_up = cbind(top_big_half_up, bot_big_half_up))
  )
}


#initialization
star_pts = cbind(cos(2 * 1:5 * pi / 5), sin(2 * 1:5 * pi / 5))

n_b <- 5
big_rhomb_tops <- cbind(rep(0, 5), rep(0, 5))
big_rhomb_bots <- star_pts

n_s <- 5
small_rhomb_tops <- star_pts
small_rhomb_bots <- star_pts[c(2, 3, 4, 5, 1), ]

for (k in 1:n_iter) {
  # creat list of new rhombs, obtained by cutting currant rhombs.
  big_rhomb_tops_next = big_rhomb_bots_next = matrix(ncol = 2, nrow = 0)
  small_rhomb_tops_next = small_rhomb_bots_next = matrix(ncol = 2, nrow = 0)
  
  # cut all big rhombs
  for (i in 1:n_b) {
    cut = cut_big_rhomb(top = c(big_rhomb_tops[i, ]), bot = c(big_rhomb_bots[i, ]))
    new_big_tops = rbind(cut$big_full[, 1], cut$big_half_right[, 1], cut$big_half_left[, 1])
    big_rhomb_tops_next = rbind(big_rhomb_tops_next, new_big_tops)
    
    new_small_tops = rbind(cut$small_half_left[, 1], cut$small_half_right[, 1])
    new_small_bots = rbind(cut$small_half_left[, 2], cut$small_half_right[, 2])
    
    small_rhomb_tops_next = rbind(small_rhomb_tops_next, new_small_tops)
    small_rhomb_bots_next = rbind(small_rhomb_bots_next, new_small_bots)
    
    new_big_bots = rbind(cut$big_full[, 2], cut$big_half_right[, 2], cut$big_half_left[, 2])
    big_rhomb_bots_next = rbind(big_rhomb_bots_next, new_big_bots)
  }
  
  # cut all small rhombs
  for (i in 1:n_s) {
    cut = cut_small_rhomb(top = c(small_rhomb_tops[i, ]), bot = c(small_rhomb_bots[i, ]))
    
    new_big_tops = rbind(cut$big_half_up[, 1], cut$big_half_low[, 1])
    new_big_bots = rbind(cut$big_half_up[, 2], cut$big_half_low[, 2])
    
    big_rhomb_tops_next <- rbind(big_rhomb_tops_next, new_big_tops)
    big_rhomb_bots_next <- rbind(big_rhomb_bots_next, new_big_bots)
    
      
    new_small_tops <- rbind(cut$small_half_low[, 1], cut$small_half_up[, 1])
    new_small_bots <- rbind(cut$small_half_low[, 2], cut$small_half_up[, 2])
      
    small_rhomb_tops_next <- rbind(small_rhomb_tops_next, new_small_tops)
    small_rhomb_bots_next <- rbind(small_rhomb_bots_next, new_small_bots)
  }
  
  # there can be tiles counted twice or more, let's remove duplicates
  # for big tiles
  M <- cbind(big_rhomb_tops_next, big_rhomb_bots_next)
  uniq_big <- !duplicated(round(M, digits = 10))
  
  # for small tiles
  N <- cbind(small_rhomb_tops_next, small_rhomb_bots_next)
  uniq_small <- !duplicated(round(N, digits = 10))
  
  # new lists of big rhombs
  big_rhomb_bots <- big_rhomb_bots_next[uniq_big, ]
  big_rhomb_tops <- big_rhomb_tops_next[uniq_big, ]
  
  # new lists of small rhombs
  small_rhomb_bots <- small_rhomb_bots_next[uniq_small, ]
  small_rhomb_tops <- small_rhomb_tops_next[uniq_small, ]
  
  # new numbers of big and small rhombs
  n_b <- nrow(big_rhomb_bots)
  n_s <- nrow(small_rhomb_bots)
} 

# Let's do the drawing now
# export to jpeg if asked
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
# draw all big rhomb tiles one by one.
for (i in 1:n_b) {
  draw_big_rhomb(top = c(big_rhomb_tops[i, ]), 
                 bot = c(big_rhomb_bots[i, ]), 
                 col = col_b, 
                 border = col_line, 
                 lwd = lwd)
  }
# draw all small rhomb tiles one by one.
for (i in 1:n_s) {
  draw_small_rhomb(top = c(small_rhomb_tops[i, ]), 
                   bot = c(small_rhomb_bots[i, ]), 
                   col = col_s, 
                   border = col_line, 
                   lwd = lwd)
}  

if (export_jpeg) {dev.off()}
