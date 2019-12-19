ggplot2.two_x_axis <- function(g1, g2) {
  g1 <- ggplotGrob(g1)
  g2 <- ggplotGrob(g2)
  
  # Get the location of the plot panel in g1.
  # These are used later when transformed elements of g2 are put back into g1
  pp <- c(subset(g1$layout, name == 'panel', se = t:r))
  
  # Overlap panel for second plot on that of the first plot
  g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == 'panel')]], pp$t, pp$l, pp$b, pp$l)
  
  # Then proceed as before:
  
  hinvert_title_grob <- function(grob){
    
    # Swap the widths
    heights <- grob$heights
    grob$heights[1] <- heights[3]
    grob$heights[3] <- heights[1]
    grob$vp[[1]]$layout$heights[1] <- heights[3]
    grob$vp[[1]]$layout$heights[3] <- heights[1]
    
    # Fix the justification
    grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
    grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
    grob$children[[1]]$y <- unit(1, 'npc') - grob$children[[1]]$y
    grob
  }
  
  # Get the x axis title from g2
  index <- which(g2$layout$name == 'xlab-b') # Which grob contains the y axis title?
  xlab <- g2$grobs[[index]]        # Extract that grob
  xlab <- hinvert_title_grob(xlab)     # Swap margins and fix justifications
  
  # Put the transformed label on the top side of g1
  g1 <- gtable_add_rows(g1, g2$heights[g2$layout[index, ]$t], pp$t-1)
  g1 <- gtable_add_grob(g1, xlab, pp$t-1, pp$l, pp$t-1, pp$r, clip = 'off', name = 'topxlab')
  
  # Get the x axis from g2 (axis line, tick marks, and tick mark labels)
  index <- which(g2$layout$name == 'axis-b')  # Which grob
  xaxis <- g2$grobs[[index]]          # Extract the grob
  
  # yaxis is a complex of grobs containing the axis line, the tick marks, and the tick mark labels.
  # The relevant grobs are contained in axis$children:
  #   axis$children[[1]] contains the axis line;
  #   axis$children[[2]] contains the tick marks and tick mark labels.
  
  # First, move the axis line to the left
  xaxis$children[[1]]$y <- unit.c(unit(0, 'npc'), unit(0, 'npc'))
  
  # Second, swap tick marks and tick mark labels
  ticks <- xaxis$children[[2]]
  ticks$heights <- rev(ticks$heights)
  ticks$grobs <- rev(ticks$grobs)
  
  # Third, move the tick marks
  ticks$grobs[[2]]$y <- ticks$grobs[[2]]$y - unit(1, 'npc') + unit(3, 'pt')
  
  # Fourth, swap margins and fix justifications for the tick mark labels
  ticks$grobs[[1]] <- hinvert_title_grob(ticks$grobs[[1]])
  
  # Fifth, put ticks back into yaxis
  xaxis$children[[2]] <- ticks
  
  # Put the transformed xaxis on the top side of g1
  g1 <- gtable_add_rows(g1, g2$heights[g2$layout[index, ]$t], pp$t-1)
  g1 <- gtable_add_grob(g1, xaxis, pp$t+1, pp$l, pp$t+1, pp$r, clip = 'off', name = 'axis-t')
  grid.newpage()
  grid.draw(g1)
}