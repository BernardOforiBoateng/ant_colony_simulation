# Required packages
require(ggplot2)
require(R6)
require(tidyverse)

set.seed(42)

# Global parameters
SIMULATION_PARAMS <- list(
  width = 800,
  height = 800,
  n_ants = 50,
  n_food_sources = 15,
  food_amount = NULL,
  evo_home_rate = 0.8,
  evo_food_rate = 0.1,
  ant_size = 10,
  pheromone_step = 1,
  screen_offset = 30,
  max_speed = 30
)

#' Vector Class - Handles 2D vector operations
Vector <- R6Class("Vector",
                  public = list(
                    x = 0,
                    y = 0,
                    
                    initialize = function(x = 0, y = 0) {
                      self$x <- x
                      self$y <- y
                    },
                    
                    add = function(other) {
                      if (inherits(other, "Vector")) {
                        Vector$new(self$x + other$x, self$y + other$y)
                      } else {
                        Vector$new(self$x + other, self$y + other)
                      }
                    },
                    
                    subtract = function(other) {
                      if (inherits(other, "Vector")) {
                        Vector$new(self$x - other$x, self$y - other$y)
                      } else {
                        Vector$new(self$x - other, self$y - other)
                      }
                    },
                    
                    multiply = function(scalar) {
                      Vector$new(self$x * scalar, self$y * scalar)
                    },
                    
                    divide = function(scalar) {
                      if (scalar != 0) {
                        Vector$new(self$x / scalar, self$y / scalar)
                      } else {
                        Vector$new()
                      }
                    },
                    
                    magnitude = function() {
                      sqrt(self$x^2 + self$y^2)
                    },
                    
                    normalize = function() {
                      mag <- self$magnitude()
                      if (mag > 0) {
                        self$divide(mag)
                      } else {
                        Vector$new()
                      }
                    },
                    
                    heading = function() {
                      atan2(self$y, self$x)
                    },
                    
                    rotate = function(angle) {
                      cos_a <- cos(angle)
                      sin_a <- sin(angle)
                      Vector$new(
                        self$x * cos_a - self$y * sin_a,
                        self$x * sin_a + self$y * cos_a
                      )
                    },
                    
                    copy = function() {
                      Vector$new(self$x, self$y)
                    },
                    
                    within_range = function(other, range) {
                      dx <- abs(self$x - other$x)
                      dy <- abs(self$y - other$y)
                      if (dx > range || dy > range) return(FALSE)
                      dist_sq <- dx^2 + dy^2
                      return(dist_sq <= range^2)
                    },
                    
                    wrap = function(width, height) {
                      self$x <- (self$x + width) %% width
                      self$y <- (self$y + height) %% height
                      self
                    }
                  )
)

#' Helper functions
translate_value <- function(value, min1, max1, min2, max2) {
  min2 + (max2 - min2) * ((value - min1)/(max1 - min1))
}

random_position <- function(width, height, offset = SIMULATION_PARAMS$screen_offset) {
  Vector$new(
    runif(1, offset, width - offset),
    runif(1, offset, height - offset)
  )
}

random_direction <- function() {
  angle <- runif(1, 0, 2*pi)
  Vector$new(cos(angle), sin(angle))
}



# First, modify the Food class to ensure proper depletion checks
Food <- R6Class("Food",
                public = list(
                  position = NULL,
                  amount = NULL,
                  
                  initialize = function(position, amount = NULL) {
                    self$position <- position
                    self$amount <- if(is.null(amount)) sample(1:20, 1) else amount
                  },
                  
                  take_food = function(amount = 1) {
                    if (self$amount > 0) {  # Changed from >= to > to ensure proper depletion
                      self$amount <- self$amount - amount
                      return(TRUE)
                    }
                    return(FALSE)
                  },
                  
                  is_depleted = function() {
                    return(self$amount <= 0)
                  }
                )
)

# Modify the FoodMap class to properly handle depleted food sources
FoodMap <- R6Class("FoodMap",
                   public = list(
                     foods = list(),
                     width = NULL,
                     height = NULL,
                     
                     initialize = function(width, height, n_foods = SIMULATION_PARAMS$n_food_sources) {
                       self$width <- width
                       self$height <- height
                       self$foods <- list()
                       
                       for (i in 1:n_foods) {
                         pos <- random_position(width, height)
                         self$foods[[i]] <- Food$new(pos)
                       }
                     },
                     
                     get_closest_food = function(position) {
                       # Filter out depleted food sources first
                       active_foods <- Filter(function(f) !f$is_depleted(), self$foods)
                       
                       if (length(active_foods) == 0) return(NULL)
                       
                       closest <- active_foods[[1]]
                       min_dist <- position$subtract(closest$position)$magnitude()
                       
                       for (food in active_foods[-1]) {
                         dist <- position$subtract(food$position)$magnitude()
                         if (dist < min_dist) {
                           closest <- food
                           min_dist <- dist
                         }
                       }
                       
                       return(closest)
                     },
                     
                     update = function() {
                       # Remove depleted food sources
                       self$foods <- Filter(function(f) !f$is_depleted(), self$foods)
                       
                       # Add new food if needed
                       while (length(self$foods) < SIMULATION_PARAMS$n_food_sources) {
                         pos <- random_position(self$width, self$height)
                         self$foods[[length(self$foods) + 1]] <- Food$new(pos)
                       }
                     }
                   )
)

#' Pheromone Class
Pheromone <- R6Class("Pheromone",
                     public = list(
                       position = NULL,
                       direction = NULL,
                       strength = 100,
                       max_strength = 100,
                       type = "home",
                       evaporation_rate = NULL,
                       
                       initialize = function(position, direction, type = "home") {
                         self$position <- position$copy()
                         self$direction <- direction$copy()
                         self$type <- type
                         self$evaporation_rate <- if(type == "home") 
                           SIMULATION_PARAMS$evo_home_rate else 
                             SIMULATION_PARAMS$evo_food_rate
                       },
                       
                       update = function() {
                         self$strength <- self$strength - self$evaporation_rate
                         return(self$strength > 0)
                       },
                       
                       combine = function(other) {
                         if (!inherits(other, "Pheromone")) return(FALSE)
                         
                         # Average positions and directions
                         self$position <- Vector$new(
                           (self$position$x + other$position$x) / 2,
                           (self$position$y + other$position$y) / 2
                         )
                         
                         self$direction <- Vector$new(
                           (self$direction$x + other$direction$x) / 2,
                           (self$direction$y + other$direction$y) / 2
                         )
                         
                         # Combine strengths up to max
                         self$strength <- min(self$strength + other$strength, self$max_strength)
                         
                         return(TRUE)
                       }
                     )
)

#' PheromoneMap Class - Manages pheromone system
PheromoneMap <- R6Class("PheromoneMap",
                        public = list(
                          home_pheromones = list(),
                          food_pheromones = list(),
                          dispersion_radius = SIMULATION_PARAMS$pheromone_step,
                          
                          initialize = function() {
                            self$home_pheromones <- list()
                            self$food_pheromones <- list()
                          },
                          
                          add_pheromone = function(position, direction, type = "home") {
                            new_pher <- Pheromone$new(position, direction, type)
                            target_list <- if(type == "home") self$home_pheromones else self$food_pheromones
                            
                            # Check for nearby pheromones to combine
                            for (i in seq_along(target_list)) {
                              if (position$within_range(target_list[[i]]$position, self$dispersion_radius)) {
                                target_list[[i]]$combine(new_pher)
                                return()
                              }
                            }
                            
                            # If no combination occurred, add new pheromone
                            if (type == "home") {
                              self$home_pheromones[[length(self$home_pheromones) + 1]] <- new_pher
                            } else {
                              self$food_pheromones[[length(self$food_pheromones) + 1]] <- new_pher
                            }
                          },
                          
                          update = function() {
                            # Update and filter home pheromones
                            self$home_pheromones <- Filter(
                              function(p) p$update(),
                              self$home_pheromones
                            )
                            
                            # Update and filter food pheromones
                            self$food_pheromones <- Filter(
                              function(p) p$update(),
                              self$food_pheromones
                            )
                          },
                          
                          get_direction = function(position, range, type = "home") {
                            pheromones <- if(type == "home") self$home_pheromones else self$food_pheromones
                            
                            total_direction <- Vector$new()
                            count <- 0
                            
                            for (p in pheromones) {
                              if (position$within_range(p$position, range)) {
                                direction <- p$position$subtract(position)
                                strength <- p$strength / 100
                                total_direction <- total_direction$add(direction$multiply(strength))
                                count <- count + 1
                              }
                            }
                            
                            if (count > 0) {
                              return(total_direction$divide(count))
                            }
                            return(Vector$new())
                          },
                          
                          get_plot_data = function() {
                            # Prepare home pheromone data
                            home_data <- do.call(rbind, lapply(self$home_pheromones, function(p) {
                              data.frame(
                                x = p$position$x,
                                y = p$position$y,
                                strength = p$strength,
                                type = "home"
                              )
                            }))
                            
                            # Prepare food pheromone data
                            food_data <- do.call(rbind, lapply(self$food_pheromones, function(p) {
                              data.frame(
                                x = p$position$x,
                                y = p$position$y,
                                strength = p$strength,
                                type = "food"
                              )
                            }))
                            
                            # Return combined data
                            return(list(
                              home = if(is.null(home_data)) data.frame() else home_data,
                              food = if(is.null(food_data)) data.frame() else food_data
                            ))
                          }
                        )
)



#' Ant Class with Improved Food Finding Logic
Ant <- R6Class("Ant",
               public = list(
                 position = NULL,
                 velocity = NULL,
                 nest_position = NULL,
                 max_speed = 15,
                 smell_radius = 30,
                 trigger_radius = 10,
                 has_food = FALSE,
                 known_food_location = NULL,
                 returning_to_known_food = FALSE,
                 last_food_amount = NULL,
                 
                 initialize = function(position, nest_position) {
                   self$position <- position$copy()
                   self$nest_position <- nest_position$copy()
                   self$velocity <- random_direction()$multiply(self$max_speed)
                   self$known_food_location <- NULL
                   self$returning_to_known_food <- FALSE
                   self$last_food_amount <- NULL
                 },
                 
                 update = function(food_map, pheromone_map, width, height) {
                   if (self$has_food) {
                     success <- self$return_to_nest(pheromone_map)
                     if (success) {
                       # Only return to known food if it wasn't depleted
                       if (!is.null(self$last_food_amount) && self$last_food_amount > 1) {
                         self$returning_to_known_food <- TRUE
                       } else {
                         self$known_food_location <- NULL
                         self$returning_to_known_food <- FALSE
                         self$last_food_amount <- NULL
                       }
                     }
                   } else {
                     if (self$returning_to_known_food && !is.null(self$known_food_location)) {
                       self$return_to_food(food_map, pheromone_map)
                     } else {
                       self$search_for_food(food_map, pheromone_map)
                     }
                   }
                   
                   # Update position with velocity
                   self$position <- self$position$add(self$velocity)
                   self$position$wrap(width, height)
                 },
                 
                 return_to_food = function(food_map, pheromone_map) {
                   # Verify food still exists at known location
                   closest_food <- food_map$get_closest_food(self$known_food_location)
                   
                   if (is.null(closest_food) || closest_food$is_depleted() || 
                       closest_food$position$subtract(self$known_food_location)$magnitude() > self$trigger_radius) {
                     # Food source is gone or depleted, forget it
                     self$known_food_location <- NULL
                     self$returning_to_known_food <- FALSE
                     self$last_food_amount <- NULL
                     self$wander()
                     return()
                   }
                   
                   dist <- self$position$subtract(self$known_food_location)$magnitude()
                   
                   if (dist < self$trigger_radius) {
                     if (closest_food$take_food()) {
                       self$has_food <- TRUE
                       self$last_food_amount <- closest_food$amount
                       self$velocity <- self$velocity$multiply(-1)
                     } else {
                       # Can't take food, source must be depleted
                       self$known_food_location <- NULL
                       self$returning_to_known_food <- FALSE
                       self$last_food_amount <- NULL
                       self$wander()
                     }
                   } else {
                     direction <- self$known_food_location$subtract(self$position)
                     self$velocity <- direction$normalize()$multiply(self$max_speed)
                   }
                   
                   pheromone_map$add_pheromone(self$position, self$velocity, "home")
                 },
                 
                 search_for_food = function(food_map, pheromone_map) {
                   closest_food <- food_map$get_closest_food(self$position)
                   
                   if (is.null(closest_food)) {
                     self$wander()
                     return()
                   }
                   
                   dist <- self$position$subtract(closest_food$position)$magnitude()
                   
                   if (dist < self$trigger_radius) {
                     if (!closest_food$is_depleted() && closest_food$take_food()) {
                       self$has_food <- TRUE
                       self$last_food_amount <- closest_food$amount
                       self$known_food_location <- closest_food$position$copy()
                       self$velocity <- self$velocity$multiply(-1)
                     } else {
                       self$wander()
                     }
                   } else if (dist < self$smell_radius) {
                     # Move towards food if within smell radius
                     direction <- closest_food$position$subtract(self$position)
                     self$velocity <- direction$normalize()$multiply(self$max_speed)
                   } else {
                     # Follow pheromones or wander
                     pheromone_direction <- pheromone_map$get_direction(
                       self$position,
                       self$smell_radius,
                       "food"
                     )
                     
                     if (pheromone_direction$magnitude() > 0) {
                       self$velocity <- pheromone_direction$normalize()$multiply(self$max_speed)
                     } else {
                       self$wander()
                     }
                   }
                   
                   pheromone_map$add_pheromone(self$position, self$velocity, "home")
                 },
                 
                 return_to_nest = function(pheromone_map) {
                   dist_to_nest <- self$position$subtract(self$nest_position)$magnitude()
                   
                   if (dist_to_nest < self$trigger_radius) {
                     self$has_food <- FALSE
                     # Return value indicates successful food delivery
                     return(TRUE)
                   } else {
                     direction <- self$nest_position$subtract(self$position)
                     self$velocity <- direction$normalize()$multiply(self$max_speed)
                   }
                   
                   pheromone_map$add_pheromone(self$position, self$velocity, "food")
                   return(FALSE)
                 },
                 
                 wander = function() {
                   angle <- runif(1, -pi/6, pi/6)
                   self$velocity <- self$velocity$rotate(angle)$normalize()$multiply(self$max_speed)
                 }
               )
)

#' Colony Class with Enhanced Metrics and Analysis
Colony <- R6Class("Colony",
                  public = list(
                    # Basic properties
                    ants = list(),
                    food_map = NULL,
                    pheromone_map = NULL,
                    nest_position = NULL,
                    width = NULL,
                    height = NULL,
                    food_collected = 0,
                    
                    # Metrics tracking
                    metrics = NULL,
                    
                    initialize = function(width = SIMULATION_PARAMS$width, 
                                          height = SIMULATION_PARAMS$height, 
                                          n_ants = SIMULATION_PARAMS$n_ants) {
                      self$width <- width
                      self$height <- height
                      self$nest_position <- Vector$new(width/2, height/2)
                      self$food_map <- FoodMap$new(width, height)
                      self$pheromone_map <- PheromoneMap$new()
                      
                      # Initialize ants
                      self$ants <- lapply(1:n_ants, function(i) {
                        Ant$new(self$nest_position$copy(), self$nest_position)
                      })
                      
                      # Initialize metrics storage
                      self$metrics <- list(
                        steps = integer(),
                        food_collected = integer(),
                        food_collection_rate = numeric(),
                        active_trails = integer(),
                        pheromone_coverage = numeric(),
                        ant_clustering = numeric(),
                        efficiency_score = numeric(),
                        avg_trail_strength = numeric(),
                        time_stamps = numeric(),
                        ants_with_food = integer(),
                        avg_distance_to_nest = numeric()
                      )
                    },
                    
                    update = function() {
                      # Update colony components
                      self$food_map$update()
                      self$pheromone_map$update()
                      
                      # Update all ants and track changes
                      for (ant in self$ants) {
                        previous_has_food <- ant$has_food
                        ant$update(self$food_map, self$pheromone_map, self$width, self$height)
                        
                        if (previous_has_food && !ant$has_food) {
                          self$food_collected <- self$food_collected + 1
                        }
                      }
                    },
                    
                    collect_metrics = function(step) {
                      # Basic metrics
                      collection_rate <- self$food_collected / step
                      
                      # Pheromone metrics
                      total_pheromones <- length(self$pheromone_map$home_pheromones) + 
                        length(self$pheromone_map$food_pheromones)
                      
                      all_pheromone_strengths <- c(
                        sapply(self$pheromone_map$home_pheromones, function(p) p$strength),
                        sapply(self$pheromone_map$food_pheromones, function(p) p$strength)
                      )
                      avg_strength <- if(length(all_pheromone_strengths) > 0) mean(all_pheromone_strengths) else 0
                      
                      # Ant behavior metrics
                      ant_positions <- do.call(rbind, lapply(self$ants, function(ant) {
                        c(ant$position$x, ant$position$y)
                      }))
                      
                      clustering <- if(nrow(ant_positions) > 1) mean(dist(ant_positions)) else 0
                      
                      ants_with_food <- sum(sapply(self$ants, function(ant) ant$has_food))
                      
                      avg_distance_to_nest <- mean(sapply(self$ants, function(ant) {
                        ant$position$subtract(self$nest_position)$magnitude()
                      }))
                      
                      # Environment coverage metrics
                      total_area <- self$width * self$height
                      pheromone_area <- total_pheromones * pi * (SIMULATION_PARAMS$pheromone_step^2)
                      coverage <- pheromone_area / total_area
                      
                      # Efficiency calculations
                      efficiency <- collection_rate * (1 - coverage) * (1 - clustering/self$width)
                      
                      # Store all metrics
                      self$metrics$steps <- c(self$metrics$steps, step)
                      self$metrics$food_collected <- c(self$metrics$food_collected, self$food_collected)
                      self$metrics$food_collection_rate <- c(self$metrics$food_collection_rate, collection_rate)
                      self$metrics$active_trails <- c(self$metrics$active_trails, total_pheromones)
                      self$metrics$pheromone_coverage <- c(self$metrics$pheromone_coverage, coverage)
                      self$metrics$ant_clustering <- c(self$metrics$ant_clustering, clustering)
                      self$metrics$efficiency_score <- c(self$metrics$efficiency_score, efficiency)
                      self$metrics$avg_trail_strength <- c(self$metrics$avg_trail_strength, avg_strength)
                      self$metrics$time_stamps <- c(self$metrics$time_stamps, Sys.time())
                      self$metrics$ants_with_food <- c(self$metrics$ants_with_food, ants_with_food)
                      self$metrics$avg_distance_to_nest <- c(self$metrics$avg_distance_to_nest, avg_distance_to_nest)
                    },
                    
                    get_plot_data = function() {
                      # Get ant data with velocity components
                      ant_data <- do.call(rbind, lapply(self$ants, function(ant) {
                        data.frame(
                          x = ant$position$x,
                          y = ant$position$y,
                          vx = ant$velocity$x,
                          vy = ant$velocity$y,
                          has_food = ant$has_food,
                          stringsAsFactors = FALSE
                        )
                      }))
                      
                      # Get food data
                      food_data <- do.call(rbind, lapply(self$food_map$foods, function(food) {
                        data.frame(
                          x = food$position$x,
                          y = food$position$y,
                          amount = food$amount,
                          stringsAsFactors = FALSE
                        )
                      }))
                      
                      # Get pheromone data
                      home_pher_data <- do.call(rbind, lapply(self$pheromone_map$home_pheromones, function(p) {
                        data.frame(
                          x = p$position$x,
                          y = p$position$y,
                          strength = p$strength,
                          type = "home",
                          stringsAsFactors = FALSE
                        )
                      }))
                      
                      food_pher_data <- do.call(rbind, lapply(self$pheromone_map$food_pheromones, function(p) {
                        data.frame(
                          x = p$position$x,
                          y = p$position$y,
                          strength = p$strength,
                          type = "food",
                          stringsAsFactors = FALSE
                        )
                      }))
                      
                      # Handle NULL cases
                      if (is.null(ant_data)) ant_data <- data.frame()
                      if (is.null(food_data)) food_data <- data.frame()
                      if (is.null(home_pher_data)) home_pher_data <- data.frame()
                      if (is.null(food_pher_data)) food_pher_data <- data.frame()
                      
                      # Create nest data
                      nest_data <- data.frame(
                        x = self$nest_position$x,
                        y = self$nest_position$y,
                        food_collected = self$food_collected,
                        stringsAsFactors = FALSE
                      )
                      
                      return(list(
                        ants = ant_data,
                        foods = food_data,
                        pheromones = list(
                          home = home_pher_data,
                          food = food_pher_data
                        ),
                        nest = nest_data
                      ))
                    },
                    
                    plot = function() {
                      data <- self$get_plot_data()
                      
                      p <- ggplot() +
                        # Plot pheromones
                        {if(nrow(data$pheromones$home) > 0)
                          geom_point(data = data$pheromones$home,
                                     aes(x = x, y = y, alpha = strength/100),
                                     color = "purple", size = 1)} +
                        {if(nrow(data$pheromones$food) > 0)
                          geom_point(data = data$pheromones$food,
                                     aes(x = x, y = y, alpha = strength/100),
                                     color = "red", size = 1)} +
                        # Plot food sources with labels
                        {if(nrow(data$foods) > 0)
                          geom_point(data = data$foods,
                                     aes(x = x, y = y),
                                     color = "orange",
                                     size = 10)} +
                        {if(nrow(data$foods) > 0)
                          geom_text(data = data$foods,
                                    aes(x = x, y = y, label = amount),
                                    color = "white",
                                    size = 3)} +
                        # Plot nest with food count label
                        geom_point(data = data$nest,
                                   aes(x = x, y = y),
                                   color = "blue",
                                   size = 10) +
                        geom_text(data = data$nest,
                                  aes(x = x, y = y, label = food_collected),
                                  color = "white",
                                  size = 4) +
                        # Plot ants as triangles
                        {if(nrow(data$ants) > 0)
                          geom_polygon(data = do.call(rbind, lapply(1:nrow(data$ants), function(i) {
                            ant <- data$ants[i,]
                            angle <- atan2(ant$vy, ant$vx)  # Use velocity for orientation
                            triangle_points <- data.frame(
                              x = ant$x + c(0, -5, 5) * cos(angle) - c(10, -5, -5) * sin(angle),
                              y = ant$y + c(0, -5, 5) * sin(angle) + c(10, -5, -5) * cos(angle),
                              group = i,
                              has_food = ant$has_food,
                              stringsAsFactors = FALSE
                            )
                          })),
                          aes(x = x, y = y, group = group, fill = has_food),
                          color = NA)} +
                        scale_fill_manual(values = c("white", "orange")) +
                        theme_dark() +
                        theme(
                          legend.position = "none",
                          panel.background = element_rect(fill = "black"),
                          panel.grid = element_blank(),
                          plot.background = element_rect(fill = "black"),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          axis.title = element_blank()
                        ) +
                        coord_fixed() +
                        xlim(0, self$width) +
                        ylim(0, self$height)
                      
                      return(p)
                    },
                    
                    plot_metrics = function() {
                      # Convert metrics to data frame
                      metrics_df <- data.frame(
                        Step = self$metrics$steps,
                        FoodRate = self$metrics$food_collection_rate,
                        Trails = self$metrics$active_trails,
                        Coverage = self$metrics$pheromone_coverage,
                        Clustering = self$metrics$ant_clustering,
                        Efficiency = self$metrics$efficiency_score,
                        TrailStrength = self$metrics$avg_trail_strength,
                        AntsWithFood = self$metrics$ants_with_food,
                        AvgDistance = self$metrics$avg_distance_to_nest
                      )
                      
                      # Create plots
                      p1 <- ggplot(metrics_df, aes(x = Step, y = FoodRate)) +
                        geom_line(color = "blue") +
                        theme_minimal() +
                        labs(title = "Food Collection Rate")
                      
                      p2 <- ggplot(metrics_df, aes(x = Step)) +
                        geom_line(aes(y = Trails), color = "red") +
                        theme_minimal() +
                        labs(title = "Active Pheromone Trails")
                      
                      p3 <- ggplot(metrics_df, aes(x = Step, y = Coverage)) +
                        geom_line(color = "purple") +
                        theme_minimal() +
                        labs(title = "Environment Coverage")
                      
                      p4 <- ggplot(metrics_df, aes(x = Step, y = Efficiency)) +
                        geom_line(color = "green") +
                        theme_minimal() +
                        labs(title = "Colony Efficiency")
                      
                      gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
                    }
                  )
)

# Add to the Colony class methods

Colony$set("public", "generate_analysis_plots", function() {
  # 1. Ant Distribution Heat Map
  ant_positions <- do.call(rbind, lapply(self$ants, function(ant) {
    data.frame(x = ant$position$x, y = ant$position$y)
  }))
  
  p1 <- ggplot(ant_positions, aes(x = x, y = y)) +
    stat_density_2d_filled(alpha = 0.7) +
    geom_point(data = data.frame(x = self$nest_position$x, y = self$nest_position$y),
               aes(x = x, y = y), color = "blue", size = 5) +
    theme_dark() +
    labs(title = "Ant Distribution Heat Map") +
    theme(legend.position = "right")
  
  # 2. Food Source Depletion Rate
  food_data <- do.call(rbind, lapply(self$food_map$foods, function(food) {
    data.frame(x = food$position$x, y = food$position$y, amount = food$amount)
  }))
  
  p2 <- ggplot(food_data, aes(x = amount)) +
    geom_histogram(bins = 10, fill = "orange", color = "black") +
    theme_minimal() +
    labs(title = "Food Source Distribution",
         x = "Remaining Food Amount",
         y = "Count")
  
  # 3. Pheromone Strength Distribution
  pheromone_data <- rbind(
    transform(self$pheromone_map$get_plot_data()$home, trail_type = "Home"),
    transform(self$pheromone_map$get_plot_data()$food, trail_type = "Food")
  )
  
  p3 <- ggplot(pheromone_data, aes(x = strength, fill = trail_type)) +
    geom_density(alpha = 0.5) +
    theme_minimal() +
    labs(title = "Pheromone Strength Distribution",
         x = "Pheromone Strength",
         y = "Density") +
    scale_fill_manual(values = c("purple", "red"))
  
  # 4. Ant Success Rate by Distance
  ant_success_data <- do.call(rbind, lapply(self$ants, function(ant) {
    dist_to_nest <- ant$position$subtract(self$nest_position)$magnitude()
    data.frame(
      distance = dist_to_nest,
      has_food = ant$has_food
    )
  }))
  
  p4 <- ggplot(ant_success_data, aes(x = distance, fill = has_food)) +
    geom_histogram(position = "fill", bins = 20) +
    theme_minimal() +
    labs(title = "Food Collection Success by Distance",
         x = "Distance from Nest",
         y = "Proportion") +
    scale_fill_manual(values = c("gray", "orange"))
  
  # Arrange all plots
  gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
  
  return(list(heatmap = p1, food_dist = p2, 
              pheromone_dist = p3, success_rate = p4))
})

# Add circular visualization of pheromone trails
Colony$set("public", "plot_circular_trails", function() {
  # Get pheromone data
  pher_data <- self$pheromone_map$get_plot_data()
  
  # Convert to polar coordinates
  nest_x <- self$nest_position$x
  nest_y <- self$nest_position$y
  
  convert_to_polar <- function(data) {
    if(nrow(data) == 0) return(data.frame())
    data$r <- sqrt((data$x - nest_x)^2 + (data$y - nest_y)^2)
    data$theta <- atan2(data$y - nest_y, data$x - nest_x)
    return(data)
  }
  
  home_polar <- convert_to_polar(pher_data$home)
  food_polar <- convert_to_polar(pher_data$food)
  
  # Create circular plot
  p <- ggplot() +
    {if(nrow(home_polar) > 0)
      geom_point(data = home_polar,
                 aes(x = theta, y = r, alpha = strength/100),
                 color = "purple", size = 1)} +
    {if(nrow(food_polar) > 0)
      geom_point(data = food_polar,
                 aes(x = theta, y = r, alpha = strength/100),
                 color = "red", size = 1)} +
    coord_polar() +
    theme_minimal() +
    labs(title = "Circular View of Pheromone Trails",
         x = "Angle",
         y = "Distance from Nest")
  
  return(p)
})

# Add to run_simulation function
run_simulation <- function(steps = 300, 
                           plot_interval = 2,
                           width = SIMULATION_PARAMS$width,
                           height = SIMULATION_PARAMS$height,
                           n_ants = SIMULATION_PARAMS$n_ants,
                           collect_metrics_interval = 10) {
  colony <- Colony$new(width, height, n_ants)
  
  # Main simulation loop
  for(step in 1:steps) {
    colony$update()
    
    if(step %% plot_interval == 0) {
      p <- colony$plot()
      print(p)
      Sys.sleep(0.005)
    }
    
    if(step %% collect_metrics_interval == 0) {
      colony$collect_metrics(step)
    }
  }
  
  # Generate additional analysis plots
  dev.new(width = 12, height = 10)
  colony$generate_analysis_plots()
  
  dev.new(width = 8, height = 8)
  print(colony$plot_circular_trails())
  
  return(colony)
}


# Modified run_simulation function to collect metrics
run_simulation <- function(steps = 1000, 
                           plot_interval = 1,
                           width = SIMULATION_PARAMS$width,
                           height = SIMULATION_PARAMS$height,
                           n_ants = SIMULATION_PARAMS$n_ants,
                           collect_metrics_interval = 10) {
  colony <- Colony$new(width, height, n_ants)
  
  options(device.ask.default = FALSE)
  dev.new(width = 10, height = 10, noRStudioGD = TRUE)
  
  for(step in 1:steps) {
    colony$update()
    
    if(step %% plot_interval == 0) {
      p <- colony$plot()
      print(p)
      Sys.sleep(0.005)
    }
    
    if(step %% collect_metrics_interval == 0) {
      colony$collect_metrics(step)
    }
  }
  
  # Plot metrics at the end
  dev.new(width = 12, height = 8)
  colony$plot_metrics()
  
  return(colony)
}

# Function to run multiple simulations with different parameters
run_parameter_study <- function(ant_counts = c(10, 30, 50, 100),
                                steps = 1000,
                                repetitions = 3) {
  results <- list()
  
  for(n_ants in ant_counts) {
    for(rep in 1:repetitions) {
      cat(sprintf("Running simulation with %d ants, repetition %d\n", n_ants, rep))
      colony <- run_simulation(steps = steps, n_ants = n_ants, plot_interval = steps + 1)
      results[[length(results) + 1]] <- list(
        n_ants = n_ants,
        repetition = rep,
        metrics = colony$metrics
      )
    }
  }
  
  return(results)
}

# Function to analyze parameter study results
analyze_results <- function(results) {
  # Convert results to data frame
  results_df <- do.call(rbind, lapply(results, function(r) {
    data.frame(
      n_ants = r$n_ants,
      repetition = r$repetition,
      final_food = tail(r$metrics$food_collected, 1),
      final_efficiency = tail(r$metrics$efficiency_score, 1),
      avg_coverage = mean(r$metrics$pheromone_coverage),
      avg_clustering = mean(r$metrics$ant_clustering)
    )
  }))
  
  # Create summary plots
  p1 <- ggplot(results_df, aes(x = factor(n_ants), y = final_food)) +
    geom_boxplot() +
    labs(title = "Food Collection by Colony Size", x = "Number of Ants")
  
  p2 <- ggplot(results_df, aes(x = factor(n_ants), y = final_efficiency)) +
    geom_boxplot() +
    labs(title = "Efficiency by Colony Size", x = "Number of Ants")
  
  gridExtra::grid.arrange(p1, p2, ncol = 2)
  
  return(results_df)
}


# Add required packages
library(gifski)
library(gganimate)

run_complete_simulation <- function(steps = 300,
                                    plot_interval = 2,
                                    width = SIMULATION_PARAMS$width,
                                    height = SIMULATION_PARAMS$height,
                                    n_ants = SIMULATION_PARAMS$n_ants) {
  colony <- Colony$new(width, height, n_ants)
  frames_data <- list()
  
  # Main simulation loop
  for(step in 1:steps) {
    colony$update()
    
    # Collect metrics
    if(step %% 10 == 0) {
      colony$collect_metrics(step)
    }
    
    # Record frame
    if(step %% plot_interval == 0) {
      current_data <- colony$get_plot_data()
      
      # Show live plot
      print(colony$plot())
      Sys.sleep(0.01)
      
      # Store frame data with proper checks
      frame_info <- list()
      
      # Store ant data
      if(!is.null(current_data$ants) && nrow(current_data$ants) > 0) {
        frame_info$ants <- transform(current_data$ants, frame = step)
      } else {
        frame_info$ants <- data.frame(
          x = numeric(0), y = numeric(0), vx = numeric(0), vy = numeric(0),
          has_food = logical(0), frame = numeric(0)
        )
      }
      
      # Store food data
      if(!is.null(current_data$foods) && nrow(current_data$foods) > 0) {
        frame_info$foods <- transform(current_data$foods, frame = step)
      } else {
        frame_info$foods <- data.frame(
          x = numeric(0), y = numeric(0), amount = numeric(0),
          frame = numeric(0)
        )
      }
      
      # Store pheromone data
      if(!is.null(current_data$pheromones$home) && nrow(current_data$pheromones$home) > 0) {
        frame_info$home_pher <- transform(current_data$pheromones$home, frame = step)
      } else {
        frame_info$home_pher <- data.frame(
          x = numeric(0), y = numeric(0), strength = numeric(0),
          type = character(0), frame = numeric(0)
        )
      }
      
      if(!is.null(current_data$pheromones$food) && nrow(current_data$pheromones$food) > 0) {
        frame_info$food_pher <- transform(current_data$pheromones$food, frame = step)
      } else {
        frame_info$food_pher <- data.frame(
          x = numeric(0), y = numeric(0), strength = numeric(0),
          type = character(0), frame = numeric(0)
        )
      }
      
      # Store nest data
      frame_info$nest <- transform(current_data$nest, frame = step)
      
      frames_data[[length(frames_data) + 1]] <- frame_info
    }
  }
  
  # Combine all frames with error checking
  all_ants <- do.call(rbind, lapply(frames_data, function(x) x$ants))
  all_foods <- do.call(rbind, lapply(frames_data, function(x) x$foods))
  all_home_pher <- do.call(rbind, lapply(frames_data, function(x) x$home_pher))
  all_food_pher <- do.call(rbind, lapply(frames_data, function(x) x$food_pher))
  all_nests <- do.call(rbind, lapply(frames_data, function(x) x$nest))
  
  # Debugging: Print the first few rows of each data frame
  cat("Debugging: all_ants\n")
  print(head(all_ants))
  cat("Debugging: all_foods\n")
  print(head(all_foods))
  cat("Debugging: all_home_pher\n")
  print(head(all_home_pher))
  cat("Debugging: all_food_pher\n")
  print(head(all_food_pher))
  cat("Debugging: all_nests\n")
  print(head(all_nests))
  
  # Create base plot
  p <- ggplot() +
    theme_dark() +
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "black"),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "black"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    ) +
    coord_fixed() +
    xlim(0, width) +
    ylim(0, height)
  
  # Add layers conditionally
  if(nrow(all_home_pher) > 0) {
    p <- p + geom_point(data = all_home_pher,
                        aes(x = x, y = y, alpha = strength/100),
                        color = "purple", size = 1)
  }
  
  if(nrow(all_food_pher) > 0) {
    p <- p + geom_point(data = all_food_pher,
                        aes(x = x, y = y, alpha = strength/100),
                        color = "red", size = 1)
  }
  
  if(nrow(all_foods) > 0) {
    p <- p + geom_point(data = all_foods,
                        aes(x = x, y = y),
                        color = "orange", size = 10)
    p <- p + geom_text(data = all_foods,
                       aes(x = x, y = y, label = amount),
                       color = "white", size = 3)
  }
  
  # Add nest (always present)
  p <- p + geom_point(data = all_nests,
                      aes(x = x, y = y),
                      color = "blue", size = 10)
  p <- p + geom_text(data = all_nests,
                     aes(x = x, y = y, label = food_collected),
                     color = "white", size = 4)
  
  # Add ants if present
  if(nrow(all_ants) > 0) {
    ant_polygons <- do.call(rbind, lapply(seq_len(nrow(all_ants)), function(i) {
      ant <- all_ants[i,]
      angle <- atan2(ant$vy, ant$vx)
      data.frame(
        x = ant$x + c(0, -5, 5) * cos(angle) - c(10, -5, -5) * sin(angle),
        y = ant$y + c(0, -5, 5) * sin(angle) + c(10, -5, -5) * cos(angle),
        group = paste(i, ant$frame),
        has_food = ant$has_food,
        frame = ant$frame,
        stringsAsFactors = FALSE
      )
    }))
    
    p <- p + geom_polygon(data = ant_polygons,
                          aes(x = x, y = y, group = group, fill = has_food),
                          color = NA)
    p <- p + scale_fill_manual(values = c("white", "orange"))
  }
  
  # Add animation components
  p <- p + transition_time(frame) + ease_aes('linear')
  
  # Debugging: Print the structure of the plot object
  cat("Debugging: Structure of the plot object\n")
  print(str(p))
  
  # Plot final metrics
  cat("\nPlotting metrics...\n")
  colony$plot_metrics()
  
  return(list(colony = colony))
}

result <- run_complete_simulation(
  steps = 300,
  plot_interval = 2
)
