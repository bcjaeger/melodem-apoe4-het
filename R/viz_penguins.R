
viz_penguins <- function() {

  penguins <- drop_na(penguins)

  penguins$flipper_length_mm <- as.numeric(penguins$flipper_length_mm)

  fig_peng <- ggplot(data = penguins) +
    aes(x = flipper_length_mm, y = bill_length_mm, label = species) +
    geom_point(aes(color = species, shape = species),
               size = 3,
               alpha = 0.8) +
    theme_bw() +
    scale_color_manual(values = c("darkorange","purple","cyan4")) +
    scale_fill_manual(values = c("darkorange","purple","cyan4")) +
    labs(x = "\nFlipper length, mm",
         y = "Bill length, mm\n") +
    coord_cartesian(ylim = c(30, 70),
                    xlim = c(170, 235)) +
    theme(panel.grid = element_blank(),
          legend.position = '',
          text = element_text(size = 18))

  fig_demo <-  fig_peng +
    geom_mark_ellipse(aes(color = species, fill = species),
                      alpha = 0.075)


  axis_1 <- rpart(formula = species ~ flipper_length_mm + bill_length_mm,
                  data = penguins,
                  control = rpart.control(maxdepth = 1))

  axis_2 <- rpart(formula = species ~ flipper_length_mm + bill_length_mm,
                  data = penguins,
                  control = rpart.control(maxdepth = 2))

  axis_3a <- ranger(formula = species ~ flipper_length_mm + bill_length_mm,
                    data = penguins,
                    num.trees = 1,
                    max.depth = 4,
                    seed = 3,
                    probability = TRUE)

  axis_3b <- ranger(formula = species ~ flipper_length_mm + bill_length_mm,
                    data = penguins,
                    num.trees = 5,
                    probability = TRUE)

  axis_3c <- ranger(formula = species ~ flipper_length_mm + bill_length_mm,
                    data = penguins,
                    num.trees = 100,
                    probability = TRUE)

  axis_3d <- ranger(formula = species ~ flipper_length_mm + bill_length_mm,
                    data = penguins,
                    num.trees = 500,
                    probability = TRUE)



  oblique_1 <- orsf(species ~ flipper_length_mm + bill_length_mm,
                    data = penguins,
                    split_min_obs = nrow(penguins)-1,
                    tree_seeds = 649725,
                    oobag_pred_type = 'none',
                    n_tree = 1)

  oblique_2 <- orsf(species ~ flipper_length_mm + bill_length_mm,
                    data = penguins,
                    split_min_obs = floor(nrow(penguins)/4),
                    tree_seeds = 649725,
                    oobag_pred_type = 'none',
                    n_tree = 1)

  oblique_3 <- orsf(species ~ flipper_length_mm + bill_length_mm,
                    data = penguins)

  grid <- expand_grid(
    flipper_length_mm = seq(170, 235, len = 50),
    bill_length_mm = seq(30, 70, len = 50)
  )

  pred_1 <- predict(axis_1, newdata = grid, type = 'prob')

  pred_2 <- predict(axis_2, newdata = grid, type = 'prob')

  pred_3a <- predict(axis_3a, data = grid)$predictions
  pred_3b <- predict(axis_3b, data = grid)$predictions
  pred_3c <- predict(axis_3c, data = grid)$predictions
  pred_3d <- predict(axis_3d, data = grid)$predictions

  pred_4 <- predict(oblique_1, new_data = grid, pred_type = 'prob')

  pred_5 <- predict(oblique_2, new_data = grid, pred_type = 'prob')

  pred_6 <- predict(oblique_3, new_data = grid, pred_type = 'prob')

  p1 <- plot_decision_surface(penguins,
                              predictions = pred_1,
                              title = '',
                              grid = grid)

  p2 <- plot_decision_surface(penguins,
                              predictions = pred_2,
                              title = '',
                              grid = grid)

  p3a <- plot_decision_surface(penguins,
                               predictions = pred_3a,
                               title = '',
                               grid = grid)

  p3b <- plot_decision_surface(penguins,
                               predictions = pred_3b,
                               title = '',
                               grid = grid)

  p3c <- plot_decision_surface(penguins,
                               predictions = pred_3c,
                               title = '',
                               grid = grid)

  p3d <- plot_decision_surface(penguins,
                               predictions = pred_3d,
                               title = '',
                               grid = grid)

  penguins_permute_1_flipper <- penguins

  penguins_permute_1_flipper$flipper_length_mm[
    which.max(penguins_permute_1_flipper$bill_length_mm)
  ] <- 200

  penguins_permute_flipper <- penguins_permute_bill <- penguins

  penguins_permute_flipper <- penguins_permute_flipper %>%
    mutate(flipper_length_mm = sample(flipper_length_mm, size = n()))

  penguins_permute_bill <- penguins_permute_bill %>%
    mutate(bill_length_mm = sample(bill_length_mm, size = n()))

  p3d_1 <- plot_decision_surface(penguins_permute_1_flipper,
                                 predictions = pred_3d,
                                 title = '',
                                 grid = grid)

  p3d_2 <- plot_decision_surface(penguins_permute_flipper,
                                 predictions = pred_3d,
                                 title = '',
                                 grid = grid)

  p3d_3 <- plot_decision_surface(penguins_permute_bill,
                                 predictions = pred_3d,
                                 title = '',
                                 grid = grid)

  p4 <- plot_decision_surface(penguins,
                              predictions = pred_4,
                              title = '',
                              grid = grid)

  p5 <- plot_decision_surface(penguins,
                              predictions = pred_5,
                              title = '',
                              grid = grid)

  p6 <- plot_decision_surface(penguins,
                              predictions = pred_6,
                              title = '',
                              grid = grid)

  list(demo = fig_demo,
       axis_1 = p1,
       axis_2 = p2,
       axis_3a = p3a,
       axis_3b = p3b,
       axis_3c = p3c,
       axis_3d = p3d,
       vi_1 = p3d_1,
       vi_flipper = p3d_2,
       vi_bill = p3d_3,
       oblique_1 = p4,
       oblique_2 = p5,
       oblique_3 = p6)

}

plot_decision_surface <- function(penguins, predictions, title, grid){

  # this is not a general function for plotting
  # decision surfaces. It just helps to minimize
  # copying and pasting of code.

  class_preds <- bind_cols(grid, predictions) %>%
    pivot_longer(cols = c(Adelie,
                          Chinstrap,
                          Gentoo)) %>%
    group_by(flipper_length_mm, bill_length_mm) %>%
    arrange(desc(value)) %>%
    dplyr::slice(1)

  cols <- c(Adelie = "darkorange",
            Chinstrap = "purple",
            Gentoo = "cyan4")

  cols_fill <- cols[unique(class_preds$name)]

  ggplot(class_preds, aes(y = bill_length_mm,
                          x = flipper_length_mm)) +
    geom_raster(aes(fill = name), alpha = .3) +
    geom_point(data = penguins,
               size = 3,
               aes(color = species, shape = species),
               alpha = 0.8) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols_fill) +
    theme_bw() +
    labs(x = "\nFlipper length, mm",
         y = "Bill length, mm\n") +
    coord_cartesian(ylim = c(30, 70),
                    xlim = c(170, 235)) +
    theme(panel.grid = element_blank(),
          legend.position = '',
          text = element_text(size = 18))

}
