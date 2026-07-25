three_level_components <- function() {
  list(
    upper_x = c(-4, -1, 2, 5),
    upper_y = c(3, -5, 1, 4),
    lower_x = c(-2, 0.5, 3),
    lower_y = c(1, -3, 2),
    observation_x = c(-1, 0, 2),
    observation_y = c(2, -2, 1)
  )
}


three_level_additive_data <- function(n_upper = 4L,
                                      n_lower = 3L,
                                      n_observations = 3L) {
  components <- three_level_components()
  design <- expand.grid(
    observation = seq_len(n_observations),
    lower = seq_len(n_lower),
    upper = seq_len(n_upper)
  )

  data.frame(
    person_id = sprintf(
      "person-%02d",
      (design$upper - 1L) * n_lower + design$lower
    ),
    dyad_id = sprintf("dyad-%02d", design$upper),
    x = components$upper_x[design$upper] +
      components$lower_x[design$lower] +
      components$observation_x[design$observation],
    y = components$upper_y[design$upper] +
      components$lower_y[design$lower] +
      components$observation_y[design$observation],
    stringsAsFactors = FALSE
  )
}


three_level_unbalanced_data <- function() {
  observation_counts <- matrix(
    c(4, 2, 3,
      3, 4, 0,
      1, 2, 4,
      2, 3, 1),
    nrow = 4,
    byrow = TRUE
  )

  upper_x <- c(-4, -1, 2, 5)
  upper_y <- c(3, -5, 1, 4)
  upper_z <- c(5, 0, -2, 3)
  lower_x <- c(-2, 0.5, 3)
  lower_y <- c(1, -3, 2)
  lower_z <- c(0, 4, -1)
  observation_x <- c(-1, 0, 2, 1.5)
  observation_y <- c(2, -2, 1, 0.5)
  observation_z <- c(3, 1, -2, 0)

  rows <- list()
  row_number <- 0L
  for (upper in seq_len(nrow(observation_counts))) {
    for (lower in seq_len(ncol(observation_counts))) {
      count <- observation_counts[upper, lower]
      if (count == 0L) {
        next
      }

      for (observation in seq_len(count)) {
        row_number <- row_number + 1L
        rows[[row_number]] <- data.frame(
          person_id = sprintf(
            "person-%02d",
            (upper - 1L) * ncol(observation_counts) + lower
          ),
          dyad_id = sprintf("dyad-%02d", upper),
          x = upper_x[upper] + lower_x[lower] +
            observation_x[observation] + upper * lower / 10,
          y = upper_y[upper] + lower_y[lower] +
            observation_y[observation] - upper * observation / 12,
          z = upper_z[upper] + lower_z[lower] +
            observation_z[observation] + lower * observation / 15,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  data <- do.call(rbind, rows)
  rownames(data) <- NULL
  data$x[c(2, 14, 24)] <- NA_real_
  data$y[c(5, 11, 27)] <- NA_real_
  data$z[c(3, 18, 23)] <- NA_real_
  data
}


three_level_safe_cor <- function(x, y) {
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]

  if (length(x) < 2L ||
      length(unique(x)) < 2L ||
      length(unique(y)) < 2L) {
    return(NA_real_)
  }

  stats::cor(x, y)
}


three_level_pair_oracle <- function(data,
                                    first,
                                    second,
                                    lower = "person_id",
                                    upper = "dyad_id") {
  keep <- stats::complete.cases(data[c(first, second, lower, upper)])
  keep <- keep &
    is.finite(data[[first]]) &
    is.finite(data[[second]])
  pair <- data[keep, c(first, second, lower, upper), drop = FALSE]

  if (nrow(pair) == 0L) {
    return(c(level1 = NA_real_, level2 = NA_real_, level3 = NA_real_))
  }

  lower_ids <- unique(pair[[lower]])
  lower_index <- match(pair[[lower]], lower_ids)
  lower_mean_first <- numeric(length(lower_ids))
  lower_mean_second <- numeric(length(lower_ids))
  level1_first <- numeric(nrow(pair))
  level1_second <- numeric(nrow(pair))

  for (unit in seq_along(lower_ids)) {
    rows <- which(lower_index == unit)
    lower_mean_first[unit] <- mean(pair[[first]][rows])
    lower_mean_second[unit] <- mean(pair[[second]][rows])
    level1_first[rows] <- pair[[first]][rows] - lower_mean_first[unit]
    level1_second[rows] <- pair[[second]][rows] - lower_mean_second[unit]
  }

  lower_upper <- pair[[upper]][match(lower_ids, pair[[lower]])]
  upper_ids <- unique(lower_upper)
  upper_index <- match(lower_upper, upper_ids)
  upper_mean_first <- numeric(length(upper_ids))
  upper_mean_second <- numeric(length(upper_ids))
  level2_first <- numeric(length(lower_ids))
  level2_second <- numeric(length(lower_ids))

  for (unit in seq_along(upper_ids)) {
    rows <- which(upper_index == unit)
    upper_mean_first[unit] <- mean(lower_mean_first[rows])
    upper_mean_second[unit] <- mean(lower_mean_second[rows])
    level2_first[rows] <- lower_mean_first[rows] - upper_mean_first[unit]
    level2_second[rows] <- lower_mean_second[rows] - upper_mean_second[unit]
  }

  c(
    level1 = three_level_safe_cor(level1_first, level1_second),
    level2 = three_level_safe_cor(level2_first, level2_second),
    level3 = three_level_safe_cor(upper_mean_first, upper_mean_second)
  )
}


three_level_matrix_oracle <- function(data,
                                      variables,
                                      lower = "person_id",
                                      upper = "dyad_id") {
  levels <- c("level1", "level2", "level3")
  output <- lapply(levels, function(level) {
    matrix(
      1,
      nrow = length(variables),
      ncol = length(variables),
      dimnames = list(variables, variables)
    )
  })
  names(output) <- levels

  if (length(variables) < 2L) {
    return(output)
  }

  for (first in seq_len(length(variables) - 1L)) {
    for (second in seq.int(first + 1L, length(variables))) {
      expected <- three_level_pair_oracle(
        data,
        variables[first],
        variables[second],
        lower,
        upper
      )
      for (level in levels) {
        output[[level]][first, second] <- expected[[level]]
        output[[level]][second, first] <- expected[[level]]
      }
    }
  }

  output
}


expect_three_level_oracle <- function(object,
                                      data,
                                      variables,
                                      tolerance = 1e-12) {
  levels <- c("level1", "level2", "level3")
  actual <- get_matrix(object, numeric = TRUE)
  expected <- three_level_matrix_oracle(data, variables)

  expect_named(actual, levels)
  for (level in levels) {
    actual_matrix <- as.matrix(actual[[level]])[variables, variables]
    expect_equal(
      actual_matrix[upper.tri(actual_matrix)],
      expected[[level]][upper.tri(expected[[level]])],
      tolerance = tolerance,
      info = level
    )
    expect_equal(
      actual_matrix[lower.tri(actual_matrix)],
      expected[[level]][lower.tri(expected[[level]])],
      tolerance = tolerance,
      info = paste(level, "symmetry")
    )
  }
}


test_that("three-level Pearson correlations match additive hand oracles", {
  data <- three_level_additive_data()
  clusters <- list(person = "person_id", dyad = "dyad_id")

  result <- wbCorr(
    data,
    cluster = clusters,
    method = "pearson",
    inference = "none"
  )
  default_result <- wbCorr(
    data,
    cluster = clusters,
    method = "pearson"
  )

  expect_true(methods::is(result, "wbCorrNested"))
  expect_identical(default_result@settings$inference, "none")
  expect_equal(
    get_matrix(default_result, numeric = TRUE),
    get_matrix(result, numeric = TRUE),
    tolerance = 0
  )
  expect_three_level_oracle(result, data, c("x", "y"))

  components <- three_level_components()
  direct_expected <- c(
    level1 = stats::cor(
      components$observation_x,
      components$observation_y
    ),
    level2 = stats::cor(components$lower_x, components$lower_y),
    level3 = stats::cor(components$upper_x, components$upper_y)
  )
  oracle <- three_level_pair_oracle(data, "x", "y")
  expect_equal(oracle, direct_expected, tolerance = 1e-12)

  numeric_matrices <- get_matrix(result, numeric = TRUE)
  formatted_matrices <- get_matrix(result)
  tables <- get_table(result)
  expect_named(numeric_matrices, c("level1", "level2", "level3"))
  expect_named(formatted_matrices, c("level1", "level2", "level3"))
  expect_named(tables, c("level1", "level2", "level3"))
  expect_true(all(vapply(numeric_matrices, function(matrix) {
    all(vapply(matrix, is.numeric, logical(1)))
  }, logical(1))))
  expect_true(all(vapply(formatted_matrices, function(matrix) {
    all(vapply(matrix, is.character, logical(1)))
  }, logical(1))))
  expect_true(all(vapply(tables, is.data.frame, logical(1))))
  expect_true(all(vapply(tables, function(table) {
    all(table$inference_status == "not_requested")
  }, logical(1))))
})


test_that("unbalanced pairwise estimates match pair-specific base R oracles", {
  data <- three_level_unbalanced_data()
  result <- suppressWarnings(
    wbCorr(
      data,
      cluster = list(person = "person_id", dyad = "dyad_id"),
      inference = "none"
    )
  )

  expect_three_level_oracle(result, data, c("x", "y", "z"))

  pair_oracles <- list(
    xy = three_level_pair_oracle(data, "x", "y"),
    xz = three_level_pair_oracle(data, "x", "z"),
    yz = three_level_pair_oracle(data, "y", "z")
  )
  expect_false(isTRUE(all.equal(pair_oracles$xy, pair_oracles$xz)))
  expect_false(isTRUE(all.equal(pair_oracles$xy, pair_oracles$yz)))
})


test_that("three-level components center and reconstruct pairwise observations", {
  data <- three_level_unbalanced_data()
  decompose <- getFromNamespace(
    "decompose_nested_three_level_pair",
    "wbCorr"
  )
  components <- decompose(
    data$x,
    data$y,
    data$person_id,
    data$dyad_id
  )

  expect_equal(
    as.numeric(tapply(
      components$level1$x,
      components$level1$lower_unit,
      sum
    )),
    rep(0, nrow(components$level2)),
    tolerance = 1e-12
  )
  expect_equal(
    as.numeric(tapply(
      components$level1$y,
      components$level1$lower_unit,
      sum
    )),
    rep(0, nrow(components$level2)),
    tolerance = 1e-12
  )
  expect_equal(
    as.numeric(tapply(
      components$level2$x,
      components$level2$upper_unit,
      sum
    )),
    rep(0, nrow(components$level3)),
    tolerance = 1e-12
  )
  expect_equal(
    as.numeric(tapply(
      components$level2$y,
      components$level2$upper_unit,
      sum
    )),
    rep(0, nrow(components$level3)),
    tolerance = 1e-12
  )

  reconstructed_x <- components$level1$x +
    components$level2$x[components$level1$lower_unit] +
    components$level3$x[components$level1$upper_unit]
  reconstructed_y <- components$level1$y +
    components$level2$y[components$level1$lower_unit] +
    components$level3$y[components$level1$upper_unit]
  expect_equal(
    reconstructed_x,
    data$x[components$pair_rows],
    tolerance = 1e-12
  )
  expect_equal(
    reconstructed_y,
    data$y[components$pair_rows],
    tolerance = 1e-12
  )
})


test_that("recursive internals preserve the same identities beyond three levels", {
  design <- expand.grid(
    observation = seq_len(2L),
    lower_local = seq_len(2L),
    middle_local = seq_len(2L),
    upper = seq_len(3L)
  )
  design$middle <- (design$upper - 1L) * 2L + design$middle_local
  design$lower <- (design$middle - 1L) * 2L + design$lower_local
  x <- 2 * design$upper - design$middle_local +
    0.5 * design$lower_local + c(-1, 1)
  y <- -design$upper + 0.7 * design$middle_local -
    design$lower_local + c(2, -2)
  decompose <- getFromNamespace("decompose_nested_pair", "wbCorr")
  components <- decompose(
    x,
    y,
    list(design$lower, design$middle, design$upper)
  )

  expect_length(components$components, 4L)
  expect_equal(
    as.numeric(tapply(
      components$components[[1L]]$x,
      components$unit_keys[[1L]][[1L]],
      sum
    )),
    rep(0, length(unique(design$lower))),
    tolerance = 1e-12
  )
  expect_equal(
    as.numeric(tapply(
      components$components[[2L]]$x,
      components$unit_keys[[2L]][[2L]],
      sum
    )),
    rep(0, length(unique(design$middle))),
    tolerance = 1e-12
  )
  expect_equal(
    as.numeric(tapply(
      components$components[[3L]]$x,
      components$unit_keys[[3L]][[3L]],
      sum
    )),
    rep(0, length(unique(design$upper))),
    tolerance = 1e-12
  )

  reconstructed_x <- components$components[[1L]]$x +
    components$components[[2L]]$x[
      components$unit_keys[[1L]][[1L]]
    ] +
    components$components[[3L]]$x[
      components$unit_keys[[1L]][[2L]]
    ] +
    components$components[[4L]]$x[
      components$unit_keys[[1L]][[3L]]
    ]
  reconstructed_y <- components$components[[1L]]$y +
    components$components[[2L]]$y[
      components$unit_keys[[1L]][[1L]]
    ] +
    components$components[[3L]]$y[
      components$unit_keys[[1L]][[2L]]
    ] +
    components$components[[4L]]$y[
      components$unit_keys[[1L]][[3L]]
    ]
  expect_equal(reconstructed_x, x, tolerance = 1e-12)
  expect_equal(reconstructed_y, y, tolerance = 1e-12)
})


test_that("three-level decomposition recovers simulated component correlations", {
  set.seed(908)
  n_upper <- 200L
  n_lower_per_upper <- 5L
  n_observations_per_lower <- 6L
  n_lower <- n_upper * n_lower_per_upper
  n_observations <- n_lower * n_observations_per_lower

  correlated_pair <- function(n, correlation) {
    first <- rnorm(n)
    second <- correlation * first +
      sqrt(1 - correlation^2) * rnorm(n)
    cbind(first, second)
  }

  upper_components <- correlated_pair(n_upper, 0.6)
  lower_components <- correlated_pair(n_lower, -0.4)
  lower_group <- rep(seq_len(n_upper), each = n_lower_per_upper)
  lower_components[, 1L] <- lower_components[, 1L] -
    ave(lower_components[, 1L], lower_group)
  lower_components[, 2L] <- lower_components[, 2L] -
    ave(lower_components[, 2L], lower_group)

  observation_components <- correlated_pair(n_observations, 0.3)
  observation_group <- rep(
    seq_len(n_lower),
    each = n_observations_per_lower
  )
  observation_components[, 1L] <- observation_components[, 1L] -
    ave(observation_components[, 1L], observation_group)
  observation_components[, 2L] <- observation_components[, 2L] -
    ave(observation_components[, 2L], observation_group)

  data <- data.frame(
    person_id = observation_group,
    dyad_id = rep(
      seq_len(n_upper),
      each = n_lower_per_upper * n_observations_per_lower
    ),
    x = rep(
      upper_components[, 1L],
      each = n_lower_per_upper * n_observations_per_lower
    ) +
      rep(lower_components[, 1L], each = n_observations_per_lower) +
      observation_components[, 1L],
    y = rep(
      upper_components[, 2L],
      each = n_lower_per_upper * n_observations_per_lower
    ) +
      rep(lower_components[, 2L], each = n_observations_per_lower) +
      observation_components[, 2L]
  )

  result <- wbCorr(
    data,
    cluster = list(person = "person_id", dyad = "dyad_id"),
    inference = "none"
  )
  matrices <- get_matrix(result, numeric = TRUE)
  expect_lt(
    abs(as.matrix(matrices$level1)["x", "y"] - 0.3),
    0.04
  )
  expect_lt(
    abs(as.matrix(matrices$level2)["x", "y"] + 0.4),
    0.06
  )
  expect_lt(
    abs(as.matrix(matrices$level3)["x", "y"] - 0.6),
    0.12
  )
})


test_that("listwise three-level estimates equal manual row filtering", {
  data <- three_level_unbalanced_data()
  clusters <- list(person = "person_id", dyad = "dyad_id")
  complete <- stats::complete.cases(data[c("x", "y", "z")])
  filtered <- data[complete, , drop = FALSE]

  listwise <- suppressWarnings(
    wbCorr(
      data,
      cluster = clusters,
      inference = "none",
      missing_data = "listwise"
    )
  )
  manual <- suppressWarnings(
    wbCorr(
      filtered,
      cluster = clusters,
      inference = "none"
    )
  )

  expect_equal(
    get_matrix(listwise, numeric = TRUE),
    get_matrix(manual, numeric = TRUE),
    tolerance = 0
  )
  expect_three_level_oracle(listwise, filtered, c("x", "y", "z"))
})


test_that("nested estimates are invariant to rows, columns, and ID labels", {
  data <- three_level_unbalanced_data()
  clusters <- list(person = "person_id", dyad = "dyad_id")
  reference <- suppressWarnings(
    wbCorr(data, cluster = clusters, inference = "none")
  )

  person_ids <- unique(data$person_id)
  dyad_ids <- unique(data$dyad_id)
  relabelled <- data
  relabelled$person_id <- factor(
    paste0("member-", match(data$person_id, rev(person_ids))),
    levels = paste0("member-", seq_along(person_ids))
  )
  relabelled$dyad_id <- factor(
    paste0("group-", match(data$dyad_id, rev(dyad_ids))),
    levels = paste0("group-", seq_along(dyad_ids))
  )
  set.seed(4201)
  relabelled <- relabelled[
    sample(seq_len(nrow(relabelled))),
    c("z", "dyad_id", "x", "person_id", "y"),
    drop = FALSE
  ]
  changed <- suppressWarnings(
    wbCorr(relabelled, cluster = clusters, inference = "none")
  )

  reference_matrices <- get_matrix(reference, numeric = TRUE)
  changed_matrices <- get_matrix(changed, numeric = TRUE)
  variables <- c("x", "y", "z")
  for (level in c("level1", "level2", "level3")) {
    expect_equal(
      as.matrix(changed_matrices[[level]])[variables, variables],
      as.matrix(reference_matrices[[level]])[variables, variables],
      tolerance = 1e-12,
      info = level
    )
  }
})


test_that("distinct numeric hierarchy IDs are never collapsed by formatting", {
  close_ids <- c(1, 1 + 1e-15, 2, 3)
  expect_length(unique(close_ids), 4L)

  data <- data.frame(
    person_id = rep(close_ids, each = 3L),
    dyad_id = rep(c(1, 1, 1 + 1e-15, 1 + 1e-15), each = 3L),
    x = rep(c(-2, 1, 0, 3), each = 3L) +
      rep(c(-1, 0, 1), times = 4L),
    y = rep(c(1, -3, 2, 4), each = 3L) +
      rep(c(1, -2, 1), times = 4L)
  )
  expect_length(unique(data$dyad_id), 2L)
  result <- wbCorr(
    data,
    cluster = list(person = "person_id", dyad = "dyad_id"),
    inference = "none"
  )

  expect_three_level_oracle(result, data, c("x", "y"))
  expect_equal(get_table(result)$level2$n_level2, 4L)
  expect_equal(get_table(result)$level3$n_level3, 2L)
})


test_that("nested cluster specifications have a strict two-column shape", {
  data <- three_level_additive_data()

  expect_error(
    wbCorr(
      data,
      cluster = list("person_id", "dyad_id"),
      inference = "none"
    ),
    "named list"
  )
  expect_error(
    wbCorr(
      data,
      cluster = data.frame(
        person = "person_id",
        dyad = "dyad_id"
      ),
      inference = "none"
    ),
    "named list"
  )
  expect_error(
    wbCorr(
      data,
      cluster = structure(
        list("person_id", "dyad_id"),
        names = c("person", "")
      ),
      inference = "none"
    ),
    "named list"
  )
  expect_error(
    wbCorr(
      data,
      cluster = list(person = "person_id"),
      inference = "none"
    ),
    "exactly two"
  )
  expect_error(
    wbCorr(
      data,
      cluster = list(
        observation = "x",
        person = "person_id",
        dyad = "dyad_id"
      ),
      inference = "none"
    ),
    "exactly two"
  )
  expect_error(
    wbCorr(
      data,
      cluster = list(
        person = c("person_id", "dyad_id"),
        dyad = "dyad_id"
      ),
      inference = "none"
    ),
    "column"
  )
  expect_error(
    wbCorr(
      data,
      cluster = list(person = "unknown_id", dyad = "dyad_id"),
      inference = "none"
    ),
    "column"
  )
})


test_that("nested hierarchy paths must be complete and globally strict", {
  data <- three_level_additive_data()
  clusters <- list(person = "person_id", dyad = "dyad_id")

  both_missing <- rbind(
    data,
    data.frame(
      person_id = NA_character_,
      dyad_id = NA_character_,
      x = 1e12,
      y = -1e12
    )
  )
  expect_equal(
    get_matrix(
      wbCorr(both_missing, cluster = clusters, inference = "none"),
      numeric = TRUE
    ),
    get_matrix(
      wbCorr(data, cluster = clusters, inference = "none"),
      numeric = TRUE
    ),
    tolerance = 0
  )

  reversed <- list(dyad = "dyad_id", person = "person_id")
  expect_error(
    wbCorr(data, cluster = reversed, inference = "none"),
    "strictly nested"
  )

  crossed <- data
  crossed$dyad_id[which(crossed$person_id == "person-01")[1L]] <- "dyad-02"
  expect_error(
    wbCorr(crossed, cluster = clusters, inference = "none"),
    "strictly nested"
  )

  missing_lower <- data
  missing_lower$person_id[1L] <- NA_character_
  expect_error(
    wbCorr(missing_lower, cluster = clusters, inference = "none"),
    "missing hierarchy"
  )

  missing_upper <- data
  missing_upper$dyad_id[1L] <- NA_character_
  expect_error(
    wbCorr(missing_upper, cluster = clusters, inference = "none"),
    "missing hierarchy"
  )

  local_ids <- data
  global_number <- as.integer(sub("person-", "", local_ids$person_id))
  local_ids$person_id <- paste0(
    "local-",
    (global_number - 1L) %% 3L + 1L
  )
  local_id_error <- tryCatch(
    {
      wbCorr(local_ids, cluster = clusters, inference = "none")
      NA_character_
    },
    error = function(error) conditionMessage(error)
  )
  expect_match(local_id_error, "strictly nested")
  expect_match(local_id_error, "composite lower ID")
})


test_that("unsupported three-level settings fail before estimation", {
  data <- three_level_additive_data()
  clusters <- list(person = "person_id", dyad = "dyad_id")

  supported <- wbCorr(
    data,
    cluster = clusters,
    method = "pearson",
    inference = "none",
    between_weighting = "equal_clusters",
    centering_rows = "pairwise_complete"
  )
  expect_true(methods::is(supported, "wbCorrNested"))

  expect_error(
    wbCorr(
      data,
      cluster = clusters,
      method = "spearman",
      inference = "none"
    ),
    "Pearson"
  )
  expect_error(
    wbCorr(data, cluster = clusters, inference = "analytic"),
    "[Aa]nalytic"
  )
  expect_error(
    wbCorr(
      data,
      cluster = clusters,
      inference = "none",
      between_weighting = "cluster_size"
    ),
    "equal_clusters"
  )
  expect_error(
    wbCorr(
      data,
      cluster = clusters,
      inference = "none",
      centering_rows = "all_available"
    ),
    "pairwise_complete"
  )
})


test_that("singletons and zero-variance levels return unavailable coefficients", {
  clusters <- list(person = "person_id", dyad = "dyad_id")

  one_observation <- three_level_additive_data(n_observations = 1L)
  observation_result <- suppressWarnings(
    wbCorr(one_observation, cluster = clusters, inference = "none")
  )
  observation_matrices <- get_matrix(observation_result, numeric = TRUE)
  expect_true(is.na(as.matrix(observation_matrices$level1)["x", "y"]))
  expect_true(is.finite(as.matrix(observation_matrices$level2)["x", "y"]))
  expect_true(is.finite(as.matrix(observation_matrices$level3)["x", "y"]))

  one_lower <- three_level_additive_data(n_lower = 1L)
  lower_result <- suppressWarnings(
    wbCorr(one_lower, cluster = clusters, inference = "none")
  )
  lower_matrices <- get_matrix(lower_result, numeric = TRUE)
  expect_true(is.finite(as.matrix(lower_matrices$level1)["x", "y"]))
  expect_true(is.na(as.matrix(lower_matrices$level2)["x", "y"]))
  expect_true(is.finite(as.matrix(lower_matrices$level3)["x", "y"]))

  one_upper <- three_level_additive_data(n_upper = 1L)
  upper_result <- suppressWarnings(
    wbCorr(one_upper, cluster = clusters, inference = "none")
  )
  upper_matrices <- get_matrix(upper_result, numeric = TRUE)
  expect_true(is.finite(as.matrix(upper_matrices$level1)["x", "y"]))
  expect_true(is.finite(as.matrix(upper_matrices$level2)["x", "y"]))
  expect_true(is.na(as.matrix(upper_matrices$level3)["x", "y"]))

  constant <- three_level_additive_data()
  constant$x <- 1
  constant_result <- suppressWarnings(
    wbCorr(constant, cluster = clusters, inference = "none")
  )
  constant_matrices <- get_matrix(constant_result, numeric = TRUE)
  for (level in c("level1", "level2", "level3")) {
    expect_true(
      is.na(as.matrix(constant_matrices[[level]])["x", "y"]),
      info = level
    )
  }
})


test_that("top-level bootstrap is reproducible and reports its yield", {
  data <- three_level_additive_data()
  clusters <- list(person = "person_id", dyad = "dyad_id")
  point <- wbCorr(data, cluster = clusters, inference = "none")

  set.seed(730)
  legacy <- NULL
  expect_warning(
    legacy <- wbCorr(
      data,
      cluster = clusters,
      bootstrap = TRUE,
      nboot = 10
    ),
    "deprecated"
  )
  expect_identical(legacy@settings$inference, "cluster_bootstrap")

  set.seed(731)
  first <- suppressWarnings(
    wbCorr(
      data,
      cluster = clusters,
      inference = "cluster_bootstrap",
      nboot = 20
    )
  )
  set.seed(731)
  second <- suppressWarnings(
    wbCorr(
      data,
      cluster = clusters,
      inference = "cluster_bootstrap",
      nboot = 20
    )
  )

  expect_equal(
    get_matrix(first, numeric = TRUE),
    get_matrix(point, numeric = TRUE),
    tolerance = 0
  )
  expect_equal(get_table(first), get_table(second), tolerance = 0)

  tables <- get_table(first)
  for (level in c("level1", "level2", "level3")) {
    table <- tables[[level]]
    expect_true(all(table$n_boot_attempted == 20L), info = level)
    expect_true(
      all(table$n_boot_valid <= table$n_boot_attempted),
      info = level
    )
    expect_true(all(table$n_boot_valid >= 10L), info = level)

    interval_column <- grep(
      "^Cluster bootstrap .* CI$",
      names(table),
      value = TRUE
    )
    expect_length(interval_column, 1L)
    expect_true(all(!is.na(table[[interval_column]])), info = level)

    public_p_columns <- intersect(
      names(table),
      c("p", "p_value", "p.value", "p-value")
    )
    expect_length(public_p_columns, 0L)
  }
})


test_that("bootstrap requires three pair-contributing upper units", {
  data <- three_level_additive_data(n_upper = 3L)
  pair_empty <- data$dyad_id == "dyad-03"
  data$x[pair_empty] <- NA_real_
  data$y[pair_empty] <- NA_real_

  result <- wbCorr(
    data,
    cluster = list(person = "person_id", dyad = "dyad_id"),
    inference = "cluster_bootstrap",
    nboot = 10
  )
  for (level in c("level1", "level2", "level3")) {
    table <- get_table(result, level)[[level]]
    expect_identical(table$n_boot_attempted, 0L)
    expect_identical(table$n_boot_valid, 0L)
    expect_identical(
      table$inference_reason,
      "fewer_than_three_contributing_upper_units"
    )
  }
})


test_that("top-level bootstrap matches an independent duplicated-draw oracle", {
  data <- three_level_unbalanced_data()[, c(
    "person_id",
    "dyad_id",
    "x",
    "y"
  )]
  # Keep one intact upper cluster with no complete x-y pair. It belongs in the
  # bootstrap sampling frame even though it cannot contribute to the observed
  # pair coefficient.
  uninformative_upper <- data$dyad_id == "dyad-04"
  data$x[uninformative_upper] <- NA_real_
  data$y[uninformative_upper] <- NA_real_
  complete_path <- complete.cases(data[c("person_id", "dyad_id")])
  bootstrap_frame <- data[complete_path, , drop = FALSE]
  upper_ids <- unique(bootstrap_frame$dyad_id)
  expect_length(upper_ids, 4L)
  expect_length(
    unique(data$dyad_id[is.finite(data$x) & is.finite(data$y)]),
    3L
  )
  nboot <- 20L
  manual <- matrix(
    NA_real_,
    nrow = nboot,
    ncol = 3L,
    dimnames = list(NULL, c("level1", "level2", "level3"))
  )

  set.seed(144)
  for (bootstrap_index in seq_len(nboot)) {
    sampled_upper <- sample(
      seq_along(upper_ids),
      length(upper_ids),
      replace = TRUE
    )
    pieces <- vector("list", length(sampled_upper))
    for (draw_index in seq_along(sampled_upper)) {
      rows <- bootstrap_frame$dyad_id ==
        upper_ids[[sampled_upper[[draw_index]]]]
      piece <- bootstrap_frame[rows, , drop = FALSE]
      piece$person_id <- paste0(
        "draw-",
        draw_index,
        "-",
        piece$person_id
      )
      piece$dyad_id <- paste0("draw-", draw_index)
      pieces[[draw_index]] <- piece
    }
    bootstrap_data <- do.call(rbind, pieces)
    manual[bootstrap_index, ] <- three_level_pair_oracle(
      bootstrap_data,
      "x",
      "y"
    )
  }

  set.seed(144)
  result <- wbCorr(
    data,
    cluster = list(person = "person_id", dyad = "dyad_id"),
    inference = "cluster_bootstrap",
    nboot = nboot
  )
  for (level in colnames(manual)) {
    valid <- manual[, level]
    valid <- valid[is.finite(valid)]
    expected_interval <- quantile(
      valid,
      c(0.025, 0.975),
      names = FALSE
    )
    interval <- result@levels[[level]]$confidence_intervals
    table <- result@levels[[level]]$table
    expect_equal(interval$CI_lower, expected_interval[[1L]],
                 tolerance = 1e-12)
    expect_equal(interval$CI_upper, expected_interval[[2L]],
                 tolerance = 1e-12)
    expect_identical(table$n_boot_attempted, nboot)
    expect_identical(table$n_boot_valid, as.integer(length(valid)))
  }
})


test_that("three-level matrices and tables export without reshaping", {
  data <- three_level_additive_data()
  result <- wbCorr(
    data,
    cluster = list(person = "person_id", dyad = "dyad_id"),
    inference = "none"
  )
  matrix_path <- tempfile(fileext = ".xlsx")
  table_path <- tempfile(fileext = ".xlsx")

  expect_identical(
    to_excel(get_matrix(result, numeric = TRUE), matrix_path),
    matrix_path
  )
  expect_identical(
    to_excel(get_table(result), table_path),
    table_path
  )
  expect_true(file.exists(matrix_path))
  expect_true(file.exists(table_path))
})


test_that("ordinary two-level calls retain the legacy result contract", {
  data <- three_level_additive_data()
  two_level_data <- data[c("person_id", "x", "y")]

  by_name <- wbCorr(
    two_level_data,
    cluster = "person_id",
    inference = "none"
  )
  by_vector <- wbCorr(
    two_level_data[c("x", "y")],
    cluster = two_level_data$person_id,
    inference = "none"
  )

  expect_true(methods::is(by_name, "wbCorr"))
  expect_false(methods::is(by_name, "wbCorrNested"))
  expect_named(get_table(by_name), c("within", "between"))
  expect_true(all(c("within", "between") %in%
                    names(get_matrix(by_name, numeric = TRUE))))
  expect_equal(get_table(by_name), get_table(by_vector), tolerance = 0)
  expect_equal(
    get_matrix(by_name, numeric = TRUE),
    get_matrix(by_vector, numeric = TRUE),
    tolerance = 0
  )
})
