# Deterministic model-space coordinates. Resizing only changes the SVG viewport.
.mplus_diagram_layout <- function(graph) {
  structure <- if (!is.null(graph$structure)) graph$structure else graph
  nodes <- structure$nodes
  edges <- structure$edges
  ids <- sort(vapply(nodes, `[[`, character(1), "id"))
  nodes <- stats::setNames(nodes, vapply(nodes, `[[`, character(1), "id"))[ids]
  coords <- stats::setNames(lapply(seq_along(ids), function(i) list(x = 140, y = 120 + (i - 1) * 130)), ids)
  scalar <- function(x, fallback = NA) if (is.null(x) || !length(x)) fallback else x[[1]]
  roles <- vapply(nodes, function(n) scalar(n$role, "observed"), character(1))
  waves <- vapply(nodes, function(n) {
    w <- n$wave
    if (is.null(w) && n$name %in% names(structure$mapping$waves)) w <- structure$mapping$waves[[n$name]]
    as.numeric(scalar(w, NA_real_))
  }, numeric(1))
  constructs <- vapply(nodes, function(n) {
    z <- n$construct
    if (is.null(z) && n$name %in% names(structure$mapping$constructs)) z <- structure$mapping$constructs[[n$name]]
    as.character(scalar(z, n$name))
  }, character(1))
  row_levels <- character()
  if (any(is.finite(waves))) {
    time_levels <- sort(unique(waves[is.finite(waves)]))
    row_levels <- sort(unique(constructs[is.finite(waves)]))
    for (id in ids[is.finite(waves)]) {
      row <- match(constructs[[id]], row_levels)
      # Observed and within components occupy separate bands, even at one wave.
      layer <- if (roles[[id]] == "observed" && any(roles == "within")) 100 else 0
      coords[[id]] <- list(x = 340 + (match(waves[[id]], time_levels) - 1) * 240,
                           y = 180 + (row - 1) * 260 + layer)
    }
    other <- ids[!is.finite(waves)]
    for (i in seq_along(other)) coords[[other[[i]]]] <- list(x = 120, y = 150 + (i - 1) * 150)
  } else {
    loading <- Filter(function(e) identical(e$type, "loading"), edges)
    measurement <- unique(vapply(loading, `[[`, character(1), "to"))
    regression <- Filter(function(e) identical(e$type, "regression"), edges)
    structural <- union(setdiff(ids, measurement), unique(unlist(lapply(regression, function(e) c(e$from, e$to)))))
    structural <- sort(structural)
    if (!length(structural)) structural <- ids
    ranks <- stats::setNames(rep(0, length(structural)), structural)
    pending <- structural
    done <- character()
    while (length(pending)) {
      ready <- pending[vapply(pending, function(id) {
        parents <- vapply(Filter(function(e) e$to == id && e$from %in% structural, regression), `[[`, character(1), "from")
        all(parents %in% done)
      }, logical(1))]
      if (!length(ready)) break # Cycles remain visible in a deterministic fallback column.
      for (id in ready) {
        parents <- vapply(Filter(function(e) e$to == id && e$from %in% structural, regression), `[[`, character(1), "from")
        if (length(parents)) ranks[[id]] <- max(ranks[parents]) + 1
      }
      done <- c(done, ready)
      pending <- setdiff(pending, ready)
    }
    if (length(pending)) ranks[pending] <- if (length(done)) max(ranks[done]) + 1 else 0
    row_used <- stats::setNames(rep(0, max(ranks) + 1), as.character(0:max(ranks)))
    group_sizes <- vapply(structural, function(id) sum(vapply(loading, function(e) e$from == id, logical(1))), integer(1))
    column_spacing <- max(480, max(group_sizes) * 145 + 180)
    assigned <- character()
    for (id in structural) {
      col <- as.character(ranks[[id]])
      children <- sort(setdiff(vapply(Filter(function(e) e$from == id, loading), `[[`, character(1), "to"), c(structural, assigned)))
      y <- 120 + row_used[[col]] * 115
      if (!length(regression)) {
        coords[[id]] <- list(x = 140, y = y + max(0, length(children) - 1) * 50)
        for (j in seq_along(children)) coords[[children[[j]]]] <- list(x = 400, y = y + (j - 1) * 100)
        row_used[[col]] <- row_used[[col]] + max(2, length(children) + 1)
      } else {
        center <- 220 + ranks[[id]] * column_spacing
        coords[[id]] <- list(x = center, y = y)
        for (j in seq_along(children)) coords[[children[[j]]]] <- list(x = center + (j - (length(children) + 1) / 2) * 145, y = y + 170)
        row_used[[col]] <- row_used[[col]] + if (length(children)) 3 else 2
      }
      assigned <- c(assigned, children)
    }
  }
  # Keep growth measurement rays clear of intermediate observed nodes.
  if (identical(structure$family, "growth") && any(is.finite(waves))) {
    observed <- ids[is.finite(waves)]
    groups <- sort(unique(constructs[observed]))
    for (id in observed) coords[[id]]$y <- 300 + (match(constructs[[id]], groups) - 1) * 560
    for (id in ids[roles %in% c("growth_intercept", "growth_slope")]) {
      targets <- vapply(Filter(function(e) e$type == "loading" && e$from == id && e$to %in% observed, edges), `[[`, character(1), "to")
      if (length(targets)) {
        center <- mean(vapply(coords[targets], `[[`, numeric(1), "x"))
        row <- min(match(constructs[targets], groups))
        coords[[id]] <- list(x = center, y = (if (roles[[id]] == "growth_intercept") 100 else 500) + (row - 1) * 560)
      }
    }
  }
  simplified_coords <- coords
  # With one/two constructs, put measurement layers outside the within-person
  # dynamics, so the full view cannot put observed nodes on cross-lagged paths.
  if (identical(structure$family, "ri_clpm") && any(is.finite(waves))) {
    row_levels <- sort(unique(constructs[roles == "within"]))
    if (length(row_levels) <= 2) {
      for (id in ids[roles %in% c("within", "observed")]) {
        row <- match(constructs[[id]], row_levels)
        if (is.na(row)) next
        coords[[id]]$y <- if (roles[[id]] == "within") 230 + (row - 1) * 240 else if (row == 1) 60 else 640
      }
      for (id in ids[roles == "random_intercept"]) {
        row <- match(constructs[[id]], row_levels)
        if (!is.na(row)) coords[[id]] <- list(x = 120, y = if (row == 1) 60 else 640)
      }
    }
  }
  # SEM measurement fans can extend left of their exogenous factors. Choose
  # the initial bend outside those nodes; subsequent side drags preserve this
  # saved bend instead of silently re-routing a manually edited correlation.
  sem_covariance_bend <- function(e, initial, visible_ids = ids) {
    if (!identical(structure$family, "sem") || e$type != "covariance") return(initial)
    incoming <- vapply(Filter(function(edge) edge$type %in% c("loading", "regression"), edges), `[[`, character(1), "to")
    if (e$from %in% incoming || e$to %in% incoming) return(initial)
    from <- unlist(coords[[e$from]]); to <- unlist(coords[[e$to]])
    if (abs(to[[2]] - from[[2]]) < abs(to[[1]] - from[[1]])) return(initial)
    size <- function(id) {
      name <- nodes[[id]]$name
      c(w = max(if (roles[[id]] == "random_intercept") 174 else 106,
                min(320, min(24, nchar(name)) * 8 + 24)),
        h = 52 + (max(1, ceiling(nchar(name) / 24)) - 1) * 17 +
          if (roles[[id]] %in% c("observed", "latent")) 0 else 13)
    }
    a <- from - c(size(e$from)[[1]] / 2, 0)
    b <- to - c(size(e$to)[[1]] / 2, 0)
    delta <- b - a; span <- max(1, sqrt(sum(delta^2)))
    normal <- c(-delta[[2]], delta[[1]]) / span
    if (normal[[1]] > 0) normal <- -normal
    others <- setdiff(visible_ids, c(e$from, e$to))
    t <- seq(0, 1, length.out = 201)
    for (bend in initial * c(1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6, 8)) {
      sagitta <- span * .15 * bend / 55
      along <- max(.1, (1 - 4 * (sagitta / span)^2) / 3)
      c1 <- a + delta * along + normal * sagitta * 4 / 3
      c2 <- b - delta * along + normal * sagitta * 4 / 3
      points <- outer((1-t)^3, a) + outer(3*(1-t)^2*t, c1) +
        outer(3*(1-t)*t^2, c2) + outer(t^3, b)
      crossed <- any(vapply(others, function(id) {
        center <- unlist(coords[[id]]); half <- size(id) / 2 + 24
        any(abs(points[, 1] - center[[1]]) < half[[1]] &
            abs(points[, 2] - center[[2]]) < half[[2]])
      }, logical(1)))
      if (!crossed) return(bend)
    }
    bend
  }
  curves <- list()
  original_covariance_bends <- list()
  for (e in edges) {
    offset <- if (e$type == "covariance") 55 else 0
    reverse <- any(vapply(edges, function(other) other$id != e$id && other$from == e$to && other$to == e$from, logical(1)))
    if (reverse && e$from != e$to) offset <- 45
    if (e$type == "covariance") original_covariance_bends[[e$id]] <- offset
    offset <- sem_covariance_bend(e, offset)
    if (identical(structure$family, "ri_clpm") && e$type == "loading" &&
        roles[[e$from]] == "random_intercept" && length(row_levels) <= 2 && is.finite(waves[[e$to]])) {
      wave_index <- match(waves[[e$to]], sort(unique(waves[is.finite(waves)])))
      row <- match(constructs[[e$to]], row_levels)
      if (wave_index > 1) offset <- (if (row == 1) -1 else 1) * (90 + (wave_index - 2) * 55)
    }
    override <- list()
    if (offset != 0) override$curve <- offset
    if (e$type == "regression" && is.finite(waves[[e$from]]) && is.finite(waves[[e$to]]) &&
        waves[[e$from]] != waves[[e$to]] && constructs[[e$from]] != constructs[[e$to]]) {
      # Cross-lagged labels otherwise share a crossing midpoint. Move along
      # their own paths so text stays centered on the line, not above it.
      override$label_dx <- .18 * (coords[[e$to]]$x - coords[[e$from]]$x)
      override$label_dy <- .18 * (coords[[e$to]]$y - coords[[e$from]]$y)
    }
    if (length(override)) curves[[e$id]] <- override
  }
  # In ordinary path/SEM diagrams, distinct regressions can share an exact
  # midpoint (e.g. X1 -> Y2 and X2 -> Y1). Move every colliding coefficient
  # along its own path, leaving node coordinates and statistical paths intact.
  # Existing saved states override this automatic layout in the public entry.
  regressions <- Filter(function(e) e$type == "regression", edges)
  if (!any(is.finite(waves)) && length(regressions) > 1L) {
    regressions <- regressions[order(vapply(regressions, `[[`, character(1), "id"))]
    centers <- lapply(regressions, function(e) {
      from <- unlist(coords[[e$from]], use.names = FALSE)
      to <- unlist(coords[[e$to]], use.names = FALSE)
      delta <- to - from
      normal <- c(-delta[2L], delta[1L]) / max(1, sqrt(sum(delta^2)))
      override <- curves[[e$id]]
      curve <- if (is.null(override$curve)) 0 else override$curve
      list(point = (from + to) / 2 + normal * curve / 2 +
             c(if (is.null(override$label_dx)) 0 else override$label_dx,
               if (is.null(override$label_dy)) 0 else override$label_dy),
           delta = delta, normal = normal, curve = curve)
    })
    overlap <- function(a, b) abs(a[1L] - b[1L]) < 64 && abs(a[2L] - b[2L]) < 26
    collided <- vapply(seq_along(centers), function(i) any(vapply(
      setdiff(seq_along(centers), i), function(j) overlap(centers[[i]]$point, centers[[j]]$point), logical(1))), logical(1))
    occupied <- lapply(centers[!collided], `[[`, "point")
    for (i in which(collided)) {
      center <- centers[[i]]
      candidates <- lapply(c(-.18, .18, -.28, .28, -.36, .36, -.42, .42), function(t)
        t * center$delta - 2 * t^2 * center$curve * center$normal)
      # Keep automatic labels on their own paths. If every bounded along-path
      # candidate is occupied, retain a deterministic candidate for manual review
      # rather than moving a coefficient to an unrelated off-line location.
      chosen <- candidates[[1L]]
      for (candidate in candidates) {
        point <- center$point + candidate
        if (!any(vapply(occupied, function(other) overlap(point, other), logical(1)))) {
          chosen <- candidate; break
        }
      }
      id <- regressions[[i]]$id
      override <- curves[[id]]
      if (is.null(override)) override <- list()
      override$label_dx <- (if (is.null(override$label_dx)) 0 else override$label_dx) + chosen[1L]
      override$label_dy <- (if (is.null(override$label_dy)) 0 else override$label_dy) + chosen[2L]
      curves[[id]] <- override
      occupied[[length(occupied) + 1L]] <- center$point + chosen
    }
  }
  if (!length(curves)) curves <- structure(list(), names = character())
  view <- list(nodes = coords, edges = curves, viewport = list(x = 0, y = 0, k = 1))
  simplified <- view
  simplified$nodes <- simplified_coords
  if (identical(structure$family, "sem")) {
    indicators <- vapply(Filter(function(e) e$type == "loading", edges), `[[`, character(1), "to")
    dynamic <- unique(unlist(lapply(Filter(function(e) e$type == "regression", edges), function(e) c(e$from, e$to))))
    visible_ids <- ids[!ids %in% indicators | ids %in% dynamic | roles != "observed"]
    for (e in Filter(function(e) e$type == "covariance", edges))
      simplified$edges[[e$id]]$curve <- sem_covariance_bend(e, original_covariance_bends[[e$id]], visible_ids)
  }
  list(full = view, simplified = simplified)
}
