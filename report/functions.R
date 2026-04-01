backgroundCard <- function(fileName) {
  # read file
  content <- readLines(fileName)

  # extract yaml metadata
  # Find the positions of the YAML delimiters (----- or ---)
  yamlStart <- grep("^---|^-----", content)[1]
  yamlEnd <- grep("^---|^-----", content)[2]

  if (any(is.na(c(yamlStart, yamlEnd)))) {
    metadata <- NULL
  } else {
    # identify YAML block
    id <- (yamlStart + 1):(yamlEnd - 1)
    # Parse the YAML content
    metadata <- yaml::yaml.load(paste(content[id], collapse = "\n"))
    # eliminate yaml part from content
    content <- content[-(yamlStart:yamlEnd)]
  }

  tmpFile <- tempfile(fileext = ".md")
  writeLines(text = content, con = tmpFile)

  # metadata referring to keys
  backgroundKeywords <- list(
    header = "bslib::card_header",
    footer = "bslib::card_footer"
  )
  keys <- names(backgroundKeywords) |>
    rlang::set_names() |>
    purrr::map(\(x) {
      if (x %in% names(metadata)) {
        paste0(backgroundKeywords[[x]], "(metadata[[x]])") |>
          rlang::parse_expr() |>
          rlang::eval_tidy()
      } else {
        NULL
      }
    }) |>
    purrr::compact()

  arguments <- c(
    # metadata referring to arguments of card
    metadata[names(metadata) %in% names(formals(bslib::card))],
    # content
    list(
      keys$header,
      bslib::card_body(shiny::HTML(suppressWarnings(markdown::markdownToHTML(
        file = tmpFile, fragment.only = TRUE
      )))),
      keys$footer
    ) |>
      purrr::compact()
  )

  unlink(tmpFile)

  do.call(bslib::card, arguments)
}
summaryCdmName <- function(data) {
  if (length(data) == 0) {
    return(list("<b>CDM names</b>" = ""))
  }
  x <- data |>
    purrr::map(\(x) {
      x |>
        dplyr::group_by(.data$cdm_name) |>
        dplyr::summarise(number_rows = dplyr::n(), .groups = "drop")
    }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$cdm_name) |>
    dplyr::summarise(
      number_rows = as.integer(sum(.data$number_rows)),
      .groups = "drop"
    ) |>
    dplyr::mutate(label = paste0(.data$cdm_name, " (", .data$number_rows, ")")) |>
    dplyr::pull("label") |>
    rlang::set_names() |>
    as.list()
  list("<b>CDM names</b>" = x)
}
summaryPackages <- function(data) {
  if (length(data) == 0) {
    return(list("<b>Packages versions</b>" = ""))
  }
  x <- data |>
    purrr::map(\(x) {
      x |>
        omopgenerics::addSettings(
          settingsColumn = c("package_name", "package_version")
        ) |>
        dplyr::group_by(.data$package_name, .data$package_version) |>
        dplyr::summarise(number_rows = dplyr::n(), .groups = "drop") |>
        dplyr::right_join(
          omopgenerics::settings(x) |>
            dplyr::select(c("package_name", "package_version")) |>
            dplyr::distinct(),
          by = c("package_name", "package_version")
        ) |>
        dplyr::mutate(number_rows = dplyr::coalesce(.data$number_rows, 0))
    }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$package_name, .data$package_version) |>
    dplyr::summarise(
      number_rows = as.integer(sum(.data$number_rows)),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$package_name) |>
    dplyr::group_split() |>
    as.list()
  lab <- "<b>"
  names(x) <- x |>
    purrr::map_chr(\(x) {
      if (nrow(x) > 1) {
        lab <<- "<b style='color:red'>"
        paste0("<b style='color:red'>", unique(x$package_name), " (Multiple versions!) </b>")
      } else {
        paste0(
          x$package_name, " (version = ", x$package_version,
          "; number records = ", x$number_rows,")"
        )
      }
    })
  x <- x |>
    purrr::map(\(x) {
      if (nrow(x) > 1) {
        paste0(
          "version = ", x$package_version, "; number records = ",
          x$number_rows
        ) |>
          rlang::set_names() |>
          as.list()
      } else {
        x$package_name
      }
    })
  list(x) |>
    rlang::set_names(nm = paste0(lab, "Packages versions</b>"))
}
summaryMinCellCount <- function(data) {
  if (length(data) == 0) {
    return(list("<b>Min Cell Count Suppression</b>" = ""))
  }
  x <- data |>
    purrr::map(\(x) {
      x |>
        omopgenerics::addSettings(settingsColumn = "min_cell_count") |>
        dplyr::group_by(.data$min_cell_count) |>
        dplyr::summarise(number_rows = dplyr::n(), .groups = "drop") |>
        dplyr::right_join(
          omopgenerics::settings(x) |>
            dplyr::select("min_cell_count") |>
            dplyr::distinct(),
          by = "min_cell_count"
        ) |>
        dplyr::mutate(number_rows = dplyr::coalesce(.data$number_rows, 0))
    }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$min_cell_count) |>
    dplyr::summarise(
      number_rows = as.integer(sum(.data$number_rows)),
      .groups = "drop"
    ) |>
    dplyr::mutate(min_cell_count = as.integer(.data$min_cell_count)) |>
    dplyr::arrange(.data$min_cell_count) |>
    dplyr::mutate(
      label = dplyr::if_else(
        .data$min_cell_count == 0L,
        "<b style='color:red'>Not censored</b>",
        paste0("Min cell count = ", .data$min_cell_count)
      ),
      label = paste0(.data$label, " (", .data$number_rows, ")")
    ) |>
    dplyr::pull("label") |>
    rlang::set_names() |>
    as.list()
  lab <- ifelse(any(grepl("Not censored", unlist(x))), "<b style='color:red'>", "<b>")
  list(x) |>
    rlang::set_names(nm = paste0(lab, "Min Cell Count Suppression</b>"))
}
summaryPanels <- function(data) {
  if (length(data) == 0) {
    return(list("<b>Panels</b>" = ""))
  }
  x <- data |>
    purrr::map(\(x) {
      if (nrow(x) == 0) {
        res <- omopgenerics::settings(x) |>
          dplyr::select(!c(
            "result_id", "package_name", "package_version", "group", "strata",
            "additional", "min_cell_count"
          )) |>
          dplyr::relocate("result_type") |>
          as.list() |>
          purrr::map(\(x) sort(unique(x)))
      } else {
        sets <- c("result_type", omopgenerics::settingsColumns(x))
        res <- x |>
          omopgenerics::addSettings(settingsColumn = sets) |>
          dplyr::relocate(dplyr::all_of(sets)) |>
          omopgenerics::splitAll() |>
          dplyr::select(!c(
            "variable_name", "variable_level", "estimate_name",
            "estimate_type", "estimate_value", "result_id"
          )) |>
          as.list() |>
          purrr::map(\(values) {
            values <- as.list(table(values))
            paste0(names(values), " (number rows = ", values, ")") |>
              rlang::set_names() |>
              as.list()
          })
      }
      res
    })
  list(x) |>
    rlang::set_names(nm = "<b>Panels</b>")
}
simpleTable <- function(result,
                        header = character(),
                        group = character(),
                        hide = character()) {
  # initial checks
  if (length(header) == 0) header <- character()
  if (length(group) == 0) group <- NULL
  if (length(hide) == 0) hide <- character()

  if (nrow(result) == 0) {
    return(gt::gt(dplyr::tibble()))
  }

  result <- result |>
    omopgenerics::addSettings() |>
    omopgenerics::splitAll() |>
    dplyr::select(-"result_id")

  # format estimate column
  formatEstimates <- c(
    "N (%)" = "<count> (<percentage>%)",
    "N" = "<count>",
    "median [Q25 - Q75]" = "<median> [<q25> - <q75>]",
    "mean (SD)" = "<mean> (<sd>)",
    "[Q25 - Q75]" = "[<q25> - <q75>]",
    "range" = "[<min> <max>]",
    "[Q05 - Q95]" = "[<q05> - <q95>]"
  )
  result <- result |>
    visOmopResults::formatEstimateValue(
      decimals = c(integer = 0, numeric = 1, percentage = 0)
    ) |>
    visOmopResults::formatEstimateName(estimateName = formatEstimates) |>
    suppressMessages() |>
    visOmopResults::formatHeader(header = header) |>
    dplyr::select(!dplyr::any_of(c("estimate_type", hide)))
  if (length(group) > 1) {
    id <- paste0(group, collapse = "; ")
    result <- result |>
      tidyr::unite(col = !!id, dplyr::all_of(group), sep = "; ", remove = TRUE)
    group <- id
  }
  result <- result |>
    visOmopResults::formatTable(groupColumn = group)
  return(result)
}
tidyDT <- function(x,
                   columns,
                   pivotEstimates) {
  groupColumns <- omopgenerics::groupColumns(x)
  strataColumns <- omopgenerics::strataColumns(x)
  additionalColumns <- omopgenerics::additionalColumns(x)
  settingsColumns <- omopgenerics::settingsColumns(x)

  # split and add settings
  x <- x |>
    omopgenerics::splitAll() |>
    omopgenerics::addSettings()

  # remove density
  x <- x |>
    dplyr::filter(!.data$estimate_name %in% c("density_x", "density_y"))

  # estimate columns
  if (pivotEstimates) {
    estCols <- unique(x$estimate_name)
    x <- x |>
      omopgenerics::pivotEstimates()
  } else {
    estCols <- c("estimate_name", "estimate_type", "estimate_value")
  }

  # order columns
  cols <- list(
    "CDM name" = "cdm_name", "Group" = groupColumns, "Strata" = strataColumns,
    "Additional" = additionalColumns, "Settings" = settingsColumns,
    "Variable" = c("variable_name", "variable_level")
  ) |>
    purrr::map(\(x) x[x %in% columns]) |>
    purrr::compact()
  cols[["Estimates"]] <- estCols
  x <- x |>
    dplyr::select(dplyr::all_of(unname(unlist(cols))))

  # prepare the header
  container <- shiny::tags$table(
    class = "display",
    shiny::tags$thead(
      purrr::imap(cols, \(x, nm) shiny::tags$th(colspan = length(x), nm)) |>
        shiny::tags$tr(),
      shiny::tags$tr(purrr::map(unlist(cols), shiny::tags$th))
    )
  )

  # create DT table
  DT::datatable(
    data = x,
    filter = "top",
    container = container,
    rownames = FALSE,
    options = list(searching = FALSE)
  )
}
prepareResult <- function(result, resultList) {
  purrr::map(resultList, \(x) filterResult(result, x))
}
filterResult <- function(result, filt) {

  q <- function(nm) {
    paste0(".data$", nm, " %in% filt[[\"", nm, "\"]]") |>
      rlang::parse_exprs() |>
      rlang::eval_tidy()
  }

  # filter columns
  cols <- c("cdm_name", "variable_name", "variable_level", "estimate_name")
  cols <- cols[cols %in% names(filt)]
  for (nm in cols) {
    result <- dplyr::filter(result, !!!q(nm))
  }

  # filter settings
  cols <- c(
    "result_id", "result_type", "package_name", "package_version",
    "min_cell_count", omopgenerics::settingsColumns(result = result)
  )
  cols <- cols[cols %in% names(filt)]
  for (nm in cols) {
    result <- omopgenerics::filterSettings(result, !!!q(nm))
  }

  # filter group
  cols <- omopgenerics::groupColumns(result = result)
  cols <- cols[cols %in% names(filt)]
  for (nm in cols) {
    result <- omopgenerics::filterGroup(result, !!!q(nm))
  }

  # filter strata
  cols <- omopgenerics::strataColumns(result = result)
  cols <- cols[cols %in% names(filt)]
  for (nm in cols) {
    result <- omopgenerics::filterStrata(result, !!!q(nm))
  }

  # filter additional
  cols <- omopgenerics::additionalColumns(result = result)
  cols <- cols[cols %in% names(filt)]
  for (nm in cols) {
    result <- omopgenerics::filterAdditional(result, !!!q(nm))
  }

  # correct settings
  set <- omopgenerics::settings(result)
  # remove columns that all are NA
  cols <- colnames(set) |>
    purrr::keep(\(x) any(!is.na(x)))
  set <- set |>
    dplyr::select(dplyr::all_of(cols))
  # replace NA for '-NA-'
  set <- set |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::where(is.character),
      .fns = \(x) dplyr::coalesce(x, "-NA-")
    ))

  # final result
  omopgenerics::newSummarisedResult(x = result, settings = set)
}
getValues <- function(result, resultList) {
  resultList |>
    purrr::imap(\(x, nm) {
      res <- filterResult(result, x)
      values <- res |>
        dplyr::select(!c("estimate_type", "estimate_value")) |>
        dplyr::distinct() |>
        omopgenerics::splitAll() |>
        dplyr::select(!"result_id") |>
        as.list() |>
        purrr::map(\(x) sort(unique(x)))
      valuesSettings <- omopgenerics::settings(res) |>
        dplyr::select(!dplyr::any_of(c(
          "result_id", "result_type", "package_name", "package_version",
          "group", "strata", "additional", "min_cell_count"
        ))) |>
        as.list() |>
        purrr::map(\(x) sort(unique(x[!is.na(x)]))) |>
        purrr::compact()
      values <- c(values, valuesSettings)
      names(values) <- paste0(nm, "_", names(values))
      values
    }) |>
    purrr::flatten()
}
getSelected <- function(choices) {
  purrr::imap(choices, \(vals, nm) {
    if (grepl("_denominator_sex$", nm)) {
      if ("Both" %in% vals) return("Both")
      return(vals[[1]])
    }

    if (grepl("_denominator_age_group$", nm)) {
      bounds <- regmatches(vals, regexec("^(\\d+) to (\\d+)$", vals))
      valid <- vapply(bounds, length, integer(1)) == 3
      if (any(valid)) {
        ranges <- vapply(bounds[valid], \(x) as.numeric(x[3]) - as.numeric(x[2]), numeric(1))
        return(vals[valid][[which.max(ranges)]])
      } else {
        return(vals[[1]])
      }
    }

    if (grepl("_outcome_cohort_name$", nm)) {
      return(vals[[1]])
    }

    vals
  })
}
renderInteractivePlot <- function(plt, interactive) {
  plt <- stylePlot(plt)
  if (interactive) {
    plotly::renderPlotly(plt)
  } else {
    shiny::renderPlot(plt)
  }
}
updateMessage <- shiny::div(
  style = "font-size: 8pt; color: var(--bs-danger);",
  shiny::icon("circle-exclamation"),
  "Filters have changed please consider to use the update content button!"
)
plotColours <- function(plot) {
  built <- ggplot2::ggplot_build(plot)

  colour_scale <- built$plot$scales$get_scales("colour")
  if (!is.null(colour_scale)) {
    colorLabels <- colour_scale$get_labels()
  } else {
    colorLabels <- character()
  }

  fill_scale <- built$plot$scales$get_scales("fill")
  if (!is.null(fill_scale)) {
    fillLabels <- fill_scale$get_labels()
  } else {
    fillLabels <- character()
  }

  list(color = colorLabels, fill = fillLabels)
}
stylePlot <- function(plot, style = "_brand.yml") {
  theme  <- visOmopResults::themeVisOmop(style = style)
  brand  <- brand.yml::read_brand_yml(path = style)

  # new colours
  newColors <- visOmopPlotPalette(brand = brand)

  # colours in use
  currentColors <- plotColours(plot = plot)

  # edit color
  if (length(currentColors$color) > 0 && !is.null(newColors$color)) {
    values <- getPalette(colors = newColors$color, n = length(currentColors$color)) |>
      rlang::set_names(currentColors$color)
    plot <- plot +
      ggplot2::scale_colour_manual(values = values)
  }

  # edit fill
  if (length(currentColors$fill) > 0 && !is.null(newColors$fill)) {
    values <- getPalette(colors = newColors$fill, n = length(currentColors$fill)) |>
      rlang::set_names(currentColors$fill)
    plot <- plot +
      ggplot2::scale_fill_manual(values = values)
  }

  plot +
    theme
}
visOmopPlotPalette <- function(brand) {
  colourPalette <- brand$defaults$visOmopResults$plot$colour_palette
  fillPalette <- brand$defaults$visOmopResults$plot$fill_palette

  # get names from palette if needed
  if (!is.null(colourPalette)) {
    colourPalette <- colourPalette |>
      purrr::map_chr(\(x) brand.yml::brand_color_pluck(brand, x))
  }

  if (!is.null(fillPalette)) {
    fillPalette <- fillPalette |>
      purrr::map_chr(\(x) brand.yml::brand_color_pluck(brand, x))
  } else {
    fillPalette <- colourPalette
  }

  list(color = colourPalette, fill = fillPalette)
}
hex2hue <- function(h) {
  r <- strtoi(substr(h, 2, 3), 16) / 255
  g <- strtoi(substr(h, 4, 5), 16) / 255
  b <- strtoi(substr(h, 6, 7), 16) / 255
  max_c <- max(r, g, b)
  min_c <- min(r, g, b)
  if (max_c == min_c) {
    return(0)
  }
  d <- max_c - min_c
  hue <- switch(which.max(c(r, g, b)),
                `1` = (g - b) / d + ifelse(g < b, 6, 0),
                `2` = (b - r) / d + 2,
                `3` = (r - g) / d + 4)
  (hue / 6) * 360
}
sortHue <- function(hex, ref) {
  l <- length(hex)
  if (l > 2) {
    hue <- purrr::map_dbl(hex, hex2hue)
    hex <- hex[order(hue)]
  }
  id <- which(hex == ref)
  hex[c(id:l, seq_len(id-1))]
}
getPalette <- function(colors, n) {
  if (n <= length(colors)) {
    colors <- sortHue(colors[1:n], colors[1])
  } else {
    ramp <- colorRampPalette(c(sortHue(colors, colors[1]), colors[1]))
    colors <- ramp(n = n + 1)[1:n]
  }
  return(colors)
}
