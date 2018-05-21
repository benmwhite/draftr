#' Computing points over replacement
#'
#' @param df A tidy data frame or tibble containing a column "player" for the player names, a column "position" with the positions for each player, and a column "spots" with the number of roster spots available for each position. The rest of the columns should be the point projections with one column for each different projection source.
#' @param teams The number of teams in the draft.
#' @param sds The number of standard deviations away from the mean to estimate the floors and ceilings for each player.
#' @param plotTitle A character string providing the main title of the output plot.
#'
#' @return A list with two elements: \code{df} and \code{chart}. \code{df} is a data frame with the computed points over replacement values, and \code{chart} is a ggplot object which produces the error bar chart.
#' @examples
#' foo <- get_porp(bball, plotTitle = "Example Data")
#' head(foo$df)
#' foo$chart

get_porp <- function(df, teams = 3L, sds = 3, plotTitle = NULL) {

  # checks to make sure data frame input is formatted correctly -------------------


  # verifying that the main input is a data frame object
  stopifnot(is.data.frame(df))

  # verifying that data frame is formatted appropriately
  columntypes <- sapply(df, typeof)
  columncheck <- unique(
    columntypes[!(names(columntypes) %in% c("player", "position", "spots"))]
  )

  stopifnot("player" %in% names(df),
            "position" %in% names(df),
            "spots" %in% names(df),
            "double" %in% columncheck,
            length(columncheck) == 1)

  # finding replacement projection
  tmp <- dplyr::mutate(
      dplyr::group_by(
        tidyr::gather(df,
                      source,
                      projection,
                      -c("player", "position", "spots")),
        position, source),
      rank = dplyr::dense_rank(desc(projection)))
  tmp$rep.spot <- tmp$spots * teams == tmp$rank
  repDat <- dplyr::select(dplyr::filter(tmp, rep.spot),
                  position,
                  source,
                  projection)
  names(repDat)[[3]] <- "rep.proj"

  # computing porp and creating output ------------------
  # output is list with two objects, a data frame with the average projection, average
  # points over replacement, and estimates of the floor and ceiling points over replacement
  # based on the "sds" argument.
  outDf <- data.frame(
    dplyr::summarise(
      dplyr::group_by(
        dplyr::mutate(
          dplyr::left_join(tmp, repDat, by = c("position", "source")),
          porp = projection - rep.proj),
        player, position),
      avg.proj = mean(projection),
      avg.porp = mean(porp),
      floor.porp = avg.porp - sds*sd(porp),
      ceiling.porp = avg.porp + sds*sd(porp)),
    stringsAsFactors = FALSE)

  outPlot <- ggplot2::ggplot(outDf,
                             ggplot2::aes(x = avg.porp,
                                          y = reorder(player, avg.porp),
                                          colour  = position)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = floor.porp, xmax = ceiling.porp)) +
    ggplot2::theme_minimal() +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::ylab("Player") +
    ggplot2::xlab("Projected points over replacement") +
    ggplot2::ggtitle(plotTitle)

  out <- list(df = dplyr::arrange(outDf, desc(avg.porp)), chart = outPlot)

  return(out)
}

