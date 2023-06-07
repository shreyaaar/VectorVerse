#' Generates a Voronoi diagram with random seed points.
#'
#' @param numPoints The number of points to generate in the grid.
#' @param numSeeds The number of random seed points to generate.
#' @param minX The minimum x-coordinate value for the grid.
#' @param maxX The maximum x-coordinate value for the grid.
#' @param minY The minimum y-coordinate value for the grid.
#' @param maxY The maximum y-coordinate value for the grid.
#' @param palette The color palette for the Voronoi diagram.
#'   Default is rainbow(numSeeds).
#' @param ... Additional arguments to be passed to the image function.
#'
#' @return None (plot is displayed).
#'
#' @examples
#' # Generate a Voronoi diagram with defalt parameters
#' VoronoiVerse(100, 20, 0, 10, 0, 10)
#'
#' # Generate a Voronoi diagram with a custom color palette
#' VoronoiVerse(100, 20, 0, 10, 0, 10, palette = c("#AAE6AF", "#AAE6D9", "#AAADE6", "#E6AAE6", "#E6AAAA"))
#'
#' @importFrom stats runif
#' @importFrom graphics image
#' @import RColorBrewer
#' @import ggplot2
#'
#' @export
VoronoiVerse <- function(numPoints = 100, numSeeds = 20,
                         minX = 0, maxX = 10, minY = 0, maxY = 10,
                         palette = rainbow(numSeeds), ...) {
  # Generate random seed points
  seedX <- runif(numSeeds, minX, maxX)
  seedY <- runif(numSeeds, minY, maxY)

  # Create a grid of points
  gridX <- seq(minX, maxX, length.out = numPoints)
  gridY <- seq(minY, maxY, length.out = numPoints)

  # Initialize matrix to store Voronoi regions
  voronoiMatrix <- matrix(0, nrow = numPoints, ncol = numPoints)

  # Calculate Voronoi regions
  for (i in 1:numPoints) {
    for (j in 1:numPoints) {
      distances <- sqrt((gridX[i] - seedX)^2 + (gridY[j] - seedY)^2)
      closestSeed <- which.min(distances)
      voronoiMatrix[i, j] <- closestSeed
    }
  }

  # Plot the Voronoi diagram
  image(gridX, gridY, voronoiMatrix, col = palette, xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "", ...)
}
