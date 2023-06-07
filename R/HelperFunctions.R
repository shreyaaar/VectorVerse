#' Helper function
#' Lets the user input the color choices
#'
#' @return list of color codes
#'

select_colors <- function() {

  # asks if user wants default option
  selection_option <- tolower(readline("Enter 'custom' to select your own colors or 'default' to use the default color option: "))


  if (selection_option == "custom") {

    # asks how many colors
    num_colors <- as.integer(readline("Enter the number of colors: "))

    # makes empty vector to store color codes
    code_list <- vector("character", length = num_colors)

    # asks which colors
    for (i in 1:num_colors) {
      color_name <- tolower(readline(paste("Enter the color name for color", i, "(or enter '?' for a list of colors): ")))

      # if user wants to see color list
      if (color_name == "?") {
        available_colors <- colors()
        available_colors <- unique(gsub("[[:digit:]]", "", available_colors))
        print(available_colors)
        color_name <- tolower(readline(paste("Enter the color name for color", i, ": ")))
      }

      # converts color to hex code
      color_code <- col2rgb(color_name)
      palette_code <- paste0("#", sprintf("%02X%02X%02X", color_code[1], color_code[2], color_code[3]))
      code_list[i] <- palette_code
    }

    # returns list of codes
    return(code_list)


    #if user wants default
  } else {

    default_colors <- c("red", "green", "blue", "yellow", "purple", "orange", "cyan")

    default_codes <- sapply(default_colors, function(color_name) {
      color_code <- col2rgb(color_name)
      paste0("#", sprintf("%02X%02X%02X", color_code[1], color_code[2], color_code[3]))
    })

    return(default_codes)
  }
}








