mahasiswa <- function(
    name,
    address,
    gender,
    age,
    hobbies,
    gpa) {
  env <- new.env()
  env$name <- name
  env$address <- address
  env$gender <- gender
  env$age <- age
  env$hobbies <- hobbies
  env$gpa <- gpa
  env$to_string <- function() {
    paste(env$name, env$age, env$gender, env$address, toString(env$hobbies), env$gpa)
  }
  env
}

node <- function(item) {
  env <- new.env()
  env$item <- item
  env$next_node <- NULL
  env$previous_node <- NULL
  env
}

linked_list <- function() { # nolint: cyclocomp_linter.
  env <- new.env()

  env$head <- NULL
  env$tail <- NULL

  env$append <- function(item) {
    new_node <- node(item)
    if (is.null(env$head)) {
      env$tail <- new_node
      env$head <- new_node
    } else {
      env$tail$next_node <- new_node
      new_node$previous_node <- env$tail
      env$tail <- new_node
    }
  }

  env$prepend <- function(item) {
    new_node <- node(item)
    if (is.null(env$head)) {
      env$head <- new_node
      env$tail <- new_node
    } else {
      new_node$next_node <- env$head
      env$head$previous_node <- new_node
      env$head <- new_node
    }
  }

  env$insert <- function(item, index) {
    if (index < 0 || index > env$size()) {
      return(cat("[WARN] index must be between 0 -", env$size(), "\n"))
    }

    if (index == 0) {
      return(env$prepend(item))
    }

    if (index == env$size()) {
      return(env$append(item))
    }

    new_node <- node(item)
    current_node <- env$head
    counter <- 0

    while (!is.null(current_node) && counter < index - 1) {
      current_node <- current_node$next_node
      counter <- counter + 1
    }

    new_node$next_node <- current_node$next_node
    current_node$next_node$previous_node <- new_node
    current_node$next_node <- new_node
    new_node$previous_node <- current_node
  }

  env$pop <- function() {
    removed_item <- NULL

    if (!is.null(env$tail)) {
      removed_item <- env$tail$item
      env$tail <- env$tail$previous_node
    }

    if (is.null(env$tail)) {
      env$head <- NULL
    } else {
      env$tail$next_node <- NULL
    }

    return(removed_item)
  }

  env$unshift <- function() {
    removed_item <- NULL

    if (!is.null(env$head)) {
      removed_item <- env$head$item
      env$head <- env$head$next_node
    }

    if (is.null(env$head)) {
      env$tail <- NULL
    } else {
      env$head$previous_node <- NULL
    }

    return(removed_item)
  }

  env$remove <- function(index) {
    if (is.null(env$head)) {
      return(NULL)
    }

    if (index < 0 || index > env$size() - 1) {
      return(cat("[WARN] index must be between 0 -", env$size(), "\n"))
    }

    if (index == 0) {
      return(env$unshift())
    }

    if (index == env$size() - 1) {
      return(env$pop())
    }

    removed_item <- NULL

    current_node <- env$head
    counter <- 0
    while (!is.null(current_node) && counter < index) {
      current_node <- current_node$next_node
      counter <- counter + 1
    }

    removed_item <- current_node$item
    current_node$previous_node$next_node <- current_node$next_node
    current_node$next_node$previous_node <- current_node$previous_node
    current_node <- NULL
    return(removed_item)
  }

  env$size <- function() {
    current_node <- env$head
    counter <- 0
    while (!is.null(current_node)) {
      counter <- counter + 1
      current_node <- current_node$next_node
    }
    counter
  }

  env$all <- function() {
    items <- list()
    current_node <- env$head
    while (!is.null(current_node)) {
      append(items, current_node$item)
      current_node <- current_node$next_node
    }
    return(items)
  }

  env$display <- function() {
    current_node <- env$head
    counter <- 0
    while (!is.null(current_node)) {
      cat(counter, "|", current_node$item$to_string(), "\n")
      current_node <- current_node$next_node
      counter <- counter + 1
    }
  }

  env$display_backward <- function() {
    current_node <- env$tail
    counter <- 0
    while (!is.null(current_node)) {
      cat(counter, "|", current_node$item$to_string(), "\n")
      current_node <- current_node$previous_node
      counter <- counter + 1
    }
  }

  env
}
