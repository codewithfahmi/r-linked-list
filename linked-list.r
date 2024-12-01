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
    paste(
      paste("name:", env$name),
      paste("age:", env$age),
      paste("gender:", env$gender),
      paste("address:", env$address),
      paste("hobbies:", toString(env$hobbies)),
      paste("gpa:", env$gpa),
      sep = ", "
    )
  }
  env
}

node <- function(item) {
  env <- new.env()
  env$item <- item
  env$next_node <- NULL
  env
}

linked_list <- function() { # nolint: cyclocomp_linter.
  env <- new.env()

  env$head <- NULL

  env$append <- function(item) {
    if (is.null(env$head)) {
      return(env$prepend(item))
    }

    current_node <- env$head
    while (!is.null(current_node$next_node)) {
      current_node <- current_node$next_node
    }
    current_node$next_node <- node(item)
    env$head
  }

  env$prepend <- function(item) {
    new_node <- node(item)
    new_node$next_node <- env$head
    env$head <- new_node
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
    current_node$next_node <- new_node
  }

  env$pop <- function() {
    if (is.null(env$head)) {
      return(NULL)
    }

    if (is.null(env$head$next_node)) {
      removed_item <- env$head$item
      env$head <- NULL
      return(removed_item)
    }

    current_node <- env$head
    while (!is.null(current_node$next_node$next_node)) {
      current_node <- current_node$next_node
    }
    removed_item <- current_node$next_node$item
    current_node$next_node <- NULL
    return(removed_item)
  }

  env$unshift <- function() {
    if (is.null(env$head)) {
      return(NULL)
    }

    removed_item <- env$head$item
    env$head <- env$head$next_node
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

    current_node <- env$head
    counter <- 0
    while (!is.null(current_node) && counter < index - 1) {
      current_node <- current_node$next_node
      counter <- counter + 1
    }

    removed_item <- current_node$next_node$item
    current_node <- current_node$next_node$next_node
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

  env$bubble_sort <- function(comparator) {
    if (is.null(env$head) || is.null(env$head$next_node)) {
      return()
    }

    sorted <- FALSE
    while (!sorted) {
      sorted <- TRUE
      current_node <- env$head

      while (!is.null(current_node$next_node)) {
        next_node <- current_node$next_node
        if (comparator(current_node$item, next_node$item)) {
          temp <- current_node$item
          current_node$item <- next_node$item
          next_node$item <- temp
          sorted <- FALSE
        }
        current_node <- current_node$next_node
      }
    }
  }

  env$insertion_sort <- function(comparator) {
    if (is.null(env$head) || is.null(env$head$next_node)) {
      return()
    }

    sorted_head <- NULL
    current_node <- env$head

    while (!is.null(current_node)) {
      next_node <- current_node$next_node

      if (is.null(sorted_head) || !comparator(sorted_head$item, current_node$item)) { # nolint: line_length_linter.
        current_node$next_node <- sorted_head
        sorted_head <- current_node
      } else {
        sorted_node <- sorted_head
        while (!is.null(sorted_node$next_node) && comparator(sorted_node$next_node$item, current_node$item)) { # nolint: line_length_linter.
          sorted_node <- sorted_node$next_node
        }
        current_node$next_node <- sorted_node$next_node
        sorted_node$next_node <- current_node
      }
      current_node <- next_node
    }
    env$head <- sorted_head
  }


  env
}

compare_by_name <- function(a, b) {
  return(a$name < b$name)
}

ll <- linked_list()
ll$append(mahasiswa("Fahmi", "Jepara", 21, "L", list("Makan", "Tidur"), 2.5))
ll$append(mahasiswa("Budi", "Jakarta", "L", 20, list("Futsal", "Game"), 3.0))
ll$append(mahasiswa("Alya", "Bandung", "P", 22, list("Membaca", "Renang"), 3.8))
ll$insertion_sort(compare_by_name)
ll$display()
