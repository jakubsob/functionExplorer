#' @title FunctionData
#' @description R6 class wrapper for data returned by \code{functiondepends} functions
#' @importFrom rlang %||%
FunctionData <- R6::R6Class(
  "FunctionData",
  public = list(
    #' @field path Character path of repository
    path = NULL,
    #' @field envir Environment to store parsed functions in
    envir = NULL, 
    #' @field functions Data.frame with functions found in \code{path}
    functions = NULL,
    #' @field dependencies Data.frame serving as edge list of function dependencies
    dependencies = NULL,
    #' @field graph, Igraph object
    graph = NULL,
    
    #' @description Initialize path and create new environment
    #' @param path Character path to search source R files in
    initialize = function() {
      self$envir <- new.env()
    },
    
    #' @description Check if fields are initialized
    is_initialized = function() {
      !any(
        is.null(self$envir), 
        is.null(self$path),
        is.null(self$functions),
        is.null(self$dependencies),
        is.null(self$graph)
      )
    },
    
    #' @description Reset fields to NULL
    reset = function() {
      self$path <- NULL
      self$envir <- new.env()
      self$functions <- NULL
      self$dependencies <- NULL
      self$graph <- NULL
    },
    
    #' @description Create path to downloaded data
    #' @param repo Character, name of repository in format Name/Repo
    #' @param branch Character, name of branch
    #' 
    #' @return Character, path
    create_path = function(repo, branch) {
      file.path(tempdir(), glue::glue("{basename(repo)}-{branch}"))
    },
    
    #' @description Download repository from GitHub
    #' @param repo Character, name of repository in format Name/Repo
    #' @param branch Character, name of branch
    #' 
    #' @return Logical, indicates whether download succeeded
    download_github = function(repo, branch) {
      destfile <- self$create_path(repo, branch)
      destfile_zip <- glue::glue("{destfile}.zip")
      tryCatch(
        {
          suppressWarnings(
            utils::download.file(
              url = sprintf("https://github.com/%s/archive/%s.zip", repo, branch),
              destfile = destfile_zip,
              quiet = TRUE
            )
          )
          utils::unzip(destfile_zip, exdir = tempdir(), overwrite = TRUE)
          unlink(destfile_zip)
          self$set_path(destfile)
          return(TRUE)
        },
        error = function(e) {
          return(FALSE)
        }
      )
    },
    
    #' @description Set \code{path} field
    #' @param path Character path to search source R files in
    set_path = function(path) {
      self$path <- path
    },
    
    #' @description Getter for \code{path} field
    get_path = function() {
      self$path
    },
    
    #' @description Set \code{envir} field
    #' @param envir Environment, default is \code{new.env()}
    set_envir = function(envir = new.env()) {
      self$envir <- envir
    },
    
    #' @description Getter for \code{envir} field 
    get_envir = function() {
      self$envir
    },
    
    #' @description Search for functions in \code{path}
    find_functions = function() {
      if (is.null(self$path) | is.null(self$envir)) {
        warning("Object not initialized")
        return()
      }
      self$functions <- functiondepends::find_functions(self$path, envir = self$envir) 
      if (!"Path" %in% names(self$functions)) {
        self$functions <- self$functions %>% 
          tidyr::unite("Path", tidyselect::starts_with("Level"), sep = "/")
      }
    },
    
    #' @description Getter for \code{functions} field
    get_functions = function() {
      self$functions
    },
    
    #' @description Find dependencies of found functions
    #' @param in_envir Logical, to be passed to \code{functiondepends::find_dependencies}
    find_dependencies = function(in_envir = TRUE) {
      if (is.null(self$path) | is.null(self$envir) | is.null(self$functions)) {
        warning("Object not initialized")
        return()
      }
      self$dependencies <- functiondepends::find_dependencies(
        unique(self$functions$Function),
        envir = self$envir,
        in_envir = in_envir,
        add_info = TRUE
      )
    },
    
    #' @description Getter for \code{dependencies} field
    get_dependencies = function() {
      self$dependencies
    },
    
    #' @description Make graph
    make_graph = function() {
      if (is.null(self$dependencies)) {
        warning("Object not initialized")
        return()
      }
      vertices <- unique(c(self$dependencies$Source, self$depedencies$Target))
      vertices <- vertices[!is.na(vertices)]
      links <- self$dependencies %>% 
        dplyr::select(Source, Target) %>% 
        dplyr::filter(!is.na(.))
      
      vertices <- links %>% 
        tidyr::pivot_longer(c(Source, Target), names_to = "Type", values_to = "Vertex") %>%
        dplyr::distinct(Vertex)
      
      self$graph <- igraph::graph_from_data_frame(
        d = links,
        vertices = vertices,
        directed = TRUE
      )
    },
    
    #' @description Get \code{graph}
    #' @param node Character, name of node to select
    #' @param mode Character, mode of neighborhood
    #' @param order Integer, order of neighborhood
    get_graph = function(node = NULL, mode = NULL, order = NULL) {
      if (is.null(node)) {
        return(self$graph)
      }
      g_ego <- igraph::ego(self$graph, order = order %||% 1, nodes = node, mode = mode %||% "all")
      igraph::induced_subgraph(self$graph, unlist(g_ego))
    },
    
    #' @description Plot network
    #' @param node Character, name of node to select
    #' @param mode Character, mode of neighborhood
    #' @param order Integer, order of neighborhood
    plot = function(node = NULL, mode = NULL, order = NULL) {
      graph <- self$get_graph(node, mode, order)
      
      edges <- graph %>% 
        igraph::as_edgelist() %>% 
        tibble::as_tibble(.name_repair = "minimal") %>% 
        magrittr::set_colnames(c("from", "to")) %>% 
        dplyr::mutate(
          shadow = TRUE,
          arrows = "to"
        )
      
      nodes <- tibble::tibble(
        id = names(igraph::V(graph)),
        label = id,
        size = sqrt(igraph::degree(graph)) * 10
      ) %>% 
        dplyr::left_join(
          self$functions %>% dplyr::select(Function, group = Source),
          by = c("id" = "Function")
        ) %>% 
        dplyr::mutate(
          group = dplyr::if_else(is.na(group), "Ext", group)
        )
      
      visNetwork::visNetwork(
        nodes = nodes,
        edges = edges,
        width = "100%"
      ) %>%
        visNetwork::visEdges(smooth = FALSE) %>% 
        visNetwork::visOptions(highlightNearest = TRUE) %>% 
        visNetwork::visPhysics(stabilization = FALSE)
    },
    
    #' @description Plot network
    #' @param node Character, name of node to select
    #' @param mode Character, mode of neighborhood
    #' @param order Integer, order of neighborhood
    #' @import ggplot2
    plot_distribution = function(node = NULL, mode = "all", order = NULL) {
      graph <- self$get_graph(node, mode, order)
      if (!self$is_initialized() | length(igraph::V(graph)) < 2) {
        return(NULL)
      }
      
      tibble::tibble(
        Degree = igraph::degree(graph, mode = mode)
      ) %>%
        dplyr::group_by(Degree) %>% 
        dplyr::tally(name = "Count") %>% 
        ggplot(aes(x = Degree, y = Count)) +
        geom_col() +
        theme_light() +
        labs(title = glue::glue("Node \"{mode}\" degree distribution"))
    }
  )
)