#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' \dontrun{
#' create_game()
#' }
#'
#' @export
create_game <- function()
{
    game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( game )
}



#' @title
#'   Select a door in a Monty Hall Problem game.
#'
#' @description
#'   `select_door()` selects an initial door from the three doors
#'   presented to the contestant at the beginning of the game.
#'
#' @details
#'  In the "Let's Make a Deal" game show, the contestant is presented
#'  with three doors to choose from. Behind two doors are goats and the
#'  other door has a car behind it.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 1 numeric vector indiciting
#' whether the contestant selected door 1, 2, or 3.
#'
#' @examples
#'   select_door()
#'
#' \dontrun{
#' select_door()
#' }
#'
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Open a goat door in a Monty Hall Problem game.
#'
#' @description
#'   `open_goat_door()` opens one of the doors that contains a
#'   goat after the contestant has made their intial door choice.
#'
#' @details
#'   After the contestant chooses a door in the "Let's Make a Deal"
#'   game show, Monty Hall will reveal one of the goat doors to the
#'   contestant.
#'
#' @param game = the argument containing the positions of the goats and car from the create_game function
#'  from the create_game function
#'
#' @param a.pick = the arguement containing the contestants initial door selection from the select_door function
#'
#'
#' @return The function returns a length 1 numeric vector indiciting
#' whether the opened door containing the goat was door 1, 2, or 3.
#' This function will not open the door with the car behind it or
#' the door that was the contestants initial pick.
#'
#' @examples
#'   open_goat_door( game, a.pick )
#'
#' \dontrun{
#' open_goat_door( game, a.pick )
#' }
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Decide whether to change the contestants initial door selection
#'   in the Monty Hall problem game or change their initial selection.
#'
#' @description
#'   `change_door()` decides for the contestant as to whether they are
#'   keeping their initial door selection or changing their initial
#'   selection.
#'
#' @details
#'   After the Monty Hall reveals a goat door in the "Let's Make a Deal"
#'   game show, the contestant must make a decision as to whehter they
#'   are going to keep their initial door selection or change their
#'   selection to the other available door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch.
#'
#' @param stay = a true/false argument where we assign the "TRUE" value to if the contestant is sticking with their initial door selection and "FALSE" if they are staying with their initial selection.
#'
#' @param opened.door = the argument which has the returned door from the open_goat_door function.
#'
#' @param a.pick = the arguement containing the contestants initial door selection from the select_door function
#'
#'
#' @return The function returns a length 1 numeric vector indictating
#' the contestants final door choice in the Monty Hall Problem game.
#' The door choice will either be doors 1, 2, or 3.
#'
#' @examples
#'   change_door( stay=T, opened.door, a.pick )
#'
#' \dontrun{
#' change_door( stay=T, opened.door, a.pick )
#' }
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine if the contestant won the Monty Hall Problem game.
#'
#' @description
#'   `determine_winner()` determines if the contestants final door
#'   selection contained a car or a goat.
#'
#' @details
#'   After the contestant makes their final door selection in the
#'   "Let's Make a Deal" game show, Monty Hall reveals whether the
#'   contestants door contains a car or goat.
#'
#' @param final.pick = the argument which has the returned door from the change_door function.
#'
#'  @param game = the argument containing the positions of the goats and car from the create_game function
#'
#' @return The function returns a length 3 or 4 character vector
#'   indicating whether the contestant won or loss the game.
#'
#' @examples
#'   determine_winner( final.pick, game )
#'
#' \dontrun{
#' determine_winner( final.pick, game )
#' }
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Play an entirety of the Monty Hall Problem game.
#'
#' @description
#'   `play_game()` plays the entirety of the Monty Hall Problem
#'   game from game creation to determining if the contestant
#'   won or loss.
#'
#' @details
#'   The game replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a data framewith the results of
#' the Monty Hall Problem game. The data frame shows whether it
#' was advantegous to stay or switch in a particular Monty Hall game.
#'
#'
#' @examples
#'   play_game()
#'
#' \dontrun{
#' play_game()
#' }
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Play the Monty Hall Problem game a set number of times.
#'
#' @description
#'   `play_n_games()` plays the Monty Hall Problem game a
#'   assigned amount of times.
#'
#' @details
#'   The game replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param n = The number of times the Monty Hall game will be run.
#'
#' @return The function returns a data frame with the results of
#' the Monty Hall game simulations. The rows contain the game strategy
#' information. The columns contain the game outcome information.
#'
#' @examples
#'   play_n_games( n=100 )
#'
#' \dontrun{
#' play_n_games( n=100 )
#' }
#'
#' @export
play_n_games <- function( n=100 )
{

  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}
