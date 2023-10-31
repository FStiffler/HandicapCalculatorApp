#' Calculate course handicap
#'
#' Calculates the course handicap of a player based on the player's handicap
#' index and relevant course parameters
#'
#' @param handicap # current handicap index of player
#' @param cr # course rating of selected club and tee box
#' @param sr # slope rating of selected club and tee box
#' @param par # course rating of selected club and tee box
#'
#' @return The player's course adjusted handicap
#' @export
#'
#' @examples
#' calculate_course_handicap(49, 67.5, 132, 69)
calculate_course_handicap<-function(handicap, cr, sr, par){
  
  courseHandicap <- round(handicap*sr/113+cr-par, 0)
  return(courseHandicap)
  
}