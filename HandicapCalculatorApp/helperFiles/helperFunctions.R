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


#' Calculate additional strokes
#'
#' Calculates how many additional strokes the player may take at each hole based
#' on the player's course handicap
#'
#' @param courseData # data frame with containing course information for every hole
#' @param courseHandicap # the calculated course handicap from function calculate_course_handicap() 
#'
#' @return # update data frame with a new column for additional strokes based on course handicap
#' @export
#'
#' @examples
calculate_additional_strokes<-function(courseData, courseHandicap){
  
  # calculate the least amount of additional strokes to be added to every hole
  minAdditionalStrokes<-rep(floor(courseHandicap/18), 18)
  
  # calculate the remaining strokes to be distributed on the holes
  remainingStrokes<-courseHandicap%%18
  
  # distribute the remaining strokes assuming decreasing hole difficulty
  remainingStrokesDistributed<-c(rep(1, remainingStrokes),rep(0, 18-remainingStrokes))
  
  # sum up additional strokes
  additionalStrokes <- minAdditionalStrokes+remainingStrokesDistributed
  
  
  # sort holes based on difficulty in descending order
  courseDataOut<-courseData%>%
    arrange(hcp)%>%
    add_column(additionalStrokes = additionalStrokes)%>%
    arrange(hole)
  
  return(courseDataOut)
  
}


#' Calculate stableford points based on number of strokes
#' 
#' Assigns points based on the registered number of strokes. The points are assigned
#' with reference to netto PAR. 
#'
#' @param overNettoPar 
#'
#' @return # The corresponding stableford points based on the number of strokes
#' @export
#'
#' @examples
#' calculate_stableford_points(0)
calculate_stableford_points <- function(overNettoPar){

  if(overNettoPar==-4){
    stablefordPoints <- 6
  }else if(overNettoPar==-3){
    stablefordPoints <- 5
  }else if(overNettoPar==-2){
    stablefordPoints <- 4
  }else if(overNettoPar==-1){
    stablefordPoints <- 3
  }else if(overNettoPar==0){
    stablefordPoints <- 2
  }else if(overNettoPar==1){
    stablefordPoints <- 1
  }else if(overNettoPar>=2){
    stablefordPoints <- 0
  }else{
    stop("Invalid number of strokes")
  }
  
}