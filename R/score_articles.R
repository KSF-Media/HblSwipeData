#' Returns personlaised score for given list of articles
#'
#' Returns IDs for the given articles together with their scores. The scores are
#' based on the swipe information which is stored in a global variable.
#'
#' @param in_uid identifier for the user
#' @param uuids_to_score list of article ids to be scored
#' @details
#' This function was built for an API to serve the HBL Commute App
#' @export

score_articles <- function(in_uid, uuids_to_score){
  #To do: read swipe information
  uuid_scores <- data.frame(uuid = character(), score = numeric())
  for(uuid in uuids_to_score){
    tags = get_article_by_uuid(uuid)$Tags[[1]]
    score = tag_score(in_uid, tags)
    uuid_scores = rbind(uuid_scores, data.frame(uuid = uuid, score = score))
  }
  #return(jsonlite::toJSON(uuid_scores))
  return(uuid_scores)
}
