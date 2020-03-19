#' @title Open Data
#'
#' @description tba.
#'
#' @details tba.
#'
#' @param data tba.
#'
#' @return tba.
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#'
#' @export

create_open_data <- function(smr,
                             trends,
                             type = c("smr", "crude"),
                             split = NULL,
                             time = NULL){

  if (type == "smr"){

    smr %<>%
      dplyr::filter(period == 3) %>%
      dplyr::rename(ObservedNumberOfDeaths  = deaths,
                    PredictedNumberOfDeaths = pred,
                    NumberOfPatients        = pats,
                    SMR = smr,
                    CrudeRate = crd_rate,
                    LocationCode = location,
                    NHSBoard = hb) %>%
      dplyr::mutate(LocationCode = case_when(LocationCode == "Scot" ~ "S92000003",
                                             TRUE ~ LocationCode),
                    NHSBoard     = case_when(NHSBoard == "Scotland" ~ "S92000003",
                                             TRUE ~ NHSBoard),
                    ObservedNumberOfDeathsQF = case_when(LocationCode ==
                                                           "S92000003" ~ "d",
                                                         TRUE ~ ""),
                    PredictedNumberOfDeathsQF = case_when(LocationCode ==
                                                           "S92000003" ~ "d",
                                                         TRUE ~ ""),
                    NumberOfPatientsQF = case_when(LocationCode ==
                                                           "S92000003" ~ "d",
                                                         TRUE ~ "")) %>%
      select("ObservedNumberOfDeaths",	"ObservedNumberOfDeathsQF",
             "PredictedNumberOfDeaths",	"PredictedNumberOfDeathsQF",
             "NumberOfPatients",	"NumberOfPatientsQF",	"SMR",	"CrudeRate",
             "LocationCode",	"NHSBoard")

  }

  if(type == "crude"){

    trends %<>%
      filter(sub_grp == split & tolower(time_period) == time) %>%
      dplyr::rename(NumberOfDeaths   = deaths,
                    NumberOfPatients = pats,
                    CrudeRate = crd_rate,
                    LocationCode = location,
                    NHSBoard = hb) %>%
      dplyr::mutate(LocationCode = case_when(LocationCode == "Scot" ~ "S92000003",
                                             TRUE ~ LocationCode),
                    NHSBoard     = case_when(NHSBoard == "Scotland" ~ "S92000003",
                                             TRUE ~ NHSBoard),
                    NumberOfDeathsQF = case_when(LocationCode ==
                                                           "S92000003" ~ "d",
                                                         TRUE ~ ""),
                    NumberOfPatientsQF = case_when(LocationCode ==
                                                     "S92000003" ~ "d",
                                                   TRUE ~ "")) %>%
      dplyr::select("NHSBoard",	{{time}}, "LocationCode",
                    "NumberOfDeaths",	"NumberOfDeathsQF",
                    "NumberOfPatients",	"NumberOfPatientsQF",	"CrudeRate")



  }


}
