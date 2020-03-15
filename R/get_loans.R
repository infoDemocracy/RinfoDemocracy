#' Get loans
#'
#' This function gets live data about loans from the Electoral Commission donations database.
#'
#' @return A data.frame. Variable names are cleaned.
#'
#' @examples
#' get_loans()
#' @name get_loans
#' @importFrom magrittr %>%
NULL

#' @rdname get_loans
#' @export

get_loans <- function() {
  
  ec_loans_raw <- httr::GET('http://search.electoralcommission.org.uk/api/csv/loans',
                      query = list(sort = "StartDate",
                                   order = "desc")) %>% 
    httr::content(type = "text/csv",
            encoding = 'UTF-8',
            col_types = readr::cols(
              ECRef = readr::col_character(),
              RegulatedEntityName = readr::col_character(),
              RegulatedEntityType = readr::col_character(),
              Value = readr::col_character(),
              LoanStatus = readr::col_character(),
              StartDate = readr::col_date(format = '%d/%m/%Y'),
              AccountingUnitName = readr::col_character(),
              LoanParticipantName = readr::col_character(),
              LoanParticipantType = readr::col_character(),
              CompanyRegistrationNumber = readr::col_character(),
              Postcode = readr::col_character(),
              LoanType = readr::col_character(),
              RateOfInterestDescription = readr::col_character(),
              AmountRepaid = readr::col_character(),
              AmountConverted = readr::col_character(),
              AmountOutstanding = readr::col_character(),
              EndDate = readr::col_date(format = '%d/%m/%Y'),
              DateRepaid = readr::col_date(format = '%d/%m/%Y'),
              DateEcLastNotified = readr::col_character(),
              IsReportedPrePoll = readr::col_logical(),
              ReportingPeriodName = readr::col_character(),
              IsAggregation = readr::col_logical(),
              RegulatedEntityId = readr::col_double(),
              AccountingUnitId = readr::col_double(),
              LoanParticipantId = readr::col_double(),
              CampaigningName = readr::col_character(),
              RegisterName = readr::col_character(),
              IsIrishSource = readr::col_logical()
            ))
  
  readr::stop_for_problems(ec_loans_raw)

  ec_loans_raw <- ec_loans_raw %>% 
    dplyr::rename(
      dntn_ec_ref = ECRef,
      dntn_regulated_entity_name = RegulatedEntityName,
      dntn_regulated_entity_type = RegulatedEntityType,
      dntn_value = Value,
      dntn_loan_status = LoanStatus,
      dntn_start_date = StartDate,
      dntn_accounting_unit_name = AccountingUnitName,
      dntn_loan_participnt_name = LoanParticipantName,
      dntn_loan_participnt_type = LoanParticipantType,
      dntn_company_registration_number = CompanyRegistrationNumber,
      dntn_postcode = Postcode,
      dntn_loan_type = LoanType,
      dntn_rate_of_interest_description = RateOfInterestDescription,
      dntn_amount_repaid = AmountRepaid,
      dntn_amount_converted = AmountConverted,
      dntn_amount_outstanding = AmountOutstanding,
      dntn_end_date = EndDate,
      dntn_date_repaid = DateRepaid,
      dntn_date_ec_last_notified = DateEcLastNotified,
      dntn_is_reported_pre_poll = IsReportedPrePoll,
      dntn_reporting_period_name = ReportingPeriodName,
      dntn_is_aggregation = IsAggregation,
      dntn_regulated_entity_id = RegulatedEntityId,
      dntn_accounting_unit_id = AccountingUnitId,
      dntn_loan_participant_id = LoanParticipantId,
      dntn_campaigning_name = CampaigningName,
      dntn_register_name = RegisterName,
      dntn_is_irish_source = IsIrishSource) %>% 
    dplyr::mutate(
      dntn_value = as.numeric(stringr::str_replace_all(dntn_value, '[\\£|,]', '')),
      dntn_amount_repaid = as.numeric(stringr::str_replace_all(dntn_amount_repaid, '[\\£|,]', '')),
      dntn_amount_converted = as.numeric(stringr::str_replace_all(dntn_amount_converted, '[\\£|,]', '')),
      dntn_amount_outstanding = as.numeric(stringr::str_replace_all(dntn_amount_outstanding, '[\\£|,]', '')),
      download_date = lubridate::today(),
      type = "Loan")
  
  ec_loans_raw
}
