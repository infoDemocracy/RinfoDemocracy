#' Get donations
#'
#' This function gets donations from the electoral commission website.
#'
#' @return A data.frame.
#'
#' @examples
#' get_donations()
#' @name get_donations
#' @importFrom magrittr %>%
NULL

#' @rdname get_donations
#' @export

get_donations <- function() {
  
  ec_donations_raw <- httr::GET('http://search.electoralcommission.org.uk/api/csv/Donations',
                          query = list(sort = "AcceptedDate",
                                       order = "desc",
                                       prePoll = "true",
                                       postPoll = "true")) %>% 
    httr::content(type = "text/csv",
            encoding = 'UTF-8',
            col_types = cols(
              ECRef = col_character(),
              RegulatedEntityName = col_character(),
              RegulatedEntityType = col_character(),
              Value = col_character(),
              AcceptedDate = col_date(format = '%d/%m/%Y'),
              AccountingUnitName = col_character(),
              DonorName = col_character(),
              AccountingUnitsAsCentralParty = col_logical(),
              IsSponsorship = col_logical(),
              DonorStatus = col_character(),
              RegulatedDoneeType = col_character(),
              CompanyRegistrationNumber = col_character(),
              Postcode = col_character(),
              DonationType = col_character(),
              NatureOfDonation = col_character(),
              PurposeOfVisit = col_character(),
              DonationAction = col_character(),
              ReceivedDate = col_date(format = '%d/%m/%Y'),
              ReportedDate = col_date(format = '%d/%m/%Y'),
              IsReportedPrePoll = col_logical(),
              ReportingPeriodName = col_character(),
              IsBequest = col_logical(),
              IsAggregation = col_logical(),
              RegulatedEntityId = col_double(),
              AccountingUnitId = col_double(),
              DonorId = col_double(),
              CampaigningName = col_character(),
              RegisterName = col_character(),
              IsIrishSource = col_logical()
            ),
            na = c("", "NA", "N/A"))
  
  readr::stop_for_problems(ec_donations_raw)
  
  ec_donations_raw <- 
    ec_donations_raw %>% 
    dplyr::rename(dntn_ec_ref = ECRef,
           dntn_regulated_entity_name = RegulatedEntityName,
           dntn_regulated_entity_type = RegulatedEntityType,
           dntn_value = Value,
           dntn_accepted_date = AcceptedDate,
           dntn_accounting_unit_name = AccountingUnitName,
           dntn_donor_name = DonorName,
           dntn_accounting_unit_as_central_party = AccountingUnitsAsCentralParty,
           dntn_is_sponsorship = IsSponsorship,
           dntn_donor_status = DonorStatus,
           dntn_regulated_donee_type = RegulatedDoneeType,
           dntn_company_registration_number = CompanyRegistrationNumber,
           dntn_postcode = Postcode,
           dntn_donation_type = DonationType,
           dntn_nature_of_donation = NatureOfDonation,
           dntn_purpose_of_visit = PurposeOfVisit,
           dntn_donation_action = DonationAction,
           dntn_received_date = ReceivedDate,
           dntn_reported_date = ReportedDate,
           dntn_is_reported_pre_poll = IsReportedPrePoll,
           dntn_reporting_period_name = ReportingPeriodName,
           dntn_is_bequest = IsBequest,
           dntn_is_aggregation = IsAggregation,
           dntn_regulated_entity_id = RegulatedEntityId,
           dntn_accounting_unit_id = AccountingUnitId,
           dntn_donor_id = DonorId,
           dntn_campaigning_name = CampaigningName,
           dntn_register_name = RegisterName,
           dntn_is_irish_source = IsIrishSource) %>%
    dplyr::mutate(dntn_value = as.numeric(stringr::str_replace_all(dntn_value, '[\\£|,]', '')),
           download_date = lubridate::today(),
           type = "Donation")
  
  ec_donations_raw
}
