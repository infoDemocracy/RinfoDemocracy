#' Get spending
#'
#' This function gets live data about spending from the Electoral Commission spending database.
#'
#' @return A data.frame. Variable names are cleaned.
#'
#' @examples
#' get_spending()
#' @name get_spending
#' @importFrom magrittr %>%
NULL

#' @rdname get_spending
#' @export

get_spending <- function() {
  
  ec_spending_raw <- httr::GET('http://search.electoralcommission.org.uk/api/csv/Spending',
                               query = list(sort = "DateIncurred",
                                            order = "desc",
                                            prePoll = "true",
                                            postPoll = "true")) %>% 
    httr::content(
      type = "text/csv",
      encoding = 'UTF-8',
      col_types = readr::cols(
        ECRef = readr::col_character(),
        RegulatedEntityName = readr::col_character(),
        ReportingPeriodName = readr::col_character(),
        RegulatedEntityType = readr::col_character(),
        TotalExpenditure = readr::col_character(),
        DateIncurred = readr::col_date(format = '%d/%m/%Y'),
        ExpenseCategoryName = readr::col_character(),
        SupplierName = readr::col_character(),
        FullAddress = readr::col_character(),
        AmountInEngland = readr::col_character(),
        AmountInScotland = readr::col_character(),
        AmountInWales = readr::col_character(),
        AmountInNorthernIreland = readr::col_character(),
        DateOfClaimForPayment = readr::col_date(format = '%d/%m/%Y'),
        DatePaid = readr::col_date(format = '%d/%m/%Y'),
        RedactedSupportingInvoiceId = readr::col_double(),
        RegulatedEntityId = readr::col_double(),
        SupplierId = readr::col_double(),
        JointCampaignName = readr::col_character(),
        UnregisteredCampaignerName = readr::col_logical(),
        CampaigningName = readr::col_character(),
        IsOutsideSection75 = readr::col_logical()
      ))
  
  readr::stop_for_problems(ec_spending_raw)
  
  ec_spending_raw <- ec_spending_raw %>% 
    dplyr::rename(
      ec_ref = ECRef,
      regulated_entity_name = RegulatedEntityName,
      reporting_period_name = ReportingPeriodName,
      regulated_entity_type = RegulatedEntityType,
      total_expenditure = TotalExpenditure,
      date_incurred = DateIncurred,
      expense_category_name = ExpenseCategoryName,
      supplier_name = SupplierName,
      full_address = FullAddress,
      amount_in_england = AmountInEngland,
      amount_in_scotland = AmountInScotland,
      amount_in_wales = AmountInWales,
      amount_in_northern_ireland = AmountInNorthernIreland,
      date_of_claim_for_payment = DateOfClaimForPayment,
      date_paid = DatePaid,
      redacted_supporting_invoice_id = RedactedSupportingInvoiceId,
      regulated_entity_id = RegulatedEntityId,
      supplier_id = SupplierId,
      joint_campaign_name = JointCampaignName,
      unregistered_campaigner_name = UnregisteredCampaignerName,
      campaigning_name = CampaigningName,
      is_outside_section_75 = IsOutsideSection75
    ) %>% 
    dplyr::mutate(
      total_expenditure = as.numeric(stringr::str_replace_all(total_expenditure, '[\\£|,]', '')),
      amount_in_england = as.numeric(stringr::str_replace_all(amount_in_england, '[\\£|,]', '')),
      amount_in_scotland = as.numeric(stringr::str_replace_all(amount_in_scotland, '[\\£|,]', '')),
      amount_in_wales = as.numeric(stringr::str_replace_all(amount_in_wales, '[\\£|,]', '')),
      amount_in_northern_ireland = as.numeric(stringr::str_replace_all(amount_in_northern_ireland, '[\\£|,]', ''))
    )
  
  ec_spending_raw
}
