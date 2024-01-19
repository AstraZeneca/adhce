#' Create ADHCE.
#'
#' Create ADHCE, the dataset to support analysis for hierarchical composite
#' endpoints. This dataset has a structure of one record per individual. It
#' stores information around the most severe event an individual undergoes
#' during a fixed follow-up study.
#'
#' @param dataset_tte The time to event dataset to pull the non-continuous
#'   endpoints from. ex: adtte.
#' @param dataset_bds The BDS dataset to pull the continuous endpoint from.
#'   ex: adlb, advs
#' @param dataset_adsl The ADSL dataset. Used to ensure that every individual
#'   has an endpoint in the final output dataset.ex: adsl.
#' @param endpoints Character vector of non-continuous endpoints in the TTE
#'   dataset. Endpoints should be listed in the desired hierarchical order.
#' @param cont_endpoint The variable in the BDS dataset that stores the value
#'   of the continuous endpoint. ex: AVAL, CHG
#' @param filter Filter condition for the BDS dataset with the continuous
#'  endpoint. This filter should ensure that only one record will be chosen per
#'  individual in the BDS dataset. ex: ANL01FL == "Y"
#' @param cutoff Fixed follow-up of the study in days. Should be an integer.
#' @param ady The variable to compare the cutoff date to. Used to ensure that
#'   only data that falls on or before the cutoff date is used to determine an
#'   individual's most severe outcome. ex: ADY
#' @param set_values_to Defines the variables to be set for the new parameter.
#'   PARAM and PARAMCD are required. The values must not exist in the TTE or BDS
#'   datasets that are passed into the function.
#' @param keep_source_vars The variables to keep in the ADHCE dataset in
#'   addition to those created by the function. These should be variables that
#'   exist across all three datasets that have been passed in. These variables
#'   will be put at the front of the ADHCE dataset in the order passed in to
#'   the parameter.
#'   ex: exprs(STUDYID, USUBJID, TRTP, TRTPN)
#' @returns The created ADHCE dataset.
create_hce <- function(dataset_tte = NULL,
                       dataset_bds = NULL,
                       dataset_adsl = NULL,
                       filter = NULL,
                       cutoff = NULL,
                       ady = ADY,
                       endpoints = NULL,
                       cont_endpoint = AVAL,
                       set_values_to,
                       keep_source_vars = exprs(STUDYID, USUBJID)) {
  # Checks input parameters ---------------------------
  # Check function parameters (variables)
  cont_endpoint <- assert_symbol(enexpr(cont_endpoint))
  ady <- assert_symbol(enexpr(ady))
  filter <- assert_filter_cond(enexpr(filter))
  assert_expr_list(keep_source_vars)
  assert_character_vector(endpoints)
  assert_integer_scalar(cutoff)
  assert_varval_list(set_values_to, required_elements = c("PARAMCD", "PARAM"))

  # Check that none of the input datasets are grouped
  df_list <- list(dataset_tte, dataset_bds, dataset_adsl)

  if (TRUE %in% lapply(df_list, is.grouped_df)) {
    abort("Please ungroup data frames before passing into the function")
  }

  # Check function parameters (datasets)
  assert_data_frame(dataset_tte, required_vars = append(
    keep_source_vars,
    exprs(PARAMCD, AVAL, CNSR)
  ))
  assert_data_frame(dataset_bds, required_vars = expr_c(
    keep_source_vars,
    cont_endpoint, ady
  ))
  assert_data_frame(dataset_adsl, required_vars = keep_source_vars)

  # Check for missing values in required variables
  # TTE
  na_check_tte <- dataset_tte %>%
    select(!!!keep_source_vars, PARAMCD, AVAL, CNSR) %>%
    filter(if_any(everything(), is.na))

  if (nrow(na_check_tte > 0)) {
    na_columns <- paste0(
      colnames(na_check_tte)[colSums(is.na(na_check_tte))
                             > 0],
      collapse = ", "
    )
    err_msg <- paste0(
      "Missing values found in following columns of TTE ",
      "dataset: ",
      sprintf(
        "Please check %s for `NA` values.",
        na_columns
      )
    )
    abort(err_msg)
  }

  # BDS
  na_check_bds <- dataset_bds %>%
    select(!!!keep_source_vars, !!cont_endpoint, !!ady) %>%
    filter(if_any(everything(), is.na))

  if (nrow(na_check_bds > 0)) {
    na_columns <- paste0(
      colnames(na_check_bds)[colSums(is.na(na_check_bds))
                             > 0],
      collapse = ", "
    )
    err_msg <- paste0(
      "Missing values found in following columns of BDS dataset: ",
      sprintf(
        "Please check %s for `NA` values.",
        na_columns
      )
    )
    abort(err_msg)
  }

  # ADSL
  na_check_adsl <- dataset_adsl %>%
    select(!!!keep_source_vars) %>%
    filter(if_any(everything(), is.na))

  if (nrow(na_check_adsl > 0)) {
    na_columns <- paste0(
      colnames(na_check_adsl)[colSums(is.na(na_check_adsl))
                              > 0],
      collapse = ", "
    )
    err_msg <- paste0(
      "Missing values found in following columns of ADSL dataset: ",
      sprintf(
        "Please check %s for `NA` values.",
        na_columns
      )
    )
    abort(err_msg)
  }

  # Check that provided noncontinuous endpoints exist in the TTE dataset
  if (any(!(endpoints %in% dataset_tte$PARAMCD))) {
    err_msg <- paste0(
      "The argument endpoints must contain values that already exist in the",
      "PARAMCD variable of the time-to-event dataset.\n",
      "Please check as it does not currently follow this convention"
    )
    abort(err_msg)
  }

  # Check if all subjects in ADSL have records in either BDS or TTE dataset
  all_subjects <- dataset_tte %>%
    bind_rows(dataset_bds) %>%
    distinct(USUBJID)

  subject_check <-
    dataset_adsl %>%
    select(USUBJID) %>%
    filter(!(USUBJID %in% all_subjects$USUBJID))

  if (nrow(subject_check > 0)) {
    subjects <- paste0(subject_check$USUBJID, collapse = ", ")
    err_msg <- paste0(
      "The following subjects have records in ADSL but do not have ",
      "corresponding records in time to event or BDS dataset: \n",
      sprintf(
        "%s. \n",
        subjects
      ),
      "Please check"
    )
    abort(err_msg)
  }

  # Check that the parameter doesn't exist in either the TTE or the BDS dataset.
  if (!is.null(set_values_to$PARAMCD) && !is.null(dataset_tte)) {
    assert_param_does_not_exist(dataset_tte, set_values_to$PARAMCD)
  }

  if (!is.null(set_values_to$PARAMCD) && !is.null(dataset_bds)) {
    assert_param_does_not_exist(dataset_bds, set_values_to$PARAMCD)
  }

  # Check that PARAM and PARAMCD are specified

  # Time-to-event outcomes ---------------------------
  # Create hierarchy vector based on endpoint list.
  # The order that the endpoints are passed in will determine the order in the
  # hierarchy.
  hierarchy <- seq(length(endpoints))
  
  # Select only uncensored events 
  tte_cnsr <- dataset_tte %>%
    filter(CNSR == 0)
  
  # Filter out any records occurring after fixed follow-up day
  tte_cutoff <- tte_cnsr %>%
    filter(AVAL <= cutoff)

  # Select only specified endpoints from time-to event dataset
  tte_params <- filter_exist(
    dataset = tte_cutoff,
    dataset_add = tte_cutoff,
    by_vars = exprs(USUBJID, PARAMCD),
    filter_add = PARAMCD %in% endpoints
  )

  order <- data.frame(ENDPOINT = endpoints, PARAMN = hierarchy)

  if (!("PARAMN" %in% colnames(tte_params))) {
    # If PARAMN doesn't exist, then create it.
    tte_params <-
      tte_params %>%
      left_join(order, by = c("PARAMCD" = "ENDPOINT"))
    # If PARAMN does exist, then overwrite it.
  } else if ("PARAMN" %in% colnames(tte_params)) {
    tte_params <-
      tte_params %>%
      select(-PARAMN) %>%
      left_join(order, by = c("PARAMCD" = "ENDPOINT"))
  }

  max_paramn_non_cont <- max(tte_params$PARAMN)

  # Continuous endpoint ----
  # Filter any records occurring after fixed follow-up day
  bds_cutoff <- dataset_bds %>%
    filter(!!ady <= cutoff)

  # Apply filter to BDS dataset if one has been passed in
  bds_cutoff_extreme <-
    bds_cutoff %>%
    filter(!!filter)

  # Combined TTE and BDS data ---------------------------
  endpoints <-
    bds_cutoff_extreme %>%
    mutate(AVAL = !!cont_endpoint,
           PARAMN = max_paramn_non_cont + 1) %>%
    bind_rows(tte_params)

  # Filter hce record with highest severity based on PARAMN
  severe <- filter_extreme(
    dataset = endpoints,
    by_vars = exprs(USUBJID),
    order = exprs(PARAMN),
    mode = "first"
  )

  dataset_adsl <-
    dataset_adsl %>%
    select(USUBJID)

  merged <- dplyr::left_join(dataset_adsl, severe, by=c("USUBJID"))

  # Output message for individuals who don't have a severe event before cutoff
  merged_check <- merged %>%
    filter(is.na(AVAL))

  if (nrow(merged_check > 0)) {
    bad_val_subjects <- paste0(merged_check$USUBJID, collapse = ", ")
    msg <- paste0(
      "The following individuals don't have a record prior to the provided ",
      "cutoff, hence they won't be present in the output dataset: \n",
      sprintf(
        "%s. \n",
        bad_val_subjects
      )
    )
    inform(msg)
  }

  merged <- merged %>%
    filter(!is.na(AVAL))

  # Derive additional variables (AVAL, AVALCAT1, AVALCA1N, PADY) ---
  merged1 <- merged %>%
    mutate(
      PADY = cutoff,
      AVAL_ = AVAL
    )

  max <-
    merged1 %>%
    filter(PARAMN == max_paramn_non_cont + 1) %>%
    mutate(AVAL_abs = abs(AVAL_)) %>%
    arrange(desc(AVAL_abs)) %>%
    select(AVAL_abs) %>%
    slice(1)

  factor <- max(c(max$AVAL_abs, cutoff))

  adhce <-
    merged1 %>%
    mutate(
      AVALCAT1 = PARAM,
      AVALCA1N = factor * PARAMN,
      AVAL = AVAL_ + AVALCA1N,
    ) %>%
    process_set_values_to(set_values_to) %>%
    select(!!!keep_source_vars, PARAM, PARAMCD, AVAL, AVALCAT1, AVALCA1N, PADY)

  # Output ADHCE dataset
  return(adhce)
}
