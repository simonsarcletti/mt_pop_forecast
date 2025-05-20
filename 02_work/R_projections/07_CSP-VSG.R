############################################################################## #
# Filename
#    01_CSp-VSG.R
#
# Description
#   Projection with CSP-VSG method such as in Wilson 2014
#
# Project   OEROK_Evaluierung und Dekomposition
# Author(s) Simon Sarcletti
# Date      2025-02-25
#
# Copyright JOANNEUM RESEARCH, 2025
############################################################################## #

load(file.path(wd_res, "final_csp_test_pred_2022-2024.RData"))
load(file.path(wd_res, "final_vsg_test_pred_2022-2024.RData"))

# create csp-vsg als average ---------------------------------------------------
csp_vsg_test <- csp_text_export %>%
  left_join(select(vsg_test_for_export, -population), 
            by = join_by(municipality_code, sex, age_group, year)) %>%
  ungroup() %>%
  group_by(municipality_code, sex, age_group, year) %>%
  mutate(PRED_csp_vsg = sum(PRED_csp_final, PRED_vsg, na.rm = TRUE) /2) %>%
  select(-c(PRED_csp_final, PRED_vsg))


# export
save(csp_vsg_test, 
     file = file.path(wd_res, "final_csp-csg_test_2022-2024.RData"))


# csp-vsg prediction -----------------------------------------------------------
load(file.path(wd_res, "25-35_CSP_prediction.RData"))
load(file.path(wd_res, "25-35_VSG_prediction.RData"))

csp_vsg_pred <- csp_pred_export %>%
  left_join(select(vsg_pred_for_export, -population), 
            by = join_by(municipality_code, sex, age_group, year)) %>%
  ungroup() %>%
  group_by(municipality_code, sex, age_group, year) %>%
  mutate(PRED_csp_vsg = sum(PRED_csp_final, PRED_vsg, na.rm = TRUE) /2) %>%
  select(-c(PRED_csp_final, PRED_vsg))


save(csp_vsg_pred, 
     file = file.path(wd_res, "25-35_CSP-VSG_prediction.RData"))










