:- module(validation_suite, [run_dynamic_suite/0]).
:- use_module(library(prolog_stack)).
:- use_module(library(time)).
:- use_module(scenario_manager).
:- use_module(data_validation).
:- use_module(report_generator).

:- chdir('../prolog').

:- dynamic test_passed/1, test_failed/3, test_case/4.

% --- Test Case Definitions ---
test_case('testsets/1945_truman_san_francisco_conference_proceeding.pl', '0', '1945_TRUMAN_SAN_FRANCISCO_CONFERENCE_PROCEEDING', 1).
test_case('testsets/1948_truman_national_health_insurance.pl', '1948_truman_national_health_insurance', '1948_TRUMAN_NATIONAL_HEALTH_INSURANCE', 2).
test_case('testsets/1949_truman_employment_act_1946_enforcement.pl', '1949_truman_employment_act_1946_enforcement', '1949_TRUMAN_EMPLOYMENT_ACT_1946_ENFORCEMENT', 3).
test_case('testsets/1951_truman_nato_alliance_binding.pl', '1951_truman_nato_alliance_binding', '1951_TRUMAN_NATO_ALLIANCE_BINDING', 4).
test_case('testsets/1959_eisenhower_defense_budget_systematic_planning.pl', '1959_eisenhower_defense_budget_systematic_planning', '1959_EISENHOWER_DEFENSE_BUDGET_SYSTEMATIC_PLANNING', 5).
test_case('testsets/1959_eisenhower_dod_reorganization_authority.pl', '1959_eisenhower_dod_reorganization_authority', '1959_EISENHOWER_DOD_REORGANIZATION_AUTHORITY', 6).
test_case('testsets/1960_eisenhower_nuclear_test_ban_negotiations.pl', '1960_eisenhower_nuclear_test_ban_negotiations', '1960_EISENHOWER_NUCLEAR_TEST_BAN_NEGOTIATIONS', 7).
test_case('testsets/1961_kennedy_fiscal_restraint_budget_discipline.pl', '1961_kennedy_fiscal_restraint_budget_discipline', '1961_KENNEDY_FISCAL_RESTRAINT_BUDGET_DISCIPLINE', 8).
test_case('testsets/1962_kennedy_permanent_unemployment_compensation_strengthening.pl', '1962_kennedy_permanent_unemployment_compensation_strengthening', '1962_KENNEDY_PERMANENT_UNEMPLOYMENT_COMPENSATION_STRENGTHENING', 9).
test_case('testsets/1963_kennedy_corporate_tax_rate_reduction.pl', '1963_kennedy_corporate_tax_rate_reduction', '1963_KENNEDY_CORPORATE_TAX_RATE_REDUCTION', 10).
test_case('testsets/1965_johnson_voting_rights_act_federal_registrars.pl', '1965_johnson_voting_rights_act_federal_registrars', '1965_JOHNSON_VOTING_RIGHTS_ACT_FEDERAL_REGISTRARS', 11).
test_case('testsets/1966_johnson_great_society_health_education_expansion.pl', '1966_johnson_great_society_health_education_expansion', '1966_JOHNSON_GREAT_SOCIETY_HEALTH_EDUCATION_EXPANSION', 12).
test_case('testsets/1968_johnson_nuclear_nonproliferation_treaty.pl', '1968_johnson_nuclear_nonproliferation_treaty', '1968_JOHNSON_NUCLEAR_NONPROLIFERATION_TREATY', 13).
test_case('testsets/1980_carter_atlantic_alliance_collective_security.pl', '1980_carter_atlantic_alliance_collective_security', '1980_CARTER_ATLANTIC_ALLIANCE_COLLECTIVE_SECURITY', 14).
test_case('testsets/1983_reagan_federal_spending_control_deficit_reduction.pl', '1983_reagan_federal_spending_control_deficit_reduction', '1983_REAGAN_FEDERAL_SPENDING_CONTROL_DEFICIT_REDUCTION', 15).
test_case('testsets/1985_reagan_poverty_line_tax_exemption.pl', '1985_reagan_poverty_line_tax_exemption', '1985_REAGAN_POVERTY_LINE_TAX_EXEMPTION', 16).
test_case('testsets/1986_reagan_balanced_budget_amendment.pl', '1986_reagan_balanced_budget_amendment', '1986_REAGAN_BALANCED_BUDGET_AMENDMENT', 17).
test_case('testsets/1986_reagan_line_item_veto.pl', '1986_reagan_line_item_veto', '1986_REAGAN_LINE_ITEM_VETO', 18).
test_case('testsets/1988_reagan_deficit_reduction_agreement.pl', '1988_reagan_deficit_reduction_agreement', '1988_REAGAN_DEFICIT_REDUCTION_AGREEMENT', 19).
test_case('testsets/1989_bush_gramm_rudman_hollings_deficit_targets.pl', '1989_bush_gramm_rudman_hollings_deficit_targets', '1989_BUSH_GRAMM_RUDMAN_HOLLINGS_DEFICIT_TARGETS', 20).
test_case('testsets/1990_bush_capital_gains_tax_reduction.pl', '1990_bush_capital_gains_tax_reduction', '1990_BUSH_CAPITAL_GAINS_TAX_REDUCTION', 21).
test_case('testsets/1990_bush_head_start_expansion.pl', '1990_bush_head_start_expansion', '1990_BUSH_HEAD_START_EXPANSION', 22).
test_case('testsets/1992_bush_defense_budget_reduction_30_percent.pl', '1992_bush_defense_budget_reduction_30_percent', '1992_BUSH_DEFENSE_BUDGET_REDUCTION_30_PERCENT', 23).
test_case('testsets/1994_clinton_family_medical_leave_act.pl', '1994_clinton_family_medical_leave_act', '1994_CLINTON_FAMILY_MEDICAL_LEAVE_ACT', 24).
test_case('testsets/2000_clinton_deficit_reduction_act_debt_paydown.pl', '2000_clinton_deficit_reduction_act_debt_paydown', '2000_CLINTON_DEFICIT_REDUCTION_ACT_DEBT_PAYDOWN', 25).
test_case('testsets/2001_bush_Emergency_appropriations_40billion_mobilization.pl', '2001_bush_Emergency_appropriations_40billion_mobilization', '2001_BUSH_EMERGENCY_APPROPRIATIONS_40BILLION_MOBILIZATION', 26).
test_case('testsets/2003_bush_clear_skies_emissions_mandate.pl', '2003_bush_clear_skies_emissions_mandate', '2003_BUSH_CLEAR_SKIES_EMISSIONS_MANDATE', 27).
test_case('testsets/sotu_1945_truman_unconditional_surrender_doctrine.pl', 'sotu_1945_truman_unconditional_surrender_doctrine', 'SOTU_1945_TRUMAN_UNCONDITIONAL_SURRENDER_DOCTRINE', 28).
test_case('testsets/sotu_1945_truman_war_crimes_prosecution_mandate.pl', 'sotu_1945_truman_war_crimes_prosecution_mandate', 'SOTU_1945_TRUMAN_WAR_CRIMES_PROSECUTION_MANDATE', 29).
test_case('testsets/sotu_1946_truman_comprehensive_executive_budget.pl', 'sotu_1946_truman_comprehensive_executive_budget', 'SOTU_1946_TRUMAN_COMPREHENSIVE_EXECUTIVE_BUDGET', 30).
test_case('testsets/sotu_1946_truman_full_employment_bill.pl', 'sotu_1946_truman_full_employment_bill', 'SOTU_1946_TRUMAN_FULL_EMPLOYMENT_BILL', 31).
test_case('testsets/sotu_1946_truman_war_liquidation_peacetime_reconversion_program.pl', 'sotu_1946_truman_war_liquidation_peacetime_reconversion_program', 'SOTU_1946_TRUMAN_WAR_LIQUIDATION_PEACETIME_RECONVERSION_PROGRAM', 32).
test_case('testsets/sotu_1947_truman_labor_management_procedural_reform.pl', 'sotu_1947_truman_labor_management_procedural_reform', 'SOTU_1947_TRUMAN_LABOR_MANAGEMENT_PROCEDURAL_REFORM', 33).
test_case('testsets/sotu_1947_truman_price_stability_tripartite_responsibility.pl', 'sotu_1947_truman_price_stability_tripartite_responsibility', 'SOTU_1947_TRUMAN_PRICE_STABILITY_TRIPARTITE_RESPONSIBILITY', 34).
test_case('testsets/sotu_1947_truman_wartime_emergency_controls_termination.pl', 'sotu_1947_truman_wartime_emergency_controls_termination', 'SOTU_1947_TRUMAN_WARTIME_EMERGENCY_CONTROLS_TERMINATION', 35).
test_case('testsets/sotu_1948_truman_federal_education_aid.pl', '0', 'SOTU_1948_TRUMAN_FEDERAL_EDUCATION_AID', 36).
test_case('testsets/sotu_1948_truman_social_security_expansion.pl', 'sotu_1948_truman_social_security_expansion', 'SOTU_1948_TRUMAN_SOCIAL_SECURITY_EXPANSION', 37).
test_case('testsets/sotu_1949_truman_fiscal_surplus_mandate.pl', 'sotu_1949_truman_fiscal_surplus_mandate', 'SOTU_1949_TRUMAN_FISCAL_SURPLUS_MANDATE', 38).
test_case('testsets/sotu_1949_truman_inflation_control_authorities.pl', '0', 'SOTU_1949_TRUMAN_INFLATION_CONTROL_AUTHORITIES', 39).
test_case('testsets/sotu_1950_truman_government_employment_stabilization.pl', '0', 'SOTU_1950_TRUMAN_GOVERNMENT_EMPLOYMENT_STABILIZATION', 40).
test_case('testsets/sotu_1950_truman_reciprocal_trade_agreements.pl', 'sotu_1950_truman_reciprocal_trade_agreements', 'SOTU_1950_TRUMAN_RECIPROCAL_TRADE_AGREEMENTS', 41).
test_case('testsets/sotu_1950_truman_selective_service_maintenance.pl', 'sotu_1950_truman_selective_service_maintenance', 'SOTU_1950_TRUMAN_SELECTIVE_SERVICE_MAINTENANCE', 42).
test_case('testsets/sotu_1951_truman_korean_war_collective_defense.pl', 'sotu_1951_truman_korean_war_collective_defense', 'SOTU_1951_TRUMAN_KOREAN_WAR_COLLECTIVE_DEFENSE', 43).
test_case('testsets/sotu_1951_truman_marshall_plan_economic_assistance.pl', 'sotu_1951_truman_marshall_plan_economic_assistance', 'SOTU_1951_TRUMAN_MARSHALL_PLAN_ECONOMIC_ASSISTANCE', 44).
test_case('testsets/sotu_1953_eisenhower_bipartisan_foreign_policy_coordination.pl', 'sotu_1953_eisenhower_bipartisan_foreign_policy_coordination', 'SOTU_1953_EISENHOWER_BIPARTISAN_FOREIGN_POLICY_COORDINATION', 45).
test_case('testsets/sotu_1953_eisenhower_conditional_foreign_aid_reciprocity.pl', 'sotu_1953_eisenhower_conditional_foreign_aid_reciprocity', 'SOTU_1953_EISENHOWER_CONDITIONAL_FOREIGN_AID_RECIPROCITY', 46).
test_case('testsets/sotu_1953_eisenhower_reciprocal_trade_agreements_extension.pl', 'sotu_1953_eisenhower_reciprocal_trade_agreements_extension', 'SOTU_1953_EISENHOWER_RECIPROCAL_TRADE_AGREEMENTS_EXTENSION', 47).
test_case('testsets/sotu_1954_eisenhower_consolidated_foreign_assistance_authority.pl', 'sotu_1954_eisenhower_consolidated_foreign_assistance_authority', 'SOTU_1954_EISENHOWER_CONSOLIDATED_FOREIGN_ASSISTANCE_AUTHORITY', 48).
test_case('testsets/sotu_1954_eisenhower_european_defense_community.pl', 'sotu_1954_eisenhower_european_defense_community', 'SOTU_1954_EISENHOWER_EUROPEAN_DEFENSE_COMMUNITY', 49).
test_case('testsets/sotu_1954_eisenhower_mutual_security_pact_korea.pl', '0', 'SOTU_1954_EISENHOWER_MUTUAL_SECURITY_PACT_KOREA', 50).
test_case('testsets/sotu_1955_eisenhower_information_program_foreign_advocacy.pl', 'sotu_1955_eisenhower_information_program_foreign_advocacy', 'SOTU_1955_EISENHOWER_INFORMATION_PROGRAM_FOREIGN_ADVOCACY', 51).
test_case('testsets/sotu_1955_eisenhower_nato_collective_defense.pl', '0', 'SOTU_1955_EISENHOWER_NATO_COLLECTIVE_DEFENSE', 52).
test_case('testsets/sotu_1955_eisenhower_un_arms_limitation_framework.pl', 'sotu_1955_eisenhower_un_arms_limitation_framework', 'SOTU_1955_EISENHOWER_UN_ARMS_LIMITATION_FRAMEWORK', 53).
test_case('testsets/sotu_1956_eisenhower_collective_security_system.pl', 'sotu_1956_eisenhower_collective_security_system', 'SOTU_1956_EISENHOWER_COLLECTIVE_SECURITY_SYSTEM', 54).
test_case('testsets/sotu_1956_eisenhower_federal_payroll_reduction.pl', 'sotu_1956_eisenhower_federal_payroll_reduction', 'SOTU_1956_EISENHOWER_FEDERAL_PAYROLL_REDUCTION', 55).
test_case('testsets/sotu_1956_eisenhower_social_security_unemployment_expansion.pl', 'sotu_1956_eisenhower_social_security_unemployment_expansion', 'SOTU_1956_EISENHOWER_SOCIAL_SECURITY_UNEMPLOYMENT_EXPANSION', 56).
test_case('testsets/sotu_1957_eisenhower_business_pricing_restraint.pl', 'sotu_1957_eisenhower_business_pricing_restraint', 'SOTU_1957_EISENHOWER_BUSINESS_PRICING_RESTRAINT', 57).
test_case('testsets/sotu_1957_eisenhower_federal_fiscal_restraint.pl', 'sotu_1957_eisenhower_federal_fiscal_restraint', 'SOTU_1957_EISENHOWER_FEDERAL_FISCAL_RESTRAINT', 58).
test_case('testsets/sotu_1957_eisenhower_wage_productivity_alignment.pl', '0', 'SOTU_1957_EISENHOWER_WAGE_PRODUCTIVITY_ALIGNMENT', 59).
test_case('testsets/sotu_1958_eisenhower_ballistic_missile_acceleration_program.pl', 'sotu_1958_eisenhower_ballistic_missile_acceleration_program', 'SOTU_1958_EISENHOWER_BALLISTIC_MISSILE_ACCELERATION_PROGRAM', 60).
test_case('testsets/sotu_1958_eisenhower_nato_alliance_cohesion.pl', 'sotu_1958_eisenhower_nato_alliance_cohesion', 'SOTU_1958_EISENHOWER_NATO_ALLIANCE_COHESION', 61).
test_case('testsets/sotu_1958_eisenhower_strategic_air_command_deterrent.pl', '0', 'SOTU_1958_EISENHOWER_STRATEGIC_AIR_COMMAND_DETERRENT', 62).
test_case('testsets/sotu_1959_eisenhower_treaty_self_enforcing_mechanisms.pl', 'sotu_1959_eisenhower_treaty_self_enforcing_mechanisms', 'SOTU_1959_EISENHOWER_TREATY_SELF_ENFORCING_MECHANISMS', 63).
test_case('testsets/sotu_1960_eisenhower_antarctica_treaty_inspection.pl', 'sotu_1960_eisenhower_antarctica_treaty_inspection', 'SOTU_1960_EISENHOWER_ANTARCTICA_TREATY_INSPECTION', 64).
test_case('testsets/sotu_1960_eisenhower_foreign_aid_cooperation_framework.pl', 'sotu_1960_eisenhower_foreign_aid_cooperation_framework', 'SOTU_1960_EISENHOWER_FOREIGN_AID_COOPERATION_FRAMEWORK', 65).
test_case('testsets/sotu_1961_kennedy_manpower_development_training.pl', 'sotu_1961_kennedy_manpower_development_training', 'SOTU_1961_KENNEDY_MANPOWER_DEVELOPMENT_TRAINING', 66).
test_case('testsets/sotu_1961_kennedy_wage_price_restraint_labor_management.pl', 'sotu_1961_kennedy_wage_price_restraint_labor_management', 'SOTU_1961_KENNEDY_WAGE_PRICE_RESTRAINT_LABOR_MANAGEMENT', 67).
test_case('testsets/sotu_1962_kennedy_manpower_training_development_act.pl', 'sotu_1962_kennedy_manpower_training_development_act', 'SOTU_1962_KENNEDY_MANPOWER_TRAINING_DEVELOPMENT_ACT', 68).
test_case('testsets/sotu_1963_johnson_civil_rights_legislation.pl', 'sotu_1963_johnson_civil_rights_legislation', 'SOTU_1963_JOHNSON_CIVIL_RIGHTS_LEGISLATION', 69).
test_case('testsets/sotu_1963_johnson_executive_budgetary_restraint.pl', 'sotu_1963_johnson_executive_budgetary_restraint', 'SOTU_1963_JOHNSON_EXECUTIVE_BUDGETARY_RESTRAINT', 70).
test_case('testsets/sotu_1963_johnson_tax_bill_recession_insurance.pl', 'sotu_1963_johnson_tax_bill_recession_insurance', 'SOTU_1963_JOHNSON_TAX_BILL_RECESSION_INSURANCE', 71).
test_case('testsets/sotu_1963_kennedy_progressive_income_tax_reduction.pl', 'sotu_1963_kennedy_progressive_income_tax_reduction', 'SOTU_1963_KENNEDY_PROGRESSIVE_INCOME_TAX_REDUCTION', 72).
test_case('testsets/sotu_1963_kennedy_tax_base_broadening_structural_reform.pl', 'sotu_1963_kennedy_tax_base_broadening_structural_reform', 'SOTU_1963_KENNEDY_TAX_BASE_BROADENING_STRUCTURAL_REFORM', 73).
test_case('testsets/sotu_1964_johnson_deficit_reduction_budget.pl', '0', 'SOTU_1964_JOHNSON_DEFICIT_REDUCTION_BUDGET', 74).
test_case('testsets/sotu_1964_johnson_war_on_poverty.pl', 'sotu_1964_johnson_war_on_poverty', 'SOTU_1964_JOHNSON_WAR_ON_POVERTY', 75).
test_case('testsets/sotu_1965_johnson_alliance_for_progress.pl', 'sotu_1965_johnson_alliance_for_progress', 'SOTU_1965_JOHNSON_ALLIANCE_FOR_PROGRESS', 76).
test_case('testsets/sotu_1965_johnson_federal_preclearance_litigation_bypass.pl', 'sotu_1965_johnson_federal_preclearance_litigation_bypass', 'SOTU_1965_JOHNSON_FEDERAL_PRECLEARANCE_LITIGATION_BYPASS', 77).
test_case('testsets/sotu_1965_johnson_military_superiority_commitment.pl', 'sotu_1965_johnson_military_superiority_commitment', 'SOTU_1965_JOHNSON_MILITARY_SUPERIORITY_COMMITMENT', 78).
test_case('testsets/sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc.pl', 'sotu_1965_johnson_peaceful_trade_expansion_eastern_bloc', 'SOTU_1965_JOHNSON_PEACEFUL_TRADE_EXPANSION_EASTERN_BLOC', 79).
test_case('testsets/sotu_1965_johnson_uniform_voting_standard_elimination.pl', 'sotu_1965_johnson_uniform_voting_standard_elimination', 'SOTU_1965_JOHNSON_UNIFORM_VOTING_STANDARD_ELIMINATION', 80).
test_case('testsets/sotu_1966_johnson_federal_civil_rights_enforcement_crime.pl', '0', 'SOTU_1966_JOHNSON_FEDERAL_CIVIL_RIGHTS_ENFORCEMENT_CRIME', 81).
test_case('testsets/sotu_1967_johnson_department_consolidation_labor_commerce.pl', 'sotu_1967_johnson_department_consolidation_labor_commerce', 'SOTU_1967_JOHNSON_DEPARTMENT_CONSOLIDATION_LABOR_COMMERCE', 82).
test_case('testsets/sotu_1967_johnson_medicare_establishment.pl', 'sotu_1967_johnson_medicare_establishment', 'SOTU_1967_JOHNSON_MEDICARE_ESTABLISHMENT', 83).
test_case('testsets/sotu_1967_johnson_war_on_poverty_mechanism.pl', 'sotu_1967_johnson_war_on_poverty_mechanism', 'SOTU_1967_JOHNSON_WAR_ON_POVERTY_MECHANISM', 84).
test_case('testsets/sotu_1968_johnson_international_development_association_expansion.pl', 'sotu_1968_johnson_international_development_association_expansion', 'SOTU_1968_JOHNSON_INTERNATIONAL_DEVELOPMENT_ASSOCIATION_EXPANSION', 85).
test_case('testsets/sotu_1968_johnson_san_antonio_formula.pl', 'sotu_1968_johnson_san_antonio_formula', 'SOTU_1968_JOHNSON_SAN_ANTONIO_FORMULA', 86).
test_case('testsets/sotu_1969_johnson_medicare_expansion.pl', 'sotu_1969_johnson_medicare_expansion', 'SOTU_1969_JOHNSON_MEDICARE_EXPANSION', 87).
test_case('testsets/sotu_1969_johnson_model_cities_program.pl', '1969', 'SOTU_1969_JOHNSON_MODEL_CITIES_PROGRAM', 88).
test_case('testsets/sotu_1969_johnson_social_security_benefit_increase.pl', 'sotu_1969_johnson_social_security_benefit_increase', 'SOTU_1969_JOHNSON_SOCIAL_SECURITY_BENEFIT_INCREASE', 89).
test_case('testsets/sotu_1970_nixon_doctrine_burden_sharing.pl', 'sotu_1970_nixon_doctrine_burden_sharing', 'SOTU_1970_NIXON_DOCTRINE_BURDEN_SHARING', 90).
test_case('testsets/sotu_1970_nixon_government_institutional_reform.pl', 'sotu_1970_nixon_government_institutional_reform', 'SOTU_1970_NIXON_GOVERNMENT_INSTITUTIONAL_REFORM', 91).
test_case('testsets/sotu_1970_nixon_revenue_sharing.pl', 'sotu_1970_nixon_revenue_sharing', 'SOTU_1970_NIXON_REVENUE_SHARING', 92).
test_case('testsets/sotu_1971_nixon_environmental_preservation_mandate.pl', 'sotu_1971_nixon_environmental_preservation_mandate', 'SOTU_1971_NIXON_ENVIRONMENTAL_PRESERVATION_MANDATE', 93).
test_case('testsets/sotu_1971_nixon_full_employment_budget.pl', 'sotu_1971_nixon_full_employment_budget', 'SOTU_1971_NIXON_FULL_EMPLOYMENT_BUDGET', 94).
test_case('testsets/sotu_1971_nixon_welfare_floor_and_work_requirement.pl', 'sotu_1971_nixon_welfare_floor_and_work_requirement', 'SOTU_1971_NIXON_WELFARE_FLOOR_AND_WORK_REQUIREMENT', 95).
test_case('testsets/sotu_1972_nixon_burden_sharing_doctrine.pl', 'sotu_1972_nixon_burden_sharing_doctrine', 'SOTU_1972_NIXON_BURDEN_SHARING_DOCTRINE', 96).
test_case('testsets/sotu_1972_nixon_defense_below_human_resources.pl', 'sotu_1972_nixon_defense_below_human_resources', 'SOTU_1972_NIXON_DEFENSE_BELOW_HUMAN_RESOURCES', 97).
test_case('testsets/sotu_1972_nixon_strategic_arms_limitation.pl', '0', 'SOTU_1972_NIXON_STRATEGIC_ARMS_LIMITATION', 98).
test_case('testsets/sotu_1973_nixon_decentralized_problem_solving.pl', 'sotu_1973_nixon_decentralized_problem_solving', 'SOTU_1973_NIXON_DECENTRALIZED_PROBLEM_SOLVING', 99).
test_case('testsets/sotu_1973_nixon_federal_spending_cap.pl', 'sotu_1973_nixon_federal_spending_cap', 'SOTU_1973_NIXON_FEDERAL_SPENDING_CAP', 100).
test_case('testsets/sotu_1973_nixon_revenue_sharing.pl', 'sotu_1973_nixon_revenue_sharing', 'SOTU_1973_NIXON_REVENUE_SHARING', 101).
test_case('testsets/sotu_1974_nixon_energy_independence_infrastructure.pl', '1974', 'SOTU_1974_NIXON_ENERGY_INDEPENDENCE_INFRASTRUCTURE', 102).
test_case('testsets/sotu_1974_nixon_universal_healthcare_access.pl', 'sotu_1974_nixon_universal_healthcare_access', 'SOTU_1974_NIXON_UNIVERSAL_HEALTHCARE_ACCESS', 103).
test_case('testsets/sotu_1974_nixon_voluntary_inflation_control.pl', 'sotu_1974_nixon_voluntary_inflation_control', 'SOTU_1974_NIXON_VOLUNTARY_INFLATION_CONTROL', 104).
test_case('testsets/sotu_1975_ford_federal_spending_ceiling.pl', 'sotu_1975_ford_federal_spending_ceiling', 'SOTU_1975_FORD_FEDERAL_SPENDING_CEILING', 105).
test_case('testsets/sotu_1975_ford_tax_reduction_stimulus.pl', 'peak', 'SOTU_1975_FORD_TAX_REDUCTION_STIMULUS', 106).
test_case('testsets/sotu_1975_ford_veto_moratorium_new_programs.pl', 'sotu_1975_ford_veto_moratorium_new_programs', 'SOTU_1975_FORD_VETO_MORATORIUM_NEW_PROGRAMS', 107).
test_case('testsets/sotu_1976_ford_defense_domestic_rebalance.pl', 'sotu_1976_ford_defense_domestic_rebalance', 'SOTU_1976_FORD_DEFENSE_DOMESTIC_REBALANCE', 108).
test_case('testsets/sotu_1976_ford_federal_expenditure_restraint.pl', 'sotu_1976_ford_federal_expenditure_restraint', 'SOTU_1976_FORD_FEDERAL_EXPENDITURE_RESTRAINT', 109).
test_case('testsets/sotu_1976_ford_federalism_devolution.pl', 'sotu_1976_ford_federalism_devolution', 'SOTU_1976_FORD_FEDERALISM_DEVOLUTION', 110).
test_case('testsets/sotu_1977_ford_federal_spending_restraint.pl', 'sotu_1977_ford_federal_spending_restraint', 'SOTU_1977_FORD_FEDERAL_SPENDING_RESTRAINT', 111).
test_case('testsets/sotu_1977_ford_presidential_transition_protocol.pl', 'sotu_1977_ford_presidential_transition_protocol', 'SOTU_1977_FORD_PRESIDENTIAL_TRANSITION_PROTOCOL', 112).
test_case('testsets/sotu_1977_ford_strategic_arms_limitation.pl', 'sotu_1977_ford_strategic_arms_limitation', 'SOTU_1977_FORD_STRATEGIC_ARMS_LIMITATION', 113).
test_case('testsets/sotu_1978_carter_national_energy_program.pl', 'sotu_1978_carter_national_energy_program', 'SOTU_1978_CARTER_NATIONAL_ENERGY_PROGRAM', 114).
test_case('testsets/sotu_1978_carter_private_sector_led_expansion.pl', 'sotu_1978_carter_private_sector_led_expansion', 'SOTU_1978_CARTER_PRIVATE_SECTOR_LED_EXPANSION', 115).
test_case('testsets/sotu_1978_carter_tax_reform_and_reduction.pl', 'sotu_1978_carter_tax_reform_and_reduction', 'SOTU_1978_CARTER_TAX_REFORM_AND_REDUCTION', 116).
test_case('testsets/sotu_1979_carter_airline_deregulation_model.pl', 'sotu_1979_carter_airline_deregulation_model', 'SOTU_1979_CARTER_AIRLINE_DEREGULATION_MODEL', 117).
test_case('testsets/sotu_1979_carter_hospital_cost_containment.pl', 't', 'SOTU_1979_CARTER_HOSPITAL_COST_CONTAINMENT', 118).
test_case('testsets/sotu_1979_carter_voluntary_wage_price_restraint_compact.pl', 'sotu_1979_carter_voluntary_wage_price_restraint_compact', 'SOTU_1979_CARTER_VOLUNTARY_WAGE_PRICE_RESTRAINT_COMPACT', 119).
test_case('testsets/sotu_1980_carter_economic_embargo_soviet.pl', 'sotu_1980_carter_economic_embargo_soviet', 'SOTU_1980_CARTER_ECONOMIC_EMBARGO_SOVIET', 120).
test_case('testsets/sotu_1980_carter_salt_ii_maintenance.pl', 'sotu_1980_carter_salt_ii_maintenance', 'SOTU_1980_CARTER_SALT_II_MAINTENANCE', 121).
test_case('testsets/sotu_1981_reagan_categorical_block_grant_conversion.pl', '0', 'SOTU_1981_REAGAN_CATEGORICAL_BLOCK_GRANT_CONVERSION', 122).
test_case('testsets/sotu_1981_reagan_federal_spending_reduction.pl', '0', 'SOTU_1981_REAGAN_FEDERAL_SPENDING_REDUCTION', 123).
test_case('testsets/sotu_1981_reagan_regulatory_elimination.pl', 'sotu_1981_reagan_regulatory_elimination', 'SOTU_1981_REAGAN_REGULATORY_ELIMINATION', 124).
test_case('testsets/sotu_1982_reagan_federal_spending_growth_reduction.pl', 'sotu_1982_reagan_federal_spending_growth_reduction', 'SOTU_1982_REAGAN_FEDERAL_SPENDING_GROWTH_REDUCTION', 125).
test_case('testsets/sotu_1982_reagan_three_year_tax_rate_reduction.pl', 'sotu_1982_reagan_three_year_tax_rate_reduction', 'SOTU_1982_REAGAN_THREE_YEAR_TAX_RATE_REDUCTION', 126).
test_case('testsets/sotu_1983_reagan_monetary_inflation_expectation_anchoring.pl', 'sotu_1983_reagan_monetary_inflation_expectation_anchoring', 'SOTU_1983_REAGAN_MONETARY_INFLATION_EXPECTATION_ANCHORING', 127).
test_case('testsets/sotu_1983_reagan_social_security_commission_rescue.pl', 'sotu_1983_reagan_social_security_commission_rescue', 'SOTU_1983_REAGAN_SOCIAL_SECURITY_COMMISSION_RESCUE', 128).
test_case('testsets/sotu_1984_reagan_federal_spending_ceiling.pl', 'sotu_1984_reagan_federal_spending_ceiling', 'SOTU_1984_REAGAN_FEDERAL_SPENDING_CEILING', 129).
test_case('testsets/sotu_1984_reagan_regulatory_reduction.pl', 'sotu_1984_reagan_regulatory_reduction', 'SOTU_1984_REAGAN_REGULATORY_REDUCTION', 130).
test_case('testsets/sotu_1984_reagan_tax_indexation.pl', 'sotu_1984_reagan_tax_indexation', 'SOTU_1984_REAGAN_TAX_INDEXATION', 131).
test_case('testsets/sotu_1985_reagan_enterprise_zones_establishment.pl', 'sotu_1985_reagan_enterprise_zones_establishment', 'SOTU_1985_REAGAN_ENTERPRISE_ZONES_ESTABLISHMENT', 132).
test_case('testsets/sotu_1985_reagan_tax_rate_reduction_mechanism.pl', '0', 'SOTU_1985_REAGAN_TAX_RATE_REDUCTION_MECHANISM', 133).
test_case('testsets/sotu_1986_reagan_gramm_rudman_hollings.pl', 'sotu_1986_reagan_gramm_rudman_hollings', 'SOTU_1986_REAGAN_GRAMM_RUDMAN_HOLLINGS', 134).
test_case('testsets/sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality.pl', 'sotu_1987_reagan_afghanistan_soviet_withdrawal_conditionality', 'SOTU_1987_REAGAN_AFGHANISTAN_SOVIET_WITHDRAWAL_CONDITIONALITY', 135).
test_case('testsets/sotu_1987_reagan_central_america_regional_containment.pl', 'sotu_1987_reagan_central_america_regional_containment', 'SOTU_1987_REAGAN_CENTRAL_AMERICA_REGIONAL_CONTAINMENT', 136).
test_case('testsets/sotu_1987_reagan_soviet_military_containment_budget.pl', 'sotu_1987_reagan_soviet_military_containment_budget', 'SOTU_1987_REAGAN_SOVIET_MILITARY_CONTAINMENT_BUDGET', 137).
test_case('testsets/sotu_1988_reagan_limited_government_personal_liberty_doctrine.pl', 'sotu_1988_reagan_limited_government_personal_liberty_doctrine', 'SOTU_1988_REAGAN_LIMITED_GOVERNMENT_PERSONAL_LIBERTY_DOCTRINE', 138).
test_case('testsets/sotu_1988_reagan_strategic_defense_initiative.pl', '0', 'SOTU_1988_REAGAN_STRATEGIC_DEFENSE_INITIATIVE', 139).
test_case('testsets/sotu_1989_bush_capital_gains_tax_rate_reduction.pl', 'sotu_1989_bush_capital_gains_tax_rate_reduction', 'SOTU_1989_BUSH_CAPITAL_GAINS_TAX_RATE_REDUCTION', 140).
test_case('testsets/sotu_1989_bush_line_item_veto_proposal.pl', 'sotu_1989_bush_line_item_veto_proposal', 'SOTU_1989_BUSH_LINE_ITEM_VETO_PROPOSAL', 141).
test_case('testsets/sotu_1990_bush_family_savings_plan.pl', '0', 'SOTU_1990_BUSH_FAMILY_SAVINGS_PLAN', 142).
test_case('testsets/sotu_1991_bush_decentralized_social_provision.pl', 'sotu_1991_bush_decentralized_social_provision', 'SOTU_1991_BUSH_DECENTRALIZED_SOCIAL_PROVISION', 143).
test_case('testsets/sotu_1991_bush_gulf_security_framework.pl', 'sotu_1991_bush_gulf_security_framework', 'SOTU_1991_BUSH_GULF_SECURITY_FRAMEWORK', 144).
test_case('testsets/sotu_1991_bush_middle_east_peace_mediation.pl', 'sotu_1991_bush_middle_east_peace_mediation', 'SOTU_1991_BUSH_MIDDLE_EAST_PEACE_MEDIATION', 145).
test_case('testsets/sotu_1991_bush_soviet_conditional_engagement.pl', 'sotu_1991_bush_soviet_conditional_engagement', 'SOTU_1991_BUSH_SOVIET_CONDITIONAL_ENGAGEMENT', 146).
test_case('testsets/sotu_1991_bush_un_coalition_enforcement.pl', 'sotu_1991_bush_un_coalition_enforcement', 'SOTU_1991_BUSH_UN_COALITION_ENFORCEMENT', 147).
test_case('testsets/sotu_1991_bush_wmd_proliferation_controls.pl', 'sotu_1991_bush_wmd_proliferation_controls', 'SOTU_1991_BUSH_WMD_PROLIFERATION_CONTROLS', 148).
test_case('testsets/sotu_1992_bush_operation_desert_storm_authorization_retrospective.pl', 'sotu_1992_bush_operation_desert_storm_authorization_retrospective', 'SOTU_1992_BUSH_OPERATION_DESERT_STORM_AUTHORIZATION_RETROSPECTIVE', 149).
test_case('testsets/sotu_1992_bush_strategic_nuclear_force_reduction.pl', 'sotu_1992_bush_strategic_nuclear_force_reduction', 'SOTU_1992_BUSH_STRATEGIC_NUCLEAR_FORCE_REDUCTION', 150).
test_case('testsets/sotu_1993_clinton_deficit_reduction_program.pl', '0', 'SOTU_1993_CLINTON_DEFICIT_REDUCTION_PROGRAM', 151).
test_case('testsets/sotu_1993_clinton_immediate_jobs_investment_package.pl', 'sotu_1993_clinton_immediate_jobs_investment_package', 'SOTU_1993_CLINTON_IMMEDIATE_JOBS_INVESTMENT_PACKAGE', 152).
test_case('testsets/sotu_1993_clinton_public_investment_prioritization.pl', '0', 'SOTU_1993_CLINTON_PUBLIC_INVESTMENT_PRIORITIZATION', 153).
test_case('testsets/sotu_1994_clinton_deficit_reduction_budget.pl', 'sotu_1994_clinton_deficit_reduction_budget', 'SOTU_1994_CLINTON_DEFICIT_REDUCTION_BUDGET', 154).
test_case('testsets/sotu_1994_clinton_reinvention_government.pl', 'sotu_1994_clinton_reinvention_government', 'SOTU_1994_CLINTON_REINVENTION_GOVERNMENT', 155).
test_case('testsets/sotu_1995_clinton_congressional_accountability_law.pl', 'sotu_1995_clinton_congressional_accountability_law', 'SOTU_1995_CLINTON_CONGRESSIONAL_ACCOUNTABILITY_LAW', 156).
test_case('testsets/sotu_1995_clinton_lobby_gift_ban.pl', 'sotu_1995_clinton_lobby_gift_ban', 'SOTU_1995_CLINTON_LOBBY_GIFT_BAN', 157).
test_case('testsets/sotu_1995_clinton_new_covenant_skills_framework.pl', 'sotu_1995_clinton_new_covenant_skills_framework', 'SOTU_1995_CLINTON_NEW_COVENANT_SKILLS_FRAMEWORK', 158).
test_case('testsets/sotu_1996_clinton_balanced_budget_requirement.pl', 'sotu_1996_clinton_balanced_budget_requirement', 'SOTU_1996_CLINTON_BALANCED_BUDGET_REQUIREMENT', 159).
test_case('testsets/sotu_1996_clinton_media_industry_rating_system.pl', '1996', 'SOTU_1996_CLINTON_MEDIA_INDUSTRY_RATING_SYSTEM', 160).
test_case('testsets/sotu_1996_clinton_v_chip_television_requirement.pl', '0', 'SOTU_1996_CLINTON_V_CHIP_TELEVISION_REQUIREMENT', 161).
test_case('testsets/sotu_1997_clinton_budget_balance_mechanism.pl', '0', 'SOTU_1997_CLINTON_BUDGET_BALANCE_MECHANISM', 162).
test_case('testsets/sotu_1997_clinton_campaign_finance_reform_soft_money_ban.pl', 'sotu_1997_clinton_campaign_finance_reform_soft_money_ban', 'SOTU_1997_CLINTON_CAMPAIGN_FINANCE_REFORM_SOFT_MONEY_BAN', 163).
test_case('testsets/sotu_1997_clinton_welfare_employment_transition.pl', 'sotu_1997_clinton_welfare_employment_transition', 'SOTU_1997_CLINTON_WELFARE_EMPLOYMENT_TRANSITION', 164).
test_case('testsets/sotu_1998_clinton_balanced_budget_commitment.pl', 'sotu_1998_clinton_balanced_budget_commitment', 'SOTU_1998_CLINTON_BALANCED_BUDGET_COMMITMENT', 165).
test_case('testsets/sotu_1998_clinton_minimum_wage_increase.pl', 'sotu_1998_clinton_minimum_wage_increase', 'SOTU_1998_CLINTON_MINIMUM_WAGE_INCREASE', 166).
test_case('testsets/sotu_1999_clinton_medicare_trust_fund_surplus_allocation.pl', 'sotu_1999_clinton_medicare_trust_fund_surplus_allocation', 'SOTU_1999_CLINTON_MEDICARE_TRUST_FUND_SURPLUS_ALLOCATION', 167).
test_case('testsets/sotu_1999_clinton_social_security_surplus_dedication.pl', 'sotu_1999_clinton_social_security_surplus_dedication', 'SOTU_1999_CLINTON_SOCIAL_SECURITY_SURPLUS_DEDICATION', 168).
test_case('testsets/sotu_1999_clinton_usa_accounts_universal_savings.pl', 'sotu_1999_clinton_usa_accounts_universal_savings', 'SOTU_1999_CLINTON_USA_ACCOUNTS_UNIVERSAL_SAVINGS', 169).
test_case('testsets/sotu_2000_clinton_community_policing_brady_law.pl', 'sotu_2000_clinton_community_policing_brady_law', 'SOTU_2000_CLINTON_COMMUNITY_POLICING_BRADY_LAW', 170).
test_case('testsets/sotu_2000_clinton_welfare_reform_work_requirement.pl', 'sotu_2000_clinton_welfare_reform_work_requirement', 'SOTU_2000_CLINTON_WELFARE_REFORM_WORK_REQUIREMENT', 171).
test_case('testsets/sotu_2001_bush_Global_counterterrorism_coalition_alignment.pl', 'sotu_2001_bush_Global_counterterrorism_coalition_alignment', 'SOTU_2001_BUSH_GLOBAL_COUNTERTERRORISM_COALITION_ALIGNMENT', 172).
test_case('testsets/sotu_2001_bush_Taliban_ultimatum_structural_coercion.pl', 'sotu_2001_bush_Taliban_ultimatum_structural_coercion', 'SOTU_2001_BUSH_TALIBAN_ULTIMATUM_STRUCTURAL_COERCION', 173).
test_case('testsets/sotu_2001_bush_annual_student_testing_mandate.pl', 'sotu_2001_bush_annual_student_testing_mandate', 'SOTU_2001_BUSH_ANNUAL_STUDENT_TESTING_MANDATE', 174).
test_case('testsets/sotu_2001_bush_health_insurance_tax_credits.pl', 'sotu_2001_bush_health_insurance_tax_credits', 'SOTU_2001_BUSH_HEALTH_INSURANCE_TAX_CREDITS', 175).
test_case('testsets/sotu_2002_bush_terrorist_detention_and_prosecution.pl', '2002', 'SOTU_2002_BUSH_TERRORIST_DETENTION_AND_PROSECUTION', 176).
test_case('testsets/sotu_2002_bush_unilateral_military_intervention_doctrine.pl', 'sotu_2002_bush_unilateral_military_intervention_doctrine', 'SOTU_2002_BUSH_UNILATERAL_MILITARY_INTERVENTION_DOCTRINE', 177).
test_case('testsets/sotu_2002_bush_weapons_of_mass_destruction_prevention_regime.pl', 'sotu_2002_bush_weapons_of_mass_destruction_prevention_regime', 'SOTU_2002_BUSH_WEAPONS_OF_MASS_DESTRUCTION_PREVENTION_REGIME', 178).
test_case('testsets/sotu_2003_bush_accelerated_tax_relief_permanence.pl', '0', 'SOTU_2003_BUSH_ACCELERATED_TAX_RELIEF_PERMANENCE', 179).
test_case('testsets/sotu_2003_bush_medicare_prescription_drug_expansion.pl', 'sotu_2003_bush_medicare_prescription_drug_expansion', 'SOTU_2003_BUSH_MEDICARE_PRESCRIPTION_DRUG_EXPANSION', 180).
test_case('testsets/sotu_2004_bush_iraqi_governing_council_transition.pl', '0', 'SOTU_2004_BUSH_IRAQI_GOVERNING_COUNCIL_TRANSITION', 181).
test_case('testsets/sotu_2004_bush_medicare_prescription_drug_benefit.pl', 'sotu_2004_bush_medicare_prescription_drug_benefit', 'SOTU_2004_BUSH_MEDICARE_PRESCRIPTION_DRUG_BENEFIT', 182).
test_case('testsets/sotu_2004_bush_patriot_act_renewal.pl', 'sotu_2004_bush_patriot_act_renewal', 'SOTU_2004_BUSH_PATRIOT_ACT_RENEWAL', 183).
test_case('testsets/sotu_2005_bush_health_savings_accounts_expansion.pl', 'sotu_2005_bush_health_savings_accounts_expansion', 'SOTU_2005_BUSH_HEALTH_SAVINGS_ACCOUNTS_EXPANSION', 184).
test_case('testsets/sotu_2005_bush_no_child_left_behind_enforcement.pl', 'sotu_2005_bush_no_child_left_behind_enforcement', 'SOTU_2005_BUSH_NO_CHILD_LEFT_BEHIND_ENFORCEMENT', 185).
test_case('testsets/sotu_2005_bush_social_security_reform_trajectory.pl', 'sotu_2005_bush_social_security_reform_trajectory', 'SOTU_2005_BUSH_SOCIAL_SECURITY_REFORM_TRAJECTORY', 186).
test_case('testsets/sotu_2006_bush_global_democracy_advancement_doctrine.pl', '0', 'SOTU_2006_BUSH_GLOBAL_DEMOCRACY_ADVANCEMENT_DOCTRINE', 187).
test_case('testsets/sotu_2006_bush_iraqi_inclusive_government_building.pl', 'sotu_2006_bush_iraqi_inclusive_government_building', 'SOTU_2006_BUSH_IRAQI_INCLUSIVE_GOVERNMENT_BUILDING', 188).
test_case('testsets/sotu_2006_bush_iraqi_security_force_training_handoff.pl', 'sotu_2006_bush_iraqi_security_force_training_handoff', 'SOTU_2006_BUSH_IRAQI_SECURITY_FORCE_TRAINING_HANDOFF', 189).

% --- Test Suite Runner ---
run_dynamic_suite :-
    retractall(test_passed(_)),
    retractall(test_failed(_, _, _)),
    writeln('--- STARTING DYNAMIC VALIDATION ---'),
    forall(test_case(Path, ID, Label, N), run_single_test(Path, ID, Label, N)),
    count_and_report,
    % Call validate_all directly from data_validation module
    data_validation:validate_all.

% --- Single Test Executor ---
%  Per-test timeout guard (60s) prevents any single test from consuming
%  the entire sweep timeout. Elapsed timing aids diagnostic profiling.
run_single_test(Path, ID, _Label, N) :-
    format('~n[~w] EXECUTING: ~w~n', [N, Path]),
    get_time(T0),
    catch(
        call_with_time_limit(
            60,
            (   catch_with_backtrace(
                    ( load_and_run(Path, ID) ->
                        assertz(test_passed(Path)),
                        format('[PASS] ~w~n', [Path])
                    ;   assertz(test_failed(Path, audit_failed, 'load_and_run returned false')),
                        format('[AUDIT FAIL] ~w~n', [Path])
                    ),
                    E,
                    (   assertz(test_failed(Path, exception, E)),
                        format('[FAIL] Exception for ~w: ~w~n', [Path, E]),
                        print_prolog_backtrace(current_output, E)
                    )
                ),
                report_generator:generate_llm_feedback(ID)
            )
        ),
        time_limit_exceeded,
        (   assertz(test_failed(Path, timeout, 'Exceeded 60s per-test limit')),
            format('[TIMEOUT] ~w~n', [Path])
        )
    ),
    get_time(T1),
    Elapsed is T1 - T0,
    format('[ELAPSED] ~w: ~3fs~n', [Path, Elapsed]).

% --- Result Counter & Reporter ---
count_and_report :-
    findall(P, test_passed(P), Ps), length(Ps, PC),
    findall(F, test_failed(F,_,_), Fs), length(Fs, FC),
    writeln(''),
    writeln('=================================================='),
    writeln('           TEST SUITE SUMMARY'),
    writeln('=================================================='),
    format('Passed: ~w~n', [PC]),
    format('Failed: ~w~n', [FC]),
    (FC > 0 -> report_failures ; true),
    writeln('==================================================').

report_failures :-
    writeln('--- FAILED TESTS ---'),
    forall(test_failed(Path, Type, Detail),
           format('~n  - [~w] ~w~n    Reason: ~w~n', [Type, Path, Detail])).

