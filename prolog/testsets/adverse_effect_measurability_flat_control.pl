% ============================================================================
% CONSTRAINT STORY: adverse_effect_measurability_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_adverse_effect_measurability_flat_control, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: adverse_effect_measurability_flat_control
 *   human_readable: H-2A Adverse Effect Wage Rate Certification System
 *   domain: political_economy/labor_migration/administrative_law
 *
 * SUMMARY:
 *   The H-2A program's legitimacy rests on a statutory guarantee: admitting
 *   foreign guest workers into US agriculture will not adversely affect the
 *   wages of similarly employed domestic workers. DOL operationalizes this
 *   guarantee through the Adverse Effect Wage Rate (AEWR), computed annually
 *   from USDA Farm Labor Survey data. The certification regime functions as a
 *   commitment system — DOL's authority to certify admissions is legitimated
 *   by its claim to enforce a fixed, measured wage baseline. But the survey
 *   underlying that baseline has degraded in coverage over decades: shrinking
 *   sample sizes mean many regional AEWRs are imputed or modeled rather than
 *   directly observed, so the 'measured baseline' the entire certification
 *   chain depends on is, in a growing number of regions, closer to an
 *   administrative construct than an empirical fact. This story treats the
 *   constraint as a single, flat structure — the tension between the
 *   coordination function (a predictable wage floor enabling orderly
 *   certification) and its drift toward theatrical measurement is captured
 *   through stakeholder divergence and temporal metrics, not through
 *   decomposition into separate readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(adverse_effect_measurability_flat_control, 0.62).
domain_priors:suppression_score(adverse_effect_measurability_flat_control, 0.55).
domain_priors:theater_ratio(adverse_effect_measurability_flat_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(adverse_effect_measurability_flat_control, extractiveness, 0.62).
narrative_ontology:constraint_metric(adverse_effect_measurability_flat_control, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(adverse_effect_measurability_flat_control, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(adverse_effect_measurability_flat_control, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(adverse_effect_measurability_flat_control, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(adverse_effect_measurability_flat_control, tangled_rope).
narrative_ontology:human_readable(adverse_effect_measurability_flat_control, "H-2A Adverse Effect Wage Rate Certification System").
narrative_ontology:topic_domain(adverse_effect_measurability_flat_control, "political_economy/labor_migration/administrative_law").

domain_priors:requires_active_enforcement(adverse_effect_measurability_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(adverse_effect_measurability_flat_control, adverse_effect_measurability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(adverse_effect_measurability_flat_control, agricultural_employers).
narrative_ontology:constraint_beneficiary(adverse_effect_measurability_flat_control, dol_certifying_office).
narrative_ontology:constraint_victim(adverse_effect_measurability_flat_control, us_farmworkers).
narrative_ontology:constraint_victim(adverse_effect_measurability_flat_control, h2a_guest_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(adverse_effect_measurability_flat_control, h2a_guest_workers).
narrative_ontology:constraint_vindicates(adverse_effect_measurability_flat_control, statutory_wage_protection_guarantee).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the H-2A certification process, sets the Adverse Effect Wage Rate (AEWR) annually using USDA Farm Labor Survey data, and certifies employer applications on the premise that the AEWR prevents wage suppression. Its institutional legitimacy rests on the claim that the wage baseline it publishes is a real, measured floor, not an administrative fiction; it controls the methodology and its revisions.
narrative_ontology:constraint_stakeholder(adverse_effect_measurability_flat_control, dol_certifying_office, agenda_setter,
    institutional, generational, analytical, national).

% Petition for H-2A certification, pay the published AEWR, and gain access to a guest-worker labor pool with lower turnover and no independent bargaining leverage. They lobby the survey methodology, litigate wage-rate increases, and can shift acreage or crop mix if a regional AEWR rises, giving them exit options domestic workers lack.
narrative_ontology:constraint_stakeholder(adverse_effect_measurability_flat_control, agricultural_employers, beneficiary,
    organized, biographical, mobile, national).

% Compete for the same jobs the AEWR is supposed to protect. The wage baseline is measured by a survey with declining sample coverage in many states, so the number that is supposed to represent 'their' prevailing wage is often modeled or imputed rather than directly observed in their labor market. They have no standing in the AEWR-setting process and cannot appeal a rate they believe understates true market wages.
narrative_ontology:constraint_stakeholder(adverse_effect_measurability_flat_control, us_farmworkers, payer,
    powerless, biographical, trapped, regional).

% Are tied to a single certified employer through the visa and are paid at the AEWR, which is often set below what they might command in a competitive local labor market; the visa's employer-lock removes the ability to seek a higher-paying position even where alternatives exist. They also benefit relative to their home-country wage floor, which is why they enter the program at all.
narrative_ontology:constraint_stakeholder(adverse_effect_measurability_flat_control, h2a_guest_workers, payer,
    powerless, immediate, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(adverse_effect_measurability_flat_control, h2a_guest_workers, beneficiary).

% Conducts the Farm Labor Survey that feeds the AEWR calculation. Sample sizes and regional coverage have shrunk over decades due to budget constraints, meaning some regional AEWRs are statistically thin or imputed from adjacent regions. This office does not adjudicate wage disputes but its methodological choices determine whether the 'measured baseline' the whole certification regime depends on is empirically solid or largely notional.
narrative_ontology:constraint_stakeholder(adverse_effect_measurability_flat_control, usda_farm_labor_survey_office, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(adverse_effect_measurability_flat_control, usda_farm_labor_survey_office, agenda_setter).

% Litigate and comment on AEWR methodology on behalf of farmworkers but have no seat in the annual survey design or certification decision itself; their objections are addressed, if at all, through slow notice-and-comment rulemaking rather than real-time adjustment.
narrative_ontology:constraint_stakeholder(adverse_effect_measurability_flat_control, worker_advocacy_organizations, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(adverse_effect_measurability_flat_control, agricultural_employers).
narrative_ontology:fixing_cost_class(adverse_effect_measurability_flat_control, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administratively predictable wage floor that lets employers plan guest-worker hiring and lets the government certify admissions without a case-by-case wage investigation for every petition — genuine coordination value if the underlying wage measurement is sound.
% TRANSFER_FUNCTION: Moves labor market leverage from domestic farmworkers and guest workers (who cannot bargain above the administratively set rate, and in the guest-worker case cannot bargain at all) to agricultural employers, who obtain a legally certified, artificially anchored wage ceiling in practice even though the AEWR is nominally a floor.
% ABSENT_VOICES: US farmworkers and H-2A workers themselves have no formal role in setting the AEWR; the survey methodology is set by USDA/DOL technical staff and contested, if at all, by advocacy organizations through litigation years after the fact — the workers whose wages the system claims to protect are structurally absent from its calibration.
% DISAPPEARANCE_RATIONALE: Employers and DOL would say the certification regime's disappearance would collapse orderly guest-worker admission and invite unregulated wage undercutting; worker advocates would say its disappearance would simply remove a legitimating cover for a wage rate that already functions as a suppressed administrative ceiling rather than a market floor, and would force employers into genuine local wage competition.
% FOUNDING_PROBLEM: Congress required, in the 1986 IRCA framework, that admitting temporary agricultural guest workers not adversely affect the wages of similarly employed US workers — the felt problem was that guest-worker admission absent a wage floor would drive down domestic farm wages and displace US labor.
% FOUNDING_PROBLEM_CORROBORATION: DOL and agricultural employer associations attest the AEWR mechanism continues to serve its founding purpose. GAO reports, academic labor economists, and farmworker advocacy litigation (outside the beneficiary set) have repeatedly found the Farm Labor Survey's declining sample sizes and imputation methods produce wage estimates with substantial measurement uncertainty, undermining the claim that a real, observed 'similarly employed' wage baseline exists in many certified regions.
narrative_ontology:disappearance_verdict(adverse_effect_measurability_flat_control, contested).
narrative_ontology:founding_problem_status(adverse_effect_measurability_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(adverse_effect_measurability_flat_control, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-22',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(adverse_effect_measurability_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(adverse_effect_measurability_flat_control, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(adverse_effect_measurability_flat_control_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(adverse_effect_measurability_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(adverse_effect_measurability_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that the AEWR increasingly functions as an anchored ceiling employers can plan around rather than a genuine floor tracking local labor market conditions, especially where imputed data understate true prevailing wages. Theater ratio (0.58) captures the widening gap between the survey's claimed measurement function and its actual statistical thinness — the ratio has risen steadily as sample coverage eroded, which is why the temporal series shows monotonic increase rather than a stable value. Suppression (0.55) is a raw structural property: domestic farmworkers have no formal channel to contest an AEWR they believe understates their market wage, and H-2A workers are visa-locked to a single employer, both independent of enforcement intensity or scope.
 *
 * PERSPECTIVAL GAP:
 *   DOL and agricultural employer associations experience this constraint as a functioning coordination mechanism: a stable, legally defensible wage rate that lets certification proceed predictably. Domestic farmworkers and worker advocates experience the same structure as an extractive mechanism whose legitimating measurement has hollowed out — the 'similarly employed' wage the statute promises to protect is, in an increasing share of regions, not actually observed. The engine computes these as structurally distinct per-seat classifications from the same authored data; the divergence is the finding, not an inconsistency to be resolved.
 *
 * DIRECTIONALITY LOGIC:
 *   Agricultural employers are structural beneficiaries: they gain a certified, predictable wage rate and mobility to shift operations if a regional AEWR becomes unfavorable — their exit options push their effective directionality toward the beneficiary end. US farmworkers are targets with no formal role in AEWR-setting and no exit from the regional labor market — trapped exit pushes their directionality toward full target. H-2A workers are also targets by wage structure (employer-lock, no bargaining) but partially offset by the benefit of dollar-denominated wages relative to home-country baselines, which is why they carry a dual beneficiary/payer role rather than a pure victim designation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — guest-worker admission driving down domestic farm wages absent a wage floor — remains at least partially live (agricultural labor markets are still thin and geographically concentrated, giving employers wage-setting power in the absence of any floor). This prevents a simple 'pure extraction' reading: the coordination function is not dead, which is why this is authored as tangled_rope rather than snare. But the measurement infrastructure that gives the floor its legitimacy has degraded independently of whether the underlying problem persists, producing a constraint whose coordination claim increasingly outruns its evidentiary basis — the theater_ratio trend is the diagnostic trace of that divergence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aewr_measurement_validity,
    'In regions where the Farm Labor Survey sample is thin or where the AEWR is imputed from adjacent regions, does the published rate still represent a genuine measurement of ''similarly employed'' domestic wages, or has it become an administrative construct decoupled from the local labor market it claims to describe?',
    'Independent re-survey of thin-sample regions with adequate sample size, compared against the imputed AEWR; GAO or OIG audit of USDA survey methodology and coverage trends over the interval.',
    'If the AEWR is substantially decoupled from actual local wages in a material share of certified regions, the statutory guarantee DOL claims to enforce is not being measured at all in those regions, which would support reclassifying the certification regime''s operation there as closer to snare than tangled_rope — the coordination premise fails where the measurement fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aewr_measurement_validity, empirical, 'Whether the AEWR wage baseline is genuinely measured or increasingly administratively imputed.').

omega_variable(
    founding_problem_persistence,
    'Is the original 1986 concern — guest-worker admission suppressing domestic farm wages absent a floor — still empirically live in the agricultural labor markets where H-2A certifications concentrate, or has the sector''s structure (consolidation, year-round guest-worker reliance in some regions) changed such that the AEWR now primarily preserves employer wage-setting power rather than counteracting it?',
    'Longitudinal wage studies comparing counties with high H-2A penetration against comparable counties with low penetration, controlling for crop mix and mechanization trends.',
    'If the founding problem has substantially receded in high-penetration regions, the certification regime there functions closer to a piton — a system defended on its founding rationale while the coordination function it names has weakened relative to the extraction it still performs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the 1986 wage-suppression concern remains empirically active or has been superseded by structural sector change.').

omega_variable(
    guest_worker_net_directionality,
    'Given that H-2A workers are both wage-suppressed relative to a competitive local labor market AND wage-benefited relative to home-country alternatives, does the constraint net-extract from them or net-benefit them?',
    'Comparative analysis of H-2A wages against (a) competitive US farm labor market rates absent the program and (b) home-country agricultural wage rates, weighted by worker-reported preference and re-enrollment rates.',
    'Resolving this toward net-extraction would strengthen the case for treating guest workers primarily as victims; resolving toward net-benefit would support their dual beneficiary/payer role as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guest_worker_net_directionality, conceptual, 'Whether guest workers are net beneficiaries or net victims of the wage-setting mechanism, given the dual comparison baseline.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(adverse_effect_measurability_flat_control, 1987, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(adve_tr_t1987, adverse_effect_measurability_flat_control, theater_ratio, 1987, 0.25).
narrative_ontology:measurement_basis(adve_tr_t1987, observed).
narrative_ontology:measurement(adve_tr_t1997, adverse_effect_measurability_flat_control, theater_ratio, 1997, 0.32).
narrative_ontology:measurement_basis(adve_tr_t1997, observed).
narrative_ontology:measurement(adve_tr_t2005, adverse_effect_measurability_flat_control, theater_ratio, 2005, 0.4).
narrative_ontology:measurement_basis(adve_tr_t2005, observed).
narrative_ontology:measurement(adve_tr_t2012, adverse_effect_measurability_flat_control, theater_ratio, 2012, 0.47).
narrative_ontology:measurement_basis(adve_tr_t2012, observed).
narrative_ontology:measurement(adve_tr_t2018, adverse_effect_measurability_flat_control, theater_ratio, 2018, 0.53).
narrative_ontology:measurement_basis(adve_tr_t2018, observed).
narrative_ontology:measurement(adve_tr_t2024, adverse_effect_measurability_flat_control, theater_ratio, 2024, 0.58).
narrative_ontology:measurement_basis(adve_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(adve_be_t1987, adverse_effect_measurability_flat_control, base_extractiveness, 1987, 0.35).
narrative_ontology:measurement_basis(adve_be_t1987, observed).
narrative_ontology:measurement(adve_be_t1997, adverse_effect_measurability_flat_control, base_extractiveness, 1997, 0.42).
narrative_ontology:measurement_basis(adve_be_t1997, observed).
narrative_ontology:measurement(adve_be_t2005, adverse_effect_measurability_flat_control, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement_basis(adve_be_t2005, observed).
narrative_ontology:measurement(adve_be_t2012, adverse_effect_measurability_flat_control, base_extractiveness, 2012, 0.54).
narrative_ontology:measurement_basis(adve_be_t2012, observed).
narrative_ontology:measurement(adve_be_t2018, adverse_effect_measurability_flat_control, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement_basis(adve_be_t2018, observed).
narrative_ontology:measurement(adve_be_t2024, adverse_effect_measurability_flat_control, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(adve_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(adve_su_t1987, adverse_effect_measurability_flat_control, suppression_requirement, 1987, 0.4).
narrative_ontology:measurement_basis(adve_su_t1987, observed).
narrative_ontology:measurement(adve_su_t1997, adverse_effect_measurability_flat_control, suppression_requirement, 1997, 0.44).
narrative_ontology:measurement_basis(adve_su_t1997, observed).
narrative_ontology:measurement(adve_su_t2005, adverse_effect_measurability_flat_control, suppression_requirement, 2005, 0.47).
narrative_ontology:measurement_basis(adve_su_t2005, observed).
narrative_ontology:measurement(adve_su_t2012, adverse_effect_measurability_flat_control, suppression_requirement, 2012, 0.5).
narrative_ontology:measurement_basis(adve_su_t2012, observed).
narrative_ontology:measurement(adve_su_t2018, adverse_effect_measurability_flat_control, suppression_requirement, 2018, 0.53).
narrative_ontology:measurement_basis(adve_su_t2018, observed).
narrative_ontology:measurement(adve_su_t2024, adverse_effect_measurability_flat_control, suppression_requirement, 2024, 0.55).
narrative_ontology:measurement_basis(adve_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(adverse_effect_measurability_flat_control, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(adverse_effect_measurability_flat_control, 0.12).
narrative_ontology:affects_constraint(adverse_effect_measurability_flat_control, h2a_visa_employer_lock).
narrative_ontology:affects_constraint(adverse_effect_measurability_flat_control, farm_labor_survey_methodology).

% DUAL FORMULATION NOTE:
% This story treats the adverse-effect wage guarantee as a single flat constraint per the construction-perturbation control instructions, without decomposing it into separate readings of the statutory text. It links to the H-2A visa employer-lock constraint (which shares the same worker population but a distinct extraction mechanism — mobility restriction rather than wage measurement) and to the underlying Farm Labor Survey methodology constraint (which is the upstream measurement infrastructure this story's legitimacy depends on).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
