% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__strong_exclusivity_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: trips_agreement_interpretive_kernel__strong_exclusivity_reading
 *   human_readable: TRIPS Agreement: Strong Patent Exclusivity Reading
 *   domain: international_trade_law/public_health_policy/intellectual_property_regime
 *
 * SUMMARY:
 *   This constraint represents the 'strong exclusivity' reading of the WTO
 *   TRIPS Agreement, which emphasizes high uniform patent protections and
 *   narrowly construes flexibilities for public health. This reading is
 *   actively enforced through WTO dispute settlement mechanisms and bilateral
 *   trade pressures, benefiting pharmaceutical innovators and developed
 *   nations while imposing significant costs on low-income states and
 *   patients. The claimed type is 'tangled_rope' because it purports to
 *   coordinate innovation incentives (a genuine function) but does so with
 *   substantial, asymmetric extraction and active suppression of
 *   alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.85).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.9).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Agreement: Strong Patent Exclusivity Reading").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade_law/public_health_policy/intellectual_property_regime").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '1e74f5fb-e5c9-402a-9791-1bae9b150303').
narrative_ontology:cs_kernel_codification('1e74f5fb-e5c9-402a-9791-1bae9b150303', fixed_text).
narrative_ontology:cs_authority_grounding('1e74f5fb-e5c9-402a-9791-1bae9b150303', lineage).
narrative_ontology:cs_interpretation_layer_present('1e74f5fb-e5c9-402a-9791-1bae9b150303').
narrative_ontology:cs_reading_relation('1e74f5fb-e5c9-402a-9791-1bae9b150303', trips_agreement_interpretive_kernel__public_health_flexibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('1e74f5fb-e5c9-402a-9791-1bae9b150303', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('1e74f5fb-e5c9-402a-9791-1bae9b150303', foundational, strong_ip_protection_drives_innovation).
narrative_ontology:cs_axiom_status(strong_ip_protection_drives_innovation, holdable).
narrative_ontology:cs_axiom_grounding('1e74f5fb-e5c9-402a-9791-1bae9b150303', strong_ip_protection_drives_innovation, empirically_contingent).
narrative_ontology:cs_axiom('1e74f5fb-e5c9-402a-9791-1bae9b150303', foundational, flexibilities_are_narrow_exceptions).
narrative_ontology:cs_axiom_status(flexibilities_are_narrow_exceptions, holdable).
narrative_ontology:cs_axiom_grounding('1e74f5fb-e5c9-402a-9791-1bae9b150303', flexibilities_are_narrow_exceptions, conventional).
narrative_ontology:cs_reference_frame('1e74f5fb-e5c9-402a-9791-1bae9b150303', original_trips_negotiating_intent).
narrative_ontology:cs_drift_state('1e74f5fb-e5c9-402a-9791-1bae9b150303', contemporary_public_health_crises, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1e74f5fb-e5c9-402a-9791-1bae9b150303', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_innovators).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developed_nations).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_developing_countries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from extended patent monopolies and high prices for patented medicines, incentivizing R&D. They actively lobby for strong IP enforcement and narrow interpretations of TRIPS flexibilities.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_innovators, beneficiary,
    organized, generational, arbitrage, global).

% Advocate for and enforce strong patent protections, aligning with their domestic pharmaceutical industries. They use trade mechanisms to pressure developing countries into compliance with this reading.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developed_nations, agenda_setter,
    institutional, generational, mobile, global).

% Bear the cost of high drug prices and limited access to generic medicines, leading to public health crises. Their ability to use TRIPS flexibilities like compulsory licensing is severely constrained by this reading.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_states, payer,
    powerless, immediate, trapped, national).

% Directly suffer from lack of access to essential medicines due to high prices and patent barriers. They have virtually no exit options from this system.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_developing_countries, payer,
    powerless, immediate, trapped, local).

% Interpret the TRIPS agreement and issue binding rulings. This reading influences their decisions towards stricter patent enforcement, often siding with developed nations and pharmaceutical companies.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_panels, agenda_setter,
    institutional, biographical, constrained, global).

% Argue for broader interpretation of TRIPS flexibilities to prioritize public health over patent rights. While they can influence policy, their direct impact on WTO rulings is limited by the strong exclusivity reading.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a global baseline for intellectual property protection, aiming to coordinate innovation incentives across diverse national legal systems and prevent free-riding on R&D investments.
% TRANSFER_FUNCTION: Transfers economic value from consumers and national health budgets in developing countries to pharmaceutical patent holders, in exchange for the promise of future innovation.
% ABSENT_VOICES: Public health advocates and representatives of patient groups in developing countries are often marginalized in the formal dispute settlement process, where the strong exclusivity reading dominates. They would argue for a more balanced interpretation prioritizing access.
% DISAPPEARANCE_RATIONALE: If this strong exclusivity reading of TRIPS vanished, developing countries would immediately expand generic drug production and compulsory licensing, leading to a rapid drop in drug prices and a significant shift in global pharmaceutical market dynamics. Pharmaceutical innovators would face reduced profits and potentially shift R&D focus.
% FOUNDING_PROBLEM: Lack of uniform global intellectual property protection was perceived to disincentivize innovation, particularly in pharmaceuticals, leading to concerns about free-riding on R&D investments.
% FOUNDING_PROBLEM_CORROBORATION: Pharmaceutical innovators and developed nations attest the problem is live, citing ongoing need for R&D incentives. Low-income states and public health advocates contest this, arguing the problem has shifted from lack of protection to lack of access, and that the current regime over-incentivizes certain types of innovation while hindering others.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) due to the monopoly pricing enabled by strong patent rights, leading to high drug costs. Suppression is also high (0.9) because the WTO dispute settlement system and bilateral trade agreements actively enforce this interpretation, limiting the ability of developing countries to implement public health flexibilities. Theater ratio is low (0.1) as the enforcement is direct and effective, not merely performative. Accessibility collapse is high (0.75) because alternative access mechanisms (like generic production) are severely curtailed. Resistance is moderate (0.6) from public health advocates and developing nations, but it faces strong institutional counter-pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pharmaceutical innovators, this is a legitimate 'rope' that coordinates global innovation and protects their investments. From the perspective of low-income states and patients, it operates as a 'snare' that extracts wealth and denies access to essential medicines. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical innovators and developed nations are clear beneficiaries (low directionality), gaining from the extended monopolies and market access. Low-income states and patients are clear victims (high directionality), bearing the costs of high drug prices and limited access. WTO dispute panels act as agenda-setters, their interpretations often reinforcing this strong exclusivity reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (incentivizing innovation) is still live, but its function has drifted significantly towards extraction. The high extractiveness and suppression, coupled with the contested status of the founding problem, indicate that while coordination exists, it is heavily skewed. This prevents mislabeling it as a pure 'rope' (which would ignore the extraction) or a pure 'snare' (which would ignore the genuine, albeit distorted, coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretation_of_flexibilities,
    'To what extent are the ''flexibilities'' (e.g., compulsory licensing, parallel imports) within the TRIPS agreement genuinely available to developing countries under this reading, versus being effectively nullified by enforcement mechanisms?',
    'Analysis of WTO dispute panel rulings and bilateral trade agreements, specifically examining the conditions and frequency of successful invocation of flexibilities by developing countries.',
    'If flexibilities are found to be effectively nullified, the constraint''s suppression and extractiveness are higher than currently estimated, pushing it closer to a pure Snare. If some genuine flexibility remains, it retains its Tangled Rope character.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretation_of_flexibilities, empirical, 'Ambiguity regarding the practical availability of TRIPS public health flexibilities.').

omega_variable(
    innovation_incentive_efficacy,
    'Does the strong exclusivity reading of TRIPS genuinely lead to increased pharmaceutical innovation that benefits global public health, or does it primarily incentivize ''me-too'' drugs and profit-driven research for wealthy markets?',
    'Longitudinal studies correlating TRIPS implementation with R&D output, drug discovery for neglected diseases, and access to essential medicines in developing countries.',
    'If the innovation incentive is weak or misdirected, the coordination function of the constraint is undermined, strengthening the argument for it being primarily extractive. If strong, it reinforces the ''tangled_rope'' classification by validating the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_efficacy, empirical, 'Uncertainty about the actual impact of strong patent protection on socially beneficial pharmaceutical innovation.').

omega_variable(
    legitimacy_of_wto_interpretive_authority,
    'Is the WTO dispute settlement body''s interpretive authority over TRIPS legitimate, given the significant public health implications and the power asymmetry between member states?',
    'Analysis of international legal scholarship on treaty interpretation, state practice, and the principle of ''common but differentiated responsibilities'' in international law. This is a conceptual question about the source and scope of interpretive power.',
    'If the legitimacy is widely contested or found to be weak, the constraint''s suppression is less stable and more reliant on raw power, potentially shifting its classification towards a Snare from the perspective of victim states. If robust, it reinforces the institutional nature of the Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_wto_interpretive_authority, conceptual, 'The legitimacy of the WTO''s binding interpretive authority over TRIPS, especially concerning public health.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(trip_tr_t2000, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(trip_tr_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(trip_tr_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(trip_tr_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2015, 0.09).
narrative_ontology:measurement(trip_tr_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(trip_tr_t2024, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(trip_be_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 1995, 0.7).
narrative_ontology:measurement(trip_be_t2000, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(trip_be_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(trip_be_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(trip_be_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2015, 0.83).
narrative_ontology:measurement(trip_be_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement(trip_be_t2024, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement(trip_su_t2000, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(trip_su_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2005, 0.85).
narrative_ontology:measurement(trip_su_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2010, 0.87).
narrative_ontology:measurement(trip_su_t2015, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2015, 0.88).
narrative_ontology:measurement(trip_su_t2020, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2020, 0.89).
narrative_ontology:measurement(trip_su_t2024, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_supply_chains).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, global_health_equity).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, access_to_medicines).

% DUAL FORMULATION NOTE:
% This constraint is the 'strong_exclusivity_reading' of the TRIPS Agreement interpretive kernel. It coexists with the 'public_health_flexibility_reading' and is influenced by the 'dispute_settlement_interpretive_authority' reading. Each reading represents a distinct structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
