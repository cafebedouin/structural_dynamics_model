% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__first_held_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: electronic_money_emergence__first_held_reading
 *   human_readable: Emergence of Electronic Money (First Held Reading)
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'first_held_reading' of the
 *   'electronic_money_emergence' kernel. It defines the emergence of digital
 *   money as a discrete institutional event, marked by the first time an
 *   institutional bearer held dematerialized currency in a form
 *   distinguishable from physical notes. This reading emphasizes legal and
 *   regulatory recognition as the observable threshold for this ontological
 *   transition. The constraint is classified as a Mountain because it
 *   describes a historical fact of institutional recognition, which, once
 *   established, is unchangeable and not subject to extraction or
 *   suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.05).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.0).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, mountain).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "Emergence of Electronic Money (First Held Reading)").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic_history/monetary_theory/technology_studies").

domain_priors:emerges_naturally(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, '1f9eabe8-6c75-4d53-9534-37399356755b').
narrative_ontology:cs_kernel_codification('1f9eabe8-6c75-4d53-9534-37399356755b', formalized).
narrative_ontology:cs_authority_grounding('1f9eabe8-6c75-4d53-9534-37399356755b', lineage).
narrative_ontology:cs_interpretation_layer_present('1f9eabe8-6c75-4d53-9534-37399356755b').
narrative_ontology:cs_reading_relation('1f9eabe8-6c75-4d53-9534-37399356755b', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f9eabe8-6c75-4d53-9534-37399356755b', electronic_money_emergence__m4_m5_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('1f9eabe8-6c75-4d53-9534-37399356755b', foundational, emergence_is_discrete_institutional_event).
narrative_ontology:cs_axiom_status(emergence_is_discrete_institutional_event, holdable).
narrative_ontology:cs_axiom_grounding('1f9eabe8-6c75-4d53-9534-37399356755b', emergence_is_discrete_institutional_event, conventional).
narrative_ontology:cs_axiom('1f9eabe8-6c75-4d53-9534-37399356755b', secondary, observable_threshold_tied_to_legal_recognition).
narrative_ontology:cs_axiom_status(observable_threshold_tied_to_legal_recognition, holdable).
narrative_ontology:cs_axiom_grounding('1f9eabe8-6c75-4d53-9534-37399356755b', observable_threshold_tied_to_legal_recognition, conventional).
narrative_ontology:cs_reference_frame('1f9eabe8-6c75-4d53-9534-37399356755b', institutional_measurement_paradigm).
narrative_ontology:cs_drift_state('1f9eabe8-6c75-4d53-9534-37399356755b', contemporary_digital_asset_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1f9eabe8-6c75-4d53-9534-37399356755b', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_banks).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, financial_institutions).
narrative_ontology:constraint_vindicates(electronic_money_emergence__first_held_reading, institutional_measurement_doctrine).
narrative_ontology:constraint_vindicates(electronic_money_emergence__first_held_reading, legal_recognition_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear, institutionally measurable definition of electronic money, which aids in monetary policy, regulation, and statistical reporting. Their authority is reinforced by the discrete, observable nature of this emergence.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, central_banks, beneficiary,
    institutional, generational, analytical, global).

% Benefit from the legal and regulatory clarity provided by an institutionally recognized form of electronic money, enabling them to innovate and operate within defined boundaries. This clarity reduces legal and operational risk.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, financial_institutions, beneficiary,
    organized, biographical, constrained, global).

% Analyze the historical and theoretical implications of electronic money's emergence as a discrete institutional event. This reading provides a clear, measurable point of transition for their models.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, monetary_theorists, observer,
    analytical, civilizational, analytical, universal).

% Examine the legal frameworks and precedents established by the institutional recognition of electronic money. This reading grounds their analysis in concrete legal and regulatory shifts.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, institutionally recognized definition and starting point for electronic money, enabling coordinated legal, regulatory, and economic frameworks for its management and use.
% TRANSFER_FUNCTION: Establishes the ontological boundary for what counts as 'electronic money' for institutional purposes, transferring definitional authority to legal and regulatory bodies.
% ABSENT_VOICES: Historians of technology or social theorists who emphasize earlier conceptual or technical precursors might argue that this reading overlooks the 'pre-institutional' emergence of digital money, but their perspective is outside the institutional measurement frame.
% DISAPPEARANCE_RATIONALE: The historical fact of institutional recognition, once it occurred, is immutable. If this constraint (as a reading) vanished, the historical event itself would not change, though its interpretation might revert to a more diffuse or conceptual understanding.
% FOUNDING_PROBLEM: The problem of defining and regulating a new form of currency that lacked physical form, requiring a clear institutional threshold for its legal and economic integration.
% FOUNDING_PROBLEM_CORROBORATION: Central banks and financial regulators continue to grapple with defining and regulating new forms of digital assets, corroborating the ongoing relevance of establishing clear institutional thresholds for monetary categories. Economic historians also attest to the historical challenge of integrating novel currency forms into existing systems.
narrative_ontology:disappearance_verdict(electronic_money_emergence__first_held_reading, world_unchanged).
narrative_ontology:founding_problem_status(electronic_money_emergence__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(electronic_money_emergence__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__first_held_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__first_held_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, ExtMetricName, E),
    domain_priors:suppression_score(electronic_money_emergence__first_held_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(electronic_money_emergence__first_held_reading),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(electronic_money_emergence__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness, suppression, and theater_ratio are all near zero because this reading describes a historical event of institutional recognition, not an ongoing extractive mechanism. The 'emergence' is treated as a factual, non-negotiable point in time from this perspective. Accessibility collapse is high (0.9) because, from an institutional perspective, alternatives to this definition (e.g., purely conceptual emergence) are largely irrelevant for practical policy and legal frameworks. Resistance is zero because the historical fact itself is not resisted, though its interpretation is contested.
 *
 * PERSPECTIVAL GAP:
 *   Other readings of electronic money's emergence (e.g., 'became_thinkable_reading' or 'm4_m5_collapse_reading') would yield different classifications and metric profiles, as they focus on conceptual possibility or statistical artifacts rather than discrete institutional events. This reading provides a clear, measurable point of transition, which benefits institutions seeking to define and regulate digital currency.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks and financial institutions are beneficiaries because this reading provides a clear, institutionally measurable definition of electronic money, which aids in monetary policy, regulation, and statistical reporting. This clarity reinforces their authority and reduces risk. Monetary theorists and legal scholars are observers, benefiting from a clear historical marker for their analytical work.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint, as a Mountain, is not subject to mandatrophy. It describes a historical event of institutional recognition, which does not have a 'mandate' that can outlive its function. The question of its 'naturalness' is addressed by the omegas, acknowledging the contestability of this specific definition of 'emergence' against other readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''Mountain'' describing an immutable historical fact, or is its ''naturalness'' a consequence of adopting the ''first_held_reading'' of the ''electronic_money_emergence'' kernel?',
    'Comparison with sibling readings: if other readings (e.g., ''became_thinkable_reading'') yield different classifications (e.g., Rope or Snare), it suggests the ''Mountain'' classification is reading-dependent rather than intrinsic to the phenomenon.',
    'If the ''Mountain'' classification is reading-dependent, the constraint''s effective naturalness is lower, and its classification might shift to a ''False Summit Mountain'' or even a ''Tangled Rope'' if beneficiaries actively defend this specific interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity between intrinsic naturalness and reading-dependent naturalness for this kernel.').

omega_variable(
    institutional_vs_conceptual_emergence,
    'Does the ''first_held_reading'' adequately capture the full emergence of digital money, or does it exclude earlier conceptual or technical precursors that are equally valid definitions of ''emergence''?',
    'Historical and technological analysis of pre-institutional digital payment systems and conceptualizations of dematerialized currency. If significant precursors are identified, the ''first_held_reading'' might be seen as a narrow, institutionally biased definition.',
    'If earlier precursors are deemed equally valid, the ''first_held_reading'' might be reclassified as a ''Tangled Rope'' or ''Snare'' if institutional actors actively suppress alternative historical narratives to maintain their definitional authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_conceptual_emergence, empirical, 'Whether institutional recognition is the sole valid criterion for ''emergence''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__first_held_reading, theater_ratio, 1970, 0.0).
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__first_held_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__first_held_reading, theater_ratio, 1990, 0.0).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__first_held_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(elec_tr_t2010, electronic_money_emergence__first_held_reading, theater_ratio, 2010, 0.0).
narrative_ontology:measurement(elec_tr_t2020, electronic_money_emergence__first_held_reading, theater_ratio, 2020, 0.0).

% Extraction over time
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__first_held_reading, base_extractiveness, 1970, 0.05).
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__first_held_reading, base_extractiveness, 1980, 0.05).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__first_held_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__first_held_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(elec_be_t2010, electronic_money_emergence__first_held_reading, base_extractiveness, 2010, 0.05).
narrative_ontology:measurement(elec_be_t2020, electronic_money_emergence__first_held_reading, base_extractiveness, 2020, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1970, electronic_money_emergence__first_held_reading, suppression_requirement, 1970, 0.0).
narrative_ontology:measurement(elec_su_t1980, electronic_money_emergence__first_held_reading, suppression_requirement, 1980, 0.0).
narrative_ontology:measurement(elec_su_t1990, electronic_money_emergence__first_held_reading, suppression_requirement, 1990, 0.0).
narrative_ontology:measurement(elec_su_t2000, electronic_money_emergence__first_held_reading, suppression_requirement, 2000, 0.0).
narrative_ontology:measurement(elec_su_t2010, electronic_money_emergence__first_held_reading, suppression_requirement, 2010, 0.0).
narrative_ontology:measurement(elec_su_t2020, electronic_money_emergence__first_held_reading, suppression_requirement, 2020, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
