% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__infrastructure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__infrastructure_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: digital_money_emergence_boundary__infrastructure_reading
 *   human_readable: Digital Money Emergence Boundary (Infrastructure Reading)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint defines the emergence of digital money through the lens
 *   of the underlying infrastructure that enabled electronic transfer between
 *   financial institutions (e.g., ATMs, ACH, SWIFT). It posits that digital
 *   money exists when banks can move it electronically, even if consumers
 *   cannot directly hold it. The constraint itself is a historical boundary,
 *   treated as a Mountain due to its factual nature as a technological and
 *   institutional development. However, the control over this essential
 *   infrastructure by specific providers introduces beneficiaries, triggering
 *   a False Summit Mountain evaluation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.15).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.2).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, mountain).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence Boundary (Infrastructure Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:emerges_naturally(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, '0229dbaa-771d-46c5-95a5-8faeb950b98c').
narrative_ontology:cs_kernel_codification('0229dbaa-771d-46c5-95a5-8faeb950b98c', formalized).
narrative_ontology:cs_authority_grounding('0229dbaa-771d-46c5-95a5-8faeb950b98c', extraction).
narrative_ontology:cs_interpretation_layer_present('0229dbaa-771d-46c5-95a5-8faeb950b98c').
narrative_ontology:cs_reading_relation('0229dbaa-771d-46c5-95a5-8faeb950b98c', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('0229dbaa-771d-46c5-95a5-8faeb950b98c', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('0229dbaa-771d-46c5-95a5-8faeb950b98c', foundational, electronic_transfer_capacity_defines_digital_money).
narrative_ontology:cs_axiom_status(electronic_transfer_capacity_defines_digital_money, holdable).
narrative_ontology:cs_axiom_grounding('0229dbaa-771d-46c5-95a5-8faeb950b98c', electronic_transfer_capacity_defines_digital_money, empirically_contingent).
narrative_ontology:cs_axiom('0229dbaa-771d-46c5-95a5-8faeb950b98c', secondary, institutional_movement_precedes_consumer_access).
narrative_ontology:cs_axiom_status(institutional_movement_precedes_consumer_access, holdable).
narrative_ontology:cs_axiom_grounding('0229dbaa-771d-46c5-95a5-8faeb950b98c', institutional_movement_precedes_consumer_access, empirically_contingent).
narrative_ontology:cs_reference_frame('0229dbaa-771d-46c5-95a5-8faeb950b98c', standardized_interbank_transfer_capacity).
narrative_ontology:cs_drift_state('0229dbaa-771d-46c5-95a5-8faeb950b98c', contemporary_digital_economy, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0229dbaa-771d-46c5-95a5-8faeb950b98c', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, commercial_banks).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__infrastructure_reading, commercial_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Entities like SWIFT and ACH operators that built, maintain, and control the electronic rails for interbank transfers. They define the standards and protocols, and benefit from the indispensable nature of their services.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers, beneficiary).

% Utilize the electronic transfer infrastructure for their operations, paying fees for transactions but gaining immense efficiency and reach. Their business models are now entirely dependent on this infrastructure.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, commercial_banks, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, commercial_banks, beneficiary).

% Oversee and regulate the financial system, including the digital money infrastructure. They benefit from the stability and efficiency it provides for monetary policy and financial supervision, and can influence its evolution.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__infrastructure_reading, central_banks, observer).

% Monitor compliance and stability within the financial system, relying on the established digital transfer mechanisms. They analyze the impact of this infrastructure on market integrity and consumer protection.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, financial_regulators, observer,
    institutional, generational, analytical, national).

% While benefiting indirectly from the efficiency of digital money, they do not directly hold or transact with digital instruments at this stage of emergence, and have no direct influence over the infrastructure's design or operation.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, consumers, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__infrastructure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, secure, and efficient mechanism for interbank electronic transfer of funds, enabling a global financial system that transcends physical cash settlement.
% TRANSFER_FUNCTION: Facilitates the rapid, large-scale movement of monetary value between commercial bank accounts, from sending banks to receiving banks, replacing slower and more costly physical methods.
% ABSENT_VOICES: Theorists focused on the conceptualization of digital money would argue that the true emergence predates this infrastructure. Consumers would argue that digital money only truly emerges when they can directly hold and transact with it, not just when banks can move it.
% DISAPPEARANCE_RATIONALE: If the infrastructure for electronic interbank transfers vanished overnight, the global financial system would cease to function, leading to a catastrophic collapse of commerce and credit. All modern financial arrangements depend on it.
% FOUNDING_PROBLEM: The problem of slow, costly, and insecure physical settlement of interbank transfers, and the need for a reliable, standardized system to move monetary value electronically across institutions.
% FOUNDING_PROBLEM_CORROBORATION: Financial historians, central bank reports, and ongoing operational needs of the global banking system consistently corroborate the historical problem and the continuing, critical role of this infrastructure. Independent economic analyses confirm the efficiency gains and systemic importance.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(digital_money_emergence_boundary__infrastructure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__infrastructure_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, ExtMetricName, E),
    domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(digital_money_emergence_boundary__infrastructure_reading),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(digital_money_emergence_boundary__infrastructure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.2) reflect that the *emergence boundary itself* is a descriptive historical point, not an actively extractive or suppressive mechanism. The high accessibility collapse (0.9) indicates that once this infrastructure was in place, the previous methods for interbank transfer became largely obsolete. Resistance is low (0.1) as it represents a technological progression. The claimed type is 'mountain' because it describes a fundamental shift in the nature of money, driven by technological capacity. The presence of beneficiaries (banking infrastructure providers) on a claimed mountain is intentional, to trigger the False Summit Mountain detection, acknowledging that while the *emergence* is natural, the *control* of the resulting system can be extractive.
 *
 * PERSPECTIVAL GAP:
 *   While this reading defines emergence by institutional capacity, other readings (conceptualization, consumer holdings) would place the boundary elsewhere. The engine's classification of this 'mountain' with beneficiaries will highlight the tension between the natural emergence of a technological capacity and the subsequent institutional control and potential for extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Banking infrastructure providers are clear beneficiaries and agenda-setters, as they control the essential rails and derive value from their operation. Commercial banks are both payers (for using the infrastructure) and beneficiaries (from the efficiency it provides). Central banks and financial regulators act as institutional observers and agenda-setters, influencing the system's stability. Consumers are largely excluded from this specific definition of emergence, as their direct access to digital instruments comes later.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_ambiguity,
    'Is the ''infrastructure_reading'' the definitive emergence point for digital money, or merely one valid perspective among others?',
    'Consensus across historical and economic disciplines on a single, universally accepted definition of digital money''s emergence.',
    'If other readings are equally valid, the ''emergence'' is a contested concept, not a fixed boundary, potentially reclassifying this as a conceptual ''rope'' or ''tangled_rope'' reflecting the ongoing definitional contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_ambiguity, conceptual, 'Ambiguity regarding the definitive emergence point of digital money.').

omega_variable(
    conceptualization_vs_infrastructure_primacy,
    'Does the theoretical conceptualization of digital money (as per `conceptualization_reading`) precede and enable the infrastructure-driven emergence, or does the infrastructure''s existence drive the conceptualization?',
    'Detailed historical analysis tracing the causal links between theoretical breakthroughs and practical infrastructure development.',
    'If conceptualization is primary, this infrastructure reading describes a *realization* or *implementation* of digital money, not its fundamental emergence, potentially shifting its classification to a ''rope'' that coordinates the application of a prior concept.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptualization_vs_infrastructure_primacy, empirical, 'Causal primacy between theoretical conceptualization and infrastructure development.').

omega_variable(
    institutional_vs_consumer_emergence,
    'Does digital money truly emerge when institutions can move it electronically, or only when consumers can directly hold and transact with digital instruments (as per `consumer_holdings_reading`)?',
    'Analysis of the functional definition of ''money'' and its historical evolution, particularly regarding direct public access versus institutional intermediation.',
    'If consumer holdings are deemed the true emergence, this infrastructure reading describes an *intermediate stage* of digital money, not its full emergence, potentially reclassifying it as a ''scaffold'' that enabled a later, more complete form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_consumer_emergence, conceptual, 'Debate over whether institutional capacity or consumer access defines digital money''s emergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1967, 1997).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1967, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1967, 0.05).
narrative_ontology:measurement(digi_tr_t1972, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1972, 0.05).
narrative_ontology:measurement(digi_tr_t1977, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1977, 0.05).
narrative_ontology:measurement(digi_tr_t1982, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1982, 0.05).
narrative_ontology:measurement(digi_tr_t1987, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1987, 0.05).
narrative_ontology:measurement(digi_tr_t1992, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1992, 0.05).
narrative_ontology:measurement(digi_tr_t1997, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1997, 0.05).

% Extraction over time
narrative_ontology:measurement(digi_be_t1967, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1967, 0.1).
narrative_ontology:measurement(digi_be_t1972, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1972, 0.12).
narrative_ontology:measurement(digi_be_t1977, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1977, 0.13).
narrative_ontology:measurement(digi_be_t1982, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1982, 0.14).
narrative_ontology:measurement(digi_be_t1987, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1987, 0.15).
narrative_ontology:measurement(digi_be_t1992, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1992, 0.15).
narrative_ontology:measurement(digi_be_t1997, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1997, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1967, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1967, 0.1).
narrative_ontology:measurement(digi_su_t1972, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1972, 0.15).
narrative_ontology:measurement(digi_su_t1977, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1977, 0.18).
narrative_ontology:measurement(digi_su_t1982, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1982, 0.2).
narrative_ontology:measurement(digi_su_t1987, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1987, 0.2).
narrative_ontology:measurement(digi_su_t1992, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1992, 0.2).
narrative_ontology:measurement(digi_su_t1997, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1997, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
