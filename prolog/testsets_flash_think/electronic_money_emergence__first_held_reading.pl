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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: electronic_money_emergence__first_held_reading
 *   human_readable: Electronic Money Emergence: First Institutional Holding
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This constraint defines the emergence of electronic money as a discrete
 *   institutional event: the point at which the first institutional bearer
 *   (e.g., a central bank or commercial bank) formally held dematerialized
 *   currency in a form distinguishable from physical notes. This reading
 *   emphasizes legal and regulatory recognition as the observable threshold
 *   for an ontological transition in the nature of money. It is distinct from
 *   the conceptual possibility of digital money or its retroactive
 *   statistical classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.15).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.1).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, mountain).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "Electronic Money Emergence: First Institutional Holding").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic_history/monetary_theory/technology_studies").

domain_priors:emerges_naturally(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, 'b9560c81-91b9-4dee-a4cc-dba7668d54ea').
narrative_ontology:cs_kernel_codification('b9560c81-91b9-4dee-a4cc-dba7668d54ea', formalized).
narrative_ontology:cs_authority_grounding('b9560c81-91b9-4dee-a4cc-dba7668d54ea', lineage).
narrative_ontology:cs_interpretation_layer_present('b9560c81-91b9-4dee-a4cc-dba7668d54ea').
narrative_ontology:cs_reading_relation('b9560c81-91b9-4dee-a4cc-dba7668d54ea', electronic_money_emergence__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('b9560c81-91b9-4dee-a4cc-dba7668d54ea', electronic_money_emergence__m4_m5_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('b9560c81-91b9-4dee-a4cc-dba7668d54ea', foundational, institutional_action_defines_monetary_ontology).
narrative_ontology:cs_axiom_status(institutional_action_defines_monetary_ontology, holdable).
narrative_ontology:cs_axiom_grounding('b9560c81-91b9-4dee-a4cc-dba7668d54ea', institutional_action_defines_monetary_ontology, conventional).
narrative_ontology:cs_axiom('b9560c81-91b9-4dee-a4cc-dba7668d54ea', secondary, dematerialization_requires_formal_recognition).
narrative_ontology:cs_axiom_status(dematerialization_requires_formal_recognition, holdable).
narrative_ontology:cs_axiom_grounding('b9560c81-91b9-4dee-a4cc-dba7668d54ea', dematerialization_requires_formal_recognition, conventional).
narrative_ontology:cs_reference_frame('b9560c81-91b9-4dee-a4cc-dba7668d54ea', institutional_recognition_of_dematerialized_value).
narrative_ontology:cs_drift_state('b9560c81-91b9-4dee-a4cc-dba7668d54ea', contemporary_digital_finance, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b9560c81-91b9-4dee-a4cc-dba7668d54ea', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_banks).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, financial_institutions).
narrative_ontology:constraint_vindicates(electronic_money_emergence__first_held_reading, institutional_definition_of_money).
narrative_ontology:constraint_vindicates(electronic_money_emergence__first_held_reading, dematerialization_of_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and regulate the nature of money, including its electronic forms. They benefit from the clarity and control afforded by a recognized institutional definition of electronic money.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, central_banks, agenda_setter,
    institutional, generational, analytical, universal).

% Are the primary institutional bearers of dematerialized currency. They benefit from the legal and operational framework that defines and legitimizes electronic money as a distinct asset class.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, financial_institutions, beneficiary,
    powerful, biographical, mobile, global).

% Interpret and define the legal status and implications of electronic money, contributing to the formal recognition and understanding of its emergence.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% Document and analyze the historical process of electronic money's emergence, providing context and evidence for its institutional definition.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, economic_historians, observer,
    analytical, generational, analytical, universal).

% Advocate for the primacy of physical notes and coins, and would object to the dematerialization of currency. Their perspective is largely excluded from the institutional processes that define electronic money's emergence.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, physical_currency_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__first_held_reading, diffuse).
narrative_ontology:fixing_cost_class(electronic_money_emergence__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common institutional understanding and legal framework for a new form of money, enabling its secure, standardized, and regulated use across financial systems.
% TRANSFER_FUNCTION: Defines the ontological shift of value representation from physical to dematerialized forms, allowing for the institutional transfer and accounting of electronic value.
% ABSENT_VOICES: Advocates for purely physical or commodity-backed money, who would argue against the very concept of dematerialized institutional currency, are structurally excluded from the definitional process.
% DISAPPEARANCE_RATIONALE: If the institutional recognition of electronic money vanished overnight, the entire modern financial system, built on the holding and transaction of dematerialized value, would collapse. Global commerce, banking, and central bank operations would cease to function as currently understood.
% FOUNDING_PROBLEM: How to legally and institutionally recognize, manage, and regulate value that is no longer represented by physical notes or coins, but by digital entries in ledgers held by financial entities.
% FOUNDING_PROBLEM_CORROBORATION: Legal frameworks, central bank policies, and global financial industry standards corroborate the ongoing need for this definition, as the nature of digital money continues to evolve with new technologies like cryptocurrencies and CBDCs.
narrative_ontology:disappearance_verdict(electronic_money_emergence__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(electronic_money_emergence__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__first_held_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is claimed as a Mountain because it describes a fundamental, widely recognized institutional fact that, once established, largely persists. Its extractiveness and suppression are low because it's primarily a descriptive boundary and a framework for coordination, rather than an active mechanism for rent extraction or coercion. The slight increase in extractiveness over time reflects the growing institutional control and benefits derived from this established definition. Theater ratio is very low as the recognition is functional and not performative.
 *
 * PERSPECTIVAL GAP:
 *   While this reading presents the emergence as a stable institutional fact, other readings (e.g., 'became_thinkable_reading' or 'm4_m5_collapse_reading') would frame the emergence differently, leading to different classifications. This constraint focuses solely on the institutional holding perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks and financial institutions are beneficiaries, gaining clarity, control, and a new asset class from this definition. Legal scholars and economic historians act as observers, analyzing and documenting the phenomenon. Advocates for physical currency are excluded, as their perspective is outside the institutional framework defining this emergence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_fact_vs_natural_law,
    'Is the emergence of electronic money, as defined by institutional holding, a natural evolution of monetary systems or a constructed boundary that benefits specific institutional actors?',
    'Analysis of historical counterfactuals where institutional recognition was withheld or alternative definitions were adopted, to see if the ''emergence'' still occurred in the same form. This would involve examining the role of power in shaping monetary definitions.',
    'If primarily constructed, the constraint''s classification would shift from a descriptive Mountain to a more active, potentially extractive, type (e.g., Tangled Rope or Rope) that coordinates institutional power and benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_fact_vs_natural_law, conceptual, 'Ambiguity between natural institutional evolution and constructed definition of electronic money.').

omega_variable(
    timing_of_emergence_ambiguity,
    'Does the ''first institutional holding'' accurately capture the moment of electronic money''s emergence, or is it merely one observable threshold among others (e.g., conceptual possibility, widespread public adoption, or statistical reclassification)?',
    'Comparative analysis with other proposed emergence criteria, evaluating their explanatory power and consistency across different historical contexts and monetary systems.',
    'If other thresholds prove more foundational, this reading''s ''emergence'' claim might be reclassified as a secondary effect or a specific institutional manifestation, rather than the primary emergence event.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(timing_of_emergence_ambiguity, empirical, 'Ambiguity regarding the precise timing and nature of electronic money''s emergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__first_held_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__first_held_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__first_held_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__first_held_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(elec_tr_t2010, electronic_money_emergence__first_held_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(elec_tr_t2020, electronic_money_emergence__first_held_reading, theater_ratio, 2020, 0.05).

% Extraction over time
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__first_held_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__first_held_reading, base_extractiveness, 1980, 0.11).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__first_held_reading, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__first_held_reading, base_extractiveness, 2000, 0.13).
narrative_ontology:measurement(elec_be_t2010, electronic_money_emergence__first_held_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(elec_be_t2020, electronic_money_emergence__first_held_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1970, electronic_money_emergence__first_held_reading, suppression_requirement, 1970, 0.08).
narrative_ontology:measurement(elec_su_t1980, electronic_money_emergence__first_held_reading, suppression_requirement, 1980, 0.09).
narrative_ontology:measurement(elec_su_t1990, electronic_money_emergence__first_held_reading, suppression_requirement, 1990, 0.09).
narrative_ontology:measurement(elec_su_t2000, electronic_money_emergence__first_held_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(elec_su_t2010, electronic_money_emergence__first_held_reading, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(elec_su_t2020, electronic_money_emergence__first_held_reading, suppression_requirement, 2020, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__first_held_reading, information_standard).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'electronic_money_emergence' kernel, each defining emergence differently. This reading focuses on institutional holding and formal recognition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
