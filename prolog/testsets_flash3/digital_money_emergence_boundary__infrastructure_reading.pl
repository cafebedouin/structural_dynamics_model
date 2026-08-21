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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   This constraint defines the emergence of digital money from the
 *   perspective of the underlying financial infrastructure. It posits that
 *   digital money became 'real' when the systems for electronic transfer
 *   (like ATMs, ACH, SWIFT) were established, allowing banks to move funds
 *   digitally, even if consumers didn't yet hold digital instruments
 *   directly. This reading places the boundary in the mid-to-late 20th
 *   century, emphasizing the operational capabilities of the banking system
 *   as the defining factor. It is a Mountain because it describes a
 *   historical fact and a conceptual boundary that, from this reading's
 *   perspective, is fixed by the historical development of technology.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__infrastructure_reading, 0.15).
domain_priors:suppression_score(digital_money_emergence_boundary__infrastructure_reading, 0.05).
domain_priors:theater_ratio(digital_money_emergence_boundary__infrastructure_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__infrastructure_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__infrastructure_reading, mountain).
narrative_ontology:human_readable(digital_money_emergence_boundary__infrastructure_reading, "Digital Money Emergence Boundary (Infrastructure Reading)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__infrastructure_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:emerges_naturally(digital_money_emergence_boundary__infrastructure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__infrastructure_reading, '906b2016-8f28-469c-bf79-b6e2f39f6d96').
narrative_ontology:cs_kernel_codification('906b2016-8f28-469c-bf79-b6e2f39f6d96', formalized).
narrative_ontology:cs_authority_grounding('906b2016-8f28-469c-bf79-b6e2f39f6d96', expertise).
narrative_ontology:cs_reading_relation('906b2016-8f28-469c-bf79-b6e2f39f6d96', digital_money_emergence_boundary__conceptualization_reading, coexists_with).
narrative_ontology:cs_reading_relation('906b2016-8f28-469c-bf79-b6e2f39f6d96', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('906b2016-8f28-469c-bf79-b6e2f39f6d96', foundational, digital_money_defined_by_transfer_capability).
narrative_ontology:cs_axiom_status(digital_money_defined_by_transfer_capability, holdable).
narrative_ontology:cs_axiom_grounding('906b2016-8f28-469c-bf79-b6e2f39f6d96', digital_money_defined_by_transfer_capability, conventional).
narrative_ontology:cs_reference_frame('906b2016-8f28-469c-bf79-b6e2f39f6d96', operational_banking_system_capability).
narrative_ontology:cs_drift_state('906b2016-8f28-469c-bf79-b6e2f39f6d96', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('906b2016-8f28-469c-bf79-b6e2f39f6d96', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These entities (e.g., SWIFT, ACH operators) provide and maintain the electronic rails that enable interbank digital transfers. Their existence and function are vindicated by this definition of digital money's emergence, as it centers their role. They benefit from the continued reliance on their systems for the definition and movement of digital value.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, banking_infrastructure_providers, beneficiary,
    institutional, generational, constrained, global).

% Academics and researchers who study the evolution of money and financial systems. They analyze historical data and conceptual frameworks to define key transitions, including the emergence of digital money. Their work is to interpret and classify these historical shifts.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, financial_historians, observer,
    analytical, generational, analytical, global).

% Monetary authorities responsible for defining and regulating money supply. Their operational definitions of money often align with the ability of the banking system to move funds, making this reading relevant to their policy and statistical frameworks. They set the agenda for what counts as 'money' in official statistics.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__infrastructure_reading, central_banks, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, infrastructure-centric definition for the emergence of digital money, allowing for consistent historical analysis and policy formulation based on the operational capabilities of financial systems.
% TRANSFER_FUNCTION: This constraint defines a historical boundary, transferring conceptual clarity regarding the 'start date' of digital money from ambiguity to a specific set of technological milestones. It implicitly transfers definitional authority to the operational capabilities of financial infrastructure.
% ABSENT_VOICES: Advocates for a consumer-centric view of digital money's emergence (e.g., those who emphasize direct consumer holdings) or those who prioritize theoretical conceptualization would argue this reading is too narrow, but they are not structurally excluded from the historical debate itself.
% DISAPPEARANCE_RATIONALE: This constraint is a historical interpretation. If this specific reading vanished, the historical events (ATMs, ACH, SWIFT) would still have occurred, and digital money would still exist. The debate over its 'emergence' would simply lack this particular, infrastructure-focused boundary definition, but the underlying reality would be unaffected.
% FOUNDING_PROBLEM: The problem of precisely dating the 'birth' of digital money, given its gradual evolution and different definitional criteria.
% FOUNDING_PROBLEM_CORROBORATION: Financial historians and economists outside of specific infrastructure providers corroborate the ongoing challenge of defining digital money's emergence, acknowledging the validity of different perspectives on this historical boundary.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__infrastructure_reading, world_unchanged).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__infrastructure_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__infrastructure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness is low (0.15) because this is primarily a definitional constraint, not one that actively extracts resources. Any 'benefit' to infrastructure providers is from their historical vindication, not direct extraction. Suppression is low (0.05) as it's a conceptual boundary, not actively enforced against dissenters. Accessibility collapse is high (0.9) because, from this perspective, the historical facts of infrastructure development are largely settled, leaving little room for alternative 'emergence' points within this specific framing. Resistance is low (0.05) because the debate is conceptual, not about active enforcement. The claimed type is Mountain because it describes a fixed historical boundary and a conceptual framework that, within its own logic, is unchangeable.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap in this reading itself, as it defines a historical boundary. The 'gap' exists between this reading and other readings of the same kernel, which define the emergence of digital money differently. This reading is internally consistent in its focus on infrastructure.
 *
 * DIRECTIONALITY LOGIC:
 *   Banking infrastructure providers are beneficiaries because this reading highlights their foundational role in the emergence of digital money, validating their historical significance. Financial historians are observers, analyzing the phenomenon. Central banks are agenda-setters as their operational definitions often align with this infrastructure-centric view.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergence_criteria_ambiguity,
    'Is the ''emergence'' of digital money best defined by the operational capacity of financial infrastructure, by theoretical conceptualization, or by direct consumer access to digital instruments?',
    'A consensus among financial historians and economists on a single, universally accepted definition, or a clear policy mandate from a global financial authority.',
    'If a different criterion (e.g., consumer holdings) becomes dominant, this ''infrastructure_reading'' would be reclassified as a historical artifact or a less relevant perspective, potentially shifting its type from Mountain to Piton if its influence atrophies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergence_criteria_ambiguity, conceptual, 'Ambiguity in the primary criterion for defining digital money''s emergence.').

omega_variable(
    m4_m5_collapse_impact,
    'To what extent did the emergence of electronic bank deposits, as enabled by this infrastructure, truly ''collapse'' the distinction between M4/M5 money supply categories, or was this a gradual redefinition?',
    'Detailed historical analysis of central bank monetary statistics and academic debates from the period, focusing on the timing and nature of definitional changes.',
    'If the ''collapse'' was less distinct or more gradual than implied, the impact of this infrastructure on monetary definitions might be re-evaluated, potentially weakening the ''naturalness'' of this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m4_m5_collapse_impact, empirical, 'The precise impact of electronic transfers on traditional money supply definitions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__infrastructure_reading, 1960, 1980).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1960, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1960, 0.0).
narrative_ontology:measurement(digi_tr_t1970, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1970, 0.0).
narrative_ontology:measurement(digi_tr_t1980, digital_money_emergence_boundary__infrastructure_reading, theater_ratio, 1980, 0.0).

% Extraction over time
narrative_ontology:measurement(digi_be_t1960, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(digi_be_t1970, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(digi_be_t1980, digital_money_emergence_boundary__infrastructure_reading, base_extractiveness, 1980, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1960, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1960, 0.05).
narrative_ontology:measurement(digi_su_t1970, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1970, 0.05).
narrative_ontology:measurement(digi_su_t1980, digital_money_emergence_boundary__infrastructure_reading, suppression_requirement, 1980, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__infrastructure_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__conceptualization_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__infrastructure_reading, digital_money_emergence_boundary__consumer_holdings_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
