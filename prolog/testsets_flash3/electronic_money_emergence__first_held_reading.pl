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
 *   human_readable: Electronic Money Emergence (First Held Reading)
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This constraint defines the emergence of electronic money as a discrete,
 *   institutionally verifiable event: the first time an institutional bearer
 *   held dematerialized currency in a form distinguishable from physical
 *   notes. This reading emphasizes observable, legal, and regulatory
 *   thresholds, making 'emergence' an ontological transition rather than a
 *   conceptual or statistical artifact. It is one reading of the
 *   'electronic_money_emergence' kernel, distinct from
 *   'became_thinkable_reading' (conceptual possibility) and
 *   'm4_m5_collapse_reading' (statistical reclassification).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.15).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.05).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, mountain).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "Electronic Money Emergence (First Held Reading)").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic_history/monetary_theory/technology_studies").

domain_priors:emerges_naturally(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, '0980511e-e80b-4851-899e-24e459c9dc95').
narrative_ontology:cs_kernel_codification('0980511e-e80b-4851-899e-24e459c9dc95', formalized).
narrative_ontology:cs_authority_grounding('0980511e-e80b-4851-899e-24e459c9dc95', lineage).
narrative_ontology:cs_interpretation_layer_present('0980511e-e80b-4851-899e-24e459c9dc95').
narrative_ontology:cs_reading_relation('0980511e-e80b-4851-899e-24e459c9dc95', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('0980511e-e80b-4851-899e-24e459c9dc95', electronic_money_emergence__m4_m5_collapse_reading, coexists_with).
narrative_ontology:cs_axiom('0980511e-e80b-4851-899e-24e459c9dc95', foundational, emergence_is_institutional_event).
narrative_ontology:cs_axiom_status(emergence_is_institutional_event, holdable).
narrative_ontology:cs_axiom_grounding('0980511e-e80b-4851-899e-24e459c9dc95', emergence_is_institutional_event, conventional).
narrative_ontology:cs_axiom('0980511e-e80b-4851-899e-24e459c9dc95', foundational, dematerialized_currency_is_distinct_from_physical).
narrative_ontology:cs_axiom_status(dematerialized_currency_is_distinct_from_physical, holdable).
narrative_ontology:cs_axiom_grounding('0980511e-e80b-4851-899e-24e459c9dc95', dematerialized_currency_is_distinct_from_physical, empirically_contingent).
narrative_ontology:cs_reference_frame('0980511e-e80b-4851-899e-24e459c9dc95', clear_institutional_demarcation).
narrative_ontology:cs_drift_state('0980511e-e80b-4851-899e-24e459c9dc95', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0980511e-e80b-4851-899e-24e459c9dc95', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_banks).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, financial_regulators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a clear, institutionally recognized definition of electronic money, which allows for precise monetary policy and regulatory oversight. The emergence of electronic money as a distinct category provides a new domain for their authority.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, central_banks, beneficiary,
    institutional, generational, analytical, national).

% Benefit from the clear institutional demarcation of electronic money, enabling them to establish specific regulatory frameworks for digital assets and transactions, ensuring stability and preventing illicit activities. This reading provides a concrete starting point for their mandate.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, financial_regulators, beneficiary,
    institutional, generational, analytical, national).

% Seek to precisely date and define the transition points in monetary history. This reading offers a specific, institutionally verifiable event for the emergence of electronic money, fitting a historical methodology focused on observable institutional shifts.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, monetary_historians, observer,
    analytical, civilizational, analytical, universal).

% Were the first entities (e.g., banks, payment processors) to hold dematerialized currency in a form distinct from physical notes, thereby creating the observable phenomenon this reading defines as 'emergence'. Their actions set the precedent for the new monetary form.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, early_institutional_bearers, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, institutionally verifiable definition for the emergence of electronic money, enabling consistent historical analysis and regulatory application across different financial systems.
% TRANSFER_FUNCTION: Establishes a conceptual boundary, transferring the 'status of emergence' from abstract possibility to concrete institutional recognition, thereby shaping how monetary history is understood and regulated.
% ABSENT_VOICES: Theorists who emphasize the conceptual or statistical emergence of digital money would argue that this reading overemphasizes institutional recognition at the expense of underlying technological or economic shifts. They are often excluded from policy-making circles that prioritize clear, actionable definitions.
% DISAPPEARANCE_RATIONALE: If this specific reading of electronic money's emergence vanished, the historical events of institutional holding would still have occurred. The world would not rearrange; rather, the *interpretation* of those events as 'emergence' would shift to alternative conceptual or statistical framings.
% FOUNDING_PROBLEM: The problem of precisely dating and defining the transition from physical to dematerialized forms of currency for historical and regulatory purposes.
% FOUNDING_PROBLEM_CORROBORATION: Monetary historians and legal scholars corroborate the ongoing need for clear definitions of monetary transitions. Central banks and financial regulators also attest to the live status of this problem, as new forms of digital assets continue to challenge existing definitions, requiring clear historical precedents.
narrative_ontology:disappearance_verdict(electronic_money_emergence__first_held_reading, world_unchanged).
narrative_ontology:founding_problem_status(electronic_money_emergence__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The constraint is classified as a Mountain because it describes an objective historical event, a 'first' institutional holding, which is unchangeable. Its extractiveness is low (0.15) as it primarily serves to define a historical fact, not to extract rents. Suppression is minimal (0.05) because the definition is largely accepted within institutional and regulatory circles. Accessibility collapse is high (0.9) as the historical event itself is fixed, leaving little room for alternative 'firsts'. Resistance is low (0.02) because the factual claim of a 'first holding' is not widely contested, though its *interpretation* as 'emergence' is.
 *
 * PERSPECTIVAL GAP:
 *   While the 'first held' event is a historical fact, the interpretation of this event as 'emergence' is where perspectival gaps arise. Those who prioritize conceptual or statistical definitions of emergence would view this reading as overly narrow or institutionally biased. However, from the perspective of institutional actors, this reading provides the necessary clarity for policy and regulation.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banks and financial regulators are beneficiaries because this reading provides a clear, actionable historical precedent for their regulatory mandates. Monetary historians also benefit from a precise, institutionally grounded definition. Early institutional bearers are the agenda-setters as their actions created the observable event. No direct victims are identified, as the constraint defines a historical fact rather than imposing costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_distinguishable_form,
    'What precise criteria define ''a form distinguishable from physical notes'' in the context of early dematerialized currency holdings?',
    'Historical legal analysis of early banking regulations and accounting practices, alongside technological specifications of early electronic payment systems.',
    'A stricter definition might push the ''first held'' date later, potentially shifting the perceived timeline of electronic money''s emergence. A looser definition might move it earlier, incorporating more rudimentary forms of dematerialized value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_distinguishable_form, empirical, 'Ambiguity in the precise technical and legal criteria for distinguishing dematerialized currency from physical notes at the point of institutional holding.').

omega_variable(
    emergence_as_event_vs_process,
    'Is ''emergence'' best understood as a discrete, datable event (as this reading claims) or as a gradual, continuous process?',
    'Conceptual analysis within monetary theory, weighing the utility of event-based definitions for policy against process-based definitions for historical understanding. This is a framing choice.',
    'If emergence is primarily a process, this reading''s focus on a ''first held'' event becomes a convenient but potentially misleading simplification, potentially influencing how policy addresses ongoing monetary innovation. If it''s an event, this reading provides a clear anchor.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergence_as_event_vs_process, conceptual, 'The fundamental conceptual disagreement over whether ''emergence'' is an event or a process, which shapes the validity of a ''first held'' definition.').

omega_variable(
    natural_law_vs_institutional_construct,
    'Is the ''emergence'' of electronic money, even when tied to an institutional ''first held'' event, a natural consequence of technological and economic evolution (a Mountain), or is it fundamentally an institutional construct shaped by regulatory and legal choices (a Snare or Tangled Rope)?',
    'Comparative historical analysis of different jurisdictions'' approaches to digital money, examining whether similar ''first held'' events occurred independently or were driven by specific policy decisions. If policy choices significantly altered the timing or nature of ''emergence'', it suggests a constructed element.',
    'If primarily a natural law, the constraint remains a Mountain. If substantially a construct, the classification could shift towards a Snare or Tangled Rope, implying that the ''emergence'' was shaped by identifiable beneficiaries and imposed costs, rather than being an inevitable historical fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_institutional_construct, conceptual, 'Ambiguity over whether the ''first held'' emergence is a natural historical fact or an institutionally constructed definition that benefits certain actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1945, electronic_money_emergence__first_held_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__first_held_reading, theater_ratio, 1970, 0.0).
narrative_ontology:measurement(elec_tr_t1995, electronic_money_emergence__first_held_reading, theater_ratio, 1995, 0.0).
narrative_ontology:measurement(elec_tr_t2024, electronic_money_emergence__first_held_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(elec_be_t1945, electronic_money_emergence__first_held_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__first_held_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(elec_be_t1995, electronic_money_emergence__first_held_reading, base_extractiveness, 1995, 0.15).
narrative_ontology:measurement(elec_be_t2024, electronic_money_emergence__first_held_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t1945, electronic_money_emergence__first_held_reading, suppression_requirement, 1945, 0.05).
narrative_ontology:measurement(elec_su_t1970, electronic_money_emergence__first_held_reading, suppression_requirement, 1970, 0.05).
narrative_ontology:measurement(elec_su_t1995, electronic_money_emergence__first_held_reading, suppression_requirement, 1995, 0.05).
narrative_ontology:measurement(elec_su_t2024, electronic_money_emergence__first_held_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__first_held_reading, information_standard).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'electronic_money_emergence' kernel. This 'first_held_reading' emphasizes institutional recognition, while 'became_thinkable_reading' focuses on conceptual possibility and 'm4_m5_collapse_reading' on statistical reclassification. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
