% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__cultural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__cultural_contraction_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__cultural_contraction_reading
 *   human_readable: Honor Code as Cultural Substrate (Cultural Contraction Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint describes the transformation of the underlying cultural
 *   substrate that once supported dueling as a legitimate form of 'honor
 *   satisfaction'. In this 'cultural contraction' reading, dueling became
 *   unthinkable not primarily due to external legal prohibitions (though
 *   these existed), but because the very concept of honor and the social
 *   identity of the 'honorable man' fundamentally changed, giving way to a
 *   'culture of dignity'. This is modeled as a Mountain whose 'naturalness'
 *   (the cultural substrate) eroded, making the practice of dueling
 *   structurally impossible rather than merely suppressed. The low
 *   extractiveness and suppression reflect that the constraint is not
 *   actively enforced, but rather describes a changed reality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.05).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.02).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Honor Code as Cultural Substrate (Cultural Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, '2802f909-6917-4655-b874-8801ad4b6ce1').
narrative_ontology:cs_kernel_codification('2802f909-6917-4655-b874-8801ad4b6ce1', implicit).
narrative_ontology:cs_authority_grounding('2802f909-6917-4655-b874-8801ad4b6ce1', practice).
narrative_ontology:cs_interpretation_layer_present('2802f909-6917-4655-b874-8801ad4b6ce1').
narrative_ontology:cs_reading_relation('2802f909-6917-4655-b874-8801ad4b6ce1', honor_satisfaction_substrate__practice_decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('2802f909-6917-4655-b874-8801ad4b6ce1', honor_satisfaction_substrate__composite_overdetermined_reading, coexists_with).
narrative_ontology:cs_axiom('2802f909-6917-4655-b874-8801ad4b6ce1', foundational, honor_is_an_internal_cultural_construct).
narrative_ontology:cs_axiom_status(honor_is_an_internal_cultural_construct, holdable).
narrative_ontology:cs_axiom_grounding('2802f909-6917-4655-b874-8801ad4b6ce1', honor_is_an_internal_cultural_construct, conventional).
narrative_ontology:cs_axiom('2802f909-6917-4655-b874-8801ad4b6ce1', foundational, cultures_of_dignity_supersede_cultures_of_honor).
narrative_ontology:cs_axiom_status(cultures_of_dignity_supersede_cultures_of_honor, holdable).
narrative_ontology:cs_axiom_grounding('2802f909-6917-4655-b874-8801ad4b6ce1', cultures_of_dignity_supersede_cultures_of_honor, empirically_contingent).
narrative_ontology:cs_reference_frame('2802f909-6917-4655-b874-8801ad4b6ce1', honor_culture_as_dominant_substrate).
narrative_ontology:cs_drift_state('2802f909-6917-4655-b874-8801ad4b6ce1', post_enlightenment_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('2802f909-6917-4655-b874-8801ad4b6ce1', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, contemporary_citizens).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, historical_duellists).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, cultural_evolution_theory).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, dignity_culture_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who, in a prior cultural substrate, would have been compelled by honor to duel. In this reading, their very identity as 'honorable' shifts, making dueling unthinkable rather than merely illegal.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, historical_duellists, payer,
    moderate, biographical, identity_locked, local).

% Individuals living in a 'culture of dignity' where personal affronts are resolved through legal or social means, not violence. They benefit from the absence of dueling as a social obligation, experiencing it as a natural state.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, contemporary_citizens, beneficiary,
    moderate, generational, mobile, national).

% Academics who study the evolution of honor codes and the transition from cultures of honor to cultures of dignity. They analyze the structural changes in the underlying cultural substrate.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, cultural_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In a 'culture of honor', the honor code coordinated social responses to affronts, providing a clear (though violent) mechanism for status maintenance and conflict resolution. In a 'culture of dignity', this function is replaced by other social and legal mechanisms.
% TRANSFER_FUNCTION: The constraint's transformation transfers the burden of conflict resolution from individual violent satisfaction to collective legal and social processes, effectively eliminating the 'cost' of dueling.
% ABSENT_VOICES: No voices are 'absent' in the sense of being suppressed by this constraint, as it describes a fundamental cultural shift. Those who might wish for a return to dueling are simply operating under a different, anachronistic cultural substrate.
% DISAPPEARANCE_RATIONALE: This constraint describes a fundamental cultural shift that has already occurred. If the 'cultural contraction' that made dueling unthinkable were to 'disappear' overnight, it would imply a reversal of deep historical processes, which is not a meaningful counterfactual. The current 'culture of dignity' would remain, as the substrate has already transformed.
% FOUNDING_PROBLEM: The problem of maintaining social order and resolving personal affronts in a way that aligns with evolving societal values regarding individual worth and collective responsibility.
% FOUNDING_PROBLEM_CORROBORATION: Cultural historians and sociologists corroborate that societies continually grapple with these problems, and the shift to 'cultures of dignity' represents a particular historical solution that remains operative, albeit with new challenges. This is attested by academic research and public discourse on conflict resolution and social justice.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_substrate__cultural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because it describes a fundamental, emergent property of a cultural system. Its 'naturalness' is the deep-seated cultural logic that makes certain actions thinkable or unthinkable. The low extractiveness and suppression reflect that dueling ceased due to a shift in this underlying cultural substrate, not due to active coercion or rent-seeking. The high accessibility_collapse (0.95) and low resistance (0.01) indicate that alternatives (dueling) became almost entirely unthinkable and met no significant resistance from those who might have wished to continue the practice, as their very cultural framework had shifted.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap in this reading, as it describes a macro-level cultural transformation that is largely observed and analyzed, rather than experienced differently by active participants in an ongoing constraint. The 'unthinkability' of dueling is a shared cultural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Historical duellists, in this reading, are not 'victims' in the extractive sense, but rather agents whose identity and action-set were fundamentally reshaped by the cultural shift. Contemporary citizens are beneficiaries of a less violent social order. Cultural historians are observers of this macro-level transformation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_primacy_of_cultural_shift,
    'To what extent was the decline of dueling primarily driven by the internal transformation of the honor code (cultural contraction) versus external legal and institutional pressures?',
    'Comparative historical analysis across societies with varying legal enforcement timelines and cultural shifts, and detailed micro-historical studies of individual decisions to duel or not.',
    'If external pressures are found to be primary, this constraint would be reclassified as a Piton or Snare (exogenous enforcement) rather than a Mountain (endogenous cultural shift). If cultural contraction is primary, the Mountain classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_of_cultural_shift, empirical, 'Determining the dominant causal factor in the decline of dueling.').

omega_variable(
    definition_of_cultural_substrate,
    'Is ''cultural substrate'' a sufficiently robust concept to ground a Mountain classification, or does it risk reifying social constructs as natural laws?',
    'Conceptual analysis and philosophical debate on the nature of cultural emergence and its relationship to ''natural law'' in social systems. Examination of whether the ''unthinkability'' of dueling is truly an emergent property or a deeply internalized social norm.',
    'If ''cultural substrate'' is deemed too fluid or constructed, the Mountain classification might be challenged, potentially leading to reclassification as a deeply internalized Snare or Rope, depending on the degree of active maintenance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_cultural_substrate, conceptual, 'Conceptual grounding of ''cultural substrate'' as a Mountain-like constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1700, 0.1).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1750, 0.07).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1800, 0.05).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1850, 0.03).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1900, 0.01).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1700, 0.1).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1750, 0.07).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1800, 0.05).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1850, 0.03).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1900, 0.01).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__cultural_contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor_satisfaction_substrate' kernel, focusing on the internal cultural transformation. It is linked to sibling readings that emphasize exogenous enforcement or a composite view.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
