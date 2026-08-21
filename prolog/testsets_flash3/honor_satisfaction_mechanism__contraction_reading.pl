% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__contraction_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__contraction_reading
 *   human_readable: Dueling as Cognitively Unthinkable (Contraction Reading)
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This constraint describes the historical process by which dueling, as a
 *   mechanism for honor satisfaction, became not merely illegal or socially
 *   frowned upon, but cognitively unthinkable – a category-level
 *   impossibility within the prevailing normative system. This 'contraction
 *   reading' posits a fundamental shift in social cognition, where the very
 *   concept of dueling as a legitimate act was evacuated from the possibility
 *   space. It is claimed as a Mountain because its persistence is due to a
 *   deep-seated normative shift, not active enforcement against a live
 *   alternative. The metrics reflect this: extremely low extractiveness (it
 *   doesn't 'take' anything, it's an absence), high suppression (the
 *   cognitive impossibility itself), and zero theater (no performance needed
 *   for something unthinkable).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__contraction_reading, 0.05).
domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, 0.95).
domain_priors:theater_ratio(honor_satisfaction_mechanism__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_mechanism__contraction_reading, "Dueling as Cognitively Unthinkable (Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__contraction_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:emerges_naturally(honor_satisfaction_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__contraction_reading, '6b4515e0-f53b-45d6-89a0-9c9128e513e2').
narrative_ontology:cs_kernel_codification('6b4515e0-f53b-45d6-89a0-9c9128e513e2', implicit).
narrative_ontology:cs_authority_grounding('6b4515e0-f53b-45d6-89a0-9c9128e513e2', practice).
narrative_ontology:cs_interpretation_layer_present('6b4515e0-f53b-45d6-89a0-9c9128e513e2').
narrative_ontology:cs_reading_relation('6b4515e0-f53b-45d6-89a0-9c9128e513e2', honor_satisfaction_mechanism__decline_reading, forecloses).
narrative_ontology:cs_reading_relation('6b4515e0-f53b-45d6-89a0-9c9128e513e2', honor_satisfaction_mechanism__composite_reading, forecloses).
narrative_ontology:cs_axiom('6b4515e0-f53b-45d6-89a0-9c9128e513e2', foundational, dueling_is_cognitively_inaccessible).
narrative_ontology:cs_axiom_status(dueling_is_cognitively_inaccessible, holdable).
narrative_ontology:cs_axiom_grounding('6b4515e0-f53b-45d6-89a0-9c9128e513e2', dueling_is_cognitively_inaccessible, conventional).
narrative_ontology:cs_axiom('6b4515e0-f53b-45d6-89a0-9c9128e513e2', secondary, state_monopoly_on_violence_is_naturalized).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_is_naturalized, holdable).
narrative_ontology:cs_axiom_grounding('6b4515e0-f53b-45d6-89a0-9c9128e513e2', state_monopoly_on_violence_is_naturalized, conventional).
narrative_ontology:cs_reference_frame('6b4515e0-f53b-45d6-89a0-9c9128e513e2', dueling_as_unthinkable_norm).
narrative_ontology:cs_drift_state('6b4515e0-f53b-45d6-89a0-9c9128e513e2', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6b4515e0-f53b-45d6-89a0-9c9128e513e2', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__contraction_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, state_legal_system).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__contraction_reading, bourgeois_public_sphere).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the cognitive shift away from dueling, as it consolidates the state's monopoly on legitimate violence and legal recourse, reducing challenges to its authority.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, state_legal_system, beneficiary,
    institutional, generational, arbitrage, national).

% Benefits from the disappearance of dueling as a social norm, fostering a more 'rational' and less violent mode of conflict resolution aligned with its values of order and civility.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, bourgeois_public_sphere, beneficiary,
    organized, biographical, mobile, regional).

% Historically, this group would have been the primary participants in dueling. In this reading, their entire framework for honor and satisfaction becomes obsolete, making dueling not just illegal but culturally incomprehensible to new generations.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, honor_bound_aristocracy, excluded,
    powerless, generational, identity_locked, local).

% Study the historical processes by which dueling became unthinkable, analyzing the cognitive and normative shifts that led to its structural disappearance from the social imagination.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__contraction_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This constraint, as a cognitive impossibility, coordinates social behavior by removing dueling from the set of thinkable actions for resolving honor disputes, thereby channeling conflict into legal or social (non-violent) means.
% TRANSFER_FUNCTION: It transfers the 'right' to violence from individuals to the state, and shifts the burden of honor satisfaction from personal combat to social reputation and legal redress.
% ABSENT_VOICES: The 'honor-bound' individuals of previous eras, for whom dueling was a legitimate and necessary mechanism, are absent. Their worldview, which made dueling thinkable, has been superseded.
% DISAPPEARANCE_RATIONALE: If the cognitive impossibility of dueling 'disappeared' overnight, it would mean dueling became thinkable again. This would represent a fundamental shift in social norms and legal frameworks, effectively a 'rearrangement' of the world, but the constraint itself (as an absence) would not 'disappear' as it is already a non-presence.
% FOUNDING_PROBLEM: The problem of uncontrolled private violence and challenges to state authority posed by the practice of dueling.
% FOUNDING_PROBLEM_CORROBORATION: Legal codes and historical sociological analyses attest that the problem of dueling as a widespread social practice is dead, having been replaced by state monopoly on violence and alternative social mechanisms for honor. No contemporary benefiting party genuinely argues dueling is a live problem requiring its current 'unthinkability'.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_satisfaction_mechanism__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05 declining to 0.01) reflects that this constraint is not about active extraction, but about the absence of a practice. Its 'cost' is the loss of a former social mechanism, but this is framed as a societal gain in this reading. Suppression is extremely high (0.95 rising to 0.99) because the constraint's power lies in making dueling cognitively inaccessible; it's not just illegal, but beyond the realm of consideration for most. Accessibility collapse is near total (0.98) as alternatives (dueling) are not just suppressed but conceptually unavailable. Resistance is negligible (0.01) because there's no active constituency fighting for the 'right' to duel in this period, as the cognitive shift has already occurred.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and the emerging bourgeois society, this is a natural and beneficial evolution. From the perspective of the historical aristocracy, it represents a loss of a fundamental aspect of their identity and social order, though this perspective is largely 'absent' in the period this constraint describes.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legal system and the bourgeois public sphere are beneficiaries, as the disappearance of dueling consolidates their authority and normative frameworks. The 'honor-bound aristocracy' of previous eras, while not actively 'victims' in the traditional sense, are structurally excluded as their entire social code becomes obsolete, making their exit options 'identity_locked' to a defunct system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_vs_structural_disappearance,
    'Is dueling truly ''cognitively unthinkable'' (a category-level impossibility), or merely structurally suppressed by state power and social stigma?',
    'Analysis of historical primary sources (diaries, letters, legal records) for evidence of internal deliberation or temptation to duel, even when illegal. If such evidence is widespread, it suggests suppression rather than cognitive impossibility.',
    'If merely suppressed, the constraint would be reclassified from Mountain to Snare or Tangled Rope, as it would imply active enforcement against a live (though suppressed) alternative, rather than a fundamental normative shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_vs_structural_disappearance, empirical, 'Distinguishing between internal cognitive shift and external structural suppression.').

omega_variable(
    reading_framing_ambiguity,
    'Is the ''contraction_reading'' a more accurate description of the historical process than the ''decline_reading'' or ''composite_reading''?',
    'Further historical research comparing the explanatory power of each reading against the full range of available evidence, particularly focusing on the qualitative nature of the shift in social norms and individual agency.',
    'If a sibling reading (e.g., ''composite_reading'') is found to be more accurate, this constraint would be superseded by a different structural classification, reflecting a more complex interplay of factors than a simple cognitive contraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Ambiguity in the most appropriate historical framing of dueling''s disappearance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__contraction_reading, 1850, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1850, 0.0).
narrative_ontology:measurement(hono_tr_t1875, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1875, 0.0).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1900, 0.0).
narrative_ontology:measurement(hono_tr_t1925, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1925, 0.0).
narrative_ontology:measurement(hono_tr_t1950, honor_satisfaction_mechanism__contraction_reading, theater_ratio, 1950, 0.0).

% Extraction over time
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1850, 0.05).
narrative_ontology:measurement(hono_be_t1875, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1875, 0.04).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1900, 0.03).
narrative_ontology:measurement(hono_be_t1925, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1925, 0.02).
narrative_ontology:measurement(hono_be_t1950, honor_satisfaction_mechanism__contraction_reading, base_extractiveness, 1950, 0.01).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1850, 0.95).
narrative_ontology:measurement(hono_su_t1875, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1875, 0.96).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1900, 0.97).
narrative_ontology:measurement(hono_su_t1925, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1925, 0.98).
narrative_ontology:measurement(hono_su_t1950, honor_satisfaction_mechanism__contraction_reading, suppression_requirement, 1950, 0.99).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, state_monopoly_on_violence).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__contraction_reading, bourgeois_civility_norms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
