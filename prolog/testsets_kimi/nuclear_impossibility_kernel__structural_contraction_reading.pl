% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__structural_contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: nuclear_impossibility_kernel__structural_contraction_reading
 *   human_readable: Nuclear War as Structurally Unreachable (Structural Contraction Reading)
 *   domain: strategic/international_relations
 *
 * SUMMARY:
 *   This constraint instantiates the structural_contraction_reading of the
 *   nuclear_impossibility_kernel. The reading holds that nuclear weapons do
 *   not merely raise the cost of war or create a credibility problem; they
 *   remove direct great-power nuclear war from the reachable set of strategic
 *   actions because mutual annihilation is guaranteed. Proxy conflicts are
 *   interpreted as substitution phenomena, not as continuations of
 *   great-power war by other means. The constraint is authored as a mountain
 *   because, within this reading, the limitation is a physical-logical
 *   boundary rather than an enforced extractive arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, 0.05).
domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, 0.05).
domain_priors:theater_ratio(nuclear_impossibility_kernel__structural_contraction_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Nuclear War as Structurally Unreachable (Structural Contraction Reading)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "strategic/international_relations").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, '49514246-ed42-44b5-ad8f-8e6b0131c21d').
narrative_ontology:cs_kernel_codification('49514246-ed42-44b5-ad8f-8e6b0131c21d', formalized).
narrative_ontology:cs_authority_grounding('49514246-ed42-44b5-ad8f-8e6b0131c21d', expertise).
narrative_ontology:cs_interpretation_layer_present('49514246-ed42-44b5-ad8f-8e6b0131c21d').
narrative_ontology:cs_reading_relation('49514246-ed42-44b5-ad8f-8e6b0131c21d', nuclear_impossibility_kernel__rational_dropout_reading, forecloses).
narrative_ontology:cs_reading_relation('49514246-ed42-44b5-ad8f-8e6b0131c21d', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('49514246-ed42-44b5-ad8f-8e6b0131c21d', foundational, mutual_annihilation_guaranteed).
narrative_ontology:cs_axiom_status(mutual_annihilation_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('49514246-ed42-44b5-ad8f-8e6b0131c21d', mutual_annihilation_guaranteed, empirically_contingent).
narrative_ontology:cs_axiom('49514246-ed42-44b5-ad8f-8e6b0131c21d', secondary, proxy_conflict_substitution_principle).
narrative_ontology:cs_axiom_status(proxy_conflict_substitution_principle, holdable).
narrative_ontology:cs_axiom_grounding('49514246-ed42-44b5-ad8f-8e6b0131c21d', proxy_conflict_substitution_principle, empirically_contingent).
narrative_ontology:cs_reference_frame('49514246-ed42-44b5-ad8f-8e6b0131c21d', nuclear_war_unreachable_set).
narrative_ontology:cs_drift_state('49514246-ed42-44b5-ad8f-8e6b0131c21d', post_cold_war_proxy_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('49514246-ed42-44b5-ad8f-8e6b0131c21d', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes direct nuclear war between nuclear-armed great powers from the set of strategically reachable outcomes by ensuring any nuclear exchange results in mutual annihilation, thereby enforcing strategic stability through logical impossibility rather than through negotiated restraint.
% TRANSFER_FUNCTION: No transfer function; the constraint operates as a physical-logical boundary rather than as an arrangement that moves resources, status, or risk between agents.
% ABSENT_VOICES: Non-nuclear states whose security is shaped by deterrence relationships but who were excluded from constructing the strategic framework; civilian populations bearing existential risk without institutional voice in strategic doctrine.
% DISAPPEARANCE_RATIONALE: If the structural impossibility dissolved and a rational path to nuclear victory emerged, the post-1945 strategic order would collapse: extended deterrence guarantees would fail, alliance structures would fragment, and arms racing would intensify as war-fighting strategies became thinkable again.
% FOUNDING_PROBLEM: The existential risk that nuclear weapons would make interstate war between nuclear powers universally catastrophic, requiring a strategic framework that accounts for the disappearance of victory.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated independently by natural science (weapons effects) and by strategic studies across multiple national contexts outside any single beneficiary institution.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__structural_contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 0.05, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near zero (0.05) because the constraint operates as a physical-logical limit, not as a rent-seeking arrangement. Suppression is near zero (0.05) because no enforcement is required to maintain the constraint; it is self-executing via the physics of nuclear arsenals. Theater ratio is minimal (0.02) because the constraint does not depend on performative maintenance. Accessibility collapse is very high (0.95) because, once the strategic geometry is understood, alternatives to avoidance collapse completely. Resistance is near zero (0.05) because states do not resist the impossibility; they reorganize strategy around it. The flat measurement series over eighty units of interval record stable mountain behavior.
 *
 * PERSPECTIVAL GAP:
 *   The gap is not between seated beneficiaries and payersâthere are noneâbut between analytical observers adopting different readings of the same kernel. An observer in the rational_dropout seat sees a high-cost choice; an observer in the structural_contraction seat sees a logical wall. The engine computes no directionality asymmetry because no agent structurally subsidizes or is extracted by the constraint itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling because it lacks the markers of atrophied institutional maintenance (no agenda setter, no enforcement decay, no rising theater ratio) and lacks the markers of extraction (no beneficiaries, no victims, no active suppression). Were the constraint to show rising theater ratio or concentrated gains to a status-quo actor, it would flag as a false summit. As authored, it presents the profile of a genuine mountain: near-zero extraction, near-zero suppression, high natural-law accessibility collapse, and no parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    annihilation_contingency,
    'Is mutual annihilation guaranteed by the physical law of nuclear effects, or contingent on force posture, arsenal size, and targeting doctrine?',
    'Counterfactual strategic simulation and historical archive analysis: if arsenals were smaller or purely counterforce-targeted, would annihilation remain guaranteed for all belligerents?',
    'If contingent on posture rather than physics, the constraint weakens from mountain toward rational_dropout or tangled_rope; if guaranteed by physical law, the structural contraction reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annihilation_contingency, empirical, 'Physical guarantee versus strategic contingency of annihilation').

omega_variable(
    contested_kernel_ambiguity,
    'Is this constraint a genuine physical limit or a stabilized strategic equilibrium that benefits status-quo powers?',
    'False summit evaluation: identify whether any party captures rents from the impossibility framing and whether the constraint persists through enforcement or through physics.',
    'If beneficiaries are identified who capture gains from the framing, the engine would reclassify via false_summit_mountain from mountain to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_kernel_ambiguity, conceptual, 'Natural-law versus constructed-equilibrium ambiguity for nuclear impossibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t0, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(nucl_tr_t16, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 16, 0.02).
narrative_ontology:measurement(nucl_tr_t32, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 32, 0.02).
narrative_ontology:measurement(nucl_tr_t48, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 48, 0.02).
narrative_ontology:measurement(nucl_tr_t64, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 64, 0.02).
narrative_ontology:measurement(nucl_tr_t80, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 80, 0.02).

% Extraction over time
narrative_ontology:measurement(nucl_be_t0, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(nucl_be_t16, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 16, 0.05).
narrative_ontology:measurement(nucl_be_t32, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 32, 0.05).
narrative_ontology:measurement(nucl_be_t48, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 48, 0.05).
narrative_ontology:measurement(nucl_be_t64, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 64, 0.05).
narrative_ontology:measurement(nucl_be_t80, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 80, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nuclear_impossibility_kernel__structural_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
