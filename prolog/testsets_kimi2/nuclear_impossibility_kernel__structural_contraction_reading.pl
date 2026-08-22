% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Nuclear War Structural Impossibility (Structural Contraction Reading)
 *   domain: strategic studies/international relations/nuclear deterrence theory
 *
 * SUMMARY:
 *   This constraint story instantiates the structural_contraction_reading of
 *   the nuclear_impossibility_kernel. The reading holds that nuclear weapons
 *   do not merely raise the cost of great-power war or create a deterrent
 *   paradox; they physically eliminate strategic victory from the reachable
 *   set. Mutual assured destruction is not a risk to be managed but a
 *   boundary condition of the strategic environment. Consequently, proxy wars
 *   are substitution phenomena â outlets for competition that are not
 *   continuations of nuclear strategy â and limited nuclear war is a
 *   category error. The constraint is authored as a Mountain because the
 *   reading treats it as a physical-strategic law: it would persist
 *   regardless of doctrinal advocacy, and no party captures rents from its
 *   operation. The metrics are authored independently and remain near-zero on
 *   extraction and suppression because the reading frames the impossibility
 *   as emergent from physics and logistics, not from enforceable human
 *   arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, 0.02).
domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, 0.05).
domain_priors:theater_ratio(nuclear_impossibility_kernel__structural_contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Nuclear War Structural Impossibility (Structural Contraction Reading)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "strategic studies/international relations/nuclear deterrence theory").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, '3def4645-52fe-4381-9f29-cb647b9c2cd6').
narrative_ontology:cs_kernel_codification('3def4645-52fe-4381-9f29-cb647b9c2cd6', formalized).
narrative_ontology:cs_authority_grounding('3def4645-52fe-4381-9f29-cb647b9c2cd6', expertise).
narrative_ontology:cs_interpretation_layer_present('3def4645-52fe-4381-9f29-cb647b9c2cd6').
narrative_ontology:cs_reading_relation('3def4645-52fe-4381-9f29-cb647b9c2cd6', nuclear_impossibility_kernel__rational_dropout_reading, forecloses).
narrative_ontology:cs_reading_relation('3def4645-52fe-4381-9f29-cb647b9c2cd6', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('3def4645-52fe-4381-9f29-cb647b9c2cd6', foundational, mutual_annihilation_guarantees_victory_impossibility).
narrative_ontology:cs_axiom_status(mutual_annihilation_guarantees_victory_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('3def4645-52fe-4381-9f29-cb647b9c2cd6', mutual_annihilation_guarantees_victory_impossibility, empirically_contingent).
narrative_ontology:cs_axiom('3def4645-52fe-4381-9f29-cb647b9c2cd6', secondary, proxy_war_substitution_not_continuation).
narrative_ontology:cs_axiom_status(proxy_war_substitution_not_continuation, holdable).
narrative_ontology:cs_axiom_grounding('3def4645-52fe-4381-9f29-cb647b9c2cd6', proxy_war_substitution_not_continuation, empirically_contingent).
narrative_ontology:cs_reference_frame('3def4645-52fe-4381-9f29-cb647b9c2cd6', nuclear_victory_reachable_set_empty).
narrative_ontology:cs_drift_state('3def4645-52fe-4381-9f29-cb647b9c2cd6', post_cold_war_counterforce_modernization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3def4645-52fe-4381-9f29-cb647b9c2cd6', '').
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
% COORDINATION_FUNCTION: None. The constraint is a physical boundary condition, not a coordination arrangement.
% TRANSFER_FUNCTION: No transfer. The constraint operates as a natural limit on strategic action space.
% ABSENT_VOICES: Limited nuclear war theorists and disarmament advocates contest the physical-impossibility framing but are marginalized in orthodox deterrence discourse.
% DISAPPEARANCE_RATIONALE: If the physical guarantee of mutual annihilation disappeared, strategic victory would re-enter the reachable set, alliance structures would abandon deterrence-by-impossibility, and the post-1945 great-war taboo would collapse.
% FOUNDING_PROBLEM: The recurrence of catastrophic great-power war in the industrial age.
% FOUNDING_PROBLEM_CORROBORATION: Historians attest the long peace among nuclear powers; they do not agree on whether this peace is caused by physical impossibility, rational deterrence, or geopolitical coincidence. No external party can corroborate the physical-impossibility claim because it rests on an untested counterfactual.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__structural_contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 0.02, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored at 0.02 because a genuine physical boundary does not extract from agents; it merely constrains the action space. Suppression is 0.05 because the constraint requires no active enforcement â once the physical capacity for mutual annihilation exists, no additional coercive machinery is needed to make victory unreachable. Theater ratio is 0.00 because the reading admits no performative maintenance: the constraint is not staged. Accessibility collapse is 0.92 because, once the physics is understood, the alternative (winning a nuclear war) collapses as a coherent strategic objective. Resistance is 0.15 because a minority of strategists continue to theorize limited nuclear war and counterforce options, but they do not mount sustained resistance to the boundary itself.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality derivation is required. The constraint has no beneficiaries and no victims. Any agent's relationship to the boundary is symmetric: all nuclear-armed states are equally constrained by the physical reach of adversary arsenals.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable in the standard sense. The constraint is not a mandate but a boundary condition. Mandatrophy presupposes an authored arrangement whose founding problem may have died; this reading denies that the constraint was authored to solve a problem in the first place. The R5 genealogy is included for schema completeness but does not drive classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contingent_vs_natural_boundary,
    'Is the impossibility of nuclear victory a contingent political-technical arrangement maintained by arsenals and doctrine, or a genuine physical-strategic boundary condition?',
    'Comprehensive disarmament or a counterforce technological breakthrough would test the boundary: if victory becomes thinkable, the constraint was contingent; if not, it approaches a natural boundary.',
    'If contingent, the constraint reclassifies as tangled_rope or snare maintained by human choice; if genuine, it remains mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contingent_vs_natural_boundary, conceptual, 'Contingency of the nuclear impossibility boundary').

omega_variable(
    proxy_war_substitution_validity,
    'Are proxy wars genuinely non-nuclear substitutions for great-power war, or independent phenomena that would occur even without nuclear weapons?',
    'Comparative historical analysis of pre-nuclear great-power competition versus post-1945 proxy conflict frequency and intensity.',
    'If proxy wars are independent, the structural contraction reading overclaims the reach of the nuclear boundary; if substitution, the reading''s secondary axiom is corroborated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proxy_war_substitution_validity, empirical, 'Empirical status of proxy war substitution claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
