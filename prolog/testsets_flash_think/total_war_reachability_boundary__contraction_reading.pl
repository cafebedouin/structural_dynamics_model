% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contraction_reading, []).

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
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Total War Reachability Boundary (Contraction Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint, the 'contraction_reading' of the
 *   'total_war_reachability_boundary' kernel, asserts that the advent of
 *   nuclear weapons fundamentally and permanently contracted the strategic
 *   space, making winnable total war a physical and logical impossibility. It
 *   is claimed as a Mountain because its persistence is seen as an
 *   irreducible consequence of the physics of nuclear weapons and the
 *   strategic reality of Mutual Assured Destruction (MAD). The metrics
 *   reflect this: very low extractiveness (no one 'benefits' from
 *   species-level extinction risk), very high suppression (the absolute
 *   nature of the limit), and negligible resistance. Sibling readings include
 *   'dropping_reading' (total war is merely less probable, a Rope) and
 *   'contingent_reachability_reading' (total war is a Piton, potentially
 *   reachable with technological shifts).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.05).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.95).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Total War Reachability Boundary (Contraction Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, '54e5ddf8-32b8-4bf5-9d22-0cfa5e06d2de').
narrative_ontology:cs_kernel_codification('54e5ddf8-32b8-4bf5-9d22-0cfa5e06d2de', implicit).
narrative_ontology:cs_authority_grounding('54e5ddf8-32b8-4bf5-9d22-0cfa5e06d2de', self_enforcing).
narrative_ontology:cs_reading_relation('54e5ddf8-32b8-4bf5-9d22-0cfa5e06d2de', total_war_reachability_boundary__dropping_reading, forecloses).
narrative_ontology:cs_reading_relation('54e5ddf8-32b8-4bf5-9d22-0cfa5e06d2de', total_war_reachability_boundary__contingent_reachability_reading, forecloses).
narrative_ontology:cs_axiom('54e5ddf8-32b8-4bf5-9d22-0cfa5e06d2de', foundational, mutual_assured_destruction_is_absolute).
narrative_ontology:cs_axiom_status(mutual_assured_destruction_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('54e5ddf8-32b8-4bf5-9d22-0cfa5e06d2de', mutual_assured_destruction_is_absolute, empirically_contingent).
narrative_ontology:cs_axiom('54e5ddf8-32b8-4bf5-9d22-0cfa5e06d2de', foundational, total_war_is_species_suicide).
narrative_ontology:cs_axiom_status(total_war_is_species_suicide, holdable).
narrative_ontology:cs_axiom_grounding('54e5ddf8-32b8-4bf5-9d22-0cfa5e06d2de', total_war_is_species_suicide, empirically_contingent).
narrative_ontology:cs_reference_frame('54e5ddf8-32b8-4bf5-9d22-0cfa5e06d2de', post_nuclear_era_strategic_reality).
narrative_ontology:cs_drift_state('54e5ddf8-32b8-4bf5-9d22-0cfa5e06d2de', contemporary_strategic_thought, gap(stable, minor, true)).
narrative_ontology:cs_created_at('54e5ddf8-32b8-4bf5-9d22-0cfa5e06d2de', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, humanity).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, all_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the existential risk of nuclear war, with no collective means to opt out of the strategic reality that total war is unwinnable and catastrophic. The species itself is the ultimate victim of this constraint.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, humanity, payer,
    powerless, civilizational, trapped, universal).

% Possess and maintain nuclear arsenals, thereby enforcing the constraint of mutual assured destruction. While they are the agents of deterrence, they are also constrained by the impossibility of 'winning' a total war, making them both enforcers and ultimate victims of the system they maintain.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, nuclear_states, agenda_setter,
    institutional, generational, constrained, global).

% Study and interpret the implications of nuclear deterrence, contributing to the understanding and articulation of this strategic boundary. They analyze the constraint's persistence and potential vulnerabilities without directly experiencing its coercive force.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, strategic_analysts, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This constraint coordinates the behavior of nuclear-armed states by making total war an irrational and self-defeating option, thereby preventing its initiation through mutual deterrence.
% TRANSFER_FUNCTION: It transfers the possibility of 'victory' in total war from all states to none, replacing it with the certainty of mutual destruction and species-level existential risk.
% ABSENT_VOICES: Future generations, who have no say in the creation or maintenance of nuclear arsenals but would bear the ultimate cost of their use, are absent. Non-state actors who might acquire nuclear weapons are also excluded from the strategic dialogue that maintains this boundary.
% DISAPPEARANCE_RATIONALE: If the impossibility of winnable total war vanished overnight (e.g., through a perfect defense system or a technological breakthrough making first-strike survivable), the entire global security architecture based on deterrence would collapse. Strategic calculus would fundamentally change, potentially leading to a return to pre-nuclear thinking where total war was a viable, if costly, option, with profound geopolitical reorganization.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons, which made traditional concepts of total war obsolete and necessitated a new strategic reality where such a conflict was unwinnable.
% FOUNDING_PROBLEM_CORROBORATION: The continued absence of total war between nuclear powers, the existence of international treaties (e.g., NPT), and the consistent articulation of deterrence theory in strategic doctrines across nuclear states, corroborated by independent strategic analyses and historical observation, attest that the founding problem remains live and the constraint persists.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_reachability_boundary__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high suppression (0.95) and accessibility collapse (0.98) reflect the absolute nature of the nuclear deterrent: once understood, the option of winnable total war is effectively removed. Extractiveness is very low (0.05) because no party genuinely benefits from this constraint; rather, all are subject to its existential risk. The low theater ratio (0.05) indicates that the constraint is a fundamental reality, not a performance. Resistance is negligible (0.02) because the strategic reality is widely accepted, even if some actors occasionally test its boundaries. The temporal measurements show a rise in suppression as the reality of MAD solidified over the decades following 1945, while extractiveness and theater remained low, consistent with the emergence of a Mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear states, the constraint is a necessary evil, a 'stable' reality that prevents catastrophe. From the perspective of humanity as a whole, it is an imposed existential burden. The engine's per-seat classification will reflect this divergence, with nuclear states experiencing it as a powerful, self-enforcing limit, and humanity as a universal, inescapable threat.
 *
 * DIRECTIONALITY LOGIC:
 *   Humanity and all states are universal victims (d=1.0) as they bear the existential risk. Nuclear states, while 'agenda_setters' in maintaining deterrence, are also fundamentally constrained by the impossibility of winning total war, placing them closer to the target end (d~0.7-0.8) than a pure beneficiary. Strategic analysts are observers (d=0.5).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_or_rope_or_piton_ambiguity,
    'Is the impossibility of winnable total war a true Mountain (permanent, irreducible strategic reality), a Rope (a stable but contingent deterrence equilibrium), or a Piton (an atrophied capability that could be revived by technological change)?',
    'Empirical observation of future technological developments (e.g., perfect missile defense, novel weapon systems) and their impact on strategic stability, or a breakdown of deterrence leading to limited nuclear exchange.',
    'If it''s a Rope, the constraint is a coordination mechanism that could unravel; if a Piton, it could be overcome. Reclassification to Rope or Piton would imply a higher degree of agency and contestability than the Mountain claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mountain_or_rope_or_piton_ambiguity, conceptual, 'Ambiguity regarding the fundamental nature and permanence of the strategic boundary.').

omega_variable(
    universality_of_unwinnability,
    'Is the ''unwinnable'' status of total war truly universal for all actors, or are there scenarios (e.g., asymmetric capabilities, irrational actors, or non-state proliferation) where some actors might perceive a winnable path or a tolerable cost?',
    'Analysis of emerging strategic doctrines, proliferation to new actors, and the psychological profiles of decision-makers in extreme scenarios.',
    'If some actors perceive winnability, the constraint''s universality and absolute suppression would be undermined, potentially shifting it towards a more contingent or even extractive classification for those actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_of_unwinnability, empirical, 'Whether the perception of unwinnability is truly universal across all potential actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__contraction_reading, theater_ratio, 1945, 0.01).
narrative_ontology:measurement(tota_tr_t1955, total_war_reachability_boundary__contraction_reading, theater_ratio, 1955, 0.02).
narrative_ontology:measurement(tota_tr_t1965, total_war_reachability_boundary__contraction_reading, theater_ratio, 1965, 0.03).
narrative_ontology:measurement(tota_tr_t1975, total_war_reachability_boundary__contraction_reading, theater_ratio, 1975, 0.04).
narrative_ontology:measurement(tota_tr_t1985, total_war_reachability_boundary__contraction_reading, theater_ratio, 1985, 0.04).
narrative_ontology:measurement(tota_tr_t1995, total_war_reachability_boundary__contraction_reading, theater_ratio, 1995, 0.05).
narrative_ontology:measurement(tota_tr_t2005, total_war_reachability_boundary__contraction_reading, theater_ratio, 2005, 0.05).
narrative_ontology:measurement(tota_tr_t2015, total_war_reachability_boundary__contraction_reading, theater_ratio, 2015, 0.05).
narrative_ontology:measurement(tota_tr_t2025, total_war_reachability_boundary__contraction_reading, theater_ratio, 2025, 0.05).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1945, 0.01).
narrative_ontology:measurement(tota_be_t1955, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1955, 0.02).
narrative_ontology:measurement(tota_be_t1965, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1965, 0.03).
narrative_ontology:measurement(tota_be_t1975, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1975, 0.04).
narrative_ontology:measurement(tota_be_t1985, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1985, 0.04).
narrative_ontology:measurement(tota_be_t1995, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1995, 0.05).
narrative_ontology:measurement(tota_be_t2005, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2005, 0.05).
narrative_ontology:measurement(tota_be_t2015, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2015, 0.05).
narrative_ontology:measurement(tota_be_t2025, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2025, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1945, 0.3).
narrative_ontology:measurement(tota_su_t1955, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1955, 0.5).
narrative_ontology:measurement(tota_su_t1965, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(tota_su_t1975, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1975, 0.85).
narrative_ontology:measurement(tota_su_t1985, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1985, 0.9).
narrative_ontology:measurement(tota_su_t1995, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1995, 0.92).
narrative_ontology:measurement(tota_su_t2005, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2005, 0.93).
narrative_ontology:measurement(tota_su_t2015, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2015, 0.94).
narrative_ontology:measurement(tota_su_t2025, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2025, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
