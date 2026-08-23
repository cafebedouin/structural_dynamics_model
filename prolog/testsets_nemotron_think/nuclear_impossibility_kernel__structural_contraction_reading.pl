% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
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
 *   constraint_id: nuclear_impossibility_kernel__structural_contraction_reading
 *   human_readable: Nuclear Victory Impossibility (Structural Contraction Reading)
 *   domain: strategic/nuclear/deterrence
 *
 * SUMMARY:
 *   This constraint story instantiates the structural_contraction_reading of
 *   the nuclear_impossibility_kernel. The reading asserts that nuclear
 *   weapons created a physical impossibility: the M-set (the set of reachable
 *   strategic outcomes) contracts such that no site-expansion cell can
 *   represent a nuclear victory; mutual annihilation is guaranteed by the
 *   physics of arsenals and delivery systems. War exits the reachable set
 *   entirely. Proxy wars are substitution, not continuation — they operate in
 *   a different strategic space. This reading stands apart from the
 *   credibility_paradox_reading (deterrence requires incredible threats) and
 *   the rational_dropout_reading (victory possible but not worth the cost).
 *   The structural contraction reading claims a harder constraint: victory is
 *   not merely incredible or too costly — it is structurally absent from the
 *   possibility space.
 *
 * KEY AGENTS:
 *   - nuclear_armed_states: Primary subjects of the constraint (institutional/analytical) — their strategic options are contracted by the physical limit
 *   - deterrence_theorists: Analytical observers (analytical/analytical) — map the M-set contraction
 *   - counterforce_proponents: Would-be challengers to the constraint (institutional/constrained) — argue for site-expansion via damage limitation
 *   - non_nuclear_states: Excluded from the core constraint but affected by its shadow (organized/trapped)
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
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Nuclear Victory Impossibility (Structural Contraction Reading)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "strategic/nuclear/deterrence").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, '20f48ff4-6bfb-4924-82ec-04944c42da0d').
narrative_ontology:cs_kernel_codification('20f48ff4-6bfb-4924-82ec-04944c42da0d', formalized).
narrative_ontology:cs_authority_grounding('20f48ff4-6bfb-4924-82ec-04944c42da0d', lineage).
narrative_ontology:cs_interpretation_layer_present('20f48ff4-6bfb-4924-82ec-04944c42da0d').
narrative_ontology:cs_reading_relation('20f48ff4-6bfb-4924-82ec-04944c42da0d', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_reading_relation('20f48ff4-6bfb-4924-82ec-04944c42da0d', nuclear_impossibility_kernel__rational_dropout_reading, forecloses).
narrative_ontology:cs_axiom('20f48ff4-6bfb-4924-82ec-04944c42da0d', foundational, nuclear_victory_physically_impossible).
narrative_ontology:cs_axiom_status(nuclear_victory_physically_impossible, holdable).
narrative_ontology:cs_axiom_grounding('20f48ff4-6bfb-4924-82ec-04944c42da0d', nuclear_victory_physically_impossible, empirically_contingent).
narrative_ontology:cs_axiom('20f48ff4-6bfb-4924-82ec-04944c42da0d', secondary, proxy_wars_are_substitution_not_continuation).
narrative_ontology:cs_axiom_status(proxy_wars_are_substitution_not_continuation, holdable).
narrative_ontology:cs_axiom_grounding('20f48ff4-6bfb-4924-82ec-04944c42da0d', proxy_wars_are_substitution_not_continuation, empirically_contingent).
narrative_ontology:cs_reference_frame('20f48ff4-6bfb-4924-82ec-04944c42da0d', mutual_assured_destruction_as_physical_fact).
narrative_ontology:cs_drift_state('20f48ff4-6bfb-4924-82ec-04944c42da0d', post_cold_war_strategic_ambiguity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('20f48ff4-6bfb-4924-82ec-04944c42da0d', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, mutual_assured_destruction_doctrine).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_peace_theory).
narrative_ontology:constraint_vindicates(nuclear_impossibility_kernel__structural_contraction_reading, strategic_stability_as_physical_fact).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear arsenals; their strategic planning is bounded by the physical impossibility of victory. They cannot exit the constraint except through disarmament, which faces its own coordination barriers. They experience the constraint as a symmetric limit — it applies equally to all nuclear powers.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_armed_states, observer,
    institutional, generational, analytical, global).

% Analyze and model the M-set contraction. Their professional standing depends on accurately mapping the constraint. They benefit epistemically from the constraint's clarity (d ≈ 0.0) but do not extract from it.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, deterrence_theorists, observer,
    analytical, civilizational, analytical, universal).

% Advocate doctrines (counterforce targeting, missile defense, limited nuclear options) that attempt to expand the M-set and re-introduce victory states. They are excluded from the constraint's core logic — the physics denies their project — but they occupy institutional positions that let them resource the attempt.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, counterforce_proponents, excluded,
    institutional, biographical, constrained, global).

% Live under the shadow of the constraint without participating in the nuclear balance. They cannot exit the risk of annihilation and have no voice in the doctrines that maintain it. The constraint's physical limit protects them from nuclear war but also freezes the hierarchy that excludes them.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states, excluded,
    organized, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates by making nuclear war unwinnable, thereby stabilizing the strategic balance without need for active coordination mechanisms. It solves the coordination problem of mutual restraint by removing the payoff for defection entirely — not by raising its cost, but by eliminating the 'victory' outcome from the game tree.
% TRANSFER_FUNCTION: No transfer occurs. The constraint is not a mechanism that moves resources; it is a boundary condition that defines what outcomes exist. Any 'transfer' (arms spending, doctrine development) is a response to the constraint, not its operation.
% ABSENT_VOICES: Proponents of nuclear warfighting (counterforce theorists, limited nuclear war advocates, missile defense architects) are structurally excluded from the constraint's logic — the physics denies their project — but they occupy institutional positions that let them pursue site-expansion. Their exclusion is not political but structural: the M-set has no cell for their project.
% DISAPPEARANCE_RATIONALE: If the physical impossibility of nuclear victory ceased to hold (e.g., through perfect missile defense, disintegrated arsenals, or new physics), the strategic game would fundamentally restructure: great power war would become thinkable again, the nuclear taboo would dissolve, and the 80-year Long Peace would face its first genuine stress test.
% FOUNDING_PROBLEM: The problem of great power war in the nuclear age: how to prevent catastrophic conflict when the means of destruction exceed any political objective.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the historical record of Cold War crises (Cuban Missile Crisis, Able Archer), the continued existence of nuclear arsenals, and the consensus of strategic studies literature outside the nuclear establishments (e.g., International Relations theorists, peace researchers, disarmament advocates). Nuclear establishments themselves attest the problem is live but frame it as requiring modernization, not resolution.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is near-zero (0.05) because the constraint does not transfer resources — it defines the boundary of the possible. Suppression is near-zero (0.05) because the constraint requires no enforcement; it is a fact of physics and geometry. Theater ratio is minimal (0.02) because there is no performative maintenance of a physical law. Accessibility collapse is near-total (0.95) because once the physics of mutual assured destruction is understood, no alternative strategic pathway to victory exists within the nuclear exchange itself. Resistance is negligible (0.02) because one cannot resist a physical limit — one can only deny it or build doctrines that pretend it away. The measurement series shows remarkable stability across 80 years: the constraint's metrics are flat because the underlying physics has not changed. The slight uptick in suppression_requirement (0.02 to 0.05) reflects the growing doctrinal apparatus (missile defense, counterforce targeting) that attempts to deny the contraction — but the constraint itself remains unenforced.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute identical classifications for all seats because the constraint is a genuine Mountain with no asymmetric extraction. The perspectival gap appears only when comparing this reading to its siblings: the credibility_paradox_reading and rational_dropout_reading will show seat divergence because they describe constructed strategic dilemmas, not physical impossibilities. This reading's claim is that the gap is illusory — the other readings mistake doctrinal confusion for structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   All nuclear-armed states sit at d ≈ 0.5 (symmetric) — the constraint bounds them equally. No party benefits extractively from the impossibility itself; the benefit is negative (avoidance of catastrophe). Non-nuclear states are also symmetric but with less agency. The analytical observer seat sees d = 0.0 (beneficiary of clarity). There are no payers in the extractive sense because no transfer occurs. The constraint is a Mountain: it extracts from no one, suppresses no one, and persists regardless of belief.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint has no mandate to atrophy — it is not an institution but a physical fact. Mandatrophy is inapplicable. The founding problem (preventing great power war) remains live, but the constraint is not the solution; it is the condition that makes the problem acute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the structural contraction reading a distinct constraint from the credibility paradox and rational dropout readings of the same kernel?',
    'Compare ε values and stakeholder structures across the three readings; if ε differs materially or beneficiary/victim sets diverge, they are separate constraints per ε-invariance.',
    'If separate, each reading gets its own classification; if unified, the kernel has one ε with observer-dependent classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings instantiate one constraint or three.').

omega_variable(
    natural_law_vs_constructed_order,
    'Is the impossibility of nuclear victory a genuine physical/logical limit (Mountain) or a constructed strategic doctrine that benefits nuclear establishments (false summit)?',
    'Historical analysis of whether counterforce doctrines, limited nuclear war concepts, or missile defense programs have successfully expanded the ''winnable'' region of the M-set, versus whether they merely perform the expansion while the physical contraction holds.',
    'If false summit, FSM reclassifies to tangled_rope with nuclear establishments as beneficiaries; if genuine Mountain, classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_order, empirical, 'Natural-law vs. constructed ambiguity for a Mountain claiming physical impossibility.').

omega_variable(
    forecloses_rational_dropout,
    'Does the structural contraction reading''s core premise (victory physically impossible) logically foreclose the rational dropout reading''s premise (victory possible but cost-exceeds-benefit) within a single strategic framework?',
    'Formal modeling of the M-set: if the reachable set contains zero victory states, rational dropout''s ''possible but not worth it'' is logically impossible in that framework.',
    'If forecloses, the two readings cannot be held simultaneously by one actor; if coexists_with, they are competing but compatible framings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(forecloses_rational_dropout, conceptual, 'Logical relation between structural contraction and rational dropout readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nuc_imposs_struct_contr_tr_t0, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0, 0.01).
narrative_ontology:measurement(nuc_imposs_struct_contr_tr_t10, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 10, 0.01).
narrative_ontology:measurement(nuc_imposs_struct_contr_tr_t20, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 20, 0.01).
narrative_ontology:measurement(nuc_imposs_struct_contr_tr_t30, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 30, 0.02).
narrative_ontology:measurement(nuc_imposs_struct_contr_tr_t40, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 40, 0.02).
narrative_ontology:measurement(nuc_imposs_struct_contr_tr_t50, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 50, 0.02).
narrative_ontology:measurement(nuc_imposs_struct_contr_tr_t60, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 60, 0.02).
narrative_ontology:measurement(nuc_imposs_struct_contr_tr_t70, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 70, 0.02).
narrative_ontology:measurement(nuc_imposs_struct_contr_tr_t80, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 80, 0.02).

% Extraction over time
narrative_ontology:measurement(nuc_imposs_struct_contr_be_t0, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(nuc_imposs_struct_contr_be_t10, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 10, 0.03).
narrative_ontology:measurement(nuc_imposs_struct_contr_be_t20, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 20, 0.04).
narrative_ontology:measurement(nuc_imposs_struct_contr_be_t30, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 30, 0.04).
narrative_ontology:measurement(nuc_imposs_struct_contr_be_t40, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 40, 0.05).
narrative_ontology:measurement(nuc_imposs_struct_contr_be_t50, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(nuc_imposs_struct_contr_be_t60, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(nuc_imposs_struct_contr_be_t70, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 70, 0.05).
narrative_ontology:measurement(nuc_imposs_struct_contr_be_t80, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 80, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(nuc_imposs_struct_contr_su_t0, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(nuc_imposs_struct_contr_su_t10, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 10, 0.03).
narrative_ontology:measurement(nuc_imposs_struct_contr_su_t20, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 20, 0.04).
narrative_ontology:measurement(nuc_imposs_struct_contr_su_t30, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 30, 0.04).
narrative_ontology:measurement(nuc_imposs_struct_contr_su_t40, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 40, 0.05).
narrative_ontology:measurement(nuc_imposs_struct_contr_su_t50, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 50, 0.05).
narrative_ontology:measurement(nuc_imposs_struct_contr_su_t60, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 60, 0.05).
narrative_ontology:measurement(nuc_imposs_struct_contr_su_t70, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 70, 0.05).
narrative_ontology:measurement(nuc_imposs_struct_contr_su_t80, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 80, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__structural_contraction_reading, global_infrastructure).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__credibility_paradox_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_deterrence_credibility).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, extended_deterrence_architecture).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, arms_control_verification).

% DUAL FORMULATION NOTE:
% Part of the nuclear_impossibility_kernel constraint family. This reading (structural_contraction) claims the kernel's ε is near-zero (Mountain). The rational_dropout_reading claims moderate ε (tangled_rope — coordination via deterrence with extraction via arms racing). The credibility_paradox_reading claims high ε (snare — incredible threats maintained by coercion). The three ε values differ by wide margins, confirming they are distinct constraints per ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
