% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nuclear_impossibility_kernel__structural_contraction_reading
 *   human_readable: Nuclear Impossibility: War Exit via Physical Constraint (Structural Contraction Reading)
 *   domain: strategic_studies/nuclear_deterrence/physical_limits
 *
 * SUMMARY:
 *   This constraint instantiates the structural_contraction_reading of the
 *   nuclear_impossibility_kernel: nuclear weapons have created a topological
 *   impossibility in the strategic outcome space. Total war, once a rational
 *   option that states could choose (at great cost), is no longer in the
 *   reachable set of rational strategies because mutual annihilation is
 *   guaranteed. This reading differs fundamentally from its siblings: the
 *   credibility_paradox_reading argues that nuclear deterrence is unstable
 *   because the threat to use them is incredible (since using them violates
 *   rationality), while the rational_dropout_reading argues that war remains
 *   logically possible but is rationally avoidable because costs exceed
 *   benefits. The structural_contraction_reading makes a stronger claim: war
 *   is no longer rationally chooseable because no winning outcome exists.
 *   Victory has exited the strategy space entirely—not because it's too
 *   expensive, but because it's mathematically impossible given mutual
 *   destruction guarantees. This reading treats nuclear weapons as having
 *   created a physical/logical law equivalent to the laws of thermodynamics:
 *   states cannot choose to win a total nuclear war because winning is
 *   structurally excluded. The measurement trajectory shows extractiveness
 *   rising slightly over time (0.0 to 0.08) as the constraint's suppressive
 *   force accumulates—the constraint becomes more salient as nuclear arsenals
 *   mature and the impossibility becomes undeniable. Theater ratio remains
 *   low because the structural fact requires no institutional
 *   performance—mutual annihilation is performatively self-evident.
 *
 * KEY AGENTS:
 *   - All rational actors (universal beneficiary from structural contraction): benefit from the impossibility because it forecloses mutual annihilation outcomes
 *   - Nuclear-armed states (status-quo beneficiaries): benefit by having the constraint foreclose revisionist war strategies
 *   - Non-nuclear states (constrained): benefit from mutual impossibility but constrained by exclusion from nuclear symmetry
 *   - Analytical game theorist (observer): sees the outcome space contraction as a topological/mathematical fact independent of politics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, 0.08).
domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, 0.02).
domain_priors:theater_ratio(nuclear_impossibility_kernel__structural_contraction_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Nuclear Impossibility: War Exit via Physical Constraint (Structural Contraction Reading)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "strategic_studies/nuclear_deterrence/physical_limits").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, '4958cf57-27b8-4039-a4ad-78a17423f0b5').
narrative_ontology:cs_kernel_codification('4958cf57-27b8-4039-a4ad-78a17423f0b5', distributed).
narrative_ontology:cs_authority_grounding('4958cf57-27b8-4039-a4ad-78a17423f0b5', distributed).
narrative_ontology:cs_reading_relation('4958cf57-27b8-4039-a4ad-78a17423f0b5', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_reading_relation('4958cf57-27b8-4039-a4ad-78a17423f0b5', nuclear_impossibility_kernel__rational_dropout_reading, influences).
narrative_ontology:cs_axiom('4958cf57-27b8-4039-a4ad-78a17423f0b5', foundational, mutual_annihilation_mathematically_guarantees_no_victory).
narrative_ontology:cs_axiom_status(mutual_annihilation_mathematically_guarantees_no_victory, holdable).
narrative_ontology:cs_axiom_grounding('4958cf57-27b8-4039-a4ad-78a17423f0b5', mutual_annihilation_mathematically_guarantees_no_victory, empirically_contingent).
narrative_ontology:cs_axiom('4958cf57-27b8-4039-a4ad-78a17423f0b5', foundational, war_exits_reachable_strategy_set_entirely).
narrative_ontology:cs_axiom_status(war_exits_reachable_strategy_set_entirely, holdable).
narrative_ontology:cs_axiom_grounding('4958cf57-27b8-4039-a4ad-78a17423f0b5', war_exits_reachable_strategy_set_entirely, empirically_contingent).
narrative_ontology:cs_reference_frame('4958cf57-27b8-4039-a4ad-78a17423f0b5', classical_war_as_rational_political_instrument).
narrative_ontology:cs_drift_state('4958cf57-27b8-4039-a4ad-78a17423f0b5', post_second_strike_capability_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('4958cf57-27b8-4039-a4ad-78a17423f0b5', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, all_rational_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATIONS (MOUNTAIN) — No rational escape from the constraint that war leads to annihilation. The constraint is immutable from this position: mutual destruction is guaranteed by physics, not by institutional choice. Civilians experience the constraint as an unchangeable law — war is no longer a recoverable option.
constraint_indexing:constraint_classification(nuclear_impossibility_kernel__structural_contraction_reading, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: STATE STRATEGIC PLANNERS / ANALYTICAL (MOUNTAIN) — From the standpoint of classical strategic rationality (utility maximization, victory/defeat calculus), nuclear weapons have eliminated the victory class entirely. No outcome of total war yields positive utility; the game-theoretic reachable set has contracted to exclude war as a rational strategy. This is not a political choice — it is a mathematical/physical fact about the outcome space.
constraint_indexing:constraint_classification(nuclear_impossibility_kernel__structural_contraction_reading, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: GAME-THEORETIC OBSERVER (MOUNTAIN) — War has exited the reachable set entirely. The strategic outcome space that once included 'total war → conquest → political reorganization' now contains only mutual annihilation. This is a topological fact about the strategy space, not an opinion or institutional convention. M-set contraction eliminates the site-expansion cell — no victory condition exists that does not end in annihilation for all parties.
constraint_indexing:constraint_classification(nuclear_impossibility_kernel__structural_contraction_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(nuclear_impossibility_kernel__structural_contraction_reading, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nuclear_impossibility_kernel__structural_contraction_reading, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

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
 *   Extractiveness (0.08): Minimal. The structural contraction imposes no active extraction—it is the absence of extraction through the avoidance of annihilation. The small positive value reflects that nuclear powers asymmetrically benefit from the status-quo preservation the constraint enforces (they retain the arsenals that create the impossibility). Suppression (0.02): Minimal. The constraint operates through the physical fact of mutual destruction, not through active suppression of alternatives. The minimal value reflects only the informational asymmetry that some states lack nuclear capability. Theater ratio (0.15): Low. The structural fact (mutual annihilation guarantees victory is impossible) requires no theatrical performance—it is self-evident once arsenals mature. The slight rise over time reflects growing institutional codification (doctrines, treaties, taboos) that ritualize what was once a raw fact, but theater remains minimal because the underlying physics requires no performance.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives (powerless civilians, institutional planners, analytical observers) arrive at mountain classification because the constraint is universally experienced as immutable. There is no perspectival gap—this is the exceptional case where all indices converge to the same type because the constraint operates at the level of physical possibility itself. The constraint does not extract differentially by power or position: it bars total war for all rational actors uniformly. The absence of perspectival gap indicates a genuine natural law (though omega_4 flags the false-summit risk: does the 'universal beneficiary' framing naturalize what is actually an institutional arrangement benefiting status-quo powers?).
 *
 * DIRECTIONALITY LOGIC:
 *   All rational agents are beneficiaries of the impossibility constraint—they benefit from the guaranteed absence of mutual annihilation. Directionality is universally low (d near 0.0): the constraint subsidizes all agents equally by removing the catastrophic outcome. This produces a mountain classification across all perspectives because the beneficiary set is universal and the constraint operates at the level of physical law rather than institutional extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_contraction_vs_rational_dropout,
    'Is the war-exit produced by logical impossibility (structural contraction: no winning strategy exists) or by unfavorable cost-benefit (rational dropout: winning strategies exist but cost exceeds benefit)?',
    'Formal game-theoretic proof vs. empirical utility assignment. Does victory remain in the payoff matrix but with negative utility? Or is victory undefined in the outcome space?',
    'If structural contraction: mountain classification holds. If rational dropout: reclassify as rope (rational coordination to avoid a bad outcome). The reading''s core distinguishing axiom depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_contraction_vs_rational_dropout, conceptual, 'Logical impossibility vs. unfavorable cost-benefit analysis').

omega_variable(
    proxy_war_as_substitution_vs_continuation,
    'Are proxy wars (Korea, Vietnam, Afghanistan, proxy conflicts in Syria/Ukraine) genuine substitutes for total war, or are they partial continuations of a war that nuclear weapons prevented from escalating?',
    'Structural analysis: do proxy wars satisfy the strategic goals that total war would have served? Or do they represent a degraded, second-best continuation constrained by the impossibility?',
    'If substitution: the structural contraction is complete — rational war has been replaced by rational proxy activity. If continuation: the constraint is incomplete — war persists in degraded form, suggesting the contraction is partial, not total.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_war_as_substitution_vs_continuation, conceptual, 'Whether proxy wars are substitutes or degraded continuations of total war').

omega_variable(
    credibility_of_irrational_threats,
    'Does the structural impossibility of rational victory make nuclear threats inherently incredible (because any use violates rationality), or does the irreversibility of escalation make threats credible precisely because victory is impossible?',
    'Historical analysis of close-call escalations (Cuban Missile Crisis, Kargil, multiple nuclear near-misses). Do decision-makers treat the threat as credible despite its irrationality? Does the impossibility of backing down (once escalation crosses the threshold) restore credibility?',
    'If impossible = incredible: the credibility_paradox_reading is correct, and the structural contraction reading underestimates the persistence of deterrent risk. If impossible = irrevocable, hence credible: both readings coexist — the contraction is real, but so is the paradox that makes it unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_of_irrational_threats, empirical, 'Credibility of threats when rationality is violated by execution').

omega_variable(
    false_summit_natural_law_beneficiary,
    'Is the structural contraction a genuine physical law (all rational agents benefit from the mutual impossibility), or a naturalizing frame that benefits specific institutional actors (nuclear powers, status-quo states)?',
    'Examination of who benefits from the ''no rational war'' framing: status-quo powers benefit by foreclosing revisionist war; non-nuclear states benefit by delegitimizing the tool that created the asymmetry. Alternative framings that question the beneficiary universality.',
    'If genuine natural law: mountain holds, beneficiaries are universal (all parties benefit from avoidance of annihilation). If institutional naturalizing: false summit triggers FSM evaluation — reclassify as tangled_rope or snare depending on who bears the constraint''s suppressive force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_beneficiary, conceptual, 'Whether structural contraction is natural law or naturalized institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 1945, 1991).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_struct_tr_t1945, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(nucl_struct_tr_t1991, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1991, 0.15).

% Extraction over time
narrative_ontology:measurement(nucl_struct_be_t1945, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1945, 0.0).
narrative_ontology:measurement(nucl_struct_be_t1950, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1950, 0.02).
narrative_ontology:measurement(nucl_struct_be_t1962, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1962, 0.03).
narrative_ontology:measurement(nucl_struct_be_t1991, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1991, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__structural_contraction_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__structural_contraction_reading, 0.02).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__credibility_paradox_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, deterrence_stability_proxy_war_substitution).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_taboo_ritualization).

% DUAL FORMULATION NOTE:
% The nuclear_impossibility_kernel has three structurally distinct readings, each with different ε values and classification types. This constraint (structural_contraction_reading) represents the claim that war is logically impossible. The credibility_paradox_reading (ε ≈ 0.45, Tangled Rope) argues that deterrence is unstable due to the paradox of incredible threats. The rational_dropout_reading (ε ≈ 0.35, Rope) argues that war is rationally avoidable but not impossible. All three are readings of the same kernel (how nuclear weapons changed war), but their divergent ε values and classification types reflect genuine structural differences in how the kernel is read. The sibling readings are networked to this constraint to show the kernel's internal structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
