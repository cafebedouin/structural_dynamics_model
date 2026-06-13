% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   human_readable: Nuclear Mutual Annihilation Structural Impossibility (Contraction Reading)
 *   domain: strategic_studies/international_relations/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint embodies a specific reading of how nuclear weapons
 *   changed strategic possibility. The structural-contraction reading claims
 *   that mutual nuclear arsenals create a physical impossibility: no rational
 *   path to victory exists because mutual annihilation is guaranteed. This is
 *   distinct from a rational-dropout reading (victory is possible but costs
 *   exceed any benefit) or a credibility-paradox reading (deterrence requires
 *   an irrational threat). Under the structural-contraction reading, war
 *   itself exits the reachable set — not because it is irrational (rational
 *   actors can be irrational) but because the outcome is physically
 *   impossible to survive. The constraint is presented as a genuine mountain:
 *   a natural-law-like limit on strategic possibility that persists
 *   regardless of belief, incentive, or political desire. The declared
 *   beneficiaries are paradoxical: humanity-continuation benefits from the
 *   constraint, but no actor benefits from administering it or extracting
 *   from it — it is pure structural fact.
 *
 * KEY AGENTS:
 *   - Nuclear-armed states: trapped in mutual impossibility; cannot exit without losing second-strike capacity; benefit from constraint but cannot manipulate or profit from it.
 *   - Rival nuclear powers: each faces the same constraint from the other; coordination emerges from shared structural binding, not negotiation.
 *   - Non-nuclear states: benefit from large-scale war being removed from feasible set; face vulnerability to proxy-war substitution.
 *   - Proxy-war actors: exist in the constraint's shadow; conduct wars because great-power direct conflict is impossible; structurally excluded from constraint negotiation.
 *   - Strategic theorists: analytical seat; measure and articulate the constraint's logical structure without power over its operation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, 0.15).
domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, 0.08).
domain_priors:theater_ratio(nuclear_impossibility_kernel__structural_contraction_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Nuclear Mutual Annihilation Structural Impossibility (Contraction Reading)").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "strategic_studies/international_relations/nuclear_deterrence").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, 'baaedb48-3907-4ca3-8f76-06ebde663f46').
narrative_ontology:cs_kernel_codification('baaedb48-3907-4ca3-8f76-06ebde663f46', formalized).
narrative_ontology:cs_authority_grounding('baaedb48-3907-4ca3-8f76-06ebde663f46', expertise).
narrative_ontology:cs_interpretation_layer_present('baaedb48-3907-4ca3-8f76-06ebde663f46').
narrative_ontology:cs_reading_relation('baaedb48-3907-4ca3-8f76-06ebde663f46', nuclear_impossibility_kernel__rational_dropout_reading, forecloses).
narrative_ontology:cs_reading_relation('baaedb48-3907-4ca3-8f76-06ebde663f46', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('baaedb48-3907-4ca3-8f76-06ebde663f46', foundational, mutual_annihilation_logically_certain).
narrative_ontology:cs_axiom_status(mutual_annihilation_logically_certain, holdable).
narrative_ontology:cs_axiom_grounding('baaedb48-3907-4ca3-8f76-06ebde663f46', mutual_annihilation_logically_certain, empirically_contingent).
narrative_ontology:cs_axiom('baaedb48-3907-4ca3-8f76-06ebde663f46', foundational, war_exits_reachable_action_set).
narrative_ontology:cs_axiom_status(war_exits_reachable_action_set, holdable).
narrative_ontology:cs_axiom_grounding('baaedb48-3907-4ca3-8f76-06ebde663f46', war_exits_reachable_action_set, deontological).
narrative_ontology:cs_reference_frame('baaedb48-3907-4ca3-8f76-06ebde663f46', pre_nuclear_great_power_war_possibility).
narrative_ontology:cs_drift_state('baaedb48-3907-4ca3-8f76-06ebde663f46', contemporary_continued_arsenal_maintenance, gap(stable, minor, false)).
narrative_ontology:cs_created_at('baaedb48-3907-4ca3-8f76-06ebde663f46', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, humanity_continuation_interest).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__structural_contraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(nuclear_impossibility_kernel__structural_contraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

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
 *   Extractiveness is low (0.15) because no party collects rents or surplus from the constraint's operation — it operates as a pure structural fact, not as a mechanism administered for gain. Suppression is minimal (0.08) because the constraint does not require active coercion to persist; it persists through the physical facts of mutual arsenals. Theater is also low (0.12) and stable over the interval — the constraint's operation is largely transparent: if arsenals remain, impossibility remains. The measurement series is deliberately flat because a genuine mountain should show no drift in its extractiveness or suppression over time: the physics does not change, and the strategic logic holds as long as second-strike capacity persists. Accessibility collapse is very high (0.92) — once the logic of mutual annihilation is understood, there are no alternatives; war is not an option any rational actor pursues. Resistance is near-zero (0.04) because parties do not resist a constraint that eliminates a previously catastrophic option; they may resent it (forced into permanent standoff), but they do not resist it the way a population resists extraction.
 *
 * PERSPECTIVAL GAP:
 *   The gap between nuclear-armed states and non-nuclear states is structural. Nuclear-armed states face the constraint directly: war is impossible for them because mutual annihilation is certain. Non-nuclear states benefit from that impossibility (great-power wars do not happen), but they face a different constraint: they may become theaters for proxy conflict precisely because the primary powers cannot war directly. Strategic theorists and nuclear powers perceive the constraint differently: theorists analyze it as a logical structure; powers experience it as a binding condition they cannot exit. From the theorist's analytical seat, the constraint is a solved problem (the logic is proven); from a nuclear power's seat, it is an unresolved tension (how to maintain credible deterrence while the threat must remain unexecuted).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality here is unusual because the constraint is non-extractive. All named agents are beneficiaries in the sense that all benefit from war being impossible. Nuclear-armed states benefit from the constraint but cannot benefit from it being administered — they are trapped by it, not coordinated by it. The directionality of each agent is near d=0.0 (beneficiary), but the meaning is inverted: they benefit from the constraint not existing as an enforced mechanism but from existing as a physical fact. Non-nuclear states are similarly beneficiaries (d near 0.0). Proxy-war actors are excluded, not payers — they would object to the constraint's effect (being forced into proxy theaters), but they do not pay a transfer to sustain it. The constraint sustains itself through the persistence of second-strike arsenals, which is both an act and a necessity for both sides.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows no sign of mandatrophy. Its founding problem (vulnerability to conquest by great-power war) remains live — the very fact that no great-power wars have occurred since 1945 proves the constraint persists. The mandate (prevent great-power war through mutual impossibility) is not separated from its function; they are one and the same. If anything, the constraint shows evidence of persistent necessity because the underlying vulnerability it addresses has not disappeared — nations still fear each other, still compete for power, still prepare for war, and the constraint is the only mechanism preventing that competition from resulting in civilization-ending conflict. Theater is low and stable: the constraint's operation is not theatrical because it does not require constant performance — it requires arsenals, which nations maintain openly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_strategic_construction,
    'Is mutual annihilation a physical impossibility (a natural law of thermodynamics and ballistics) or a strategic construction that depends on second-strike capacity being maintained, which is a choice?',
    'Decompose into two constraints: one is the physical law (no technology can make nuclear weapons unmake physics); the other is the strategic choice (nations maintain second-strike arsenals). This reading collapses them; distinguish them and measure each separately.',
    'If the constraint is decomposed, the natural-law component is a genuine mountain; the second-strike maintenance component is a tangled-rope (coordination + enforcement of arsenals). This reading treats them as one, which may be analytically incorrect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_strategic_construction, conceptual, 'The constraint may be a decomposable pair: physics (true mountain) + strategic choice (maintenance of deterrent capacity).').

omega_variable(
    structural_vs_contingent_impossibility,
    'Is the impossibility of rational victory structural (follows from the logic of mutual second-strike capacity) or contingent (depends on both sides believing mutual destruction is guaranteed)?',
    'Historical counterfactual analysis: would a rational actor with credible belief in superiority (belief that they could survive/win despite mutual arsenals) pursue war? The constraint claims impossibility is structural; if belief-revision could motivate war, the impossibility is contingent on shared epistemic states.',
    'If contingent, the constraint is vulnerable to rational error (one side miscalculates survivability or win-probability). The structural reading claims no amount of belief can change the outcome; the contingent reading claims miscalculation could trigger attempted war, collapsing the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_contingent_impossibility, empirical, 'Whether mutual annihilation is logically certain or depends on shared beliefs about survivability.').

omega_variable(
    proxy_war_substitution_mechanism,
    'Are proxy wars a substitute for large-scale great-power conflict (as this reading claims), or are they a genuinely independent phenomenon that would occur regardless of nuclear deterrence?',
    'Compare conflict patterns in nuclear vs. non-nuclear dyads at equivalent power-asymmetry levels. If nuclear dyads show lower direct-conflict rates but equivalent or higher proxy-war rates, substitution is supported. If proxy-war patterns are independent of nuclear status, the constraint is not a necessary explanation.',
    'If proxy wars are purely substitutional, the constraint transforms conflict form but does not reduce it; humanity''s ''benefit'' is that great-power wars are gone but peripheral wars continue. If proxy wars are independent, the constraint reduces war more broadly, and its benefit is larger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_war_substitution_mechanism, empirical, 'Whether proxy wars are a necessary outlet for conflicts that nuclear impossibility prevents.').

omega_variable(
    readings_kernel_dispute,
    'This constraint is one reading of the nuclear_impossibility_kernel. How do the three readings (structural_contraction, rational_dropout, credibility_paradox) relate logically?',
    'Committer-frame analysis: structural_contraction claims war is logically impossible; rational_dropout claims war is costlier-than-any-benefit but logically possible; credibility_paradox claims deterrence is self-undermining because credible threats require something irrational. These are three distinct empirical/logical claims about the same kernel.',
    'If structural_contraction is true, rational_dropout is subsumed (if impossible, it is certainly not rational to attempt). If rational_dropout is true but not structural_contraction, credibility_paradox remains live (threat must be made credible even though victory is unrealistic). If credibility_paradox is true, both others are problematized. The three readings foreclose different subsets of each other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(readings_kernel_dispute, conceptual, 'The logical relationships between the three kernel readings of nuclear deterrence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t0, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(nucl_tr_t10, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(nucl_tr_t20, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(nucl_tr_t40, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(nucl_tr_t60, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement(nucl_tr_t80, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 80, 0.12).

% Extraction over time
narrative_ontology:measurement(nucl_be_t0, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(nucl_be_t10, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(nucl_be_t20, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(nucl_be_t40, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(nucl_be_t60, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 60, 0.16).
narrative_ontology:measurement(nucl_be_t80, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 80, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t0, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(nucl_su_t10, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 10, 0.06).
narrative_ontology:measurement(nucl_su_t20, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 20, 0.07).
narrative_ontology:measurement(nucl_su_t40, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 40, 0.08).
narrative_ontology:measurement(nucl_su_t60, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 60, 0.08).
narrative_ontology:measurement(nucl_su_t80, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 80, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__structural_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nuclear_impossibility_kernel__structural_contraction_reading, 0.08).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the nuclear_impossibility_kernel family. The structural-contraction reading claims war is logically impossible; the rational-dropout reading claims war is costlier-than-benefit but possible; the credibility-paradox reading claims deterrence is self-undermining. All three read from the same kernel (the empirical fact of mutual nuclear arsenals) but instantiate different constraints with different ε values and different core logical claims. The three stories are linked via affects_constraints to enable cross-reading comparative analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
