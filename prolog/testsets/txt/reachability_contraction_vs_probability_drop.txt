% ============================================================================
% CONSTRAINT STORY: reachability_contraction_vs_probability_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reachability_contraction_vs_probability_drop, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reachability_contraction_vs_probability_drop
 *   human_readable: Reachability Contraction vs Probability Drop: Great-Power Total War Post-1945
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   The post-1945 disappearance of great-power total war from the practical
 *   strategic possibility space represents a fundamental shift in
 *   international relations — either a natural law emerging from nuclear
 *   deterrence logic or a contingent institutional achievement of the liberal
 *   order and non-proliferation regime. This constraint tests the distinction
 *   between reachability (structural removal of an option from the set of
 *   possible strategies) and probability (options remain reachable but carry
 *   prohibitive costs). The kernel question — whether readings can leave the
 *   reachable space or merely shift in probability — is central to how
 *   commitment-system machinery represents institutional change. If
 *   great-power total war has become structurally unreachable, the
 *   international order has fundamentally contracted the strategic option
 *   space available to revisionist powers. If it has only become
 *   probabilistically disfavored, the reachability remains contingent on
 *   enforcement, and actors may perceive themselves as constrained rather
 *   than bound. The measurement trajectory shows increasing extractiveness
 *   (0.38 → 0.52) as non-aligned powers recognize their exclusion from
 *   great-power strategic options, and increasing theater ratio (0.52 → 0.64)
 *   as Cold War deterrence doctrines persist through institutional
 *   performance rather than active enforcement. This is a genuinely ambiguous
 *   constraint where the six DR types are not perspectival readings of the
 *   same fact but rather readings of two competing interpretations of what
 *   the constraint IS.
 *
 * KEY AGENTS:
 *   - Nuclear-Armed Great Power Collective: Primary beneficiary (institutional/arbitrage) — captures stability dividend and protection from existential threat; can compete via economic, technological, and soft-power means
 *   - Liberal Hegemon (US): Secondary beneficiary (institutional/arbitrage) — enforcement role in non-proliferation regime and liberal order; gains from status quo
 *   - International Legal Order: Beneficiary-victim hybrid (institutional/arbitrage) — gains legitimacy from rules-based order but faces pressure when rules are contested
 *   - Revisionist Great Power (China, Russia): Victim-beneficiary hybrid (powerful/constrained) — prevented from pursuing certain military objectives but protected from existential threats by same logic
 *   - Non-Aligned State: Primary victim (powerless/trapped) — structurally excluded from great-power strategic toolkit; constrained to asymmetric or proxy competition
 *   - Asymmetric Conflict Targets: Secondary victim (powerless/trapped) — small powers face adversaries that cannot escalate to total war but face no corresponding restraint in asymmetric modes
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional order as law of nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reachability_contraction_vs_probability_drop, 0.52).
domain_priors:suppression_score(reachability_contraction_vs_probability_drop, 0.68).
domain_priors:theater_ratio(reachability_contraction_vs_probability_drop, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reachability_contraction_vs_probability_drop, extractiveness, 0.52).
narrative_ontology:constraint_metric(reachability_contraction_vs_probability_drop, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(reachability_contraction_vs_probability_drop, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reachability_contraction_vs_probability_drop, tangled_rope).
narrative_ontology:human_readable(reachability_contraction_vs_probability_drop, "Reachability Contraction vs Probability Drop: Great-Power Total War Post-1945").
narrative_ontology:topic_domain(reachability_contraction_vs_probability_drop, "international_relations/strategic_studies/institutional_history").

domain_priors:requires_active_enforcement(reachability_contraction_vs_probability_drop).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(reachability_contraction_vs_probability_drop, distributed).
narrative_ontology:cs_authority_grounding(reachability_contraction_vs_probability_drop, expertise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reachability_contraction_vs_probability_drop, nuclear_armed_great_powers).
narrative_ontology:constraint_beneficiary(reachability_contraction_vs_probability_drop, liberal_hegemon_us).
narrative_ontology:constraint_beneficiary(reachability_contraction_vs_probability_drop, international_legal_order).
narrative_ontology:constraint_victim(reachability_contraction_vs_probability_drop, non_aligned_states).
narrative_ontology:constraint_victim(reachability_contraction_vs_probability_drop, asymmetric_conflict_targets).
narrative_ontology:constraint_victim(reachability_contraction_vs_probability_drop, revisionist_powers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ALIGNED STATE (SNARE) — Structurally locked out of the great-power strategic option space. Cannot pursue total war against nuclear powers; cannot threaten escalation credibly; constrained to asymmetric or proxy modalities. The contraction (if real) is presented as natural law but functions as extraction — removes legitimate strategic options while leaving the victim visible and targetable. Maximum experienced suppression.
constraint_indexing:constraint_classification(reachability_contraction_vs_probability_drop, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REVISIONIST GREAT POWER (TANGLED ROPE) — Faces genuine coordination pressure (nuclear stability, mutual assured destruction logic) that prevents certain military strategies. But also benefits from the same logic: their own territory is protected from existential threat through the contraction. Experiences the constraint as both binding (cannot pursue certain objectives) and coordinating (cannot be annihilated by conventional means). Constrained by the strategic reorientation but not powerless.
constraint_indexing:constraint_classification(reachability_contraction_vs_probability_drop, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: NUCLEAR-ARMED GREAT POWER COLLECTIVE (ROPE) — Core beneficiary of the constraint. Nuclear deterrence enables stable mutual non-aggression without the costs of total war preparation. The contraction is experienced as coordination: a stable equilibrium that enables all parties to pursue other objectives (economic, technological, soft-power competition) without existential risk. High arbitrage — can exit to alternative competition modalities while maintaining the constraint.
constraint_indexing:constraint_classification(reachability_contraction_vs_probability_drop, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LIBERAL INTERNATIONAL ORDER COALITION (SCAFFOLD) — Organized institutional actors (UN system, laws of war, arms control regimes, democratic peace theory) that see the contraction as a temporary sunset-dependent structure. The institutional framework removes total war from the strategic option space, but this removal is contingent on sustained enforcement (nuclear non-proliferation, strategic stability agreements, institutional legitimacy). Coalition perceives a window: if the enforcement regime fails or nuclear proliferation spreads, the contraction reverts. Sunset logic: as long as the institutional order remains legitimate and enforcement costs are absorbed.
constraint_indexing:constraint_classification(reachability_contraction_vs_probability_drop, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR RHETORIC SYSTEM (PITON) — The institutional apparatus that naturalizes the reachability contraction (strategic stability doctrine, nuclear deterrence theory, international law prohibitions on wars of aggression) persists through performative maintenance even as its structural rationale has shifted. Theater ratio high: the apparatus performs legitimacy through strategic arms control talks, non-proliferation review conferences, and academic theorizing about deterrence stability. The machinery functions more to maintain the narrative of a natural law than to actively enforce the contraction.
constraint_indexing:constraint_classification(reachability_contraction_vs_probability_drop, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — Claims the contraction of great-power total war from the reachable strategic space is a natural law: nuclear weapons make such war logically impossible (mutual annihilation is irrational), thus it has been removed from the civilizationally reachable option space. This perspective risks naturalizing a contingent institutional arrangement (deterrence stability, non-proliferation regime, liberal order enforcement) as an immutable law of strategic reality. The false summit flag triggers here — the beneficiaries and structural data reveal this as contingent.
constraint_indexing:constraint_classification(reachability_contraction_vs_probability_drop, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reachability_contraction_vs_probability_drop_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reachability_contraction_vs_probability_drop, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reachability_contraction_vs_probability_drop, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reachability_contraction_vs_probability_drop, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reachability_contraction_vs_probability_drop, TR),
    TR >= 0.70.

:- end_tests(reachability_contraction_vs_probability_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from non-aligned states by removing great-power total war as an option while leaving them vulnerable to great-power pressure via other modalities. Non-aligned states cannot credibly threaten escalation, cannot pursue symmetrical military strategies, and are forced into subordinate roles (client states, proxy conflicts, economic dependency). But the extraction is not maximal because non-aligned states retain agency in asymmetric modes (guerrilla warfare, terrorism, disruption). The trajectory shows increasing extractiveness over the interval as the enforcement regime solidifies and non-aligned states recognize the finality of their exclusion. Suppression (0.68): High. Multiple barriers prevent exit: physical (nuclear weapons are expensive and technically difficult), legal (NPT regime), institutional (US-led enforcement through intelligence and sanctions), and ideological (liberal order delegitimizes certain military objectives). However, suppression is not total — some actors have acquired nuclear weapons despite the regime (India, Pakistan, Israel, North Korea). Theater ratio (0.64): Moderately high. The institutional machinery maintaining the reachability contraction (strategic arms control negotiations, non-proliferation conferences, deterrence stability doctrine) is substantially performative. These activities maintain the appearance of active enforcement while the underlying logic (mutual annihilation preventing total war) operates independently of institutional action. As the regime has solidified, the performative component has increased — fewer substantive new agreements are negotiated, but ritual maintenance continues.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is not between power levels but between two incompatible interpretations of what the constraint is. The natural law reading (mountain) would classify the constraint as immutable from all perspectives — great-power total war is logically impossible, thus unreachable. The institutional reading (tangled rope) would classify it as coordination-with-extraction from most perspectives, with the most radical gap appearing between the liberal power collective (rope: pure coordination for us) and non-aligned states (snare: pure extraction for us). The analytical observer must choose which framing to adopt, and the choice determines the entire classification structure. The constraint demonstrates why commitment system framing matters: the same domain fact (great-power total war has not occurred since 1945) can represent either an immutable law or a contingent institutional achievement depending on whether the constraint IS one thing (natural law) or one reading of a contested kernel (institutional).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality diverges sharply between beneficiaries and victims. Nuclear-armed great powers (institutional/arbitrage) experience low directionality value (d ≈ 0.1-0.2): they are net beneficiaries with escape options, so experienced extraction chi is low or negative. Revisionist great powers (powerful/constrained) experience moderate directionality (d ≈ 0.55-0.65): they are partially victimized (certain strategic options removed) but also benefit from mutual protection, so chi is moderate. Non-aligned states (powerless/trapped) experience maximum directionality (d ≈ 0.9-0.95): they are structurally excluded with no exit options, so chi is maximized. The analytical observer perceives the constraint as reachability contraction (mountain if natural law, institutional if contingent), with directionality depending on which framing is adopted. If mountain, directionality is stable across observers (all see the same immutable law). If institutional, directionality varies based on whether the observer sees themselves as beneficiary (liberal order member) or victim (non-aligned state).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies the mandatrophy in its most acute form: the analytical observer's classification is hostage to an unresolvable disagreement about what the constraint IS. If the observer adopts the natural law frame (reachability contraction is immutable), the constraint is mountain (ε → 0.25, suppression ≤ 0.05), and the declared beneficiaries trigger false summit detection — the engine reclassifies to tangled_rope because beneficiaries are present. If the observer adopts the institutional frame (reachability contraction is contingent), the constraint is tangled_rope (ε ≈ 0.52), and the classification is stable. The resolution is not analytical but political: which framing do the agents and institutions adopt in their strategic planning? If great powers plan as though the contraction is natural law (permanent), they design institutions to maintain deterrence but do not prepare for reactivation. If they plan as though it is institutional (contingent), they design enforcement mechanisms and monitor proliferation risks. The measurement trajectory (increasing extractiveness, increasing theater ratio) suggests a drift from institutional framing (early Cold War saw active enforcement and strategic engagement) toward natural law framing (later period treats the contraction as self-sustaining). This drift itself is the extractive mechanism: as the constraint becomes naturalized, enforcement costs drop and benefits accrue to the institutional beneficiaries with minimal maintenance labor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reachability_vs_probability_kernel,
    'Is great-power total war unreachable (structurally removed from possibility space) or merely improbable (probabilistically disfavored but still within reachable space)?',
    'Distinction test: if the contraction is reachability (structural), then revisionist powers face an OPTION CONSTRAINT — certain strategic objectives become formally impossible to pursue via total war, regardless of rationality calculus. If probability drop, then revisionist powers face a PAYOFF CONSTRAINT — total war remains an option but carries prohibitively high expected costs. Historical test: can we identify moments when actors perceived total war as genuinely unreachable vs. moments when they perceived it as reachable-but-disvalued? Institutional commitment analysis: what mechanisms would be required to reactivate great-power total war? Technology transfer (nuclear proliferation collapse)? Institutional delegitimation (UN system failure)? Strategic realignment?',
    'If reachability contraction: the constraint is structural, non-reversible within the commitment system (requires regime change). Classification remains tangled_rope from institutional positions. If probability drop: the constraint is incentive-based, reversible if payoff structures change. Classification shifts toward rope from all positions (pure coordination without extraction). This is the kernel distinction that separates ''commitment system machinery can represent unreachable readings'' from ''commitment systems only model probability shifts over stable option sets.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reachability_vs_probability_kernel, conceptual, 'Whether great-power total war is structurally unreachable or probabilistically disfavored').

omega_variable(
    enforcement_mechanism_contingency,
    'What institutional enforcement mechanisms sustain the reachability contraction, and how would their failure reactivate total war as an option?',
    'Decomposition of enforcement layers: (1) nuclear deterrence stability (mutual assured destruction logic), (2) non-proliferation regime (IAEA, NPT enforcement), (3) liberal international legal order (UN Charter, laws of war, war crimes tribunals), (4) economic interdependence (trade networks raising costs of conflict). Test each layer for necessity: which could fail without reactivating total war as an option? Which are redundant? Which is doing the reachability work vs. merely raising probability costs?',
    'If enforcement is multi-layered and redundant: reachability contraction is robust. If enforcement depends on a single critical mechanism (e.g., nuclear deterrence only): contraction is fragile and could revert with that mechanism''s failure. Affects classification stability across time horizons.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_contingency, empirical, 'Which enforcement mechanisms sustain the reachability contraction').

omega_variable(
    revisionist_power_perception_gap,
    'Do revisionist powers perceive the contraction as immutable (reachability) or as contingent (probability shift)?',
    'Analysis of revisionist strategic doctrine, military planning scenarios, and escalation ladders. Do they treat total war as a ''never'' option (reachability frame) or as a ''not now'' option (probability frame)? Historical: during Cold War, did Soviet and Chinese strategists treat great-power total war as logically impossible or as strategically irrational? Current: does Chinese military doctrine treat conflict with the US as capped at certain intensity levels (reachability) or as escalation-manageable at any intensity (probability)?',
    'If revisionist actors perceive reachability contraction: they may accept it as binding on their own strategy. If they perceive probability drop: they may treat it as reversible and plan accordingly. Perception shapes strategic stability — mismatched frames between US (treating as reachability) and China (treating as probability) create miscalculation risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revisionist_power_perception_gap, empirical, 'How revisionist powers perceive the constraint: reachable-but-disfavored vs structurally unreachable').

omega_variable(
    false_summit_natural_law_claim,
    'Is the disappearance of winnable great-power total war a natural law of nuclear strategy or a contingent institutional achievement?',
    'Compare two framings: (A) Nuclear-driven natural law — mutual annihilation logic makes great-power total war logically impossible, thus reading it out of reachable space. (B) Institutional achievement — the liberal international order, non-proliferation regime, and deterrence stability doctrine create a contingent institutional frame that removes total war from legitimized strategic options. If A is correct, the constraint is mountain. If B is correct, the constraint is tangled_rope with beneficiaries and victims. The existence of non-aligned states that perceive the constraint as extractive (they lack great-power options) suggests institutional rather than natural-law framing.',
    'If natural law: reclassify to mountain (remove beneficiaries, declare emerges_naturally: true). If institutional: maintain tangled_rope with full beneficiary/victim analysis. The false summit detector will flag this constraint given the declared beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether the constraint is a natural law of nuclear deterrence or a contingent institutional arrangement').

omega_variable(
    nuclear_proliferation_horizon,
    'As nuclear weapons proliferate to additional powers, does the reachability contraction apply to new nuclear powers or does it dissolve?',
    'Test cases: India-Pakistan (both nuclear, have fought wars post-nuclearization), Israel (undeclared, surrounded by non-nuclear states), Iran scenario (if acquires nuclear weapons, does it reactivate total war as reachable against neighbors?). If contraction applies uniformly to all nuclear pairs, it is truly structural. If it applies only to superpowers or specific dyads, it is contingent on great-power-specific factors (stability doctrines, command control, second-strike capability), suggesting institutional rather than natural law.',
    'Empirical boundary test: proliferation to smaller powers and unstable dyads may reveal whether the contraction is a universal consequence of nuclear weapons or a privileged outcome of specific great-power institutional arrangements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nuclear_proliferation_horizon, empirical, 'Whether reachability contraction extends to all nuclear powers or only specific dyads').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reachability_contraction_vs_probability_drop, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reach_theater_t0, reachability_contraction_vs_probability_drop, theater_ratio, 0, 0.52).
narrative_ontology:measurement(reach_theater_t5, reachability_contraction_vs_probability_drop, theater_ratio, 5, 0.6).
narrative_ontology:measurement(reach_theater_t10, reachability_contraction_vs_probability_drop, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(reach_extract_t0, reachability_contraction_vs_probability_drop, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(reach_extract_t5, reachability_contraction_vs_probability_drop, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(reach_extract_t10, reachability_contraction_vs_probability_drop, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reachability_contraction_vs_probability_drop, enforcement_mechanism).
narrative_ontology:affects_constraint(reachability_contraction_vs_probability_drop, nuclear_deterrence_stability).
narrative_ontology:affects_constraint(reachability_contraction_vs_probability_drop, non_proliferation_regime).
narrative_ontology:affects_constraint(reachability_contraction_vs_probability_drop, international_legal_order_war_crimes).
narrative_ontology:affects_constraint(reachability_contraction_vs_probability_drop, great_power_strategic_realignment).

% DUAL FORMULATION NOTE:
% The reachability vs probability distinction generates two structurally distinct constraint readings from the same domain fact. The natural law reading (reachability as immutable) would be a separate constraint story (reachability_natural_law_great_power_war) with ε ≈ 0.08, mountain classification, and no beneficiaries. The institutional reading (probability drop with contingent enforcement) is this constraint story (reachability_contraction_vs_probability_drop) with ε ≈ 0.52, tangled_rope classification, and full beneficiary/victim structure. Both should be linked via network to show they are readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reachability_contraction_vs_probability_drop, powerful, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
