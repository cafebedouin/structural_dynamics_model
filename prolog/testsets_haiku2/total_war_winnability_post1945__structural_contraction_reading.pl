% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__structural_contraction_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: total_war_winnability_post1945__structural_contraction_reading
 *   human_readable: Total War Structural Winnability Collapse (Post-1945 Nuclear Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint describes the structural elimination of total war
 *   winnability as a consequence of nuclear deterrence. The reading
 *   instantiated here claims that the post-1945 collapse of total-war
 *   reachability is NOT primarily a normative shift (law, ethics, culture)
 *   but a physical structural change: nuclear weapons made it mathematically
 *   impossible for a belligerent to secure total victory because an
 *   opponent's second-strike capability cannot be eliminated. The constraint
 *   is a Mountain — a natural law governing the reachable space of strategic
 *   outcomes — not a social construction maintained by enforcement. Global
 *   civilian populations benefit from this structural contraction (they are
 *   spared from living in a world where total annihilation is strategically
 *   rational), but no beneficiary 'maintains' the constraint; it persists
 *   because the physics of nuclear deterrence is invariant. This reading
 *   coexists with (but is structurally distinct from) normative and
 *   cultural-drift readings of the same post-1945 outcome that ascribe the
 *   collapse to treaty law, humanitarian doctrine evolution, and strategic
 *   culture change rather than to physical structural impossibility.
 *
 * KEY AGENTS:
 *   - Global civilian populations: the beneficiaries of the winnability collapse, spared from living in a world where total war is a reachable strategic option
 *   - Nuclear-armed great powers: the institutional observers whose arsenals constitute the constraint; their strategic doctrines (Mutually Assured Destruction, second-strike capability) operationalize the winnability elimination
 *   - Non-nuclear states: excluded from the constraint by the fact of their non-nuclear status; they cannot apply total-war strategies themselves and remain vulnerable to powers with nuclear weapons, but their exclusion is structural, not enforced
 *   - Strategic theorists and physicists: analytical observers who measure and interpret the winnability collapse across different frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__structural_contraction_reading, 0.0).
domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, 0.0).
domain_priors:theater_ratio(total_war_winnability_post1945__structural_contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__structural_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_winnability_post1945__structural_contraction_reading, "Total War Structural Winnability Collapse (Post-1945 Nuclear Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__structural_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__structural_contraction_reading, 'ef7568eb-56b9-49c1-beab-98da0030f5d6').
narrative_ontology:cs_kernel_codification('ef7568eb-56b9-49c1-beab-98da0030f5d6', distributed).
narrative_ontology:cs_authority_grounding('ef7568eb-56b9-49c1-beab-98da0030f5d6', expertise).
narrative_ontology:cs_reading_relation('ef7568eb-56b9-49c1-beab-98da0030f5d6', total_war_winnability_post1945__normative_reading_drop, influences).
narrative_ontology:cs_reading_relation('ef7568eb-56b9-49c1-beab-98da0030f5d6', total_war_winnability_post1945__strategic_culture_drift, influences).
narrative_ontology:cs_axiom('ef7568eb-56b9-49c1-beab-98da0030f5d6', foundational, second_strike_capability_nullifies_victory).
narrative_ontology:cs_axiom_status(second_strike_capability_nullifies_victory, holdable).
narrative_ontology:cs_axiom_grounding('ef7568eb-56b9-49c1-beab-98da0030f5d6', second_strike_capability_nullifies_victory, empirically_contingent).
narrative_ontology:cs_axiom('ef7568eb-56b9-49c1-beab-98da0030f5d6', foundational, structural_elimination_primacy_over_norm).
narrative_ontology:cs_axiom_status(structural_elimination_primacy_over_norm, holdable).
narrative_ontology:cs_axiom_grounding('ef7568eb-56b9-49c1-beab-98da0030f5d6', structural_elimination_primacy_over_norm, empirically_contingent).
narrative_ontology:cs_reference_frame('ef7568eb-56b9-49c1-beab-98da0030f5d6', winnability_structural_elimination_permanent).
narrative_ontology:cs_drift_state('ef7568eb-56b9-49c1-beab-98da0030f5d6', contemporary_post_cold_war, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ef7568eb-56b9-49c1-beab-98da0030f5d6', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__structural_contraction_reading, global_civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nuclear weapons made total war structurally un-winnable: a belligerent cannot secure decisive military victory when the opponent holds second-strike capability. Civilian populations benefit from the elimination of total-war scenarios (where annihilation was theoretically possible as a military strategy) even though they remain under deterrence risk. They cannot exit or alter this constraint; it operates as a structural property of the post-1945 weapons regime.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, global_civilian_populations, beneficiary,
    powerless, civilizational, trapped, universal).

% Possess the nuclear arsenals that constitute the structural constraint. Their strategic doctrines (Mutually Assured Destruction, second-strike doctrine) acknowledge and operationalize the winnability collapse, even when their declarative doctrines claim deterrent sufficiency. They cannot choose whether winnability is mathematically possible; they can only choose whether to accept it operationally.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, nuclear_armed_great_powers, observer,
    institutional, civilizational, analytical, global).

% Are excluded from the structural constraint by virtue of lacking nuclear capability. They remain vulnerable to total-war scenarios in principle (conquest, annihilation, unconditional surrender) when facing nuclear-armed adversaries, but are structurally barred from applying total-war strategies themselves. Their exclusion is not maintained by enforcement but by the physics of nuclear deterrence.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, non_nuclear_states, excluded,
    moderate, generational, constrained, global).

% Analyze and interpret the winnability collapse through different frameworks (structural-realist, normative-legal, cultural-shift). Their analytical seat does not collect from the constraint or bear its costs; they measure its operation across different readings.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__structural_contraction_reading, strategic_theorists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__structural_contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__structural_contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nuclear deterrence coordinates the behavior of great powers by making total military victory impossible: no belligerent can eliminate an opponent's retaliation capacity, so victory conditions collapse. This is not a designed coordination mechanism but an emergent structural property of symmetric nuclear capability.
% TRANSFER_FUNCTION: None. The constraint extracts nothing from any party and transfers nothing between parties. It operates as a structural fact about the physical reachability of outcomes, not as a mechanism that collects gains.
% ABSENT_VOICES: Future populations in counterfactual nuclear-exchange scenarios cannot speak to the constraint's operation because the constraint prevents those scenarios from occurring. The constraint is most directly testified to by strategic theorists analyzing deterrence mechanics, not by affected parties (since the consequence of winnability removal is prevention, not cost-bearing).
% DISAPPEARANCE_RATIONALE: This constraint does not persist by enforcement, institutional maintenance, or voluntary compliance. It persists because nuclear physics and second-strike doctrine mathematics are invariant. If nuclear weapons disappeared, winnability would return to the reachable space — but not because the constraint 'disappeared' in any meaningful sense. The constraint's presence or absence is determined by whether nuclear arsenals exist, not by social choice. The world-unchanged verdict reflects that the constraint's operation is not contingent on any party maintaining it.
% FOUNDING_PROBLEM: Before 1945, total military victory was structurally possible: a sufficiently overwhelming military force could defeat an opponent, occupy its territory, and eliminate its capacity to retaliate. This made total war (conquest without negotiation limit) a reachable strategic option. The founding problem was the *reachability* of total-war scenarios as military strategies, not their social desirability.
% FOUNDING_PROBLEM_CORROBORATION: The structural winnability of total war—that military victory could be achieved through annihilation—is a historical fact testified to by pre-1945 strategic outcomes (WWII unconditional surrender as the limit case), military theory (Clausewitz on annihilation, Napoleon's operations), and counterfactual analysis of pre-nuclear weapons regimes. The structural change post-1945 is corroborated by declassified nuclear doctrine (SIOP, Soviet equivalent analyses), physicists and arms-control theorists analyzing second-strike stability, and the empirical absence of great-power total wars in the nuclear era despite repeated crises where total war would have been strategically rational in earlier systems. Outside observers (international-relations scholars, historians, physicists) attest the constraint, not the benefiting parties (global populations cannot testify to counterfactuals).
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__structural_contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__structural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__structural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__structural_contraction_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.0 throughout the interval because the constraint is pure natural law: it extracts nothing from any party and transfers nothing between parties. No one collects rents from the winnability elimination. Suppression is 0.0 because there are no alternatives to suppress — the constraint operates as a brute fact about the reachable space, not as a mechanism that must be enforced against resistance. Theater ratio is 0.0 because there is no performative machinery: the constraint's persistence is guaranteed by physics and second-strike doctrine mathematics, not by institutional maintenance or rhetorical framing. Accessibility collapse is high (0.92) because once the constraint is understood (second-strike doctrine mathematically eliminates victory), alternatives (total-war strategies with expectation of winning) collapse completely — they are no longer reachable. Resistance is negligible (0.05) because the constraint is not something any actor resists in principle; even actors who dislike the constraint (regimes that would prefer a world where annihilation is possible) cannot resist the physics. The flat measurement profile across the 80-year interval reflects the constraint's invariance: if nuclear arsenals persist, winnability remains collapsed; the constraint does not decay or intensify.
 *
 * PERSPECTIVAL GAP:
 *   A structural-contraction reading and a normative-reading (legal/ethical illegitimacy) are occupying different epistemic territories. From the structural seat, total war is not abandoned — it is physically unreachable. From the normative seat, total war is abandoned through law and ethics — it remains reachable in principle but has been declared illegitimate. An actor's strategic doctrine can deploy both frames simultaneously (we do not pursue total war because it is both impossible and wrong), but they are analytically distinct. The constraint described here is the structural one; its sibling readings describe the normative one. The engine computes these as distinct constraints with distinct ε values and distinct beneficiary/victim structures because they have different referents: this constraint is about reachability; the normative reading is about legitimacy. A regime that accepts the structural reading while rejecting the normative one would compute differently from a regime that accepts both, but both accept this constraint's existence as a structural fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality is near 0.0 (global civilian populations are fully subsidized by the constraint — they incur zero cost and gain pure prevention benefit). Victim directionality is hypothetical (only in counterfactual scenarios where nuclear weapons did not exist or where total war remained reachable). Non-nuclear states are excluded from the constraint's benefits and risks by structural fact, not by enforcement. Nuclear-armed great powers are the analytical observers; they operationalize the constraint through second-strike doctrine, but they are not targets or beneficiaries in the extraction sense — they are the seats that acknowledge and accept the winnability collapse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (total war as a reachable strategic option in pre-nuclear systems) is dead — it no longer exists as a strategic reachability question. The constraint persists not because the problem persists but because the solution (nuclear deterrence) is structural and irreversible so long as arsenals exist. This is not mandatrophy in the classic sense (function outlived but constraint maintained); rather, it is structural permanence: the constraint cannot atrophy because it is not maintained by institutional will but by physical fact. The disappearance verdict is 'world_unchanged' because the constraint is not contingent on social choice — its presence tracks the presence of nuclear weapons, not the presence of social institutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_frame_natural_law_vs_constructed,
    'Is the winnability collapse a brute fact of nuclear physics (natural law), or is it a property of the *current* strategic doctrine interpreting nuclear arsenals (constructed constraint that could be reframed)?',
    'Trace the genealogy of second-strike doctrine: if winnability collapse holds as a mathematical property independent of doctrine (e.g., any rational actor with assured retaliation capability would compute victory as impossible), the constraint is natural law. If winnability collapse depends on doctrinal acceptance (some regimes could reframe ''acceptable losses'' to claim victory despite retaliation), it is constructed.',
    'If natural law: Mountain classification is correct; the constraint requires no enforcement and no beneficiary structure. If constructed: the reading reclassifies to Tangled Rope or Piton (doctrine maintenance as the active constraint); beneficiaries become doctrine-holding elites who collect strategic stability benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_frame_natural_law_vs_constructed, conceptual, 'Whether winnability collapse is invariant physical fact or doctrine-dependent interpretation.').

omega_variable(
    sibling_reading_coexistence,
    'Can this reading (structural contraction: winnability physically removed) coexist with the normative reading (winnability dropped via legal/ethical norms) in a single analytical framework, or do they foreclose each other?',
    'Examine whether a regime can simultaneously accept that total war is (a) structurally impossible by physics AND (b) normatively illegitimate by treaty law. If both framings are deployed by the same analyst, they coexist; if one is invoked to deny the other''s applicability, they foreclose.',
    'If coexistence: the readings form a constraint family with shared referent (total war post-1945) but different epistemological bases (physics vs. norm). If foreclosure: one reading''s correctness entails the other is misconceptualized (the engine computes this from cs_structure.reading_relations).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Relationship between structural-impossibility and normative-illegitimacy framings of the same outcome.').

omega_variable(
    hypothetical_victim_set_validity,
    'What populations should be listed as beneficiaries of the winnability collapse when those populations exist only in counterfactual scenarios (the nuclear exchange that did not occur)?',
    'Distinguish between (a) actual beneficiaries (those spared by the constraint''s operation), who are the global population of 1945–present, and (b) hypothetical beneficiaries in the counterfactual where total war remained reachable. Determine whether beneficiary status should track actual prevention or counterfactual comparison.',
    'If actual beneficiaries are the measure: the constraint beneficiaries are living populations (global civilians since 1945). If counterfactual comparison is the measure: beneficiaries are the populations who would exist in the nuclear-exchange counterfactual and are spared by this reading''s reality. The beneficiary set changes the constraint''s classification implications.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hypothetical_victim_set_validity, preference, 'How to treat beneficiaries in constraints that prevent scenarios rather than allocate goods.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__structural_contraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(tota_tr_t10, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 10, 0.0).
narrative_ontology:measurement(tota_tr_t20, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 20, 0.0).
narrative_ontology:measurement(tota_tr_t40, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 40, 0.0).
narrative_ontology:measurement(tota_tr_t60, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 60, 0.0).
narrative_ontology:measurement(tota_tr_t80, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 80, 0.0).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(tota_be_t10, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 10, 0.0).
narrative_ontology:measurement(tota_be_t20, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 20, 0.0).
narrative_ontology:measurement(tota_be_t40, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 40, 0.0).
narrative_ontology:measurement(tota_be_t60, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 60, 0.0).
narrative_ontology:measurement(tota_be_t80, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 80, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(tota_su_t10, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 10, 0.0).
narrative_ontology:measurement(tota_su_t20, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 20, 0.0).
narrative_ontology:measurement(tota_su_t40, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 40, 0.0).
narrative_ontology:measurement(tota_su_t60, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 60, 0.0).
narrative_ontology:measurement(tota_su_t80, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 80, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__structural_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__structural_contraction_reading, 0.0).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945__normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945__strategic_culture_drift).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-part kernel contest: 'total_war_winnability_post1945'. The structural-contraction reading (this story) claims winnability elimination is a physics-based structural property of nuclear deterrence. The normative reading claims it is a legal/ethical prohibition developed through treaty law. The cultural-drift reading claims it is an ideational shift in strategic elite consensus. All three readings share the referent (total war post-1945 became un-reachable/un-thinkable) but differ in their explanation for why. The constraint stories are linked via network.affects_constraints because the structural claim, if true, would undermine the normative and cultural claims as explanations for the same outcome — the structure makes the normative and cultural shifts almost overdetermined. However, they coexist as live analytical positions; the readings are not foreclosed by each other in academic discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
