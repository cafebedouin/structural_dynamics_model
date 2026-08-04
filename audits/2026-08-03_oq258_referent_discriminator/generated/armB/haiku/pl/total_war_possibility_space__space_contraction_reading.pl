% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__space_contraction_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Total War Removed from Strategic Possibility Space
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel
 *   'total_war_possibility_space': the claim that nuclear weapons removed
 *   total war from being strategically thinkable, not merely from being
 *   preferable or rational. Under this reading, great-power states operate
 *   under a structural boundary condition wherein total war has exited the
 *   space of plans they can coherently formulate. The reading predicts
 *   observable institutional consequences: dissolution of conscription
 *   doctrine, atrophy of general staff war-gaming for civilizational
 *   conflict, and reorganization of strategic studies toward sub-nuclear
 *   domains. This reading is distinct from the deterrence equilibrium reading
 *   (total war remains reachable but is deterred by mutual vulnerability) and
 *   the nuclear taboo reading (total war became normatively prohibited
 *   through constructed social contract). The space_contraction reading makes
 *   a claim about WHAT IS THINKABLE — not what is preferred, not what is
 *   permitted by norms, but what can logically close as a plan with a winning
 *   outcome.
 *
 * KEY AGENTS:
 *   - great_power_states: institutional beneficiaries — operate under the constraint that total war has been removed from strategic possibility space
 *   - civilian_populations_globally: powerless beneficiaries — spared total mobilization and civilizational warfare, though they did not negotiate this benefit
 *   - military_planning_institutions: observer seats — once organized around total-war scenarios, now reorganized around sub-threshold operations and proxy conflicts
 *   - strategic_studies_discipline: observer seat — shifted analytical focus from great-power total war to deterrence stability, limited war, and sub-nuclear conflict
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.21).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.08).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.21).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Total War Removed from Strategic Possibility Space").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '31dc180c-1025-4291-a979-d2960bcd29ce').
narrative_ontology:cs_kernel_codification('31dc180c-1025-4291-a979-d2960bcd29ce', distributed).
narrative_ontology:cs_authority_grounding('31dc180c-1025-4291-a979-d2960bcd29ce', expertise).
narrative_ontology:cs_interpretation_layer_present('31dc180c-1025-4291-a979-d2960bcd29ce').
narrative_ontology:cs_reading_relation('31dc180c-1025-4291-a979-d2960bcd29ce', total_war_possibility_space__deterrence_equilibrium_reading, forecloses).
narrative_ontology:cs_reading_relation('31dc180c-1025-4291-a979-d2960bcd29ce', total_war_possibility_space__nuclear_taboo_reading, influences).
narrative_ontology:cs_axiom('31dc180c-1025-4291-a979-d2960bcd29ce', foundational, total_war_materially_unwinnable).
narrative_ontology:cs_axiom_status(total_war_materially_unwinnable, holdable).
narrative_ontology:cs_axiom_grounding('31dc180c-1025-4291-a979-d2960bcd29ce', total_war_materially_unwinnable, empirically_contingent).
narrative_ontology:cs_axiom('31dc180c-1025-4291-a979-d2960bcd29ce', foundational, possibility_space_genuine_contraction).
narrative_ontology:cs_axiom_status(possibility_space_genuine_contraction, holdable).
narrative_ontology:cs_axiom_grounding('31dc180c-1025-4291-a979-d2960bcd29ce', possibility_space_genuine_contraction, deontological).
narrative_ontology:cs_reference_frame('31dc180c-1025-4291-a979-d2960bcd29ce', total_war_strategic_possibility_open).
narrative_ontology:cs_drift_state('31dc180c-1025-4291-a979-d2960bcd29ce', post_thermonuclear_physics_internalization, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('31dc180c-1025-4291-a979-d2960bcd29ce', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, great_power_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, civilian_populations_globally).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot rationally choose total war as a strategy because the outcome space no longer includes any state victory condition that survives nuclear exchange. Their strategic planning apparatus operates under the structural constraint that great-power conflict has been removed from the thinkable alternatives. This is not a preference they hold — it is a boundary condition on what calculations are logically coherent.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, great_power_states, beneficiary,
    institutional, generational, analytical, global).

% Exist under a constraint that removes total war from great-power planning. They have no agency in this arrangement and cannot exit it, but they are spared the mobilization and destruction patterns that characterized pre-nuclear great-power conflict.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, civilian_populations_globally, beneficiary,
    powerless, generational, trapped, global).

% Institutional structures that once organized around the possibility of total war (general staff doctrine, force mobilization planning, conscription logistics) now must reorganize around sub-nuclear conflict modalities. Their previous planning apparatus has atrophied or been reoriented toward proxy conflicts and sub-threshold operations.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, military_planning_institutions, observer,
    institutional, generational, analytical, global).

% An epistemic community organized around analyzing great-power conflict now operates under the constraint that the highest-stakes outcome (total war) is not within the possibility space to be analyzed. Scholarship reorganizes toward deterrence stability, limited war theory, proxy conflict, and sub-nuclear escalation dynamics.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_studies_discipline, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This constraint solves no coordination problem in the classical sense. Rather, it establishes a boundary condition: great-power states operate under a structural impossibility, not a preference or agreement. The constraint removes an outcome from the thinkable, which implicitly coordinates behavior by eliminating an entire class of strategic options from rational consideration.
% TRANSFER_FUNCTION: No direct transfer occurs. The constraint operates via possibility-space contraction: it removes a state of the world (total war outcome) from the set of strategically coherent plans. States cannot transfer resources to conduct total war because the planning logic that would justify such preparation no longer closes (mutual destruction cannot be won).
% ABSENT_VOICES: States that might benefit from total war (if it remained possible) — revisionist powers seeking maximal territorial conquest, ideological movements seeking civilizational annihilation — are structurally excluded from advocating for total war because the outcome space itself has collapsed. They cannot coherently argue for something and then rationally execute it. Academic voices that argue total war remains thinkable are a minority position; they are present but marginalized.
% DISAPPEARANCE_RATIONALE: If nuclear weapons disappeared and the material constraint were removed, the possibility space would reopen: total war would again become a thinkable strategic outcome. This would reorganize military planning, conscription doctrine, and strategic studies scholarship. However, the contest is over whether the removal of the option from thought is a PHYSICAL impossibility (weapons constraint) or a COGNITIVE/INSTITUTIONAL lock-in (social contract narrative now internalized even if material constraint were removed). The 'contested' verdict reflects this irreducible ambiguity.
% FOUNDING_PROBLEM: Total war in the industrial age killed tens of millions and threatened civilizational survival. Great powers sought to preserve strategic capacity while managing the accumulation of destructive capacity. The founding problem was: how can great powers maintain mutual deterrence and strategic advantage without sliding into mutually annihilating warfare?
% FOUNDING_PROBLEM_CORROBORATION: Military strategists, nuclear deterrence theorists (Schelling, Waltz, Jervis), and official strategic doctrine across nuclear-armed states all attest the problem remains live: preventing inadvertent escalation to all-out war remains the central organizing problem of great-power military strategy. Outside beneficiary parties (states, civilians), strategic studies scholars acknowledge this as a genuine structural constraint, though they contest its SOURCE (whether material or cognitive).
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, contested).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.21, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__space_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_possibility_space__space_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is authored as a mountain (emerges_naturally: true) because the reading asserts that nuclear physics creates an unbreachable boundary: total war cannot be won because the destruction is mutual and unlimited. This is treated as a natural law of strategic logic, not a negotiated agreement. However, extractiveness is not zero (0.21 at interval end) because the constraint carries an epistemic cost: military institutions and strategic thinkers bear the cost of reorganization, uncertainty about whether the boundary is truly unbreakable (the material_vs_cognitive_constraint omega documents this), and the inability to plan for the highest-stakes outcome. The accessibility_collapse is very high (0.92) because once the nuclear physics is understood, the possibility space genuinely closes for rational great-power planning — there is no way to make total war strategically coherent. Resistance is very low (0.04) because no military institution can rationally argue for total war as an option; resistance would require demonstrating a path to victory, which the constraint forecloses. The measurement trajectory shows extractiveness declining from 1945 (0.35, when the constraint was new and its implications unclear) to 1980 (0.18, when the boundary was fully internalized), then rising slightly (0.21 at 2026) as new nuclear states acquire capacity and create renewed uncertainty about whether the boundary holds universally. Theater_ratio stays low (0.08–0.12) because the constraint carries minimal performative content — military institutions genuinely cannot plan what the constraint removes; the performance is minimal.
 *
 * PERSPECTIVAL GAP:
 *   The great_power_states seat and the civilian_populations seat should compute the same type (mountain, beneficiary directionality) because both experience the constraint as a boundary condition, not as enforcement requiring suppression. The military_planning_institutions seat experiences the constraint as an imposed limitation — it reorganized its cognitive and material apparatus around possibilities that have been removed — and may compute as a payer seat experiencing suppression (inability to plan certain scenarios). The strategic_studies_discipline seat is an observer; it experiences the constraint as a boundary condition on what can be theorized. The engine should compute consistent per-seat classifications because the structural situation is genuinely the same: the possibility space has contracted, affecting different institutions differently but in consistent ways.
 *
 * DIRECTIONALITY LOGIC:
 *   Great-power states and civilian populations are beneficiaries (d near 0.0) in the sense that the constraint is structurally favorable: it removes a catastrophic outcome. However, 'beneficiary' is a misnomer here because no one negotiated or agreed to the benefit — it is imposed by physics. The constraint has no payers in the classical sense because it is not a transfer mechanism. The extractiveness (0.21) represents the epistemic cost: the inability to plan for the highest-stakes scenario, the uncertainty about whether the boundary is truly universal (new nuclear states), and the vulnerability to any breakdown of the constraint. This is not extraction of resources from one party to another; it is a cost imposed on strategic rationality itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing total war while maintaining strategic deterrence) remains live according to every credible observer outside the benefiting parties. Military strategists continue to organize around maintaining strategic stability, which presupposes the constraint's persistence. If the constraint were removed, the founding problem would re-emerge as a live strategic challenge. This reading does NOT exhibit mandatrophy; the founding problem has NOT outlived the constraint. The measurement trajectory shows no decline in functional relevance; extractiveness is stable to rising, not declining.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_vs_cognitive_constraint,
    'Is total war removed from the strategically thinkable because of the MATERIAL FACT of nuclear destruction (physical impossibility of winning), or because of COGNITIVE/INSTITUTIONAL LOCK-IN (internalized taboo even if weapons were hypothetically removed)?',
    'Counterfactual analysis via gaming scenario: if nuclear weapons were magically removed overnight but memory of them persisted, would military planners immediately re-adopt total-war planning doctrines? Empirical observation: post-Cold War strategic culture shift; do states that acquire nuclear capacity immediately reorganize planning, or do they inherit institutional suppression of total-war thinking?',
    'If material: the constraint is a genuine mountain — the physics of thermonuclear exchange creates an unbreachable boundary. If cognitive/institutional: the constraint is a cultural achievement (tangled_rope or snare framing where the reading itself becomes extractive if it prevents adaptive thinking). This reading asserts the material reading; the omega documents the live challenge to it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_vs_cognitive_constraint, empirical, 'Whether the constraint is physical law or internalized social boundary.').

omega_variable(
    natural_law_vs_false_summit_candidate,
    'Do great-power military institutions genuinely treat total war as impossible-to-plan, or do they maintain contingency frameworks that preserve the option intellectually while publicly disavowing it?',
    'Declassified strategic war games, general staff planning documents, and credible testimony from strategic planners describing the actual planning space they operate within. Do contingencies for total war exist, or is the planning apparatus genuinely bounded?',
    'If contingencies exist, this reading has misidentified the constraint as categorical impossibility when it is actually high-cost suppression. If the planning apparatus is genuinely bounded, the reading is correct and the constraint is a natural law. FSM triggers if beneficiaries are identified while emerges_naturally is true — the omega documents whether this is a genuine mountain or a false summit where the reading itself benefits interested parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_false_summit_candidate, empirical, 'Whether the constraint represents genuine possibility-space contraction or strategic disavowal masking retained options.').

omega_variable(
    competing_reading_incoherence,
    'Can the deterrence_equilibrium_reading (total war remains reachable but deterred) and the space_contraction_reading (total war exits possibility space) both be true, or are they logically incompatible?',
    'Formal game-theoretic analysis: if total war remains in the strategic space but is deterred by mutual vulnerability, can deterrence fail? If yes, total war remains thinkable (deterrence reading is correct). If deterrence cannot fail without simultaneous cognitive/material collapse, the space_contraction reading is correct and forecloses the deterrence reading within a single framework.',
    'If the readings are logically incompatible (forecloses relation), this reading''s core axiom contradicts deterrence equilibrium and they cannot both be held in the same strategic framework. If they coexist (different parties hold different readings), the relation is coexists_with and both readings model distinct institutional positions. The resolution affects the cs_structure.reading_relations declaration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_reading_incoherence, conceptual, 'Whether this reading logically forecloses the deterrence equilibrium reading or merely competes with it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.08).
narrative_ontology:measurement_basis(tota_tr_t1945, observed).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__space_contraction_reading, theater_ratio, 1962, 0.09).
narrative_ontology:measurement_basis(tota_tr_t1962, observed).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__space_contraction_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement_basis(tota_tr_t1980, observed).
narrative_ontology:measurement(tota_tr_t2000, total_war_possibility_space__space_contraction_reading, theater_ratio, 2000, 0.11).
narrative_ontology:measurement_basis(tota_tr_t2000, observed).
narrative_ontology:measurement(tota_tr_t2015, total_war_possibility_space__space_contraction_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement_basis(tota_tr_t2015, observed).
narrative_ontology:measurement(tota_tr_t2026, total_war_possibility_space__space_contraction_reading, theater_ratio, 2026, 0.12).
narrative_ontology:measurement_basis(tota_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement_basis(tota_be_t1945, observed).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1962, 0.24).
narrative_ontology:measurement_basis(tota_be_t1962, observed).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1980, 0.18).
narrative_ontology:measurement_basis(tota_be_t1980, observed).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement_basis(tota_be_t2000, observed).
narrative_ontology:measurement(tota_be_t2015, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2015, 0.18).
narrative_ontology:measurement_basis(tota_be_t2015, observed).
narrative_ontology:measurement(tota_be_t2026, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2026, 0.21).
narrative_ontology:measurement_basis(tota_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_possibility_space__space_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% total_war_possibility_space kernel has three constraint stories: (1) space_contraction_reading (this story) — total war exits possibility space entirely via material physics; (2) deterrence_equilibrium_reading — total war remains reachable but is deterred by mutual vulnerability; (3) nuclear_taboo_reading — total war becomes normatively prohibited via constructed social contract. Each reading instantiates a different constraint with different ε (material/logical constraint vs. game-theoretic cost vs. social norm). The readings are not compatible within a single framework; space_contraction forecloses deterrence_equilibrium. Links are bidirectional: affects_constraints carries the forest topology; decomposition is documented in each story's kernel_context commentary and the omega documenting competing_reading_incoherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
