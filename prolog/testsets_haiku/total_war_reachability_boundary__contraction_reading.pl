% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Total War Reachability Contraction (Nuclear MAD Reading)
 *   domain: strategic_studies/international_relations/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel
 *   total_war_reachability_boundary. The contraction_reading asserts that
 *   nuclear weapons contracted the strategic space fundamentally: once both
 *   superpowers possessed secure second-strike capability (late 1960s
 *   onward), total war transitioned from a reachable outcome (albeit costly)
 *   to a physical and game-theoretic impossibility. The constraint is claimed
 *   as a mountain — not a human choice, coordination mechanism, or durable
 *   equilibrium that could be undone, but a hard boundary imposed by the
 *   structure of mutually assured destruction. No actor benefits from this
 *   constraint; all actors bear the burden of living under extinction risk.
 *   The reading distinguishes itself from the contingent_reachability_reading
 *   (which claims capability persists but atrophied, a piton) and the
 *   dropping_reading (which claims total war remains reachable but
 *   coordination prevents use, a rope). The three readings contest what
 *   'reachability' means and what role technology and actor choice play in
 *   maintaining or overturning the boundary.
 *
 * KEY AGENTS:
 *   - human_species (payer): bears species-level extinction risk from the physical possibility of total nuclear war
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.02).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.0).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Total War Reachability Contraction (Nuclear MAD Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "strategic_studies/international_relations/nuclear_deterrence").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, '384a149c-46b7-431d-afff-93c04794db3f').
narrative_ontology:cs_kernel_codification('384a149c-46b7-431d-afff-93c04794db3f', distributed).
narrative_ontology:cs_authority_grounding('384a149c-46b7-431d-afff-93c04794db3f', expertise).
narrative_ontology:cs_interpretation_layer_present('384a149c-46b7-431d-afff-93c04794db3f').
narrative_ontology:cs_reading_relation('384a149c-46b7-431d-afff-93c04794db3f', total_war_reachability_boundary__contingent_reachability_reading, forecloses).
narrative_ontology:cs_reading_relation('384a149c-46b7-431d-afff-93c04794db3f', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_axiom('384a149c-46b7-431d-afff-93c04794db3f', foundational, total_war_physically_impossible_under_symmetrical_second_strike).
narrative_ontology:cs_axiom_status(total_war_physically_impossible_under_symmetrical_second_strike, holdable).
narrative_ontology:cs_axiom_grounding('384a149c-46b7-431d-afff-93c04794db3f', total_war_physically_impossible_under_symmetrical_second_strike, empirically_contingent).
narrative_ontology:cs_axiom('384a149c-46b7-431d-afff-93c04794db3f', secondary, victory_logic_breaks_under_mutual_deterrence).
narrative_ontology:cs_axiom_status(victory_logic_breaks_under_mutual_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('384a149c-46b7-431d-afff-93c04794db3f', victory_logic_breaks_under_mutual_deterrence, instrumental).
narrative_ontology:cs_reference_frame('384a149c-46b7-431d-afff-93c04794db3f', mad_boundary_formalization).
narrative_ontology:cs_drift_state('384a149c-46b7-431d-afff-93c04794db3f', contemporary_proliferation_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('384a149c-46b7-431d-afff-93c04794db3f', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, human_species).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears the species-level extinction risk created by the physical possibility of total war via nuclear exchange. No exit from the planetary system. No ability to renegotiate the laws of physics that make MAD operational.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, human_species, payer,
    powerless, civilizational, trapped, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This constraint is not a coordination mechanism; it is a physical boundary. MAD (Mutually Assured Destruction) produces equilibrium outcomes, but the constraint itself is the mathematical impossibility of victory under conditions of symmetrical second-strike capability.
% TRANSFER_FUNCTION: No transfer. The constraint transfers nothing; it precludes an entire category of outcome (total war victory) by making it physically unrealizable.
% ABSENT_VOICES: Strategic theorists who hold the contingent_reachability_reading or dropping_reading readings would dispute this constraint's naturalness, arguing that reachability is technology-dependent or that deterrence is a coordination arrangement rather than a physical law. They are not absent from the conversation; they are alternative framings of the same kernel.
% DISAPPEARANCE_RATIONALE: If this constraint 'disappeared' (i.e., if total war became winnable), the world would undergo species-level extinction or face credible extinction risk. The question is whether the constraint itself can disappear: this reading asserts it cannot under current physics and weapons architecture. The disappearance_verdict is world_unchanged because the constraint is not a human choice — it is a feature of physical reality and game-theoretic structure.
% FOUNDING_PROBLEM: How can rational actors avoid mutual annihilation in a state system where conventional total war is possible? Early nuclear strategists posed this as a design problem solvable by doctrine and force posture. This reading proposes it was actually solved by physics: once both sides have secure second-strike capability, total war becomes logically impossible to win, regardless of doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Strategic theorists from Schelling (The Strategy of Conflict) through Jervis (The Meaning of the Nuclear Revolution) to contemporary game theorists document the problem as solved by the structure of MAD. No major strategic power currently believes total war is winnable under conditions of symmetrical nuclear forces. This assessment is corroborated outside the nuclear-armed states by non-aligned security scholars and independent arms-control researchers.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contraction_reading, 0.02, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Under the contraction reading, extractiveness is near-zero (0.02 at interval end) because the constraint extracts nothing from anyone; it is not a distribution mechanism. Suppression and theater are zero because there is no active enforcement machinery and no performative maintenance — the boundary is maintained by physics and game theory, not by human choice. Accessibility_collapse is very high (0.95) because once total war becomes impossible under MAD, there is no alternative path to victory; actors are trapped by the structure itself. Resistance is zero because the constraint is not actively defended by any party — it is simply the case. The measurement series show extractiveness and other metrics essentially flat across the interval at negligible values, consistent with a genuine natural law that imposes no ongoing overhead.
 *
 * PERSPECTIVAL GAP:
 *   There is only one stakeholder seat (human_species as payer) because the constraint imposes universal extinction risk and no actor benefits. The mountain reading yields identical classifications from all seats: no seat benefits, all seats bear the risk. The gap emerges between this reading and the sibling readings, not within this reading's stakeholder set. The sibling contingent_reachability_reading would identify technological capability-maintainers (military establishments, weapons labs) as de facto beneficiaries of keeping reachability alive; the sibling dropping_reading would identify deterrence-doctrine custodians as beneficiaries of the coordination function. This reading finds no beneficiaries because it denies those roles are meaningful under MAD — the boundary is not maintained by anyone's choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation is trivial for this reading: the single stakeholder (human_species) is a victim bearing extinction risk. Directional d=1.0 (full target). No beneficiary exists to generate d≈0.0. The engine would compute this from the declared victim set and the lack of any beneficiary, plus the stakeholder's trapped exit options and civilizational time horizon.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading shows no mandatrophy because it claims the founding problem (how to avoid mutual annihilation) is dead: physics and game theory solved it once second-strike capability became symmetric. The constraint's founding problem (total war avoidance) and its current function (total war impossibility) are aligned. No gap emerges between historical justification and present operation — the constraint operates as designed. The reading explicitly rejects the piton diagnosis (mandatrophy via atrophy) by claiming the boundary is natural, not maintained by human choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_mad_boundary,
    'Is the contraction of total war reachability a feature of physics and game theory (a natural law), or is it a durable but contingent equilibrium that could reverse if technology or actors'' beliefs change?',
    'Empirical falsification: emergence of credible first-strike capability that defeats second-strike survivability; or evidence that a rational actor has begun preparing for nuclear war under the belief that victory is possible despite MAD symmetry.',
    'If the boundary is natural: the constraint is a mountain and total war remains unreachable. If contingent: the constraint is a piton or a rope, depending on how actively it must be maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_mad_boundary, empirical, 'Whether total war reachability contraction is a law of physics/strategy or an equilibrium dependent on technology and actor belief.').

omega_variable(
    victim_universality_under_local_nuclear_exchange,
    'Does the species-level victim claim hold for limited nuclear exchange, or only for full-scale strategic exchange? Would a regional nuclear war between non-superpowers constitute total war under this reading?',
    'Clarification of what counts as ''total war'' in this reading: does it mean (a) any nuclear exchange, (b) exchange involving superpowers, (c) exchange that exhausts both sides'' arsenals, or (d) exchange that triggers cascading deterrence or climate effects (nuclear winter scenarios)?',
    'If regional exchanges are excluded, the victim set contracts from species to regional populations, and the constraint becomes partially avoidable through restraint. If all nuclear exchanges count as total war, the victim set remains universal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_universality_under_local_nuclear_exchange, conceptual, 'Scope and definition of ''total war'' in relation to victim universality.').

omega_variable(
    actor_rationality_assumption,
    'Does the contraction boundary hold only for rational actors with accurate information, or is it robust to irrational decision-making, miscalculation, or strategic deception?',
    'Historical case study of near-nuclear incidents (Cuban Missile Crisis, Able Archer 83) to determine whether irrationality or miscalculation nearly activated total war despite MAD structure; forward analysis of AI-controlled systems or adversarial deception scenarios.',
    'If the boundary requires rationality and accurate information, then the mountain is conditional and could collapse under uncertainty or irrationality. If robust to actor error, the mountain is more secure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(actor_rationality_assumption, empirical, 'Whether the total war contraction boundary is robust to irrational or miscalculating actors.').

omega_variable(
    reading_contention_total_war_reachability,
    'This constraint is one reading of the contested kernel total_war_reachability_boundary. The contraction_reading asserts that total war became unreachable; the contingent_reachability_reading asserts it remains reachable but atrophied; the dropping_reading asserts it remains reachable but coordination (deterrence doctrine) prevents its use. Which reading is correct depends on what ''reachability'' means: physical possibility, rational-actor deliberation, or something else. How should the ambiguity be resolved?',
    'Operator consultation and kernel framing clarification: does reachability mean (a) technological/physical capability to execute, (b) rational-actor optimal choice, (c) game-theoretic feasibility under symmetry, or (d) empirical belief-state of decision-makers? Each frame produces a different reading-family classification and different policy implications.',
    'The reading classification (mountain vs. piton vs. rope) depends entirely on which definition is adopted. A mountain reading requires that total war is physically impossible or game-theoretically infeasible; a piton reading requires that capability exists but is theatrically maintained; a rope reading requires that coordination solves the problem.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contention_total_war_reachability, conceptual, 'Cross-reading contention over what ''reachability'' means in the kernel and how it maps to constraint types.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__contraction_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement_basis(tota_tr_t1945, projected).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__contraction_reading, theater_ratio, 1962, 0.0).
narrative_ontology:measurement_basis(tota_tr_t1962, observed).
narrative_ontology:measurement(tota_tr_t1983, total_war_reachability_boundary__contraction_reading, theater_ratio, 1983, 0.0).
narrative_ontology:measurement_basis(tota_tr_t1983, observed).
narrative_ontology:measurement(tota_tr_t2000, total_war_reachability_boundary__contraction_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement_basis(tota_tr_t2000, observed).
narrative_ontology:measurement(tota_tr_t2026, total_war_reachability_boundary__contraction_reading, theater_ratio, 2026, 0.0).
narrative_ontology:measurement_basis(tota_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1945, 0.0).
narrative_ontology:measurement_basis(tota_be_t1945, projected).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1962, 0.01).
narrative_ontology:measurement_basis(tota_be_t1962, observed).
narrative_ontology:measurement(tota_be_t1983, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1983, 0.015).
narrative_ontology:measurement_basis(tota_be_t1983, observed).
narrative_ontology:measurement(tota_be_t2000, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2000, 0.02).
narrative_ontology:measurement_basis(tota_be_t2000, observed).
narrative_ontology:measurement(tota_be_t2026, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2026, 0.02).
narrative_ontology:measurement_basis(tota_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1945, 0.0).
narrative_ontology:measurement_basis(tota_su_t1945, projected).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1962, 0.0).
narrative_ontology:measurement_basis(tota_su_t1962, observed).
narrative_ontology:measurement(tota_su_t1983, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1983, 0.0).
narrative_ontology:measurement_basis(tota_su_t1983, observed).
narrative_ontology:measurement(tota_su_t2000, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2000, 0.0).
narrative_ontology:measurement_basis(tota_su_t2000, observed).
narrative_ontology:measurement(tota_su_t2026, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2026, 0.0).
narrative_ontology:measurement_basis(tota_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__contraction_reading, 0.0).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, nuclear_deterrence_doctrine_rope).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, strategic_stability_institutional_maintenance).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel total_war_reachability_boundary. The three readings (contraction, contingent_reachability, dropping) share the same referent — the reachability of total war in the nuclear age — but instantiate different constraints with different ε values, beneficiary structures, and classifications. The contraction_reading claims ε≈0 (no extraction, physical boundary), no beneficiaries, universal victims. The contingent_reachability_reading claims ε is technology-dependent and would produce a piton classification. The dropping_reading claims ε is coordination-dependent and would produce a rope classification. All three link to nuclear_deterrence_doctrine_rope and strategic_stability_institutional_maintenance, which together form the constraint family surrounding nuclear strategy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
