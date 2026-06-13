% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contingent_reachability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contingent_reachability_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: total_war_reachability_boundary__contingent_reachability_reading
 *   human_readable: Total War Reachability Constraint (Contingent Technological Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   The total war reachability boundary is a constraint on strategic action:
 *   the shared understanding that nuclear weapons have made total war between
 *   peer powers technically reachable but outcome-indeterminate and
 *   catastrophically costly. This reading (contingent_reachability_reading)
 *   asserts the boundary is contingent on current technological equilibrium.
 *   Current weapons systems (submarine-launched ballistic missiles,
 *   distributed command-and-control, verification via satellite) make
 *   second-strike retaliation sufficiently survivable that a first strike
 *   cannot guarantee victory. But this equilibrium is not permanent.
 *   Hypersonic delivery, AI-enabled targeting, autonomous systems, and
 *   quantum cryptanalysis could contract the window for retaliation
 *   sufficiently that a first strike becomes strategically rational. The
 *   constraint is a piton: the function (deterrence coordination) has
 *   atrophied as the threat has receded into abstraction (no nuclear war has
 *   occurred in 80 years), but the machinery persists through institutional
 *   inertia and performative maintenance (doctrinal statements, force
 *   postures, arms-control theater). It could reverse with technological
 *   change, making reachability windows reappear and deterrence logic
 *   unstable.
 *
 * KEY AGENTS:
 *   - nuclear_weapons_states: maintain the deterrence framework and benefit from current stalemate; agenda-setter role
 *   - global_civilian_populations: depend on the boundary holding, bear existential risk if it reverses; payer role
 *   - states_developing_destabilizing_technologies: invest in technologies that could breach the boundary; beneficiary of current arbitrage window; organized power
 *   - arms_control_verifiers: measure technological change and detect boundary shifts; observer role
 *   - conventional_military_planners: operate under assumption that escalation is bounded; benefit from the constraint
 *   - technology_researchers: excluded from deterrence governance; develop dual-use systems that threaten the boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contingent_reachability_reading, 0.42).
domain_priors:suppression_score(total_war_reachability_boundary__contingent_reachability_reading, 0.28).
domain_priors:theater_ratio(total_war_reachability_boundary__contingent_reachability_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contingent_reachability_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contingent_reachability_reading, piton).
narrative_ontology:human_readable(total_war_reachability_boundary__contingent_reachability_reading, "Total War Reachability Constraint (Contingent Technological Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contingent_reachability_reading, "international_relations/strategic_studies/nuclear_deterrence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contingent_reachability_reading, '8dd4125c-81ba-443c-a9b5-8bff5e40de99').
narrative_ontology:cs_kernel_codification('8dd4125c-81ba-443c-a9b5-8bff5e40de99', formalized).
narrative_ontology:cs_authority_grounding('8dd4125c-81ba-443c-a9b5-8bff5e40de99', extraction).
narrative_ontology:cs_interpretation_layer_present('8dd4125c-81ba-443c-a9b5-8bff5e40de99').
narrative_ontology:cs_reading_relation('8dd4125c-81ba-443c-a9b5-8bff5e40de99', total_war_reachability_boundary__contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('8dd4125c-81ba-443c-a9b5-8bff5e40de99', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_axiom('8dd4125c-81ba-443c-a9b5-8bff5e40de99', foundational, reachability_contingent_on_technology).
narrative_ontology:cs_axiom_status(reachability_contingent_on_technology, holdable).
narrative_ontology:cs_axiom_grounding('8dd4125c-81ba-443c-a9b5-8bff5e40de99', reachability_contingent_on_technology, empirically_contingent).
narrative_ontology:cs_axiom('8dd4125c-81ba-443c-a9b5-8bff5e40de99', secondary, boundary_reversal_possible_with_tech_change).
narrative_ontology:cs_axiom_status(boundary_reversal_possible_with_tech_change, holdable).
narrative_ontology:cs_axiom_grounding('8dd4125c-81ba-443c-a9b5-8bff5e40de99', boundary_reversal_possible_with_tech_change, instrumental).
narrative_ontology:cs_reference_frame('8dd4125c-81ba-443c-a9b5-8bff5e40de99', mutual_assured_destruction_equilibrium).
narrative_ontology:cs_drift_state('8dd4125c-81ba-443c-a9b5-8bff5e40de99', contemporary_dual_use_technology_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8dd4125c-81ba-443c-a9b5-8bff5e40de99', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__contingent_reachability_reading, states_developing_destabilizing_technologies).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contingent_reachability_reading, global_civilian_populations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contingent_reachability_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contingent_reachability_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).
:- end_tests(total_war_reachability_boundary__contingent_reachability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater ratio is high (0.68 at 2026) because enforcement is now predominantly doctrinal performance rather than technological necessity. The constraint persists largely through strategic command posture statements, arms-control treaty text, and military training regimen — but actual maintenance of the reachability boundary is no longer active. Extraction is low-to-moderate (0.42) because the constraint does not transfer significant resources; it transfers existential risk from states to populations. Suppression is low (0.28) because there is no active coercive enforcement of the boundary — it persists by inertia. Resistance is moderate (0.55) at the structural level but much lower at class and individual levels, indicating that the constraint is widely accepted (populations do not actively resist nuclear deterrence doctrine) but strategically contested (emerging powers and dual-use technology developers resist the exclusionary frame). The measurement series shows theater ratio rising dramatically from 1945 to 2000, then stabilizing — this is the signature of a piton: functional value diminished but maintenance continued at high cost. Suppression requirement tracks theater loosely but lower: the constraint requires less active suppression as it becomes more performative (no one questions the deterrence doctrine at the class level; strategic questioning only occurs at the institutional level among states developing alternative technologies).
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear-weapons-states seat, the reachability boundary is a functional coordination mechanism they maintain through doctrine and force structure — it is a rope or tangled rope depending on how one weights the deterrence benefit to themselves versus the existential risk to populations. From the civilian-population seat, it is a constraint they depend on but have no control over — a pure structural fact that either holds or does not. From the states-developing-destabilizing-technologies seat, it is a boundary they are actively testing and preparing to breach — a barrier that will reverse, and they benefit from planning in that direction. From the arms-control-verifiers seat, it is a measurable technical equilibrium that is slowly eroding. These seats should compute different directionalities (d_values) from the structural data: nuclear weapons states near d=0.3 (modest beneficiary), populations near d=1.0 (full targets of the risk), destabilizing-tech states near d=-0.2 (arbitrage beneficiaries of its eventual reversal), verifiers near d=0.5 (symmetric, neither benefiting nor paying).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are states developing destabilizing technologies (they benefit from the current window where deterrence holds but they can prepare breaching strategies without consequence) and nuclear weapons states (they benefit from deterrence equilibrium, though their benefit is diminishing as they recognize the boundary is eroding). Victims are global civilian populations (they bear the existential risk if the boundary reverses and no backup deterrence mechanism exists). The directionality for nuclear weapons states is near the symmetric point (d ≈ 0.35) because they benefit from coordination (the deterrence framework) but are also targets if the system fails. Civilian populations get d ≈ 0.95 (nearly pure targets). States developing destabilizing tech get d ≈ -0.1 (they are structural beneficiaries of the boundary's eventual reversal, even though they appear as payers in the current moment). This asymmetry is the piton signature: beneficiaries are diffuse and distant (future states exploiting the breach), payers are universal and immediate (populations), and the agenda-setter (nuclear weapons states) is slowly realizing they are trapped by the constraint rather than protected by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real and live during the Cold War: how to prevent escalation into total war between peers with mutual annihilation capability. The founding problem status is now contested. Deterrence orthodoxy asserts the problem remains live and the coordination (reachability boundary) still works. Arms-control analysts and technology researchers assert the problem is dead (no peer has seriously moved toward initiating total war in decades) but the coordination persists as zombie inertia. The constraint exhibits classic mandatrophy: the original function (coordinating deterrence in the face of an active threat) has given way to performative maintenance (doctrinal statements, force postures, arms-control theater) because no one wants to explicitly abandon it (abandonment would require renegotiating the entire international security architecture) but the primary threat (mutual escalation spiral) is no longer the organizing principle of strategic life. The piton classification captures this: the function has atrophied, but the machinery persists. Fixing the mandatrophy would require either (a) explicit re-coordination around a new doctrine (defense dominance, technology governance, graduated deterrence) or (b) acceptance of the boundary's reversal and preparation for a post-deterrence strategic world. Neither is politically feasible at institutional scale.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'Is the reachability boundary a technological fact (contingent on current equilibrium) or a strategic doctrine that persists through political commitment?',
    'Historical analysis of technological inflection points (Cuban Missile Crisis, introduction of submarine-launched ballistic missiles, satellite verification systems, hypersonic weapons tests, AI-enabled targeting) and how states responded: did reachability windows open and states consciously choose not to exploit them, or was the boundary truly technologically immutable?',
    'If the boundary is doctrine, it can persist through commitment even as technology erodes it (becomes more piton-like, purely performative). If the boundary is technological, its reversal is inevitable as technology advances — the reading''s core premise. The classification turns on this distinction: doctrine-as-boundary reads as snare (coordinating states extract deterrence rent from populations); technology-as-boundary reads as piton (the function atrophies as tech makes it obsolete).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Whether the reachability boundary is contingent on technology or sustained by political commitment.').

omega_variable(
    piton_versus_scaffold_timeline,
    'If the boundary reverses through technological change, does it do so abruptly (piton scenario: sudden obsolescence, no warning) or gradually (scaffold scenario: declining effectiveness over years, time for policy adjustment)?',
    'Monitoring of dual-use technology trajectories (AI, quantum computing, hypersonics, autonomous systems) and prediction intervals for when each could contract reachability windows. Cross-checked against historical rates of strategic doctrine change and arms-control negotiation cycles.',
    'Abrupt reversal supports piton (the constraint persists through inertia and no one is maintaining it for its original function). Gradual reversal supports scaffold (there is still time to consciously transition, so the constraint retains intentional architecture). The temporal structure of breakdown determines which constraint-type correctly models the situation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_versus_scaffold_timeline, empirical, 'The timeline and mechanism of technological boundary erosion.').

omega_variable(
    beneficiary_structure_ambiguity,
    'Who truly benefits from the reachability boundary holding: nuclear powers (deterrence), populations (survival), or states developing destabilizing tech (the arbitrage window the boundary is beginning to provide)?',
    'Analysis of weapons development budgets, strategic doctrine statements, crisis-period decision-making, and resource allocation to deterrence maintenance versus destabilizing technology development. If resources are flowing to both with equal institutional priority, the boundary serves multiple beneficiaries; if resources flow to destabilization, the piton reading (atrophied function, maintained for theater) is supported.',
    'If the constraint is genuinely coordinating deterrence (all parties benefit from the boundary holding), it is a rope. If only nuclear powers benefit and populations bear cost, it is a tangled rope. If destabilizing-tech states benefit from its erosion, it is piton (maintained for show, not function). The beneficiary structure is the test.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, empirical, 'Whether the constraint''s benefits are distributed or captured.').

omega_variable(
    reading_contest_structural_delta,
    'This reading (contingent_reachability_reading) contests two sibling readings with mutually exclusive core premises. How would the classification differ under each reading?',
    'Compare the constraint''s computed type under each reading''s metrics and beneficiary structure: contraction_reading would classify as mountain (reachability is permanently off the table, no beneficiaries, no victims from reachability); dropping_reading would classify as rope (reachability is a coordination equilibrium, all parties benefit from the boundary). This reading classifies as piton (function atrophied, could reverse with tech). The divergence is the signal.',
    'If contraction_reading is true, the policy implication is acceptance of permanent deterrence equilibrium. If dropping_reading is true, the implication is stabilizing the coordination. If contingent_reachability_reading is true, the implication is governance of dual-use technology and explicit decision-making about the boundary before it reverses. The reading determines the remediation class.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_structural_delta, conceptual, 'Whether the kernel admits one reading or multiple coexisting readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contingent_reachability_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1962, 0.25).
narrative_ontology:measurement(tota_tr_t1980, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 1980, 0.45).
narrative_ontology:measurement(tota_tr_t2000, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2000, 0.62).
narrative_ontology:measurement(tota_tr_t2015, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2015, 0.68).
narrative_ontology:measurement(tota_tr_t2026, total_war_reachability_boundary__contingent_reachability_reading, theater_ratio, 2026, 0.68).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1962, 0.38).
narrative_ontology:measurement(tota_be_t1980, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(tota_be_t2000, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(tota_be_t2015, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(tota_be_t2026, total_war_reachability_boundary__contingent_reachability_reading, base_extractiveness, 2026, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1945, 0.05).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1962, 0.18).
narrative_ontology:measurement(tota_su_t1980, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 1980, 0.32).
narrative_ontology:measurement(tota_su_t2000, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(tota_su_t2015, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2015, 0.26).
narrative_ontology:measurement(tota_su_t2026, total_war_reachability_boundary__contingent_reachability_reading, suppression_requirement, 2026, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contingent_reachability_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__contingent_reachability_reading, 0.12).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contingent_reachability_reading, total_war_reachability_boundary__dropping_reading).

% DUAL FORMULATION NOTE:
% The total_war_reachability_boundary kernel decomposes into three constraint stories with structurally distinct ε values and beneficiary/victim structures. The contingent_reachability_reading (this file) models reachability as technology-dependent and the constraint as a piton. The contraction_reading (sibling) models reachability as permanently blocked and the constraint as a mountain. The dropping_reading (sibling) models reachability as a coordinated equilibrium and the constraint as a rope. The three readings coexist as live positions held by different institutional actors (nuclear weapons states, arms-control analysts, technology researchers); they do not logically foreclose each other within different parties' frameworks. The ε values differ markedly: piton reading has moderate extraction (0.42) because the boundary transfers existential risk but not resources; contraction reading has near-zero extraction (mountain); dropping reading has low extraction because the coordination is mutual benefit. The boundary between readings is not measurement-dependent — it is ontologically distinct claims about what reachability IS and whether it can reverse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__contingent_reachability_reading, organized, 0.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
