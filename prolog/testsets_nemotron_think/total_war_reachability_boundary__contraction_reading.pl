% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Total War Reachability Boundary (Contraction Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint story captures the contraction_reading of the
 *   total_war_reachability_boundary kernel: the claim that nuclear weapons
 *   and mutual assured destruction (MAD) have physically contracted the
 *   strategic space such that winnable total war has left the feasible set
 *   entirely. The constraint is the boundary itself — the absolute limit on
 *   what strategic outcomes are physically possible. Unlike the sibling
 *   readings, this reading treats the boundary as a mountain: a natural law
 *   arising from the physics of nuclear exchange, not a coordination
 *   equilibrium (rope) or atrophied capability (piton). No actor benefits
 *   from this boundary in the extractive sense — no state can win a total
 *   war, and the extinction risk is universal. The constraint extracts
 *   nothing; it simply defines the perimeter of the possible.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.05).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.1).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Total War Reachability Boundary (Contraction Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, 'a0b18f48-a456-4c7e-85cf-ed23a7a06218').
narrative_ontology:cs_kernel_codification('a0b18f48-a456-4c7e-85cf-ed23a7a06218', distributed).
narrative_ontology:cs_authority_grounding('a0b18f48-a456-4c7e-85cf-ed23a7a06218', distributed).
narrative_ontology:cs_reading_relation('a0b18f48-a456-4c7e-85cf-ed23a7a06218', total_war_reachability_boundary__dropping_reading, forecloses).
narrative_ontology:cs_reading_relation('a0b18f48-a456-4c7e-85cf-ed23a7a06218', total_war_reachability_boundary__contingent_reachability_reading, forecloses).
narrative_ontology:cs_axiom('a0b18f48-a456-4c7e-85cf-ed23a7a06218', foundational, total_war_physically_impossible_under_mad).
narrative_ontology:cs_axiom_status(total_war_physically_impossible_under_mad, holdable).
narrative_ontology:cs_axiom_grounding('a0b18f48-a456-4c7e-85cf-ed23a7a06218', total_war_physically_impossible_under_mad, empirically_contingent).
narrative_ontology:cs_axiom('a0b18f48-a456-4c7e-85cf-ed23a7a06218', secondary, nuclear_exchange_entails_species_extinction_risk).
narrative_ontology:cs_axiom_status(nuclear_exchange_entails_species_extinction_risk, holdable).
narrative_ontology:cs_axiom_grounding('a0b18f48-a456-4c7e-85cf-ed23a7a06218', nuclear_exchange_entails_species_extinction_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('a0b18f48-a456-4c7e-85cf-ed23a7a06218', absolute_nuclear_boundary).
narrative_ontology:cs_drift_state('a0b18f48-a456-4c7e-85cf-ed23a7a06218', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a0b18f48-a456-4c7e-85cf-ed23a7a06218', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contraction_reading, mutual_assured_destruction_logic).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contraction_reading, nuclear_weapons_create_absolute_strategic_boundary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States possessing nuclear weapons operate within the boundary. They cannot exit the constraint (physics applies regardless of doctrine) but can choose doctrines that acknowledge or deny the mountain's absoluteness. Their strategic planning is bounded by the physical impossibility of winnable total war.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, nuclear_armed_states, observer,
    institutional, generational, analytical, global).

% States without nuclear weapons are subject to the same physical boundary — any total war involving nuclear powers risks species-level effects. They have no leverage to alter the boundary but may advocate for disarmament to remove the extinction risk.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, non_nuclear_states, observer,
    moderate, generational, analytical, global).

% Analysts who study the boundary. The contraction_reading theorists (e.g., early MAD proponents) treat it as mountain; dropping_reading theorists (e.g., limited nuclear war advocates) treat it as rope; contingent_reading theorists (e.g., technological optimists) treat it as piton. Their exit is intellectual — they can switch readings.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, strategic_theorists, observer,
    analytical, biographical, analytical, global).

% The ultimate stakeholder. The mountain constraint makes species extinction the price of total war. No exit exists — the constraint is planetary and species-encompassing. This is not a beneficiary/victim relationship in the extractive sense; it is the condition of existence under MAD.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, human_species, observer,
    powerless, civilizational, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint does not solve a coordination problem — it is a physical boundary that makes certain coordination problems (how to fight and win total war) moot by removing the option from the feasible set. The coordination function belongs to the nuclear deterrence system (a separate constraint), not the boundary itself.
% TRANSFER_FUNCTION: No transfer occurs. The mountain constraint moves nothing between agents. The nuclear weapons system transfers risk (extinction risk to all), but the reachability boundary is the limit that makes that transfer total and irreversible.
% ABSENT_VOICES: Future generations who would inherit the consequences of any boundary failure. They cannot participate in current strategic discourse but bear the full cost of the mountain's potential falsification. Also absent: voices from the pre-nuclear era who experienced winnable total war as feasible — their experience is historically excluded from the current strategic ontology.
% DISAPPEARANCE_RATIONALE: If the mountain constraint vanished — meaning winnable total war re-entered the feasible set — the entire post-1945 strategic order would collapse. Deterrence doctrines, arms control regimes, non-proliferation treaties, and great power conflict management all presuppose the boundary's absoluteness. Their rearrangement would be total and immediate.
% FOUNDING_PROBLEM: The problem of how to prevent total war between nuclear-armed great powers without relying on perpetual luck or enlightened leadership. The mountain reading asserts this problem was solved by physics itself: the weapons made the war unwinnable, rendering the problem obsolete by removing its object.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing great power total war) is corroborated as live by the continued existence of nuclear arsenals, the persistence of deterrence doctrines, and the universal consensus among states that nuclear war must be prevented. No state has declared the problem solved and disarmed unilaterally. The mountain reading's claim that physics solved it is contested by dropping_reading and contingent_reachability_reading proponents who argue human institutions (not physics) maintain the peace.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contraction_reading, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   The mountain classification rests on three pillars: (1) emerges_naturally = true — the boundary arises from physics (energy release, fallout, climatic effects), not human design; (2) accessibility_collapse = 0.95 — once MAD logic is understood, no strategic alternative to mutual vulnerability exists for nuclear-armed adversaries; (3) resistance = 0.05 — no serious strategic doctrine contests the physical impossibility, only the political interpretation. Extractiveness and suppression are near-zero because the constraint does not extract resources or coerce behavior — it defines the game's rules. Theater is minimal because there is no performative maintenance of a physical law.
 *
 * PERSPECTIVAL GAP:
 *   The dropping_reading and contingent_reachability_reading would compute different seat classifications because they model the boundary as coordination (rope) or inertial artifact (piton) with active enforcement and beneficiaries (deterrence establishments, arms control bureaucracies). This reading denies those structural features — the boundary simply is. The divergence between readings is the measurement target.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is uniform across all agents (d ≈ 0.5 symmetric) because the mountain imposes identical physical limits on all nuclear-armed states and the species as a whole. No agent can position itself as beneficiary — the constraint subsidizes no one and extracts from no one. The universal victim set (extinction risk) is a property of the nuclear weapons system, not this mountain constraint; the mountain is the boundary that makes the victim set universal. The engine will derive symmetric directionality from the absence of beneficiaries/victims and the analytical exit_options of all observers.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable — a mountain has no mandate to atrophy. The mandatrophy concept applies to constructed constraints whose founding problem has vanished. This reading claims the founding problem (survival under nuclear threat) is eternal and the boundary is permanent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the contraction_reading the correct reading of the total_war_reachability_boundary kernel, or do sibling readings (dropping_reading, contingent_reachability_reading) better capture strategic reality?',
    'Empirical test: if a major power conflict occurs without nuclear escalation to species-extinction levels, the mountain claim is falsified and dropping_reading or contingent_reachability_reading gains validity. Historical near-misses (Cuban Missile Crisis, Able Archer) provide contested evidence.',
    'If mountain claim fails, the constraint reclassifies from mountain to rope (dropping_reading) or piton (contingent_reachability_reading), fundamentally altering the strategic ontology — from absolute physical limit to coordination equilibrium or reversible technological artifact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'Contestation between kernel readings on whether total war is physically impossible, improbable but reachable, or technologically contingent.').

omega_variable(
    residual_probability_vs_impossibility,
    'Does MAD create a genuine physical impossibility (probability = 0) of winnable total war, or merely an astronomically low but non-zero probability?',
    'Formal modeling of nuclear exchange scenarios with current arsenals, command/control reliability, and escalation pathways. If any scenario yields a non-extinction outcome for one side, the mountain claim weakens.',
    'Non-zero probability, however small, means the constraint is not a mountain but a rope or snare with extreme suppression — changing the classification from natural law to coordination/extraction structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_probability_vs_impossibility, empirical, 'Whether the mountain''s ''absolute'' claim holds or admits measure-zero exceptions that reopen the feasible set.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of total war structural (physics of nuclear exchange) or institutional (deterrence doctrines, arms control, taboo)?',
    'Counterfactual analysis: if all nuclear weapons were abolished tomorrow, would total war return to the feasible set immediately (institutional suppression) or remain impossible due to other WMD/technological factors (structural)?',
    'If institutional, the mountain is maintained by active coordination (rope-like); if structural, it is a genuine natural law. This determines whether the constraint''s persistence requires enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Whether the constraint''s suppression is inherent to physics or maintained by human institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twrb_cr_tr_t0, total_war_reachability_boundary__contraction_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(twrb_cr_tr_t20, total_war_reachability_boundary__contraction_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(twrb_cr_tr_t40, total_war_reachability_boundary__contraction_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(twrb_cr_tr_t60, total_war_reachability_boundary__contraction_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(twrb_cr_tr_t80, total_war_reachability_boundary__contraction_reading, theater_ratio, 80, 0.05).

% Extraction over time
narrative_ontology:measurement(twrb_cr_be_t0, total_war_reachability_boundary__contraction_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(twrb_cr_be_t20, total_war_reachability_boundary__contraction_reading, base_extractiveness, 20, 0.05).
narrative_ontology:measurement(twrb_cr_be_t40, total_war_reachability_boundary__contraction_reading, base_extractiveness, 40, 0.05).
narrative_ontology:measurement(twrb_cr_be_t60, total_war_reachability_boundary__contraction_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement(twrb_cr_be_t80, total_war_reachability_boundary__contraction_reading, base_extractiveness, 80, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(twrb_cr_su_t0, total_war_reachability_boundary__contraction_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(twrb_cr_su_t20, total_war_reachability_boundary__contraction_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(twrb_cr_su_t40, total_war_reachability_boundary__contraction_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(twrb_cr_su_t60, total_war_reachability_boundary__contraction_reading, suppression_requirement, 60, 0.1).
narrative_ontology:measurement(twrb_cr_su_t80, total_war_reachability_boundary__contraction_reading, suppression_requirement, 80, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contraction_reading, global_infrastructure).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, nuclear_deterrence_coordination).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, arms_control_regime).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, strategic_stability_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is the contraction_reading of the total_war_reachability_boundary kernel. It differs from dropping_reading (rope: deterrence as coordination equilibrium) and contingent_reachability_reading (piton: atrophied but reversible technological boundary) in ε (0.05 vs. ~0.3-0.5), beneficiary structure (none vs. deterrence establishments), and temporal dynamics (static vs. drifting). The three readings form a constraint family linked by shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
