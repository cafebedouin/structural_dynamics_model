% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__dropping_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Total War Reachability Boundary (Dropping Probability Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint describes the boundary of total war reachability, which
 *   this 'dropping_reading' interprets as a Tangled Rope. While the mechanism
 *   of nuclear deterrence itself can be seen as a Rope (a coordination
 *   equilibrium), the boundary it maintains is a Tangled Rope due to the
 *   inherent extraction (existential threat) and suppression (of non-nuclear
 *   states' agency) involved. The reading emphasizes that the probability of
 *   total war has dropped since the Cold War peak, but its reachability
 *   remains a structural feature of the international system. The metrics
 *   reflect this decreasing perceived threat over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.7).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.65).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Total War Reachability Boundary (Dropping Probability Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, '15907aa7-deff-4ee5-9345-c8077a07c4a0').
narrative_ontology:cs_kernel_codification('15907aa7-deff-4ee5-9345-c8077a07c4a0', implicit).
narrative_ontology:cs_authority_grounding('15907aa7-deff-4ee5-9345-c8077a07c4a0', extraction).
narrative_ontology:cs_interpretation_layer_present('15907aa7-deff-4ee5-9345-c8077a07c4a0').
narrative_ontology:cs_reading_relation('15907aa7-deff-4ee5-9345-c8077a07c4a0', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_reading_relation('15907aa7-deff-4ee5-9345-c8077a07c4a0', total_war_reachability_boundary__contraction_reading, forecloses).
narrative_ontology:cs_axiom('15907aa7-deff-4ee5-9345-c8077a07c4a0', foundational, total_war_remains_feasible).
narrative_ontology:cs_axiom_status(total_war_remains_feasible, holdable).
narrative_ontology:cs_axiom_grounding('15907aa7-deff-4ee5-9345-c8077a07c4a0', total_war_remains_feasible, empirically_contingent).
narrative_ontology:cs_axiom('15907aa7-deff-4ee5-9345-c8077a07c4a0', foundational, deterrence_is_coordination_game).
narrative_ontology:cs_axiom_status(deterrence_is_coordination_game, holdable).
narrative_ontology:cs_axiom_grounding('15907aa7-deff-4ee5-9345-c8077a07c4a0', deterrence_is_coordination_game, conventional).
narrative_ontology:cs_reference_frame('15907aa7-deff-4ee5-9345-c8077a07c4a0', cold_war_deterrence_equilibrium).
narrative_ontology:cs_drift_state('15907aa7-deff-4ee5-9345-c8077a07c4a0', post_cold_war_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('15907aa7-deff-4ee5-9345-c8077a07c4a0', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, strategic_elites).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, global_populations).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and enforce the nuclear deterrence regime, benefiting from the strategic stability it provides and the leverage it grants in international relations. They bear the cost of maintaining arsenals but gain security from direct conflict.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_powers, agenda_setter,
    institutional, civilizational, constrained, global).

% Live under the existential threat of nuclear war, with limited agency to alter the deterrence framework. They are coordinated into a non-nuclear status but pay the cost of suppressed sovereignty and constant threat.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, non_nuclear_states, payer,
    moderate, biographical, constrained, national).

% Bear the ultimate existential risk of nuclear conflict. They have no direct means to exit the system or influence its operation, being trapped by the decisions of nuclear powers.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, global_populations, payer,
    powerless, immediate, trapped, universal).

% Manage the complex systems of nuclear deterrence, arms control, and strategic theory. They gain influence, resources, and professional standing from the persistence and perceived necessity of the deterrence framework.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, strategic_elites, beneficiary,
    institutional, generational, analytical, global).

% Advocate for nuclear disarmament and alternative security architectures. They are largely excluded from the core decision-making processes of nuclear states and bear the psychological and political costs of challenging the established order.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, anti_nuclear_movements, excluded,
    organized, biographical, constrained, global).

% Monitor nuclear proliferation, facilitate arms control negotiations, and provide forums for dialogue. They operate within the existing deterrence framework, seeking to mitigate risks rather than fundamentally alter the total war reachability boundary.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, international_organizations, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__dropping_reading, nuclear_powers).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__dropping_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents direct military conflict between nuclear-armed states by establishing a credible threat of mutually assured destruction, thereby coordinating their strategic behavior towards non-aggression.
% TRANSFER_FUNCTION: Transfers a degree of strategic stability and security from direct great power conflict to nuclear powers, at the cost of existential risk, suppressed sovereignty for non-nuclear states, and a constant psychological burden on global populations.
% ABSENT_VOICES: Future generations, who would bear the ultimate consequences of nuclear war, and populations in non-nuclear states, whose security is contingent on the actions of nuclear powers, are largely absent from the core strategic dialogues. Disarmament advocates are also marginalized.
% DISAPPEARANCE_RATIONALE: If the boundary of total war reachability vanished (e.g., nuclear weapons became impossible or irrelevant), the entire international security architecture, including alliances, military doctrines, and diplomatic norms, would undergo a fundamental and rapid reorganization.
% FOUNDING_PROBLEM: Preventing catastrophic great power wars, particularly after the advent of nuclear weapons made such conflicts potentially civilization-ending.
% FOUNDING_PROBLEM_CORROBORATION: Strategic analysts, historians of the Cold War, and international relations scholars widely corroborate that nuclear deterrence emerged to solve the problem of preventing direct conflict between nuclear-armed states. While the nature of great power competition has evolved, the core problem of preventing catastrophic war remains live.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__dropping_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__dropping_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.70 at end) remains high because the ultimate cost of failure is existential, even if the probability of that failure has decreased. Suppression (0.65) is substantial as the nuclear order actively constrains the strategic choices of non-nuclear states and suppresses alternatives like disarmament. Theater ratio (0.30) has decreased from Cold War highs, reflecting less overt posturing, but still exists in military exercises and declaratory policy. Accessibility collapse is high (0.80) because genuine alternatives to the nuclear order are extremely difficult to achieve. Resistance (0.40) is moderate, with anti-nuclear movements existing but lacking the power to dismantle the system. The time series for extractiveness, suppression, and theater ratio show a decline, reflecting the 'dropping probability' aspect of this reading, but stabilize at high-to-moderate levels, indicating continued reachability and structural impact.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear powers perceive the total war reachability boundary as a necessary evil or even a stable coordination mechanism (a Rope), ensuring their security. For non-nuclear states and global populations, the same boundary is an imposed threat and a source of extraction (a Snare or Tangled Rope), limiting their sovereignty and imposing existential risk. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers and strategic elites are beneficiaries, gaining security and influence from the deterrence framework (low directionality). Non-nuclear states and global populations are victims/payers, bearing the existential risk and suppressed agency (high directionality). International organizations and anti-nuclear movements operate within or against this framework, with varying degrees of constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_stability_ambiguity,
    'Is the current deterrence equilibrium genuinely stable, or is it prone to collapse under specific conditions (e.g., technological shifts, miscalculation, proliferation)?',
    'Analysis of historical near-misses, modeling of future technological impacts on strategic stability, and empirical study of decision-making under extreme stress.',
    'If found to be highly unstable, the constraint''s effective extractiveness and suppression would be higher than currently measured, pushing its classification closer to a Snare due to the amplified risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_stability_ambiguity, empirical, 'Uncertainty regarding the long-term stability of nuclear deterrence.').

omega_variable(
    extraction_justification_ambiguity,
    'Is the existential threat to global populations a necessary and unavoidable cost for preventing great power war, or an unjustified extraction by nuclear powers for their own security interests?',
    'Conceptual analysis of moral philosophy and international ethics, combined with policy debates on alternative security paradigms (e.g., common security, disarmament).',
    'If deemed unjustified, the constraint''s classification would lean more strongly towards a Snare, as the coordination story would be seen as cover for pure extraction. If deemed necessary, it would reinforce the Tangled Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_justification_ambiguity, conceptual, 'Whether the costs imposed by deterrence are justifiable.').

omega_variable(
    probability_of_use_measurement,
    'How reliably can the probability of nuclear weapon use be measured or estimated, given the lack of historical data, the complexity of human decision-making under extreme stress, and the influence of ''black swan'' events?',
    'Development of more robust probabilistic risk assessment models for low-probability, high-impact events, incorporating insights from cognitive science and complex systems theory.',
    'If the probability is found to be significantly higher than current estimates, the perceived extractiveness of the constraint would increase, potentially shifting its classification. If lower, it would reinforce the ''dropping probability'' aspect of this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probability_of_use_measurement, empirical, 'Uncertainty in quantifying the probability of nuclear weapon use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_reachability_boundary__dropping_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(tota_tr_t6, total_war_reachability_boundary__dropping_reading, theater_ratio, 6, 0.45).
narrative_ontology:measurement(tota_tr_t12, total_war_reachability_boundary__dropping_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(tota_tr_t18, total_war_reachability_boundary__dropping_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement(tota_tr_t24, total_war_reachability_boundary__dropping_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(tota_tr_t30, total_war_reachability_boundary__dropping_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_reachability_boundary__dropping_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(tota_be_t6, total_war_reachability_boundary__dropping_reading, base_extractiveness, 6, 0.85).
narrative_ontology:measurement(tota_be_t12, total_war_reachability_boundary__dropping_reading, base_extractiveness, 12, 0.8).
narrative_ontology:measurement(tota_be_t18, total_war_reachability_boundary__dropping_reading, base_extractiveness, 18, 0.75).
narrative_ontology:measurement(tota_be_t24, total_war_reachability_boundary__dropping_reading, base_extractiveness, 24, 0.72).
narrative_ontology:measurement(tota_be_t30, total_war_reachability_boundary__dropping_reading, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_reachability_boundary__dropping_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(tota_su_t6, total_war_reachability_boundary__dropping_reading, suppression_requirement, 6, 0.75).
narrative_ontology:measurement(tota_su_t12, total_war_reachability_boundary__dropping_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(tota_su_t18, total_war_reachability_boundary__dropping_reading, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(tota_su_t24, total_war_reachability_boundary__dropping_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(tota_su_t30, total_war_reachability_boundary__dropping_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, nuclear_proliferation_constraint).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, great_power_competition_norms).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'total_war_reachability_boundary' kernel, focusing on the dropping probability of total war while maintaining its reachability. It is linked to sibling readings that offer alternative interpretations of this boundary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
