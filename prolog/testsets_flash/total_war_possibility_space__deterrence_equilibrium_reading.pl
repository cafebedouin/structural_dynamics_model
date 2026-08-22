% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__deterrence_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__deterrence_equilibrium_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: total_war_possibility_space__deterrence_equilibrium_reading
 *   human_readable: Deterrence Equilibrium for Total War
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint represents the 'deterrence equilibrium' reading of the
 *   total war possibility space. It posits that total war remains a
 *   strategically reachable option, but its initiation is deterred by the
 *   catastrophic costs of mutual vulnerability, primarily through nuclear
 *   weapons. This reading emphasizes rational cost-benefit calculations,
 *   continuous investment in war-fighting capabilities as a deterrent signal,
 *   and the ongoing development of strategic doctrines like counterforce
 *   targeting and escalation ladders. The constraint is a Tangled Rope
 *   because it provides a coordination function (preventing total war) but
 *   does so through asymmetric extraction (existential risk and resource
 *   diversion from the global populace).
 *
 * KEY AGENTS:
 *   - nuclear_powers: Primary agenda-setters and beneficiaries (institutional/constrained)
 *   - global_populace: Primary victims (powerless/trapped)
 *   - non_nuclear_states: Secondary victims (moderate/constrained)
 *   - defense_industries: Secondary beneficiaries (organized/mobile)
 *   - strategic_analysts: Observers (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, 0.6).
domain_priors:suppression_score(total_war_possibility_space__deterrence_equilibrium_reading, 0.7).
domain_priors:theater_ratio(total_war_possibility_space__deterrence_equilibrium_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(total_war_possibility_space__deterrence_equilibrium_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__deterrence_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__deterrence_equilibrium_reading, "Deterrence Equilibrium for Total War").
narrative_ontology:topic_domain(total_war_possibility_space__deterrence_equilibrium_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__deterrence_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__deterrence_equilibrium_reading, 'c488a28a-2c77-4abe-a7b7-8a14258a899c').
narrative_ontology:cs_kernel_codification('c488a28a-2c77-4abe-a7b7-8a14258a899c', implicit).
narrative_ontology:cs_authority_grounding('c488a28a-2c77-4abe-a7b7-8a14258a899c', practice).
narrative_ontology:cs_interpretation_layer_present('c488a28a-2c77-4abe-a7b7-8a14258a899c').
narrative_ontology:cs_reading_relation('c488a28a-2c77-4abe-a7b7-8a14258a899c', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_reading_relation('c488a28a-2c77-4abe-a7b7-8a14258a899c', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('c488a28a-2c77-4abe-a7b7-8a14258a899c', foundational, rational_actor_cost_benefit_calculation).
narrative_ontology:cs_axiom_status(rational_actor_cost_benefit_calculation, holdable).
narrative_ontology:cs_axiom_grounding('c488a28a-2c77-4abe-a7b7-8a14258a899c', rational_actor_cost_benefit_calculation, empirically_contingent).
narrative_ontology:cs_axiom('c488a28a-2c77-4abe-a7b7-8a14258a899c', foundational, mutual_vulnerability_as_deterrent).
narrative_ontology:cs_axiom_status(mutual_vulnerability_as_deterrent, holdable).
narrative_ontology:cs_axiom_grounding('c488a28a-2c77-4abe-a7b7-8a14258a899c', mutual_vulnerability_as_deterrent, empirically_contingent).
narrative_ontology:cs_reference_frame('c488a28a-2c77-4abe-a7b7-8a14258a899c', cold_war_strategic_stability).
narrative_ontology:cs_drift_state('c488a28a-2c77-4abe-a7b7-8a14258a899c', contemporary_multi_polar_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c488a28a-2c77-4abe-a7b7-8a14258a899c', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__deterrence_equilibrium_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_powers).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__deterrence_equilibrium_reading, defense_industries).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, global_populace).
narrative_ontology:constraint_victim(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and modernize nuclear arsenals, develop strategic doctrines, and engage in signaling to ensure mutual vulnerability. They benefit from the perceived security of deterrence but bear the immense cost and risk of maintaining the capability. Their exit is constrained by the perceived need to maintain parity.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, nuclear_powers, agenda_setter,
    institutional, generational, constrained, global).

% Lives under the constant existential threat of total war, bearing the psychological and material costs of defense spending without direct agency in strategic decisions. Their 'payment' is the risk to life and future, and their exit options are non-existent.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, global_populace, payer,
    powerless, immediate, trapped, universal).

% Are subject to the strategic calculations of nuclear powers and may be drawn into proxy conflicts or face nuclear threats. They bear the costs of regional instability and may seek their own nuclear capabilities as a constrained exit, further perpetuating the system.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, non_nuclear_states, payer,
    moderate, biographical, constrained, global).

% Profit from the continuous demand for advanced weaponry, surveillance systems, and strategic research driven by the deterrence paradigm. They are beneficiaries of the ongoing strategic competition, with relatively mobile capital and expertise.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, defense_industries, beneficiary,
    organized, biographical, mobile, global).

% Study and theorize about deterrence, escalation, and nuclear strategy. They provide intellectual frameworks that inform policy but are not direct actors in the enforcement or payment of the constraint.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__deterrence_equilibrium_reading, strategic_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the behavior of nuclear-armed states by establishing a shared understanding that initiating total war would result in unacceptable mutual destruction, thereby preventing direct conflict between them.
% TRANSFER_FUNCTION: Transfers a sense of 'negative peace' (absence of total war) to the global populace, in exchange for continuous investment in military capabilities and acceptance of existential risk. It also transfers resources from national budgets to defense industries.
% ABSENT_VOICES: Future generations, who bear the long-term risks of nuclear proliferation and environmental catastrophe without having consented to the deterrence framework. Also, global civil society movements advocating for disarmament, whose calls are often marginalized by strategic realpolitik.
% DISAPPEARANCE_RATIONALE: If the deterrence equilibrium vanished overnight (e.g., through a sudden, credible first-strike capability by one power, or a complete breakdown of communication), total war would become strategically viable, leading to immediate global conflict or capitulation, fundamentally reorganizing international relations and human civilization.
% FOUNDING_PROBLEM: The problem of preventing large-scale, catastrophic conflict between great powers in an era of increasingly destructive weaponry, particularly after the advent of nuclear weapons.
% FOUNDING_PROBLEM_CORROBORATION: Military strategists and government officials across nuclear-armed states consistently attest to the ongoing necessity of deterrence. Independent international relations scholars, while often critical of its costs, generally corroborate that the threat of total war remains a live concern that deterrence addresses, even if imperfectly.
narrative_ontology:disappearance_verdict(total_war_possibility_space__deterrence_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__deterrence_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_possibility_space__deterrence_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__deterrence_equilibrium_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__deterrence_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__deterrence_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the 'peace' achieved is precarious and comes at the cost of immense resource allocation to military readiness and the constant existential threat to humanity. Suppression is also high, as the global populace has no direct means to opt out of this strategic framework. Theater ratio is moderate, reflecting that while deterrence requires real capabilities, some aspects of strategic signaling and doctrine development can become performative. The metrics show fluctuations, particularly a peak during the Cold War (1962-1985) when extractiveness and suppression were highest, followed by a dip post-Cold War and a slight rise in recent years, reflecting renewed great power competition.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear powers perceive this as a necessary, albeit costly, coordination mechanism for global stability. The global populace and non-nuclear states experience it as a highly extractive and suppressive arrangement, where their security is held hostage to the strategic calculations of a few. The engine's per-seat classification should reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers, as agenda-setters, benefit from the stability deterrence provides, placing them closer to the beneficiary end. However, they also bear immense costs and risks, so their directionality is not fully at 0.0. The global populace and non-nuclear states are clear targets, bearing the existential risk and resource diversion, placing them closer to the target end. Defense industries are beneficiaries, profiting from the continuous demand for military hardware.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; the founding problem (preventing total war) is still very much 'live'. The classification as a Tangled Rope prevents mislabeling it as a pure Rope (ignoring the extraction) or a Snare (ignoring the coordination function). The ongoing investment in capabilities and strategic thought indicates active maintenance, not inertial decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_stability_empirical_basis,
    'Is the observed absence of total war genuinely attributable to the deterrence equilibrium, or are other factors (e.g., economic interdependence, normative shifts) more significant?',
    'Counterfactual historical analysis, comparative studies of non-nuclear great power relations, and empirical testing of deterrence theory''s predictions in limited conflicts.',
    'If deterrence is less effective than claimed, the extractiveness (costs of maintaining arsenals) is less justified, potentially reclassifying it closer to a Snare. If other factors are dominant, the coordination function of deterrence is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_stability_empirical_basis, empirical, 'Uncertainty regarding the causal efficacy of deterrence in preventing total war.').

omega_variable(
    total_war_strategic_reachability,
    'Is total war truly ''strategically reachable'' as a rational choice, or has the sheer scale of destruction rendered it unthinkable, even if technically possible?',
    'Analysis of decision-making under extreme duress, psychological studies of leaders facing existential choices, and historical case studies of near-misses. This is a conceptual distinction from the ''space contraction'' reading.',
    'If total war is genuinely unthinkable, the deterrence equilibrium is a theatrical performance (higher theater_ratio) maintaining a capability for a non-existent threat, pushing it towards a Piton or a more extractive Snare. This would align it closer to the ''space contraction'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(total_war_strategic_reachability, conceptual, 'Ambiguity regarding the actual strategic viability of total war as a rational option.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint best understood as a deterrence equilibrium, or as a nuclear taboo, or as a contraction of the possibility space for total war?',
    'Analysis of state behavior, rhetorical patterns, and resource allocation: continued investment in war-fighting capabilities supports deterrence; explicit normative condemnation supports taboo; absence of strategic planning for total war supports space contraction.',
    'Reclassification to ''nuclear_taboo_reading'' would imply lower extractiveness (normative constraint is cheaper) and higher suppression (internalized norm). Reclassification to ''space_contraction_reading'' would imply higher accessibility_collapse (alternatives truly gone) and potentially lower extractiveness (no need for active deterrence of the unthinkable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the ''total_war_possibility_space'' kernel. This reading emphasizes rational calculation and mutual vulnerability. Sibling readings (nuclear_taboo_reading, space_contraction_reading) offer alternative explanations for the absence of total war, focusing on normative prohibition or the inherent unthinkability of such conflict, respectively.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__deterrence_equilibrium_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement(tota_tr_t1985, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(tota_tr_t1991, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 1991, 0.18).
narrative_ontology:measurement(tota_tr_t2001, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2001, 0.15).
narrative_ontology:measurement(tota_tr_t2024, total_war_possibility_space__deterrence_equilibrium_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1945, 0.5).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1962, 0.65).
narrative_ontology:measurement(tota_be_t1985, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(tota_be_t1991, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 1991, 0.6).
narrative_ontology:measurement(tota_be_t2001, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement(tota_be_t2024, total_war_possibility_space__deterrence_equilibrium_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(tota_su_t1962, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1962, 0.75).
narrative_ontology:measurement(tota_su_t1985, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1985, 0.8).
narrative_ontology:measurement(tota_su_t1991, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 1991, 0.65).
narrative_ontology:measurement(tota_su_t2001, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2001, 0.68).
narrative_ontology:measurement(tota_su_t2024, total_war_possibility_space__deterrence_equilibrium_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__deterrence_equilibrium_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_possibility_space' kernel. The other readings are 'nuclear_taboo_reading' and 'space_contraction_reading', each offering a distinct explanation for the absence of total war.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
