% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__countervailing_thinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__countervailing_thinkable, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: war_winnability_post_1945__countervailing_thinkable
 *   human_readable: Counterforce Winnability Doctrine (Post-1945)
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   The countervailing reading holds that nuclear weapons constrain but do
 *   not eliminate the possibility of limited victory through counterforce
 *   targeting. This doctrine, originating in early Cold War extended
 *   deterrence requirements, persists as the operational foundation for U.S.
 *   and Russian nuclear posture despite the end of the founding strategic
 *   problem. The constraint coordinates crisis signaling and escalation
 *   management (genuine coordination function) while simultaneously
 *   extracting sustained force structure, budget authority, and planning
 *   relevance for the military-industrial complex and strategic establishment
 *   (asymmetric extraction). Arms control regimes and strategic stability
 *   institutions bear the cost through undermined verification, eroded crisis
 *   stability, and negotiating positions weakened by counterforce-optimized
 *   force postures. The claim/metric gap is deliberate: the constraint is
 *   CLAIMED as tangled_rope (coordination + extraction) while the authored
 *   metrics describe the operational reality — the engine measures divergence
 *   from the claimed type.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, 0.62).
domain_priors:suppression_score(war_winnability_post_1945__countervailing_thinkable, 0.55).
domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, extractiveness, 0.62).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__countervailing_thinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__countervailing_thinkable, "Counterforce Winnability Doctrine (Post-1945)").
narrative_ontology:topic_domain(war_winnability_post_1945__countervailing_thinkable, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__countervailing_thinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__countervailing_thinkable, 'd48010ae-3bfa-4e7f-90de-d4ec81246f7a').
narrative_ontology:cs_kernel_codification('d48010ae-3bfa-4e7f-90de-d4ec81246f7a', distributed).
narrative_ontology:cs_authority_grounding('d48010ae-3bfa-4e7f-90de-d4ec81246f7a', practice).
narrative_ontology:cs_interpretation_layer_present('d48010ae-3bfa-4e7f-90de-d4ec81246f7a').
narrative_ontology:cs_reading_relation('d48010ae-3bfa-4e7f-90de-d4ec81246f7a', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('d48010ae-3bfa-4e7f-90de-d4ec81246f7a', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('d48010ae-3bfa-4e7f-90de-d4ec81246f7a', foundational, counterforce_victory_achievable).
narrative_ontology:cs_axiom_status(counterforce_victory_achievable, holdable).
narrative_ontology:cs_axiom_grounding('d48010ae-3bfa-4e7f-90de-d4ec81246f7a', counterforce_victory_achievable, empirically_contingent).
narrative_ontology:cs_axiom('d48010ae-3bfa-4e7f-90de-d4ec81246f7a', foundational, limited_nuclear_war_controllable).
narrative_ontology:cs_axiom_status(limited_nuclear_war_controllable, holdable).
narrative_ontology:cs_axiom_grounding('d48010ae-3bfa-4e7f-90de-d4ec81246f7a', limited_nuclear_war_controllable, empirically_contingent).
narrative_ontology:cs_axiom('d48010ae-3bfa-4e7f-90de-d4ec81246f7a', secondary, extended_deterrence_requires_war_fighting_options).
narrative_ontology:cs_axiom_status(extended_deterrence_requires_war_fighting_options, holdable).
narrative_ontology:cs_axiom_grounding('d48010ae-3bfa-4e7f-90de-d4ec81246f7a', extended_deterrence_requires_war_fighting_options, instrumental).
narrative_ontology:cs_reference_frame('d48010ae-3bfa-4e7f-90de-d4ec81246f7a', counterforce_doctrine_framework).
narrative_ontology:cs_drift_state('d48010ae-3bfa-4e7f-90de-d4ec81246f7a', post_cold_war_great_power_competition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d48010ae-3bfa-4e7f-90de-d4ec81246f7a', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, strategic_planning_establishment).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, strategic_stability_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, allied_non_nuclear_states).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__countervailing_thinkable, nuclear_deterrence_requirements_justify_force_structure).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__countervailing_thinkable, limited_nuclear_options_enhance_credibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives sustained funding, mission justification, and procurement programs from the doctrinal requirement to maintain counterforce targeting capabilities and limited nuclear war planning. The constraint that 'winnability remains thinkable' directly authorizes force modernization, targeting infrastructure, and exercise regimes that would be harder to justify under pure deterrence-only frameworks.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, beneficiary,
    institutional, generational, arbitrage, global).

% Sets nuclear posture, targeting guidance, and operational plans. Gains professional mission continuity and institutional relevance from the requirement to plan for 'winnable' nuclear scenarios. Career paths and bureaucratic equities are built around counterforce planning competence. Exit would mean abandoning the core professional identity of nuclear strategists.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, strategic_planning_establishment, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__countervailing_thinkable, strategic_planning_establishment, beneficiary).

% Authorizes nuclear posture reviews and targeting guidance. Benefits from the perceived credibility of limited nuclear options in crisis bargaining, but bears the risk of inadvertently lowering the nuclear threshold. Constrained by alliance commitments and domestic political structures that make doctrinal shifts costly.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, nuclear_armed_states_leadership, agenda_setter,
    institutional, biographical, constrained, national).

% Treaty frameworks (START, INF, CTBT, NPT review cycles) are undermined when doctrinal planning assumes counterforce victory scenarios. Verification regimes become harder to negotiate when one party's force structure is optimized for warfighting rather than deterrence alone. Exit from the regime is possible but carries high diplomatic and strategic costs.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes, payer,
    organized, generational, constrained, global).

% Crisis management mechanisms, hotlines, and stability dialogues are eroded when operational plans assume escalation can be controlled. The thinkability of limited victory creates pressure for launch-under-warning postures and pre-delegation authorities that reduce decision time. These institutions bear the cost of increased crisis instability without authority to change doctrinal assumptions.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, strategic_stability_institutions, payer,
    organized, generational, constrained, global).

% Civil society and diplomatic actors pushing for nuclear elimination or no-first-use commitments. Their voices are structurally excluded from nuclear planning cells and posture reviews. They would object to the operationalization of winnability but lack access to the forums where targeting guidance is set.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, disarmament_advocacy_networks, excluded,
    moderate, generational, mobile, global).

% Depend on extended deterrence but have no voice in counterforce planning. Bear the risk of being drawn into limited nuclear exchanges on their territory. Their security architecture is shaped by doctrines they cannot influence; exit from alliance structures is politically prohibitive.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, allied_non_nuclear_states, payer,
    moderate, biographical, constrained, regional).

% Academic strategists, historians, and independent analysts who study the constraint from outside the operational apparatus. They see the full structural asymmetry between the coordination function (deterrence stability) and the extraction function (force structure perpetuation). No material stake in the outcome.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework for crisis management and escalation control among nuclear-armed states by defining what constitutes a 'limited' nuclear exchange, enabling signaling and off-ramps that pure deterrence lacks.
% TRANSFER_FUNCTION: Moves strategic credibility, budget authority, and operational relevance from arms control institutions and stability mechanisms to the military-industrial complex and strategic planning establishment, by treating counterforce victory scenarios as operationally valid planning cases.
% ABSENT_VOICES: Disarmament advocacy networks and allied non-nuclear states are structurally excluded from nuclear posture reviews and targeting guidance decisions. They would argue that thinkable winnability lowers the nuclear threshold and undermines the normative taboo, but the planning cells are closed to them by classification and institutional design.
% DISAPPEARANCE_RATIONALE: If the countervailing winnability doctrine vanished overnight, nuclear force structures would shift toward pure deterrence postures (smaller arsenals, de-alerted forces, no counterforce targeting), arms control negotiations would lose their primary structural obstacle, and crisis stability mechanisms would become the central organizing principle of nuclear policy. The military-industrial complex would lose its primary doctrinal justification for current force levels.
% FOUNDING_PROBLEM: Early Cold War vulnerability to Soviet conventional superiority in Europe created a perceived need for nuclear options that could defeat, not just punish, an adversary — establishing counterforce targeting as the solution to the 'extended deterrence credibility' problem.
% FOUNDING_PROBLEM_CORROBORATION: The strategic planning establishment attests the problem remains live, citing great-power competition and conventional asymmetry. Arms control regimes and independent scholars (e.g., Union of Concerned Scientists, Nuclear Threat Initiative analyses) attest the founding problem is substantially resolved by conventional precision-strike capabilities and that the doctrine persists as mission maintenance. Declassified posture reviews and budget testimonies from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__countervailing_thinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__countervailing_thinkable, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__countervailing_thinkable, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__countervailing_thinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__countervailing_thinkable, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__countervailing_thinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__countervailing_thinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the doctrinal requirement to maintain counterforce capabilities well beyond what pure deterrence demands — driving arsenal size, accuracy, and readiness levels that serve warfighting planning. Suppression (0.55) captures the active marginalization of alternative frameworks (no-first-use, minimum deterrence, disarmament) in posture reviews and planning cells. Theater ratio (0.38) measures the growing gap between exercises/plans that assume controllable escalation and the empirical reality that any nuclear use likely triggers uncontrolled escalation. Accessibility collapse (0.48) and resistance (0.45) reflect that alternatives exist and are advocated but face high institutional barriers. Measurements use a shared time grid aligned to historical inflection points (1945, 1960 SIOP, 1975 SALT/peak arsenals, 1990 Cold War end, 2005 post-9/11 posture, 2025 current modernization).
 *
 * PERSPECTIVAL GAP:
 *   From the strategic planner's seat, the constraint is genuine coordination — it provides the shared vocabulary and escalation ladders that make crisis management possible. From the arms control regime's seat, the same structure operates as extraction — every counterforce capability deployed is a verification complication and a stability decrement. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Military-industrial complex and strategic planning establishment are structural beneficiaries (d ≈ 0.15-0.2): they collect budget, mission, and professional relevance. Arms control regimes and strategic stability institutions are structural victims (d ≈ 0.75-0.8): they bear degraded verification, crisis instability, and negotiating disadvantage. Nuclear-armed state leadership sits near symmetric (d ≈ 0.4): they gain crisis bargaining credibility but bear escalation risk. Allied non-nuclear states are payers with constrained exit (d ≈ 0.6). Disarmament advocates are excluded (d not computed). Analytical observers are analytical (d = 0.5 by definition). The engine derives these from beneficiary/victim declarations + exit options + power levels.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Soviet conventional superiority in Europe) is historically resolved, yet the doctrine persists and has expanded to new theaters. This is classic mandatrophy: the coordination function (extended deterrence credibility) has been largely supplanted by conventional precision strike, but the extraction function (force structure perpetuation) has captured the institutional machinery. The constraint is not a snare because the coordination function remains real (crisis signaling still uses the vocabulary); it is not a pure rope because the extraction is asymmetric and actively enforced. Tangled rope correctly captures the hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the countervailing_thinkable reading a distinct structural constraint or merely a rhetorical variant of the deterrence_unthinkable reading?',
    'Compare operational outputs: if counterforce targeting guidance, exercise scenarios, and procurement requirements differ materially from what pure deterrence would produce, the reading instantiates a distinct constraint with its own ε.',
    'If distinct, this reading has independent extractive force on arms control regimes; if variant, the extraction is attributable to the kernel as a whole and should be modeled once.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading is a separate constraint or a rhetorical framing of the same kernel.').

omega_variable(
    counterforce_credibility_empirical,
    'Does counterforce targeting actually enhance deterrence credibility, or is this an unverified axiom that serves force structure perpetuation?',
    'Systematic analysis of crisis outcomes where counterforce capabilities were signaled vs. not signaled; war gaming with adversarial red teams not socialized into the doctrine.',
    'If credibility enhancement is unverified, the coordination function is largely theatrical and the constraint shifts toward snare; if verified, the tangled_rope classification holds with genuine coordination value.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterforce_credibility_empirical, empirical, 'Empirical status of the core coordination claim.').

omega_variable(
    arms_control_extraction_pathway,
    'Through what specific mechanisms does counterforce planning undermine arms control — is it force structure incompatibility, verification complexity, or political signaling?',
    'Case study of treaty negotiations (START II, INF, New START) tracing specific counterforce requirements to specific negotiating failures or verification gaps.',
    'Clarifies whether the victim relationship is structural (force structure makes treaties impossible) or political (planning posture signals non-commitment), affecting whether the extraction is inherent to the constraint or contingent on leadership.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arms_control_extraction_pathway, empirical, 'Causal pathway from counterforce doctrine to arms control degradation.').

omega_variable(
    limited_nuclear_war_controllability,
    'Is the operational assumption that limited nuclear exchanges can be controlled and terminated a genuine coordination achievement or a shared delusion that enables extraction?',
    'Red-team/blue-team exercises with adversarial thinking unconstrained by doctrine; historical analysis of crisis decision-making under nuclear shadow (Cuban Missile Crisis, Able Archer, Kargil).',
    'If controllability is illusory, the coordination function is largely fictive and the constraint is extractive at core; if real under bounded conditions, the tangled_rope classification is structurally sound.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(limited_nuclear_war_controllability, conceptual, 'Whether the constraint''s coordination function survives empirical stress-testing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__countervailing_thinkable, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_tr_t1945, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_tr_t1960, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_tr_t1975, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1975, 0.32).
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_tr_t1990, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_tr_t2005, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_tr_t2025, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_be_t1945, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_be_t1960, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1960, 0.52).
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_be_t1975, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_be_t1990, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_be_t2005, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_be_t2025, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_su_t1945, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_su_t1960, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_su_t1975, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_su_t1990, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_su_t2005, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(war_winnability_post_1945__countervailing_thinkable_su_t2025, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__countervailing_thinkable, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__countervailing_thinkable, 0.12).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945__rhetorical_contraction).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, nuclear_force_structure_modernization).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, extended_deterrence_assurance_architecture).

% DUAL FORMULATION NOTE:
% Part of the war_winnability_post_1945 constraint family. This reading (countervailing_thinkable) has ε ≈ 0.62 with substantial extraction on arms control regimes. The deterrence_unthinkable reading has ε ≈ 0.15 (mountain-like, minimal extraction). The rhetorical_contraction reading has ε ≈ 0.45 (discursive extraction without operational counterpart). The three readings share the kernel but instantiate structurally distinct constraints with different beneficiary/victim structures and different operational consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__countervailing_thinkable, institutional, 0.18).
constraint_indexing:directionality_override(war_winnability_post_1945__countervailing_thinkable, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
