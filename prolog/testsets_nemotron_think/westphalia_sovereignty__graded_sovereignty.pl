% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__graded_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__graded_sovereignty, []).

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
 *   constraint_id: westphalia_sovereignty__graded_sovereignty
 *   human_readable: Graded Sovereignty: Intervention Legitimacy Calibrated to State Capacity
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   The graded sovereignty reading reinterprets the Westphalian kernel from a
 *   binary (sovereign/not) to a scalar: territorial authority exists on a
 *   spectrum from full (Western democracies) to nominal (failed states).
 *   Intervention legitimacy is calibrated to capacity deficits — the less
 *   capacity a state demonstrates, the more legitimate external intervention
 *   becomes. This creates a de facto tiered state system where powerful
 *   states and international organizations act as capacity-evaluation
 *   authorities, gaining legitimacy to intervene in weak states. The
 *   constraint is actively enforced through UN Security Council resolutions,
 *   regional organization mandates, and bilateral intervention doctrines.
 *   Weak states bear the costs of lost autonomy and paternalistic oversight;
 *   powerful states gain discretionary intervention rights. The coordination
 *   function is managing state failure and transnational threats; the
 *   extraction function is the concentration of intervention legitimacy in
 *   the hands of capacity evaluators.
 *
 * KEY AGENTS:
 *   - capacity_evaluation_authorities: Primary agenda_setter (institutional/analytical) — defines capacity metrics, authorizes interventions
 *   - powerful_states: Primary beneficiary (institutional) — gains intervention legitimacy, shapes evaluation criteria
 *   - international_organizations: Agenda_setter/beneficiary (institutional) — operationalizes evaluations, gains relevance
 *   - weak_states: Primary payer (organized/powerless) — loses autonomy, subject to conditional sovereignty
 *   - failed_states: Payer (powerless) — nominal sovereignty, maximal intervention legitimacy
 *   - fragile_states: Payer (moderate) — contested sovereignty, conditional intervention risk
 *   - populations_in_weak_states: Excluded (powerless) — affected by interventions but not consulted
 *   - scholars_observers: Observer (analytical) — analyzes the system without stakes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.72).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.78).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.72).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty: Intervention Legitimacy Calibrated to State Capacity").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, 'a61ee321-c899-49db-aa07-d38b171ecf44').
narrative_ontology:cs_kernel_codification('a61ee321-c899-49db-aa07-d38b171ecf44', formalized).
narrative_ontology:cs_authority_grounding('a61ee321-c899-49db-aa07-d38b171ecf44', extraction).
narrative_ontology:cs_interpretation_layer_present('a61ee321-c899-49db-aa07-d38b171ecf44').
narrative_ontology:cs_reading_relation('a61ee321-c899-49db-aa07-d38b171ecf44', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('a61ee321-c899-49db-aa07-d38b171ecf44', westphalia_sovereignty__conditional_responsibility, coexists_with).
narrative_ontology:cs_axiom('a61ee321-c899-49db-aa07-d38b171ecf44', foundational, sovereignty_is_scalar_capacity).
narrative_ontology:cs_axiom_status(sovereignty_is_scalar_capacity, holdable).
narrative_ontology:cs_axiom_grounding('a61ee321-c899-49db-aa07-d38b171ecf44', sovereignty_is_scalar_capacity, conventional).
narrative_ontology:cs_axiom('a61ee321-c899-49db-aa07-d38b171ecf44', secondary, intervention_legitimacy_calibrated_to_capacity).
narrative_ontology:cs_axiom_status(intervention_legitimacy_calibrated_to_capacity, holdable).
narrative_ontology:cs_axiom_grounding('a61ee321-c899-49db-aa07-d38b171ecf44', intervention_legitimacy_calibrated_to_capacity, instrumental).
narrative_ontology:cs_reference_frame('a61ee321-c899-49db-aa07-d38b171ecf44', westphalian_hierarchy).
narrative_ontology:cs_drift_state('a61ee321-c899-49db-aa07-d38b171ecf44', post_cold_war, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a61ee321-c899-49db-aa07-d38b171ecf44', '2026-08-03T12:00:00Z').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, powerful_states).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, international_organizations).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, failed_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, fragile_states).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__graded_sovereignty, graded_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__graded_sovereignty, capacity_based_intervention_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% International organizations (UN, World Bank, IMF), powerful states, and NGOs that define and measure state capacity (CPIA, Fragile States Index, governance indicators). They set the criteria that determine intervention legitimacy, control the evaluation process, and gain institutional relevance and resources from the hierarchy. They can exit by changing metrics or withdrawing from evaluation roles.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Permanent Security Council members and other major powers. They gain discretionary intervention legitimacy in weak states, shape capacity criteria to serve strategic interests, and avoid accountability for selective enforcement. They bear some costs (military, diplomatic, reputational) but these are far outweighed by the strategic autonomy gained. They can exit the constraint by reverting to absolute sovereignty claims, but benefit too much to do so.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, powerful_states, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, powerful_states, agenda_setter).

% UN Secretariat, regional organizations (AU, EU, OAS), development banks. They operationalize capacity evaluations, authorize peacekeeping and state-building missions, and gain budgetary and mandate expansion from the hierarchy. Their exit is constrained by institutional inertia and dependence on powerful state funding.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, international_organizations, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__graded_sovereignty, international_organizations, beneficiary).

% States with limited administrative capacity but functioning governments (e.g., many Global South states). They face conditional sovereignty: aid, trade, and security cooperation depend on capacity scores. They lose policy autonomy (conditionality), suffer intervention risk, but cannot exit the system — leaving means losing all international recognition and support.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, weak_states, payer,
    organized, biographical, constrained, national).

% States with collapsed central authority (e.g., Somalia 1990s, Yemen, South Sudan). They have nominal sovereignty only; intervention legitimacy is maximal. They bear the full costs of external administration, trusteeship, or military intervention with zero capacity to resist or shape evaluation criteria. Exit is impossible — they are the object of the constraint, not agents within it.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, failed_states, payer,
    powerless, immediate, trapped, local).

% States on the margins of failure (e.g., Afghanistan post-2001, DRC, Haiti). They face continuous evaluation, conditional aid, and threat of escalated intervention. They have some agency to improve scores but are structurally disadvantaged — the metrics reflect institutional capacities they lack. Exit is theoretically possible (build capacity) but practically blocked by the same structural deficits the metrics measure.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, fragile_states, payer,
    moderate, biographical, constrained, national).

% Citizens of weak, fragile, and failed states. They experience the humanitarian and security consequences of interventions (positive and negative) but have no voice in capacity evaluations or intervention decisions. Their consent is neither sought nor required; they are the putative beneficiaries of the coordination function but structurally excluded from its governance.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, populations_in_weak_states, excluded,
    powerless, biographical, trapped, local).

% International lawyers, political theorists, IR scholars. They analyze the constraint's legitimacy, efficacy, and justice from outside the system. They do not collect rents or bear costs directly, but their discourse shapes the legitimacy of the evaluation apparatus over long time horizons.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, scholars_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__graded_sovereignty, powerful_states).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__graded_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the problem of state failure and transnational threats (terrorism, pandemics, migration, environmental collapse) by providing a calibrated framework for legitimate external intervention when a state cannot fulfill its core functions.
% TRANSFER_FUNCTION: Transfers autonomy and decision-making authority from weak/fragile/failed states to capacity_evaluation_authorities and powerful_states, in the form of intervention rights, conditionality, and conditional sovereignty. The transfer is calibrated: the lower the measured capacity, the greater the transfer.
% ABSENT_VOICES: Populations in weak states (excluded stakeholder) — they would object to paternalistic interventions that undermine self-determination, but are structurally excluded from the evaluation and authorization process. Also absent: non-Western conceptualizations of sovereignty (e.g., African Union's 'non-indifference' vs. 'non-interference') that challenge the scalar metric.
% DISAPPEARANCE_RATIONALE: If graded sovereignty vanished overnight, the international system would revert to a contested binary: either absolute non-intervention (weak states gain full autonomy but lose protection) or ad hoc interventionism (powerful states intervene without any legitimacy framework). The tiered hierarchy, capacity metrics, and conditional aid regimes would collapse, reorganizing North-South relations, UN mandate structures, and development finance.
% FOUNDING_PROBLEM: The post-Cold War proliferation of state failure (Somalia, Rwanda, Balkans) created a legitimacy vacuum: absolute sovereignty prevented intervention in genocide and collapse, but unregulated intervention invited abuse. A calibrated framework was needed to distinguish legitimate from illegitimate intervention.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by UN Secretariat reports (1992 Agenda for Peace, 2000 Brahimi Report) and the Responsibility to Protect (2005) as a live challenge. However, Global South states (Non-Aligned Movement, G77) and critical scholars (Chimni, Anghie) corroborate that the problem has been instrumentalized: the capacity metric serves powerful state interests, not the original humanitarian intent. No neutral arbiter confirms the problem remains as originally framed.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__graded_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__graded_sovereignty, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__graded_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__graded_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint transfers substantial autonomy from weak to strong states under the guise of capacity assessment. Suppression (0.78) is high because the tiered hierarchy is maintained by active enforcement (Security Council resolutions, conditionality, military intervention) and the collapse of the absolute sovereignty alternative for weak states. Theater ratio (0.42) is moderate: capacity assessments and humanitarian justifications are real but increasingly performative as the same powerful states that benefit from the hierarchy also dominate the evaluation machinery. Accessibility collapse (0.65) reflects that weak states cannot practically claim absolute sovereignty; resistance (0.55) is moderate — weak states resist rhetorically and occasionally materially, but structural power asymmetry limits effective resistance. The measurement series (1990-2026) shows rising extractiveness and suppression as the post-Cold War order institutionalized the hierarchy, with theater rising as humanitarian language expands to cover strategic interventions.
 *
 * PERSPECTIVAL GAP:
 *   From the capacity_evaluation_authorities seat, the constraint is a rope: genuine coordination to manage state failure and protect populations. From the weak_states seat, it is a snare: extraction of autonomy with no exit. From the powerful_states seat, it is a tangled rope: they coordinate (gain legitimacy for interventions they want) and extract (avoid accountability for interventions they impose). The engine will compute these divergences from the structural data — the declared beneficiaries, victims, power levels, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: capacity_evaluation_authorities (d ~0.1), powerful_states (d ~0.25), international_organizations (d ~0.15) — they set the rules and collect legitimacy rents. Victims: weak_states (d ~0.85), failed_states (d ~0.95), fragile_states (d ~0.75) — they bear autonomy losses with constrained exit. Populations_in_weak_states are excluded (d ~0.9) — they experience intervention effects but have no voice in capacity evaluations. Scholars_observers are analytical (d=0.5). Overrides adjust powerful_states upward from derived ~0.1 because they also bear intervention costs (blood/treasure), making them less than full beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (managing state failure and transnational threats) is live but contested — the arrangement has drifted from a transitional post-Cold War mechanism to a permanent hierarchical system. The mandate has not been resolved; instead, the capacity-evaluation apparatus has become self-justifying. The constraint persists because the beneficiaries (powerful states, international organizations) control the evaluation metrics and have no incentive to sunset the hierarchy. This is a classic mandatrophy: the coordination function (managing failure) has been captured by the extraction function (legitimizing intervention).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_contest,
    'Is the Westphalian sovereignty kernel a categorical inviolability (absolute_non_intervention), a conditional status (conditional_responsibility), or a scalar capacity (graded_sovereignty)?',
    'Comparative analysis of UN Security Council practice, ICJ jurisprudence, and state practice over 1990-2026 to identify which framing dominates authoritative interpretations.',
    'If absolute_non_intervention is the true kernel, graded_sovereignty is a deviation; if graded_sovereignty is the kernel, absolute_non_intervention is a myth. Determines whether this reading is a foreclosure or a coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_contest, conceptual, 'Irreducible ambiguity about which reading correctly captures the Westphalian kernel.').

omega_variable(
    capacity_metrics_legitimacy,
    'What metrics constitute ''state capacity'' for intervention calibration, and who authoritatively defines them?',
    'Trace the genealogy of capacity indices (World Bank CPIA, Fragile States Index, OECD DAC criteria) and their adoption in UN/regional organization mandates.',
    'If metrics are contested or politicized, the coordination function degrades to extraction; if metrics are stable and accepted, the coordination function is genuine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capacity_metrics_legitimacy, empirical, 'Legitimacy of the measurement apparatus that operationalizes the scalar.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of weak-state autonomy structural (enforced by powerful states/institutions) or internalized (weak states accept hierarchical status as legitimate)?',
    'Post-intervention trajectory analysis: if weak states resist intervention legitimacy claims after capacity improves, suppression is structural; if they endorse the hierarchy, internalized component is significant.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint persists even without active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the graded sovereignty hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wsg_tr_t0, westphalia_sovereignty__graded_sovereignty, theater_ratio, 0, 0.25).
narrative_ontology:measurement(wsg_tr_t6, westphalia_sovereignty__graded_sovereignty, theater_ratio, 6, 0.3).
narrative_ontology:measurement(wsg_tr_t12, westphalia_sovereignty__graded_sovereignty, theater_ratio, 12, 0.35).
narrative_ontology:measurement(wsg_tr_t18, westphalia_sovereignty__graded_sovereignty, theater_ratio, 18, 0.38).
narrative_ontology:measurement(wsg_tr_t24, westphalia_sovereignty__graded_sovereignty, theater_ratio, 24, 0.4).
narrative_ontology:measurement(wsg_tr_t30, westphalia_sovereignty__graded_sovereignty, theater_ratio, 30, 0.41).
narrative_ontology:measurement(wsg_tr_t36, westphalia_sovereignty__graded_sovereignty, theater_ratio, 36, 0.42).

% Extraction over time
narrative_ontology:measurement(wsg_be_t0, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(wsg_be_t6, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(wsg_be_t12, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(wsg_be_t18, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(wsg_be_t24, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 24, 0.71).
narrative_ontology:measurement(wsg_be_t30, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(wsg_be_t36, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 36, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(wsg_su_t0, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(wsg_su_t6, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(wsg_su_t12, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(wsg_su_t18, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(wsg_su_t24, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(wsg_su_t30, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 30, 0.77).
narrative_ontology:measurement(wsg_su_t36, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 36, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__graded_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__graded_sovereignty, 0.12).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty__conditional_responsibility).

% DUAL FORMULATION NOTE:
% The Westphalian kernel decomposes into three constraint stories: absolute_non_intervention (mountain claim, low extraction), conditional_responsibility (rope/tangled_rope, moderate extraction), and graded_sovereignty (tangled_rope, high extraction). The graded_sovereignty reading structurally depends on the absolute_non_intervention reading as its foil — the hierarchy only makes sense against a background of nominal equality. It influences conditional_responsibility by providing a capacity-based metric that R2P advocates can adopt or reject.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__graded_sovereignty, institutional, 0.25).
constraint_indexing:directionality_override(westphalia_sovereignty__graded_sovereignty, powerless, 0.9).
constraint_indexing:directionality_override(westphalia_sovereignty__graded_sovereignty, organized, 0.75).
constraint_indexing:directionality_override(westphalia_sovereignty__graded_sovereignty, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
