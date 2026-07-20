% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__graded_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: westphalia_sovereignty__graded_sovereignty
 *   human_readable: Graded Sovereignty: State Capacity as Scalar Foundation for Intervention Legitimacy
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   The graded sovereignty reading treats Westphalian sovereignty not as a
 *   categorical equal entitlement but as a scalar capacity gradient. Full
 *   sovereignty is reserved for Western democracies and similarly capable
 *   states; weak and failed states occupy lower tiers where their nominal
 *   territorial authority is overridden by international capacity-evaluation
 *   authorities. This creates a hierarchical system where intervention
 *   legitimacy is calibrated to perceived governance deficits, producing
 *   structural asymmetry between evaluators and evaluated.
 *
 * KEY AGENTS:
 *   - Capacity-evaluation authorities (UN, World Bank, OECD governance indices): Agenda-setters who define state capacity metrics and intervention thresholds.
 *   - Full-sovereignty states (Western democracies and allies): Primary beneficiaries whose territorial authority remains categorically shielded.
 *   - Weak states: Payers subject to intrusive monitoring, conditional aid, and paternalistic oversight.
 *   - Failed-state populations: Payers who bear the direct human and economic costs of state-building interventions.
 *   - UN Security Council: Agenda-setter that formalizes capacity assessments into intervention mandates.
 *   - Global South advocacy coalition: Excluded voices contesting the colonial grammar of capacity metrics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.78).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.75).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.78).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty: State Capacity as Scalar Foundation for Intervention Legitimacy").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, '8233a84c-c790-496a-a24f-93434b533dac').
narrative_ontology:cs_kernel_codification('8233a84c-c790-496a-a24f-93434b533dac', formalized).
narrative_ontology:cs_authority_grounding('8233a84c-c790-496a-a24f-93434b533dac', extraction).
narrative_ontology:cs_interpretation_layer_present('8233a84c-c790-496a-a24f-93434b533dac').
narrative_ontology:cs_reading_relation('8233a84c-c790-496a-a24f-93434b533dac', westphalia_sovereignty__absolute_non_intervention, influences).
narrative_ontology:cs_reading_relation('8233a84c-c790-496a-a24f-93434b533dac', westphalia_sovereignty__conditional_responsibility, coexists_with).
narrative_ontology:cs_axiom('8233a84c-c790-496a-a24f-93434b533dac', foundational, sovereignty_scalar_not_categorical).
narrative_ontology:cs_axiom_status(sovereignty_scalar_not_categorical, holdable).
narrative_ontology:cs_axiom_grounding('8233a84c-c790-496a-a24f-93434b533dac', sovereignty_scalar_not_categorical, empirically_contingent).
narrative_ontology:cs_axiom('8233a84c-c790-496a-a24f-93434b533dac', foundational, capacity_metrics_ground_intervention).
narrative_ontology:cs_axiom_status(capacity_metrics_ground_intervention, holdable).
narrative_ontology:cs_axiom_grounding('8233a84c-c790-496a-a24f-93434b533dac', capacity_metrics_ground_intervention, instrumental).
narrative_ontology:cs_reference_frame('8233a84c-c790-496a-a24f-93434b533dac', scalar_capacity_framework).
narrative_ontology:cs_drift_state('8233a84c-c790-496a-a24f-93434b533dac', post_1990s_statebuilding_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8233a84c-c790-496a-a24f-93434b533dac', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, full_sovereignty_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, failed_state_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets state capacity metrics, governance benchmarks, and fragility indices; determines when sovereignty is classified as full, limited, or nominal; legitimizes intervention in capacity-deficit jurisdictions and captures authority, budgets, and institutional centrality from the evaluation role.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, capacity_evaluation_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Enjoy the highest tier of sovereignty recognition; their territorial authority is treated as presumptively legitimate and inviolable regardless of internal conduct. They participate in setting capacity standards and benefit from the hierarchical distinction that shields them from intrusive oversight.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, full_sovereignty_states, beneficiary,
    powerful, generational, mobile, national).

% Classified as limited or nominal sovereignty; subject to intrusive monitoring, conditional aid, governance benchmarks, and potential intervention. Their policy autonomy is systematically overridden by capacity evaluators, and they cannot opt out of the evaluation architecture without risking isolation or coercive measures.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, weak_states, payer,
    powerless, biographical, trapped, national).

% Live under nominal sovereignty where territorial authority is hollow or externally administered. They bear the direct human and economic costs of peacekeeping, state-building, and humanitarian intervention framed as capacity restoration, without meaningful say in the institutional design imposed on them.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, failed_state_populations, payer,
    powerless, immediate, trapped, local).

% Formalizes intervention mandates based on capacity assessments and threat determinations. Converts scalar sovereignty ratings into Chapter VII resolutions, peacekeeping authorizations, and sanctions regimes that operationalize the graded framework.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, un_security_council, agenda_setter,
    institutional, generational, analytical, global).

% Contests that graded sovereignty replicates colonial hierarchies under technocratic cover. Largely excluded from Bretton Woods institutions, OECD forums, and UN reform processes where capacity thresholds and evaluation protocols are codified.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__graded_sovereignty, global_south_advocacy_coalition, excluded,
    moderate, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__graded_sovereignty, diffuse).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__graded_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a scalable framework for legitimate international engagement with states that lack effective governance capacity, ostensibly preventing humanitarian catastrophe, security vacuums, and transnational threats from ungoverned spaces.
% TRANSFER_FUNCTION: Moves policy autonomy, territorial control, and institutional design authority from weak states and failed-state populations to capacity-evaluation authorities and full-sovereignty states through conditional aid, peacebuilding mandates, and calibrated intervention.
% ABSENT_VOICES: Weak states and failed-state populations are nominally consulted in capacity assessments but do not author the metrics; global south advocacy coalitions contest the hierarchical framing but are excluded from Bretton Woods institutions and UN reform forums where capacity thresholds are codified.
% DISAPPEARANCE_RATIONALE: If graded sovereignty vanished, interventions in Somalia, the DRC, and Haiti would lose their primary doctrinal frame; full-sovereignty states would lose the categorical distinction that shields them from intrusive oversight; the international order would revert to contested absolute or conditional sovereignty frames, and the state-building industry would collapse.
% FOUNDING_PROBLEM: Post-Cold War state collapse and humanitarian emergencies where nominal sovereignty masked brutal internal conditions and security vacuums (Somalia 1991, Rwanda 1994, Balkans).
% FOUNDING_PROBLEM_CORROBORATION: Humanitarian intervention scholars and the ICISS report attest the problem from outside pure state-interest framing; critical international legal scholars and post-colonial theorists contest that the founding problem was ever systematically about protection rather than order maintenance and neo-imperial hierarchy.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__graded_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__graded_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__graded_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__graded_sovereignty, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.78) because the framework systematically transfers policy autonomy from weak states to evaluators. Suppression is high (0.75) because weak states cannot opt out of the evaluation architecture without risking isolation or intervention. Theater is moderate (0.40): state-building metrics, best-practice workshops, and capacity assessments generate substantial performative activity that obscures persistent hierarchical control. Accessibility collapse is substantial (0.65): once a state is classified as fragile or failed, alternatives to international stewardship collapse diplomatically and institutionally. Resistance is moderate (0.55): targeted states and some powerful non-Western states actively contest the framework, preventing it from reading as pure extraction.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (capacity-evaluation authorities) experiences the constraint as necessary coordination for global stability; the payer seats (weak states and failed-state populations) experience it as paternalistic extraction dressed in technocratic language. Full-sovereignty states experience it as benign background structure because their tier exempts them from scrutiny. The engine computes this divergence from structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Capacity-evaluation authorities and full-sovereignty states are structural beneficiaries: they author the metrics, enjoy presumptive sovereignty, and control the intervention agenda (low d, subsidized by the constraint). Weak states and failed-state populations are structural targets: they pay in lost autonomy and direct intervention costs, with exit options ranging from constrained to trapped (high d, amplified Ï).
 *
 * MANDATROPHY ANALYSIS:
 *   The framework prevents mislabeling by requiring both beneficiaries and victims for tangled-rope classification. The genuine coordination functionâpreventing catastrophic state collapseâprevents snare classification, while the identifiable victim tier and active enforcement prevent rope classification. The temporal measurements show extraction accumulating as the capacity-evaluation apparatus matured, confirming the hybrid structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_metric_neutrality,
    'Are state capacity metrics genuinely neutral governance indicators, or do they encode Western institutional preferences and neo-colonial standards?',
    'Comparative historical analysis of metric adoption: if non-Western states that achieve comparable governance outcomes on indigenous institutional models are systematically rated lower, the metrics are not neutral.',
    'If the metrics encode Western preferences, the coordination function is cover for hierarchical extraction and the constraint drifts toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_metric_neutrality, empirical, 'Whether capacity metrics are culturally neutral or Western-biased.').

omega_variable(
    intervention_motivation,
    'Is capacity-based intervention primarily motivated by protection of populations and global stability, or by geopolitical control and resource access?',
    'Pattern analysis of intervention selectivity: compare capacity-deficit states that were intervened against versus those that were ignored, controlling for strategic interest.',
    'If selectivity tracks strategic interest rather than capacity deficit, the coordination story is cover and the constraint is extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_motivation, empirical, 'Whether intervention tracks capacity deficits or power politics.').

omega_variable(
    kernel_reading_underdetermination,
    'Does the graded sovereignty reading represent the only coherent interpretation of post-1990s practice, or is conditional responsibility a better fit for the same historical record?',
    'Examine whether interventions in the record are justified by capacity deficits alone, atrocity thresholds alone, or hybrid claims; track how sibling readings are invoked in Security Council deliberations.',
    'If conditional responsibility accounts for the same practice without the hierarchical tiering, graded sovereignty may be an unnecessarily extractive framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the same practice is better explained by a less extractive sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__graded_sovereignty, theater_ratio, 0, 0.2).
narrative_ontology:measurement(west_tr_t5, westphalia_sovereignty__graded_sovereignty, theater_ratio, 5, 0.25).
narrative_ontology:measurement(west_tr_t10, westphalia_sovereignty__graded_sovereignty, theater_ratio, 10, 0.3).
narrative_ontology:measurement(west_tr_t15, westphalia_sovereignty__graded_sovereignty, theater_ratio, 15, 0.35).
narrative_ontology:measurement(west_tr_t20, westphalia_sovereignty__graded_sovereignty, theater_ratio, 20, 0.38).
narrative_ontology:measurement(west_tr_t25, westphalia_sovereignty__graded_sovereignty, theater_ratio, 25, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(west_be_t5, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(west_be_t10, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(west_be_t15, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(west_be_t20, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 20, 0.73).
narrative_ontology:measurement(west_be_t25, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(west_su_t5, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(west_su_t10, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(west_su_t15, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(west_su_t20, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(west_su_t25, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 25, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__graded_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, conditional_responsibility).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the westphalia_sovereignty kernel, decomposed per the Îµ-invariance principle. Sibling readings instantiate structurally distinct claims with different beneficiary/victim structures and Îµ values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
