% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__intermediate_channels
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__intermediate_channels, []).

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
 *   constraint_id: commerce_clause_scope__intermediate_channels
 *   human_readable: Commerce Clause Intermediate Channels Reading
 *   domain: constitutional/law/federalism
 *
 * SUMMARY:
 *   This constraint story models the intermediate_channels reading of the
 *   commerce_clause_scope kernel. Under this reading, federal power reaches
 *   (1) channels of interstate commerce, (2) instrumentalities and persons or
 *   things in interstate commerce, and (3) activities substantially affecting
 *   interstate commerce, but only if the activity is economic and not reached
 *   via attenuated causal chains. The reading attempts to balance national
 *   economic coordination against state sovereignty through categorical
 *   limiting principles. It is claimed as a coordination mechanism
 *   (federalism balance) but operates with asymmetric extraction of state
 *   regulatory autonomy in the economic sphere, while leaving the economic
 *   versus non-economic boundary unstable and manipulable.
 *
 * KEY AGENTS:
 *   - federal_government: Primary beneficiary and agenda-setter (institutional/constrained) â captures expanded commerce power and administers the doctrine through legislation and litigation
 *   - state_governments: Dual-positioned agent (organized/constrained) â retains non-economic autonomy but loses economic regulatory capacity under the substantial effects test
 *   - local_non_economic_actors: Primary target (powerless/trapped) â theoretically protected by limiting principles but vulnerable to judicial recharacterization of their conduct as economic
 *   - constitutional_scholars: Analytical observer (analytical/analytical) â tracks doctrinal instability and the manipulability of the economic versus non-economic distinction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, 0.58).
domain_priors:suppression_score(commerce_clause_scope__intermediate_channels, 0.45).
domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, extractiveness, 0.58).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__intermediate_channels, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__intermediate_channels, "Commerce Clause Intermediate Channels Reading").
narrative_ontology:topic_domain(commerce_clause_scope__intermediate_channels, "constitutional/law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__intermediate_channels).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__intermediate_channels, 'e9b0232e-d923-4d2d-87a0-aee5b528678b').
narrative_ontology:cs_kernel_codification('e9b0232e-d923-4d2d-87a0-aee5b528678b', formalized).
narrative_ontology:cs_authority_grounding('e9b0232e-d923-4d2d-87a0-aee5b528678b', lineage).
narrative_ontology:cs_interpretation_layer_present('e9b0232e-d923-4d2d-87a0-aee5b528678b').
narrative_ontology:cs_reading_relation('e9b0232e-d923-4d2d-87a0-aee5b528678b', commerce_clause_scope__narrow_originalist, coexists_with).
narrative_ontology:cs_reading_relation('e9b0232e-d923-4d2d-87a0-aee5b528678b', commerce_clause_scope__broad_effects_test, coexists_with).
narrative_ontology:cs_axiom('e9b0232e-d923-4d2d-87a0-aee5b528678b', foundational, substantial_effects_limited_to_economic_activity).
narrative_ontology:cs_axiom_status(substantial_effects_limited_to_economic_activity, holdable).
narrative_ontology:cs_axiom_grounding('e9b0232e-d923-4d2d-87a0-aee5b528678b', substantial_effects_limited_to_economic_activity, conventional).
narrative_ontology:cs_axiom('e9b0232e-d923-4d2d-87a0-aee5b528678b', foundational, non_economic_activity_requires_jurisdictional_nexus).
narrative_ontology:cs_axiom_status(non_economic_activity_requires_jurisdictional_nexus, holdable).
narrative_ontology:cs_axiom_grounding('e9b0232e-d923-4d2d-87a0-aee5b528678b', non_economic_activity_requires_jurisdictional_nexus, conventional).
narrative_ontology:cs_reference_frame('e9b0232e-d923-4d2d-87a0-aee5b528678b', constitutional_federalism_equilibrium).
narrative_ontology:cs_drift_state('e9b0232e-d923-4d2d-87a0-aee5b528678b', contemporary_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e9b0232e-d923-4d2d-87a0-aee5b528678b', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__intermediate_channels, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, federal_government).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, local_non_economic_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises commerce power through Congress and federal agencies to regulate interstate economic channels, instrumentalities, and activities with substantial effects on interstate commerce. Benefits from broad authority to address national economic problems but remains bound by judicial precedent requiring categorical limits on non-economic regulation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, federal_government, agenda_setter,
    institutional, generational, constrained, national).

% Retain significant regulatory authority over non-economic local matters such as family law, criminal law, and education under the limiting principles of this reading. Simultaneously lose exclusive authority to regulate local economic activities that substantially affect interstate commerce, and must defend their laws against federal preemption challenges.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_governments, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, state_governments, payer).

% Individuals and entities engaged in local, non-commercial activity such as personal cultivation, possession of firearms in school zones, or gender-motivated violence that the reading attempts to place beyond federal reach. The instability of the economic versus non-economic distinction leaves them vulnerable to federal regulation when courts recharacterize their conduct or accept attenuated causal chains.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, local_non_economic_actors, payer,
    powerless, biographical, trapped, local).

% Analyze and debate the coherence of the three-category test and the manipulability of limiting principles. They document that the economic versus non-economic line shifts with judicial composition and political context, producing doctrinal instability for litigants and legislatures.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__intermediate_channels, federal_government).
narrative_ontology:fixing_cost_class(commerce_clause_scope__intermediate_channels, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides regulatory authority between federal and state governments in a federal system, providing a uniform national framework for interstate economic channels while reserving non-economic local matters to state police powers.
% TRANSFER_FUNCTION: Transfers regulatory authority over interstate economic channels, instrumentalities, and local activities with substantial aggregate economic effects from state governments to the federal government; transfers protective jurisdiction over non-economic local conduct from the federal government back to the states.
% ABSENT_VOICES: State actors seeking broad economic autonomy beyond the channel and instrumentality nexus; originalist scholars who reject the substantial effects test entirely; civil libertarians who view any federal regulation of local conduct as overreach. They are present in academic and political discourse but structurally excluded from the doctrinal framework the judiciary enforces.
% DISAPPEARANCE_RATIONALE: If the intermediate_channels reading vanished overnight, the federal-state regulatory boundary would destabilize. Congress would lack clear authority to legislate against national economic problems, states would face uncertainty about the validity of their own laws, and the judiciary would lose the doctrinal framework for adjudicating federalism conflicts, causing rearrangement of statutes and enforcement practices.
% FOUNDING_PROBLEM: The Articles of Confederation failed to provide a national government with adequate power to regulate interstate trade, leading to state-level protectionism, trade barriers, and economic fragmentation; the Commerce Clause was adopted to empower federal regulation of interstate commerce while preserving state sovereignty over local matters.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of the Philadelphia Convention and the Federalist Papers, attested by constitutional historians outside the immediate beneficiary set, confirm the trade-barrier problem under the Articles. However, the claim that the current substantial-effects-plus-limiting-principles framework remains necessary to solve that problem is contested by originalist scholars and state attorneys general, who argue the modern doctrine far exceeds the founding design.
narrative_ontology:disappearance_verdict(commerce_clause_scope__intermediate_channels, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__intermediate_channels, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__intermediate_channels, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_scope__intermediate_channels, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__intermediate_channels, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__intermediate_channels_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__intermediate_channels_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is medium (0.58) because the doctrine transfers substantial economic regulatory authority to the federal government while preserving non-economic state domains, creating a mixed but net-extractive transfer of sovereignty. Suppression is moderate (0.45) because states cannot nullify federal law and must litigate to resist, but political and judicial channels remain open. Theater ratio is moderate (0.40) because the limiting principles (economic versus non-economic line, jurisdictional element requirement) are frequently manipulated to reach desired outcomes, generating performative boundary policing that obscures functional expansion. Accessibility collapse is moderately high (0.70) because precedent and the supremacy clause legally foreclose alternatives such as nullification or interposition. Resistance is moderate (0.50) due to persistent state litigation, originalist criticism, and political federalism movements.
 *
 * PERSPECTIVAL GAP:
 *   The federal government seat should compute as coordination-with-benefit because it both solves a national collective-action problem and captures authority. The state government seat should compute as mixed but net-extraction because the economic category swallows most regulatory domains, leaving only residual non-economic protection. The local non-economic actor seat should compute as near-target because the limiting principles fail to reliably protect them. The divergence is driven by scope asymmetry: the federal government operates at national scope with institutional power, while local actors are powerless at local scope.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is the primary beneficiary and capturer of extracted authority (low directionality). State governments sit in a dual position: they benefit from the protection of non-economic domains but pay through lost economic autonomy; structurally, the economic sphere is expansive and growing, so their net position leans toward payer (moderate-high directionality). Local non-economic actors are targets (high directionality) because the shield protecting them is unstable. Constitutional scholars occupy the analytical seat with no direct extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â state trade barriers under the Articles â is dead as a live threat, but the doctrine persists and is actively enforced. The mismatch between founding_problem_status=contested/dead and disappearance_verdict=world_rearranges flags potential mandatrophy. However, the doctrine still coordinates a genuine ongoing federalism problem (multi-state externalities, race-to-the-bottom dynamics in national markets), so it is not a pure piton. The limiting principles have a scaffold-like flavor, but without a sunset clause the arrangement has institutionalized rather than transitioned. The reading resists classification as snare because the coordination function is real and non-theatrical, even if asymmetric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_non_economic_boundary_stability,
    'Is the economic versus non-economic distinction a stable, administrable legal line, or is it inherently manipulable by courts to reach desired outcomes?',
    'Systematic coding of Supreme Court and circuit decisions over time to measure inter-coder reliability in classifying conduct as economic versus non-economic; natural experiment from changes in judicial composition.',
    'If the line is manipulable, the limiting principles fail and this reading functionally converges toward the broad_effects_test sibling, increasing extractiveness and collapsing the federal-state boundary. If stable, the reading maintains its structural integrity as a genuine coordination device.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_non_economic_boundary_stability, empirical, 'Stability of the economic versus non-economic distinction under judicial interpretation').

omega_variable(
    federalism_coordination_or_extraction,
    'Does this doctrine primarily coordinate a necessary division of power in a complex national economy, or does it extract sovereignty from states to aggrandize federal authority under the guise of coordination?',
    'Comparative analysis of federal systems with different commerce-power allocations; measurement of state regulatory capacity before and after key Commerce Clause decisions; assessment of whether national economic externalities justify federal preemption.',
    'If primarily extraction, classification shifts toward snare. If primarily coordination, classification shifts toward rope. The current tangled_rope classification depends on this balance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federalism_coordination_or_extraction, conceptual, 'Coordination versus extraction nature of the federalism division').

omega_variable(
    sibling_reading_structural_pressure,
    'How would the classification change if the broad_effects_test or narrow_originalist sibling reading were adopted instead of this intermediate_channels reading?',
    'Comparison of the structural data (beneficiary/victim sets, extractiveness, suppression) across the three constraint stories in the commerce_clause_scope family.',
    'Adopting broad_effects_test would likely increase extractiveness and victim scope, shifting toward snare. Adopting narrow_originalist would reduce extractiveness but potentially collapse national coordination capacity, shifting toward rope or mountain depending on empirical premises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_pressure, conceptual, 'Structural pressure from sibling readings in the Commerce Clause kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__intermediate_channels, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__intermediate_channels, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comm_tr_t16, commerce_clause_scope__intermediate_channels, theater_ratio, 16, 0.25).
narrative_ontology:measurement(comm_tr_t32, commerce_clause_scope__intermediate_channels, theater_ratio, 32, 0.4).
narrative_ontology:measurement(comm_tr_t48, commerce_clause_scope__intermediate_channels, theater_ratio, 48, 0.42).
narrative_ontology:measurement(comm_tr_t64, commerce_clause_scope__intermediate_channels, theater_ratio, 64, 0.4).
narrative_ontology:measurement(comm_tr_t80, commerce_clause_scope__intermediate_channels, theater_ratio, 80, 0.4).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__intermediate_channels, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(comm_be_t16, commerce_clause_scope__intermediate_channels, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(comm_be_t32, commerce_clause_scope__intermediate_channels, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(comm_be_t48, commerce_clause_scope__intermediate_channels, base_extractiveness, 48, 0.6).
narrative_ontology:measurement(comm_be_t64, commerce_clause_scope__intermediate_channels, base_extractiveness, 64, 0.58).
narrative_ontology:measurement(comm_be_t80, commerce_clause_scope__intermediate_channels, base_extractiveness, 80, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_scope__intermediate_channels, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(comm_su_t16, commerce_clause_scope__intermediate_channels, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(comm_su_t32, commerce_clause_scope__intermediate_channels, suppression_requirement, 32, 0.5).
narrative_ontology:measurement(comm_su_t48, commerce_clause_scope__intermediate_channels, suppression_requirement, 48, 0.48).
narrative_ontology:measurement(comm_su_t64, commerce_clause_scope__intermediate_channels, suppression_requirement, 64, 0.45).
narrative_ontology:measurement(comm_su_t80, commerce_clause_scope__intermediate_channels, suppression_requirement, 80, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__intermediate_channels, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__intermediate_channels, 0.1).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__broad_effects_test).

% DUAL FORMULATION NOTE:
% This constraint is one member of the commerce_clause_scope constraint family, decomposed per the epsilon-invariance principle because the colloquial label 'Commerce Clause scope' conflates structurally distinct interpretive claims with different epsilon values, beneficiary and victim structures, and constraint types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
