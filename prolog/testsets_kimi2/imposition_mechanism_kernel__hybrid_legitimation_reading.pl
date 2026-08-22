% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-12-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__hybrid_legitimation_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__hybrid_legitimation_reading
 *   human_readable: Hybrid Legitimation Mechanism (Symbolic Authority + Institutional Incentives)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid_legitimation_reading of the
 *   imposition_mechanism_kernel. It models a historical state-formation
 *   pattern in which new norms achieve legitimacy neither through pure
 *   bottom-up adoption (endogenous_climb) nor through raw coercion
 *   (exogenous_override), but through a synthetic mechanism: the symbolic
 *   transfer of imperial or transcendent authority (the emperor's example)
 *   combined with differentiated institutional incentives that bind local
 *   elites to the center before mass compliance is achieved. The arrangement
 *   coordinates a stratified social order while asymmetrically extracting
 *   labor and deference from the peasant majority and displacing subaltern
 *   practitioners.
 *
 * KEY AGENTS:
 *   - court_bureaucracy: Primary agenda-setter (institutional/constrained) â designs incentives and orchestrates symbolic authority transfer
 *   - local_elite_brokers: Primary beneficiary (powerful/mobile) â early adopters who receive privileges for brokerage
 *   - peasant_majority: Primary target (powerless/trapped) â bears labor and compliance costs under stratified adoption
 *   - subaltern_practitioners: Secondary target and excluded voice (powerless/identity_locked) â pays cultural displacement costs and is excluded from negotiation
 *   - comparative_historian: Analytical observer (analytical/analytical) â compares imposition mechanisms across cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.58).
domain_priors:suppression_score(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.52).
domain_priors:theater_ratio(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__hybrid_legitimation_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__hybrid_legitimation_reading, "Hybrid Legitimation Mechanism (Symbolic Authority + Institutional Incentives)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(imposition_mechanism_kernel__hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__hybrid_legitimation_reading, '0c37d5f9-c452-41e6-833d-2e94f1f7f07d').
narrative_ontology:cs_kernel_codification('0c37d5f9-c452-41e6-833d-2e94f1f7f07d', fixed_text).
narrative_ontology:cs_authority_grounding('0c37d5f9-c452-41e6-833d-2e94f1f7f07d', lineage).
narrative_ontology:cs_interpretation_layer_present('0c37d5f9-c452-41e6-833d-2e94f1f7f07d').
narrative_ontology:cs_reading_relation('0c37d5f9-c452-41e6-833d-2e94f1f7f07d', imposition_mechanism_kernel__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c37d5f9-c452-41e6-833d-2e94f1f7f07d', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('0c37d5f9-c452-41e6-833d-2e94f1f7f07d', foundational, symbolic_authority_transfers_legitimacy).
narrative_ontology:cs_axiom_status(symbolic_authority_transfers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0c37d5f9-c452-41e6-833d-2e94f1f7f07d', symbolic_authority_transfers_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('0c37d5f9-c452-41e6-833d-2e94f1f7f07d', foundational, institutional_incentives_require_elite_brokerage).
narrative_ontology:cs_axiom_status(institutional_incentives_require_elite_brokerage, holdable).
narrative_ontology:cs_axiom_grounding('0c37d5f9-c452-41e6-833d-2e94f1f7f07d', institutional_incentives_require_elite_brokerage, empirically_contingent).
narrative_ontology:cs_reference_frame('0c37d5f9-c452-41e6-833d-2e94f1f7f07d', symbolic_authority_transfer_state).
narrative_ontology:cs_drift_state('0c37d5f9-c452-41e6-833d-2e94f1f7f07d', contemporary_historical_synthesis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0c37d5f9-c452-41e6-833d-2e94f1f7f07d', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, court_bureaucracy).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__hybrid_legitimation_reading, local_elite_brokers).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, peasant_majority).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__hybrid_legitimation_reading, subaltern_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and enforces the institutional incentives that bind local elites to imperial norms, and orchestrates the symbolic transfer of the emperor's authority to new legal and cultural standards. Its members' careers and legitimacy depend on the imperial system's continuity; they cannot abandon the legitimating framework without undermining their own authority.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, court_bureaucracy, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, court_bureaucracy, beneficiary).

% Adopt court-sponsored norms early and serve as the local interface between imperial administration and rural populations. Receive titles, tax privileges, and land-tenure security in exchange for compliance and local enforcement. They retain some negotiating leverage with the center but depend on imperial recognition for their elevated status.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, local_elite_brokers, beneficiary,
    powerful, biographical, mobile, regional).

% Provide the labor, tax revenue, and deference that make the normative order materially sustainable. Comply with new norms after elite adoption, often under combined pressure of incentive and symbolic example. Their movement is bound by land tenure and subsistence agriculture; refusing compliance risks deprivation.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, peasant_majority, payer,
    powerless, immediate, trapped, local).

% Maintain pre-imperial customary practices that are delegitimized by the emperor's symbolic example. Bear the costs of cultural displacement and loss of juridical standing. Their social identity is fused with the displaced practices, making adoption of imperial norms a form of self-erasure; they are not consulted in brokerage negotiations.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, subaltern_practitioners, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__hybrid_legitimation_reading, subaltern_practitioners, excluded).

% Studies the diffusion pattern across empires and periods, comparing elite-first stratified adoption with alternative imposition mechanisms. Does not participate in the constraint's operation or bear its costs.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__hybrid_legitimation_reading, comparative_historian, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__hybrid_legitimation_reading, diffuse).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__hybrid_legitimation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates normative order across a stratified society by aligning local elite behavior with imperial standards through a combination of symbolic prestige and material incentive, reducing the transaction costs of state expansion.
% TRANSFER_FUNCTION: Moves labor, tax compliance, and cultural deference from the peasant majority and subaltern practitioners to the state apparatus and local elite brokers, in exchange for institutional privileges and protected status.
% ABSENT_VOICES: Subaltern practitioners and indigenous ritual specialists are excluded from the brokerage process between court and local elites; their objections to norm displacement are rendered invisible by the emperor's symbolic authority. Peasant elders who might advocate for syncretic adaptation lack formal standing in the incentive structure.
% DISAPPEARANCE_RATIONALE: If the hybrid legitimation mechanism vanished, the stratified adoption sequence would collapse: local elites would lose the institutional incentives that bind them to the center, peasant compliance would revert to local or endogenous patterns, and the state's symbolic authority would require reconquest through either pure coercion or genuine bottom-up conversion.
% FOUNDING_PROBLEM: How to extend imperial or state authority into regions with distinct local norms without incurring the full cost of permanent military occupation or waiting for generational cultural change.
% FOUNDING_PROBLEM_CORROBORATION: Comparative historians and historical sociologists outside the benefiting state apparatus (e.g., Weberian and post-Weberian state-formation theorists) attest that the problem of low-capacity state expansion is genuine; however, they dispute whether hybrid legitimation solved it or merely deferred its costs.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__hybrid_legitimation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__hybrid_legitimation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__hybrid_legitimation_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_mechanism_kernel__hybrid_legitimation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_mechanism_kernel__hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 because stratified adoption concentrates costs on non-elites while preserving a genuine coordination function for state expansion. Suppression is 0.52: the mechanism requires active brokerage and institutional maintenance, not merely symbolic charisma, but falls short of monopoly-on-violence coercion. Theater ratio is 0.42 because imperial symbolic authority has a strong performative dimension that persists and intensifies as original charismatic force routinizes. Accessibility collapse is 0.60: local alternatives are delegitimized by the emperor's example but not physically eradicated. Resistance is 0.48 because subaltern groups and peasants engage in passive resistance and foot-dragging, yet open revolt is dampened by the aura of legitimate authority.
 *
 * PERSPECTIVAL GAP:
 *   The court bureaucracy experiences the constraint as a necessary coordination mechanism preserving imperial unity; local elites experience it as a beneficial exchange of compliance for privilege; the peasant majority experiences it as a compulsory transfer of labor and cultural practice; subaltern practitioners experience it as erasure. The engine computes these divergent seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Court bureaucracy and local elite brokers sit near the beneficiary pole: they collect tax compliance, political deference, and institutional stability. Peasant majority and subaltern practitioners sit near the target pole: they bear the costs of norm adoption and displacement, with subaltern practitioners further toward full target due to identity-locked exit. The comparative historian sits at analytical exit with no directional stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâextending state authority without full coercion or waiting for generational changeâwas genuinely solved by the hybrid mechanism for certain historical regimes. However, the mechanism's persistence beyond the founding period risks mandatrophy: brokerage becomes hereditary, incentives ossify into rents, and symbolic authority decays into theatrical performance. The temporal measurements show slowly rising extractiveness, theater, and suppression over the interval, suggesting the mechanism drifts toward inertial maintenance rather than active coordination. The R5 genealogy flags this: founding_problem_status is contested because modern historiography disputes whether the hybrid model solved the problem or merely deferred its costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    charisma_vs_materiality,
    'Does the symbolic authority transfer (emperor''s example) actually causally legitimate norms, or is it epiphenomenal to the material incentives that drive elite compliance?',
    'Controlled comparison of norm adoption rates in regions with and without charismatic symbolic infrastructure, holding material incentives constant.',
    'If symbolic authority is epiphenomenal, the constraint collapses toward exogenous override (material coercion/incentive); if causal, the hybrid classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charisma_vs_materiality, empirical, 'Whether symbolic authority is causally active or decorative in elite compliance.').

omega_variable(
    enforcement_cost_naturalness,
    'Are the moderate enforcement costs evidence of a genuine coordination function (voluntary elite compliance lowers enforcement needs) or of a temporarily stable extraction equilibrium?',
    'Measurement of enforcement cost trajectories over the norm-diffusion lifecycle; if costs rise after elite incentives decay, the initial stability was extracted.',
    'Rising costs would signal the constraint is a scaffold that failed to transition, or a snare with deferred coercion; stable costs support tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_cost_naturalness, conceptual, 'Whether moderate enforcement reflects coordination or deferred extraction.').

omega_variable(
    kernel_reading_location,
    'This constraint is the hybrid_legitimation_reading of the imposition_mechanism_kernel. How would its structural classification change under sibling readings?',
    'Archival micro-history tracing the sequence of elite versus popular adoption and the presence or absence of symbolic authority infrastructure in specific cases.',
    'If grassroots demand precedes state recognition, the endogenous_climb_reading would assign near-symmetric directionality to the populace; if violence precedes all else, the exogenous_override_reading would concentrate near-target directionality on all non-state actors, flattening elite beneficiary status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Locates this constraint within its kernel family and identifies the empirical hinge between readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__hybrid_legitimation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_legitimation_tr_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(hybrid_legitimation_tr_t8, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(hybrid_legitimation_tr_t16, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(hybrid_legitimation_tr_t24, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(hybrid_legitimation_tr_t32, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 32, 0.48).
narrative_ontology:measurement(hybrid_legitimation_tr_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, theater_ratio, 40, 0.5).

% Extraction over time
narrative_ontology:measurement(hybrid_legitimation_be_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hybrid_legitimation_be_t8, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(hybrid_legitimation_be_t16, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(hybrid_legitimation_be_t24, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(hybrid_legitimation_be_t32, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(hybrid_legitimation_be_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_legitimation_su_t0, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hybrid_legitimation_su_t8, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(hybrid_legitimation_su_t16, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(hybrid_legitimation_su_t24, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(hybrid_legitimation_su_t32, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(hybrid_legitimation_su_t40, imposition_mechanism_kernel__hybrid_legitimation_reading, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__hybrid_legitimation_reading, exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is the hybrid_legitimation_reading of the imposition_mechanism_kernel, sitting between endogenous_climb_reading (bottom-up) and exogenous_override_reading (top-down coercion) as a distinct synthetic mechanism. The kernel decomposes into three structurally distinct claims with different epsilon values and directionality structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
