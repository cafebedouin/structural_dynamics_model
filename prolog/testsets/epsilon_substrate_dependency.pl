% ============================================================================
% CONSTRAINT STORY: epsilon_substrate_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epsilon_substrate_dependency, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: epsilon_substrate_dependency
 *   human_readable: Epsilon Substrate Dependency in Deferential Realism
 *   domain: epistemology/formal_systems/measurement_theory
 *
 * SUMMARY:
 *   The Deferential Realism framework models constraint classification
 *   through a seat/gauge/orientation architecture where base extractiveness
 *   (epsilon) is the foundational primitive: it feeds effective extraction
 *   (chi), which determines classification type. Epsilon is declared
 *   judgment-authored per reading rather than computed from world-anchors,
 *   enabling the framework to model reading-dependent classification for
 *   contested kernels. This design choice creates a structural tension: the
 *   framework's most load-bearing primitive rests on its least-grounded
 *   input, transferring epistemic cost from framework flexibility to
 *   empirical grounding expectation and external audit capacity. The
 *   constraint is claimed as snare (extraction masked as necessary
 *   flexibility) while metrics describe substantial extraction with rising
 *   enforcement requirement over the framework's development interval.
 *
 * KEY AGENTS:
 *   - framework_architects: Agenda-setters (institutional/arbitrage) — design and maintain the epsilon-as-judgment architecture, control declaration discipline
 *   - analytical_flexibility: Beneficiary (non-agent abstract capacity) — gains from epsilon remaining judgment-authored, enabling reading-relative classification
 *   - empirical_grounding_expectation: Payer (non-agent epistemic norm/identity_locked) — bears cost of judgment-bottleneck at foundation
 *   - cross_reading_comparability: Payer (non-agent capacity/constrained) — loses anchor-based falsification across readings
 *   - external_auditors: Payers (institutional/constrained) — face high verification cost for the primitive they most need to check
 *   - story_generators: Payers (moderate/constrained) — must author epsilon without clear world-anchored procedure, producing variance
 *   - framework_consumers: Observers (organized/mobile) — use outputs while accounting for known judgment-authored foundation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epsilon_substrate_dependency, 0.68).
domain_priors:suppression_score(epsilon_substrate_dependency, 0.72).
domain_priors:theater_ratio(epsilon_substrate_dependency, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epsilon_substrate_dependency, extractiveness, 0.68).
narrative_ontology:constraint_metric(epsilon_substrate_dependency, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(epsilon_substrate_dependency, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(epsilon_substrate_dependency, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(epsilon_substrate_dependency, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epsilon_substrate_dependency, snare).
narrative_ontology:human_readable(epsilon_substrate_dependency, "Epsilon Substrate Dependency in Deferential Realism").
narrative_ontology:topic_domain(epsilon_substrate_dependency, "epistemology/formal_systems/measurement_theory").

domain_priors:requires_active_enforcement(epsilon_substrate_dependency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(epsilon_substrate_dependency, 'aa0df1c6-3149-4b90-9b59-4580e005b428').
narrative_ontology:cs_kernel_codification('aa0df1c6-3149-4b90-9b59-4580e005b428', formalized).
narrative_ontology:cs_authority_grounding('aa0df1c6-3149-4b90-9b59-4580e005b428', expertise).
narrative_ontology:cs_interpretation_layer_present('aa0df1c6-3149-4b90-9b59-4580e005b428').
narrative_ontology:cs_reading_relation('aa0df1c6-3149-4b90-9b59-4580e005b428', epsilon_substrate_dependency__vocabulary_collision_reading, coexists_with).
narrative_ontology:cs_reading_relation('aa0df1c6-3149-4b90-9b59-4580e005b428', epsilon_substrate_dependency__measurement_architecture_reading, coexists_with).
narrative_ontology:cs_axiom('aa0df1c6-3149-4b90-9b59-4580e005b428', foundational, seat_gauge_orientation_irreducibility).
narrative_ontology:cs_axiom_status(seat_gauge_orientation_irreducibility, holdable).
narrative_ontology:cs_axiom_grounding('aa0df1c6-3149-4b90-9b59-4580e005b428', seat_gauge_orientation_irreducibility, empirically_contingent).
narrative_ontology:cs_axiom('aa0df1c6-3149-4b90-9b59-4580e005b428', foundational, epsilon_judgment_necessity).
narrative_ontology:cs_axiom_status(epsilon_judgment_necessity, holdable).
narrative_ontology:cs_axiom_grounding('aa0df1c6-3149-4b90-9b59-4580e005b428', epsilon_judgment_necessity, instrumental).
narrative_ontology:cs_axiom('aa0df1c6-3149-4b90-9b59-4580e005b428', secondary, reading_independence_preservation).
narrative_ontology:cs_axiom_status(reading_independence_preservation, holdable).
narrative_ontology:cs_axiom_grounding('aa0df1c6-3149-4b90-9b59-4580e005b428', reading_independence_preservation, conventional).
narrative_ontology:cs_reference_frame('aa0df1c6-3149-4b90-9b59-4580e005b428', judgment_authored_epsilon_architecture).
narrative_ontology:cs_drift_state('aa0df1c6-3149-4b90-9b59-4580e005b428', post_oq26_reading_relativity_documentation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('aa0df1c6-3149-4b90-9b59-4580e005b428', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epsilon_substrate_dependency, framework_architects).
narrative_ontology:constraint_beneficiary(epsilon_substrate_dependency, analytical_flexibility).
narrative_ontology:constraint_victim(epsilon_substrate_dependency, empirical_grounding_expectation).
narrative_ontology:constraint_victim(epsilon_substrate_dependency, cross_reading_comparability).
narrative_ontology:constraint_victim(epsilon_substrate_dependency, external_auditors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(epsilon_substrate_dependency, story_generators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and maintain the Deferential Realism classification apparatus. Declare that base extractiveness epsilon is authored by judgment per reading rather than computed from world-anchors, preserving flexibility to model contested kernels and reading-relative classification. Control the §6.4 declaration discipline and operator ruling apparatus that adjudicates what counts as honest authoring versus gaming.
narrative_ontology:constraint_stakeholder(epsilon_substrate_dependency, framework_architects, agenda_setter,
    institutional, generational, arbitrage, universal).

% The capacity to model reading-dependent classification without forcing premature convergence. Benefits from epsilon remaining judgment-authored: different readings of the same kernel can declare different epsilon values without the framework rejecting one as false, enabling the committer axis to function.
narrative_ontology:constraint_stakeholder(epsilon_substrate_dependency, analytical_flexibility, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(epsilon_substrate_dependency, analytical_flexibility).

% The epistemic norm that a measurement framework's primitives should be anchored to observable world-structure rather than authored by judgment. Pays the cost of epsilon's substrate dependency: the framework's most load-bearing primitive (epsilon feeds chi, chi determines classification) rests on the least-grounded input, creating a judgment-bottleneck at the foundation.
narrative_ontology:constraint_stakeholder(epsilon_substrate_dependency, empirical_grounding_expectation, payer,
    analytical, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_non_agent(epsilon_substrate_dependency, empirical_grounding_expectation).

% The capacity to compare classifications across readings of the same kernel on a common metric basis. Constrained by epsilon substrate dependency: when two readings declare different epsilon values for structurally similar arrangements, the framework cannot adjudicate which is more accurate without external anchor, limiting cross-reading falsification.
narrative_ontology:constraint_stakeholder(epsilon_substrate_dependency, cross_reading_comparability, payer,
    analytical, generational, constrained, universal).
narrative_ontology:stakeholder_non_agent(epsilon_substrate_dependency, cross_reading_comparability).

% Researchers, domain experts, or oversight bodies attempting to validate Deferential Realism classifications against independent evidence. Face high audit cost: epsilon is the input they would most want to verify (it determines classification), but it is the one the framework declares judgment-authored and reading-relative, making independent verification structurally difficult.
narrative_ontology:constraint_stakeholder(epsilon_substrate_dependency, external_auditors, payer,
    institutional, biographical, constrained, global).

% Human or AI authors tasked with generating constraint stories. Must author epsilon values under §6.4 declaration discipline (claim and metrics independent, no tuning to predicted output) but lack clear world-anchored procedure for deriving epsilon, creating systematic variance across runs and generators even for the same constraint.
narrative_ontology:constraint_stakeholder(epsilon_substrate_dependency, story_generators, payer,
    moderate, biographical, constrained, global).

% Users of Deferential Realism classifications for decision-making, research, or policy analysis. Observe the epsilon substrate dependency as a known limitation: they can use the framework's outputs but must account for the judgment-authored foundation when assessing reliability.
narrative_ontology:constraint_stakeholder(epsilon_substrate_dependency, framework_consumers, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables the framework to model reading-dependent classification and contested kernels without forcing premature empirical convergence: different readings can declare different epsilon values, and the committer axis can function without requiring world-anchored epsilon computation.
% TRANSFER_FUNCTION: Transfers epistemic cost from framework architects (who gain flexibility) to empirical grounding expectation, cross-reading comparability, and external auditors (who lose anchor-based verification and face judgment-bottleneck at the foundation).
% ABSENT_VOICES: Formal epistemologists and measurement theorists who would argue for grounding primitives in observable structure are structurally absent from the operator ruling process that maintains epsilon as judgment-authored; their objection (that load-bearing primitives should not rest on least-grounded inputs) is anticipated but not adjudicated within the framework's authority structure.
% DISAPPEARANCE_RATIONALE: If epsilon substrate dependency vanished (epsilon became world-anchor-computed rather than judgment-authored), the committer axis would collapse or require fundamental redesign: readings could no longer declare independent epsilon values, cross-reading comparison would become mechanically decidable, and the framework would lose its capacity to model reading-relativity without forcing convergence. The entire seat/gauge/orientation architecture depends on epsilon remaining judgment-authored to preserve reading independence.
% FOUNDING_PROBLEM: Early framework development (pre-v8) faced the problem of modeling contested kernels where different parties genuinely see different constraints: forcing epsilon to be world-anchor-computed would either reject one reading as false or require a meta-level arbitration the framework was not designed to provide.
% FOUNDING_PROBLEM_CORROBORATION: Framework architects attest the problem remains live, citing ongoing kernel decomposition work (BGS, capital punishment, constitutional authority) where readings must declare independent epsilon values. External auditors and measurement theorists acknowledge the contested-kernel modeling problem is real but argue the solution (judgment-authored epsilon) creates a worse problem (ungrounded foundation). The OQ-26 reading-relativity documentation and §6.4 declaration discipline requirement provide independent evidence that epsilon variance is a known, managed tension rather than an oversight.
narrative_ontology:disappearance_verdict(epsilon_substrate_dependency, world_rearranges).
narrative_ontology:founding_problem_status(epsilon_substrate_dependency, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(epsilon_substrate_dependency, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-03',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(epsilon_substrate_dependency, 'none', 1).
narrative_ontology:epsilon_provenance(epsilon_substrate_dependency, 0.68, 'claude-sonnet-4-5-20250929', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epsilon_substrate_dependency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(epsilon_substrate_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epsilon_substrate_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68) because the arrangement transfers epistemic cost from architects to multiple victim seats (grounding expectation, comparability, auditors, generators) while concentrating flexibility benefit with the architects. Suppression is higher (0.72) because the constraint's persistence depends on active enforcement of §6.4 declaration discipline and operator rulings that maintain epsilon as judgment-authored against external pressure for world-anchored computation. Theater ratio is moderate (0.42): the declaration discipline is real (it does prevent metric-tuning to predicted outputs), but a growing share of enforcement activity defends the judgment-authored architecture itself rather than serving the original contested-kernel modeling problem. Accessibility collapse is low-moderate (0.38): alternatives exist (world-anchor epsilon computation, meta-level arbitration, rejecting reading-relativity) but are suppressed by the framework's authority structure. Resistance is moderate (0.55): external auditors and measurement theorists actively object, but framework consumers accept the tradeoff as documented limitation. The measurement series shows extraction and suppression intensifying as the framework matures and the epsilon substrate dependency becomes more load-bearing (more kernel families, more readings, more classification decisions riding on judgment-authored epsilon).
 *
 * PERSPECTIVAL GAP:
 *   From the framework architects' seat, epsilon substrate dependency is necessary flexibility that enables the framework's core function (modeling reading-relativity). From the external auditors' and empirical grounding expectation seats, the same structure operates as extraction: the framework claims measurement rigor while resting its most critical primitive on judgment, transferring audit cost to external parties. The engine computes this divergence from the structural data; the claimed type (snare) reflects the external-audit perspective while acknowledging the architects' coordination framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework architects are structural beneficiaries (collect flexibility, control the rules, arbitrage exit — d near beneficiary end). Empirical grounding expectation and cross-reading comparability are targets with identity-locked exit (the epistemic norms cannot leave the framework without dissolving their own function — d near full target). External auditors and story generators are constrained payers (bear verification and authoring costs, limited exit — d moderately high). Framework consumers are observers with mobile exit (can use or not use the framework — d near symmetric). Analytical flexibility is a non-agent beneficiary (abstract capacity, not a seat that experiences directionality).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy characteristics: the founding problem (modeling contested kernels without forced convergence) is live, but the solution (judgment-authored epsilon) has accumulated extraction beyond the coordination function. The rising theater ratio (0.25 → 0.42) indicates growing performative maintenance of declaration discipline relative to actual contested-kernel resolution. The framework could model reading-relativity with less extraction (e.g., world-anchored epsilon with explicit reading-dependent modifiers, or meta-level arbitration with documented uncertainty), but the current architecture concentrates flexibility with architects while diffusing audit cost. The snare classification captures this: genuine coordination function present, but extraction substantially exceeds coordination cost and persists through suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epsilon_generation_variance,
    'What is the actual variance in epsilon values across independent story generators for the same constraint, and does it exceed the variance the framework''s declaration discipline can tolerate?',
    'Systematic A/B generation study with multiple generators (human and AI) authoring epsilon for identical constraint scenarios, measuring inter-generator variance and comparing to intra-generator variance and to the epsilon differences that flip classification types.',
    'High variance that exceeds type-flip thresholds would establish epsilon substrate dependency as producing unreliable classifications; low variance would support the architects'' claim that declaration discipline constrains judgment sufficiently. Variance concentrated in contested-kernel cases would support the necessity claim; variance uniform across all constraint types would indicate systematic ungroundedness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_generation_variance, empirical, 'Whether epsilon generation variance undermines classification reliability').

omega_variable(
    world_anchor_feasibility,
    'Could epsilon be computed from world-anchored observables (power asymmetry, exit-option distribution, enforcement cost) without losing the capacity to model reading-dependent classification?',
    'Prototype implementation of world-anchor epsilon computation with explicit reading-dependent modifiers; test whether it can reproduce the classification divergence the current judgment-authored approach produces for known kernel families (BGS, capital punishment, constitutional authority).',
    'If feasible, world-anchor computation would eliminate the substrate dependency while preserving reading-relativity, exposing the current architecture as extractive rather than necessary. If infeasible, it would vindicate the architects'' necessity claim and reclassify the constraint as tangled rope (genuine coordination with unavoidable extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(world_anchor_feasibility, conceptual, 'Whether the judgment-authored architecture is structurally necessary or chosen').

omega_variable(
    declaration_discipline_enforcement,
    'Does §6.4 declaration discipline (claim and metrics independent, no tuning) actually prevent epsilon gaming, or does it create a theater of rigor while leaving epsilon effectively unconstrained?',
    'Audit of generated constraint stories for correlation between claimed_type and authored epsilon: if epsilon systematically tracks claimed_type despite the independence requirement, declaration discipline is failing. Cross-check against operator ruling history for cases where epsilon was rejected as gamed.',
    'If discipline is effective, the theater ratio should be lower and the constraint reclassifies toward tangled rope (real enforcement cost). If discipline is theater, the theater ratio is accurate and the constraint remains snare (extraction masked as rigor).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaration_discipline_enforcement, empirical, 'Whether declaration discipline constrains epsilon authoring or merely performs constraint').

omega_variable(
    committer_axis_necessity,
    'Is the committer axis (reading-dependent classification) a necessary feature for modeling contested kernels, or could the framework achieve the same analytical coverage through observer-axis diversity alone?',
    'Theoretical analysis of whether observer-axis power/exit variation can produce the same classification divergence that committer-axis reading variation produces; test on known kernel families whether observer-only modeling captures the structural differences readings claim to see.',
    'If observer-axis suffices, the committer axis and its epsilon substrate dependency are unnecessary complexity, and the constraint reclassifies as pure extraction. If committer-axis is necessary, it supports the architects'' coordination framing and shifts classification toward tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_axis_necessity, conceptual, 'Whether reading-relativity is necessary or redundant with observer-relativity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epsilon_substrate_dependency, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epsi_tr_t0, epsilon_substrate_dependency, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(epsi_tr_t0, observed).
narrative_ontology:measurement(epsi_tr_t6, epsilon_substrate_dependency, theater_ratio, 6, 0.31).
narrative_ontology:measurement_basis(epsi_tr_t6, observed).
narrative_ontology:measurement(epsi_tr_t12, epsilon_substrate_dependency, theater_ratio, 12, 0.36).
narrative_ontology:measurement_basis(epsi_tr_t12, observed).
narrative_ontology:measurement(epsi_tr_t18, epsilon_substrate_dependency, theater_ratio, 18, 0.39).
narrative_ontology:measurement_basis(epsi_tr_t18, observed).
narrative_ontology:measurement(epsi_tr_t24, epsilon_substrate_dependency, theater_ratio, 24, 0.42).
narrative_ontology:measurement_basis(epsi_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(epsi_be_t0, epsilon_substrate_dependency, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(epsi_be_t0, observed).
narrative_ontology:measurement(epsi_be_t6, epsilon_substrate_dependency, base_extractiveness, 6, 0.54).
narrative_ontology:measurement_basis(epsi_be_t6, observed).
narrative_ontology:measurement(epsi_be_t12, epsilon_substrate_dependency, base_extractiveness, 12, 0.61).
narrative_ontology:measurement_basis(epsi_be_t12, observed).
narrative_ontology:measurement(epsi_be_t18, epsilon_substrate_dependency, base_extractiveness, 18, 0.65).
narrative_ontology:measurement_basis(epsi_be_t18, observed).
narrative_ontology:measurement(epsi_be_t24, epsilon_substrate_dependency, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(epsi_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(epsi_su_t0, epsilon_substrate_dependency, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(epsi_su_t0, observed).
narrative_ontology:measurement(epsi_su_t6, epsilon_substrate_dependency, suppression_requirement, 6, 0.58).
narrative_ontology:measurement_basis(epsi_su_t6, observed).
narrative_ontology:measurement(epsi_su_t12, epsilon_substrate_dependency, suppression_requirement, 12, 0.65).
narrative_ontology:measurement_basis(epsi_su_t12, observed).
narrative_ontology:measurement(epsi_su_t18, epsilon_substrate_dependency, suppression_requirement, 18, 0.69).
narrative_ontology:measurement_basis(epsi_su_t18, observed).
narrative_ontology:measurement(epsi_su_t24, epsilon_substrate_dependency, suppression_requirement, 24, 0.72).
narrative_ontology:measurement_basis(epsi_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epsilon_substrate_dependency, identity_coordination).
narrative_ontology:affects_constraint(epsilon_substrate_dependency, seat_gauge_orientation_ontology).
narrative_ontology:affects_constraint(epsilon_substrate_dependency, declaration_discipline_enforcement).
narrative_ontology:affects_constraint(epsilon_substrate_dependency, reading_relativity_architecture).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the seat_gauge_orientation_kernel. All three readings (ontological_commitment, vocabulary_collision, measurement_architecture) must link via affects_constraints to model the kernel family. The epsilon substrate dependency is diagnosed differently by each reading: ontological_commitment (this reading) attributes it to role irreducibility; vocabulary_collision attributes it to naming confusion; measurement_architecture attributes it to asymmetric audit structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
