% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__naturalist_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: price_formation_kernel__naturalist_reading
 *   human_readable: Naturalist Reading: Price Formation as Scarcity Equilibrium
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   This constraint instantiates the naturalist reading of the
 *   price_formation_kernel: the claim that prices emerge spontaneously from
 *   the intersection of objective scarcity and subjective preference,
 *   operating as a decentralized information-transfer mechanism. In housing
 *   markets, this reading treats observed prices as the efficient aggregation
 *   of supply constraints (land, materials, labor) and demand preferences
 *   (location, amenities, tenure type). Policy interventions â zoning, rent
 *   control, mortgage subsidies â are modeled as exogenous distortions that
 *   generate deadweight loss by obstructing the equilibrium path. The
 *   constraint is claimed as a Mountain: it would persist regardless of
 *   institutional structure, has no beneficiaries or victims, and requires no
 *   enforcement. The sibling readings (institutional, georgist,
 *   financialization) challenge this by identifying constructed, extractive,
 *   or disequilibrium dynamics that the naturalist reading renders invisible.
 *   This story is authored clean: only the naturalist reading's constraint is
 *   modeled here; the kernel contest is routed to omega variables and network
 *   edges.
 *
 * KEY AGENTS:
 *   - No seated beneficiaries or victims: the constraint is claimed as a natural law (Mountain) with zero extraction and no parties to name.
 *   - neoclassical_economists: Analytical observer (institutional/analytical) â formalizes and maintains the naturalist equilibrium framework.
 *   - housing_market_participants: Diffuse transactional seat (power varies) â operate within the constraint without being structurally subsidized or extracted by it under this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__naturalist_reading, 0.0).
domain_priors:suppression_score(price_formation_kernel__naturalist_reading, 0.0).
domain_priors:theater_ratio(price_formation_kernel__naturalist_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__naturalist_reading, mountain).
narrative_ontology:human_readable(price_formation_kernel__naturalist_reading, "Naturalist Reading: Price Formation as Scarcity Equilibrium").
narrative_ontology:topic_domain(price_formation_kernel__naturalist_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__naturalist_reading, 'a1f11277-d66a-427e-bcaa-d0afee2410d1').
narrative_ontology:cs_kernel_codification('a1f11277-d66a-427e-bcaa-d0afee2410d1', formalized).
narrative_ontology:cs_authority_grounding('a1f11277-d66a-427e-bcaa-d0afee2410d1', expertise).
narrative_ontology:cs_interpretation_layer_present('a1f11277-d66a-427e-bcaa-d0afee2410d1').
narrative_ontology:cs_reading_relation('a1f11277-d66a-427e-bcaa-d0afee2410d1', price_formation_kernel__institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('a1f11277-d66a-427e-bcaa-d0afee2410d1', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1f11277-d66a-427e-bcaa-d0afee2410d1', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('a1f11277-d66a-427e-bcaa-d0afee2410d1', foundational, price_reflects_scarcity_preference).
narrative_ontology:cs_axiom_status(price_reflects_scarcity_preference, holdable).
narrative_ontology:cs_axiom_grounding('a1f11277-d66a-427e-bcaa-d0afee2410d1', price_reflects_scarcity_preference, empirically_contingent).
narrative_ontology:cs_reference_frame('a1f11277-d66a-427e-bcaa-d0afee2410d1', scarcity_preference_equilibrium).
narrative_ontology:cs_drift_state('a1f11277-d66a-427e-bcaa-d0afee2410d1', post_financialization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a1f11277-d66a-427e-bcaa-d0afee2410d1', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__naturalist_reading, price_formation_kernel).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns resource allocation with relative scarcity and subjective preference without centralized coordination; prices clear markets by conveying decentralized information about supply and demand conditions.
% TRANSFER_FUNCTION: No transfer â the constraint governs the informational content of prices but does not move resources from one agent to another by design; any redistribution is a downstream market outcome, not an extracted transfer.
% ABSENT_VOICES: Institutionalist economists, Georgist campaigners, and financialization theorists are absent from the core naturalist framework; they would argue that scarcity is itself constructed by zoning, credit, and ownership structures, but their voices are excluded from the naturalist model's closed-form equilibrium.
% DISAPPEARANCE_RATIONALE: If the scarcity-preference equilibrium mechanism ceased to operate, resource allocation would lose its decentralized signaling system; however, as a natural-law constraint, it is not a human arrangement that could disappear overnight â the verdict encodes its status as a structural boundary condition rather than a constructed institution.
% FOUNDING_PROBLEM: Decentralized resource allocation under conditions of scarcity and heterogeneous preferences â the coordination problem that market exchange spontaneously solves without centralized design.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by the absence of any beneficiary institution claiming authorship; the phenomenon is observed across all human societies and historical periods regardless of political regime. Independent anthropological and historical evidence from outside the economics discipline confirms that price-like exchange ratios emerge spontaneously in diverse cultural contexts.
narrative_ontology:disappearance_verdict(price_formation_kernel__naturalist_reading, world_unchanged).
narrative_ontology:founding_problem_status(price_formation_kernel__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__naturalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__naturalist_reading, 0.0, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__naturalist_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(price_formation_kernel__naturalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(price_formation_kernel__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.0 because the naturalist reading asserts no extraction: prices are signals, not transfers. Suppression is 0.0 because the constraint requires no enforcement â it operates spontaneously. Theater_ratio is 0.0 because there is no performative maintenance. Accessibility_collapse is 0.95 because once scarcity and preference are understood as the drivers, alternative theories (pure construction, pure finance) collapse in explanatory power for the naturalist. Resistance is 0.02 because the constraint meets negligible active resistance â even critics typically accept scarcity as a factor, merely disputing its exclusivity. The metrics are authored independently of the claim: the claim is mountain, and the metrics describe a boundary-condition profile.
 *
 * PERSPECTIVAL GAP:
 *   Minimal perspectival divergence under this reading: all agents, regardless of power or scope, experience price as an ambient informational constraint rather than a targeted extraction. The only divergence is analytical â naturalist versus institutional observer frames â not distributional.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim declarations: directionality is uniform across all power atoms at the analytical fallback (near 0.5, symmetric), because the constraint does not extract from or subsidize any specific agent. The engine will derive d â 0.5 for all seats, producing near-zero effective extraction (Ï â 0) regardless of scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The mountain classification prevents mislabeling this constraint as a piton or snare: there is no concentrated beneficiary capturing rents, no enforcement apparatus, no theatrical compliance, and no atrophied mandate. If it were a piton, we would expect a high theater_ratio and an agenda_setter who could change it but does not; if it were a snare, we would expect identifiable victims and active suppression of alternatives. None are present. The constraint's persistence is fully decoupled from human maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_contestability,
    'Is price formation in contemporary housing markets genuinely a natural scarcity equilibrium, or is it constructed by institutional frameworks (zoning, credit policy, tax treatment) that the naturalist reading treats as exogenous?',
    'Comparative institutional analysis: measure price elasticity to housing supply under varying zoning regimes, and compare price paths across jurisdictions with different credit and tax institutions. If prices systematically track institutional variables rather than physical scarcity, the naturalness claim is weakened.',
    'If resolved toward construction, this constraint would reclassify from mountain to tangled_rope or snare under the institutional reading; if resolved toward naturalness, the mountain classification is corroborated and sibling readings are diagnosed as false summits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_contestability, empirical, 'Whether price formation is natural law or constructed institution').

omega_variable(
    policy_intervention_efficiency,
    'Do policy interventions in housing markets (rent control, zoning, mortgage deductions) systematically create deadweight loss relative to a counterfactual natural equilibrium, or do they correct pre-existing market failures (externalities, imperfect information, monopoly)?',
    'Natural experiments and quasi-experimental evidence from policy changes in comparable housing markets; measure deadweight loss against Pigouvian or Coasean benchmarks.',
    'If interventions correct market failures, the naturalist reading''s policy conclusions are undermined without necessarily dissolving the scarcity-equilibrium axiom; if they consistently create deadweight loss, the naturalist reading is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intervention_efficiency, empirical, 'Whether housing policy interventions are efficiency-reducing or failure-correcting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__naturalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(price_naturalist_tr_t0, price_formation_kernel__naturalist_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(price_naturalist_tr_t25, price_formation_kernel__naturalist_reading, theater_ratio, 25, 0.0).
narrative_ontology:measurement(price_naturalist_tr_t50, price_formation_kernel__naturalist_reading, theater_ratio, 50, 0.0).

% Extraction over time
narrative_ontology:measurement(price_naturalist_be_t0, price_formation_kernel__naturalist_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(price_naturalist_be_t25, price_formation_kernel__naturalist_reading, base_extractiveness, 25, 0.0).
narrative_ontology:measurement(price_naturalist_be_t50, price_formation_kernel__naturalist_reading, base_extractiveness, 50, 0.0).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(price_formation_kernel__naturalist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, financialization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested price_formation_kernel. The kernel decomposes into four structurally distinct constraints because the colloquial label 'price formation' conflates natural scarcity equilibrium (naturalist), institutional construction (institutional), land rent separation (georgist), and credit-driven asset pricing (financialization). Each reading has a different epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
