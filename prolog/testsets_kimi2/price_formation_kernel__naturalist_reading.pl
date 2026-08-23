% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Naturalist Reading of Price Formation
 *   domain: political_economy/housing_markets/institutional_analysis
 *
 * SUMMARY:
 *   Under the naturalist reading, price formation in housing markets is a
 *   mountain constraint: an emergent equilibrium process reflecting objective
 *   scarcity and preference. It is not constructed by any institution and
 *   extracts from no one. Policy interventions that override price signals
 *   are understood as distortions that create deadweight loss. The constraint
 *   operates without seated parties: no beneficiary captures rents from its
 *   operation, and no victim bears its costs. The kernel is contested by
 *   other readings that treat price formation as constructed or extractive;
 *   those are modeled as separate constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__naturalist_reading, 0.02).
domain_priors:suppression_score(price_formation_kernel__naturalist_reading, 0.05).
domain_priors:theater_ratio(price_formation_kernel__naturalist_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(price_formation_kernel__naturalist_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__naturalist_reading, mountain).
narrative_ontology:human_readable(price_formation_kernel__naturalist_reading, "Naturalist Reading of Price Formation").
narrative_ontology:topic_domain(price_formation_kernel__naturalist_reading, "political_economy/housing_markets/institutional_analysis").

domain_priors:emerges_naturally(price_formation_kernel__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__naturalist_reading, 'b0d62ffc-c668-4d3f-a714-d47ba63153e0').
narrative_ontology:cs_kernel_codification('b0d62ffc-c668-4d3f-a714-d47ba63153e0', formalized).
narrative_ontology:cs_authority_grounding('b0d62ffc-c668-4d3f-a714-d47ba63153e0', expertise).
narrative_ontology:cs_interpretation_layer_present('b0d62ffc-c668-4d3f-a714-d47ba63153e0').
narrative_ontology:cs_reading_relation('b0d62ffc-c668-4d3f-a714-d47ba63153e0', price_formation_kernel__institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('b0d62ffc-c668-4d3f-a714-d47ba63153e0', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0d62ffc-c668-4d3f-a714-d47ba63153e0', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('b0d62ffc-c668-4d3f-a714-d47ba63153e0', foundational, price_reflects_objective_scarcity).
narrative_ontology:cs_axiom_status(price_reflects_objective_scarcity, holdable).
narrative_ontology:cs_axiom_grounding('b0d62ffc-c668-4d3f-a714-d47ba63153e0', price_reflects_objective_scarcity, empirically_contingent).
narrative_ontology:cs_axiom('b0d62ffc-c668-4d3f-a714-d47ba63153e0', secondary, intervention_distorts_natural_allocation).
narrative_ontology:cs_axiom_status(intervention_distorts_natural_allocation, holdable).
narrative_ontology:cs_axiom_grounding('b0d62ffc-c668-4d3f-a714-d47ba63153e0', intervention_distorts_natural_allocation, instrumental).
narrative_ontology:cs_reference_frame('b0d62ffc-c668-4d3f-a714-d47ba63153e0', competitive_equilibrium_baseline).
narrative_ontology:cs_drift_state('b0d62ffc-c668-4d3f-a714-d47ba63153e0', contemporary_housing_policy_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b0d62ffc-c668-4d3f-a714-d47ba63153e0', '').
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
% COORDINATION_FUNCTION: Allocates scarce housing and goods to highest-valuation uses through decentralized price signals without central coordination.
% TRANSFER_FUNCTION: Moves purchasing power and resources toward scarcer goods; no net extraction is structurally required between agent classes under this reading.
% ABSENT_VOICES: Institutionalist economists, georgist analysts, and financialization researchers are absent from the naturalist framing, which treats zoning, land rent, and credit expansion as secondary frictions rather than structural price determinants.
% DISAPPEARANCE_RATIONALE: Overnight disappearance of the equilibrium price mechanism would eliminate scarcity signaling, requiring central planning or arbitrary allocation and collapsing decentralized market coordination in housing and other sectors.
% FOUNDING_PROBLEM: How to allocate scarce resources among competing uses in the absence of a central planner.
% FOUNDING_PROBLEM_CORROBORATION: Attested by the broad neoclassical economics tradition and by the observable persistence of scarcity-driven exchange; contested by institutional and heterodox economists who argue the problem is already solved by constructed mechanisms.
narrative_ontology:disappearance_verdict(price_formation_kernel__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__naturalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__naturalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(price_formation_kernel__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__naturalist_reading, 0.02, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is negligible (0.02) because the naturalist reading treats price as a discovered signal, not a constructed transfer. Suppression is negligible (0.05) because the constraint requires no enforcement to persist. Accessibility collapse is very high (0.92): once scarcity is understood, alternatives such as arbitrary price-setting or non-market allocation collapse as viable. Resistance is minimal (0.05) because agents do not oppose a natural law; they adapt to it. Theater ratio is near zero (0.02) because there is no performative maintenance. The measurement series shows flat low values across the interval, consistent with a stable natural-law constraint.
 *
 * PERSPECTIVAL GAP:
 *   The naturalist reading computes as mountain from every analytical seat because the constraint has no seated parties. However, observers adopting sibling readings of the same kernel would compute the same observed price data as extractive coordination with asymmetric beneficiaries. That divergence is captured by the kernel's decomposition into separate constraints, not by per-seat variation within this story.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim declarations are authored because the naturalist reading asserts no party is structurally subsidized or extracted by the equilibrium mechanism. All agents face the constraint symmetrically as a feature of their environment. Directionality is uniform and near-neutral: the constraint neither subsidizes nor targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a mandate and therefore not subject to mandatrophy. Its persistence does not depend on enforcement or institutional maintenance. The classification prevents mislabeling by requiring active enforcement and seated parties for tangled_rope or snare; their absence here certifies the mountain reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_naturalist_contest,
    'This constraint is one reading of kernel price_formation_kernel (naturalist_reading). What would change structurally if the sibling readings (institutional_reading, georgist_reading, financialization_reading) were adopted?',
    'Comparative analysis of housing price data against institutional variation, land rent decomposition, and credit expansion metrics to test which reading''s structural predictions match observed outcomes.',
    'Under sibling readings, price formation acquires beneficiaries, victims, and active enforcement, reclassifying the constraint from mountain to tangled_rope or snare; under the naturalist reading, it remains a zero-party mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_naturalist_contest, conceptual, 'Structural delta between naturalist reading and sibling readings of the price formation kernel').

omega_variable(
    housing_market_natural_law_ambiguity,
    'Is the natural equilibrium reading of housing price formation a genuine natural law, or a constructed frame that obscures institutional and financial drivers?',
    'Empirical testing of whether housing prices clear at scarcity-preference equilibrium when institutional variables (zoning, tax treatment, credit supply) are controlled.',
    'If institutional drivers dominate, the mountain claim is falsified; the constraint must be decomposed into multiple constraints per the epsilon-invariance principle, with non-zero extraction and identifiable parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(housing_market_natural_law_ambiguity, empirical, 'Empirical ambiguity between natural-law and constructed framing of housing price formation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__naturalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(price_nat_tr_t0, price_formation_kernel__naturalist_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(price_nat_tr_t15, price_formation_kernel__naturalist_reading, theater_ratio, 15, 0.02).
narrative_ontology:measurement(price_nat_tr_t30, price_formation_kernel__naturalist_reading, theater_ratio, 30, 0.02).

% Extraction over time
narrative_ontology:measurement(price_nat_be_t0, price_formation_kernel__naturalist_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(price_nat_be_t15, price_formation_kernel__naturalist_reading, base_extractiveness, 15, 0.02).
narrative_ontology:measurement(price_nat_be_t30, price_formation_kernel__naturalist_reading, base_extractiveness, 30, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(price_formation_kernel__naturalist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, institutional_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__naturalist_reading, financialization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the price_formation_kernel. The naturalist reading treats price formation as a mountain (zero extraction, no parties). Sibling readings decompose the same kernel into institutional, georgist, and financialization constraints with distinct beneficiary/victim structures and higher extraction. See their respective stories for structural deltas.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
