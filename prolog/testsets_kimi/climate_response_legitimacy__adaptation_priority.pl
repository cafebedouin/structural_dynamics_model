% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__adaptation_priority, []).

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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Climate Response Legitimacy: Adaptation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the adaptation_priority reading of the
 *   climate_response_legitimacy kernel. It frames legitimate climate action
 *   as accepting a warming trajectory and protecting vulnerable populations
 *   through resilience infrastructure and adaptive capacity. The reading
 *   creates structural asymmetry: wealthy nations preserve their development
 *   model and growth trajectory, while low-income regions enter the victim
 *   set through a persistent adaptation finance gap (estimated $350 billion
 *   annually) and future generations bear compounding warming costs. The
 *   constraint is not pure extraction because genuine adaptation needs exist;
 *   however, the framework systematically under-resources those needs while
 *   legitimizing the continued emissions of wealthy economies. The authored
 *   claim is tangled_rope because the coordination function is real but
 *   inseparable from asymmetric extraction.
 *
 * KEY AGENTS:
 *   - wealthy_nations (agenda_setter/beneficiary): Control climate finance architecture and preserve growth models
 *   - low_income_vulnerable_regions (payer): Bear climate impacts and the adaptation deficit
 *   - future_generations (excluded victim): Bear compounding warming costs with no voice
 *   - incumbent_industries (beneficiary): Avoid structural transformation under growth-preserving policy
 *   - climate_justice_movements (observer): Contest the framework as inadequate and expose protection gaps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.74).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.7).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.74).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Climate Response Legitimacy: Adaptation Priority Reading").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, '2fcd23e1-2222-4c94-9c1d-5d0f787b2176').
narrative_ontology:cs_kernel_codification('2fcd23e1-2222-4c94-9c1d-5d0f787b2176', formalized).
narrative_ontology:cs_authority_grounding('2fcd23e1-2222-4c94-9c1d-5d0f787b2176', lineage).
narrative_ontology:cs_interpretation_layer_present('2fcd23e1-2222-4c94-9c1d-5d0f787b2176').
narrative_ontology:cs_reading_relation('2fcd23e1-2222-4c94-9c1d-5d0f787b2176', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('2fcd23e1-2222-4c94-9c1d-5d0f787b2176', climate_response_legitimacy__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('2fcd23e1-2222-4c94-9c1d-5d0f787b2176', foundational, unavoidable_warming_requires_adaptive_protection).
narrative_ontology:cs_axiom_status(unavoidable_warming_requires_adaptive_protection, holdable).
narrative_ontology:cs_axiom_grounding('2fcd23e1-2222-4c94-9c1d-5d0f787b2176', unavoidable_warming_requires_adaptive_protection, empirically_contingent).
narrative_ontology:cs_axiom('2fcd23e1-2222-4c94-9c1d-5d0f787b2176', foundational, development_model_preservation_legitimate).
narrative_ontology:cs_axiom_status(development_model_preservation_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('2fcd23e1-2222-4c94-9c1d-5d0f787b2176', development_model_preservation_legitimate, conventional).
narrative_ontology:cs_reference_frame('2fcd23e1-2222-4c94-9c1d-5d0f787b2176', legitimate_climate_response_via_adaptation).
narrative_ontology:cs_drift_state('2fcd23e1-2222-4c94-9c1d-5d0f787b2176', contemporary_post_paris, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2fcd23e1-2222-4c94-9c1d-5d0f787b2176', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_nations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, incumbent_industries).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, low_income_vulnerable_regions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the international climate finance architecture and sets adaptation priorities through UNFCCC processes, NDCs, and bilateral aid. Preserves domestic economic growth models and avoids structural transformation while framing their climate response as resilience-building and protection of the vulnerable.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_nations, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, wealthy_nations, beneficiary).

% Face escalating climate impacts with an estimated $350 billion annual adaptation finance gap. Receive adaptation funding that is insufficient, often debt-bearing, and shaped by donor priorities rather than local needs. Cannot exit the warming trajectory or the international policy framework that accepts it.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, low_income_vulnerable_regions, payer,
    moderate, biographical, constrained, regional).

% Bear the compounding costs of deferred mitigation and accepted warming, including irreversible tipping points and locked-in sea-level rise. Not present in policy negotiations and unable to object to the adaptation-priority framing that legitimizes higher long-term warming.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, future_generations, excluded,
    powerless, civilizational, trapped, global).

% Continue operating under a growth-preserving policy framework that avoids rapid decarbonization mandates, structural economic transformation, or demand reduction implied by alternative readings of climate legitimacy.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, incumbent_industries, beneficiary,
    powerful, biographical, mobile, global).

% Analyze and contest the framework as inadequate, documenting that adaptation finance pledges are systematically unmet, that warming is outpacing resilience investment, and that the framework protects polluters while exposing vulnerable populations and future generations to compounding harms.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_justice_movements, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__adaptation_priority, wealthy_nations).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels resources and technical capacity to vulnerable populations to build resilience against climate impacts that are already occurring or locked in, solving the collective-action problem of who pays for unavoidable damages.
% TRANSFER_FUNCTION: Moves adaptation finance and technical capacity from wealthy nations to vulnerable regions, while transferring the costs of continued warming and deferred mitigation to low-income regions and future generations; preserves the economic growth trajectory and development model of wealthy nations and incumbent industries.
% ABSENT_VOICES: Future generations cannot object to the accepted warming trajectory; degrowth and radical mitigation advocates are procedurally marginalized in UNFCCC negotiations; informal-settlement residents, indigenous peoples, and non-state territories lack formal representation in adaptation finance allocation.
% DISAPPEARANCE_RATIONALE: Without this legitimating framework, wealthy nations would lose the policy architecture that allows them to claim climate action while preserving their growth model. The international climate regime would likely shift toward either aggressive mitigation or structural economic transformation, or face a severe legitimacy crisis as the gap between rhetoric and protection collapsed.
% FOUNDING_PROBLEM: Climate change is already causing harmful impacts that fall disproportionately on populations who contributed least to the problem; a legitimate response must protect them from these unavoidable impacts.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group II attests to adaptation needs and gaps from an analytical-scientific seat. Climate justice movements and civil-society organizations corroborate the protection deficit from outside the benefiting parties. Wealthy nations self-assert that their finance pledges address the problem, but independent OECD tracking and civil-society audits find substantial shortfalls relative to estimated needs.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__adaptation_priority, 0.74, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.74) is high because the framework permits wealthy nations to avoid structural economic transformation while the costs of accepted warming fall disproportionately on those with the least capacity to adapt. Suppression (0.70) is high because the legitimacy of this reading depends on actively marginalizing alternatives (mitigation_priority, degrowth_transformation) as economically disruptive or unrealistic. Theater ratio (0.58) is elevated: adaptation finance pledges are systematically unmet while the underlying growth model remains untouched. Accessibility collapse (0.60) reflects that within the UNFCCC architecture, alternatives are frequently ruled out of order as incompatible with national development priorities. Resistance (0.55) reflects sustained pressure from climate-vulnerable nations and justice movements. The measurement series share one time grid so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the wealthy-nation seat, the constraint is legitimate coordination fulfilling historical responsibility through resilience support; from the low-income-vulnerable-region seat, it is an extractive arrangement that accepts their immiseration while preserving the system that caused it. The future-generation seat would register near-full target. The engine computes this divergence from structural positions and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy nations are structural beneficiaries (d near the beneficiary end): the constraint subsidizes their growth model by exempting it from rapid transformation. Low-income vulnerable regions are targets (d near the target end): they bear the physical impacts and the finance gap. Future generations are trapped targets (d near the full-target end): they have no exit and no voice. Incumbent industries are beneficiaries (d near the beneficiary end). The divergence is driven by beneficiary and victim declarations modulated by exit options: wealthy nations and industries have arbitrage-grade or mobile exit; vulnerable regions are constrained; future generations are trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâprotecting vulnerable populations from unavoidable climate impactsâremains live, which prevents simple mandatrophy classification. However, the adaptation-priority reading risks mandatrophy if the warming it accepts becomes so severe that adaptation limits are breached, transforming the constraint from a tangled rope into a pure snare or piton. Currently, the coordination function is not entirely theatrical because resilience infrastructure does provide real benefits; the extraction is layered onto that coordination rather than replacing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_gap_structural_or_contingent,
    'Is the $350 billion annual adaptation finance gap a contingent failure of political will, or a structural consequence of preserving the wealthy-nation growth model within this reading?',
    'Comparative analysis of adaptation funding levels and institutional design in alternative policy frameworks (e.g., degrowth-transformed economies or high-mitigation scenarios) to determine whether the gap closes when growth preservation is not a binding constraint.',
    'If structural, the constraint is more deeply extractive than its coordination surface suggests and reform cannot untangle the rope without abandoning the reading''s core growth-preservation feature; if contingent, increased political will could in principle align the framework with its legitimating narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_gap_structural_or_contingent, conceptual, 'Whether the adaptation gap is structural or contingent to the framework').

omega_variable(
    warming_trajectory_adaptation_limit,
    'At what warming level does the accepted trajectory render adaptation and resilience insufficient, collapsing the coordination function of this constraint?',
    'Empirical tracking of loss and damage events relative to adaptation infrastructure effectiveness; synthesis of tipping-point literature and adaptation-limit studies from IPCC Working Group II.',
    'If adaptation limits are reached within the constraint''s time horizon, the constraint degrades toward piton or snareâperformative resilience maintaining legitimacy while actual protection fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warming_trajectory_adaptation_limit, empirical, 'Warming level at which adaptation function collapses').

omega_variable(
    suppression_mechanism_institutional_or_discursive,
    'Is the suppression of alternative readings (mitigation_priority, degrowth_transformation) primarily enforced through institutional rules of the UNFCCC, or through internalized political-economic common sense?',
    'Historical analysis of when alternative framings gained and lost formal traction in UNFCCC processes; tracking of which parties tabled motions aligned with sibling readings and how they were procedurally marginalized.',
    'Institutional suppression is more reversible through procedural reform; internalized suppression suggests higher effective extraction and lower exit options for reform within the current architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_institutional_or_discursive, conceptual, 'Structural versus internalized suppression of alternative climate framings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__adaptation_priority, theater_ratio, 0, 0.3).
narrative_ontology:measurement(clim_tr_t5, climate_response_legitimacy__adaptation_priority, theater_ratio, 5, 0.35).
narrative_ontology:measurement(clim_tr_t10, climate_response_legitimacy__adaptation_priority, theater_ratio, 10, 0.42).
narrative_ontology:measurement(clim_tr_t15, climate_response_legitimacy__adaptation_priority, theater_ratio, 15, 0.48).
narrative_ontology:measurement(clim_tr_t20, climate_response_legitimacy__adaptation_priority, theater_ratio, 20, 0.52).
narrative_ontology:measurement(clim_tr_t25, climate_response_legitimacy__adaptation_priority, theater_ratio, 25, 0.55).
narrative_ontology:measurement(clim_tr_t30, climate_response_legitimacy__adaptation_priority, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__adaptation_priority, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(clim_be_t5, climate_response_legitimacy__adaptation_priority, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(clim_be_t10, climate_response_legitimacy__adaptation_priority, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(clim_be_t15, climate_response_legitimacy__adaptation_priority, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(clim_be_t20, climate_response_legitimacy__adaptation_priority, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(clim_be_t25, climate_response_legitimacy__adaptation_priority, base_extractiveness, 25, 0.71).
narrative_ontology:measurement(clim_be_t30, climate_response_legitimacy__adaptation_priority, base_extractiveness, 30, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__adaptation_priority, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(clim_su_t5, climate_response_legitimacy__adaptation_priority, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(clim_su_t10, climate_response_legitimacy__adaptation_priority, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(clim_su_t15, climate_response_legitimacy__adaptation_priority, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(clim_su_t20, climate_response_legitimacy__adaptation_priority, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(clim_su_t25, climate_response_legitimacy__adaptation_priority, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(clim_su_t30, climate_response_legitimacy__adaptation_priority, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_response_legitimacy kernel, which decomposes into three structurally distinct constraints: adaptation_priority (this file), mitigation_priority, and degrowth_transformation. Each reading has a different epsilon, beneficiary/victim structure, and classification. They compete for legitimacy and finance within the same international policy architecture and are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
