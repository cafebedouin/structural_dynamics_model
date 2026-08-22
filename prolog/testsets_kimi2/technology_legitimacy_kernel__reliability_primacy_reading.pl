% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__reliability_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__reliability_primacy_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__reliability_primacy_reading
 *   human_readable: Reliability-Primacy Climate Technology Legitimacy Gate
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This is the reliability_primacy reading of the
 *   technology_legitimacy_kernel: a policy-embedded criterion holding that a
 *   technology is legitimate for climate mitigation if and only if it
 *   provides dispatchable, baseload-capable generation to ensure grid
 *   stability. The constraint functions as a gatekeeping mechanism within
 *   energy governance, operating through grid codes, resource adequacy
 *   standards, and climate-finance eligibility rules. It benefits nuclear
 *   operators and incumbent dispatchable fleets while imposing costs on
 *   intermittent renewable developers and ratepayers who fund the resulting
 *   capacity markets and reliability infrastructure.
 *
 * KEY AGENTS:
 *   - Grid reliability authorities (agenda_setter, institutional/constrained) â administer the legitimacy gate through grid codes and resource adequacy standards.
 *   - Nuclear operators (beneficiary, powerful/constrained) â designated as inherently legitimate, gaining access to green finance and capacity markets.
 *   - Dispatchable fleet operators (beneficiary, powerful/constrained) â retain market relevance and revenue through reliability-must-run and capacity market structures.
 *   - Intermittent renewable developers (payer, moderate/constrained) â must add costly storage or firming to qualify, eroding their cost advantage.
 *   - Ratepayers (payer, powerless/trapped) â bear the cost of reliability infrastructure and capacity markets without practical grid exit.
 *   - Climate velocity advocates (excluded, organized/constrained) â prioritized speed of deployment is excluded when reliability is treated as the non-negotiable prerequisite.
 *   - Decarbonization analysts (observer, analytical/analytical) â evaluate whether alternative architectures could achieve stability without baseload.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.62).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.58).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Reliability-Primacy Climate Technology Legitimacy Gate").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, '33aa7572-9d56-46d1-9215-4702deff570d').
narrative_ontology:cs_kernel_codification('33aa7572-9d56-46d1-9215-4702deff570d', formalized).
narrative_ontology:cs_authority_grounding('33aa7572-9d56-46d1-9215-4702deff570d', expertise).
narrative_ontology:cs_interpretation_layer_present('33aa7572-9d56-46d1-9215-4702deff570d').
narrative_ontology:cs_reading_relation('33aa7572-9d56-46d1-9215-4702deff570d', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('33aa7572-9d56-46d1-9215-4702deff570d', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('33aa7572-9d56-46d1-9215-4702deff570d', foundational, dispatchable_baseload_prerequisite_for_climate_legitimacy).
narrative_ontology:cs_axiom_status(dispatchable_baseload_prerequisite_for_climate_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('33aa7572-9d56-46d1-9215-4702deff570d', dispatchable_baseload_prerequisite_for_climate_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('33aa7572-9d56-46d1-9215-4702deff570d', classical_baseload_grid_architecture).
narrative_ontology:cs_drift_state('33aa7572-9d56-46d1-9215-4702deff570d', high_renewable_penetration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('33aa7572-9d56-46d1-9215-4702deff570d', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, dispatchable_fleet_operators).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and operate nuclear generation assets that provide dispatchable, baseload-capable power. Under the reliability-primacy framing, their technology class is designated as inherently legitimate for climate mitigation, unlocking access to green finance, capacity markets, and streamlined permitting. Their exit is constrained by the massive capital intensity and long lead times of nuclear projects, which lock them into the policy framework.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_operators, beneficiary,
    powerful, generational, constrained, national).

% Operate gas, coal-with-CCS, geothermal, and reservoir hydro assets that can be dispatched on demand. The reliability criterion validates their continued role in a decarbonizing grid and sustains revenue streams through capacity markets and reliability-must-run contracts. Their exit is constrained by sunk infrastructure costs and asset-specific regulatory commitments.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, dispatchable_fleet_operators, beneficiary,
    powerful, biographical, constrained, national).

% Develop wind and solar photovoltaic projects that generate variable output. The constraint requires them to add costly storage or firming capacity to be considered legitimate climate mitigation, raising their levelized cost of capital and eliminating their cost advantage in many markets. Their exit options are constrained by the fact that grid interconnection and offtake agreements are gated by the reliability standard.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers, payer,
    moderate, biographical, constrained, global).

% Residential and commercial electricity consumers who pay tariffs that fund capacity markets, reliability reserves, and grid infrastructure sized for baseload-plus-backup architectures rather than optimized renewable-storage systems. They are locked into the distribution grid with no practical exit to alternative supply in most jurisdictions.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers, payer,
    powerless, biographical, trapped, national).

% Institutional bodies responsible for setting and enforcing grid codes, resource adequacy standards, and reliability criteria. They administer the legitimacy gate by determining which technologies qualify as dispatchable and baseload-capable. Their position is constrained by statutory reliability mandates and inherited engineering paradigms.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_reliability_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Climate policy advocates and NGOs who prioritize rapid deployment of the fastest-available technologies to meet carbon budget constraints. They are structurally excluded from the legitimacy conversation when reliability is treated as the non-negotiable prerequisite, because their preferred technologies (unfettered intermittent renewables) fail the dispatchability test.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_velocity_advocates, excluded,
    organized, generational, constrained, global).

% Independent researchers and system modelers who evaluate tradeoffs between speed of decarbonization, cost, and reliability. They observe that alternative grid architectures can achieve stability without traditional baseload, but their findings are often dismissed by reliability authorities as insufficiently conservative.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, decarbonization_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Electrical grids require real-time balancing of generation and load; the constraint coordinates investment toward technologies that provide frequency regulation, inertia, and dispatchable capacity to prevent instability and blackout.
% TRANSFER_FUNCTION: Moves capital, policy support, and legitimacy from intermittent renewable developers and ratepayers toward dispatchable generators (nuclear, gas, hydro, geothermal) by making dispatchability the gate for climate finance and grid access.
% ABSENT_VOICES: Climate velocity advocates who argue carbon budget timelines override reliability framing; environmental justice advocates concerned with nuclear waste legacy and fossil-fuel lock-in; distributed-energy proponents who argue demand-side flexibility and prosumer aggregation can substitute for supply-side baseload.
% DISAPPEARANCE_RATIONALE: If the reliability-as-legitimacy criterion vanished overnight, capital would flow to lowest-cost intermittent generation, capacity markets would shrink or transform into storage/flexibility markets, grid operators would pursue alternative stability models (demand response, sector coupling, geographic smoothing), and the technology mix would restructure around speed and cost rather than dispatchability.
% FOUNDING_PROBLEM: Electrical grids with high penetration of variable renewable energy face challenges in maintaining frequency stability and resource adequacy, requiring either dispatchable backup generation or expensive grid-scale storage and flexibility infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Grid engineers and reliability authorities attest the stability problem is live under current architectures. Climate economists, renewable energy system analysts, and international energy agencies attest the problem is solvable through alternative architectures and that the baseload framing imposes unnecessary costs and deployment delays; these sources sit outside the beneficiary set.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__reliability_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the constraint systematically transfers capital and policy legitimacy toward dispatchable incumbents and adds capital burdens to intermittent challengers and ratepayers. Suppression (0.58) reflects the structural marginalization of alternative grid architectures (high-renewable, demand-flexibility, distributed systems) through grid codes and finance gates, though these alternatives are not violently extinguished. Theater ratio (0.28) is low-moderate: grid stability is a genuine engineering need, but the exclusivity framing (if and only if dispatchable baseload) performs more certainty than the technical literature supports, given emerging demonstrations of stable high-renewable grids. Accessibility collapse (0.45) is moderateâalternatives to baseload are technically understood but economically and politically disadvantaged. Resistance (0.55) is significant from renewable industries, climate economists, and system analysts who see the criterion as slowing decarbonization.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear and dispatchable operators experience the constraint as legitimate engineering prudence that validates their role; intermittent developers experience it as a protectionist gate that neutralizes their cost advantage; ratepayers experience it as an opaque cost adder on their bills; reliability authorities experience it as the institutionalization of their professional mandate. The engine computes these divergent seat classifications from the structural asymmetries in exit options, beneficiary declarations, and power.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (nuclear_operators, dispatchable_fleet_operators) sit near the full-beneficiary end of directionalityâthey are subsidized by policy legitimacy and market structures the constraint creates. Payers (intermittent_renewable_developers, ratepayers) sit near the full-target endâthey bear the extraction directly through added capital costs and tariff-funded capacity markets. Grid reliability authorities, as agenda_setters, sit closer to the beneficiary end than symmetric but are constrained by their statutory mandate rather than capturing rents. Climate velocity advocates are excluded from the directionality derivation entirely by their exclusion from the stakeholder conversation.
 *
 * MANDATROPHY ANALYSIS:
 *   Without acknowledging victims, the constraint might be misread as a Rope (genuine coordination around grid stability). The presence of ratepayers and intermittent developers as identifiable victims prevents that misclassification. Conversely, without acknowledging the real coordination function (grid stability is an actual collective-action problem), it might be misread as a Snare (pure extraction). The Tangled Rope classification captures the hybrid: the coordination is real, but the chosen solution (dispatchable baseload as the exclusive legitimacy criterion) asymmetrically benefits incumbent technologies and extracts from challengers and consumers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reliability_vs_alternative_stability_architectures,
    'Is dispatchable baseload the only technically viable path to grid stability, or can high shares of variable renewables achieve equivalent reliability through storage, demand flexibility, and geographic smoothing?',
    'Comparative grid modeling and empirical demonstration of high-renewable systems (e.g., Denmark, South Australia, Iberian peninsula) achieving stability without baseload; cost-optimal capacity expansion modeling under different flexibility assumptions.',
    'If alternative architectures achieve equivalent stability, the coordination function could be met without the current extraction from intermittent developers and ratepayers, pushing classification toward snare; if baseload remains indispensable, the coordination function is genuine and the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliability_vs_alternative_stability_architectures, empirical, 'Whether grid stability requires baseload or can be achieved through alternative architectures.').

omega_variable(
    kernel_reading_contest,
    'Does the reliability_primacy reading capture an objective structural feature of energy governance, or is it one of several competing frames that redistribute costs and benefits differently without neutral adjudication?',
    'Cross-jurisdictional comparison of jurisdictions adopting different primary readings (e.g., velocity-priority jurisdictions with high renewable deployment targets vs. reliability-priority jurisdictions with baseload preservation), measuring outcomes on stability, cost, and decarbonization speed.',
    'If outcomes are similar across readings, the constraint is primarily a distributional mechanism; if reliability-first jurisdictions show measurably more stable grids but slower decarbonization, the reading captures a real coordination-extraction tradeoff.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the reliability reading reflects objective grid physics or a contestable policy frame.').

omega_variable(
    ratepayer_cost_attribution,
    'How much of the reliability cost borne by ratepayers is technically necessary for grid stability versus a transfer to incumbent dispatchable generators through capacity market design?',
    'Regulatory cost-of-service audits, capacity market price discovery analysis, and comparison of ratepayer bills across jurisdictions with different resource adequacy mechanisms.',
    'High unnecessary cost would indicate the extraction component dominates the coordination component; low unnecessary cost would support the tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratepayer_cost_attribution, empirical, 'The proportion of ratepayer reliability costs that are technically necessary versus transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tech_tr_t5, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(tech_tr_t10, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(tech_tr_t15, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(tech_tr_t25, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement(tech_tr_t30, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tech_be_t5, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(tech_be_t10, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(tech_be_t15, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(tech_be_t25, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(tech_be_t30, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(tech_su_t5, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(tech_su_t10, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(tech_su_t15, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(tech_su_t25, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(tech_su_t30, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__reliability_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, velocity_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, precautionary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the technology_legitimacy_kernel, decomposed from the colloquial label 'technology legitimacy for climate mitigation' which conflates reliability, velocity, and precautionary criteria. Each reading has a distinct beneficiary/victim structure and epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
