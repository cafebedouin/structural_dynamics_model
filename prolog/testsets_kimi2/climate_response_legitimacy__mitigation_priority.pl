% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__mitigation_priority, []).

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
 *   constraint_id: climate_response_legitimacy__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Response Legitimacy
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint story models the 'mitigation_priority' reading of the
 *   contested climate_response_legitimacy kernel. Under this reading,
 *   legitimate climate response is defined as prioritizing emissions
 *   reductions through technological innovation and carbon pricing while
 *   preserving economic growth (decoupling). The framework operates as a
 *   global policy paradigm administered by international climate
 *   institutions, extracting transition costs from fossil fuel workforces and
 *   risking future generations if technological decoupling fails. It is
 *   claimed as coordination (solving the atmospheric commons dilemma) but
 *   structurally functions as asymmetric extraction: some parties are
 *   coordinated into compliance while others bear the costs and risks of the
 *   chosen pathway. The framework suppresses alternative readings
 *   (adaptation-priority, degrowth-transformation) through institutional
 *   marginalization despite their logical coherence.
 *
 * KEY AGENTS:
 *   - International climate institutions (IPCC/OECD/UNFCCC): Agenda-setter â institutional power, analytical exit, administers the growth-preserving mitigation framework.
 *   - Renewable technology sector: Beneficiary â powerful, mobile exit, captures policy rents and market share from the framework.
 *   - Carbon market infrastructure: Beneficiary â institutional, arbitrage exit, collects rents from trading and verification.
 *   - Fossil fuel workforces: Payer â moderate power, constrained exit, bear stranded assets and transition displacement.
 *   - Future generations: Payer â powerless, trapped exit, locked into climate outcomes with no voice.
 *   - Climate vulnerable nations: Payer â powerless, constrained exit, bear physical impacts and CDR land pressures.
 *   - Degrowth advocates: Excluded â organized, mobile exit, systematically marginalized from institutional processes.
 *   - Climate justice analysts: Observer â organized, analytical exit, document distributional asymmetries.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, 0.62).
domain_priors:suppression_score(climate_response_legitimacy__mitigation_priority, 0.55).
domain_priors:theater_ratio(climate_response_legitimacy__mitigation_priority, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__mitigation_priority, "Mitigation-Priority Climate Response Legitimacy").
narrative_ontology:topic_domain(climate_response_legitimacy__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__mitigation_priority, '582e0095-2efd-4bca-b980-a85648f8326e').
narrative_ontology:cs_kernel_codification('582e0095-2efd-4bca-b980-a85648f8326e', formalized).
narrative_ontology:cs_authority_grounding('582e0095-2efd-4bca-b980-a85648f8326e', expertise).
narrative_ontology:cs_interpretation_layer_present('582e0095-2efd-4bca-b980-a85648f8326e').
narrative_ontology:cs_reading_relation('582e0095-2efd-4bca-b980-a85648f8326e', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('582e0095-2efd-4bca-b980-a85648f8326e', climate_response_legitimacy__degrowth_transformation, forecloses).
narrative_ontology:cs_axiom('582e0095-2efd-4bca-b980-a85648f8326e', foundational, growth_preservation_imperative).
narrative_ontology:cs_axiom_status(growth_preservation_imperative, holdable).
narrative_ontology:cs_axiom_grounding('582e0095-2efd-4bca-b980-a85648f8326e', growth_preservation_imperative, instrumental).
narrative_ontology:cs_axiom('582e0095-2efd-4bca-b980-a85648f8326e', foundational, technological_decoupling_sufficiency).
narrative_ontology:cs_axiom_status(technological_decoupling_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('582e0095-2efd-4bca-b980-a85648f8326e', technological_decoupling_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('582e0095-2efd-4bca-b980-a85648f8326e', market_mitigation_optimality).
narrative_ontology:cs_drift_state('582e0095-2efd-4bca-b980-a85648f8326e', contemporary_emissions_gap_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('582e0095-2efd-4bca-b980-a85648f8326e', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, renewable_tech_sector).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, carbon_market_infrastructure).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, fossil_fuel_workforces).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, climate_vulnerable_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the global mitigation architecture through IPCC assessment reports, UNFCCC negotiations, and OECD policy coordination. Sets the framing that legitimate climate response must preserve economic growth while decarbonizing through technological innovation and carbon pricing. Exit would mean abandoning the expert-legitimated policy paradigm they have built over three decades.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, international_climate_institutions, agenda_setter,
    institutional, civilizational, analytical, global).

% Receives substantial policy support, subsidies, and mandated market share through the mitigation-priority framework. Benefits from carbon pricing that disadvantages fossil competitors and from government procurement of renewable infrastructure. Can pivot to alternative technological pathways but is deeply fused with the current policy paradigm.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, renewable_tech_sector, beneficiary,
    powerful, biographical, mobile, global).

% Operates exchanges, verification bodies, and offset registries that monetize the carbon price signal. Directly collects rents from trading volumes and certification fees. The constraint's enforcement of emissions accounting and compliance creates the market itself.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, carbon_market_infrastructure, beneficiary,
    institutional, biographical, arbitrage, global).

% Bear the transition costs of the mitigation-priority framework through job losses, community decline, and stranded assets in extraction and processing regions. They pay through economic displacement rather than direct transfer, with limited geographic mobility and retraining access.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, fossil_fuel_workforces, payer,
    moderate, biographical, constrained, regional).

% Are structurally locked into the climate outcomes produced by current mitigation choices. If technological decoupling and CDR scale-up fail, they bear the full cost of overshoot and delayed action with no institutional voice in present decisions.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Face disproportionate climate impacts while the framework prioritizes emissions reduction over adaptation finance. Many host land-based CDR projects that displace local populations. Their influence in setting the mitigation agenda is minimal despite bearing the highest physical risks.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, climate_vulnerable_nations, payer,
    powerless, generational, constrained, global).

% Argue that preserving economic growth is incompatible with planetary boundaries and that the mitigation-priority framework is a delaying tactic. They are systematically marginalized in IPCC plenary processes and OECD policy forums despite holding intellectual positions that challenge the framework's core premises.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, degrowth_advocates, excluded,
    organized, generational, mobile, global).

% Document the distributional asymmetries of carbon pricing and technological mitigation pathways. They track whether transition costs fall on vulnerable populations and whether the framework's benefits accrue to wealthy nations and corporations.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__mitigation_priority, climate_justice_analysts, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global mitigation action by creating a common price signal for carbon emissions and directing capital toward low-carbon technological innovation, solving the free-riding problem on the atmospheric commons through enforceable national commitments.
% TRANSFER_FUNCTION: Moves transition costs and stranded asset risks from the present economic system to fossil fuel workforces and communities; moves climate risk to future generations if technological decoupling proves insufficient; moves capital and policy support to renewable technology and carbon market infrastructure.
% ABSENT_VOICES: Degrowth advocates arguing for dismantling growth imperatives; climate justice movements from the Global South arguing for reparative and non-market frameworks; indigenous peoples whose land is targeted for CDR scale-up. These voices are institutionally marginalized in OECD and IPCC mitigation-focused processes.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority framework vanished, carbon pricing regimes would collapse, renewable subsidy structures would shift, fossil fuel transition timelines would be renegotiated, and the policy vacuum would likely be filled by adaptation-emergency or degrowth-transformation agendas â the global climate governance architecture would reorganize around different first principles.
% FOUNDING_PROBLEM: The atmospheric commons is a collective-action problem where individual nations and firms have incentives to free-ride on emissions reductions, requiring a coordinated global response to avoid dangerous warming.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists attest the commons problem is live. However, heterodox economists and climate justice advocates from outside the beneficiary set contest whether the mitigation-priority framework was designed to solve the commons problem or to preserve existing growth-centric political economy; they argue the founding problem has been co-opted by growth-preserving interests. Corroboration is split across seats.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_legitimacy__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__mitigation_priority, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that the constraint moves real costs to fossil fuel workforces and future generations while concentrating policy benefits in technology and finance sectors. Suppression (0.55) captures the active exclusion of degrowth and adaptation-first alternatives from OECD/IPCC processes. Theater ratio (0.40) acknowledges the growing performative dimension: net-zero pledges, offset markets, and techno-optimistic projections that outpace delivery. Accessibility collapse (0.50) is moderate because alternative frameworks remain intellectually accessible but are institutionally marginalized. Resistance (0.60) is substantial from fossil interests and climate justice movements. The metrics and claimed_type are independently authored: the engine may compute a different per-seat classification, which is the intended measurement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (international institutions) experiences the constraint as genuine coordination solving a collective-action problem. The payer seats (fossil workforces, climate-vulnerable nations, future generations) experience it as asymmetric risk transfer. The beneficiary seats (renewable sector, carbon markets) experience it as opportunity creation. These divergences are structurally determined by directionality: beneficiaries have low d, payers have high d, and the agenda-setter sits near the coordination center.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (renewable_tech_sector, carbon_market_infrastructure) receive policy rents and market creation, placing them at low d. Agenda-setters administer the framework without direct extraction, sitting near d=0.5. Payers include fossil_fuel_workforces (economic displacement, high d), climate_vulnerable_nations (risk transfer, high d), and future_generations (locked-in outcomes, near d=1.0 with trapped exit). Excluded actors (degrowth_advocates) are pushed outside the directionality calculation by their exclusion from the institutional framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (atmospheric commons free-riding) remains live, but the solution framework has drifted toward preserving growth imperatives that may be incompatible with the problem's actual solution. This is contested: beneficiaries claim the framework is still solving the founding problem, while payers and excluded voices argue the framework has been captured by growth-preserving interests. Mandatrophy is not resolved because the founding problem is contested rather than dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_empirical_status,
    'Is absolute decoupling of GDP from emissions actually occurring at the scale and speed required to meet climate targets under the mitigation-priority framework?',
    'Comprehensive meta-analysis of decoupling trends in OECD economies, including consumption-based accounting and material footprint indicators, alongside IPCC scenario validation against observed emissions trajectories.',
    'If absolute decoupling is not occurring, the framework''s foundational empirical claim is falsified, increasing extractiveness and potentially reclassifying the constraint toward snare as the coordination story becomes cover for growth preservation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_empirical_status, empirical, 'Whether GDP-emissions decoupling is empirically realized at required rates.').

omega_variable(
    intergenerational_risk_assignment,
    'Does the framework''s reliance on future CDR and innovation constitute a structural transfer of risk to future generations, or a legitimate investment in their future?',
    'Intergenerational equity audit comparing the framework''s implicit discount rates against climate damages projections, and analysis of whether CDR scale-up is treated as certainty or contingency in policy design.',
    'If the risk transfer is structural, future_generations'' directionality moves closer to full target and the constraint''s asymmetric extraction intensifies; if legitimate investment, the victim status is contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_risk_assignment, conceptual, 'Whether technological dependency structurally offloads climate risk to the future.').

omega_variable(
    carbon_pricing_regressive_impact,
    'Does carbon pricing extract regressively from low-income households, and if so, does revenue recycling offset this within the mitigation-priority framework?',
    'Distributional incidence studies across jurisdictions with operational carbon pricing, tracking household burden by income decile and the presence or absence of revenue recycling mechanisms.',
    'If carbon pricing is regressive and unrecycled, the victim set expands to include low-income current households, raising extractiveness and potentially revealing a broader snare structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_pricing_regressive_impact, empirical, 'Whether carbon pricing operates as a regressive extraction mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__mitigation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crlp_mit_tr_t0, climate_response_legitimacy__mitigation_priority, theater_ratio, 0, 0.15).
narrative_ontology:measurement(crlp_mit_tr_t5, climate_response_legitimacy__mitigation_priority, theater_ratio, 5, 0.2).
narrative_ontology:measurement(crlp_mit_tr_t10, climate_response_legitimacy__mitigation_priority, theater_ratio, 10, 0.26).
narrative_ontology:measurement(crlp_mit_tr_t15, climate_response_legitimacy__mitigation_priority, theater_ratio, 15, 0.32).
narrative_ontology:measurement(crlp_mit_tr_t20, climate_response_legitimacy__mitigation_priority, theater_ratio, 20, 0.36).
narrative_ontology:measurement(crlp_mit_tr_t25, climate_response_legitimacy__mitigation_priority, theater_ratio, 25, 0.38).
narrative_ontology:measurement(crlp_mit_tr_t30, climate_response_legitimacy__mitigation_priority, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(crlp_mit_be_t0, climate_response_legitimacy__mitigation_priority, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(crlp_mit_be_t5, climate_response_legitimacy__mitigation_priority, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(crlp_mit_be_t10, climate_response_legitimacy__mitigation_priority, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(crlp_mit_be_t15, climate_response_legitimacy__mitigation_priority, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(crlp_mit_be_t20, climate_response_legitimacy__mitigation_priority, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(crlp_mit_be_t25, climate_response_legitimacy__mitigation_priority, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(crlp_mit_be_t30, climate_response_legitimacy__mitigation_priority, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(crlp_mit_su_t0, climate_response_legitimacy__mitigation_priority, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(crlp_mit_su_t5, climate_response_legitimacy__mitigation_priority, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(crlp_mit_su_t10, climate_response_legitimacy__mitigation_priority, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(crlp_mit_su_t15, climate_response_legitimacy__mitigation_priority, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(crlp_mit_su_t20, climate_response_legitimacy__mitigation_priority, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(crlp_mit_su_t25, climate_response_legitimacy__mitigation_priority, suppression_requirement, 25, 0.53).
narrative_ontology:measurement(crlp_mit_su_t30, climate_response_legitimacy__mitigation_priority, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__mitigation_priority, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
