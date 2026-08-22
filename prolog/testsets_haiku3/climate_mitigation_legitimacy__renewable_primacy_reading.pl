% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__renewable_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__renewable_primacy_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__renewable_primacy_reading
 *   human_readable: Renewable Primacy Energy Policy Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The renewable primacy reading of climate mitigation legitimacy asserts
 *   that decarbonization can be achieved faster and more cheaply through
 *   rapid deployment of renewables plus storage, without requiring new
 *   nuclear capacity. This reading privileges deployment speed and cost
 *   reduction over baseload reliability assumptions. Nuclear projects are
 *   reframed as capital sinks that slow down climate action by locking
 *   resources into multi-decade build cycles. The reading coordinates rapid
 *   transition through distributed generation models and shorter innovation
 *   cycles. It extracts from nuclear-dependent utilities and capital-holders
 *   by devaluing their existing infrastructure and preventing new investment
 *   in their traditional technology. The claim/metric gap is deliberate: this
 *   is presented as a technical-economic reading (rope: coordination problem
 *   solved, parties benefit from faster transition), while the metrics
 *   describe it as substantially extractive with active suppression of
 *   alternative pathways — the engine should detect that divergence.
 *
 * KEY AGENTS:
 *   - distributed_renewable_developers — primary beneficiary, sets agenda through lobbying
 *   - energy_storage_manufacturers — direct beneficiary from storage-centric models
 *   - independent_power_producers — agenda-setter, powerful, capture policy-making
 *   - climate_advocacy_constituencies — beneficiary through speed narrative alignment
 *   - nuclear_industry_capital_holders — victim, stranded-asset risk bearer
 *   - incumbent_baseload_utilities — victim, identity-locked to centralized generation
 *   - grid_reliability_authorities — victim, carry operational risk from assumption that storage will scale
 *   - energy_systems_engineers — observers, disagreement internal to the seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_legitimacy__renewable_primacy_reading, 0.42).
domain_priors:theater_ratio(climate_mitigation_legitimacy__renewable_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewable Primacy Energy Policy Reading").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, 'dc248ede-cfe7-4ae3-9aab-a1639dafeaee').
narrative_ontology:cs_kernel_codification('dc248ede-cfe7-4ae3-9aab-a1639dafeaee', fixed_text).
narrative_ontology:cs_authority_grounding('dc248ede-cfe7-4ae3-9aab-a1639dafeaee', extraction).
narrative_ontology:cs_interpretation_layer_present('dc248ede-cfe7-4ae3-9aab-a1639dafeaee').
narrative_ontology:cs_reading_relation('dc248ede-cfe7-4ae3-9aab-a1639dafeaee', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('dc248ede-cfe7-4ae3-9aab-a1639dafeaee', climate_mitigation_legitimacy__portfolio_pragmatism_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc248ede-cfe7-4ae3-9aab-a1639dafeaee', climate_mitigation_legitimacy__degrowth_sufficiency_reading, influences).
narrative_ontology:cs_axiom('dc248ede-cfe7-4ae3-9aab-a1639dafeaee', foundational, renewables_storage_technical_sufficiency).
narrative_ontology:cs_axiom_status(renewables_storage_technical_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('dc248ede-cfe7-4ae3-9aab-a1639dafeaee', renewables_storage_technical_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('dc248ede-cfe7-4ae3-9aab-a1639dafeaee', foundational, speed_cost_advantage_over_nuclear).
narrative_ontology:cs_axiom_status(speed_cost_advantage_over_nuclear, holdable).
narrative_ontology:cs_axiom_grounding('dc248ede-cfe7-4ae3-9aab-a1639dafeaee', speed_cost_advantage_over_nuclear, empirically_contingent).
narrative_ontology:cs_reference_frame('dc248ede-cfe7-4ae3-9aab-a1639dafeaee', rapid_distributed_decarbonization).
narrative_ontology:cs_drift_state('dc248ede-cfe7-4ae3-9aab-a1639dafeaee', contemporary_storage_cost_plateau, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dc248ede-cfe7-4ae3-9aab-a1639dafeaee', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_renewable_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, energy_storage_manufacturers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, independent_power_producers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, climate_advocacy_constituencies).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry_capital_holders).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, incumbent_baseload_utilities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, grid_reliability_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from policy frameworks that prioritize renewable deployment and mandate rapid capacity build-out. Their business models assume continuous policy support for solar/wind/storage. Exit looks like relocating to jurisdictions with stronger renewable mandates or pivoting to other energy tech; competitive pressure is high but not identity-fusing.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_renewable_developers, beneficiary,
    organized, biographical, mobile, global).

% Directly benefit from the reading's core claim that storage solves variability, enabling dispatch without baseload. Their revenue depends on storage-centric grid models. Can relocate supply chains and markets; operate in competitive global markets.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, energy_storage_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Set energy policy through lobbying and regulatory engagement. Directly benefit from policies that mandate renewable procurement, subsidize storage, and disfavor new nuclear capital. Control substantial capital deployment decisions and influence legislative timing.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, independent_power_producers, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, independent_power_producers, agenda_setter).

% Benefit from the reading's alignment with rapid decarbonization narratives and localized renewable deployment. Their legitimacy depends on visible speed-to-deployment metrics. Exit is to alternative climate framings (degrowth, nuclear-inclusive); no economic dependency.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, climate_advocacy_constituencies, beneficiary,
    organized, generational, mobile, global).

% Bear the extraction as their capital commitments to multi-decade nuclear projects are deferred or cancelled under renewable-primacy policy. Their sunk capital becomes stranded. Exit requires diversifying into renewables and storage, which dilutes their competitive advantage in traditional nuclear technology.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry_capital_holders, payer,
    institutional, generational, constrained, global).

% Bear extraction through regulatory pressure to retire coal/gas plants and transition to renewables before their infrastructure capital has fully amortized. Their institutional identity is built on centralized generation; decentralized renewable models threaten their business model structure and workforce organization.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, incumbent_baseload_utilities, payer,
    institutional, generational, identity_locked, regional).

% Allocate investment based on policy signals. Renewable-primacy readings shift capital flows toward distributed generation and storage; nuclear projects face financing pressure. No direct stake in the reading's truth, only in capital allocation efficiency.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, financial_capital_markets, observer,
    institutional, biographical, arbitrage, global).

% Analyze technical feasibility and cost trajectories across scenarios. Disagree internally about whether storage plus renewables can reliably substitute for baseload at scale. Professional disagreement, not economic interest.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, energy_systems_engineers, observer,
    organized, biographical, analytical, global).

% Bear extraction through mandatory acceptance of renewable-dominant portfolios and responsibility for grid stability without traditional baseload capacity. Their mandate (maintain reliability) now operates under policy constraints that assume storage solves variability — if storage fails to scale as promised, they carry the operational risk. Can influence policy but cannot opt out of the mandate.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_reliability_authorities, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, grid_reliability_authorities, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_renewable_developers).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__renewable_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates rapid climate mitigation by privileging deployment speed and cost reduction in renewable and storage technology. Solves the timing problem: faster policy-driven build-out of renewables + storage beats slower multi-decade nuclear projects to decarbonization targets.
% TRANSFER_FUNCTION: Moves investment capital and policy support from nuclear projects toward distributed renewable and storage deployment. Transfers stranded-asset risk from utilities to nuclear-dependent firms. Transfers grid-reliability risk from centralized operators to distributed generation participants and storage providers.
% ABSENT_VOICES: Nuclear sector engineers and long-term stability advocates are excluded from the policy consensus around renewable primacy; they argue for portfolio approaches and longer time horizons. Baseload-necessity reading proponents (large institutional utilities, some reliability engineers) are marginalized in renewable-primacy jurisdictions despite holding technical expertise.
% DISAPPEARANCE_RATIONALE: If renewable-primacy policy and its enforcement disappeared, capital would flow back to nuclear projects currently shelved, utilities would retain centralized generation models longer, and the speed and geographic distribution of decarbonization would shift materially. Grid architecture, investment cycles, and technology development priorities would reorganize around different dispatch assumptions.
% FOUNDING_PROBLEM: Climate mitigation requires rapid decarbonization. Nuclear projects have 10-20 year build cycles; renewables have 1-3 year deployment cycles. Policy needed to accelerate deployment within the climate timeline.
% FOUNDING_PROBLEM_CORROBORATION: Climate advocates and renewable developers attest the founding problem is live and urgent. Nuclear proponents and baseload-necessity advocates attest that rapid deployment without reliable baseload will create grid stability problems, shifting the problem rather than solving it. Independent grid studies disagree on whether storage can truly replace baseload at continental scales; IPCC modeling shows scenarios where both readings are provisionally compatible, depending on assumptions about storage cost and deployment rate. No external corroboration from outside the interested parties resolves whether 'rapid deployment' and 'reliable decarbonization' are compatible under renewable primacy.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__renewable_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.68 because the reading systematically channels investment away from nuclear into renewables, cancels or shelves existing nuclear projects, and shifts capital risks. The extraction is neither accidental nor marginal — it is structural to the reading's core claim that nuclear is a wasteful alternative. Suppression is moderate (0.42) because the renewable-primacy reading is not defended through outright denial of nuclear engineering validity; rather, it is defended by reframing time horizons and cost comparisons in ways that systematically disadvantage nuclear projects. This is suppression-through-framing rather than suppression-through-coercion: the metrics reflect that grid-reliability authorities and nuclear proponents face narrative marginalization. Theater rises early and plateaus because performative deployment announcements (solar installations, battery projects) are common, but the core extraction (capital re-allocation from nuclear) is real and continues. The measurement series shows extraction climbing steeply in years 0-15 (the policy enforcement phase when nuclear projects are cancelled and renewable mandates intensify), then plateauing as the policy regime stabilizes and new nuclear investment redirects elsewhere. This is not extraction decay — it is saturation: the reading has captured the policy space and extracted its maximum from the incumbent firms.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter (independent power producers) and beneficiary seats, the renewable primacy reading is a coordination solution: solve climate faster, enable distributed innovation, reduce long-term costs. From the victim seats (nuclear capital holders, incumbent utilities), the same arrangement operates as extractive rent-capture: their competitive advantages are systematically devalued, their existing capital is stranded, and their engineering expertise is marginalized. From the grid-reliability authorities' seat, there is a dangerous misalignment: they are told they are coordinating a transition, but they are actually bearing the systemic risk that the reading's core assumption (storage will scale cheaply) is wrong. If storage costs remain high or deployment lags, grid operators become the fall-guy for the reading's failed premise. The engine should compute these divergences from the structural data without the authored claim prejudging the outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable developers and storage manufacturers have high directionality toward beneficiary (d ≈ 0.1-0.2): they receive direct support and policy favoritism. Climate advocates are near-beneficiary (d ≈ 0.15): they benefit from speed narrative and visibility but have no economic capture. Independent power producers are the agenda-setters (d ≈ 0.25-0.35): they capture value and influence policy but are themselves organizationally constrained by grid requirements. Nuclear capital holders are full targets (d ≈ 0.9-1.0): their asset values are systematically devalued by the reading's policy implementation. Incumbent baseload utilities are near-target but with an identity component (d ≈ 0.85): they bear economic extraction AND face organizational identity crisis (centralized generation model is delegitimized). Grid-reliability authorities sit awkwardly between roles (d ≈ 0.65-0.75): they appear to be implementing coordination (transitioning to renewables) but bear the operational risk that storage does not scale as promised, making them effectively targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem (climate mitigation speed) is live and urgent by any measure. But the reading may be experiencing mandatrophy drift: as renewable deployment accelerates and storage costs fall, the reading's core justification (faster than nuclear, cheaper than nuclear) transitions from contestable claim to observed fact. At that point, the extraction from nuclear holders becomes less a means to solve a coordination problem and more a residual wealth transfer. Policy may need to ask whether the reading continues to solve a live problem or has become a mechanism for extracting value from a defeated incumbent. This is not yet present in the data (mandatrophy_resolved stays false), but the theatrical plateau around year 20-30 suggests the reading's policy function is shifting from problem-solving to rent-maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    storage_deployment_gap,
    'Will energy storage deploy at the speed and cost required for renewable-only grids at multi-hour durations and continental scales?',
    'Empirical tracking: storage cost, deployment rate, and availability of raw materials (lithium, cobalt) over the next 10-15 years. If costs remain 2-3x current projections or deployment stalls below 30% of capacity, the assumption fails.',
    'If the gap is real, renewable-only grids face intermittency crises, grid operators become the failure-bearers, and nuclear or gas backup becomes necessary. The reading''s core justification (faster and cheaper than nuclear) collapses, turning the extraction from nuclear capital holders into a zero-sum transfer without coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(storage_deployment_gap, empirical, 'Whether storage cost and deployment trajectories match renewable-primacy assumptions.').

omega_variable(
    baseload_reliability_necessity,
    'Is reliable, dispatchable baseload capacity structurally necessary for grid stability at high renewable penetration, or can demand flexibility and storage together substitute?',
    'Grid modeling and pilot studies at 80%+ renewable penetration. If grid stability requires backup dispatchable generation or rolling blackouts occur, baseload is necessary; if stability holds, the reading is correct.',
    'If baseload is necessary, the renewable-primacy reading forecloses the baseload-necessity reading — they cannot coexist. If baseload is contingent (depends on grid design, demand management, etc.), the readings coexist with different assumptions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(baseload_reliability_necessity, conceptual, 'Whether baseload power is structurally necessary or contingent on grid design choices.').

omega_variable(
    capital_reallocation_feasibility,
    'Can nuclear and utility capital be quickly redeployed into renewable and storage businesses, or does identity-lock and expertise-mismatch create stranded capital that cannot reallocate?',
    'Track nuclear firms and utilities'' ability to pivot into renewables and storage. If pivots succeed at scale, capital reallocation is smooth; if most nuclear capital holders exit the energy sector, stranded capital is real and the reading extracts without redeployment possibility.',
    'If reallocation is easy, the extraction is temporary (victims become winners in the new regime). If reallocation is hard, the reading permanently transfers wealth from nuclear to renewable constituencies — shifting from coordination cost to wealth transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_reallocation_feasibility, empirical, 'Whether nuclear capital can reallocate to renewables or remains stranded.').

omega_variable(
    reading_framing_necessity,
    'Is the renewable-primacy framing''s exclusion of nuclear necessary to achieve rapid decarbonization, or is it contingent on political coalitions and incumbent competition?',
    'Counterfactual: if nuclear had not been politically opposed in renewable-primacy jurisdictions, could both technologies have deployed in parallel? Examine jurisdictions where both deploy (e.g., France) to see if the dichotomy is technical or political.',
    'If the exclusion is contingent on politics, the renewable-primacy reading forecloses portfolio approaches only through power dynamics, not technical necessity. If it is necessary (resources are truly zero-sum), the reading''s extraction is a side effect of solving a real constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_necessity, conceptual, 'Whether nuclear exclusion is technically necessary or politically contingent.').

omega_variable(
    kernel_reading_distinction,
    'Are the renewable-primacy, baseload-necessity, and portfolio-pragmatism readings genuinely distinct readings of one kernel (decarbonization legitimacy), or are they different constraints entirely?',
    'Examine whether the three readings agree on the referent (what arrangement is being evaluated) and differ only on the normative assessment. If they disagree on the referent itself (what counts as decarbonization, what timeline matters), they may be separate constraints rather than readings of one kernel.',
    'If they are truly sibling readings, the engine computes per-seat classifications from a shared constraint. If they are separate constraints, each should be authored independently with its own ε, victims, beneficiaries, and omegas. This omega documents the uncertainty about the kernel structure itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Whether this is one contested kernel with multiple readings or multiple distinct constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__renewable_primacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(clim_tr_t35, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement(clim_tr_t40, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(clim_be_t35, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(clim_be_t40, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 5, 0.36).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(clim_su_t35, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 35, 0.42).
narrative_ontology:measurement(clim_su_t40, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__renewable_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__renewable_primacy_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_mitigation_legitimacy kernel. The renewable-primacy reading asserts renewables plus storage can achieve full decarbonization faster and cheaper than nuclear, privileging distributed generation and shorter capital cycles. Sibling readings offer alternative legitimacy framings: baseload-necessity asserts nuclear is necessary for reliable decarbonization; portfolio-pragmatism asserts both technologies matter; degrowth-sufficiency asserts demand reduction makes both expansion unnecessary. All four readings are authored as separate constraint stories, each with its own ε, beneficiary/victim structure, and stakeholder surface. They are linked via network.affects_constraints to enable contamination propagation analysis and kernel contest tracking. The readings coexist as live policy positions held by different constituencies; decomposing the kernel into separate constraints prevents collapsing the contest into a single type classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__renewable_primacy_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
