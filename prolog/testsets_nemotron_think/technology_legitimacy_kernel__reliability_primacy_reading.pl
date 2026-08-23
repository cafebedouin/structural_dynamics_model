% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__reliability_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Dispatchable Baseload Primacy as Climate Technology Legitimacy Gate
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint story captures the 'reliability primacy' reading of the
 *   contested technology_legitimacy_kernel: the claim that only dispatchable,
 *   baseload-capable generation technologies are legitimate for climate
 *   mitigation. The constraint operates through grid codes, capacity markets,
 *   integrated resource plans, and policy frameworks (tax credits, clean
 *   energy standards) that explicitly or implicitly privilege
 *   high-capacity-factor resources. Nuclear is the primary beneficiary — its
 *   existing fleet and new-build projects receive substantial subsidies and
 *   market advantages justified by 'reliability.' Wind and solar developers
 *   are the primary victims, forced to overbuild or add storage to qualify.
 *   Ratepayers bear the cost premium. The constraint has a genuine
 *   coordination function (grid reliability is a real physical requirement)
 *   but the specific baseload-only formulation extracts rents for incumbents
 *   and delays cheaper decarbonization. Extraction has risen over 30 years as
 *   renewable penetration increased and the baseload gate was extended from
 *   'reliability metric' to 'legitimacy gate.' Theater ratio has risen as
 *   reliability rhetoric increasingly covers rent extraction. Suppression
 *   requirement has risen as renewable+storage alternatives demonstrate
 *   technical viability but face institutional exclusion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.68).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.72).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Dispatchable Baseload Primacy as Climate Technology Legitimacy Gate").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, '44dfcfba-3931-4ae1-99dc-4180e4c2fbed').
narrative_ontology:cs_kernel_codification('44dfcfba-3931-4ae1-99dc-4180e4c2fbed', formalized).
narrative_ontology:cs_authority_grounding('44dfcfba-3931-4ae1-99dc-4180e4c2fbed', expertise).
narrative_ontology:cs_interpretation_layer_present('44dfcfba-3931-4ae1-99dc-4180e4c2fbed').
narrative_ontology:cs_reading_relation('44dfcfba-3931-4ae1-99dc-4180e4c2fbed', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('44dfcfba-3931-4ae1-99dc-4180e4c2fbed', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('44dfcfba-3931-4ae1-99dc-4180e4c2fbed', foundational, grid_stability_requires_dispatchable_baseload).
narrative_ontology:cs_axiom_status(grid_stability_requires_dispatchable_baseload, holdable).
narrative_ontology:cs_axiom_grounding('44dfcfba-3931-4ae1-99dc-4180e4c2fbed', grid_stability_requires_dispatchable_baseload, empirically_contingent).
narrative_ontology:cs_axiom('44dfcfba-3931-4ae1-99dc-4180e4c2fbed', foundational, intermittent_renewables_cannot_provide_reliability_without_firm_pairing).
narrative_ontology:cs_axiom_status(intermittent_renewables_cannot_provide_reliability_without_firm_pairing, holdable).
narrative_ontology:cs_axiom_grounding('44dfcfba-3931-4ae1-99dc-4180e4c2fbed', intermittent_renewables_cannot_provide_reliability_without_firm_pairing, empirically_contingent).
narrative_ontology:cs_axiom('44dfcfba-3931-4ae1-99dc-4180e4c2fbed', secondary, capacity_factor_is_primary_reliability_metric).
narrative_ontology:cs_axiom_status(capacity_factor_is_primary_reliability_metric, holdable).
narrative_ontology:cs_axiom_grounding('44dfcfba-3931-4ae1-99dc-4180e4c2fbed', capacity_factor_is_primary_reliability_metric, conventional).
narrative_ontology:cs_reference_frame('44dfcfba-3931-4ae1-99dc-4180e4c2fbed', dispatchable_baseload_primacy_framework).
narrative_ontology:cs_drift_state('44dfcfba-3931-4ae1-99dc-4180e4c2fbed', contemporary_energy_transition, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('44dfcfba-3931-4ae1-99dc-4180e4c2fbed', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, hydro_geothermal_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, storage_industry).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, gas_ccs_proponents).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, wind_solar_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, climate_urgency_advocates).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__reliability_primacy_reading, grid_stability_requires_dispatchable_generation).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__reliability_primacy_reading, capacity_factor_as_primary_reliability_metric).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Existing nuclear fleet and new-build projects receive legitimacy premium and policy support (capacity markets, tax credits, streamlined licensing) because their high capacity factor satisfies the baseload gate. They actively shape reliability standards through regulatory engagement and grid code committees. Exit is arbitrage-grade: they can threaten closure or relocate investment to favorable jurisdictions.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_industry, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_industry, agenda_setter).

% System operators (ISOs, TSOs) define and enforce reliability standards, capacity accreditation rules, and grid codes. They benefit from the constraint because it simplifies operational planning around dispatchable resources. Their exit is constrained: they are mandated entities with legal obligations to maintain reliability, but they have discretion in how they interpret 'reliability' — a discretion they currently exercise in favor of baseload-centric metrics.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__reliability_primacy_reading, grid_operators, beneficiary).

% Variable renewable developers must either add storage (adding 30-100% to LCOE), accept reduced capacity value in capacity markets, or be excluded from 'legitimate' climate mitigation portfolios. They are organized through trade associations but lack the institutional embeddedness of grid operators. Exit is constrained: they can pivot to storage hybrid projects or jurisdictions with different legitimacy criteria, but the baseload gate shapes global finance and policy.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, wind_solar_developers, payer,
    organized, biographical, constrained, national).

% End-use electricity customers bear the cost premium of baseload requirements: capacity payments for nuclear/gas, storage integration costs, and foregone cheaper renewable generation. Organized through consumer advocates and industrial groups but trapped by geographic monopoly of distribution utilities and essential-service nature of electricity. No meaningful exit: cannot opt out of grid-supplied power or the regulatory compact that socializes these costs.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers, payer,
    organized, biographical, trapped, local).

% Battery and long-duration storage developers benefit from the constraint because it creates a mandatory pairing requirement for wind/solar to achieve 'legitimate' status. They are not the primary architects of the gate but capture significant value from it. Mobile exit: global supply chains and multiple market applications (grid, transport, behind-the-meter) give them arbitrage-grade flexibility.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, storage_industry, beneficiary,
    moderate, biographical, mobile, global).

% Fossil fuel interests with carbon capture and storage (CCS) projects claim the dispatchable baseload mantle to access climate legitimacy and policy support. They wield institutional power through existing energy infrastructure and political relationships. Arbitrage exit: they can pivot between 'bridge fuel' and 'decarbonized dispatchable' framings depending on policy winds.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, gas_ccs_proponents, beneficiary,
    institutional, generational, arbitrage, national).

% Advocates for maximum-speed decarbonization who argue the baseload gate delays deployment of the cheapest abatement (wind/solar) and risks missing carbon budgets. They are excluded from the legitimacy-defining process (grid codes, capacity markets, integrated resource plans) which is dominated by incumbents and engineers. Constrained exit: they can shift to subnational, corporate, or international venues but the baseload gate shapes the dominant policy paradigm.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_urgency_advocates, excluded,
    moderate, generational, constrained, global).

% Independent analysts, academics, and modelers who evaluate the constraint's empirical basis: whether grids genuinely require baseload, whether storage/demand-response/interconnection can substitute, and whether the gate's costs are justified by reliability gains. No material stake; exit is analytical — they can change their assessment as evidence evolves.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures electricity system reliability as variable renewable penetration increases by defining a clear, enforceable standard (dispatchable baseload capacity) that guarantees resource adequacy and operational stability.
% TRANSFER_FUNCTION: Moves financial resources from ratepayers (via higher electricity costs) and wind/solar developers (via storage mandates and reduced capacity value) to nuclear, hydro, geothermal, gas-CCS, and storage operators (via capacity payments, legitimacy premiums, and mandatory procurement). Also transfers deployment risk from incumbents to challengers.
% ABSENT_VOICES: Communities hosting nuclear waste facilities and mining operations for battery materials (excluded from reliability calculus); future generations bearing climate risk from delayed deployment (not represented in current grid planning horizons); distributed energy resource aggregators and demand-response providers (excluded by centralized capacity market designs that favor large dispatchable units).
% DISAPPEARANCE_RATIONALE: If the baseload legitimacy gate vanished overnight, capacity markets would revalue resources based on marginal reliability contribution rather than capacity factor; wind/solar deployment would accelerate without mandatory storage pairing; nuclear and gas-CCS would lose policy premiums; ratepayer costs would initially drop but reliability events might increase during transition; the entire resource adequacy paradigm would shift from 'firm capacity' to 'probabilistic reliability contribution.'
% FOUNDING_PROBLEM: Early grid integration of variable renewables (2000s-2010s) created genuine operational challenges: frequency stability, ramping requirements, and resource adequacy during extended low-wind/solar periods. Grid operators and planners needed a tractable reliability metric, and capacity factor of dispatchable plants became the default proxy.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators (NERC, regional ISOs) attest that reliability challenges are real and growing with renewable penetration. Renewable industry associations (AWEA, SEIA), storage developers, and independent analysts (NREL, IEA, academic researchers) attest that the founding problem is substantially solvable without baseload primacy — via storage, demand response, interconnection, and advanced inverters — and that the baseload gate now persists as institutional inertia and incumbent protection. Legislative testimony in multiple jurisdictions (US FERC proceedings, EU electricity market design reform, UK REMA) documents both positions.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__reliability_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the substantial cost transfer from ratepayers and renewable developers to dispatchable incumbents, measured against the marginal reliability benefit of baseload over demonstrated alternatives. Suppression (0.72) reflects active exclusion: capacity market rules that de-rate renewables, grid codes written for synchronous generators, permitting barriers for storage-as-transmission, and the legitimacy gate itself which shapes finance and policy. Theater (0.42) reflects that ~40% of enforcement activity (reliability studies, capacity accreditation proceedings, grid code revisions) performs the coordination function while the marginal reliability gain from baseload over firm renewable+storage portfolios shrinks. Accessibility collapse (0.61) reflects that alternative framings (velocity, precautionary) exist but are structurally excluded from the venues where legitimacy is adjudicated. Resistance (0.58) reflects active pushback from renewable industry, climate advocates, and some regulators — but the constraint persists because its beneficiaries control the adjudication venues.
 *
 * PERSPECTIVAL GAP:
 *   From the grid_operator seat (agenda_setter), the constraint appears as necessary coordination: they face real reliability events and the baseload metric is operationally tractable. From the wind_solar_developer and ratepayer seats (payers), the same structure operates as enforced extraction: they pay for a reliability standard that newer technologies can meet more cheaply but are not allowed to. From the nuclear_industry seat (beneficiary/agenda_setter), the constraint is both coordination (they provide a real service) and extraction (they capture rents beyond service cost). The analytical_observer sees the full structure: a genuine coordination problem (grid reliability) captured by a specific technological formulation (baseload) that benefits incumbents.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear_industry and gas_ccs_proponents are full beneficiaries (d ~0.1-0.2): they collect rents, shape rules, have arbitrage exit. Grid_operators are partial beneficiaries (d ~0.3): they gain operational simplicity but bear legal reliability obligation. Storage_industry is a secondary beneficiary (d ~0.25): they capture mandated demand but didn't design the gate. Wind_solar_developers are targets (d ~0.8): they pay storage penalty or face de-rating, constrained exit. Ratepayers are full targets (d ~0.95): trapped, essential service, no exit. Climate_urgency_advocates are excluded (d not computed): not in the adjudication venue. The directionality derivation from beneficiary/victim + exit + power produces this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (early renewable integration challenges) was live and the baseload gate was a genuine coordination response. But the problem has evolved: storage, demand response, interconnection, and advanced inverters now demonstrate reliability contribution without baseload. The constraint persists because the adjudication venues (grid codes, capacity markets) are controlled by beneficiaries (incumbents, grid operators) who face no competitive pressure to update the metric. This is mandatrophy: the mandate (ensure reliability) has outlived the specific means (baseload primacy) but the means persists because the authority structure extracts benefit from preventing revision. The classification (tangled_rope, not snare) correctly captures that coordination function remains real — grids do need reliability — but the specific formulation now extracts asymmetrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (reliability_primacy) of the contested technology_legitimacy_kernel. Sibling readings are velocity_primacy_reading and precautionary_reading. What structural elements do the readings disagree on, and does any reading foreclose another?',
    'Map each reading''s beneficiary/victim sets, extractiveness referents, and coordination functions. If beneficiary sets are disjoint and coordination functions contradictory, readings may foreclose. If they address different failure modes (reliability vs speed vs irreversibility), they likely coexist as competing legitimacy frameworks.',
    'If readings foreclose, only one can be institutionalized in a given jurisdiction — the kernel resolves to a single constraint. If they coexist, multiple legitimacy gates operate simultaneously, creating policy incoherence and higher aggregate extraction. The engine''s cs_structure reading_relations and axioms capture this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural relationship between sibling readings of the technology legitimacy kernel').

omega_variable(
    baseload_necessity_empirical,
    'Does grid stability genuinely require dispatchable baseload generation, or can reliability be achieved at lower cost through renewable+storage+demand-response+interconnection portfolios?',
    'Empirical evidence from high-renewable grids (CAISO, ERCOT, South Australia, Germany): frequency stability events, loss-of-load probability studies, capacity value of storage, cost of firm renewable portfolios vs nuclear/gas. If firm renewable portfolios achieve equivalent reliability at lower LCOE, the baseload necessity claim is falsified and the constraint''s coordination function is separable from its extraction.',
    'If baseload is not necessary, the constraint''s coordination function is a cover for extraction — reclassification toward snare. If baseload is necessary for deep decarbonization (e.g., seasonal storage gap), the tangled_rope classification holds but the extraction magnitude depends on whether incumbents capture rents above service cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_necessity_empirical, empirical, 'Whether the baseload gate''s coordination function is empirically necessary or a contingent institutional choice').

omega_variable(
    nuclear_rent_capture,
    'How much of the nuclear industry''s policy premium (capacity payments, tax credits, streamlined licensing, liability caps) reflects genuine reliability service cost versus monopoly rent captured through the baseload legitimacy gate?',
    'Cost-of-service analysis comparing nuclear''s marginal reliability contribution to its total policy-supported revenue. Regulatory discovery of actual costs vs. market revenues. Counterfactual: what would nuclear earn in a technology-neutral reliability market?',
    'If rent capture is high (>50% of premium), the constraint''s extraction is dominantly incumbent protection — strengthens snare character. If rent capture is low, the tangled_rope coordination function dominates. Also determines whether the nuclear_industry stakeholder is primarily beneficiary or agenda_setter.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_rent_capture, empirical, 'Magnitude of rent capture by nuclear industry through the baseload legitimacy gate').

omega_variable(
    suppression_mechanism_ratepayers,
    'Is ratepayers'' trapped position structural (geographic monopoly, essential service) or internalized (acceptance of ''reliability requires baseload'' narrative)?',
    'Post-exit suppression trajectory: if ratepayers in jurisdictions with retail choice or community choice aggregation still bear baseload costs (via socialized transmission, capacity markets), suppression is structural. If they could exit but don''t because they believe the narrative, internalized component exists.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than structural measures suggest — ratepayers carry the legitimacy gate with them even when structural exit options exist. This affects the directionality derivation for the ratepayer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ratepayers, empirical, 'Structural vs internalized suppression of ratepayers under the baseload legitimacy gate').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tlrrp_tr_t0, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(tlrrp_tr_t5, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(tlrrp_tr_t10, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(tlrrp_tr_t15, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement(tlrrp_tr_t20, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(tlrrp_tr_t25, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(tlrrp_tr_t30, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(tlrrp_be_t0, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tlrrp_be_t5, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(tlrrp_be_t10, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(tlrrp_be_t15, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(tlrrp_be_t20, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(tlrrp_be_t25, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(tlrrp_be_t30, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tlrrp_su_t0, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(tlrrp_su_t5, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(tlrrp_su_t10, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(tlrrp_su_t15, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(tlrrp_su_t20, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(tlrrp_su_t25, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(tlrrp_su_t30, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__reliability_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__reliability_primacy_reading, 0.12).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, capacity_market_design).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, renewable_integration_standards).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_subsidy_programs).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, clean_energy_standard_definitions).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, grid_code_reliability_metrics).

% DUAL FORMULATION NOTE:
% This constraint (reliability_primacy_reading) is one of three readings of the technology_legitimacy_kernel. The velocity_primacy_reading (deployment speed gate) and precautionary_reading (bounded failure modes gate) are sibling constraints with different beneficiary/victim structures and ε values. All three share the kernel but instantiate different constraints. This reading's high extractiveness (0.68) and tangled_rope classification contrast with velocity_reading's likely lower extractiveness (wind/solar beneficiaries, different victims) and precautionary_reading's distinct victim set (future generations, waste-hosting communities). The kernel decomposes because the label 'legitimate climate technology' conflates structurally distinct legitimacy criteria.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_legitimacy_kernel__reliability_primacy_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
