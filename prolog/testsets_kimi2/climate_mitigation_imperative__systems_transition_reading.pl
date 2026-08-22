% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__systems_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__systems_transition_reading, []).

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
 *   constraint_id: climate_mitigation_imperative__systems_transition_reading
 *   human_readable: Systems Transition Reading of Climate Mitigation Imperative
 *   domain: energy_policy/climate_governance
 *
 * SUMMARY:
 *   This constraint story models the systems_transition_reading of the
 *   climate_mitigation_imperative kernel: the claim that climate mitigation
 *   requires structural transformation toward decentralized, democratically
 *   controlled energy systems, and that nuclear power is incompatible with
 *   this transition because it perpetuates extractive centralization. The
 *   constraint operates through policy frameworks, sustainable finance
 *   taxonomies, and grid planning rules that preference distributed
 *   renewables and exclude nuclear from low-carbon eligibility. It is
 *   authored as a Tangled Rope because it combines a genuine coordination
 *   function (democratizing energy ownership, accelerating distributed
 *   deployment) with asymmetric extraction (stranded costs imposed on nuclear
 *   operators and centralized utilities). The metrics and claimed type are
 *   independently authored: the reading itself presents the constraint as
 *   corrective justice, while the metrics capture the extractive pressure on
 *   excluded incumbents.
 *
 * KEY AGENTS:
 *   - distributed_renewables_sector: Primary beneficiary (organized/constrained) â receives preferential policy and capital flows
 *   - nuclear_operators: Primary payer (powerful/constrained) â bears exclusion costs and stranded assets
 *   - energy_democracy_movements: Agenda setter (organized/mobile) â sets the normative framework linking mitigation to democratic control
 *   - centralized_utilities: Secondary payer (institutional/constrained) â faces disaggregation and obsolescence pressure
 *   - national_energy_planners: Agenda setter (institutional/analytical) â codifies the imperative into law and planning
 *   - frontline_communities: Intended beneficiary (powerless/trapped) â targeted by democratic ownership rhetoric but often capital-constrained
 *   - nuclear_workforce_communities: Excluded voice (moderate/trapped) â absent from transition planning
 *   - independent_energy_analysts: Analytical observer (analytical/analytical) â evaluates pathway effectiveness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.65).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Systems Transition Reading of Climate Mitigation Imperative").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy_policy/climate_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, '6312c017-be1e-470f-9e73-dec65a6c763a').
narrative_ontology:cs_kernel_codification('6312c017-be1e-470f-9e73-dec65a6c763a', distributed).
narrative_ontology:cs_authority_grounding('6312c017-be1e-470f-9e73-dec65a6c763a', distributed).
narrative_ontology:cs_reading_relation('6312c017-be1e-470f-9e73-dec65a6c763a', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('6312c017-be1e-470f-9e73-dec65a6c763a', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_axiom('6312c017-be1e-470f-9e73-dec65a6c763a', foundational, democratic_energy_transition_imperative).
narrative_ontology:cs_axiom_status(democratic_energy_transition_imperative, holdable).
narrative_ontology:cs_axiom_grounding('6312c017-be1e-470f-9e73-dec65a6c763a', democratic_energy_transition_imperative, deontological).
narrative_ontology:cs_axiom('6312c017-be1e-470f-9e73-dec65a6c763a', foundational, nuclear_extractive_centralization).
narrative_ontology:cs_axiom_status(nuclear_extractive_centralization, holdable).
narrative_ontology:cs_axiom_grounding('6312c017-be1e-470f-9e73-dec65a6c763a', nuclear_extractive_centralization, empirically_contingent).
narrative_ontology:cs_reference_frame('6312c017-be1e-470f-9e73-dec65a6c763a', decentralized_democratic_energy).
narrative_ontology:cs_drift_state('6312c017-be1e-470f-9e73-dec65a6c763a', contemporary_climate_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6312c017-be1e-470f-9e73-dec65a6c763a', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, distributed_renewables_sector).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, frontline_communities).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, nuclear_operators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, centralized_utilities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives preferential grid access, climate finance, and procurement mandates under transition frameworks that privilege distributed generation. Market expansion depends on policy exclusions of centralized alternatives and preferential interconnection rules for distributed assets.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_renewables_sector, beneficiary,
    organized, biographical, constrained, global).

% Own and operate low-carbon baseload generation that is structurally excluded from green taxonomies and transition planning. Face stranded asset risk, denial of sustainability-linked finance, and regulatory phase-out justified by the incompatibility of nuclear with democratic energy goals.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_operators, payer,
    powerful, generational, constrained, national).

% Coordinate grassroots and policy advocacy linking climate mitigation to community ownership and decentralized governance. Their legitimacy depends on maintaining that centralized corporate energy models are structurally extractive and incompatible with justice.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, energy_democracy_movements, agenda_setter,
    organized, generational, mobile, global).

% Operate transmission and centralized generation infrastructure now classified as incompatible with democratic transition. Required to accept distributed interconnection on preferential terms, absorb grid modernization costs, and disaggregate vertically integrated structures.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, centralized_utilities, payer,
    institutional, generational, constrained, national).

% Named as the intended beneficiaries of democratic energy transition and local ownership programs. In practice, many lack capital to participate in distributed ownership and remain ratepayers to utilities, receiving the rhetoric of democracy without its structural preconditions.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, frontline_communities, beneficiary,
    powerless, biographical, trapped, local).

% Codify the systems transition framing into law, grid codes, and national climate plans. Enforce technology-specific procurement rules and sustainable finance taxonomies that operationalize nuclear exclusion and distributed preference.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, national_energy_planners, agenda_setter,
    institutional, generational, analytical, national).

% Communities where nuclear facilities are primary employers and taxpayers. They would object to plant closures and exclusion from transition planning but are not represented in energy democracy fora that assume nuclear phase-out as a prerequisite.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_workforce_communities, excluded,
    moderate, biographical, trapped, local).

% Assess whether decentralized renewable pathways achieve mitigation targets cost-effectively and reliably. Their findings are contested by movement advocates when they suggest centralized low-carbon sources may reduce emissions faster.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, independent_energy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate rapid decarbonization by aligning investment, grid planning, and ownership structures around geographically distributed renewable energy that can be governed by local communities rather than centralized corporations.
% TRANSFER_FUNCTION: Moves public and private investment capital, grid priority access, and policy legitimacy from centralized generation (especially nuclear and fossil) toward distributed solar, wind, and storage; moves compliance costs and stranded asset burdens onto centralized utilities and nuclear operators.
% ABSENT_VOICES: Nuclear workforce communities and incumbent baseload operators are largely excluded from transition planning; consumer voices in regions where distributed generation is technically suboptimal are underrepresented in energy democracy fora.
% DISAPPEARANCE_RATIONALE: If the imperative vanished, distributed renewables would lose preferential policy and financing frameworks, centralized utilities and nuclear would regain planning legitimacy, grid architectures would revert to unidirectional centralized design, and the coalition of energy democracy movements would lose its organizing framework.
% FOUNDING_PROBLEM: Climate change driven by fossil fuel combustion requires rapid decarbonization of energy systems, but early climate policy was captured by centralized corporate interests and technocratic solutions that failed to reduce emissions equitably or rapidly enough.
% FOUNDING_PROBLEM_CORROBORATION: Energy democracy movements and critical climate scholars attest to the founding problem from outside the benefiting renewable sector; mainstream IPCC and IEA assessments do not frame the problem as corporate capture, though they increasingly acknowledge equity dimensions. Nuclear operators and centralized utilities contest the founding problem narrative entirely.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__systems_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__systems_transition_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__systems_transition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__systems_transition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) is high because the constraint redirects capital, grid priority, and policy legitimacy from nuclear and centralized utilities toward distributed renewables, imposing significant stranded asset and compliance costs. Suppression (0.65) is high because the framework requires active enforcement: nuclear must be excluded from green taxonomies, centralized procurement must be disallowed, and distributed generation must receive preferential interconnection. Theater ratio (0.48) is moderate-high because the rhetoric of democratic control and community ownership increasingly exceeds the actual rate of structural democratization. Accessibility collapse (0.50) is moderate: nuclear remains technically viable but is policy-blocked as an alternative. Resistance (0.55) reflects sustained opposition from nuclear operators, utilities, and labor. Temporal measurements show monotonic increases across all metrics, indicating the constraint has hardened from aspirational framing into enforceable structure over the interval.
 *
 * PERSPECTIVAL GAP:
 *   Agenda-setter seats (energy_democracy_movements, national_energy_planners) experience the constraint as necessary coordination for climate justice: it aligns investment with equity and prevents corporate capture of the transition. Payer seats (nuclear_operators, centralized_utilities) experience the same structure as extractive industrial policy that destroys viable low-carbon infrastructure and efficient grid architecture. Frontline_communities experience a gap between beneficiary rhetoric and actual ownership capacity. The engine computes this divergence from the structural data rather than the narrative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation assigns low d to distributed_renewables_sector and frontline_communities because they are declared beneficiaries with constrained but not trapped exit. Nuclear_operators and centralized_utilities are declared victims (payers) with constrained exit, yielding high d. Energy_democracy_movements are agenda setters with mobile exit, sitting near the beneficiary end because the constraint's persistence advances their governance vision. National_energy_planners are institutional agenda setters with analytical exit. No overrides are required because the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination and asymmetric extraction. The coordination function is real: distributed renewables deployment does solve collective-action problems in grid decarbonization and can enable local ownership. The extraction is real: nuclear operators bear costs unrelated to emissions performance, and centralized utilities face forced disaggregation. If the coordination function were absent, the constraint would be a snare using climate policy to destroy incumbents. If the extraction were absent, it would be a rope coordinating distributed deployment without cost to excluded parties. The rising theater_ratio suggests the coordination function may be partially atrophying into performance, but active deployment numbers keep it from piton status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    systems_transition_kernel_contest,
    'Is the incompatibility between nuclear energy and democratic energy transition an empirical structural fact or a normative commitment of this reading?',
    'Comparative institutional analysis of publicly governed nuclear programs versus distributed renewable governance models; historical cases of democratic control in centralized grids.',
    'If nuclear can be governed democratically, the victim structure narrows to specific extractive utilities rather than the technology itself, potentially reclassifying nuclear operators from payer to coordinated party.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systems_transition_kernel_contest, conceptual, 'Whether nuclear exclusion is empirical or normative in the systems transition reading').

omega_variable(
    decentralization_coordination_or_cover,
    'Does the decentralization imperative primarily solve a collective-action problem in energy transition, or does it function as a distributional weapon against incumbent centralized generators?',
    'Outcome analysis comparing emissions reductions and cost curves between centralized-low-carbon and distributed-low-carbon jurisdictions with matched renewable potential.',
    'If decentralization is primarily coordination, the constraint reads more rope-like for renewable adopters; if primarily distributional against incumbents, extraction dominates and the constraint approaches snare-like asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralization_coordination_or_cover, empirical, 'Whether decentralization is coordination mechanism or distributional weapon').

omega_variable(
    nuclear_exclusion_mechanism,
    'Is nuclear exclusion driven by structural policy barriers or by identity-locked ideological commitment within the energy democracy movement?',
    'Policy trace analysis: are nuclear projects excluded by explicit legal prohibitions, or by preferential grid codes and subsidy structures that make nuclear uneconomic without formal bans?',
    'Explicit prohibition indicates higher suppression; structural economic exclusion indicates lower suppression but higher accessibility_collapse for nuclear as an alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_exclusion_mechanism, empirical, 'Structural versus ideological mechanism of nuclear exclusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmi_str_tr_t0, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cmi_str_tr_t5, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(cmi_str_tr_t10, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(cmi_str_tr_t15, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(cmi_str_tr_t20, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(cmi_str_tr_t25, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(cmi_str_tr_t30, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(cmi_str_be_t0, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cmi_str_be_t5, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(cmi_str_be_t10, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(cmi_str_be_t15, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(cmi_str_be_t20, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(cmi_str_be_t25, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(cmi_str_be_t30, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cmi_str_su_t0, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cmi_str_su_t5, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(cmi_str_su_t10, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(cmi_str_su_t15, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(cmi_str_su_t20, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(cmi_str_su_t25, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(cmi_str_su_t30, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__systems_transition_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, opportunity_cost_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_mitigation_imperative kernel, decomposed per the Îµ-invariance principle because the sibling readings instantiate structurally distinct constraints with different beneficiary/victim structures and Îµ values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
