% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__baseload_necessity_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__baseload_necessity_reading
 *   human_readable: Baseload Necessity Constraint in Climate Mitigation
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint is the baseload_necessity_reading of the contested
 *   climate_mitigation_legitimacy kernel. It instantiates the claim that
 *   reliable decarbonization requires dispatchable baseload power which
 *   renewables cannot provide at scale. As a commitment-system constraint, it
 *   formalizes grid reliability as a policy-technical kernel interpreted by
 *   centralized authorities to mandate nuclear and incumbent baseload assets,
 *   while classifying renewable-only and distributed pathways as inadequate.
 *   The structural delta for this reading is that nuclear enters the
 *   beneficiary set as necessary infrastructure, renewable-only pathways are
 *   classified as inadequate, and high capital concentration flows into
 *   long-lived centralized assets. Sibling readings include
 *   renewable_primacy_reading, portfolio_pragmatism_reading, and
 *   degrowth_sufficiency_reading. This story treats the baseload necessity
 *   claim as a single clean constraint with its own epsilon and stakeholder
 *   surface, per the epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - centralized_grid_authorities (agenda_setter, institutional/constrained) â administers reliability standards that encode baseload as axiomatic
 *   - nuclear_industry (beneficiary, powerful/constrained) â accrues policy preference, subsidies, and rate-based capital
 *   - incumbent_baseload_operators (beneficiary, powerful/constrained) â gains capacity payments and extended asset life
 *   - renewable_energy_developers (payer, moderate/constrained) â faces procurement caps and policy ceilings
 *   - distributed_energy_communities (payer, moderate/constrained) â excluded from baseload-mandated procurement
 *   - ratepayers (payer, powerless/trapped) â bear concentrated capital risk through rate-basing
 *   - climate_policy_analysts (observer, analytical) â contests empirical basis of baseload necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.62).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Necessity Constraint in Climate Mitigation").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, 'd6d97755-73a6-4d28-93f9-0c6de3d184dd').
narrative_ontology:cs_kernel_codification('d6d97755-73a6-4d28-93f9-0c6de3d184dd', formalized).
narrative_ontology:cs_authority_grounding('d6d97755-73a6-4d28-93f9-0c6de3d184dd', expertise).
narrative_ontology:cs_interpretation_layer_present('d6d97755-73a6-4d28-93f9-0c6de3d184dd').
narrative_ontology:cs_reading_relation('d6d97755-73a6-4d28-93f9-0c6de3d184dd', climate_mitigation_legitimacy__renewable_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('d6d97755-73a6-4d28-93f9-0c6de3d184dd', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('d6d97755-73a6-4d28-93f9-0c6de3d184dd', climate_mitigation_legitimacy__degrowth_sufficiency_reading, influences).
narrative_ontology:cs_axiom('d6d97755-73a6-4d28-93f9-0c6de3d184dd', foundational, dispatchable_baseload_physical_necessity).
narrative_ontology:cs_axiom_status(dispatchable_baseload_physical_necessity, holdable).
narrative_ontology:cs_axiom_grounding('d6d97755-73a6-4d28-93f9-0c6de3d184dd', dispatchable_baseload_physical_necessity, empirically_contingent).
narrative_ontology:cs_axiom('d6d97755-73a6-4d28-93f9-0c6de3d184dd', foundational, renewable_intermittency_inadequacy).
narrative_ontology:cs_axiom_status(renewable_intermittency_inadequacy, holdable).
narrative_ontology:cs_axiom_grounding('d6d97755-73a6-4d28-93f9-0c6de3d184dd', renewable_intermittency_inadequacy, empirically_contingent).
narrative_ontology:cs_reference_frame('d6d97755-73a6-4d28-93f9-0c6de3d184dd', dispatchable_reliability_supremacy).
narrative_ontology:cs_drift_state('d6d97755-73a6-4d28-93f9-0c6de3d184dd', post_storage_cost_revolution, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d6d97755-73a6-4d28-93f9-0c6de3d184dd', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_baseload_operators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_energy_communities).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, ratepayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers reliability standards and resource adequacy frameworks that codify dispatchable baseload as physically necessary for grid stability. Their institutional authority and regulatory mandate depend on maintaining the centralized reliability narrative and excluding renewable-only adequacy models from formal accreditation.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, centralized_grid_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Receives policy preference, direct subsidies, loan guarantees, and streamlined permitting framed as climate necessity. Captures long-term contracts and rate-based capital allocations for new and existing nuclear assets under the baseload mandate, with revenue streams secured by the constraint's exclusion of renewable-only pathways.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_industry, beneficiary,
    powerful, biographical, constrained, global).

% Owns existing dispatchable fossil and nuclear fleets that gain extended operational life, capacity-market premiums, and must-run status when baseload is treated as non-substitutable. Benefits from the structural devaluation of intermittent generation in resource adequacy frameworks.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, incumbent_baseload_operators, beneficiary,
    powerful, biographical, constrained, national).

% Develops wind, solar, and battery storage projects that face procurement caps, curtailment orders, and policy ceilings because intermittency is framed as incompatible with the baseload reliability requirement. Market access is contingent on accepting supplementary status to dispatchable assets.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_developers, payer,
    moderate, biographical, constrained, national).

% Operates rooftop solar, microgrids, and prosumer networks classified as supplementary rather than primary. Excluded from baseload-mandated procurement streams and resource adequacy credits, their decentralized model is treated as inadequate for reliability regardless of local performance.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, distributed_energy_communities, payer,
    moderate, biographical, constrained, regional).

% Bear the concentrated capital risk of long-lived baseload assets through utility rate-basing and cost-recovery mechanisms. Lack practical exit from the centralized grid and its associated cost structure, absorbing the financial overhang of nuclear construction and stranded-asset risk.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, ratepayers, payer,
    powerless, biographical, trapped, national).

% Evaluates whether baseload necessity is empirically grounded or incumbent protection. Produces contested modeling of high-renewable viability versus baseload-dependent pathways; findings are selectively adopted or rejected by grid authorities depending on institutional alignment.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_policy_analysts, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__baseload_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures continuous electricity supply by mandating dispatchable generation capacity that can be called on demand, preventing grid instability and blackouts during periods of low renewable output.
% TRANSFER_FUNCTION: Moves capital allocation, policy preference, and market share from renewable and distributed energy pathways toward nuclear and incumbent baseload assets; transfers investment risk from asset owners to captive ratepayers through guaranteed cost recovery and rate-basing.
% ABSENT_VOICES: Renewable primacy advocates who model 100% renewable-plus-storage systems, and distributed-energy proponents who argue for prosumer-based reliability, are present in public discourse but structurally excluded from resource adequacy decision tables where baseload necessity is treated as axiomatic.
% DISAPPEARANCE_RATIONALE: If the baseload necessity constraint vanished, procurement frameworks would shift to portfolio-neutral or renewable-first auctions, capital would reallocate toward storage and grid flexibility, and incumbent baseload operators would face accelerated stranded-asset risk. The grid architecture would reorganize around intermittency management rather than dispatchable supremacy.
% FOUNDING_PROBLEM: Variable renewable energy cannot guarantee continuous electricity supply without massive storage or backup, creating a risk of blackouts and grid instability during extended low-generation periods.
% FOUNDING_PROBLEM_CORROBORATION: Grid authorities and incumbent operators assert the founding problem remains live. Independent renewable-integration studies from university research groups and national labs contest the severity and the baseload-necessity remedy. No party entirely outside the benefiting factions uncontestedly attests the problem; the corroboration record is split along institutional lines.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint channels capital into long-lived baseload assets with guaranteed cost recovery, decoupled from competitive market tests. Suppression (0.62) reflects the active exclusion of renewable-only adequacy models from regulatory accreditation and the marginalization of distributed alternatives. Theater ratio (0.48) has risen over the interval as the reliability justification has become increasingly performative relative to the improving empirical viability of storage and demand response. Accessibility collapse (0.75) is high because once the baseload frame is accepted, renewable-only pathways collapse as legitimate alternatives within institutional planning. Resistance (0.58) is moderate-to-high from renewable developers, climate advocates, and competing modelers. The measurement series share a single time grid to prevent misaligned temporal substitution.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (grid authorities) experiences the constraint as legitimate coordination required for civilizational reliability, while the payer seats (renewable developers and ratepayers) experience the same structure as enforced extraction that protects incumbent assets. The beneficiary seats (nuclear industry, incumbent operators) see necessary climate infrastructure; the excluded distributed-energy communities see a false summit that presents incumbent preference as physical law. The engine computes this divergence from the structural data rather than the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear industry and incumbent baseload operators are structural beneficiaries (low d, subsidized by the constraint). Centralized grid authorities sit near the beneficiary end through institutional authority amplification, though they do not collect direct rents. Renewable energy developers, distributed communities, and ratepayers are structural targets (high d): they bear the costs of capital concentration, market exclusion, and rate-basing. Ratepayers are particularly trapped because they lack grid exit and must absorb the cost overhang. Climate policy analysts occupy the analytical seat with neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not a pure snare because the grid reliability coordination function is genuine â blackouts are real and baseload does provide dispatchability. It is not a pure rope because the arrangement asymmetrically extracts from renewable pathways and ratepayers to benefit incumbent baseload owners. The Tangled Rope classification captures both the real coordination problem (preventing instability) and the asymmetric extraction (capital concentration, risk socialization, exclusion of alternatives). If the founding problem of renewable intermittency were fully solved by storage and the constraint persisted, it would drift toward Piton or Snare; the temporal measurements show extraction and theater rising, indicating ongoing drift toward the extraction pole.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    renewable_baseload_feasibility_boundary,
    'Can a combination of storage, demand response, and geographic aggregation fully substitute for dispatchable baseload at scale, or is there a physical floor to renewable reliability?',
    'Empirical demonstration at national scale (e.g., sustained 100% renewable weeks or months in large grids) or systematic failure of such demonstrations with documented instability.',
    'If renewables prove sufficient, the baseload necessity constraint is revealed as constructed extraction protecting incumbents; if they fail, the coordination function is validated and extraction recast as necessary cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_baseload_feasibility_boundary, empirical, 'Whether renewable intermittency is a transient or permanent barrier to full decarbonization.').

omega_variable(
    capital_risk_allocation_ambiguity,
    'Is the rate-basing of long-lived nuclear capital a necessary cost of reliable decarbonization, or a transfer of investment risk from shareholders to captive ratepayers?',
    'Comparative analysis of cost-of-capital and risk allocation in merchant nuclear versus public-utility nuclear projects, relative to renewable-plus-storage financing.',
    'If risk is systematically socialized while returns are privatized, extraction dominates coordination; if risk and return are symmetric, the constraint leans toward genuine infrastructure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_risk_allocation_ambiguity, empirical, 'Whether capital concentration represents necessary infrastructure cost or asymmetric risk transfer.').

omega_variable(
    kernel_reading_containment,
    'Does the baseload necessity reading represent an irreducible empirical disagreement about grid physics, or a commitment-system rivalry over whose infrastructure receives capital?',
    'Track whether new empirical evidence (e.g., grid-scale storage viability) shifts adherence patterns, or whether adherence correlates with institutional position and capital exposure.',
    'If adherence is institutionally locked regardless of evidence, the constraint is extraction-governed; if evidence-responsive, it functions more like a falsifiable empirical hypothesis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_containment, conceptual, 'Whether this reading is driven by physics or by capital-commitment rivalry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 16, 0.44).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_mitigation_legitimacy kernel. The baseload_necessity reading decomposes from the colloquial 'climate mitigation requires baseload' claim by treating the baseload requirement as a specific policy-technical constraint with distinct beneficiaries and victim structure. Sibling readings instantiate different empirical and normative commitments from the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
