% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__renewable_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: climate_mitigation_legitimacy__renewable_primacy_reading
 *   human_readable: Renewable Primacy in Decarbonization Strategy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'renewable primacy' reading of climate
 *   mitigation strategy, asserting that renewables plus storage can achieve
 *   full decarbonization faster and cheaper than nuclear. It is one reading
 *   of the broader 'climate_mitigation_legitimacy' kernel. This reading
 *   structurally disadvantages nuclear power and fossil fuels by diverting
 *   capital and policy support towards intermittent renewables and
 *   distributed generation, based on claims of superior speed and
 *   cost-effectiveness. The constraint is classified as a Tangled Rope
 *   because it genuinely coordinates investment towards decarbonization (a
 *   collective action problem) but does so by extracting from and suppressing
 *   alternatives (nuclear, fossil fuels) through a specific narrative and
 *   policy framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, 0.65).
domain_priors:suppression_score(climate_mitigation_legitimacy__renewable_primacy_reading, 0.7).
domain_priors:theater_ratio(climate_mitigation_legitimacy__renewable_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewable Primacy in Decarbonization Strategy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, 'd0290309-3365-407b-b4eb-c3d80884e8db').
narrative_ontology:cs_kernel_codification('d0290309-3365-407b-b4eb-c3d80884e8db', distributed).
narrative_ontology:cs_authority_grounding('d0290309-3365-407b-b4eb-c3d80884e8db', practice).
narrative_ontology:cs_interpretation_layer_present('d0290309-3365-407b-b4eb-c3d80884e8db').
narrative_ontology:cs_reading_relation('d0290309-3365-407b-b4eb-c3d80884e8db', climate_mitigation_legitimacy__baseload_necessity_reading, influences).
narrative_ontology:cs_reading_relation('d0290309-3365-407b-b4eb-c3d80884e8db', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('d0290309-3365-407b-b4eb-c3d80884e8db', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('d0290309-3365-407b-b4eb-c3d80884e8db', foundational, renewables_are_fastest_cheapest_decarbonization).
narrative_ontology:cs_axiom_status(renewables_are_fastest_cheapest_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('d0290309-3365-407b-b4eb-c3d80884e8db', renewables_are_fastest_cheapest_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('d0290309-3365-407b-b4eb-c3d80884e8db', secondary, nuclear_is_too_slow_expensive_risky).
narrative_ontology:cs_axiom_status(nuclear_is_too_slow_expensive_risky, holdable).
narrative_ontology:cs_axiom_grounding('d0290309-3365-407b-b4eb-c3d80884e8db', nuclear_is_too_slow_expensive_risky, empirically_contingent).
narrative_ontology:cs_reference_frame('d0290309-3365-407b-b4eb-c3d80884e8db', rapid_cost_effective_decarbonization_via_renewables).
narrative_ontology:cs_drift_state('d0290309-3365-407b-b4eb-c3d80884e8db', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d0290309-3365-407b-b4eb-c3d80884e8db', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_energy_proponents).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, fossil_fuel_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, grid_operators_with_legacy_infrastructure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, energy_consumers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, energy_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from policies prioritizing renewable deployment and associated grid upgrades. They advocate for market mechanisms and regulatory frameworks that favor intermittent generation with storage over other forms of decarbonization.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_developers, beneficiary,
    organized, biographical, mobile, global).

% Advocate for policies that decentralize energy generation and favor local grids, seeing this as a faster and more resilient path to decarbonization. They benefit from reduced regulatory hurdles for small-scale projects.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_energy_proponents, beneficiary,
    moderate, biographical, constrained, local).

% Bears the cost of reduced investment, regulatory barriers, and public perception challenges when policy prioritizes renewables. They argue for nuclear's role as a reliable, carbon-free baseload source, but face a narrative that frames their technology as too slow and expensive.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry, payer,
    institutional, generational, constrained, national).

% Faces accelerated phase-out pressure and reduced investment in new projects as renewable primacy gains policy traction. This reading directly undermines their long-term viability.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, fossil_fuel_industry, payer,
    institutional, generational, constrained, global).

% Incur significant costs for grid modernization and adaptation to handle intermittent renewable sources, often without commensurate revenue increases. They are caught between decarbonization mandates and the technical challenges of integrating a high proportion of renewables.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_operators_with_legacy_infrastructure, payer,
    institutional, biographical, constrained, national).

% Actively promote the narrative of renewable primacy, influencing public opinion and policy. They set the agenda for what constitutes 'fast' and 'cheap' decarbonization, often framing nuclear as a distraction or a false solution.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, climate_activists_and_advocates, agenda_setter,
    organized, generational, mobile, global).

% Benefit from potentially lower long-term energy costs and reduced environmental impact. However, they may bear short-term costs of grid transition and potential reliability issues if the transition is poorly managed.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, energy_consumers, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, energy_consumers, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates investment and policy towards a specific technological pathway (renewables + storage) for climate mitigation, aiming to accelerate decarbonization by focusing resources.
% TRANSFER_FUNCTION: Transfers capital and political support from nuclear and fossil fuel sectors towards renewable energy development and associated infrastructure, driven by the claim of superior speed and cost-effectiveness.
% ABSENT_VOICES: Proponents of advanced nuclear technologies, who argue their solutions are being unfairly excluded from the 'fast and cheap' narrative. They would highlight the land use, intermittency, and material intensity challenges of an all-renewable grid.
% DISAPPEARANCE_RATIONALE: If the 'renewables primacy' narrative vanished, energy policy would immediately become more technology-agnostic. Investment would flow back into nuclear research and deployment, and the political landscape for climate mitigation would shift to a portfolio approach, fundamentally altering current energy transition plans.
% FOUNDING_PROBLEM: The urgent need for rapid and cost-effective decarbonization to address climate change, coupled with a desire to avoid the perceived risks and long lead times of nuclear power.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and economists outside the renewable industry corroborate the urgency of decarbonization. However, the 'faster and cheaper' claim for renewables-only is contested by some energy system modelers and nuclear advocates, who point to specific grid stability challenges and material requirements.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_legitimacy__renewable_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) reflects the opportunity costs and direct financial disadvantages imposed on non-renewable technologies, particularly nuclear, which are excluded from 'fast and cheap' narratives. Suppression (0.70) is high due to the active political and media campaigns that frame nuclear as an undesirable or unfeasible option, limiting its policy space and public acceptance. Theater ratio (0.20) is moderate; while there's genuine effort towards decarbonization, some of the 'faster and cheaper' claims serve to justify the exclusion of alternatives rather than purely reflecting objective analysis. The metrics show a rising trend in extractiveness and suppression as this narrative gains dominance and policy implementation hardens.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of renewable proponents, this is a Rope, coordinating efficient decarbonization. From the nuclear industry's perspective, it's a Snare, actively suppressing a viable alternative. The engine's classification as Tangled Rope reflects the hybrid nature: a genuine coordination function for climate mitigation, but with significant asymmetric extraction and suppression of alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable energy developers and distributed energy proponents are clear beneficiaries, as the constraint channels resources and policy support towards their technologies. The nuclear and fossil fuel industries, along with grid operators tied to legacy infrastructure, are victims, bearing the costs of exclusion and forced adaptation. Climate activists act as agenda-setters, actively shaping the narrative and policy direction. Energy consumers are diffuse beneficiaries of decarbonization but also bear some costs of transition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (fast, cheap decarbonization) is still live, but the 'faster and cheaper' claim is contested. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring the extraction from nuclear) or a pure Snare (ignoring the genuine climate coordination function). The rising extractiveness and suppression over time suggest a drift towards greater rent-seeking within the coordination framework, where the 'faster and cheaper' narrative becomes a tool for market capture rather than a purely objective assessment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    faster_cheaper_empirical_validity,
    'Is the claim that renewables + storage are ''faster and cheaper'' for full decarbonization empirically robust across all relevant scales and geographies, or does it depend on specific assumptions about grid integration, material availability, and social acceptance?',
    'Comprehensive, independent energy system modeling that includes full lifecycle costs, grid stability requirements, and material constraints for both renewable-dominant and mixed-technology pathways, validated against real-world deployment data.',
    'If the claim is robust, the constraint''s coordination function is strengthened, and extraction from nuclear is justified as an efficient allocation. If not, the extraction from nuclear is less justified, pushing the classification closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(faster_cheaper_empirical_validity, empirical, 'Empirical validity of the ''faster and cheaper'' claim for renewables-only decarbonization.').

omega_variable(
    nuclear_exclusion_justification,
    'Is the exclusion of nuclear from the ''fast and cheap'' narrative a necessary consequence of its inherent characteristics (e.g., long lead times, high upfront costs, waste disposal), or is it a strategic suppression by renewable advocates?',
    'Comparative analysis of regulatory environments and financing mechanisms for nuclear vs. renewables, examining whether policy choices actively disadvantage nuclear beyond its intrinsic properties. Also, analysis of public discourse framing.',
    'If exclusion is primarily strategic, the suppression metric is higher than justified by technical factors alone, indicating a stronger extractive component. If intrinsic, the suppression is a natural consequence of technological characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_exclusion_justification, conceptual, 'Whether nuclear''s exclusion is intrinsic or strategically imposed.').

omega_variable(
    framing_under_determination,
    'Does the ''renewable primacy'' framing accurately capture the optimal decarbonization pathway, or is it one of several defensible framings that lead to different policy conclusions?',
    'Analysis of how different normative goals (e.g., speed, cost, resilience, equity, land use) prioritize different technologies, and whether the ''faster and cheaper'' framing implicitly prioritizes certain goals over others. This would involve a multi-criteria decision analysis.',
    'If other framings are equally defensible and lead to different optimal portfolios, the ''renewable primacy'' constraint is more extractive, as it suppresses alternative legitimate pathways. If it is uniquely optimal, its coordination function is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'Alternative framings of optimal decarbonization pathways.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__renewable_primacy_reading, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(clim_tr_t2014, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2014, 0.12).
narrative_ontology:measurement(clim_tr_t2018, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement(clim_tr_t2021, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2021, 0.18).
narrative_ontology:measurement(clim_tr_t2024, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(clim_be_t2014, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2014, 0.5).
narrative_ontology:measurement(clim_be_t2018, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement(clim_be_t2021, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2021, 0.62).
narrative_ontology:measurement(clim_be_t2024, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(clim_su_t2014, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2014, 0.55).
narrative_ontology:measurement(clim_su_t2018, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2018, 0.63).
narrative_ontology:measurement(clim_su_t2021, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2021, 0.68).
narrative_ontology:measurement(clim_su_t2024, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__renewable_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, energy_grid_modernization_mandate).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, carbon_pricing_mechanisms).

% DUAL FORMULATION NOTE:
% This constraint is the 'renewable_primacy_reading' of the 'climate_mitigation_legitimacy' kernel. Its claims of speed and cost-effectiveness directly influence the policy space for other decarbonization technologies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
