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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Renewable Primacy in Decarbonization (Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'renewable primacy' reading of the broader
 *   'climate_mitigation_legitimacy' kernel. It asserts that a decarbonized
 *   energy system can be achieved faster and cheaper through renewables and
 *   storage than through nuclear power. This claim, while presented as an
 *   empirical truth, functions as a powerful constraint in energy policy,
 *   directing resources and delegitimizing alternatives. The claim's
 *   acceptance coordinates investment towards renewables while extracting
 *   capital and political will from the nuclear sector. The claimed type is
 *   'tangled_rope' because it genuinely coordinates climate action but does
 *   so by extracting from specific alternative technologies and their
 *   proponents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, 0.75).
domain_priors:suppression_score(climate_mitigation_legitimacy__renewable_primacy_reading, 0.8).
domain_priors:theater_ratio(climate_mitigation_legitimacy__renewable_primacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewable Primacy in Decarbonization (Reading)").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, '6f14f8f9-4c0b-4e32-9ab0-a29611361cd1').
narrative_ontology:cs_kernel_codification('6f14f8f9-4c0b-4e32-9ab0-a29611361cd1', formalized).
narrative_ontology:cs_authority_grounding('6f14f8f9-4c0b-4e32-9ab0-a29611361cd1', expertise).
narrative_ontology:cs_interpretation_layer_present('6f14f8f9-4c0b-4e32-9ab0-a29611361cd1').
narrative_ontology:cs_reading_relation('6f14f8f9-4c0b-4e32-9ab0-a29611361cd1', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('6f14f8f9-4c0b-4e32-9ab0-a29611361cd1', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f14f8f9-4c0b-4e32-9ab0-a29611361cd1', climate_mitigation_legitimacy__portfolio_pragmatism_reading, forecloses).
narrative_ontology:cs_axiom('6f14f8f9-4c0b-4e32-9ab0-a29611361cd1', foundational, renewable_cost_declines_continue).
narrative_ontology:cs_axiom_status(renewable_cost_declines_continue, holdable).
narrative_ontology:cs_axiom_grounding('6f14f8f9-4c0b-4e32-9ab0-a29611361cd1', renewable_cost_declines_continue, empirically_contingent).
narrative_ontology:cs_axiom('6f14f8f9-4c0b-4e32-9ab0-a29611361cd1', foundational, grid_flexibility_achievable_with_storage).
narrative_ontology:cs_axiom_status(grid_flexibility_achievable_with_storage, holdable).
narrative_ontology:cs_axiom_grounding('6f14f8f9-4c0b-4e32-9ab0-a29611361cd1', grid_flexibility_achievable_with_storage, empirically_contingent).
narrative_ontology:cs_axiom('6f14f8f9-4c0b-4e32-9ab0-a29611361cd1', secondary, nuclear_capital_cost_prohibitive).
narrative_ontology:cs_axiom_status(nuclear_capital_cost_prohibitive, holdable).
narrative_ontology:cs_axiom_grounding('6f14f8f9-4c0b-4e32-9ab0-a29611361cd1', nuclear_capital_cost_prohibitive, empirically_contingent).
narrative_ontology:cs_reference_frame('6f14f8f9-4c0b-4e32-9ab0-a29611361cd1', rapid_cost_declining_renewables_trajectory).
narrative_ontology:cs_drift_state('6f14f8f9-4c0b-4e32-9ab0-a29611361cd1', contemporary_energy_transition, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6f14f8f9-4c0b-4e32-9ab0-a29611361cd1', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, climate_activists_renewables_focused).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, grid_operators_baseload_focused).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_energy_paradigm).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, cost_declining_renewables_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These entities develop and deploy solar, wind, and battery storage projects. They benefit from policies and investment flows directed by the 'renewables primacy' narrative, gaining market share and capital. Their ability to move capital to regions with favorable policies gives them arbitrage-like exit options.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_developers, beneficiary,
    organized, biographical, arbitrage, global).

% Advocate for rapid decarbonization primarily through renewable energy. They benefit from the narrative's acceptance as it aligns with their advocacy goals and directs public and political will towards their preferred solutions. Their exit options are constrained by their commitment to climate action.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, climate_activists_renewables_focused, beneficiary,
    organized, biographical, constrained, global).

% Comprises companies involved in nuclear power plant construction, operation, and fuel cycle. They bear the costs of reduced investment, policy support, and public trust due to the 'renewables primacy' claim. Their identity is deeply tied to nuclear technology, making exit difficult.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry, payer,
    organized, generational, identity_locked, global).

% Individuals and groups who champion nuclear power as a necessary component of decarbonization. They face delegitimization and reduced influence in policy debates. Their advocacy is often an identity-locked commitment to nuclear technology.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_advocates, payer,
    moderate, biographical, identity_locked, national).

% Responsible for maintaining grid stability and reliability, often prioritizing dispatchable baseload power. They bear the costs of adapting to a grid dominated by intermittent renewables and face pressure to de-emphasize traditional baseload sources. Their institutional mandate constrains their options.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_operators_baseload_focused, payer,
    institutional, generational, constrained, national).

% Government officials and legislative bodies responsible for setting energy and climate policy. They are influenced by the 'renewables primacy' narrative, which shapes funding allocations, regulatory frameworks, and international commitments. Their decisions are constrained by political cycles and public opinion.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, policy_makers, agenda_setter,
    institutional, immediate, constrained, national).

% Academics and researchers who analyze energy systems, costs, and policy effectiveness. They critically evaluate the 'renewables primacy' claim, providing data and models that can either corroborate or challenge its tenets. Their role is to provide objective analysis.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, analytical_observers_energy_economists, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_developers).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__renewable_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates investment, policy, and public discourse towards a specific, rapid decarbonization pathway centered on renewable energy and storage, aiming to solve the collective action problem of climate change.
% TRANSFER_FUNCTION: Transfers capital, political will, and public trust from alternative decarbonization technologies (like nuclear) to renewable energy and storage solutions, influencing market dynamics and policy priorities.
% ABSENT_VOICES: Nuclear engineers focused on safety and operational reliability, and energy security analysts concerned about grid resilience, are often marginalized in policy discussions dominated by this narrative. They would argue for a more diversified, technology-inclusive approach.
% DISAPPEARANCE_RATIONALE: If the claim 'renewables plus storage can achieve full decarbonization faster and cheaper than nuclear' vanished overnight, the energy policy landscape would immediately diversify. Nuclear projects would regain legitimacy and investment, policy debates would become more technology-neutral, and the pace and cost of decarbonization would be re-evaluated across a broader portfolio of options.
% FOUNDING_PROBLEM: The urgent need to achieve full decarbonization rapidly and cost-effectively, identifying the optimal technological pathway to address climate change.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration primarily comes from renewable industry associations, some academic analyses, and climate NGOs focused on renewable deployment, all of whom are direct or indirect beneficiaries of the claim's acceptance. Independent corroboration from truly neutral parties is contested, with other analyses suggesting different optimal pathways or roles for nuclear.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_mitigation_legitimacy__renewable_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.75) is high because the claim, when adopted in policy, diverts significant capital and political support away from nuclear, effectively extracting resources from that sector. Suppression (0.80) is also high, as the narrative actively works to exclude nuclear from policy consideration and public discourse, limiting its accessibility as an alternative. The theater ratio (0.15) is low because this is a live, actively contested and enforced claim, not a performative relic. Resistance (0.75) is high from nuclear advocates. Accessibility collapse (0.70) is substantial as nuclear becomes a less viable policy option. The measurement series shows a rising trend in extractiveness and suppression as the 'renewables primacy' narrative gained traction and policy influence from 2010 to 2030, then stabilizing as it became a dominant paradigm.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of renewable energy developers and climate activists, this constraint is a beneficial coordination mechanism, efficiently guiding society towards decarbonization. From the perspective of the nuclear industry and its advocates, it is a highly extractive and suppressive force, unfairly disadvantaging a viable decarbonization pathway. Policy makers, as agenda-setters, experience it as a powerful, often politically expedient, framework for decision-making.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable energy developers and climate activists are clear beneficiaries (low d) as the constraint directs resources and legitimacy towards their preferred solutions. The nuclear industry, nuclear advocates, and grid operators focused on baseload are targets (high d) as they bear the costs of reduced investment, policy support, and the need to adapt to a renewables-dominated grid. Policy makers act as agenda-setters, mediating these flows.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_robustness_of_claim,
    'Is the claim ''renewables plus storage can achieve full decarbonization faster and cheaper than nuclear'' empirically robust across all relevant contexts (e.g., different grid sizes, resource endowments, political systems)?',
    'Comprehensive, independent, and geographically diverse techno-economic analyses comparing full system costs and deployment timelines for various decarbonization pathways, including nuclear and renewables+storage.',
    'If the claim is universally robust, the constraint functions as a genuine Rope for decarbonization. If it holds only under specific conditions, its application as a universal truth becomes extractive, pushing it towards a Snare for contexts where nuclear might be optimal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_robustness_of_claim, empirical, 'Uncertainty regarding the universal empirical validity of the ''renewables primacy'' claim.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of nuclear development primarily due to the inherent economic and technical superiority of renewables, or due to active policy exclusion and lobbying efforts by renewable advocates?',
    'Analysis of policy decisions and funding allocations in jurisdictions that have explicitly adopted technology-neutral energy policies versus those with renewables-first mandates. If nuclear projects struggle even with neutral policy, it suggests inherent superiority; if they thrive, it suggests policy-driven suppression.',
    'If suppression is primarily policy-driven, the constraint''s effective suppression is higher and more coercive than if it were purely market-driven, strengthening its Snare-like qualities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Distinguishing between market-driven and policy-driven suppression of nuclear power.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''climate_mitigation_legitimacy'' kernel. What specific structural elements would change if a sibling reading (e.g., ''baseload_necessity_reading'') were adopted as the dominant frame?',
    'Compare policy outcomes and investment patterns in jurisdictions where the ''baseload necessity'' reading is dominant. Observe changes in victim/beneficiary sets and resource allocation.',
    'Adopting ''baseload_necessity_reading'' would shift nuclear from a victim to a beneficiary, and grid operators (baseload-focused) would become beneficiaries, while renewable developers might become payers or face more stringent grid integration requirements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Clarifying the structural deltas between this reading and its siblings within the ''climate_mitigation_legitimacy'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__renewable_primacy_reading, 2010, 2040).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(clim_tr_t2015, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(clim_tr_t2025, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2025, 0.16).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2030, 0.15).
narrative_ontology:measurement(clim_tr_t2035, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2035, 0.14).
narrative_ontology:measurement(clim_tr_t2040, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2040, 0.13).

% Extraction over time
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(clim_be_t2015, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(clim_be_t2025, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2025, 0.72).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2030, 0.75).
narrative_ontology:measurement(clim_be_t2035, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2035, 0.76).
narrative_ontology:measurement(clim_be_t2040, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2040, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(clim_su_t2015, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement(clim_su_t2025, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2025, 0.78).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2030, 0.8).
narrative_ontology:measurement(clim_su_t2035, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2035, 0.81).
narrative_ontology:measurement(clim_su_t2040, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2040, 0.8).


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
% This constraint is one reading of the 'climate_mitigation_legitimacy' kernel, focusing on the primacy of renewables and storage. It is structurally distinct from sibling readings that emphasize baseload necessity, portfolio diversity, or degrowth, each of which would yield different epsilon values and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
