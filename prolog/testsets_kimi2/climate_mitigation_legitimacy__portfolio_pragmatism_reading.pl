% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__portfolio_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__portfolio_pragmatism_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__portfolio_pragmatism_reading
 *   human_readable: Technology-Neutral Decarbonization Portfolio Mandate
 *   domain: energy/policy/technology
 *
 * SUMMARY:
 *   This constraint is the portfolio_pragmatism_reading of the
 *   climate_mitigation_legitimacy kernel. It instantiates the policy claim
 *   that optimal decarbonization requires a technology-neutral portfolio
 *   including both nuclear and renewables. The claim operates as a
 *   coordination mechanism across fragmented clean-energy industries but
 *   simultaneously extracts public capital and political voice from host
 *   communities, fossil incumbents, and demand-side advocates. The constraint
 *   is actively enforced through subsidy design, regulatory eligibility
 *   rules, and the institutional authority of integrated assessment modeling.
 *
 * KEY AGENTS:
 *   - national_climate_coalitions: agenda setter (institutional/mobile) â sets the technology-neutral frame and enforces it through planning and subsidy
 *   - integrated_clean_energy_developers: primary beneficiary (powerful/mobile) â captures public capital and regulatory certainty
 *   - climate_policy_modeling_institutions: secondary beneficiary (institutional/constrained) â legitimates the frame through optimization narratives
 *   - infrastructure_host_communities: primary target (powerless/trapped) â bears siting and risk costs without veto
 *   - degrowth_advocacy_networks: political target (moderate/constrained) â excluded from the policy conversation
 *   - fossil_fuel_incumbents: economic target (powerful/constrained) â faces stranded assets and marginalization
 *   - competition_economists: analytical observer (analytical/analytical) â tracks divergence between neutrality claims and outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.65).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.6).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Technology-Neutral Decarbonization Portfolio Mandate").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "energy/policy/technology").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '7c4bec16-3004-4fcc-942f-64b591385b80').
narrative_ontology:cs_kernel_codification('7c4bec16-3004-4fcc-942f-64b591385b80', formalized).
narrative_ontology:cs_authority_grounding('7c4bec16-3004-4fcc-942f-64b591385b80', expertise).
narrative_ontology:cs_interpretation_layer_present('7c4bec16-3004-4fcc-942f-64b591385b80').
narrative_ontology:cs_reading_relation('7c4bec16-3004-4fcc-942f-64b591385b80', climate_mitigation_legitimacy__baseload_necessity_reading, influences).
narrative_ontology:cs_reading_relation('7c4bec16-3004-4fcc-942f-64b591385b80', climate_mitigation_legitimacy__renewable_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('7c4bec16-3004-4fcc-942f-64b591385b80', climate_mitigation_legitimacy__degrowth_sufficiency_reading, forecloses).
narrative_ontology:cs_axiom('7c4bec16-3004-4fcc-942f-64b591385b80', foundational, diversified_generation_portfolio_optimal).
narrative_ontology:cs_axiom_status(diversified_generation_portfolio_optimal, holdable).
narrative_ontology:cs_axiom_grounding('7c4bec16-3004-4fcc-942f-64b591385b80', diversified_generation_portfolio_optimal, instrumental).
narrative_ontology:cs_axiom('7c4bec16-3004-4fcc-942f-64b591385b80', foundational, no_technology_a_priori_excluded).
narrative_ontology:cs_axiom_status(no_technology_a_priori_excluded, holdable).
narrative_ontology:cs_axiom_grounding('7c4bec16-3004-4fcc-942f-64b591385b80', no_technology_a_priori_excluded, conventional).
narrative_ontology:cs_reference_frame('7c4bec16-3004-4fcc-942f-64b591385b80', technology_neutral_optimal_mix_framework).
narrative_ontology:cs_drift_state('7c4bec16-3004-4fcc-942f-64b591385b80', renewable_cost_parity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7c4bec16-3004-4fcc-942f-64b591385b80', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, integrated_clean_energy_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_policy_modeling_institutions).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, reliability_focused_grid_operators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fossil_fuel_incumbents).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, infrastructure_host_communities).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, degrowth_advocacy_networks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the technology-neutral policy frame through national energy plans, subsidy allocation, and regulatory standards. Coordinates across ministries to maintain portfolio balance between nuclear and renewable constituencies, enforcing the frame by excluding technology-specific or demand-side alternatives from official roadmaps.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, national_climate_coalitions, agenda_setter,
    institutional, generational, mobile, national).

% Develop large-scale generation assets across nuclear, wind, and solar. Benefit from policy certainty that keeps all technologies eligible for public support and grid access, allowing portfolio hedging against any single technology's cost or political risk.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, integrated_clean_energy_developers, beneficiary,
    powerful, biographical, mobile, national).

% Produce integrated assessment models and net-zero roadmaps that legitimize the technology-neutral portfolio as the optimal pathway. Their authority and continued funding depend on maintaining the framing that both nuclear and renewables are indispensable.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_policy_modeling_institutions, beneficiary,
    institutional, civilizational, constrained, global).

% Manage system reliability and benefit from a diversified portfolio that spreads intermittency and outage risk across technologies, reducing the need for demand curtailment or extreme storage buildout.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, reliability_focused_grid_operators, beneficiary,
    institutional, generational, constrained, national).

% Face stranded asset risk and exclusion from the decarbonization portfolio. Their political resistance is marginalized by the technology-neutral frame, which treats fossil fuels as universally excluded rather than negotiating a transition role.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fossil_fuel_incumbents, payer,
    powerful, biographical, constrained, global).

% Bear land-use change, noise, radiation risk perception, and transmission-line encroachment from both nuclear and renewable projects. They are consulted in siting processes but lack veto power over national portfolio mandates that determine where infrastructure must go.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, infrastructure_host_communities, payer,
    powerless, generational, trapped, local).

% Argue that demand reduction and sufficiency make large-scale generation expansion unnecessary. Their perspective is structurally excluded from technology-neutral policy tables, which presuppose continued energy service growth and frame the problem as supply-side technology choice.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, degrowth_advocacy_networks, payer,
    moderate, civilizational, constrained, global).

% Evaluate whether technology-neutral frameworks actually achieve neutral outcomes or instead mask technology-specific subsidy capture and regulatory bias. They observe the divergence between modeled neutrality and implemented policy.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, competition_economists, observer,
    analytical, biographical, analytical, national).

narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__portfolio_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns fragmented capital, regulatory approval, and grid planning across nuclear and renewable industries to achieve emission reductions at a scale and speed that single-technology pathways might not sustain, while managing system reliability concerns.
% TRANSFER_FUNCTION: Moves public capital, regulatory priority, and social license from fossil fuel infrastructure and demand-side sufficiency frameworks toward large-scale nuclear and renewable generation projects, concentrating development risk in host communities and policy authority in modeling institutions.
% ABSENT_VOICES: Degrowth advocates and sufficiency-oriented planners are excluded from the technology-neutral optimization frame; fossil fuel workers and communities dependent on extractive industries are present only as transition objects, not as agents with a legitimate stake in the speed or direction of the shift.
% DISAPPEARANCE_RATIONALE: If the technology-neutral portfolio mandate disappeared, renewable and nuclear constituencies would compete for separate policy tracks, fossil incumbents would exploit the fracture to delay decarbonization, and the integrated policy modeling that currently coordinates capital allocation would collapse into adversarial bidding.
% FOUNDING_PROBLEM: Climate change requires rapid, large-scale displacement of fossil fuel combustion; fragmented technology-specific advocacy impedes the capital mobilization and regulatory certainty needed for timely emission reductions.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists attest the emissions problem is live. However, the claim that the solution requires a technology-neutral portfolio including nuclear is corroborated primarily by integrated assessment modelers and institutions with funding tied to infrastructure-intensive pathways; sufficiency researchers and ecological economists outside the benefiting parties contest the founding problem's framing.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.65 reflects substantial channeling of public capital and regulatory priority toward large infrastructure developers and away from demand-side or fossil alternatives, though genuine coordination value exists. Suppression at 0.60 reflects the structural exclusion of degrowth and sufficiency frameworks from policy tables, not physical violence. Theater ratio 0.42 indicates moderate performative neutrality: the technology-neutral frame is maintained even as empirical cost curves increasingly favor renewables, and policy practice deviates from neutral optimization. Accessibility collapse 0.65: once the portfolio frame is accepted, non-infrastructure alternatives become politically invisible. Resistance 0.55: fossil incumbents and degrowth networks actively contest the frame, but are marginalized.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (policy institutions, diversified developers) experience the constraint as necessary coordination to prevent technology-war fragmentation. The payer seats (host communities, fossil incumbents, degrowth advocates) experience it as extraction of land, capital, and political voice. The engine computes this divergence from structural data: beneficiaries have mobile or analytical exit and institutional power; payers have trapped or constrained exit and local or moderate power.
 *
 * DIRECTIONALITY LOGIC:
 *   Integrated clean energy developers, policy modeling institutions, and grid operators are structural beneficiaries (low d, subsidized by the constraint's allocation of capital and authority). Fossil fuel incumbents, host communities, and degrowth advocates are structural targets (high d, extraction of assets, land, and voice). National climate coalitions sit near symmetric but with agenda-setting power that tilts them toward the beneficiary pole. Competition economists are analytical observers with near-neutral d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by distinguishing the genuine coordination function (aligning capital across technologies for reliability and speed) from the extractive overlay (suppressing demand-side alternatives and channeling rents to infrastructure developers). Without this distinction, the policy could be read as pure snare (if extraction dominated) or pure rope (if neutrality were genuine). The temporal measurements show rising theater_ratio, suggesting the coordination story is increasingly performative as cost curves shift, but the underlying collective-action problem (decarbonization at scale) remains live, keeping it tangled rather than pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_neutrality_empirical_status,
    'Does the technology-neutral portfolio remain optimal under contemporary cost curves, or has it become a coalition-management fiction that lags empirical reality?',
    'Comparative levelized cost of energy analysis and systems modeling that removes policy-forced nuclear inclusion to see if neutrality still produces mixed portfolios.',
    'If empirical neutrality now favors renewables alone, the constraint''s extraction component rises (theater increases) and its coordination function degrades toward piton or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_neutrality_empirical_status, empirical, 'Empirical status of technology-neutral optimization').

omega_variable(
    demand_side_exclusion_mechanism,
    'Is the exclusion of degrowth and sufficiency voices a necessary boundary condition for capital mobilization, or an extractive suppression of lower-cost alternatives?',
    'Comparative policy analysis of jurisdictions that have integrated demand reduction into climate strategy versus those that have not, measuring cost and speed of emission reductions.',
    'If sufficiency is a viable lower-cost alternative, the constraint''s accessibility_collapse is higher than structurally necessary and the suppression metric is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demand_side_exclusion_mechanism, conceptual, 'Nature of demand-side exclusion').

omega_variable(
    kernel_reading_foreclosure_validity,
    'Does the portfolio pragmatism reading genuinely foreclose renewable primacy and degrowth sufficiency, or do these readings coexist as live policy options within overlapping institutional frameworks?',
    'Cross-national legislative mapping: do any jurisdictions simultaneously maintain technology-neutral portfolio mandates and renewable-only or degrowth transition plans?',
    'If readings coexist in practice, the forecloses relations should be reclassified as coexists_with or influences, altering the kernel''s structural topology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_validity, conceptual, 'Whether foreclosure relations are empirically valid').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(clim_tr_t32, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(clim_tr_t40, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(clim_be_t32, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement(clim_be_t40, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(clim_su_t32, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(clim_su_t40, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% The climate_mitigation_legitimacy kernel decomposes into four structurally distinct readings because the colloquial label 'climate mitigation legitimacy' conflates multiple incompatible claims about how decarbonization should proceed: baseload necessity (dispatchable power required), renewable primacy (renewables sufficient), portfolio pragmatism (technology-neutral mix), and degrowth sufficiency (demand reduction). Each reading has a distinct epsilon, beneficiary structure, and empirical basis. This story is the portfolio_pragmatism_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
