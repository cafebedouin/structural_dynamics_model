% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__quantitative_growth_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: performance_legitimacy__quantitative_growth_reading
 *   human_readable: Legitimacy via Quantitative GDP Growth
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint represents the 'quantitative growth' reading of the
 *   broader 'performance legitimacy' kernel, where the state's right to rule
 *   is primarily justified by its ability to deliver high GDP growth rates,
 *   economic expansion, and job creation. This reading prioritizes raw
 *   economic output, often tolerating significant environmental and social
 *   costs as necessary for development. The constraint operates as a Tangled
 *   Rope, coordinating vast economic activity but with substantial,
 *   asymmetric extraction from specific groups and the environment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.78).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.7).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "Legitimacy via Quantitative GDP Growth").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, '4c576850-5553-4a07-a069-66f51edb734c').
narrative_ontology:cs_kernel_codification('4c576850-5553-4a07-a069-66f51edb734c', formalized).
narrative_ontology:cs_authority_grounding('4c576850-5553-4a07-a069-66f51edb734c', extraction).
narrative_ontology:cs_interpretation_layer_present('4c576850-5553-4a07-a069-66f51edb734c').
narrative_ontology:cs_reading_relation('4c576850-5553-4a07-a069-66f51edb734c', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('4c576850-5553-4a07-a069-66f51edb734c', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c576850-5553-4a07-a069-66f51edb734c', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('4c576850-5553-4a07-a069-66f51edb734c', foundational, gdp_growth_is_primary_indicator_of_progress).
narrative_ontology:cs_axiom_status(gdp_growth_is_primary_indicator_of_progress, holdable).
narrative_ontology:cs_axiom_grounding('4c576850-5553-4a07-a069-66f51edb734c', gdp_growth_is_primary_indicator_of_progress, empirically_contingent).
narrative_ontology:cs_axiom('4c576850-5553-4a07-a069-66f51edb734c', foundational, economic_stability_requires_continuous_expansion).
narrative_ontology:cs_axiom_status(economic_stability_requires_continuous_expansion, holdable).
narrative_ontology:cs_axiom_grounding('4c576850-5553-4a07-a069-66f51edb734c', economic_stability_requires_continuous_expansion, empirically_contingent).
narrative_ontology:cs_reference_frame('4c576850-5553-4a07-a069-66f51edb734c', post_reform_economic_takeoff).
narrative_ontology:cs_drift_state('4c576850-5553-4a07-a069-66f51edb734c', contemporary_slowdown_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4c576850-5553-4a07-a069-66f51edb734c', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, local_government_officials).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, state_leadership).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, environmental_advocates).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, displaced_communities).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, international_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority that frames GDP growth as the primary metric of national success and the foundation of its legitimacy. Directs policy and resource allocation to achieve growth targets, often prioritizing economic expansion over other concerns.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, state_leadership, agenda_setter,
    institutional, civilizational, constrained, national).

% Large state-owned and private enterprises heavily invested in export-oriented manufacturing and infrastructure. Directly benefits from state policies that prioritize growth, including subsidies, cheap land, and relaxed environmental regulations. Their success is often directly tied to GDP figures.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    powerful, generational, arbitrage, global).

% Their career progression and performance evaluations are heavily tied to achieving local GDP growth targets. This incentivizes them to attract investment, often leading to overcapacity, debt, and environmental degradation, but securing their political standing.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, local_government_officials, beneficiary,
    organized, biographical, constrained, regional).

% Technocrats and policy advisors responsible for designing and implementing economic strategies. They interpret data and formulate policies to achieve growth targets, often operating within the established paradigm that prioritizes quantitative expansion.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, economic_planners, agenda_setter,
    institutional, biographical, constrained, national).

% Benefit from the stable, growth-oriented environment and large market. They can move capital in and out, leveraging the growth imperative for favorable investment conditions, but are also subject to state policy shifts.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, international_investors, beneficiary,
    powerful, immediate, arbitrage, global).

% Bear the costs of environmental degradation (pollution, resource depletion) resulting from unchecked growth. Their calls for sustainability are often suppressed or sidelined in favor of economic targets, with limited avenues for effective resistance.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, environmental_advocates, payer,
    powerless, generational, trapped, national).

% Communities often displaced by large-scale infrastructure projects or industrial expansion, bearing direct social and economic costs. They have minimal power to resist and few viable alternatives for livelihood or relocation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, displaced_communities, payer,
    powerless, immediate, trapped, local).

% Will inherit the long-term environmental and social consequences of current growth-at-all-costs policies, including climate change impacts, resource scarcity, and accumulated debt. Their interests are represented only through advocacy and analytical projections.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, future_generations, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(performance_legitimacy__quantitative_growth_reading, future_generations).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:fixing_cost_class(performance_legitimacy__quantitative_growth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns state apparatus, industrial policy, and local governance towards the singular goal of maximizing GDP growth rates, providing a clear, measurable target for economic activity and political performance.
% TRANSFER_FUNCTION: Transfers resources, political capital, and social/environmental costs towards growth-oriented industries and projects, from environmental protection, social welfare, and future sustainability, to the industrial-export complex and officials measured on GDP.
% ABSENT_VOICES: Environmental scientists, social welfare advocates, and communities negatively impacted by rapid industrialization are often marginalized or silenced. They would argue for a more balanced development model that prioritizes sustainability and equity over raw growth figures.
% DISAPPEARANCE_RATIONALE: If the legitimacy of the state were no longer tied to GDP growth, the entire political economy would undergo a fundamental reorientation. Investment priorities would shift, local officials would lose their primary performance metric, and the industrial-export complex would face new constraints, leading to a massive restructuring of power and resources.
% FOUNDING_PROBLEM: The constraint was built to address widespread poverty, underdevelopment, and the historical imperative for rapid modernization and national strength following periods of instability.
% FOUNDING_PROBLEM_CORROBORATION: State leadership and official media consistently assert that continued high growth is essential for national stability and prosperity, implying the founding problem is still live. However, independent economists, environmental NGOs, and social critics argue that while the original problem has been substantially addressed, the current growth model now creates new, severe problems (e.g., inequality, pollution), making its 'live' status contested.
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(performance_legitimacy__quantitative_growth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__quantitative_growth_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__quantitative_growth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__quantitative_growth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.78) because the relentless pursuit of GDP growth leads to externalized costs (environmental damage, social inequality, overcapacity) that are borne by victims while beneficiaries profit. Suppression (0.70) is significant as alternative development models or critiques of the growth paradigm are actively marginalized or suppressed to maintain the focus on quantitative targets. Theater ratio (0.45) is moderate and increasing, reflecting a growing gap between the claimed benefits of growth (e.g., broad prosperity) and the actual outcomes, with some growth figures potentially masking underlying inefficiencies or unsustainable practices. The measurement series show a clear trend of increasing extractiveness, suppression, and theatricality over time as the growth model matures and its costs accumulate.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state leadership and economic beneficiaries, this constraint is a necessary coordination mechanism for national development and stability. They perceive the costs as unavoidable trade-offs for collective prosperity. From the perspective of victims and critics, it is a highly extractive system that leverages state power to concentrate benefits while externalizing costs, with the coordination narrative serving as a cover for rent-seeking and unsustainable practices.
 *
 * DIRECTIONALITY LOGIC:
 *   State leadership, the industrial-export complex, and local government officials are clear beneficiaries, as their power, profits, and careers are directly tied to achieving growth targets. International investors also benefit from the stable, growth-oriented environment. Environmental advocates, displaced communities, and future generations are the primary targets/victims, bearing the costs of pollution, resource depletion, and social disruption with limited power or exit options. Economic planners act as agenda-setters, implementing the growth imperative.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gdp_as_welfare_proxy,
    'To what extent does GDP growth accurately reflect improvements in overall societal welfare, rather than merely economic activity?',
    'Longitudinal studies correlating GDP growth with alternative welfare indicators (e.g., Genuine Progress Indicator, Human Development Index) and public satisfaction surveys.',
    'If GDP is a poor proxy for welfare, the constraint''s claimed coordination function (delivering prosperity) is undermined, increasing its effective extractiveness and theater ratio. If it correlates strongly, the coordination function is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gdp_as_welfare_proxy, empirical, 'Ambiguity of GDP as a measure of societal welfare.').

omega_variable(
    growth_sustainability_tradeoff,
    'Is the current rate and model of quantitative GDP growth environmentally and socially sustainable in the long term?',
    'Ecological footprint analysis, resource depletion modeling, and social impact assessments over multi-generational time horizons.',
    'If unsustainable, the constraint''s long-term viability as a source of legitimacy is severely compromised, and its extractiveness from future generations is confirmed as severe. If sustainable, the justification for the current model is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_sustainability_tradeoff, empirical, 'Trade-off between quantitative growth and long-term sustainability.').

omega_variable(
    job_creation_vs_overcapacity,
    'Does the investment-driven growth model still effectively create broad-based employment, or is it primarily generating industrial overcapacity and debt?',
    'Detailed sector-by-sector analysis of employment elasticity to investment, and assessment of capacity utilization rates across key industries.',
    'If overcapacity is dominant, the constraint''s claim of job creation as a coordination function is weakened, increasing its theater ratio and revealing a more purely extractive dynamic for beneficiaries of over-investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(job_creation_vs_overcapacity, empirical, 'Effectiveness of growth model for job creation versus overcapacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t1980, performance_legitimacy__quantitative_growth_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(perf_tr_t1990, performance_legitimacy__quantitative_growth_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(perf_tr_t2000, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(perf_tr_t2010, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(perf_tr_t2020, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2020, 0.43).
narrative_ontology:measurement(perf_tr_t2025, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(perf_be_t1980, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(perf_be_t1990, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(perf_be_t2000, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2000, 0.67).
narrative_ontology:measurement(perf_be_t2010, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2010, 0.73).
narrative_ontology:measurement(perf_be_t2020, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement(perf_be_t2025, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t1980, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(perf_su_t1990, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(perf_su_t2000, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(perf_su_t2010, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(perf_su_t2020, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(perf_su_t2025, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, export_led_development).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, state_owned_enterprise_subsidies).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, environmental_regulation_enforcement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
