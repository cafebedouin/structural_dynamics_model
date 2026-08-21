% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__opportunity_cost_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__opportunity_cost_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: climate_mitigation_imperative__opportunity_cost_reading
 *   human_readable: Climate Mitigation: Opportunity Cost Imperative
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'opportunity_cost_reading' of the broader
 *   'climate_mitigation_imperative' kernel. It asserts that effective climate
 *   mitigation demands the fastest deployment of carbon reduction per dollar
 *   invested. Consequently, technologies like nuclear, characterized by high
 *   capital intensity and long development timelines, are deemed
 *   'net-harmful' due to the opportunity cost of diverting resources from
 *   faster, cheaper alternatives. This reading, while framed as a rational
 *   approach to a global crisis, effectively extracts legitimacy and funding
 *   from certain low-carbon technologies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, 0.7).
domain_priors:suppression_score(climate_mitigation_imperative__opportunity_cost_reading, 0.75).
domain_priors:theater_ratio(climate_mitigation_imperative__opportunity_cost_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_imperative__opportunity_cost_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__opportunity_cost_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__opportunity_cost_reading, "Climate Mitigation: Opportunity Cost Imperative").
narrative_ontology:topic_domain(climate_mitigation_imperative__opportunity_cost_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__opportunity_cost_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__opportunity_cost_reading, 'd15079c5-8f54-4d3b-92af-fa0bc5554d22').
narrative_ontology:cs_kernel_codification('d15079c5-8f54-4d3b-92af-fa0bc5554d22', implicit).
narrative_ontology:cs_authority_grounding('d15079c5-8f54-4d3b-92af-fa0bc5554d22', expertise).
narrative_ontology:cs_reading_relation('d15079c5-8f54-4d3b-92af-fa0bc5554d22', climate_mitigation_imperative__portfolio_optimization_reading, forecloses).
narrative_ontology:cs_reading_relation('d15079c5-8f54-4d3b-92af-fa0bc5554d22', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('d15079c5-8f54-4d3b-92af-fa0bc5554d22', foundational, mitigation_speed_is_paramount).
narrative_ontology:cs_axiom_status(mitigation_speed_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('d15079c5-8f54-4d3b-92af-fa0bc5554d22', mitigation_speed_is_paramount, empirically_contingent).
narrative_ontology:cs_axiom('d15079c5-8f54-4d3b-92af-fa0bc5554d22', foundational, capital_intensity_is_a_cost_not_an_investment).
narrative_ontology:cs_axiom_status(capital_intensity_is_a_cost_not_an_investment, holdable).
narrative_ontology:cs_axiom_grounding('d15079c5-8f54-4d3b-92af-fa0bc5554d22', capital_intensity_is_a_cost_not_an_investment, empirically_contingent).
narrative_ontology:cs_reference_frame('d15079c5-8f54-4d3b-92af-fa0bc5554d22', cost_effectiveness_paradigm).
narrative_ontology:cs_drift_state('d15079c5-8f54-4d3b-92af-fa0bc5554d22', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d15079c5-8f54-4d3b-92af-fa0bc5554d22', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__opportunity_cost_reading, nuclear_energy_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote policies and funding allocations that prioritize the fastest and most cost-effective carbon reduction, often citing economic analyses of opportunity costs. They benefit from the perceived efficiency and impact of this approach.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_advocates, agenda_setter,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_advocates, beneficiary).

% Benefit from the policy and financial support diverted towards their technologies due to their perceived speed and cost-effectiveness in carbon reduction. They actively champion this imperative.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_advocates, beneficiary,
    powerful, biographical, mobile, global).

% Bear the cost of this imperative through reduced funding, political support, and public legitimacy. They argue for nuclear's role in baseload power and long-term decarbonization, but face significant headwinds from the focus on immediate cost-per-carbon-reduction metrics.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, nuclear_energy_advocates, payer,
    powerful, generational, constrained, national).

% While not directly part of the low-carbon debate, they are excluded from the 'solution' space. However, internal divisions within the low-carbon sector (e.g., over nuclear) can indirectly benefit them by slowing overall mitigation progress.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, fossil_fuel_industry, excluded,
    institutional, biographical, arbitrage, global).

% Provide the data and models that underpin the 'fastest deployment per dollar' metric, often without direct advocacy for specific technologies. Their analyses are used to justify the imperative.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, economic_analysts, observer,
    analytical, immediate, analytical, universal).

% Are tasked with implementing climate mitigation strategies. They often adopt principles like 'fastest deployment per dollar' to guide resource allocation, balancing political feasibility with scientific urgency.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__opportunity_cost_reading, policy_makers, agenda_setter,
    institutional, immediate, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__opportunity_cost_reading, renewable_energy_advocates).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__opportunity_cost_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global and national efforts towards the most rapid and cost-effective reduction of greenhouse gas emissions, by prioritizing technologies that deliver the greatest carbon reduction per unit of investment and time.
% TRANSFER_FUNCTION: Transfers political capital, public funding, and policy support away from capital-intensive, slow-to-deploy low-carbon technologies (like nuclear) towards faster, cheaper alternatives (like renewables), based on a carbon-per-dollar-per-year metric.
% ABSENT_VOICES: Long-term energy security planners (who might prioritize baseload power over speed), communities reliant on nuclear for jobs, and those who see nuclear as a necessary part of a diverse energy portfolio. They are often sidelined by the immediate imperative for fastest carbon reduction, as their concerns are framed as secondary to the primary goal.
% DISAPPEARANCE_RATIONALE: If this imperative vanished, the debate over energy policy would shift significantly, potentially re-opening funding and political support for nuclear, and altering the prioritization of different mitigation strategies. Resource allocation for climate action would reorganize around different criteria.
% FOUNDING_PROBLEM: The urgent need to rapidly reduce global greenhouse gas emissions to avoid catastrophic climate change, coupled with limited financial and political capital, requiring efficient allocation of resources.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists, IPCC reports, and environmental organizations (outside the direct beneficiaries of specific technologies) corroborate the urgency of the problem. However, the specific interpretation of 'efficiency' and 'harm' is contested by nuclear advocates and some energy system modelers.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__opportunity_cost_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__opportunity_cost_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__opportunity_cost_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_mitigation_imperative__opportunity_cost_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__opportunity_cost_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__opportunity_cost_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__opportunity_cost_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) is high because this principle, when adopted, actively redirects significant capital and political will away from nuclear, effectively 'taxing' its viability. Suppression (0.75) is substantial as it frames nuclear as a 'net-harm' in the mitigation context, actively suppressing its policy alternatives and public acceptance. Resistance (0.8) is high due to strong advocacy from the nuclear industry and its supporters. Theater ratio (0.1) is low because the principle is directly applied in policy and funding decisions, with little performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of renewable energy advocates, this constraint is a rational and necessary 'rope' for efficient climate action. From the perspective of nuclear energy advocates, it operates as a 'snare' that unfairly targets their technology, ignoring its long-term benefits and reliability. The engine's classification as a 'tangled_rope' reflects the genuine coordination function (rapid mitigation) coupled with the asymmetric extraction from nuclear.
 *
 * DIRECTIONALITY LOGIC:
 *   Climate mitigation advocates and renewable energy advocates are beneficiaries, as the principle channels resources and support towards their preferred solutions. Nuclear energy advocates are the primary targets/payers, as their technology is deemed 'net-harmful' and loses out on critical resources. Policy makers act as agenda setters, implementing policies guided by this imperative. Economic analysts provide the 'objective' metrics that justify the directionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_validity_of_net_harm,
    'Is the claim that nuclear energy is ''net-harmful'' for climate mitigation, due to opportunity costs, empirically robust across all relevant timescales and system boundaries?',
    'Comprehensive, independent lifecycle assessments and integrated energy system models that account for grid stability, long-term energy security, and full system costs (including storage and transmission for intermittent renewables), compared against the ''carbon-per-dollar-per-year'' metric.',
    'If the ''net-harm'' claim is empirically disproven, the constraint''s extractiveness from nuclear would decrease, potentially reclassifying it towards a ''rope'' or ''scaffold'' for a broader portfolio approach. If strongly confirmed, the ''snare'' aspect would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_validity_of_net_harm, empirical, 'Empirical robustness of nuclear''s ''net-harm'' claim in climate mitigation.').

omega_variable(
    framing_of_mitigation_imperative,
    'Is ''fastest deployment per dollar'' the only valid or optimal interpretation of the ''climate mitigation imperative,'' or are other framings (e.g., long-term system resilience, energy justice, technological diversity) equally valid and potentially less extractive?',
    'A conceptual re-evaluation of climate mitigation goals, potentially through deliberative democratic processes or interdisciplinary expert consensus, to weigh the relative importance of speed/cost against other values.',
    'If alternative framings gain prominence, the constraint''s suppressive and extractive force on nuclear would diminish, as its ''net-harm'' conclusion is contingent on the ''fastest deployment per dollar'' premise. This could lead to a reclassification towards a ''rope'' or ''scaffold'' that accommodates a broader range of solutions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_mitigation_imperative, conceptual, 'Conceptual framing of the climate mitigation imperative and its impact on technology choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__opportunity_cost_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t6, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(clim_tr_t18, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_imperative__opportunity_cost_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(clim_be_t6, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(clim_be_t18, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_imperative__opportunity_cost_reading, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(clim_su_t6, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(clim_su_t18, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 18, 0.73).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_imperative__opportunity_cost_reading, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__opportunity_cost_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__opportunity_cost_reading, climate_mitigation_imperative__systems_transition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_mitigation_imperative' kernel, focusing on opportunity costs. It is linked to sibling readings that offer alternative interpretations of the same core imperative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
