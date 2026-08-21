% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__portfolio_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: climate_mitigation_legitimacy__portfolio_pragmatism_reading
 *   human_readable: Optimal Decarbonization via Technology-Neutral Portfolio
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'portfolio pragmatism' reading of the
 *   broader 'climate_mitigation_legitimacy' kernel. It posits that optimal
 *   decarbonization requires a technology-neutral approach, integrating both
 *   nuclear and renewable energy sources, with an emphasis on regional
 *   optimization and moderate capital diversification. This reading aims to
 *   avoid ideological privileging of one clean technology over another,
 *   focusing instead on the most effective and reliable path to climate
 *   goals. The claimed type is 'rope' as it describes an ideal coordination
 *   mechanism, though its implementation faces real-world challenges.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.25).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.15).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Optimal Decarbonization via Technology-Neutral Portfolio").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '76043441-5280-4bcd-8b7b-4f618e10f610').
narrative_ontology:cs_kernel_codification('76043441-5280-4bcd-8b7b-4f618e10f610', formalized).
narrative_ontology:cs_authority_grounding('76043441-5280-4bcd-8b7b-4f618e10f610', expertise).
narrative_ontology:cs_interpretation_layer_present('76043441-5280-4bcd-8b7b-4f618e10f610').
narrative_ontology:cs_reading_relation('76043441-5280-4bcd-8b7b-4f618e10f610', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('76043441-5280-4bcd-8b7b-4f618e10f610', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('76043441-5280-4bcd-8b7b-4f618e10f610', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('76043441-5280-4bcd-8b7b-4f618e10f610', foundational, technology_agnostic_decarbonization).
narrative_ontology:cs_axiom_status(technology_agnostic_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('76043441-5280-4bcd-8b7b-4f618e10f610', technology_agnostic_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('76043441-5280-4bcd-8b7b-4f618e10f610', foundational, risk_diversification_is_optimal).
narrative_ontology:cs_axiom_status(risk_diversification_is_optimal, holdable).
narrative_ontology:cs_axiom_grounding('76043441-5280-4bcd-8b7b-4f618e10f610', risk_diversification_is_optimal, empirically_contingent).
narrative_ontology:cs_reference_frame('76043441-5280-4bcd-8b7b-4f618e10f610', evidence_based_policy_making).
narrative_ontology:cs_drift_state('76043441-5280-4bcd-8b7b-4f618e10f610', contemporary_policy_debate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('76043441-5280-4bcd-8b7b-4f618e10f610', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, energy_consumers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_industry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, environmental_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for designing and implementing energy policies to meet decarbonization targets. They must balance scientific advice, economic realities, and political pressures, often struggling to maintain true technology neutrality.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Provide the foundational scientific understanding of climate change and the technical potential of various mitigation technologies. They advocate for evidence-based, effective decarbonization strategies.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_scientists, observer,
    analytical, civilizational, analytical, global).

% Benefits from policies that include nuclear power as a viable, dispatchable, low-carbon option, ensuring continued investment and operational capacity. They advocate for its role in a diversified energy mix.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry, beneficiary,
    powerful, generational, constrained, national).

% Benefits from policies that support the expansion of solar, wind, and other renewable sources, recognizing their decreasing costs and environmental benefits. They advocate for their rapid deployment.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_industry, beneficiary,
    powerful, generational, constrained, national).

% Push for aggressive decarbonization to protect ecosystems and human health. While many historically favored renewables exclusively, a growing pragmatic wing supports nuclear as part of an urgent, effective portfolio.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, environmental_advocates, beneficiary,
    organized, generational, constrained, global).

% Ultimately bear the costs and receive the benefits of energy policy. They seek reliable, affordable, and clean energy, benefiting from a stable, diversified portfolio that minimizes price volatility and environmental impact.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, energy_consumers, beneficiary,
    moderate, biographical, constrained, national).

% While a major economic actor, this constraint focuses on clean energy solutions, effectively excluding fossil fuels from the 'optimal portfolio' discussion. They actively lobby against decarbonization efforts in general.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fossil_fuel_industry, excluded,
    powerful, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__portfolio_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse energy technologies and capital investments towards the common goal of rapid and reliable decarbonization, leveraging the strengths of both dispatchable and intermittent low-carbon sources.
% TRANSFER_FUNCTION: Directs public and private capital, research & development, and regulatory support towards a balanced portfolio of nuclear and renewable energy technologies, away from fossil fuels.
% ABSENT_VOICES: Advocates for a purely renewable or purely nuclear pathway, or those for whom degrowth is the only solution, are often marginalized in the 'optimal portfolio' discussion, as their positions are seen as less pragmatic or comprehensive.
% DISAPPEARANCE_RATIONALE: If the commitment to a technology-neutral, diversified portfolio vanished, energy policy would likely revert to privileging specific technologies (e.g., all-renewables or all-nuclear), leading to less robust, potentially slower, or more expensive decarbonization pathways, and increased political conflict over energy choices.
% FOUNDING_PROBLEM: The challenge of achieving deep, rapid, and reliable decarbonization of energy systems while maintaining grid stability and economic affordability, recognizing the limitations of any single technology.
% FOUNDING_PROBLEM_CORROBORATION: International Energy Agency (IEA) reports, Intergovernmental Panel on Climate Change (IPCC) assessments, and national energy security analyses consistently highlight the need for diverse, low-carbon energy sources to meet climate goals, corroborating the problem's ongoing relevance from outside specific industry beneficiaries.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).
:- end_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.25) is low, reflecting the ideal of an efficient, coordinated solution, but not zero due to inherent transaction costs and the political economy of energy transitions. Suppression (0.15) is also low, as the constraint itself promotes inclusion of diverse technologies rather than suppressing them, though it implicitly suppresses fossil fuels. Theater ratio (0.10) is minimal, as the focus is on tangible decarbonization outcomes. Resistance (0.40) is moderate, stemming from advocates of single-technology solutions and political inertia. The measurements show slight fluctuations, reflecting the ongoing challenge of maintaining true technology neutrality amidst shifting political and economic landscapes.
 *
 * PERSPECTIVAL GAP:
 *   While the 'portfolio pragmatism' reading is presented as an optimal, neutral approach, its implementation is often contested. Advocates for 'renewable primacy' might view any inclusion of nuclear as a diversion of resources, while 'baseload necessity' proponents might argue it doesn't go far enough to guarantee grid stability. The engine's per-seat classification would highlight how different stakeholders perceive the 'neutrality' and 'optimality' of this approach based on their specific interests and positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Policy makers are the agenda setters, tasked with implementing this approach. Climate scientists act as observers, providing the evidence base. Both the nuclear and renewable energy industries are beneficiaries, as this reading ensures their inclusion in future energy plans. Environmental advocates and energy consumers are also beneficiaries, as they gain from effective, reliable decarbonization. The fossil fuel industry is structurally excluded from this solution space, as the constraint is about clean energy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_technology_neutrality,
    'Is the ''technology-neutral'' claim truly neutral in practice, or does it implicitly favor certain technologies due to existing infrastructure, lobbying, or perceived risks?',
    'Detailed analysis of capital allocation, regulatory hurdles, and public funding distribution across nuclear and renewable projects over time, compared to stated policy goals.',
    'If implicit bias is found, the constraint''s effective extractiveness and suppression might be higher for the disfavored technology, shifting its classification towards a Tangled Rope or Snare for those specific actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_technology_neutrality, empirical, 'Assessing the practical neutrality of technology-neutral energy policy.').

omega_variable(
    optimal_definition_ambiguity,
    'What criteria define ''optimal'' decarbonization (e.g., lowest cost, fastest deployment, highest reliability, greatest social equity, minimal land use)? Different definitions could lead to different portfolio compositions.',
    'Explicit policy articulation of weighted criteria for ''optimality'' and subsequent modeling of portfolio outcomes under those weights.',
    'A shift in the definition of ''optimal'' could significantly alter the perceived benefits and costs for various stakeholders, potentially reclassifying the constraint if it no longer serves the ''optimal'' goal for a majority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(optimal_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''optimal'' decarbonization.').

omega_variable(
    portfolio_robustness_vs_cost,
    'How do the costs of capital diversification and maintaining a broad technology portfolio compare to the benefits of increased grid robustness, energy security, and accelerated decarbonization?',
    'Comprehensive economic modeling comparing single-technology pathways against diversified portfolios across various future scenarios (e.g., fuel price shocks, extreme weather events).',
    'If diversification costs significantly outweigh benefits, the ''optimal'' claim weakens, potentially increasing perceived extractiveness for consumers and shifting the constraint towards a less efficient coordination type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(portfolio_robustness_vs_cost, empirical, 'Trade-off between portfolio robustness and associated costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 2020, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2020, 0.08).
narrative_ontology:measurement(clim_tr_t2025, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2025, 0.1).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2030, 0.12).
narrative_ontology:measurement(clim_tr_t2035, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2035, 0.11).
narrative_ontology:measurement(clim_tr_t2040, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2040, 0.1).
narrative_ontology:measurement(clim_tr_t2045, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2045, 0.09).
narrative_ontology:measurement(clim_tr_t2050, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2050, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2020, 0.2).
narrative_ontology:measurement(clim_be_t2025, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2025, 0.22).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2030, 0.25).
narrative_ontology:measurement(clim_be_t2035, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2035, 0.27).
narrative_ontology:measurement(clim_be_t2040, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2040, 0.28).
narrative_ontology:measurement(clim_be_t2045, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2045, 0.27).
narrative_ontology:measurement(clim_be_t2050, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2050, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2020, 0.12).
narrative_ontology:measurement(clim_su_t2025, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2025, 0.14).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2030, 0.15).
narrative_ontology:measurement(clim_su_t2035, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2035, 0.16).
narrative_ontology:measurement(clim_su_t2040, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2040, 0.15).
narrative_ontology:measurement(clim_su_t2045, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2045, 0.14).
narrative_ontology:measurement(clim_su_t2050, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2050, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
