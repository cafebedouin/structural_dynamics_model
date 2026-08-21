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
 *   climate mitigation legitimacy kernel. It asserts that optimal
 *   decarbonization requires a technology-neutral approach, integrating both
 *   nuclear and various renewable energy sources, adapted to regional
 *   contexts. This reading prioritizes efficiency, reliability, and speed of
 *   emissions reduction over ideological or single-technology preferences. It
 *   implicitly extracts from advocates of exclusive technology pathways by
 *   requiring compromise and diversification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.65).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.7).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Optimal Decarbonization via Technology-Neutral Portfolio").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '6cdf3d59-344e-428a-a784-47104b22a6ae').
narrative_ontology:cs_kernel_codification('6cdf3d59-344e-428a-a784-47104b22a6ae', formalized).
narrative_ontology:cs_authority_grounding('6cdf3d59-344e-428a-a784-47104b22a6ae', expertise).
narrative_ontology:cs_interpretation_layer_present('6cdf3d59-344e-428a-a784-47104b22a6ae').
narrative_ontology:cs_reading_relation('6cdf3d59-344e-428a-a784-47104b22a6ae', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6cdf3d59-344e-428a-a784-47104b22a6ae', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('6cdf3d59-344e-428a-a784-47104b22a6ae', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('6cdf3d59-344e-428a-a784-47104b22a6ae', foundational, technology_neutrality_for_decarbonization).
narrative_ontology:cs_axiom_status(technology_neutrality_for_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('6cdf3d59-344e-428a-a784-47104b22a6ae', technology_neutrality_for_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('6cdf3d59-344e-428a-a784-47104b22a6ae', foundational, cost_effectiveness_maximization).
narrative_ontology:cs_axiom_status(cost_effectiveness_maximization, holdable).
narrative_ontology:cs_axiom_grounding('6cdf3d59-344e-428a-a784-47104b22a6ae', cost_effectiveness_maximization, instrumental).
narrative_ontology:cs_reference_frame('6cdf3d59-344e-428a-a784-47104b22a6ae', evidence_based_climate_policy).
narrative_ontology:cs_drift_state('6cdf3d59-344e-428a-a784-47104b22a6ae', contemporary_energy_transition, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6cdf3d59-344e-428a-a784-47104b22a6ae', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, energy_planners).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_energy_companies).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, pure_renewable_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, pure_nuclear_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_scientists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for designing and implementing energy policies to achieve decarbonization targets. They must balance various interests and scientific advice to mandate a technology-neutral approach, often facing political pressure from single-technology lobbies.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, energy_policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Provide the scientific basis for decarbonization targets and assess the efficacy of various energy technologies. This reading aligns with their objective of finding the most effective path to emissions reduction, unconstrained by ideological preferences.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_scientists, beneficiary,
    analytical, generational, analytical, global).

% Companies with investments across various energy sectors (renewables, nuclear, grid infrastructure) benefit from a technology-neutral policy that encourages a balanced portfolio, allowing them to leverage existing assets and diversify risk.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_energy_companies, beneficiary,
    powerful, biographical, mobile, global).

% Advocate for an energy system based solely on renewable sources. They bear the 'cost' of this constraint by having to accept nuclear power as part of the optimal mix, diverting some capital and political will from their preferred path.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, pure_renewable_advocates, payer,
    organized, biographical, constrained, national).

% Advocate for nuclear power as the primary solution. They bear the 'cost' of this constraint by having to accept renewables as part of the optimal mix, diverting some capital and political will from their preferred path.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, pure_nuclear_advocates, payer,
    organized, biographical, constrained, national).

% Their interests are fundamentally opposed to decarbonization. While not directly paying into this specific constraint, they are excluded from the policy conversation it defines and actively work to undermine its premises.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fossil_fuel_lobby, excluded,
    institutional, biographical, arbitrage, global).

% Evaluate the cost-effectiveness and feasibility of different energy portfolios. Their analysis often supports the pragmatic, technology-neutral approach, but they do not directly implement or enforce the policy.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, economic_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__portfolio_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse energy technologies (nuclear, various renewables) and investment strategies to achieve the most efficient and reliable decarbonization pathway, balancing cost, reliability, and speed.
% TRANSFER_FUNCTION: Transfers political and financial capital towards a balanced, diversified energy portfolio, away from exclusive investment in or advocacy for single-technology solutions.
% ABSENT_VOICES: The fossil_fuel_lobby is structurally excluded from a constraint focused on decarbonization. They would argue against the urgency of decarbonization or for continued reliance on fossil fuels, but their position is antithetical to the constraint's premise.
% DISAPPEARANCE_RATIONALE: If the imperative for a technology-neutral portfolio vanished, energy policy would likely swing towards single-technology mandates (e.g., 'all renewables' or 'all nuclear'), leading to suboptimal decarbonization outcomes, increased costs, or grid instability. The global energy transition would become less efficient and more contentious.
% FOUNDING_PROBLEM: The urgent need for deep, rapid, and reliable decarbonization of global energy systems, facing diverse regional conditions, technological uncertainties, and political resistance to any single solution.
% FOUNDING_PROBLEM_CORROBORATION: The Intergovernmental Panel on Climate Change (IPCC) reports, national academies of sciences, and independent energy agencies consistently corroborate the urgency and complexity of the decarbonization challenge, supporting the need for pragmatic, multi-faceted approaches.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The base extractiveness (0.65) reflects the 'cost' imposed on single-technology advocates who must compromise their preferred approach and accept diversification. Suppression (0.70) is high because active policy enforcement is required to counter strong lobbying and political pressure for single-technology mandates. The theater ratio (0.20) is low, as the constraint is primarily driven by pragmatic, results-oriented goals, with minimal performative maintenance. Accessibility collapse is moderate (0.50) as it opens some technological avenues while closing others (e.g., pure single-technology paths). Resistance is moderate (0.60) from both pro-nuclear and pro-renewable exclusive camps.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of energy policy makers and climate scientists, this constraint is a necessary coordination mechanism for effective decarbonization. From the perspective of single-technology advocates, it is an extractive imposition that forces them to dilute their preferred solutions and accept less optimal (in their view) alternatives. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Climate mitigation advocates, energy planners, and diversified energy companies are beneficiaries (low d) as this approach aligns with their goals or business models. Pure renewable and pure nuclear advocates are targets (high d) as they are compelled to accept technologies they might prefer to exclude. The fossil fuel lobby is excluded, as their interests are antithetical to the constraint's core purpose.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''portfolio pragmatism'' reading, or is it a cover for specific technology interests?',
    'Analysis of policy outcomes: if the portfolio consistently favors one technology over another despite evidence of sub-optimality, reclassify as a ''captured'' reading.',
    'If captured, the constraint''s effective extraction would be higher, benefiting specific technology lobbies rather than collective decarbonization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing genuine pragmatism from disguised technological capture within the climate mitigation legitimacy kernel.').

omega_variable(
    optimal_mix_regional_variation,
    'What constitutes the ''optimal'' technology mix, and how much does it vary by region and over time?',
    'Detailed regional energy system modeling and ongoing empirical assessment of technology costs, performance, and grid integration challenges.',
    'If regional variation is extreme, a ''one-size-fits-all'' portfolio approach could become extractive; if a consistent optimal mix emerges, the constraint''s coordination function is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(optimal_mix_regional_variation, empirical, 'Uncertainty regarding the precise composition and dynamic nature of the ''optimal'' technology portfolio.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 20, 0.7).


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
