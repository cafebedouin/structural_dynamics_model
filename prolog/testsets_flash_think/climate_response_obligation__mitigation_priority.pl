% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__mitigation_priority, []).

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
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Intergenerational Climate Mitigation Obligation
 *   domain: Climate Policy / Political Economy / Intergenerational Ethics
 *
 * SUMMARY:
 *   This constraint represents the ethical and scientific obligation to
 *   rapidly decarbonize to prevent future harm, driven by the imperative of
 *   intergenerational justice. It is the 'mitigation_priority' reading of the
 *   broader 'climate_response_obligation' kernel. This reading emphasizes
 *   proactive prevention of warming over adaptation to its effects, and
 *   prioritizes systemic change over individual consumption reduction. It
 *   demands significant costs from the current generation, particularly
 *   high-emitting nations and industries, for the benefit of future
 *   generations and vulnerable communities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.85).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.75).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.85).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Intergenerational Climate Mitigation Obligation").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "Climate Policy / Political Economy / Intergenerational Ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, 'e3ea710e-531b-4324-8562-715ede66805f').
narrative_ontology:cs_kernel_codification('e3ea710e-531b-4324-8562-715ede66805f', formalized).
narrative_ontology:cs_authority_grounding('e3ea710e-531b-4324-8562-715ede66805f', expertise).
narrative_ontology:cs_interpretation_layer_present('e3ea710e-531b-4324-8562-715ede66805f').
narrative_ontology:cs_reading_relation('e3ea710e-531b-4324-8562-715ede66805f', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('e3ea710e-531b-4324-8562-715ede66805f', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('e3ea710e-531b-4324-8562-715ede66805f', foundational, intergenerational_equity_imperative).
narrative_ontology:cs_axiom_status(intergenerational_equity_imperative, holdable).
narrative_ontology:cs_axiom_grounding('e3ea710e-531b-4324-8562-715ede66805f', intergenerational_equity_imperative, deontological).
narrative_ontology:cs_axiom('e3ea710e-531b-4324-8562-715ede66805f', foundational, precautionary_principle_application).
narrative_ontology:cs_axiom_status(precautionary_principle_application, holdable).
narrative_ontology:cs_axiom_grounding('e3ea710e-531b-4324-8562-715ede66805f', precautionary_principle_application, empirically_contingent).
narrative_ontology:cs_reference_frame('e3ea710e-531b-4324-8562-715ede66805f', precautionary_principle_framework).
narrative_ontology:cs_drift_state('e3ea710e-531b-4324-8562-715ede66805f', contemporary_political_economy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e3ea710e-531b-4324-8562-715ede66805f', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, vulnerable_communities).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_fuel_industry).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, high_emitting_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, current_global_south_citizens).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, mitigation_advocacy_groups).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, current_global_north_citizens).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, current_global_south_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Will inherit the consequences of current emissions, benefiting directly from rapid decarbonization and minimized warming. They have no direct voice in current policy decisions.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% As inhabitants of historically high-emitting nations, they are expected to bear a disproportionate share of the transition costs (e.g., higher energy prices, lifestyle changes, investment in new infrastructure) to decarbonize rapidly. They also benefit from current high-carbon lifestyles.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, current_global_north_citizens, payer,
    powerful, biographical, constrained, global).

% Are highly vulnerable to climate impacts and thus benefit significantly from mitigation. However, they also face development challenges and are asked to forgo some carbon-intensive development paths, bearing some transition costs despite lower historical emissions.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, current_global_south_citizens, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, current_global_south_citizens, payer).

% Faces the prospect of stranded assets and declining demand for its products due to rapid decarbonization. It actively resists mitigation policies and seeks to delay the transition.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_fuel_industry, payer,
    organized, immediate, constrained, global).

% Provide the scientific evidence for climate change and the ethical arguments for intergenerational justice, forming the intellectual foundation for the mitigation obligation. They advocate for policy based on their findings.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_scientists_ethicists, observer,
    analytical, generational, analytical, universal).

% Such as the UNFCCC, attempt to coordinate global mitigation efforts, set targets (e.g., Paris Agreement), and monitor national commitments. They face challenges in enforcement and securing ambitious action from sovereign states.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, international_governance_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Represent the interests of future generations and vulnerable communities, advocating for stronger and faster decarbonization policies. They mobilize public support and pressure governments and industries.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, mitigation_advocacy_groups, beneficiary,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global action to rapidly reduce greenhouse gas emissions, ensuring a stable climate for future generations and preventing catastrophic warming, based on shared scientific understanding and ethical principles.
% TRANSFER_FUNCTION: Transfers economic costs, technological shifts, and lifestyle changes from the current generation (especially high-emitting nations and industries) to avoid future environmental and social harms for future generations and vulnerable communities.
% ABSENT_VOICES: Non-human species and ecosystems, who bear direct consequences of warming but have no voice in policy. Future generations, whose interests are represented by proxies and advocates, but who cannot directly consent to the burdens or benefits.
% DISAPPEARANCE_RATIONALE: If the obligation to mitigate climate change vanished overnight, global emissions would likely accelerate without restraint, leading to severe and irreversible environmental and social collapse, fundamentally altering human civilization and the planet's ecosystems.
% FOUNDING_PROBLEM: The scientific understanding that anthropogenic greenhouse gas emissions are causing dangerous global warming, threatening the habitability of the planet for future generations, coupled with the ethical imperative to prevent foreseeable harm and ensure intergenerational equity.
% FOUNDING_PROBLEM_CORROBORATION: The Intergovernmental Panel on Climate Change (IPCC) assessment reports, national science academies worldwide, and a broad consensus among climate scientists and ethicists globally, all independent of the fossil fuel industry or high-emitting nations, consistently corroborate the problem's existence and urgency.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__mitigation_priority, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.85, rising to 0.95 by 2050) because rapid decarbonization requires fundamental economic and societal transformation, imposing substantial costs on current actors, especially those with vested interests in fossil fuels. Suppression is also high (0.75, rising to 0.87) due to the need to overcome political inertia, economic resistance, and the suppression of alternative, less costly (but ultimately catastrophic) pathways. Theater ratio is low (0.15, rising to 0.25) because the actions required are concrete and measurable (e.g., emissions reductions, renewable energy deployment), though some performative elements exist in international negotiations. The rising trend in extractiveness and suppression reflects the increasing urgency and scale of the required action as the climate crisis intensifies.
 *
 * PERSPECTIVAL GAP:
 *   The obligation is experienced very differently by its beneficiaries and victims. Future generations, while benefiting most, have no direct voice. Current high-emitting industries perceive it as an existential threat and an unfair burden, while climate scientists and ethicists view it as an urgent moral imperative. This divergence drives much of the political contestation.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and vulnerable communities are the primary beneficiaries (low d) as they are protected from severe climate impacts. The fossil fuel industry and high-emitting nations are the primary targets/victims (high d) as they bear the most significant costs of transition and asset stranding. Current citizens in general are payers, experiencing both costs and benefits. International governance bodies act as agenda-setters, attempting to enforce the obligation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intergenerational_discount_rate_ambiguity,
    'What is the ethically appropriate discount rate for future harms and benefits, and how does it affect the perceived urgency and cost-benefit ratio of mitigation?',
    'Philosophical consensus on intergenerational ethics, or policy decisions reflecting societal values regarding future welfare.',
    'A low discount rate would increase the perceived value of future benefits, justifying higher current mitigation costs. A high discount rate would diminish future benefits, making mitigation seem less urgent and more costly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_discount_rate_ambiguity, conceptual, 'Ambiguity in valuing future welfare against present costs.').

omega_variable(
    burden_sharing_equity_ambiguity,
    'How should the burden of rapid decarbonization be equitably distributed between historically high-emitting nations (Global North) and developing nations (Global South)?',
    'International negotiations leading to a globally accepted framework for ''common but differentiated responsibilities and respective capabilities'' that is perceived as fair by all parties.',
    'Unresolved inequity in burden-sharing can undermine global cooperation and enforcement, leading to slower mitigation and increased resistance from nations perceiving unfair treatment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burden_sharing_equity_ambiguity, preference, 'Equity in distributing mitigation costs.').

omega_variable(
    technological_feasibility_ambiguity,
    'Can the necessary technologies for rapid decarbonization (e.g., carbon capture, advanced renewables, sustainable agriculture) be developed and deployed at the required scale and speed without unacceptable economic disruption?',
    'Empirical observation of technological progress, cost reductions, and deployment rates over the next decade, coupled with economic modeling of transition pathways.',
    'If technological solutions prove insufficient or too costly, the extractiveness and suppression required to meet mitigation targets would increase dramatically, potentially leading to a re-evaluation of targets or a shift towards adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_feasibility_ambiguity, empirical, 'Uncertainty about the pace and cost of technological solutions for decarbonization.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the resistance to rapid decarbonization primarily structural (e.g., economic dependency on fossil fuels, lack of viable alternatives) or internalized (e.g., ideological opposition to climate action, denial of scientific consensus)?',
    'Analysis of policy effectiveness: if structural barriers are removed but resistance persists, it suggests a stronger internalized component. Public opinion surveys and political discourse analysis can also provide insight.',
    'If resistance is largely internalized, the effective suppression required to enforce the constraint is higher than structural measures suggest, as it requires overcoming deeply held beliefs and identities. If primarily structural, policy interventions can be more directly targeted at economic and technological transitions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized resistance to climate action.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 1990, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1990, climate_response_obligation__mitigation_priority, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(clim_tr_t2000, climate_response_obligation__mitigation_priority, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(clim_tr_t2010, climate_response_obligation__mitigation_priority, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(clim_tr_t2020, climate_response_obligation__mitigation_priority, theater_ratio, 2020, 0.18).
narrative_ontology:measurement(clim_tr_t2030, climate_response_obligation__mitigation_priority, theater_ratio, 2030, 0.2).
narrative_ontology:measurement(clim_tr_t2040, climate_response_obligation__mitigation_priority, theater_ratio, 2040, 0.22).
narrative_ontology:measurement(clim_tr_t2050, climate_response_obligation__mitigation_priority, theater_ratio, 2050, 0.25).

% Extraction over time
narrative_ontology:measurement(clim_be_t1990, climate_response_obligation__mitigation_priority, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(clim_be_t2000, climate_response_obligation__mitigation_priority, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(clim_be_t2010, climate_response_obligation__mitigation_priority, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(clim_be_t2020, climate_response_obligation__mitigation_priority, base_extractiveness, 2020, 0.82).
narrative_ontology:measurement(clim_be_t2030, climate_response_obligation__mitigation_priority, base_extractiveness, 2030, 0.88).
narrative_ontology:measurement(clim_be_t2040, climate_response_obligation__mitigation_priority, base_extractiveness, 2040, 0.92).
narrative_ontology:measurement(clim_be_t2050, climate_response_obligation__mitigation_priority, base_extractiveness, 2050, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1990, climate_response_obligation__mitigation_priority, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(clim_su_t2000, climate_response_obligation__mitigation_priority, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(clim_su_t2010, climate_response_obligation__mitigation_priority, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(clim_su_t2020, climate_response_obligation__mitigation_priority, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(clim_su_t2030, climate_response_obligation__mitigation_priority, suppression_requirement, 2030, 0.78).
narrative_ontology:measurement(clim_su_t2040, climate_response_obligation__mitigation_priority, suppression_requirement, 2040, 0.83).
narrative_ontology:measurement(clim_su_t2050, climate_response_obligation__mitigation_priority, suppression_requirement, 2050, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, global_infrastructure).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, global_carbon_pricing).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, renewable_energy_transition).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, fossil_fuel_subsidies_elimination).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'mitigation_priority' reading of the 'climate_response_obligation' kernel, focusing on rapid decarbonization. It is linked to sibling readings that offer alternative approaches to the same core problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
