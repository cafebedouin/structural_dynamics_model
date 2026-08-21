% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__degrowth_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__degrowth_sufficiency_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: climate_mitigation_legitimacy__degrowth_sufficiency_reading
 *   human_readable: Decarbonization Requires Demand Reduction (Degrowth Sufficiency Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth sufficiency' reading of the
 *   climate mitigation legitimacy kernel. It posits that effective
 *   decarbonization fundamentally requires a reduction in energy demand,
 *   thereby making large-scale generation expansion (whether fossil, nuclear,
 *   or even large-scale renewables) unnecessary. This reading challenges the
 *   dominant paradigm of green growth and technological fixes, asserting a
 *   structural necessity for societal and economic transformation towards
 *   sufficiency. The claim is presented as a 'mountain' (a structural
 *   requirement of reality), but its high extractiveness and suppression,
 *   coupled with identifiable beneficiaries and victims, position it as a
 *   false summit candidate for engine reclassification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.78).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.85).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, mountain).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Decarbonization Requires Demand Reduction (Degrowth Sufficiency Reading)").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).
domain_priors:emerges_naturally(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '113617e6-b988-44d0-8b23-e5a09a71192e').
narrative_ontology:cs_kernel_codification('113617e6-b988-44d0-8b23-e5a09a71192e', implicit).
narrative_ontology:cs_authority_grounding('113617e6-b988-44d0-8b23-e5a09a71192e', distributed).
narrative_ontology:cs_reading_relation('113617e6-b988-44d0-8b23-e5a09a71192e', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('113617e6-b988-44d0-8b23-e5a09a71192e', climate_mitigation_legitimacy__renewable_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('113617e6-b988-44d0-8b23-e5a09a71192e', climate_mitigation_legitimacy__portfolio_pragmatism_reading, forecloses).
narrative_ontology:cs_axiom('113617e6-b988-44d0-8b23-e5a09a71192e', foundational, ecological_limits_constrain_growth).
narrative_ontology:cs_axiom_status(ecological_limits_constrain_growth, holdable).
narrative_ontology:cs_axiom_grounding('113617e6-b988-44d0-8b23-e5a09a71192e', ecological_limits_constrain_growth, empirically_contingent).
narrative_ontology:cs_axiom('113617e6-b988-44d0-8b23-e5a09a71192e', foundational, demand_reduction_is_sufficient_for_decarbonization).
narrative_ontology:cs_axiom_status(demand_reduction_is_sufficient_for_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('113617e6-b988-44d0-8b23-e5a09a71192e', demand_reduction_is_sufficient_for_decarbonization, empirically_contingent).
narrative_ontology:cs_reference_frame('113617e6-b988-44d0-8b23-e5a09a71192e', ecological_sufficiency_framework).
narrative_ontology:cs_drift_state('113617e6-b988-44d0-8b23-e5a09a71192e', contemporary_growth_paradigm, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('113617e6-b988-44d0-8b23-e5a09a71192e', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_activists).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, local_communities).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, fossil_fuel_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, growth_oriented_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for this reading, seeing it as the most direct and ecologically sound path to climate stability. They benefit from the legitimacy this framing provides to their advocacy.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_activists, beneficiary,
    organized, generational, mobile, global).

% Are posited as the ultimate beneficiaries of a stable climate and reduced resource depletion, achieved through demand reduction and sufficiency.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Benefit from reduced environmental impact, less large-scale infrastructure development (e.g., power plants, transmission lines), and potentially more localized, resilient energy systems.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, local_communities, beneficiary,
    moderate, biographical, constrained, local).

% Directly targeted by any decarbonization effort, but particularly by one emphasizing demand reduction, which undermines their core business model of supplying ever-increasing energy. Their capital investments become stranded assets.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, fossil_fuel_industry, payer,
    institutional, biographical, constrained, global).

% Faces existential threat from this reading, as it deems large-scale generation (including nuclear) unnecessary. Their long-term, capital-intensive projects are de-prioritized or cancelled.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_industry, payer,
    institutional, biographical, constrained, national).

% While supporting decarbonization, they are victims of this specific reading because their business model relies on large-scale deployment of new generation capacity (wind farms, solar arrays), which this reading explicitly seeks to minimize.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_energy_developers, payer,
    powerful, biographical, constrained, global).

% The prevailing economic paradigm, which assumes continuous growth, is fundamentally challenged. This reading implies a restructuring of economic priorities and metrics, imposing significant costs on industries and nations tied to growth.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, growth_oriented_economies, payer,
    institutional, generational, constrained, global).

% Are tasked with implementing decarbonization policies. Adopting this reading would require a radical shift in policy frameworks, potentially facing strong resistance from economic and industrial lobbies.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, policy_makers, agenda_setter,
    institutional, biographical, constrained, national).

% Represent the dominant economic paradigm and are largely excluded from the 'degrowth sufficiency' discourse as a legitimate policy path. They would argue that demand reduction is economically catastrophic and unnecessary.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, economic_growth_advocates, excluded,
    powerful, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__degrowth_sufficiency_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__degrowth_sufficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate societal efforts towards reduced energy consumption and resource use, ensuring climate targets are met without relying on large-scale, capital-intensive energy infrastructure expansion.
% TRANSFER_FUNCTION: Transfers capital and resources away from large-scale energy projects (fossil, nuclear, large renewables) towards efficiency, conservation, and local, smaller-scale solutions. It also transfers societal expectations from continuous economic growth to ecological sufficiency.
% ABSENT_VOICES: Economic growth advocates, large energy infrastructure developers, and those who believe technological innovation alone will solve climate change without lifestyle changes are largely absent from the core discourse of this reading, or are actively dismissed.
% DISAPPEARANCE_RATIONALE: If the premise that decarbonization requires demand reduction vanished, climate mitigation efforts would likely continue to focus predominantly on supply-side technological solutions (large-scale renewables, nuclear, carbon capture), leading to continued large-scale energy expansion and potentially missing climate targets if demand continues to grow unchecked. The global energy economy would reorganize around a different set of priorities.
% FOUNDING_PROBLEM: The perceived failure of supply-side decarbonization strategies to adequately address climate change, coupled with concerns about resource depletion, ecological overshoot, and social equity arising from continuous economic growth.
% FOUNDING_PROBLEM_CORROBORATION: Ecological economists, some climate scientists, and degrowth advocates outside the direct beneficiaries corroborate the problem's persistence, citing planetary boundaries, resource limits, and the inadequacy of technology-only solutions. This corroboration is often contested by mainstream economists and policymakers.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, ExtMetricName, E),
    domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(climate_mitigation_legitimacy__degrowth_sufficiency_reading),
    narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the significant costs imposed on growth-dependent industries and economies by a policy of demand reduction. Suppression (0.85) is high because this reading directly challenges powerful incumbent interests and the prevailing economic paradigm, requiring active suppression of alternative (growth-oriented) decarbonization narratives and policies. Resistance (0.90) is very high due to the radical nature of the proposed shift. Accessibility collapse (0.40) is moderate, as alternative decarbonization pathways (e.g., 100% renewables, nuclear expansion) are widely promoted and perceived as viable. Theater ratio is low (0.10) because this reading is a direct, unvarnished policy proposal, not a performative maintenance of an atrophied function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of its beneficiaries, this constraint is a necessary truth for planetary survival. From the perspective of its victims, it is an economically destructive and politically unfeasible ideology. The engine's classification will highlight this divergence by likely reclassifying the claimed 'mountain' into a more extractive type, reflecting the real-world contestation and imposition of costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Climate activists, future generations, and local communities are structural beneficiaries, as they are posited to gain from a stable climate and reduced environmental impact. The fossil fuel, nuclear, and large-scale renewable energy industries, along with growth-oriented economies, are victims, as their business models and foundational assumptions are undermined. Policy makers act as agenda-setters, navigating the intense contestation. Economic growth advocates are excluded, as their core premise is rejected by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by treating the 'necessity' claim as a structural assertion (a mountain) while simultaneously documenting its high extractiveness and suppression. This allows the False Summit Mountain (FSM) detection to fire, revealing that what is claimed as a natural law is, in practice, a highly contested and extractive policy proposal. The high resistance and suppression further indicate that this 'necessity' is far from universally accepted or naturally emergent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demand_reduction_sufficiency_empirical_basis,
    'Is the empirical evidence for demand reduction alone being sufficient for decarbonization robust enough to justify foregoing large-scale generation expansion?',
    'Comprehensive, peer-reviewed energy system modeling that rigorously integrates demand-side measures with climate targets, without relying on significant new supply-side infrastructure.',
    'If insufficient, the ''mountain'' claim weakens, and the constraint might be reclassified as a ''snare'' or ''tangled_rope'' due to the imposed costs without guaranteed climate benefits. If sufficient, the ''mountain'' claim gains stronger empirical grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demand_reduction_sufficiency_empirical_basis, empirical, 'Empirical validity of demand reduction as a sole decarbonization strategy.').

omega_variable(
    societal_acceptance_of_sufficiency_paradigm,
    'Can societies genuinely embrace a sufficiency-oriented paradigm, or is the political and cultural will for such a radical shift fundamentally absent?',
    'Longitudinal studies of public opinion, policy implementation success rates, and electoral outcomes in jurisdictions attempting sufficiency-based policies. This would assess the ''resistance'' metric''s underlying drivers.',
    'If societal acceptance is low, the constraint''s high suppression and resistance are revealed as inherent to its implementation, making it more akin to a ''snare'' that must coerce behavior. If acceptance is high, it moves closer to a ''rope'' or genuine ''mountain'' of collective action.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(societal_acceptance_of_sufficiency_paradigm, empirical, 'Feasibility of societal transition to a sufficiency paradigm.').

omega_variable(
    natural_law_vs_normative_choice,
    'Is ''decarbonization requires demand reduction'' a natural law (a structural feature of reality) or a normative choice (a preferred policy pathway)?',
    'Philosophical and scientific consensus on the ''hard limits'' of planetary boundaries and resource availability, and whether these necessitate demand reduction regardless of technological advancement. This directly addresses the ''emerges_naturally'' claim.',
    'If confirmed as a natural law, the ''mountain'' classification is upheld. If revealed as a normative choice, the FSM reclassification to a ''tangled_rope'' or ''snare'' is structurally validated, highlighting the political and economic interests involved in its promotion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_normative_choice, conceptual, 'Ambiguity between natural necessity and policy preference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2030, 0.1).
narrative_ontology:measurement(clim_tr_t2040, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2040, 0.1).
narrative_ontology:measurement(clim_tr_t2050, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2050, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2020, 0.73).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2030, 0.76).
narrative_ontology:measurement(clim_be_t2040, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2040, 0.77).
narrative_ontology:measurement(clim_be_t2050, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2050, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2030, 0.83).
narrative_ontology:measurement(clim_su_t2040, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2040, 0.84).
narrative_ontology:measurement(clim_su_t2050, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2050, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_mitigation_legitimacy' kernel, focusing on demand reduction. It is structurally distinct from other readings that prioritize supply-side solutions, and its metrics reflect its unique position within the broader debate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
