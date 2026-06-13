% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__adaptation_priority, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_harm_prevention__adaptation_priority
 *   human_readable: Climate Adaptation Priority Framework
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint describes a climate policy framework that prioritizes
 *   adaptation and resilience building in the near term, implicitly accepting
 *   a higher global warming trajectory due to the perceived political and
 *   economic infeasibility of aggressive mitigation. It is one reading of the
 *   'climate_harm_prevention' kernel, specifically the 'adaptation_priority'
 *   reading. Sibling readings include 'mitigation_priority' and
 *   'degrowth_reading', which propose different primary responses to climate
 *   change.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.65).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.7).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Climate Adaptation Priority Framework").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, 'a123927c-4d4a-49da-b081-d67d2c75b45a').
narrative_ontology:cs_kernel_codification('a123927c-4d4a-49da-b081-d67d2c75b45a', distributed).
narrative_ontology:cs_authority_grounding('a123927c-4d4a-49da-b081-d67d2c75b45a', extraction).
narrative_ontology:cs_interpretation_layer_present('a123927c-4d4a-49da-b081-d67d2c75b45a').
narrative_ontology:cs_reading_relation('a123927c-4d4a-49da-b081-d67d2c75b45a', climate_harm_prevention__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('a123927c-4d4a-49da-b081-d67d2c75b45a', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('a123927c-4d4a-49da-b081-d67d2c75b45a', foundational, near_term_resilience_is_primary_duty).
narrative_ontology:cs_axiom_status(near_term_resilience_is_primary_duty, holdable).
narrative_ontology:cs_axiom_grounding('a123927c-4d4a-49da-b081-d67d2c75b45a', near_term_resilience_is_primary_duty, deontological).
narrative_ontology:cs_axiom('a123927c-4d4a-49da-b081-d67d2c75b45a', foundational, deep_mitigation_is_politically_infeasible).
narrative_ontology:cs_axiom_status(deep_mitigation_is_politically_infeasible, holdable).
narrative_ontology:cs_axiom_grounding('a123927c-4d4a-49da-b081-d67d2c75b45a', deep_mitigation_is_politically_infeasible, empirically_contingent).
narrative_ontology:cs_reference_frame('a123927c-4d4a-49da-b081-d67d2c75b45a', pragmatic_adaptation_first).
narrative_ontology:cs_drift_state('a123927c-4d4a-49da-b081-d67d2c75b45a', contemporary_climate_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a123927c-4d4a-49da-b081-d67d2c75b45a', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, global_north_economies).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directly benefit from immediate resilience investments (e.g., sea walls, early warning systems) that protect against current climate impacts, but remain exposed to unmitigated long-term warming.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Avoid politically and economically costly near-term mitigation efforts, allowing continued economic growth and fossil fuel reliance, while contributing to adaptation funds. They benefit from deferring systemic change.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, global_north_economies, beneficiary,
    institutional, biographical, mobile, global).

% Bear the long-term, unmitigated costs of a higher warming trajectory, including more severe and widespread climate impacts, resource scarcity, and ecological collapse, due to insufficient present-day mitigation.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, generational, trapped, universal).

% Receive some adaptation funding but face disproportionate and unavoidable harm from climate change due to their limited resources and geographic vulnerability, which adaptation alone cannot fully address.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions, payer,
    powerless, generational, trapped, regional).

% Provide projections of climate impacts and evaluate the efficacy of adaptation versus mitigation strategies. Their warnings about long-term warming are acknowledged but often deprioritized in policy decisions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, climate_scientists, observer,
    analytical, civilizational, analytical, global).

% Argue for aggressive emissions reductions and systemic change, but their proposals are often deemed politically or economically unfeasible within the adaptation-first framework, leading to their marginalization in policy debates.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, mitigation_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international funding and technical assistance for resilience projects, allowing nations to respond to immediate climate threats and share best practices for adaptation.
% TRANSFER_FUNCTION: Transfers financial resources and technical expertise from wealthier nations to vulnerable regions for adaptation projects, while transferring the burden of long-term climate impacts to future generations and less resilient regions.
% ABSENT_VOICES: Future generations and ecosystems, who bear the unmitigated costs of this approach, have no direct voice in current policy. Mitigation advocates are often sidelined, their arguments for systemic change deemed impractical.
% DISAPPEARANCE_RATIONALE: If this framework vanished, the immediate focus on adaptation would likely collapse, leading to a scramble for short-term disaster response. However, it might also open space for more aggressive mitigation strategies to gain traction, fundamentally altering the global climate policy landscape.
% FOUNDING_PROBLEM: The immediate and visible impacts of climate change were causing widespread suffering and economic damage, while deep mitigation was perceived as politically and economically unachievable in the short term.
% FOUNDING_PROBLEM_CORROBORATION: International aid organizations and local communities in vulnerable regions corroborate the urgency of immediate adaptation needs. Political leaders in developed nations corroborate the perceived infeasibility of rapid, deep mitigation. Climate scientists corroborate the ongoing and escalating nature of climate impacts.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates immediate climate response (benefiting present vulnerable populations) but simultaneously extracts from future generations and low-adaptation-capacity regions by deferring costly mitigation. Extractiveness (0.65) is high due to the intergenerational transfer of climate debt. Suppression (0.70) is significant, as political and economic narratives actively suppress alternatives like rapid decarbonization or degrowth. Theater ratio (0.20) is moderate; while adaptation efforts are real, they can also serve as a performative substitute for deeper systemic change.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of present vulnerable populations, this framework offers crucial, life-saving support. From the perspective of future generations, it represents a profound intergenerational injustice. The engine's classification will highlight this divergence, showing a 'Rope' for the immediate beneficiaries and a 'Snare' for the long-term victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Present vulnerable populations are beneficiaries of immediate adaptation efforts (low d). Global North economies are also beneficiaries, as they avoid disruptive mitigation costs (low d). Future generations and low-adaptation-capacity regions are clear victims, bearing the brunt of unmitigated warming (high d). Climate scientists are observers, and mitigation advocates are excluded, as their proposals are deemed outside the 'feasible' policy space.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intergenerational_equity_valuation,
    'How should the welfare of future generations be weighted against the immediate costs and benefits to present generations in climate policy decisions?',
    'Ethical and economic frameworks for intergenerational discounting and justice, potentially informed by citizen assemblies or deliberative democracy processes.',
    'A higher weighting for future generations would reclassify this constraint towards a Snare, highlighting the extraction. A lower weighting would reinforce its Rope-like coordination function for the present.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_equity_valuation, preference, 'The ethical valuation of intergenerational equity in climate policy.').

omega_variable(
    political_economic_infeasibility_claim,
    'Is aggressive climate mitigation truly politically and economically infeasible, or is this claim a rationalization for maintaining existing extractive economic structures?',
    'Empirical analysis of historical transitions, technological readiness assessments, and political economy studies of vested interests in fossil fuel industries.',
    'If infeasibility is disproven, the ''coordination'' aspect of this constraint diminishes, and its extractive nature (deferring costs) becomes more prominent, pushing it closer to a Snare. If proven, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_economic_infeasibility_claim, empirical, 'The veracity of claims regarding the infeasibility of climate mitigation.').

omega_variable(
    adaptation_vs_mitigation_efficacy,
    'To what extent can adaptation truly prevent harm from climate change, given a higher warming trajectory, versus the harm prevented by mitigation?',
    'Integrated assessment models comparing outcomes under different adaptation and mitigation scenarios, and empirical studies of adaptation limits.',
    'If adaptation is found to be largely insufficient at higher warming levels, the ''beneficiary'' status of present vulnerable populations becomes more tenuous, and the constraint''s overall extractiveness increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_vs_mitigation_efficacy, empirical, 'The relative efficacy and limits of adaptation versus mitigation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_harm_prevention__adaptation_priority, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(clim_tr_t2010, climate_harm_prevention__adaptation_priority, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__adaptation_priority, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(clim_tr_t2030, climate_harm_prevention__adaptation_priority, theater_ratio, 2030, 0.25).
narrative_ontology:measurement_basis(clim_tr_t2030, projected).
narrative_ontology:measurement(clim_tr_t2040, climate_harm_prevention__adaptation_priority, theater_ratio, 2040, 0.3).
narrative_ontology:measurement_basis(clim_tr_t2040, projected).
narrative_ontology:measurement(clim_tr_t2050, climate_harm_prevention__adaptation_priority, theater_ratio, 2050, 0.35).
narrative_ontology:measurement_basis(clim_tr_t2050, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_harm_prevention__adaptation_priority, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(clim_be_t2010, climate_harm_prevention__adaptation_priority, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__adaptation_priority, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(clim_be_t2030, climate_harm_prevention__adaptation_priority, base_extractiveness, 2030, 0.7).
narrative_ontology:measurement_basis(clim_be_t2030, projected).
narrative_ontology:measurement(clim_be_t2040, climate_harm_prevention__adaptation_priority, base_extractiveness, 2040, 0.73).
narrative_ontology:measurement_basis(clim_be_t2040, projected).
narrative_ontology:measurement(clim_be_t2050, climate_harm_prevention__adaptation_priority, base_extractiveness, 2050, 0.75).
narrative_ontology:measurement_basis(clim_be_t2050, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_harm_prevention__adaptation_priority, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(clim_su_t2010, climate_harm_prevention__adaptation_priority, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__adaptation_priority, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(clim_su_t2030, climate_harm_prevention__adaptation_priority, suppression_requirement, 2030, 0.75).
narrative_ontology:measurement_basis(clim_su_t2030, projected).
narrative_ontology:measurement(clim_su_t2040, climate_harm_prevention__adaptation_priority, suppression_requirement, 2040, 0.78).
narrative_ontology:measurement_basis(clim_su_t2040, projected).
narrative_ontology:measurement(clim_su_t2050, climate_harm_prevention__adaptation_priority, suppression_requirement, 2050, 0.8).
narrative_ontology:measurement_basis(clim_su_t2050, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'climate_harm_prevention' kernel, each representing a distinct policy approach to climate change. They are linked to highlight their competing and influencing relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
