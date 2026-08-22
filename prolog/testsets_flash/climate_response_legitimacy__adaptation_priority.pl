% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__adaptation_priority, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Climate Response Legitimacy: Adaptation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'adaptation priority' reading of
 *   legitimate climate response, where the warming trajectory is accepted,
 *   and the focus shifts to protecting vulnerable populations through
 *   resilience and adaptive capacity. This approach allows wealthy nations to
 *   largely maintain their development models and defer aggressive
 *   mitigation, while low-income regions and vulnerable populations bear the
 *   immediate impacts and the long-term costs of an adaptation deficit
 *   (estimated at $350B annually). The constraint is claimed as a Tangled
 *   Rope, reflecting its dual function of coordinating necessary adaptation
 *   efforts while extracting from vulnerable populations and future
 *   generations by deferring mitigation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.75).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Climate Response Legitimacy: Adaptation Priority Reading").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, 'c72b79e5-c795-4bbf-8529-ccf1e25cff18').
narrative_ontology:cs_kernel_codification('c72b79e5-c795-4bbf-8529-ccf1e25cff18', distributed).
narrative_ontology:cs_authority_grounding('c72b79e5-c795-4bbf-8529-ccf1e25cff18', extraction).
narrative_ontology:cs_interpretation_layer_present('c72b79e5-c795-4bbf-8529-ccf1e25cff18').
narrative_ontology:cs_reading_relation('c72b79e5-c795-4bbf-8529-ccf1e25cff18', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('c72b79e5-c795-4bbf-8529-ccf1e25cff18', climate_response_legitimacy__degrowth_transformation, influences).
narrative_ontology:cs_axiom('c72b79e5-c795-4bbf-8529-ccf1e25cff18', foundational, warming_trajectory_is_accepted).
narrative_ontology:cs_axiom_status(warming_trajectory_is_accepted, holdable).
narrative_ontology:cs_axiom_grounding('c72b79e5-c795-4bbf-8529-ccf1e25cff18', warming_trajectory_is_accepted, conventional).
narrative_ontology:cs_axiom('c72b79e5-c795-4bbf-8529-ccf1e25cff18', foundational, vulnerable_populations_require_immediate_protection).
narrative_ontology:cs_axiom_status(vulnerable_populations_require_immediate_protection, holdable).
narrative_ontology:cs_axiom_grounding('c72b79e5-c795-4bbf-8529-ccf1e25cff18', vulnerable_populations_require_immediate_protection, deontological).
narrative_ontology:cs_reference_frame('c72b79e5-c795-4bbf-8529-ccf1e25cff18', pragmatic_adaptation_framework).
narrative_ontology:cs_drift_state('c72b79e5-c795-4bbf-8529-ccf1e25cff18', contemporary_climate_negotiations, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c72b79e5-c795-4bbf-8529-ccf1e25cff18', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_nations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, fossil_fuel_industries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, global_financial_institutions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, low_income_regions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and fund adaptation measures, often through loans, while largely maintaining their current economic models and emissions trajectories. They benefit from deferring costly mitigation and maintaining economic growth, shifting the burden of climate impacts to others.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_nations, agenda_setter,
    institutional, generational, mobile, global).

% Bear the immediate and severe impacts of climate change, relying on adaptation funding that is often insufficient, conditional, or debt-creating. They have limited capacity to resist or exit their exposure to climate hazards.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, vulnerable_populations, payer,
    powerless, immediate, trapped, local).

% Experience significant adaptation deficits, requiring substantial external funding for resilience infrastructure. They are forced to accept the warming trajectory set by historical emitters and bear the costs of protecting their populations, often through increased national debt.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, low_income_regions, payer,
    organized, biographical, constrained, regional).

% Benefit from a climate response that prioritizes adaptation over aggressive mitigation, allowing for continued fossil fuel extraction and consumption. They actively lobby against policies that would constrain their operations.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, fossil_fuel_industries, beneficiary,
    institutional, biographical, arbitrage, global).

% Profit from financing adaptation projects, often through loans to vulnerable nations, which can increase debt burdens. They benefit from the continued flow of capital for climate-related investments without fundamentally altering the global economic system.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, global_financial_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Will inherit a world with higher warming trajectories and compounded climate impacts due to deferred mitigation. They bear the long-term costs of current adaptation-focused strategies without having a voice in present policy decisions.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_legitimacy__adaptation_priority, future_generations).

% Provide the scientific basis for understanding climate change and its impacts, often highlighting the limitations of adaptation without aggressive mitigation. Their warnings about long-term risks are often acknowledged but not fully integrated into policy.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_scientists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to fund and implement resilience infrastructure and adaptive capacity building in regions most affected by climate change, managing the immediate humanitarian and economic crises arising from climate impacts.
% TRANSFER_FUNCTION: Transfers financial resources, technology, and expertise from wealthier nations to vulnerable populations and low-income regions for adaptation, while simultaneously transferring the long-term costs of unmitigated warming to future generations and the immediate costs of impacts to vulnerable populations.
% ABSENT_VOICES: Future generations are structurally absent, unable to advocate for more aggressive mitigation. Indigenous communities and frontline communities, though present, are often marginalized in decision-making processes, with their traditional knowledge and specific needs underrepresented.
% DISAPPEARANCE_RATIONALE: If this approach to climate response vanished, the immediate consequence would be a catastrophic increase in human suffering and economic disruption in vulnerable regions, as existing adaptation efforts would cease. Wealthier nations would face increased migration pressures and global instability, forcing a rapid, uncoordinated, and likely more costly shift towards either aggressive mitigation or a more coercive adaptation strategy.
% FOUNDING_PROBLEM: The recognition that climate change is already causing unavoidable impacts, particularly in vulnerable regions, and that immediate action is needed to protect lives and livelihoods, even as global emissions continue.
% FOUNDING_PROBLEM_CORROBORATION: International climate reports (e.g., IPCC), humanitarian organizations, and scientific consensus corroborate the live status of climate impacts and the need for adaptation. However, the framing of adaptation as the primary legitimate response, rather than mitigation, is contested by many climate justice advocates and some scientific bodies.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__adaptation_priority, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising because the adaptation deficit grows, and the costs of climate impacts disproportionately fall on those least responsible for emissions. Suppression is also high and increasing, as vulnerable populations are trapped by their exposure and lack of resources, and their calls for climate justice and mitigation are often sidelined. The theater ratio is moderate and rising, as adaptation funding often serves to maintain the status quo of emissions rather than genuinely solving the underlying problem, with some projects being performative rather than truly effective. The increasing extractiveness and suppression reflect the growing burden on victims and the active maintenance of a system that defers fundamental change.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of wealthy nations and industries, this is a pragmatic and necessary coordination mechanism to manage an unavoidable crisis. From the perspective of vulnerable populations and future generations, it is an extractive mechanism that perpetuates injustice and defers accountability, forcing them to adapt to a crisis they did not create. The engine's classification will highlight this divergence, showing a Tangled Rope for the victims and a more Rope-like or even Scaffold-like (temporary support) classification for the beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy nations and fossil fuel industries are clear beneficiaries, as this approach allows them to continue their economic activities with less disruption. Global financial institutions also benefit from financing adaptation projects. Vulnerable populations, low-income regions, and future generations are the primary victims, bearing the direct and deferred costs of climate impacts and insufficient mitigation. The 'adaptation priority' reading structurally subsidizes the current economic models of wealthy nations at the expense of others.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_deficit_resolution,
    'Will the adaptation deficit be genuinely closed by sufficient, non-debt-creating finance, or will it continue to grow, exacerbating extraction?',
    'Tracking of international climate finance flows, assessment of adaptation project effectiveness, and analysis of debt burdens in recipient nations over the next decade.',
    'If the deficit is closed, the extractiveness from vulnerable populations would decrease, potentially shifting the constraint towards a more genuine Rope. If it grows, the constraint''s Snare-like qualities would intensify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_deficit_resolution, empirical, 'Uncertainty regarding the actual financial burden of adaptation on vulnerable populations.').

omega_variable(
    intergenerational_equity_framing,
    'Is the deferral of mitigation costs to future generations an acceptable trade-off for current adaptation, or an unacceptable intergenerational injustice?',
    'Philosophical and ethical debate, intergenerational impact assessments, and legal challenges by youth climate movements. This is a conceptual and preference-based question.',
    'If framed as unacceptable injustice, the extractiveness from future generations would be amplified, pushing the overall constraint towards a Snare. If framed as a necessary trade-off, the perceived extractiveness might be damped.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_equity_framing, conceptual, 'Ambiguity in the ethical valuation of intergenerational climate burdens.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''adaptation priority'' response, or is it a cover story for ''mitigation deferral''?',
    'Analysis of actual emissions trajectories and mitigation investments by wealthy nations over time. If mitigation efforts remain low despite adaptation funding, it supports the ''mitigation deferral'' interpretation.',
    'If reclassified as ''mitigation deferral'', the extractiveness and suppression would be seen as higher, and the coordination function would be re-evaluated as largely theatrical, pushing the constraint towards a Snare or Piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ambiguity in the true intent and function of the adaptation-first approach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_response_legitimacy__adaptation_priority, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(clim_tr_t2010, climate_response_legitimacy__adaptation_priority, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(clim_tr_t2020, climate_response_legitimacy__adaptation_priority, theater_ratio, 2020, 0.35).
narrative_ontology:measurement(clim_tr_t2030, climate_response_legitimacy__adaptation_priority, theater_ratio, 2030, 0.4).
narrative_ontology:measurement(clim_tr_t2040, climate_response_legitimacy__adaptation_priority, theater_ratio, 2040, 0.45).
narrative_ontology:measurement(clim_tr_t2050, climate_response_legitimacy__adaptation_priority, theater_ratio, 2050, 0.5).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(clim_be_t2010, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(clim_be_t2020, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(clim_be_t2030, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement(clim_be_t2040, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2040, 0.72).
narrative_ontology:measurement(clim_be_t2050, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2050, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(clim_su_t2010, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(clim_su_t2020, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(clim_su_t2030, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2030, 0.75).
narrative_ontology:measurement(clim_su_t2040, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2040, 0.8).
narrative_ontology:measurement(clim_su_t2050, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2050, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__adaptation_priority, 0.15).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_response_legitimacy' kernel. Its focus on adaptation influences the viability and perceived urgency of both mitigation and degrowth approaches by absorbing political will and financial resources.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
