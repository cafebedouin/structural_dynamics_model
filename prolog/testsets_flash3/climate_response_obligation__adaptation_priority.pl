% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__adaptation_priority, []).

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
 *   constraint_id: climate_response_obligation__adaptation_priority
 *   human_readable: Climate Response Obligation: Adaptation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'adaptation priority' reading of the
 *   broader climate response obligation kernel. It posits that 2-3°C warming
 *   is inevitable and that policy should focus on building resilience rather
 *   than costly prevention. This reading serves to defer immediate economic
 *   costs for current wealthy generations and high-carbon industries, while
 *   imposing severe burdens on future generations and vulnerable nations. The
 *   claimed type is 'tangled_rope' because it offers a coordination function
 *   (managing unavoidable impacts) but is deeply extractive due to the
 *   asymmetric distribution of costs and benefits.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.85).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.78).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.85).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Climate Response Obligation: Adaptation Priority Reading").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, '90654d73-0f7e-4c5e-9254-a39db3a6ef7e').
narrative_ontology:cs_kernel_codification('90654d73-0f7e-4c5e-9254-a39db3a6ef7e', distributed).
narrative_ontology:cs_authority_grounding('90654d73-0f7e-4c5e-9254-a39db3a6ef7e', extraction).
narrative_ontology:cs_interpretation_layer_present('90654d73-0f7e-4c5e-9254-a39db3a6ef7e').
narrative_ontology:cs_reading_relation('90654d73-0f7e-4c5e-9254-a39db3a6ef7e', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('90654d73-0f7e-4c5e-9254-a39db3a6ef7e', climate_response_obligation__degrowth_reading, influences).
narrative_ontology:cs_axiom('90654d73-0f7e-4c5e-9254-a39db3a6ef7e', foundational, economic_growth_is_paramount).
narrative_ontology:cs_axiom_status(economic_growth_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('90654d73-0f7e-4c5e-9254-a39db3a6ef7e', economic_growth_is_paramount, instrumental).
narrative_ontology:cs_axiom('90654d73-0f7e-4c5e-9254-a39db3a6ef7e', foundational, adaptation_is_pragmatic_response).
narrative_ontology:cs_axiom_status(adaptation_is_pragmatic_response, holdable).
narrative_ontology:cs_axiom_grounding('90654d73-0f7e-4c5e-9254-a39db3a6ef7e', adaptation_is_pragmatic_response, empirically_contingent).
narrative_ontology:cs_reference_frame('90654d73-0f7e-4c5e-9254-a39db3a6ef7e', current_economic_paradigm).
narrative_ontology:cs_drift_state('90654d73-0f7e-4c5e-9254-a39db3a6ef7e', contemporary_climate_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('90654d73-0f7e-4c5e-9254-a39db3a6ef7e', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, current_generation_wealthy_nations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, fossil_fuel_industries).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, high_carbon_consumers).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_nations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, vulnerable_ecosystems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Avoids immediate, costly decarbonization efforts and maintains current economic structures, shifting the burden of climate change to future generations and less developed regions. Benefits from continued high-carbon consumption and economic growth.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, current_generation_wealthy_nations, beneficiary,
    institutional, immediate, arbitrage, global).

% Protected from rapid transition costs and divestment pressures, allowing continued operation and profitability. Actively lobbies for adaptation-focused policies over mitigation.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, fossil_fuel_industries, beneficiary,
    institutional, biographical, arbitrage, global).

% Maintains current consumption patterns and lifestyles without significant disruption or increased costs associated with decarbonization. Benefits from the deferral of systemic change.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, high_carbon_consumers, beneficiary,
    organized, immediate, constrained, global).

% Will inherit a world with 2-3°C warming, facing severe and irreversible climate impacts, increased adaptation costs, and diminished natural resources. Bears the deferred costs of current inaction.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, future_generations, payer,
    powerless, generational, trapped, universal).

% Disproportionately affected by climate impacts (sea-level rise, extreme weather, resource scarcity) despite historically low emissions. Bears significant adaptation costs with limited resources and receives insufficient support from wealthy nations.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, global_south_nations, payer,
    moderate, generational, constrained, global).

% Suffers irreversible damage and biodiversity loss due to increased warming, with many species and habitats unable to adapt to the rapid changes. Bears the ultimate ecological cost.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, vulnerable_ecosystems, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__adaptation_priority, vulnerable_ecosystems).

% Provide data and projections on climate change impacts and mitigation pathways. Their warnings about the severity of 2-3°C warming are acknowledged but often downplayed in policy decisions driven by this constraint.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_scientists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global efforts towards managing the unavoidable impacts of climate change, focusing on building resilience and protective infrastructure, rather than attempting to prevent the warming itself.
% TRANSFER_FUNCTION: Transfers the immediate economic costs of decarbonization from current wealthy generations and high-carbon industries to future generations and vulnerable nations, who bear the costs of adaptation and unmitigated climate impacts.
% ABSENT_VOICES: Future generations and non-human ecosystems are structurally excluded from current policy-making, bearing the costs without representation. Indigenous communities, often on the front lines of climate impacts, are frequently marginalized in adaptation planning.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the global climate policy landscape would immediately shift towards more aggressive mitigation targets and intergenerational equity, requiring significant economic restructuring in wealthy nations and a re-evaluation of fossil fuel investments. Resource allocation for climate action would fundamentally change.
% FOUNDING_PROBLEM: The perceived high economic and social costs of rapid decarbonization, coupled with the inertia of existing energy systems and consumption patterns, made aggressive prevention seem politically unfeasible.
% FOUNDING_PROBLEM_CORROBORATION: Wealthy nations and fossil fuel industries attest that the costs of prevention remain prohibitive and that adaptation is the only pragmatic path. Climate scientists and global south nations corroborate the problem of political feasibility but dispute the inevitability of high warming and the ethical implications of prioritizing adaptation.
narrative_ontology:disappearance_verdict(climate_response_obligation__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__adaptation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_response_obligation__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__adaptation_priority, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the policy choice effectively transfers immense climate-related costs from current beneficiaries to future victims. Suppression (0.78) is also high, as this policy requires actively downplaying mitigation alternatives and suppressing the voices of those who would bear the costs. The theater ratio is low (0.1) because the commitment to adaptation is genuine, even if the framing of inevitability is contested; it's not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of current beneficiaries, this is a pragmatic and economically rational response to an 'inevitable' future. From the perspective of victims, it is a deeply unjust and extractive deferral of responsibility. The engine's classification will highlight this divergence, showing a 'tangled_rope' for the victims and potentially a 'rope' or even 'scaffold' for the beneficiaries, depending on their specific exit options and power.
 *
 * DIRECTIONALITY LOGIC:
 *   Current wealthy nations, fossil fuel industries, and high-carbon consumers are clear beneficiaries, avoiding immediate transition costs. Future generations, global south nations, and vulnerable ecosystems are the primary victims, bearing the brunt of unmitigated climate change. Climate scientists act as observers, providing data that often contradicts the 'inevitability' narrative but is selectively integrated into policy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_of_warming,
    'Is 2-3°C warming truly inevitable, or is this framing a rhetorical device to justify inaction on mitigation?',
    'Analysis of IPCC scenarios and policy pathways: if aggressive mitigation policies could still limit warming below 2°C, the inevitability claim is a rhetorical construct.',
    'If the inevitability is a construct, the constraint''s extractiveness and suppression are higher, as it actively suppresses viable alternatives. If truly inevitable, the coordination function of adaptation is more central.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_of_warming, conceptual, 'Whether the inevitability of warming is an empirical fact or a policy choice.').

omega_variable(
    intergenerational_justice_framing,
    'Is the distribution of costs and benefits across generations and regions just, or does this policy constitute intergenerational and international injustice?',
    'Ethical analysis using principles of intergenerational equity and climate justice, assessing whether the current generation''s benefits outweigh the future generations'' burdens.',
    'If deemed unjust, the constraint''s classification as ''tangled_rope'' or ''snare'' is reinforced, highlighting the moral hazard. If deemed just (e.g., by a ''least harm'' principle), the coordination aspect might be emphasized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_justice_framing, preference, 'Ethical evaluation of the intergenerational and international distribution of climate burdens.').

omega_variable(
    fossil_capital_protection,
    'To what extent is the ''adaptation priority'' narrative driven by the desire to protect fossil fuel capital and high-carbon industries from transition costs?',
    'Analysis of lobbying expenditures, policy influence, and financial ties between high-carbon industries and proponents of adaptation-first policies.',
    'Strong evidence of industry capture would increase the perceived extractiveness and suppression, potentially reclassifying the constraint closer to a ''snare'' for the victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_capital_protection, empirical, 'Role of fossil capital in shaping climate policy priorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__adaptation_priority, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t10, climate_response_obligation__adaptation_priority, theater_ratio, 10, 0.1).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__adaptation_priority, theater_ratio, 20, 0.1).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__adaptation_priority, theater_ratio, 30, 0.1).
narrative_ontology:measurement(clim_tr_t40, climate_response_obligation__adaptation_priority, theater_ratio, 40, 0.1).
narrative_ontology:measurement(clim_tr_t50, climate_response_obligation__adaptation_priority, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__adaptation_priority, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(clim_be_t10, climate_response_obligation__adaptation_priority, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__adaptation_priority, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__adaptation_priority, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(clim_be_t40, climate_response_obligation__adaptation_priority, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(clim_be_t50, climate_response_obligation__adaptation_priority, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__adaptation_priority, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(clim_su_t10, climate_response_obligation__adaptation_priority, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__adaptation_priority, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__adaptation_priority, suppression_requirement, 30, 0.77).
narrative_ontology:measurement(clim_su_t40, climate_response_obligation__adaptation_priority, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(clim_su_t50, climate_response_obligation__adaptation_priority, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, global_carbon_markets).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, international_development_aid).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, fossil_fuel_subsidies).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
