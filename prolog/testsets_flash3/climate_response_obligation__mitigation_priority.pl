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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Climate Response Obligation: Mitigation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'mitigation priority' reading of the
 *   broader climate response obligation, emphasizing rapid decarbonization to
 *   prevent future harm, driven by intergenerational justice. It posits that
 *   current generations, particularly in the Global North, bear a moral and
 *   practical obligation to minimize warming by transitioning away from
 *   fossil fuels, incurring significant costs in the process. Future
 *   generations and climate-vulnerable nations are the primary beneficiaries,
 *   while fossil fuel industries and high-emitting economies are the primary
 *   payers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.65).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.4).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Climate Response Obligation: Mitigation Priority Reading").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, '142cc35e-d724-45f6-aa7d-95b6e6629edd').
narrative_ontology:cs_kernel_codification('142cc35e-d724-45f6-aa7d-95b6e6629edd', formalized).
narrative_ontology:cs_authority_grounding('142cc35e-d724-45f6-aa7d-95b6e6629edd', lineage).
narrative_ontology:cs_interpretation_layer_present('142cc35e-d724-45f6-aa7d-95b6e6629edd').
narrative_ontology:cs_reading_relation('142cc35e-d724-45f6-aa7d-95b6e6629edd', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('142cc35e-d724-45f6-aa7d-95b6e6629edd', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('142cc35e-d724-45f6-aa7d-95b6e6629edd', foundational, intergenerational_justice_demands_minimizing_warming).
narrative_ontology:cs_axiom_status(intergenerational_justice_demands_minimizing_warming, holdable).
narrative_ontology:cs_axiom_grounding('142cc35e-d724-45f6-aa7d-95b6e6629edd', intergenerational_justice_demands_minimizing_warming, deontological).
narrative_ontology:cs_axiom('142cc35e-d724-45f6-aa7d-95b6e6629edd', foundational, rapid_decarbonization_is_primary_means_to_minimize_warming).
narrative_ontology:cs_axiom_status(rapid_decarbonization_is_primary_means_to_minimize_warming, holdable).
narrative_ontology:cs_axiom_grounding('142cc35e-d724-45f6-aa7d-95b6e6629edd', rapid_decarbonization_is_primary_means_to_minimize_warming, empirically_contingent).
narrative_ontology:cs_reference_frame('142cc35e-d724-45f6-aa7d-95b6e6629edd', scientific_consensus_on_1_5c_limit).
narrative_ontology:cs_drift_state('142cc35e-d724-45f6-aa7d-95b6e6629edd', contemporary_policy_inertia, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('142cc35e-d724-45f6-aa7d-95b6e6629edd', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, global_south_nations).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_fuel_industries).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, high_emitting_consumers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_north_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Primary beneficiaries of minimized warming and avoided climate catastrophe. They bear no costs in the present but are entirely dependent on current actions for their future well-being. Their 'exit' is non-existence or a degraded planet.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, beneficiary,
    powerless, generational, trapped, universal).

% Benefit from mitigation efforts as they are disproportionately vulnerable to climate impacts despite low historical emissions. They advocate for rapid decarbonization and climate finance, bearing some adaptation costs but demanding mitigation from others.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_south_nations, beneficiary,
    organized, generational, constrained, global).

% Bear significant costs through stranded assets, carbon pricing, and regulatory restrictions on their core business model. Their 'exit' involves a complete transformation of their operations or eventual obsolescence.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_fuel_industries, payer,
    institutional, biographical, constrained, global).

% Bear costs through higher energy prices, taxes on carbon-intensive goods, and lifestyle changes (e.g., reduced air travel, electric vehicle mandates). Their 'exit' is to resist policy changes or find less carbon-intensive alternatives.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, high_emitting_consumers, payer,
    moderate, immediate, constrained, national).

% Bear the largest share of transition costs due to historical emissions and higher per-capita footprints. This includes investments in renewable energy, infrastructure upgrades, and potential economic restructuring. Their 'exit' is to delay or dilute mitigation efforts.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_north_economies, payer,
    institutional, biographical, constrained, global).

% Provide the scientific basis for understanding climate change and the urgency of mitigation. They do not directly benefit or pay but their findings drive the policy imperative and inform the debate.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_scientists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global efforts to reduce greenhouse gas emissions, requiring collective action from nations, industries, and individuals to achieve a stable climate future, preventing a tragedy of the commons for atmospheric resources.
% TRANSFER_FUNCTION: Transfers economic resources, technological innovation, and behavioral changes from current high-emitting entities (industries, consumers, Global North nations) to future generations and climate-vulnerable regions (Global South nations) in the form of avoided climate damages.
% ABSENT_VOICES: Future generations are structurally absent from current policy debates, their interests represented by advocates. Non-human species and ecosystems, which are profoundly impacted, also lack direct representation.
% DISAPPEARANCE_RATIONALE: If the obligation to prioritize mitigation vanished, global decarbonization efforts would slow dramatically, leading to higher warming trajectories, increased climate disasters, and severe intergenerational and geopolitical conflict over resource scarcity and displacement. The global economy and social structures would be forced to rearrange under extreme environmental stress.
% FOUNDING_PROBLEM: The problem of anthropogenic climate change, driven by greenhouse gas emissions, threatening long-term planetary habitability and human well-being, particularly for future generations and vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus (IPCC reports, national academies of science) overwhelmingly corroborates the existence and urgency of the climate problem. International bodies (UNFCCC) and a broad coalition of civil society organizations and vulnerable nations also attest to its live status, independent of the fossil fuel industry's counter-narratives.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__mitigation_priority, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) reflects the substantial economic and social costs imposed on current high-emitting actors for the benefit of future generations. Suppression (0.40) is moderate, as the constraint relies on a mix of international agreements, national policies, and social pressure, but faces significant resistance from entrenched interests. Theater ratio (0.20) indicates that while some 'greenwashing' and performative actions exist, there are also genuine, impactful mitigation efforts. The rising trend in extractiveness and suppression over time reflects increasing policy stringency and the growing urgency of the climate crisis.
 *
 * PERSPECTIVAL GAP:
 *   The 'mitigation priority' reading is experienced as an urgent moral imperative by future generations and vulnerable nations, while it is perceived as an economic burden and threat to sovereignty by fossil fuel industries and some Global North political factions. The engine's per-seat classification will reflect this divergence, showing a 'rope' or 'scaffold' for beneficiaries and a 'snare' or 'tangled_rope' for payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are full beneficiaries (d=0.0) as they receive the benefits without current cost. Global South nations are also beneficiaries (d low) due to their vulnerability and low historical emissions. Fossil fuel industries, high-emitting consumers, and Global North economies are targets (d high) as they bear the direct costs of decarbonization and economic restructuring. Climate scientists are analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intergenerational_discount_rate,
    'What is the appropriate discount rate for future harms and benefits in climate policy, and how does it affect the perceived burden of mitigation?',
    'Ethical and economic consensus building on intergenerational equity, potentially informed by long-term social welfare functions.',
    'A low discount rate (prioritizing future generations) would increase the perceived urgency and justification for high current extraction for mitigation. A high discount rate would favor present consumption and adaptation over costly prevention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_discount_rate, preference, 'The ethical weighting of future well-being against present costs.').

omega_variable(
    burden_sharing_equity,
    'What constitutes an equitable distribution of mitigation burdens between the Global North (historical emitters) and Global South (vulnerable, low emitters)?',
    'International negotiations and agreements (e.g., UNFCCC processes) that establish mechanisms for climate finance, technology transfer, and differentiated responsibilities.',
    'Resolution would clarify the ''payer'' set and the magnitude of extraction for specific national economies, potentially shifting the classification for some Global North nations from ''payer'' to ''beneficiary'' if they receive sufficient climate finance for their own transitions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_sharing_equity, conceptual, 'Fairness in allocating climate mitigation costs.').

omega_variable(
    technological_breakthrough_impact,
    'Will unforeseen technological breakthroughs (e.g., cheap, scalable carbon capture) significantly reduce the cost and perceived extractiveness of rapid decarbonization?',
    'Empirical observation of technological development and deployment trajectories over the next 10-20 years.',
    'If such breakthroughs occur, the perceived extractiveness for current payers would decrease, potentially shifting the constraint''s classification towards a ''rope'' or ''scaffold'' as the burden lessens. If not, the current high extractiveness remains justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_breakthrough_impact, empirical, 'Impact of future technology on mitigation costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__mitigation_priority, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t10, climate_response_obligation__mitigation_priority, theater_ratio, 10, 0.13).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__mitigation_priority, theater_ratio, 20, 0.16).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__mitigation_priority, theater_ratio, 30, 0.18).
narrative_ontology:measurement(clim_tr_t40, climate_response_obligation__mitigation_priority, theater_ratio, 40, 0.19).
narrative_ontology:measurement(clim_tr_t50, climate_response_obligation__mitigation_priority, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__mitigation_priority, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(clim_be_t10, climate_response_obligation__mitigation_priority, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__mitigation_priority, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__mitigation_priority, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(clim_be_t40, climate_response_obligation__mitigation_priority, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(clim_be_t50, climate_response_obligation__mitigation_priority, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__mitigation_priority, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t10, climate_response_obligation__mitigation_priority, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__mitigation_priority, suppression_requirement, 20, 0.36).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__mitigation_priority, suppression_requirement, 30, 0.38).
narrative_ontology:measurement(clim_su_t40, climate_response_obligation__mitigation_priority, suppression_requirement, 40, 0.39).
narrative_ontology:measurement(clim_su_t50, climate_response_obligation__mitigation_priority, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, global_infrastructure).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_response_obligation' kernel, focusing on mitigation. It is structurally distinct from 'adaptation_priority' and 'degrowth_reading', which represent alternative approaches to the same overarching climate challenge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
