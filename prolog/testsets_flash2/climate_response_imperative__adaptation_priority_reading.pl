% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__adaptation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__adaptation_priority_reading, []).

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
 *   constraint_id: climate_response_imperative__adaptation_priority_reading
 *   human_readable: Climate Response Imperative: Adaptation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This constraint represents the 'adaptation priority' reading of the
 *   broader climate response imperative. It frames climate action primarily
 *   as building resilience and reducing damage in regions already exposed to
 *   climate impacts, while treating global emissions reduction (mitigation)
 *   as a secondary, aspirational goal. This reading is often favored by
 *   developed nations and industries that benefit from delaying costly
 *   mitigation, effectively shifting the burden of climate change onto
 *   developing nations and vulnerable communities who are least responsible
 *   for historical emissions. The constraint is claimed as a Tangled Rope,
 *   reflecting its dual function of coordinating immediate responses while
 *   simultaneously extracting from vulnerable populations by deferring
 *   systemic change.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, 0.68).
domain_priors:suppression_score(climate_response_imperative__adaptation_priority_reading, 0.75).
domain_priors:theater_ratio(climate_response_imperative__adaptation_priority_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__adaptation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__adaptation_priority_reading, "Climate Response Imperative: Adaptation Priority Reading").
narrative_ontology:topic_domain(climate_response_imperative__adaptation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__adaptation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__adaptation_priority_reading, '6d87de66-9502-4c67-886a-9e46839e1dc4').
narrative_ontology:cs_kernel_codification('6d87de66-9502-4c67-886a-9e46839e1dc4', formalized).
narrative_ontology:cs_authority_grounding('6d87de66-9502-4c67-886a-9e46839e1dc4', extraction).
narrative_ontology:cs_interpretation_layer_present('6d87de66-9502-4c67-886a-9e46839e1dc4').
narrative_ontology:cs_reading_relation('6d87de66-9502-4c67-886a-9e46839e1dc4', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d87de66-9502-4c67-886a-9e46839e1dc4', climate_response_imperative__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('6d87de66-9502-4c67-886a-9e46839e1dc4', foundational, adaptation_is_primary_response).
narrative_ontology:cs_axiom_status(adaptation_is_primary_response, holdable).
narrative_ontology:cs_axiom_grounding('6d87de66-9502-4c67-886a-9e46839e1dc4', adaptation_is_primary_response, conventional).
narrative_ontology:cs_axiom('6d87de66-9502-4c67-886a-9e46839e1dc4', foundational, mitigation_is_aspirational_not_urgent).
narrative_ontology:cs_axiom_status(mitigation_is_aspirational_not_urgent, holdable).
narrative_ontology:cs_axiom_grounding('6d87de66-9502-4c67-886a-9e46839e1dc4', mitigation_is_aspirational_not_urgent, conventional).
narrative_ontology:cs_reference_frame('6d87de66-9502-4c67-886a-9e46839e1dc4', pragmatic_response_to_impacts).
narrative_ontology:cs_drift_state('6d87de66-9502-4c67-886a-9e46839e1dc4', contemporary_climate_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6d87de66-9502-4c67-886a-9e46839e1dc4', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__adaptation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, global_north_developed_nations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, fossil_fuel_industries).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, global_south_developing_nations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, vulnerable_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from deferring costly mitigation efforts, allowing continued economic growth based on existing energy infrastructure. They advocate for adaptation funding but resist binding mitigation targets, shifting the immediate burden to exposed regions.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, global_north_developed_nations, beneficiary,
    institutional, generational, arbitrage, global).

% Directly benefit from the delayed and aspirational nature of mitigation, allowing continued operation and expansion. They actively lobby against stringent emissions regulations and promote adaptation as the primary response.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, fossil_fuel_industries, beneficiary,
    organized, biographical, mobile, global).

% Bear the immediate and escalating costs of climate impacts and are forced to prioritize resilience-building with limited resources. They are least responsible for historical emissions but face the most severe consequences, creating a vicious circle of debt and vulnerability.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, global_south_developing_nations, payer,
    powerless, generational, trapped, global).

% Face direct threats to livelihoods, homes, and cultural heritage from climate change. They are forced to adapt or relocate, often without adequate support, and have minimal influence over global climate policy decisions.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, vulnerable_communities, payer,
    powerless, immediate, identity_locked, local).

% Provide the scientific basis for understanding climate change and its impacts, consistently advocating for both urgent mitigation and adaptation. Their warnings about the limits of adaptation are often downplayed or ignored by political actors.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, climate_scientists, observer,
    analytical, civilizational, analytical, universal).

% Administer and disburse funds for climate adaptation projects, often through loans that increase the debt burden of developing nations. Their policies shape the implementation of adaptation strategies and can reinforce the priority of adaptation over mitigation.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, international_financial_institutions, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to address the immediate and visible impacts of climate change, channeling resources towards disaster preparedness, infrastructure hardening, and ecosystem restoration in affected regions.
% TRANSFER_FUNCTION: Transfers the primary burden of climate response from global emissions reduction (mitigation) to local damage control and resilience-building (adaptation), effectively shifting costs from historical emitters to vulnerable populations.
% ABSENT_VOICES: Future generations, who will inherit a world with higher temperatures and more severe impacts due to delayed mitigation, are structurally absent from current policy-making. Indigenous communities, whose traditional lands and ways of life are disproportionately affected, are often marginalized in adaptation planning.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority imperative vanished, the global climate policy landscape would immediately shift. Developed nations would face immense pressure for aggressive mitigation, fossil fuel industries would lose their primary justification for continued operation, and developing nations would demand reparations and a more equitable distribution of climate responsibility. The current financial flows and political alliances would be fundamentally reorganized.
% FOUNDING_PROBLEM: The immediate and visible impacts of climate change (extreme weather, sea-level rise, desertification) were causing widespread suffering and economic damage, requiring urgent, localized responses.
% FOUNDING_PROBLEM_CORROBORATION: Vulnerable communities and climate scientists universally attest that the problem of climate impacts is live and escalating. Developed nations and international financial institutions also acknowledge the problem, using it to justify adaptation funding, though their framing often downplays the role of mitigation.
narrative_ontology:disappearance_verdict(climate_response_imperative__adaptation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__adaptation_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__adaptation_priority_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_response_imperative__adaptation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__adaptation_priority_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__adaptation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__adaptation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because this reading allows developed nations to externalize the costs of climate change, forcing developing nations to divert scarce resources to adaptation rather than development. Suppression (0.75) is also high, as the global political and economic structures effectively suppress calls for equitable mitigation and climate justice, channeling discourse and funding towards adaptation. The theater ratio (0.4) indicates that while genuine adaptation efforts occur, a significant portion of the 'response' serves to maintain the status quo of high emissions by creating the appearance of action without addressing root causes. The rising trend in extractiveness and suppression over time reflects the increasing burden on vulnerable nations as mitigation is delayed.
 *
 * PERSPECTIVAL GAP:
 *   Developed nations perceive this as a pragmatic, necessary response to an urgent problem, emphasizing global cooperation on adaptation. Developing nations, however, experience it as a deeply unjust imposition, forcing them to pay for a crisis they did not create, while the primary polluters avoid responsibility. The engine's classification will highlight this structural asymmetry, showing a Tangled Rope for the victims and potentially a Rope or even a Mountain (false summit) for the beneficiaries, who frame it as an inevitable, natural response.
 *
 * DIRECTIONALITY LOGIC:
 *   Global North developed nations and fossil fuel industries are clear beneficiaries (low directionality), as this reading allows them to continue their current economic models. Global South developing nations and vulnerable communities are the primary targets (high directionality), bearing the direct costs of adaptation and suffering the impacts of insufficient mitigation. Climate scientists act as observers, providing critical data but often lacking direct policy influence. International financial institutions, while providing adaptation funding, also act as agenda-setters, shaping the terms of response in ways that can reinforce the adaptation-first paradigm.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mitigation_adaptation_causal_link,
    'To what extent does prioritizing adaptation over mitigation create a feedback loop that increases future adaptation costs and makes effective mitigation more difficult?',
    'Longitudinal economic modeling that integrates climate impact projections with adaptation investment scenarios, comparing outcomes under different mitigation/adaptation mixes.',
    'If a strong negative feedback loop is confirmed, the ''adaptation priority'' reading''s long-term extractiveness would be higher than currently measured, as it locks in greater future costs for vulnerable populations. This would strengthen its classification as a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_adaptation_causal_link, empirical, 'Whether adaptation priority exacerbates future climate challenges.').

omega_variable(
    responsibility_attribution_ambiguity,
    'Is the historical responsibility for climate change sufficiently accounted for in the current distribution of adaptation burdens, or is the ''adaptation priority'' reading a mechanism for avoiding historical accountability?',
    'International legal and ethical frameworks for climate justice, coupled with historical emissions accounting and analysis of financial flows for adaptation vs. loss and damage.',
    'If historical accountability is found to be systematically avoided, the ''adaptation priority'' reading''s suppression and extractiveness would be re-evaluated as higher, reflecting a deeper structural injustice. This would reinforce its Snare-like qualities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(responsibility_attribution_ambiguity, conceptual, 'The role of historical responsibility in current climate response distribution.').

omega_variable(
    adaptation_limits_acknowledgment,
    'Is the ''adaptation priority'' reading genuinely acknowledging the physical limits of adaptation, or is it promoting a false sense of security that delays necessary mitigation?',
    'Scientific consensus reports on ''hard limits'' to adaptation (e.g., unlivable heat, irreversible sea-level rise) and analysis of policy documents for explicit recognition of these limits.',
    'If the limits are systematically downplayed, the ''theater_ratio'' would be higher, as the narrative of ''adaptability'' becomes performative cover for inaction. This would push the constraint towards a Piton or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_limits_acknowledgment, empirical, 'Whether the physical limits of adaptation are genuinely acknowledged.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__adaptation_priority_reading, 2000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_response_imperative__adaptation_priority_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(clim_tr_t2004, climate_response_imperative__adaptation_priority_reading, theater_ratio, 2004, 0.25).
narrative_ontology:measurement(clim_tr_t2008, climate_response_imperative__adaptation_priority_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement(clim_tr_t2012, climate_response_imperative__adaptation_priority_reading, theater_ratio, 2012, 0.35).
narrative_ontology:measurement(clim_tr_t2016, climate_response_imperative__adaptation_priority_reading, theater_ratio, 2016, 0.38).
narrative_ontology:measurement(clim_tr_t2020, climate_response_imperative__adaptation_priority_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(clim_tr_t2024, climate_response_imperative__adaptation_priority_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(clim_be_t2004, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 2004, 0.55).
narrative_ontology:measurement(clim_be_t2008, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 2008, 0.6).
narrative_ontology:measurement(clim_be_t2012, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 2012, 0.63).
narrative_ontology:measurement(clim_be_t2016, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 2016, 0.65).
narrative_ontology:measurement(clim_be_t2020, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(clim_be_t2024, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(clim_su_t2004, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 2004, 0.65).
narrative_ontology:measurement(clim_su_t2008, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 2008, 0.68).
narrative_ontology:measurement(clim_su_t2012, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 2012, 0.7).
narrative_ontology:measurement(clim_su_t2016, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 2016, 0.72).
narrative_ontology:measurement(clim_su_t2020, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 2020, 0.74).
narrative_ontology:measurement(clim_su_t2024, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
