% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Adaptation-Priority Reading of the Climate Response Obligation
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the adaptation-priority reading of the climate
 *   response obligation kernel: the position that 2-3°C of warming should be
 *   accepted as a fixed constraint on policy and that resources should flow
 *   toward resilience and adaptation rather than toward preventing the
 *   warming itself. This is a distinct, ε-stable constraint from the sibling
 *   readings (mitigation_priority, degrowth_reading) — it has its own
 *   beneficiary structure (fossil capital, current high-emitting generations,
 *   wealthy states with adaptation capacity) and its own victim structure
 *   (future generations, Global South frontline states, small island states)
 *   that the mitigation and degrowth readings do not share, since those
 *   readings reallocate cost toward current high emitters instead of toward
 *   future/peripheral populations. Do not average this story's ε against the
 *   siblings' — each reading is generated and measured independently per the
 *   ε-invariance principle.
 *
 * KEY AGENTS:
 *   - fossil_capital_incumbents: primary agenda-setter and beneficiary (institutional/arbitrage) — funds the framing and avoids stranded-asset exposure
 *   - current_generation_high_emitters: beneficiary (organized/mobile) — avoids near-term transition costs
 *   - wealthy_coastal_and_temperate_states: beneficiary (institutional/arbitrage) — captures the adaptation investment
 *   - global_south_frontline_states, small_island_states, subsistence_agricultural_communities: primary targets (powerless/trapped) — bear physical impacts without adaptation capacity
 *   - future_generations: primary target (powerless/trapped, civilizational horizon) — inherits the locked-in warming with no seat in the decision
 *   - climate_scientists_and_ipcc_working_groups: analytical observer — documents the physical basis both readings selectively cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.71).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.58).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.71).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Adaptation-Priority Reading of the Climate Response Obligation").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, '8116514a-210b-4434-b930-1b5dd6105a5e').
narrative_ontology:cs_kernel_codification('8116514a-210b-4434-b930-1b5dd6105a5e', distributed).
narrative_ontology:cs_authority_grounding('8116514a-210b-4434-b930-1b5dd6105a5e', extraction).
narrative_ontology:cs_interpretation_layer_present('8116514a-210b-4434-b930-1b5dd6105a5e').
narrative_ontology:cs_reading_relation('8116514a-210b-4434-b930-1b5dd6105a5e', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('8116514a-210b-4434-b930-1b5dd6105a5e', climate_response_obligation__degrowth_reading, forecloses).
narrative_ontology:cs_axiom('8116514a-210b-4434-b930-1b5dd6105a5e', foundational, warming_within_accepted_band_is_manageable_via_engineering).
narrative_ontology:cs_axiom_status(warming_within_accepted_band_is_manageable_via_engineering, holdable).
narrative_ontology:cs_axiom_grounding('8116514a-210b-4434-b930-1b5dd6105a5e', warming_within_accepted_band_is_manageable_via_engineering, empirically_contingent).
narrative_ontology:cs_axiom('8116514a-210b-4434-b930-1b5dd6105a5e', foundational, continued_high_throughput_growth_is_compatible_with_climate_obligation).
narrative_ontology:cs_axiom_status(continued_high_throughput_growth_is_compatible_with_climate_obligation, holdable).
narrative_ontology:cs_axiom_grounding('8116514a-210b-4434-b930-1b5dd6105a5e', continued_high_throughput_growth_is_compatible_with_climate_obligation, instrumental).
narrative_ontology:cs_axiom('8116514a-210b-4434-b930-1b5dd6105a5e', secondary, present_generation_transition_cost_avoidance_is_legitimate_priority).
narrative_ontology:cs_axiom_status(present_generation_transition_cost_avoidance_is_legitimate_priority, holdable).
narrative_ontology:cs_axiom_grounding('8116514a-210b-4434-b930-1b5dd6105a5e', present_generation_transition_cost_avoidance_is_legitimate_priority, conventional).
narrative_ontology:cs_reference_frame('8116514a-210b-4434-b930-1b5dd6105a5e', post_paris_agreement_ndc_framework).
narrative_ontology:cs_drift_state('8116514a-210b-4434-b930-1b5dd6105a5e', post_2023_global_stocktake, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8116514a-210b-4434-b930-1b5dd6105a5e', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, fossil_capital_incumbents).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, current_generation_high_emitters).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, wealthy_coastal_and_temperate_states).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_frontline_states).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, small_island_states).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, subsistence_agricultural_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, insurance_and_reinsurance_industry).
narrative_ontology:constraint_vindicates(climate_response_obligation__adaptation_priority, warming_lock_in_thesis).
narrative_ontology:constraint_vindicates(climate_response_obligation__adaptation_priority, adaptation_cost_effectiveness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fund research institutes, think tanks, and lobbying campaigns that frame 2-3°C warming as a technically settled inevitability rather than a policy choice, redirecting the policy conversation from decarbonization mandates toward resilience infrastructure spending. Continue extracting and selling fossil assets on a multi-decade depreciation schedule that a rapid mitigation path would strand. Their capital can relocate across jurisdictions and asset classes; their political influence sets the terms of the adaptation-versus-prevention debate in the bodies that would otherwise regulate them.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, fossil_capital_incumbents, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__adaptation_priority, fossil_capital_incumbents, beneficiary).

% Consumers and voters in high-emissions economies who avoid the near-term costs of carbon pricing, transport electrification, and industrial retooling by supporting policies that defer prevention in favor of resilience spending funded largely by their own governments. They personally will not live to bear the compounding physical risk that deferral generates decades out, and can migrate within or between wealthy regions if local climate impacts worsen.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, current_generation_high_emitters, beneficiary,
    organized, biographical, mobile, national).

% Have the fiscal capacity to build seawalls, retrofit infrastructure, subsidize crop-switching, and relocate vulnerable populations domestically. Adaptation investment concentrates in their jurisdictions because they can pay for it; they experience the 2-3°C pathway as a manageable engineering problem rather than an existential one.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, wealthy_coastal_and_temperate_states, beneficiary,
    institutional, generational, arbitrage, national).

% Face the same physical warming trajectory without the fiscal, technical, or institutional capacity to build comparable resilience infrastructure. Loss-and-damage financing pledged at international negotiations arrives slower and smaller than the adaptation gap it is meant to close. Populations facing repeated flooding, drought, or crop failure have limited capital or legal pathways to relocate internationally.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, global_south_frontline_states, payer,
    powerless, generational, trapped, regional).

% Face sea-level rise that threatens total territorial loss at the accepted warming trajectory — a risk with no adaptation engineering solution at the scale required. Have no meaningful exit: relocation of an entire nation-state's population and sovereignty has no established legal or political mechanism. Their diplomatic voice in negotiations is real but structurally outweighed by the emitting states whose consent adaptation-priority policy requires.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, small_island_states, payer,
    powerless, civilizational, trapped, regional).

% Depend on rainfall and temperature patterns that are directly disrupted by the accepted warming pathway. Resilience investment reaching them, if any, is a fraction of what reaches wealthy agricultural sectors with insurance, irrigation infrastructure, and futures markets. Exit means abandoning ancestral land and livelihood with no guaranteed destination.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, subsistence_agricultural_communities, payer,
    powerless, generational, trapped, local).

% Inherit a climate system locked into 2-3°C of warming (or more, given ongoing emissions under this policy) plus whatever adaptation infrastructure the current generation chose to build or not build. Have no representation in the decisions being made now that determine their physical environment; cannot exit a warming trajectory already committed to the atmosphere by the time they are born.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Produce the probability distributions and impact assessments that both the adaptation-priority and mitigation-priority readings cite selectively. Document that adaptation and mitigation are not substitutes past certain warming thresholds — some impacts (species loss, ice sheet collapse, ecosystem tipping points) have no adaptation response — but their findings are filtered through political framing before reaching policy.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_scientists_and_ipcc_working_groups, observer,
    analytical, generational, analytical, global).

% Profits from pricing and underwriting the resilience-and-adaptation market: catastrophe bonds, parametric insurance, infrastructure hardening contracts. Has a direct commercial interest in the adaptation-priority framing becoming the dominant policy response, since it expands their addressable market relative to a decarbonization pathway that would shrink certain lines of business (fossil asset insurance) while not proportionally growing others.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, insurance_and_reinsurance_industry, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__adaptation_priority, insurance_and_reinsurance_industry, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__adaptation_priority, fossil_capital_incumbents).
narrative_ontology:fixing_cost_class(climate_response_obligation__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a real policy response to warming that is, at current emissions trajectories, partly locked in regardless of near-term mitigation choices — building sea walls, drought-resistant crops, and heat-adapted infrastructure is genuine, needed coordination for the warming already committed to the atmosphere.
% TRANSFER_FUNCTION: Moves the cost of climate impact from the current, high-emitting generation (who avoid transition costs to their energy and industrial systems) to future generations and low-capacity regions (who inherit the physical impacts and bear the adaptation costs without commensurate historical benefit from the emissions that caused them).
% ABSENT_VOICES: Future generations have no seat in the negotiations that select this pathway. Small island states and Global South frontline states are present at climate negotiations but structurally outvoted by the emitting economies whose continued emissions this reading normalizes; their objections are recorded in every COP text and routinely unmet by finance commitments.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority framing lost its institutional and political hold, decarbonization commitments would need to tighten to avoid the acknowledged 2-3°C pathway, fossil asset valuations would face faster write-down pressure, and adaptation finance currently concentrated in wealthy states would need to be either matched by mitigation investment or redirected toward loss-and-damage financing for the states now treated as unavoidable casualties of the accepted trajectory.
% FOUNDING_PROBLEM: International climate negotiations repeatedly failed to secure binding, sufficiently rapid decarbonization commitments; some warming is already locked in by past emissions and current infrastructure inertia, so building resilience capacity was framed as the pragmatic response to that partly-irreducible physical reality.
% FOUNDING_PROBLEM_CORROBORATION: IPCC working groups and independent climate scientists attest that some warming and some adaptation need are physically locked in regardless of near-term policy — that much is corroborated outside fossil capital's interest. But the same scientific bodies do not corroborate that 2-3°C specifically is the necessary or acceptable target rather than a policy choice reflecting insufficient mitigation effort; that framing is corroborated primarily by fossil capital incumbents and beneficiary governments, not by independent analysis of what mitigation remains technically and economically feasible.
narrative_ontology:disappearance_verdict(climate_response_obligation__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__adaptation_priority, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__adaptation_priority, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is high (0.71 at T=40) and rising because the pathway's cost structure shifts systematically away from those who benefit from continued fossil fuel use and toward those without the capacity to adapt or the standing to object — a textbook asymmetric transfer riding on a genuine coordination need (some adaptation is unavoidable regardless of mitigation pace). Suppression (0.58) reflects the political and financial machinery required to keep decarbonization commitments below what independent climate assessments would recommend — lobbying, selective citation of scientific uncertainty, and institutional capture of negotiation venues, rather than raw physical coercion. Theater ratio (0.42) is substantial and rising: adaptation-priority framing increasingly performs concern for frontline states (loss-and-damage funds announced, rarely disbursed at pledged scale) while functioning primarily to protect fossil asset valuations from faster write-down. Accessibility collapse (0.48) is moderate — mitigation remains technically and economically available, unlike a true mountain constraint; the alternative (aggressive decarbonization) has not been foreclosed by physics, only by political economy. Resistance (0.62) is substantial, driven by Global South negotiating blocs, youth climate movements, and small island state diplomacy at COP forums, none of which have yet been able to force a change in the trajectory.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil capital incumbents and current high-emitting populations sit near the beneficiary end of directionality: they avoid transition costs now and their arbitrage/mobile exit options let them relocate capital or residence if local conditions worsen before the largest impacts land. Wealthy states similarly benefit — their fiscal capacity converts the adaptation-priority framing into a genuinely lower-cost outcome for them specifically. Future generations, small island states, and subsistence communities sit at the target end: trapped exit options (no legal mechanism for a sinking nation to relocate its sovereignty; no capital to build seawalls; not yet born to object) combine with the civilizational/generational time horizon to make the extraction structurally unavoidable rather than a matter of individual choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is deliberately chosen over snare because a genuine coordination function exists — resilience investment against warming already locked in by past emissions is real and necessary regardless of how the mitigation debate resolves. Classifying this purely as extraction would erase the legitimate adaptation-planning function; classifying it purely as coordination would erase the asymmetric cost transfer this reading specifically encodes (as distinct from mitigation_priority, which allocates cost differently). The founding_problem mismatch check applies here: founding_problem_status is 'contested' rather than 'dead' because the underlying physical lock-in genuinely exists (not a zombie mandate), but the specific 2-3°C target and the pace of decarbonization it excuses are actively disputed outside the beneficiary set — this is a live contest over degree and allocation, not a fully manufactured mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    warming_lock_in_versus_policy_choice,
    'Is the 2-3°C trajectory this reading accepts as ''inevitable'' actually physically locked in by past emissions and infrastructure inertia, or is it a policy choice reflecting insufficient mitigation ambition that remains technically avoidable?',
    'Compare independent (non-fossil-funded) integrated assessment model runs of feasible rapid-decarbonization pathways against the emissions trajectory actually being pursued under adaptation-priority policy; if feasible lower-warming pathways exist and are foreclosed only by political and financial resistance rather than technical or economic constraints, the ''inevitability'' framing is a constructed cover rather than a natural limit.',
    'If the 2-3°C target is a policy choice rather than a physical inevitability, this reading''s core premise is substantially weakened and the constraint''s beneficiary structure (fossil capital protection) becomes the primary explanatory factor rather than a side effect of unavoidable physics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warming_lock_in_versus_policy_choice, empirical, 'Whether the accepted warming target reflects physical lock-in or political-economic choice.').

omega_variable(
    adaptation_mitigation_substitutability_limit,
    'Past what warming threshold does adaptation cease to be a viable substitute for mitigation — i.e., are there impacts (ice sheet collapse, mass species extinction, ecosystem tipping points) for which no resilience investment is a functional response?',
    'Cross-reference IPCC impact assessments for irreversible/non-adaptable impact categories against the specific 2-3°C trajectory this reading accepts.',
    'If substantial classes of harm within the accepted trajectory have no adaptation response, the ''adaptation as resilience investment'' framing understates the true victim exposure and the constraint''s extractiveness is higher than the resilience-spending numbers alone suggest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_mitigation_substitutability_limit, empirical, 'Whether adaptation can functionally substitute for mitigation across the full range of impacts at the accepted warming level.').

omega_variable(
    kernel_reading_selection_mechanism,
    'Given that the climate_response_obligation kernel supports at least three live readings (adaptation_priority, mitigation_priority, degrowth_reading) with materially different beneficiary/victim structures, what determines which reading dominates actual international policy at any given time?',
    'Track the relative lobbying expenditure, negotiating bloc composition, and fossil-asset exposure of governments whose policy shifts between these readings over successive COP cycles.',
    'If reading selection tracks fossil capital''s political influence rather than updated scientific assessment, this corroborates the adaptation_priority reading''s authority_grounding as extraction rather than expertise or practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, conceptual, 'What structurally determines which kernel reading becomes dominant international policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__adaptation_priority, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t8, climate_response_obligation__adaptation_priority, theater_ratio, 8, 0.26).
narrative_ontology:measurement(clim_tr_t16, climate_response_obligation__adaptation_priority, theater_ratio, 16, 0.31).
narrative_ontology:measurement(clim_tr_t24, climate_response_obligation__adaptation_priority, theater_ratio, 24, 0.35).
narrative_ontology:measurement(clim_tr_t32, climate_response_obligation__adaptation_priority, theater_ratio, 32, 0.39).
narrative_ontology:measurement(clim_tr_t40, climate_response_obligation__adaptation_priority, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__adaptation_priority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t8, climate_response_obligation__adaptation_priority, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(clim_be_t16, climate_response_obligation__adaptation_priority, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(clim_be_t24, climate_response_obligation__adaptation_priority, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(clim_be_t32, climate_response_obligation__adaptation_priority, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(clim_be_t40, climate_response_obligation__adaptation_priority, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__adaptation_priority, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(clim_su_t8, climate_response_obligation__adaptation_priority, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(clim_su_t16, climate_response_obligation__adaptation_priority, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(clim_su_t24, climate_response_obligation__adaptation_priority, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(clim_su_t32, climate_response_obligation__adaptation_priority, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(clim_su_t40, climate_response_obligation__adaptation_priority, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__adaptation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, degrowth_reading).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, loss_and_damage_financing_mechanism).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, fossil_fuel_subsidy_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the climate_response_obligation kernel. mitigation_priority reallocates cost toward current high emitters and fossil capital by treating warming minimization as the binding obligation; degrowth_reading forecloses this reading's premise entirely by rejecting continued high-throughput economic activity as compatible with any adequate climate response. All three should be authored as separate files with independent ε values and linked here; contamination propagation analysis should treat weakening of this reading's institutional support as downstream pressure increasing the plausibility of the sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
