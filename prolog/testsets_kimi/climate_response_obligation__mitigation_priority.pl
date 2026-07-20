% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Climate Response Obligation â Mitigation Priority Reading
 *   domain: climate_policy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint story instantiates the mitigation_priority reading of the
 *   contested climate_response_obligation kernel. The reading holds that
 *   intergenerational justice requires rapid decarbonization to minimize
 *   warming, positioning future generations as primary beneficiaries and the
 *   current generationâespecially Global North economies and fossil
 *   capitalâas the parties bearing transition costs and stranded assets.
 *   The constraint operates as a tangled rope: it solves a genuine
 *   collective-action problem (atmospheric commons management) while
 *   asymmetrically extracting from present economic actors to benefit the
 *   unborn. The claim/metric independence is maintained by authoring Tangled
 *   Rope as the structural claim while the metrics reflect substantial
 *   extraction and suppression. The sibling readings (adaptation_priority,
 *   degrowth_reading) are treated as separate constraints per the
 *   Îµ-invariance principle.
 *
 * KEY AGENTS:
 *   - future_generations: Primary beneficiary (powerless/trapped/universal) â receive avoided catastrophic harm but possess no temporal exit or political voice
 *   - global_south_nations: Secondary beneficiary (organized/constrained/global) â gain from historical-responsibility norms and climate-finance flows
 *   - fossil_capital: Primary target (powerful/constrained/global) â bears stranded-asset extraction
 *   - global_north_economies: Secondary target (institutional/constrained/global) â bears disproportionate mitigation burden
 *   - present_transition_bearers: Tertiary target (moderate/constrained/global) â bears energy-transition costs in higher prices and taxes
 *   - unfccc_regime: Agenda-setter (institutional/analytical/global) â administers and enforces the mitigation architecture
 *   - climate_scientists: Analytical observer (organized/analytical/global) â provides empirical foundation without collecting rents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.7).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.74).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.7).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Climate Response Obligation â Mitigation Priority Reading").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "climate_policy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, '856574aa-551b-4c9b-bb36-b7c3a453d1e7').
narrative_ontology:cs_kernel_codification('856574aa-551b-4c9b-bb36-b7c3a453d1e7', formalized).
narrative_ontology:cs_authority_grounding('856574aa-551b-4c9b-bb36-b7c3a453d1e7', expertise).
narrative_ontology:cs_interpretation_layer_present('856574aa-551b-4c9b-bb36-b7c3a453d1e7').
narrative_ontology:cs_reading_relation('856574aa-551b-4c9b-bb36-b7c3a453d1e7', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('856574aa-551b-4c9b-bb36-b7c3a453d1e7', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('856574aa-551b-4c9b-bb36-b7c3a453d1e7', foundational, rapid_decarbonization_imperative).
narrative_ontology:cs_axiom_status(rapid_decarbonization_imperative, holdable).
narrative_ontology:cs_axiom_grounding('856574aa-551b-4c9b-bb36-b7c3a453d1e7', rapid_decarbonization_imperative, empirically_contingent).
narrative_ontology:cs_axiom('856574aa-551b-4c9b-bb36-b7c3a453d1e7', foundational, intergenerational_equity_principle).
narrative_ontology:cs_axiom_status(intergenerational_equity_principle, holdable).
narrative_ontology:cs_axiom_grounding('856574aa-551b-4c9b-bb36-b7c3a453d1e7', intergenerational_equity_principle, deontological).
narrative_ontology:cs_reference_frame('856574aa-551b-4c9b-bb36-b7c3a453d1e7', stable_climate_intergenerational_equity).
narrative_ontology:cs_drift_state('856574aa-551b-4c9b-bb36-b7c3a453d1e7', contemporary_net_zero_pledge_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('856574aa-551b-4c9b-bb36-b7c3a453d1e7', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, global_south_nations).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_capital).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_north_economies).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, present_transition_bearers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit the climate system shaped by present emissions policy; primary beneficiaries of avoided catastrophic warming but possess no political voice or temporal exit; locked into the atmospheric commons by birth timing.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Receive avoided climate damages and climate-finance flows under the mitigation-priority frame; historically low emitters who gain from enforcement of historical-responsibility norms.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_south_nations, beneficiary,
    organized, generational, constrained, global).

% Holds carbon-intensive reserves and infrastructure that face stranding under rapid decarbonization timelines; bears direct extraction via reserve write-downs, divestment pressure, and regulatory obsolescence.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_capital, payer,
    powerful, biographical, constrained, global).

% Bear disproportionate mitigation burden under common-but-differentiated-responsibility principles; locked into high-carbon infrastructure and expected to finance the global transition due to historical emissions.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_north_economies, payer,
    institutional, generational, constrained, global).

% Bear transition costs through higher energy prices, infrastructure retrofit taxes, and labor-market displacement; pay now for benefits that accrue primarily to unborn future generations.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, present_transition_bearers, payer,
    moderate, biographical, constrained, global).

% Administers the global mitigation architecture through NDCs, transparency frameworks, and climate-finance mechanisms; sets the rules that define adequate response and maintains compliance machinery.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, unfccc_regime, agenda_setter,
    institutional, generational, analytical, global).

% Provide the empirical foundation for carbon budgets and warming projections; do not collect rents from the constraint but their findings structurally shape what counts as a legitimate climate response.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_scientists, observer,
    organized, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global emissions reductions to prevent catastrophic climate change and manage the atmospheric commons, solving the collective-action problem where individual nations' rational emission trajectories produce collective ruin.
% TRANSFER_FUNCTION: Moves transition costs, stranded-asset losses, and mitigation-finance obligations from present-generation fossil capital, Global North economies, and transition-bearers to future generations (via avoided catastrophic damages) and Global South nations (via climate finance and avoided impacts).
% ABSENT_VOICES: Adaptation-priority advocates who argue locked-in warming makes mitigation insufficient; degrowth proponents who see efficiency-driven decarbonization as a false summit; future generations themselves are physically absent from the negotiating rooms where their primary-beneficiary status is asserted.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority obligation vanished, NDCs would collapse, carbon markets unravel, stranded-asset risk would reverse, climate-finance architecture would dissolve, and the global policy default would revert to adaptation-only or uncoordinated national measures â atmospheric-commons governance would reorganize fundamentally.
% FOUNDING_PROBLEM: Anthropogenic greenhouse-gas emissions create a tragedy of the atmospheric commons where uncoordinated national and corporate rationality produces collectively catastrophic warming, threatening future human flourishing.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports (expertise seat, outside the direct beneficiary set); climate scientists as observers attest the physical risk. Fossil-capital economists and some Global North policymakers contest the severity and the assignment of historical responsibility, corroborating the contested status from outside the benefiting parties.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__mitigation_priority, 0.7, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.70) is high because the constraint imposes concentrated stranded-asset losses and transition costs on present actors for the benefit of future generations. Suppression (0.74) is higher still because the mitigation-priority framework actively marginalizes adaptation-only and degrowth alternatives in international climate finance and policy discourse; the enforcement machinery (UNFCCC, NDCs, carbon markets) has matured and hardened. Theater ratio (0.40) reflects the growing gap between net-zero pledges and actual emissions trajectoriesâperformative commitment exceeds functional decarbonization. Accessibility collapse (0.68) is substantial because 'do nothing' or 'adapt only' alternatives have become politically illegitimate in mainstream climate governance. Resistance (0.72) is high due to persistent fossil-capital opposition and Global North reluctance to finance historical-responsibility transfers. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The future-generations seat experiences this constraint as survival infrastructure with d near 0.0; the fossil-capital seat experiences it as expropriation with d near 1.0. The Global North economies sit at high d (historical-responsibility obligations) despite high global power, because the constraint's scope is universal and their exit is locked by infrastructure and norms. The UNFCCC regime sits near symmetric (d ~0.5): it administers the constraint but does not personally capture the extracted value.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (future_generations, global_south_nations) derive low directionality: the constraint subsidizes their climate-risk exposure. Victim declarations (fossil_capital, global_north_economies, present_transition_bearers) derive high directionality: the constraint extracts via asset stranding, infrastructure retrofit mandates, and transition taxes. Climate scientists carry analytical exit and observer role, giving them neutral directionality. No overrides are required because the structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâuncoordinated emissions threatening future catastropheâremains live, so mandatrophy_resolved is not declared. The constraint avoids misclassification as a Snare because the coordination function (atmospheric commons management) is structurally genuine and independently verifiable via climate physics. It avoids misclassification as a pure Rope because the asymmetric cost distribution (Global North, fossil capital) is not incidental but constitutive of the reading. Should equilibrium climate sensitivity prove substantially lower than projected, the founding problem would deaden and the constraint would drift toward Piton or Snare; the temporal measurements model this extraction accumulation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    obligation_naturalness,
    'Is the intergenerational climate obligation a discovered moral fact (natural law) or a constructed policy coordination mechanism?',
    'Comparative anthropology of obligation: if all human societies independently discover similar duties to posterity, naturalness is supported; if the obligation tracks institutional capacity to model climate futures, constructedness is supported.',
    'If natural law, the constraint approaches Mountain from the beneficiary seat; if constructed, it remains Tangled Rope with contested extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(obligation_naturalness, conceptual, 'Whether the obligation is discovered or constructed').

omega_variable(
    adaptation_finance_suppression,
    'Does the mitigation-priority reading structurally suppress adaptation finance and planning, leaving current vulnerable populations without needed resilience investment?',
    'Cross-national regression of adaptation finance flows against mitigation pledge stringency; case studies of NDC budget allocation.',
    'If mitigation crowds out adaptation, the coordination story is partially cover for asymmetric present-generation harm; classification shifts toward Snare for current vulnerable populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_finance_suppression, empirical, 'Whether mitigation priority suppresses adaptation alternatives').

omega_variable(
    transition_cost_incidence,
    'Do transition costs actually fall on Global North economies and fossil capital as the reading claims, or are they passed downstream to global working classes and consumers?',
    'Input-output analysis of carbon-pricing incidence; stranded-asset holder wealth demographics.',
    'If costs are regressive, the directional derivation for working-class stakeholders shifts toward full-target, and the constraint''s intergenerational justice claim becomes a class-extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_cost_incidence, empirical, 'Whether transition costs are regressive or targeted as claimed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__mitigation_priority, theater_ratio, 0, 0.08).
narrative_ontology:measurement(clim_tr_t6, climate_response_obligation__mitigation_priority, theater_ratio, 6, 0.12).
narrative_ontology:measurement(clim_tr_t12, climate_response_obligation__mitigation_priority, theater_ratio, 12, 0.18).
narrative_ontology:measurement(clim_tr_t18, climate_response_obligation__mitigation_priority, theater_ratio, 18, 0.25).
narrative_ontology:measurement(clim_tr_t24, climate_response_obligation__mitigation_priority, theater_ratio, 24, 0.3).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__mitigation_priority, theater_ratio, 30, 0.36).
narrative_ontology:measurement(clim_tr_t35, climate_response_obligation__mitigation_priority, theater_ratio, 35, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__mitigation_priority, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(clim_be_t6, climate_response_obligation__mitigation_priority, base_extractiveness, 6, 0.3).
narrative_ontology:measurement(clim_be_t12, climate_response_obligation__mitigation_priority, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(clim_be_t18, climate_response_obligation__mitigation_priority, base_extractiveness, 18, 0.5).
narrative_ontology:measurement(clim_be_t24, climate_response_obligation__mitigation_priority, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__mitigation_priority, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(clim_be_t35, climate_response_obligation__mitigation_priority, base_extractiveness, 35, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__mitigation_priority, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(clim_su_t6, climate_response_obligation__mitigation_priority, suppression_requirement, 6, 0.32).
narrative_ontology:measurement(clim_su_t12, climate_response_obligation__mitigation_priority, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(clim_su_t18, climate_response_obligation__mitigation_priority, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(clim_su_t24, climate_response_obligation__mitigation_priority, suppression_requirement, 24, 0.62).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__mitigation_priority, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(clim_su_t35, climate_response_obligation__mitigation_priority, suppression_requirement, 35, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, global_infrastructure).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, degrowth_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'climate response obligation' conflates three structurally distinct readings: mitigation_priority (this story), adaptation_priority, and degrowth_reading. They share the kernelâthe obligation to respond to anthropogenic climate changeâbut differ on what the obligation requires, who benefits, and who pays. Each reading carries a distinct epsilon, stakeholder structure, and classification; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
