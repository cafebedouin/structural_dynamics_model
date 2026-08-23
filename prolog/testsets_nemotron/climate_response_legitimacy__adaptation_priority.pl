% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Climate Response Legitimacy — Adaptation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint story captures the 'adaptation priority' reading of the
 *   contested kernel 'climate response legitimacy.' The reading asserts that
 *   legitimate climate action accepts a warming trajectory (2.7-3.5C by 2100
 *   under current policies) and prioritizes protecting vulnerable populations
 *   through resilience infrastructure and adaptive capacity. This framing
 *   emerged from the Copenhagen/Paris pivot and now structures the
 *   international climate finance architecture. The constraint is claimed as
 *   tangled_rope: it coordinates adaptation finance flows (genuine
 *   coordination function) while extracting from low-income nations via the
 *   $350B adaptation deficit, debt-distress conditionality, and the deferral
 *   of mitigation costs to future generations. Wealthy nations and fossil
 *   incumbents are beneficiaries; the Global South and future generations are
 *   payers. The engine will compute per-seat classifications from this
 *   structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.42).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Climate Response Legitimacy — Adaptation Priority Reading").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, '4732a80d-e2fa-464f-892a-13c9bb2d63ee').
narrative_ontology:cs_kernel_codification('4732a80d-e2fa-464f-892a-13c9bb2d63ee', formalized).
narrative_ontology:cs_authority_grounding('4732a80d-e2fa-464f-892a-13c9bb2d63ee', extraction).
narrative_ontology:cs_interpretation_layer_present('4732a80d-e2fa-464f-892a-13c9bb2d63ee').
narrative_ontology:cs_reading_relation('4732a80d-e2fa-464f-892a-13c9bb2d63ee', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('4732a80d-e2fa-464f-892a-13c9bb2d63ee', climate_response_legitimacy__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('4732a80d-e2fa-464f-892a-13c9bb2d63ee', foundational, warming_trajectory_accepted_as_given).
narrative_ontology:cs_axiom_status(warming_trajectory_accepted_as_given, holdable).
narrative_ontology:cs_axiom_grounding('4732a80d-e2fa-464f-892a-13c9bb2d63ee', warming_trajectory_accepted_as_given, instrumental).
narrative_ontology:cs_axiom('4732a80d-e2fa-464f-892a-13c9bb2d63ee', foundational, adaptation_finance_as_primary_justice_obligation).
narrative_ontology:cs_axiom_status(adaptation_finance_as_primary_justice_obligation, holdable).
narrative_ontology:cs_axiom_grounding('4732a80d-e2fa-464f-892a-13c9bb2d63ee', adaptation_finance_as_primary_justice_obligation, deontological).
narrative_ontology:cs_axiom('4732a80d-e2fa-464f-892a-13c9bb2d63ee', secondary, mitigation_deferral_as_pragmatic_necessity).
narrative_ontology:cs_axiom_status(mitigation_deferral_as_pragmatic_necessity, holdable).
narrative_ontology:cs_axiom_grounding('4732a80d-e2fa-464f-892a-13c9bb2d63ee', mitigation_deferral_as_pragmatic_necessity, instrumental).
narrative_ontology:cs_reference_frame('4732a80d-e2fa-464f-892a-13c9bb2d63ee', post_copenhagen_paris_pivot_legitimacy).
narrative_ontology:cs_drift_state('4732a80d-e2fa-464f-892a-13c9bb2d63ee', contemporary_stocktake_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4732a80d-e2fa-464f-892a-13c9bb2d63ee', '2026-08-03T14:22:11Z').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_nation_governments).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, global_financial_institutions).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, climate_adaptation_finance_intermediaries).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, low_income_nations).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, frontline_communities_global_south).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations_global_south).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, small_island_developing_states).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, agricultural_workers_tropics).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, adaptation_finance_as_primary_obligation).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, resilience_infrastructure_as_justice).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, warming_trajectory_acceptance_as_realism).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, differentiated_responsibility_via_finance_not_mitigation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the terms of international climate negotiation, define what counts as legitimate climate action, and control the flow of adaptation finance. Their development model (high per-capita emissions, consumption-intensive) is preserved by framing adaptation as the primary obligation. They pledge finance but control disbursement terms, conditionality, and definition of 'bankable' projects. Exit from the constraint is easy: they can redefine legitimacy at any COP.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_nation_governments, agenda_setter,
    institutional, biographical, arbitrage, global).

% Continue extraction and combustion under the adaptation-priority framing because the constraint explicitly accepts the warming trajectory. No phaseout schedule is required; carbon capture and 'abated' fossil narratives are legitimized. They fund adaptation-oriented think tanks and lobby for 'pragmatic' climate policy that protects asset values. Their exit is trivial: they are not bound by the constraint, they benefit from its enforcement.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, fossil_fuel_incumbents, beneficiary,
    powerful, biographical, arbitrage, global).

% Design and administer the adaptation finance architecture (Green Climate Fund, World Bank climate windows, blended finance facilities). They extract fees, set debt terms, and define 'bankable' resilience projects — which overwhelmingly favor middle-income countries and private-sector returns. The $350B adaptation deficit is measured in their instruments. They face no enforcement risk; the constraint creates their market.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, global_financial_institutions, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, global_financial_institutions, agenda_setter).

% Consultancies, project developers, insurance firms, and ratings agencies that intermediate adaptation finance. They capture rents from project preparation, monitoring, and verification. Their business model depends on the adaptation-priority framing persisting. They can pivot to other 'resilience' markets if the framing shifts — moderate exit constraint.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_adaptation_finance_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Bear the adaptation deficit ($350B/year gap) while having contributed minimally to historical emissions. They must borrow at premium rates for resilience infrastructure, diverting resources from health, education, and development. Their negotiation power is limited to moral suasion and bloc voting (G77, AOSIS, LDC Group). Exit from the constraint means accepting unmanaged climate impacts — not a real option.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, low_income_nations, payer,
    moderate, generational, constrained, national).

% Experience impacts first and most severely: heat mortality, crop failure, flood displacement, disease expansion. They have no political voice in the forums where adaptation priority is defined. Finance rarely reaches them — 'bankable' projects are large infrastructure, not community-level resilience. Identity-locked to place and livelihood; migration is existential loss, not exit.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, frontline_communities_global_south, payer,
    powerless, biographical, trapped, local).

% Inherit a world of higher warming (2.7-3.5C) because the adaptation-priority reading defers mitigation. They bear compounded costs: locked-in damages, reduced adaptive capacity from degraded ecosystems, and the political economy of a world that normalized inadequate response. No voice, no exit, no representation in current negotiations.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, future_generations_global_south, payer,
    powerless, civilizational, trapped, global).

% Face existential threat from sea-level rise and storm intensification under accepted warming trajectories. Adaptation finance is insufficient for survival-scale measures (relocation, land reclamation). They negotiate as a bloc (AOSIS) but their leverage is moral, not material. Exit is physical disappearance of territory — not a choice.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, small_island_developing_states, payer,
    moderate, generational, constrained, national).

% Livelihoods depend on climate-sensitive agriculture in zones where warming exceeds crop tolerance. Adaptation measures (irrigation, heat-tolerant varieties) are capital-intensive and often controlled by agribusiness, not smallholders. Their identity is fused to land and practice; migration means cultural dissolution. The constraint offers them resilience rhetoric while delivering insufficient finance.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, agricultural_workers_tropics, payer,
    powerless, biographical, identity_locked, local).

% Advocate for mitigation-priority or degrowth-transformation readings. They are present in COP observer spaces but structurally excluded from the negotiation text where 'legitimate response' is defined. Their demand for historical responsibility and reparative finance is ruled out of order by the adaptation-priority framing. They can mobilize, litigate, and shift discourse but cannot vote on the constraint.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_justice_movements, excluded,
    organized, generational, mobile, global).

% Produce the assessment reports that all readings cite. The adaptation-priority reading selectively cites WGII (impacts/adaptation) while downplaying WGIII (mitigation pathways) and the carbon budget math of WGI. Scientists cannot enforce policy but their framing authority is contested. Analytical seat: they see the full structure but do not collect or pay.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, ipcc_and_science_assessment_bodies, observer,
    institutional, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international climate finance flows toward adaptation projects in vulnerable countries; provides a shared framing that allows wealthy nations to meet 'climate obligation' without threatening their development model; synchronizes multilateral development bank portfolios around resilience infrastructure.
% TRANSFER_FUNCTION: Moves the burden of climate impacts from wealthy nations (who caused them) to low-income nations (who suffer them), mediated by a finance architecture that extracts fees and imposes debt. The $350B/year adaptation deficit is a transfer from vulnerable populations to financial intermediaries and creditor nations. Mitigation costs (stranded assets, transition) are deferred to future generations globally.
% ABSENT_VOICES: Frontline communities in the Global South (trapped, no representation), future generations (structurally excluded), smallholder farmers and fishers whose livelihoods are erased by accepted warming. Climate justice movements are physically present at COPs but procedurally excluded from the text where legitimacy is negotiated. Their demand for mitigation as primary obligation is ruled out of scope.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority framing vanished overnight, the legitimacy basis for the current finance architecture would collapse. Wealthy nations would face direct pressure for mitigation commensurate with 1.5C/2C pathways. The $350B deficit would be re-litigated as reparative obligation, not voluntary finance. Fossil fuel incumbents would lose the 'pragmatic acceptance' cover. The entire institutional edifice of climate diplomacy would reorganize around a different core claim.
% FOUNDING_PROBLEM: The founding problem was the perceived impossibility of rapid global decarbonization given entrenched fossil infrastructure, North-South development inequality, and the failure of Kyoto-style binding targets. The adaptation-priority reading emerged from the 2009 Copenhagen collapse and the 2015 Paris pivot to 'nationally determined' contributions — it was built to save the climate regime from total failure by lowering the bar for what counts as legitimate action.
% FOUNDING_PROBLEM_CORROBORATION: Wealthy nation negotiators and UNFCCC secretariat attest the problem is live: mitigation at required speed remains politically impossible, adaptation is the only feasible collective action. Climate justice movements, Global South negotiators, and independent legal scholars (e.g., CIEL, Third World Network) attest the founding problem is dead or manufactured: the 'impossibility' was constructed by wealthy nations' refusal to regulate their own emissions, and the adaptation frame now functions to legitimize that refusal. No corroboration from outside the beneficiary set for the 'live' claim.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__adaptation_priority, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is high because the constraint transfers the bulk of climate costs onto those least responsible and least able to pay, while the coordination function (adaptation finance) captures only a fraction of needed resources. Suppression (0.42) is moderate: the constraint does not use direct coercion but structurally suppresses alternatives by defining 'legitimate climate response' in a way that excludes mitigation-priority and degrowth readings. Theater ratio (0.55) is high and rising: the 'resilience' and 'adaptation' rhetoric increasingly masks the reality of insufficient finance and deferred mitigation. Accessibility collapse (0.38) is moderate — alternatives (rapid mitigation, degrowth) remain conceptually available but are politically suppressed. Resistance (0.51) is significant from climate justice movements and Global South blocs but has not shifted the constraint's core framing.
 *
 * PERSPECTIVAL GAP:
 *   From the wealthy nation/institutional seat, this is a rope: genuine coordination of adaptation finance under political constraints. From the low-income nation/frontline community seat, this is a snare: the coordination story is cover for extracting adaptation costs onto the vulnerable while preserving the emitter's model. From the future generations seat, it is a snare with civilizational time horizon: they pay the compounded cost of deferred mitigation with no voice. The engine computes these divergences from the declared power/exit/scope data.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy nation governments and fossil incumbents are structural beneficiaries (d near 0.0-0.15): they preserve their development model and asset values. Global financial institutions and adaptation intermediaries are beneficiaries (d ~0.2) who capture rents from the finance architecture. Low-income nations, frontline communities, future generations, SIDS, and agricultural workers are payers (d 0.7-1.0): they bear the adaptation deficit, the locked-in damages, and the compounded costs of higher warming. Their exit options range from constrained to trapped to identity-locked. Climate justice movements are excluded (analytical observer role). IPCC sits as analytical observer — sees the structure but does not collect or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Copenhagen collapse, perceived impossibility of binding mitigation) was real in 2009 but has mutated. The adaptation-priority framing now serves to legitimize the very inaction that made the founding problem seem intractable. Wealthy nations use 'political realism' to justify inadequate mitigation, then cite the resulting warming as justification for adaptation priority. This circularity is the mandatrophy: the constraint's mandate (protect the vulnerable) has been captured by the arrangement that ensures their vulnerability deepens. The founding problem status is 'contested' because beneficiaries claim it is live while payers and excluded voices demonstrate it is manufactured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_finance_deficit_mechanism,
    'Is the $350B/year adaptation deficit a structural feature of the finance architecture (designed to extract) or a contingent failure of political will (fixable within the current framing)?',
    'Trace the history of adaptation finance pledges (Copenhagen $100B, Paris Article 9, Glasgow doubling) against actual disbursements, grant-equivalent values, and conditionality. If the gap persists across all administrations and correlates with creditor-country fiscal cycles, it is structural.',
    'If structural, the constraint is a snare from the payer seats regardless of claimed coordination function. If contingent, the tangled_rope classification holds — the coordination function is real but under-resourced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_finance_deficit_mechanism, empirical, 'Whether the adaptation deficit is designed extraction or political failure.').

omega_variable(
    mitigation_deferral_compounding,
    'What is the quantitative compounding function for intergenerational costs under the accepted warming trajectory (2.7-3.5C) versus a mitigation-priority pathway (1.5-2C)?',
    'Integrated assessment model comparison (e.g., REMIND, MESSAGE, IMAGE) with damage functions calibrated to recent empirical literature (Burke et al. 2015, Kahn et al. 2021). Must include non-market damages, ecosystem collapse thresholds, and conflict migration multipliers.',
    'If compounding is superlinear (likely), the adaptation-priority reading extracts exponentially from future generations — this reading''s effective extraction for future_generations_global_south approaches 1.0. If sublinear, the intergenerational transfer is large but bounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mitigation_deferral_compounding, empirical, 'Compounding trajectory of deferred mitigation costs.').

omega_variable(
    coordination_extraction_separability,
    'Can the adaptation finance coordination function be separated from the extraction architecture (debt conditionality, intermediary rents, ''bankable'' project bias) without collapsing the finance flows entirely?',
    'Counterfactual: model a grant-based, directly-accessed, community-governed adaptation fund at scale ($350B/year). If it fails to disburse effectively, the extraction architecture may be functionally necessary for coordination. If it succeeds, the extraction is separable and the constraint is a snare wearing a rope''s clothes.',
    'If inseparable, the tangled_rope classification is correct: coordination and extraction are structurally fused. If separable, the constraint is a snare (coordination is pretext) and the engine should detect this from the beneficiary/payer structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction are structurally fused or separable.').

omega_variable(
    reading_framing_under_determination,
    'Does the ''adaptation priority'' framing represent a single coherent reading, or does it bundle two distinct claims: (a) adaptation finance as justice obligation, and (b) warming acceptance as realism?',
    'Analyze negotiating texts (Paris Agreement, Glasgow Pact, Sharm el-Sheikh Implementation Plan) for whether adaptation finance language is conditional on mitigation ambition. If adaptation finance is pledged *without* mitigation conditionality, the two claims are bundled. If they are linked, the reading may be two constraints.',
    'If bundled, this is one constraint with high internal tension (tangled_rope). If two constraints, decompose: one for adaptation finance obligation (rope/scaffold), one for warming acceptance (snare). The ε values would differ dramatically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_under_determination, conceptual, 'Whether the adaptation_priority reading conflates two structurally distinct claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 2009, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_tr_t2009, climate_response_legitimacy__adaptation_priority, theater_ratio, 2009, 0.25).
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_tr_t2015, climate_response_legitimacy__adaptation_priority, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_tr_t2021, climate_response_legitimacy__adaptation_priority, theater_ratio, 2021, 0.47).
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_tr_t2027, climate_response_legitimacy__adaptation_priority, theater_ratio, 2027, 0.53).
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_tr_t2030, climate_response_legitimacy__adaptation_priority, theater_ratio, 2030, 0.55).
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_tr_t2035, climate_response_legitimacy__adaptation_priority, theater_ratio, 2035, 0.58).

% Extraction over time
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_be_t2009, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2009, 0.35).
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_be_t2015, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_be_t2021, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2021, 0.58).
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_be_t2027, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2027, 0.65).
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_be_t2030, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_be_t2035, climate_response_legitimacy__adaptation_priority, base_extractiveness, 2035, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_su_t2009, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2009, 0.25).
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_su_t2015, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2015, 0.32).
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_su_t2021, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2021, 0.38).
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_su_t2027, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2027, 0.42).
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_su_t2030, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2030, 0.44).
narrative_ontology:measurement(climate_response_legitimacy__adaptation_priority_su_t2035, climate_response_legitimacy__adaptation_priority, suppression_requirement, 2035, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__adaptation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, international_climate_finance_architecture).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, loss_and_damage_finance_facility).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, fossil_fuel_subsidy_regime).

% DUAL FORMULATION NOTE:
% This story is the adaptation_priority reading of the climate_response_legitimacy kernel. It differs from mitigation_priority (epsilon ~0.35, claimed rope) in that it accepts warming trajectory as given rather than betting on decoupling. It differs from degrowth_transformation (epsilon ~0.25, claimed scaffold) in that it preserves the wealthy-nation development model. All three readings share the kernel but instantiate different constraints with different beneficiary/victim structures and different ε. The adaptation_priority reading dominates the finance architecture and thus structurally influences the other two by defining the 'legitimate' space of action.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_legitimacy__adaptation_priority, institutional, 0.12).
constraint_indexing:directionality_override(climate_response_legitimacy__adaptation_priority, powerful, 0.15).
constraint_indexing:directionality_override(climate_response_legitimacy__adaptation_priority, moderate, 0.75).
constraint_indexing:directionality_override(climate_response_legitimacy__adaptation_priority, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
