% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__adaptation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Adaptation-Priority Reading of the Climate Response Imperative
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This story instantiates the adaptation-priority reading of the climate
 *   response imperative kernel: the position that climate response should be
 *   organized primarily around resilience-building and damage reduction in
 *   exposed regions, treating emissions mitigation as an aspirational
 *   longer-horizon goal rather than an immediate binding obligation. Under
 *   this reading's own lights, the standing arrangement (adaptation-forward
 *   international climate diplomacy, chronically underdelivered mitigation
 *   pledges, expanding adaptation finance markets) is the arrangement under
 *   contest. The structural delta from the sibling readings is that
 *   present-day developing and low-emitting nations enter the victim set
 *   directly: they face immediate capital requirements for resilience
 *   infrastructure that they structurally cannot meet, while the emissions
 *   driving the damage continue to rise because mitigation is deferred —
 *   producing a vicious circle where nations least responsible for the crisis
 *   bear its highest and most immediate costs, and where the total adaptation
 *   bill compounds every year mitigation is delayed.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, 0.68).
domain_priors:suppression_score(climate_response_imperative__adaptation_priority_reading, 0.58).
domain_priors:theater_ratio(climate_response_imperative__adaptation_priority_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__adaptation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__adaptation_priority_reading, "Adaptation-Priority Reading of the Climate Response Imperative").
narrative_ontology:topic_domain(climate_response_imperative__adaptation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__adaptation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__adaptation_priority_reading, 'd74756e4-b02d-412d-a382-4f9513cfb0e2').
narrative_ontology:cs_kernel_codification('d74756e4-b02d-412d-a382-4f9513cfb0e2', distributed).
narrative_ontology:cs_authority_grounding('d74756e4-b02d-412d-a382-4f9513cfb0e2', distributed).
narrative_ontology:cs_reading_relation('d74756e4-b02d-412d-a382-4f9513cfb0e2', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('d74756e4-b02d-412d-a382-4f9513cfb0e2', climate_response_imperative__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('d74756e4-b02d-412d-a382-4f9513cfb0e2', foundational, locked_in_impacts_demand_immediate_resilience_investment).
narrative_ontology:cs_axiom_status(locked_in_impacts_demand_immediate_resilience_investment, holdable).
narrative_ontology:cs_axiom_grounding('d74756e4-b02d-412d-a382-4f9513cfb0e2', locked_in_impacts_demand_immediate_resilience_investment, empirically_contingent).
narrative_ontology:cs_axiom('d74756e4-b02d-412d-a382-4f9513cfb0e2', foundational, mitigation_timelines_may_reasonably_extend_beyond_present_political_cycle).
narrative_ontology:cs_axiom_status(mitigation_timelines_may_reasonably_extend_beyond_present_political_cycle, holdable).
narrative_ontology:cs_axiom_grounding('d74756e4-b02d-412d-a382-4f9513cfb0e2', mitigation_timelines_may_reasonably_extend_beyond_present_political_cycle, instrumental).
narrative_ontology:cs_reference_frame('d74756e4-b02d-412d-a382-4f9513cfb0e2', unfccc_common_but_differentiated_responsibilities).
narrative_ontology:cs_drift_state('d74756e4-b02d-412d-a382-4f9513cfb0e2', post_paris_agreement_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d74756e4-b02d-412d-a382-4f9513cfb0e2', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__adaptation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, high_emitting_industrial_economies).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, adaptation_finance_and_consulting_sector).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, low_lying_coastal_nations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, sahel_agricultural_communities).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, small_island_developing_states).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, future_generations_in_exposed_regions).
narrative_ontology:constraint_vindicates(climate_response_imperative__adaptation_priority_reading, climate_adaptation_is_the_pragmatic_response).
narrative_ontology:constraint_vindicates(climate_response_imperative__adaptation_priority_reading, resilience_building_is_achievable_without_emissions_cuts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the terms of international climate negotiation, framing resilience and adaptation financing as the primary deliverable while treating binding emissions cuts as aspirational targets missed cycle after cycle. Continue to benefit from existing energy infrastructure and industrial base while adaptation funding pledges substitute for the harder political cost of decarbonization. Can relocate capital, insure against climate risk, and absorb costs that are catastrophic elsewhere.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, high_emitting_industrial_economies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__adaptation_priority_reading, high_emitting_industrial_economies, beneficiary).

% Extract continued value from fossil infrastructure as long as the dominant policy frame treats adaptation as the practical near-term response and mitigation as a longer-horizon aspiration. Fund research, lobbying, and public messaging that favors resilience narratives over binding phase-out timelines, since adaptation framing does not require them to change their core business model.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, fossil_fuel_incumbents, beneficiary,
    institutional, biographical, arbitrage, global).

% Builds a growing industry around resilience infrastructure, climate risk consulting, insurance products, and adaptation project financing. Revenue scales with the volume of adaptation need, creating an institutional interest in adaptation remaining the dominant frame rather than the underlying emissions problem being solved.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, adaptation_finance_and_consulting_sector, beneficiary,
    organized, biographical, mobile, global).

% Face rising seas, saltwater intrusion, and repeated storm damage now, while contributing a negligible fraction of historical emissions. Must divert scarce sovereign budget and borrow against future revenue to build seawalls and relocate infrastructure, entering a debt spiral to adapt to a crisis they did not create and whose root cause continues unabated abroad.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, low_lying_coastal_nations, payer,
    powerless, civilizational, trapped, national).

% Experience desertification and rainfall disruption that erodes subsistence agriculture year over year. Depend on international adaptation aid that is chronically underfunded relative to pledges, while global mitigation commitments continue to be pushed to later decades, meaning the physical driver of their crisis keeps intensifying.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, sahel_agricultural_communities, payer,
    powerless, generational, trapped, regional).

% Face existential territorial loss and must fund adaptation and eventual managed retreat with minimal fiscal capacity and no meaningful ability to compel binding mitigation from major emitters. Their negotiating bloc voices at UN climate forums are heard but rarely translate into enforceable mitigation commitments — their central demand is structurally sidelined by the adaptation-first frame.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, small_island_developing_states, payer,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__adaptation_priority_reading, small_island_developing_states, excluded).

% Inherit both a warmer baseline climate and a smaller window in which adaptation remains physically feasible, because deferred mitigation compounds the damage that adaptation spending will eventually have to absorb. Have no voice in current negotiations and no capacity to alter the trajectory being set today.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, future_generations_in_exposed_regions, payer,
    powerless, civilizational, trapped, global).

% Track the gap between pledged adaptation finance and disbursed funds, and between mitigation targets and actual emissions trajectories, across successive COP cycles. Their reports document the widening shortfall but carry no independent enforcement power over either finance flows or mitigation commitments.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, climate_finance_negotiators, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__adaptation_priority_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_response_imperative__adaptation_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a real and necessary global response to unavoidable near-term climate impacts — building seawalls, drought-resistant agriculture, early warning systems, and managed retreat — for damage that is already locked in regardless of future mitigation success.
% TRANSFER_FUNCTION: Moves the practical burden of climate response from the historically high-emitting economies (who continue emitting while mitigation remains aspirational) onto the exposed, low-emitting regions, who must self-finance survival infrastructure now while global emissions continue to raise the total damage they will eventually have to adapt to.
% ABSENT_VOICES: Future generations in exposed regions have no seat in current negotiations. Present populations of small island states and Sahel communities are nominally represented in UN forums but their core mitigation demands are structurally deprioritized relative to the adaptation-finance agenda that dominant economies prefer to fund.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority framing disappeared and were replaced by binding, front-loaded mitigation commitments, the entire structure of who bears near-term cost would shift: high emitters would face immediate decarbonization costs instead of deferring them into future adaptation bills paid mostly by others. The adaptation finance and consulting sector would shrink relative to a mitigation-first regime, and exposed nations would face a bounded rather than open-ended damage trajectory.
% FOUNDING_PROBLEM: International climate diplomacy needed a response to climate impacts that were already unavoidable even under optimistic mitigation scenarios — building the case that adaptation and resilience investment is a necessary complement to mitigation, not a substitute for it.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group II assessments and the UNFCCC Loss and Damage mechanism negotiations — bodies outside the beneficiary set of high-emitting economies — corroborate that adaptation is a genuine and necessary function, but also document that adaptation finance pledges (the $100bn/year commitment) have been chronically underdelivered while mitigation targets have been repeatedly missed, supporting the reading that adaptation has drifted from complement to substitute in practice, not merely in this reading's own framing.
narrative_ontology:disappearance_verdict(climate_response_imperative__adaptation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__adaptation_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__adaptation_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_imperative__adaptation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__adaptation_priority_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.68 and rising over the interval: it is not that adaptation spending itself is extractive (the coordination function — building sea walls, drought-resistant crops, early warning systems — is genuine and necessary), but that the adaptation-priority FRAME extracts a structural concession from exposed nations: it substitutes for binding mitigation commitments from the historically responsible economies, shifting cost onto those least able to pay while a growing consulting/finance sector profits from the volume of adaptation need itself. Suppression (0.58) reflects the structural lock exposed nations face: no realistic exit from needing adaptation funds, and no leverage to compel mitigation from major emitters through the same negotiating channels. Theater ratio (0.42, rising) captures the growing gap between adaptation pledges announced at climate summits and finance actually disbursed — a substitution of visible commitment-making for material transfer.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of high-emitting economies, this looks like pragmatic, incremental progress: real money flowing to real resilience projects, with mitigation continuing on its own separate (if slower) track. From the seat of low-lying coastal nations, the same structure looks like being handed a shrinking window and an unpayable bill for a crisis authored elsewhere — the coordination story and the extraction story are the same set of facts read from opposite structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   High-emitting industrial economies and fossil fuel incumbents sit near the full-beneficiary end: the adaptation-priority frame lets them defer the political and economic cost of decarbonization indefinitely while appearing responsive through adaptation finance pledges. The adaptation finance and consulting sector benefits from the volume of unmet need rather than from the need being resolved. Low-lying coastal nations, Sahel communities, and SIDS sit near the full-target end: trapped exit options (no alternative territory, no capacity to unilaterally compel global mitigation), immediate capital requirements they cannot internally generate, and a cost that compounds every year global emissions continue rising. Future generations in exposed regions are the most extreme target: zero voice in current negotiation and inherit both a degraded baseline and a shrinking adaptation window.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that some climate impacts are already locked in and require resilience investment regardless of future mitigation success — remains genuinely live; this is not a pure zombie mandate. The mandatrophy concern is narrower: the founding justification (adaptation as NECESSARY COMPLEMENT to mitigation) has been used to license a practical arrangement (adaptation as SUBSTITUTE, with mitigation aspirational) that a purely coordination-based reading would not license. Classifying this as tangled_rope rather than snare or pure rope preserves both halves: the coordination function (resilience infrastructure genuinely helps exposed populations survive) is real, but it operates alongside, and partly launders, an asymmetric extraction (deferred mitigation cost socialized onto those least responsible). A pure snare framing would miss that adaptation spending has genuine victim-side value; a pure rope framing would miss the enforcement-like persistence of underdelivered mitigation commitments alongside overdelivered adaptation rhetoric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_complement_or_substitute,
    'Is adaptation-priority framing operating as a genuine complement to mitigation efforts occurring on a separate track, or has it become a practical substitute that reduces political pressure for binding mitigation commitments?',
    'Track whether global mitigation pledges (NDCs) tighten or loosen in years following major adaptation finance announcements; a substitution effect would show mitigation ambition stagnating or declining as adaptation commitments rise.',
    'If substitution is confirmed, the tangled_rope classification is well-supported and the extraction component is understated; if adaptation and mitigation genuinely move together, the constraint is closer to a rope with real coordination value and less extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_complement_or_substitute, empirical, 'Whether adaptation-priority framing substitutes for or complements mitigation.').

omega_variable(
    kernel_reading_selection_pressure,
    'Why has the adaptation-priority reading gained institutional dominance over the mitigation-priority and degrowth readings within actual international negotiation outcomes, despite mitigation-priority framing being more prominent in public rhetoric?',
    'Comparative analysis of COP outcome texts, finance disbursement data, and the revealed preference of which commitments are legally binding (adaptation finance pledges are typically non-binding; mitigation targets are nominally binding but weakly enforced) versus which commitments attract concrete institutional machinery.',
    'If adaptation-priority is revealed as the operative institutional default despite mitigation-priority rhetoric, this reading''s tangled_rope classification is strengthened as the description of actual practice rather than stated intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Which reading of the kernel actually governs institutional practice versus public framing.').

omega_variable(
    responsibility_cost_inversion_magnitude,
    'How large is the actual gap between historical emissions responsibility and current adaptation cost burden across nations, and is this gap widening or stabilizing?',
    'Compile historical cumulative emissions data by nation against current adaptation spending as a share of GDP by nation; track the correlation coefficient over time.',
    'A widening and strongly negative correlation (low historical emitters bearing the highest adaptation cost burden) would sharpen the victim-set justification for low_lying_coastal_nations, sahel_agricultural_communities, and small_island_developing_states; a weak or closing correlation would weaken the extraction claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(responsibility_cost_inversion_magnitude, empirical, 'Empirical magnitude of the responsibility-cost inversion this reading claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__adaptation_priority_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__adaptation_priority_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t5, climate_response_imperative__adaptation_priority_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(clim_tr_t10, climate_response_imperative__adaptation_priority_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(clim_tr_t15, climate_response_imperative__adaptation_priority_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(clim_tr_t20, climate_response_imperative__adaptation_priority_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(clim_tr_t25, climate_response_imperative__adaptation_priority_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(clim_tr_t30, climate_response_imperative__adaptation_priority_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(clim_be_t5, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(clim_be_t10, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(clim_be_t15, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(clim_be_t20, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(clim_be_t25, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(clim_be_t30, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(clim_su_t5, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(clim_su_t10, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement(clim_su_t15, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 15, 0.53).
narrative_ontology:measurement(clim_su_t20, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(clim_su_t25, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 25, 0.57).
narrative_ontology:measurement(clim_su_t30, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__adaptation_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__adaptation_priority_reading, 0.15).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, degrowth_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the climate_response_imperative kernel. mitigation_priority_reading authors a distinct ε for the standing arrangement as that reading's advocates see it (emissions reduction via technology and markets, adaptation treated as residual) — a different victim set (future populations globally, rather than present exposed-region populations specifically) and a different extraction profile (extraction borne by industries facing decarbonization costs, versus extraction borne by exposed nations facing unmet adaptation need here). degrowth_reading authors ε for the standing arrangement as its advocates see it (incremental adaptation/mitigation within a growth-committed global economy), typically with the highest extractiveness of the three readings since it locates the coordination failure at the level of the growth paradigm itself, implicating both mitigation and adaptation infrastructure as insufficient patches. All three share the same kernel object (the climate_response_imperative) but are structurally distinct constraints per the ε-invariance principle — none is a measurement of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
