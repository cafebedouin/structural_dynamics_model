% ============================================================================
% CONSTRAINT STORY: climate_response_action__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__adaptation_priority, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: climate_response_action__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Response: Resilience Investment Over Emissions Reduction
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the adaptation-priority reading of the contested
 *   'climate response action' kernel: the claim that climate policy should
 *   treat further temperature rise as substantially locked in and direct
 *   scarce capital toward resilience infrastructure and protection of
 *   vulnerable populations, rather than toward aggressive emissions
 *   reduction. This is a genuinely distinct constraint from the
 *   mitigation-priority reading (which treats temperature rise as still
 *   preventable below 2°C through emissions cuts) and the
 *   degrowth-transformation reading (which rejects GDP-growth-compatible
 *   technological substitution as the frame entirely). The three readings are
 *   not the same constraint measured differently — they instantiate different
 *   beneficiary/victim structures, different capital allocations, and
 *   different claims about what is fixed versus what is still a policy
 *   choice. This file covers ONLY the adaptation-priority reading.
 *
 * KEY AGENTS:
 *   - high_emissions_economies: agenda_setter/beneficiary (institutional/arbitrage) — sets the inevitability framing, retains emissions discretion
 *   - fossil_fuel_incumbents: beneficiary (organized/arbitrage) — benefits from removed mitigation pressure
 *   - resilience_infrastructure_contractors: beneficiary (organized/mobile) — captures the $540B annual investment stream
 *   - developing_nation_treasuries: payer (moderate/constrained) — funds the $350B North-South gap from limited fiscal capacity
 *   - unprotected_low_income_populations: payer (powerless/trapped) — bears impacts where protection doesn't arrive
 *   - small_island_states: payer/excluded (powerless/trapped) — faces existential loss adaptation cannot address
 *   - future_generations: payer (powerless/trapped) — inherits the accepted warming baseline
 *   - climate_finance_institutions: observer (institutional/analytical) — tracks the financing gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__adaptation_priority, 0.58).
domain_priors:suppression_score(climate_response_action__adaptation_priority, 0.42).
domain_priors:theater_ratio(climate_response_action__adaptation_priority, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__adaptation_priority, "Adaptation-Priority Climate Response: Resilience Investment Over Emissions Reduction").
narrative_ontology:topic_domain(climate_response_action__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__adaptation_priority, '9deac562-0171-407c-ae57-05e5e01c21c2').
narrative_ontology:cs_kernel_codification('9deac562-0171-407c-ae57-05e5e01c21c2', distributed).
narrative_ontology:cs_authority_grounding('9deac562-0171-407c-ae57-05e5e01c21c2', distributed).
narrative_ontology:cs_reading_relation('9deac562-0171-407c-ae57-05e5e01c21c2', climate_response_action__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('9deac562-0171-407c-ae57-05e5e01c21c2', climate_response_action__degrowth_transformation, influences).
narrative_ontology:cs_axiom('9deac562-0171-407c-ae57-05e5e01c21c2', foundational, committed_warming_is_substantially_fixed).
narrative_ontology:cs_axiom_status(committed_warming_is_substantially_fixed, holdable).
narrative_ontology:cs_axiom_grounding('9deac562-0171-407c-ae57-05e5e01c21c2', committed_warming_is_substantially_fixed, empirically_contingent).
narrative_ontology:cs_axiom('9deac562-0171-407c-ae57-05e5e01c21c2', foundational, protection_of_vulnerable_populations_has_first_claim_on_climate_capital).
narrative_ontology:cs_axiom_status(protection_of_vulnerable_populations_has_first_claim_on_climate_capital, holdable).
narrative_ontology:cs_axiom_grounding('9deac562-0171-407c-ae57-05e5e01c21c2', protection_of_vulnerable_populations_has_first_claim_on_climate_capital, deontological).
narrative_ontology:cs_reference_frame('9deac562-0171-407c-ae57-05e5e01c21c2', unfccc_common_but_differentiated_responsibility_baseline).
narrative_ontology:cs_drift_state('9deac562-0171-407c-ae57-05e5e01c21c2', post_paris_agreement_ndc_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9deac562-0171-407c-ae57-05e5e01c21c2', '').
narrative_ontology:cs_kernel_id(climate_response_action__adaptation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, high_emissions_economies).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, resilience_infrastructure_contractors).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, protected_coastal_property_owners).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, developing_nation_treasuries).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, unprotected_low_income_populations).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, small_island_states).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, temperature_rise_inevitability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the dominant framing in international climate finance forums that adaptation investment is the pragmatic response given 'already locked-in' warming. Continues near-term emissions at low marginal cost while directing finance toward resilience projects that primarily protect its own coastal and agricultural assets. Faces no binding mitigation obligation under this framing and retains full policy discretion over its own emissions trajectory.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, high_emissions_economies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, high_emissions_economies, beneficiary).

% Benefits directly from any framing that treats continued warming as settled and unavoidable rather than as a function of ongoing extraction decisions. Funds research and advocacy for adaptation-first policy because it removes political pressure on production volumes. Faces essentially no binding constraint under this reading and can continue operations while appearing to support 'realistic' climate policy.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, fossil_fuel_incumbents, beneficiary,
    organized, biographical, arbitrage, global).

% Captures a large and growing share of the $540B annual resilience investment stream — seawalls, flood barriers, climate-controlled agriculture, managed retreat logistics. Has a direct commercial interest in the scale and permanence of the adaptation-first framing and lobbies against mitigation policies that would reduce projected damage estimates driving contract sizing.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, resilience_infrastructure_contractors, beneficiary,
    organized, biographical, mobile, national).

% Owns high-value real estate in jurisdictions wealthy enough to fund seawalls, elevated infrastructure, and insurance backstops. Receives public and private protective investment while facing minimal exposure to the residual warming this framing accepts as inevitable; can relocate assets if protection ultimately fails.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, protected_coastal_property_owners, beneficiary,
    powerful, biographical, mobile, regional).

% Asked to fund or co-fund resilience infrastructure against a $350B North-South financing gap, often through debt rather than grants, while having contributed a small fraction of cumulative emissions. Fiscal capacity is limited by existing debt service burdens; declining the adaptation framework risks being left without either mitigation relief or adaptation finance.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, developing_nation_treasuries, payer,
    moderate, generational, constrained, national).

% Lives in areas where resilience investment does not arrive because it is not commercially or politically prioritized — informal settlements, rural floodplains, urban heat islands in poorer districts. Bears the direct physical and economic cost of climate impacts the adaptation framework accepts as the trade for avoiding stronger mitigation commitments. Has no meaningful capacity to relocate or self-insure.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, unprotected_low_income_populations, payer,
    powerless, immediate, trapped, local).

% Faces existential territorial loss under any warming trajectory this framing accepts as a baseline; adaptation investment cannot address submersion of sovereign territory the way mitigation could have. Advocates loudly in UNFCCC forums for aggressive mitigation but is structurally outvoted by larger emitters who prefer financing adaptation to committing to deeper emissions cuts.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, small_island_states, payer,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, small_island_states, excluded).

% Inherits whatever level of warming results from present-day mitigation deferral, plus whatever adaptation infrastructure does or does not persist. Has no seat in current financing negotiations and no capacity to renegotiate the temperature baseline this framing accepts as fixed.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Multilateral development banks and climate funds evaluate financing proposals, track the North-South gap, and produce independent assessments of whether adaptation investment is keeping pace with locked-in warming. Can shift capital allocation but does not set the underlying policy framing.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, climate_finance_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__adaptation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_response_action__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital deployment toward protecting populations and assets from climate impacts that are, at minimum, already partially locked in by historical emissions — genuine adaptation need exists regardless of future mitigation trajectory, and directing finance to resilience infrastructure solves a real and urgent protection problem.
% TRANSFER_FUNCTION: Moves scarce public and development finance toward resilience infrastructure (seawalls, flood defense, drought-resistant agriculture, managed retreat) primarily in and for wealthier, better-organized jurisdictions and asset holders, while low-income populations and small island states receive comparatively little protection and continue to bear escalating physical risk from warming that adaptation-first framing implicitly deprioritizes preventing.
% ABSENT_VOICES: Small island state delegations and frontline low-income communities raise mitigation-ambition demands in UNFCCC negotiations but are structurally outweighed by the voting and financing power of high-emissions economies; future generations have no representative seat in any current financing or policy negotiation.
% DISAPPEARANCE_RATIONALE: High-emissions economies and fossil fuel incumbents would experience little disruption if the adaptation-priority framing disappeared and mitigation obligations tightened instead — the world would 'rearrange' sharply for them in the form of new binding constraints. For small island states and low-income populations, the framing's disappearance in favor of aggressive mitigation would fundamentally change their survival odds; the framing's disappearance in favor of nothing (no coordinated response at all) would leave them worse off still. The verdict differs by which counterfactual replaces the constraint, which is itself contested between the sibling readings.
% FOUNDING_PROBLEM: Even under best-case mitigation trajectories, some further warming is already locked in by historical and near-term emissions, and vulnerable populations facing near-term climate impacts (floods, heatwaves, sea-level rise) need protective infrastructure now, independent of how aggressively future emissions are cut.
% FOUNDING_PROBLEM_CORROBORATION: IPCC physical science assessments (an independent, non-beneficiary scientific body) corroborate that some warming and associated impacts are already committed regardless of mitigation pathway, establishing a genuine adaptation need. However, independent development-finance analysts and small island state delegations — also outside the beneficiary set — separately attest that the adaptation-priority framing is being used by high-emissions economies and fossil incumbents to substitute for, rather than complement, mitigation commitment, which is a distinct and contested claim from the underlying physical need.
narrative_ontology:disappearance_verdict(climate_response_action__adaptation_priority, contested).
narrative_ontology:founding_problem_status(climate_response_action__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__adaptation_priority, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_action__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__adaptation_priority, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the framing does real, needed coordination work (protecting people from already-committed warming impacts is genuine) while simultaneously functioning as a mechanism by which high-emissions economies and fossil incumbents avoid tighter mitigation obligations at the cost of developing nations' fiscal capacity and frontline populations' safety — this dual character is precisely why the claimed type is tangled_rope rather than a clean rope or snare. Suppression (0.42) is moderate: no single actor is coerced into accepting the framing, but the structural asymmetry of who sets international financing agendas versus who must accept whatever terms are offered constitutes a soft form of suppression. Theater ratio (0.31) captures that a meaningful share of 'resilience investment' announcements function as diplomatic signaling relative to actual disbursed capital, a gap tracked by climate finance institutions. Accessibility collapse (0.48) is moderate — small island states and vulnerable populations can and do argue for the mitigation-priority alternative, so the alternative has not fully collapsed, but the practical political economy makes it difficult to enact. Resistance (0.55) reflects the vocal, sustained opposition from small island state coalitions and civil society climate justice movements.
 *
 * PERSPECTIVAL GAP:
 *   From the high-emissions-economy agenda-setter seat, this reads as pragmatic realism: warming is partially locked in, so protecting people is the responsible immediate priority. From the small island state and low-income population payer seats, the identical structure reads as a mechanism that launders continued high emissions into a humanitarian-sounding protection narrative while the underlying driver of harm goes unaddressed. The engine computes these as structurally different seat classifications from the same base data — this divergence is the object of analysis, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   High-emissions economies, fossil fuel incumbents, resilience contractors, and protected property owners sit near the beneficiary end of directionality: the framing either removes constraints on their behavior (continued emissions) or directs capital toward their commercial or asset interests. Developing nation treasuries, low-income populations, small island states, and future generations sit near the target end: they bear financing burdens, physical exposure, or inherited warming baselines without commensurate voice in setting the framework. The trapped exit options for small island states and future generations are structural, not chosen — there is no arbitrage available against sea-level rise or against an already-elapsed emissions trajectory.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — some warming is already locked in and needs immediate protective response — remains genuinely live per independent physical science assessment (IPCC), so this is not a pure mandatrophy case where the mandate has hollowed out entirely. What is contested is whether the adaptation-priority framing, AS CURRENTLY FINANCED AND POLITICALLY DEPLOYED, has expanded beyond addressing that live problem into serving as a substitute for mitigation commitment that its most powerful proponents would otherwise have to make. The tangled_rope classification (rather than snare) preserves the genuine coordination function while still registering the asymmetric extraction running through the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_claim_or_policy_choice,
    'Is ''temperature rise is substantially locked in'' a genuine physical constraint independent of near-term policy choices, or is it a framing that becomes true only because financing is directed toward adaptation instead of mitigation — i.e., is the inevitability partly self-fulfilling?',
    'Compare IPCC committed-warming estimates (which hold regardless of near-term policy) against the marginal warming attributable to emissions reductions foregone specifically because adaptation financing displaced mitigation financing in this period. If the marginal foregone-mitigation warming is small relative to already-committed warming, the inevitability claim is largely independent of this framing''s political deployment; if large, the framing is substantially self-fulfilling.',
    'If the inevitability is genuinely exogenous, this reading''s core premise is validated as responding to fixed physical reality (closer to a rope). If the inevitability is significantly self-fulfilling through financing displacement, the coordination story is substantially cover for extraction (closer to a snare), and the tangled_rope classification would need to shift toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_claim_or_policy_choice, empirical, 'Whether accepted warming is exogenously fixed or partly produced by this framing''s own capital allocation choices.').

omega_variable(
    financing_gap_beneficiary_capture,
    'Does the $350B North-South financing gap function as documented unmet need driving genuine reform pressure, or has the gap itself become a stable equilibrium that resilience contractors and high-emissions economies have limited incentive to close?',
    'Track whether committed vs. disbursed adaptation finance converges over the interval, and whether contractor lobbying positions correlate with gap persistence rather than gap closure.',
    'If the gap is closing, the tangled_rope''s coordination function is strengthening relative to its extraction function. If the gap persists or widens despite rising nominal commitments, theater_ratio and extractiveness trajectories should be revised upward and the classification reassessed toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financing_gap_beneficiary_capture, empirical, 'Whether the financing gap is a transitional problem being solved or a stable extractive equilibrium.').

omega_variable(
    kernel_framing_selection,
    'Given that the same underlying warming trajectory could be read through adaptation_priority, mitigation_priority, or degrowth_transformation lenses, what determines which reading dominates international policy discourse at a given time — genuine physical/economic constraint, or the relative organizational power of each reading''s beneficiaries?',
    'Compare the historical sequence of which reading has dominated UNFCCC negotiating text at each COP against the relative lobbying expenditure and voting bloc composition of high-emissions economies versus small island state / civil society coalitions across the same period.',
    'If reading dominance tracks physical/economic constraint tightening (e.g., adaptation gaining salience as committed warming becomes clearer), the kernel contest is substantially empirically resolvable. If it tracks lobbying power and voting bloc composition instead, the kernel contest is substantially a power contest dressed as a scientific/economic disagreement, which would support reading the sibling coexistence as adversarial rather than genuinely undetermined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_selection, conceptual, 'Whether the choice among kernel readings tracks empirical constraint or organizational power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__adaptation_priority, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2015, climate_response_action__adaptation_priority, theater_ratio, 2015, 0.18).
narrative_ontology:measurement_basis(clim_tr_t2015, observed).
narrative_ontology:measurement(clim_tr_t2019, climate_response_action__adaptation_priority, theater_ratio, 2019, 0.22).
narrative_ontology:measurement_basis(clim_tr_t2019, observed).
narrative_ontology:measurement(clim_tr_t2023, climate_response_action__adaptation_priority, theater_ratio, 2023, 0.27).
narrative_ontology:measurement_basis(clim_tr_t2023, observed).
narrative_ontology:measurement(clim_tr_t2027, climate_response_action__adaptation_priority, theater_ratio, 2027, 0.29).
narrative_ontology:measurement_basis(clim_tr_t2027, projected).
narrative_ontology:measurement(clim_tr_t2031, climate_response_action__adaptation_priority, theater_ratio, 2031, 0.3).
narrative_ontology:measurement_basis(clim_tr_t2031, projected).
narrative_ontology:measurement(clim_tr_t2035, climate_response_action__adaptation_priority, theater_ratio, 2035, 0.31).
narrative_ontology:measurement_basis(clim_tr_t2035, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t2015, climate_response_action__adaptation_priority, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement_basis(clim_be_t2015, observed).
narrative_ontology:measurement(clim_be_t2019, climate_response_action__adaptation_priority, base_extractiveness, 2019, 0.45).
narrative_ontology:measurement_basis(clim_be_t2019, observed).
narrative_ontology:measurement(clim_be_t2023, climate_response_action__adaptation_priority, base_extractiveness, 2023, 0.52).
narrative_ontology:measurement_basis(clim_be_t2023, observed).
narrative_ontology:measurement(clim_be_t2027, climate_response_action__adaptation_priority, base_extractiveness, 2027, 0.56).
narrative_ontology:measurement_basis(clim_be_t2027, projected).
narrative_ontology:measurement(clim_be_t2031, climate_response_action__adaptation_priority, base_extractiveness, 2031, 0.58).
narrative_ontology:measurement_basis(clim_be_t2031, projected).
narrative_ontology:measurement(clim_be_t2035, climate_response_action__adaptation_priority, base_extractiveness, 2035, 0.58).
narrative_ontology:measurement_basis(clim_be_t2035, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2015, climate_response_action__adaptation_priority, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement_basis(clim_su_t2015, observed).
narrative_ontology:measurement(clim_su_t2019, climate_response_action__adaptation_priority, suppression_requirement, 2019, 0.34).
narrative_ontology:measurement_basis(clim_su_t2019, observed).
narrative_ontology:measurement(clim_su_t2023, climate_response_action__adaptation_priority, suppression_requirement, 2023, 0.38).
narrative_ontology:measurement_basis(clim_su_t2023, observed).
narrative_ontology:measurement(clim_su_t2027, climate_response_action__adaptation_priority, suppression_requirement, 2027, 0.4).
narrative_ontology:measurement_basis(clim_su_t2027, projected).
narrative_ontology:measurement(clim_su_t2031, climate_response_action__adaptation_priority, suppression_requirement, 2031, 0.41).
narrative_ontology:measurement_basis(clim_su_t2031, projected).
narrative_ontology:measurement(clim_su_t2035, climate_response_action__adaptation_priority, suppression_requirement, 2035, 0.42).
narrative_ontology:measurement_basis(clim_su_t2035, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__adaptation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the climate_response_action kernel, each authored as an independent, ε-invariant constraint per the ε-invariance principle. adaptation_priority accepts warming as substantially fixed and reallocates capital to resilience (ε=0.58, tangled_rope). mitigation_priority treats temperature rise as still preventable via emissions reduction and carbon markets while preserving GDP growth (separate file, separate ε). degrowth_transformation rejects the growth-compatible frame entirely in favor of reduced throughput and sufficiency (separate file, separate ε). These are not the same constraint measured three ways — they have different beneficiary/victim structures, different capital flows, and different premises about what is fixed versus chosen. Network edges here are declared as mutual influence: which reading dominates international negotiating text materially shifts the political and financing viability of the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
