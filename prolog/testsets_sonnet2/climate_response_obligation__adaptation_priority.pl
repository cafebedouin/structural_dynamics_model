% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   This story instantiates the adaptation_priority reading of the contested
 *   climate_response_obligation kernel: the claim that 2-3°C of warming
 *   should be treated as a fixed constraint on policy, and that public and
 *   private investment should therefore prioritize resilience (seawalls, heat
 *   adaptation, agricultural resilience, insurance mechanisms) over further
 *   mitigation spending. The reading treats the emissions trajectory as
 *   settled and reallocates the debate to how best to live with the outcome.
 *   Two sibling readings of the same kernel are NOT this constraint:
 *   mitigation_priority treats the trajectory as still contingent on
 *   near-term decarbonization and argues intergenerational justice requires
 *   minimizing warming rather than accommodating it; degrowth_reading rejects
 *   the growth-and-efficiency frame entirely and argues for reduced material
 *   throughput to stay within planetary boundaries. Each reading has its own
 *   ε, beneficiary/victim structure, and classification — they are linked
 *   here only through cs_structure.reading_relations and
 *   network.affects_constraints, not merged.
 *
 * KEY AGENTS:
 *   - current_generation_high_emitters: primary beneficiary (organized/arbitrage) — avoids near-term transition costs
 *   - fossil_capital_incumbents: agenda-setting beneficiary (institutional/arbitrage) — protects asset value by stabilizing the inevitability narrative
 *   - wealthy_region_governments: agenda-setting beneficiary (institutional/mobile) — builds domestic resilience infrastructure with public funds
 *   - future_generations: primary victim (powerless/trapped) — inherits locked-in physical consequences with no voice in current allocation
 *   - global_south_frontline_populations: primary victim (powerless/trapped) — bears physical impacts without adaptation finance at needed scale
 *   - low_lying_island_states: excluded party (powerless/trapped) — faces harm no adaptation investment can remedy
 *   - climate_scientists_and_ipcc_reviewers: analytical observer — supplies the evidentiary basis for whether the 'inevitable' claim is technically sound
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.71).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.58).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.71).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Adaptation-Priority Reading of the Climate Response Obligation").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, '2e7c3c4f-4390-49e8-9fd1-1889af2df1cf').
narrative_ontology:cs_kernel_codification('2e7c3c4f-4390-49e8-9fd1-1889af2df1cf', distributed).
narrative_ontology:cs_authority_grounding('2e7c3c4f-4390-49e8-9fd1-1889af2df1cf', distributed).
narrative_ontology:cs_reading_relation('2e7c3c4f-4390-49e8-9fd1-1889af2df1cf', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('2e7c3c4f-4390-49e8-9fd1-1889af2df1cf', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('2e7c3c4f-4390-49e8-9fd1-1889af2df1cf', foundational, residual_warming_is_fixed_policy_input).
narrative_ontology:cs_axiom_status(residual_warming_is_fixed_policy_input, holdable).
narrative_ontology:cs_axiom_grounding('2e7c3c4f-4390-49e8-9fd1-1889af2df1cf', residual_warming_is_fixed_policy_input, empirically_contingent).
narrative_ontology:cs_axiom('2e7c3c4f-4390-49e8-9fd1-1889af2df1cf', secondary, resilience_investment_is_the_rational_response_to_locked_in_risk).
narrative_ontology:cs_axiom_status(resilience_investment_is_the_rational_response_to_locked_in_risk, holdable).
narrative_ontology:cs_axiom_grounding('2e7c3c4f-4390-49e8-9fd1-1889af2df1cf', resilience_investment_is_the_rational_response_to_locked_in_risk, instrumental).
narrative_ontology:cs_reference_frame('2e7c3c4f-4390-49e8-9fd1-1889af2df1cf', post_paris_carbon_budget_consensus).
narrative_ontology:cs_drift_state('2e7c3c4f-4390-49e8-9fd1-1889af2df1cf', contemporary_overshoot_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2e7c3c4f-4390-49e8-9fd1-1889af2df1cf', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, current_generation_high_emitters).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, fossil_capital_incumbents).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, wealthy_region_governments).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_frontline_populations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, low_lying_island_states).
narrative_ontology:constraint_vindicates(climate_response_obligation__adaptation_priority, warming_is_technically_unavoidable_at_two_to_three_degrees).
narrative_ontology:constraint_vindicates(climate_response_obligation__adaptation_priority, resilience_investment_is_more_tractable_than_rapid_decarbonization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Continues consumption and production patterns built on fossil energy without bearing the cost of a rapid transition. Frames the 2-3°C trajectory as already locked in, which converts continued emission into a non-decision rather than an ongoing choice. Captures the near-term economic benefit of deferred transition costs.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, current_generation_high_emitters, beneficiary,
    organized, biographical, arbitrage, national).

% Finances research, lobbying, and media framing that stabilizes the 'inevitability' narrative and channels public investment toward resilience infrastructure (seawalls, insurance backstops, air conditioning) rather than toward stranding fossil assets. Sets the policy agenda through sustained influence over energy and climate ministries; retains full ability to redirect capital but does not, because the adaptation framing protects existing asset value.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, fossil_capital_incumbents, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__adaptation_priority, fossil_capital_incumbents, beneficiary).

% Enacts adaptation budgets (flood defenses, heat-resilient grids, managed retreat funds) sized to protect their own populations and tax bases. Can relocate capital and populations internally and possesses the fiscal and engineering capacity to build resilience infrastructure domestically. Administers the choice between mitigation and adaptation spending and consistently tilts toward the latter.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, wealthy_region_governments, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__adaptation_priority, wealthy_region_governments, beneficiary).

% Inherits a climate system already committed to 2-3°C of warming and the compounding physical consequences (sea level rise, ecosystem collapse, agricultural disruption) that adaptation investment made today cannot retroactively prevent. Has no representation in the decisions being made now and no capacity to renegotiate the emissions budget already spent on their behalf.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Faces the physical impacts of warming (drought, flooding, heat stress, crop failure) without the fiscal capacity, insurance markets, or engineered infrastructure that wealthy regions are building for themselves. Adaptation finance pledged internationally arrives at a fraction of stated commitments and concentrates where it can attract co-investment, which is rarely where the physical risk is worst.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, global_south_frontline_populations, payer,
    powerless, generational, trapped, regional).

% Faces territorial loss for which no resilience investment is a substitute — there is no adaptation engineering that preserves a nation whose land disappears under sea level rise. Raises loss-and-damage and 1.5°C-consistent mitigation demands in international forums but lacks the negotiating leverage of major emitters and is structurally unable to force a change in the aggregate emissions trajectory that determines its survival.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, low_lying_island_states, excluded,
    powerless, civilizational, trapped, global).

% Assesses emissions trajectories, remaining carbon budgets, and the physical consequences of different warming pathways. Documents that 2-3°C is a policy trajectory contingent on near-term mitigation choices, not a fixed physical ceiling — supplying the evidentiary basis against which the 'inevitability' framing can be checked.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_scientists_and_ipcc_reviewers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__adaptation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_response_obligation__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates limited public investment on resilience infrastructure that protects people and assets already exposed to warming impacts — seawalls, heat-resilient grids, drought-tolerant agriculture, insurance backstops — rather than spreading investment across both mitigation and adaptation, on the premise that some warming is locked in regardless of near-term mitigation effort.
% TRANSFER_FUNCTION: Moves the cost of climate impact from present-tense transition disruption (borne by current fossil-dependent economies and capital) to future-tense physical harm (borne by future generations and populations without the fiscal capacity to adapt), while moving adaptation benefit from wherever physical risk is worst to wherever fiscal and engineering capacity is concentrated.
% ABSENT_VOICES: Future generations and low-lying island states would object that 'inevitable' warming is itself a product of current mitigation choices, not an independent physical fact — but neither has standing in the budget and policy processes that make the adaptation-priority allocation. Global South frontline populations are nominally represented in COP processes but without the negotiating leverage of major emitters or fossil capital.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority framing collapsed, mitigation finance and fossil asset stranding would move back to the center of policy debate, fossil capital's asset base would be re-priced against a faster phase-out, and public investment currently earmarked for resilience infrastructure in wealthy regions would face pressure to redirect toward decarbonization and toward loss-and-damage transfers to the Global South.
% FOUNDING_PROBLEM: Decades of insufficient mitigation action left a residual warming trajectory that will occur even under aggressive near-term decarbonization, creating a genuine practical need for resilience and adaptation investment regardless of mitigation policy.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports and independent carbon budget analyses (attesting from outside the fossil capital and current-emitter beneficiary set) corroborate that some further warming is now locked in by past emissions and support genuine adaptation need; the same sources also attest that the magnitude of future warming — and thus the adaptation burden imposed on future generations and the Global South — remains a function of near-term mitigation choices, contradicting the 'fixed and unavoidable' framing that the adaptation-priority reading uses to deprioritize mitigation spending.
narrative_ontology:disappearance_verdict(climate_response_obligation__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.71) and rising over the interval because the adaptation-priority framing, once institutionalized in budget allocations and international finance architecture, increasingly substitutes for mitigation spending rather than complementing it — the 'inevitability' claim becomes self-reinforcing as sunk resilience investment lengthens the horizon before mitigation becomes urgent again for wealthy regions. Suppression is moderate (0.58): there is no direct coercive enforcement against dissenting scientific voices, but there is a real narrative-suppression mechanism in how carbon-budget uncertainty gets rounded down to certainty in policy communication, and in how loss-and-damage claims from the Global South are structurally deprioritized in negotiation forums that current emitters control. Theater ratio rises to 0.44 because a growing share of "climate action" budget lines are adaptation projects that provide genuine but geographically concentrated protection, while functioning rhetorically as evidence of climate seriousness that displaces mitigation pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the wealthy-region-government seat, the arrangement looks like prudent, technically grounded risk management given a genuinely partially-locked-in warming trajectory. From the future-generations and Global South frontline seats, the identical allocation looks like extraction: the party best positioned to have prevented the harm chose instead to protect itself against the harm's consequences while leaving the harm's cause largely unaddressed. The engine should compute these as structurally different experiences of the same allocation, not as a disagreement about facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Current generation high emitters and fossil capital incumbents sit near the full-beneficiary end: they avoid transition costs and retain asset value, with mobile/arbitrage exit options that let them relocate capital ahead of physical or regulatory risk. Wealthy region governments are beneficiaries with institutional power and mobile exit — they can fund adaptation domestically. Future generations and global south frontline populations sit near the full-target end: trapped exit options (temporal immobility for the former, economic/geographic immobility for the latter), powerless power atom, and no capacity to renegotiate the emissions budget already committed. Low-lying island states are excluded rather than merely victimized — their harm (territorial loss) has no adaptation remedy, so they are outside the coordination the adaptation-priority reading offers at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — residual warming exists regardless of near-term mitigation, creating genuine adaptation need — is real and remains partially live (mandatrophy_resolved is false; this is not simply an obsolete mandate). What makes this a tangled_rope rather than a rope is that the reading extends the adaptation-need premise (genuinely live for already-committed warming) to justify deprioritizing mitigation for NOT-yet-committed warming, where the same premise does not hold. The coordination function (build resilience for locked-in warming) is real; the extraction (using that fact to avoid mitigation costs for warming that is not yet locked in) rides on top of it and requires active narrative maintenance — hence requires_active_enforcement is true.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_technical_vs_political,
    'Is 2-3°C warming a technically fixed outcome of past emissions (physical inevitability) or a projection contingent on near-term mitigation policy choices that remain politically live (constructed inevitability)?',
    'Compare IPCC remaining-carbon-budget estimates against actual near-term emissions pathways: if the range of plausible near-term mitigation scenarios still produces materially different warming outcomes by 2100, the trajectory is not yet physically locked in and the inevitability framing is a policy choice, not a physical fact.',
    'If the trajectory is substantially still contingent on near-term policy, the adaptation-priority reading''s core premise is a constructed inevitability that primarily benefits parties who avoid mitigation costs by asserting the outcome is already fixed — strengthening the case for tangled_rope classification. If genuinely physically locked in regardless of near-term action, the coordination function (resilience investment for unavoidable warming) is more purely rope-like at the margin already committed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_technical_vs_political, empirical, 'Whether the 2-3°C trajectory is a physical fact or a policy-contingent projection presented as fact.').

omega_variable(
    adaptation_finance_capture,
    'Does adaptation finance concentrate in wealthy regions because physical risk is concentrated there, or because fiscal/engineering capacity and co-investment requirements systematically exclude the highest-risk, lowest-capacity regions?',
    'Compare the geographic distribution of pledged versus disbursed adaptation finance against independent physical vulnerability indices (e.g., climate risk indices weighting exposure, sensitivity, and existing adaptive capacity).',
    'If disbursement tracks fiscal capacity rather than physical vulnerability, the adaptation-priority reading''s resilience investment is itself extractive in its allocation, not just in its substitution for mitigation — sharpening the victim classification of Global South frontline populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_finance_capture, empirical, 'Whether adaptation finance follows risk or follows capacity to co-invest.').

omega_variable(
    kernel_framing_choice,
    'Is the appropriate unit of analysis the adaptation_priority reading alone (as authored here), or should it be evaluated jointly with mitigation_priority as two poles of a single resource-allocation constraint, since real-world climate finance is always some mixture of both?',
    'This is a conceptual framing choice, not an empirical one: per the ε-invariance principle, the two readings have different ε, different beneficiary/victim sets, and different classification, so they are authored as separate linked stories rather than one story with a mixture parameter. Adopting the alternative (single blended story) would erase the specific claim that deprioritizing mitigation is the extractive move.',
    'If a future author chose to merge the readings into one blended-allocation story, the extraction specific to prioritizing adaptation OVER mitigation would be diluted by whatever mitigation spending remains, likely producing a lower measured ε and a rope or scaffold classification instead of tangled_rope. The decomposition into separate readings is deliberate and preserves the sharper signal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether adaptation_priority should be a standalone reading or blended with mitigation_priority into one allocation constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__adaptation_priority, theater_ratio, 0, 0.25).
narrative_ontology:measurement(clim_tr_t5, climate_response_obligation__adaptation_priority, theater_ratio, 5, 0.3).
narrative_ontology:measurement(clim_tr_t10, climate_response_obligation__adaptation_priority, theater_ratio, 10, 0.34).
narrative_ontology:measurement(clim_tr_t15, climate_response_obligation__adaptation_priority, theater_ratio, 15, 0.38).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__adaptation_priority, theater_ratio, 20, 0.41).
narrative_ontology:measurement(clim_tr_t25, climate_response_obligation__adaptation_priority, theater_ratio, 25, 0.43).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__adaptation_priority, theater_ratio, 30, 0.44).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__adaptation_priority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t5, climate_response_obligation__adaptation_priority, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(clim_be_t10, climate_response_obligation__adaptation_priority, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(clim_be_t15, climate_response_obligation__adaptation_priority, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__adaptation_priority, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(clim_be_t25, climate_response_obligation__adaptation_priority, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__adaptation_priority, base_extractiveness, 30, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__adaptation_priority, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(clim_su_t5, climate_response_obligation__adaptation_priority, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(clim_su_t10, climate_response_obligation__adaptation_priority, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(clim_su_t15, climate_response_obligation__adaptation_priority, suppression_requirement, 15, 0.53).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__adaptation_priority, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(clim_su_t25, climate_response_obligation__adaptation_priority, suppression_requirement, 25, 0.57).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__adaptation_priority, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__adaptation_priority, 0.15).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__degrowth_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the climate_response_obligation kernel. adaptation_priority (this story) treats residual warming as fixed and reallocates investment toward resilience; mitigation_priority treats the trajectory as still contingent and argues for rapid decarbonization; degrowth_reading rejects the growth/efficiency frame underlying both. Each reading is authored as its own constraint with its own ε, stakeholders, and classification. adaptation_priority structurally influences mitigation_priority downstream: sustained adaptation investment and the 'locked-in' narrative reduce the political urgency and available fiscal headroom for the mitigation spending that mitigation_priority calls for, without logically foreclosing it — hence 'influences' rather than 'forecloses' in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
