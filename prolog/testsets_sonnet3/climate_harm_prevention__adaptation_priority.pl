% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__adaptation_priority, []).

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
 *   constraint_id: climate_harm_prevention__adaptation_priority
 *   human_readable: Adaptation-Priority Reading of Climate Harm Prevention
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the adaptation-priority reading of the
 *   climate_harm_prevention kernel: the claim that legitimate climate
 *   response should prioritize near-term resilience building for present
 *   populations because mitigation is politically and economically
 *   infeasible, accepting a higher warming trajectory as the price. This is a
 *   distinct constraint from the mitigation_priority reading (which holds
 *   emissions reduction is achievable and obligatory) and from the
 *   degrowth_reading (which holds that growth-compatible mitigation is
 *   impossible and planned contraction is required). Each reading has its own
 *   beneficiary/victim structure and its own epsilon; they are linked as
 *   siblings via network.affects_constraints and are not merged here.
 *
 * KEY AGENTS:
 *   - national_climate_ministries: sets the adaptation-priority budget allocation and administers the infeasibility premise as policy
 *   - present_coastal_and_urban_vulnerable_populations: primary near-term beneficiary of protective infrastructure
 *   - fossil_fuel_incumbents: structural beneficiary of continued extraction under a mitigation-deferred regime
 *   - future_generations: bears the compounding cost of a warming trajectory locked in by present deferral
 *   - small_island_states: faces existential loss under a warming level this reading treats as an acceptable tradeoff
 *   - climate_science_and_ipcc_bodies: analytical observer documenting the gap between claimed infeasibility and available mitigation pathways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.62).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.48).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Adaptation-Priority Reading of Climate Harm Prevention").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, '00bc02a0-d392-46e3-8570-9d60c42aba29').
narrative_ontology:cs_kernel_codification('00bc02a0-d392-46e3-8570-9d60c42aba29', distributed).
narrative_ontology:cs_authority_grounding('00bc02a0-d392-46e3-8570-9d60c42aba29', distributed).
narrative_ontology:cs_reading_relation('00bc02a0-d392-46e3-8570-9d60c42aba29', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('00bc02a0-d392-46e3-8570-9d60c42aba29', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('00bc02a0-d392-46e3-8570-9d60c42aba29', foundational, mitigation_politically_infeasible_within_relevant_timeframe).
narrative_ontology:cs_axiom_status(mitigation_politically_infeasible_within_relevant_timeframe, holdable).
narrative_ontology:cs_axiom_grounding('00bc02a0-d392-46e3-8570-9d60c42aba29', mitigation_politically_infeasible_within_relevant_timeframe, empirically_contingent).
narrative_ontology:cs_axiom('00bc02a0-d392-46e3-8570-9d60c42aba29', foundational, present_vulnerable_populations_have_priority_claim_over_future_generations).
narrative_ontology:cs_axiom_status(present_vulnerable_populations_have_priority_claim_over_future_generations, holdable).
narrative_ontology:cs_axiom_grounding('00bc02a0-d392-46e3-8570-9d60c42aba29', present_vulnerable_populations_have_priority_claim_over_future_generations, deontological).
narrative_ontology:cs_reference_frame('00bc02a0-d392-46e3-8570-9d60c42aba29', growth_compatible_gradual_transition_baseline).
narrative_ontology:cs_drift_state('00bc02a0-d392-46e3-8570-9d60c42aba29', post_paris_ratchet_failure, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('00bc02a0-d392-46e3-8570-9d60c42aba29', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_coastal_and_urban_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, high_emission_national_economies).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, small_island_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets budget allocation between adaptation infrastructure and emissions policy, and administers the political calculus that mitigation is not achievable within current electoral or fiscal cycles. Directs public funds toward seawalls, cooling centers, and resilient agriculture rather than carbon pricing or fossil phase-out, and defends this as the responsible use of limited political capital.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, national_climate_ministries, agenda_setter,
    institutional, biographical, arbitrage, national).

% Receives near-term flood defenses, heat mitigation, and emergency infrastructure funded by the adaptation-priority allocation. Benefits are real and immediate, but the underlying warming trajectory that necessitates them continues to rise, meaning the protections must be perpetually re-funded and expanded against an escalating baseline.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_coastal_and_urban_vulnerable_populations, beneficiary,
    moderate, immediate, constrained, national).

% Continues extraction and sale of fossil fuels largely undisturbed because political capital and public funds are directed toward adaptation rather than mitigation or phase-out mandates. Lobbies to reinforce the 'mitigation is infeasible' framing that legitimizes this constraint, since it removes pressure on their core business model.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, fossil_fuel_incumbents, beneficiary,
    powerful, biographical, arbitrage, global).

% Avoids the near-term economic disruption of rapid decarbonization — no stranded assets, no abrupt industrial transition costs. Front-loads adaptation spending instead, which is politically easier to sell as protecting citizens rather than imposing costs on industry.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, high_emission_national_economies, beneficiary,
    institutional, biographical, arbitrage, national).

% Inherits a higher warming trajectory locked in by today's deferred mitigation, along with the compounding costs of ecosystem collapse, tipping-point risk, and adaptation limits that present populations do not face. Has no seat in current budget allocation or political bargaining and cannot renegotiate the emissions already committed on their behalf.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Lacks the fiscal, technical, and institutional capacity to build the resilience infrastructure that wealthier nations fund for their own populations. Bears the residual climate damage from a warming trajectory it did not choose and cannot afford to adapt to, while contributing little to the emissions driving it.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions, payer,
    powerless, generational, trapped, regional).

% Faces existential territorial loss at warming levels the adaptation-priority trajectory accepts as tolerable elsewhere. Adaptation is not a viable framing for a nation losing its land base — for these states, the reading's core premise (near-term resilience over mitigation) has no adaptation pathway that preserves their existence.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, small_island_states, payer,
    powerless, civilizational, trapped, global).

% Publishes warming trajectory projections and attribution science that document the gap between adaptation-priority outcomes and mitigation-priority outcomes. Provides the evidentiary basis against which the 'infeasibility' claim underlying this reading can be checked, without controlling policy allocation itself.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, climate_science_and_ipcc_bodies, observer,
    analytical, civilizational, analytical, global).

% Argues emissions reduction remains technically and economically feasible and that 'infeasibility' is a political choice dressed as constraint. Largely locked out of the budget-setting rooms where the adaptation-priority allocation is decided, and characterizes the reading as a legitimation device for continued fossil dependence.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, mitigation_advocacy_coalitions, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__adaptation_priority, fossil_fuel_incumbents).
narrative_ontology:fixing_cost_class(climate_harm_prevention__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates present public resources toward protecting current populations from near-term climate harm — flood defense, heat resilience, agricultural adaptation — under a genuine political-economy constraint that rapid mitigation faces powerful incumbent resistance and short electoral time horizons.
% TRANSFER_FUNCTION: Moves fiscal and political capital toward adaptation infrastructure for present populations and away from mitigation investment, effectively transferring climate burden from the present to the future and from high-capacity to low-capacity regions.
% ABSENT_VOICES: Future generations have no representation in current budget allocation. Small island states and low-adaptation-capacity regions participate in international forums but lack the bargaining power to alter the trajectory that determines their fate. Mitigation advocacy coalitions are present in public discourse but structurally outside the rooms where the infeasibility premise is operationalized into budgets.
% DISAPPEARANCE_RATIONALE: If this reading's legitimacy claim collapsed — if 'mitigation is infeasible' stopped functioning as an accepted premise — political capital and public funds currently protected for adaptation spending would face pressure to redirect toward emissions reduction, fossil fuel incumbents would lose a key legitimating narrative, and the warming trajectory itself would become a contested rather than accepted parameter in budget politics.
% FOUNDING_PROBLEM: Governments faced genuine near-term political and economic barriers to rapid decarbonization — entrenched fossil infrastructure, employment dependency, and electoral cycles shorter than mitigation payoff horizons — while vulnerable populations faced immediate, escalating climate harm requiring protection now.
% FOUNDING_PROBLEM_CORROBORATION: National climate ministries and high-emission economies attest the infeasibility premise is live and binding given current political constraints. Independent climate economists and the mitigation advocacy coalitions — outside the beneficiary set — attest that decarbonization pathways remain technically and economically available and that 'infeasibility' substantially reflects incumbent political resistance rather than a hard physical or economic ceiling; IPCC synthesis reports document feasible mitigation pathways still available within relevant timeframes, which weighs against the founding problem being as fixed as this reading treats it.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__adaptation_priority, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 at interval end: substantial but not maximal, reflecting that real coordination value exists (present populations genuinely receive protective infrastructure) alongside a genuine transfer of risk and cost onto future and low-capacity populations who have no voice in the allocation. Suppression is moderate (0.48) because the constraint operates less through direct coercion and more through agenda-control — the infeasibility premise forecloses mitigation-priority budget lines from serious consideration in national politics, and fossil incumbents actively reinforce that premise. Theater ratio is moderate and rising (0.25 to 0.40) as adaptation spending increasingly functions as visible political performance addressing immediate constituent anxiety while doing nothing to arrest the trajectory driving future harm — a Goodhart-style substitution of visible resilience spending for the harder, less visible work of emissions reduction.
 *
 * PERSPECTIVAL GAP:
 *   From national climate ministries' seat, this reading is the coordinated, responsible use of scarce political capital under real constraint — a rope. From future generations' and small island states' seat, the same structure computes as extraction: a warming trajectory is being locked in for their benefit-free inheritance while present beneficiaries capture the protective spending. The engine's per-seat computation is expected to diverge sharply here, which is the intended diagnostic value of authoring this as a tangled_rope rather than reconciling the claim to a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Present vulnerable populations and high-emission economies sit near the beneficiary end: real protection now, no near-term cost imposed on their industrial base. Fossil fuel incumbents sit at the beneficiary extreme — arbitrage-grade exit, direct commercial benefit from continued extraction. Future generations and small island states sit at the full-target end: trapped exit options (they cannot renegotiate today's emissions), civilizational time horizon, and zero seat in the allocation decision that determines their outcomes. Low-adaptation-capacity regions are similarly trapped, bearing residual damage without the fiscal capacity wealthier adaptation-funding nations use to protect their own populations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine near-term political and fiscal barriers to rapid decarbonization plus urgent present vulnerability — is contested rather than resolved. It remains partly live (electoral cycles, entrenched fossil infrastructure) but the 'infeasibility' framing is also documented as a legitimation device sustained substantially by the same fossil incumbents who benefit from mitigation deferral, per corroboration from outside the beneficiary set. This is not a resolved mandatrophy case; it is an active site of contest over whether the mandate is genuinely load-bearing or increasingly performative cover for continued extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infeasibility_premise_authenticity,
    'Is the political/economic infeasibility of mitigation a genuine structural constraint, or a constructed narrative sustained by incumbent lobbying that could be dissolved by different political coalitions?',
    'Comparative policy analysis across jurisdictions with similar starting conditions but different political coalitions — where mitigation-priority coalitions achieved rapid decarbonization despite comparable fossil dependency, the infeasibility premise weakens as a structural claim.',
    'If the premise is substantially constructed rather than structural, this reading''s claim to be the ''legitimate'' response is undermined and it reclassifies closer to snare (extraction dressed as necessity) rather than tangled_rope (genuine coordination plus extraction). If structural, the tangled_rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infeasibility_premise_authenticity, empirical, 'Whether mitigation infeasibility is genuine structural constraint or constructed narrative.').

omega_variable(
    adaptation_mitigation_tradeoff_reality,
    'Is adaptation spending genuinely in fiscal/political competition with mitigation spending, or could both be pursued simultaneously without the tradeoff this reading assumes?',
    'Budget allocation studies examining whether nations that increased adaptation spending correspondingly decreased mitigation investment, versus nations that pursued both without tradeoff.',
    'If the tradeoff is largely false, the reading''s core justification (mitigation is infeasible so we must choose adaptation) loses force and the beneficiary structure looks more like elective extraction than necessity-driven coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_mitigation_tradeoff_reality, empirical, 'Whether adaptation and mitigation spending are genuinely in zero-sum competition.').

omega_variable(
    reading_selection_as_kernel_framing,
    'Given three coherent readings of the same kernel (adaptation_priority, mitigation_priority, degrowth_reading), what determines which reading a given political coalition adopts, and is that selection itself capturable by the parties who benefit most from each reading?',
    'Track which coalitions (fossil incumbents, mitigation advocates, degrowth movements) fund or promote which reading''s framing in public discourse and legislative testimony.',
    'If reading-selection correlates strongly with which coalition benefits from the resulting allocation, this suggests the kernel-contest itself is a site of strategic framing rather than good-faith policy disagreement — relevant to how much weight the ''legitimate response'' language in each reading''s premise should carry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_as_kernel_framing, conceptual, 'Whether kernel-reading selection tracks beneficiary interest rather than independent policy judgment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__adaptation_priority, theater_ratio, 0, 0.25).
narrative_ontology:measurement(clim_tr_t6, climate_harm_prevention__adaptation_priority, theater_ratio, 6, 0.29).
narrative_ontology:measurement(clim_tr_t12, climate_harm_prevention__adaptation_priority, theater_ratio, 12, 0.32).
narrative_ontology:measurement(clim_tr_t18, climate_harm_prevention__adaptation_priority, theater_ratio, 18, 0.35).
narrative_ontology:measurement(clim_tr_t24, climate_harm_prevention__adaptation_priority, theater_ratio, 24, 0.38).
narrative_ontology:measurement(clim_tr_t30, climate_harm_prevention__adaptation_priority, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__adaptation_priority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t6, climate_harm_prevention__adaptation_priority, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(clim_be_t12, climate_harm_prevention__adaptation_priority, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(clim_be_t18, climate_harm_prevention__adaptation_priority, base_extractiveness, 18, 0.57).
narrative_ontology:measurement(clim_be_t24, climate_harm_prevention__adaptation_priority, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(clim_be_t30, climate_harm_prevention__adaptation_priority, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__adaptation_priority, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(clim_su_t6, climate_harm_prevention__adaptation_priority, suppression_requirement, 6, 0.36).
narrative_ontology:measurement(clim_su_t12, climate_harm_prevention__adaptation_priority, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(clim_su_t18, climate_harm_prevention__adaptation_priority, suppression_requirement, 18, 0.43).
narrative_ontology:measurement(clim_su_t24, climate_harm_prevention__adaptation_priority, suppression_requirement, 24, 0.46).
narrative_ontology:measurement(clim_su_t30, climate_harm_prevention__adaptation_priority, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the climate_harm_prevention kernel, each authored as a separate constraint per the ε-invariance principle: adaptation_priority (this story), mitigation_priority, and degrowth_reading. Each reading has a distinct beneficiary/victim structure and a distinct epsilon assessed by that reading's own lights against the standing arrangement it describes. The readings are linked here rather than merged because they instantiate structurally different claims about what the legitimate climate response is, not different measurements of one claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
