% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Adaptation-Priority Climate Response (2-3°C Acceptance)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The adaptation-priority reading of the climate response obligation kernel
 *   accepts 2-3°C warming as inevitable and frames the rational response as
 *   investing in resilience infrastructure rather than costly prevention
 *   (mitigation). This reading gained dominance in policy circles after the
 *   Copenhagen Accord (2009) and was cemented by the Paris Agreement's
 *   nationally determined contributions structure, which treats mitigation as
 *   voluntary while adaptation finance remains chronically underfunded. The
 *   constraint operates as a tangled_rope: it performs a genuine coordination
 *   function (mobilizing adaptation finance, standardizing resilience
 *   metrics, creating insurance mechanisms) while simultaneously extracting
 *   from future generations and the Global South by locking in warming levels
 *   they did not choose and cannot escape. The coordination function is real
 *   — climate impacts require adaptation — but the extraction is asymmetric:
 *   the beneficiaries (current high-income populations, fossil capital) avoid
 *   transition costs while the victims (future generations, Global South)
 *   bear the unmitigated impacts. Active enforcement is required because the
 *   constraint must continuously suppress mitigation alternatives (carbon
 *   pricing, fossil fuel phaseout, demand-side measures) and police the
 *   boundary of 'acceptable' climate discourse.
 *
 * KEY AGENTS:
 *   - current_generation_high_income: Primary beneficiary (powerful/mobile) — avoids transition costs, captures adaptation investment
 *   - fossil_capital_incumbents: Primary beneficiary (institutional/arbitrage) — protects asset values, delays stranded asset recognition
 *   - adaptation_finance_institutions: Secondary beneficiary (organized/constrained) — captures growing adaptation finance flows
 *   - future_generations_global: Primary victim (powerless/trapped) — bears unmitigated impacts without voice or exit
 *   - global_south_populations: Primary victim (powerless/constrained) — bears disproportionate impacts with minimal adaptation finance
 *   - low_lying_island_states: Acute victim (powerless/trapped) — faces existential threat from locked-in sea level rise
 *   - climate_vulnerable_ecosystems: Non-agent victim (excluded) — bears irreversible losses
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.72).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Adaptation-Priority Climate Response (2-3°C Acceptance)").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, '9303eee1-aab2-4593-9336-e667d7268bdb').
narrative_ontology:cs_kernel_codification('9303eee1-aab2-4593-9336-e667d7268bdb', formalized).
narrative_ontology:cs_authority_grounding('9303eee1-aab2-4593-9336-e667d7268bdb', extraction).
narrative_ontology:cs_interpretation_layer_present('9303eee1-aab2-4593-9336-e667d7268bdb').
narrative_ontology:cs_reading_relation('9303eee1-aab2-4593-9336-e667d7268bdb', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('9303eee1-aab2-4593-9336-e667d7268bdb', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('9303eee1-aab2-4593-9336-e667d7268bdb', foundational, mitigation_cost_exceeds_adaptation_benefit).
narrative_ontology:cs_axiom_status(mitigation_cost_exceeds_adaptation_benefit, holdable).
narrative_ontology:cs_axiom_grounding('9303eee1-aab2-4593-9336-e667d7268bdb', mitigation_cost_exceeds_adaptation_benefit, empirically_contingent).
narrative_ontology:cs_axiom('9303eee1-aab2-4593-9336-e667d7268bdb', foundational, intergenerational_discount_rate_justified).
narrative_ontology:cs_axiom_status(intergenerational_discount_rate_justified, holdable).
narrative_ontology:cs_axiom_grounding('9303eee1-aab2-4593-9336-e667d7268bdb', intergenerational_discount_rate_justified, conventional).
narrative_ontology:cs_axiom('9303eee1-aab2-4593-9336-e667d7268bdb', secondary, technological_adaptation_potential_unbounded).
narrative_ontology:cs_axiom_status(technological_adaptation_potential_unbounded, holdable).
narrative_ontology:cs_axiom_grounding('9303eee1-aab2-4593-9336-e667d7268bdb', technological_adaptation_potential_unbounded, empirically_contingent).
narrative_ontology:cs_reference_frame('9303eee1-aab2-4593-9336-e667d7268bdb', unfccc_prevention_mandate).
narrative_ontology:cs_drift_state('9303eee1-aab2-4593-9336-e667d7268bdb', post_paris_agreement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9303eee1-aab2-4593-9336-e667d7268bdb', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, current_generation_high_income).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, fossil_capital_incumbents).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, adaptation_finance_institutions).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations_global).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_populations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, low_lying_island_states).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, climate_vulnerable_ecosystems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Avoids the transition costs of rapid decarbonization (energy system transformation, demand reduction, asset stranding) while capturing the majority of adaptation investment flows (coastal defense, agricultural resilience, urban cooling). Can relocate or insure against residual climate risks. Their consumption patterns drive the emissions that make 2-3°C 'inevitable.'
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, current_generation_high_income, beneficiary,
    powerful, biographical, mobile, global).

% Actively shapes the adaptation-priority framing through lobbying, funding of integrated assessment modeling that embeds high discount rates, and strategic delay of mitigation policy. Protects trillions in asset values by preventing stranded asset recognition. Captures adaptation finance indirectly through 'resilience' contracts for fossil infrastructure hardening. Has arbitrage-grade exit via asset diversification and political capture.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, fossil_capital_incumbents, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__adaptation_priority, fossil_capital_incumbents, beneficiary).

% Multilateral development banks, climate funds, and private adaptation finance vehicles that capture growing adaptation finance flows. They benefit from the constraint's coordination function (standardized metrics, project pipelines) but are structurally constrained by the pledges of donor countries. Their institutional mandate aligns with the adaptation-priority frame.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, adaptation_finance_institutions, beneficiary,
    organized, generational, constrained, global).

% Bears the full stream of unmitigated climate damages (extreme heat, sea level rise, ecosystem collapse, agricultural disruption) without any voice in the decisions that locked in 2-3°C warming. No exit possible — they inhabit the world the constraint creates. Their interests are represented only through proxy advocates with no enforcement power.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, future_generations_global, payer,
    powerless, civilizational, trapped, universal).

% Bears disproportionate climate impacts (lethal heat, crop failure, water scarcity, displacement) while receiving a fraction of promised adaptation finance. Constrained exit: migration barriers, debt burdens, and historical responsibility inequities trap populations in high-vulnerability zones. Their adaptation needs are systematically underfunded while adaptation finance concentrates in wealthy regions.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, global_south_populations, payer,
    powerless, generational, constrained, global).

% Faces existential threat from locked-in sea level rise at 2-3°C (multi-meter commitment over centuries). No meaningful adaptation exists for territorial loss; the constraint's coordination function is literally impossible for this seat. Trapped by sovereignty and geography. Their advocacy in UNFCCC ('1.5 to stay alive') is the clearest articulation of the constraint's victim structure.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, low_lying_island_states, payer,
    powerless, generational, trapped, regional).

% Coral reefs, tropical forests, permafrost systems, and polar ecosystems that face irreversible collapse at 2-3°C warming. Non-agent victim included for structural completeness — the constraint extracts from biophysical systems that cannot advocate but whose loss cascades to human victims. Hard adaptation ceilings are breached for these systems at the accepted warming level.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_vulnerable_ecosystems, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__adaptation_priority, climate_vulnerable_ecosystems).

% Sees the full structure: the coordination function (adaptation is necessary) and the extraction function (prevention is suppressed to protect current beneficiaries). Does not bear costs or collect benefits from the constraint. The engine's analytical seat.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__adaptation_priority, fossil_capital_incumbents).
narrative_ontology:fixing_cost_class(climate_response_obligation__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes adaptation finance, standardizes resilience metrics, creates insurance and risk-pooling mechanisms, and coordinates disaster response infrastructure — genuine coordination functions that would be needed at any warming level.
% TRANSFER_FUNCTION: Transfers the cost of prevented warming (mitigation investment, stranded assets, demand reduction) from current high emitters and fossil capital to future generations and the Global South, who bear the residual damages of 2-3°C warming plus the costs of adaptation that is systematically underfunded.
% ABSENT_VOICES: Future generations are structurally excluded (cannot be present). Global South negotiators are present in UNFCCC but systematically outvoted and under-resourced in the technical bodies (IPCC, SBSTA) where the 'inevitability' of 2-3°C is produced. Climate-vulnerable ecosystems have no voice. The adaptation-priority framing dominates precisely because the seats that would object are excluded or overpowered.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority constraint vanished overnight, mitigation would become the primary policy frame: carbon pricing would expand, fossil fuel phaseout schedules would accelerate, adaptation finance would be reallocated from resilience projects to prevention, and the 'inevitability' narrative would collapse. The world would rearrange around a mitigation-priority or degrowth frame — the constraint actively structures the policy space.
% FOUNDING_PROBLEM: The UNFCCC (1992) was founded to 'prevent dangerous anthropogenic interference with the climate system' — a mitigation mandate. The adaptation-priority reading emerged after Kyoto's failure and Copenhagen's collapse as a pragmatic displacement: when prevention proved politically difficult for major emitters, the mandate shifted to 'managing the unavoidable.'
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (prevent dangerous interference) is attested as dead by IPCC AR6 WGIII (mitigation pathways to 1.5°C require immediate peak and rapid decline — not occurring), by the UNEP Emissions Gap Report (current policies → 2.8°C), and by Global South negotiators who document the mitigation mandate's abandonment. The adaptation-priority reading's own proponents (World Bank adaptation flagship reports, OECD resilience frameworks) implicitly concede the founding problem is dead by centering adaptation. No corroboration from outside the beneficiary set supports the claim that the founding problem is still live.
narrative_ontology:disappearance_verdict(climate_response_obligation__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__adaptation_priority, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__adaptation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_response_obligation__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__adaptation_priority, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction (0.68) is substantial: the constraint transfers mitigation costs from current emitters to future bearers of climate damages. The 0.35→0.68 trajectory reflects the hardening of the 'adaptation priority' framing from a pragmatic complement to mitigation (early 1990s) into a substitute for mitigation (post-2009). Suppression (0.72) is high because the constraint's persistence requires active exclusion of mitigation pathways from serious policy consideration — carbon pricing remains fragmented, fossil fuel subsidies persist, and the IPCC's mitigation scenarios are systematically filtered through integrated assessment models that embed high discount rates. Theater ratio (0.42) is moderate and rising: adaptation finance pledges (the $100B goal, Loss and Damage fund) perform coordination while delivering a fraction of committed funds, and resilience metrics standardize what cannot be adapted. Accessibility collapse (0.65) reflects that once the 2-3°C threshold is accepted as 'inevitable,' the mitigation alternative space collapses — not because physics forbids it, but because the institutional and cognitive infrastructure for prevention has been dismantled. Resistance (0.58) is significant but fragmented: climate movements, Global South negotiators, and youth litigation challenge the constraint but lack coordinated power to shift the dominant frame.
 *
 * PERSPECTIVAL GAP:
 *   The adaptation-priority seat (current high-income, fossil capital, adaptation institutions) experiences this as a pragmatic rope: climate change is happening, we must adapt, mitigation is too costly/slow. The mitigation-priority seat (future generations, Global South, island states, climate-vulnerable ecosystems) experiences it as a snare: the 'inevitability' of 2-3°C is a self-fulfilling prophecy produced by the very actors who benefit from avoiding mitigation. The degrowth seat experiences it as a piton: the constraint performs the theater of climate action while the material throughput driving the crisis accelerates. The engine computes this divergence from the structural data — the declared beneficiaries/victims, power/exit differentials, and the measurement series.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (current_generation_high_income, fossil_capital_incumbents, adaptation_finance_institutions) are structurally positioned to avoid transition costs and capture adaptation rents — their directionality d is low (near 0.1-0.2). Victims (future_generations_global, global_south_populations, low_lying_island_states) have zero exit options (trapped or constrained) and bear the full extractive weight — their d is high (near 0.9-1.0). The analytical observer has d=0.5 by definition. The suppression of mitigation alternatives is the enforcement mechanism that maintains this directional asymmetry: if mitigation were seriously pursued, the beneficiary extraction would collapse.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (coordinating a global response to climate change) was live in 1992 (UNFCCC) but the adaptation-priority reading emerged as a mandatrophic displacement: the original mitigation mandate (prevent dangerous anthropogenic interference) was substituted with an adaptation mandate (manage the unavoidable) precisely because the mitigation mandate threatened beneficiary interests. The constraint now persists not because it solves the coordination problem (it fails to prevent the warming that makes adaptation necessary) but because it redistributes the costs of climate change toward those with no political voice. This is a classic mandatrophy pattern — the mandate has outlived its function but the constraint remains because its beneficiaries are powerful and its victims are voiceless.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine coordination arrangement for climate resilience, or an extraction mechanism that protects fossil capital and current-generation consumption at the expense of future generations and the Global South?',
    'Compare the marginal cost of prevented warming (mitigation investment) against the marginal cost of adapted warming (resilience investment + residual damages) across income deciles and generations; assess whether the adaptation-priority framing was adopted by parties who would bear mitigation costs or by parties who would bear climate impacts.',
    'If extraction, the constraint is a tangled_rope or snare protecting identifiable beneficiaries; if genuine coordination, it is a rope or scaffold with sunset provisions. The kernel contest (adaptation_priority vs. mitigation_priority vs. degrowth_reading) turns on this structural distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the adaptation-priority framing is a natural law of climate economics or a constructed constraint benefiting current high emitters').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of mitigation alternatives structural (institutional lock-in, stranded asset politics, discount rate institutionalization) or internalized (normalized intergenerational discounting, technological optimism as cognitive barrier, adaptation framing as psychological comfort)?',
    'Track suppression persistence after policy windows open (e.g., post-Paris Agreement ratification, post-IPCC 1.5°C report): if mitigation alternatives remain suppressed despite formal commitment, classify as structural; if suppression tracks with narrative control and expert consensus manufacturing, classify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target populations (future generations, Global South) carry the suppression cognitively even after formal barriers are nominally removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of mitigation pathways').

omega_variable(
    adaptation_effectiveness_ceiling,
    'Does adaptation investment have a hard effectiveness ceiling at 2-3°C warming (biophysical limits to adaptation), making the coordination function itself fraudulent beyond a threshold?',
    'Assess IPCC AR6 WGII adaptation limits literature: identify warming thresholds where adaptation becomes impossible for specific systems (coral reefs, tropical agriculture, human habitability zones); map adaptation finance flows against those thresholds.',
    'If adaptation has hard ceilings below 2-3°C for critical systems, the constraint''s coordination claim is structurally false — it coordinates toward an impossible outcome, making it a snare rather than a tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_effectiveness_ceiling, empirical, 'Biophysical limits to adaptation at accepted warming levels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crap_tr_t0, climate_response_obligation__adaptation_priority, theater_ratio, 0, 0.18).
narrative_ontology:measurement(crap_tr_t5, climate_response_obligation__adaptation_priority, theater_ratio, 5, 0.24).
narrative_ontology:measurement(crap_tr_t10, climate_response_obligation__adaptation_priority, theater_ratio, 10, 0.31).
narrative_ontology:measurement(crap_tr_t15, climate_response_obligation__adaptation_priority, theater_ratio, 15, 0.36).
narrative_ontology:measurement(crap_tr_t20, climate_response_obligation__adaptation_priority, theater_ratio, 20, 0.39).
narrative_ontology:measurement(crap_tr_t25, climate_response_obligation__adaptation_priority, theater_ratio, 25, 0.41).
narrative_ontology:measurement(crap_tr_t30, climate_response_obligation__adaptation_priority, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(crap_be_t0, climate_response_obligation__adaptation_priority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(crap_be_t5, climate_response_obligation__adaptation_priority, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(crap_be_t10, climate_response_obligation__adaptation_priority, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(crap_be_t15, climate_response_obligation__adaptation_priority, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(crap_be_t20, climate_response_obligation__adaptation_priority, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(crap_be_t25, climate_response_obligation__adaptation_priority, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(crap_be_t30, climate_response_obligation__adaptation_priority, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(crap_su_t0, climate_response_obligation__adaptation_priority, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(crap_su_t5, climate_response_obligation__adaptation_priority, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(crap_su_t10, climate_response_obligation__adaptation_priority, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(crap_su_t15, climate_response_obligation__adaptation_priority, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(crap_su_t20, climate_response_obligation__adaptation_priority, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(crap_su_t25, climate_response_obligation__adaptation_priority, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(crap_su_t30, climate_response_obligation__adaptation_priority, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__adaptation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, fossil_fuel_subsidy_regime).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, international_adaptation_finance_architecture).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, loss_and_damage_fund_governance).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, carbon_budget_allocation_mechanisms).

% DUAL FORMULATION NOTE:
% This constraint is one member of the climate_response_obligation constraint family (kernel: climate_response_obligation). The family has three readings: adaptation_priority (this file, ε=0.68, tangled_rope), mitigation_priority (ε≈0.25, rope or scaffold), degrowth_reading (ε≈0.15, rope). The adaptation_priority reading influences the mitigation_priority reading by capturing the policy window and finance flows that would otherwise fund prevention. The degrowth_reading forecloses the adaptation_priority reading's growth-compatible framing within any single party's framework, but they coexist across different political coalitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__adaptation_priority, institutional, 0.15).
constraint_indexing:directionality_override(climate_response_obligation__adaptation_priority, powerful, 0.2).
constraint_indexing:directionality_override(climate_response_obligation__adaptation_priority, powerless, 0.95).
constraint_indexing:directionality_override(climate_response_obligation__adaptation_priority, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
