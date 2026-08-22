% ============================================================================
% CONSTRAINT STORY: climate_response_action__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Adaptation-Priority Climate Response Regime
 *   domain: political economy/climate policy/intergenerational ethics
 *
 * SUMMARY:
 *   Under the adaptation-priority organization of climate response,
 *   governments and development financiers direct several hundred billion
 *   dollars annually toward resilience infrastructure and adaptive capacity,
 *   formally accepting committed temperature rise as the planning basis and
 *   formally prioritizing vulnerable populations. The arrangement solves a
 *   real problem, since some warming is locked in and exposed populations
 *   need defenses during any transition, while embedding a large North-South
 *   financing gap, protection that concentrates where assets concentrate, and
 *   a political dividend for actors relieved of rapid mitigation obligations.
 *   KEY AGENTS (by structural relationship): developed_nation_governments —
 *   agenda-setter and net beneficiary (institutional/arbitrage);
 *   construction_engineering_consortia — primary contract beneficiary
 *   (powerful/mobile); fossil_fuel_producers — indirect beneficiary via the
 *   acceptance premise (institutional/arbitrage);
 *   multilateral_development_banks — intermediary collecting fees while
 *   administering flows (institutional/mobile); developing_nation_governments
 *   — principal payer seat (organized/constrained);
 *   vulnerable_populations_unprotected_regions — bodily bearers of the
 *   protection gap (powerless/trapped); climate_displaced_communities —
 *   bearers with no seat (powerless/trapped); future_generations — bearers
 *   absent by construction (powerless/trapped);
 *   independent_climate_economists — analytical observers
 *   (analytical/analytical). Family note: this file is one member of the
 *   climate_response_action family; see network.dual_formulation_note for how
 *   its epsilon relates to the sibling files.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__adaptation_priority, 0.64).
domain_priors:suppression_score(climate_response_action__adaptation_priority, 0.48).
domain_priors:theater_ratio(climate_response_action__adaptation_priority, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, extractiveness, 0.64).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__adaptation_priority, "Adaptation-Priority Climate Response Regime").
narrative_ontology:topic_domain(climate_response_action__adaptation_priority, "political economy/climate policy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_action__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__adaptation_priority, 'd4f109e9-f563-4a0d-8843-e70313409688').
narrative_ontology:cs_kernel_codification('d4f109e9-f563-4a0d-8843-e70313409688', distributed).
narrative_ontology:cs_authority_grounding('d4f109e9-f563-4a0d-8843-e70313409688', distributed).
narrative_ontology:cs_reading_relation('d4f109e9-f563-4a0d-8843-e70313409688', climate_response_action__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('d4f109e9-f563-4a0d-8843-e70313409688', climate_response_action__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('d4f109e9-f563-4a0d-8843-e70313409688', foundational, accepted_warming_as_planning_basis).
narrative_ontology:cs_axiom_status(accepted_warming_as_planning_basis, holdable).
narrative_ontology:cs_axiom_grounding('d4f109e9-f563-4a0d-8843-e70313409688', accepted_warming_as_planning_basis, empirically_contingent).
narrative_ontology:cs_axiom('d4f109e9-f563-4a0d-8843-e70313409688', foundational, vulnerable_population_protection_first).
narrative_ontology:cs_axiom_status(vulnerable_population_protection_first, holdable).
narrative_ontology:cs_axiom_grounding('d4f109e9-f563-4a0d-8843-e70313409688', vulnerable_population_protection_first, deontological).
narrative_ontology:cs_reference_frame('d4f109e9-f563-4a0d-8843-e70313409688', managed_warming_protection_framework).
narrative_ontology:cs_drift_state('d4f109e9-f563-4a0d-8843-e70313409688', contemporary_finance_architecture, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d4f109e9-f563-4a0d-8843-e70313409688', '').
narrative_ontology:cs_kernel_id(climate_response_action__adaptation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, construction_engineering_consortia).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, fossil_fuel_producers).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, multilateral_development_banks).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, developed_nation_governments).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, developing_nation_governments).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, vulnerable_populations_unprotected_regions).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, climate_displaced_communities).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, future_generations).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, committed_warming_planning_premise).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, bankable_resilience_investment_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the rules of adaptation finance through fund boards, donor coordination, and lending conditions, and direct the largest shares of announced adaptation capital. They can finance protection for their own territories from domestic budgets, and their firms win a large share of adaptation contracts. Their acceptance of committed warming reduces pressure for rapid domestic emissions cuts. Leaving the arrangement would mean ceding agenda control to rival framings, so they stay and shape it.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, developed_nation_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, developed_nation_governments, beneficiary).

% Bid on and deliver sea walls, resilient grids, water systems, and early-warning infrastructure funded by adaptation finance. Revenue scales with announced adaptation spending, and contracts concentrate in markets where procurement rules favor established Northern firms. Exit is easy: the same consortium serves energy, transport, and defense clients.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, construction_engineering_consortia, beneficiary,
    powerful, biographical, mobile, global).

% Operate under a response frame that treats warming as committed and centers defense rather than rapid supply-side phase-out. Every year the acceptance premise holds, production continues under a climate-policy umbrella. They fund adaptation-adjacent initiatives and cite protection spending as evidence of responsibility. Exit would mean writing down reserves; staying costs little.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, fossil_fuel_producers, beneficiary,
    institutional, biographical, arbitrage, global).

% Channel and blend adaptation finance, charge fees and interest, and attach policy conditions to disbursement. They report against adaptation targets and expand mandates accordingly. Their institutional identity is bound up with being the delivery vehicle for climate resilience; restructuring away from lender-led adaptation would unsettle their portfolio model.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, multilateral_development_banks, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, multilateral_development_banks, agenda_setter).

% Face adaptation needs exceeding their fiscal capacity by hundreds of billions annually. They choose among diverting development budgets, borrowing at elevated rates, or leaving populations exposed. Collective bargaining through the G77 bloc gives them voice in negotiations but not control over fund governance or disbursement conditions. Exiting the finance architecture means forfeiting access to the capital their protection depends on.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, developing_nation_governments, payer,
    organized, biographical, constrained, national).

% Live with flood, heat, drought, and storm exposure that adaptation finance reaches last and least. Defenses concentrate where capital and assets concentrate; their protection arrives as warnings and modest works, if at all. Migration is costly, dangerous, and often blocked by border regimes. They bear the gap between promised and delivered protection directly as bodily risk.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, vulnerable_populations_unprotected_regions, payer,
    powerless, immediate, trapped, regional).

% Are moving now, or will move, as regions pass livability thresholds. They appear in adaptation plans as projected numbers rather than participants; relocation decisions are made by governments and insurers. They would contest allocation priorities and demand mobility rights but hold no standing in finance governance.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, climate_displaced_communities, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, climate_displaced_communities, excluded).

% Inherit the warming the acceptance premise locks in, the debt issued to finance today's defenses, and the diminished adaptive margin left after decades of deferred prevention. They hold no seat in any negotiation and cannot exit the outcome.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, future_generations, excluded).

% Assess adaptation finance flows against exposure-weighted need, publish gap analyses, and testify to legislatures. They collect nothing from the arrangement and can criticize it freely; their influence runs through publication and advisory channels.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, independent_climate_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__adaptation_priority, construction_engineering_consortia).
narrative_ontology:fixing_cost_class(climate_response_action__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates scattered adaptation needs into financeable pipelines: pooled funds, standardized resilience metrics, and bankable projects let sea defenses, early-warning systems, drought-tolerant agriculture, and heat-resilient housing be built once at scale instead of improvised disaster by disaster. It addresses harms already committed and that no emissions pathway now eliminates.
% TRANSFER_FUNCTION: Moves roughly $540B annually in adaptation capital from developed-nation treasuries, development lenders, and private investors into infrastructure and protection programs, with disbursement governed by bankability criteria, leaving a $350B unfilled need in developing nations and delivering the densest protections to regions with the most assets to defend.
% ABSENT_VOICES: Residents of regions written off as unprotectable, communities already displaced, and future generations would all contest allocation priorities; none holds decision authority. Loss-and-damage advocates from exposed states won a fund at COP27, but its capitalization remains a fraction of estimated need and its board seats do not control the larger lending apparatus.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority apparatus vanished overnight, funded defense pipelines would halt mid-construction, multilateral adaptation windows would close, and the fiscal envelope now routed to resilience would be contested between rival response programs; exposed populations would face the same hazards with fewer warnings, weaker defenses, and no dedicated finance channel.
% FOUNDING_PROBLEM: By the mid-2010s it was clear that past emissions had committed the world to substantial warming regardless of mitigation success, and that coastal cities, farms, and water systems would absorb those impacts undefended unless someone financed defenses during the transition decades.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group II assessments corroborate committed warming and adaptation need from outside any benefiting party, and reinsurance loss series independently attest rising exposure. No outside party corroborates the stronger operative claim, that protection investment adequately substitutes for prevention, which is advanced chiefly by actors whose fiscal position or commercial book improves under the acceptance premise.
narrative_ontology:disappearance_verdict(climate_response_action__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_action__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__adaptation_priority, 0.64, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.64 because the arrangement delivers real protection (sea defenses, early warning, and resilient agriculture save lives now) while embedding a $350B financing gap that prices protection beyond developing nations' fiscal reach and concentrates delivered defenses where assets concentrate. Suppression is 0.48: the arrangement enforces itself through fund-governance control, disbursement conditionality, and agenda-setting rather than physical coercion; rival response programs survive openly but command little capital. Theater is 0.32 and rising: pledged adaptation finance systematically exceeds delivered finance, and resilience plans multiply faster than shovels. Accessibility_collapse is 0.35, since understanding the arrangement does not eliminate alternatives: mitigation-first and transformation programs remain institutionally live. Resistance is 0.60: G77 bloc demands for grant-based finance, loss-and-damage victories, and climate-justice mobilization meet the arrangement head-on. All three tracked series share one six-point grid; the trends are decadal smoothings over an annual COP-cycle oscillation in pledges and deliveries, which averages out rather than driving the signal. Suppression_requirement is tracked because the enforcement picture is not static: lender conditionality and blended-finance requirements hardened over the interval. The suppression mechanism is structural (fiscal conditionality, governance rules), not internalized.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats compute a very different arrangement from the payer seats. From developed-nation treasuries and bank boardrooms the structure reads as prudent risk management: fiduciary duty, bankable projects, measurable resilience outputs. From developing-nation finance ministries and unprotected communities the same structure reads as protection rationed by ability to pay, with equity language attached to allocation behavior that tracks creditworthiness. The engine computes this divergence from the structural data; the authored claim does not adjudicate it. Coalition capacity differentiates otherwise similar payer seats: developing_nation_governments hold organized power through the G77 bloc and have extracted concessions such as the loss-and-damage fund, while their dispersed constituents hold none.
 *
 * DIRECTIONALITY LOGIC:
 *   Construction consortia sit nearest the beneficiary pole: revenue scales with adaptation spending and exit is trivial. Fossil producers benefit indirectly but materially, since the acceptance premise is what keeps supply-side phase-out off the agenda, placing them well below symmetric. Multilateral banks collect fees and interest yet also genuinely intermediate scarce capital, landing them near the middle. Developed-nation governments are net beneficiaries (avoided mitigation costs, domestic contract capture, territorial protection) though they also contribute funds, holding them below symmetric. Developing-nation governments, unprotected populations, displaced communities, and future generations sit at or near the full-target pole: they bear the financing gap, the protection disparity, and the locked-in warming respectively, with trapped or structurally absent exit.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure coordination would erase the financing gap and the protection disparities; the extraction is not overhead, it is the allocation rule. Reading it as pure extraction would erase the lives that delivered defenses save and the genuine unavoidability of some warming. The hybrid classification holds both facts. On genealogy: the founding problem, committed warming demanding defense, is live and independently corroborated, so no mandatrophy is declared. The forward risk is different: if pledge-delivery divergence keeps widening, resilience planning could decay into performance (plans published, funds announced, defenses unbuilt), at which point the theater series, not the founding problem, becomes the diagnostic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the adaptation_priority reading of the climate_response_action kernel; how would classification shift under the mitigation_priority or degrowth_transformation readings of the same commitment?',
    'Comparative classification of the sibling constraint files; divergent verdicts locate the disagreement in specific structural elements, the acceptance premise versus the growth-compatibility premise.',
    'If sibling readings classify with materially different extraction over the same fiscal envelope, the contested element is the acceptance premise; if they converge, the extraction is a property of the response apparatus itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Kernel-level contest: this story is one reading of climate_response_action; siblings are separate constraints.').

omega_variable(
    lock_in_magnitude_dispute,
    'Is the warming treated as inevitable by the acceptance premise actually committed at the assumed magnitude, or does the premise overstate lock-in in ways that generate deferral rents?',
    'Attribution science and updated carbon-budget assessments compared against the planning assumptions embedded in adaptation finance instruments.',
    'Overstated lock-in converts part of the measured extraction into pure deferral rent and strengthens the mitigation reading; accurate lock-in confirms the defensive function as genuine coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lock_in_magnitude_dispute, empirical, 'Empirical status of the inevitability premise underlying the arrangement.').

omega_variable(
    financing_gap_structural_status,
    'Is the $350B North-South adaptation financing gap a durable structural feature of the arrangement or a transitional artifact of capital-market development?',
    'Decade-scale tracking of concessional flows, borrowing spreads for exposed states, and fund replenishment outcomes.',
    'A durable gap marks protection-by-ability-to-pay as built-in extraction; a closing gap supports the transitional-coordination reading and lowers effective extraction for developing-nation seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financing_gap_structural_status, empirical, 'Whether the financing gap is structural or transitional.').

omega_variable(
    equity_language_allocation_divergence,
    'Does actual fund allocation track the declared vulnerability of recipient populations, or does it track recipient fiscal capability and asset concentration?',
    'Exposure-weighted need indices matched against disbursed adaptation finance by region and income stratum.',
    'Capability-tracking allocation means the protection-priority axiom is overridden in practice, pushing the arrangement toward pure extraction for unprotected seats despite its equity framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_language_allocation_divergence, empirical, 'Whether equity rhetoric matches allocation behavior.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__adaptation_priority, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_action__adaptation_priority, theater_ratio, 0, 0.18).
narrative_ontology:measurement(clim_tr_t3, climate_response_action__adaptation_priority, theater_ratio, 3, 0.22).
narrative_ontology:measurement(clim_tr_t6, climate_response_action__adaptation_priority, theater_ratio, 6, 0.26).
narrative_ontology:measurement(clim_tr_t9, climate_response_action__adaptation_priority, theater_ratio, 9, 0.29).
narrative_ontology:measurement(clim_tr_t12, climate_response_action__adaptation_priority, theater_ratio, 12, 0.31).
narrative_ontology:measurement(clim_tr_t15, climate_response_action__adaptation_priority, theater_ratio, 15, 0.32).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_action__adaptation_priority, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(clim_be_t3, climate_response_action__adaptation_priority, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(clim_be_t6, climate_response_action__adaptation_priority, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(clim_be_t9, climate_response_action__adaptation_priority, base_extractiveness, 9, 0.61).
narrative_ontology:measurement(clim_be_t12, climate_response_action__adaptation_priority, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(clim_be_t15, climate_response_action__adaptation_priority, base_extractiveness, 15, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_action__adaptation_priority, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(clim_su_t3, climate_response_action__adaptation_priority, suppression_requirement, 3, 0.42).
narrative_ontology:measurement(clim_su_t6, climate_response_action__adaptation_priority, suppression_requirement, 6, 0.44).
narrative_ontology:measurement(clim_su_t9, climate_response_action__adaptation_priority, suppression_requirement, 9, 0.46).
narrative_ontology:measurement(clim_su_t12, climate_response_action__adaptation_priority, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(clim_su_t15, climate_response_action__adaptation_priority, suppression_requirement, 15, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__degrowth_transformation).

% DUAL FORMULATION NOTE:
% The colloquial label 'climate response' decomposes, per the epsilon-invariance principle, into three structurally distinct arrangements that share one contested kernel. This file authors epsilon only for the adaptation-priority arrangement: its extraction attaches to the North-South financing gap, capability-weighted protection disparities, and deferral rents from the acceptance premise, while its coordination function is genuine defense against committed warming. The mitigation-priority sibling authors epsilon for the emissions-reduction arrangement (offset-market integrity, delayed-cut rents, innovation-subsidy capture); the degrowth sibling authors epsilon for the throughput-transformation arrangement (sufficiency costs, transition burdens). The upstream/downstream structure is lateral rather than hierarchical: this reading influences the mitigation sibling through resource competition and carbon-budget erosion, and coexists with the degrowth sibling as rival live programs. All three files cross-link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
