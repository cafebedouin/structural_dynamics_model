% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__option_value_preserving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__option_value_preserving, []).

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
 *   constraint_id: acceptable_risk_energy__option_value_preserving
 *   human_readable: Multi-Pathway Option Preservation under Deep Uncertainty (Option-Value Reading of Acceptable Risk)
 *   domain: energy_policy/risk_assessment/decision_theory
 *
 * SUMMARY:
 *   Across most industrialized grids, a standing body of law and market
 *   design — capacity markets, zero-emission credit programs,
 *   retirement-review requirements, strategic fuel reserves — keeps more than
 *   one energy pathway commercially viable at any time, on the stated ground
 *   that no planner knows which pathway the future will need. This story
 *   instantiates the option_value_preserving reading of the
 *   acceptable_risk_energy kernel: the arrangement under contest is the
 *   multi-pathway maintenance regime itself, assessed by the reading's own
 *   lights. Its coordination function is genuine — real-option hedging under
 *   deep uncertainty — and its costs are equally real: subsidy transfers to
 *   incumbents, continued emissions from retained fossil capacity, and
 *   prolonged waste and accident exposure around retained plants. Per the
 *   epsilon-invariance principle this is one of three linked stories: the
 *   colloquial label 'acceptable risk' conflates three structurally distinct
 *   policy constraints, and the sibling readings (catastrophic_tail_dominant,
 *   expected_value_dominant) instantiate different constraints with different
 *   epsilon, victim sets, and classifications. KEY AGENTS (by structural
 *   relationship): - system_regulators: agenda-setter
 *   (institutional/constrained) — administers capacity markets and retirement
 *   reviews - nuclear_operators: beneficiary (institutional/constrained) —
 *   collects conditional support payments - fossil_generators: beneficiary
 *   and receipt seat (powerful/constrained) — collects unconditional standby
 *   capacity payments - energy_intensive_industries: beneficiary
 *   (organized/mobile) — buys supply security, holds jurisdictional exit -
 *   future_decision_makers: silent beneficiary (powerless/trapped,
 *   civilizational horizon) — inherits preserved options and deferred burdens
 *   - ratepayers: payer (moderate/constrained) — carries subsidy riders and
 *   capacity charges - climate_exposed_populations: payer (powerless/trapped,
 *   global scope) — bears continued emissions - plant_host_communities:
 *   dual-positioned payer/beneficiary (moderate/constrained) — bear exposure
 *   alongside plant-dependent livelihoods - rapid_decarbonization_advocates:
 *   excluded (organized/constrained) — closure prescriptions framed as
 *   extreme - integrated_assessment_analysts: analytical observer
 *   (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, 0.52).
domain_priors:suppression_score(acceptable_risk_energy__option_value_preserving, 0.5).
domain_priors:theater_ratio(acceptable_risk_energy__option_value_preserving, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, extractiveness, 0.52).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__option_value_preserving, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__option_value_preserving, "Multi-Pathway Option Preservation under Deep Uncertainty (Option-Value Reading of Acceptable Risk)").
narrative_ontology:topic_domain(acceptable_risk_energy__option_value_preserving, "energy_policy/risk_assessment/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__option_value_preserving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__option_value_preserving, '0ad3293b-fab7-4a64-8625-f5e6b9412c3f').
narrative_ontology:cs_kernel_codification('0ad3293b-fab7-4a64-8625-f5e6b9412c3f', distributed).
narrative_ontology:cs_authority_grounding('0ad3293b-fab7-4a64-8625-f5e6b9412c3f', distributed).
narrative_ontology:cs_reading_relation('0ad3293b-fab7-4a64-8625-f5e6b9412c3f', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('0ad3293b-fab7-4a64-8625-f5e6b9412c3f', acceptable_risk_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('0ad3293b-fab7-4a64-8625-f5e6b9412c3f', foundational, irreversibility_warrants_option_preservation).
narrative_ontology:cs_axiom_status(irreversibility_warrants_option_preservation, holdable).
narrative_ontology:cs_axiom_grounding('0ad3293b-fab7-4a64-8625-f5e6b9412c3f', irreversibility_warrants_option_preservation, instrumental).
narrative_ontology:cs_axiom('0ad3293b-fab7-4a64-8625-f5e6b9412c3f', foundational, no_single_pathway_is_robust_across_scenarios).
narrative_ontology:cs_axiom_status(no_single_pathway_is_robust_across_scenarios, holdable).
narrative_ontology:cs_axiom_grounding('0ad3293b-fab7-4a64-8625-f5e6b9412c3f', no_single_pathway_is_robust_across_scenarios, empirically_contingent).
narrative_ontology:cs_reference_frame('0ad3293b-fab7-4a64-8625-f5e6b9412c3f', multi_pathway_flexibility_baseline).
narrative_ontology:cs_drift_state('0ad3293b-fab7-4a64-8625-f5e6b9412c3f', contemporary_post_paris_post_fukushima, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0ad3293b-fab7-4a64-8625-f5e6b9412c3f', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, nuclear_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, fossil_generators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, energy_intensive_industries).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, future_decision_makers).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, ratepayers).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, climate_exposed_populations).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, plant_host_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, plant_host_communities).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, real_options_theory).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, deep_uncertainty_hedging_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the capacity markets, subsidy programs, and retirement-review processes through which pathway viability is administered. They set the rules determining which plants receive standing payments and which closure applications proceed. Their professional standing is bound to the framework they administer; leaving means leaving the institutions that define their careers.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, system_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Operate reactors whose capital was sunk decades ago and whose revenue now depends substantially on zero-emission credits and capacity payments justified by fuel diversity. Closing a plant means permanent decommissioning costs and the loss of a licensed, trained workforce; continuing means collecting support payments under rules they help shape through technical testimony.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, nuclear_operators, beneficiary,
    institutional, generational, constrained, continental).

% Receive capacity payments for keeping dispatchable units available, including units that would be uneconomic in an energy-only market. They fund advocacy for reliability standards that preserve their role and face asset stranding if the pathway they occupy is closed by regulation.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, fossil_generators, beneficiary,
    powerful, biographical, constrained, continental).

% Depend on stable, diversified electricity supply for continuous processes such as smelting, chemicals, and data processing. They support portfolio breadth because single-pathway systems expose them to fuel-specific price shocks, and they can relocate production across jurisdictions if local supply arrangements deteriorate.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, energy_intensive_industries, beneficiary,
    organized, biographical, mobile, global).

% Will inherit whichever infrastructure, supply chains, and skills survive today's decisions, along with the deferred waste, emissions, and debt. They hold the largest stake in the options being preserved and the burdens being deferred, yet exist at no table where the trade-off is decided and cannot decline the inheritance.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, future_decision_makers, beneficiary,
    powerless, civilizational, trapped, global).

% Pay capacity charges, subsidy riders, and stranded-cost recovery on utility bills regardless of usage choices. They cannot opt out of the cost socialization that keeps marginal units online, though they benefit incidentally from the reliability it buys.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, ratepayers, payer,
    moderate, biographical, constrained, national).

% Live downstream and downwind of the emissions that continue because dispatchable fossil capacity stays in the portfolio. Exposure accumulates over generations; relocation offers no exit from a global atmospheric commons.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, climate_exposed_populations, payer,
    powerless, generational, trapped, global).

% Host the operating plants and their effluents, waste shipments, and accident risk. Local employment, tax bases, and civic identity are tied to the facilities staying open, so continued operation brings both exposure and livelihood — a dual position few other seats hold.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, plant_host_communities, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__option_value_preserving, plant_host_communities, beneficiary).

% Organize for accelerated fossil retirement and rapid renewable buildout. They appear in regulatory proceedings and legislatures, but their core prescription — closing the fossil pathway on a fixed schedule — is procedurally framed as extreme within the portfolio doctrine, and their proposals are routinely narrowed to pilot scale.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, rapid_decarbonization_advocates, excluded,
    organized, generational, constrained, global).

% Publish scenario ensembles, mortality-per-TWh comparisons, and option-value decompositions. They hold no enforcement power; their models feed the deliberations of the other seats and occasionally supply the vocabulary each faction borrows.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, integrated_assessment_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__option_value_preserving, fossil_generators).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__option_value_preserving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the deep-uncertainty portfolio problem: no single energy pathway is robust across all plausible futures (fuel shocks, storage-cost surprises, climate-sensitivity revisions, breakthrough technologies), so maintaining multiple commercially viable pathways preserves the ability to shift as information arrives — a system-scale real option no individual actor can purchase privately.
% TRANSFER_FUNCTION: Moves ratepayer funds (via capacity payments, zero-emission credits, and R&D appropriations) to incumbent generators in each pathway; moves continued operating life — and its emissions, waste, and accident burdens — onto host communities and the atmospheric commons; preserves unexercised deployment options for future decision-makers.
% ABSENT_VOICES: Future decision-makers are wholly absent: they hold the largest stake in both the preserved options and the deferred burdens and have no seat anywhere the trade-off is decided. Rapid-decarbonization advocates are partially in the room but their closure schedules are procedurally framed as extreme; strict expected-value analysts stand outside the framing entirely, since the portfolio doctrine declines to rank pathways by a single metric.
% DISAPPEARANCE_RATIONALE: If the multi-pathway maintenance regime vanished overnight, capacity markets would unwind, marginal fossil and nuclear units would retire within years, and the specialized supply chains and workforces behind each pathway would begin irreversible atrophy. The energy system would rearrange around whichever pathways survived market selection alone, and the option to reverse course would be gone before anyone knew which future arrived.
% FOUNDING_PROBLEM: Mid-century energy crises — the oil shocks above all, and the early arc of nuclear promise and disillusionment — taught planners that committing an entire system to one pathway produces catastrophic lock-in when forecasts fail. The multi-pathway doctrine was built to prevent irreversible bets made under uncertainty.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the real-options economics tradition, integrated assessment scenario ensembles (which deliberately maintain multiple pathways), and the technology lock-in literature all attest the hedging problem remains unsolved. Signal in the other direction also comes from outside the beneficiary set: a strand of energy economics argues the option premium is systematically overstated and functions as incumbency cover — the dispute itself confirms the problem is contested on the merits, not dead.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__option_value_preserving, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__option_value_preserving, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__option_value_preserving, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__option_value_preserving, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__option_value_preserving, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__option_value_preserving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__option_value_preserving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon = 0.52 is authored for the standing multi-pathway arrangement as this reading itself prices it: the reading endorses the hedge yet concedes that a material share of what is spent on 'flexibility' exceeds any defensible option premium and that retained fossil operation continues uncompensated externalities. Suppression = 0.50 is a raw structural property, unscaled by power or scope: the regime must actively hold off both closure movements — climate-driven fossil retirement and cost-driven nuclear retirement — through procedural gatekeeping, matching the reading's specified moderate suppression of both extremes. Theater_ratio = 0.35: capacity procurement and workforce retention are functional, but a growing fraction of flexibility discourse operates as incumbency advocacy. Accessibility_collapse = 0.35: the sibling readings remain fully live and adoptable — several jurisdictions effectively run them — so understanding this constraint does not collapse its alternatives. Resistance = 0.60: both extremes contest the regime continuously. Claimed_type = tangled_rope is asserted from structure — genuine coordination function plus asymmetric extraction plus active enforcement — independently of the metric values; the engine computes per-seat types from the structural data, and any divergence between claim and computed type is the measurement the corpus exists to take. All three tracked series share one seven-point grid (1973-2025) so no metric row is backfilled or substituted.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the regulator seat the arrangement is prudent portfolio management it personally administers; from the ratepayer seat it is a bill line it cannot refuse; from the operator seats it is survival; from the climate-exposed seat it is a deferred invoice it never agreed to; from the future-decision-maker seat it is simultaneously the century's best gift and its largest unpaid liability. Same-level actors diverge too: fossil_generators and nuclear_operators hold comparable institutional standing with similarly constrained exits, yet fossil seats collect unconditional standby revenue while nuclear seats collect support contingent on political tolerance — different effective relationships despite equal nominal power. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (operators, generators, intensive industries, future decision-makers) derive low d; victims (ratepayers, climate-exposed populations, host communities) derive high d; the regulator sits near-symmetric as administrator. Exit modulation does real work: future_decision_makers are trapped beneficiaries — they cannot decline the inheritance, pinning their d near the beneficiary pole while leaving them powerless; climate_exposed_populations are trapped targets at global scope, which amplifies their effective chi. No directionality_overrides are authored: the role-plus-exit derivation captures every seat's relationship, and the two same-standing operator classes are already differentiated by their distinct declarations and payment structures. gain_flow names fossil_generators because standby capacity payments are the largest unconditional transfer in the regime and demonstrably accrue to that seat; nuclear support is conditional and politically contested, so no second receipt seat is named. fixing_cost = prohibitive: rebuilding an atrophied pathway — licensed workforce, supplier base, fuel-cycle competence — takes decades, so unwinding the regime forfeits options whose replacement cost dwarfs the annual savings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — avoiding irreversible single-pathway commitment after the oil shocks demonstrated forecast failure — remains live: storage-cost trajectories, climate sensitivity, fusion timing, and fuel geopolitics are all unresolved. Founding_problem_status live crossed with disappearance_verdict world_rearranges yields no zombie flag. The tangled_rope classification is what prevents mislabeling in both directions: a pure-snare reading would erase the genuine option value that independent real-options analysis corroborates; a pure-rope reading would erase the rent capture and externality continuation that the rising theater and extractiveness series document. Mandatrophy is not declared: the mandate has not outlived its function, though the uncertainty-resolution omega tracks the precise condition under which it would.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of kernel acceptable_risk_energy (reading: option_value_preserving). What structurally changes under each sibling reading?',
    'Comparative classification across the three linked reading stories: the same referent arrangement scored under each reading''s own lights, with victim sets and enforcement profiles compared side by side.',
    'Under catastrophic_tail_dominant the victim set collapses toward future climate victims and fossil suppression becomes severe; under expected_value_dominant nuclear closes on mortality accounting and the victim set shifts to nuclear host communities and displaced workers. This story''s epsilon, victims, and type hold only for the option-value weighting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel contestation: three readings of acceptable risk constitute three distinct constraints.').

omega_variable(
    option_value_vs_incumbency_rent,
    'What share of the regime''s cost purchases genuine decision flexibility, and what share is incumbency rent collected under flexibility language?',
    'Decomposition studies comparing capacity payments to independently modeled reliability value; revealed-preference tests asking whether private actors would repurchase the preserved option at the price paid.',
    'If rent dominates, epsilon rises toward snare territory and the regulator seat''s d shifts upward toward capture; if the option premium is real, the rope component strengthens and the current classification stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(option_value_vs_incumbency_rent, empirical, 'Whether measured maintenance cost splits into option premium versus incumbency rent.').

omega_variable(
    deep_uncertainty_resolution_horizon,
    'Will the uncertainties justifying the hedge — storage-cost trajectories, climate sensitivity, fusion timing, fuel geopolitics — resolve within the lifetime of the infrastructure being maintained?',
    'Monitor convergence of expert forecasts and realized cost curves against the 40-80 year asset lifetimes represented in the portfolio.',
    'If uncertainty resolves soon, the preserved options expire worthless, the coordination function dies, and the regime becomes a piton or snare candidate; if uncertainty persists across asset lifetimes, the hedge remains live and the founding problem stays open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deep_uncertainty_resolution_horizon, empirical, 'Lifetime of the uncertainty that grounds the option-preservation mandate.').

omega_variable(
    suppression_symmetry_question,
    'Is the regime''s suppression of the two closure movements symmetric, or does procedural gatekeeping bite harder in one direction?',
    'Compare approval friction, delay lengths, and success rates for fossil retirement filings versus nuclear retirement filings across jurisdictions.',
    'Asymmetric friction would reveal the direction of capture, shift the regulator seat''s effective d, and tilt the tangled_rope toward protecting whichever pathway the gatekeeping favors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_symmetry_question, empirical, 'Symmetry of enforcement against the two closure extremes.').

omega_variable(
    pathway_atrophy_irreversibility,
    'Is pathway viability genuinely recoverable once lost, or does exit permanently destroy the option through workforce, supplier, and licensing-knowledge decay?',
    'Historical reconstruction of atrophied supply chains — post-moratorium nuclear vendor decay, coal-mining skill loss — combined with re-entry cost studies.',
    'If atrophy is irreversible, premature-closure opportunity costs are large and the constraint''s protective function strengthens; if pathways can be rebuilt cheaply, the maintenance regime overpays for a cheap option and epsilon falls.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pathway_atrophy_irreversibility, empirical, 'Irreversibility of pathway exit, which calibrates the size of the option being preserved.').

omega_variable(
    cs_authority_framing,
    'Is the adjudicating authority over acceptable risk genuinely distributed expertise, or an incumbency coalition whose authority is grounded in what the regime''s stability lets its members collect?',
    'Trace whose technical submissions reliably prevail in retirement reviews and capacity rulemakings, and whether contrary expert consensus has ever displaced the portfolio baseline.',
    'Under the extraction-grounded framing, the commitment-system pattern shifts toward capture-consolidated authority and the regulator seat''s d rises; under distributed expertise, the current framing stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_authority_framing, conceptual, 'Framing under-determination in the authority structure over the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__option_value_preserving, 1973, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1973, acceptable_risk_energy__option_value_preserving, theater_ratio, 1973, 0.15).
narrative_ontology:measurement_basis(acce_tr_t1973, observed).
narrative_ontology:measurement(acce_tr_t1981, acceptable_risk_energy__option_value_preserving, theater_ratio, 1981, 0.18).
narrative_ontology:measurement_basis(acce_tr_t1981, observed).
narrative_ontology:measurement(acce_tr_t1991, acceptable_risk_energy__option_value_preserving, theater_ratio, 1991, 0.22).
narrative_ontology:measurement_basis(acce_tr_t1991, observed).
narrative_ontology:measurement(acce_tr_t2001, acceptable_risk_energy__option_value_preserving, theater_ratio, 2001, 0.26).
narrative_ontology:measurement_basis(acce_tr_t2001, observed).
narrative_ontology:measurement(acce_tr_t2011, acceptable_risk_energy__option_value_preserving, theater_ratio, 2011, 0.3).
narrative_ontology:measurement_basis(acce_tr_t2011, observed).
narrative_ontology:measurement(acce_tr_t2018, acceptable_risk_energy__option_value_preserving, theater_ratio, 2018, 0.33).
narrative_ontology:measurement_basis(acce_tr_t2018, observed).
narrative_ontology:measurement(acce_tr_t2025, acceptable_risk_energy__option_value_preserving, theater_ratio, 2025, 0.35).
narrative_ontology:measurement_basis(acce_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(acce_be_t1973, acceptable_risk_energy__option_value_preserving, base_extractiveness, 1973, 0.3).
narrative_ontology:measurement_basis(acce_be_t1973, observed).
narrative_ontology:measurement(acce_be_t1981, acceptable_risk_energy__option_value_preserving, base_extractiveness, 1981, 0.36).
narrative_ontology:measurement_basis(acce_be_t1981, observed).
narrative_ontology:measurement(acce_be_t1991, acceptable_risk_energy__option_value_preserving, base_extractiveness, 1991, 0.4).
narrative_ontology:measurement_basis(acce_be_t1991, observed).
narrative_ontology:measurement(acce_be_t2001, acceptable_risk_energy__option_value_preserving, base_extractiveness, 2001, 0.44).
narrative_ontology:measurement_basis(acce_be_t2001, observed).
narrative_ontology:measurement(acce_be_t2011, acceptable_risk_energy__option_value_preserving, base_extractiveness, 2011, 0.47).
narrative_ontology:measurement_basis(acce_be_t2011, observed).
narrative_ontology:measurement(acce_be_t2018, acceptable_risk_energy__option_value_preserving, base_extractiveness, 2018, 0.5).
narrative_ontology:measurement_basis(acce_be_t2018, observed).
narrative_ontology:measurement(acce_be_t2025, acceptable_risk_energy__option_value_preserving, base_extractiveness, 2025, 0.52).
narrative_ontology:measurement_basis(acce_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1973, acceptable_risk_energy__option_value_preserving, suppression_requirement, 1973, 0.38).
narrative_ontology:measurement_basis(acce_su_t1973, observed).
narrative_ontology:measurement(acce_su_t1981, acceptable_risk_energy__option_value_preserving, suppression_requirement, 1981, 0.42).
narrative_ontology:measurement_basis(acce_su_t1981, observed).
narrative_ontology:measurement(acce_su_t1991, acceptable_risk_energy__option_value_preserving, suppression_requirement, 1991, 0.45).
narrative_ontology:measurement_basis(acce_su_t1991, observed).
narrative_ontology:measurement(acce_su_t2001, acceptable_risk_energy__option_value_preserving, suppression_requirement, 2001, 0.46).
narrative_ontology:measurement_basis(acce_su_t2001, observed).
narrative_ontology:measurement(acce_su_t2011, acceptable_risk_energy__option_value_preserving, suppression_requirement, 2011, 0.48).
narrative_ontology:measurement_basis(acce_su_t2011, observed).
narrative_ontology:measurement(acce_su_t2018, acceptable_risk_energy__option_value_preserving, suppression_requirement, 2018, 0.49).
narrative_ontology:measurement_basis(acce_su_t2018, observed).
narrative_ontology:measurement(acce_su_t2025, acceptable_risk_energy__option_value_preserving, suppression_requirement, 2025, 0.5).
narrative_ontology:measurement_basis(acce_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__option_value_preserving, resource_allocation).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__expected_value_dominant).

% DUAL FORMULATION NOTE:
% 'Acceptable risk in energy policy' is a colloquial label covering three structurally distinct constraints: tail-weighted closure doctrine, expected-harm minimization doctrine, and option-preserving portfolio doctrine. They differ in epsilon (what each counts as a cost), in victim sets (future climate victims versus nuclear host communities versus opportunity-cost bearers), and in enforcement profiles. Per the epsilon-invariance principle the label is decomposed into three linked stories; this file is the option_value_preserving member and links to both siblings via affects_constraints. The three readings are parallel responses to the same oil-shock-era founding problem rather than a derivation chain, so the edges are informative rather than hierarchical: no member is upstream in the sense of evidential priority, though each reading's adoption changes the operating environment of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
