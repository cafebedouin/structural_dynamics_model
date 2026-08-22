% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__degrowth_reading, []).

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
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Degrowth Reading: Structural Transformation Imperative for Global North Climate Response
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the climate_response_imperative
 *   kernel: the degrowth_reading, which holds that climate response requires
 *   structural economic transformation of Global North economies — reduced
 *   material consumption, redistribution toward the Global South and future
 *   generations, and post-growth institutions — as the enabling condition for
 *   both mitigation and adaptation. The constraint's governed population is
 *   present-day Global North society; its beneficiary structure reaches
 *   across time (the unborn) and space (the South). Family membership: this
 *   is one of three linked readings of the same kernel; the sibling stories
 *   (mitigation_priority_reading, adaptation_priority_reading) instantiate
 *   different response regimes with different victim sets and different
 *   epsilon, and are separate files linked via network.affects_constraints.
 *   Epsilon's referent here is the standing arrangement under contest — the
 *   transformation imperative as it bears on present-day Northern
 *   populations, assessed by this reading's own lights — never the
 *   green-growth arrangement this reading rejects. KEY AGENTS (by structural
 *   relationship): - global_north_high_consumption_households: Primary target
 *   (organized/constrained) — bears consumption caps and working-time
 *   conversion, retains electoral recourse - carbon_intensive_sector_workers:
 *   Concentrated target (moderate/trapped) — bear transition costs with least
 *   mobility - growth_dependent_firms: Institutional target
 *   (institutional/arbitrage) — lose demand base, defer costs through capital
 *   mobility - future_generations: Principal beneficiary (powerless/trapped)
 *   — receive the stabilized climate; absent from every negotiating table -
 *   global_south_populations: Primary beneficiary (organized/constrained) —
 *   receive transfers and avoided damages -
 *   low_income_global_north_households: Dual-positioned
 *   (moderate/constrained) — net redistribution recipients with real
 *   energy-cost exposure - post_growth_governance_bodies: Agenda setter
 *   (institutional/constrained) — administer budgets, caps, and transfers -
 *   climate_economists_decoupling_debaters: Analytical observer
 *   (analytical/analytical) — adjudicate the feasibility evidence both camps
 *   cite
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.7).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.65).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Degrowth Reading: Structural Transformation Imperative for Global North Climate Response").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, 'a2ac50f7-587d-4aef-9019-9f0884b6ff46').
narrative_ontology:cs_kernel_codification('a2ac50f7-587d-4aef-9019-9f0884b6ff46', formalized).
narrative_ontology:cs_authority_grounding('a2ac50f7-587d-4aef-9019-9f0884b6ff46', expertise).
narrative_ontology:cs_interpretation_layer_present('a2ac50f7-587d-4aef-9019-9f0884b6ff46').
narrative_ontology:cs_reading_relation('a2ac50f7-587d-4aef-9019-9f0884b6ff46', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2ac50f7-587d-4aef-9019-9f0884b6ff46', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_axiom('a2ac50f7-587d-4aef-9019-9f0884b6ff46', foundational, northern_throughput_reduction_is_structurally_necessary).
narrative_ontology:cs_axiom_status(northern_throughput_reduction_is_structurally_necessary, holdable).
narrative_ontology:cs_axiom_grounding('a2ac50f7-587d-4aef-9019-9f0884b6ff46', northern_throughput_reduction_is_structurally_necessary, empirically_contingent).
narrative_ontology:cs_axiom('a2ac50f7-587d-4aef-9019-9f0884b6ff46', foundational, fair_shares_bind_present_consumption_across_generations_and_regions).
narrative_ontology:cs_axiom_status(fair_shares_bind_present_consumption_across_generations_and_regions, holdable).
narrative_ontology:cs_axiom_grounding('a2ac50f7-587d-4aef-9019-9f0884b6ff46', fair_shares_bind_present_consumption_across_generations_and_regions, deontological).
narrative_ontology:cs_reference_frame('a2ac50f7-587d-4aef-9019-9f0884b6ff46', fair_share_within_planetary_boundaries).
narrative_ontology:cs_drift_state('a2ac50f7-587d-4aef-9019-9f0884b6ff46', contemporary_post_sr15_implementation_gap, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('a2ac50f7-587d-4aef-9019-9f0884b6ff46', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, low_income_global_north_households).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, global_north_high_consumption_households).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, carbon_intensive_sector_workers).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, growth_dependent_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, low_income_global_north_households).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, planetary_boundaries_framework).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, fair_share_carbon_budget_accounting).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, absolute_decoupling_insufficiency_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and administer annual carbon budgets, consumption caps, and working-time standards, and operate the transfer mechanisms that move revenue from high-throughput sectors and households to redistribution recipients. Their authority rests on treaty mandates and scientific advisory bodies. Exit is limited: they are constituted by the arrangements they administer and cannot relocate their mandate.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, post_growth_governance_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Face capped carbon allowances, higher prices for flights, meat, and large homes, and reduced discretionary consumption; in exchange they gain shorter standard working weeks and expanded public services. They hold electoral weight in the jurisdictions that would enact the caps, and their practical exit — shifting consumption into untaxed domains or abroad — narrows as border adjustments extend coverage.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_high_consumption_households, payer,
    organized, biographical, constrained, global).

% Work in regions built around steel, cement, internal combustion, and fossil extraction; the transition eliminates or transforms their jobs faster than comparable alternatives appear locally. Retraining programs exist on paper but are geographically mismatched; selling a house in a declining industrial town is the practical form their lack of alternatives takes.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, carbon_intensive_sector_workers, payer,
    moderate, immediate, trapped, regional).

% Depend on expanding sales volumes; demand contraction and material caps shrink their addressable markets. They fund opposition research, litigation, and relocation of production to laxer jurisdictions, and their capital mobility lets them defer costs that trapped workers and households cannot defer.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, growth_dependent_firms, payer,
    institutional, biographical, arbitrage, global).

% Do not yet exist and therefore hold no seat in any legislature, market, or treaty negotiation; their stake — a stable climate system and intact material bases — enters decisions only through advocate proxies, guardianship clauses, and the arguments of the living. Nothing they could do changes the terms they inherit.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__degrowth_reading, future_generations, excluded).

% Receive technology transfer, adaptation finance, and a larger share of the remaining atmospheric commons under fair-share accounting; they also face the sharpest unadapted damages if stabilization fails. Migration offers partial escape from local impacts but not from the global system within which their claims are made.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_populations, beneficiary,
    organized, generational, constrained, global).

% Gain from rebated carbon revenue, expanded public transport, retrofitted housing, and shorter working weeks; they also absorb energy-price pass-through before rebates arrive and live in the least energy-efficient building stock. Their monthly budgets feel both sides of the transfer within the same fiscal year.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, low_income_global_north_households, beneficiary,
    moderate, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__degrowth_reading, low_income_global_north_households, payer).

% Run the models and audits the dispute turns on — decoupling rates, required transformation speeds, welfare accounting of leisure and health offsets. They publish in the journals both camps cite and hold no enforcement role; their exit is disciplinary reputation, not geography.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, climate_economists_decoupling_debaters, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__degrowth_reading, global_south_populations).
narrative_ontology:fixing_cost_class(climate_response_imperative__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of stabilizing the climate system within fair-share carbon and material budgets when supply-side efficiency and technology policy alone cannot reduce throughput fast enough — coordinating demand reduction across Global North populations so remaining budgets are respected and adaptation capacity is funded.
% TRANSFER_FUNCTION: Moves consumption capacity, working time, and wealth from present-day Global North populations (proportionally more from high-consuming households and growth-dependent firms) toward future generations, Global South populations, and low-income Northern households, via carbon rationing, redistribution, and working-time conversion.
% ABSENT_VOICES: Future generations hold the largest stake and no seat anywhere — they are represented only by proxies. Within the present, households whose infrastructure and routines bind them to high-carbon living (periurban commuters, aviation-dependent regions) voice objection mainly as electoral backlash after rules are set rather than as seated interests during design; and Southern negotiators hold fewer seats than the fair-share arithmetic of their populations' stakes implies.
% DISAPPEARANCE_RATIONALE: If the imperative vanished overnight, climate governance would reorganize around whichever sibling reading commanded the remaining coalition: technology-led mitigation would reclaim the budget headroom the transformation was to free, adaptation finance would be rescaled to residual-damage triage, and the redistribution architecture — the transfer streams to the South and the guardianship claims of the unborn — would lose its funding basis within a fiscal cycle.
% FOUNDING_PROBLEM: Northern material throughput exceeds fair shares of planetary boundaries, and efficiency gains have historically been outrun by volume growth, so stabilization appeared unreachable without deliberate demand-side transformation rather than technology policy alone.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: IPCC physical-science assessments document carbon-budget overshoot under current policy; the IEA — an energy-security body with no sympathy for degrowth framing — projects policy shortfalls of the same magnitude; material-flow accounts show absolute global throughput still rising. No source outside the beneficiary set attests that the problem is solved; the dispute among the readings concerns the remedy, not the overshoot.
narrative_ontology:disappearance_verdict(climate_response_imperative__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__degrowth_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.70: the burden on the governed population is real and large (capped consumption, converted working time, transferred wealth), but the reading's own accounting claims partial repayment through leisure, health co-benefits, and expanded public services, which holds it below pure-extraction levels. Suppression (0.65) is authored as a raw structural property — the binding instruments (allowances, caps, border adjustments) the arrangement requires to hold against exit and evasion — and is deliberately unscaled; only extractiveness is scaled by the engine through directionality and scope. Theater (0.32) reflects a discourse stage heavy on summits, net-zero pledges, and symbolic austerity relative to delivered instruments. Accessibility_collapse (0.60): accepting the reading's diagnosis collapses the green-growth alternative considerably but not completely — large-scale carbon removal and geoengineering remain conceivable outs. Resistance (0.78) is among the highest plausible for a policy construct: growth coalitions, consumer electorates, and industrial regions all contest it, and the gilets jaunes episode shows the backlash is electorally potent. The measurement series share one grid (t = 0, 5, 10, 15, 20, 25, 30, spanning roughly 1995–2025 of the discourse era) so every tracked metric is authored at every examined point. Suppression_requirement is tracked because escalation-with-delay is this story's central dynamic: every year of postponed transformation shrinks the residual budget and raises the intensity of the instruments the imperative must eventually deploy — the series models an enforcement ratchet driven by procrastination, not by expanding bureaucratic appetite. Rising theater alongside rising required enforcement is the signature of a constraint whose advocacy outpaces its machinery.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is a budget-administration problem with technical parameters; from the trapped worker seat it is a localized, uncompensated loss arriving faster than any replacement; from the organized household seat it is a price signal answered at the ballot box; from the beneficiary seats it is a claim finally being honored after centuries of externalization. One structure, four different lived arrangements. The engine computes per-seat types from power, exit, and directionality; nothing in the authored claim adjudicates between these experiences, and the divergence between the payer seats' computed classification and the beneficiary seats' is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: future_generations and global_south_populations derive near-full-beneficiary positions, with their trapped/constrained exit reinforcing reception rather than exposure; low_income_global_north_households derive beneficiary-side values tempered by their secondary payer position (the derivation reads their primary declaration; the tempering is documented narratively rather than forced through an override). Victim declarations drive high directionality: trapped workers sit nearest the full-target end because no exit amplifies effective extraction; organized households sit slightly below them because electoral recourse damps; growth_dependent_firms sit lowest among payers because arbitrage-grade capital mobility dampens effective extraction even though their formal exposure is high. Universal and global scopes amplify verification difficulty and thus effective extraction on the target side. No directionality_overrides are authored: the derivation from declared roles, power atoms, and exit options reproduces the structural relationships without correction, and the available override granularity (per power atom, not per agent) is too coarse to improve on it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards in both directions. Against snare-mislabeling: the arrangement has real, identifiable payers, but no seat captures the gains — the extracted consumption capacity flows to parties absent from the transaction (the unborn) or outside the taxing jurisdiction (the South), and the coordination function (budget stabilization) is genuine and primary, not cover. Against mountain-mislabeling: the necessity rhetoric ('physics demands it') tempts natural-law framing, but the arrangement is constructed, actively enforced, and heavily resisted — emerges_naturally is false and the resistance metric records the difference between a law and a program. The mandatrophy risk lives in the founding problem: if absolute decoupling or scalable carbon removal were achieved, the imperative's mandate would die while its institutions (budget authorities, rationing bureaucracies, transfer mechanisms) persisted theatrically — the omegas route that contingency, and the theater series rising ahead of any mandate death is the early signature to watch. The R5 interview records the founding problem as live and externally corroborated, so no zombie flag is asserted today; the contingency is carried structurally instead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_incidence_delta,
    'This constraint is one reading of the climate_response_imperative kernel; how would the sibling readings redistribute the victim and beneficiary sets over the same physical problem?',
    'Comparative classification of the sibling stories (climate_response_imperative__mitigation_priority_reading, climate_response_imperative__adaptation_priority_reading) against shared structural data on budgets, damages, and transfer flows.',
    'Under mitigation_priority, present-day Northern populations pay through carbon prices while retaining growth-era consumption norms, and future generations receive a smaller guaranteed benefit; under adaptation_priority, exposed Southern populations become payers of residual damage rather than beneficiaries of transfer. The disagreement is located in the sufficiency-of-technology premise and in cost incidence across time and space; resolving it redistributes every seat in this story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_incidence_delta, conceptual, 'Committer structure: one reading of a three-reading kernel; victim and beneficiary sets are reading-indexed, not topic-indexed.').

omega_variable(
    absolute_decoupling_feasibility,
    'Can GDP be absolutely decoupled from emissions and material throughput at the rates fair-share budgets require?',
    'Material-flow accounting and sectoral modeling compared against required decoupling rates; natural experiments from jurisdictions achieving sustained absolute reductions.',
    'If decoupling at required rates is feasible, the necessity premise weakens and the imperative becomes one policy option among several, drifting the computed classification toward lighter coordination; if infeasible, the imperative hardens toward biophysical necessity and the resistance it meets becomes costlier to sustain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_decoupling_feasibility, empirical, 'The pivotal empirical premise: whether demand-side transformation is necessary or merely preferable.').

omega_variable(
    cdr_scalability,
    'Can carbon dioxide removal scale to the gigatonne levels rival readings rely on, at tolerable cost, land, and energy side effects?',
    'Deployment trajectories of direct air capture, bioenergy with capture, and nature-based removal audited against modeled requirement curves and independent cost engineering.',
    'If removal scales, part of the transformation burden shifts off present-day Northern consumption and this reading''s elimination-of-CDR premise fails; if it does not, delayed action locks in the escalated-burden trajectory the measurement series records.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_scalability, empirical, 'Whether the reading''s rejection of reliance on unproven carbon removal is vindicated by deployment reality.').

omega_variable(
    democratic_persistence_under_present_day_costs,
    'Can an arrangement whose heaviest costs fall on the present-day electorate that holds agenda power over it persist democratically?',
    'Comparative study of consumption-reduction policies that survived elections versus those reversed (fuel-tax reversals, revenue-recycling successes), and of constitutional guardianship designs that insulate long-horizon budgets from electoral cycles.',
    'If persistence fails, the arrangement remains advocacy-stage — high theater, thin enforcement — and computed classifications skew toward performative maintenance until enforcement institutions exist; if it persists, active enforcement is real and the hybrid coordination-plus-incidence structure holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_persistence_under_present_day_costs, conceptual, 'Persistence question: whether the payer majority''s agenda power dissolves the arrangement or the design survives contact with elections.').

omega_variable(
    compensation_offset_magnitude,
    'How much of the measured burden on present-day Northern populations is offset by the reading''s own compensation channels — reduced working time, health co-benefits, reduced status competition, expanded public services?',
    'Welfare-accounting studies valuing leisure, health, and equality gains against consumption losses under post-growth scenarios, disaggregated by income decile.',
    'Large offsets lower effective extraction below the authored epsilon and soften payer-seat divergence; negligible offsets confirm the payer seats experience the arrangement as pure sacrifice and push computed per-seat types toward harder categories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compensation_offset_magnitude, empirical, 'Self-critical check on the reading''s own compensation claim, which is what holds epsilon below pure-extraction levels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cri_degrowth_tr_t0, climate_response_imperative__degrowth_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cri_degrowth_tr_t5, climate_response_imperative__degrowth_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement(cri_degrowth_tr_t10, climate_response_imperative__degrowth_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(cri_degrowth_tr_t15, climate_response_imperative__degrowth_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement(cri_degrowth_tr_t20, climate_response_imperative__degrowth_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(cri_degrowth_tr_t25, climate_response_imperative__degrowth_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(cri_degrowth_tr_t30, climate_response_imperative__degrowth_reading, theater_ratio, 30, 0.32).

% Extraction over time
narrative_ontology:measurement(cri_degrowth_be_t0, climate_response_imperative__degrowth_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(cri_degrowth_be_t5, climate_response_imperative__degrowth_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(cri_degrowth_be_t10, climate_response_imperative__degrowth_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(cri_degrowth_be_t15, climate_response_imperative__degrowth_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(cri_degrowth_be_t20, climate_response_imperative__degrowth_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(cri_degrowth_be_t25, climate_response_imperative__degrowth_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(cri_degrowth_be_t30, climate_response_imperative__degrowth_reading, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(cri_degrowth_su_t0, climate_response_imperative__degrowth_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(cri_degrowth_su_t5, climate_response_imperative__degrowth_reading, suppression_requirement, 5, 0.47).
narrative_ontology:measurement(cri_degrowth_su_t10, climate_response_imperative__degrowth_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(cri_degrowth_su_t15, climate_response_imperative__degrowth_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement(cri_degrowth_su_t20, climate_response_imperative__degrowth_reading, suppression_requirement, 20, 0.59).
narrative_ontology:measurement(cri_degrowth_su_t25, climate_response_imperative__degrowth_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(cri_degrowth_su_t30, climate_response_imperative__degrowth_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__adaptation_priority_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition: the colloquial label 'climate response' conflates three structurally distinct response regimes instantiated as separate stories of one kernel. They differ in who governs, who pays, and who benefits — hence different epsilon over the same physical problem, per the epsilon-invariance principle. The degrowth reading is downstream of the physical-science consensus (carbon-budget accounting) but contests the sufficiency premises of the mitigation-priority reading and the triage premises of the adaptation-priority reading; edges run from this story to both siblings, and the sibling files carry reciprocal edges and their own deltas.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
