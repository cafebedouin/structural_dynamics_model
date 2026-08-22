% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__degrowth_reading, []).

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
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Growth-Imperative Political Economy as Assessed by the Degrowth Reading of Climate Harm Prevention
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the degrowth reading of the
 *   climate_harm_prevention kernel. The standing arrangement under contest is
 *   the growth-imperative political economy of the Global North: the ensemble
 *   of fiscal, monetary, trade, and cultural commitments that make GDP
 *   expansion the non-negotiable objective of economic governance. The
 *   reading's claim is that mitigation sufficient to prevent catastrophic
 *   harm is physically and politically impossible inside that framework, and
 *   that legitimate climate response therefore requires planned contraction
 *   of Northern throughput - freeing atmospheric space for Southern
 *   development and reducing the harm transferred to future generations. Per
 *   the epsilon-referent rule, extractiveness is authored for the standing
 *   arrangement as this reading assesses it, never for the contraction regime
 *   the reading endorses. The claimed type (tangled_rope) is my independent
 *   structural judgment of the standing arrangement from this seat; the
 *   metrics describe its operation as the reading sees it; the engine
 *   computes per-seat classifications from the structural data, and
 *   divergence between claim and computed type is the measurement the corpus
 *   exists to take. Sibling readings are separate files, not described here.
 *
 * KEY AGENTS:
 *   - growth_dependent_welfare_states: agenda-setter (institutional/arbitrage) - administers the growth commitments; identity fused with growth delivery
 *   - northern_asset_owners: primary beneficiary (institutional/arbitrage) - captures the arrangement's surplus; freest exit in the system
 *   - northern_households: beneficiary with payer exposure (organized/constrained) - consumption subsidized by the arrangement, would bear contraction's front-line costs
 *   - global_south_populations: primary target (powerless/trapped) - bears impacts and development-space foreclosure
 *   - future_generations: primary target (powerless/trapped, civilizational horizon) - inherits the depleted budget with no seat and no exit
 *   - global_south_negotiating_bloc: organized payer voice (organized/constrained) - formal standing, limited leverage
 *   - degrowth_ecological_economics_movement: excluded critic (moderate/constrained) - supplies the reading but sits outside agenda-setting venues
 *   - ipcc_climate_science_community: analytical observer (analytical/analytical) - attests the physical premises all readings argue over
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.84).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.68).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Growth-Imperative Political Economy as Assessed by the Degrowth Reading of Climate Harm Prevention").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, 'f46b3ef9-6e75-4ced-aeeb-669ce6c463b1').
narrative_ontology:cs_kernel_codification('f46b3ef9-6e75-4ced-aeeb-669ce6c463b1', formalized).
narrative_ontology:cs_authority_grounding('f46b3ef9-6e75-4ced-aeeb-669ce6c463b1', expertise).
narrative_ontology:cs_interpretation_layer_present('f46b3ef9-6e75-4ced-aeeb-669ce6c463b1').
narrative_ontology:cs_reading_relation('f46b3ef9-6e75-4ced-aeeb-669ce6c463b1', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('f46b3ef9-6e75-4ced-aeeb-669ce6c463b1', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('f46b3ef9-6e75-4ced-aeeb-669ce6c463b1', foundational, growth_compatible_mitigation_impossible).
narrative_ontology:cs_axiom_status(growth_compatible_mitigation_impossible, holdable).
narrative_ontology:cs_axiom_grounding('f46b3ef9-6e75-4ced-aeeb-669ce6c463b1', growth_compatible_mitigation_impossible, empirically_contingent).
narrative_ontology:cs_axiom('f46b3ef9-6e75-4ced-aeeb-669ce6c463b1', foundational, disproportionate_consumers_bear_contraction_duty).
narrative_ontology:cs_axiom_status(disproportionate_consumers_bear_contraction_duty, holdable).
narrative_ontology:cs_axiom_grounding('f46b3ef9-6e75-4ced-aeeb-669ce6c463b1', disproportionate_consumers_bear_contraction_duty, deontological).
narrative_ontology:cs_reference_frame('f46b3ef9-6e75-4ced-aeeb-669ce6c463b1', planetary_boundary_steady_state).
narrative_ontology:cs_drift_state('f46b3ef9-6e75-4ced-aeeb-669ce6c463b1', contemporary_overshoot, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f46b3ef9-6e75-4ced-aeeb-669ce6c463b1', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, northern_asset_owners).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, growth_dependent_welfare_states).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, northern_households).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_south_populations).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, northern_households).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_south_negotiating_bloc).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central banks, treasuries, and finance ministries set inflation targets, fiscal rules, and recovery programs that all presuppose expansion; electoral legitimacy is staked on delivering growth, and pension and debt architectures are indexed to it. They can reposition policy mixes and portfolios faster than the societies they manage, but their institutional self-conception has fused with the growth-delivery function.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, growth_dependent_welfare_states, agenda_setter,
    institutional, generational, arbitrage, national).

% Pension funds, sovereign wealth vehicles, and private capital hold claims on growth's returns; asset appreciation and capital income concentrate the surplus the arrangement generates. They rebalance geographically and sectorally as conditions shift, giving them the freest exit of any seat in the system.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, northern_asset_owners, beneficiary,
    institutional, generational, arbitrage, global).

% Employment, mortgages, and retirement security are structured around continuing expansion, and present consumption levels are subsidized by cheap throughput. Under the reading's remedy they would bear contraction's front-line costs in income, asset values, and employment; no individual household can exit growth-dependence on its own.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, northern_households, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__degrowth_reading, northern_households, payer).

% Bear drought, flood, heat mortality, and displacement driven overwhelmingly by Northern cumulative emissions, while the shrinking residual carbon budget forecloses the development space the North already used. Migration is the principal exit and it narrows as destination polities close.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_populations, payer,
    powerless, biographical, trapped, regional).

% Hold no seat in any present forum yet inherit the depleted budget, the destabilized systems, and the infrastructure lock-in decided now. Exit is undefined for them: no action available to them changes the inheritance, which is precisely what makes them the arrangement's least contestable payers.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Coordinates in UNFCCC processes to press equity, loss-and-damage, and atmospheric headroom claims. Organized voice with formal standing, but limited leverage against Northern domestic growth commitments; exiting the treaty track would forfeit the only venue where the transfer is even negotiable.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_negotiating_bloc, payer,
    organized, generational, constrained, continental).

% Scholars and movements proposing caps, planned contraction, and redistribution publish and organize, but sit outside central-bank models, treasury forecasting frameworks, and mainstream party platforms. Their exclusion from the venues where Northern trajectories are actually set is part of how the arrangement maintains itself.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, degrowth_ecological_economics_movement, excluded,
    moderate, generational, constrained, continental).

% Assesses carbon budgets, overshoot, and residual headroom; supplies the physical premises over which all three readings of the kernel argue. Collects nothing from the arrangement and pays nothing under it.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, ipcc_climate_science_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__degrowth_reading, northern_asset_owners).
narrative_ontology:fixing_cost_class(climate_harm_prevention__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The growth framework coordinates savings, investment, employment, and fiscal capacity around expanding output; it manages distributional conflict by growing the pie rather than renegotiating shares; and it provides the macroeconomic foundation on which welfare-state financing and debt service operate.
% TRANSFER_FUNCTION: Moves atmospheric sink capacity and ecological headroom from the Global South and future generations to present Northern consumption; moves the returns to ecological drawdown upward to asset owners; and moves political legitimacy to governments that deliver growth.
% ABSENT_VOICES: Future generations have no seat anywhere. Global South populations hold formal seats in treaty processes but not in the domestic fiscal and monetary arenas where Northern trajectories are actually fixed. Degrowth and ecological-economics voices are outside central-bank models and treasury forecasts. Unanimity in growth-commitment venues arises partly because these seats were never in the room.
% DISAPPEARANCE_RATIONALE: If the growth-commitment machinery vanished overnight, debt service, pension indexation, employment policy, and trade regimes would all break simultaneously, producing a fiscal-legitimacy vacuum that some successor arrangement would immediately fill; climate trajectories would also diverge sharply depending on whether contraction or another growth form replaced it. Every named seat's situation depends on the arrangement.
% FOUNDING_PROBLEM: Postwar mass unemployment and distributional conflict: how to secure full employment and rising living standards without renegotiating property relations, solved by committing the state to managed expansion.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and labor-market statisticians attest the original unemployment and distribution problem the arrangement was built for; the IPCC assessment cycle and carbon-cycle literature attest the new binding constraint it now collides with; ecological economics and degrowth scholarship document the mismatch from outside the benefiting parties. No source disputes that the original problem was real; the contest is over whether it remains the operative one.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__degrowth_reading, 0.84, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.84 at interval end) because the arrangement consumes a shared atmospheric sink for concentrated benefit while transferring destabilization costs to non-consenting parties; the series rises monotonically because each year of growth-first policy shrinks the residual budget, mechanically intensifying what the arrangement takes. Suppression (0.68) is a raw structural property, unscaled by power or scope: growth-dependence is enforced structurally (debt service, pension liabilities, employment guarantees tied to expansion) with a smaller internalized component (growth as common sense), and alternatives remain visible but institutionally foreclosed. Theater ratio (0.52) tracks the growing share of climate activity that is performative - net-zero pledges, offset markets, green-growth projections - relative to delivered absolute reductions. Accessibility collapse (0.48) is moderate: understanding growth-dependence closes some exits (there is no return to a pre-growth equilibrium) but post-growth designs remain articulable. Resistance (0.58) reflects climate-justice mobilization, Southern bloc insistence on equity, and degrowth scholarship. The three measurement series share one time grid (1992-2024, seven points) so every metric is authored at every examined point; the dynamics are monotonic drift, not cyclical, so no oscillation mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter/beneficiary seats should compute differently. From the Southern and future-generation positions the arrangement operates as extraction with no compensating coordination benefit they receive - likely computing snare-flavored. From inside the treasury and the asset base, the same debt/pension/employment architecture reads as indispensable stability - likely computing rope-flavored. The institutional identity-lock matters here: growth_dependent_welfare_states exhibit institutional fusion (the state has 'become' its growth-delivery function), so the agenda-setter seat cannot easily conceive its own exit; if that frame broke, fiscal redesign becomes thinkable and the seat's computed position would shift. The engine derives this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: asset owners collect the surplus directly; welfare states collect legitimacy and fiscal capacity; Northern households collect the consumption subsidy. Victim declarations map to the transfer: Southern populations and future generations bear the harms and the foreclosure. Trapped exit and powerless power push the victim seats toward the full-target end of directionality; arbitrage exit pushes asset owners toward the beneficiary end; Northern households sit nearer symmetric than asset owners because wage dependence exposes them, but their consumption subsidy keeps them beneficiary-side. Scope amplification applies because the arrangement operates over a universal-scale commons (the atmosphere), where verification failure is structural. No directionality overrides are used: the derivation chain distinguishes the two institutional seats through their roles (agenda_setter vs beneficiary) and exit profiles, so an override keyed only to the institutional power atom would blur rather than sharpen the picture.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope call preserves both truths the reading insists on: the growth framework genuinely coordinates (its overnight disappearance rearranges everything - Q5 verdict world_rearranges), and its extraction is now incompatible with the kernel's purpose. Reading the arrangement as pure snare would recommend simple demolition and ignore the coordination function millions depend on; reading it as rope would license green-growth continuation and ignore the asymmetry the physics forbids. The R5 interview locates the mandatrophy question precisely: the founding problem (employment and distribution under scarcity) is contested - partially live, but the framework now generates a problem it cannot solve. The status-contested x world_rearranges combination flags zombie-risk without asserting it: the arrangement persists because fixing it is prohibitively costly for those positioned to fix it, while its gains accrue to a named seat - the receipt surface records capture, which is what keeps this a maintained tangled rope rather than an inertial piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the climate_harm_prevention kernel; which structural facts would change if a sibling reading (mitigation_priority or adaptation_priority) were instantiated instead?',
    'Track which reading''s beneficiary/victim structure gets codified into treaty text, fiscal rules, and central-bank or treasury mandates; the instantiated reading is the one whose structure the enforcement machinery actually serves.',
    'Under mitigation_priority the growth boundary survives and extraction relocates onto stranded fossil assets with a narrower victim set; under adaptation_priority the harm transfer to the Global South and future generations is accepted as given and the payer set widens without compensation; under this reading the growth boundary itself is the extraction object and contraction is the remedy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story is the degrowth member of a three-reading kernel family.').

omega_variable(
    absolute_decoupling_feasibility,
    'Is absolute decoupling of GDP from emissions and material throughput at the rates the remaining carbon budget requires physically and historically achievable?',
    'Consumption-based emission accounting, material flow analysis, and comparison of observed national decoupling rates against the required rates published in carbon-budget assessments.',
    'If decoupling at required speed is feasible, the mitigation_priority reading regains ground and this reading''s impossibility premise weakens, pulling the arrangement back toward ordinary rope-like coordination; if infeasible, the degrowth premise hardens and the extraction attributed to the growth framework is irreducible within it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_decoupling_feasibility, empirical, 'The empirical hinge on which the reading''s foundational impossibility claim turns.').

omega_variable(
    contraction_political_stabilizability,
    'Can planned economic contraction be democratically stabilized, or does any attempt collapse into unplanned depression, authoritarian rationing, or political reversal?',
    'Comparative study of deliberate downsizing episodes (post-growth stabilization attempts, wartime rationing, managed stagnation), plus participatory modeling of contraction pathways with distributional safeguards.',
    'If stabilizable, the reading''s remedy constitutes implementable coordination and the demanded constraint carries scaffold-like transitional structure; if not, the remedy is aspirational and the standing arrangement persists by default regardless of its assessed extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contraction_political_stabilizability, empirical, 'Whether the reading''s endorsed alternative is administrable at all.').

omega_variable(
    northern_household_net_position,
    'Are Northern households net beneficiaries of the standing growth arrangement, or simultaneous co-victims via wage stagnation, precarity, and localized climate harm?',
    'Distributional national accounts splitting growth''s returns between capital and labor, combined with household-level exposure mapping to climate impacts and growth-instability shocks.',
    'If households are substantially co-victims, their directionality sits nearer symmetric than the beneficiary declaration suggests, and contraction burden-sharing within the North becomes a progressive-versus-regressive design question rather than a North-versus-rest transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(northern_household_net_position, empirical, 'Ambiguity in the internal beneficiary structure of the Global North.').

omega_variable(
    growth_naturalness_status,
    'Is growth-dependence a constructed policy commitment reversible by institutional design, or an emergent quasi-natural property of complex monetary economies?',
    'Historical analysis of how growth regimes were deliberately constructed (postwar settlements, debt structures, central-bank mandates) and comparative political economy of societies that stabilized without expansion.',
    'If quasi-natural, the arrangement approaches mountain character and contraction demands misread the structure they target; if constructed, it is unwindable policy and the tangled_rope assessment with active enforcement holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_naturalness_status, conceptual, 'Natural-law versus constructed-arrangement ambiguity in the growth imperative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_harm_prevention__degrowth_reading, theater_ratio, 1992, 0.22).
narrative_ontology:measurement(clim_tr_t1997, climate_harm_prevention__degrowth_reading, theater_ratio, 1997, 0.26).
narrative_ontology:measurement(clim_tr_t2003, climate_harm_prevention__degrowth_reading, theater_ratio, 2003, 0.3).
narrative_ontology:measurement(clim_tr_t2009, climate_harm_prevention__degrowth_reading, theater_ratio, 2009, 0.35).
narrative_ontology:measurement(clim_tr_t2015, climate_harm_prevention__degrowth_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__degrowth_reading, theater_ratio, 2020, 0.46).
narrative_ontology:measurement(clim_tr_t2024, climate_harm_prevention__degrowth_reading, theater_ratio, 2024, 0.52).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_harm_prevention__degrowth_reading, base_extractiveness, 1992, 0.6).
narrative_ontology:measurement(clim_be_t1997, climate_harm_prevention__degrowth_reading, base_extractiveness, 1997, 0.63).
narrative_ontology:measurement(clim_be_t2003, climate_harm_prevention__degrowth_reading, base_extractiveness, 2003, 0.67).
narrative_ontology:measurement(clim_be_t2009, climate_harm_prevention__degrowth_reading, base_extractiveness, 2009, 0.71).
narrative_ontology:measurement(clim_be_t2015, climate_harm_prevention__degrowth_reading, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__degrowth_reading, base_extractiveness, 2020, 0.8).
narrative_ontology:measurement(clim_be_t2024, climate_harm_prevention__degrowth_reading, base_extractiveness, 2024, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_harm_prevention__degrowth_reading, suppression_requirement, 1992, 0.5).
narrative_ontology:measurement(clim_su_t1997, climate_harm_prevention__degrowth_reading, suppression_requirement, 1997, 0.54).
narrative_ontology:measurement(clim_su_t2003, climate_harm_prevention__degrowth_reading, suppression_requirement, 2003, 0.57).
narrative_ontology:measurement(clim_su_t2009, climate_harm_prevention__degrowth_reading, suppression_requirement, 2009, 0.6).
narrative_ontology:measurement(clim_su_t2015, climate_harm_prevention__degrowth_reading, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__degrowth_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(clim_su_t2024, climate_harm_prevention__degrowth_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__adaptation_priority).

% DUAL FORMULATION NOTE:
% The colloquial label 'climate policy' conflates three structurally distinct readings of one kernel (climate_harm_prevention). This file instantiates the degrowth reading: epsilon is authored for the standing growth-imperative arrangement as this reading assesses it (high - the framework appropriates the atmospheric commons for concentrated Northern benefit while foreclosing the contraction the physics requires). Sibling stories instantiate mitigation_priority (epsilon authored for the fossil-transition shortfall inside a preserved growth boundary) and adaptation_priority (epsilon authored for the resilience deficit under an accepted warming trajectory). Different victim sets, different epsilon, different types - linked here as one constraint family; the upstream physical premises (carbon-budget finitude) are cited by all three, which is why the upstream story influences the downstream readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
