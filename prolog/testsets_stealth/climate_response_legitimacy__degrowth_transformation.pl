% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__degrowth_transformation, []).

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
 *   constraint_id: climate_response_legitimacy__degrowth_transformation
 *   human_readable: Growth Imperative in Wealthy-Nation Climate Response (Degrowth Transformation Reading)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   Wealthy nations organize climate policy around the maintenance of
 *   economic growth: decarbonization is pursued insofar as it can be
 *   reconciled with expanding output, and options requiring deliberate
 *   contraction are ruled out before costing. This story instantiates the
 *   degrowth_transformation reading of the climate_response_legitimacy
 *   kernel, which holds that a legitimate response requires dismantling the
 *   growth imperative through universal basic services, working-time
 *   reduction, and democratic firm ownership. Per the epsilon-referent rule
 *   for kernel readings, extractiveness is authored for the STANDING
 *   arrangement under contest — the growth-embedded climate-response order —
 *   as this reading assesses it; the reading's endorsed steady-state
 *   alternative is not the referent and is not classified here. Sibling
 *   readings (mitigation_priority, adaptation_priority) are separate
 *   constraints in separate files; the decomposition follows the
 *   epsilon-invariance principle because the readings assign different seat
 *   structures and different epsilon to the same contested question. KEY
 *   AGENTS (by structural relationship): - wealthy_nation_asset_owners:
 *   primary beneficiary (powerful/arbitrage) — collects growth returns,
 *   relocates capital - fossil_carbon_incumbents: concentrated beneficiary
 *   (powerful/arbitrage) — balance sheets assume continued throughput -
 *   current_developed_economy_households: dual-positioned beneficiary/payer
 *   (moderate/constrained) — consumes above sustainable share while locked
 *   into growth-priced obligations - future_generations: primary target
 *   (powerless/trapped) — inherits warming and depletion with no voice -
 *   global_south_climate_vulnerable: primary target (moderate/constrained) —
 *   bears impacts of others' emissions -
 *   finance_ministries_and_central_banks: agenda setter
 *   (institutional/constrained) — administers growth-calibrated machinery -
 *   degrowth_and_post_growth_movements: excluded voice (moderate/constrained)
 *   - climate_ethics_and_intergenerational_justice_scholars: analytical
 *   observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, 0.78).
domain_priors:suppression_score(climate_response_legitimacy__degrowth_transformation, 0.7).
domain_priors:theater_ratio(climate_response_legitimacy__degrowth_transformation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__degrowth_transformation, "Growth Imperative in Wealthy-Nation Climate Response (Degrowth Transformation Reading)").
narrative_ontology:topic_domain(climate_response_legitimacy__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__degrowth_transformation, '3020c7b2-5e75-4b26-bea7-9e0e17f1070d').
narrative_ontology:cs_kernel_codification('3020c7b2-5e75-4b26-bea7-9e0e17f1070d', distributed).
narrative_ontology:cs_authority_grounding('3020c7b2-5e75-4b26-bea7-9e0e17f1070d', expertise).
narrative_ontology:cs_interpretation_layer_present('3020c7b2-5e75-4b26-bea7-9e0e17f1070d').
narrative_ontology:cs_reading_relation('3020c7b2-5e75-4b26-bea7-9e0e17f1070d', climate_response_legitimacy__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('3020c7b2-5e75-4b26-bea7-9e0e17f1070d', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('3020c7b2-5e75-4b26-bea7-9e0e17f1070d', foundational, growth_dismantling_required_for_legitimacy).
narrative_ontology:cs_axiom_status(growth_dismantling_required_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('3020c7b2-5e75-4b26-bea7-9e0e17f1070d', growth_dismantling_required_for_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('3020c7b2-5e75-4b26-bea7-9e0e17f1070d', foundational, intergenerational_carbon_debt_obligation).
narrative_ontology:cs_axiom_status(intergenerational_carbon_debt_obligation, holdable).
narrative_ontology:cs_axiom_grounding('3020c7b2-5e75-4b26-bea7-9e0e17f1070d', intergenerational_carbon_debt_obligation, deontological).
narrative_ontology:cs_axiom('3020c7b2-5e75-4b26-bea7-9e0e17f1070d', secondary, technological_dependency_risk_imposition).
narrative_ontology:cs_axiom_status(technological_dependency_risk_imposition, holdable).
narrative_ontology:cs_axiom_grounding('3020c7b2-5e75-4b26-bea7-9e0e17f1070d', technological_dependency_risk_imposition, instrumental).
narrative_ontology:cs_reference_frame('3020c7b2-5e75-4b26-bea7-9e0e17f1070d', planetary_boundary_sufficiency_framework).
narrative_ontology:cs_drift_state('3020c7b2-5e75-4b26-bea7-9e0e17f1070d', contemporary_net_zero_pledge_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3020c7b2-5e75-4b26-bea7-9e0e17f1070d', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, wealthy_nation_asset_owners).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, current_developed_economy_households).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, fossil_carbon_incumbents).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, global_south_climate_vulnerable).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, current_developed_economy_households).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, absolute_decoupling_insufficiency_thesis).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, intergenerational_ecological_debt_doctrine).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__degrowth_transformation, common_but_differentiated_responsibility_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold portfolios whose returns depend on continued expansion of output and asset prices. Capital income scales with aggregate growth, and holdings can be relocated across jurisdictions and asset classes faster than any single government can alter the rules those returns depend on.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, wealthy_nation_asset_owners, beneficiary,
    powerful, biographical, arbitrage, global).

% Consume several times the global-average share of material and energy throughput and enjoy infrastructure, healthcare, and leisure funded by decades of expansion. The same households carry mortgages, pensions, and employment contracts priced on the assumption that incomes keep rising, so their practical ability to step outside growth-dependent arrangements is narrow even where they sympathize with change.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, current_developed_economy_households, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__degrowth_transformation, current_developed_economy_households, payer).

% Own reserves, pipelines, refineries, and combustion supply chains whose book value assumes decades of continued throughput. They fund political parties, shape media narratives, and can shift investment across borders; their balance sheets are the clearest single expression of the assumption that expansion continues.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, fossil_carbon_incumbents, beneficiary,
    powerful, generational, arbitrage, global).

% Will inhabit whatever climate, depleted soils, drained aquifers, and reduced biodiversity the present leaves behind. They cast no votes, hold no assets, and enter current decisions only through proxy advocates; nothing they could do changes the terms they inherit.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Populations in low-emitting regions — delta farmers, pastoralists, coastal urban poor — face floods, heat extremes, and crop failure driven overwhelmingly by emissions they did not produce. Negotiating forums give them procedural seats but little leverage over the consumption patterns that determine their exposure; migration is their main adaptive move and it is increasingly blocked.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, global_south_climate_vulnerable, payer,
    moderate, generational, constrained, regional).

% Operate fiscal and monetary machinery — deficit rules, inflation targets, pension regulation — calibrated to expanding tax bases and rising nominal incomes. Their mandates, forecasting models, and staff training all presume growth; administering a deliberate contraction lies outside their legal instructions and professional repertoire.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, finance_ministries_and_central_banks, agenda_setter,
    institutional, generational, constrained, national).

% Scholars, activists, and cooperative networks arguing for universal basic services, shorter working hours, and worker-owned firms. They publish, organize, and run local pilots but hold no ministry portfolios, command no major-party programs, and are routinely characterized as economically illiterate in mainstream policy venues.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, degrowth_and_post_growth_movements, excluded,
    moderate, generational, constrained, global).

% Analyze the distribution of climate burdens across cohorts and borders, formalize duties owed to people who do not yet exist, and audit whether official targets match stated commitments. They hold no enforcement power and depend on neither the growth dividend nor its dismantling.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, climate_ethics_and_intergenerational_justice_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__degrowth_transformation, wealthy_nation_asset_owners).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__degrowth_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Expands purchasing power, employment, and tax bases on a synchronized schedule: savings are mobilized because demand is expected to grow, pensions are financed by the next cohort's larger wages, and innovation is rewarded with bigger markets.
% TRANSFER_FUNCTION: Moves carbon space, material throughput, and sink stability from future generations and low-emitting regions to present wealthy-nation production and consumption; moves financial returns from aggregate expansion to asset holders.
% ABSENT_VOICES: Future generations are absent except through proxy advocates; global-south affected communities hold procedural seats but not agenda power; post-growth economists sit outside official appraisal frameworks. Unanimity in policy venues reflects who was admitted, not agreement among all affected.
% DISAPPEARANCE_RATIONALE: Pension systems financed by next-generation wages, employment tied to expanding demand, sovereign debt serviced from growing receipts, and asset prices capitalized on continued expansion would all destabilize simultaneously; provisioning would have to be rebuilt on non-growth foundations before the rearrangement settled.
% FOUNDING_PROBLEM: Post-war mass unemployment and reconstruction: growth was adopted as the mechanism for full employment, rising living standards, and social peace after depression and war.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: ecological economists and UN material-flow assessments attest that additional growth in wealthy nations no longer purchases proportional wellbeing gains, supporting a spent founding justification; treasury and OECD analyses attest that provisioning still structurally depends on growth, supporting liveness. The split among independent expert bodies is itself the corroboration of contested status.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__degrowth_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__degrowth_transformation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_legitimacy__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__degrowth_transformation, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78) because, on this reading's accounting, the standing arrangement transfers ecological capacity across cohorts and borders without compensation: wealthy nations have consumed carbon-budget shares far beyond any proportional allocation while the costs land on parties who cannot refuse. Suppression (0.70) is substantial but not total: post-growth options are not banned, they are institutionally foreclosed — fiscal rules, mandate structures, and project-appraisal frameworks all presuppose growth, so alternatives die in appraisal rather than in prohibition. Theater ratio (0.45) reflects the widening gap between performed decoupling (net-zero pledges, offset markets, efficiency rhetoric) and flat-to-rising aggregate material footprints in wealthy nations; roughly half of visible activity is assurance performance rather than throughput reduction. Accessibility collapse (0.50): post-growth alternatives demonstrably exist (cooperatives, basic-services pilots, working-time trials) but collapse to irrelevance inside official evaluation, persisting only at society's margins. Resistance (0.60): climate-justice movements, loss-and-damage diplomacy, and degrowth scholarship constitute real, growing resistance that has shifted discourse without yet shifting throughput. The three measurement series share one grid {0, 15, 30, 45, 60, 75}: extractiveness accumulates monotonically as carbon budgets are consumed; theater climbs as pledge-based governance replaces delivery; suppression_requirement hardens through the neoliberal consolidation (t=30–45), dips at t=60 when the post-2008 legitimacy fracture briefly widened tolerated dissent, then re-hardens. The claim (tangled_rope) and the metrics were authored independently: the claim records the structure I believe true, the metrics record the operation I believe observable.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the arrangement as the coordinating backbone of provisioning — pensions, employment, fiscal capacity — and computes low personal exposure; the arbitrage-grade beneficiary seats experience pure subsidy; the trapped target seats (future persons, climate-exposed regions) experience uncompensated burden with zero exit; the dual-positioned household seat computes ambivalence, receiving the growth dividend while living inside growth-priced obligations. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries with arbitrage exit (asset owners, incumbents) derive directionality near the full-beneficiary end. The dual-positioned household seat derives near-symmetric directionality from its offsetting declarations — genuine consumption benefit against genuine lock-in cost. Trapped, powerless victims (future generations) and constrained low-power victims (global south) derive directionality near the full-target end, amplified by the arrangement's global scope, which makes verification of transferred costs harder. The agenda-setter administers without collecting rents; its position reflects continuity interest rather than receipt. No directionality_overrides are authored: the declared beneficiary/victim structure plus exit atoms already yields the correct ordering, and adding overrides would merely duplicate the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two mislabels. Calling the growth arrangement a pure rope hides the transfer: it does coordinate pensions, employment, and innovation, but the same structure moves ecological capacity to parties who cannot consent, so coordination-only framing launders the asymmetry. Calling it a pure snare ignores that its coordination functions are real — removing growth overnight collapses pay-as-you-go pensions and employment before any replacement exists, which is precisely why this reading pairs dismantling with universal basic services, time redistribution, and ownership change rather than bare contraction. The R5 genealogy interview returns founding_problem_status=contested: the founding problem (mass unemployment, provisioning) is disputed rather than dead, so no zombie flag fires, but the mismatch consumer should watch the persistence-with-disputed-justification signature. The degrowth program itself, if enacted, would need its own story — its transitional machinery is a candidate scaffold with an implicit sunset once the steady state is reached.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolute_decoupling_sufficiency,
    'Can wealthy nations cut emissions and material throughput fast enough to meet carbon-budget arithmetic while GDP continues to grow?',
    'Compare historical absolute-decoupling rates (territorial and consumption-based emissions, material-flow accounts) against the reduction rates required by remaining carbon budgets; watch frontier economies for demonstrated sufficiency-rate decoupling.',
    'If decoupling at the required rate is demonstrated, this reading''s core empirical axiom weakens and mitigation_priority gains ground; if not, the standing arrangement''s high extractiveness stands confirmed and the dismantling requirement strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_decoupling_sufficiency, empirical, 'Whether growth-preserving decoupling can satisfy climate arithmetic — the empirical hinge between this reading and mitigation_priority.').

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one reading of the climate_response_legitimacy kernel; how would the sibling readings change the seat structure and classification?',
    'Author the sibling stories (mitigation_priority, adaptation_priority) and compare computed per-seat classifications across the family.',
    'Under mitigation_priority, current developed-economy generations remain beneficiaries via continued growth; under adaptation_priority, vulnerable populations become beneficiaries of resilience spending and emitters face weaker constraint. The classification in this file applies only to the degrowth instantiation; cross-reading comparison is the corpus-level measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: reading-relative seat sets within the climate_response_legitimacy kernel.').

omega_variable(
    transition_incidence_survivability,
    'If this reading were enacted, can the current developed-economy cohort absorb income reduction and structural change without collapsing the provisioning systems (pensions, health, employment) that the transition itself depends on?',
    'Natural experiments: universal-basic-services pilots, working-time reduction trials, and cooperative conversions tracked over a decade for provisioning stability and net emissions effect.',
    'Determines whether the reading''s counterfactual seat structure (current generation as cost-bearers, future generations as beneficiaries) can ever bind, or whether the political-feasibility barrier keeps the program aspirational; feeds implementation-risk assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transition_incidence_survivability, empirical, 'Whether the transformation''s incidence is politically and materially survivable for the paying cohort.').

omega_variable(
    proxy_representation_of_future_persons,
    'Future generations appear as cost-bearers only through proxy advocates; is proxy-authored representation an adequate basis for weighting their claims against present cost-bearers?',
    'No dataset settles it; resolution requires adopting a representational stance — constitutional future-generations bodies, ombudsperson institutions, or explicit discount-rate choices.',
    'If proxies are deemed inadequate, the victim weighting falls and the standing arrangement reads less extractive; if adequate, the current high extractiveness stands. The classification is stable but its moral weight is stance-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_representation_of_future_persons, preference, 'Representational adequacy for absent parties — a values question beneath the empirical victim declaration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__degrowth_transformation, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__degrowth_transformation, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t15, climate_response_legitimacy__degrowth_transformation, theater_ratio, 15, 0.16).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t30, climate_response_legitimacy__degrowth_transformation, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(clim_tr_t30, observed).
narrative_ontology:measurement(clim_tr_t45, climate_response_legitimacy__degrowth_transformation, theater_ratio, 45, 0.31).
narrative_ontology:measurement_basis(clim_tr_t45, observed).
narrative_ontology:measurement(clim_tr_t60, climate_response_legitimacy__degrowth_transformation, theater_ratio, 60, 0.38).
narrative_ontology:measurement_basis(clim_tr_t60, observed).
narrative_ontology:measurement(clim_tr_t75, climate_response_legitimacy__degrowth_transformation, theater_ratio, 75, 0.45).
narrative_ontology:measurement_basis(clim_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t15, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 15, 0.53).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t30, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(clim_be_t30, observed).
narrative_ontology:measurement(clim_be_t45, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 45, 0.68).
narrative_ontology:measurement_basis(clim_be_t45, observed).
narrative_ontology:measurement(clim_be_t60, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 60, 0.73).
narrative_ontology:measurement_basis(clim_be_t60, observed).
narrative_ontology:measurement(clim_be_t75, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 75, 0.78).
narrative_ontology:measurement_basis(clim_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t15, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t30, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 30, 0.56).
narrative_ontology:measurement_basis(clim_su_t30, observed).
narrative_ontology:measurement(clim_su_t45, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 45, 0.66).
narrative_ontology:measurement_basis(clim_su_t45, observed).
narrative_ontology:measurement(clim_su_t60, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 60, 0.63).
narrative_ontology:measurement_basis(clim_su_t60, observed).
narrative_ontology:measurement(clim_su_t75, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 75, 0.7).
narrative_ontology:measurement_basis(clim_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__degrowth_transformation, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__adaptation_priority).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'legitimate climate response' decomposes into three structurally distinct constraints — one per reading of the climate_response_legitimacy kernel. Each reading assigns a different seat structure and a different epsilon to the same contested question, so forcing them into one story would violate epsilon-invariance. This file (degrowth_transformation) is downstream of the decoupling-sufficiency dispute: the mitigation_priority reading's viability claim is exactly what this reading's foundational axiom denies, hence the forecloses edge. The adaptation_priority reading shares this reading's concern for the exposed but locates legitimacy in protection rather than transformation, hence coexists_with.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
