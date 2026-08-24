% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Degrowth Structural Transformation Imperative
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This constraint story instantiates the degrowth reading of the contested
 *   kernel 'climate_response_imperative'. It asserts that staying within
 *   1.5-2°C requires not just technological decarbonization but structural
 *   reduction of energy and material throughput in the Global North, with
 *   redistribution to meet Global South needs and future generations' rights.
 *   The constraint is claimed as a tangled rope: it performs a genuine
 *   coordination function (solving the carbon budget allocation problem) but
 *   extracts asymmetrically from present Global North populations who did not
 *   design the growth paradigm but bear transition costs. The sibling
 *   readings — mitigation_priority (green growth/techno-optimism) and
 *   adaptation_priority (resilience-first) — offer different structural
 *   diagnoses and victim/beneficiary maps.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.68).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.45).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Degrowth Structural Transformation Imperative").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, 'f2187cbf-75d2-4c9e-aa84-0e39b661cf2a').
narrative_ontology:cs_kernel_codification('f2187cbf-75d2-4c9e-aa84-0e39b661cf2a', distributed).
narrative_ontology:cs_authority_grounding('f2187cbf-75d2-4c9e-aa84-0e39b661cf2a', distributed).
narrative_ontology:cs_reading_relation('f2187cbf-75d2-4c9e-aa84-0e39b661cf2a', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('f2187cbf-75d2-4c9e-aa84-0e39b661cf2a', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_axiom('f2187cbf-75d2-4c9e-aa84-0e39b661cf2a', foundational, structural_transformation_necessary_for_1p5c).
narrative_ontology:cs_axiom_status(structural_transformation_necessary_for_1p5c, holdable).
narrative_ontology:cs_axiom_grounding('f2187cbf-75d2-4c9e-aa84-0e39b661cf2a', structural_transformation_necessary_for_1p5c, deontological).
narrative_ontology:cs_axiom('f2187cbf-75d2-4c9e-aa84-0e39b661cf2a', foundational, cdr_reliance_violates_precautionary_principle).
narrative_ontology:cs_axiom_status(cdr_reliance_violates_precautionary_principle, holdable).
narrative_ontology:cs_axiom_grounding('f2187cbf-75d2-4c9e-aa84-0e39b661cf2a', cdr_reliance_violates_precautionary_principle, empirically_contingent).
narrative_ontology:cs_axiom('f2187cbf-75d2-4c9e-aa84-0e39b661cf2a', foundational, global_north_historical_responsibility_requires_consumption_reduction).
narrative_ontology:cs_axiom_status(global_north_historical_responsibility_requires_consumption_reduction, holdable).
narrative_ontology:cs_axiom_grounding('f2187cbf-75d2-4c9e-aa84-0e39b661cf2a', global_north_historical_responsibility_requires_consumption_reduction, deontological).
narrative_ontology:cs_reference_frame('f2187cbf-75d2-4c9e-aa84-0e39b661cf2a', post_paris_agreement_carbon_budget_allocation).
narrative_ontology:cs_drift_state('f2187cbf-75d2-4c9e-aa84-0e39b661cf2a', post_ipcc_ar6_wgiii_2022, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f2187cbf-75d2-4c9e-aa84-0e39b661cf2a', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_south_populations).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, global_north_working_populations).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, global_north_middle_income_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_north_middle_income_households).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, planetary_boundaries_framework).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, climate_justice_principle).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, post_growth_economics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face reduced consumption, shorter working hours, and potential job displacement in carbon-intensive sectors. Organized through unions and social movements but constrained by national labor markets and lack of viable post-growth employment alternatives. Exit means migration or accepting precarity.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_working_populations, payer,
    organized, biographical, constrained, continental).

% Bear consumption reduction and lifestyle changes (less travel, smaller housing, reduced meat/dairy). Also benefit from improved public services, reduced work time, and climate stability. Exit options limited by mortgage debt, career specialization, and social expectations.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_middle_income_households, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__degrowth_reading, global_north_middle_income_households, beneficiary).

% Control investment flows, media narratives, and policy access. Currently set the agenda against degrowth through green growth framing. Could capture transition rents via carbon markets, green tech monopolies, and adaptation finance. Exit via capital mobility and jurisdictional arbitrage.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_elites_and_corporations, agenda_setter,
    institutional, generational, arbitrage, global).

% Inherit the climatic consequences of present choices. Cannot organize, exit, or advocate directly. Their interests are represented only through proxy claims by present actors. The constraint's coordination function is explicitly justified by their benefit.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_imperative__degrowth_reading, future_generations).

% Bear disproportionate climate impacts despite minimal historical emissions. Benefit from Global North consumption reduction freeing carbon budget, technology transfer, and climate finance. Constrained by debt, trade terms, and limited political voice in global governance. Exit blocked by border regimes and economic dependency.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_populations, beneficiary,
    moderate, generational, constrained, continental).

% Produce the empirical basis for the constraint (carbon budgets, tipping points, mitigation pathways). Do not bear costs or collect rents from the policy response. Their authority is epistemic, not institutional.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, climate_science_and_policy_observers, observer,
    analytical, civilizational, analytical, universal).

% Demand climate reparations, technology transfer, and policy space for development. Structurally excluded from Global North domestic policy decisions that determine the carbon budget. Their objection to green growth framing is marginalized in UNFCCC negotiations.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_governments_and_movements, excluded,
    organized, generational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a globally sufficient mitigation pathway that stays within the remaining carbon budget while enabling adaptation finance and technology transfer to the Global South, solving the collective action problem of fair burden-sharing across generations and regions.
% TRANSFER_FUNCTION: Moves consumption entitlements and carbon budget from present Global North populations (especially middle and working classes) to future generations and Global South populations, mediated by redistributive institutions (carbon dividends, universal basic services, shortened work week, public investment).
% ABSENT_VOICES: Future generations are structurally absent and represented only by proxy. Global South governments and movements are present in international forums but excluded from the domestic policy decisions in Global North countries that determine the actual consumption reduction trajectory. Global North working-class communities dependent on carbon-intensive industries are often consulted only after policy design, not during.
% DISAPPEARANCE_RATIONALE: If the degrowth imperative vanished overnight, Global North emissions would continue on green growth trajectories relying on unproven CDR, carbon budgets would be exceeded, Global South would face uncompensated loss and damage, and the coordination problem of fair burden-sharing would remain unsolved — the world would rearrange toward catastrophic warming and deepened North-South injustice.
% FOUNDING_PROBLEM: The founding problem is the dual crisis of ecological overshoot (exceeding planetary boundaries, particularly climate) and global inequality, which the post-WWII growth paradigm cannot resolve because it requires perpetual expansion on a finite planet while the Global South's development claims remain unmet.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGIII identifies demand-side mitigation and sufficiency as necessary for 1.5°C pathways. Climate justice movements (e.g., Climate Justice Alliance, Fridays for Future MAPA) attest the problem is live and the growth paradigm is the cause. Mainstream economists and Global North governments attest the problem is solvable via green growth (contested). Post-growth scholars (Hickel, Kallis, Jackson) corroborate from outside the beneficiary set of the current arrangement.
narrative_ontology:disappearance_verdict(climate_response_imperative__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__degrowth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__degrowth_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the material sacrifice required of Global North populations: consumption reduction, working-time reduction, sectoral job loss. Suppression (0.45) is moderate — the constraint is not yet enforced at scale; suppression would rise if degrowth policies were implemented (carbon rationing, bans on advertising, maximum income). Theater ratio (0.25) is low because the coordination function (fair carbon budget allocation) is central, not performative — though green growth rhetoric performs a theater function for the status quo. Accessibility collapse (0.55): alternatives (CDR, geoengineering, green growth) exist but are increasingly shown as insufficient or risky. Resistance (0.72) is high: political, corporate, and cultural resistance from Global North elites, aspirational classes, and carbon-intensive communities.
 *
 * PERSPECTIVAL GAP:
 *   From the Global North payer seats, the constraint feels like extraction (lost consumption, job insecurity). From the Global South beneficiary seat, it feels like overdue justice (carbon space, finance). From the future generations seat (proxy), it feels like survival. From the agenda_setter seat, it feels like a threat to be managed or captured. The engine computes these per-seat types from the structural data; the claimed tangled_rope captures the coordination-extraction hybrid at the system level.
 *
 * DIRECTIONALITY LOGIC:
 *   Global North working and middle-income populations are payers (d ~ 0.7-0.8): they bear consumption/work reduction with constrained exit. Global North elites are agenda_setters with arbitrage exit (d ~ 0.1-0.2): they shape policy and can capture transition rents. Future generations are beneficiaries but powerless and trapped (d ~ 0.0 structurally, but cannot collect). Global South populations are beneficiaries with constrained exit (d ~ 0.3): they gain carbon space and finance but remain in dependent position. The directionality derivation from beneficiary/victim declarations + power + exit produces this gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The growth paradigm's mandate (lifting all boats via rising GDP) is dead for Global North (secular stagnation, inequality) but live for Global South (development needs). The degrowth reading argues the mandate has atrophied into a piton for Global North elites while remaining a snare for Global South. The constraint's mandatrophy is unresolved: post-growth institutions are not yet built, and the transition could be captured (green growth) or deepened (degrowth).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_vs_extraction,
    'Is the degrowth transformation a genuine coordination necessity (carbon budget physics) or an extractive policy choice masquerading as necessity?',
    'Empirical test: if mitigation_priority pathways (techno-optimistic CDR, efficiency) fail to deliver 1.5°C in next decade, coordination necessity of degrowth increases. If they succeed, degrowth extraction component dominates.',
    'If coordination necessity, the constraint trends toward rope (coordination function dominates). If extraction dominates, it trends toward snare (asymmetric burden without reciprocal coordination gain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_vs_extraction, empirical, 'Whether the structural transformation is physically necessary or politically chosen.').

omega_variable(
    cdr_feasibility_and_justice,
    'Are carbon dioxide removal technologies (BECCS, DACCS) feasible at scale without land/food/water conflicts that would harm Global South?',
    'Monitoring of CDR deployment pilots, land-use modeling, and Global South civil society resistance to CDR projects.',
    'If CDR is infeasible/unjust, degrowth reading''s claim that mitigation_priority relies on ''unproven CDR'' is vindicated — the coordination function of degrowth strengthens. If CDR proves viable and just, mitigation_priority reading gains coordination credibility.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cdr_feasibility_and_justice, empirical, 'The empirical status of the technological alternative that the degrowth reading rejects.').

omega_variable(
    distributional_justice_within_global_north,
    'Can the consumption reduction be distributed progressively (burden on high emitters) rather than regressively (austerity on working class)?',
    'Policy design analysis: carbon dividends, frequent flyer levies, luxury carbon taxes, maximum income policies vs. flat carbon taxes, regressive consumption taxes.',
    'If regressive, the constraint is a snare for working class. If progressive, the tangled_rope coordination function holds with fairer extraction distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_justice_within_global_north, conceptual, 'Whether the extraction within Global North can be structured justly.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''climate_response_imperative'' frame the problem as a single optimization (emissions reduction) or as a plural justice problem (mitigation + adaptation + distribution + historical responsibility)?',
    'Analysis of UNFCCC texts, IPCC report framing, and climate justice movement demands: whether the kernel admits multiple incommensurable framings or forces a single metric.',
    'If single optimization, mitigation_priority_reading forecloses others. If plural justice, all three readings coexist as partial truths requiring synthesis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel itself structures the contest or the contest reveals kernel underdetermination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 2015, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2015, climate_response_imperative__degrowth_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(clim_tr_t2020, climate_response_imperative__degrowth_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(clim_tr_t2025, climate_response_imperative__degrowth_reading, theater_ratio, 2025, 0.2).
narrative_ontology:measurement(clim_tr_t2030, climate_response_imperative__degrowth_reading, theater_ratio, 2030, 0.22).
narrative_ontology:measurement(clim_tr_t2035, climate_response_imperative__degrowth_reading, theater_ratio, 2035, 0.24).
narrative_ontology:measurement(clim_tr_t2040, climate_response_imperative__degrowth_reading, theater_ratio, 2040, 0.25).
narrative_ontology:measurement(clim_tr_t2045, climate_response_imperative__degrowth_reading, theater_ratio, 2045, 0.25).
narrative_ontology:measurement(clim_tr_t2050, climate_response_imperative__degrowth_reading, theater_ratio, 2050, 0.25).

% Extraction over time
narrative_ontology:measurement(clim_be_t2015, climate_response_imperative__degrowth_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(clim_be_t2020, climate_response_imperative__degrowth_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(clim_be_t2025, climate_response_imperative__degrowth_reading, base_extractiveness, 2025, 0.52).
narrative_ontology:measurement(clim_be_t2030, climate_response_imperative__degrowth_reading, base_extractiveness, 2030, 0.6).
narrative_ontology:measurement(clim_be_t2035, climate_response_imperative__degrowth_reading, base_extractiveness, 2035, 0.65).
narrative_ontology:measurement(clim_be_t2040, climate_response_imperative__degrowth_reading, base_extractiveness, 2040, 0.68).
narrative_ontology:measurement(clim_be_t2045, climate_response_imperative__degrowth_reading, base_extractiveness, 2045, 0.68).
narrative_ontology:measurement(clim_be_t2050, climate_response_imperative__degrowth_reading, base_extractiveness, 2050, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2015, climate_response_imperative__degrowth_reading, suppression_requirement, 2015, 0.2).
narrative_ontology:measurement(clim_su_t2020, climate_response_imperative__degrowth_reading, suppression_requirement, 2020, 0.3).
narrative_ontology:measurement(clim_su_t2025, climate_response_imperative__degrowth_reading, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement(clim_su_t2030, climate_response_imperative__degrowth_reading, suppression_requirement, 2030, 0.42).
narrative_ontology:measurement(clim_su_t2035, climate_response_imperative__degrowth_reading, suppression_requirement, 2035, 0.45).
narrative_ontology:measurement(clim_su_t2040, climate_response_imperative__degrowth_reading, suppression_requirement, 2040, 0.45).
narrative_ontology:measurement(clim_su_t2045, climate_response_imperative__degrowth_reading, suppression_requirement, 2045, 0.45).
narrative_ontology:measurement(clim_su_t2050, climate_response_imperative__degrowth_reading, suppression_requirement, 2050, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__degrowth_reading, 0.18).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__adaptation_priority_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the kernel 'climate_response_imperative' into three readings with distinct ε values and victim/beneficiary structures. The degrowth_reading has highest extractiveness (0.68) because it assigns transition costs to present Global North populations. The mitigation_priority_reading has lower extractiveness (~0.35) by relying on future CDR and innovation. The adaptation_priority_reading has moderate extractiveness (~0.50) concentrated on Global South. They are linked by shared carbon budget physics but diverge on burden-sharing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__degrowth_reading, organized, 0.75).
constraint_indexing:directionality_override(climate_response_imperative__degrowth_reading, moderate, 0.65).
constraint_indexing:directionality_override(climate_response_imperative__degrowth_reading, institutional, 0.15).
constraint_indexing:directionality_override(climate_response_imperative__degrowth_reading, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
