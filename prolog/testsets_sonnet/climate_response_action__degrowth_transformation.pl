% ============================================================================
% CONSTRAINT STORY: climate_response_action__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__degrowth_transformation, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: climate_response_action__degrowth_transformation
 *   human_readable: Degrowth Transformation Reading of Climate Response
 *   domain: economic/political/intergenerational
 *
 * SUMMARY:
 *   This story instantiates the degrowth_transformation reading of the
 *   climate_response_action kernel: the claim that adequate climate response
 *   requires structural rejection of GDP growth as an organizing principle,
 *   prioritizing sufficiency, equity, and throughput reduction over
 *   technological substitution. It is a distinct constraint from the
 *   mitigation_priority reading (which retains growth via decarbonized
 *   substitution and carbon markets) and the adaptation_priority reading
 *   (which accepts warming and prioritizes resilience investment). Each
 *   reading has a different beneficiary/victim structure and a different
 *   epsilon; they are linked here only through network.affects_constraints,
 *   not merged.
 *
 * KEY AGENTS:
 *   - global_south_development_claimants: primary beneficiary (powerless/trapped) — gains carbon-budget and development-space claim
 *   - fossil_fuel_capital_owners: primary target (powerful/mobile) — asset devaluation from throughput wind-down
 *   - high_consumption_wealthy_households: primary target (powerful/mobile) — consumption and wealth redistribution target
 *   - future_generations: structural beneficiary, non-agent — burden shifted toward present wealthy populations
 *   - mitigation_priority_coalition: excluded institutional actor — displaced framework, not part of this reading's deliberation
 *   - climate_economists_analytical: analytical observer — models throughput/decoupling evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, 0.42).
domain_priors:suppression_score(climate_response_action__degrowth_transformation, 0.55).
domain_priors:theater_ratio(climate_response_action__degrowth_transformation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, extractiveness, 0.42).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__degrowth_transformation, scaffold).
narrative_ontology:human_readable(climate_response_action__degrowth_transformation, "Degrowth Transformation Reading of Climate Response").
narrative_ontology:topic_domain(climate_response_action__degrowth_transformation, "economic/political/intergenerational").

narrative_ontology:has_sunset_clause(climate_response_action__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__degrowth_transformation, 'ab10a16b-993e-460a-8d93-63a5c152a0b3').
narrative_ontology:cs_kernel_codification('ab10a16b-993e-460a-8d93-63a5c152a0b3', distributed).
narrative_ontology:cs_authority_grounding('ab10a16b-993e-460a-8d93-63a5c152a0b3', distributed).
narrative_ontology:cs_reading_relation('ab10a16b-993e-460a-8d93-63a5c152a0b3', climate_response_action__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('ab10a16b-993e-460a-8d93-63a5c152a0b3', climate_response_action__adaptation_priority, influences).
narrative_ontology:cs_axiom('ab10a16b-993e-460a-8d93-63a5c152a0b3', foundational, growth_is_not_the_organizing_principle).
narrative_ontology:cs_axiom_status(growth_is_not_the_organizing_principle, holdable).
narrative_ontology:cs_axiom_grounding('ab10a16b-993e-460a-8d93-63a5c152a0b3', growth_is_not_the_organizing_principle, empirically_contingent).
narrative_ontology:cs_axiom('ab10a16b-993e-460a-8d93-63a5c152a0b3', foundational, present_wealthy_populations_bear_transition_burden).
narrative_ontology:cs_axiom_status(present_wealthy_populations_bear_transition_burden, holdable).
narrative_ontology:cs_axiom_grounding('ab10a16b-993e-460a-8d93-63a5c152a0b3', present_wealthy_populations_bear_transition_burden, deontological).
narrative_ontology:cs_reference_frame('ab10a16b-993e-460a-8d93-63a5c152a0b3', growth_oriented_industrial_economy).
narrative_ontology:cs_drift_state('ab10a16b-993e-460a-8d93-63a5c152a0b3', post_planetary_boundaries_evidence, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ab10a16b-993e-460a-8d93-63a5c152a0b3', '').
narrative_ontology:cs_kernel_id(climate_response_action__degrowth_transformation, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, global_south_development_claimants).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, low_income_households_global_north).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, care_and_informal_economy_workers).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, fossil_fuel_capital_owners).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, high_consumption_wealthy_households).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, growth_dependent_pension_funds).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, export_oriented_manufacturing_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, low_income_households_global_north).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, planetary_boundaries_framework).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, ecological_debt_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Populations in countries with low historical emissions but high exposure to climate harm, who under this reading gain a claim to remaining carbon budget and development space currently occupied by Global North consumption. They have no exit from the climate system itself; their leverage is moral and diplomatic, not economic.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, global_south_development_claimants, beneficiary,
    powerless, generational, trapped, global).

% Not yet born, cannot bargain, and inherit whatever throughput trajectory current populations choose. This reading explicitly shifts adjustment burden onto currently wealthy populations to reduce what future generations must absorb. Listed as non-agent: they cannot act to claim this benefit themselves.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_action__degrowth_transformation, future_generations).

% Stand to gain from universal basic services, working-time reduction, and sufficiency provisioning that decouples wellbeing from wage income and consumption growth. Some also lose informally where the growth economy currently supplies cheap goods and gig-work income; net effect depends on how redistribution is implemented.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, low_income_households_global_north, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, low_income_households_global_north, payer).

% Perform socially necessary labor (caregiving, subsistence, community maintenance) currently uncounted and unrewarded by GDP-organized economies. A sufficiency-and-equity framework would recognize and redistribute resources toward this labor; under the current growth paradigm they subsidize the system without being counted in it.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, care_and_informal_economy_workers, beneficiary,
    powerless, biographical, trapped, national).

% Hold assets whose value depends on continued extraction and throughput growth. A degrowth transformation directly devalues these holdings and forecloses the technological-substitution narrative that would let them relabel extraction as transition. They retain capital mobility to redeploy into other sectors or jurisdictions, but the specific asset class is targeted for wind-down.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, fossil_fuel_capital_owners, payer,
    powerful, biographical, mobile, global).

% Hold consumption patterns and accumulated wealth that this reading identifies as the primary throughput to be reduced. They face redistribution, consumption caps, or wealth taxation aimed explicitly at freeing ecological and development space for others. Mobile in the sense of being able to relocate assets, but the reading targets consumption regardless of jurisdiction.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, high_consumption_wealthy_households, payer,
    powerful, biographical, mobile, global).

% Institutional investors whose actuarial promises assume continued compound growth in asset values. A structural rejection of GDP growth as organizing principle threatens the return assumptions underlying pension solvency, creating a real transition cost for current and near-retirement beneficiaries who did not choose the paradigm shift.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, growth_dependent_pension_funds, payer,
    organized, generational, constrained, national).

% Employed in throughput-intensive export sectors (autos, consumer goods, shipping) whose output this reading treats as excess to be wound down rather than decarbonized in place via substitution. Their livelihoods depend on continued production volume; the reading offers working-time reduction and UBS as compensation but does not guarantee equivalent income in the transition window.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, export_oriented_manufacturing_workers, payer,
    moderate, biographical, trapped, national).

% Institutions committed to the technological-substitution and carbon-market reading of climate response — multilateral climate finance bodies, green-tech industry coalitions, carbon-market operators. Their preferred pathway is structurally displaced by this reading's rejection of substitution-without-throughput-reduction; they are not part of this reading's internal deliberation, only its target of persuasion or opposition.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, mitigation_priority_coalition, excluded,
    institutional, generational, analytical, global).

% Ecological economists and degrowth researchers who model throughput reduction pathways and evaluate feasibility. They document empirical claims (decoupling evidence, GDP-wellbeing correlation limits) that the reading relies on, without personally bearing the redistribution costs or receiving the redistributed benefits.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, climate_economists_analytical, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a deliberate, planned reduction in aggregate material and energy throughput across wealthy economies, paired with redistribution of remaining ecological and development space toward historically low-emitting populations — solving the problem that continued growth-oriented mitigation cannot fit within remaining carbon and material budgets without disproportionately foreclosing Global South development.
% TRANSFER_FUNCTION: Moves consumption capacity, capital returns, and throughput-linked income from high-consumption Global North households and fossil-linked capital owners toward Global South development claims, low-income households, care workers, and (structurally) future generations who would otherwise absorb un-mitigated climate damage.
% ABSENT_VOICES: The mitigation-priority coalition (carbon-market operators, green-tech capital, growth-committed multilateral institutions) is structurally excluded from shaping this reading's internal logic — they would object that the reading discards workable substitution pathways and underestimates political feasibility costs, but their preferred framework is exactly what this reading is built to reject.
% DISAPPEARANCE_RATIONALE: If this reading of climate response were abandoned overnight, redistribution claims from Global South development advocates would lose their organizing framework, pension funds and export-manufacturing constituencies would face no throughput-reduction pressure, and climate policy would default back toward the mitigation-priority (growth-preserving) or adaptation-priority (resilience-focused) readings — a materially different policy trajectory with different winners.
% FOUNDING_PROBLEM: Standard mitigation pathways relying on technological substitution and carbon markets appear structurally incapable of achieving required emissions reductions within remaining carbon budgets while also permitting continued Global North consumption growth and Global South development — the founding problem is this arithmetic incompatibility between growth-preserving mitigation and planetary boundaries.
% FOUNDING_PROBLEM_CORROBORATION: Ecological economists and IPCC working-group contributors outside the degrowth advocacy movement (e.g., authors of decoupling-literature reviews) corroborate that empirical decoupling of GDP growth from absolute resource throughput has not been demonstrated at required rates; mitigation-priority and mainstream development economists dispute the corroboration's policy implication, arguing decoupling is achievable at sufficient speed with different investment allocation — the founding-problem diagnosis is corroborated as an empirical concern from outside the degrowth camp, but its remedy (rejecting growth as organizing principle) is not.
narrative_ontology:disappearance_verdict(climate_response_action__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__degrowth_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__degrowth_transformation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_action__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__degrowth_transformation, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__degrowth_transformation_tests).
:- end_tests(climate_response_action__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42 at interval end) because the reading does impose real, targeted costs on specific wealthy and capital-owning populations, but it is coupled to a genuine and openly stated coordination function (fitting climate response within planetary boundaries while preserving Global South development space) rather than disguised rent-seeking. Suppression is moderate (0.55): the reading has no enforcement apparatus of its own — it is a policy platform, not a legal regime — but faces significant political resistance that would require considerable state capacity to implement against opposition. Resistance is authored high (0.78) reflecting the well-documented political feasibility barriers: growth-dependent institutions (pension funds, export sectors, mitigation-priority coalitions) actively oppose the framework. Accessibility collapse is moderate-low (0.35): alternative readings (mitigation_priority, adaptation_priority) remain fully live policy options, so this reading has not foreclosed its competitors.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South development claimants, future generations, low-income Global North households, and care/informal-economy workers are declared beneficiaries because the reading's explicit redistribution mechanism (UBS, working-time reduction, wealth transfer) directs resources and ecological space toward them — low d. Fossil fuel capital owners, high-consumption wealthy households, growth-dependent pension funds, and export-oriented manufacturing workers are declared victims because the reading's throughput-reduction mechanism directly devalues their assets, consumption patterns, or livelihoods — high d, tempered for capital owners by genuine asset mobility (exit_options: mobile) versus the trapped situation of manufacturing workers who cannot easily requalify.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification hinges on has_sunset_clause: this reading frames itself as a transitional restructuring toward a post-growth steady state, not a permanent extraction apparatus — the justification is explicitly the transition (decoupling from growth dependency) rather than indefinite redistribution for its own sake. This prevents the reading from being mislabeled as pure extraction (snare) directed at wealthy populations: the coordination function (fitting within planetary boundaries while preserving development rights) is genuine and named, and the beneficiary set includes populations (future generations, Global South) who cannot themselves capture rents, which is inconsistent with a pure extraction reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_empirical_status,
    'Can absolute decoupling of GDP growth from material/energy throughput be achieved at the rate and scale required to meet climate targets, making the mitigation_priority reading viable without the degrowth reading''s structural rejection of growth?',
    'Longitudinal empirical tracking of absolute decoupling rates in leading economies against required IPCC-consistent throughput reduction trajectories; resolvable in principle but contested in current literature.',
    'If sufficient decoupling is empirically demonstrated, the founding problem this reading claims to solve is substantially weakened, and the mitigation_priority reading''s growth-preserving premise becomes more defensible; if decoupling proves structurally impossible at required rates, this reading''s founding problem is strongly corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_empirical_status, empirical, 'Whether absolute decoupling is achievable, which determines whether growth-rejection is actually necessary.').

omega_variable(
    political_feasibility_vs_extraction_framing,
    'Is the high resistance this reading meets evidence that it threatens genuine entrenched extraction (fossil capital, throughput-dependent wealth), or evidence that the reform itself is poorly calibrated to political reality and would fail to deliver its promised redistribution even if adopted?',
    'Comparative case study of partial degrowth-adjacent policy implementations (e.g., work-time reduction pilots, wealth tax attempts) and whether redistribution promises were honored in practice versus captured or diluted during implementation.',
    'If implementation consistently dilutes redistribution while still imposing throughput-reduction costs on ordinary workers, the reading''s actual operation may drift toward tangled_rope (coordination cover for a different extraction pattern) rather than the scaffold this story claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_vs_extraction_framing, conceptual, 'Whether resistance reflects genuine threat to extraction or genuine implementation risk.').

omega_variable(
    intergenerational_representation_gap,
    'Since future_generations is listed as a non-agent beneficiary with no capacity to advocate for itself, does the reading''s redistribution toward them get captured by present-day institutional proxies (NGOs, states) whose interests diverge from actual future populations?',
    'Track whether institutions claiming to represent intergenerational equity (sovereign wealth funds, youth climate litigation bodies) actually direct resources toward throughput reduction versus toward their own institutional persistence.',
    'If proxy capture is significant, the declared beneficiary status of future_generations may be more nominal than structural, weakening the beneficiary-based justification for redistribution costs imposed on current payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_representation_gap, empirical, 'Whether non-agent future beneficiaries are adequately represented by present proxies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__degrowth_transformation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_action__degrowth_transformation, theater_ratio, 0, 0.12).
narrative_ontology:measurement(clim_tr_t8, climate_response_action__degrowth_transformation, theater_ratio, 8, 0.15).
narrative_ontology:measurement(clim_tr_t16, climate_response_action__degrowth_transformation, theater_ratio, 16, 0.19).
narrative_ontology:measurement(clim_tr_t24, climate_response_action__degrowth_transformation, theater_ratio, 24, 0.22).
narrative_ontology:measurement(clim_tr_t32, climate_response_action__degrowth_transformation, theater_ratio, 32, 0.25).
narrative_ontology:measurement(clim_tr_t40, climate_response_action__degrowth_transformation, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_action__degrowth_transformation, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(clim_be_t8, climate_response_action__degrowth_transformation, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(clim_be_t16, climate_response_action__degrowth_transformation, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(clim_be_t24, climate_response_action__degrowth_transformation, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(clim_be_t32, climate_response_action__degrowth_transformation, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(clim_be_t40, climate_response_action__degrowth_transformation, base_extractiveness, 40, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(climate_response_action__degrowth_transformation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__degrowth_transformation, resource_allocation).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the climate_response_action kernel (degrowth_transformation, mitigation_priority, adaptation_priority). Each reading has a distinct epsilon, distinct beneficiary/victim structure, and distinct claimed_type authored independently — they are not the same constraint measured differently. degrowth_transformation directly displaces the political legitimacy and resource allocation the mitigation_priority reading depends on (both compete for the same policy-adoption slot), and stands in tension with adaptation_priority's premise of accepting warming as given. Network edges here record that structural coupling, not equivalence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
