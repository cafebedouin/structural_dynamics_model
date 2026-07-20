% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__qualitative_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__qualitative_development_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: performance_legitimacy__qualitative_development_reading
 *   human_readable: High-Quality Development Legitimacy Constraint (Qualitative Development Reading)
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint instantiates the qualitative_development_reading of the
 *   performance_legitimacy kernel: the claim that state legitimacy rests on
 *   structural transformation toward innovation, sustainability, and
 *   efficiency rather than raw GDP growth. It is a contested reading within a
 *   kernel that also permits quantitative, techno-nationalist, and
 *   livelihood-security framings. The constraint operates as a developmental
 *   imperative enforced through industrial policy, cadre evaluation reform,
 *   credit guidance, and environmental regulation, redirecting resources from
 *   traditional manufacturing and property-finance local governments toward
 *   high-tech sectors and state-backed innovation institutions.
 *
 * KEY AGENTS:
 *   - central_planners (agenda_setter/institutional): reshapes national development priorities and enforces the paradigm shift
 *   - high_tech_sectors (beneficiary/organized): receives prioritized credit and policy support
 *   - state_innovation_ecosystem (beneficiary/institutional): intermediates capital and policy toward strategic industries
 *   - traditional_manufacturing (payer/organized): bears compliance costs and credit rationing
 *   - property_dependent_local_governments (payer/institutional/trapped): loses fiscal autonomy as land-finance model is dismantled
 *   - traditional_sector_workers (excluded/powerless): displaced by restructuring without voice in design
 *   - academic_economists (observer/moderate): debate the net efficiency of the transformation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.62).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.58).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "High-Quality Development Legitimacy Constraint (Qualitative Development Reading)").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, '5bd46c6b-06fb-4db5-aa0f-256543f09b65').
narrative_ontology:cs_kernel_codification('5bd46c6b-06fb-4db5-aa0f-256543f09b65', implicit).
narrative_ontology:cs_authority_grounding('5bd46c6b-06fb-4db5-aa0f-256543f09b65', extraction).
narrative_ontology:cs_interpretation_layer_present('5bd46c6b-06fb-4db5-aa0f-256543f09b65').
narrative_ontology:cs_reading_relation('5bd46c6b-06fb-4db5-aa0f-256543f09b65', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('5bd46c6b-06fb-4db5-aa0f-256543f09b65', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5bd46c6b-06fb-4db5-aa0f-256543f09b65', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('5bd46c6b-06fb-4db5-aa0f-256543f09b65', foundational, innovation_priority_over_speed).
narrative_ontology:cs_axiom_status(innovation_priority_over_speed, holdable).
narrative_ontology:cs_axiom_grounding('5bd46c6b-06fb-4db5-aa0f-256543f09b65', innovation_priority_over_speed, empirically_contingent).
narrative_ontology:cs_axiom('5bd46c6b-06fb-4db5-aa0f-256543f09b65', foundational, state_led_upgrade_imperative).
narrative_ontology:cs_axiom_status(state_led_upgrade_imperative, holdable).
narrative_ontology:cs_axiom_grounding('5bd46c6b-06fb-4db5-aa0f-256543f09b65', state_led_upgrade_imperative, instrumental).
narrative_ontology:cs_reference_frame('5bd46c6b-06fb-4db5-aa0f-256543f09b65', innovation_driven_development_state).
narrative_ontology:cs_drift_state('5bd46c6b-06fb-4db5-aa0f-256543f09b65', post_property_crackdown_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5bd46c6b-06fb-4db5-aa0f-256543f09b65', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_innovation_ecosystem).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturing).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the national development agenda, reshaping cadre evaluation criteria away from raw GDP growth toward innovation, environmental compliance, and debt sustainability. They enforce the transition through industrial policy, credit guidance, and regulatory overhaul. They cannot easily revert to pure growth rhetoric without losing credibility on the 'new era' development paradigm.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, central_planners, agenda_setter,
    institutional, generational, constrained, national).

% Receive priority credit allocation, subsidies, tax incentives, and state procurement under the innovation-driven development strategy. Their growth is championed as proof of the 'high-quality' model. They depend on continued policy favor and state-backed venture capital to maintain competitive position against global rivals.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, high_tech_sectors, beneficiary,
    organized, biographical, constrained, national).

% Comprises state venture capital guidance funds, national laboratories, and technology transfer institutions that intermediate capital and policy support toward strategic emerging industries. Their institutional mandate and budgets expand under the qualitative development paradigm.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_innovation_ecosystem, beneficiary,
    institutional, generational, constrained, national).

% Faces tightened environmental enforcement, credit rationing, and industrial upgrading mandates that raise compliance costs and force elimination of backward capacity. Many firms lose access to bank lending or are pressured into consolidation. Exit to the new model requires capital and technology they lack.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_manufacturing, payer,
    organized, biographical, constrained, national).

% Historically relied on land sales and property development for fiscal revenue and infrastructure financing. Under the high-quality development and 'common prosperity' frameworks, land-finance models are restricted, property speculation curbed, and local debt placed under central scrutiny. Their fiscal autonomy collapses while expenditure obligations remain.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    institutional, biographical, trapped, regional).

% Millions of workers in traditional manufacturing and construction whose employment stability depends on the old growth model. They are not at the table when industrial restructuring is designed; their retraining and social insurance transitions are underfunded relative to the speed of sectoral contraction.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_sector_workers, excluded,
    powerless, immediate, trapped, regional).

% Debate whether the 'high-quality development' shift is a genuine structural upgrade or a re-branding of state intervention that imposes transition costs on politically weaker regions and sectors. Their research informs internal policy reviews but does not set the agenda.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, academic_economists, observer,
    moderate, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transition from an investment-and-export-driven, environmentally destructive growth model to an innovation-driven, sustainable, and technologically competitive economy by realigning credit, cadre incentives, and industrial policy.
% TRANSFER_FUNCTION: Moves fiscal capacity, bank credit, land-use rights, and policy attention from traditional manufacturing and property-dependent local governments toward high-tech sectors and the state-backed innovation ecosystem.
% ABSENT_VOICES: Traditional sector workers and rural migrants displaced by industrial upgrading are absent from policy design; local governments whose fiscal model is dismantled are consulted but overruled by central mandates.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared, credit would flow back to property and heavy industry, local governments would resume land-finance, the innovation ecosystem would lose subsidies and state procurement, and the central leadership would need an alternative legitimacy narrative â the political economy reorganizes around a different development coalition.
% FOUNDING_PROBLEM: The pre-reform growth model produced unsustainable local government debt, severe environmental degradation, property-sector bubbles, technological dependence on foreign supply chains, and overcapacity in heavy industry â threatening both financial stability and long-term competitiveness.
% FOUNDING_PROBLEM_CORROBORATION: Central planners and the state innovation ecosystem attest the problem remains live. Independent economists and traditional manufacturing associations argue the restructuring pace exceeds the absorptive capacity of local economies and creates systemic risks of its own; international financial institutions acknowledge macroeconomic imbalances but dispute whether the prescribed remedy avoids a middle-income trap. No source outside the benefiting parties fully corroborates both the diagnosis and the specific remedy.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__qualitative_development_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__qualitative_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__qualitative_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the constraint systematically redirects fiscal and financial resources from politically weaker actors (traditional industry, local governments) toward state-favored sectors, extracting transition costs that are not fully compensated. Suppression (0.58) reflects active enforcement: environmental and debt-compliance crackdowns, credit rationing, and cadre evaluation changes that close off the old growth model as a viable alternative. Theater ratio (0.45) captures the gap between the 'high-quality development' rhetoric and the partial, uneven implementation â significant real resource shifts occur, but slogan repetition and vanity innovation projects remain prominent. Accessibility collapse (0.55) indicates that alternative development models (pure welfare statism, laissez-faire growth, import-led recovery) are politically closed off but not structurally impossible. Resistance (0.52) reflects documented pushback from local governments and traditional sectors, often expressed through hidden debt accumulation, regulatory arbitrage, and growth-target lobbying.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (central planners) experiences the constraint as a necessary and broadly beneficial coordination mechanism preventing systemic crisis. The payer seats (traditional manufacturing, property-dependent local governments) experience it as an asymmetric extraction that destroys their operating model without providing viable replacement pathways. The beneficiary seats (high-tech sectors, innovation ecosystem) experience it as enabling subsidy and strategic priority. These divergences are structurally derived from the beneficiary/victim declarations and exit asymmetries: trapped local governments versus constrained but prioritized high-tech firms.
 *
 * DIRECTIONALITY LOGIC:
 *   Central planners sit low-d as the constraint's authors and enforcers, though they bear political risk if the transformation fails. High-tech sectors and the innovation ecosystem are declared beneficiaries with constrained but favorable exit, placing them in the low-d beneficiary zone. Traditional manufacturing and property-dependent local governments are declared victims with constrained or trapped exit, placing them in the high-d target zone where effective extraction is amplified. Academic economists hold analytical exit and symmetric directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework prevents mislabeling this constraint as a pure snare by requiring evidence of a genuine coordination function: the constraint addresses real collective-action problems (environmental externalities, overcapacity, financial instability) that market mechanisms alone had not resolved. However, the mandatory victim and active-enforcement declarations prevent it from being classified as a pure rope, because the same structure that coordinates also asymmetrically extracts from identifiable losers. The tangled_rope classification captures this hybridity without requiring the author to resolve it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is the qualitative_development_reading of the performance_legitimacy kernel. Which empirical or political conditions determine whether this reading remains dominant versus reverting to quantitative_growth_reading or shifting to livelihood_security_reading?',
    'Track policy pivot signals in Five-Year Plans, Party Congress reports, and cadre evaluation criteria; measure whether innovation metrics or livelihood metrics gain rhetorical and budgetary priority during growth slowdowns.',
    'If the reading is contingent on growth rates above a threshold, it functions as a scaffold-like transitional framing rather than a stable legitimacy basis; if it persists through slowdowns, it is a durable constraint redefining the state''s legitimacy foundation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contingency, empirical, 'Contingency of the qualitative development reading within the performance legitimacy kernel').

omega_variable(
    coordination_or_extraction,
    'Does the high-quality development framework solve a genuine collective-action problem (environmental externalities, overcapacity, technological upgrading) or does it primarily transfer resources to politically favored high-tech constituencies under the cover of structural transformation?',
    'Compare total-factor productivity and environmental outcome trajectories in jurisdictions with differential enforcement intensity; test whether innovation subsidies crowd out private R&D or genuinely complement it.',
    'If genuine coordination dominates, the constraint is a rope or tangled rope with low extraction; if resource transfer dominates, it is a snare where the coordination story is cover for patronage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_or_extraction, empirical, 'Ambiguity between coordination function and extractive transfer in structural transformation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_qual_dev_tr_t0, performance_legitimacy__qualitative_development_reading, theater_ratio, 0, 0.65).
narrative_ontology:measurement(perf_qual_dev_tr_t2, performance_legitimacy__qualitative_development_reading, theater_ratio, 2, 0.62).
narrative_ontology:measurement(perf_qual_dev_tr_t4, performance_legitimacy__qualitative_development_reading, theater_ratio, 4, 0.58).
narrative_ontology:measurement(perf_qual_dev_tr_t6, performance_legitimacy__qualitative_development_reading, theater_ratio, 6, 0.52).
narrative_ontology:measurement(perf_qual_dev_tr_t8, performance_legitimacy__qualitative_development_reading, theater_ratio, 8, 0.5).
narrative_ontology:measurement(perf_qual_dev_tr_t10, performance_legitimacy__qualitative_development_reading, theater_ratio, 10, 0.47).
narrative_ontology:measurement(perf_qual_dev_tr_t12, performance_legitimacy__qualitative_development_reading, theater_ratio, 12, 0.45).

% Extraction over time
narrative_ontology:measurement(perf_qual_dev_be_t0, performance_legitimacy__qualitative_development_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(perf_qual_dev_be_t2, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(perf_qual_dev_be_t4, performance_legitimacy__qualitative_development_reading, base_extractiveness, 4, 0.46).
narrative_ontology:measurement(perf_qual_dev_be_t6, performance_legitimacy__qualitative_development_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(perf_qual_dev_be_t8, performance_legitimacy__qualitative_development_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(perf_qual_dev_be_t10, performance_legitimacy__qualitative_development_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(perf_qual_dev_be_t12, performance_legitimacy__qualitative_development_reading, base_extractiveness, 12, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(perf_qual_dev_su_t0, performance_legitimacy__qualitative_development_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(perf_qual_dev_su_t2, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2, 0.35).
narrative_ontology:measurement(perf_qual_dev_su_t4, performance_legitimacy__qualitative_development_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(perf_qual_dev_su_t6, performance_legitimacy__qualitative_development_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(perf_qual_dev_su_t8, performance_legitimacy__qualitative_development_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(perf_qual_dev_su_t10, performance_legitimacy__qualitative_development_reading, suppression_requirement, 10, 0.57).
narrative_ontology:measurement(perf_qual_dev_su_t12, performance_legitimacy__qualitative_development_reading, suppression_requirement, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the performance_legitimacy kernel, which decomposes into four structurally distinct constraints (quantitative_growth_reading, qualitative_development_reading, techno_nationalist_reading, livelihood_security_reading) due to epsilon-invariance: each reading has different beneficiary/victim structures, different empirical claims, and different policy implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
