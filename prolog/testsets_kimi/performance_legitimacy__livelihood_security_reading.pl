% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__livelihood_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__livelihood_security_reading, []).

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
 *   constraint_id: performance_legitimacy__livelihood_security_reading
 *   human_readable: Performance Legitimacy: Livelihood Security Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint instantiates the livelihood_security_reading of the
 *   contested performance_legitimacy kernel. In this reading, state
 *   legitimacy is grounded in delivering tangible improvements in citizens'
 *   daily material conditions â employment, healthcare, education, and
 *   elderly care â that are directly experienced. The constraint
 *   structurally prioritizes consumption support and service delivery over
 *   capital-intensive industrial expansion and local government
 *   infrastructure spending, enforcing redistribution through centralized
 *   fiscal and credit controls. It is one of four live readings of how to
 *   define 'performance' for legitimacy purposes, competing with quantitative
 *   growth, qualitative development, and techno-nationalist framings within
 *   the same state-capitalist domain.
 *
 * KEY AGENTS:
 *   - State redistribution apparatus: agenda_setter (institutional/generational) â administers the fiscal machinery and enforces consumption priority.
 *   - Service sectors: beneficiary (organized) â receive redirected state funding for healthcare, education, and elderly care.
 *   - Household consumption: beneficiary (moderate) â experience tangible welfare improvements but are constrained to state provision.
 *   - Capital-intensive industry: payer (powerful) â bear the cost of redirected credit and starved investment cycles.
 *   - Local government infrastructure: payer (organized) â lose fiscal autonomy and infrastructure budgets to central redistribution mandates.
 *   - Investment-priority advocates: excluded (moderate) â argue for resumed industrial expansion but are marginalized in livelihood-dominant discourse.
 *   - Political economy analysts: observer (analytical) â assess fiscal sustainability and long-term tradeoffs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, 0.62).
domain_priors:suppression_score(performance_legitimacy__livelihood_security_reading, 0.58).
domain_priors:theater_ratio(performance_legitimacy__livelihood_security_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(performance_legitimacy__livelihood_security_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__livelihood_security_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__livelihood_security_reading, "Performance Legitimacy: Livelihood Security Reading").
narrative_ontology:topic_domain(performance_legitimacy__livelihood_security_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__livelihood_security_reading, 'd1a7b923-82ff-4b85-88c9-29435d7f4951').
narrative_ontology:cs_kernel_codification('d1a7b923-82ff-4b85-88c9-29435d7f4951', distributed).
narrative_ontology:cs_authority_grounding('d1a7b923-82ff-4b85-88c9-29435d7f4951', practice).
narrative_ontology:cs_interpretation_layer_present('d1a7b923-82ff-4b85-88c9-29435d7f4951').
narrative_ontology:cs_reading_relation('d1a7b923-82ff-4b85-88c9-29435d7f4951', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('d1a7b923-82ff-4b85-88c9-29435d7f4951', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('d1a7b923-82ff-4b85-88c9-29435d7f4951', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_axiom('d1a7b923-82ff-4b85-88c9-29435d7f4951', foundational, tangible_livelihood_as_legitimacy_core).
narrative_ontology:cs_axiom_status(tangible_livelihood_as_legitimacy_core, holdable).
narrative_ontology:cs_axiom_grounding('d1a7b923-82ff-4b85-88c9-29435d7f4951', tangible_livelihood_as_legitimacy_core, conventional).
narrative_ontology:cs_axiom('d1a7b923-82ff-4b85-88c9-29435d7f4951', foundational, consumption_priority_over_accumulation).
narrative_ontology:cs_axiom_status(consumption_priority_over_accumulation, holdable).
narrative_ontology:cs_axiom_grounding('d1a7b923-82ff-4b85-88c9-29435d7f4951', consumption_priority_over_accumulation, instrumental).
narrative_ontology:cs_reference_frame('d1a7b923-82ff-4b85-88c9-29435d7f4951', consumption_priority_development).
narrative_ontology:cs_drift_state('d1a7b923-82ff-4b85-88c9-29435d7f4951', contemporary_policy_ecosystem, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d1a7b923-82ff-4b85-88c9-29435d7f4951', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, service_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__livelihood_security_reading, household_consumption).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, capital_intensive_industry).
narrative_ontology:constraint_victim(performance_legitimacy__livelihood_security_reading, local_government_infrastructure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the fiscal and political machinery that mandates consumption and service delivery priority over capital-intensive investment. Derives governing legitimacy from measurable improvements in household welfare and enforces budget reallocation through central planning, credit guidance, and political discipline over local officials.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, state_redistribution_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Healthcare, education, elderly care, and social service providers who receive prioritized state funding and policy support. Their institutional survival and expansion depend on continued redistribution toward consumption; they cannot easily replace state fiscal support with private revenue without collapsing access.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, service_sectors, beneficiary,
    organized, biographical, constrained, national).

% Citizens who experience tangible material improvements in daily life through subsidized or universal healthcare, education, elderly care, and employment support. Dependent on state budgetary priorities for continued service quality; exit to fully private provision is prohibitively expensive or unavailable at scale.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, household_consumption, beneficiary,
    moderate, biographical, constrained, national).

% Heavy industrial and infrastructure expansion sectors that bear the cost of redirected credit and fiscal resources. Investment cycles are shortened or starved as state lending and budget allocations prioritize social consumption over productive capacity expansion; sunk costs and political licensing make exit difficult.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, capital_intensive_industry, payer,
    powerful, biographical, constrained, national).

% Sub-national administrative units that lose fiscal autonomy and infrastructure budgets to central redistribution mandates. Forced to fund consumption-oriented projects and social transfers instead of local capital investment; politically bound to comply with central livelihood targets.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, local_government_infrastructure, payer,
    organized, biographical, constrained, national).

% Economists, planners, and industrial policy voices who argue for resumed capital-intensive expansion and infrastructure-led growth. Marginalized in policy discourse where livelihood metrics and consumption indicators dominate legitimacy claims; their exclusion is structural to the prevailing reading.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, investment_priority_advocates, excluded,
    moderate, biographical, constrained, national).

% Independent scholars and institutions who track the fiscal sustainability of consumption-priority models and the long-term tradeoffs between service delivery and productive investment. They assess whether the legitimacy model is stabilizing or depleting its own productive base.
narrative_ontology:constraint_stakeholder(performance_legitimacy__livelihood_security_reading, political_economy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__livelihood_security_reading, diffuse).
narrative_ontology:fixing_cost_class(performance_legitimacy__livelihood_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Collective provision of healthcare, education, elderly care, and employment security at national scale, solving market failures in social service delivery and maintaining political stability through tangible, experience-near welfare improvements.
% TRANSFER_FUNCTION: Moves fiscal resources, credit allocation, and policy priority from capital-intensive industrial expansion and local infrastructure budgets toward service sectors and household consumption.
% ABSENT_VOICES: Investment-priority strategists and industrial planners who argue for productive capacity expansion over consumption; structurally excluded from policy discourse where livelihood metrics determine legitimacy and budget flows.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, fiscal and credit flows would reverse toward industrial and infrastructure investment, service sector funding would contract sharply, household consumption support would erode, and the political legitimacy model would shift from service delivery to an alternative performance reading or basis â the developmental state reorganizes around a different redistribution logic.
% FOUNDING_PROBLEM: Post-developmental or post-transition regimes facing legitimacy deficits from rapid aggregate growth without distributive justice; the need to provide tangible, experience-near welfare gains to maintain political support amid rising inequality, social disruption, and uneven development.
% FOUNDING_PROBLEM_CORROBORATION: Development economists outside the state apparatus corroborate the genuine coordination function in poverty reduction and coverage expansion. Fiscal analysts and credit rating institutions outside the beneficiary set corroborate accumulating structural imbalances from under-investment. Independent household surveys support welfare gains; independent macro-financial analysis supports investment starvation.
narrative_ontology:disappearance_verdict(performance_legitimacy__livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__livelihood_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__livelihood_security_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__livelihood_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__livelihood_security_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__livelihood_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__livelihood_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__livelihood_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is a tangled rope because it combines a genuine coordination function â collective provision of healthcare, education, and elderly care at scale â with asymmetric extraction from capital-intensive industry and local infrastructure budgets. Extractiveness (0.62) reflects the substantial fiscal diversion required to sustain consumption priority; suppression (0.58) reflects the active political and budgetary enforcement needed to override investment-first resistance. Theater ratio (0.40) captures the moderate performative pressure on local officials to report livelihood metrics that justify the model, even as real service provision occurs. Accessibility collapse (0.60) reflects the erosion of local fiscal autonomy and market alternatives once the centralized redistribution system is entrenched. Resistance (0.55) captures the pushback from industrial and local government payers. The metrics and claimed type are authored independently: the structural claim is tangled rope, and the metrics describe the observed operation without tuning to match the engine.
 *
 * PERSPECTIVAL GAP:
 *   From the household and service-sector seats, the constraint computes as coordination â they receive tangible benefits and have few alternatives. From the capital-intensive industry and local infrastructure seats, the same constraint computes as extraction â they pay the cost of the transfer through foregone investment and lost fiscal autonomy. The state apparatus occupies an intermediate position: it benefits from legitimacy but is locked into a fiscal structure that may deplete the productive base. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The state redistribution apparatus sits near the beneficiary end (d low) because the constraint subsidizes its legitimacy and governance capacity. Service sectors and household consumption are structural beneficiaries (d low to moderate) â they receive the transferred resources. Capital-intensive industry and local government infrastructure are structural targets (d high) â they bear the extraction through diverted budgets and credit rationing. The divergence is sharp between the household seat (experiences coordination as welfare) and the industrial seat (experiences the same structure as extraction), despite both operating within the same national political economy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â legitimacy deficit from growth without distribution â is contested. If the problem were genuinely solved (broad welfare secured and self-sustaining), the constraint could in principle sunset or degrade to a rope. However, because the model actively suppresses investment alternatives and creates constituencies dependent on continued redistribution, it risks mandatrophy only if the productive base collapses entirely. The classification as tangled rope prevents mislabeling the genuine service provision as pure extraction, while the victim declarations prevent mislabeling the industrial budget starvation as mere coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_sustainability_ambiguity,
    'Does the consumption-priority model extract from productive investment faster than it replenishes the fiscal base, creating a sustainability trap?',
    'Longitudinal fiscal accounting separating consumption transfers from productive investment returns; cross-national comparison of state-capitalist fiscal models over multi-decade horizons.',
    'If extraction exceeds replenishment, the constraint degrades toward a time-bounded snare as the productive base collapses; if balanced, it persists as a tangled rope with genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_ambiguity, empirical, 'Whether livelihood redistribution is fiscally sustainable or consumes its own productive base.').

omega_variable(
    reading_stability,
    'Is the livelihood security reading a stable structural commitment or a tactical adjustment within a shifting performance legitimacy kernel?',
    'Historical analysis of policy priority shifts between livelihood, high-quality development, quantitative growth, and techno-nationalist readings during leadership transitions or external shocks.',
    'If tactical, the constraint''s extractiveness and coordination function may spike or collapse abruptly during kernel shifts; if stable, it persists as a genuine long-term constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_stability, conceptual, 'Whether this reading is a durable commitment or a tactical position within a contested kernel.').

omega_variable(
    authority_grounding_ambiguity,
    'Does the state''s authority rest on the practice of service delivery itself, or on extracting legitimacy by preventing the performance kernel from settling into any single rival reading?',
    'Analyze whether the regime shifts readings when one fails (extraction pattern) or doubles down on service delivery capacity (practice pattern) during legitimacy crises.',
    'If extraction, the constraint is more brittle, more theatrical, and vulnerable to sudden collapse; if practice, it is more robust and genuinely coordinated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_ambiguity, conceptual, 'Whether authority derives from actual service practice or from kernel ambiguity management.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__livelihood_security_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__livelihood_security_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__livelihood_security_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__livelihood_security_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__livelihood_security_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__livelihood_security_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(perf_tr_t25, performance_legitimacy__livelihood_security_reading, theater_ratio, 25, 0.4).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__livelihood_security_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__livelihood_security_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__livelihood_security_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__livelihood_security_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__livelihood_security_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(perf_be_t25, performance_legitimacy__livelihood_security_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__livelihood_security_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__livelihood_security_reading, suppression_requirement, 5, 0.49).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__livelihood_security_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__livelihood_security_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__livelihood_security_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement(perf_su_t25, performance_legitimacy__livelihood_security_reading, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__livelihood_security_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__livelihood_security_reading, techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the performance_legitimacy kernel. The kernel decomposes into four structurally distinct constraints because the label 'performance legitimacy' conflates competing claims about what counts as legitimate state performance. Each reading has a different epsilon, beneficiary/victim structure, and resource allocation logic. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
