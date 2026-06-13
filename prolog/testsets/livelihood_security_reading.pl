% ============================================================================
% CONSTRAINT STORY: livelihood_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_livelihood_security_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: livelihood_security_reading
 *   human_readable: Performance Legitimacy via Livelihood Security
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint is the livelihood security reading of the performance
 *   legitimacy kernel. The regime grounds its authority in delivering
 *   tangible improvements citizens directly experience: employment in
 *   services, healthcare access, education quality, elderly care
 *   infrastructure. Resource allocation shifts from capital-intensive
 *   industrial expansion toward consumption support and social services. The
 *   claim is rope (genuine coordination solving a legitimacy problem); the
 *   metrics show moderate extraction (industrial sectors and local
 *   governments pay the cost) and rising theater (as the constraint matures,
 *   more activity is devoted to publicizing the gains than delivering new
 *   ones). The engine measures the divergence; the claim and metrics are
 *   independent facts.
 *
 * KEY AGENTS:
 *   - central_planning_authority: Agenda-setter (institutional/analytical) — sets allocation priorities toward livelihood security
 *   - service_sector_workers: Beneficiary (organized/constrained) — employment depends on the constraint
 *   - household_consumers: Beneficiary (organized/constrained) — receive direct service improvements
 *   - elderly_population: Beneficiary (powerless/trapped) — depend entirely on care infrastructure
 *   - capital_intensive_industrial_sectors: Payer (powerful/constrained) — lose investment priority
 *   - local_government_infrastructure_budgets: Payer (institutional/trapped) — fiscal transfers shrink
 *   - export_oriented_manufacturers: Payer (powerful/mobile) — lose policy support, face higher costs
 *   - development_economists: Observer (analytical/analytical) — document trade-offs and sustainability questions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(livelihood_security_reading, 0.42).
domain_priors:suppression_score(livelihood_security_reading, 0.38).
domain_priors:theater_ratio(livelihood_security_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(livelihood_security_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(livelihood_security_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(livelihood_security_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(livelihood_security_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(livelihood_security_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(livelihood_security_reading, rope).
narrative_ontology:human_readable(livelihood_security_reading, "Performance Legitimacy via Livelihood Security").
narrative_ontology:topic_domain(livelihood_security_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(livelihood_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(livelihood_security_reading, '512eec1a-c2eb-46e8-8974-1bf094030c90').
narrative_ontology:cs_kernel_codification('512eec1a-c2eb-46e8-8974-1bf094030c90', formalized).
narrative_ontology:cs_authority_grounding('512eec1a-c2eb-46e8-8974-1bf094030c90', practice).
narrative_ontology:cs_interpretation_layer_present('512eec1a-c2eb-46e8-8974-1bf094030c90').
narrative_ontology:cs_reading_relation('512eec1a-c2eb-46e8-8974-1bf094030c90', livelihood_security_reading__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('512eec1a-c2eb-46e8-8974-1bf094030c90', livelihood_security_reading__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('512eec1a-c2eb-46e8-8974-1bf094030c90', livelihood_security_reading__techno_nationalist_reading, influences).
narrative_ontology:cs_axiom('512eec1a-c2eb-46e8-8974-1bf094030c90', foundational, legitimacy_via_felt_experience).
narrative_ontology:cs_axiom_status(legitimacy_via_felt_experience, holdable).
narrative_ontology:cs_axiom_grounding('512eec1a-c2eb-46e8-8974-1bf094030c90', legitimacy_via_felt_experience, instrumental).
narrative_ontology:cs_axiom('512eec1a-c2eb-46e8-8974-1bf094030c90', secondary, consumption_priority_over_accumulation).
narrative_ontology:cs_axiom_status(consumption_priority_over_accumulation, holdable).
narrative_ontology:cs_axiom_grounding('512eec1a-c2eb-46e8-8974-1bf094030c90', consumption_priority_over_accumulation, conventional).
narrative_ontology:cs_reference_frame('512eec1a-c2eb-46e8-8974-1bf094030c90', growth_legitimacy_consensus).
narrative_ontology:cs_drift_state('512eec1a-c2eb-46e8-8974-1bf094030c90', post_inequality_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('512eec1a-c2eb-46e8-8974-1bf094030c90', '').
narrative_ontology:cs_kernel_id(livelihood_security_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(livelihood_security_reading, service_sector_workers).
narrative_ontology:constraint_beneficiary(livelihood_security_reading, household_consumers).
narrative_ontology:constraint_beneficiary(livelihood_security_reading, elderly_population).
narrative_ontology:constraint_beneficiary(livelihood_security_reading, healthcare_recipients).
narrative_ontology:constraint_beneficiary(livelihood_security_reading, education_beneficiaries).
narrative_ontology:constraint_victim(livelihood_security_reading, capital_intensive_industrial_sectors).
narrative_ontology:constraint_victim(livelihood_security_reading, local_government_infrastructure_budgets).
narrative_ontology:constraint_victim(livelihood_security_reading, export_oriented_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets resource allocation priorities toward consumption support, social services, and redistribution mechanisms. Justifies the shift as delivering tangible improvements citizens directly experience, grounding regime legitimacy in felt quality-of-life gains rather than aggregate growth statistics or industrial capacity metrics.
narrative_ontology:constraint_stakeholder(livelihood_security_reading, central_planning_authority, agenda_setter,
    institutional, generational, analytical, national).

% Receive expanded employment in healthcare, education, elderly care, and consumer services as resource allocation shifts toward these sectors. Their livelihoods depend on the constraint's persistence; exit would mean returning to manufacturing or agricultural work with lower wages and security.
narrative_ontology:constraint_stakeholder(livelihood_security_reading, service_sector_workers, beneficiary,
    organized, biographical, constrained, national).

% Experience direct improvements in daily life: subsidized healthcare, expanded education access, elderly care infrastructure, consumption vouchers, housing support. The constraint delivers coordination benefits they can see and feel, distinguishing it from abstract growth metrics.
narrative_ontology:constraint_stakeholder(livelihood_security_reading, household_consumers, beneficiary,
    organized, biographical, constrained, national).

% Receive expanded care infrastructure, pension support, and medical services as the constraint prioritizes their needs. They have no exit option and depend entirely on the delivery system the constraint funds.
narrative_ontology:constraint_stakeholder(livelihood_security_reading, elderly_population, beneficiary,
    powerless, immediate, trapped, national).

% Bear the cost of resource reallocation away from industrial expansion and capital investment toward consumption and services. Their growth is constrained by credit allocation, land use priorities, and fiscal transfers that now flow to social spending. Exit means relocating production, which is costly and politically fraught.
narrative_ontology:constraint_stakeholder(livelihood_security_reading, capital_intensive_industrial_sectors, payer,
    powerful, generational, constrained, national).

% Face reduced fiscal transfers and borrowing capacity as central resources shift to consumption support and social services. Infrastructure projects are delayed or canceled; local officials must deliver services with shrinking budgets while the central authority claims credit for the consumption gains.
narrative_ontology:constraint_stakeholder(livelihood_security_reading, local_government_infrastructure_budgets, payer,
    institutional, generational, trapped, regional).

% Lose policy support as the constraint deprioritizes export competitiveness in favor of domestic consumption. They face higher labor costs as service-sector wages rise, reduced infrastructure investment in export zones, and credit rationing. Their exit option is real but costly: relocating supply chains takes years.
narrative_ontology:constraint_stakeholder(livelihood_security_reading, export_oriented_manufacturers, payer,
    powerful, biographical, mobile, global).

% Analyze whether livelihood security can sustain legitimacy without underlying productivity growth, whether consumption-led models are stable, and whether the constraint represents genuine development or deferred crisis. They document the trade-offs and publish the structural tensions.
narrative_ontology:constraint_stakeholder(livelihood_security_reading, development_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates resource allocation toward sectors that deliver visible, felt improvements in daily life, solving the collective action problem of sustaining regime legitimacy through tangible service delivery rather than abstract growth metrics.
% TRANSFER_FUNCTION: Moves fiscal resources, credit allocation, and policy priority from capital-intensive industrial expansion and local infrastructure spending toward household consumption support, healthcare, education, and elderly care services.
% ABSENT_VOICES: Future generations who will inherit the deferred infrastructure investment and potential productivity stagnation; industrial workers whose sectors are deprioritized; local officials whose budgets shrink while service demands rise. They are structurally excluded from the legitimacy calculus because the constraint optimizes for immediate felt experience.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, resource allocation would revert to industrial expansion and infrastructure investment; service-sector employment would contract; household consumption support would collapse; the regime would lose its primary legitimacy mechanism and face pressure to find alternative grounding (growth statistics, nationalist mobilization, or coercion).
% FOUNDING_PROBLEM: Aggregate GDP growth and industrial capacity expansion were not translating into felt improvements in citizens' daily lives, creating a legitimacy gap where the regime's performance claims were disconnected from lived experience.
% FOUNDING_PROBLEM_CORROBORATION: Development economists, household survey data, and independent social science research corroborate that the legitimacy-via-growth model faced diminishing returns as inequality widened and environmental costs mounted. The constraint's shift toward livelihood security addresses a real coordination problem, though its sustainability is contested.
narrative_ontology:disappearance_verdict(livelihood_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(livelihood_security_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(livelihood_security_reading, '046e0a40c34cddf4fff29b8c15f632dbdef31b7a',
    'c6d6880c39ec6bdfedde2a1d41cc00211f451559', '2026-06-11',
    'performance_legitimacy_kernel', 'agent/example_platform_commission.json',
    'claude-sonnet-4-20250514', 'temperature=1.0').
narrative_ontology:story_seed(livelihood_security_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(livelihood_security_reading_tests).
:- end_tests(livelihood_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) because the constraint genuinely coordinates a shift toward felt improvements, but industrial sectors and local governments bear real costs in foregone investment and fiscal capacity. Suppression is moderate-low (0.38) because the constraint operates through budget allocation and credit policy rather than coercion, though local officials and industrial managers have limited voice in the reallocation. Theater rises slowly (0.18 → 0.28) as the constraint matures and more resources go to publicizing service gains than expanding them. Accessibility collapse is moderate (0.48) because alternative legitimacy models (growth statistics, nationalist mobilization) remain conceptually available. Resistance is moderate (0.52) because industrial sectors and local governments push back through policy channels, though they cannot exit the system.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (service workers, households, elderly), the constraint delivers real coordination solving a felt need. From the payer seats (industrial sectors, local governments), the same structure extracts resources from productive investment to fund consumption that may not be sustainable. From the analytical seat, the constraint is a legitimacy mechanism whose long-term viability depends on whether consumption-led models can sustain themselves without underlying productivity growth.
 *
 * DIRECTIONALITY LOGIC:
 *   The central planning authority is the agenda-setter with analytical exit (d near beneficiary end, though it bears political risk if the model fails). Service-sector workers, household consumers, and the elderly are beneficiaries with constrained or trapped exit (d near beneficiary end). Capital-intensive industrial sectors, local governments, and export manufacturers are payers with constrained or mobile exit (d near target end, modulated by their exit options). Development economists are observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not mandatrophy: the founding problem (legitimacy gap from growth-without-felt-improvement) is live, and the constraint's disappearance would force the regime to find alternative legitimacy grounding. The rising theater ratio indicates some drift toward performance over substance, but the core coordination function remains active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consumption_sustainability,
    'Can a legitimacy model grounded in household consumption and service delivery sustain itself without underlying productivity growth in industrial and infrastructure sectors?',
    'Longitudinal fiscal and economic data: if consumption support requires escalating debt or inflation, the model is unsustainable; if service-sector productivity rises enough to fund itself, the model is stable.',
    'If unsustainable, the constraint is deferred crisis rather than genuine coordination, and the regime will face a legitimacy collapse when fiscal limits bind. If sustainable, the constraint represents a real shift in development strategy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consumption_sustainability, empirical, 'Whether consumption-led legitimacy is fiscally sustainable.').

omega_variable(
    alternative_reading_foreclosure,
    'Does prioritizing livelihood security structurally foreclose the quantitative growth and techno-nationalist readings, or do all four readings remain live within the regime''s policy space?',
    'Policy document analysis and budget allocation over time: if industrial investment and R&D spending collapse, the alternative readings are foreclosed; if they persist at reduced but viable levels, the readings coexist.',
    'If foreclosed, this reading has won the internal contest and the kernel has collapsed to a single interpretation. If coexisting, the regime is managing a portfolio of legitimacy claims and the readings remain in tension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Whether this reading forecloses or coexists with sibling readings.').

omega_variable(
    theater_drift_threshold,
    'At what theater ratio does the constraint''s coordination function collapse into pure performance, where publicizing service gains replaces delivering them?',
    'Service delivery outcome data compared to publicity spending: if outcomes plateau while publicity spending rises, the threshold has been crossed.',
    'If the threshold is crossed, the constraint degrades from rope to piton, and the regime''s legitimacy mechanism becomes self-undermining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_drift_threshold, empirical, 'Threshold at which coordination collapses into theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(livelihood_security_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(live_tr_t0, livelihood_security_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(live_tr_t5, livelihood_security_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement(live_tr_t10, livelihood_security_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement(live_tr_t15, livelihood_security_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(live_tr_t20, livelihood_security_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(live_tr_t25, livelihood_security_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(live_be_t0, livelihood_security_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(live_be_t5, livelihood_security_reading, base_extractiveness, 5, 0.37).
narrative_ontology:measurement(live_be_t10, livelihood_security_reading, base_extractiveness, 10, 0.39).
narrative_ontology:measurement(live_be_t15, livelihood_security_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(live_be_t20, livelihood_security_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(live_be_t25, livelihood_security_reading, base_extractiveness, 25, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(live_su_t0, livelihood_security_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(live_su_t5, livelihood_security_reading, suppression_requirement, 5, 0.32).
narrative_ontology:measurement(live_su_t10, livelihood_security_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement(live_su_t15, livelihood_security_reading, suppression_requirement, 15, 0.36).
narrative_ontology:measurement(live_su_t20, livelihood_security_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(live_su_t25, livelihood_security_reading, suppression_requirement, 25, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(livelihood_security_reading, resource_allocation).
narrative_ontology:affects_constraint(livelihood_security_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(livelihood_security_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(livelihood_security_reading, techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the performance_legitimacy kernel. Each reading instantiates a different legitimacy mechanism with different beneficiary/victim structures and different resource allocation priorities. They are linked via network.affects_constraints because resource shifts toward livelihood security directly constrain the fiscal and policy space available to the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
