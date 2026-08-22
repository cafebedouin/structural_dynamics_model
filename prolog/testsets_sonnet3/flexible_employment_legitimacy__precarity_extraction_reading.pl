% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__precarity_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__precarity_extraction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: flexible_employment_legitimacy__precarity_extraction_reading
 *   human_readable: Platform 'Flexible Employment' as Structural Precarity / Surplus Extraction
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This story instantiates the precarity_extraction_reading of the
 *   flexible_employment_legitimacy kernel: it treats the standing arrangement
 *   of contractor-classified, algorithmically-managed platform work as
 *   structural precarity through which platforms extract surplus value that
 *   would otherwise be captured by workers or absorbed as employer costs.
 *   This is one of three readings of the same kernel text (worker
 *   classification and 'flexibility' rhetoric). The sibling
 *   market_efficiency_reading treats the identical arrangement as a
 *   legitimate, welfare-improving matching mechanism; the sibling
 *   developmental_state_reading treats it as a transitional form the state
 *   should manage toward eventual formalization. All three readings describe
 *   the SAME standing arrangement — the difference is what each reading holds
 *   to be true about its function and trajectory, per DP-001 ε-invariance and
 *   the kernel-reading discipline: each reading gets its own ε, evaluated by
 *   that reading's own lights, not averaged across readings.
 *
 * KEY AGENTS:
 *   - platform_operators: agenda_setter (institutional/arbitrage) — designs and enforces the classification and algorithmic-control regime; primary capturer of the extraction
 *   - gig_delivery_drivers, ride_hail_drivers, microtask_workers: primary targets (powerless/trapped) — bear risk-externalization costs and algorithmic discipline with no comparable exit
 *   - corporate_labor_purchasers, platform_shareholders: secondary beneficiaries — capture cost savings and valuation upside without administering the arrangement directly
 *   - social_insurance_systems: diffuse institutional payer — absorbs the fiscal externality of under-insured, under-employed workers churning into public assistance
 *   - worker_organizing_networks: excluded voice — structurally kept out of algorithm design and rate-setting
 *   - labor_economists_regulators: analytical observer — documents misclassification and cost-shifting empirically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.78).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.68).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, tangled_rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Platform 'Flexible Employment' as Structural Precarity / Surplus Extraction").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, 'c5fb8151-2b8c-4606-9c00-701b46ab5b80').
narrative_ontology:cs_kernel_codification('c5fb8151-2b8c-4606-9c00-701b46ab5b80', distributed).
narrative_ontology:cs_authority_grounding('c5fb8151-2b8c-4606-9c00-701b46ab5b80', extraction).
narrative_ontology:cs_interpretation_layer_present('c5fb8151-2b8c-4606-9c00-701b46ab5b80').
narrative_ontology:cs_reading_relation('c5fb8151-2b8c-4606-9c00-701b46ab5b80', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5fb8151-2b8c-4606-9c00-701b46ab5b80', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('c5fb8151-2b8c-4606-9c00-701b46ab5b80', foundational, algorithmic_control_constitutes_employment_relationship).
narrative_ontology:cs_axiom_status(algorithmic_control_constitutes_employment_relationship, holdable).
narrative_ontology:cs_axiom_grounding('c5fb8151-2b8c-4606-9c00-701b46ab5b80', algorithmic_control_constitutes_employment_relationship, empirically_contingent).
narrative_ontology:cs_axiom('c5fb8151-2b8c-4606-9c00-701b46ab5b80', foundational, risk_externalization_is_uncompensated_cost_transfer).
narrative_ontology:cs_axiom_status(risk_externalization_is_uncompensated_cost_transfer, holdable).
narrative_ontology:cs_axiom_grounding('c5fb8151-2b8c-4606-9c00-701b46ab5b80', risk_externalization_is_uncompensated_cost_transfer, empirically_contingent).
narrative_ontology:cs_reference_frame('c5fb8151-2b8c-4606-9c00-701b46ab5b80', standard_employment_relationship_baseline).
narrative_ontology:cs_drift_state('c5fb8151-2b8c-4606-9c00-701b46ab5b80', post_gig_economy_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c5fb8151-2b8c-4606-9c00-701b46ab5b80', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_shareholders).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, corporate_labor_purchasers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, gig_delivery_drivers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, ride_hail_drivers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, microtask_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, social_insurance_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design the app's dispatch, rating, and deactivation algorithms; classify workers as independent contractors to avoid employer obligations (minimum wage floors, unemployment insurance, employer-side payroll tax, collective bargaining exposure). Sets pay algorithms opaquely and can alter rates or terms unilaterally through app updates. Captures the delta between what it would cost to run a workforce under employment law and what it actually pays.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, beneficiary).

% Hold equity valued in part on the assumption that labor costs remain below full-employment-cost benchmarks; do not administer the platform directly but the extraction accrues to them through valuation and dividends.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_shareholders, beneficiary,
    institutional, generational, arbitrage, global).

% Restaurants, retailers, and logistics firms that contract platform labor on-demand, avoiding the fixed costs and legal exposure of direct employment; can switch platforms or in-source at relatively low friction.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, corporate_labor_purchasers, beneficiary,
    powerful, biographical, mobile, national).

% Supply vehicle, fuel, insurance, and unpaid waiting time; bear all downside of accidents, illness, and demand fluctuation with no sick pay, no employer-side social insurance contribution, and no guaranteed hours. Algorithmic deactivation for low ratings or refused jobs functions as discipline without due process. Many depend on the income for survival and have no comparable alternative at similar pay in their local labor market.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_delivery_drivers, payer,
    powerless, immediate, trapped, local).

% Similar structural position to delivery drivers: absorb vehicle depreciation, fuel, and insurance costs that a formal employer would otherwise share; algorithmic pay changes (surge suppression, incentive withdrawal) are imposed without negotiation. Some hold multi-apping as a partial hedge, but this multiplies administrative burden rather than restoring bargaining power.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, ride_hail_drivers, payer,
    powerless, immediate, trapped, local).

% Perform piecework digital tasks (labeling, transcription, content moderation) for sub-minimum effective wages once unpaid search and rejection time is counted; global labor-supply pooling by the platform means any local wage floor is arbitraged away by routing tasks to workers in lower-cost jurisdictions.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, microtask_workers, payer,
    powerless, immediate, trapped, global).

% Attempt to organize gig workers for reclassification or sectoral bargaining but are structurally excluded from the platforms' own decision processes over algorithm design and pay-rate setting; litigation and ballot-initiative routes are slow and platforms fund well-resourced counter-campaigns.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, worker_organizing_networks, excluded,
    moderate, generational, constrained, national).

% Public unemployment, disability, and healthcare systems absorb the costs platforms externalize by not classifying workers as employees; when gig workers churn into public assistance programs the fiscal burden shifts to taxpayers rather than the firms whose labor demand generated it.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, social_insurance_systems, payer,
    institutional, generational, trapped, national).

% Study misclassification, algorithmic wage-setting, and cost-shifting empirically; can recommend reclassification rules or portable-benefit mandates but do not control platform operations directly.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_economists_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__precarity_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Platforms do solve a real matching problem — connecting fragmented, spatially dispersed labor supply with fragmented, time-variable demand for rides, deliveries, and microtasks — faster and at larger scale than prior informal or agency-based arrangements.
% TRANSFER_FUNCTION: Moves the cost of insurance, downtime, equipment, and demand-risk from the platform (which would bear these as an employer) onto individual workers, while moving the surplus created by algorithmic wage suppression and global labor-pool arbitrage to platform operators, shareholders, and purchasing firms.
% ABSENT_VOICES: Gig workers as a class have no seat in algorithm design, rate-setting, or deactivation appeals; worker organizing networks are kept out of the platforms' internal governance and face funded countermobilization when they seek reclassification through courts or ballot measures.
% DISAPPEARANCE_RATIONALE: If contractor classification and algorithmic wage-setting disappeared overnight and full employment status applied, platform unit economics would shift sharply (mandatory minimum wage floors, employer payroll tax, unemployment and injury insurance), forcing consolidation, price increases to consumers, or reduced worker counts — the labor-supply pool, valuation models, and public-benefit fiscal flows would all reorganize.
% FOUNDING_PROBLEM: Coordinating a large, spatially and temporally variable pool of willing workers with equally variable, spiky consumer demand for rides, delivery, and digital microtasks, without the fixed-cost overhead of scheduling and staffing a conventional workforce.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators attest the founding problem (matching variable supply and demand) remains live and that flexibility benefits workers who value autonomy. Labor economists and social insurance administrators, external to platform ownership, attest via peer-reviewed misclassification studies and public-benefit cost-shifting analyses that the coordination problem is largely solved by the technology itself and that continued contractor classification now functions primarily to avoid statutory labor costs rather than to solve a residual matching problem.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__precarity_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.78 at interval end) because, under this reading, the wage a platform pays is systematically below what a comparable employer-of-record wage would be once unpaid downtime, equipment costs, and forgone social insurance contributions are counted — the gap is captured as margin rather than passed through as genuinely lower consumer prices or genuinely higher worker autonomy value. Suppression is authored substantial (0.68) because algorithmic deactivation, opaque rate-setting, and well-funded countermobilization against reclassification efforts function as active mechanisms discouraging exit and organizing, not merely as background market friction. Theater ratio is moderate (0.42) and rising: 'flexibility' and 'be your own boss' framing performs an autonomy narrative that increasingly diverges from the algorithmically scheduled, rating-disciplined reality of the work — this is the metric substitution channel (Goodhart drift) the temporal series is built to surface. All three metrics share one time grid (T=0 to T=24) so no metric's value is silently substituted at a point another metric tracks.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform_operators sit at the full-beneficiary end: they set the terms, capture the classification-avoidance savings, and hold arbitrage-grade exit (can relocate operations, restructure entities, or exit jurisdictions facing reclassification pressure). Gig workers sit at the full-target end: trapped exit (dependent on the income, no comparable local alternative at similar pay), immediate time horizon (cannot absorb income shocks), and no voice in the terms imposed on them — this is a paradigm case of the derivation chain producing high d from victim declaration plus trapped exit without needing an override. Social insurance systems occupy an unusual institutional-payer position: they do not choose to bear the cost but absorb it structurally as workers churn into public programs, which is why they are listed as payer despite institutional power — power does not determine directionality here, exposure does.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than pure snare) is deliberate under this reading: a genuine coordination function is acknowledged (matching variable, fragmented labor supply to variable, fragmented demand faster than prior informal arrangements could) — this is what distinguishes the reading from treating flexible employment as pure extraction with a cover story. What makes it tangled rather than a clean rope is that the SAME mechanism that performs the coordination (the algorithm, the app, the contractor classification) is also the mechanism through which asymmetric extraction occurs, and persistence requires active enforcement (contesting reclassification litigation, lobbying against portable-benefit mandates, algorithmic deactivation as discipline). The classification prevents mislabeling this as pure coordination (which would erase the extraction) or pure extraction (which would erase the real matching function that even critical labor economists acknowledge platforms perform).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_gain_risk_externalization_net,
    'Do the wage premiums platform workers sometimes report (relative to comparable low-skill formal-sector work) actually exceed the monetized value of the risk, equipment, and benefit costs externalized onto them, or is the apparent premium an artifact of undercounting unpaid time and depreciation?',
    'Time-use studies capturing unpaid search/waiting time combined with full-cost accounting of vehicle depreciation, insurance, and forgone employer-side social insurance contributions, compared against matched formal-sector wages for equivalent effective hours.',
    'If the net comparison favors workers once full costs are counted, this reading''s extraction claim weakens substantially and the classification should move toward the market_efficiency_reading''s territory; if it confirms a net loss, the extraction claim under this reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_gain_risk_externalization_net, empirical, 'Whether platform wage premiums survive full risk-externalization cost accounting.').

omega_variable(
    algorithmic_control_as_employment_relationship,
    'Does algorithmic direction of work (dynamic pricing, route assignment, deactivation for performance metrics) constitute a de facto employment relationship under existing labor law tests (control, integration, economic dependence), or is it structurally distinct from direct supervisory control?',
    'Comparative legal analysis and misclassification litigation outcomes across jurisdictions; a consistent judicial finding that algorithmic control satisfies employment-relationship tests would corroborate this reading''s premise that the classification is a legal fiction rather than an accurate description.',
    'If courts/regulators consistently find algorithmic control equivalent to employment control, the contractor classification itself becomes the primary extraction mechanism (strengthening tangled_rope/snare readings); if courts find it structurally distinct, the developmental_state_reading''s transitional framing gains support instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_as_employment_relationship, conceptual, 'Whether algorithmic management legally and functionally equals direct employment control.').

omega_variable(
    kernel_reading_selection_evidence,
    'What observable evidence would distinguish which of the three kernel readings (precarity_extraction, market_efficiency, developmental_state) best fits the trajectory of platform labor markets going forward, rather than merely reflecting which reading an observer starts from?',
    'Longitudinal tracking of whether platforms voluntarily or under regulatory pressure move toward portable benefits and reclassification (supporting developmental_state), whether worker earnings and autonomy measures improve with market maturation absent regulation (supporting market_efficiency), or whether extraction metrics (this story''s ε, suppression, theater_ratio) continue rising under stable enforcement (supporting precarity_extraction).',
    'Resolving this would not change this story''s own ε (fixed under ε-invariance) but would inform which reading the broader kernel contest should weight most heavily in policy design.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'What future evidence would arbitrate between the three sibling kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(flex_tr_t4, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(flex_tr_t8, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(flex_tr_t12, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(flex_tr_t16, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(flex_tr_t20, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(flex_tr_t24, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(flex_be_t4, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 4, 0.6).
narrative_ontology:measurement(flex_be_t8, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(flex_be_t12, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(flex_be_t16, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 16, 0.73).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(flex_be_t24, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 24, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(flex_su_t4, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(flex_su_t8, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(flex_su_t12, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(flex_su_t16, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 16, 0.65).
narrative_ontology:measurement(flex_su_t20, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(flex_su_t24, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__precarity_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__precarity_extraction_reading, 0.12).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__developmental_state_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the flexible_employment_legitimacy kernel per the ε-invariance principle: precarity_extraction_reading (this file, tangled_rope, ε=0.78), market_efficiency_reading (rope, expected low ε), and developmental_state_reading (scaffold, expected moderate ε with a declared sunset toward formalization). All three describe the same standing contractor-classification/algorithmic-management arrangement but differ in what each holds to be true about its function, persistence, and trajectory. Linked via affects_constraints in both directions per the network decomposition rule; each file documents the relationship independently in commentary.kernel_context / narrative_context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
