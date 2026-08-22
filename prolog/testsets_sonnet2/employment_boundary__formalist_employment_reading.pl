% ============================================================================
% CONSTRAINT STORY: employment_boundary__formalist_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__formalist_employment_reading, []).

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
 *   constraint_id: employment_boundary__formalist_employment_reading
 *   human_readable: Formalist Employment Boundary (Contract-and-Supervision Test)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This story instantiates the formalist reading of the employment boundary
 *   kernel: employment status is determined by the presence of a formal
 *   written contract and direct human supervision. Under this reading,
 *   platform workers — drivers, couriers, task-based gig labor — are
 *   independent contractors because the platform's control operates through
 *   an app and algorithm rather than a named human supervisor issuing direct
 *   instructions, and because the parties' written agreement labels the
 *   relationship as non-employment. This reading excludes platform workers
 *   from the victim set of 'employment precarity' proper (the formalist view
 *   holds they voluntarily chose flexible, unsupervised work) and excludes
 *   platforms from employer-side obligations (payroll tax, unemployment
 *   insurance, workers' compensation, minimum wage floors). The structural
 *   consequence is high measured extraction: costs that would otherwise be
 *   internalized by the employing entity are externalized onto workers
 *   directly and onto state social insurance systems that receive no
 *   corresponding contribution stream. Two sibling readings of the same
 *   underlying kernel are NOT part of this constraint: the
 *   substantive_employment_reading holds that economic dependence and
 *   algorithmic control (not contract form) define employment, placing
 *   platform workers back inside the employee category with a very different
 *   beneficiary/victim structure and a much lower or negated ε; the
 *   hybrid_security_reading proposes a third legal category with tailored,
 *   narrower protections. Each of those is a separate constraint story with
 *   its own ε, its own stakeholder structure, and its own classification —
 *   this file authors only the formalist reading, cleanly, per the
 *   ε-invariance principle.
 *
 * KEY AGENTS:
 *   - platform_operators: agenda_setter/beneficiary (institutional/arbitrage) — sets classification terms, captures the cost-externalization gain
 *   - platform_drivers_and_couriers: payer (powerless/constrained) — bears uninsured labor cost under algorithmic direction without employee protections
 *   - state_unemployment_insurance_systems: payer (institutional/trapped) — absorbs fiscal externality without corresponding contributions
 *   - traditional_employers_subject_to_full_costs: payer (organized/constrained) — competes at structural cost disadvantage
 *   - consumer_price_beneficiaries: beneficiary (moderate/mobile) — captures lower prices from the externalized cost
 *   - state_legislatures_and_courts: observer/agenda_setter (institutional/analytical) — controls which test governs, and thus which reading is legally entrenched
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, 0.79).
domain_priors:suppression_score(employment_boundary__formalist_employment_reading, 0.62).
domain_priors:theater_ratio(employment_boundary__formalist_employment_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__formalist_employment_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__formalist_employment_reading, "Formalist Employment Boundary (Contract-and-Supervision Test)").
narrative_ontology:topic_domain(employment_boundary__formalist_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__formalist_employment_reading, '5ba3cbb4-c567-4b4c-a10e-5dda4f4d3912').
narrative_ontology:cs_kernel_codification('5ba3cbb4-c567-4b4c-a10e-5dda4f4d3912', distributed).
narrative_ontology:cs_authority_grounding('5ba3cbb4-c567-4b4c-a10e-5dda4f4d3912', distributed).
narrative_ontology:cs_reading_relation('5ba3cbb4-c567-4b4c-a10e-5dda4f4d3912', employment_boundary__substantive_employment_reading, forecloses).
narrative_ontology:cs_reading_relation('5ba3cbb4-c567-4b4c-a10e-5dda4f4d3912', employment_boundary__hybrid_security_reading, influences).
narrative_ontology:cs_axiom('5ba3cbb4-c567-4b4c-a10e-5dda4f4d3912', foundational, contract_form_determines_status).
narrative_ontology:cs_axiom_status(contract_form_determines_status, holdable).
narrative_ontology:cs_axiom_grounding('5ba3cbb4-c567-4b4c-a10e-5dda4f4d3912', contract_form_determines_status, conventional).
narrative_ontology:cs_axiom('5ba3cbb4-c567-4b4c-a10e-5dda4f4d3912', secondary, flexibility_choice_justifies_exclusion_from_employee_protections).
narrative_ontology:cs_axiom_status(flexibility_choice_justifies_exclusion_from_employee_protections, holdable).
narrative_ontology:cs_axiom_grounding('5ba3cbb4-c567-4b4c-a10e-5dda4f4d3912', flexibility_choice_justifies_exclusion_from_employee_protections, instrumental).
narrative_ontology:cs_reference_frame('5ba3cbb4-c567-4b4c-a10e-5dda4f4d3912', industrial_era_direct_supervision_standard).
narrative_ontology:cs_drift_state('5ba3cbb4-c567-4b4c-a10e-5dda4f4d3912', platform_economy_algorithmic_dispatch_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5ba3cbb4-c567-4b4c-a10e-5dda4f4d3912', '').
narrative_ontology:cs_kernel_id(employment_boundary__formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_operators).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_shareholders).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, consumer_price_beneficiaries).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, platform_drivers_and_couriers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, state_unemployment_insurance_systems).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, traditional_employers_subject_to_full_costs).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, freedom_of_contract_doctrine).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, worker_choice_of_flexibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design the contract terms, the app-based dispatch and rating system, and the classification structure that places workers outside the legal employment relationship. Directs work in practice — assigning tasks, setting prices, deactivating accounts for performance — while the formal contract disclaims supervision. Avoids payroll tax, unemployment insurance contributions, workers' compensation premiums, minimum wage floors, and overtime liability by maintaining the independent-contractor classification, and lobbies legislatures and litigates aggressively to preserve it.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__formalist_employment_reading, platform_operators, beneficiary).

% Perform the work under algorithmic direction — assigned routes, monitored ratings, deactivation risk functioning as discipline — but hold no formal employment contract and no direct human supervisor, so they fall outside minimum wage floors, overtime, unemployment insurance, and employer-side payroll tax contributions. Bear the full cost of vehicle depreciation, fuel, insurance, and unpaid waiting time. Exit exists nominally (can log off any platform) but real alternatives across the local gig labor market offer the same terms, so exit does not escape the classification.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_drivers_and_couriers, payer,
    powerless, biographical, constrained, local).

% Absorb the fiscal externality when platform workers experience income loss or economic hardship: because they are classified as contractors, no employer-side unemployment insurance contributions were collected on their behalf, yet many still draw on general safety-net programs during downturns (as seen acutely during pandemic-era income collapse), leaving the state system paying out without having received the corresponding contribution stream.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, state_unemployment_insurance_systems, payer,
    institutional, generational, trapped, national).

% Compete in adjacent labor markets (taxi companies, courier firms, retail delivery) while paying full payroll tax, workers' compensation, unemployment insurance, and minimum wage/overtime costs for functionally similar labor. The formalist classification of their platform competitors as non-employers creates a structural cost disadvantage that some absorb through margin compression and others exit by converting their own workforce to contractor arrangements, spreading the classification further.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, traditional_employers_subject_to_full_costs, payer,
    organized, generational, constrained, national).

% Purchase rides, deliveries, and platform-mediated services at prices lower than they would be if the labor cost included the employer-side social insurance contributions that the formalist reading exempts platforms from paying. Benefit directly and are largely unaware of, or indifferent to, the classification question underlying the price.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, consumer_price_beneficiaries, beneficiary,
    moderate, immediate, mobile, national).

% Adjudicate and legislate the classification test itself — some jurisdictions codify the formal contract-and-supervision test into statute or case law (favoring platforms), others adopt an economic-realities or ABC test (favoring the substantive reading). Their choice of test is the mechanism by which this reading is either entrenched or displaced.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, state_legislatures_and_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(employment_boundary__formalist_employment_reading, state_legislatures_and_courts, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(employment_boundary__formalist_employment_reading, platform_operators).
narrative_ontology:fixing_cost_class(employment_boundary__formalist_employment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The formal contract-and-supervision test provides a bright-line, administrable rule that lets courts, regulators, and businesses determine employment status without case-by-case inquiry into economic dependence — a genuine coordination benefit for legal certainty and low adjudication cost.
% TRANSFER_FUNCTION: Moves the cost of social insurance (unemployment, workers' compensation, employer payroll tax contributions, minimum wage floors) from platform operators to workers themselves and, where workers subsequently draw on public safety nets, to state insurance systems and general taxpayers. Also transfers competitive cost advantage from platforms to consumers via lower prices, at the expense of traditional employers bearing full labor costs.
% ABSENT_VOICES: Platform workers as a class have limited organized voice in the legislative and judicial processes that set the classification test — collective bargaining is itself foreclosed by their non-employee status in most jurisdictions, a structural bind where the classification prevents the very organizing that could contest the classification. Displaced traditional-sector workers whose employers convert to contractor models to compete are rarely named parties in platform classification litigation.
% DISAPPEARANCE_RATIONALE: If the formalist reading were displaced overnight by the substantive reading, platform operators would face immediate reclassification liability: back pay for overtime and minimum wage shortfalls, retroactive unemployment insurance and payroll tax assessments, and a fundamentally different unit-economics model for platform labor. Traditional employers would regain a cost parity they currently lack. Prices to consumers would likely rise. The arrangement is load-bearing for the current platform business model, not incidental to it.
% FOUNDING_PROBLEM: The formal contract-and-supervision test predates the platform economy; it was built to distinguish genuine independent businesses (a plumber hired for a job, a subcontractor with their own crew and equipment) from employees, so that genuine small-business autonomy would not be swept into employer obligations meant for direct-supervision relationships.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and industry associations attest the founding problem remains live — that platform workers are genuinely autonomous entrepreneurs choosing flexible work, structurally identical to the independent contractor the test was built to protect. Labor economists, several state attorneys general, and international bodies (e.g. ILO working papers on platform labor, UK Supreme Court's Uber v Aslam reasoning) attest from outside the platform industry that algorithmic control functionally replicates direct supervision even absent a human supervisor, and that the test's founding assumptions no longer track the practice it is applied to — corroboration exists on both sides, which is itself part of why the reading remains contested rather than settled.
narrative_ontology:disappearance_verdict(employment_boundary__formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__formalist_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__formalist_employment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(employment_boundary__formalist_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__formalist_employment_reading, 0.79, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__formalist_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__formalist_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.79 at interval end) because the formalist test, once entrenched, systematically routes costs that a genuine employment relationship would require the platform to bear (payroll tax, insurance, wage floors) onto workers and the state instead — this is a real, measurable cost transfer, not a labeling dispute. Suppression is moderate-high (0.62) because the classification is defended through active litigation, lobbying for statutory codification (e.g., ballot initiatives establishing a contractor carve-out), and contract terms that foreclose collective bargaining, which itself forecloses the primary channel workers would use to contest the classification. Accessibility collapse is moderate (0.5): workers nominally retain formal legal avenues (misclassification suits, legislative advocacy) but as a practical matter the classification is difficult to individually litigate against an institutional counterparty. Resistance is substantial (0.58) reflecting ongoing worker organizing, misclassification litigation, and legislative contest in multiple jurisdictions — this is not an unresisted constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the platform operator's seat, this looks like a rope: a bright-line rule providing legal certainty that lets a large, decentralized, arm's-length labor market function without costly case-by-case adjudication of every worker's status — a real coordination benefit for administrability. From the driver/courier seat and the state insurance system's seat, the same rule computes as extraction: algorithmic control that functions as supervision in substance is formally disclaimed, and the costs that would attach to genuine employment are moved off the platform's books. The engine's per-seat computation should surface this divergence directly from the structural data (power, exit, beneficiary/victim declarations) rather than from any narrative reconciliation — that divergence is the specific empirical claim under contest between this reading and its siblings.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators sit at the clear beneficiary end: institutional power, arbitrage-grade exit (can relocate operations, restructure contracts, or exit jurisdictions that tighten the test), and direct capture of the cost-externalization gain. Platform drivers and couriers sit near the target end: powerless, constrained exit (alternative platforms offer structurally identical terms, so 'exit' from one platform is not exit from the classification), and they bear the uninsured cost directly. State unemployment insurance systems and traditional employers are institutional/organized payers whose 'victimhood' is structural and diffuse rather than individually experienced, which is why they are named as separate victim groups rather than folded into the worker group — the mechanism of harm differs (fiscal externality vs. competitive disadvantage) even though the source is the same classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing genuine independent businesses from disguised employment — was real and remains partially live (some platform work genuinely resembles autonomous contracting). But the classification's continued application to algorithmically-directed, single-app-dependent labor is contested precisely because the mechanism of control has changed since the test was designed: direct human supervision was the observable proxy for economic dependence in the test's founding era, and algorithmic dispatch, real-time performance monitoring, and deactivation risk may now perform the same substantive function without tripping the formal trigger. This is not treated here as settled fact — hence the contested founding_problem_status and the reading_relations to the substantive reading — but it is the reason this constraint is authored as tangled_rope (genuine administrability coordination bundled with asymmetric extraction under active enforcement) rather than as a pure snare or a pure rope: both the coordination function and the extraction are structurally real under this reading's own metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_control_as_supervision,
    'Does algorithmic dispatch, real-time performance rating, and deactivation risk constitute ''direct supervision'' in substance, such that the formal contract-and-supervision test is being satisfied by a different mechanism than the one the test''s drafters had in mind?',
    'Comparative case analysis across jurisdictions that have litigated this question (e.g. UK Supreme Court Uber v Aslam, California AB5/Prop 22 litigation, EU Platform Work Directive implementation) — track whether courts and regulators converge on treating algorithmic control as functionally equivalent to human supervision.',
    'If algorithmic control is judicially recognized as equivalent to direct supervision, the formalist reading''s own internal test would reclassify platform workers as employees without requiring adoption of the substantive reading at all — collapsing this constraint''s structural basis from within rather than by external displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_as_supervision, conceptual, 'Whether algorithmic control satisfies the formalist test''s own supervision prong.').

omega_variable(
    worker_choice_authenticity,
    'Do platform workers genuinely choose contractor status for flexibility, or is the ''choice'' constrained by the absence of alternative employment structures in the relevant local labor market — i.e., is the beneficiary declaration for ''worker choice'' real or a post-hoc justification for a take-it-or-leave-it contract?',
    'Survey and revealed-preference research on platform workers'' stated versus counterfactual employment preferences, and analysis of whether workers who prefer employee status have a realistic alternative within the same labor market.',
    'If choice is substantially constrained, the formalist reading''s coordination justification (workers value flexibility, the test protects their autonomous choice) weakens significantly, pushing the classification toward extraction-dominant rather than genuinely mixed coordination/extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_choice_authenticity, empirical, 'Whether declared worker preference for flexibility is a genuine choice or a constrained default.').

omega_variable(
    kernel_reading_displacement_trajectory,
    'Is the formalist reading''s dominance stable, or is it being actively displaced jurisdiction-by-jurisdiction by the substantive or hybrid readings — i.e., which reading of the employment_boundary kernel will hold the legally operative position in the medium term?',
    'Track legislative and judicial adoption rates of ABC-test statutes, economic-realities tests, and hybrid third-category schemes (e.g. Uber v Aslam in the UK, EU Platform Work Directive, various US state ballot initiatives) over a multi-year window.',
    'A trend toward substantive or hybrid reading adoption would mean this formalist constraint''s high ε reflects a reading in structural decline rather than a stable equilibrium — the extraction measured here may be a transitional rather than a persistent feature of the labor market.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_displacement_trajectory, empirical, 'Whether the formalist reading is gaining or losing ground against its sibling readings across jurisdictions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__formalist_employment_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t0, employment_boundary__formalist_employment_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(empl_tr_t0, observed).
narrative_ontology:measurement(empl_tr_t4, employment_boundary__formalist_employment_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement_basis(empl_tr_t4, observed).
narrative_ontology:measurement(empl_tr_t8, employment_boundary__formalist_employment_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(empl_tr_t8, observed).
narrative_ontology:measurement(empl_tr_t12, employment_boundary__formalist_employment_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement_basis(empl_tr_t12, observed).
narrative_ontology:measurement(empl_tr_t16, employment_boundary__formalist_employment_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement_basis(empl_tr_t16, observed).
narrative_ontology:measurement(empl_tr_t20, employment_boundary__formalist_employment_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement_basis(empl_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(empl_be_t0, employment_boundary__formalist_employment_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(empl_be_t0, observed).
narrative_ontology:measurement(empl_be_t4, employment_boundary__formalist_employment_reading, base_extractiveness, 4, 0.64).
narrative_ontology:measurement_basis(empl_be_t4, observed).
narrative_ontology:measurement(empl_be_t8, employment_boundary__formalist_employment_reading, base_extractiveness, 8, 0.69).
narrative_ontology:measurement_basis(empl_be_t8, observed).
narrative_ontology:measurement(empl_be_t12, employment_boundary__formalist_employment_reading, base_extractiveness, 12, 0.73).
narrative_ontology:measurement_basis(empl_be_t12, observed).
narrative_ontology:measurement(empl_be_t16, employment_boundary__formalist_employment_reading, base_extractiveness, 16, 0.76).
narrative_ontology:measurement_basis(empl_be_t16, observed).
narrative_ontology:measurement(empl_be_t20, employment_boundary__formalist_employment_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement_basis(empl_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t0, employment_boundary__formalist_employment_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(empl_su_t0, observed).
narrative_ontology:measurement(empl_su_t4, employment_boundary__formalist_employment_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement_basis(empl_su_t4, observed).
narrative_ontology:measurement(empl_su_t8, employment_boundary__formalist_employment_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement_basis(empl_su_t8, observed).
narrative_ontology:measurement(empl_su_t12, employment_boundary__formalist_employment_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement_basis(empl_su_t12, observed).
narrative_ontology:measurement(empl_su_t16, employment_boundary__formalist_employment_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement_basis(empl_su_t16, observed).
narrative_ontology:measurement(empl_su_t20, employment_boundary__formalist_employment_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(empl_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__formalist_employment_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(employment_boundary__formalist_employment_reading, 0.1).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, employment_boundary__substantive_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, employment_boundary__hybrid_security_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language 'gig worker classification' dispute per the ε-invariance principle. formalist_employment_reading (this file): ε=0.79, tangled_rope, platform workers excluded from the precarity victim set. substantive_employment_reading (sibling): defines employment by economic dependence and algorithmic control, includes platform workers as employees, and would carry a substantially different — likely much lower for platforms-as-employers-obligated, but differently structured — ε and beneficiary/victim set. hybrid_security_reading (sibling): a third-category constraint with its own tailored protections, its own ε, and a partial-coordination structure distinct from both. All three share the same underlying kernel (employment_boundary) but are NOT the same constraint — they are linked here via affects_constraints, not merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
