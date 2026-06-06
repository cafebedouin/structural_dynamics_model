% ============================================================================
% CONSTRAINT STORY: administrative_burden_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_administrative_burden_extraction, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: administrative_burden_extraction
 *   human_readable: Administrative Burden Extraction in Healthcare Delivery
 *   domain: health_workforce_economics/organizational_behavior/gender_labor
 *
 * SUMMARY:
 *   The administrative burden in healthcare delivery has grown substantially
 *   over the past two decades, driven by EHR implementation, insurance prior
 *   authorization requirements, quality metric reporting, and regulatory
 *   compliance documentation. What began as a coordination mechanism
 *   (digitizing records for safety and portability) has accumulated
 *   extractive layers that now consume 2-3 hours of physician labor per
 *   clinical hour worked. The constraint exhibits classic tangled rope
 *   structure: genuine coordination needs (care coordination across
 *   providers, medication safety checks, billing accuracy) are real, but the
 *   current implementation extracts far more physician labor than the
 *   coordination function requires. The theater ratio (0.48) reflects that
 *   nearly half of administrative tasks are performative rather than
 *   functional — prior authorizations that are routinely approved after
 *   appeal, inbox messages that could be handled by non-clinical staff,
 *   documentation requirements that satisfy billing codes rather than
 *   clinical needs. The suppression trajectory shows enforcement
 *   intensification: as physicians have pushed back against administrative
 *   burden, health systems have responded by making the requirements more
 *   rigid and comprehensive, not by reducing them.
 *
 * KEY AGENTS:
 *   - Practicing Physicians: Primary victims (powerless to moderate / trapped to constrained) — bear uncompensated labor extraction; exit options vary by specialty, career stage, and debt burden
 *   - Patient Care Quality: Secondary victim (powerless/trapped) — abstract collective good that suffers when physician time is diverted from clinical care to administrative tasks
 *   - Health System Administrators: Primary beneficiaries (institutional/arbitrage) — extract physician labor for billing optimization, quality metric achievement, and regulatory compliance without hiring additional staff
 *   - Insurance Companies: Primary beneficiaries (institutional/arbitrage) — use prior authorization theater to delay and deny care, extracting physician labor for appeals while claiming utilization management function
 *   - EHR Vendors: Secondary beneficiaries (institutional/arbitrage) — profit from degraded state through upgrade cycles and add-on modules; original coordination function has atrophied
 *   - Physician Union Organizers: Organized resistance (organized/mobile) — building collective bargaining power to demand protected administrative time and contract limits on burden
 *   - Insurance Company Medical Directors: Captured institutional actors (institutional/constrained) — former physicians now administering the extraction mechanism; identity-locked into insurance industry framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(administrative_burden_extraction, 0.58).
domain_priors:suppression_score(administrative_burden_extraction, 0.67).
domain_priors:theater_ratio(administrative_burden_extraction, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(administrative_burden_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(administrative_burden_extraction, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(administrative_burden_extraction, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(administrative_burden_extraction, tangled_rope).
narrative_ontology:human_readable(administrative_burden_extraction, "Administrative Burden Extraction in Healthcare Delivery").
narrative_ontology:topic_domain(administrative_burden_extraction, "health_workforce_economics/organizational_behavior/gender_labor").

domain_priors:requires_active_enforcement(administrative_burden_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(administrative_burden_extraction, health_system_administrators).
narrative_ontology:constraint_beneficiary(administrative_burden_extraction, insurance_companies).
narrative_ontology:constraint_beneficiary(administrative_burden_extraction, ehr_vendors).
narrative_ontology:constraint_victim(administrative_burden_extraction, practicing_physicians).
narrative_ontology:constraint_victim(administrative_burden_extraction, patient_care_quality).
narrative_ontology:constraint_vindicates(administrative_burden_extraction, documentation_equals_quality_doctrine).
narrative_ontology:constraint_vindicates(administrative_burden_extraction, administrative_efficiency_hypothesis).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED PRIMARY CARE PHYSICIAN (SNARE) — Cannot exit without abandoning career identity and decade+ of training investment. Faces 2-3 hours of inbox work per clinical hour, prior authorization denials requiring unpaid appeals labor, and EHR documentation requirements that have tripled since 2010. The coordination story (better documentation improves care) is cover — the actual function is extraction of uncompensated labor to satisfy billing requirements and administrative metrics. Maximum experienced extraction.
constraint_indexing:constraint_classification(administrative_burden_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-CAREER SPECIALIST (TANGLED ROPE) — Constrained by practice overhead, malpractice tail coverage, and family obligations, but has some negotiating power and can shift to concierge models or reduce panel size. Experiences genuine coordination benefit (EHR enables care coordination across specialists) alongside substantial extraction (prior authorization theater, inbox overload). The constraint both enables and extracts — classic tangled rope structure.
constraint_indexing:constraint_classification(administrative_burden_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HEALTH SYSTEM ADMINISTRATOR (ROPE) — Benefits from physician labor extraction through billing optimization, quality metric achievement, and regulatory compliance without hiring additional staff. Experiences the constraint as coordination: documentation requirements enable revenue capture and risk management. Net beneficiary — the administrative burden runs toward this agent's institutional goals, not away from them.
constraint_indexing:constraint_classification(administrative_burden_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INSURANCE COMPANY MEDICAL DIRECTOR (TANGLED ROPE) — Former physician now administering prior authorization systems. Benefits from cost containment through administrative friction but also bears the coordination cost of managing appeals and peer-to-peer reviews. Identity-locked into the insurance industry's framing of utilization management as quality control. Experiences both coordination function (preventing inappropriate utilization) and extraction mechanism (administrative theater that delays rather than prevents care).
constraint_indexing:constraint_classification(administrative_burden_extraction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PHYSICIAN UNION ORGANIZER (SCAFFOLD) — Organized labor seeing the administrative burden as a temporary coordination failure with a sunset: collective bargaining for protected administrative time, contract language limiting inbox volume, and legislative advocacy for prior authorization reform. The constraint is transitional — its justification is the transition to a system where administrative labor is either compensated or eliminated, not the steady state of uncompensated extraction.
constraint_indexing:constraint_classification(administrative_burden_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: EHR VENDOR (PITON) — The original coordination function (digitizing medical records for portability and safety) has atrophied into a billing optimization and regulatory compliance tool. The system persists through institutional inertia and switching costs, not because it improves clinical workflow. Vendors benefit from the degraded state — each regulatory change requires expensive upgrades, and physician dissatisfaction creates demand for add-on modules that promise (but rarely deliver) efficiency gains.
constraint_indexing:constraint_classification(administrative_burden_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, some administrative coordination is necessary for complex healthcare delivery (care coordination, safety checks, billing accuracy). But the current implementation extracts far more physician labor than the coordination function requires. The analytical view sees both the genuine coordination need (not a snare) and the substantial extraction layered on top (not a rope). Classic tangled rope: the coordination story is real but insufficient to explain the constraint's actual operation.
constraint_indexing:constraint_classification(administrative_burden_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(administrative_burden_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(administrative_burden_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(administrative_burden_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(administrative_burden_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(administrative_burden_extraction, TR),
    TR >= 0.70.

:- end_tests(administrative_burden_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. Physicians spend 2-3 hours on administrative tasks per clinical hour, with much of this labor uncompensated (salaried physicians) or poorly compensated (fee-for-service physicians whose administrative time is not billable). The extraction has increased steadily from 0.32 in 2000 to 0.58 in 2020 as EHR requirements, prior authorization complexity, and quality reporting have expanded. The value reflects that while some administrative coordination is necessary, the current burden far exceeds coordination requirements — time-motion studies suggest 40-50% of current tasks represent genuine coordination, meaning 50-60% is extractive overhead. Suppression (0.67): High. Exit barriers include decade+ training investment, medical school debt averaging $200k+, state licensing requirements, malpractice tail coverage, and professional identity fusion. Primary care physicians face the highest suppression (fewer alternative career paths, lower income to absorb debt). The suppression has intensified over the interval as health systems have made administrative requirements more rigid in response to physician resistance. Theater ratio (0.48): Moderate-high. Prior authorization systems routinely approve 50-70% of initially denied requests on appeal, revealing the denial as administrative friction rather than utilization management. Inbox messages include many tasks that could be handled by non-clinical staff but are routed to physicians due to liability concerns and cost-shifting. EHR documentation requirements are optimized for billing capture rather than clinical utility. The theater has increased as regulatory complexity has grown faster than functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural phenomenon appears radically different depending on the observer's position in the extraction flow. Health system administrators see coordination (Rope) — documentation requirements enable revenue capture and quality metrics. Insurance companies see utilization management (Rope with coordination story). Trapped primary care physicians see pure extraction (Snare) — uncompensated labor with no exit. Mid-career specialists see mixed coordination and extraction (Tangled Rope) — EHR enables care coordination but inbox overload extracts time. EHR vendors see a degraded system maintained through inertia (Piton) — the original coordination function has atrophied but switching costs prevent replacement. Physician union organizers see a temporary problem with a sunset (Scaffold) — collective bargaining and legislative reform will either compensate the labor or eliminate the burden. The analytical observer sees the genuine coordination need layered with substantial extraction (Tangled Rope) — not a snare (coordination is real) but not a rope (extraction exceeds coordination requirements). The perspectival gap reveals that 'administrative burden' is not a neutral description but a contested site where coordination and extraction are structurally entangled.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position and beneficiary/victim declarations. Health system administrators and insurance companies are primary beneficiaries with arbitrage exit options — they experience low or negative effective extraction (the constraint subsidizes their institutional goals). Practicing physicians are primary victims with exit options ranging from trapped (primary care, high debt, mid-career) to constrained (specialists, established practices, lower debt). The engine derives high d values for physician perspectives, amplifying their experienced extraction. Insurance company medical directors occupy an unusual position: institutional power with constrained exit (identity-locked into insurance industry) and mixed beneficiary/victim status (benefit from cost containment, bear coordination costs of managing appeals). The physician union organizer perspective has mobile exit options and organized power, producing lower experienced extraction — they see the constraint as changeable through collective action. The analytical observer sees both the genuine coordination function and the substantial extraction, producing a tangled rope classification with moderate effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that tangled rope classification requires BOTH genuine coordination function AND asymmetric extraction operating through the same mechanism. The administrative burden is not pure extraction (snare) because care coordination, medication safety checks, and billing accuracy are real coordination needs. It is not pure coordination (rope) because the current implementation extracts far more physician labor than these functions require — prior authorization theater, inbox overload, and billing-optimized documentation are extraction mechanisms layered onto coordination infrastructure. The mandate (coordinate complex healthcare delivery) has not outlived its function, but the implementation has accumulated extractive overhead that now dominates the constraint's operation. The scaffold perspective (physician unions) represents organized resistance building toward a sunset: either the administrative labor will be compensated as the professional work it is, or it will be eliminated through automation and task-shifting to appropriate staff. The piton perspective (EHR vendors) shows how the coordination infrastructure itself has degraded — the original function (digitize records for safety and portability) has atrophied into a billing optimization tool maintained through switching costs rather than clinical utility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_threshold,
    'What proportion of current administrative tasks represent genuine coordination requirements versus extractive overhead?',
    'Time-motion studies comparing administrative burden across healthcare systems with different regulatory and payment structures; identification of tasks that correlate with quality outcomes versus those that correlate only with billing capture',
    'If >70% is genuine coordination: constraint is closer to rope with high inherent cost. If <30% is genuine coordination: constraint is closer to snare with coordination cover story. Current evidence suggests 40-50% range, supporting tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_threshold, empirical, 'Proportion of administrative burden representing genuine coordination versus extraction').

omega_variable(
    gender_differential_impact,
    'Does the administrative burden extraction disproportionately affect women physicians through gendered expectations of responsiveness and emotional labor?',
    'Stratified analysis of inbox message volume, response time expectations, and patient requests by physician gender; qualitative analysis of how administrative burden interacts with gendered professional norms',
    'If differential exists: the constraint has an additional extraction mechanism operating through gender norms, amplifying effective extraction for women physicians. This would increase the victim group''s internal heterogeneity and potentially shift some women physicians from ''constrained'' to ''trapped'' exit options.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_differential_impact, empirical, 'Whether administrative burden extraction operates differentially by physician gender').

omega_variable(
    burnout_causality_direction,
    'Is administrative burden a primary cause of physician burnout, or is burnout a convenient framing that naturalizes what is actually a labor extraction mechanism?',
    'Longitudinal studies tracking administrative burden and burnout measures; comparison of burnout rates in systems with different administrative structures; analysis of whether ''burnout'' discourse shifts focus from institutional extraction to individual resilience',
    'If burnout is primarily caused by administrative burden: interventions targeting burden reduction should reduce burnout. If burnout framing naturalizes extraction: the discourse itself becomes part of the constraint''s suppression mechanism, preventing collective action by individualizing a structural problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burnout_causality_direction, conceptual, 'Whether burnout discourse naturalizes labor extraction as individual pathology').

omega_variable(
    prior_authorization_theater_ratio,
    'What proportion of prior authorization denials are overturned on appeal, and does this proportion reveal the system as primarily theater versus genuine utilization management?',
    'Analysis of prior authorization denial and appeal rates across insurers and procedure types; correlation between denial rates and actual inappropriate utilization versus revenue impact',
    'If >60% of denials are overturned on appeal: prior authorization is primarily extractive theater (administrative friction that delays care without preventing inappropriate utilization). If <30% overturn rate: system has genuine utilization management function. Current evidence suggests 50-70% overturn rates for many procedure categories, indicating substantial theater component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prior_authorization_theater_ratio, empirical, 'Proportion of prior authorization denials overturned on appeal as measure of theater versus function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(administrative_burden_extraction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(admin_burden_theater_2000, administrative_burden_extraction, theater_ratio, 0, 0.28).
narrative_ontology:measurement(admin_burden_theater_2005, administrative_burden_extraction, theater_ratio, 5, 0.35).
narrative_ontology:measurement(admin_burden_theater_2010, administrative_burden_extraction, theater_ratio, 10, 0.42).
narrative_ontology:measurement(admin_burden_theater_2015, administrative_burden_extraction, theater_ratio, 15, 0.46).
narrative_ontology:measurement(admin_burden_theater_2020, administrative_burden_extraction, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(admin_burden_extract_2000, administrative_burden_extraction, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(admin_burden_extract_2005, administrative_burden_extraction, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(admin_burden_extract_2010, administrative_burden_extraction, base_extractiveness, 10, 0.49).
narrative_ontology:measurement(admin_burden_extract_2015, administrative_burden_extraction, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(admin_burden_extract_2020, administrative_burden_extraction, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(admin_burden_suppress_2000, administrative_burden_extraction, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(admin_burden_suppress_2010, administrative_burden_extraction, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(admin_burden_suppress_2020, administrative_burden_extraction, suppression_requirement, 20, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(administrative_burden_extraction, resource_allocation).
narrative_ontology:affects_constraint(administrative_burden_extraction, physician_burnout_epidemic).
narrative_ontology:affects_constraint(administrative_burden_extraction, primary_care_shortage).
narrative_ontology:affects_constraint(administrative_burden_extraction, gender_wage_gap_medicine).

% DUAL FORMULATION NOTE:
% The administrative burden constraint is upstream of physician burnout (burnout is partly caused by uncompensated administrative labor extraction) and primary care shortage (administrative burden is highest in primary care, driving specialty selection). It is also structurally linked to gender wage gaps in medicine if women physicians face higher administrative burden through gendered expectations of responsiveness and emotional labor (omega variable gender_differential_impact). These are separate constraints with their own extractiveness values, but they share structural dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(administrative_burden_extraction, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
