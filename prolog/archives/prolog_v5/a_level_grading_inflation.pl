% ============================================================================
% CONSTRAINT STORY: a_level_grading_inflation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_a_level_grading_inflation, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: a_level_grading_inflation
 *   human_readable: A-Level Grade Inflation and Credential Devaluation
 *   domain: education/credentialing/labor_market
 *
 * SUMMARY:
 *   A-level grade inflation in England represents a structural constraint
 *   where the institutional incentives for schools to present high grades
 *   (funding mechanisms, league table positioning, parental choice) are
 *   decoupled from the epistemic function of grades (signaling student
 *   achievement reliably to universities and employers). Over the past 20+
 *   years, the proportion of students achieving A and A* grades has risen
 *   from ~15% to ~30%, while international benchmarks (PISA, TIMSS) show no
 *   corresponding increase in underlying competence. This creates an
 *   extractive dynamic: schools benefit from headline grades through funding
 *   and reputation, while universities, employers, and students from prior
 *   cohorts bear the cost of credential devaluation. The constraint exhibits
 *   all six DR types depending on perspective, making it a diagnostic
 *   exemplar for institutional credentialism and identity-based lock-in.
 *   Teachers are identity-locked (their professional identity fused with
 *   grade inflation under institutional pressure), schools experience
 *   coordination gains (solving the enrollment problem), universities face a
 *   coordination cost (interpreting inflated grades), and the labor market
 *   faces a snare (trapped in a degraded signal system with no exit).
 *
 * KEY AGENTS:
 *   - Schools tied to results-based funding: Primary beneficiary (institutional/arbitrage) — capture funding and league table benefits from rising headline grades
 *   - Labor market signal reliability: Primary victim (powerless/trapped) — abstract epistemic commons bearing full cost of credential deterioration
 *   - University admissions offices: Secondary victim (moderate/constrained) — forced to develop context-dependent interpretation of grades and supplementary assessment
 *   - Teachers and subject specialists: Secondary victim (moderate/identity_locked) — identity fused with institutional pressure to inflate; structurally mobile but cognitively trapped
 *   - Standards authorities (Ofqual): Organized reformer (organized/constrained) — attempting sunset intervention through grade boundary recalibration and moderation
 *   - Students in prior cohorts: Indirect victim (moderate/trapped) — their credentials depreciate as grades inflate; cannot exit the labor market timing decision
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(a_level_grading_inflation, 0.58).
domain_priors:suppression_score(a_level_grading_inflation, 0.62).
domain_priors:theater_ratio(a_level_grading_inflation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(a_level_grading_inflation, extractiveness, 0.58).
narrative_ontology:constraint_metric(a_level_grading_inflation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(a_level_grading_inflation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(a_level_grading_inflation, tangled_rope).
narrative_ontology:human_readable(a_level_grading_inflation, "A-Level Grade Inflation and Credential Devaluation").
narrative_ontology:topic_domain(a_level_grading_inflation, "education/credentialing/labor_market").

domain_priors:requires_active_enforcement(a_level_grading_inflation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(a_level_grading_inflation, schools_funding_tied_to_results).
narrative_ontology:constraint_beneficiary(a_level_grading_inflation, students_in_inflated_cohorts).
narrative_ontology:constraint_victim(a_level_grading_inflation, labor_market_signal_reliability).
narrative_ontology:constraint_victim(a_level_grading_inflation, universities_admissions).
narrative_ontology:constraint_victim(a_level_grading_inflation, students_in_prior_cohorts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LABOR MARKET SIGNAL RELIABILITY (SNARE) — The epistemic commons of educational credentialing is trapped in grade inflation with no exit. As A grades become commonplace, the signal value of an A grade asymptotically approaches that of a B grade. Employers and universities cannot escape the credential deterioration; they must either accept inflated grades as currency or rebuild alternative assessment mechanisms from scratch. The crisis bears concentrated costs on those who relied on prior grade distributions (students from 10-15 years ago, employers hiring based on historical credential interpretation). Maximum extraction from an abstract collective that cannot organize.
constraint_indexing:constraint_classification(a_level_grading_inflation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNIVERSITY ADMISSIONS (TANGLED ROPE) — Universities face both coordination and extraction. The coordination function is genuine: grades enable screening of applicants at scale when direct assessment is infeasible. The extraction is asymmetric: as grade inflation accelerates, universities must add context-dependency (knowing which schools inflate and which don't) and alternative tests (SATs, entrance exams) to maintain signal. This labor is extracted from admissions offices without payment — they bear the cost of interpreting grade inflation while schools that cause it capture the benefit of rising headline grades. High suppression (constrained exit) because universities cannot simply refuse to process A grades without losing applicants to universities that will accept them.
constraint_indexing:constraint_classification(a_level_grading_inflation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SCHOOLS FUNDED BY RESULTS (ROPE) — Schools experience the constraint as pure coordination: publishing high grades attracts students, parents, and (in UK context) triggers funding mechanisms tied to league tables and Ofsted ratings. The mechanism is coordination because schools are solving a genuine enrollment problem — communicating student achievement. But the constraint is enforced by institutional pressure (performance metrics) and peer competition (league table positioning). From the school's perspective, this is coordination: we need to signal quality to survive. The institutional arbitrage position means they can exit — a school can choose not to inflate grades if it shifts strategy to value-added metrics or specialist positioning — but few do because the current system rewards headline grades.
constraint_indexing:constraint_classification(a_level_grading_inflation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STANDARDS AUTHORITY REFORM (SCAFFOLD) — Organized actors (Ofqual, education researchers, university sector bodies) see grade inflation as a temporary coordination failure with potential sunset. Interventions include: grade boundaries recalibration, reduction of resit opportunities, sample moderation of assessment, and experimental value-added metrics. These interventions are sunset-clause mechanisms — they aim to restore signal reliability within a defined timeframe (typically 5-10 years for norm-shifting). However, the constraint persists partly because the coalition's authority is distributed across multiple institutions with conflicting incentives. Theater ratio is high because much of the reform activity (consultation processes, statistical reanalysis, communications campaigns) is performative — the underlying incentive structure (league tables rewarding headline grades) remains in place.
constraint_indexing:constraint_classification(a_level_grading_inflation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NORM-REFERENCED ASSESSMENT (PITON) — From a long-term view, the norm-referenced grading system (where grades reflect position in a cohort distribution) has become largely theatrical. The system assumes stable difficulty and consistent cohort ability; both assumptions broke 20+ years ago. Grade inflation is the symptom — the underlying cause is that norm-referenced assessment only works if standards and cohort ability remain fixed, but political pressure, curriculum changes, and teaching-to-the-test dynamics shifted both. The system persists through institutional inertia: exam boards continue producing grades because the infrastructure exists and alternatives (criterion-referenced or competency-based assessment) would require wholesale reorganization. Theater ratio is high because the grading ritual maintains appearance of comparison when the underlying comparability has eroded.
constraint_indexing:constraint_classification(a_level_grading_inflation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: TEACHERS AND SUBJECT SPECIALISTS (TANGLED ROPE, IDENTITY_LOCKED) — Teachers experience the constraint as mixed coordination and coercion. The coordination function: assessment enables feedback and progression decisions. The extraction: performance metrics and school accountability systems create pressure to inflate grades. The identity lock is critical here — teachers' professional identity is constituted partly through their role as assessors. Many cannot imagine themselves as 'the teacher who gives low grades' in the context of institutional pressure; their identity as competent educator is fused with supporting student achievement. This identity fusion prevents them from exiting the inflation dynamic even when they intellectually recognize it as harmful. The psychological mechanism (identity lock) combines with structural mechanism (performance metrics tied to school funding) to produce suppression. High theater: much of the 'assessment' activity becomes optimizing for grades rather than measuring learning.
constraint_indexing:constraint_classification(a_level_grading_inflation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some credential drift is inherent to any mass credentialing system: as educational access broadens (more students complete A-levels), statistical distribution of ability widens, creating pressure to maintain grades or face claims of systemic failure. This perspective sees grade inflation as an immutable law of credentialing systems under expansion pressure. However, the structural data contradicts the mountain classification: the constraint is enforced through institutional mechanisms (league tables, funding formulas, reputational competition) and perceptual mechanisms (identity lock), not through mathematical or logical necessity. The analytical frame risks naturalizing what is actually a contingent institutional arrangement.
constraint_indexing:constraint_classification(a_level_grading_inflation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(a_level_grading_inflation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(a_level_grading_inflation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(a_level_grading_inflation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(a_level_grading_inflation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(a_level_grading_inflation, TR),
    TR >= 0.70.

:- end_tests(a_level_grading_inflation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Schools and inflated-cohort students extract benefit through credential premium during the 2-4 year verification window (university admissions, early career hiring). The extraction is sustained through institutional reinforcement (league tables reward headline grades) and perceptual reinforcement (both schools and families have internalized the grade distribution as normal). Over the 16-year measurement interval, extractiveness has nearly tripled (0.22 → 0.58), indicating structural accumulation rather than transient effect. Suppression (0.62): High. Multiple mechanisms: (1) Structural — schools that don't inflate face competitive disadvantage; students who receive lower grades face real opportunity costs. (2) Perceptual — teachers' identity lock prevents them from exiting the inflation dynamic even when they recognize harm. (3) Systemic — universities cannot simply reject inflated grades without losing applicants to competitors; labor market cannot instantly recalibrate expectations. Theater ratio (0.68): High and rising. The 'assessment' ritual increasingly decouples from actual learning measurement. Time spent on grade-optimizing behavior (focusing curriculum on examinable content, teaching to test formats, resit strategies) rises faster than time spent on knowledge acquisition. The theater component (performative compliance with assessment procedures) has grown from 0.35 to 0.68 over the interval, indicating Goodhart drift — the measurement (grades) has become the target, replacing the original goal (reliable signal of competence).
 *
 * PERSPECTIVAL GAP:
 *   The constraint reveals dramatic perspectival divergence. Schools and inflated-cohort students see Rope — pure coordination of enrollment and funding signals. Reform authorities see Scaffold — temporary misalignment being corrected through recalibration (sunset clause). Teachers see Tangled Rope with identity lock — both coordination (legitimate assessment function) and coercion (institutional pressure to inflate). Universities see Tangled Rope — coordination mixed with asymmetric cost. Prior students and the labor market see Snare — trapped in credential deterioration with no exit. The analytical observer risks seeing Mountain — credentialism is inherent to mass education systems — but the structural data reveals this as naturalization of contingent institutional arrangements (league tables, funding mechanisms, identity-based professional pressure).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (schools, current students) occupy positions of institutional power with arbitrage exit options — they can theoretically exit by adopting alternative positioning strategies (value-added marketing, specialist niches) but few do because the current system rewards headline grades. The derived d values for beneficiaries are low (~0.15-0.25), producing negative or minimal χ — they experience the constraint as beneficial coordination. Victims (universities, labor market, prior students, teachers) occupy positions with constrained or trapped exit options. Universities face a coordination problem (they must screen applicants) combined with extraction (they bear the interpretive labor without control over grade supply). The derived d for universities is high (~0.65), producing high χ. Teachers face identity lock combined with institutional suppression, producing a d in the 0.60-0.75 range depending on whether the primary binding mechanism is identity (internal) or incentive (external). Prior students face pure trap (they cannot re-take exams to capture the new grade distribution), producing maximum d (~0.95). The signal reliability crisis is abstract (no agent can directly exit) and faces universal d (~0.80+, maximum experienced extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing that all six types are legitimate readings of the same phenomenon. The mandatrophy ('what is the true type?') dissolves when we recognize that the constraint operates differently for different agents: schools genuinely coordinate; universities genuinely face mixed coordination and extraction; teachers face mixed function and coercion with identity lock preventing exit; the abstract signal system faces pure trap. The false summit risk is high (the analytical observer naturalizing credential inflation as inherent to education) — the engine's false summit detector should flag the mountain perspective as over-generalization. The resolution lies in recognizing that the institutional incentive structure (league tables, funding mechanisms) is changeable, not natural law. The chair of Ofqual, school leaders, and teachers could collectively choose alternative accountability mechanisms; they choose not to because individual institutions face competitive pressure. This is a snare for the system-as-a-whole (everyone trapped together in a coordination game where mutual defection is rational) even though individual perspectives show different types.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cohort_ability_stability,
    'Has the underlying ability distribution of A-level candidates remained stable, or has compositional change (increasing access, changing demographics) genuinely shifted the distribution upward?',
    'Cross-cohort analysis using international benchmarks (PISA, TIMSS), value-added measures controlling for prior attainment, and stable-population comparison groups (private vs state sector with matched intake)',
    'If ability genuinely increased: some grade inflation is legitimate adjustment, extractiveness falls to 0.35-0.40, reclassifies toward Rope. If ability stable or declined: inflation is pure rent-seeking, extractiveness rises to 0.65+, reclassifies toward Snare from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohort_ability_stability, empirical, 'Whether grade inflation reflects genuine ability increase or pure credential drift').

omega_variable(
    standards_definition_ambiguity,
    'What does an A grade represent: absolute mastery of defined content, position in the cohort distribution, or demonstrated readiness for higher education?',
    'Comparative analysis of criterion-referenced vs norm-referenced grading outcomes; international comparison with fixed-standard systems (IB, A*-G scale); employer validation of skill expectations',
    'If criterion-referenced: grade inflation indicates lowered standards and extractiveness is clearly high (0.60+). If norm-referenced: inflation is the system working as designed (adjusting to cohort changes) and extractiveness falls to 0.35-0.45. The ambiguity obscures whether inflation is extraction or legitimate adaptation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(standards_definition_ambiguity, conceptual, 'Ambiguity in what A grades measure: absolute standards vs relative position vs readiness').

omega_variable(
    identity_lock_persistence,
    'Would teachers and subject specialists continue inflating grades if institutional incentives (league tables, funding metrics) were removed, or is the identity lock sufficient to sustain inflation independently?',
    'Analysis of schools with alternative accountability models (independent schools, schools in reformed systems); longitudinal tracking of grade patterns after removal of league table pressure; qualitative research on teacher motivation',
    'If identity lock is dominant: inflation persists even without institutional pressure, suppression is intrinsic and extractiveness remains high (0.55+). If institutional incentives are dominant: removing metrics would reduce inflation, and the identity_locked exit option is misclassified (should be constrained).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether grade inflation is sustained by teacher identity lock or institutional incentives').

omega_variable(
    signal_recovery_mechanism,
    'Can universities and employers calibrate to grade inflation through statistical learning (learning new grade distributions), or does inflation outpace recalibration and produce persistent signal degradation?',
    'Analysis of employer hiring patterns and university admissions selectivity over time; comparison of schools'' reputation trajectories with their grade inflation trajectories; measurement of whether grade-independent screening (entrance exams, portfolio assessment) reduces over time as institutions learn new distributions',
    'If recalibration is possible: signal reliability recovers endogenously and extractiveness falls over time (trajectory toward 0.30-0.40). If inflation outpaces recalibration: signal degradation is permanent (extractiveness stable at 0.55+) and the scaffold perspective is optimistic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signal_recovery_mechanism, empirical, 'Whether downstream actors can recalibrate to inflated grade distributions or face permanent signal loss').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(a_level_grading_inflation, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alev_tr_t0, a_level_grading_inflation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(alev_tr_t8, a_level_grading_inflation, theater_ratio, 8, 0.51).
narrative_ontology:measurement(alev_tr_t16, a_level_grading_inflation, theater_ratio, 16, 0.68).
narrative_ontology:measurement(alev_tr_t4, a_level_grading_inflation, theater_ratio, 4, 0.43).
narrative_ontology:measurement(alev_tr_t12, a_level_grading_inflation, theater_ratio, 12, 0.6).

% Extraction over time
narrative_ontology:measurement(alev_be_t0, a_level_grading_inflation, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(alev_be_t8, a_level_grading_inflation, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(alev_be_t16, a_level_grading_inflation, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(alev_be_t4, a_level_grading_inflation, base_extractiveness, 4, 0.3).
narrative_ontology:measurement(alev_be_t12, a_level_grading_inflation, base_extractiveness, 12, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(a_level_grading_inflation, identity_coordination).
narrative_ontology:boltzmann_floor_override(a_level_grading_inflation, 0.12).
narrative_ontology:affects_constraint(a_level_grading_inflation, university_admissions_information_asymmetry).
narrative_ontology:affects_constraint(a_level_grading_inflation, labor_market_credential_signaling).
narrative_ontology:affects_constraint(a_level_grading_inflation, educational_opportunity_concentration).

% DUAL FORMULATION NOTE:
% Grade inflation is structurally distinct from but causally upstream of university admissions challenges and labor market signaling failures. Universities adapt to grade inflation through supplementary assessments (increasing coordination costs downstream); labor markets adapt through credential saturation and return-to-education volatility. These downstream constraints are affected by grade inflation's trajectory but have independent structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(a_level_grading_inflation, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
