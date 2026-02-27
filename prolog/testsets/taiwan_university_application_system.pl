% ============================================================================
% CONSTRAINT STORY: taiwan_university_application_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taiwan_university_application_system, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: taiwan_university_application_system
 *   human_readable: Taiwan's Application-Based University Admission System
 *   domain: social/economic
 *
 * SUMMARY:
 *   Taiwan's Application-Based Admission (個人申請) system was introduced as a
 *   reform to reduce the stranglehold of standardized testing on university
 *   entrance and provide a more holistic assessment of applicants. The system
 *   permits students to apply directly to universities with portfolios,
 *   essays, and interview records, bypassing or supplementing the Joint
 *   College Entrance Exam (JCEE). However, the constraint has bifurcated
 *   along resource lines. The rhetoric of 'holistic evaluation' created a
 *   market for application coaching, essay services, and narrative
 *   positioning expertise. Well-resourced students gained a new avenue to
 *   signal capability; poorly resourced students face a new barrier requiring
 *   cash and cultural capital to navigate. The system exhibits all six
 *   classification types depending on structural position: for rural students
 *   it is a pure extraction mechanism (Snare); for prep coaches it is pure
 *   coordination benefit (Rope); for admissions committees it is mixed
 *   (Tangled Rope); for the testing industry it is a degraded ritual they
 *   maintain through inertia (Piton); for students with family mentors it is
 *   genuine coordination (Rope); for the field as a whole, the epistemic
 *   problem of selecting for unobservable qualities is irreducible (Mountain,
 *   though naturalization fallacy applies). The theater ratio (0.64) reflects
 *   that universities still rely heavily on test scores to filter applicants
 *   before reviewing essays — the holistic review is largely performative for
 *   shortlisted cohorts, not for initial selection.
 *
 * KEY AGENTS:
 *   - Rural Students / First-Generation Applicants: Primary victims (powerless/trapped) — cannot access coaching market; lack social capital to interpret expectations; face new resource barrier created by 'holistic' system
 *   - Elite Prep Industry / Educational Consultants: Primary beneficiaries (institutional/arbitrage) — new market created by essay and portfolio coaching demand; high arbitrage; no extraction cost
 *   - Well-Resourced High Schools: Secondary beneficiaries (institutional/arbitrage) — have in-house college counseling; alumni networks; understanding of application narratives; can embed coaching into curriculum
 *   - Wealthy/Connected Families: Beneficiaries (institutional/arbitrage) — can afford private essay coaches ($500-2000 USD for application packages); have social capital to decode university preferences
 *   - University Admissions Committees: Mixed actors (organized/constrained) — genuinely want holistic evaluation; constrained by volume and unable to verify essay authenticity; enforce the system through labor (reading, interviewing)
 *   - Standardized Testing Industry: Piton actors (institutional/arbitrage) — appear displaced but persist through institutional inertia; test scores remain primary filters; maintain arbitrage in dual-gate system
 *   - Analytical Observer: Views system as selection problem with irreducible epistemic limits (civilizational/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_university_application_system, 0.52).
domain_priors:suppression_score(taiwan_university_application_system, 0.68).
domain_priors:theater_ratio(taiwan_university_application_system, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_university_application_system, extractiveness, 0.52).
narrative_ontology:constraint_metric(taiwan_university_application_system, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(taiwan_university_application_system, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taiwan_university_application_system, tangled_rope).
narrative_ontology:human_readable(taiwan_university_application_system, "Taiwan's Application-Based University Admission System").
narrative_ontology:topic_domain(taiwan_university_application_system, "social/economic").

domain_priors:requires_active_enforcement(taiwan_university_application_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taiwan_university_application_system, elite_prep_industry).
narrative_ontology:constraint_beneficiary(taiwan_university_application_system, high_resource_schools).
narrative_ontology:constraint_beneficiary(taiwan_university_application_system, well_connected_families).
narrative_ontology:constraint_victim(taiwan_university_application_system, rural_students).
narrative_ontology:constraint_victim(taiwan_university_application_system, low_income_families).
narrative_ontology:constraint_victim(taiwan_university_application_system, first_generation_applicants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL STUDENT WITHOUT RESOURCES (SNARE) — Trapped between the rhetoric of 'holistic assessment' and the material requirement for expensive prep coaching, essay workshops, and portfolio development. Cannot exit the university system (education is career-essential); cannot access the 'application materials' coaching market; bears full extraction cost of the constraint. Maximum d (0.95) — full victim, no alternatives.
constraint_indexing:constraint_classification(taiwan_university_application_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIRST-GENERATION APPLICANT (TANGLED ROPE) — Benefits from the system's stated goal of holistic evaluation — their unique family background, leadership in community contexts, and resilience narratives CAN be conveyed in applications. However, constrained by lack of social capital to interpret what universities actually want, no family mentor experience navigating the system, and economic barriers to presentation professionalization. Moderate extraction but genuine coordination benefit also present.
constraint_indexing:constraint_classification(taiwan_university_application_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PREP INDUSTRY & CONSULTANTS (ROPE) — Experiences the application system as pure coordination benefit. The system creates a market for application essay services, portfolio coaching, interview preparation, and 'positioning strategy' consulting. These service providers have high arbitrage (can sell to multiple students, can exit if system changes). They benefit from the constraint without bearing costs — extraction runs toward them.
constraint_indexing:constraint_classification(taiwan_university_application_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: WELL-RESOURCED HIGH SCHOOLS (ROPE) — Institutional beneficiaries. These schools invest in dedicated college counseling staff, have alumni networks with universities, understand the application narrative that works, and can provide in-school essay workshops and interview coaching. They benefit from arbitrage — they apply the system's logic once and sell the benefit repeatedly to their students. No extraction cost for them.
constraint_indexing:constraint_classification(taiwan_university_application_system, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: UNIVERSITY ADMISSIONS COMMITTEES (TANGLED ROPE) — Experience the system as both coordination and extraction. Genuinely want to identify capable students beyond test scores (coordination benefit — holistic evaluation is real). But constrained by volume (thousands of applications per institution), pressure to maintain prestige signals, and inability to verify essay authenticity or applicant-to-narrative truthfulness. Active enforcement required: committees must review applications, interview candidates, write justifications. The constraint extracts labor (committee effort) and creates perverse incentives (narrative credibility signals extraction value, creating demand for coaching).
constraint_indexing:constraint_classification(taiwan_university_application_system, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: STANDARDIZED TESTING INDUSTRY (PITON) — Technically, the application system was designed to reduce test dependence. In reality, tests remain primary filters — universities use test scores to shortlist applicants before reviewing application materials. The old testing-based system persists through institutional inertia: it's easy to administer, comparable, defensible in disputes. Theater ratio high (0.64) because the performative aspect of 'holistic evaluation' coexists with persistent test reliance. The testing industry maintains arbitrage through the dual-gate system (tests filter; applications select among filtered cohort).
constraint_indexing:constraint_classification(taiwan_university_application_system, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SELECTION PROBLEM VIEW (MOUNTAIN) — From a universal/civilizational perspective, any large-scale selection system (universities choosing students; employers choosing workers; admissions committees choosing peers) faces an irreducible epistemic problem: subjective qualities (creativity, resilience, moral character) cannot be measured without relying on proxies that can be gamed. The application essay is a proxy for 'ability to tell a compelling story about oneself' not for 'actual resilience.' No selection system can escape this without abandoning the goal of holistic assessment. This perspective risks naturalizing the extractive gaming as an inevitable feature of human evaluation — but the data shows resource-dependent gaming is contingent, not inherent.
constraint_indexing:constraint_classification(taiwan_university_application_system, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_university_application_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taiwan_university_application_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_university_application_system, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taiwan_university_application_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(taiwan_university_application_system, TR),
    TR >= 0.70.

:- end_tests(taiwan_university_application_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The system creates a resource-dependent advantage for coaching access. While not as severe as pure predatory extraction (Snare across all perspectives), the effect is systematic and increasing. Initial extractiveness (0.28) reflects lower market sophistication in early years; final value (0.52) reflects mature coaching market and established expectation among elite schools that essay coaching is standard. The intermediate value (0.40) captures the inflection point where coaching became common practice rather than anomaly. Suppression (0.68): Moderate-high. Rural students face multiple suppression mechanisms: lack of nearby coaching services, cultural capital barriers (what makes an essay 'good' is tacit among elites), time cost (essays require significant effort for non-native narrative composers), financial cost (coaching is expensive relative to rural family budgets), and informational suppression (university expectations are opaque outside well-connected networks). However, suppression is not total (some rural students do navigate the system; universities do admit them) and is not backed by explicit coercion — it is structural. Theater ratio (0.64): Moderate-high. The 'holistic evaluation' narrative is performative in two ways: (1) tests remain primary filters, so essays are mostly read for margin selection within test-score bands; (2) applicant narratives are coached/professionalized, making the 'personal story' authenticity questionable. However, theater is not maximal (0.64 not 0.85) because some admissions committees genuinely engage with essays and some applicants do author authentically.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural mechanism produces opposite phenomenological experiences. The application system's stated goal is 'more holistic, fairer evaluation.' From the prep industry's perspective, it is a pure good (Rope) — it created a market and allows service providers to help students. From a wealthy family's perspective, it is a coordination mechanism (Rope) — I can hire experts to help my child present authentically, and the system works as intended. From a rural student's perspective, it is a pure extraction (Snare) — I now face an additional requirement (essay coaching) that I cannot afford, with no alternative exit. From an admissions committee's perspective, it is mixed (Tangled Rope) — I genuinely want holistic evaluation and the system enables it, but I am constrained by volume and unable to verify authenticity. From the testing industry's perspective, it is degraded but persistent (Piton) — the application system was supposed to displace us, but we persist because tests are still the primary filter. From a civilizational/analytical perspective, the constraint is an inherent feature of any large-scale selection system (Mountain) — but this naturalizes what is actually a contingent institutional arrangement (resource-dependent gaming is not intrinsic to holistic evaluation; it is contingent on inequality). The perspectival gap reveals that the constraint is fundamentally about converting test-score inequality into essay-coaching inequality, not about reducing inequality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position relative to extraction flow. Rural students: d ≈ 0.90 (trapped exit + victim status + low power → full target). First-generation applicants: d ≈ 0.70 (constrained exit + mixed victim/beneficiary status + moderate power → target but with some mobility). Prep industry: d ≈ 0.10 (arbitrage exit + beneficiary status + institutional power → beneficiary). Well-resourced schools: d ≈ 0.05 (arbitrage exit + clear beneficiary + institutional power → strong beneficiary). Admissions committees: d ≈ 0.55 (constrained exit + enforcer role + organized power → symmetric, bearing enforcement cost and benefiting from system legitimacy). The engine derives high d → high f(d) → high experienced χ for trapped students; low d → low f(d) → negative χ for beneficiaries. This produces the perspectival gap: snare from powerless (d=0.90, f(d)≈1.42), rope from institutional beneficiaries (d=0.05-0.10, f(d)≈-0.12 to -0.01).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves the mandatrophy by showing that it is NOT a mountain. The civilizational observer's argument ('all selection systems have this tradeoff') mistakes the epistemic problem of measuring unobservables for the extractive problem of resource-dependent access. The epistemological tradeoff is real (holistic vs measurable is inherent). The extractive gaming is not (it emerges only when wealthy students can buy narrative coaching). A hypothetical system that (1) provided free essay coaching to all students, (2) anonymized applications at initial read, and (3) weighted essays more heavily than tests, would solve the epistemic tradeoff AND eliminate the resource-dependent extraction. Such a system is feasible, so the extraction is not inherent — it is contingent on institutional design. Therefore, the Mountain classification is a false summit. The system is best described as Tangled Rope at the analytical level: it has a genuine coordination function (holistic evaluation) but is being overlaid with asymmetric extraction (resource-dependent coaching advantage). The mandatrophy is resolved by clarifying the distinction between the epistemic problem (unsolvable) and the extractive mechanism (contingent, solvable).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    essay_authenticity_verification,
    'What proportion of application essays are substantially authored by students versus service providers or family members?',
    'Linguistic analysis of essays vs classroom writing samples; interviews with admitted students on their authorship; market research on essay coaching uptake; university plagiarism detection audit',
    'If > 40% ghostwritten: application system is identity fraud mechanism (pure Snare). If < 10%: narrative credibility is mostly self-generated (Rope/Tangled Rope). Mid-range: extraction mechanism is active but not total.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(essay_authenticity_verification, empirical, 'Proportion of application essays substantially ghostwritten by services').

omega_variable(
    admission_outcome_by_resource_level,
    'Do students from high-income families with prep coaching access systematically achieve higher admission rates or higher-tier university placements when controlling for test scores?',
    'Regression analysis: admission outcome vs family income, controlling for test scores and application materials quality; stratified by test score band; longitudinal tracking of admitted cohorts',
    'If effect size > 0.3 standard deviations: system is extractive (income → better outcome independent of merit). If < 0.1: system is weakly extractive or mostly merit-based. Effect size determines whether suppression estimate (0.68) is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(admission_outcome_by_resource_level, empirical, 'Whether prep coaching and family resources predict admission independent of test scores').

omega_variable(
    rural_school_admission_representation,
    'Does rural school representation in admission cohorts differ systematically from their share of the applicant pool? Has this changed since application system implementation?',
    'Demographic analysis of admitted cohorts vs applicant pool; stratification by school location (urban/suburban/rural); time series from before/after application system adoption; university public admissions data (if available)',
    'If rural representation declined post-implementation: system is extractive for rural students (supports Snare classification and high suppression). If unchanged: system extracts but not differentially. If increased: system is genuinely improving access (Rope perspective stronger).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rural_school_admission_representation, empirical, 'Whether rural school representation declined after application system adoption').

omega_variable(
    university_true_flexibility_on_essays,
    'How much weight do universities actually place on application essays and portfolios versus test scores in admission decisions?',
    'University admissions office surveys; admission file analysis (test score distribution vs application material distribution in accepted vs rejected cohorts); interview with admissions staff on decision processes',
    'If essays < 20% weight: system is theater (Piton classification strengthened). If 40-60%: genuine coordination benefit exists (Rope/Tangled Rope). If > 70%: essays are primary filter (system is holistic as intended).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(university_true_flexibility_on_essays, empirical, 'Actual weight of essays versus test scores in university admission decisions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taiwan_university_application_system, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taiwan_app_tr_t0, taiwan_university_application_system, theater_ratio, 0, 0.35).
narrative_ontology:measurement(taiwan_app_tr_t5, taiwan_university_application_system, theater_ratio, 5, 0.5).
narrative_ontology:measurement(taiwan_app_tr_t10, taiwan_university_application_system, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(taiwan_app_be_t0, taiwan_university_application_system, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(taiwan_app_be_t5, taiwan_university_application_system, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(taiwan_app_be_t10, taiwan_university_application_system, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taiwan_university_application_system, information_standard).
narrative_ontology:affects_constraint(taiwan_university_application_system, standardized_testing_dependence).
narrative_ontology:affects_constraint(taiwan_university_application_system, educational_inequality_reproduction).

% DUAL FORMULATION NOTE:
% This constraint decomposes the broader 'Taiwan university admission reform' into two structurally distinct claims: (1) Application system as information standard for evaluating non-test dimensions (Rope/Tangled Rope, ε ≈ 0.30), and (2) Application system as extractive gatekeeping mechanism enabled by resource inequality (Snare from victim perspective, ε ≈ 0.52 from systemic view). The higher ε value captures the extractive layering onto the coordination function. Upstream: standardized_testing_dependence (still persists; ε ≈ 0.45, Tangled Rope). Downstream: educational_inequality_reproduction (application system amplifies via coaching market; ε ≈ 0.60, Snare from victim perspective).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(taiwan_university_application_system, powerless, 0.92).
constraint_indexing:directionality_override(taiwan_university_application_system, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
