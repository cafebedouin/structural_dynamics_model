% ============================================================================
% CONSTRAINT STORY: sotu_2001_bush_annual_student_testing_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2001_bush_annual_student_testing_mandate, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_2001_bush_annual_student_testing_mandate
 *   human_readable: Federal Annual Testing Mandate (NCLB/ESSA Era, Grades 3-8)
 *   domain: education/accountability
 *
 * SUMMARY:
 *   The federal mandate for annual standardized testing in reading and math
 *   (grades 3-8) emerged from the 2001 State of the Union address as a
 *   mechanism to increase accountability and transparency in K-12 education.
 *   The constraint exhibits the classic structure of a coordination mechanism
 *   layered with asymmetric extraction: it genuinely solves an information
 *   problem (school quality visibility) and provides leverage for reform, but
 *   it simultaneously extracts teaching time, narrows curriculum, and
 *   concentrates burden on schools serving disadvantaged students. The
 *   constraint operates through federal enforcement (funding consequences,
 *   school sanctions) paired with measurement infrastructure (testing
 *   industry, state bureaucratic apparatus). The extractiveness has risen
 *   over the 20-year interval as compliance theater has expanded and
 *   functional accountability has plateaued. The theater ratio has also risen
 *   as schools develop sophisticated test-prep protocols that optimize for
 *   test performance without improving underlying literacy and numeracy
 *   instruction.
 *
 * KEY AGENTS:
 *   - Federal Education Department and School Choice Advocates: Primary beneficiaries (institutional/arbitrage) — gain accountability leverage and market information; no exit costs
 *   - Low-Resourced School Districts: Primary victims (powerless/trapped) — forced to reallocate scarce resources to compliance; bear extraction without benefit
 *   - Disadvantaged Student Subgroups: Primary victims (powerless/trapped) — experience narrowed curriculum and testing pressure; test scores label them as deficient despite structural input inadequacy
 *   - Mid-Tier School Districts: Secondary agents (moderate/constrained) — experience mixed costs and benefits; can use accountability pressure as leverage for resources but face teaching time extraction
 *   - Advantaged Families: Secondary beneficiaries (moderate/constrained) — gain transparency and school choice leverage but experience test-prep burden
 *   - Testing Industry and State Assessment Bureaucracy: Institutional maintainers (institutional/arbitrage) — sustain the constraint through inertia; measurement infrastructure persists despite declining functional value
 *   - Education Reform and Accountability Infrastructure: Organized observers (organized/constrained) — originally envisioned sunset through alternative accountability; sunset has been structurally undermined
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2001_bush_annual_student_testing_mandate, 0.52).
domain_priors:suppression_score(sotu_2001_bush_annual_student_testing_mandate, 0.58).
domain_priors:theater_ratio(sotu_2001_bush_annual_student_testing_mandate, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2001_bush_annual_student_testing_mandate, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_2001_bush_annual_student_testing_mandate, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sotu_2001_bush_annual_student_testing_mandate, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2001_bush_annual_student_testing_mandate, tangled_rope).
narrative_ontology:human_readable(sotu_2001_bush_annual_student_testing_mandate, "Federal Annual Testing Mandate (NCLB/ESSA Era, Grades 3-8)").
narrative_ontology:topic_domain(sotu_2001_bush_annual_student_testing_mandate, "education/accountability").

domain_priors:requires_active_enforcement(sotu_2001_bush_annual_student_testing_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2001_bush_annual_student_testing_mandate, federal_oversight_capacity).
narrative_ontology:constraint_beneficiary(sotu_2001_bush_annual_student_testing_mandate, school_choice_advocates).
narrative_ontology:constraint_beneficiary(sotu_2001_bush_annual_student_testing_mandate, high_performing_schools).
narrative_ontology:constraint_victim(sotu_2001_bush_annual_student_testing_mandate, teaching_time_allocation).
narrative_ontology:constraint_victim(sotu_2001_bush_annual_student_testing_mandate, low_resourced_schools).
narrative_ontology:constraint_victim(sotu_2001_bush_annual_student_testing_mandate, disadvantaged_student_subgroups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-RESOURCED SCHOOL DISTRICT (SNARE) — Trapped by federal mandate with no exit. Must allocate scarce resources to test preparation and compliance infrastructure. Bears extraction without genuine benefit: inadequate baseline resources mean the constraint extracts teaching time while producing test scores that reflect input disparities rather than school quality. Suppression is structural: federal funding withholding and school takeover threats prevent exit.
constraint_indexing:constraint_classification(sotu_2001_bush_annual_student_testing_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-TIER SCHOOL DISTRICT (TANGLED ROPE) — Constrained by testing mandate but gains some benefit from accountability structure. Testing provides leverage to request resources and justification for instructional changes. However, substantial extraction occurs through teaching time reallocation, test prep overhead, and pressure to narrow curriculum. Mixed benefits and costs — genuine coordination function (visibility into outcomes) overlaps with asymmetric extraction (time/resource burden).
constraint_indexing:constraint_classification(sotu_2001_bush_annual_student_testing_mandate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FEDERAL OVERSIGHT / SCHOOL CHOICE ADVOCATES (ROPE) — Primary beneficiary experiencing the constraint as pure coordination. Testing data enables federal visibility into school quality, triggers accountability mechanisms, and creates information asymmetry that fuels school choice advocates' portfolio model. Benefits from the constraint flow toward this agent: expanded federal control, new competitive markets in charter/private alternatives. No significant extraction costs — the constraint functions as desired for this perspective.
constraint_indexing:constraint_classification(sotu_2001_bush_annual_student_testing_mandate, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ADVANTAGED STUDENT FAMILIES (TANGLED ROPE) — Gain transparency into school quality and leverage for school choice (benefit) but experience time extraction through test prep and narrowed curriculum (cost). For families with choice options and academic capital, the constraint's transparency is valuable; for families locked into assigned schools, both the constraint and the extraction appear asymmetric. Mixed but more benefit-weighted than victim groups.
constraint_indexing:constraint_classification(sotu_2001_bush_annual_student_testing_mandate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DISADVANTAGED STUDENT SUBGROUPS (SNARE) — Trapped in schools that face dual extraction: (1) teaching time diverted to test prep, narrowing curricular experience, (2) test scores used to label schools and students as failing despite structural input inadequacy. Suppression is high: no exit from assigned schools, no ability to contest measurement methodology, no agency over accountability response. Bear maximum extraction.
constraint_indexing:constraint_classification(sotu_2001_bush_annual_student_testing_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: ACCOUNTABILITY INFRASTRUCTURE / REFORM ORGANIZATIONS (SCAFFOLD) — Organized agents (think tanks, education departments, testing consortia) initially viewed the constraint as temporary coordination tool with sunset clause: once accountability culture matured, states could develop local assessment alternatives. However, measurement infrastructure and federal-state bureaucratic entanglement have extended the sunset indefinitely. Theater ratio has risen as compliance becomes performative. Sunset logic persists rhetorically but structurally decayed.
constraint_indexing:constraint_classification(sotu_2001_bush_annual_student_testing_mandate, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: TESTING INDUSTRY / STATE ASSESSMENT BUREAUCRACY (PITON) — Maintains the constraint through institutional inertia. Original function was transparency and accountability; current function is increasingly performative: test scores are used as accountability signals despite low predictive validity for student outcomes, and the testing infrastructure persists because changing it requires coordination across all states and federal agencies. Theater ratio is high; functional value has atrophied while enforcement apparatus remains.
constraint_indexing:constraint_classification(sotu_2001_bush_annual_student_testing_mandate, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of student outcome measurement is inherent to accountability: any system that claims results-based funding must measure results. This perspective risks naturalizing the specific federal testing mandate as inevitable rather than contingent. The engine's false summit detector will identify this as naturalization of a policy choice (not a natural law).
constraint_indexing:constraint_classification(sotu_2001_bush_annual_student_testing_mandate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2001_bush_annual_student_testing_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2001_bush_annual_student_testing_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2001_bush_annual_student_testing_mandate, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2001_bush_annual_student_testing_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2001_bush_annual_student_testing_mandate, TR),
    TR >= 0.70.

:- end_tests(sotu_2001_bush_annual_student_testing_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts teaching time from core instruction, forces resource reallocation to compliance infrastructure, and produces test scores with uncertain validity for measuring school quality. However, it is not maximal extraction (0.66+) because legitimate accountability and transparency functions exist and benefit some agents. The extraction is asymmetric: low-resourced schools bear most of the burden while gaining least benefit. Suppression (0.58): Moderate-high. Schools face federal funding sanctions and intervention threat if test scores fall below state-set benchmarks, creating structural barriers to exit. Suppression is highest for low-resourced schools with few alternatives (trapped exit); lower for affluent districts with school choice and private alternatives (constrained exit). Theater ratio (0.65): High and rising. Schools have developed sophisticated test-prep protocols and curriculum narrowing strategies that optimize for test performance without corresponding improvements in literacy instruction quality. The original measurement function (assessing school quality for accountability) has increasingly become performative (producing compliant test scores through narrowing and drilling rather than improving actual instruction). Claimed type: Tangled Rope. The constraint exhibits both genuine coordination (providing information about school outcomes) and asymmetric extraction (diverting teaching time, burdening low-resourced schools). The beneficiary is federal oversight capacity; the victims are teaching time and disadvantaged student subgroups. Active enforcement is required to maintain the constraint (federal pressure, funding sanctions).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival disagreement, with different agents experiencing radically different classification types. Federal oversight sees Rope (pure coordination enabling accountability market). Low-resourced schools see Snare (trapped extraction with no benefit). Mid-tier districts see Tangled Rope (mixed coordination and extraction with some agency). Advantaged families see Tangled Rope or Rope depending on whether they prioritize test transparency (Rope benefit) vs. curriculum narrowing (extraction cost). The testing industry and state bureaucracy see Piton (maintenance of performative compliance infrastructure). Education reformers see Scaffold (temporary until alternative accountability matured) but the sunset has not occurred. The analytical observer risks seeing Mountain (accountability measurement as inevitable natural law) but the structural data reveals this as a false summit: the specific annual testing design is a contingent policy choice, not a natural law of accountability. The perspectival gap reveals that the constraint functions as designed for federal oversight (coordination benefit) but extracts from schools and students (asymmetric extraction), making Tangled Rope the core classification from the system's structural view.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each agent derives from their power level, exit options, and relationship to extraction flow. Federal oversight (institutional + arbitrage) has low d (beneficiary with exit options → low-to-negative chi). Low-resourced schools (powerless + trapped) have high d (victim with no exit → high chi via f(d) ≈ 1.42). Disadvantaged students (powerless + trapped) have maximum d (victims doubly trapped by school assignment and measurement methodology → maximum chi). Mid-tier schools (moderate + constrained) have medium-high d (victims but with some agency and some benefit from accountability leverage → medium chi via f(d) ≈ 1.00). Testing industry (institutional + arbitrage) has low d despite maintaining the constraint (beneficiary via contracts and compliance work → low-to-negative chi). The constraint's design funnels extraction toward powerless agents and away from institutional beneficiaries, producing a steep directionality gradient that drives the high suppression score.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint's design as Tangled Rope (not Snare) is justified by the presence of genuine coordination benefits (federal visibility into school performance) overlaid on asymmetric extraction (teaching time, resource burden). However, the decomposition into separate perspectives reveals that the coordination benefit accrues primarily to federal oversight (institutional beneficiary) while extraction costs accrue to low-resourced schools and disadvantaged students (powerless victims). This is not a case of coordination failure masquerading as extraction; rather, it is a case where the coordination mechanism is functioning as designed but benefiting high-power agents at the expense of low-power agents. The constraint is 'tangled' because it genuinely coordinates (provides information) while extracting (diverts time and resources). The mandatrophy is resolved by acknowledging that coordination and extraction are not opposites — the same mechanism can do both simultaneously. The false summit risk is that analytical observers may naturalize the testing mandate as inevitable infrastructure for accountability (Mountain) when it is actually a specific institutional choice (Tangled Rope) whose benefits concentrate on high-power agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    test_validity_measurement_congruence,
    'Do standardized test scores in grades 3-8 validly measure the literacy and numeracy skills schools are responsible for teaching, or do they conflate test-taking skills, student motivation, and prior preparation?',
    'Longitudinal validity study: correlation between grade 3-8 test scores and (a) college readiness in grade 12, (b) post-secondary attainment, (c) labor market outcomes. Decompose measurement error by student demographic and school context.',
    'If valid: constraint provides genuine coordination signal (accountability function works). If invalid or context-dependent: constraint extracts teaching time for low-reliability signal; reclassify from Tangled Rope to Snare for disadvantaged student subgroups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(test_validity_measurement_congruence, empirical, 'Whether test scores measure intended constructs or conflate multiple factors').

omega_variable(
    school_quality_causality,
    'Do test score differences reflect school quality differences, or do they primarily reflect student composition and family background prior to school entry?',
    'Natural experiment or quasi-experimental design using school choice, school switching, or quasi-random school assignment. Isolate school value-added net of student entry characteristics.',
    'If school-caused: constraint creates genuine accountability signal for improving practice. If student-composition-driven: constraint extracts resources from high-need schools for low-signal measurement; reclassify extraction mechanism for low-resourced schools upward (higher d, higher chi).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(school_quality_causality, empirical, 'Whether test scores measure school quality or student composition effects').

omega_variable(
    teaching_narrowing_trade_off,
    'What proportion of curriculum time is diverted to test preparation vs. deep instruction in tested subjects, and what is the causal effect on student learning in untested domains?',
    'Classroom observation studies before/after testing mandate; curriculum analysis of instructional time allocation by subject; assessment of learning in untested domains (science, social studies, arts) correlation with testing pressure.',
    'If narrowing is minimal: constraint''s suppression is overstated. If narrowing is severe: constraint produces hidden extraction (time/learning cost) not visible in test scores; increases chi for all school-level perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teaching_narrowing_trade_off, empirical, 'Magnitude of curriculum narrowing and causal effect on untested domains').

omega_variable(
    accountability_response_efficacy,
    'Do schools classified as failing under the mandate actually implement improvements, or do they cycle through compliance theater (staff turnover, consultant hiring, minimal instructional change)?',
    'Process evaluation of schools in improvement status: actual instructional changes implemented vs. compliance documentation produced. Follow-up test scores and teaching practice observations over 3+ year improvement cycle.',
    'If efficacious: constraint serves genuine accountability function (lower chi for moderate perspectives). If theater: constraint''s suppression increases (schools locked in compliance loop with no path to improvement); reclassify upward toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_response_efficacy, empirical, 'Whether accountability responses produce real instructional improvement or performative compliance').

omega_variable(
    federal_funding_leverage,
    'Is the federal testing mandate enforceable without the threat of funding withholding, or is the constraint''s suppression power dependent on linking test results to Title I dollars?',
    'Policy analysis of penalty mechanisms; comparison of compliance in high-stakes vs. low-stakes testing contexts; state variation in actual funding consequences for poor test performance.',
    'If funding leverage is critical: suppression is a function of federal coercion, not autonomous choice (confirm high suppression). If funding leverage is rhetorical: suppression mechanisms are more subtle (school reputation, parent pressure) — may be lower than measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_funding_leverage, empirical, 'Whether federal testing mandate depends on funding sanctions for compliance').

omega_variable(
    scaffold_sunset_realism,
    'Was the scaffold perspective''s original belief in a sunset clause ever realistic, or has the testing infrastructure and federal-state entanglement made sunset politically impossible?',
    'Historical policy analysis of NCLB design intention and ESSA revision; interviews with policy architects; analysis of state-level attempts to exit federal testing regime.',
    'If sunset was realistic: constraint may revert to Rope or pure coordination as measurement alternatives mature. If sunset was always rhetorical: scaffold perspective is misclassified; reclassify as Piton or Tangled Rope with extended theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_sunset_realism, conceptual, 'Whether scaffold''s sunset clause was ever structurally viable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2001_bush_annual_student_testing_mandate, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_2001_bush_annual_student_testing_mandate, theater_ratio, 0, 0.4).
narrative_ontology:measurement(sotu_tr_t5, sotu_2001_bush_annual_student_testing_mandate, theater_ratio, 5, 0.55).
narrative_ontology:measurement(sotu_tr_t10, sotu_2001_bush_annual_student_testing_mandate, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_2001_bush_annual_student_testing_mandate, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu_be_t5, sotu_2001_bush_annual_student_testing_mandate, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(sotu_be_t10, sotu_2001_bush_annual_student_testing_mandate, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2001_bush_annual_student_testing_mandate, resource_allocation).
narrative_ontology:affects_constraint(sotu_2001_bush_annual_student_testing_mandate, school_choice_market_formation).
narrative_ontology:affects_constraint(sotu_2001_bush_annual_student_testing_mandate, public_private_school_sorting).
narrative_ontology:affects_constraint(sotu_2001_bush_annual_student_testing_mandate, teacher_professionalization_constraint).

% DUAL FORMULATION NOTE:
% The annual testing mandate is downstream of federal accountability architecture (which sets standards for school performance) and upstream of school-level responses (curriculum narrowing, test prep, instructional time reallocation). Separate constraint stories should model the federal standard-setting function and school-level response mechanisms; this story focuses on the testing mandate itself as measurement infrastructure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
