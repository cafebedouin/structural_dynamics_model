% ============================================================================
% CONSTRAINT STORY: taiwan_university_application_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   Taiwan's application-based university admission system (個人申請, Individual
 *   Application pathway) was introduced as a reform to move beyond pure
 *   standardized test ranking and enable holistic evaluation of student
 *   potential. However, the system has evolved into a mechanism that benefits
 *   wealthy urban families and a commercial preparation industry while
 *   creating new barriers for rural and low-income students. The constraint
 *   exhibits hybrid coordination-extraction properties: the holistic
 *   evaluation function is real (universities genuinely benefit from
 *   assessing student potential beyond test scores), but the extraction
 *   mechanism is equally real (rural students bear disproportionate costs of
 *   portfolio development and application coaching services concentrated in
 *   urban centers). The theater_ratio (0.68) reflects performative diversity
 *   narratives and merit-based framing that masks the underlying credential
 *   inflation and class-based stratification being reinforced by the system.
 *   The extractiveness trajectory (0.32→0.52 over the interval) shows how the
 *   system has drifted from its original coordination intent toward
 *   asymmetric extraction as the preparation industry professionalized and
 *   coaching services became prerequisite for competitive applications.
 *
 * KEY AGENTS:
 *   - Rural Low-Income Students: Primary victims (powerless/trapped) — lack access to application coaching, cultural capital signaling, and portfolio development resources concentrated in urban centers
 *   - Wealthy Urban Families: Primary beneficiaries (powerful/arbitrage) — can afford comprehensive application coaching, have cultural capital for portfolio signaling, leverage family networks for extracurricular opportunities
 *   - University Admissions Offices: Institutional beneficiaries (institutional/arbitrage) — gain discretion to shape student cohorts, justify selective recruitment via 'fit' and 'potential' metrics, increase institutional reputation
 *   - Elite Preparation Industry: Organized beneficiaries (organized/arbitrage) — cram schools, portfolio coaching services, essay mentors capture commercial market created by application-based system; revenue grows as system matures
 *   - Educational Equity Advocates: Analytical observers (analytical/analytical) — see both genuine coordination function (holistic assessment) and extraction mechanism (conversion of transparent inequality into opaque inequality)
 *   - Standardized Test Authority (CEEC): Institutional actor facing degradation (institutional/constrained) — maintains testing infrastructure through institutional inertia despite reduced formal gatekeeper role; sees own function decline
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_university_application_system, 0.52).
domain_priors:suppression_score(taiwan_university_application_system, 0.65).
domain_priors:theater_ratio(taiwan_university_application_system, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_university_application_system, extractiveness, 0.52).
narrative_ontology:constraint_metric(taiwan_university_application_system, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(taiwan_university_application_system, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taiwan_university_application_system, tangled_rope).
narrative_ontology:human_readable(taiwan_university_application_system, "Taiwan's Application-Based University Admission System").
narrative_ontology:topic_domain(taiwan_university_application_system, "social/economic").

domain_priors:requires_active_enforcement(taiwan_university_application_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taiwan_university_application_system, elite_preparation_industry).
narrative_ontology:constraint_beneficiary(taiwan_university_application_system, wealthy_families).
narrative_ontology:constraint_beneficiary(taiwan_university_application_system, university_admissions_offices).
narrative_ontology:constraint_victim(taiwan_university_application_system, rural_low_income_students).
narrative_ontology:constraint_victim(taiwan_university_application_system, educational_equity).
narrative_ontology:constraint_victim(taiwan_university_application_system, standardized_test_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL LOW-INCOME STUDENT (SNARE) — Trapped by lack of access to application coaching, portfolio development resources, and cultural capital signaling required by holistic review. Cannot exit: standardized test was transparent metric; application-based system requires expensive preparation services concentrated in urban centers. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.71.
constraint_indexing:constraint_classification(taiwan_university_application_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNIVERSITY ADMISSIONS OFFICE (ROPE) — Benefits from discretion: application-based system justifies selective recruitment of students with demonstrable 'fit' and potential for institutional reputation. Experiences constraint as coordination mechanism enabling nuanced student matching. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(taiwan_university_application_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE PREPARATION INDUSTRY (ROPE) — Organized actors (cram schools, portfolio coaching services, essay mentors) benefit from conversion of standardized test preparation into application-coaching services. Experiences constraint as coordination function that creates market demand for expertise they supply. d≈0.10, f(d)≈-0.09, σ=0.9 → χ≈-0.05.
constraint_indexing:constraint_classification(taiwan_university_application_system, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: MIDDLE-CLASS URBAN STUDENT (TANGLED ROPE) — Mobile: can access preparation services and has social capital for application, but effectiveness varies. Benefits from elimination of pure test-score ranking (may overcome poor exam performance via portfolio narrative). Also constrained by subjective evaluation criteria and preparation cost (₹200K–500K for comprehensive coaching). d≈0.48, f(d)≈0.61, σ=1.0 → χ≈0.32.
constraint_indexing:constraint_classification(taiwan_university_application_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: EDUCATIONAL EQUITY OBSERVER (TANGLED ROPE) — Sees coordination function (holistic assessment more robust than test-only) but also sees extraction mechanism: system converts transparent inequality (test scores) into opaque inequality (access to application coaching and cultural capital signaling). Theater_ratio=0.68 reflects performative diversity narratives masking rising credential inflation and stratification. d≈0.70, f(d)≈1.08, σ=1.0 → χ≈0.56.
constraint_indexing:constraint_classification(taiwan_university_application_system, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: STANDARDIZED TEST AUTHORITY (PITON) — Traditional College Entrance Exam (CEEC) persists through institutional inertia despite reduced formal role. Maintains theater through parallel testing system (many students still take exam as 'backup'). Sees own function as degraded: once gatekeeper, now optional supplementary option. theater_ratio=0.68 reflects maintenance of testing infrastructure despite reduced legitimacy. d≈0.45, f(d)≈0.43, σ=1.0 → χ≈0.30.
constraint_indexing:constraint_classification(taiwan_university_application_system, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_university_application_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taiwan_university_application_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_university_application_system, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.52): Moderate-high. The system creates asymmetric benefits for wealthy families and the preparation industry while imposing costs on rural/low-income students. However, extractiveness is not maximal because the holistic evaluation function is genuine — universities do benefit from assessing factors beyond test scores, and some students without coaching access do gain admission. The 0.52 value reflects that this is not pure rent-seeking but a real mixed mechanism. Suppression (0.65): Moderate-high. Barriers to alternative pathways include: (1) standardized test is still used but no longer primary route (students feel pressure to do both); (2) portfolio development services are professionally gatekept and concentrated geographically; (3) cultural capital required for effective self-presentation is distributed by class; (4) coaching service industry actively markets itself as essential, creating perception of necessity. Theater ratio (0.68): Moderate-high. Universities deploy merit-based and diversity rhetoric around application-based selection. However, admissions criteria are often opaque, weighting of components varies by institution and year, and portfolio authenticity is frequently achieved through paid ghost-writing. The gap between stated holistic assessment ideal and actual opaque criteria-shifting reflects substantial performative content.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates acute perspectival divergence. Rural low-income students see pure extraction (snare): they are trapped by lack of access to coaching and cultural capital, which are now prerequisites. University admissions offices see coordination (rope): they experience the system as solving the problem of matching students to institutional fit. The elite preparation industry sees coordination (rope): they experience expansion of market demand for their services. Middle-class urban students see mixed coordination-extraction (tangled rope): they benefit from non-test pathways but also face rising preparation costs. Educational equity observers see genuine hybrid (tangled rope) but note the theater: the system claims to increase equity while actually increasing hidden stratification by moving from transparent inequality (test scores) to opaque inequality (portfolio access and cultural signaling). The standardized test authority sees its own degradation (piton): the testing infrastructure persists through inertia but has lost functional centrality — the agency maintains testing capacity even though fewer students rely on it as primary pathway.
 *
 * DIRECTIONALITY LOGIC:
 *   Rural low-income students: Victims + trapped → d≈0.92, f(d)≈1.38. No exit options; bear full cost of system complexity. Wealthy families: Beneficiaries + arbitrage → d≈0.05, f(d)≈-0.12. Can exit (test-based backup exists, or emigration); primary beneficiaries. University admissions: Institutional + arbitrage → d≈0.08, f(d)≈-0.11. Gain discretion and soft benefits (institutional reputation); can modify criteria at will. Preparation industry: Organized + arbitrage → d≈0.10, f(d)≈-0.09. Highly mobile; captures commercial market directly. Middle-class students: Mixed victim+beneficiary + mobile → d≈0.48, f(d)≈0.61. Can access coaching (though expensive), benefit from holistic assessment, but constrained by rising credential inflation. Educational equity observers: Analytical position → d≈0.70, f(d)≈1.08. See full extraction structure; bears abstract cost of system inequality. Test authority: Institutional + constrained (cannot exit completely but lost primacy) → d≈0.45, f(d)≈0.43. Degraded position but maintains infrastructure.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolution for this constraint requires distinguishing between: (A) the coordination function (holistic assessment is genuinely better at identifying student potential than pure test ranking), and (B) the extraction mechanism (the system has displaced transparent inequality with opaque inequality, and created a class-stratified preparation industry). The constraint is NOT pure coordination (rope) because rural students cannot benefit from the holistic assessment pathway — they are priced out by coaching services and culturally excluded from portfolio signaling. The constraint is NOT pure extraction (snare) because universities and admissions offices genuinely value holistic information and the system does enable some non-test-advantaged students to gain admission. The mandatrophy is resolved by classifying this as tangled rope: the system provides genuine coordination value (better student-institution matching) AND requires active enforcement (universities must invest in holistic review) AND benefits identifiable groups (wealthy families, preparation industry, universities) AND harms identifiable groups (rural low-income students, educational equity). The theater_ratio of 0.68 reflects that performative rhetoric around equity and merit masks the actual class stratification being reinforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subjective_evaluation_bias,
    'Does holistic review genuinely reduce bias or merely displace test-taking bias into demographic signaling (family background, geographic origin, parental occupation coded via essays and portfolios)?',
    'Longitudinal analysis of admitted cohort demographics pre/post-reform; evaluation of whether portfolio-based selection shows correlation with parental income/education stronger or weaker than test-score correlation',
    'If bias displaced (not reduced): application system is pure extraction mechanism disguised as equity reform. If bias actually reduced: system is genuine tangled rope (coordination + asymmetric extraction trade-off).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subjective_evaluation_bias, empirical, 'Whether holistic review reduces bias or displaces it to demographic signaling').

omega_variable(
    portfolio_access_inequality,
    'What proportion of admitted students through application pathway had access to paid coaching services vs. self-directed application preparation?',
    'Survey of admitted cohort; cost-tracking of application preparation services (cram schools, private mentors, professional essay writers); regional analysis of service availability',
    'If >70% of admitted students used paid services: suppression gate confirmed (0.65+), system is highly extractive. If <40%: suppression overstated, system has genuine mobility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(portfolio_access_inequality, empirical, 'Proportion of students with access to paid application coaching').

omega_variable(
    authenticity_of_portfolio_content,
    'Are student portfolios (essays, project descriptions, extracurricular narratives) genuine student work or substantially ghost-written by professional coaching services?',
    'Linguistic analysis of portfolio submissions; surveys of students regarding coaching service involvement; comparison of portfolio quality distribution vs. standardized writing samples from same students',
    'If portfolio content is substantially inauthentic: theater_ratio should be higher (0.75+), revealing more performative system. If authentic: theater_ratio justified at 0.68.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_of_portfolio_content, empirical, 'Whether student portfolios are authentic or ghost-written').

omega_variable(
    admissions_criteria_transparency,
    'How transparent are university weighting systems for application components (essay, portfolios, test scores, extracurriculars)? Are weighting formulas published and stable across years?',
    'Analysis of published weighting criteria by institution; comparison of stated criteria with reconstructed criteria from admitted cohort; year-to-year variance in criteria',
    'If criteria are opaque or unstable: extractiveness and suppression should be higher (≥0.60). If transparent and stable: system behaves more like rope than snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(admissions_criteria_transparency, empirical, 'Transparency and stability of university weighting criteria').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taiwan_university_application_system, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tw_uni_app_tr_t0, taiwan_university_application_system, theater_ratio, 0, 0.45).
narrative_ontology:measurement(tw_uni_app_tr_t5, taiwan_university_application_system, theater_ratio, 5, 0.58).
narrative_ontology:measurement(tw_uni_app_tr_t10, taiwan_university_application_system, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(tw_uni_app_be_t0, taiwan_university_application_system, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(tw_uni_app_be_t5, taiwan_university_application_system, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(tw_uni_app_be_t10, taiwan_university_application_system, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taiwan_university_application_system, resource_allocation).
narrative_ontology:affects_constraint(taiwan_university_application_system, taiwan_standardized_test_system).
narrative_ontology:affects_constraint(taiwan_university_application_system, cram_school_market_expansion).
narrative_ontology:affects_constraint(taiwan_university_application_system, educational_inequality_urban_rural).

% DUAL FORMULATION NOTE:
% The application-based admission system decomposes into three structurally distinct constraints: (1) the holistic evaluation mechanism itself (genuine coordination, low ε), (2) the preparation industry gatekeeping effect (high ε extraction), and (3) the displacement of transparent test-based inequality with opaque portfolio-based inequality (moderate ε mixed). This story focuses on the integrated system effect; upstream constraints include the standardized test system (which application-based system was meant to replace/supplement) and downstream includes cram school market expansion and widening urban-rural educational gaps.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(taiwan_university_application_system, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
