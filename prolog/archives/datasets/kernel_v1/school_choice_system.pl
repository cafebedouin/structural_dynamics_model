% ============================================================================
% CONSTRAINT STORY: school_choice_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_school_choice_system, []).

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
 *   constraint_id: school_choice_system
 *   human_readable: School Choice System: Coordination and Asymmetric Extraction
 *   domain: education/policy
 *
 * SUMMARY:
 *   School choice systems (charter schools, vouchers, open enrollment across
 *   district lines, selective schools within districts) represent a policy
 *   mechanism for decoupling school assignment from neighborhood residence.
 *   The stated coordination function is genuine: rigid assignment to failing
 *   neighborhood schools prevents families from accessing alternatives and
 *   prevents successful schools from expanding. However, the implementation
 *   exhibits asymmetric extraction: affluent families can exercise choice
 *   through information access, transportation flexibility, and application
 *   navigation; low-income families face material barriers that render choice
 *   formal but inaccessible. The constraint exhibits tangled rope structure —
 *   real coordination coexists with asymmetric extraction — alongside snare
 *   dynamics for trapped populations, rope dynamics for beneficiaries, and
 *   piton dynamics for the accountability system that justifies the policy.
 *   The extractiveness trajectory shows accumulation over 15 years (0.25 →
 *   0.58) as selection pressure intensifies, lowest-performing schools
 *   decline in composition and funding, and the two-tier system stabilizes.
 *   The theater ratio rises as accountability metrics (test scores, choice
 *   access) diverge from student wellbeing and learning quality.
 *
 * KEY AGENTS:
 *   - Low-Income Families: Primary victims (powerless/trapped) — formal choice exists but material barriers (transportation, information, application complexity, work schedule inflexibility) make choice inaccessible. Bears the suppression of limited de facto options.
 *   - Affluent Families: Primary beneficiaries (institutional/arbitrage) — can exercise choice through information access, transportation flexibility, residential mobility, and social networks. Captures high-performing school access.
 *   - High-Performing Public Schools: Secondary beneficiary (institutional/arbitrage) — benefits from positive selectivity, motivated student population, engaged families; genuine coordination function (better match of student/school).
 *   - Charter School Operators: Beneficiary (institutional/arbitrage) — captures public funding, attracts motivated families, has freedom from district constraints; experiences genuine coordination function alongside profit extraction.
 *   - Public School Districts: Mixed actor (moderate/constrained) — forced to compete (coordination benefit: improved performance), but also experiences extraction through loss of best students and families, declining funding, residual population becomes harder to serve.
 *   - Lowest-Performing Schools: Primary victims (powerless/trapped) — experience pure extraction through selection losses, declining resources, and negative composition shifts; no capacity to improve or exit.
 *   - Accountability and Metrics System: Institutional actor (institutional/arbitrage) — justifies choice through test score gaps and choice access data, but metrics have become performative and drive gaming rather than genuine learning improvement.
 *   - Analytical Observer: Sees natural law (civilizational/analytical) — risks treating unequal school quality as inherent rather than policy-contingent.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(school_choice_system, 0.52).
domain_priors:suppression_score(school_choice_system, 0.58).
domain_priors:theater_ratio(school_choice_system, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(school_choice_system, extractiveness, 0.52).
narrative_ontology:constraint_metric(school_choice_system, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(school_choice_system, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(school_choice_system, tangled_rope).
narrative_ontology:human_readable(school_choice_system, "School Choice System: Coordination and Asymmetric Extraction").
narrative_ontology:topic_domain(school_choice_system, "education/policy").

domain_priors:requires_active_enforcement(school_choice_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(school_choice_system, affluent_families).
narrative_ontology:constraint_beneficiary(school_choice_system, high_performing_schools).
narrative_ontology:constraint_beneficiary(school_choice_system, charter_school_operators).
narrative_ontology:constraint_victim(school_choice_system, low_income_families).
narrative_ontology:constraint_victim(school_choice_system, neighborhood_public_schools).
narrative_ontology:constraint_victim(school_choice_system, students_in_school_instability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME FAMILY (SNARE) — Formally has choice but faces material barriers: transportation costs, information asymmetries about school quality, application complexity, inflexible work schedules, lack of social networks informing school selection. The choice mechanism exists but its prerequisites (time, information, mobility) are inaccessible. Maximum extraction: family bears the suppression of limited access while high-performing schools capture the motivated families they want.
constraint_indexing:constraint_classification(school_choice_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MIDDLE-CLASS FAMILY (TANGLED ROPE) — Can exercise choice through modest time/resource investment or residential relocation. Benefits from access to better-performing schools than assignment would provide. Also bears costs: information burden, potential longer commute, risk of mismatched school fit, family displacement. Genuine mixed experience — coordination function (better match with students' needs and school capacity) coexists with asymmetric extraction (wealthier families capture the best options).
constraint_indexing:constraint_classification(school_choice_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HIGH-PERFORMING PUBLIC SCHOOL (ROPE) — Pure coordination benefit: choice mechanism allows them to serve motivated families, builds school community with aligned expectations, enables enrollment planning. No extraction burden — the school captures positive selectivity and network effects. Low effective extractiveness from this position.
constraint_indexing:constraint_classification(school_choice_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: CHARTER SCHOOL OPERATOR (ROPE) — Pure coordination plus profit opportunity: mechanism enables market entry, attracts motivated families, provides public funding. Coordination function is genuine (serves students who would otherwise be trapped in bad district schools). Extraction (profit margin, selective enrollment) is modest relative to the real coordination benefit. Net beneficiary with low suppression overhead.
constraint_indexing:constraint_classification(school_choice_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: PUBLIC SCHOOL DISTRICT (TANGLED ROPE) — Experiences choice as both coordination and extraction. Genuine benefit: forced to compete, improve pedagogy, serve remaining families better. Real extraction: loss of highest-performing students and families to selective schools and charters; funding follows students, reducing resources for lowest-performing schools; residual school population becomes increasingly disadvantaged; district loses best teachers to charter or suburban migration. Moderate power (can adjust policies, invest in instruction) but constrained by state funding formulas and competition.
constraint_indexing:constraint_classification(school_choice_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LOWEST-PERFORMING SCHOOLS (SNARE) — Experience pure extraction and abandonment. Choice mechanisms drain motivated families and best-performing students, leaving behind a more difficult-to-serve population with fewer resources. Suppression is extreme: trapped in a negative selection spiral with no capacity to improve enrollment, no ability to exit the policy regime, declining per-pupil funding as students depart. Maximum experienced extraction with no exit option.
constraint_indexing:constraint_classification(school_choice_system, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 7: ACCOUNTABILITY METRICS (PITON) — School choice policies are justified through choice-amplifying metrics (test score gaps, graduation rates, value-added measures) that have become increasingly performative. The metrics drive school gaming (teaching to test, pushing out low-performing students), diverge from student wellbeing, and measure choice access rather than actual learning. The reporting apparatus persists through institutional inertia despite degraded function. Theater ratio: high (schools optimize metrics rather than pedagogy; choice data published but not actionable for powerless families).
constraint_indexing:constraint_classification(school_choice_system, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LIMITS (MOUNTAIN) — From a civilizational view, school quality is inextricably linked to residential location and family background. Choice cannot overcome these constraints — it merely redistributes who gets access to scarce high-quality seats. The constraint appears natural (fixed supply of good schools, unequal family resources) and immutable. This perspective risks naturalizing what is actually a policy choice (funding formulas, school assignment, zoning).
constraint_indexing:constraint_classification(school_choice_system, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(school_choice_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(school_choice_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(school_choice_system, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(school_choice_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(school_choice_system, TR),
    TR >= 0.70.

:- end_tests(school_choice_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The system exhibits genuine coordination (matching families with schools better suited to their preferences and needs) alongside significant asymmetric extraction (affluent families capture scarce high-quality seats, low-income families bear the suppression cost of limited access). The 0.52 value reflects that coordination is real but extraction dominates for trapped populations. The trajectory from 0.25 to 0.58 over 15 years shows accumulation as selection pressure increases, lowest-performing schools decline, and stable two-tier structures form. Suppression (0.58): Moderate-high. Multiple barriers suppress choice for low-income families: transportation costs (bus fare, longer commute, schedule constraints for working parents); information asymmetry (affluent families have social networks providing school quality information, low-income families lack these); application complexity (choice requires navigation of multiple school applications, different deadlines, enrollment procedures); enrollment timing (schools may reach capacity before low-income families learn about them); risk aversion (low-income families face greater risk from school mismatch — job loss means family relocation, school instability cascades). These barriers are structural, not individual. Theater ratio (0.65): High. Accountability metrics (test score gaps, choice access percentages, graduation rates) have become increasingly performative. Schools optimize for metrics rather than learning (teaching to test). Choice data is published (showing which students accessed choice) but is not actionable for powerless families (knowing 30% of families chose out of the district does not help a trapped family access choice). Metrics measure choice access, not choice quality or student learning outcomes. The gap between metric and function widens as systems mature.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary (Rope) and victim (Snare) perspectives reveals that the constraint's extractiveness is not intrinsic but perspectival. The same policy mechanism produces coordination for agents with choice and extraction for agents without. The gap is not a measurement ambiguity — it reflects genuine structural asymmetry in access and exit options. The false summit (Mountain) perspective reveals that the analytical observer risks naturalizing policy contingency as natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries with arbitrage exit (affluent families, charter operators) derive low d from beneficiary status + high exit capacity. Victims with trapped exit (low-income families in worst schools) derive high d from victim status + no exit capacity. Mixed-victim agents with constrained exit (public school districts, lowest-performing schools) derive moderate-to-high d from victim status + some but limited exit capacity (can improve instructional quality, but cannot overcome selection pressure or funding inadequacy). The suppression values calibrate how much the structural barriers matter: high suppression (0.58) means that formal choice without material access is inaccessible, increasing effective d and χ for trapped agents.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE is the canonical claim type because it correctly captures the coexistence of genuine coordination (solving the assignment rigidity problem) with asymmetric extraction (affluent families capture scarce quality, low-income families bear the suppression cost). The constraint is NOT a pure rope (which would require minimal extraction and no victims). It is NOT a pure snare for all populations (beneficiaries genuinely experience coordination, not extraction). The tangled rope type preserves both functions and prevents the common error of misclassifying mixed extraction-coordination as either pure coordination (Rope) or pure extraction (Snare). The false summit perspective (Mountain) is a diagnostic tool: it exposes the risk that policy makers naturalize what is actually contingent (unequal school quality is treated as natural consequence of housing and family background, rather than as a choice about school funding and assignment rules).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    choice_accessibility_threshold,
    'What level of information, transportation, application complexity, and time cost constitutes a genuine choice option for low-income families vs a formal-but-inaccessible option?',
    'Longitudinal tracking of family choice patterns by income level; measurement of actual vs stated accessibility (families who know about options vs families who apply vs families who enroll); qualitative interviews on decision barriers',
    'If threshold is low (modest information/time sufficient): choice mechanism is more rope than snare for low-income families. If threshold is high (substantial barriers required): choice is largely inaccessible, snare classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(choice_accessibility_threshold, empirical, 'Threshold for genuine vs formal choice accessibility by income level').

omega_variable(
    selection_bias_vs_school_quality,
    'How much of the test score gap between choice schools and non-choice schools reflects genuine school quality improvement vs student selection bias (motivated families, higher baseline achievement, fewer special-needs students)?',
    'Quasi-experimental design comparing charter/choice students to observationally similar public school students; randomized lottery analysis for oversubscribed schools; value-added models controlling for student and family characteristics',
    'If selection dominates (>60% of gap): choice mechanism is primarily redistributive (allocating scarce spots), not improving overall quality. If school quality dominates (>40% of gap): genuine coordination benefit exists, snare classification for non-chosen schools is overstated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selection_bias_vs_school_quality, empirical, 'Decomposition of choice school gains into selection vs school quality effects').

omega_variable(
    equilibrium_redistribution_stability,
    'Does choice reach an equilibrium where everyone who can exit does so, creating stable two-tier systems? Or does continuing instability and redistribution prevent equilibrium?',
    'Historical time-series of school enrollment shifts over 10+ years; demographic tracking of school composition changes; inter-generational family school choices; comparative analysis across districts with different choice policies',
    'If stable two-tier: snare classification for trapped populations is permanent structural feature. If continuing redistribution: system is dynamically unstable, potentially opening windows for intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equilibrium_redistribution_stability, empirical, 'Long-term equilibrium structure of school choice systems').

omega_variable(
    neighborhood_school_recovery_capacity,
    'Can neighborhood public schools that lose motivated families and resources recover through pedagogical improvement, or are negative selection effects (composition decline) self-reinforcing?',
    'Case studies of district schools that improved despite choice competition; measurement of recovery investment vs selection pressure; teaching quality metrics in recovering vs declining schools',
    'If recovery is possible: tangled rope is accurate (schools experience extraction but have agency to improve). If self-reinforcing decline: snare classification is more accurate (trapped in negative spiral).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neighborhood_school_recovery_capacity, empirical, 'Recovery capacity of non-chosen public schools under choice pressure').

omega_variable(
    charter_accountability_mechanism,
    'Are charter schools genuinely subject to closure for poor performance (accountability function) or insulated from accountability pressure by political protection or enrollment fluctuation?',
    'Tracking of charter closures vs public school closures for equivalent performance; analysis of charter performance variation; case studies of underperforming charters that remain open',
    'If genuine accountability: rope classification for charter operators is defensible (market discipline + coordination). If insulated: piton or snare classification (protected institutions extracting public funding with degraded accountability).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(charter_accountability_mechanism, empirical, 'Actual charter school accountability vs claimed market discipline').

omega_variable(
    funding_follow_student_adequacy,
    'Does per-pupil funding follow the student adequately to maintain educational quality in non-chosen schools, or does funding adequacy decline for remaining students?',
    'Tracking of per-pupil spending in choice vs non-choice schools; analysis of fixed costs (facilities, administration) that don''t scale with enrollment loss; comparative resource availability for remaining students',
    'If funding follows adequately: snare classification for lowest-performing schools is overstated (they retain adequate resources). If not: snare is accurate (declining resources compound selection losses, creating self-reinforcing decline).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(funding_follow_student_adequacy, empirical, 'Whether per-pupil funding adequately follows student movement').

omega_variable(
    false_summit_natural_law,
    'Is the unequal distribution of school quality a natural consequence of housing segregation and family-background effects (mountain), or a policy choice contingent on school funding mechanisms and assignment rules (tangled rope / snare)?',
    'Comparative analysis: districts with different funding formulas (property-tax-dependent vs equalized), different choice architectures (universal vs limited), different residential integration levels; historical before/after of policy changes',
    'If natural law: choice is inevitable redistribution mechanism, snare for low-income families is unavoidable. If contingent: different policies (equalized funding, universal access design) could reshape the constraint entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether unequal school quality reflects natural law or policy design').

omega_variable(
    information_asymmetry_persistence,
    'As choice systems mature and information spreads, does the information asymmetry advantage for affluent families persist or diminish? Can universal information provision overcome the structural advantage?',
    'Longitudinal tracking of information awareness by family SES over 5-10 years; evaluation of information interventions (choice guides, school comparison tools); measurement of choice-making quality improvements among low-income families',
    'If asymmetry persists despite information: suggests structural barriers (time/transportation/risk-aversion) beyond information are the binding constraint. If asymmetry diminishes: information provision alone may reduce extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_persistence, empirical, 'Whether information asymmetry advantages persist as systems mature').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(school_choice_system, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(schoolchoice_tr_t0, school_choice_system, theater_ratio, 0, 0.35).
narrative_ontology:measurement(schoolchoice_tr_t5, school_choice_system, theater_ratio, 5, 0.48).
narrative_ontology:measurement(schoolchoice_tr_t10, school_choice_system, theater_ratio, 10, 0.62).
narrative_ontology:measurement(schoolchoice_tr_t15, school_choice_system, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(schoolchoice_be_t0, school_choice_system, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(schoolchoice_be_t5, school_choice_system, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(schoolchoice_be_t10, school_choice_system, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(schoolchoice_be_t15, school_choice_system, base_extractiveness, 15, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(schoolchoice_su_t0, school_choice_system, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(schoolchoice_su_t10, school_choice_system, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(school_choice_system, resource_allocation).
narrative_ontology:affects_constraint(school_choice_system, residential_segregation_and_school_composition).
narrative_ontology:affects_constraint(school_choice_system, school_funding_adequacy).
narrative_ontology:affects_constraint(school_choice_system, teacher_quality_distribution).

% DUAL FORMULATION NOTE:
% School choice represents one reading of the school assignment problem. An alternative reading (universal equalized funding + integrated neighborhoods) would produce a different constraint structure. These are distinct policies on the same domain; network links show how choice policy generates downstream effects on segregation, funding adequacy, and teacher distribution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(school_choice_system, institutional, 0.78).
constraint_indexing:directionality_override(school_choice_system, moderate, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
