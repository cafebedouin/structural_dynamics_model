% ============================================================================
% CONSTRAINT STORY: educational_inequality_reproduction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_educational_inequality_reproduction, []).

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
 *   constraint_id: educational_inequality_reproduction
 *   human_readable: Educational Inequality Reproduction Mechanism
 *   domain: education/social_stratification
 *
 * SUMMARY:
 *   Educational inequality reproduction is a structurally hybrid constraint
 *   that functions simultaneously as coordination mechanism (credential
 *   signaling, skill development, social integration) and extraction
 *   mechanism (sorting by inherited advantage, wealth concentration in elite
 *   institutions, legitimation of stratification). The constraint
 *   demonstrates the full range of DR types from different structural
 *   positions: powerless students experience snare (trapped in a system that
 *   reproduces their disadvantage); first-generation seekers experience
 *   tangled rope (genuine skill acquisition alongside disproportionate cost
 *   and navigation burden); elite institutions experience rope (credential
 *   coordination without extraction overhead); teachers experience tangled
 *   rope (commitment to education coexisting with labor exploitation);
 *   standardized testing experiences piton degradation (high theater, low
 *   function); progressive reformers experience scaffold (temporary problem
 *   solvable through policy sunset); analytical observers risk false summit
 *   (naturalizing institutional design choices as immutable laws); high-SES
 *   families experience tangled rope with potential identity lock
 *   (simultaneous benefit and belief in meritocracy). The constraint's
 *   extractiveness has increased from 0.42 to 0.58 over 60 years (likely
 *   representing the period from 1965-2025), while theater has increased from
 *   0.48 to 0.70. The rising theater ratio indicates that institutional focus
 *   has shifted from actual equalization of opportunity toward performative
 *   gestures (diversity initiatives, aspirational rhetoric) that maintain
 *   theatrical function while extraction mechanisms persist unchanged. The
 *   rising extractiveness reflects that inequality gaps have persisted or
 *   widened despite policy interventions, suggesting the constraint's
 *   reproduction mechanisms are more robust than reform efforts have been
 *   able to address.
 *
 * KEY AGENTS:
 *   - Low-Income Students: Primary victim (powerless/trapped) — structurally locked in through legal requirement, economic necessity, and neighborhood assignment; experience snare with maximal extraction
 *   - First-Generation College Seekers: Secondary victim (moderate/constrained) — structurally mobile but face cultural capital, navigation, and financial constraints; experience tangled rope with significant extraction
 *   - Elite Educational Institutions: Primary beneficiary (institutional/arbitrage) — concentrate resources, control credential monopoly, arbitrage enrollment and outcome definitions; experience rope with minimal extraction overhead
 *   - High-SES Families: Secondary beneficiary (institutional/constrained) — gain advantaged access while maintaining identity belief in meritocracy; experience tangled rope with identity lock
 *   - Teachers in Under-Resourced Schools: Organized secondary victim (organized/constrained) — coordinate educational delivery while bearing extraction through low wages, inadequate resources, and impossible outcome expectations
 *   - Credential Gatekeepers (Colleges, Employers): Institutional beneficiary (institutional/arbitrage) — maintain monopoly on credential valuation and hiring gatekeeping; experience rope
 *   - Standardized Testing Industry: Institutional actor (institutional/arbitrage) — maintains piton through institutional inertia; testing infrastructure persists despite low functional value
 *   - Progressive Reform Movement: Organized secondary actor (organized/constrained) — frame inequality as solvable through policy; experience scaffold with sunset logic if reforms materialize
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(educational_inequality_reproduction, 0.58).
domain_priors:suppression_score(educational_inequality_reproduction, 0.68).
domain_priors:theater_ratio(educational_inequality_reproduction, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(educational_inequality_reproduction, extractiveness, 0.58).
narrative_ontology:constraint_metric(educational_inequality_reproduction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(educational_inequality_reproduction, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(educational_inequality_reproduction, tangled_rope).
narrative_ontology:human_readable(educational_inequality_reproduction, "Educational Inequality Reproduction Mechanism").
narrative_ontology:topic_domain(educational_inequality_reproduction, "education/social_stratification").

domain_priors:requires_active_enforcement(educational_inequality_reproduction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(educational_inequality_reproduction, high_socioeconomic_status_families).
narrative_ontology:constraint_beneficiary(educational_inequality_reproduction, credential_gatekeepers).
narrative_ontology:constraint_beneficiary(educational_inequality_reproduction, resource_concentrated_institutions).
narrative_ontology:constraint_victim(educational_inequality_reproduction, low_socioeconomic_status_students).
narrative_ontology:constraint_victim(educational_inequality_reproduction, first_generation_learners).
narrative_ontology:constraint_victim(educational_inequality_reproduction, structurally_marginalized_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME STUDENT (SNARE) — Trapped by resource barriers, neighborhood school quality, family capital constraints, and belief in individual meritocracy. No exit option from the constraint: schooling is legally mandatory and economically necessary, yet the institution itself reproduces inequality. Maximum experienced extraction without proportional benefit.
constraint_indexing:constraint_classification(educational_inequality_reproduction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIRST-GENERATION COLLEGE SEEKER (TANGLED ROPE) — Structurally mobile (can in principle attend college) but constrained by lack of cultural capital, institutional navigation barriers, and financial burden. Benefits from education's coordination function (skill development, credential value) while bearing disproportionate costs (greater time-to-degree, higher debt, navigation labor). Significant extraction alongside genuine upskilling.
constraint_indexing:constraint_classification(educational_inequality_reproduction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE EDUCATIONAL INSTITUTION (ROPE) — Experiences education as coordination: signaling quality, credentialing students, producing social capital networks. Net beneficiary through arbitrage (can shift enrollment patterns, credential value, selection criteria) without incurring extraction overhead. Views the system as legitimate meritocratic sorting.
constraint_indexing:constraint_classification(educational_inequality_reproduction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TEACHERS IN UNDER-RESOURCED SCHOOLS (TANGLED ROPE) — Organized collectively through unions/professional bodies. Coordinate educational delivery to disadvantaged students while bearing extraction: low wages, inadequate resources, responsibility for outcome gaps they cannot individually close. Genuine commitment to coordination coexists with structural exploitation of labor and emotional care.
constraint_indexing:constraint_classification(educational_inequality_reproduction, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STANDARDIZED TESTING APPARATUS (PITON) — SAT/ACT/standardized achievement testing persists as a degraded sorting mechanism. Functionally, the apparatus measures socioeconomic status and family educational background; it has low correlation with college success beyond freshman year and near-zero correlation with professional achievement. Theater ratio is high: testing is maintained through institutional inertia, credential tradition, and investment sunk in testing infrastructure. The apparatus serves as a proxy for family advantage, not as a functional assessment device.
constraint_indexing:constraint_classification(educational_inequality_reproduction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PROGRESSIVE EDUCATION REFORM (SCAFFOLD) — Organized agents (education policy advocates, equity researchers, community organizers) frame inequality reproduction as a temporary coordination failure solvable through policy intervention: equitable funding, culturally responsive pedagogy, affirmative action, universal pre-K. These reforms reduce theater (lower performativity, higher function) and lower effective extraction through structural access changes. Sunset logic applies if: universal Pre-K + adequately funded schools + equitable wealth redistribution eliminate the inequality pipeline. Estimated sunset if reforms materialize: 25-40 years (requires political will and sustained funding).
constraint_indexing:constraint_classification(educational_inequality_reproduction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some stratification by inherited advantage is treated as an immutable property of human social organization. Educational inequality is reframed as an inherent consequence of family investment disparities, genetic variation in abilities, or market-driven credential value. This perspective naturalizes contingent institutional arrangements (funding allocation models, credential monopolies, cultural capital weighting in assessment) as laws of social nature. The engine will likely flag this as a false summit — the structural data reveals these are design choices, not natural laws.
constraint_indexing:constraint_classification(educational_inequality_reproduction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: HIGH-SOCIOECONOMIC STATUS FAMILY (TANGLED ROPE) — Institutional-level agent (family as organizing unit) benefits from advantaged access while experiencing education as genuine coordination (child development, social integration, credential positioning). Constrained by the need to maintain credential value through continued investment and institutional positioning. Both benefits from extraction (unequal advantage) and perceives the system as fair (meritocratic). Identity partially locked into belief in earned advantage.
constraint_indexing:constraint_classification(educational_inequality_reproduction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(educational_inequality_reproduction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(educational_inequality_reproduction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(educational_inequality_reproduction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(educational_inequality_reproduction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(educational_inequality_reproduction, TR),
    TR >= 0.70.

:- end_tests(educational_inequality_reproduction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting that education both genuinely develops human capital (coordination function) and significantly sorts by inherited advantage (extraction function). The value is not higher (0.70+) because education does produce legitimate benefits across SES levels — the constraint is hybrid, not pure extraction. The 16-percentage-point increase from 0.42 to 0.58 over 60 years reflects institutional concentration: as cost of higher education has risen and funding inequality between school districts has increased, extraction relative to coordination has grown. Suppression (0.68): High. Barriers to educational mobility include: unequal school funding (neighborhood wealth determining resources), unequal access to test preparation and cultural capital, credential signaling that sorts by background rather than skill, belief in meritocratic justification that prevents recognition of systemic barriers, and resource concentration in elite institutions. However, suppression is not absolute (0.85+) because educational mobility is possible and some institutional access exists across SES levels. Theater ratio (0.64): Moderate-high. Standardized testing, diversity initiatives, and aspiration-focused messaging represent significant theatrical components that do not correspond to functional equalization. But not all institutional activity is theater — curriculum, teacher instruction, and credentialing do have real educational function. The 22-percentage-point increase in theater from 0.48 to 0.70 reflects that policy discourse has increasingly emphasized aspirational rhetoric (individual responsibility, meritocratic ideology) that masks unchanged extraction mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a fundamental perspectival conflict. High-SES families and elite institutions perceive education as pure coordination (rope) — it genuinely develops human capital, creates social bonds, and produces credential value. They benefit from the system and can point to their children's real learning as evidence of function. Low-income students perceive education as extraction (snare) — the system sorts them into lower-status positions, concentrates resources away from their schools, and offers credentials with lower value despite identical credentials. Teachers perceive the hybrid (tangled rope) — they provide genuine education while being exploited and scapegoated for outcome gaps driven by resource inequality. The analytical observer risks perceiving a natural law (mountain) — inequality is inherent to human variation, and education merely sorts naturally distributed abilities. This would be a false summit: the structural data shows that funding inequality, credential monopoly, and cultural capital weighting in assessment are institutional design choices, not natural facts. The perspective gap reveals that this constraint is legitimated through one group's genuine experience (elite families' real educational benefits) combined with a false universalization of that experience (meritocratic ideology) that naturalizes institutional structures benefiting that group.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint exhibits strong directionality asymmetry across structural positions. Low-income students have d ≈ 0.95 (full targets, trapped exit) producing f(d) ≈ 1.42, making effective extraction χ extremely high from their perspective. First-generation seekers have d ≈ 0.75 (victims, constrained exit) producing f(d) ≈ 1.10, moderate effective extraction. Elite institutions have d ≈ 0.05 (beneficiaries, arbitrage exit) producing f(d) ≈ -0.12, negative effective extraction (they experience subsidy, not cost). High-SES families have d ≈ 0.20 (beneficiaries with constraints) producing f(d) ≈ 0.02, minimal effective extraction experienced. Teachers have d ≈ 0.60 (mixed victim/partial-beneficiary, constrained exit) producing f(d) ≈ 0.95, moderate-high experienced extraction. The perspectival gap is maximal: beneficiaries experience coordination (rope), targets experience extraction (snare), and mixed agents experience hybrid dynamics (tangled rope). The scope multiplier σ(S) = 1.0 (national) does not magnify extraction as much as global scope would, indicating this is a within-nation stratification constraint rather than global economic exploitation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying that the coordination function is REAL but UNEQUALLY DISTRIBUTED. Education does produce genuine human capital development, social integration, and credential signaling value. The mandatrophy error would be concluding either that (a) education is pure extraction (snare) and therefore should be eliminated, or (b) education is pure coordination (rope) and therefore cannot have inequality-reproducing effects. The correct analysis recognizes that the SAME INSTITUTIONAL STRUCTURE produces coordination benefits for advantaged students (rope perspective) and extraction costs for disadvantaged students (snare perspective). The constraint is tangled rope because it has BOTH genuine coordination function AND asymmetric extraction, not because it is ambiguously one or the other. The extraction arises not from education itself but from the institutional design: unequal funding, credential monopolies, cultural bias in assessment, and neighborhood-based school assignment. These are policy choices, not necessary features of education. Reforms addressing these structural features (equitable funding, multiple credentialing pathways, culturally responsive assessment, school choice or integration) would reduce extraction without eliminating coordination — moving the constraint toward rope or scaffold. The false summits (mountain perspectives) that naturalize these institutional choices as inevitable should be rejected: the framework shows these are contingent design patterns, not laws of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    family_capital_causation,
    'Is educational inequality primarily caused by family capital differences, or do institutional design choices (funding models, assessment practices, cultural bias in curriculum) actively reproduce inequality independently of family factors?',
    'Comparative analysis of inequality trajectories across countries with different funding and assessment models; longitudinal tracking of students matched on family capital across different school resource environments; natural experiments (school funding reforms, assessment changes)',
    'If family capital is primary driver: constraint may be rope or mountain (coordination/immutable). If institutional design is primary driver: constraint is snare/tangled rope (extractive/remediable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_capital_causation, empirical, 'Whether inequality is driven by family capital or institutional design').

omega_variable(
    credential_signaling_function,
    'Does the educational credential primarily signal human capital acquired in school, or does it primarily sort by inherited socioeconomic status and family background?',
    'Employer demand analysis — do employers value specific skills taught in school or the credential as status marker? Comparative earnings analysis within credentials across family backgrounds; test whether credential premium persists when controlling for actual skill measures vs family SES',
    'If credential signals skill: education is genuine coordination mechanism (Rope). If credential primarily sorts by background: education is extraction mechanism with minimal function (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_signaling_function, empirical, 'Whether education credentials signal skills or sort by background').

omega_variable(
    intergenerational_mobility_threshold,
    'What percentage of low-income students must reach middle-class outcomes for the constraint to shift from snare/tangled rope to scaffold (problem-being-solved)?',
    'Historical intergenerational mobility data; comparison across decades and policy regimes; identification of critical mass thresholds in mobility rates that trigger institutional reform vs constraint degradation',
    'If mobility increases > 30% within generation: scaffold perspective validated. If mobility stagnates or declines: snare/piton perspectives confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_mobility_threshold, empirical, 'Intergenerational mobility rate threshold for constraint type shift').

omega_variable(
    identity_lock_mechanism,
    'For high-SES families and credential gatekeepers, is belief in meritocratic education an identity lock (earned advantage narrative) or an external enforced constraint?',
    'Cognitive interviews and ethnographic study of belief formation; comparison of meritocratic belief strength across credential gatekeepers in different inequality contexts; analysis of whether belief persists when shown counterevidence',
    'If identity lock: high-SES agents see rope/coordination even when structural data shows snare (perspectival gap validates identity_locked exit). If external enforcement: agents'' perception is contingent on institutional arrangement, not identity fusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Whether meritocratic belief is identity-fused or externally enforced').

omega_variable(
    reform_efficacy_boundary,
    'Is education policy able to overcome inequality reproduction through school-based reforms alone, or does inequality require wealth redistribution and social policy beyond education sector?',
    'Meta-analysis of education policy interventions (equitable funding, culturally responsive pedagogy, affirmative action, universal Pre-K) with/without accompanying social policy; comparison of inequality trends across countries with different policy mixes',
    'If school-based reforms sufficient: scaffold sunset is achievable through education sector alone. If wealth redistribution required: scaffold sunset requires multi-sectoral coordination, extending timeline and uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_efficacy_boundary, empirical, 'Whether education policy alone can overcome inequality reproduction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(educational_inequality_reproduction, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(edineq_tr_t0, educational_inequality_reproduction, theater_ratio, 0, 0.48).
narrative_ontology:measurement(edineq_tr_t20, educational_inequality_reproduction, theater_ratio, 20, 0.58).
narrative_ontology:measurement(edineq_tr_t40, educational_inequality_reproduction, theater_ratio, 40, 0.64).
narrative_ontology:measurement(edineq_tr_t60, educational_inequality_reproduction, theater_ratio, 60, 0.7).

% Extraction over time
narrative_ontology:measurement(edineq_be_t0, educational_inequality_reproduction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(edineq_be_t20, educational_inequality_reproduction, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(edineq_be_t40, educational_inequality_reproduction, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(edineq_be_t60, educational_inequality_reproduction, base_extractiveness, 60, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(educational_inequality_reproduction, resource_allocation).
narrative_ontology:affects_constraint(educational_inequality_reproduction, labor_market_credentialism).
narrative_ontology:affects_constraint(educational_inequality_reproduction, wealth_intergenerational_transfer).
narrative_ontology:affects_constraint(educational_inequality_reproduction, residential_segregation_reinforcement).

% DUAL FORMULATION NOTE:
% Educational inequality reproduction is downstream of housing market inequality (neighborhood school assignment is endogenous to residential segregation) and upstream of labor market credentialism (education credentials determine access to income-generating occupations). This story focuses on the educational institution itself as a constraint; separate stories track residential segregation effects and credential-income coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(educational_inequality_reproduction, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
