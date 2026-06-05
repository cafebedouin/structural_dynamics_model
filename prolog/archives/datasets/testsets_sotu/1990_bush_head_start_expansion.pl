% ============================================================================
% CONSTRAINT STORY: 1990_bush_head_start_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1990_bush_head_start_expansion, []).

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
 *   constraint_id: 1990_bush_head_start_expansion
 *   human_readable: Head Start Funding Expansion (1990s Bush Administration): Early Childhood Education Entitlement with School Readiness Mandate
 *   domain: education/social_policy
 *
 * SUMMARY:
 *   The Head Start funding expansion of the early 1990s represents a federal
 *   commitment to standardize human capital formation at the early childhood
 *   intake point, establishing a guaranteed federal program of early
 *   education for low-income children with an explicit school readiness
 *   mandate. The expansion allocates approximately half a billion dollars to
 *   increase access and improve the quality of early childhood education
 *   services. The structural goal is to reduce downstream educational
 *   inequality by ensuring all children, regardless of family income, have
 *   access to high-quality preschool that prepares them cognitively and
 *   socio-emotionally for formal schooling. From the perspective of
 *   low-income families, the constraint solves a genuine coordination
 *   problem: childcare is expensive, and families lack the resources to
 *   purchase quality early education privately. From the perspective of
 *   schools, the constraint is beneficial: children arrive with higher
 *   average readiness, improving instructional efficiency and reducing
 *   variance in classroom composition. From the perspective of federal
 *   education administration, the constraint implements an explicit policy
 *   choice to invest in human capital formation. The constraint classifies as
 *   pure coordination (Rope) across all major perspectives because no agent
 *   experiences significant extraction — all parties benefit from the
 *   coordination of early childhood development. However, the rising theater
 *   ratio (from 0.20 to 0.35 over the interval) suggests increasing
 *   performative content: readiness assessment has become more formal and
 *   standardized, raising questions about whether the mandate is optimizing
 *   for genuine developmental outcomes or theater compliance.
 *
 * KEY AGENTS:
 *   - Low-Income Families: Primary beneficiary (powerless/mobile) — direct access to subsidized early education that would be unaffordable in the private market
 *   - Disadvantaged Children: Primary beneficiary (via representation; moderate/constrained) — improved developmental outcomes and school readiness; some suppression through standardized assessment
 *   - Public School Systems: Secondary beneficiary (powerful/arbitrage) — receive children with higher average readiness; can allocate resources more efficiently
 *   - Federal Education Administration: Sponsoring institutional actor (institutional/arbitrage) — implements policy choice to invest in early childhood human capital; captures political credit for expansion
 *   - Early Childhood Educators and Program Staff: Organized agents (organized/constrained) — benefit from stable employment and professional infrastructure; some suppression through federal compliance requirements
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing readiness mandate as immutable feature of cognitive development rather than contingent policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1990_bush_head_start_expansion, 0.18).
domain_priors:suppression_score(1990_bush_head_start_expansion, 0.25).
domain_priors:theater_ratio(1990_bush_head_start_expansion, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1990_bush_head_start_expansion, extractiveness, 0.18).
narrative_ontology:constraint_metric(1990_bush_head_start_expansion, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(1990_bush_head_start_expansion, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1990_bush_head_start_expansion, rope).
narrative_ontology:human_readable(1990_bush_head_start_expansion, "Head Start Funding Expansion (1990s Bush Administration): Early Childhood Education Entitlement with School Readiness Mandate").
narrative_ontology:topic_domain(1990_bush_head_start_expansion, "education/social_policy").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1990_bush_head_start_expansion, low_income_families).
narrative_ontology:constraint_beneficiary(1990_bush_head_start_expansion, disadvantaged_children).
narrative_ontology:constraint_beneficiary(1990_bush_head_start_expansion, public_school_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME FAMILIES (ROPE) — Powerless agents with mobile exit options (can seek alternative childcare or rely on informal networks, though at higher cost/burden). Head Start provides genuine coordination benefit: subsidized early education that would otherwise be unaffordable. No extraction experienced — the constraint solves a real collective action problem (how to fund quality early childhood education for families below the income threshold). The agent benefits directly and perceives the constraint as enabling, not constraining.
constraint_indexing:constraint_classification(1990_bush_head_start_expansion, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: CHILDREN IN THE SCHOOL READINESS PIPELINE (ROPE) — Moderate agents (via institutional representation by parents/educators) with constrained exit options (children attend school; readiness assessment determines placement/support). The constraint coordinates the transition from home/informal care to formal schooling. The readiness mandate ensures baseline cognitive and social-emotional development. Some suppression (standardized assessment limits alternatives; baseline expectations are non-negotiable) but low — children who do not meet readiness thresholds receive additional support rather than exclusion. Net benefit: improved developmental outcomes.
constraint_indexing:constraint_classification(1990_bush_head_start_expansion, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC SCHOOL SYSTEMS AND RECEIVING COMMUNITIES (ROPE) — Powerful institutional actors with arbitrage options (can adjust curricula, teacher allocation, and resource distribution based on the readiness profile of incoming cohorts). Head Start is a pure coordination benefit: schools receive children with higher average cognitive and social-emotional readiness, reducing within-classroom variance and improving instructional efficiency. No extraction — the constraint solves a collective action problem (coordinating early development across decentralized family and informal care systems). Arbitrage agents experience the constraint as enabling.
constraint_indexing:constraint_classification(1990_bush_head_start_expansion, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL EDUCATION ADMINISTRATION AND CONGRESSIONAL SPONSORS (ROPE) — Institutional actors with arbitrage exit options (can adjust funding levels, programmatic requirements, or reporting standards). The constraint implements an explicit policy choice: federal investment in early childhood human capital formation. This is pure coordination from the sponsoring institution's perspective — it allocates resources to solve a known market failure (low private investment in early childhood for low-income families due to inability to capture returns). No extraction; the constraint reflects the institutional actor's own stated goals.
constraint_indexing:constraint_classification(1990_bush_head_start_expansion, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EARLY CHILDHOOD EDUCATORS AND HEAD START PROGRAM STAFF (ROPE) — Organized agents with constrained exit options (must comply with federal reporting standards, curriculum requirements, and professional credentialing). The constraint coordinates their labor: Head Start funding stabilizes employment, establishes wage floors (though modest), and provides professional development infrastructure. Some suppression (federal requirements reduce autonomy; wage standardization prevents individual wage competition) but low — educators experience the constraint as enabling professional credibility and stable work. Organized agents can also advocate for improved terms through collective bargaining.
constraint_indexing:constraint_classification(1990_bush_head_start_expansion, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical perspective, the school readiness constraint appears as an immutable feature of cognitive development and educational systems: children with higher baseline cognitive and social-emotional competency perform better in formal schooling; standardized readiness assessment is a natural consequence of this relationship. However, this perspective risks false-summit classification. The actual constraint is a policy choice (federal funding + readiness mandate), not an inherent law of child development. The analytical observer naturalizes a contingent institutional arrangement.
constraint_indexing:constraint_classification(1990_bush_head_start_expansion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1990_bush_head_start_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1990_bush_head_start_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1990_bush_head_start_expansion, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(1990_bush_head_start_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. Head Start expansion is a genuine coordination mechanism with minimal extraction. Low-income families experience pure benefit (subsidized access). Schools experience pure benefit (more-ready children). Federal sponsors experience pure benefit (implementation of stated policy goals). No agent perceives themselves as bearing costs relative to the constraint's primary coordination function. The modest value (not zero) reflects two factors: (1) some suppression through standardized readiness assessment, which narrows the range of accepted developmental pathways; (2) opportunity costs in the federal budget (the half-billion dollars could have been allocated elsewhere), though these are politically allocated rather than structurally imposed. Suppression (0.25): Low-moderate. The standardized school readiness assessment creates some suppression by establishing a single developmental model and marginalizing alternative approaches (Montessori, play-based, community-embedded). Children who do not meet readiness thresholds may receive stigmatizing labels or segregated support services, though federal policy emphasizes inclusion rather than exclusion. Educators face compliance requirements that limit pedagogical autonomy. However, suppression is not severe — alternative approaches exist outside the federal program, and the suppression mechanism is institutional (curriculum standards) rather than coercive (no children are excluded from school based on readiness). Theater ratio (0.35): Moderate. Early childhood assessment has increasingly performative content: standardized vocabulary and letter-recognition testing does measure genuine skills but also reflects assessment conventions (e.g., emphasis on print literacy over oral language, cultural bias toward middle-class communication norms). The readiness mandate incentivizes teaching-to-the-test in preschool settings. However, theater is not dominant — the correlation between readiness measures and later school success is empirically real, so the assessment has functional content alongside performative elements.
 *
 * PERSPECTIVAL GAP:
 *   All major perspectives classify the constraint as Rope with minimal disagreement. Low-income families see pure benefit. Schools see pure benefit. Federal sponsors see pure benefit. Educators see benefit with modest compliance burden. Children see mixed benefit (improved readiness but some suppression through standardization). The analytical observer at civilizational scope risks false-summit classification by naturalizing the readiness mandate as immutable. The perspectival gap is unusually small for a constraint story, reflecting that Head Start expansion is genuinely a coordination mechanism rather than a zero-sum extraction. The gap that does exist concerns the analytical observer's tendency to naturalize policy choices as laws of cognitive development — a pattern common in education policy where developmental science is invoked to justify particular institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (low-income families, schools, federal education administration) experience directionality favoring them: low d values (0.05–0.20) reflecting that the extraction flow runs toward them, not away. The low-income family's high mobility (mobile exit option) and direct benefit (can access alternative childcare at higher cost but retains the option) produces d ≈ 0.15. Schools' arbitrage exit option and direct benefit produce d ≈ 0.10. Federal sponsors' institutional power and arbitrage exit produce d ≈ 0.05. The modest suppression (0.25) and low extractiveness (0.18) ensure that even agents with constrained exit options (early childhood educators) do not experience high chi. The constraint is not structured to produce victim-beneficiary asymmetry because the coordination problem it solves (funding quality early childhood education for families below the income threshold) has no zero-sum extraction component.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint does not trigger mandatrophy because it is a genuine coordination mechanism (Rope) that does not claim to be a natural law (Mountain). The false-summit risk occurs at the analytical/civilizational perspective, where the readiness mandate risks being naturalized as an inherent feature of cognitive development rather than a policy choice. However, the base properties and most perspectives correctly identify the constraint as Rope. The omegas flag the potential false-summit by documenting the ambiguity: Are readiness thresholds measuring genuine developmental prerequisites or arbitrary assessment conventions? This resolves the mandatrophy: if thresholds are genuine prerequisites, the mountain perspective becomes defensible (though still contingent on the policy choice to use federal funding to implement them). If thresholds are arbitrary conventions, the analytical observer is naturalizing a constructed constraint, and the perspective should be downgraded. The constraint's structure is clear — it is coordination with some suppression, not extraction masked as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    readiness_threshold_arbitrariness,
    'Are standardized readiness thresholds (vocabulary, letter recognition, social-emotional competency measures) measuring genuine developmental prerequisites or arbitrary assessment conventions that reflect middle-class cultural norms?',
    'Longitudinal study comparing children who meet vs. narrowly miss standardized readiness thresholds; correlation between threshold-crossing and later academic outcomes controlling for socioeconomic factors; cross-cultural validation of readiness measures',
    'If thresholds are genuine prerequisites: Head Start is optimal coordination, and the mountain perspective is justified. If thresholds are arbitrary conventions: the constraint is suppressing alternative developmental pathways (e.g., play-based vs. skills-focused), and suppression should be scored higher (shifting from Rope toward Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(readiness_threshold_arbitrariness, empirical, 'Whether readiness thresholds measure genuine developmental prerequisites or cultural conventions').

omega_variable(
    extraction_through_standardization,
    'Does the readiness mandate extract value from families and educators by requiring compliance with a single developmental model, suppressing alternative approaches (e.g., Montessori, play-based, community-embedded learning)?',
    'Analysis of funding incentives: do Head Start programs that deviate from the readiness-focused curriculum receive lower ratings or reduced funding? Documentation of alternative early education models and their exclusion from the federal program. Survey of educators and families about perceived constraints from standardization.',
    'If standardization suppresses genuine alternatives: extractiveness should increase (0.18 → 0.35+), reclassifying from Rope toward Tangled Rope. If readiness focus is genuinely superior: standardization is coordination without suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_through_standardization, empirical, 'Whether readiness mandate extracts through suppression of alternative pedagogies').

omega_variable(
    federal_budget_burden_distribution,
    'Is the half-billion dollar Head Start expansion absorbed by federal revenue (tax base) in a progressive manner, or does it indirectly extract from other vulnerable populations through opportunity costs (e.g., reduced funding for special education, adult literacy, infrastructure)?',
    'Analysis of federal budget allocation: what programs were defunded or delayed to accommodate Head Start expansion? Distributional analysis of tax burden: who bears the cost? Longitudinal comparison of program funding trajectories before/after the expansion.',
    'If costs are distributed regressively or extracted from other vulnerable programs: the constraint should include secondary victims, and the beneficiary-only structure misses the full picture. This would shift analysis toward acknowledging hidden extraction flows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_budget_burden_distribution, empirical, 'Distributional incidence of Head Start expansion costs').

omega_variable(
    labor_market_response_to_readiness_mandate,
    'Does the readiness mandate, by creating a guaranteed pool of more-ready children, reduce pressure on schools to improve teaching quality, thereby suppressing teacher investment and wage growth?',
    'Comparison of teacher compensation trends in high-coverage vs. low-coverage Head Start regions; analysis of school district spending on professional development before/after Head Start expansion; evidence of curriculum degradation when readiness baseline rises',
    'If readiness mandate reduces teacher wage pressure: educators experience hidden extraction (their labor is devalued because the input problem is ''solved'' by Head Start). This would add educators to the victims array and increase suppression score.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_market_response_to_readiness_mandate, empirical, 'Whether readiness mandate suppresses teacher wages and professional investment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1990_bush_head_start_expansion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(1990_tr_t0, 1990_bush_head_start_expansion, theater_ratio, 0, 0.2).
narrative_ontology:measurement(1990_tr_t5, 1990_bush_head_start_expansion, theater_ratio, 5, 0.28).
narrative_ontology:measurement(1990_tr_t10, 1990_bush_head_start_expansion, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(1990_be_t0, 1990_bush_head_start_expansion, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(1990_be_t5, 1990_bush_head_start_expansion, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(1990_be_t10, 1990_bush_head_start_expansion, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1990_bush_head_start_expansion, resource_allocation).
narrative_ontology:affects_constraint(1990_bush_head_start_expansion, public_school_readiness_expectation).
narrative_ontology:affects_constraint(1990_bush_head_start_expansion, preschool_teacher_credentialing_standards).
narrative_ontology:affects_constraint(1990_bush_head_start_expansion, school_district_special_education_identification).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
