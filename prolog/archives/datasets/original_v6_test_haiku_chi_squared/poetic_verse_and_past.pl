% ============================================================================
% CONSTRAINT STORY: poetic_verse_and_past
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_poetic_verse_and_past, []).

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
 *   constraint_id: poetic_verse_and_past
 *   human_readable: The Inescapable Tale of Institutional Expectation
 *   domain: social/cultural
 *
 * SUMMARY:
 *   Welton Academy (the fictional preparatory school in 'Dead Poets Society')
 *   exemplifies a constraint structure that bridges individual psychological
 *   suffering and institutional class reproduction. The constraint operates
 *   through rigid expectations regarding career paths, academic achievement,
 *   and 'proper' aspiration — internalized by students, enforced by parents,
 *   and systematized by the school. The extractive mechanism works by
 *   capturing compliance (students conform to institutional pathways),
 *   suppressing alternative aspirations (creative, unconventional, or
 *   personally meaningful pursuits are discouraged), and deploying theater
 *   (performative achievement, ritualized college placement, public displays
 *   of success). The constraint is extractive because the institution and
 *   legacy families capture disproportionate benefit (reputation, alumni
 *   networks, credential value) while students bear the psychological cost of
 *   conformity. However, from the institution's perspective, the same
 *   mechanism is experienced as coordination — solving the collective action
 *   problem of 'how do we prepare students for competitive universities?'
 *   This perspectival gap is the diagnostic core: students experience snare;
 *   administrators experience rope. The theater ratio has increased over the
 *   interval (0.55→0.68) as the performative dimension of college preparation
 *   has intensified, suggesting piton-like degradation — the ritual of
 *   achievement is becoming more important than the substance.
 *
 * KEY AGENTS:
 *   - Students with divergent aspirations (powerless/trapped): bear psychological cost of conformity, suppressed creative expression, constrained by tuition/credential dependence
 *   - Legacied students facing parental override (powerless/trapped): financially dependent, structurally locked into parental expectation regime, face family disappointment cost
 *   - School administration (institutional/arbitrage): benefits from institutional reputation, donor relationships, and predictable placement outcomes; experiences constraint as coordination
 *   - Family legacy networks (powerful/arbitrage): reproduce advantage across generations through preparatory school gatekeeping, network access, and inherited expectation
 *   - Reform-minded teachers (organized/constrained): caught between mandated curriculum and personal pedagogical values; constrained by employment dependence but motivated by student wellbeing
 *   - Individual creative expression (victim/trapped): abstract collective that cannot organize; bears suppression through resource allocation, curriculum priorities, and cultural dismissal
 *   - Analytical observer (analytical/analytical): sees constraint as mechanism of class reproduction and stratification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(poetic_verse_and_past, 0.58).
domain_priors:suppression_score(poetic_verse_and_past, 0.72).
domain_priors:theater_ratio(poetic_verse_and_past, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(poetic_verse_and_past, extractiveness, 0.58).
narrative_ontology:constraint_metric(poetic_verse_and_past, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(poetic_verse_and_past, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(poetic_verse_and_past, snare).
narrative_ontology:human_readable(poetic_verse_and_past, "The Inescapable Tale of Institutional Expectation").
narrative_ontology:topic_domain(poetic_verse_and_past, "social/cultural").

domain_priors:requires_active_enforcement(poetic_verse_and_past).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(poetic_verse_and_past, school_administration).
narrative_ontology:constraint_beneficiary(poetic_verse_and_past, legacy_family_networks).
narrative_ontology:constraint_victim(poetic_verse_and_past, students_with_divergent_aspirations).
narrative_ontology:constraint_victim(poetic_verse_and_past, individual_creative_expression).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NONCONFORMING STUDENT (SNARE) — Trapped in a preparatory school culture that defines success via predetermined pathways (Ivy League, law/medicine, family legacy). Exit is structurally impossible: tuition is paid, enrollment is mandatory, and leaving means abandoning the credential signal. The constraint extracts compliance to institutional expectations, suppressing alternative aspirations (poetry, theater, philosophy). d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.64.
constraint_indexing:constraint_classification(poetic_verse_and_past, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: LEGACIED STUDENT / PARENTAL OVERRIDE (SNARE) — Even students with family wealth and network advantage are trapped: parental expectations override individual choice. The student bears the psychological cost of disappointing family legacy. Exit options are constrained by financial dependence and social pressure. d≈0.88, f(d)≈1.32, σ=0.8 → χ≈0.60. Theater ratio high: public performance of ambition (debate team, test scores) masks private doubt.
constraint_indexing:constraint_classification(poetic_verse_and_past, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: SCHOOL ADMINISTRATION (ROPE) — Sees the constraint as coordination: channeling student talent into proven career pathways, ensuring institutional reputation via placement rates, maintaining donor relationships through legacy preference. The constraint solves the collective action problem of 'how do we prepare students for competitive universities?' The administration experiences low extraction cost — the mechanism runs on tradition and inertia. d≈0.15, f(d)≈0.03, σ=0.9 → χ≈0.02. Nearly zero effective extraction from this perspective.
constraint_indexing:constraint_classification(poetic_verse_and_past, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: FAMILY LEGACY NETWORK (ROPE) — Powerful agents benefit from the constraint: it reproduces family advantage across generations. Legacy preference at universities, inherited social capital, and predictable career pathways concentrate wealth and access. The constraint is experienced as coordination: maintaining the network that binds generations. d≈0.08, f(d)≈-0.08, σ=0.9 → χ≈-0.01. Negative effective extraction — net beneficiary.
constraint_indexing:constraint_classification(poetic_verse_and_past, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: REFORM-MINDED TEACHER (TANGLED ROPE) — Constrained by curriculum mandates and parental expectations, but also sees genuine value in preparing students for competitive universities. Experiences the constraint as both coordination (guiding students toward opportunity) and extraction (enforcing conformity). The teacher cannot fully exit: employment depends on institutional compliance, yet conscience requires questioning the system. d≈0.62, f(d)≈0.82, σ=0.8 → χ≈0.38. Mixed experience drives activism for change but without full structural power.
constraint_indexing:constraint_classification(poetic_verse_and_past, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: INSTITUTIONAL THEATER / CIVILIZATIONAL VIEW (PITON) — From a long historical view, the preparatory school's expectation regime is degraded ritual: education oriented toward 'proper' career paths, with the ritual of standardized tests and college placement serving primarily to perform institutional legitimacy rather than to genuinely optimize student development. Theater ratio=0.68 captures the performative: public demonstrations of ambition (honor roll rankings, college acceptances listed in alumni newsletters), test preparation as theater, and the ritualized college interview. The underlying function (credential production, network access) persists through inertia, but its justification has become increasingly narrativized rather than functional. chi≈0.27 because the institutional actor (school administration) has arbitrage exit and derives benefit.
constraint_indexing:constraint_classification(poetic_verse_and_past, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL INEQUALITY (SNARE) — From a global/civilizational perspective, the constraint is a mechanism of class reproduction: preparatory schools systematically filter access by family wealth, instilling 'proper' aspirations that align with institutional gatekeeping. The analytical observer sees this as pure extraction from the structural standpoint of society: the system extracts compliance and conformity from students, suppresses alternative aspirations, and perpetuates inequality. d≈0.78, f(d)≈1.18, σ=1.0 → χ≈0.68. The global scope reflects that this is not a local phenomenon but a systematic feature of educational stratification.
constraint_indexing:constraint_classification(poetic_verse_and_past, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(poetic_verse_and_past_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(poetic_verse_and_past, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(poetic_verse_and_past, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(poetic_verse_and_past, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(poetic_verse_and_past, TR),
    TR >= 0.70.

:- end_tests(poetic_verse_and_past_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint extracts substantial compliance. Students surrender autonomy to institutional expectation, parents extract loyalty and family legacy reproduction, and the school extracts behavioral conformity. The value is not extreme (0.66+) because many students internalize the expectations — compliance is not purely coerced but partly voluntary. However, empirical reality (high youth depression/anxiety at elite preparatory schools, high rates of students abandoning chosen careers post-graduation, widespread reports of constrained aspiration) indicates suppression is real and costs are significant. Suppression (0.72): High. Multiple mechanisms suppress alternatives: curriculum structure (test prep dominates), resource allocation (arts/humanities underfunded), cultural messaging (non-traditional paths are framed as failure or risk), parental leverage (financial/emotional), and peer culture (nonconformity is socially costly). Exit options are severely constrained: tuition is sunk, family pressure is internalized, and leaving conveys stigma. Theater ratio (0.68): High and rising. The constraint operates increasingly through performance of achievement: standardized test scores as proxy for ability, college placement as proxy for success, honor roll rankings as public displays of legitimacy. The actual function (credential production, network access) is real, but the performative dimension has expanded: college counseling focuses on 'story-telling' about applications, test preparation has become ritualistic, and institutional marketing emphasizes placement statistics. The rise from 0.55 to 0.68 indicates Goodhart drift — the metric (college placement) is increasingly optimized at the expense of the underlying function (genuine student development).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a fundamental perspectival gap between institutional and individual viewpoints. The school administration and family legacy networks classify the constraint as rope or even beneficiary coordination — they see a mechanism for solving collective action problems (how do we maintain competitive advantage? how do we prepare students for selective universities?). Students with divergent aspirations classify it as snare — they experience it as pure extraction of compliance, with suppressed alternatives and no genuine exit. The reform-minded teacher classifies it as tangled rope — simultaneously seeing both the coordination function (students do benefit from rigorous preparation) and the extraction mechanism (conformity is coerced). The analytical observer, viewing from civilizational distance, sees the constraint as a mechanism of class reproduction and thus classifies it as snare from a structural inequality standpoint. The perspectival gap is not merely about interpretation but about power: institutional actors have arbitrage exit options and derive benefit; individual students are trapped. The theater ratio bridges perspectives — all actors can recognize the performative element, but institutional actors accept it as legitimate (theater is necessary for competitive signaling), while students experience it as alienating (theater masks suppressed authenticity).
 *
 * DIRECTIONALITY LOGIC:
 *   School administration: Beneficiary + arbitrage → d≈0.15, f(d)≈0.03. Net beneficiary; institutional reputation and donor relationships depend on successful placement, but the administration has exit options (relocate, change strategy) and bears no personal cost. Students with divergent aspirations: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction target. Constrained by tuition, enrollment mandates, credential dependence, and family pressure. Cannot exit without significant cost. Family legacy networks: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; legacy preference reproduces advantage; families have exit options (private tutoring, alternative schools) but benefit too much to exit. Reform-minded teachers: Victim + constrained → d≈0.62, f(d)≈0.82. Constrained by employment dependence but also motivated by professional values; can partially exit (change schools, modify curriculum) but at cost. Analytical observer: analytical → d≈0.78, f(d)≈1.18. Sees constraint from structural inequality perspective; high directionality because the constraint perpetuates systematic disadvantage.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves by recognizing that the snare classification is structural while the rope classification is perspectival. From the institutional actors' viewpoint, the constraint genuinely solves a coordination problem — how to prepare students for competitive universities. This is not false consciousness on the administrators' part; it is a real function. However, the coordination is achieved through extraction. The snare is not pure extraction; it is extraction justified by a coordination function. This is precisely the tangled_rope/snare boundary: the constraint simultaneously solves a coordination problem AND extracts from those it governs. The mandatrophy is resolved by noting that 'pure extraction' (snare) and 'pure coordination' (rope) are both present, but at different scales: individually, students experience extraction; institutionally, coordination is achieved. The empirical question (omega_institutional_coercion_threshold) determines whether this is snare or tangled_rope. If parental/institutional pressure crosses into pure coercion (students have no meaningful choice), the classification is snare. If students have internalized the values and made voluntary choices, the classification is tangled_rope. The fictional narrative (Dead Poets Society) presents it as snare-tending-toward-coercion; the real data (preparatory school enrollment, depression/anxiety rates, career satisfaction) suggests the same. Thus: snare is justified, not false natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_coercion_threshold,
    'At what point does parental/institutional expectation become coercive rather than guidance?',
    'Student consent analysis: distinguishing internalized aspiration from compliance-driven performance; longitudinal tracking of satisfaction and persistence in chosen paths',
    'If threshold is low (easily crossed): constraint is primarily snare. If threshold is high (internalization is voluntary): constraint becomes rope or scaffold with consent mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_coercion_threshold, conceptual, 'Threshold between guidance and coercion').

omega_variable(
    alternative_pathway_viability,
    'Are alternative career paths (arts, unconventional fields, entrepreneurship) genuinely viable from a preparatory school education, or does the institution systematically disadvantage non-traditional outcomes?',
    'Longitudinal tracking of non-traditional graduates: career satisfaction, income, network access, and institutional support for unconventional paths vs. traditional ones',
    'If truly viable: exit options upgrade from ''trapped'' to ''constrained'' for some students. If systematically disadvantaged: suppression intensifies and the snare classification solidifies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_pathway_viability, empirical, 'Whether alternative career paths are genuinely viable').

omega_variable(
    parental_pressure_source,
    'Is parental expectation a genuine family value (internal pressure) or a response to institutional signaling of what constitutes ''success''?',
    'Family interview data; comparison of parental expectations at preparatory schools vs. public schools serving same socioeconomic bracket; institutional messaging analysis',
    'If internal: parents are beneficiaries operating independently; victim is student. If institutional signaling: school is manipulating parental expectations to reinforce extraction; victim is family unit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parental_pressure_source, empirical, 'Source of parental pressure').

omega_variable(
    creative_expression_suppression_mechanism,
    'Does the constraint actively suppress creative expression (through grading, resource allocation, cultural messaging) or merely neglect it (through indifference)?',
    'Resource audit: curriculum hours, funding, facilities for arts vs. STEM/test prep; grading bias toward conventional achievement; student survey data on felt suppression vs. opportunity cost',
    'If active suppression: snare classification confirmed with high malice. If neglect: constraint may degrade to piton (performative institution that ignores rather than enforces).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(creative_expression_suppression_mechanism, empirical, 'Whether suppression is active or passive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(poetic_verse_and_past, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(poet_tr_t0, poetic_verse_and_past, theater_ratio, 0, 0.55).
narrative_ontology:measurement(poet_tr_t2, poetic_verse_and_past, theater_ratio, 2, 0.62).
narrative_ontology:measurement(poet_tr_t4, poetic_verse_and_past, theater_ratio, 4, 0.68).

% Extraction over time
narrative_ontology:measurement(poet_be_t0, poetic_verse_and_past, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(poet_be_t2, poetic_verse_and_past, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(poet_be_t4, poetic_verse_and_past, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(poetic_verse_and_past, resource_allocation).
narrative_ontology:affects_constraint(poetic_verse_and_past, elite_credential_gatekeeping).
narrative_ontology:affects_constraint(poetic_verse_and_past, intergenerational_wealth_reproduction).
narrative_ontology:affects_constraint(poetic_verse_and_past, educational_streaming_by_class).

% DUAL FORMULATION NOTE:
% The preparatory school expectation regime is downstream of broader educational stratification systems. The upstream constraints (credential gatekeeping, wealth reproduction mechanisms) establish the context that makes the school's extraction mechanism functional. The school itself is a node in a larger network of class reproduction; its ε=0.58 reflects its position as a concentrated extraction mechanism within that network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
