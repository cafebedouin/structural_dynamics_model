% ============================================================================
% CONSTRAINT STORY: university_league_table_rankings
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_university_league_table_rankings, []).

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
 *   constraint_id: university_league_table_rankings
 *   human_readable: University League Table Rankings Constraint
 *   domain: higher_education/institutional_governance
 *
 * SUMMARY:
 *   University league table rankings create a structural constraint that
 *   coordinates stakeholder information (enabling efficient matching between
 *   institutions and prospective students) while extracting institutional
 *   autonomy and distorting mission alignment. The constraint exhibits
 *   measurable growth in theater ratio (0.55 → 0.78 over 20 years),
 *   reflecting increasing metric gaming and performative behavior relative to
 *   core educational function. Extractiveness has risen from 0.35 to 0.58,
 *   indicating progressive capture of institutional decision-making by
 *   ranking optimization. The constraint is Tangled Rope at the center
 *   (genuine coordination function exists alongside asymmetric extraction)
 *   but appears as Snare from teaching-focused universities and equity
 *   missions, Rope from elite institutions, and Piton when examining degraded
 *   research quality metrics. Prospective students experience identity_locked
 *   entry options: they are structurally mobile (can choose universities on
 *   alternative criteria) but cognitively locked by internalized framing that
 *   ranking position indexes legitimate quality.
 *
 * KEY AGENTS:
 *   - Ranking Agencies: Primary beneficiaries (institutional/arbitrage) — generate revenue from rankings; maintain stakeholder demand; have exit capacity (could be displaced by alternatives)
 *   - Elite Universities: Primary beneficiaries (institutional/arbitrage) — benefit from self-reinforcing status amplification; have full exit capacity (reputation survives ranking demotion)
 *   - Teaching-Focused Universities: Primary victims (powerless/trapped) — institutional mission penalized by metrics; cannot exit because visibility/enrollment depend on ranking presence
 *   - Equity-Access Universities: Primary victims (powerless/trapped) — structural penalties for widening access (selectivity metrics punish high-diversity enrollment); cannot exit due to reputational dependency
 *   - Mid-Tier Universities: Organized secondary victims (organized/constrained) — constrained to ranking-friendly strategies; limited coalitional power; moderate extraction
 *   - Prospective Students: Moderate secondary victims (moderate/identity_locked) — identity-locked by internalized ranking status belief; structurally mobile but cognitively trapped
 *   - Research Quality Measurement: Institutional actor (institutional/constrained) — measurement has degraded (citation inflation, gaming); maintained through inertia despite reduced function (piton)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(university_league_table_rankings, 0.58).
domain_priors:suppression_score(university_league_table_rankings, 0.62).
domain_priors:theater_ratio(university_league_table_rankings, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(university_league_table_rankings, extractiveness, 0.58).
narrative_ontology:constraint_metric(university_league_table_rankings, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(university_league_table_rankings, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(university_league_table_rankings, tangled_rope).
narrative_ontology:human_readable(university_league_table_rankings, "University League Table Rankings Constraint").
narrative_ontology:topic_domain(university_league_table_rankings, "higher_education/institutional_governance").

domain_priors:requires_active_enforcement(university_league_table_rankings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(university_league_table_rankings, ranking_agencies).
narrative_ontology:constraint_beneficiary(university_league_table_rankings, elite_universities).
narrative_ontology:constraint_beneficiary(university_league_table_rankings, prospective_students_wealthy).
narrative_ontology:constraint_victim(university_league_table_rankings, teaching_quality).
narrative_ontology:constraint_victim(university_league_table_rankings, research_diversity).
narrative_ontology:constraint_victim(university_league_table_rankings, mid_tier_universities).
narrative_ontology:constraint_victim(university_league_table_rankings, access_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TEACHING-FOCUSED UNIVERSITY (SNARE) — Trapped in a league table system that penalizes teaching quality and institutional mission. Cannot exit because reputation and enrollment depend on ranking visibility. Bears full cost of metric misalignment: resources diverted to ranking optimization rather than pedagogical improvement. Zero degrees of freedom.
constraint_indexing:constraint_classification(university_league_table_rankings, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EQUITY-ACCESS MISSION (SNARE) — Universities committed to widening access face direct penalty: league tables reward student selectivity (high SAT/A-Level scores) and penalize enrolling disadvantaged students. Trapped between institutional equity commitment and ranking survival. No exit option — reputation and funding flow follow rankings.
constraint_indexing:constraint_classification(university_league_table_rankings, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-TIER UNIVERSITY COLLECTIVE (TANGLED ROPE) — Coordinated through ranking tiers, which enables strategic positioning and peer comparison. But also experiences extraction: constrained to pursue ranking-friendly metrics (research concentration, selectivity) over institutional autonomy. Coalition capacity means moderate extraction rather than total capture, but still significant.
constraint_indexing:constraint_classification(university_league_table_rankings, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ELITE UNIVERSITY (ROPE) — Benefits from ranking systems through reinforcing status. High ranking attracts top students and researchers, creating self-reinforcing network effects. Experiences constraint as pure coordination: the ranking signal enables efficient market matching. Net beneficiary with full exit capacity (could ignore rankings, reputation still holds).
constraint_indexing:constraint_classification(university_league_table_rankings, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RANKING AGENCY (ROPE) — Benefits from sustained demand for rankings; coordination benefit is genuine (stakeholders want comparable data). Low extraction experienced because agency has exit capacity (could be replaced by alternative ranking systems) and actively markets coordination benefit. Theater high but not problematic from this position.
constraint_indexing:constraint_classification(university_league_table_rankings, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: RESEARCH QUALITY METRIC (PITON) — Rankings claim to measure research quality via citation counts and research volume, but the metric has degraded: citation inflation, review manipulation, and predatory publishing make citation-based rankings increasingly performative. The ranking agencies maintain the metric through institutional inertia and stakeholder investment, despite diminishing function. Theater ratio high (0.78) derives from this degradation.
constraint_indexing:constraint_classification(university_league_table_rankings, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: PROSPECTIVE STUDENT (TANGLED ROPE with identity_locked) — Structurally mobile (could choose universities on non-ranking criteria: pedagogy, location, affordability, mission fit), but identity-locked by internalized framing that ranking position indexes legitimate quality. Exit would require abandoning the identity frame that 'going to a top-ranked university' is the pathway to success. Moderate extraction because some agency exists, but locked by cognitive frame rather than material barriers.
constraint_indexing:constraint_classification(university_league_table_rankings, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN CLAIM — FALSE SUMMIT) — The constraint risks naturalizing as an immutable feature of mass higher education: 'Institutions must be ranked; markets require signals; students need information asymmetry reduction.' This framing treats ranking systems as law-like necessities. However, structural data contradicts mountain classification — the constraint exhibits high theater (0.78), active enforcement requirements, clear beneficiaries and victims, and measurable suppression. The 'naturalness' is institutional inertia, not irreducible physics or logic.
constraint_indexing:constraint_classification(university_league_table_rankings, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(university_league_table_rankings_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(university_league_table_rankings, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(university_league_table_rankings, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(university_league_table_rankings, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(university_league_table_rankings, TR),
    TR >= 0.70.

:- end_tests(university_league_table_rankings_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting progressive institutional capture. Initial value 0.35 represents the constraint's early coordination phase (1990s-2000s) when rankings provided genuine information service with limited metric gaming. Current value 0.58 reflects extensive resource diversion to ranking optimization (research concentration over teaching development, selectivity focus over mission diversity) by non-elite institutions. Suppression (0.62): Moderate-high. Significant barriers include: ranking visibility as necessary for enrollment and funding (institutions cannot ignore tables), reputational cascades (failure to rise in rankings damages institutional position), and limited alternative signaling mechanisms. Prospective students face suppression through cognitive capture (identity-locked) in addition to structural barriers (cost/access constraints). Theater ratio (0.78): High and rising. Reflects growing disconnect between measured metrics and actual educational quality: citation metrics inflated by predatory publishing and self-citation strategies; research volume measurements decouple from research rigor; student selectivity correlates poorly with post-graduation outcomes. The teaching metrics largely absent from rankings while remaining central to educational mission. Growth from 0.55 to 0.78 indicates increasing performative behavior relative to functional value.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates dramatic perspectival divergence. The ranking agency and elite universities experience pure coordination benefit (Rope) with no meaningful extraction experienced. Teaching-focused universities experience total capture (Snare) with zero strategic autonomy. Mid-tier universities occupy the middle (Tangled Rope): real coordination benefit from peer positioning and market signaling, but substantial extraction through forced metric optimization. Prospective students occupy a boundary condition (Tangled Rope with identity_lock): they are not powerless agents, but cognitive capture creates extraction that exceeds their structural position's material constraints. The divergence reflects that league tables create genuine information service (coordination function) while selectively extracting from agents whose missions, equity commitments, or cognitive frames align poorly with optimizable metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from power, exit options, and structural position in the extraction flow. Elite universities have low d (beneficiary status, arbitrage exit) producing negative/low χ. Teaching-focused universities have high d (victim status, trapped exit) producing high χ experienced as snare. Mid-tier universities have moderate d (victim status, constrained exit with coalition capacity) producing moderate χ as tangled rope. Prospective students have elevated d despite moderate power because identity_locked exit functions similarly to trapped exit in limiting available responses: the cognitive frame preventing conceptual exit elevates experienced extraction even when material barriers are lower than trapped agents. Ranking agencies have low d (beneficiary status with arbitrage exit creating coordination benefit) producing rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint resolves mandatrophy by demonstrating that genuine coordination (market signaling, institutional comparison) coexists with measurable asymmetric extraction (metric gaming, mission distortion, access barriers). The coordination benefit is real but distributed unequally — elite institutions and ranking agencies capture it; mid-tier and access-focused institutions pay for it. The suppression (0.62) confirms that this is not pure coordination: alternatives are suppressed (rankings become hegemonic, alternative signaling mechanisms underfunded), and exit costs are significant (institutions cannot ignore rankings despite misalignment). Theater ratio (0.78) shows degradation — the measurement systems have decoupled substantially from the quality outcomes they purport to measure — but not complete theater (metric gaming serves institutional survival even if it doesn't measure quality). The false mountain claim appears when the constraint is naturalized as inherent to 'market-driven' higher education, but the structural data reveals a contingent institutional arrangement: alternative reputation mechanisms exist; rankings are maintained through active enforcement and stakeholder lock-in, not laws of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_validity_collapse,
    'Do league table metrics (citation counts, research volume, student selectivity, reputation surveys) actually measure educational quality or institutional mission effectiveness?',
    'Longitudinal correlation analysis between ranking position and long-term student outcomes (career earnings, job satisfaction, civic participation); cross-institutional comparison of pedagogical innovation vs ranking position',
    'If metrics are invalid: the constraint is pure theater (theater_ratio should exceed 0.85 and extractiveness should rise to 0.65+). If metrics correlate with true quality: constraint is coordination with embedded extraction (current classification holds).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metric_validity_collapse, empirical, 'Validity of league table metrics as educational quality measures').

omega_variable(
    metric_gaming_threshold,
    'At what percentage of institutional behavior diverted to ranking optimization does the constraint cross from coordination to pure extraction?',
    'Institutional case studies tracking resource allocation shifts (% budget to research output vs teaching development, selectivity-driven admissions vs mission-driven enrollment); comparison of metric optimization effort vs core mission outcomes',
    'If gaming >40%: constraint reclassifies toward Snare. If gaming <20%: constraint validates as Tangled Rope with genuine coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_gaming_threshold, empirical, 'Threshold for metric gaming as dominant institutional behavior').

omega_variable(
    alternative_reputation_mechanisms,
    'Could institutional reputation and student information be coordinated through mechanisms with lower suppression and theater than league tables?',
    'Comparative analysis of alternative ranking systems (U.S. News alternatives, regional rankings, disciplinary rankings, student outcomes tracking, employer satisfaction surveys); measurement of stakeholder adoption and reliance rates',
    'If effective alternatives exist: suppression should be reclassified as 0.40+ (institutional choice to adopt tables despite alternatives). If no viable alternatives: suppression reflects genuine market necessity (current classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reputation_mechanisms, conceptual, 'Viability of alternative reputation coordination mechanisms').

omega_variable(
    identity_lock_persistence,
    'Does internalized ranking status belief persist after exposure to evidence that ranking position does not predict personal educational outcomes?',
    'Longitudinal student surveys pre- and post-enrollment measuring: ranking status internalization, post-exposure attitude change, career satisfaction correlation with pre-enrollment ranking beliefs',
    'If belief persists despite disconfirming evidence: identity_locked classification is confirmed (student remains trapped by cognitive frame). If belief shifts: students are constrained but not identity_locked (exit is costly but conceptually possible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Persistence of internalized ranking status beliefs across contradicting evidence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(university_league_table_rankings, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ultr_tr_t0, university_league_table_rankings, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ultr_tr_t10, university_league_table_rankings, theater_ratio, 10, 0.68).
narrative_ontology:measurement(ultr_tr_t20, university_league_table_rankings, theater_ratio, 20, 0.78).
narrative_ontology:measurement(ultr_tr_t5, university_league_table_rankings, theater_ratio, 5, 0.61).
narrative_ontology:measurement(ultr_tr_t15, university_league_table_rankings, theater_ratio, 15, 0.74).

% Extraction over time
narrative_ontology:measurement(ultr_be_t0, university_league_table_rankings, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ultr_be_t10, university_league_table_rankings, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ultr_be_t20, university_league_table_rankings, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ultr_be_t5, university_league_table_rankings, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(ultr_be_t15, university_league_table_rankings, base_extractiveness, 15, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(university_league_table_rankings, information_standard).
narrative_ontology:boltzmann_floor_override(university_league_table_rankings, 0.12).
narrative_ontology:affects_constraint(university_league_table_rankings, research_assessment_exercise).
narrative_ontology:affects_constraint(university_league_table_rankings, institutional_gaming_for_metrics).
narrative_ontology:affects_constraint(university_league_table_rankings, student_debt_access_constraint).

% DUAL FORMULATION NOTE:
% League table rankings form an institutional family with related constraints: research assessment exercises (RAE/REF) define the underlying research metrics that rankings amplify; institutional metric gaming is the behavioral response to ranking pressures; student debt and access constraints interact with ranking-driven selectivity. Each story has distinct epsilon: rankings (0.58) coordinate information but extract autonomy; RAE/REF (0.65) measure research with higher extraction; metric gaming (0.72) is predominantly extractive behavioral response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(university_league_table_rankings, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
