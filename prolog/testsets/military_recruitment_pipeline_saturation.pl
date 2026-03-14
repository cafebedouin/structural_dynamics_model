% ============================================================================
% CONSTRAINT STORY: military_recruitment_pipeline_saturation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_military_recruitment_pipeline_saturation, []).

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
 *   constraint_id: military_recruitment_pipeline_saturation
 *   human_readable: Military Recruitment Pipeline Saturation and Institutional Coercion
 *   domain: military_policy/labor_markets
 *
 * SUMMARY:
 *   The military recruitment pipeline in the United States exhibits a
 *   structured tension between legitimate institutional need (sustained
 *   manning of standing military) and systematic economic coercion of youth
 *   from low-income backgrounds. The constraint is characterized by
 *   saturation: military recruiters are embedded in under-resourced schools,
 *   present in economically distressed communities at rates far exceeding
 *   wealthy neighborhoods, and offer the only visible pathway out of poverty
 *   for youth in regions with collapsed civilian labor markets. The
 *   extractiveness score (0.58) reflects this as a genuine Tangled Rope:
 *   military benefits from predictable youth supply and institutional
 *   coordination of recruitment logistics; schools benefit from military
 *   funding and career programming; but low-income youth face asymmetric
 *   pressures, constrained choices, and suppressed alternatives. Theater
 *   ratio (0.48) reflects that the constraint operates through explicit
 *   messaging (opportunity, choice, patriotism) that masks structural
 *   economic coercion. The measurement trajectory shows increasing
 *   extractiveness (0.35 → 0.58) and rising theater (0.25 → 0.48) over the
 *   20-year interval, indicating intensifying poverty-targeting and
 *   increasingly performative 'voluntarism' messaging.
 *
 * KEY AGENTS:
 *   - Low-income Youth: Primary victim (powerless/trapped) — lack economic alternatives, educational access, geographic mobility; recruited through coerced choice
 *   - Under-resourced Schools: Secondary victim/co-enforcer (moderate/constrained) — dependent on military funding; funnel students into pipeline while benefiting from programs
 *   - Military Command Structure: Primary beneficiary (institutional/arbitrage) — receives sustained personnel supply; controls recruitment strategy and messaging
 *   - Defense Contractors: Secondary beneficiary (institutional/arbitrage) — profit from military expansion and sustained military personnel needs
 *   - Organized Resistance Coalition: Organized opposition (organized/constrained) — parent advocates, veteran organizations, anti-war groups; face institutional power imbalance but can organize pressure
 *   - Civilian Labor Market: Structural victim (analytical/analytical) — loses human capital to military pipeline; experiences talent drain from distressed regions
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing military manning as natural law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(military_recruitment_pipeline_saturation, 0.58).
domain_priors:suppression_score(military_recruitment_pipeline_saturation, 0.62).
domain_priors:theater_ratio(military_recruitment_pipeline_saturation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(military_recruitment_pipeline_saturation, extractiveness, 0.58).
narrative_ontology:constraint_metric(military_recruitment_pipeline_saturation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(military_recruitment_pipeline_saturation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(military_recruitment_pipeline_saturation, tangled_rope).
narrative_ontology:human_readable(military_recruitment_pipeline_saturation, "Military Recruitment Pipeline Saturation and Institutional Coercion").
narrative_ontology:topic_domain(military_recruitment_pipeline_saturation, "military_policy/labor_markets").

domain_priors:requires_active_enforcement(military_recruitment_pipeline_saturation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(military_recruitment_pipeline_saturation, military_command_structure).
narrative_ontology:constraint_beneficiary(military_recruitment_pipeline_saturation, defense_contractors).
narrative_ontology:constraint_victim(military_recruitment_pipeline_saturation, low_income_youth).
narrative_ontology:constraint_victim(military_recruitment_pipeline_saturation, under_resourced_schools).
narrative_ontology:constraint_victim(military_recruitment_pipeline_saturation, civilian_labor_market).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME RECRUIT (SNARE) — Trapped by lack of economic alternatives, educational access, and geographic mobility. Military recruitment becomes the only visible pathway out of poverty. Suppression operates through structural economic immobility: no college funds, limited job market access in economically distressed regions, school counselors who actively funnel toward military. Cannot exit without abandoning all visible futures.
constraint_indexing:constraint_classification(military_recruitment_pipeline_saturation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC SCHOOL SYSTEM (TANGLED ROPE) — Under-resourced schools benefit from military funding for programs (JROTC, robotics, esports), career fairs, and infrastructure upgrades. But these same mechanisms funnel students into military pipeline, constraining their civilian career options. Active enforcement: military presence in curriculum, recruitment contracts, tracking systems. Genuine coordination function (providing pathway structure) embedded in asymmetric extraction (concentration among low-income students).
constraint_indexing:constraint_classification(military_recruitment_pipeline_saturation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MILITARY COMMAND STRUCTURE (ROPE) — Experiences the pipeline as pure coordination: organizing recruitment logistics, establishing predictable supply of trained personnel, matching demand to available youth pools. High degree of freedom — can shift recruitment strategies, invest in signing bonuses, adjust deployment windows. Receives direct benefit from constraint without bearing substantive cost. Effective extraction is minimal from this vantage point; the constraint appears as a functional coordination system.
constraint_indexing:constraint_classification(military_recruitment_pipeline_saturation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZED MILITARY RESISTANCE COALITION (TANGLED ROPE) — Parent organizations, veteran advocates, and anti-war coalitions can resist through legal challenges, school access restrictions, and public pressure. They have agency and mounting evidence of exploitation (poverty targeting, PTSD pipelines, debt traps). But they remain constrained by institutional power differentials, funding limitations, and military's control of messaging in recruitment-saturated regions. Constraint exhibits both coordination function (maintains military manning) and extraction (channeling youth through asymmetric decision-making).
constraint_indexing:constraint_classification(military_recruitment_pipeline_saturation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: VOLUNTEER MILITARY MYTH (PITON) — The United States frames its military as 'all-volunteer,' maintaining this fiction through theater despite structural evidence of systematic poverty-targeting recruitment. Theater ratio (0.48) is moderate — the myth persists because recruitment messaging focuses on opportunity and choice, masking economic coercion. The institutional apparatus (marketing, school partnerships, signing bonuses) maintains the volunteer fiction while mechanically extracting from constrained populations. Primary function (sustained military manning) persists but has partially atrophied into performative voluntarism.
constraint_indexing:constraint_classification(military_recruitment_pipeline_saturation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN CLAIM) — From a civilizational view, one could claim the military recruitment pipeline is structurally immutable: large nation-states require standing armies; standing armies require personnel; systematic recruitment is the only mechanism to sustain personnel supply without conscription. Suggests ε ≤ 0.25 and natural emergence. However, the structural evidence contradicts this: recruitment saturation is a contingent policy choice (deliberate poverty-targeting), not a law of war. The mountain perspective is a false summit — naturalization of institutional choice as structural necessity.
constraint_indexing:constraint_classification(military_recruitment_pipeline_saturation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(military_recruitment_pipeline_saturation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(military_recruitment_pipeline_saturation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(military_recruitment_pipeline_saturation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(military_recruitment_pipeline_saturation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(military_recruitment_pipeline_saturation, TR),
    TR >= 0.70.

:- end_tests(military_recruitment_pipeline_saturation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts human capital from low-income populations, concentrating military-bound youth in economically distressed regions. The extraction is not total (some youth have choices), but it is severe for powerless agents facing economic immobility. The value reflects genuine coordination (military needs personnel supply; recruitment is functional) embedded within asymmetric targeting (wealthy regions not saturated at equivalent rates). Suppression (0.62): High. Structured barriers include economic immobility (no college funds, limited job markets), geographic isolation, school counselor tracking into military pathways, absence of alternative career infrastructure in distressed regions, and public messaging that frames military as only visible opportunity. Theater ratio (0.48): Moderate. The 'all-volunteer military' framing is substantively performative — the constraint operates through economic necessity rather than genuine choice, yet messaging emphasizes agency, opportunity, and patriotic choice. As extraction intensifies, theater increases (0.25 → 0.48) because more performative language is required to maintain volunteer myth.
 *
 * PERSPECTIVAL GAP:
 *   The low-income recruit experiences Snare: coerced, trapped, no exit. The military command structure experiences Rope: functional coordination, high agency, no extraction cost. Schools experience Tangled Rope: genuine benefit (funding) and genuine harm (pipeline creation) simultaneously. The organized resistance experiences Tangled Rope: can organize pressure but constrained by institutional power and school dependency on military funds. The analytical observer risks Mountain: naturalizing military manning as a law of nature requiring saturation recruitment. The perspectival gap reveals that 'choice' in recruitment is radically unequal — wealthy youth see genuine choice (military or college or civilian career); low-income youth see coerced channeling through economic necessity. The constraint operates at different intensities in different regions, creating a two-tier system invisible to aggregate analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   The pipeline concentrates extraction on low-income youth (high d = 0.92, trapped exit → f(d) ≈ 1.38) while benefiting military command (low d = 0.08, arbitrage exit → f(d) ≈ -0.10). Schools occupy intermediate position (d = 0.55, constrained exit → f(d) ≈ 0.75): they extract from students and benefit from military funding simultaneously. The geographic saturation mechanism (high recruiter density in poor zip codes, low density in wealthy ones) is the primary driver of d differentiation — it operationalizes the structural targeting. Organized resistance has moderate agency (d = 0.45, constrained exit → f(d) ≈ 0.50) but insufficient power to break institutional dependency of schools on military funding. The false mountain perspective (analytical/universal) attempts to naturalize this institutional choice as structural necessity, but the contingency is evident in high-income region recruitment patterns (military does not saturate wealthy schools at equivalent intensity, showing choice rather than necessity).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the military recruitment pipeline is a genuine Tangled Rope with strong perspectival variation by structural position. It is NOT a pure Rope (coordination without extraction) because the extraction is severe for powerless agents and geographically concentrated. It is NOT a pure Snare (extraction without coordination) because military's functional need for personnel supply is genuine and creates real coordination problems. It IS Tangled Rope because both functions coexist: the same pipeline that coordinates military manning simultaneously extracts human capital from distressed regions. The false mountain perspective (naturalizing military manning as immutable) is a symptomatic error — it arises from institutional power (military's perspective dominates public discourse) and conflates institutional necessity with natural law. The constraint is contingent: the U.S. could choose alternative manning strategies (higher compensation to attract across income levels, civilian career pathway integration, regional desaturation). The fact that wealthy regions maintain lower saturation while poor regions are saturated reveals choice rather than necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    poverty_coercion_vs_choice,
    'Is military recruitment of low-income youth a poverty-driven coercion mechanism or a legitimate opportunity pathway for economically constrained individuals?',
    'Counterfactual analysis: recruit composition if military offered comparable compensation and benefits in civilian labor markets; exit rates and career satisfaction among military vs civilian-hired youth from same income brackets; long-term economic outcomes for recruits vs non-recruits',
    'If coercion-dominant: suppression rating increases to 0.75+, classification shifts from Tangled Rope toward Snare for low-income perspective. If opportunity-dominant: extractiveness decreases to 0.35, constraint reclassifies as legitimate labor-market coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(poverty_coercion_vs_choice, empirical, 'Poverty-driven coercion vs legitimate opportunity mechanism').

omega_variable(
    recruitment_substitution_effect,
    'Does military recruitment pipeline substitute for civilian education/employment pathways or supplement available options for youth who would otherwise exit labor force entirely?',
    'Quasi-experimental analysis comparing school cohorts in high-recruitment vs low-recruitment districts, controlling for economic conditions; longitudinal tracking of counterfactual civilian employment for military recruits; educational attainment comparison by recruitment exposure',
    'If substitution-dominant: military extraction of human capital from civilian sector; extractiveness reflects zero-sum competition. If supplementation-dominant: recruitment provides value-additive pathway, reducing extraction classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recruitment_substitution_effect, empirical, 'Whether military recruitment substitutes or supplements civilian pathways').

omega_variable(
    geographic_saturation_heterogeneity,
    'Does recruitment saturation differentially affect geographic regions by economic development, creating two-tier system where wealthy regions see recruitment as optional while distressed regions see it as coerced?',
    'Regional analysis of recruitment density (recruiters per school-age population), compensation structures, school integration levels, and public messaging by ZIP code income quintile and regional economic development index; correlation of saturation intensity with youth poverty rates',
    'If strong heterogeneity confirmed: constraint functions as geographic wealth extraction (wealthy regions retain human capital; poor regions lose it), elevating suppression and asymmetry of extraction to Snare levels for distressed regions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geographic_saturation_heterogeneity, empirical, 'Geographic heterogeneity in recruitment saturation and coercion intensity').

omega_variable(
    institutional_capture_feedback,
    'Does school system dependency on military funding create institutional capture that prevents schools from developing alternative career pathways, locking them into recruitment pipeline mechanism?',
    'Analysis of schools with vs without JROTC/military funding; alternative funding availability; school administrator decision-making on curriculum and career placement; longitudinal tracking of schools attempting to reduce recruitment presence',
    'If capture confirmed: schools become co-beneficiaries enforcing extraction rather than neutral institutions; suppression increases because schools themselves suppress alternative pathways. Tangled Rope classification becomes more stable across perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_feedback, empirical, 'School system institutional capture by military funding dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(military_recruitment_pipeline_saturation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mrps_tr_t0, military_recruitment_pipeline_saturation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mrps_tr_t10, military_recruitment_pipeline_saturation, theater_ratio, 10, 0.38).
narrative_ontology:measurement(mrps_tr_t20, military_recruitment_pipeline_saturation, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(mrps_be_t0, military_recruitment_pipeline_saturation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mrps_be_t10, military_recruitment_pipeline_saturation, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(mrps_be_t20, military_recruitment_pipeline_saturation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(military_recruitment_pipeline_saturation, resource_allocation).
narrative_ontology:affects_constraint(military_recruitment_pipeline_saturation, college_debt_trap).
narrative_ontology:affects_constraint(military_recruitment_pipeline_saturation, regional_economic_inequality).
narrative_ontology:affects_constraint(military_recruitment_pipeline_saturation, veteran_post_service_earnings_penalty).

% DUAL FORMULATION NOTE:
% The recruitment pipeline saturation is downstream of regional economic inequality (upstream constraint). Economic collapse in distressed regions creates demand pull for military pathways. The pipeline affects downstream constraints including post-service economic outcomes (veteran earnings penalties, civilian credential deficits) and labor market stratification by region.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(military_recruitment_pipeline_saturation, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
