% ============================================================================
% CONSTRAINT STORY: conquest_of_labor_exclusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_conquest_of_labor_exclusion, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: conquest_of_labor_exclusion
 *   human_readable: Conquest of Labor: Exclusion of Arab Workers from Jewish Economic Sector
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   The 'conquest of labor' (Kibbush Ha'avoda) was a foundational Labor
 *   Zionist policy from the 1920s through 1948 that systematically excluded
 *   Arab workers from the Jewish economic sector in Mandate Palestine. The
 *   policy had two stated goals: (1) provide employment for Jewish immigrants
 *   in a context where Arab workers were more numerous and willing to work
 *   for lower wages, and (2) build a separate Jewish national infrastructure
 *   independent of Arab labor to prevent economic interdependence that might
 *   undermine future partition or statehood. The Histadrut (General
 *   Federation of Jewish Workers) enforced the policy through membership
 *   restrictions (Arabs excluded until 1959), boycotts of Jewish employers
 *   who hired Arab workers, and control of employment in Jewish-owned
 *   enterprises, agricultural settlements, and public works projects funded
 *   by Zionist institutions. The policy was contested within the Zionist
 *   movement: Revisionist Zionists and some liberal Zionists opposed it on
 *   economic efficiency grounds, while Labor Zionists defended it as
 *   necessary for nation-building. The constraint exhibits different
 *   structural properties depending on the observer's position: Arab workers
 *   experienced systematic exclusion and displacement (snare); Jewish workers
 *   experienced protected employment with ideological constraints (tangled
 *   rope); Zionist institutions experienced successful
 *   infrastructure-building (rope); and the Labor Zionist vanguard framed it
 *   as temporary necessity with sunset logic (scaffold). The policy's
 *   extractiveness and suppression increased over the interval as enforcement
 *   mechanisms matured and the 1936-1939 Arab Revolt hardened ethnic-economic
 *   boundaries. Theater ratio remained relatively low because the policy's
 *   function (employment provision and infrastructure-building) was genuine,
 *   not performative, even as its extraction mechanism intensified.
 *
 * KEY AGENTS:
 *   - Displaced Arab Agricultural Workers: Primary victim (powerless/trapped) — lost livelihoods through land dispossession and systematic employment exclusion; no exit options within Mandate Palestine
 *   - Arab Urban Laborers: Secondary victim (powerless/identity_locked) — excluded from Jewish sector employment; identity-locked by anti-colonial class consciousness that made collaboration with Zionist labor institutions unthinkable
 *   - Jewish Immigrant Workers: Mixed position (moderate/constrained) — benefited from protected employment but bore costs of wage suppression, ideological policing, and foreclosed class solidarity
 *   - Histadrut and Zionist National Institutions: Primary beneficiary (institutional/arbitrage) — built institutional power through employment control, land acquisition, and settlement infrastructure
 *   - Labor Zionist Ideological Vanguard: Organized agents (organized/mobile) — framed policy as temporary scaffold for nation-building with explicit sunset logic
 *   - Cross-Ethnic Working-Class Solidarity: Abstract victim (powerless/trapped) — the potential for class-based organizing across ethnic lines was systematically suppressed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(conquest_of_labor_exclusion, 0.68).
domain_priors:suppression_score(conquest_of_labor_exclusion, 0.72).
domain_priors:theater_ratio(conquest_of_labor_exclusion, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conquest_of_labor_exclusion, extractiveness, 0.68).
narrative_ontology:constraint_metric(conquest_of_labor_exclusion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(conquest_of_labor_exclusion, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(conquest_of_labor_exclusion, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(conquest_of_labor_exclusion, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(conquest_of_labor_exclusion, tangled_rope).
narrative_ontology:human_readable(conquest_of_labor_exclusion, "Conquest of Labor: Exclusion of Arab Workers from Jewish Economic Sector").
narrative_ontology:topic_domain(conquest_of_labor_exclusion, "political_history/nationalism/settler_colonialism").

domain_priors:requires_active_enforcement(conquest_of_labor_exclusion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(conquest_of_labor_exclusion, 'c9953211-725e-4817-9645-2cbc39dcdcd7').
narrative_ontology:cs_kernel_codification('c9953211-725e-4817-9645-2cbc39dcdcd7', distributed).
narrative_ontology:cs_authority_grounding('c9953211-725e-4817-9645-2cbc39dcdcd7', lineage).
narrative_ontology:cs_interpretation_layer_present('c9953211-725e-4817-9645-2cbc39dcdcd7').
narrative_ontology:cs_reading_relation('c9953211-725e-4817-9645-2cbc39dcdcd7', conquest_of_labor_exclusion__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9953211-725e-4817-9645-2cbc39dcdcd7', conquest_of_labor_exclusion__religious_restoration_reading, coexists_with).
narrative_ontology:cs_axiom('c9953211-725e-4817-9645-2cbc39dcdcd7', foundational, jewish_labor_self_sufficiency_imperative).
narrative_ontology:cs_axiom_status(jewish_labor_self_sufficiency_imperative, holdable).
narrative_ontology:cs_axiom_grounding('c9953211-725e-4817-9645-2cbc39dcdcd7', jewish_labor_self_sufficiency_imperative, instrumental).
narrative_ontology:cs_axiom('c9953211-725e-4817-9645-2cbc39dcdcd7', foundational, separate_national_development_necessity).
narrative_ontology:cs_axiom_status(separate_national_development_necessity, holdable).
narrative_ontology:cs_axiom_grounding('c9953211-725e-4817-9645-2cbc39dcdcd7', separate_national_development_necessity, empirically_contingent).
narrative_ontology:cs_axiom('c9953211-725e-4817-9645-2cbc39dcdcd7', secondary, cross_ethnic_solidarity_impossibility).
narrative_ontology:cs_axiom_status(cross_ethnic_solidarity_impossibility, holdable).
narrative_ontology:cs_axiom_grounding('c9953211-725e-4817-9645-2cbc39dcdcd7', cross_ethnic_solidarity_impossibility, empirically_contingent).
narrative_ontology:cs_reference_frame('c9953211-725e-4817-9645-2cbc39dcdcd7', labor_zionist_nation_building_framework).
narrative_ontology:cs_drift_state('c9953211-725e-4817-9645-2cbc39dcdcd7', post_1948_state_institutionalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c9953211-725e-4817-9645-2cbc39dcdcd7', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(conquest_of_labor_exclusion, jewish_labor_movement).
narrative_ontology:constraint_beneficiary(conquest_of_labor_exclusion, histadrut_institutional_apparatus).
narrative_ontology:constraint_beneficiary(conquest_of_labor_exclusion, jewish_agricultural_settlements).
narrative_ontology:constraint_beneficiary(conquest_of_labor_exclusion, zionist_national_institutions).
narrative_ontology:constraint_victim(conquest_of_labor_exclusion, displaced_arab_agricultural_workers).
narrative_ontology:constraint_victim(conquest_of_labor_exclusion, arab_urban_laborers).
narrative_ontology:constraint_victim(conquest_of_labor_exclusion, cross_ethnic_working_class_solidarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED ARAB AGRICULTURAL WORKERS (SNARE) — Trapped by land dispossession and systematic employment exclusion. The 'conquest of labor' policy directly targeted their livelihoods, replacing Arab workers with Jewish immigrants in agricultural settlements and urban enterprises. No exit options: geographic mobility constrained by British Mandate restrictions and Zionist land acquisition patterns; alternative employment systematically foreclosed by Histadrut boycotts and Jewish-only hiring policies. Experienced as pure extraction with minimal coordination function — the policy's stated goal was explicit displacement, not mutual benefit.
constraint_indexing:constraint_classification(conquest_of_labor_exclusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: JEWISH IMMIGRANT WORKERS (TANGLED ROPE) — Constrained by economic precarity and ideological pressure. Benefited from protected employment in Jewish sector but also bore costs: lower wages than Arab workers would accept (the 'Hebrew labor' premium employers paid), ideological policing by Histadrut, and foreclosure of class solidarity across ethnic lines. Mixed experience: genuine coordination (mutual aid, labor organizing, settlement infrastructure) combined with extraction (wage suppression through captive labor market, ideological conformity requirements, complicity in displacement). The constraint coordinated Jewish workers while extracting from both Jewish and Arab working classes.
constraint_indexing:constraint_classification(conquest_of_labor_exclusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HISTADRUT AND ZIONIST NATIONAL INSTITUTIONS (ROPE) — Primary beneficiaries with arbitrage-level exit options. The 'conquest of labor' policy built institutional power: Histadrut controlled employment, housing, healthcare, and education for Jewish workers; Jewish National Fund and Jewish Agency controlled land acquisition and settlement placement. Experienced as coordination: building separate national infrastructure, preventing economic dependency on Arab labor, creating facts on the ground for future statehood. Low effective extraction because these institutions were the extraction's destination, not its source. The policy was their instrument.
constraint_indexing:constraint_classification(conquest_of_labor_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR ZIONIST IDEOLOGICAL VANGUARD (SCAFFOLD) — Organized agents (Second Aliyah pioneers, early kibbutz movement) who saw labor exclusion as temporary necessity for nation-building. Explicit sunset logic: once Jewish demographic majority and economic self-sufficiency were achieved, the exclusionary policy would no longer be necessary. The 'conquest of labor' was framed as transitional — a defensive measure during the vulnerable pre-state period, not a permanent ethno-economic separation. This perspective eroded after 1948 as the policy became institutionalized state practice rather than movement strategy.
constraint_indexing:constraint_classification(conquest_of_labor_exclusion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ARAB URBAN LABORERS (IDENTITY_LOCKED SNARE) — Structurally mobile in principle (could seek employment in Arab-majority cities, Mandate government projects, or British military installations) but identity-locked by class consciousness and anti-colonial solidarity. Saw the 'conquest of labor' as both economic exclusion and betrayal of working-class internationalism. The identity lock: their political identity was constituted through opposition to Zionist colonization, making collaboration with Jewish labor institutions unthinkable even when economically rational. The constraint was experienced as snare because the exclusion was unilateral and coercive, but the identity lock prevented exit strategies that would require accepting Zionist framing.
constraint_indexing:constraint_classification(conquest_of_labor_exclusion, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the 'conquest of labor' exhibits both genuine coordination (building parallel national infrastructure, solving collective action problems for Jewish immigrants) and substantial extraction (systematic displacement of Arab workers, foreclosure of cross-ethnic class solidarity, wage suppression through captive labor markets). The policy coordinated one national group while extracting from another and from the working class as a whole. Tangled rope classification reflects the structural duality: the coordination function was real (not mere cover) but inseparable from the extraction mechanism. The constraint required active enforcement (Histadrut boycotts, JNF land covenants, social pressure) and produced identifiable victims.
constraint_indexing:constraint_classification(conquest_of_labor_exclusion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conquest_of_labor_exclusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(conquest_of_labor_exclusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(conquest_of_labor_exclusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(conquest_of_labor_exclusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(conquest_of_labor_exclusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): Substantial. The policy systematically displaced Arab workers from employment in Jewish-owned enterprises and settlements, transferred economic resources and opportunities to Jewish immigrants, and suppressed cross-ethnic class solidarity that might have challenged both Zionist and British colonial structures. The extraction was not total (Arab workers retained employment in Arab-majority areas and British Mandate projects) but was severe within the Jewish economic sector, which expanded rapidly during the Mandate period. The value reflects that the policy's primary mechanism was exclusion and displacement, not merely differential access. Suppression (0.72): High. Multiple enforcement mechanisms: Histadrut boycotts of employers who hired Arab workers; social pressure and ideological policing within Jewish communities; JNF land covenants prohibiting Arab labor on Jewish-owned land; British Mandate policies that facilitated separate development. Arab workers faced both economic barriers (systematic exclusion) and political barriers (British suppression of Arab labor organizing, particularly after 1936 revolt). Jewish workers who advocated for cross-ethnic solidarity faced ostracism and employment loss. The suppression was structural, not merely informal. Theater ratio (0.35): Moderate-low. The policy's stated functions (employment provision for Jewish immigrants, infrastructure-building for future statehood) were genuine, not performative. The coordination mechanism was real: Histadrut did provide mutual aid, labor organizing, and settlement support for Jewish workers. The theater component reflects the gap between stated 'temporary necessity' framing and actual permanent institutionalization, and the rhetorical cover of 'socialist' labor organizing that masked ethnic exclusion. But the functional content was substantial — this was not primarily a theatrical constraint.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same policy can be experienced as coordination, extraction, or both depending on structural position. Histadrut and Zionist institutions saw successful nation-building coordination (rope) — they were the beneficiaries. Jewish immigrant workers saw mixed coordination and extraction (tangled rope) — protected employment but also wage suppression and ideological constraints. Arab workers saw pure extraction (snare) — systematic exclusion with no coordination benefit. The Labor Zionist vanguard saw temporary necessity with sunset logic (scaffold) — a transitional policy that would end once Jewish demographic and economic self-sufficiency were achieved. The analytical observer sees structural duality (tangled rope) — genuine coordination for one national group inseparable from extraction from another. The perspectival gap is not a disagreement about facts but a structural consequence of different positions relative to the extraction flow. The policy coordinated Jewish workers while displacing Arab workers through the same mechanism — the Histadrut membership restriction that provided mutual aid to Jews was the same restriction that excluded Arabs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Arab agricultural workers are victims with trapped exit → high d → high effective extraction (snare classification). Arab urban laborers are victims with identity_locked exit → high d but modulated by the cognitive lock rather than material barriers. Jewish immigrant workers are beneficiaries (protected employment) but also bear costs (wage suppression, ideological policing) → moderate d → moderate effective extraction (tangled rope). Histadrut and Zionist institutions are primary beneficiaries with arbitrage exit → low d → low/negative effective extraction (rope classification). The Labor Zionist vanguard are organized agents with mobile exit and mixed beneficiary status → low-moderate d → low effective extraction (scaffold classification, driven by sunset logic rather than high extraction). The analytical observer has analytical exit and sees the full structure → d derived from the constraint's objective properties rather than positional experience. The directionality computation captures that the same policy extracts from some agents while coordinating others, and the experienced extraction depends on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that tangled_rope is the correct analytical classification when a policy exhibits both genuine coordination (for one group) and substantial extraction (from another group) through inseparable mechanisms. The 'conquest of labor' was not pure coordination (rope) because it systematically displaced Arab workers and suppressed cross-ethnic solidarity. It was not pure extraction (snare) because it genuinely coordinated Jewish workers and built functional infrastructure. The coordination and extraction were structurally linked: the same Histadrut membership that provided mutual aid to Jewish workers was the mechanism that excluded Arab workers. The policy required active enforcement (boycotts, land covenants, social pressure) and produced identifiable victims (displaced Arab workers, suppressed class solidarity). The tangled_rope classification captures this duality without collapsing it into either pure coordination or pure extraction. The perspectival variation (snare from Arab workers, rope from Zionist institutions, scaffold from Labor Zionist vanguard) reflects genuine differences in structural position, not measurement error.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_primacy,
    'Was the ''conquest of labor'' primarily a coordination mechanism for Jewish nation-building that incidentally displaced Arab workers, or primarily an extraction mechanism that used nation-building rhetoric as cover?',
    'Historical analysis of policy formation: Were Arab workers excluded because Jewish workers needed coordination infrastructure, or were Jewish workers organized to justify Arab exclusion? Counterfactual: Would the policy have been adopted if Arab workers had been willing to join Histadrut and accept Zionist framing?',
    'If coordination-primary: Tangled Rope from analytical perspective is correct — genuine dual function. If extraction-primary: Snare from analytical perspective — the coordination story is cover for displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_primacy, conceptual, 'Whether coordination or extraction was the policy''s primary structural function').

omega_variable(
    sunset_clause_sincerity,
    'Did the Labor Zionist vanguard genuinely intend the exclusionary policy as temporary (scaffold logic), or was the ''temporary necessity'' framing a legitimation strategy for permanent ethno-economic separation?',
    'Textual analysis of internal Histadrut and Mapai documents 1920s-1940s; comparison of stated sunset conditions with actual policy persistence after those conditions were met (Jewish demographic majority achieved by 1948, economic self-sufficiency by 1960s); examination of post-1948 policy continuity.',
    'If sincere: Scaffold perspective validated — the policy was meant to sunset but became institutionalized (scaffold-to-piton drift). If insincere: Scaffold perspective is retrospective legitimation — the policy was always intended as permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_sincerity, empirical, 'Whether the scaffold framing reflected genuine intent or legitimation strategy').

omega_variable(
    class_solidarity_counterfactual,
    'Could cross-ethnic working-class solidarity have emerged if the ''conquest of labor'' policy had not been enforced, or were national/colonial divisions already insurmountable by the 1920s?',
    'Historical analysis of early cross-ethnic labor organizing attempts (1920s railway strikes, Haifa port workers); comparison with other colonial contexts where cross-ethnic class solidarity did/did not emerge; examination of British Mandate divide-and-rule strategies.',
    'If solidarity was possible: The policy''s suppression of cross-ethnic organizing was a contingent choice, and the ''victim'' status of working-class solidarity is structurally valid. If solidarity was impossible: The policy formalized an already-existing division, and the coordination function was more significant than the extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(class_solidarity_counterfactual, empirical, 'Whether cross-ethnic class solidarity was structurally possible in Mandate Palestine').

omega_variable(
    natural_law_vs_constructed_separation,
    'Is ethno-economic separation in a settler-colonial context a natural consequence of demographic engineering and land dispossession (mountain from some perspectives), or a contingent policy choice that required active enforcement (tangled_rope/snare)?',
    'Comparative analysis: Do all settler-colonial contexts produce labor exclusion policies, or only those with specific ideological commitments (Labor Zionism''s ''Hebrew labor'' doctrine)? Examination of enforcement mechanisms: How much active suppression was required to maintain the separation?',
    'If natural consequence: Some perspectives (particularly those naturalizing settler-colonial dynamics) would classify as mountain. If contingent policy: The analytical tangled_rope classification is correct — the separation required construction and maintenance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_separation, conceptual, 'Whether labor exclusion was natural consequence or constructed policy').

omega_variable(
    reading_relation_foreclosure,
    'Does the national_liberation_reading logically foreclose the settler_colonial_reading''s classification of this constraint, or can both readings coexist as competing interpretive frameworks?',
    'Logical analysis: Can a single coherent framework hold both ''defensive nation-building coordination'' (national_liberation) and ''extractive colonial displacement'' (settler_colonial) as descriptions of the same policy? Or does adopting one reading commit the observer to rejecting the other''s core premises?',
    'If foreclosure: The readings are mutually exclusive within any single analytical framework, and the constraint''s classification depends entirely on which kernel reading is adopted. If coexistence: Both readings can be held simultaneously as perspectival truths, and the constraint exhibits both coordination and extraction depending on observation position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_foreclosure, conceptual, 'Whether the two primary kernel readings foreclose each other or coexist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(conquest_of_labor_exclusion, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(col_excl_theater_1920, conquest_of_labor_exclusion, theater_ratio, 0, 0.25).
narrative_ontology:measurement(col_excl_theater_1928, conquest_of_labor_exclusion, theater_ratio, 8, 0.3).
narrative_ontology:measurement(col_excl_theater_1936, conquest_of_labor_exclusion, theater_ratio, 16, 0.35).
narrative_ontology:measurement(col_excl_theater_1948, conquest_of_labor_exclusion, theater_ratio, 28, 0.35).

% Extraction over time
narrative_ontology:measurement(col_excl_extract_1920, conquest_of_labor_exclusion, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(col_excl_extract_1928, conquest_of_labor_exclusion, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(col_excl_extract_1936, conquest_of_labor_exclusion, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(col_excl_extract_1948, conquest_of_labor_exclusion, base_extractiveness, 28, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(col_excl_suppress_1920, conquest_of_labor_exclusion, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(col_excl_suppress_1928, conquest_of_labor_exclusion, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(col_excl_suppress_1936, conquest_of_labor_exclusion, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(col_excl_suppress_1948, conquest_of_labor_exclusion, suppression_requirement, 28, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(conquest_of_labor_exclusion, resource_allocation).
narrative_ontology:affects_constraint(conquest_of_labor_exclusion, land_acquisition_displacement).
narrative_ontology:affects_constraint(conquest_of_labor_exclusion, histadrut_institutional_monopoly).
narrative_ontology:affects_constraint(conquest_of_labor_exclusion, id_1948_palestinian_exodus).

% DUAL FORMULATION NOTE:
% The 'conquest of labor' is one component of a larger constraint family around Zionist settlement and Palestinian displacement. Related constraints include land acquisition policies (JNF purchases, Ottoman/Mandate land laws), Histadrut's institutional monopoly over Jewish labor organizing, and the 1948 Palestinian exodus. Each has its own extractiveness value reflecting different mechanisms and time periods, but they form a network where each constraint's operation affected the others. The labor exclusion policy created economic facts on the ground that shaped later displacement; conversely, land dispossession created the conditions for labor exclusion by removing Arab agricultural workers from Jewish-owned land.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
