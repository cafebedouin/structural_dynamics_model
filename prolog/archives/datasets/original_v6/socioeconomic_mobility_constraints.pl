% ============================================================================
% CONSTRAINT STORY: socioeconomic_mobility_constraints
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_socioeconomic_mobility_constraints, []).

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
 *   constraint_id: socioeconomic_mobility_constraints
 *   human_readable: Socioeconomic Mobility Constraints
 *   domain: economic/social/institutional
 *
 * SUMMARY:
 *   Socioeconomic mobility constraints operate as a hybrid
 *   coordination-extraction system that enables and restricts upward mobility
 *   simultaneously. The constraint exhibits genuine coordination function:
 *   educational institutions sort by ability, credentials signal competence
 *   to employers, and family wealth transmission coordinates
 *   intergenerational stability. Yet these coordination functions are layered
 *   with substantial extraction: wealth-incumbent families capture
 *   disproportionate advantage, credential gatekeeping restricts access
 *   independent of ability, and structural barriers (inadequate school
 *   funding, unequal social capital) prevent many capable individuals from
 *   participating. The constraint's theater_ratio (0.55) reflects the gap
 *   between the public narrative of meritocratic advancement and the private
 *   reality of inherited advantage. The extractiveness value (0.58) indicates
 *   moderate-to-high extraction: the system captures significant resources
 *   (credential costs, opportunity costs, wealth concentration) from
 *   non-beneficiary groups. Different institutional actors experience this
 *   constraint through radically different perspectival lenses, from pure
 *   rope (wealth incumbents) to pure snare (low-income households trapped
 *   without exit).
 *
 * KEY AGENTS:
 *   - Low-Income Households: Primary victim (powerless/trapped) — material barriers to credential acquisition, inadequate school funding, lack of social capital networks
 *   - First-Generation Entrants: Secondary victim (moderate/constrained) — high costs of degree acquisition, social friction, labor burden; also benefits from credential signaling
 *   - Wealth-Incumbent Families: Primary beneficiary (institutional/arbitrage) — capitalize on inherited advantage, social capital, network access, legacy privilege
 *   - Educational Institutions: Institutional gatekeeper (institutional/arbitrage) — manage credential allocation; benefit from monopoly position on degree signaling
 *   - Upwardly Mobile Professionals: Successful navigators (powerful/mobile) — experienced extraction costs but achieved mobility; now enjoy beneficiary status
 *   - Reform Coalition: Organized agents (organized/constrained) — nonprofits, policymakers, alternative credentialing systems building parallel pathways with sunset logic
 *   - Legacy Admissions System: Institutional mechanism (institutional/arbitrage) — theater-driven arrangement persisting through inertia despite degraded merit function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choices as economic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(socioeconomic_mobility_constraints, 0.58).
domain_priors:suppression_score(socioeconomic_mobility_constraints, 0.68).
domain_priors:theater_ratio(socioeconomic_mobility_constraints, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(socioeconomic_mobility_constraints, extractiveness, 0.58).
narrative_ontology:constraint_metric(socioeconomic_mobility_constraints, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(socioeconomic_mobility_constraints, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(socioeconomic_mobility_constraints, tangled_rope).
narrative_ontology:human_readable(socioeconomic_mobility_constraints, "Socioeconomic Mobility Constraints").
narrative_ontology:topic_domain(socioeconomic_mobility_constraints, "economic/social/institutional").

domain_priors:requires_active_enforcement(socioeconomic_mobility_constraints).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(socioeconomic_mobility_constraints, wealth_incumbent_families).
narrative_ontology:constraint_beneficiary(socioeconomic_mobility_constraints, educational_gatekeepers).
narrative_ontology:constraint_beneficiary(socioeconomic_mobility_constraints, credential_issuers).
narrative_ontology:constraint_victim(socioeconomic_mobility_constraints, low_income_households).
narrative_ontology:constraint_victim(socioeconomic_mobility_constraints, first_generation_entrants).
narrative_ontology:constraint_victim(socioeconomic_mobility_constraints, marginalized_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME HOUSEHOLD (SNARE) — Trapped by material barriers: inadequate school funding, lack of social capital, cost of credential acquisition, housing instability. No exit path; bears full extraction cost of the system. Maximum suppression from lack of alternatives and behavioral constraints learned through poverty.
constraint_indexing:constraint_classification(socioeconomic_mobility_constraints, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIRST-GENERATION ENTRANT (TANGLED ROPE) — Constrained by high costs (debt burden, forgone income, social friction), but gains from educational access and credential signaling. System both enables upward mobility and extracts significant tuition/labor costs. Genuine coordination function (credential verification) layered with asymmetric extraction.
constraint_indexing:constraint_classification(socioeconomic_mobility_constraints, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: WEALTH-INCUMBENT FAMILY (ROPE) — Benefits from legacy advantage, inherited social capital, network access. Experiences the constraint as pure coordination: passing wealth, status, and opportunity to children is a shared cultural/family coordination mechanism. Low experienced extraction; net beneficiary.
constraint_indexing:constraint_classification(socioeconomic_mobility_constraints, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EDUCATIONAL GATEKEEPER (ROPE) — Institutional actor managing credential allocation. From this perspective, the system is coordination: sorting students by academic capacity, maintaining educational standards, signaling quality to employers. Benefits from credential monopoly but frames extraction as legitimate filtering. Low perceived extraction.
constraint_indexing:constraint_classification(socioeconomic_mobility_constraints, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: UPWARDLY MOBILE PROFESSIONAL (TANGLED ROPE) — Successfully navigated the constraint but experienced significant extraction costs (student debt, credential burnout, cultural assimilation pressure). Now enjoys mobility benefits. Perceives genuine coordination value (merit-based advancement) but recalls extraction burden. Moderate chi reflecting mixed experience.
constraint_indexing:constraint_classification(socioeconomic_mobility_constraints, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: REFORM COALITION (SCAFFOLD) — Organized agents (social mobility nonprofits, equity-focused policymakers, alternative credentialing systems) see mobility barriers as temporary institutional failures remediable through sunset-clause interventions: free/subsidized college, skills-based hiring, direct-to-work pathways. These alternatives are building in parallel to traditional credential systems. Suppression is declining for this perspective as alternatives mature.
constraint_indexing:constraint_classification(socioeconomic_mobility_constraints, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: LEGACY ADMISSIONS (PITON) — Theater-driven institutional arrangement persisting through inertia despite degraded coordination function. Nominally sorts students but actually preserves wealth dynasty. Maintained by institutional actors (universities, wealthy families) who benefit from its opacity. Theater ratio driven by the gap between public merit-based narratives and private legacy-advantage mechanisms.
constraint_indexing:constraint_classification(socioeconomic_mobility_constraints, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, some mobility stratification appears immutable: scarcity of premium positions, information asymmetries about career pathways, and biological/cognitive variation create inherent inequality. This perspective risks naturalizing contingent institutional choices (credential monopoly, legacy advantage, unequal school funding) as laws of economics. Engine should flag this as false summit.
constraint_indexing:constraint_classification(socioeconomic_mobility_constraints, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(socioeconomic_mobility_constraints_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(socioeconomic_mobility_constraints, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(socioeconomic_mobility_constraints, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(socioeconomic_mobility_constraints, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(socioeconomic_mobility_constraints, TR),
    TR >= 0.70.

:- end_tests(socioeconomic_mobility_constraints_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant resources from non-beneficiaries through credential costs (tuition, debt burden, opportunity cost), structural barriers (unequal school funding, lack of social capital), and wealth concentration (inherited advantage). The value reflects that extraction is substantial but not total — many individuals do achieve upward mobility despite barriers, and the system has genuine coordination function. Theater ratio (0.55): Moderate. The gap between meritocratic narrative and inherited-advantage reality drives theater. Public discourse emphasizes equal opportunity and merit-based advancement; private mechanisms emphasize legacy privilege, network gatekeeping, and credential monopoly. Theater has risen over the measurement interval as credential inflation has made the gap between rhetoric and reality more obvious. Suppression (0.68): High. Multiple reinforcing suppression mechanisms: inadequate school funding limits early capability development; lack of social capital creates information barriers; cost of credential acquisition creates financial dependency; career risk and social friction for non-traditional entrants; behavioral internalization of low expectations. However, suppression is not total — reform pathways (subsidized education, skills-based hiring, alternative credentials) are reducing barriers.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The wealth incumbent sees pure rope: family wealth transmission is coordination, educational access is cooperation. The low-income household sees pure snare: barriers are insurmountable, extraction is total. The reform coalition sees scaffold: alternative pathways are under construction, sunset is visible. The educational institution sees rope: credential verification is coordination, quality assurance justifies gatekeeping. The upwardly mobile professional sees tangled rope: experienced extraction costs (debt, cultural assimilation) alongside genuine mobility gain. The legacy admissions system is piton: theater-driven, institutionally maintained despite degraded function. The analytical observer risks false summit (mountain): viewing mobility stratification as immutable economic law rather than contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from each agent's structural position and exit capacity. Wealth incumbents with arbitrage options experience low d (beneficiaries with mobility): derived d ≈ 0.10-0.20, producing negative/minimal chi. Low-income households trapped without exit experience high d (victims without escape): derived d ≈ 0.90-0.95, producing maximum chi. First-generation entrants with constrained exit (high costs, career friction, identity friction) experience moderate-high d ≈ 0.65-0.75, producing substantial chi. Educational institutions with arbitrage (can shift credential standards, set tuition, maintain monopoly) experience low d despite victim framing: derived d ≈ 0.25-0.35. The reform coalition's constrained exit (can build alternatives but faces institutional resistance) produces d ≈ 0.50-0.60, supporting scaffold classification (moderate extraction, genuine exit path visible).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that socioeconomic mobility constraints ARE a legitimate tangled_rope: they coordinate credential verification and educational sorting (genuine coordination function) WHILE extracting significant resources and restricting access (asymmetric extraction). The temptation is to classify as pure snare (all extraction) or pure rope (all coordination) — but the structural data supports both functions coexisting. The false summit risk (mountain perspective) arises from naturalizing the contingent institutional choice of credential-monopoly gatekeeping as an inherent economic law. The constraint is not immutable — alternative credentialing (skills-based hiring, apprenticeships) demonstrate that credential monopoly is institutional choice, not natural law. Mandatrophy is resolved by declaring: (1) both coordination and extraction are structurally present, (2) the relative weight is empirically measurable, (3) alternative institutional arrangements exist that reduce extraction while maintaining coordination, and (4) the system's persistence despite alternatives suggests institutional inertia (piton dynamics) layered onto tangled_rope structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_necessity_threshold,
    'What fraction of economic mobility genuinely requires formal credentialing vs. is artificially restricted by credential gatekeeping?',
    'Comparison of credential requirements vs. actual job performance data; analysis of fields with and without credential barriers; international comparison of mobility rates with varying credentialing strictness',
    'If > 60% artificially restricted: snare classification strengthened (extraction mechanism). If < 40%: rope classification strengthened (genuine coordination). Threshold determines whether the system primarily filters or primarily restricts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_necessity_threshold, empirical, 'Proportion of mobility restriction attributable to credential gatekeeping vs. true skill sorting').

omega_variable(
    social_capital_inheritance_mechanism,
    'Is social capital inheritance a coordination benefit (legitimate family role) or an extraction mechanism (unearned advantage)?',
    'Decompose social capital value into legitimately shared components (mentorship, emotional support) vs. rent-capturing components (network gatekeeping, opportunity hoarding). Measure intergenerational correlation of outcomes within families vs. across demographic groups.',
    'If primarily coordination: beneficiary perspective is legitimate rope (family coordination). If primarily extraction: beneficiary perspective masks snare dynamics and requires disaggregation into separate constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_capital_inheritance_mechanism, conceptual, 'Whether inherited social capital represents coordination or extraction').

omega_variable(
    alternative_credential_sufficiency,
    'Do alternative credentialing systems (bootcamps, skills-based hiring, apprenticeships) provide equivalent signaling and actual capability development compared to traditional degree pathways?',
    'Longitudinal tracking of employment, wage, and skill outcomes for cohorts credentialed through different pathways; employer hiring patterns and retention rates; measurement of skill transfer and adaptability',
    'If equivalent: scaffold perspective confirmed (alternative pathways mature, sunset for traditional credential monopoly). If inferior: scaffold is aspirational; traditional constraint persists structurally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credential_sufficiency, empirical, 'Whether alternative credentialing systems can replace traditional degree pathways').

omega_variable(
    identity_lock_internalization_depth,
    'To what extent do low-income agents internalize the framing that they are ''less capable'' or ''don''t belong'' in high-status educational/economic spaces, independent of actual structural barriers?',
    'Psychological research on stereotype threat, impostor syndrome, internalized classism; measurement of behavioral changes when structural barriers are removed but identity frames persist; analysis of exit patterns for agents with actual mobility capacity',
    'If high internalization: many agents classified as trapped are actually identity_locked; constraint persists even after material barriers are addressed (Rope-to-Mountain misclassification risk). If low: trapped classification is accurate and reflects genuine structural barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization_depth, empirical, 'Depth of identity-based internalization of mobility constraints independent of structural barriers').

omega_variable(
    geographic_scope_variation,
    'Does socioeconomic mobility constraint operate identically across local/regional/national/global scopes, or do effective extraction and suppression vary significantly by geography?',
    'Comparative analysis of mobility rates, credential requirements, and wealth transmission efficiency across urban/rural, regional, and international contexts; identification of jurisdictions with markedly different constraint structures',
    'If uniform: national scope is accurate. If highly variable: constraint should decompose into regional/local stories with different epsilon values. Affects scope modifier σ(S) computation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geographic_scope_variation, empirical, 'Whether socioeconomic mobility constraints vary by geographic scope').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(socioeconomic_mobility_constraints, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(socmob_tr_t0, socioeconomic_mobility_constraints, theater_ratio, 0, 0.48).
narrative_ontology:measurement(socmob_tr_t15, socioeconomic_mobility_constraints, theater_ratio, 15, 0.52).
narrative_ontology:measurement(socmob_tr_t30, socioeconomic_mobility_constraints, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(socmob_be_t0, socioeconomic_mobility_constraints, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(socmob_be_t15, socioeconomic_mobility_constraints, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(socmob_be_t30, socioeconomic_mobility_constraints, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(socioeconomic_mobility_constraints, resource_allocation).
narrative_ontology:affects_constraint(socioeconomic_mobility_constraints, intergenerational_wealth_transmission).
narrative_ontology:affects_constraint(socioeconomic_mobility_constraints, educational_credential_inflation).
narrative_ontology:affects_constraint(socioeconomic_mobility_constraints, labor_market_signaling).

% DUAL FORMULATION NOTE:
% Socioeconomic mobility constraints decompose into three structurally distinct constraint families: (1) intergenerational_wealth_transmission (ε=0.45, Tangled Rope) — capital accumulation across generations with asymmetric inheritance; (2) educational_credential_inflation (ε=0.52, Tangled Rope) — credential requirement escalation as signaling mechanism; (3) labor_market_signaling (ε=0.38, Rope) — credentialing as pure coordination mechanism for skill verification. This story treats socioeconomic mobility constraints as the aggregate structural pressure created by these three mechanisms operating in combination. Each downstream story has different epsilon and different beneficiary/victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(socioeconomic_mobility_constraints, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
