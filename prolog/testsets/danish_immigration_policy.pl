% ============================================================================
% CONSTRAINT STORY: danish_immigration_policy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_danish_immigration_policy, []).

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
 *   constraint_id: danish_immigration_policy
 *   human_readable: Danish Immigration Policy Constraint
 *   domain: political_economy/immigration/governance
 *
 * SUMMARY:
 *   Danish immigration policy exhibits a structural constraint that
 *   simultaneously coordinates labor market access and extracts from asylum
 *   seekers and immigrant populations through legal status restrictions,
 *   family separation requirements, and integration barriers. The policy
 *   framework creates genuine coordination for employers and high-skill
 *   workers (efficient labor matching through point-based systems) while
 *   imposing maximum suppression on vulnerable populations (indefinite
 *   processing, material dependency, behavioral surveillance). The
 *   constraint's theater ratio (0.64) reflects that public justification
 *   centers on welfare protection and security, yet empirical analysis shows
 *   net fiscal contribution from migrants and minimal security gains from
 *   restrictions. The extractiveness trajectory shows steady accumulation:
 *   early restrictions (0.38 at t=0) have expanded through policy layering to
 *   current severity (0.58), with theater increasing alongside extractiveness
 *   — a diagnostic signal that the justification narrative is being
 *   ritualized rather than empirically validated. The constraint exhibits all
 *   six classification types across perspectives, making it a diagnostic
 *   exemplar of how the same policy structure appears as pure extraction to
 *   powerless agents, coordination to beneficiary institutions, and welfare
 *   gatekeeping mythology to the general public.
 *
 * KEY AGENTS:
 *   - Asylum Seekers and Refugees: Primary victim (powerless/trapped) — bears maximum extraction through indefinite processing, material dependency, family separation, and zero legal agency
 *   - Non-Western Immigrant Communities: Secondary victim (moderate/constrained) — face licensing restrictions, housing discrimination, employment barriers, and documented behavioral surveillance; also benefit from labor market access and welfare universalism once status secured
 *   - Danish Labor Market Employers: Primary beneficiary (institutional/arbitrage) — control labor supply through point-based systems, avoid domestic wage pressure, access skilled migrants on favorable terms
 *   - Danish Taxpayers and Welfare Beneficiaries: Secondary beneficiary (powerful/mobile) — benefit from restricted welfare access for immigrants; narrative protection through welfare-preservation framing
 *   - Integration Administration and Bureaucracy: Institutional mediator (institutional/arbitrage) — manages constraint through administrative systems; benefits from policy complexity and discretionary enforcement
 *   - EU Legal and Border System: Inter-institutional actor (institutional/constrained) — coordinates with Denmark on burden-shifting (Dublin Regulation externalities) while constraining unilateral restriction capacity
 *   - Analytical Observer: Global justice perspective (analytical/analytical) — sees through welfare-protection framing and identifies global extraction mechanism concentrating on poorest populations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(danish_immigration_policy, 0.58).
domain_priors:suppression_score(danish_immigration_policy, 0.68).
domain_priors:theater_ratio(danish_immigration_policy, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(danish_immigration_policy, extractiveness, 0.58).
narrative_ontology:constraint_metric(danish_immigration_policy, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(danish_immigration_policy, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(danish_immigration_policy, tangled_rope).
narrative_ontology:human_readable(danish_immigration_policy, "Danish Immigration Policy Constraint").
narrative_ontology:topic_domain(danish_immigration_policy, "political_economy/immigration/governance").

domain_priors:requires_active_enforcement(danish_immigration_policy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(danish_immigration_policy, danish_labor_market_employers).
narrative_ontology:constraint_beneficiary(danish_immigration_policy, danish_tax_base_beneficiaries).
narrative_ontology:constraint_beneficiary(danish_immigration_policy, danish_welfare_system_beneficiaries).
narrative_ontology:constraint_victim(danish_immigration_policy, asylum_seekers_and_refugees).
narrative_ontology:constraint_victim(danish_immigration_policy, immigrant_communities).
narrative_ontology:constraint_victim(danish_immigration_policy, non_western_migrants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASYLUM SEEKER (SNARE) — Trapped by legal status, geographic restriction, and material dependency. Cannot work, cannot exit, cannot influence policy. Bears full extraction cost through family separation requirements, integration barriers, and indefinite processing delays. Maximum experienced extraction with zero agency.
constraint_indexing:constraint_classification(danish_immigration_policy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IMMIGRANT COMMUNITY (TANGLED ROPE) — Constrained by licensing restrictions, language requirements, housing discrimination, and documented surveillance of non-Western minorities. Also benefits from labor market access, welfare universalism, and educational opportunities once legal status is secured. Mixed experience: genuine coordination (labor market integration) with asymmetric extraction (behavioral policing, family unity restrictions).
constraint_indexing:constraint_classification(danish_immigration_policy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DANISH LABOR MARKET (ROPE) — Benefits from controlled access to migrant labor. Experiences policy as coordination mechanism: point-based systems and skills matching enable efficient allocation without domestic labor shortage. Net beneficiary — extraction runs toward employers and high-skill sectors, policy is designed to serve their interests.
constraint_indexing:constraint_classification(danish_immigration_policy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTEGRATION ADMINISTRATION (SCAFFOLD) — Organized bureaucratic apparatus sees immigration policy as temporary coordination structure with sunset clauses built into integration programs and refugee quotas. Experiences enforcement as a manageable administrative burden with declining theater as digital ID systems and integration metrics replace manual case management. However, sunset never materializes — quotas reset annually, new restrictions layer atop old ones, suggesting piton degradation.
constraint_indexing:constraint_classification(danish_immigration_policy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WELFARE GATEKEEPING MYTH (PITON) — The narrative that 'welfare costs require immigration control' is largely performative. Studies show net fiscal contribution from working-age migrants; welfare spending per capita on immigrants is lower than on Danish-born citizens. Yet the welfare-protection framing persists as a public justification layer (theater = 0.78) despite contradictory evidence. The constraint maintains itself through ritualized policy reviews, media amplification of extreme cases, and institutional resistance to contradictory data.
constraint_indexing:constraint_classification(danish_immigration_policy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EU AND INTERNATIONAL LAW (TANGLED ROPE) — EU legal frameworks (Dublin Regulation, family reunification directives) create coordination obligations while allowing Denmark opt-outs and derogations. Denmark benefits from labor mobility within EU while restricting third-country migration. Experiences constraint as hybrid: genuine coordination on internal mobility with asymmetric extraction through EU burden-shifting (Dublin means border states like Italy absorb asylum applications).
constraint_indexing:constraint_classification(danish_immigration_policy, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a global justice perspective, Danish immigration policy exhibits pure extraction masked as security/welfare protection. The constraint serves rich-nation interests (labor supply control, welfare preservation) while imposing maximum costs on poor-nation populations (family separation, asylum processing delays, climate refugee exclusion). The analytical view sees through the welfare-protection framing and identifies the constraint as global extraction without meaningful coordination function for the targets.
constraint_indexing:constraint_classification(danish_immigration_policy, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(danish_immigration_policy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(danish_immigration_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(danish_immigration_policy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(danish_immigration_policy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(danish_immigration_policy, TR),
    TR >= 0.70.

:- end_tests(danish_immigration_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The policy extracts labor value from migrants (suppressed wages through skill devaluation, credential non-recognition), material security from asylum seekers (indefinite processing delays, family separation), and dignity/autonomy from all non-Western migrants (behavioral surveillance, integration requirements framed as 'culture fit'). The extraction is substantial but not totalizing — Denmark's welfare universalism and labor market access do provide real benefits to immigrants once status is secured. Suppression (0.68): High. Barriers are structural and multiple: indefinite legal status, family separation requirements, housing segregation, licensing restrictions, employment discrimination, and documented heightened enforcement against non-Western minorities. These barriers are active (requires ongoing enforcement) not passive. Theater ratio (0.64): Moderate-high. Public justification emphasizes welfare protection and cultural integration, but empirical analysis reveals net fiscal contribution and minimal integration gaps compared to other OECD nations. The welfare-gatekeeping narrative persists despite contradictory evidence — a signal that the theater is performing policy legitimacy rather than explaining necessity. Temporal trajectory shows theater increasing (0.52→0.68) as restrictions accumulate — diagnostic signal of Goodhart drift. Claimed type (Tangled Rope) reflects that genuine labor coordination (employer benefit) is layered with asymmetric extraction (target cost). The constraint requires active enforcement (point-system administration, integration monitoring, deportation capacity) — structural marker of Tangled Rope vs pure Rope.
 *
 * PERSPECTIVAL GAP:
 *   Perspectival gap between beneficiary institutions and victim populations is maximal and driven by asymmetric legal status and exit capacity. Employers see labor market coordination (Rope); asylum seekers see indefinite legal trap with forced family separation (Snare). Immigrant communities experience mixed outcomes: tangible benefits (work access, education, welfare universalism) layered with tangible restrictions (licensing limits, housing segregation, behavioral surveillance). The gap reflects genuine structural asymmetry — the policy is designed to benefit one population and extract from another. The analytical observer can view this as either domestic coordination (Rope) or global extraction (Snare) depending on whether they adopt the beneficiary's frame (wealthy-nation labor market logic) or the victim's frame (poor-nation mobility restriction). The welfare-protection narrative creates a third perspectival layer: the general public may believe the policy serves collective welfare, while immigrants experience it as discrimination, and employers benefit while claiming coordination necessity. This narrative-structural desynchronization is itself diagnostic — it suggests the constraint's extraction mechanism depends on public misunderstanding of its true function.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position relative to extraction flow. Asylum seekers (victim + trapped → d ≈ 0.95) experience maximum f(d) ≈ 1.42. Immigrant communities (victim + constrained → d ≈ 0.82) experience high f(d) ≈ 1.08. Employers (beneficiary + arbitrage → d ≈ 0.12) experience low f(d) ≈ 0.00. The asymmetry is structural: policy design concentrates benefits on employers (legal certainty, labor supply control) and distributes costs on migrants (indefinite uncertainty, legal restrictions). This is not accidental but intentional — the policy is written to extract from migrants and deliver to employers. The EU perspective (institutional + constrained → d ≈ 0.58) experiences moderate extraction through burden-shifting mechanisms embedded in Dublin Regulation. The analytical observer (analytical exit → d ≈ 0.72) sees the global structure: wealthy-nation extraction from poor-nation populations through mobility restriction. Directionality overrides are not needed — the structural data (beneficiary/victim declarations + power + exit options) produces accurate d values that explain the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that different institutional levels and perspectives produce legitimately different classifications from identical base properties. The employer sees Rope (labor market coordination) — their experience is factually accurate within their institutional frame. The asylum seeker sees Snare (pure extraction with zero agency) — their experience is factually accurate within their structural position. The analyst sees Tangled Rope or Snare depending on whether they focus on domestic coordination (labor market function) or global extraction (mobility restriction for poor-nation populations). The welfare-protection narrative (Piton of mythologized justification) is factually falsifiable through fiscal analysis but socially persistent. No single classification is 'the' answer. The constraint IS a Tangled Rope at the domestic level (labor coordination + extraction) AND a Snare at the global level (mobility restriction targeting poor populations). The perspectival plurality is not failure of the classification system but correct diagnosis of how institutional structures create different realities for different agents. The mandatrophy resolution: this constraint coordinates labor markets for wealthy nations while extracting from mobile poor populations — simultaneously Rope (domestic) and Snare (global).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_fiscal_impact_threshold,
    'What is the actual net fiscal impact of immigration on the Danish welfare system, and does it justify the suppression level embedded in policy?',
    'Longitudinal fiscal analysis controlling for demographic structure and tax contributions; comparison of lifetime fiscal impact by migration cohort and skill level',
    'If net positive: welfare-protection framing is false justification, suppression is revealed as xenophobic rather than fiscally rational, extraction classification strengthens. If net negative: suppression is partially justified, constraint shifts toward Rope from higher-income perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_fiscal_impact_threshold, empirical, 'Net fiscal impact of immigration on welfare system').

omega_variable(
    integration_success_metric_manipulation,
    'Are published integration success metrics measuring genuine outcomes or performing compliance with policy objectives?',
    'Comparison of official integration metrics with independent educational attainment, employment stability, wage trajectory, and social inclusion measures; analysis of metric definition changes correlating with policy changes',
    'If metrics are gaming outputs: theater_ratio should be higher (0.75+), constraint should classify as Piton from administration perspective. If metrics are valid: suppression justification is stronger, constraint shifts toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_success_metric_manipulation, empirical, 'Whether integration metrics measure genuine outcomes or policy compliance').

omega_variable(
    extraction_beneficiary_concentration,
    'Is extraction concentrated on specific demographic targets (non-Western, Muslim identity) or dispersed across all immigrant populations?',
    'Differential policy impact analysis: enforcement intensity, family reunification approval rates, housing access, employment discrimination complaints by origin country and religious affiliation',
    'If highly concentrated on non-Western minorities: classification shifts toward coordinated discrimination (Snare from multiple perspectives). If dispersed: suppression is economically rational rather than discriminatory, constraint shifts toward Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_beneficiary_concentration, empirical, 'Whether extraction targets are concentrated or dispersed across immigrant populations').

omega_variable(
    family_separation_sustainability,
    'Is the family separation requirement (''no welfare benefits for family members in origin country'') enforced or symbolically maintained?',
    'Compliance audit: percentage of policy violations detected vs prosecuted; longitudinal tracking of family reunification despite formal restrictions; investigation of enforcement variation by political party in office',
    'If strictly enforced: suppression is maximal (0.78+), extraction is structural and severe. If loosely enforced: constraint is largely performative (Piton), theater ratio should be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_separation_sustainability, empirical, 'Enforcement intensity of family separation policy').

omega_variable(
    alternative_policy_path_dependency,
    'Are the restrictions path-dependent on early policy choices or structurally necessary for Danish labor market function?',
    'Comparative analysis with other high-wage nations achieving similar labor market outcomes with lower suppression; historical analysis of policy decision points where less restrictive alternatives existed',
    'If path-dependent: constraint is political choice rather than natural law, extractiveness classification is strengthened. If structurally necessary: constraint shifts toward Rope, suppression is legitimate coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_policy_path_dependency, conceptual, 'Whether suppression is path-dependent or structurally necessary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(danish_immigration_policy, 0, 21).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dip_tr_t0, danish_immigration_policy, theater_ratio, 0, 0.52).
narrative_ontology:measurement(dip_tr_t7, danish_immigration_policy, theater_ratio, 7, 0.6).
narrative_ontology:measurement(dip_tr_t14, danish_immigration_policy, theater_ratio, 14, 0.64).
narrative_ontology:measurement(dip_tr_t21, danish_immigration_policy, theater_ratio, 21, 0.68).

% Extraction over time
narrative_ontology:measurement(dip_be_t0, danish_immigration_policy, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dip_be_t7, danish_immigration_policy, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(dip_be_t14, danish_immigration_policy, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(dip_be_t21, danish_immigration_policy, base_extractiveness, 21, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(danish_immigration_policy, resource_allocation).
narrative_ontology:boltzmann_floor_override(danish_immigration_policy, 0.18).
narrative_ontology:affects_constraint(danish_immigration_policy, eu_burden_shifting_asylum).
narrative_ontology:affects_constraint(danish_immigration_policy, nordic_welfare_gatekeeping).
narrative_ontology:affects_constraint(danish_immigration_policy, labor_market_credential_devaluation).

% DUAL FORMULATION NOTE:
% Danish immigration policy decomposes into separate constraints with different extractiveness values: (1) labor market coordination mechanism (ε ≈ 0.25, Rope) focused on efficient skills matching, (2) asylum processing extraction (ε ≈ 0.68, Snare) focused on indefinite status control, (3) welfare gatekeeping mythology (ε ≈ 0.72, Piton) performative narrative. The aggregate policy (ε = 0.58) reflects institutional bundling of these three structurally distinct constraints. Decomposition recommended for precision analysis of which mechanism drives observed effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(danish_immigration_policy, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
