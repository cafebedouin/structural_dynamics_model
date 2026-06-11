% ============================================================================
% CONSTRAINT STORY: post_1998_convergence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_post_1998_convergence, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: post_1998_convergence
 *   human_readable: Post-1998 Left-Government Responsiveness Convergence
 *   domain: political_economy/democratic_theory/institutional_analysis
 *
 * SUMMARY:
 *   The post-1998 convergence constraint describes the synchronized
 *   cross-national disappearance of left-government responsiveness to
 *   lower-income constituencies in the economic and welfare policy domain.
 *   The Lupu-Pontusson four-country time series (Sweden, UK, Germany, US)
 *   shows a clear structural break: pre-1998, left governments responded more
 *   strongly to lower-income preferences than right governments; post-1998,
 *   this responsiveness gradient inverts or disappears. The constraint tracks
 *   two upstream structural changes: (1) capital account liberalization in
 *   the 1990s, which gave capital holders arbitrage-grade exit options and
 *   imposed fiscal discipline on governments, and (2) Soviet collapse in
 *   1989-1991, which eliminated the ideological competitor regime and
 *   narrowed the Overton window for redistributive policy. The constraint
 *   operates through multiple mechanisms: capital flight risk constraining
 *   fiscal policy, union density decline reducing organized labor's
 *   bargaining power, median voter dynamics in aging democracies, and
 *   ideological convergence within left parties toward 'Third Way'
 *   market-friendly positions. The result is a democratic system that
 *   continues to hold elections and rotate parties but has lost the capacity
 *   to translate lower-income preferences into policy outcomes in the
 *   economic domain. The constraint is downstream of two mountain-classified
 *   constraints: scale_ceiling (the organizational scale asymmetry between
 *   capital and labor) and organization_floor (the minimum viable scale for
 *   collective action). These upstream constraints are treated as immutable
 *   in this story, though that classification itself is contestable.
 *
 * KEY AGENTS:
 *   - Lower-Income Wage Earners: Primary victim (powerless/trapped) — lost policy responsiveness from historically allied parties; no exit options within national labor markets
 *   - Mobile Capital Holders: Primary beneficiary (institutional/arbitrage) — gained policy influence through exit threat; can relocate across borders costlessly
 *   - Public Sector Unions: Secondary victim (moderate/constrained) — declining membership and strike leverage but still embedded in consultation structures
 *   - Left Party Electoral Coalitions: Institutional actor (institutional/constrained) — coordinate electoral competition but policy space narrowed by capital mobility
 *   - Financial Sector Institutions: Primary beneficiary (institutional/arbitrage) — capital account liberalization removed regulatory constraints; policy convergence protects financial interests
 *   - Export-Oriented Firms: Primary beneficiary (institutional/arbitrage) — benefit from labor cost discipline and market-friendly trade policy
 *   - Transnational Labor Networks: Organized agents (organized/mobile) — building cross-border coordination mechanisms with scaffold logic (sunset: 15-25 years)
 *   - Social Democratic Party Apparatus: Institutional actor (institutional/arbitrage) — maintains organizational form but historical function atrophied (piton perspective)
 *   - Welfare State Dependents: Secondary victim (powerless/trapped) — austerity pressure and benefit cuts; no exit options
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(post_1998_convergence, 0.68).
domain_priors:suppression_score(post_1998_convergence, 0.72).
domain_priors:theater_ratio(post_1998_convergence, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(post_1998_convergence, extractiveness, 0.68).
narrative_ontology:constraint_metric(post_1998_convergence, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(post_1998_convergence, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(post_1998_convergence, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(post_1998_convergence, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(post_1998_convergence, rope).
narrative_ontology:human_readable(post_1998_convergence, "Post-1998 Left-Government Responsiveness Convergence").
narrative_ontology:topic_domain(post_1998_convergence, "political_economy/democratic_theory/institutional_analysis").

domain_priors:requires_active_enforcement(post_1998_convergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(post_1998_convergence, mobile_capital_holders).
narrative_ontology:constraint_beneficiary(post_1998_convergence, financial_sector_institutions).
narrative_ontology:constraint_beneficiary(post_1998_convergence, export_oriented_firms).
narrative_ontology:constraint_victim(post_1998_convergence, lower_income_wage_earners).
narrative_ontology:constraint_victim(post_1998_convergence, public_sector_unions).
narrative_ontology:constraint_victim(post_1998_convergence, welfare_state_dependents).
narrative_ontology:constraint_vindicates(post_1998_convergence, capital_mobility_hypothesis).
narrative_ontology:constraint_vindicates(post_1998_convergence, race_to_bottom_thesis).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOWER-INCOME WAGE EARNERS (SNARE) — Trapped within national labor markets with declining union density and no capital mobility. Experience the constraint as pure extraction: left parties that historically represented their interests now respond primarily to median voters and capital holders. Exit options collapsed post-1998 as social democratic parties converged toward market-friendly policies. The 'coordination' story (democratic responsiveness) is cover for systematic abandonment.
constraint_indexing:constraint_classification(post_1998_convergence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PUBLIC SECTOR UNION LEADERSHIP (TANGLED ROPE) — Constrained by membership decline and fiscal pressure but still embedded in party consultation structures. Experience genuine coordination (policy input channels, collective bargaining frameworks) alongside asymmetric extraction (declining strike leverage, austerity pressure). Can exit to private sector or retire but at significant career cost. The constraint both enables their institutional role and extracts from their membership base.
constraint_indexing:constraint_classification(post_1998_convergence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MOBILE CAPITAL HOLDERS (ROPE) — Experience the constraint as pure coordination. Capital account liberalization solved a genuine collective action problem: how to allocate investment across jurisdictions efficiently. Left-government convergence toward market-friendly policy represents rational adaptation to capital mobility, not extraction. Arbitrage-grade exit (can relocate capital across borders) means effective extraction is negative — the constraint subsidizes this position.
constraint_indexing:constraint_classification(post_1998_convergence, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LEFT PARTY ELECTORAL COALITIONS (TANGLED ROPE) — Constrained by capital mobility and median voter dynamics but still coordinate electoral competition and policy formation. Experience both coordination (aggregating preferences, forming governments) and extraction (policy space narrowed by capital flight risk). Exit options limited by party system structure and career investment. The convergence represents both strategic adaptation and abandonment of historical base.
constraint_indexing:constraint_classification(post_1998_convergence, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRANSNATIONAL LABOR NETWORKS (SCAFFOLD) — Organized actors building cross-border coordination mechanisms (European Trade Union Confederation, global union federations, living wage campaigns). See the post-1998 convergence as a temporary coordination failure with a sunset: as labor organizing scales to match capital mobility, the responsiveness asymmetry will dissolve. Estimated sunset: 15-25 years for transnational collective bargaining frameworks to mature. Mobile exit options via international organizing roles.
constraint_indexing:constraint_classification(post_1998_convergence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: SOCIAL DEMOCRATIC PARTY APPARATUS (PITON) — The institutional machinery of left parties persists (membership structures, policy research units, electoral campaign infrastructure) but its historical function (translating working-class interests into policy) has atrophied. What remains is largely theatrical: consultation rituals with unions, redistributive rhetoric, symbolic welfare gestures. The apparatus maintains itself through inertia and the absence of viable alternatives, not because it delivers its founding mandate. Theater ratio reflects performative progressivism without structural redistribution.
constraint_indexing:constraint_classification(post_1998_convergence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the post-1998 convergence exhibits both genuine coordination (democratic competition, policy adaptation to changed constraints) and asymmetric extraction (systematic transfer of policy responsiveness from lower-income to capital-holding constituencies). The Lupu-Pontusson data shows the structural break clearly: pre-1998, left governments responded to lower-income preferences; post-1998, responsiveness gradient inverts. This is not a natural law (capital mobility is a policy choice, not physics) but a constructed constraint with identifiable beneficiaries and victims. The 'democracy as market mechanism' framing naturalizes what is actually institutional choice.
constraint_indexing:constraint_classification(post_1998_convergence, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(post_1998_convergence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(post_1998_convergence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(post_1998_convergence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(post_1998_convergence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(post_1998_convergence, TR),
    TR >= 0.70.

:- end_tests(post_1998_convergence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint systematically transfers policy responsiveness from lower-income wage earners (who lack exit options) to mobile capital holders (who have arbitrage-grade exit). The Lupu-Pontusson data shows this is not a marginal shift but a structural inversion: the responsiveness gradient that defined left parties for a century disappears post-1998. The extraction is substantial but not maximal because some welfare state structures persist and some left parties retain redistributive rhetoric and symbolic policies. Suppression (0.72): High. Multiple mechanisms suppress alternatives: capital flight risk constrains fiscal policy space, union density decline reduces organized labor's bargaining power, party system structure limits entry of new left parties, media ownership concentration narrows policy discourse, and ideological convergence within left parties makes redistributive positions 'unthinkable' even when popular. The suppression increased sharply post-1998 as capital mobility matured and competitor regime collapse removed the ideological alternative. Theater ratio (0.58): Moderate-high. Left parties maintain the theatrical apparatus of working-class representation (union consultation, redistributive rhetoric, symbolic welfare gestures) while actual policy responsiveness to lower-income preferences has collapsed. The theater is substantial but not total because some genuine policy differences remain (social vs market liberalism on cultural issues, degree of austerity). The ratio increased over the interval as the gap between rhetoric and policy widened. Accessibility collapse (0.42): Moderate. Alternatives have not collapsed completely — capital controls remain technically feasible, some countries (Norway, Switzerland) maintain stronger labor bargaining, and transnational labor organizing is emerging. But alternatives are substantially less accessible post-1998 than pre-1998 due to ideological convergence and institutional path dependence. Resistance (0.65): Moderate-high. The constraint meets significant resistance from unions, left party activists, anti-globalization movements, and populist challengers, but this resistance has not reversed the convergence. The resistance is real but has been largely ineffective at the policy level.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. Mobile capital holders see pure coordination (rope) — capital account liberalization solved a genuine allocation problem, and left-government convergence is rational adaptation. Lower-income wage earners see pure extraction (snare) — the 'democratic responsiveness' story is cover for systematic abandonment. Public sector unions and left party coalitions see mixed coordination and extraction (tangled_rope) — the system both enables and constrains them. Transnational labor networks see a temporary problem with a sunset (scaffold) — cross-border organizing will eventually restore bargaining power. The social democratic party apparatus sees its own degraded ritual (piton) — the institutional machinery persists but the function has atrophied. The analytical observer sees the constraint as constructed rather than natural (tangled_rope, not mountain) — capital mobility is a policy choice, not a law of physics, and the convergence reflects institutional design choices that could be reversed. The gap between the rope perspective (capital holders) and the snare perspective (wage earners) is the core political economy conflict: one group's coordination is another group's extraction, and the difference is determined by exit options and structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position relative to the constraint. Mobile capital holders are primary beneficiaries with arbitrage-grade exit — they experience negative effective extraction (the constraint subsidizes their position by disciplining labor costs and protecting financial interests). Lower-income wage earners are primary victims with trapped exit options — they experience maximum effective extraction (lost policy responsiveness with no ability to exit national labor markets). Public sector unions are in a mixed position: they are victims (declining leverage, austerity pressure) but retain some coordination benefits (consultation structures, collective bargaining frameworks), and their exit options are constrained but not absent (can exit to private sector or retire at career cost). Left party coalitions are institutional actors with constrained exit — they experience both coordination (electoral competition, government formation) and extraction (policy space narrowed). Transnational labor networks are organized actors with mobile exit options (international organizing roles) who see the constraint as temporary (scaffold perspective). The social democratic party apparatus has arbitrage-grade exit (leadership can rotate to private sector, consulting, international organizations) and experiences the constraint as degraded ritual (piton perspective). The analytical observer sees both coordination and extraction from a civilizational perspective (tangled_rope classification).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that 'democratic responsiveness' can be both a genuine coordination mechanism (aggregating preferences, forming governments, rotating parties) and an extraction mechanism (systematically transferring policy influence from those without exit to those with exit). The mandate (translate citizen preferences into policy) persists in form but has been hollowed out in the economic domain for lower-income constituencies. The convergence is not a failure of democracy-as-designed but a feature: when capital has arbitrage-grade exit and labor has trapped exit, democratic competition will converge toward capital-friendly policy regardless of which party wins. The 'democracy vs oligarchy' framing in the source essay is itself a mandatrophy question: is this system still performing its founding function (popular sovereignty) or has it become a constrained exchange system that maintains democratic theater while concentrating policy influence? The analytical perspective classifies this as tangled_rope rather than mountain precisely because the constraint is constructed (capital mobility is a policy choice) and has identifiable beneficiaries and victims, even though it operates through democratic institutions. The piton perspective (social democratic party apparatus) captures the mandatrophy most directly: the institutional form persists but the function has atrophied, maintained through inertia rather than performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_direction,
    'Does capital account liberalization cause left-government policy convergence, or does ideological shift within left parties cause both liberalization and convergence?',
    'Cross-national timing analysis: do countries that liberalized earlier show earlier convergence? Counterfactual analysis of countries that maintained capital controls (China, India pre-1991). Party manifesto content analysis pre/post liberalization.',
    'If capital mobility is causal: the constraint is structural (tangled_rope from analytical perspective confirmed). If ideological shift is causal: the constraint is more extractive (closer to snare) because the ''coordination'' story (adapting to capital mobility) is post-hoc rationalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_direction, empirical, 'Direction of causality between capital liberalization and policy convergence').

omega_variable(
    transnational_labor_viability,
    'Can labor organizing scale to match capital mobility, or is the organization floor (local/national) an immutable constraint while capital operates globally?',
    'Historical analysis of successful transnational labor coordination (International Transport Workers'' Federation, European Works Councils). Assessment of legal/institutional barriers to cross-border collective bargaining. Comparison of capital vs labor coordination costs at different scales.',
    'If labor can scale: scaffold perspective confirmed, sunset is real. If organization floor is immutable: scaffold perspective is aspirational, and the convergence is permanent extraction (snare from more perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transnational_labor_viability, empirical, 'Whether transnational labor organizing can overcome scale asymmetry').

omega_variable(
    median_voter_vs_capital_mobility,
    'Is the post-1998 convergence driven by capital mobility constraints or by median voter preferences shifting rightward independently?',
    'Decomposition of left-party policy shifts into capital-flight-risk domains (corporate taxation, financial regulation) vs median-voter domains (immigration, crime, cultural issues). If convergence is uniform across domains, median voter is primary. If concentrated in capital-sensitive domains, capital mobility is primary.',
    'If median voter: the constraint is democratic responsiveness working as designed (rope from more perspectives). If capital mobility: the constraint is extraction masked as democracy (snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(median_voter_vs_capital_mobility, empirical, 'Relative weight of capital mobility vs median voter in driving convergence').

omega_variable(
    competitor_regime_collapse_effect,
    'How much of the convergence is attributable to Soviet collapse removing the ideological alternative, versus capital mobility creating material constraints?',
    'Comparison of convergence timing in countries with strong vs weak communist parties pre-1989. Analysis of party manifesto shifts in ideological rhetoric vs material policy. Assessment of whether convergence accelerates after 1989 (ideological) or after capital account opening (material).',
    'If ideological (Soviet collapse): the constraint is partly self-imposed belief shift, increasing extractiveness. If material (capital mobility): the constraint is structural adaptation, supporting coordination interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitor_regime_collapse_effect, empirical, 'Relative contribution of Soviet collapse vs capital mobility to convergence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(post_1998_convergence, 0, 33).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(p98conv_theater_1980, post_1998_convergence, theater_ratio, 0, 0.25).
narrative_ontology:measurement(p98conv_theater_1989, post_1998_convergence, theater_ratio, 9, 0.3).
narrative_ontology:measurement(p98conv_theater_1993, post_1998_convergence, theater_ratio, 13, 0.38).
narrative_ontology:measurement(p98conv_theater_1998, post_1998_convergence, theater_ratio, 18, 0.5).
narrative_ontology:measurement(p98conv_theater_2003, post_1998_convergence, theater_ratio, 23, 0.58).
narrative_ontology:measurement(p98conv_theater_2008, post_1998_convergence, theater_ratio, 28, 0.62).
narrative_ontology:measurement(p98conv_theater_2013, post_1998_convergence, theater_ratio, 33, 0.58).

% Extraction over time
narrative_ontology:measurement(p98conv_extract_1980, post_1998_convergence, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(p98conv_extract_1989, post_1998_convergence, base_extractiveness, 9, 0.42).
narrative_ontology:measurement(p98conv_extract_1993, post_1998_convergence, base_extractiveness, 13, 0.55).
narrative_ontology:measurement(p98conv_extract_1998, post_1998_convergence, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(p98conv_extract_2003, post_1998_convergence, base_extractiveness, 23, 0.7).
narrative_ontology:measurement(p98conv_extract_2008, post_1998_convergence, base_extractiveness, 28, 0.68).
narrative_ontology:measurement(p98conv_extract_2013, post_1998_convergence, base_extractiveness, 33, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(p98conv_suppress_1980, post_1998_convergence, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(p98conv_suppress_1989, post_1998_convergence, suppression_requirement, 9, 0.48).
narrative_ontology:measurement(p98conv_suppress_1998, post_1998_convergence, suppression_requirement, 18, 0.65).
narrative_ontology:measurement(p98conv_suppress_2008, post_1998_convergence, suppression_requirement, 28, 0.72).
narrative_ontology:measurement(p98conv_suppress_2013, post_1998_convergence, suppression_requirement, 33, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(post_1998_convergence, resource_allocation).

% DUAL FORMULATION NOTE:
% The post-1998 convergence is downstream of two mountain-classified constraints (scale_ceiling and organization_floor) that establish the structural asymmetry between capital and labor mobility. Those upstream constraints are treated as immutable in this story, though their mountain classification is itself contestable — the 'natural' scale ceiling for labor organizing may be a constructed constraint that benefits capital holders. The convergence constraint has its own extractiveness value (0.68) reflecting the policy responsiveness transfer, distinct from the upstream constraints' extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(post_1998_convergence, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
