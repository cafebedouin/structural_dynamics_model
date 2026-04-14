% ============================================================================
% CONSTRAINT STORY: political_polarization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_political_polarization, []).

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
 *   constraint_id: political_polarization
 *   human_readable: Political Polarization as Coordination-Extraction Hybrid
 *   domain: political/social/institutional
 *
 * SUMMARY:
 *   Political polarization in contemporary democracies operates as a tangled
 *   coordination-extraction hybrid. The constraint coordinates the
 *   aggregation of diverse citizen preferences into coherent party platforms
 *   and mobilizes electoral participation through partisan identity, yet
 *   simultaneously extracts through suppression of cross-partisan
 *   deliberation, amplification of out-group threat perception, and
 *   concentration of political voice toward partisan media and party
 *   leadership. The constraint exhibits all properties of a tangled rope: it
 *   solves genuine coordination problems (how do millions of citizens with
 *   divergent preferences create decision-making coherence) while imposing
 *   asymmetric extraction (partisan media, platform algorithms, and party
 *   leadership capture disproportionate benefits; deliberation capacity,
 *   coalition flexibility, and unaffiliated citizens bear disproportionate
 *   costs). The extractiveness has increased from 0.35 (1990s) to 0.58
 *   (2026), driven by algorithmic amplification, declining journalistic
 *   institutional capacity, and increasing partisan identity fusion. Theater
 *   ratio has similarly increased from 0.45 to 0.68, indicating that
 *   performative partisan conflict has outpaced functional policy
 *   coordination. The constraint is actively enforced through party
 *   discipline, media gatekeeping, platform recommendation algorithms, and
 *   psychological identity mechanisms. Multiple perspectives show genuine
 *   exit options (grassroots coalitions forming across partisan lines on
 *   material issues; electoral mechanics reform proposals; algorithmic
 *   modification possibilities) suggesting scaffold dynamics—a temporary
 *   constraint being eroded by underlying material interests and
 *   institutional adaptation.
 *
 * KEY AGENTS:
 *   - Partisan Media Organizations: Primary institutional beneficiary (institutional/arbitrage) — capture audience attention and advertising revenue through polarization-driven engagement optimization
 *   - Engagement-Based Digital Platforms: Secondary institutional beneficiary (institutional/arbitrage) — algorithm optimization toward high-conflict content generates engagement metrics, user retention, and advertising revenue
 *   - Political Party Leadership: Tertiary institutional beneficiary (institutional/constrained) — benefit from partisan voter mobilization but also constrained by party base expectations
 *   - Unaffiliated Citizens: Primary victim (powerless/trapped) — structurally forced into binary choice with no viable exit or third option
 *   - Party-Fused Partisans: Secondary victim (powerless/identity_locked) — structurally mobile but cognitively trapped through identity fusion; bear costs of increasingly extreme in-group conformity
 *   - Democratic Deliberation Capacity: Tertiary victim (organized/constrained) — suppressed through partisan gatekeeping and cross-party dialogue costs; some residual coordination function persists
 *   - Grassroots Coalition Movements: Organized escape actors (organized/mobile) — forming transpartisan issue-based coalitions that bypass partisan binaries on material interests
 *   - Electoral Mechanics Institution: Piton actor (institutional/arbitrage) — persists through inertia despite widespread recognition of distortive effects; reform rhetoric continues but institutional structures unchanged
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(political_polarization, 0.58).
domain_priors:suppression_score(political_polarization, 0.65).
domain_priors:theater_ratio(political_polarization, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(political_polarization, extractiveness, 0.58).
narrative_ontology:constraint_metric(political_polarization, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(political_polarization, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(political_polarization, tangled_rope).
narrative_ontology:human_readable(political_polarization, "Political Polarization as Coordination-Extraction Hybrid").
narrative_ontology:topic_domain(political_polarization, "political/social/institutional").

domain_priors:requires_active_enforcement(political_polarization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(political_polarization, partisan_media_organizations).
narrative_ontology:constraint_beneficiary(political_polarization, political_party_leadership).
narrative_ontology:constraint_beneficiary(political_polarization, engagement_based_platforms).
narrative_ontology:constraint_victim(political_polarization, democratic_deliberation_capacity).
narrative_ontology:constraint_victim(political_polarization, cross_partisan_coalition_formation).
narrative_ontology:constraint_victim(political_polarization, unaffiliated_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNAFFILIATED VOTER (SNARE) — Trapped within the two-party binary constraint with no viable exit. Forced to choose between deeply alienating options or abandoning electoral participation. Suppression operates through structural impossibility of viable third-party candidacy, winner-take-all voting mechanics, and universal strategic voting norms. Maximum experienced extraction — complete constraint with no exit options and no meaningful coordination benefit.
constraint_indexing:constraint_classification(political_polarization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PARTY-FUSED PARTISAN (SNARE) — Structurally mobile (could abandon party affiliation) but identity-locked through partisan identity fusion. Self-concept, social networks, epistemic frameworks, and moral worldview are constituted through party membership. Exit would require becoming a different person. The constraint persists not through material barriers but through cognitive capture and identity integration. Experiences extraction through increasingly extreme in-group/out-group framing that maximizes emotional salience but minimizes deliberative capacity.
constraint_indexing:constraint_classification(political_polarization, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: LOCAL POLITICAL ORGANIZER (TANGLED ROPE) — Constrained by resource scarcity and party-affiliate dependencies but also benefits from the polarization infrastructure (party networks, donor bases, volunteer mobilization). The constraint coordinates local organizing (genuine coalition function) while extracting through ideological conformity requirements and exclusion of cross-partisan pragmatism. Significant agency but substantial costs imposed.
constraint_indexing:constraint_classification(political_polarization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PARTISAN MEDIA ORGANIZATION (ROPE) — Primary beneficiary. Polarization is the coordination mechanism that solves their core problem: audience retention and advertising revenue dependency on engagement. The constraint appears to them as pure coordination — organizing their audience into stable, predictable partisan cohorts. Extraction runs toward this agent; they experience minimal extraction themselves. Theater serves a functional purpose: emotional engagement is their business model.
constraint_indexing:constraint_classification(political_polarization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ENGAGEMENT-BASED DIGITAL PLATFORM (ROPE) — Beneficiary of polarization through algorithmic recommendation optimization toward high-engagement (high-conflict) content. The constraint coordinates content distribution and user attention allocation — a genuine coordination function. Extraction favors the platform; they externalize the cognitive and social costs of polarization while capturing engagement metrics and advertising revenue. Arbitrage exit: could shift recommendation algorithms but chooses not to due to business model misalignment.
constraint_indexing:constraint_classification(political_polarization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DEMOCRATIC DELIBERATION CAPACITY (TANGLED ROPE) — Organized actor (civil society, journalism institutions, deliberative forums) experiencing both coordination function and extraction. Polarization constrains deliberation to within-party consensus-building and inter-party conflict performance. Yet some coordination persists: parties do aggregate diverse interests into coherent platforms. The constraint requires active enforcement (party discipline, organizational hierarchy) to maintain. High suppression of cross-partisan dialogue; moderate coordination of partisan messaging.
constraint_indexing:constraint_classification(political_polarization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: GRASSROOTS COALITION MOVEMENTS (SCAFFOLD) — Organized agents (environmental, labor, progressive, conservative grassroots groups) see polarization as a temporary constraint being eroded by issue-based coalition-building that bypasses traditional partisan boundaries. Sunset mechanism: as single-issue coalitions mature and cross-partisan consensus emerges on specific policy domains (infrastructure, climate adaptation, industrial policy), the two-party binary loses explanatory power. Theater is declining as material interests (manufacturing jobs, water access, energy costs) override partisan identity. Mobile exit: these agents can and do form transpartisan coalitions when material stakes override identity salience.
constraint_indexing:constraint_classification(political_polarization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ELECTORAL MECHANICS INSTITUTION (PITON) — Winner-take-all voting, first-past-the-post, Senate apportionment, and Electoral College persist through institutional inertia despite widespread recognition that they amplify polarization. Theater ratio is high: constitutional reform language, legislative proposals for proportional representation, and campaign rhetoric about electoral reform persist while the institutions remain unchanged. The mechanism is purely performative—reform is discussed but structurally impossible without super-majoritarian consensus. Maintained through path-dependence and perceived legitimacy, not through function.
constraint_indexing:constraint_classification(political_polarization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of false summit: viewing polarization as an inevitable law of democratic systems (Duverger's law, Condorcet paradox, Arrow's theorem, spatial voting theory). The analytical perspective risks naturalizing what is partially a contingent institutional design choice (winner-take-all voting, party-based representation) as immutable mathematical law. However, some aspects may be genuinely mountain-class (the tendency toward binary coalition formation in finite strategic space is mathematically structural). The engine's false summit detection should flag this as partially naturalized.
constraint_indexing:constraint_classification(political_polarization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(political_polarization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(political_polarization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(political_polarization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(political_polarization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(political_polarization, TR),
    TR >= 0.70.

:- end_tests(political_polarization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significantly through several mechanisms: (1) partisan media and platform algorithms capture value through engagement optimization; (2) party leadership captures disproportionate voice over rank-and-file members; (3) unaffiliated citizens are forced into binary choice without viable exit; (4) cross-partisan coalition formation is suppressed. However, extraction is not maximal because genuine coordination benefits persist—parties do aggregate preferences into coherent platforms and mobilize electoral participation. The extractiveness value reflects the hybrid nature: substantial extraction alongside meaningful coordination. Suppression (0.65): High. Barriers to exit include: (1) structural (winner-take-all voting mechanics, two-party system architecture); (2) psychological (identity fusion, epistemic closure within partisan information ecosystems); (3) social (network segregation, social cost of party switching); (4) institutional (party discipline, media gatekeeping). The suppression is not total (some citizens do exit to unaffiliated status, some coalitions do form across partisan lines) but operates across multiple domains. Theater ratio (0.68): High and increasing. Much partisan performance is decoupled from policy coordination: campaign rhetoric about opponent moral degeneracy, electoral conflict theater, social media performance of partisan identity. Yet substantial functional content persists: actual policy differentiation between parties, real coalition mobilization, genuine deliberative processes within party structures. The theater ratio reflects that performance has outpaced function, but function has not disappeared.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces irreducible perspectival disagreement. Institutional beneficiaries (media, platforms, party leadership) experience the constraint as rope—solving their core coordination problems (audience aggregation, platform engagement, voter mobilization). Unaffiliated and party-fused victims experience snare—trapped in structures that extract their political voice without meaningful coordination benefit to them. Organized actors (grassroots coalitions, civil society) experience scaffold—the constraint as temporary, eroding through material-interest coalitions and electoral mechanics reform. The electoral mechanics institution experiences itself as piton—reform rhetoric persists but institutional structures remain unchanged due to path-dependence. The civilizational analytical view risks naturalizing what is contingent institutional design (two-party winner-take-all systems) as immutable law. The perspectival gaps reveal the constraint's true structure: it is a tangled rope that benefits institutional actors while extracting from distributed citizens, with organized escape routes (grassroots coalitions) available to those with sufficient agency and material interest alignment.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the extraction flow. Partisan media and platform algorithms sit at the high-extraction point (d ≈ 0.05–0.15, near pure beneficiary) because their business models directly optimize for polarization-driven engagement. They experience low or negative effective extraction—the constraint subsidizes their operations. Party leadership experiences moderate directionality (d ≈ 0.30–0.40) because they benefit from partisan voter mobilization but face constraints from party base expectations and pressure from grassroots activism. Unaffiliated voters experience maximum directionality (d ≈ 0.95) because they are forced into a binary choice that serves their interests poorly and have no exit options. Party-fused partisans experience high directionality (d ≈ 0.85) because while they have structural mobility, their identity lock prevents exercise of exit options—they are trapped in an identity frame that the constraint maintains. Grassroots coalitions experience moderate directionality (d ≈ 0.45–0.55) because they face partisan gatekeeping constraints but possess sufficient agency and transpartisan material interest to mobilize alternative coordination mechanisms. The derivation chain produces d values from beneficiary/victim declarations plus exit options, then applies sigmoid f(d) to compute experienced extractiveness chi per agent type.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through perspectival disaggregation: no single type is 'the' answer, but the classification presheaf reveals the structural topology. At the poweless/trapped perspective (unaffiliated voter), the constraint is snare. At the institutional/arbitrage perspective (partisan media), the constraint is rope. At the organized/mobile perspective (grassroots coalition), the constraint is scaffold. The mandatrophy question 'is this coordination or extraction?' is answered by 'both, depending on position.' The tangled rope classification at the moderate/constrained perspective (local organizer) represents the hybrid nature accurately. The piton classification at the institutional/civilizational perspective (electoral mechanics) identifies the performative reform ritual that masks institutional inertia. The false summit classification at the analytical/civilizational perspective flags the risk of naturalizing contingent institutional design as immutable law. All six types are present in the presheaf and all are correct from their respective positions. The analytical work is not to pick 'the right' type but to map the topological space of perspectives and identify which agents occupy which perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_rational_preference,
    'To what degree is partisan polarization driven by internalized identity fusion versus rational policy preference divergence?',
    'Longitudinal cognitive mapping studies; surveys measuring explicit identity-party fusion vs stated policy preferences; experimental manipulation of partisan labels on identical policy positions; analysis of swing voters vs identity-locked cohorts across time',
    'If primarily identity-locked: constraint is cognitive capture (snare from perspective of fused partisans) and extraction primarily operates through psychological mechanisms. If primarily rational preference: constraint is coordination of genuine ideological differences (rope) and snare classification is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_rational_preference, empirical, 'Identity fusion versus rational preference divergence in polarization').

omega_variable(
    algorithm_causation_vs_expression,
    'Do engagement-based recommendation algorithms cause polarization or merely amplify pre-existing preference heterogeneity?',
    'Natural experiments comparing algorithmic recommendation change outcomes; historical comparison of polarization trends pre/post-social media; randomized intervention studies of algorithm transparency/modification; cross-national comparison of polarization with different platform business models',
    'If primarily causal: platform business model is the key extraction mechanism (beneficiary is digital platforms, victim is deliberation capacity). If primarily expressive: polarization is driven by underlying political economy and platform role is secondary. This affects whether the constraint should have platform institutional perspectives vs media organization perspectives as primary extractors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_causation_vs_expression, empirical, 'Whether algorithms cause or express polarization').

omega_variable(
    structural_vs_contingent_binarity,
    'Is the two-party binary an inevitable feature of democratic systems (mountain) or a contingent design choice (snare/rope)?',
    'Comparative analysis of electoral outcomes under different voting systems (proportional representation, ranked choice, multi-member districts); historical analysis of US political system evolution; formal political theory analysis of strategic incentive structures under different institutional designs',
    'If structural: polarization may be inherent to democracy (mountain classification confirmed). If contingent: the constraint is institutional design and could be restructured (snare/tangled rope classification confirmed; scaffold classification as escape route confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_vs_contingent_binarity, conceptual, 'Whether two-party binarity is structural or contingent to democratic systems').

omega_variable(
    coordination_function_reality,
    'Does the partisan system genuinely coordinate heterogeneous citizens into coherent policy platforms or merely perform aggregation theater?',
    'Analysis of policy platform coherence across time; comparison of voter preferences to party platforms; tracking of party platform responsiveness to constituency feedback; measurement of within-party heterogeneity on actual votes vs performance rhetoric',
    'If genuine coordination: rope and tangled rope perspectives are accurate. If primarily theater: classification should shift toward snare and piton (extraction without meaningful coordination benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_reality, empirical, 'Whether partisan system genuinely aggregates interests or performs theater').

omega_variable(
    exit_cost_measurement,
    'What are the actual material, social, and cognitive costs of partisan identity exit?',
    'Qualitative interviews with party-switchers and exit-seekers; measurement of social network rupture costs; tracking of career/economic consequences; longitudinal psychological assessment of identity transition processes; analysis of demographic patterns in political disaffiliation',
    'If costs are extreme: identity_locked classification is accurate (cognitive capture dominates). If costs are moderate: constrained classification is more appropriate (high-cost exit exists but is surmountable). If costs are low: mobile classification should apply (exit options available).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_measurement, empirical, 'Material and psychological costs of partisan identity exit').

omega_variable(
    cross_partisan_coalition_possibility,
    'Are material-interest coalitions (labor, environmental, regional economic) capable of displacing identity-based partisan coalitions?',
    'Tracking of issue-based coalition formation success rates; measurement of cross-partisan policy support on specific material issues; historical analysis of previous coalition realignment periods; experimental study of coalition-building under various issue framings',
    'If capable: scaffold perspective is accurate and sunset timeline is real (10-30 years). If incapable: polarization will persist and scaffold is aspirational (constrains classification toward snare/piton).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_partisan_coalition_possibility, empirical, 'Capacity for material-interest coalitions to displace partisan binaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(political_polarization, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(polpol_tr_t0, political_polarization, theater_ratio, 0, 0.45).
narrative_ontology:measurement(polpol_tr_t10, political_polarization, theater_ratio, 10, 0.58).
narrative_ontology:measurement(polpol_tr_t20, political_polarization, theater_ratio, 20, 0.68).
narrative_ontology:measurement(polpol_tr_t25, political_polarization, theater_ratio, 25, 0.7).

% Extraction over time
narrative_ontology:measurement(polpol_be_t0, political_polarization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(polpol_be_t10, political_polarization, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(polpol_be_t20, political_polarization, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(polpol_be_t25, political_polarization, base_extractiveness, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(political_polarization, identity_coordination).
narrative_ontology:affects_constraint(political_polarization, deliberative_democracy_erosion).
narrative_ontology:affects_constraint(political_polarization, partisan_media_business_model).
narrative_ontology:affects_constraint(political_polarization, engagement_algorithmic_optimization).

% DUAL FORMULATION NOTE:
% Political polarization decomposes into at least three structurally distinct constraints: (1) institutional constraint on electoral mechanics (winner-take-all binarity) with ε ≈ 0.35; (2) business model constraint on media organizations (engagement-driven revenue optimization) with ε ≈ 0.62; (3) cognitive constraint on partisan identity fusion with ε ≈ 0.58. This story treats them as unified under polarization, but they should be decomposed into separate stories with network linkages. The electoral mechanics constraint is upstream; media and platform business models are downstream; cognitive identity fusion is a separate mechanism that feeds both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(political_polarization, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
