% ============================================================================
% CONSTRAINT STORY: elite_sports_funding_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_elite_sports_funding_concentration, []).

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
 *   constraint_id: elite_sports_funding_concentration
 *   human_readable: Elite Sports Funding Concentration
 *   domain: sports_economics/institutional_policy
 *
 * SUMMARY:
 *   Elite sports funding concentration creates a structural constraint that
 *   benefits medal-contending athletes, winning nations, corporate sponsors,
 *   and media outlets while extracting from grassroots participation
 *   infrastructure, non-medal sports, developing federations, and community
 *   athletic programs. The constraint exhibits the full range of DR
 *   classifications across different perspectives, revealing how a single
 *   institutional structure (concentration of public and corporate funding on
 *   Olympic/medal-focused sports) appears as efficient coordination to
 *   beneficiaries, as pure extraction to powerless agents, as a temporary
 *   problem solvable through alternative models to organized equity
 *   coalitions, and as an inescapable natural law to uncritical observers.
 *   The extractiveness value (0.58) reflects moderate-to-high extraction
 *   driven by suppression (0.62) of alternative funding pathways and barriers
 *   to entry for non-medal sports. The theater ratio (0.55) indicates that
 *   approximately half the rhetorical and institutional energy devoted to
 *   sports funding serves to maintain the concentration hierarchy rather than
 *   to optimize athletic development. Over the 30-year measurement interval,
 *   both metrics show degradation: extractiveness increased from 0.35 to 0.58
 *   as global sports became more commercialized and media concentration
 *   increased; theater ratio increased from 0.38 to 0.55 as Olympic legacy
 *   narratives and 'sport for development' rhetoric proliferated without
 *   corresponding investment in non-elite pathways.
 *
 * KEY AGENTS:
 *   - Elite Athletes with Medal Potential: Primary beneficiary (institutional/arbitrage) — receive concentrated resources, sponsorship, and career opportunities; can exit into professional sports markets
 *   - Medal-Contending National Federations: Primary beneficiary (institutional/arbitrage) — receive majority of public funding and international prestige based on medal performance
 *   - Corporate Sponsors and Broadcast Media: Primary beneficiary (institutional/arbitrage) — concentrate investment on high-viewership events and athletes; extract commercial value from performance concentration
 *   - Grassroots Athletes and Non-Medal Sport Participants: Primary victim (powerless/trapped) — locked into resource-starved pathways; no exit from underfunding without abandoning their sports
 *   - Developing National Federations: Secondary victim (moderate/constrained) — constrained by dependence on Olympic qualification pathways; face extraction through competitive pressure without resources
 *   - Public Sports Ministries: Institutional actor (powerful/constrained) — under political pressure to maximize medals; constrained by electoral accountability to medal performance metrics
 *   - Sports Equity Coalition: Organized agents (organized/mobile) — nonprofit youth sports organizations, disability sports advocates, community recreation systems building alternative funding pathways
 *   - International Olympic Committee and Sport Federations: Institutional actor (institutional/arbitrage) — maintains medal-counting institutional framework; arbitrage available (can reform standards) but constrained by inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(elite_sports_funding_concentration, 0.58).
domain_priors:suppression_score(elite_sports_funding_concentration, 0.62).
domain_priors:theater_ratio(elite_sports_funding_concentration, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(elite_sports_funding_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(elite_sports_funding_concentration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(elite_sports_funding_concentration, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(elite_sports_funding_concentration, tangled_rope).
narrative_ontology:human_readable(elite_sports_funding_concentration, "Elite Sports Funding Concentration").
narrative_ontology:topic_domain(elite_sports_funding_concentration, "sports_economics/institutional_policy").

domain_priors:requires_active_enforcement(elite_sports_funding_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(elite_sports_funding_concentration, elite_athletes_medal_contenders).
narrative_ontology:constraint_beneficiary(elite_sports_funding_concentration, winning_national_federations).
narrative_ontology:constraint_beneficiary(elite_sports_funding_concentration, corporate_sponsors).
narrative_ontology:constraint_beneficiary(elite_sports_funding_concentration, broadcast_media).
narrative_ontology:constraint_victim(elite_sports_funding_concentration, grassroots_sports_participation).
narrative_ontology:constraint_victim(elite_sports_funding_concentration, non_medal_sport_athletes).
narrative_ontology:constraint_victim(elite_sports_funding_concentration, developing_national_federations).
narrative_ontology:constraint_victim(elite_sports_funding_concentration, community_sports_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GRASSROOTS ATHLETE (SNARE) — No viable exit from the funding hierarchy. Athletes in non-medal sports or without immediate Olympic potential face systemic resource deprivation. Suppressed by performative metrics (national ranking, medal count) that render their participation invisible. Bears extraction through talent drain to elite programs and loss of recreational funding.
constraint_indexing:constraint_classification(elite_sports_funding_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEVELOPING NATIONAL FEDERATION (TANGLED ROPE) — Constrained by dependence on international funding and Olympic qualification standards. Genuine coordination function: federation manages athlete development and event access. Asymmetric extraction: disproportionate resources flow to medal-contending sports while foundational infrastructure receives minimal support. Exit cost is high (loss of international standing) but not absolute.
constraint_indexing:constraint_classification(elite_sports_funding_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEDAL-CONTENDING FEDERATION (ROPE) — Primary beneficiary. Experiences funding concentration as pure coordination mechanism — efficient allocation of resources to maximize competitive performance. High exit option (can maintain domestic programs independent of medal pressure). Net positive: receives bulk of public funding and corporate sponsorship based on performance guarantee.
constraint_indexing:constraint_classification(elite_sports_funding_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BROADCAST MEDIA AND CORPORATE SPONSORS (ROPE) — Primary beneficiary. Coordinate around medal-contending events and athletes as content. Extraction is minimal because sponsorship actively benefits both parties through visibility and commercial return. High arbitrage: can withdraw or redirect sponsorship with limited cost.
constraint_indexing:constraint_classification(elite_sports_funding_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLIC SPORTS MINISTRY (TANGLED ROPE) — Institutional actor under political pressure to maximize medal count as measure of national success. Active enforcement of performance metrics and funding allocation rules. Genuine coordination function: manages domestic sports ecosystem. Asymmetric extraction: concentrates public resources on elite athletics while recreational and grassroots infrastructure deteriorates. Constrained by electoral accountability tied to medal performance.
constraint_indexing:constraint_classification(elite_sports_funding_concentration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SPORTS EQUITY COALITION (SCAFFOLD) — Organized agents (youth sports nonprofits, disability sports advocates, equity-focused federations) see funding concentration as a temporary institutional failure with structural exit: diversified funding models, community-owned facilities, and multi-sport talent pipelines are building alternative pathways to athletic development that bypass the medal-count hierarchy. High mobility because coalition can fund and operate independently. Sunset logic: as alternative models mature, the concentration mechanism loses structural force.
constraint_indexing:constraint_classification(elite_sports_funding_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: OLYMPIC QUALIFICATION FRAMEWORK (PITON) — International institutional framework (IOC, sport-specific federations) maintains medal-counting logic through inertia. The framework persists because no global alternative has replaced it, not because it optimizes human athletic development. Theater ratio is high: the ritualistic emphasis on national medal counts obscures that competitive excellence is decoupled from mass participation or equity. Arbitrage available for wealthy nations (exit by investing in alternative prestige pathways) but not for developing nations (locked into Olympic system as path to international legitimacy).
constraint_indexing:constraint_classification(elite_sports_funding_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scale, elite athletic performance is inherently constrained by physiological limits and talent scarcity: only a small percentage of the population can achieve medal-level performance. This perspective naturalizes funding concentration as immutable consequence of human biological variation. However, structural data reveals this as false summit: talent distribution is wide, but funding distribution is artificially narrow. The constraint is institutional (merit-based funding hierarchy), not biological (innate performance limits).
constraint_indexing:constraint_classification(elite_sports_funding_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(elite_sports_funding_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(elite_sports_funding_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(elite_sports_funding_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(elite_sports_funding_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(elite_sports_funding_concentration, TR),
    TR >= 0.70.

:- end_tests(elite_sports_funding_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The original analysis separated two mechanisms: direct extraction (beneficiaries capture funding that could support grassroots infrastructure, representing ~0.35 direct transfer) and suppression of alternatives (regulatory and funding barriers that prevent grassroots and non-medal sports from competing for resources, representing ~0.23 additional extractiveness through opportunity cost). The combined effect is significant but not absolute because: (1) elite sports do generate some genuine public interest and some infrastructure spillovers, (2) alternative funding sources (community nonprofits, local government) partially compensate for reduced public spending, and (3) some developed nations maintain modest grassroots programs alongside elite funding. Suppression (0.62): Moderate-high. Multiple barriers prevent grassroots and non-medal sports from accessing elite-level funding: (a) media concentration on medal events (regulatory), (b) Olympic qualification standards that exclude non-Olympic sports from public funding (institutional gatekeeping), (c) performance metrics (national ranking, medal count) that render non-elite athletes invisible in political discourse (cognitive suppression), (d) equipment and facility access barriers that compound funding deprivation. However, suppression is not total (grassroots programs exist; some nations have diversified funding) and is partially penetrable through advocacy and alternative institutional models. Theater ratio (0.55): Moderate. Approximately 55% of institutional rhetoric and 45% of genuine functional activity. The performative elements include: (a) 'sport for development' narratives that justify elite funding as benefiting the broader population without actual community benefit mechanisms, (b) Olympic 'legacy' promises for venues that are underutilized post-games, (c) 'natural talent pyramid' rhetoric that presents concentration as inevitable rather than institutional, (d) international prestige narratives that tie national legitimacy to medal counts despite no causal link to national wellbeing. The genuine functional element (45%) is actual athletic development and competitive organization.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals that 'merit-based funding' is a framing that naturalizes what is actually a two-tiered extraction mechanism. The beneficiary's view (Rope) sees this as fair: allocate resources to those who will produce results. The victim's view (Snare) sees this as structural violence: talent is talent, but only medal-pathway talent receives recognition or resources. The coalition's view (Scaffold) sees this as a solvable institutional problem: alternative prestige narratives, participation-based metrics, and diversified funding sources can decouple athletic development from medal concentration. The ministry's view (Tangled Rope) sees pressure from above (electoral accountability tied to medal count) and constraints from below (limited resources). The IOC's view (Piton) rationalizes the system through performative logic: the Olympic framework 'develops' sport globally because it's the most visible sporting event, regardless of whether most athletes or nations actually benefit. The analytical view (Mountain) risks naturalizing the hierarchy as an immutable law of talent distribution. The gap between these perspectives is the constraint's diagnostic signature: when the same structural data produces Snare, Rope, Tangled Rope, Scaffold, Piton, and Mountain, the constraint's classification is not determined by the data alone but by the observer's structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) is computed from base extractiveness ε (0.58), directionality d, and scope σ. For the medal-contending federation: d ≈ 0.10 (beneficiary) → f(d) ≈ -0.05 → χ ≈ 0.58 × (-0.05) × 1.0 ≈ -0.03 (negative extraction, i.e., coordination benefit). For the grassroots athlete: d ≈ 0.92 (victim/trapped) → f(d) ≈ 1.38 → χ ≈ 0.58 × 1.38 × 1.0 ≈ 0.80 (high extraction experienced). For the developing federation: d ≈ 0.58 (both) → f(d) ≈ 0.67 → χ ≈ 0.58 × 0.67 × 1.0 ≈ 0.39 (moderate mixed experience). At global scope σ(S)=1.2, the grassroots athlete's experienced extractiveness amplifies to χ ≈ 0.80 × 1.2 ≈ 0.96 because the concentration mechanism operates at global media scale. The suppression value (0.62) is not scaled; it is a raw structural property: the barriers to alternative funding are real and distributed across regulatory, institutional, and cognitive mechanisms regardless of perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE CLASSIFICATION: The constraint meets all three gates. (1) Genuine coordination function: elite sports funding does coordinate athletic development, international competition, and media attention around shared standards. (2) Asymmetric extraction: benefits flow predominantly to medal-contending athletes and nations while costs (foregone grassroots funding, non-elite invisibility, competitive disadvantage for developing nations) are borne by others. (3) Active enforcement: Olympic qualification standards, national ranking systems, broadcast contracts, and funding allocation rules actively maintain the concentration. The mandatrophy is resolved by recognizing that the coordination function (legitimate) and extraction function (asymmetric) coexist in the same institutional structure. The beneficiary does not experience extraction because they are the beneficiary; the victim experiences extraction precisely because the coordination is asymmetric. The constraint persists because the coordination function is genuine enough to maintain institutional support, while the extraction is large enough that alternatives (sports equity, diversified funding) cannot challenge it without building entirely parallel systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    talent_distribution_vs_funding_distribution,
    'Is the correlation between individual athletic talent and concentration of funding allocation driven by actual talent scarcity or by institutional gatekeeping?',
    'Comparative analysis: participation rates in high-funded vs low-funded sports; longitudinal tracking of talent development outcomes in countries with diversified vs concentrated funding models; identification of mediating factors (coaching quality, equipment access, competition frequency) that affect talent realization independent of initial aptitude',
    'If talent distribution is genuinely concentrated: medal-focused funding is an efficiency mechanism (Rope from stronger perspectives). If talent distribution is wide but artificially narrowed by funding gatekeeping: concentration is extractive (Snare from stronger perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(talent_distribution_vs_funding_distribution, empirical, 'Whether talent concentration justifies funding concentration').

omega_variable(
    secondary_benefits_of_elite_funding,
    'Do infrastructure investments in elite sports create positive spillovers to grassroots participation and recreational facilities?',
    'Accounting analysis: track capital flows for sports facilities; measure accessibility of elite-level venues for community use; compare participation rates in sports with elite funding vs sports without elite infrastructure investment',
    'If spillovers are substantial: elite funding coordinates broader athletic development (Rope or Tangled Rope from more perspectives). If spillovers are minimal: elite funding extracts without return to commons (Snare confirmed, Piton for performative rhetoric about ''legacy'').',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_benefits_of_elite_funding, empirical, 'Whether elite sports funding generates grassroots spillovers').

omega_variable(
    alternative_prestige_metrics,
    'Can democratic and wealthy nations sustain international prestige and soft power through non-medal-based sporting achievement (participation rates, gender equity, disability inclusion, athlete welfare)?',
    'Comparative institutional analysis: nations emphasizing Olympic medals vs nations emphasizing participation metrics; measurement of international legitimacy, soft power outcomes, and public satisfaction; identification of cultural and institutional factors enabling non-medal prestige',
    'If alternative metrics sustain prestige: scaffold perspective is structural (sunset clause is real pathway). If medal counting is inescapable: concentrating funding is enforced by competitive logic (Snare or Tangled Rope confirmed at field scale).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_prestige_metrics, conceptual, 'Whether non-medal metrics can sustain international prestige').

omega_variable(
    suppression_mechanism_structural_vs_cognitive,
    'Is suppression of grassroots sports primarily structural (lack of facilities, coaching, equipment) or cognitive (internalized meritocratic framing that renders non-elite athletes invisible)?',
    'Intervention analysis: investment in grassroots infrastructure without narrative change vs narrative investment without capital; measurement of participation uptake, identity formation in young athletes, and perceived legitimacy of non-elite sports',
    'If structural: removing funding barriers expands participation (constraint is economic). If cognitive: internalized hierarchy persists even after material barriers are removed (constraint has deeper identity-coordination component).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_cognitive, empirical, 'Whether suppression is structural or internalized via meritocratic narrative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(elite_sports_funding_concentration, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(esfc_tr_t0, elite_sports_funding_concentration, theater_ratio, 0, 0.38).
narrative_ontology:measurement(esfc_tr_t15, elite_sports_funding_concentration, theater_ratio, 15, 0.47).
narrative_ontology:measurement(esfc_tr_t30, elite_sports_funding_concentration, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(esfc_be_t0, elite_sports_funding_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(esfc_be_t15, elite_sports_funding_concentration, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(esfc_be_t30, elite_sports_funding_concentration, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(elite_sports_funding_concentration, resource_allocation).
narrative_ontology:affects_constraint(elite_sports_funding_concentration, olympic_qualification_gatekeeping).
narrative_ontology:affects_constraint(elite_sports_funding_concentration, broadcast_media_concentration).
narrative_ontology:affects_constraint(elite_sports_funding_concentration, international_athlete_migration).

% DUAL FORMULATION NOTE:
% Elite sports funding concentration is downstream of Olympic framework and broadcast media concentration but represents a distinct structural constraint on resource distribution. Each upstream constraint has its own extractiveness reflecting its specific mechanism; the funding concentration captures the combined effect of Olympic gatekeeping plus media concentration plus institutional inertia in public sports policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(elite_sports_funding_concentration, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
