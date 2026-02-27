% ============================================================================
% CONSTRAINT STORY: mltt_economic_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mltt_economic_model, []).

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
 *   constraint_id: mltt_economic_model
 *   human_readable: Major League Table Tennis Economic Model
 *   domain: sports/economic_organization
 *
 * SUMMARY:
 *   The Major League Table Tennis economic model represents a newly
 *   consolidated organizational structure in a historically fragmented sport.
 *   MLTT centralizes professional competition, sponsorship, media rights, and
 *   player compensation previously distributed across independent
 *   tournaments, regional associations, and grassroots clubs. This constraint
 *   exhibits the core characteristics of Tangled Rope: it solves genuine
 *   coordination problems (unified broadcast standards, international
 *   tournament scheduling, centralized marketing) while simultaneously
 *   extracting from development-tier players who lack negotiating power and
 *   grassroots clubs whose talent pipeline is now controlled by league
 *   franchises. The constraint appears as pure extraction (Snare) to trapped
 *   development players, as coordination benefit (Rope) to institutional
 *   league owners, and as degraded institutional authority (Piton) to
 *   international federation structures that predate MLTT. The theater ratio
 *   has declined over the first six years as the organizational structure has
 *   stabilized—early promotional language about 'revolutionary
 *   professionalization' has given way to operational focus on franchise
 *   performance and player contracts.
 *
 * KEY AGENTS:
 *   - League Ownership and Franchises: Institutional beneficiary (institutional/arbitrage) — capture concentrated sponsorship revenue and media rights, solve coordination problem of talent concentration
 *   - Elite Players (Top 50 Ranked): Secondary beneficiary (moderate/constrained) — gain access to centralized salary structure, broadcast exposure, and infrastructure while accepting contractual constraints
 *   - Development Tier Players (Ranked 51-300): Primary victim (powerless/trapped) — dependent on league advancement pathway with limited alternative professional opportunities; accept below-market salaries and restrictive contracts
 *   - Grassroots Clubs and Regional Associations: Secondary victim (powerless/trapped) — lose autonomy and revenue as talent pipeline becomes league-controlled; bear training costs for players who may be extracted by franchises
 *   - Players Association: Organized intermediary (organized/mobile) — negotiates terms for players but faces structural asymmetry between elite and development players
 *   - International Table Tennis Federation: Institutional observer (institutional/arbitrage) — retains nominal governance authority but experiences degraded function as MLTT becomes de facto governing body
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mltt_economic_model, 0.52).
domain_priors:suppression_score(mltt_economic_model, 0.48).
domain_priors:theater_ratio(mltt_economic_model, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mltt_economic_model, extractiveness, 0.52).
narrative_ontology:constraint_metric(mltt_economic_model, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(mltt_economic_model, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mltt_economic_model, tangled_rope).
narrative_ontology:human_readable(mltt_economic_model, "Major League Table Tennis Economic Model").
narrative_ontology:topic_domain(mltt_economic_model, "sports/economic_organization").

domain_priors:requires_active_enforcement(mltt_economic_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mltt_economic_model, league_ownership).
narrative_ontology:constraint_beneficiary(mltt_economic_model, franchise_operators).
narrative_ontology:constraint_beneficiary(mltt_economic_model, elite_players).
narrative_ontology:constraint_victim(mltt_economic_model, development_tier_players).
narrative_ontology:constraint_victim(mltt_economic_model, grassroots_table_tennis_clubs).
narrative_ontology:constraint_victim(mltt_economic_model, amateur_competitive_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPMENT TIER PLAYER (SNARE) — Trapped in a system where advancement requires acceptance of league contract terms with limited negotiating power. High suppression of alternatives (limited professional opportunities outside MLTT). Cannot exit without sacrificing career development pathway. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(mltt_economic_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GRASSROOTS CLUB ECOSYSTEM (SNARE) — Local and regional clubs face extraction through league centralization policies that concentrate prize money, sponsorships, and media attention at the MLTT level. Trapped in providing talent pipeline while bearing training costs. Clubs cannot exit without abandoning pathway to visibility. d≈0.88, f(d)≈1.35, σ=0.9 → χ≈0.60.
constraint_indexing:constraint_classification(mltt_economic_model, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE PLAYER (TANGLED ROPE) — High-ranked players benefit from centralized league sponsorship, media coverage, and prize distribution while bearing constraints on team composition, scheduling, and contractual restrictions. Extraction exists (salary cap, revenue sharing limits) but coordination benefits (guaranteed income, infrastructure, international exposure) are genuine. d≈0.58, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(mltt_economic_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LEAGUE OWNERSHIP AND FRANCHISE OPERATORS (ROPE) — Primary beneficiaries experiencing the constraint as coordination mechanism. League structure solves collective action problem of talent concentration and broadcast rights negotiation. Franchises can arbitrage between league rules and player development. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.04. Net beneficiaries.
constraint_indexing:constraint_classification(mltt_economic_model, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PLAYERS ASSOCIATION AND REGULATORY BODIES (TANGLED ROPE) — Organized agents with agency to negotiate terms and enforce standards. Experience mixed coordination (unified rules, dispute resolution) and extraction (revenue caps, restricted mobility). Association has mobile exit option (strike, legal challenge) but faces institutional barriers. d≈0.52, f(d)≈0.70, σ=1.0 → χ≈0.37.
constraint_indexing:constraint_classification(mltt_economic_model, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL TABLE TENNIS FEDERATION STRUCTURES (PITON) — Legacy federation governance persists through institutional inertia despite MLTT creating parallel organizational structure. Dual governance theater (both ITTF and MLTT claim authority). theater_ratio=0.58 indicates moderate performative content in maintaining jurisdictional claims. Degraded function: international coordination happens through MLTT; federation maintains ceremonial role.
constraint_indexing:constraint_classification(mltt_economic_model, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From civilizational timescale, league-based professionalization in sport appears as inevitable structural evolution. Centralized economic models emerge as irreducible constraint from competitive market dynamics: concentration creates efficiency, networks effects drive consolidation. However, structural data (ε=0.52, suppression=0.48) contradicts mountain classification — empirically, alternative models exist (cooperative leagues, player-owned franchises, guild systems). False summit detection: constraint is contingent institutional choice, not immutable law.
constraint_indexing:constraint_classification(mltt_economic_model, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mltt_economic_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mltt_economic_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mltt_economic_model, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mltt_economic_model, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mltt_economic_model, TR),
    TR >= 0.70.

:- end_tests(mltt_economic_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. MLTT captures meaningful economic surplus from two victim groups: development-tier players face below-market compensation due to limited alternatives; grassroots clubs lose autonomous revenue streams (sponsorships, participant fees flowing to franchises). However, extractiveness is not maximal (0.66+) because elite players do receive material benefits (salary stability, broadcast exposure, infrastructure investment) and coordination functions are genuine (tournament scheduling, broadcast standards, player development programs). The extraction is hybrid rather than pure. Suppression (0.48): Moderate. Development-tier players and grassroots clubs face real barriers to exit (limited alternative professional opportunities, dependence on league for competitive visibility) but not absolute suppression—alternative models exist (independent tournaments, international club circuits, coaching careers). Suppression has structural sources (market concentration) rather than explicit coercion. Theater ratio (0.35): Low. The league exhibits functional coordination with minimal performative content—operational structure (franchise ownership, player contracts, tournament logistics) has real economic consequences rather than symbolic function. The declining trajectory reflects maturation from promotional phase to stable operations.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates substantial perspectival divergence driven by exit options and beneficiary/victim positioning. Development-tier players classify the structure as Snare (high extraction, suppression, no exit) while institutional owners classify it as Rope (coordination with positive net benefit to league). Elite players occupy an intermediate position (Tangled Rope—genuine coordination benefit mixed with real contractual constraints). The organizational coalition (Players Association + Regulatory Bodies) sees a Tangled Rope with active enforcement, experiencing both coordination function and extraction pressure. The International Federation views MLTT as Piton—a degraded institutional structure that has been superseded but persists through inertia. The civilizational analytical observer risks classifying the league-based model as an immutable mountain (inevitable evolution of professionalized sport) but the existence of alternative economic models (player cooperatives, guild systems, open tournament circuits) reveals this as a false summit—the constraint is a contingent institutional choice enabled by market concentration, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   League ownership/franchises: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.07. Net beneficiaries with minimal extraction burden. Elite players: Beneficiary and victim (mixed) + constrained → d≈0.58, f(d)≈0.75. Moderate extraction: benefits from centralized system exceed constraints. Development-tier players: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction—no alternatives. Grassroots clubs: Victim + trapped → d≈0.88, f(d)≈1.35. High extraction through loss of autonomy and revenue. Players Association: Organized + mobile → d≈0.52, f(d)≈0.70. Can negotiate and has strike capability but faces structural constraints from player-class asymmetry. International Federation: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Retains nominal authority but has little active role—Piton classification emerges from theater ratio, not directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying that the tangled rope classification is not a mislabeling of either pure coordination or pure extraction. MLTT exhibits genuine coordination function (unified broadcasting, international tournament scheduling, player development infrastructure) AND genuine asymmetric extraction (development-tier salary suppression, grassroots revenue consolidation, franchise ownership concentration). The classification would be false if extractiveness were low (would indicate Rope not Tangled Rope) or if coordination were absent (would indicate Snare). The presence of both elements at moderate intensity (ε=0.52, suppression=0.48, beneficiaries present, victims present, active enforcement required) confirms tangled rope rather than collapsing to either pure type. The mandatrophy is resolved through perspectival decomposition: different agents genuinely experience different constraint types because their structural positions generate different directionality values—this is not ambiguity but rather accurate representation of asymmetric constraint structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    franchise_financial_viability,
    'Are franchise financial models sustainable without extracting from development-tier players and grassroots clubs?',
    'Multi-year financial audit of franchise operations; comparative analysis with alternative league structures (cooperative, player-owned models); sensitivity analysis on revenue sources',
    'If sustainable: coordination model (Rope/Tangled Rope). If not: extraction is structurally required for league survival (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(franchise_financial_viability, empirical, 'Whether franchise models can be financially viable without extracting from development tier').

omega_variable(
    player_mobility_threshold,
    'What salary or contractual flexibility would convert development-tier players from trapped to constrained or mobile?',
    'Survey data on player career alternatives; analysis of contract opt-out frequency and utilization rates; international comparisons with less restrictive leagues',
    'If threshold is achievable: constraint degrades from Snare toward Tangled Rope. If threshold is economically infeasible: Snare classification is structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(player_mobility_threshold, empirical, 'Salary and contractual flexibility needed for player mobility').

omega_variable(
    grassroots_subsidy_requirement,
    'Do grassroots clubs require direct MLTT subsidy to remain viable, or is voluntary participation sufficient?',
    'Financial tracking of club closure rates pre- and post-MLTT; analysis of participation trends in regions with vs. without MLTT franchise presence; comparison with independent club models',
    'If subsidy required: victims are trapped (Snare). If voluntary: victims are constrained but not trapped (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grassroots_subsidy_requirement, empirical, 'Whether grassroots clubs require MLTT subsidies for viability').

omega_variable(
    international_federation_authority,
    'Does MLTT hold de facto governing authority over professional table tennis, or does ITTF retain residual control?',
    'Analysis of rule-setting precedence; tracking of player eligibility and sanctions decisions; documentation of jurisdictional disputes and outcomes',
    'If MLTT is dominant: federation is Piton (degraded). If authority is contested: constraint is Tangled Rope with regulatory capture risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_federation_authority, empirical, 'Whether MLTT or ITTF holds de facto governing authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mltt_economic_model, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mltt_tr_t0, mltt_economic_model, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mltt_tr_t3, mltt_economic_model, theater_ratio, 3, 0.38).
narrative_ontology:measurement(mltt_tr_t6, mltt_economic_model, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(mltt_be_t0, mltt_economic_model, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mltt_be_t3, mltt_economic_model, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(mltt_be_t6, mltt_economic_model, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mltt_economic_model, resource_allocation).
narrative_ontology:affects_constraint(mltt_economic_model, international_sports_league_proliferation).
narrative_ontology:affects_constraint(mltt_economic_model, athlete_unionization_dynamics).
narrative_ontology:affects_constraint(mltt_economic_model, grassroots_sports_participation_trends).

% DUAL FORMULATION NOTE:
% The MLTT economic model can be decomposed into two distinct constraints: (1) Professional League Coordination (ε≈0.15, primarily Rope) addressing genuine tournament scheduling and broadcast efficiency problems; (2) Economic Rent Extraction (ε≈0.58, primarily Snare) addressing talent concentration and development-tier salary suppression. The tangled rope classification at ε=0.52 reflects the empirical fact that both mechanisms operate simultaneously with similar intensity. Upstream constraints (international federation fragmentation, athlete talent concentration) create conditions enabling both the coordination and extraction functions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mltt_economic_model, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
