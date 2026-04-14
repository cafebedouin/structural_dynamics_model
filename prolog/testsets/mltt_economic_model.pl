% ============================================================================
% CONSTRAINT STORY: mltt_economic_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   domain: economic/sports/labor
 *
 * SUMMARY:
 *   Major League Table Tennis (MLTT) represents a transition from amateur-era
 *   governance to professional sports franchising. The constraint emerges
 *   from the league's centralized authority over player contracts, exclusive
 *   representation, revenue-sharing mechanisms, and scheduling control.
 *   Players gain access to professional income streams and broadcast exposure
 *   previously unavailable through fractured independent tournaments.
 *   Ownership and media partners gain consolidated talent aggregation and
 *   predictable revenue models. The economic model exhibits classic
 *   sports-industry features: monopsony power over labor (players have
 *   limited alternatives), asymmetric information (league controls broadcast
 *   valuation), and high barriers to competitive league entry. Extractiveness
 *   is moderate (0.52) rather than severe because the league does provide
 *   genuine coordination value — it legitimizes professional play, attracts
 *   media investment, and creates stable income that most players prefer to
 *   the alternative of tournament-to-tournament independence. However,
 *   suppression is substantial (0.45) because players face exclusive
 *   representation requirements, revenue-sharing caps, and limited
 *   negotiation authority. The constraint exhibits all six classification
 *   types from different perspectives, with the core tension between the
 *   league's genuine coordination function and its asymmetric extraction of
 *   player market value.
 *
 * KEY AGENTS:
 *   - League Ownership: Primary beneficiary (institutional/arbitrage) — captures monopsony rents from player aggregation; controls franchise valuations and broadcast negotiation
 *   - Broadcasting Partners: Secondary beneficiary (institutional/arbitrage) — access to consolidated talent and predictable scheduling; willingly pay because league provides programming value
 *   - Established Players: Mixed (moderate/constrained) — benefit from professionalization and guaranteed earnings, constrained by revenue-sharing and exclusive representation
 *   - Emerging Players: Primary victim (powerless/trapped) — limited alternatives to league membership; face unfavorable contract terms due to power asymmetry
 *   - Independent Tournament Operators: Secondary victim (organized/constrained) — suppressed by exclusive league contracts and player scheduling monopoly
 *   - Player Labor Autonomy: Structural victim (abstract/trapped) — players surrender individual negotiation authority to league-mediated representation
 *   - ITTF and Traditional Governance: Institutional actor in decline (institutional/arbitrage) — sees functional authority degraded by professional decoupling; maintains theatrical role through ranking systems and amateur tier
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mltt_economic_model, 0.52).
domain_priors:suppression_score(mltt_economic_model, 0.45).
domain_priors:theater_ratio(mltt_economic_model, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mltt_economic_model, extractiveness, 0.52).
narrative_ontology:constraint_metric(mltt_economic_model, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(mltt_economic_model, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mltt_economic_model, tangled_rope).
narrative_ontology:human_readable(mltt_economic_model, "Major League Table Tennis Economic Model").
narrative_ontology:topic_domain(mltt_economic_model, "economic/sports/labor").

domain_priors:requires_active_enforcement(mltt_economic_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mltt_economic_model, league_ownership).
narrative_ontology:constraint_beneficiary(mltt_economic_model, established_players).
narrative_ontology:constraint_beneficiary(mltt_economic_model, broadcasting_partners).
narrative_ontology:constraint_victim(mltt_economic_model, emerging_players).
narrative_ontology:constraint_victim(mltt_economic_model, independent_tournaments).
narrative_ontology:constraint_victim(mltt_economic_model, player_labor_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING PLAYER (SNARE) — Entry to professional play requires league membership and exclusive contracts. Limited alternative pathways for income; international tournament access mediated by league approval. High suppression of alternatives; trapped within league structure despite unfavorable contract terms. Maximum experienced extraction for players without established reputation.
constraint_indexing:constraint_classification(mltt_economic_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ESTABLISHED PLAYER (TANGLED ROPE) — Benefits from league professionalization, centralized sponsorship, and guaranteed earnings floor. Constrained by exclusive representation and revenue-sharing model that captures 40-50% of individual sponsorship. Hybrid experience: coordination function (legitimizes professional play, attracts broadcast revenue) coupled with asymmetric extraction (league captures disproportionate share of player market value).
constraint_indexing:constraint_classification(mltt_economic_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEAGUE OWNERSHIP AND MEDIA PARTNERS (ROPE) — Experience constraint as pure coordination mechanism. League aggregates player talent into franchises, creates predictable scheduling, attracts broadcast contracts, and generates revenue streams that benefit all participants. Ownership captures arbitrage advantage through league formation (converts fractured player base into consolidated asset). Low extraction overhead relative to coordination benefit — media partners willingly pay because league provides programming value.
constraint_indexing:constraint_classification(mltt_economic_model, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDEPENDENT TOURNAMENT OPERATORS (TANGLED ROPE) — Face suppression of their tournament model by exclusive league contracts requiring players to participate only in sanctioned events. Experience both coordination (league professionalization increases overall sport visibility, which can benefit remaining independent events) and extraction (league monopsony power over player scheduling reduces independent tournament viability). Constrained by league scheduling authority; organized as a collective but with limited negotiating power.
constraint_indexing:constraint_classification(mltt_economic_model, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL TABLE TENNIS GOVERNANCE (PITON) — International Table Tennis Federation (ITTF) and legacy amateur-era governance structures persist in parallel with MLTT but have lost functional authority over professional players. Theater ratio high: ITTF maintains ranking systems, tournament sanctions, and organizational legitimacy despite reduced economic relevance. Professional players increasingly ignore ITTF prioritization in favor of league scheduling. Piton classification reflects degraded coordination function maintained through institutional inertia rather than actual control over player careers.
constraint_indexing:constraint_classification(mltt_economic_model, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PLAYER UNION / COLLECTIVE BARGAINING (SCAFFOLD) — Emerging union organizing sees the league structure as temporary coordination problem with a negotiated sunset. Current suppression of player autonomy (exclusive contracts, revenue-sharing caps, limited negotiation authority) is viewed as a startup phase constraint that should diminish as player bargaining power grows. Union perspective frames league enforcement mechanisms as high-theater performance of ownership authority — enforcement weakens as collective action capacity increases. Sunset mechanism: maturation of union representation transitioning snare (for emerging players) toward rope (mutual benefit).
constraint_indexing:constraint_classification(mltt_economic_model, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / MARKET STRUCTURE (MOUNTAIN) — From a civilizational economic perspective, some degree of centralized talent aggregation and monopsony power is inherent to professional sports professionalization. All mature sports markets (NBA, Premier League, professional tennis) exhibit league-mediated player representation and concentrated revenue capture by ownership. This perspective risks naturalizing contingent institutional arrangements (exclusive contracts, revenue-sharing models, team-based franchising) as inevitable economic laws. Engine's false summit detection should flag this as naturalization of negotiable terms.
constraint_indexing:constraint_classification(mltt_economic_model, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (0.52): Moderate-high, reflecting asymmetric value capture. MLTT generates revenue through broadcasting, sponsorship, and event operations. Distribution models typically allocate 40-50% to ownership/league operations, 40-50% to players (distributed unequally by player tier), and 5-15% to operational costs. For individual players, league structures typically capture 20-40% of their personal sponsorship deals and require exclusive scheduling commitments that prevent alternative income streams. The extraction is not maximal because genuine coordination value is created: players earn substantially more through league participation than through independent alternatives, and the league does solve the collective action problem of talent aggregation. Suppression (0.45): Moderate, reflecting barriers to exit and limited alternatives. Barriers include: exclusive representation contracts (typically 2-5 year terms), geographic concentration of professional opportunities within the league, international federation scheduling priorities favoring league events, and reputational risk of defection. However, suppression is not maximal because some players can and do maintain independent tournament participation, and exit negotiations are sometimes possible. Theater ratio (0.38): Below average, indicating relatively low performative content. League operations are functionally oriented toward broadcast production, player management, and revenue generation. Governance procedures (draft systems, ranking-based tournament seeding, franchise ownership rules) have real administrative impact rather than purely theatrical function. The theater_ratio remains nonzero because some league operations (franchise brand-building, ownership narratives, governance committees) serve publicity functions beyond functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same institutional structure appears as snare, tangled_rope, rope, scaffold, piton, and false mountain depending on structural position. This perspectival diversity reflects genuine disagreement about whether MLTT is primarily coordination (solving fragmented player base) or extraction (capturing player value). Emerging players clearly see extraction; ownership clearly sees coordination; established players experience both simultaneously. The union/scaffold perspective introduces a temporal dimension: current constraint structure is viewed as temporary startup friction that should dissolve as bargaining power matures. The piton perspective (traditional governance degradation) and false mountain perspective (naturalization of sports industry structure) reveal how powerful institutional arrangements can be misclassified as immutable law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value derives from the agent's position relative to the extraction flow. Emerging players (powerless/trapped) experience high d (~0.90): they are targets of extraction with no exit options. Their compensation is below marginal revenue product, and suppression of alternatives forces acceptance. Established players (moderate/constrained) experience moderate d (~0.55): they benefit from league coordination but constrained exit prevents them from capturing full market value of their performance. League ownership (institutional/arbitrage) experience low d (~0.15): they are primary beneficiaries with complete exit flexibility (can restructure leagues, relocate, or sell franchises). Independent tournaments (organized/constrained) experience moderate-high d (~0.70): they are suppressed by league competition and scheduling monopoly but maintain some organizational capacity. The engine derives these d values from beneficiary/victim declarations and exit options; no manual override is needed for this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that tangled_rope classification (simultaneous coordination and asymmetric extraction) accurately captures the league structure, preventing misclassification as either pure rope (coordination with minimal extraction) or pure snare (extraction without coordination). The league genuinely solves the collective action problem of professional talent aggregation AND genuinely extracts player market value through monopsony power. Both functions are real and structural. The constraint remains tangled_rope across its interval despite rising extractiveness (0.38→0.52), indicating that extraction mechanisms are strengthening while coordination value is static. This trajectory suggests evolution toward snare (if extraction continues to dominate) unless countervailing forces (union formation, alternative leagues, broadcast negotiation power) restore balance. The mandate for high-extractiveness constraints (>0.70) would require `mandatrophy_resolved: true`, but this constraint (0.52) remains below that threshold and therefore does not trigger the mandate. However, if extractiveness continues rising above 0.70, the constraint would need explicit resolution documentation explaining how it avoids misclassification as pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    player_union_formation_timeline,
    'When will prospective player union achieve recognition and bargaining authority, and how will this alter revenue-sharing terms?',
    'Historical tracking of union organizing campaigns; comparison with timelines in comparable sports (professional tennis player associations, NBA players union emergence); survey of player organizing activity and stated demands',
    'If union achieves recognition within 3-5 years: scaffold sunset is real, classification shifts toward rope for established players and toward tangled_rope (with lower extraction) for emerging players. If union organizing fails: snare classification solidifies, extraction mechanisms persist unchallenged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(player_union_formation_timeline, empirical, 'Timeline for player union formation and impact on revenue-sharing').

omega_variable(
    alternative_professional_league_viability,
    'Can competing professional table tennis leagues emerge with materially different economic models (higher player share, lower exclusivity)?',
    'Analysis of barriers to entry (equipment costs, broadcast infrastructure requirements, player poaching rules, international federation recognition); case studies of league formation in comparable sports (challenger leagues in tennis, professional badminton expansions)',
    'If viable alternatives emerge: league monopsony power weakens, suppression decreases, classification shifts from snare toward tangled_rope for emerging players. If barriers are insurmountable: monopsony persists, extraction mechanisms persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_professional_league_viability, empirical, 'Viability of competing professional leagues with alternative economic models').

omega_variable(
    revenue_concentration_trajectory,
    'Does MLTT revenue growth outpace or underperform comparable professional sports'' inaugural decades, and what does this imply about league sustainable extraction capacity?',
    'Longitudinal tracking of broadcast revenue, sponsorship deals, and player earnings; comparison with NBA, PGA, professional tennis, and European football first-decade trajectories; analysis of whether revenue growth supports player wage increases or accumulates as ownership profit',
    'If revenue growth is robust and shared: extraction ratios decline naturally, snare classification weakens over time. If revenue growth stalls or concentrates in ownership: extraction persists, snare solidifies, mandatrophy becomes unresolved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revenue_concentration_trajectory, empirical, 'Revenue concentration trajectory and league economic sustainability').

omega_variable(
    international_federation_authority_preservation,
    'Will ITTF retain or regain any regulatory authority over professional play, or does MLTT represent a permanent decoupling of amateur and professional governance?',
    'Tracking of ITTF policy statements, Olympic qualification criteria, international tournament rules evolution; analysis of whether professional players must maintain amateur standing or licensing with ITTF; comparison with professional tennis and its relationship with ITF',
    'If ITTF retains regulatory authority: piton classification may transition toward rope if ITTF reasserts functional role. If MLTT achieves complete autonomy: piton degradation solidifies, traditional governance becomes pure theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_federation_authority_preservation, empirical, 'ITTF authority preservation in relation to professional play').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mltt_economic_model, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mltt_tr_t0, mltt_economic_model, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mltt_tr_t2, mltt_economic_model, theater_ratio, 2, 0.32).
narrative_ontology:measurement(mltt_tr_t5, mltt_economic_model, theater_ratio, 5, 0.38).

% Extraction over time
narrative_ontology:measurement(mltt_be_t0, mltt_economic_model, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mltt_be_t2, mltt_economic_model, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(mltt_be_t5, mltt_economic_model, base_extractiveness, 5, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mltt_economic_model, resource_allocation).
narrative_ontology:affects_constraint(mltt_economic_model, professional_table_tennis_labor_markets).
narrative_ontology:affects_constraint(mltt_economic_model, international_sports_federation_governance).
narrative_ontology:affects_constraint(mltt_economic_model, broadcast_sports_economics).

% DUAL FORMULATION NOTE:
% MLTT represents a specific instance of professional sports league formation. The constraint family includes broader sports economics constraints (broadcast monopolies, labor monopsony in professional sports) and narrower constraints (specific MLTT franchise valuations, individual player contract terms). The extractiveness value (0.52) is specific to MLTT's current revenue-sharing model and may differ for other professional table tennis initiatives or alternative league structures. The upstream constraint (professional_table_tennis_labor_markets) is more general and likely has lower extractiveness if measured across all alternative organizational forms; MLTT is a specific institutional implementation that concentrates extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
