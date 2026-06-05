% ============================================================================
% CONSTRAINT STORY: nba_franchise_valuation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nba_franchise_valuation, []).

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
 *   constraint_id: nba_franchise_valuation
 *   human_readable: NBA Franchise Valuation and Expansion Pricing Asymmetry
 *   domain: sports_economics/asset_valuation
 *
 * SUMMARY:
 *   NBA franchise valuation operates as a structural constraint on capital
 *   formation in professional sports. The constraint couples artificial
 *   scarcity (limited franchise licenses), market dominance (NBA's
 *   near-monopoly on elite basketball competition), and extraction mechanisms
 *   (expansion fees, relocation threats) into a system that creates
 *   asymmetric wealth capture. Incumbent owners benefit from appreciation
 *   driven by league growth; prospective owners face artificial entry
 *   barriers; players face suppressed wage growth justified by valuation
 *   metrics; host cities face relocation extraction. The same institutional
 *   arrangement appears as legitimate coordination from the league office and
 *   incumbent owner perspectives, but as pure extraction from prospective
 *   owner and city government perspectives. The theater component reflects
 *   that franchise valuations (especially Forbes annual rankings) are
 *   partially performative metrics used to justify governance decisions and
 *   labor negotiations rather than precise reflections of operational value.
 *
 * KEY AGENTS:
 *   - Incumbent Franchise Owners: Primary beneficiaries (institutional/arbitrage) — capture capital appreciation and monopoly rents through league scarcity, can arbitrage ownership interests
 *   - Prospective Owners: Primary victims (powerless/trapped) — face $2.5B+ expansion fees with no legitimate alternative path to NBA ownership, cannot negotiate entry price or create parallel competition
 *   - NBA League Office: Institutional beneficiary (institutional/arbitrage) — captures expansion fees, manages collective coordination, benefits from league appreciation
 *   - Players' Collective: Constrained agents (organized/constrained) — benefit from higher team revenues but face suppressed wage growth justified by valuation metrics
 *   - Competing Sports Operators: Constrained secondary actors (powerful/constrained) — could theoretically create alternatives but face significant coordination barriers
 *   - Host City Governments: Secondary victims (moderate/trapped in relocation negotiations) — face extraction through stadium financing demands using relocation threat as leverage
 *   - Secondary Market / Investment Banking: Institutional degraded actors (institutional/arbitrage) — maintain performative valuation theater through modeling inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nba_franchise_valuation, 0.58).
domain_priors:suppression_score(nba_franchise_valuation, 0.65).
domain_priors:theater_ratio(nba_franchise_valuation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nba_franchise_valuation, extractiveness, 0.58).
narrative_ontology:constraint_metric(nba_franchise_valuation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(nba_franchise_valuation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nba_franchise_valuation, tangled_rope).
narrative_ontology:human_readable(nba_franchise_valuation, "NBA Franchise Valuation and Expansion Pricing Asymmetry").
narrative_ontology:topic_domain(nba_franchise_valuation, "sports_economics/asset_valuation").

domain_priors:requires_active_enforcement(nba_franchise_valuation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nba_franchise_valuation, incumbent_franchise_owners).
narrative_ontology:constraint_beneficiary(nba_franchise_valuation, nba_league_office).
narrative_ontology:constraint_victim(nba_franchise_valuation, prospective_owners).
narrative_ontology:constraint_victim(nba_franchise_valuation, player_salary_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROSPECTIVE OWNER (SNARE) — Faces NBA expansion fees ($2.5B+) with no legitimate alternative path to NBA ownership. Cannot create parallel league, cannot negotiate entry price downward, cannot exit without abandoning ownership aspiration. Bears full cost of artificial scarcity while incumbent owners capture monopoly rents. Maximum suppression through exclusive league control.
constraint_indexing:constraint_classification(nba_franchise_valuation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETING SPORTS LEAGUE (TANGLED ROPE) — Could theoretically create alternative professional basketball league (XBA, ABA model), but faces significant coordination barriers: player acquisition costs, arena infrastructure gaps, media distribution constraints. Constrained exit with genuine but surmountable costs. Experiences both extraction (NBA scarcity premium inflates franchise values) and coordination benefit (NBA's established market infrastructure, talent pipeline). Asymmetric: NBA extracts from league operators through market dominance while providing coordination through established legitimacy.
constraint_indexing:constraint_classification(nba_franchise_valuation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT FRANCHISE OWNER (ROPE) — Experiences constraint as pure coordination: league governance, revenue sharing, scheduling, media distribution, player draft system. All mechanisms that create value through collective organization. Net beneficiary through expansion fees (each new franchise increases league value, enabling capital appreciation for existing franchises). Arbitrage exit available through secondary market sale. Low experienced extraction — the constraint subsidizes this agent.
constraint_indexing:constraint_classification(nba_franchise_valuation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NBA LEAGUE OFFICE (ROPE) — Functions as both coordinator and beneficiary. Coordinates franchises' competitive balance, revenue sharing, labor relations, media rights. Also captures expansion fees and growth in league valuation as expansion increases total market size. Arbitrage exit through commission-based governance model. Sees constraint as legitimate coordination with self-interested administration — not zero-sum extraction, but net positive value creation through unified governance.
constraint_indexing:constraint_classification(nba_franchise_valuation, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PLAYERS' COLLECTIVE (SCAFFOLD) — Experiences franchise valuation constraint as mixed: existing high valuations increase team revenue (improving salary cap room), but expansion pricing artificially inflates total league valuation, which can be used to justify lower player percentage-of-revenue allocation. Constrained exit through collective bargaining. Sees the constraint as a temporary negotiation point with sunset logic: if expansion pauses or franchise scarcity diminishes, valuation leverage shifts. Theater component: franchise valuation metrics (Forbes valuations) are partially performative, used to justify labor negotiations rather than reflecting operational cash flow.
constraint_indexing:constraint_classification(nba_franchise_valuation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SECONDARY MARKET & INVESTMENT BANKING (PITON) — Traditional valuation models for sports franchises (comparable transaction multiples, revenue capitalization, growth projections) persist largely through institutional inertia despite poor predictive power for non-traded assets. Banking fees and advisory economics drive continued use of these models even as alternative metrics (streaming rights value, arena asset value, brand licensing) become more relevant. The constraint appears as degraded institutional practice: valuation theater maintained because the system benefits from continued mystification of franchise value. Theater ratio high because actual franchise value is difficult to compute independently.
constraint_indexing:constraint_classification(nba_franchise_valuation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/global perspective, franchise scarcity and valuation asymmetry might appear as natural economic constraints: limited number of metropolitan areas can support NBA-caliber franchises, network effects create natural monopoly, capital requirements create inherent barriers to entry. This perspective naturalizes contingent institutional arrangements (league exclusivity, expansion fee setting, relocation restrictions) as immutable economic laws. However, the structural data reveals this as a false summit — the 'natural scarcity' is actively maintained through governance decisions, not inherent to sports commerce itself.
constraint_indexing:constraint_classification(nba_franchise_valuation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nba_franchise_valuation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nba_franchise_valuation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nba_franchise_valuation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nba_franchise_valuation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nba_franchise_valuation, TR),
    TR >= 0.70.

:- end_tests(nba_franchise_valuation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint generates genuine extraction: prospective owners pay artificial scarcity premiums ($2.5B+ expansion fees), players have suppressed salary growth partially justified through valuation narratives, host cities pay relocation extraction through stadium deals. But extraction is not maximal because expansion does create some genuine value (new market entry, league-wide revenue increase, established infrastructure benefits). The extractiveness has increased from 0.38 to 0.58 over the 20-year interval as expansion fees have risen and franchise valuations have grown faster than operational metrics justify. Suppression (0.65): Moderate-high. Barriers to entry include: exclusive NBA license (cannot create parallel legitimate competition), capital requirements ($2.5B+), market scarcity (limited metropolitan areas support NBA franchises), league governance control (can reject ownership applicants), relocation control (franchise movement used as leverage). However, suppression is not total — wealthy entrepreneurs can pay expansion fees, alternative leagues have historically emerged (ABA), public pressure has forced some expansion. Theater ratio (0.68): Moderately high. Forbes franchise valuations function partly as performance theater — used to justify governance decisions and labor negotiations but based on non-standardized methodologies that differ significantly from actual cash flow analysis. The theater has increased as valuation metrics become decoupled from operational data.
 *
 * PERSPECTIVAL GAP:
 *   The constraint shows distinct gaps between perspectives. Beneficiary perspectives (incumbent, league office) see Rope; organized perspectives with options (competing leagues, players' union) see Tangled Rope or Scaffold; trapped perspectives (prospective owners) see Snare. The gap width indicates strong extraction asymmetry — the same constraint structure produces fundamentally different classification outcomes based on the observer's structural position relative to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from power level, exit options, and beneficiary/victim status. Incumbent owners with arbitrage exits and beneficiary status get low d (~0.15) producing negative effective extraction (chi < 0). Prospective owners with trapped status and victim position get high d (~0.92) producing high effective extraction. League office with arbitrage and beneficiary position gets low d (~0.10). Players' union with constrained status and mixed victim/beneficiary gets moderate d (~0.55). The constraint's asymmetry reflects that exit options and structural position determine experienced extractiveness independent of nominal power level — a powerless prospective owner experiences higher chi than a powerful competing league operator with exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely Tangled Rope: it provides coordination value (unified league governance, revenue sharing, competitive balance) while extracting asymmetrically (artificial scarcity premium on expansion franchises, relocation threat extraction on cities, suppressed wage growth for players). The beneficiary perspectives (Rope) correctly identify the coordination function; the victim perspective (Snare) correctly identifies the extraction mechanism; the organized observer perspective (Scaffold) correctly identifies that the constraint has governance sunset potential (expansion pause would reduce scarcity premium). No single classification is 'correct' — the constraint legitimately exhibits both coordination and extraction functions, with asymmetric distribution of burden.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    franchise_scarcity_exogeneity,
    'Is NBA franchise scarcity a natural property of metropolitan market size or an artificially maintained governance constraint?',
    'Comparative analysis of sports leagues with different expansion policies; simulation of alternative league structures with higher franchise density; historical analysis of expansion decisions relative to qualified ownership applicants',
    'If natural: constraint approaching mountain classification, legitimizing scarcity premium. If artificial: constraint is pure extraction mechanism (snare) maintained by incumbent gatekeeping, reducing to higher chi values and stronger victim categorization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(franchise_scarcity_exogeneity, empirical, 'Whether NBA franchise scarcity is natural or maintained').

omega_variable(
    expansion_fee_capitalization,
    'Does the NBA expansion fee ($2.5B+) represent genuine value creation (new market entry, league revenue increase) or extraction from prospective owners?',
    'Pre- vs. post-expansion financial modeling of league-wide revenue growth; attribution of revenue growth to expansion franchise vs. existing franchises; analysis of whether expansion increases total league value beyond the fee amount collected',
    'If genuine value creation: constraint is primarily Rope (coordination through expansion), reducing victim characterization. If extraction mechanism: constraint is primarily Snare for prospective owners and Tangled Rope overall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansion_fee_capitalization, empirical, 'Whether expansion fees represent value creation or extraction').

omega_variable(
    player_salary_ceiling_mechanism,
    'Does franchise valuation constraint operate as a ceiling on player salary growth through revenue justification narratives, even when actual team profitability permits higher compensation?',
    'Analysis of salary cap growth rate vs. franchise valuation growth rate; franchise profitability comparison across valuation tiers; player-side perception data on whether franchise valuation metrics are used in labor negotiations',
    'If yes: constraint generates secondary extraction channel (players bear opportunity cost of suppressed wages justified by valuation metrics), elevating victim impact and extractiveness. If no: player impact is primarily through salary cap coordination rather than valuation-driven extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(player_salary_ceiling_mechanism, empirical, 'Whether franchise valuation constrains player salary growth').

omega_variable(
    relocation_threat_extraction,
    'Does the threat of franchise relocation function as an extraction mechanism on host cities through stadium financing demands and tax concessions?',
    'Analysis of public subsidies requested during franchise ownership transitions; comparison of relocation threat intensity to subsidy amounts; historical cases where threat credibility changed outcome',
    'If relocation threat is credible extraction: constraint extends to municipal governments as secondary victims, significantly broadening victim scope and increasing systemic extractiveness chi values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(relocation_threat_extraction, empirical, 'Whether relocation threats extract from host cities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nba_franchise_valuation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nbafv_tr_t0, nba_franchise_valuation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(nbafv_tr_t10, nba_franchise_valuation, theater_ratio, 10, 0.62).
narrative_ontology:measurement(nbafv_tr_t20, nba_franchise_valuation, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(nbafv_be_t0, nba_franchise_valuation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(nbafv_be_t10, nba_franchise_valuation, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(nbafv_be_t20, nba_franchise_valuation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nba_franchise_valuation, resource_allocation).
narrative_ontology:affects_constraint(nba_franchise_valuation, player_salary_cap_negotiation).
narrative_ontology:affects_constraint(nba_franchise_valuation, stadium_public_subsidy_extraction).
narrative_ontology:affects_constraint(nba_franchise_valuation, sports_league_competitive_balance).

% DUAL FORMULATION NOTE:
% NBA franchise valuation is downstream of league governance structure and upstream of labor negotiation and municipal finance constraints. The scarcity premium mechanism connects league monopoly control to player wage suppression and public subsidy extraction through shared metric (franchise valuation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nba_franchise_valuation, powerful, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
