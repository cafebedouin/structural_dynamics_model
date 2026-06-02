% ============================================================================
% CONSTRAINT STORY: professional_table_tennis_labor_markets
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_professional_table_tennis_labor_markets, []).

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
 *   constraint_id: professional_table_tennis_labor_markets
 *   human_readable: Professional Table Tennis Labor Market Extraction
 *   domain: sports_economics/labor
 *
 * SUMMARY:
 *   The professional table tennis labor market exhibits a textbook
 *   tangled_rope structure: genuine coordination mechanisms (standardized
 *   training curricula, global ranking systems, tournament infrastructure
 *   enabling talent discovery) coexist with systematic asymmetric extraction.
 *   National federations and the ITTF organize global competition
 *   infrastructure that solves real coordination problems — without
 *   centralized ranking and tournament allocation, no global talent
 *   comparison or competitive opportunity structure would exist.
 *   Simultaneously, these same institutions extract through gated access to
 *   competition opportunities, fee structures that concentrate revenue in
 *   federation hands, and monopoly control over ranking pathways. The
 *   constraint is neither pure coordination nor pure extraction; it is a
 *   hybrid where the coordination function is genuine but enables extractive
 *   structures. The perspectival gap is sharp: elite players with
 *   international visibility (powerful, mobile exit) experience the
 *   constraint as manageable coordination with embedded fees; developmental
 *   athletes (powerless, trapped) experience it as a snare where federation
 *   control blocks all alternative pathways. The measurement trajectory shows
 *   extractiveness and suppression increasing over the 20-year interval,
 *   reflecting the growth of federation administrative infrastructure and the
 *   consolidation of ITTF ranking system control. Theater ratio remains
 *   moderate because the coordination functions are substantive — unlike
 *   purely performative institutional systems, federations genuinely organize
 *   tournaments, allocate ranking points, and maintain training standards.
 *   However, the theater is rising as bureaucratic gatekeeping (certification
 *   processes, tournament licensing, ranking appeals) grows relative to the
 *   actual training/competition value delivered.
 *
 * KEY AGENTS:
 *   - National Table Tennis Federations: Primary beneficiary (institutional/arbitrage) — collect membership fees, tournament revenue, training facility fees; control athlete pathway access; can arbitrage to esports or corporate events if athlete talent pool declines
 *   - ITTF Governance Structure: Primary beneficiary (institutional/arbitrage) — maintains global ranking system, allocates World Championship and Olympic slots, collects licensing fees; core coordination function (global ranking standardization) is genuine
 *   - Developmental-Stage Athletes: Primary victim (powerless/trapped) — invest years in federation-controlled training without guaranteed professional opportunity; no alternative competitive pathway; trapped by sunk costs and identity fusion with 'national team player' identity
 *   - Semi-Professional Regional Players: Secondary victim (moderate/constrained) — constrained by tournament access control and ranking system dependence; can exit but at high cost (losing ranking infrastructure, coaching network)
 *   - Elite Top-Tier Players (World Rank 1-50): Tertiary agent (powerful/mobile) — experience tangled_rope (genuine benefit from ranking/federation infrastructure coexists with extraction), but have exit options (private tournaments, sponsorship negotiation) that reduce experienced suppression
 *   - Private League Operators (WTT): Emerging alternative (organized/mobile) — creating parallel labor market outside federation control; pressure testing federation monopoly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(professional_table_tennis_labor_markets, 0.58).
domain_priors:suppression_score(professional_table_tennis_labor_markets, 0.62).
domain_priors:theater_ratio(professional_table_tennis_labor_markets, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(professional_table_tennis_labor_markets, extractiveness, 0.58).
narrative_ontology:constraint_metric(professional_table_tennis_labor_markets, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(professional_table_tennis_labor_markets, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(professional_table_tennis_labor_markets, tangled_rope).
narrative_ontology:human_readable(professional_table_tennis_labor_markets, "Professional Table Tennis Labor Market Extraction").
narrative_ontology:topic_domain(professional_table_tennis_labor_markets, "sports_economics/labor").

domain_priors:requires_active_enforcement(professional_table_tennis_labor_markets).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(professional_table_tennis_labor_markets, national_federations).
narrative_ontology:constraint_beneficiary(professional_table_tennis_labor_markets, ittf_administrative_structure).
narrative_ontology:constraint_beneficiary(professional_table_tennis_labor_markets, elite_top_tier_players).
narrative_ontology:constraint_victim(professional_table_tennis_labor_markets, emerging_semi_professional_players).
narrative_ontology:constraint_victim(professional_table_tennis_labor_markets, developmental_stage_athletes).
narrative_ontology:constraint_victim(professional_table_tennis_labor_markets, non_elite_competitive_players).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPMENTAL ATHLETE (SNARE) — Trapped in federation-controlled pathway with no exit. Player invests years in training, accepts low/unpaid wages, absorbs all performance risk while federation controls tournament access, ranking opportunity, and international competition slots. No alternative labor market exists; coaching licenses and training structures are federation-gatekept. Exit means abandoning invested identity and training infrastructure.
constraint_indexing:constraint_classification(professional_table_tennis_labor_markets, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SEMI-PROFESSIONAL REGIONAL PLAYER (TANGLED ROPE) — Benefits from federation training standards, coaching infrastructure, and regional competition ecosystem (genuine coordination). Simultaneously extracted through: tournament fee structures that profit federations, ranking manipulation (tournaments weighted to favor federation-sponsored events), tight control over international opportunity access. High cost to exit (lose ranking infrastructure, lose coaching network) but exit is structurally possible with 3-5 year career transition.
constraint_indexing:constraint_classification(professional_table_tennis_labor_markets, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NATIONAL FEDERATION (ROPE) — Pure coordination from federation perspective. Controls training curricula (real function: standardization enables talent comparison). Allocates tournament slots (real function: prevents chaos). Collects player fees and tournament revenue (real function: funds infrastructure). Net beneficiary — arbitrage exit available if federation loses athlete pool (can shift to esports, corporate retreats, or administrative consolidation). Experiences constraint as coordination mechanism where extraction is compensation for provision.
constraint_indexing:constraint_classification(professional_table_tennis_labor_markets, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ITTF GOVERNANCE (ROPE) — Coordinating global ranking systems, standardizing competition rules, allocating World Championship and Olympic slots. These are genuine coordination functions. ITTF extracts through: licensing fees, ranking formula control, tournament prioritization decisions that favor sponsors/wealthy federations. Can arbitrage out if global competition fragment (already happening with private league emergence). Experiences constraint as coordination with embedded fee structure.
constraint_indexing:constraint_classification(professional_table_tennis_labor_markets, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ELITE TOP-TIER PLAYERS (TANGLED ROPE) — Powerful agents with mobile exit (can negotiate appearance fees, private tournaments, sponsorship outside federation control, international transfers). Experience genuine coordination benefit: federation rankings drive sponsorship value, tournament infrastructure enables high-level competition. Simultaneous extraction through: appearance fees capped or collected by federations, mandatory ranking-point-awarding tournaments, limited flexibility in competition schedule. Lower suppression because these players can arbitrage — they coordinate and extract simultaneously but retain negotiating power.
constraint_indexing:constraint_classification(professional_table_tennis_labor_markets, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — Table tennis governance structures (national federations, ITTF) were built 50+ years ago as centralized talent development and competition systems. Theater ratio is moderate-to-high: extensive bureaucratic processes for ranking appeals, tournament licensing, athlete certification that produce limited functional verification of actual player capability (subjective selection for national teams still dominates ranking-based selection). The institutional machinery persists through Olympic/governmental funding and administrative inertia despite significant alternative pathways emerging (private leagues, streaming, direct sponsorship). Primary function has partially atrophied — talent discovery no longer depends on federation tournaments (YouTube, coaching visibility, international club competitions reveal talent), but federation gatekeeping persists.
constraint_indexing:constraint_classification(professional_table_tennis_labor_markets, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(professional_table_tennis_labor_markets_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(professional_table_tennis_labor_markets, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(professional_table_tennis_labor_markets, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(professional_table_tennis_labor_markets, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(professional_table_tennis_labor_markets, TR),
    TR >= 0.70.

:- end_tests(professional_table_tennis_labor_markets_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The federation structure extracts through multiple mechanisms: (1) gated access to ranking-point tournaments (players forced into federation circuit), (2) fee structures that profit federations while leaving athletes underpaid, (3) ranking formula arbitrariness that favors federation-sponsored events, (4) monopoly control over international competition opportunity. However, extractiveness is not at snare levels (≥0.66) because: (a) genuine coordination functions exist and are substantive, (b) elite players have arbitrage options, (c) private circuits are emerging. The measured 0.58 reflects moderate-to-high extraction embedded within functional coordination. Suppression (0.62): High. Multiple structural barriers: (1) gated access to training facilities (credential/cost barriers), (2) ranking dependence creates forced federation participation, (3) career risk of non-federation pathway (unranked players cannot access sponsorship/professional opportunities), (4) identity lock-in (players internalize 'federation player' identity as core to competitive identity). The suppression is structural (material barriers) and partially internalized (identity fusion). Theater ratio (0.48): Moderate. Federation coordination functions are substantive — ranking systems actually measure player capability comparatively, training standards actually improve player technique, tournaments genuinely generate competitive data. However, theater is present in: bureaucratic gatekeeping (certification/appeals processes), ranking formula changes that lack transparent merit-based justification, administrative overhead that doesn't translate to player development. Theater ratio is rising over the interval (0.38→0.48) as federation bureaucracy expands relative to core training/competition value.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival divergence is extreme. Elite top-50 players (powerful/mobile) see coordination-with-fees (tangled_rope): federation infrastructure is genuinely valuable for ranking visibility and international competition access, but fees and appearance regulations are negotiable extraction costs. Semi-professional players (moderate/constrained) see mixed coordination-and-extraction (tangled_rope): they benefit from federation training standards and regional competition, but face high costs to exit and are trapped in federation tournament circuit for ranking maintenance. Developmental athletes (powerless/trapped) see pure extraction (snare): federation control blocks all pathways, creates multi-year sunk-cost investments with no guaranteed professional outcome, and maintains identity lock-in. The national federation (institutional/arbitrage) sees pure coordination (rope): they solve tournament logistics, maintain ranking standards, allocate slots efficiently. The ITTF (institutional/arbitrage) sees coordination (rope): global standardization and Olympic/World Championship allocation are genuine functions. The analytical observer sees inertial institutional maintenance (piton): the core coordination functions (ranking, tournament logistics) are performed more efficiently by private platforms (Heisenflop algorithm for ratings, YouTube for talent discovery, private league tournaments for competitive opportunity), yet federation gatekeeping persists through governmental funding and Olympic linkage.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position — power level, exit options, and beneficiary/victim relationship. Developmental athletes: powerless + trapped + victim → high d → high f(d) ≈ 1.42, producing snare classification. Semi-professional players: moderate + constrained + mixed victim-beneficiary → mid d ≈ 0.60 → f(d) ≈ 0.85, producing tangled_rope. Elite players: powerful + mobile + mixed beneficiary-victim → low-mid d ≈ 0.45 → f(d) ≈ 0.55, producing tangled_rope with lower experienced extraction than semi-professionals. National federations: institutional + arbitrage + beneficiary → low d ≈ 0.15 → f(d) ≈ -0.01, producing rope (effective extraction experienced as negative — coordinative burden is offset by fee and control benefits). ITTF: institutional + arbitrage + beneficiary → low d ≈ 0.12 → f(d) ≈ -0.08, producing rope. The engine derives d automatically from beneficiary/victim declarations and exit options; the classification follows from chi = epsilon × f(d) × sigma(scope). No directionality overrides are needed — the structural derivation captures the true relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy — the indeterminacy of whether a constraint is coordination mechanism or extraction mechanism — is resolved here through structural differentiation. The federation IS a coordination mechanism for allocating global competition opportunity, maintaining ranking integrity, and training standardization. It IS ALSO an extraction mechanism for capturing fees from developmental athletes who have no exit options. The constraint is not 'actually' one or the other; it is genuinely both simultaneously, experienced differently by different agents. The 'solution' to mandatrophy is not to determine the 'true' nature but to acknowledge that tangled_rope classification is correct: the system exhibits both functions, and their distribution across agents is asymmetric (beneficiaries perceive coordination; victims perceive extraction). The measurement trajectory showing rising extractiveness and suppression does not indicate mandatrophy — it indicates the coordination function is real and persistent while extraction mechanisms are strengthening (tightening of gatekeeping, increasing fee structures). The constraint is not drifting from coordination toward extraction or vice versa; both are stable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federation_monitoring_capacity,
    'Do national federations possess genuine capacity to monitor and verify player development quality, or is the monitoring function primarily theater masking extractive fee collection?',
    'Audit of coaching credential standards, training facility inspections, performance outcome tracking vs. player-reported coaching quality; comparison of federation-trained vs. independently-trained players'' international outcomes',
    'If genuine capacity: tangled_rope classification holds — coordination function is real. If theater: classification shifts toward snare for developmental players — extraction is primary function, coordination is justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federation_monitoring_capacity, empirical, 'Whether federation monitoring provides substantive player development verification').

omega_variable(
    ranking_formula_arbitrariness,
    'Do ITTF ranking formulas systematically favor federation-controlled tournaments over independent club/private tournaments in ways that are arbitrary rather than merit-based?',
    'Statistical analysis of ranking point distribution by tournament type; correlation between point-per-match and actual subsequent head-to-head performance; time-series analysis of ranking volatility before/after formula changes',
    'If systematic arbitrary favoring: suppression increases (players forced into federation circuit to maintain ranking regardless of actual development opportunity). If formula is merit-neutral: suppression decreases (alternative pathways equally valued).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ranking_formula_arbitrariness, empirical, 'Whether ITTF ranking formulas systematically favor federation-controlled tournaments').

omega_variable(
    developmental_pathway_necessity,
    'Are federation-controlled developmental pathways (training camps, coaching certification, junior circuit) genuinely necessary for reaching elite levels, or can independent pathways (club coaching, private tournaments, streaming visibility) produce equivalent outcomes?',
    'Career trajectory analysis of top 100 players: proportion trained through federation pathway vs. independent pathway; comparison of career longevity, injury rates, and peak ranking by pathway; emergence of non-federation-trained world-ranked players',
    'If federation pathways are necessary: suppression is structural necessity, not extraction. If independent pathways are equivalent: suppression is extraction mechanism — players are forced into federation structures despite non-superior outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developmental_pathway_necessity, empirical, 'Whether federation developmental pathways are genuinely necessary for elite achievement').

omega_variable(
    alternative_professional_circuits_growth,
    'Are private professional table tennis leagues (WTT — World Table Tennis league) and independent tournament circuits growing fast enough to create genuine alternative labor markets outside federation control?',
    'Longitudinal growth data: player participation in non-ITTF-affiliated tournaments as percentage of total; player earnings from private circuits vs. federation-sanctioned tournaments; emergence of players who bypass federation junior circuit entirely',
    'If rapid growth: constraint is approaching sunset — private circuits are reducing federation leverage (scaffold rather than snare for emerging players). If stagnant or declining: federation monopoly holds (snare/tangled_rope persist).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_professional_circuits_growth, empirical, 'Growth trajectory of alternative professional table tennis circuits').

omega_variable(
    identity_lock_in_mechanism,
    'Is the constraint binding developmental players primarily through material barriers (controlled training facilities, gated competition access) or through identity fusion with federation-sponsored identity (''national team player'', ''federation-certified talent'')?',
    'Qualitative analysis: exit interviews with players who left competitive table tennis, career transition narratives; psychological assessment of identity separation post-retirement; measurement of identity lock vs. structural lock through exit cost decomposition',
    'If material barriers dominate: players trapped or constrained (external barriers). If identity fusion dominates: players identity_locked (cognitive binding despite structural mobility). Affects classification and intervention design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_mechanism, empirical, 'Whether developmental player binding is material or identity-based').

omega_variable(
    mandate_substitution_drift,
    'Has the federation mandate evolved from genuine player development coordination toward administrative self-perpetuation and fee extraction as the primary institutional goal?',
    'Historical document analysis: federation mission statements, budget allocation trends, tournament fee structures over 20-year period; measurement of correlation between fee increases and player development outcome improvements',
    'If drift confirmed: theater_ratio should increase, classification drifts toward piton/snare. Mandatrophy resolution requires acknowledging institutional goal substitution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_substitution_drift, conceptual, 'Drift from player development mandate toward institutional self-perpetuation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(professional_table_tennis_labor_markets, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pttlm_tr_t0, professional_table_tennis_labor_markets, theater_ratio, 0, 0.38).
narrative_ontology:measurement(pttlm_tr_t10, professional_table_tennis_labor_markets, theater_ratio, 10, 0.43).
narrative_ontology:measurement(pttlm_tr_t20, professional_table_tennis_labor_markets, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(pttlm_be_t0, professional_table_tennis_labor_markets, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pttlm_be_t10, professional_table_tennis_labor_markets, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(pttlm_be_t20, professional_table_tennis_labor_markets, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(pttlm_su_t0, professional_table_tennis_labor_markets, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(pttlm_su_t10, professional_table_tennis_labor_markets, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(pttlm_su_t20, professional_table_tennis_labor_markets, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(professional_table_tennis_labor_markets, resource_allocation).
narrative_ontology:affects_constraint(professional_table_tennis_labor_markets, olympic_sport_monopoly_allocation).
narrative_ontology:affects_constraint(professional_table_tennis_labor_markets, athlete_wage_suppression_across_niche_sports).

% DUAL FORMULATION NOTE:
% Professional table tennis labor market extraction is part of a larger constraint family around Olympic sport governance. The table tennis case exemplifies patterns that appear in archery, badminton, weightlifting, and other niche Olympic sports where national federations control both talent development and professional opportunity. A second story (olympic_sport_monopoly_allocation) examines the ITTF-IOC relationship and Olympic qualification gatekeeping; a third (athlete_wage_suppression_across_niche_sports) examines the common mechanism across all niche sports. The three stories have different epsilon values reflecting different scope and power relationships. Link them via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
