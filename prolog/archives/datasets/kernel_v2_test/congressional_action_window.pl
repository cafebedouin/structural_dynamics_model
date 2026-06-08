% ============================================================================
% CONSTRAINT STORY: congressional_action_window
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_congressional_action_window, []).

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
 *   constraint_id: congressional_action_window
 *   human_readable: Congressional Action Window During Debt Limit Episodes
 *   domain: public_finance/constitutional_law/political_economy
 *
 * SUMMARY:
 *   The congressional action window during debt limit episodes is the period
 *   between when the statutory borrowing limit is reached (limit arrival) and
 *   when Congress passes legislation to raise or suspend the limit. This
 *   window creates a high-stakes negotiating environment in which minority
 *   factions can extract policy concessions by threatening to withhold votes
 *   needed for a limit increase. The constraint's extractiveness has
 *   increased substantially over the 1995-2023 interval as episodes have
 *   become more frequent, more protracted, and more explicitly tied to
 *   unrelated policy demands. The 2011 episode resulted in the first-ever
 *   U.S. credit rating downgrade; the 2013 episode saw a 16-day government
 *   shutdown; the 2023 episode extended to within days of Treasury's
 *   projected X Date. The theater_ratio reflects that much of the action
 *   window's activity is performative: public statements about fiscal
 *   responsibility, symbolic votes, and deadline extensions that serve
 *   primarily as negotiating tactics rather than substantive fiscal
 *   deliberation. The suppression_requirement has increased as the constraint
 *   has become more entrenched: exit options for victims have narrowed (no
 *   alternative to the federal tax system, no way to avoid market exposure
 *   during episodes), and the political costs of reform have risen as the
 *   tactic has become normalized.
 *
 * KEY AGENTS:
 *   - Taxpayers: Primary victim (powerless/trapped) — bear increased borrowing costs and economic uncertainty with no exit
 *   - Federal Program Beneficiaries: Secondary victim (powerless/constrained) — face payment delays and program disruptions during episodes
 *   - Treasury Department: Institutional victim (institutional/constrained) — implements extraordinary measures and manages market disruption; experiences both coordination function and extraction
 *   - Political Leverage Seekers: Primary beneficiary (institutional/arbitrage) — extract policy concessions by threatening to withhold votes for limit increase
 *   - Short-Term Treasury Traders: Secondary beneficiary (institutional/arbitrage) — profit from yield spread volatility during episodes
 *   - Debt Limit Reform Coalition: Organized agents (organized/constrained) — see the constraint as transitional with multiple sunset pathways
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes hybrid coordination-extraction structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(congressional_action_window, 0.68).
domain_priors:suppression_score(congressional_action_window, 0.72).
domain_priors:theater_ratio(congressional_action_window, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(congressional_action_window, extractiveness, 0.68).
narrative_ontology:constraint_metric(congressional_action_window, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(congressional_action_window, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(congressional_action_window, snare).
narrative_ontology:human_readable(congressional_action_window, "Congressional Action Window During Debt Limit Episodes").
narrative_ontology:topic_domain(congressional_action_window, "public_finance/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(congressional_action_window).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(congressional_action_window, political_leverage_seekers).
narrative_ontology:constraint_beneficiary(congressional_action_window, short_term_treasury_traders).
narrative_ontology:constraint_victim(congressional_action_window, taxpayers).
narrative_ontology:constraint_victim(congressional_action_window, treasury_operations).
narrative_ontology:constraint_victim(congressional_action_window, federal_program_beneficiaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAXPAYER (SNARE) — Trapped in the extraction mechanism with no exit. Bears increased borrowing costs (higher interest payments on federal debt) and economic uncertainty costs (market volatility, delayed payments, program disruptions) during the action window. Cannot exit the federal tax system or influence the timing of legislative action. Maximum experienced extraction.
constraint_indexing:constraint_classification(congressional_action_window, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FEDERAL PROGRAM BENEFICIARY (SNARE) — Faces payment delays, program suspensions, and uncertainty during the action window. Constrained rather than fully trapped (some can access alternative resources or delay obligations) but still bears substantial costs with minimal agency. The constraint extracts through uncertainty and disruption rather than direct financial cost.
constraint_indexing:constraint_classification(congressional_action_window, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TREASURY DEPARTMENT (TANGLED ROPE) — Experiences both coordination function (the action window is the mechanism through which Congress exercises constitutional borrowing authority) and extraction (extraordinary measures impose operational costs, market disruption increases borrowing costs, reputational damage from repeated episodes). Constrained exit: cannot refuse to implement extraordinary measures or manage the crisis, but has some operational discretion in sequencing and communication.
constraint_indexing:constraint_classification(congressional_action_window, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: POLITICAL LEVERAGE SEEKER (ROPE) — Primary beneficiary. The action window creates a high-stakes negotiating environment that amplifies the leverage of minority factions willing to threaten default. Experiences the constraint as coordination: the window is the mechanism through which policy concessions are extracted in exchange for debt limit increases. Arbitrage-level exit: can choose whether and when to deploy the leverage, can exit the strategy if costs become too high.
constraint_indexing:constraint_classification(congressional_action_window, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SHORT-TERM TREASURY TRADER (ROPE) — Benefits from yield spread volatility during the action window. Sophisticated traders with arbitrage-level exit can profit from predictable patterns: spreads widen as X Date approaches, compress after resolution. Experiences the constraint as a coordination mechanism that creates trading opportunities. Not the primary beneficiary (extraction flows mainly to political actors) but captures secondary rents.
constraint_indexing:constraint_classification(congressional_action_window, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DEBT LIMIT REFORM COALITION (SCAFFOLD) — Organized advocacy groups, policy analysts, and some legislators see the action window as a temporary dysfunction with multiple sunset pathways: automatic suspension mechanisms, repeal of the statutory limit, or indexing to budget resolutions. Constrained exit: cannot unilaterally change the law but can build coalitions and shift norms. Sees the constraint as transitional rather than permanent.
constraint_indexing:constraint_classification(congressional_action_window, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both the legitimate coordination function (Congress exercises constitutional borrowing authority through the action window) and the extractive mechanism (the window's timing and uncertainty impose deadweight costs on taxpayers and markets). The constraint is not a natural law (other democracies manage debt authorization without recurring crises) but also not pure extraction (some legislative oversight of borrowing is constitutionally warranted). Tangled rope reflects the hybrid structure.
constraint_indexing:constraint_classification(congressional_action_window, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(congressional_action_window_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(congressional_action_window, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(congressional_action_window, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(congressional_action_window, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(congressional_action_window_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The action window imposes substantial deadweight costs on taxpayers through increased borrowing costs (yield spreads widen during episodes, and the 2011 downgrade permanently raised borrowing costs), economic uncertainty (market volatility, consumer confidence declines, business investment delays), and operational disruption (Treasury extraordinary measures, federal payment delays). The extraction is not total (some coordination function exists — Congress does exercise constitutional borrowing authority), but the costs substantially exceed what would be required for legitimate fiscal oversight. The value has increased from 0.38 in 1995 to 0.68 in 2023 as episodes have become more frequent and more extractive. Suppression (0.72): High. Victims face severe barriers to exit: taxpayers cannot exit the federal tax system, program beneficiaries cannot avoid exposure to payment delays, and Treasury cannot refuse to implement extraordinary measures. The suppression is not total (some agents have constrained exit — program beneficiaries can access alternative resources, Treasury has operational discretion), but exit costs are prohibitive for most victims. The value has increased from 0.50 in 1995 to 0.72 in 2023 as the constraint has become more entrenched and normalized. Theater ratio (0.65): Moderate-high. Much of the action window's activity is performative: public statements about fiscal responsibility that are contradicted by voting records, symbolic votes on budget resolutions that have no binding effect, deadline extensions that serve primarily as negotiating tactics, and extraordinary measures announcements that amplify crisis perception. The theater is not total (some genuine fiscal deliberation occurs, and the constitutional borrowing authority is real), but the performative content is substantial and has increased over time as the tactic has become ritualized.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a clear extraction gradient from powerless/trapped victims (taxpayers, program beneficiaries) who experience it as a snare, through institutional/constrained actors (Treasury) who experience it as tangled rope (both coordination and extraction), to institutional/arbitrage beneficiaries (political leverage seekers, traders) who experience it as rope (coordination mechanism that creates opportunities). The scaffold perspective (reform coalition) sees a sunset pathway that other perspectives do not. The analytical observer recognizes the hybrid structure: the action window has a legitimate coordination function (Congress exercises constitutional borrowing authority) but imposes deadweight costs that substantially exceed what would be required for that function. The perspectival gap is not about whether the constraint exists (all perspectives agree it does) but about whether it is primarily coordination or primarily extraction, and whether it is permanent or transitional.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Taxpayers are declared victims with trapped exit → high d → high effective extraction. They bear the full cost of increased borrowing costs and economic uncertainty with no ability to exit the federal tax system or influence the timing of legislative action. Federal program beneficiaries are declared victims with constrained exit → moderately high d → moderately high effective extraction. They face payment delays and program disruptions but have some ability to access alternative resources or delay obligations. Treasury Department is declared victim with constrained exit → moderately high d, but also experiences coordination function (the action window is the mechanism through which Congress exercises constitutional borrowing authority), producing a tangled rope classification. Political leverage seekers are declared beneficiaries with arbitrage exit → low d → low or negative effective extraction. They capture policy concessions and political advantage during the action window and can choose whether and when to deploy the leverage. Short-term Treasury traders are declared beneficiaries with arbitrage exit → low d → low or negative effective extraction. They profit from yield spread volatility and can exit positions if costs become too high. The debt limit reform coalition is organized with constrained exit → moderate d, but sees a sunset pathway, producing a scaffold classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that the action window's mandate (congressional oversight of federal borrowing) has been captured by an extraction mechanism (leverage-seeking through default threats) that imposes costs far exceeding what the mandate requires. The legitimate coordination function (Congress exercises constitutional borrowing authority) could be achieved through alternative mechanisms (automatic suspension tied to budget resolutions, repeal of the statutory limit, or indexing to appropriations) that would not create the high-stakes negotiating environment. The current structure persists not because it is necessary for the mandate but because it benefits political leverage seekers who can extract policy concessions during the action window. The mandatrophy is resolved by recognizing that the constraint is a snare from the victim's perspective (taxpayers bear costs with no exit) and a rope from the beneficiary's perspective (leverage seekers capture rents), with the analytical observer seeing the hybrid structure (tangled rope: genuine coordination function captured by extraction mechanism).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    borrowing_cost_attribution,
    'What fraction of increased borrowing costs during debt limit episodes is attributable to the action window itself versus underlying fiscal conditions?',
    'Econometric decomposition of Treasury yield spreads during episodes; comparison with non-episode periods controlling for fiscal variables; analysis of credit rating agency statements attributing downgrades to political dysfunction versus fiscal fundamentals',
    'If most cost is attributable to the window: extraction is higher than base estimate (0.68). If most cost is attributable to fiscal fundamentals: extraction is lower, and the window is more coordination than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(borrowing_cost_attribution, empirical, 'Attribution of borrowing costs to action window versus fiscal fundamentals').

omega_variable(
    extraordinary_measures_necessity,
    'Are Treasury extraordinary measures a necessary operational buffer or a theatrical performance that amplifies crisis perception?',
    'Historical analysis of episodes where extraordinary measures were versus were not deployed; assessment of whether measures genuinely extended the action window or primarily served as crisis signaling; comparison with other countries'' debt management practices',
    'If genuinely necessary: theater_ratio is lower than base estimate (0.65), and the constraint has more coordination function. If primarily theatrical: theater_ratio is higher, and the constraint is more extractive performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraordinary_measures_necessity, empirical, 'Whether extraordinary measures are operationally necessary or theatrical').

omega_variable(
    reform_pathway_viability,
    'Is debt limit reform (repeal, automatic suspension, or indexing) politically viable within a generational timeframe, or is the scaffold perspective aspirational?',
    'Analysis of legislative proposals and coalition strength; comparison with other entrenched procedural rules that were or were not reformed; assessment of whether repeated crises build reform momentum or normalize dysfunction',
    'If reform is viable: scaffold perspective is structurally accurate, and the constraint has a real sunset. If reform is not viable: scaffold perspective is aspirational, and the constraint is a stable snare rather than a transitional scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_pathway_viability, preference, 'Political viability of debt limit reform within generational timeframe').

omega_variable(
    default_probability_calibration,
    'What is the true probability of default during an action window, and how does market pricing compare to that probability?',
    'Historical frequency of near-default episodes; game-theoretic analysis of credible commitment to default threat; comparison of market-implied default probabilities (from CDS spreads) with ex-post realized outcomes',
    'If market overestimates default risk: yield spreads represent excess extraction beyond genuine risk compensation. If market underestimates default risk: the constraint is more dangerous than current pricing suggests, and suppression is higher.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(default_probability_calibration, empirical, 'Calibration of true default probability versus market pricing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(congressional_action_window, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cong_act_theater_1995, congressional_action_window, theater_ratio, 0, 0.45).
narrative_ontology:measurement(cong_act_theater_2011, congressional_action_window, theater_ratio, 16, 0.58).
narrative_ontology:measurement(cong_act_theater_2013, congressional_action_window, theater_ratio, 18, 0.62).
narrative_ontology:measurement(cong_act_theater_2023, congressional_action_window, theater_ratio, 28, 0.65).

% Extraction over time
narrative_ontology:measurement(cong_act_extract_1995, congressional_action_window, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cong_act_extract_2011, congressional_action_window, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(cong_act_extract_2013, congressional_action_window, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(cong_act_extract_2023, congressional_action_window, base_extractiveness, 28, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cong_act_suppress_1995, congressional_action_window, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cong_act_suppress_2011, congressional_action_window, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(cong_act_suppress_2013, congressional_action_window, suppression_requirement, 18, 0.7).
narrative_ontology:measurement(cong_act_suppress_2023, congressional_action_window, suppression_requirement, 28, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(congressional_action_window, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% The congressional action window is downstream of limit_arrival_timing and x_date_timing (both tangled_rope constraints that determine when the action window opens and closes). The action window is a distinct constraint with its own extractiveness reflecting the political leverage mechanism and borrowing cost increases during the window. The upstream constraints have their own extractiveness values reflecting the Treasury's operational discretion in timing limit arrival and projecting X Date.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
