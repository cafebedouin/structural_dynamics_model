% ============================================================================
% CONSTRAINT STORY: scenario_planning_atrophy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scenario_planning_atrophy, []).

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
 *   constraint_id: scenario_planning_atrophy
 *   human_readable: Scenario Planning Atrophy in Strategic Decision-Making
 *   domain: strategic_foresight/organizational_governance
 *
 * SUMMARY:
 *   Scenario planning atrophy describes the organizational elimination or
 *   severe degradation of formal long-term scenario development capacity in
 *   favor of shorter-term strategic frameworks and quarterly earnings focus.
 *   This constraint operates across sectors (technology, finance,
 *   manufacturing, energy) and reflects a structural tension between capital
 *   market time preferences (quarterly cycles) and organizational survival
 *   time horizons (generational). The atrophy exhibits characteristics of
 *   both coordination (faster near-term decisions) and extraction (loss of
 *   adaptive capacity). The theater ratio (0.68) reflects that strategic
 *   planning functions persist in formal processes (board reports, annual
 *   planning) while operational resource allocation increasingly ignores
 *   scenario-based insights. The extractiveness trajectory (0.28 → 0.58 over
 *   the interval) shows accumulation of undetected strategic risk and
 *   institutional knowledge loss. This is a diagnostic case for how
 *   contingent institutional arrangements (quarterly earnings pressure) can
 *   be naturalized as immutable constraints and how organizational functions
 *   degrade through atrophy while maintaining theatrical legitimacy.
 *
 * KEY AGENTS:
 *   - Quarterly Earnings Stakeholders: Primary beneficiary (institutional/arbitrage) — capture near-term capital efficiency and decision velocity gains from eliminating foresight overhead
 *   - Long-Horizon Institutional Survival: Primary victim (powerless/trapped) — abstract collective good bearing cost of undetected strategic surprise; cannot exit or organize
 *   - Frontline Strategist: Secondary victim (powerless/trapped) — mid-career professional with foresight expertise locked into atrophied function; career risk prevents exit
 *   - Risk Management Function: Secondary victim (moderate/constrained) — bears responsibility for tail risks without foresight capacity; benefits from blame deferral when scenarios weren't planned
 *   - Corporate Strategy Department: Institutional actor (institutional/constrained) — maintains theatrical planning processes (board compliance) while losing real strategic influence
 *   - Resilience Community: Organized external actors (organized/mobile) — consultants and frameworks see gap as temporary opportunity with natural closure as crises mount
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing quarterly capital markets as immutable constraints
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scenario_planning_atrophy, 0.58).
domain_priors:suppression_score(scenario_planning_atrophy, 0.62).
domain_priors:theater_ratio(scenario_planning_atrophy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scenario_planning_atrophy, extractiveness, 0.58).
narrative_ontology:constraint_metric(scenario_planning_atrophy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(scenario_planning_atrophy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scenario_planning_atrophy, tangled_rope).
narrative_ontology:human_readable(scenario_planning_atrophy, "Scenario Planning Atrophy in Strategic Decision-Making").
narrative_ontology:topic_domain(scenario_planning_atrophy, "strategic_foresight/organizational_governance").

domain_priors:requires_active_enforcement(scenario_planning_atrophy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scenario_planning_atrophy, quarterly_earnings_stakeholders).
narrative_ontology:constraint_beneficiary(scenario_planning_atrophy, incumbent_power_structures).
narrative_ontology:constraint_victim(scenario_planning_atrophy, long_horizon_institutional_survival).
narrative_ontology:constraint_victim(scenario_planning_atrophy, organizational_adaptive_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE STRATEGIST (SNARE) — Mid-career strategic planner locked into an organization that has eliminated scenario planning capacity. Cannot exit without career sacrifice. Watches decision-makers ignore warning signals and alternative futures while powerless to intervene. Maximum experienced extraction — the constraint traps the agent's expertise in an atrophied function.
constraint_indexing:constraint_classification(scenario_planning_atrophy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: QUARTERLY EARNINGS MAXIMIZER (ROPE) — Finance leadership experiences scenario planning elimination as pure coordination benefit: freed capital, reduced analytical overhead, accelerated decision cycles. No suppression experienced. Net beneficiary with full arbitrage capacity — can reallocate planning resources to revenue optimization.
constraint_indexing:constraint_classification(scenario_planning_atrophy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: RISK MANAGEMENT FUNCTION (TANGLED ROPE) — Experiences the constraint as hybrid: genuine coordination benefit (faster crisis response once threats materialize) mixed with severe extraction (bears responsibility for undetected tail risks without foresight capacity). Constrained by regulatory requirements and liability exposure but also benefits from blame deferral when scenarios weren't planned.
constraint_indexing:constraint_classification(scenario_planning_atrophy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: RESILIENCE COMMUNITY (SCAFFOLD) — External organizations (scenario planning consultants, resilience frameworks, adaptive governance networks) see the organizational gap as temporary opportunity with natural sunset. As crises accumulate and industry examples mount, scenario planning capacity rebuilds. The constraint has high early-stage extraction but declining over time as external pressure drives norm change.
constraint_indexing:constraint_classification(scenario_planning_atrophy, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CORPORATE STRATEGY DEPARTMENT (PITON) — The strategy function has become substantially theatrical. Strategic plans are produced (satisfying governance requirements) but rarely inform actual resource allocation. Scenario planning rituals persist in board reports and annual planning cycles while operational decisions ignore them. The function maintains institutional legitimacy through performance of strategy, not strategy execution.
constraint_indexing:constraint_classification(scenario_planning_atrophy, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, the atrophy appears as an immutable consequence of capitalist time preference: quarterly earnings cycles are incompatible with multi-decade scenario planning. The constraint appears as a natural law of market economy structure. However, this naturalizes what is actually a contingent institutional choice — many organizations maintain foresight capacity despite earnings pressure. The false summit detector will flag this perspective.
constraint_indexing:constraint_classification(scenario_planning_atrophy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scenario_planning_atrophy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(scenario_planning_atrophy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scenario_planning_atrophy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(scenario_planning_atrophy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(scenario_planning_atrophy, TR),
    TR >= 0.70.

:- end_tests(scenario_planning_atrophy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and accumulating. The constraint initially extracts through organizational efficiency gains (freed capital, faster decisions) but increasingly extracts through undetected strategic risk. As foresight capacity atrophies, the organization becomes vulnerable to tail events (market disruption, regulatory change, geopolitical shock) with no early warning mechanisms. The trajectory shows classic rent-seeking: initial extraction appears as productivity gain, but compounds as institutional knowledge is lost and adaptive capacity degrades. Suppression (0.62): Moderate-high. Multiple barriers prevent scenario planning recovery: (1) budget reallocated and difficult to restore, (2) analytical talent has departed or been reassigned, (3) organizational culture has shifted to short-term focus, (4) finance leadership sees long-term planning as cost center with no measurable ROI. But suppression is not total — external consultants can rebuild capacity and some organizations maintain foresight despite earnings pressure. Theater ratio (0.68): High and increasing. Strategic planning processes persist: annual scenario development, board strategy reports, long-term planning documents. But these increasingly decouple from actual resource allocation. M&A decisions, product investments, and capital allocation increasingly ignore scenario inputs. The planning function becomes ritualistic (demonstrating strategic consciousness to governance bodies) while losing operational influence.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence across the six types. Quarterly earnings stakeholders see pure coordination benefit (Rope) — faster decisions, lower overhead, capital efficiency. The strategic foresight community sees temporary atrophy with natural recovery mechanisms (Scaffold) — each strategic surprise (COVID, supply chains, climate, geopolitics) drives organizations back to scenario planning. The strategy department sees its own theatrical degradation (Piton) — producing planning documents that boards require while lacking operational decision influence. Risk management sees mixed coordination and extraction (Tangled Rope) — the organization moves faster but risks accumulate without early detection. Strategists locked into atrophied functions see pure extraction (Snare) — their expertise is trapped in a function that has lost organizational relevance. The civilizational analyst risks naturalizing quarterly earnings cycles as law of nature (Mountain) — but this ignores examples of organizations that maintain foresight despite earnings pressure, revealing the constraint as contingent institutional choice.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: The constraint resolves mandatrophy by revealing that atrophy itself is the extraction mechanism. The beneficiaries (quarterly earnings optimizers) do not perceive they are locked into a constraint that blinds them to long-term risks. The frontline victims (strategists, institutional survival) perceive the constraint clearly but cannot exit. The organizational system exhibits no mechanism for detecting or correcting the accumulating blindness — scenario planning was the feedback mechanism, and it has been eliminated. The mandatrophy is thus resolved: this is NOT the misclassification of coordination as extraction, but the real degradation of a coordination function into a disabled state while maintaining theater. The piton classification is crucial: strategy remains performative (board-level legitimacy), but the function that detects strategic surprise has atrophied. The atrophy is the constraint, not a side effect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrophy_vs_rational_choice,
    'Is scenario planning atrophy the result of rational cost-benefit calculation (scenario planning has low ROI) or institutional drift (planning capacity was eliminated during crisis without deliberate reassessment)?',
    'Organizational archival analysis: compare organizations that explicitly decided to reduce scenario planning vs. those where capacity degraded through budget cuts. Measure subsequent strategic surprise rates.',
    'If rational choice: atrophy is efficient (lower classification). If drift: atrophy is extractive institutional inertia (higher classification). Classification could shift from Tangled Rope to Rope (beneficial) or remain stable depending on underlying mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_vs_rational_choice, empirical, 'Atrophy mechanism: rational choice vs institutional drift').

omega_variable(
    external_foresight_substitution,
    'Are organizations replacing internal scenario planning with external foresight purchasing (consulting, trend reports, AI scenario generators), or has foresight function simply vanished?',
    'Survey of organizational spending on scenario development, trend analysis, strategic consulting. Comparison of organizations with/without external foresight substitution and their strategic surprise rates.',
    'If substitution exists: effective foresight capacity persists (lower extractiveness). If function vanished: institutional knowledge loss (higher extractiveness, Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_foresight_substitution, empirical, 'External foresight substitution for internal planning capacity').

omega_variable(
    crisis_driven_recovery_timeline,
    'What is the organizational recovery timeline for scenario planning capacity after major strategic surprise? Can atrophied functions be reactivated or does institutional knowledge loss prevent recovery?',
    'Longitudinal case study of organizations that experienced major strategic surprise (market disruption, regulatory change, geopolitical shift) and tracked whether scenario planning was reestablished and how quickly.',
    'If recovery < 2 years: scaffold thesis is strong (temporary atrophy). If recovery > 5 years or doesn''t occur: piton classification is correct (function remains theatrical after shock). If recovery never occurs: snare classification (constraint is structural, not temporary).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(crisis_driven_recovery_timeline, empirical, 'Recovery timeline for scenario planning capacity after strategic surprise').

omega_variable(
    identity_lock_in_finance_leadership,
    'Do finance leaders experience quarterly earnings optimization as a constraint from which they could exit, or as constitutive of their professional identity and decision-making framework?',
    'Qualitative interviews with CFOs and finance leaders: can they articulate a scenario where long-term foresight would override quarterly targets? Do they perceive tension or alignment between these time horizons?',
    'If identity-locked: the beneficiary perspective (earnings maximizers) is locked into the constraint and cannot be a coalition partner for reform. If merely constrained: financial leadership could shift if incentives change. This affects whether the constraint is truly hybrid or is a snare with captured beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_finance_leadership, empirical, 'Professional identity lock-in for quarterly earnings optimization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scenario_planning_atrophy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scen_tr_t0, scenario_planning_atrophy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(scen_tr_t5, scenario_planning_atrophy, theater_ratio, 5, 0.52).
narrative_ontology:measurement(scen_tr_t10, scenario_planning_atrophy, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(scen_be_t0, scenario_planning_atrophy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(scen_be_t5, scenario_planning_atrophy, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(scen_be_t10, scenario_planning_atrophy, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scenario_planning_atrophy, information_standard).
narrative_ontology:affects_constraint(scenario_planning_atrophy, quarterly_earnings_primacy).
narrative_ontology:affects_constraint(scenario_planning_atrophy, organizational_myopia_in_capital_allocation).

% DUAL FORMULATION NOTE:
% Scenario planning atrophy is downstream of quarterly earnings pressure but represents a distinct structural constraint on organizational foresight capacity. The upstream constraint (quarterly earnings primacy) has its own extractiveness reflecting the capital market structure; the atrophy constraint has its own extractiveness reflecting the knowledge loss and strategic surprise vulnerability that results from eliminating foresight mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(scenario_planning_atrophy, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
