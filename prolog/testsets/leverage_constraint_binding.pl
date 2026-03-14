% ============================================================================
% CONSTRAINT STORY: leverage_constraint_binding
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_leverage_constraint_binding, []).

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
 *   constraint_id: leverage_constraint_binding
 *   human_readable: Leverage Constraint Binding in Financial Systems
 *   domain: financial/economic_policy
 *
 * SUMMARY:
 *   Leverage constraint binding is a financial mechanism where debt
 *   obligations create structural constraints on borrowers' behavior, exit
 *   options, and resource allocation. The constraint operates across
 *   institutional scales — from individual mortgages and small business loans
 *   to sovereign debt and systemic financial risk. The binding force derives
 *   from three mechanisms: (1) legal enforcement (collateral seizure,
 *   foreclosure, bankruptcy), (2) credibility in enforcement (expectations of
 *   legal consequence), and (3) identity-fusion with debt-bearer role (shame,
 *   internalized responsibility). The constraint exhibits all six DR types
 *   from different perspectives. To the debt-obligated borrower with no exit,
 *   it is a Snare (extraction with suppression). To the financial
 *   intermediary with arbitrage options, it is Rope (pure coordination). To
 *   regulatory authorities deploying it as temporary systemic risk
 *   management, it is Scaffold (with sunset logic during normalization
 *   cycles). The increasing theater_ratio (0.38 → 0.68 over 15 periods)
 *   reflects how accounting formalism, debt classification rituals, and
 *   foreclosure procedures have become increasingly performative relative to
 *   their actual verification function. The rising extractiveness (0.42 →
 *   0.62) reflects how debt-obligation terms have become more onerous over
 *   the measurement interval, with tighter collateral requirements and
 *   reduced forbearance.
 *
 * KEY AGENTS:
 *   - Debt-Obligated Borrowers: Primary victims (powerless/trapped) — structural inability to exit leverage commitments; face asset seizure, credit destruction, legal consequences on default
 *   - Financial Intermediaries: Primary beneficiaries (institutional/arbitrage) — extract interest, fees, and collateral value; maintain exit optionality through securitization and hedging
 *   - Small Businesses Using Leverage: Secondary agents (moderate/constrained) — experience mixed coordination (access to growth capital) and extraction (fixed obligations regardless of performance)
 *   - Regulatory Authorities: Organized enforcers (organized/constrained) — deploy leverage constraints as temporary coordination mechanisms with crisis-response cycles
 *   - Accounting and Legal Systems: Institutional framework (institutional/mobile) — maintain performative debt formalism; create theater around asset quality and enforcement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as thermodynamic laws of capital
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(leverage_constraint_binding, 0.58).
domain_priors:suppression_score(leverage_constraint_binding, 0.72).
domain_priors:theater_ratio(leverage_constraint_binding, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(leverage_constraint_binding, extractiveness, 0.58).
narrative_ontology:constraint_metric(leverage_constraint_binding, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(leverage_constraint_binding, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(leverage_constraint_binding, snare).
narrative_ontology:human_readable(leverage_constraint_binding, "Leverage Constraint Binding in Financial Systems").
narrative_ontology:topic_domain(leverage_constraint_binding, "financial/economic_policy").

domain_priors:requires_active_enforcement(leverage_constraint_binding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(leverage_constraint_binding, financial_intermediaries).
narrative_ontology:constraint_beneficiary(leverage_constraint_binding, leverage_providers).
narrative_ontology:constraint_victim(leverage_constraint_binding, debt_obligated_agents).
narrative_ontology:constraint_victim(leverage_constraint_binding, systemic_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBT-OBLIGATED BORROWER (SNARE) — Structurally trapped by leverage commitment. Exit options are severely constrained: default carries severe penalties (asset seizure, credit destruction, legal consequences), refinancing locks in higher rates, and bankruptcy destroys social/professional identity. The borrower experiences maximum extraction with no viable exit path.
constraint_indexing:constraint_classification(leverage_constraint_binding, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL BUSINESS (TANGLED ROPE) — Constrained but not trapped. Leverage enables growth (genuine coordination function) but creates asymmetric extraction: debt service obligates cash flows regardless of business performance. Exit is possible at high cost (asset sales, operational contraction, loss of independence). Mixed experience: benefit from expansion capital + extraction through fixed obligations.
constraint_indexing:constraint_classification(leverage_constraint_binding, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL INTERMEDIARY (ROPE) — Experiences leverage constraint as pure coordination. Arbitrage options allow exit: can securitize debt, hedge, or transfer risk. Primary beneficiary. Extraction flows toward this agent (interest, fees, collateral), but the intermediary frames the constraint as solving a genuine coordination problem: matching borrowers seeking capital with lenders seeking returns. Net positive position with full agency.
constraint_indexing:constraint_classification(leverage_constraint_binding, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (SCAFFOLD) — Organized agents (central banks, prudential regulators, bankruptcy courts) deploy leverage constraints as temporary coordination mechanisms with sunset logic. Capital requirements, stress tests, and debt-to-income limits reduce systemic fragility during normalization cycles. Suppression is enforced (regulatory oversight) but temporary: during low-rate environments, enforcement relaxes; during crises, it tightens. The constraint has built-in renewal cycles.
constraint_indexing:constraint_classification(leverage_constraint_binding, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ACCOUNTING AND LEGAL FORMALISM (PITON) — Debt classification, collateral valuation, and bankruptcy law are substantially performative. Accounting standards (mark-to-market, loss provisioning) create theater around asset quality; legal foreclosure processes are ritualized and often inefficient. The formalism persists through institutional inertia despite low functional verification. Hidden leverage and shadow banking grow around the formal constraints.
constraint_indexing:constraint_classification(leverage_constraint_binding, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, leverage constraint binding is framed as an immutable law of capital allocation: debt creates obligation, obligation creates binding, and binding creates systemic risk. The constraint appears as natural law (thermodynamic-like properties of financial systems). However, this perspective risks naturalizing what is actually a contingent institutional arrangement: leverage binding depends on legal enforcement, property rights definitions, and behavioral assumptions about default.
constraint_indexing:constraint_classification(leverage_constraint_binding, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(leverage_constraint_binding_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(leverage_constraint_binding, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(leverage_constraint_binding, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(leverage_constraint_binding, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(leverage_constraint_binding, TR),
    TR >= 0.70.

:- end_tests(leverage_constraint_binding_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significantly from trapped and constrained agents through interest, fees, collateral haircuts, and opportunity costs. But extraction is not maximal (which would be closer to 0.85) because some leverage genuinely enables coordination — small businesses do grow through debt, mortgages do enable housing access, sovereign borrowing does fund productive investment. The moderate-high value reflects that extraction coexists with coordination function. Suppression (0.72): High. Multiple barriers prevent exit: legal (foreclosure/bankruptcy costs), economic (default penalties outweigh alternatives), reputational (credit destruction), and psychological (internalized debt-bearer identity). Barriers compound — an agent trapped by one becomes less able to escape the others. Theater ratio (0.55): Moderate. Debt-obligation formalism involves significant theater: loan classification systems, collateral valuation models, default probability assessments, and bankruptcy procedures are ritualized and often decoupled from actual borrower capacity or asset quality. But theater is not dominant (which would be >0.70) because enforcement has real consequences — defaults do result in asset seizure and credit consequences. The theater increases over the measurement interval as accounting standards become more complex.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap exists between the trapped borrower and the institutional beneficiary. The borrower experiences snare (pure extraction, chi ≈ 0.85-1.0). The intermediary experiences rope (pure coordination, chi ≈ 0.1-0.2). Same constraint; opposite experiences. The moderate business with constrained exit sees this as tangled rope — genuine coordination function (access to capital) coexisting with extraction (fixed obligations). The regulatory authority sees it as scaffold because they have organized exit options (can adjust capital requirements, can implement debt jubilees, can regulate interest rates) and perceive the binding as temporary and revisable. The accounting system sees itself as degraded (piton) because debt formalism has become increasingly ritualized without corresponding verification improvements.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's structural relationship to extraction flow and exit capacity. Trapped borrowers with no exit (d ≈ 0.95) experience maximum f(d) ≈ 1.42, amplifying extractiveness. Constrained small businesses with partial exit (d ≈ 0.60) experience moderate f(d) ≈ 0.95. Institutional intermediaries who benefit and have arbitrage options (d ≈ 0.10) experience negative or minimal f(d) ≈ -0.05, suppressing extraction from their perspective. The piton perspective's d is derived from theater rather than extraction asymmetry: the accounting/legal system benefits from formalism (d ≈ 0.20) and has mobile exit options (can change standards), so it classifies as piton based on theater_ratio ≥ 0.70, not based on high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   Leverage constraint binding demonstrates how the same structural mechanism can legitimately classify as six different types depending on observational context. The mandatrophy is resolved by recognizing that this is not a flaw in the classification system but a feature: the constraint genuinely is a snare (for trapped borrowers), a tangled rope (for constrained agents), a rope (for beneficiaries), a scaffold (for regulators), and a piton (for formalism). The false summit occurs when the analytical observer naturalizes this as immutable law (mountain) — claiming that debt obligation is inherent to capital markets, like gravity is inherent to physics. The structural data reveals this is contingent: enforcement depends on state capacity, credibility depends on precedent, and internalization depends on cultural narratives. All three can change. The constraint would shift dramatically in a jurisdiction where debt enforcement failed, or where debt jubilees became normalized, or where cultural attitudes toward debt obligation shifted. The mountain classification is a false summit generated by naturalizing institutional arrangements as natural laws.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_credibility_mechanism,
    'What percentage of leverage constraint binding force derives from actual enforcement capacity vs. credibility/belief in enforcement?',
    'Cross-jurisdictional comparison: enforcement outcomes in high-corruption vs low-corruption environments; historical analysis of periods where enforcement capacity collapsed (failed states, civil wars, hyperinflation)',
    'If enforcement < 40% of binding force: constraint is more cooperative/belief-based than structural. If enforcement > 70%: constraint depends on state capacity. If enforcement capacity collapses (conflict, state failure), the binding disappears rapidly despite legal form persisting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_credibility_mechanism, empirical, 'Relative contribution of enforcement vs credibility to binding force').

omega_variable(
    identity_locked_debt_spiral,
    'For borrowers in high-extraction debt relationships, is suppression primarily structural (legal/economic barriers) or internalized (identity-fused with debt-bearer role)?',
    'Post-default behavioral analysis: do agents with legal discharge (bankruptcy, jubilee) maintain debt-like behaviors? Do debt holders internalize creditor narratives about personal responsibility? Cross-cultural comparison of debt vs honor societies.',
    'If primarily structural: debt-obligated remains trapped at biological timescale. If partially internalized: identity-locked classification appropriate; breaking psychological binding may enable escape despite legal constraints remaining. If heavily internalized: continued extraction persists post-legal-discharge through shame/guilt.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_debt_spiral, empirical, 'Degree of identity fusion in debt obligation internalization').

omega_variable(
    alternative_coordination_sufficiency,
    'Can leverage-free coordination mechanisms (equity, partnerships, cooperative ownership) achieve equivalent capital allocation efficiency as debt-based leverage?',
    'Historical analysis of equity-only firms vs leveraged peers (return on capital, innovation rates, failure rates); comparative institutional analysis (German Mittelstand, Japanese zaibatsu vs US leveraged firms); economic simulations of alternative capital allocation mechanisms',
    'If alternatives are sufficient: leverage constraint is not natural law but institutional choice, potentially susceptible to sunset. If alternatives underperform: leverage constraint may reflect genuine coordination necessity (Rope or Tangled Rope rather than Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_coordination_sufficiency, empirical, 'Whether non-leveraged coordination mechanisms can substitute for debt-based capital allocation').

omega_variable(
    systemic_cascade_mechanism,
    'Does leverage constraint binding create genuinely coordinated systemic stability or does it merely concentrate fragility at critical nodes?',
    'Network analysis of debt dependency structures; comparison of cascade patterns in leverage-heavy vs leverage-light financial systems; stress-test scenarios showing whether constraints propagate contagion',
    'If coordinates stability: Tangled Rope from systemic perspective. If concentrates fragility: Snare from systemic perspective. Determines whether regulatory scaffold perspective is accurate or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systemic_cascade_mechanism, empirical, 'Whether leverage constraints promote or concentrate systemic fragility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(leverage_constraint_binding, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lcb_tr_t0, leverage_constraint_binding, theater_ratio, 0, 0.38).
narrative_ontology:measurement(lcb_tr_t5, leverage_constraint_binding, theater_ratio, 5, 0.48).
narrative_ontology:measurement(lcb_tr_t10, leverage_constraint_binding, theater_ratio, 10, 0.55).
narrative_ontology:measurement(lcb_tr_t15, leverage_constraint_binding, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(lcb_be_t0, leverage_constraint_binding, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lcb_be_t5, leverage_constraint_binding, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(lcb_be_t10, leverage_constraint_binding, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(lcb_be_t15, leverage_constraint_binding, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(leverage_constraint_binding, resource_allocation).
narrative_ontology:affects_constraint(leverage_constraint_binding, collateral_valuation_opacity).
narrative_ontology:affects_constraint(leverage_constraint_binding, bankruptcy_procedure_formalism).
narrative_ontology:affects_constraint(leverage_constraint_binding, interest_rate_asymmetry_information).

% DUAL FORMULATION NOTE:
% Leverage constraint binding decomposes into three structurally distinct constraints: (1) collateral_valuation_opacity (ε ≈ 0.35, Tangled Rope) — agents cannot verify actual collateral quality, enabling extraction through information asymmetry; (2) bankruptcy_procedure_formalism (ε ≈ 0.48, Piton) — foreclosure rituals persist with declining functional verification; (3) interest_rate_asymmetry_information (ε ≈ 0.42, Tangled Rope) — borrowers cannot assess true cost of credit relative to creditor's cost of capital. Each has different beneficiaries, victims, and temporal dynamics. The parent story (leverage_constraint_binding) represents the integrated mechanism; the child stories represent empirically decomposable components with distinct ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(leverage_constraint_binding, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
