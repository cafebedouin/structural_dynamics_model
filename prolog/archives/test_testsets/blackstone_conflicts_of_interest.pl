% ============================================================================
% CONSTRAINT STORY: blackstone_conflicts_of_interest
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_blackstone_conflicts_of_interest, []).

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
 *   constraint_id: blackstone_conflicts_of_interest
 *   human_readable: Fiduciary Conflict Allocation in Blackstone S-1
 *   domain: corporate_governance
 *
 * SUMMARY:
 *   In its 2007 S-1 filing for its Initial Public Offering, The Blackstone
 *   Group established a corporate governance structure that explicitly and
 *   legally subordinates the interests of public common unitholders to those
 *   of its private investment funds and their Limited Partners (LPs). The
 *   partnership agreement states that any 'corporate opportunity' can be
 *   allocated to the private funds without consideration for the public
 *   entity. This creates a permanent, legally-enforced conflict of interest,
 *   making it a powerful example of a designed constraint with vastly
 *   different implications for different classes of stakeholders.
 *
 * KEY AGENTS:
 *   - Blackstone General Partners: Primary beneficiary (institutional/arbitrage) — Designs and enforces the structure to maximize the profitability of their core private fund business.
 *   - Fund Limited Partners: Secondary beneficiary (powerful/mobile) — Benefit from a legal guarantee that their capital will receive preferential treatment and access to the best investment opportunities.
 *   - Public Common Unitholders: Primary victim (powerless/trapped) — Invest capital into the public entity but have their fiduciary protections legally waived in favor of another group.
 *   - Securities and Exchange Commission (SEC): Analytical/Institutional actor — Approved the structure, viewing it through a lens of disclosure-based regulation rather than prescriptive governance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(blackstone_conflicts_of_interest, 0.75).
domain_priors:suppression_score(blackstone_conflicts_of_interest, 0.8).
domain_priors:theater_ratio(blackstone_conflicts_of_interest, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(blackstone_conflicts_of_interest, extractiveness, 0.75).
narrative_ontology:constraint_metric(blackstone_conflicts_of_interest, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(blackstone_conflicts_of_interest, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(blackstone_conflicts_of_interest, tangled_rope).
narrative_ontology:human_readable(blackstone_conflicts_of_interest, "Fiduciary Conflict Allocation in Blackstone S-1").
narrative_ontology:topic_domain(blackstone_conflicts_of_interest, "corporate_governance").

domain_priors:requires_active_enforcement(blackstone_conflicts_of_interest).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(blackstone_conflicts_of_interest, blackstone_general_partners).
narrative_ontology:constraint_beneficiary(blackstone_conflicts_of_interest, fund_limited_partners).
narrative_ontology:constraint_victim(blackstone_conflicts_of_interest, public_common_unitholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC UNITHOLDER (SNARE) — From the perspective of a public investor, this is a pure extraction mechanism. Their interests are legally and structurally subordinated with no recourse or governance mechanism to change the terms. While they can sell the stock (mobile exit), they are trapped within the terms of the agreement as long as they hold it. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.28. This extreme effective extraction firmly classifies it as a Snare.
constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GENERAL PARTNER (ROPE) — The firm's management designed this structure as a pure coordination tool. It allows them to raise public capital while legally ring-fencing their primary business: serving the Limited Partners in their funds. For them, the constraint solves the problem of conflicting duties by simply eliminating one. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.08. The negative extraction indicates a net subsidy.
constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: FUND LIMITED PARTNER (ROPE) — For the large institutional investors (LPs) in Blackstone's funds, this structure is a feature, not a bug. It provides a contractual guarantee that their interests will be prioritized over a diffuse group of public shareholders, reducing their risk. It is a pure coordination benefit. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.01.
constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — The analyst sees the complete structure: a highly effective coordination mechanism for the GP/LP relationship (the Rope component) that is funded by a legally codified, high-extraction arrangement imposed on public unitholders (the Snare component). The coexistence of both functions makes it a textbook Tangled Rope. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈1.04. While χ > 0.90, the clear coordination function prevents a Snare classification from this perspective.
constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(blackstone_conflicts_of_interest_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(blackstone_conflicts_of_interest, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(blackstone_conflicts_of_interest, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(blackstone_conflicts_of_interest_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.75) is very high because the structure's primary purpose is to allow value (in the form of investment opportunities) to be diverted from the public entity to private funds. Suppression (0.80) is also very high; public unitholders have no governance rights to challenge this core tenet of the partnership agreement. The only exit is to sell the stock. The Theater Ratio (0.20) is low because the S-1 filing is brutally honest and transparent about this subordination. It is not a hidden clause; it is a core feature of the investment, making it functionally extractive rather than performatively so.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the General Partners and Limited Partners, the constraint is a Rope—a brilliant piece of financial engineering that solves the coordination problem of managing private funds with public capital. For the Public Unitholder, it is an inescapable Snare—they are a source of permanent capital whose returns are systematically and legally subordinated. The Analytical Observer, recognizing both the valid coordination function and the severe asymmetric extraction, classifies it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived directly from the declared beneficiaries and victims. The General Partners (beneficiary, arbitrage exit) and LPs (beneficiary, mobile exit) have low 'd' values, resulting in negative effective extraction (χ < 0), hence their Rope perspective. The Public Unitholders (victim, trapped exit) have a very high 'd' value, leading to a massive effective extraction (χ > 1.0), defining their Snare perspective. The analytical view uses a canonical 'd' that sits between these extremes, revealing the mixed Tangled Rope nature.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a powerful resolution for mandatrophy. A naive analysis might label the structure a 'Snare' based on its harm to public investors, but this would miss its primary and essential function as a 'Rope' for the GP/LP relationship. Conversely, calling it a 'Rope' based on its function for insiders would ignore the severe, codified extraction from outsiders. The Tangled Rope classification from the analytical perspective is the only one that captures the full truth: it is a constraint that is simultaneously a coordination solution and an extraction mechanism, with the type depending entirely on one's structural position relative to the cash flows.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_efficiency_pricing,
    'Did the public market correctly price the risk of subordinated fiduciary duty at the time of the IPO and subsequently?',
    'Event study analysis of the IPO pricing and subsequent performance relative to comparable firms with traditional governance structures. Analysis of institutional ownership and analyst reports to gauge awareness of the clause.',
    'If the risk was fully priced in, the extractiveness (ε) could be argued to be lower, as investors were compensated for the risk. If not, the ε=0.75 value is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_efficiency_pricing, empirical, 'Whether the market correctly priced the subordinated fiduciary duty risk.').

omega_variable(
    long_term_alignment,
    'Could this structure, by ensuring the health of the core private equity business, ultimately benefit public unitholders more than a traditional fiduciary duty would have?',
    'Longitudinal performance comparison against benchmarks and peers over a multi-decade period, controlling for market cycles.',
    'If long-term performance is superior, it suggests the structure is a novel but effective form of governance (lower ε, more Rope-like). If performance lags, it confirms the extractive Snare nature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_alignment, conceptual, 'Whether the structure creates superior long-term alignment despite violating traditional duties.').

omega_variable(
    regulatory_tolerance,
    'What legal and regulatory theories enabled the SEC to approve a public offering that explicitly negates the fiduciary duty of care to public shareholders?',
    'Legal analysis of the ''corporate opportunity'' doctrines, partnership law vs. corporate law, and the regulatory climate of the mid-2000s.',
    'Understanding the regulatory rationale would clarify whether this is a feature of the legal system (a ''Mountain'' of law) or a temporary loophole (a ''Scaffold'' of deregulation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_tolerance, conceptual, 'The legal and regulatory basis for allowing the negation of fiduciary duty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(blackstone_conflicts_of_interest, 2007, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blac_tr_t2007, blackstone_conflicts_of_interest, theater_ratio, 2007, 0.2).
narrative_ontology:measurement(blac_tr_t2015, blackstone_conflicts_of_interest, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(blac_tr_t2024, blackstone_conflicts_of_interest, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(blac_be_t2007, blackstone_conflicts_of_interest, base_extractiveness, 2007, 0.75).
narrative_ontology:measurement(blac_be_t2015, blackstone_conflicts_of_interest, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(blac_be_t2024, blackstone_conflicts_of_interest, base_extractiveness, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(blackstone_conflicts_of_interest, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
