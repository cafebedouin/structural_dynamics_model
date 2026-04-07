% ============================================================================
% CONSTRAINT STORY: currency_monopoly_protection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_currency_monopoly_protection, []).

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
 *   constraint_id: currency_monopoly_protection
 *   human_readable: Currency Monopoly Protection and Monetary Control Extraction
 *   domain: political_economy/monetary_systems
 *
 * SUMMARY:
 *   Currency monopoly protection — the legal prohibition on competing
 *   monetary systems and criminal penalties for unauthorized currency
 *   issuance — represents a foundational extraction mechanism in modern
 *   political economy. Central banks and nation-states claim that this
 *   monopoly is necessary for macroeconomic stability, price predictability,
 *   and monetary policy transmission. However, the structural evidence
 *   suggests a mixed constraint with significant extractive components: the
 *   constraint simultaneously provides genuine coordination benefits (a
 *   unified medium of exchange) and enables extraction (seigniorage capture,
 *   monetary policy imposition without consent, enforced financial inclusion
 *   at state-controlled prices). The constraint's classification depends
 *   critically on the observer's structural position. Powerless innovators
 *   and unbanked populations see a pure extraction system with no exit
 *   (Snare). The central banking authority sees pure coordination (Rope).
 *   Wealthy actors experience mixed costs and benefits (Tangled Rope).
 *   Organized cryptocurrency movements see a temporary problem being solved
 *   (Scaffold). The civilizational analytical observer risks naturalizing the
 *   constraint as an immutable law of economics, but active enforcement
 *   requirements and definitional victims reveal this as a false summit. The
 *   constraint's extractiveness has increased over the measurement interval
 *   (0.42 → 0.58) as central banks have deployed negative interest rates,
 *   cash restrictions, and financial inclusion mandates that extend the reach
 *   of state monetary control. Theater ratio has remained low (0.28 → 0.38)
 *   because the enforcement is substantive legal prohibition, not
 *   performative ritual.
 *
 * KEY AGENTS:
 *   - Central Banking Authority: Primary beneficiary (institutional/arbitrage) — captures seigniorage, maintains monetary control, extracts through policy externalities
 *   - Alternative Currency Innovators: Primary victim (powerless/trapped) — face criminal penalties for competing currency systems, cannot legally exit the monopoly
 *   - Informal Economy Participants: Secondary victim (moderate/constrained) — barred from efficient non-state exchange media, forced into banking system at high transaction costs
 *   - Multinational Firms and Wealthy Individuals: Mixed actor (powerful/mobile) — benefit from currency coordination but experience extraction through capital controls and policy spillovers
 *   - Cryptocurrency Movements: Organized agent (organized/arbitrage) — building alternative pathways with explicit sunset logic relative to fiat monopoly
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(currency_monopoly_protection, 0.58).
domain_priors:suppression_score(currency_monopoly_protection, 0.72).
domain_priors:theater_ratio(currency_monopoly_protection, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(currency_monopoly_protection, extractiveness, 0.58).
narrative_ontology:constraint_metric(currency_monopoly_protection, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(currency_monopoly_protection, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(currency_monopoly_protection, snare).
narrative_ontology:human_readable(currency_monopoly_protection, "Currency Monopoly Protection and Monetary Control Extraction").
narrative_ontology:topic_domain(currency_monopoly_protection, "political_economy/monetary_systems").

domain_priors:requires_active_enforcement(currency_monopoly_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(currency_monopoly_protection, central_banking_authority).
narrative_ontology:constraint_victim(currency_monopoly_protection, alternative_currency_innovators).
narrative_ontology:constraint_victim(currency_monopoly_protection, informal_economy_participants).
narrative_ontology:constraint_victim(currency_monopoly_protection, monetary_sovereignty_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNBANKED AND ALTERNATIVE CURRENCY INNOVATORS (SNARE) — Face legal prohibition on parallel monetary systems, severe criminal penalties for currency counterfeiting or unauthorized issuance, and structural dependence on the monopoly currency for all transactions. Cannot exit the fiat money system without abandoning economic participation entirely. Maximum extraction: forced monetization through a single channel with no alternatives.
constraint_indexing:constraint_classification(currency_monopoly_protection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INFORMAL ECONOMY PARTICIPANTS (SNARE) — Barred from efficient small-denomination alternatives and from organizing non-state monetary exchange at scale. Constrained by heavy enforcement against barter systems, informal lending circles that could substitute for banking, or alternative exchange media. High extraction through seigniorage and enforced banking fees. Limited but real costs to attempting alternatives (regulatory harassment, prosecution for tax evasion).
constraint_indexing:constraint_classification(currency_monopoly_protection, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL BANKING AUTHORITY (ROPE) — Experiences the monopoly as pure coordination: issuing currency solves the collective action problem of medium-of-exchange provision and enables macroeconomic policy. The authority derives seigniorage (the spread between cost of currency production and face value) and maintains monetary control as necessary public goods. No extraction perceived — constraint appears as functional necessity.
constraint_indexing:constraint_classification(currency_monopoly_protection, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MULTINATIONAL FIRMS AND WEALTHY INDIVIDUALS (TANGLED ROPE) — Experience the constraint as both coordination (currency stability, price transparency) and extraction (capital controls, reporting requirements, monetary policy spillovers from central bank decisions). Can exit partially through forex trading, hedging, offshore accounts, and jurisdictional arbitrage, but still bear some costs from monetary policy they did not vote for. Mixed experience: genuine benefit from coordination plus unavoidable extraction.
constraint_indexing:constraint_classification(currency_monopoly_protection, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CRYPTOCURRENCY AND DECENTRALIZED FINANCE MOVEMENTS (SCAFFOLD) — Organized agents (blockchain developers, exchange platforms, decentralized protocols) are building alternative monetary systems with explicit sunset clauses relative to fiat monopoly: Bitcoin, Ethereum, and stablecoins are creating parallel transaction media that reduce dependence on state-issued currency. The constraint is temporary because technological and network effects enable opt-out without legal permission. Theater ratio remains low in this domain because the alternative is genuinely functional, not just performative.
constraint_indexing:constraint_classification(currency_monopoly_protection, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, currency monopoly may appear as an immutable law of macroeconomic organization: large-scale coordination requires a single trusted medium of exchange; fractional currency systems collapse under coordination failures (as illustrated by historical alternative currency collapses). The analytical observer risks naturalizing this as an inherent structural limit. However, the base properties reveal this as a false summit: the constraint relies on active legal enforcement (requires_active_enforcement: true), victims are definitively identified, and suppression is high (0.72) — none of these properties characterize natural law.
constraint_indexing:constraint_classification(currency_monopoly_protection, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(currency_monopoly_protection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(currency_monopoly_protection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(currency_monopoly_protection, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(currency_monopoly_protection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(currency_monopoly_protection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts seigniorage (the profit on currency production), enforces mandatory financial system participation, and enables monetary policy that transfers wealth (inflation, negative real rates) from savers to borrowers and the state. However, extractiveness is not maximal because the coordination function is partially genuine — unified currency does solve medium-of-exchange problems. The measurement trajectory (0.42 → 0.58) reflects that enforcement has intensified as central banks deploy unconventional tools (negative rates, digital currencies) that extend control deeper into economic life. Suppression (0.72): High. The constraint relies on criminal penalties for competing currency systems, regulatory prohibition of parallel exchange media, restrictions on cash use, and financial deplatforming of alternative currency platforms. The suppression is both legal (explicit prohibition) and infrastructural (control of payment systems). Theater ratio (0.38): Low. The constraint relies on substantive enforcement (criminal law, regulatory prohibition) rather than performative ritual. The low theater indicates a genuinely coercive mechanism, not one maintained by symbolic compliance. This differentiates currency monopoly from degraded institutions (Piton) — the enforcement is real and functional.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival variation. The powerless innovator perceives pure extraction with zero agency (Snare). The central bank perceives pure coordination with zero extraction (Rope). Multinational firms perceive mixed experiences (Tangled Rope). Cryptocurrency movements perceive a solvable temporary problem with a sunset horizon (Scaffold). The civilizational observer risks the false summit of naturalizing the arrangement as an economic law. The gap between the central bank's 'pure coordination' and the innovator's 'pure extraction' reveals that the constraint's classification is not determined by its intrinsic properties but by the agent's structural relationship to it. The beneficiary experiences coordination; the victim experiences extraction. Both are measuring the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Central banking authority as beneficiary + institutional power + arbitrage options → low d (around 0.05) → negative f(d) → benefits from constraint, does not bear extraction. Alternative currency innovators as victims + powerless + trapped → high d (around 0.95) → high f(d) around 1.42 → maximum extraction, no agency. Informal economy participants as victims + moderate + constrained → moderate-high d (around 0.70) → f(d) around 1.00 → significant extraction but some escape routes exist (informal credit, barter at small scales). Multinational firms as both beneficiary and victim + powerful + mobile → middle d (around 0.50) → f(d) around 0.65 → mixed extraction and benefit. The derivation chain correctly identifies why the snare classification dominates: the victims have no exit and no agency, and the suppression is high enough to prevent coordination among victims. A snare requires this structural gap between beneficiary with arbitrage and victim with entrapment.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandatrophy is resolved by recognizing that 'currency monopoly' conflates two structurally distinct claims: (1) the functional claim that macroeconomic coordination requires a single medium of exchange, and (2) the institutional claim that this coordination must be enforced by state legal prohibition against alternatives. The functional claim, if true, would justify Rope or Tangled Rope classification. The institutional claim as implemented is extractive and classificatory as Snare. The historical resolution mechanism is technological: as cryptocurrency and decentralized finance mature, the state's legal monopoly is rendered obsolete by a technological monopoly on better coordination. The constraint will remain Snare for as long as the state can suppress alternatives; it will transition to Scaffold or Rope if alternatives become functionally superior and legally permitted; it will become Piton if the state continues to enforce a monopoly that has already been superseded. The current classification (Snare) is stable because enforcement remains substantive and victims remain locked in.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_vs_extraction_pretext,
    'Is currency monopoly protection genuinely necessary for macroeconomic stability, or is the ''necessity'' rhetoric a pretext for extraction and monetary control?',
    'Comparative historical analysis of coordinated monetary systems (currency unions, mutual-credit systems) that achieved macroeconomic stability without monopoly enforcement. Empirical testing of whether crypto volatility is a function of decentralization itself or of immature adoption and speculative dynamics.',
    'If necessity is genuine: reclassify as Rope (pure coordination with inherent enforcement costs). If pretext: maintain Snare (extraction disguised as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_vs_extraction_pretext, empirical, 'Whether monopoly protection serves coordination or extraction').

omega_variable(
    seigniorage_distribution_legitimacy,
    'How much of the seigniorage extracted through currency monopoly is spent on genuine public goods (price stability, transaction infrastructure) versus capturing for state spending and central banker compensation?',
    'Detailed accounting of seigniorage flows by central banks; measurement of actual inflation impacts across income groups; comparison of seigniorage revenue versus public good provision metrics (financial system stability, transaction cost reduction, poverty reduction).',
    'If public goods exceed 70% of seigniorage: evidence the constraint has coordination-dominant function (Tangled Rope or Rope). If less than 30%: evidence of pure extraction (Snare confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(seigniorage_distribution_legitimacy, empirical, 'Legitimacy of seigniorage appropriation as public good').

omega_variable(
    alternative_currency_functional_equivalence,
    'Do cryptocurrency and alternative payment systems provide functionally equivalent monetary services (store of value, unit of account, medium of exchange) at competitive costs relative to fiat systems?',
    'Comparative analysis of transaction costs, settlement times, volatility, acceptance breadth, and network effects across fiat and alternative systems. Longitudinal tracking of adoption rates as technology matures.',
    'If functionally equivalent: scaffold perspective confirmed — exit path is real and will mature within 20-30 years, making the monopoly temporary. If alternatives persistently fail: monopoly protection may reflect genuine technical necessity rather than extraction preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_currency_functional_equivalence, empirical, 'Functional equivalence of alternative currencies').

omega_variable(
    enforcement_cost_vs_extraction_rent,
    'What proportion of the measured suppression (0.72) is necessary enforcement of a single medium of exchange versus regulatory rent-seeking (unnecessary restrictions on banking, remittance pricing, cash restrictions)?',
    'Decomposition of enforcement mechanisms by category: essential (counterfeiting prevention), efficiency (banking regulation), and rent-seeking (negative interest rates on deposits, cash withdrawal limits, bank account requirements for government services). Cross-national comparison of enforcement costs for similar-sized economies.',
    'If enforcement cost is 0.15-0.25: most suppression is rent-seeking (Snare confirmed). If 0.50+: much suppression is legitimately necessary (moves toward Tangled Rope or Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_cost_vs_extraction_rent, empirical, 'Decomposition of enforcement cost versus regulatory rent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(currency_monopoly_protection, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(currmon_tr_t0, currency_monopoly_protection, theater_ratio, 0, 0.28).
narrative_ontology:measurement(currmon_tr_t10, currency_monopoly_protection, theater_ratio, 10, 0.32).
narrative_ontology:measurement(currmon_tr_t20, currency_monopoly_protection, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(currmon_be_t0, currency_monopoly_protection, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(currmon_be_t10, currency_monopoly_protection, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(currmon_be_t20, currency_monopoly_protection, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(currency_monopoly_protection, information_standard).
narrative_ontology:affects_constraint(currency_monopoly_protection, seigniorage_extraction).
narrative_ontology:affects_constraint(currency_monopoly_protection, financial_repression).
narrative_ontology:affects_constraint(currency_monopoly_protection, monetary_policy_externalities).

% DUAL FORMULATION NOTE:
% Currency monopoly protection decomposes into three structurally linked constraints: (1) seigniorage extraction (ε ~0.15, Rope-Tangled Rope boundary), (2) financial repression through negative real rates and inflation (ε ~0.45, Tangled Rope), and (3) monetary policy externalities imposed without consent (ε ~0.38, Tangled Rope). The present story focuses on the enforcement mechanism (the legal prohibition structure) rather than the distributional outcomes. The upstream story for this constraint is the institutional history of central banking; the downstream stories are specific extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(currency_monopoly_protection, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
