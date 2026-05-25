% ============================================================================
% CONSTRAINT STORY: us_sanctions_moex_2024
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_sanctions_moex_2024, []).

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
 *   constraint_id: us_sanctions_moex_2024
 *   human_readable: U.S. Sanctions on Moscow Exchange (MOEX) 2024
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The U.S. sanctions on Moscow Exchange (MOEX) imposed in 2024 constitute a
 *   multi-institutional constraint spanning geopolitical competition, capital
 *   market architecture, and technological infrastructure. The sanction
 *   regime creates asymmetric extraction (Russian capital markets isolated
 *   from Western price discovery and international liquidity) while
 *   maintaining a coordination function for the Western financial alliance
 *   (enforcing capital segregation as a collective security mechanism). The
 *   constraint exhibits all characteristics of a tangled rope: genuine
 *   coordination benefits for the Western bloc (enforcement of alliance
 *   coherence, capital flow control, institutional prestige), active
 *   enforcement requirements (OFAC designation, SWIFT restrictions,
 *   correspondent banking controls), and asymmetric extraction (Russian
 *   participants bear maximum costs while Western institutions capture
 *   coordination rents). The extractiveness has increased from initial
 *   imposition (0.45) to current level (0.68) as alternative systems mature
 *   and shadow-market opacity provides incomplete relief. Theater ratio has
 *   grown (0.35 → 0.52) as compliance documentation increasingly substitutes
 *   for actual economic segregation, indicating degradation toward piton
 *   status over extended timescale. The constraint's classification varies
 *   dramatically by perspective: trapped Russian traders see snare (no exit),
 *   Western institutional actors see rope (arbitrage options, coordination
 *   benefits), organized alternative-market builders see tangled rope
 *   (constrained but building workarounds), enforcement authorities see
 *   scaffold (temporary with implicit sunset), compliance departments see
 *   piton (degrading ritual), and analytical observers see the full
 *   tangled-rope hybrid that prevents both false naturalization (mountain)
 *   and naive coordination (rope).
 *
 * KEY AGENTS:
 *   - Moscow Exchange Participants: Primary victims (powerless/trapped) — domestic traders, clearing houses, market makers bear full extraction through capital flow isolation
 *   - U.S. and Western Financial Infrastructure: Primary beneficiary (institutional/arbitrage) — Treasury, OFAC, major banks capture coordination function and capital flow rents
 *   - Russian Capital Markets: Institutional victim (moderate/constrained) — MOEX and clearing system face operational constraints and revenue loss
 *   - BRICS/Alternative Infrastructure Builders: Secondary beneficiary (organized/constrained) — China, India, UAE develop parallel settlement systems (SPFS, RMB, digital currencies) with partial exit ramp
 *   - Enforcement Coalition: Organized implementer (organized/constrained) — Treasury, OFAC, EU authorities, major custodians maintain enforcement perimeter with sunset logic
 *   - Global Financial Compliance Systems: Piton actor (institutional/arbitrage) — banks develop degraded compliance theater that is largely performative after initial implementation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes hybrid structure that neither sanctions advocates nor opponents fully acknowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_sanctions_moex_2024, 0.68).
domain_priors:suppression_score(us_sanctions_moex_2024, 0.78).
domain_priors:theater_ratio(us_sanctions_moex_2024, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_sanctions_moex_2024, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_sanctions_moex_2024, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(us_sanctions_moex_2024, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_sanctions_moex_2024, tangled_rope).
narrative_ontology:human_readable(us_sanctions_moex_2024, "U.S. Sanctions on Moscow Exchange (MOEX) 2024").
narrative_ontology:topic_domain(us_sanctions_moex_2024, "geopolitical/economic").

domain_priors:requires_active_enforcement(us_sanctions_moex_2024).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_sanctions_moex_2024, us_financial_infrastructure).
narrative_ontology:constraint_beneficiary(us_sanctions_moex_2024, western_capital_markets).
narrative_ontology:constraint_victim(us_sanctions_moex_2024, russian_capital_markets).
narrative_ontology:constraint_victim(us_sanctions_moex_2024, moscow_exchange_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOSCOW EXCHANGE PARTICIPANTS (SNARE) — Domestic traders and market participants trapped within MOEX ecosystem face unilateral closure of international capital flows, forced ruble-denominated trading, and no practical exit from national financial architecture. Maximum experienced extraction with no alternatives. Zero degrees of freedom — cannot arbitrage, cannot migrate capital, cannot access Western markets.
constraint_indexing:constraint_classification(us_sanctions_moex_2024, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RUSSIAN CAPITAL MARKETS INFRASTRUCTURE (SNARE) — The exchange itself, clearing houses, and market-making ecosystem face operational constraints: reduced trading volume, eliminated foreign participation, forced isolation from global price discovery mechanisms. Constrained but not fully trapped — can operate domestically, but cannot exit the sanctions regime. Extraction flows through loss of revenue, institutional prestige, and market liquidity.
constraint_indexing:constraint_classification(us_sanctions_moex_2024, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. AND WESTERN FINANCIAL INFRASTRUCTURE (ROPE) — Beneficiary institutional actors experience the sanctions as a coordination mechanism enforcing capital segregation. Arbitrage exit option (can selectively engage or disengage Russian markets) means they experience this as coordination rather than coercion. Benefits from reduced Russian capital competition, enforced dollar/euro dominance in Western-aligned markets, and ability to control who participates in global financial architecture.
constraint_indexing:constraint_classification(us_sanctions_moex_2024, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE MARKET INFRASTRUCTURE (TANGLED ROPE) — Organized non-Western actors (China, India, UAE, India) building parallel settlement systems (SPFS, cross-border RMB arrangements, gold trading) experience both constraint and benefit. Constrained by inability to directly access MOEX, but benefit from creating alternative pathways. Mixed extraction-coordination: asymmetric pressure to build workarounds, but also genuine institutional incentive to create non-dollar-dependent infrastructure. Active enforcement required to maintain sanction walls; alternative markets provide partial exit ramp.
constraint_indexing:constraint_classification(us_sanctions_moex_2024, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ENFORCEMENT COALITION (SCAFFOLD) — OFAC, Treasury, EU authorities, and coordinating financial regulators see sanctions as temporary enforcement mechanism with explicit sunset logic tied to geopolitical conditions. Requires active coordination (SWIFT restrictions, correspondent banking controls, entity listing) to maintain enforcement perimeter. Theater ratio moderate (enforcement rituals necessary but not purely performative). Sunset condition implicit: sanctions terminate if political conditions change (armistice, regime change, negotiated settlement). Organized agents with deliberate exit mechanism.
constraint_indexing:constraint_classification(us_sanctions_moex_2024, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: SANCTIONS COMPLIANCE THEATER (PITON) — Global financial institutions (banks, brokers, custodians) maintain extensive compliance departments, audit trails, and screening protocols that are substantially performative: primary function is regulatory attestation rather than economic outcome. As sanctions regime persists, institutions develop workarounds, shadow compliance procedures, and transaction opacity that reduce actual enforcement. Theater ratio high — compliance documentation substitutes for economic isolation. Original function (capital segregation) degrades into regulatory theater maintained by institutional inertia and legal liability risk, not by actual market isolation mechanisms.
constraint_indexing:constraint_classification(us_sanctions_moex_2024, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, sanctions on MOEX are simultaneously a coordination mechanism (enforcing capital flow separation) and an extraction mechanism (consolidating Western financial hegemony). The constraint has genuine coordination function for the Western bloc — enforcing alliance coherence through capital control — but also extracts through forced dollar intermediation, capital flow rents, and institutional pricing power. Not a mountain (not inevitable) and not a pure rope (asymmetric extraction embedded). Genuine hybrid with both coordination and extraction functionally present.
constraint_indexing:constraint_classification(us_sanctions_moex_2024, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_sanctions_moex_2024_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_sanctions_moex_2024, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_sanctions_moex_2024, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_sanctions_moex_2024, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_sanctions_moex_2024, TR),
    TR >= 0.70.

:- end_tests(us_sanctions_moex_2024_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Russian market participants experience severe capital flow isolation — no access to Western liquidity, forced ruble-denomination, eliminated foreign participation. Extractiveness is not maximal (0.90+) because alternative markets provide partial relief and shadow-market workarounds reduce actual isolation. The trajectory from 0.45 to 0.68 reflects maturation of alternative payment systems (SPFS, RMB arrangements) that reduce effective isolation while increasing institutional complexity and transaction costs. Suppression (0.78): High. Russian actors face multiple coordinated barriers: OFAC designation blocks U.S. financial institutions, SWIFT restrictions eliminate correspondent banking, EU secondary sanctions target intermediaries, extraterritorial enforcement threatens global banks that service Russian accounts. Barriers are not complete (shadow markets, crypto, gold trading provide partial alternatives) but sufficiently high to trap most mainstream market participants. Enforcement is active and coordinated across Western alliance. Theater ratio (0.52): Moderate. Initial implementation (2022-2023) required genuine economic restructuring and functional capital segregation, producing lower theater. Current state shows increasing performativity: compliance documentation proliferates while shadow-market opacity and alternative settlement reduce actual isolation. Trend toward piton suggests that as alternatives mature, enforcement becomes increasingly ritual rather than functionally segregating capital. Theater ratio below 0.70 reflects that economic isolation remains partially real, not purely ornamental.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival divergence across six distinct types. Russian traders trapped in MOEX experience pure snare — unilateral extraction with no exit. Western institutions experience rope — coordination mechanism that benefits their capital positioning. Alternative-market builders experience tangled rope — constrained by sanctions but benefiting from institutional opportunity to build competing systems. Enforcement authorities experience scaffold — temporary coordination with explicit sunset (political conditions change, sanctions terminate). Global compliance systems experience piton — enforcement theater that degrades as alternatives mature. Analytical observer recognizes tangled rope with false-mountain risk (naturalizing contingent geopolitical choice as immutable constraint). The perspectival divergence reveals that 'sanctions' is not a single constraint but a presheaf: the same structural arrangement maps to different types depending on the observing position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by agent's structural relationship to the constraint. MOEX participants (trapped, powerless) have d ≈ 0.95 → f(d) ≈ 1.42 (maximum experienced extraction). Western institutions (beneficiary, institutional, arbitrage) have d ≈ 0.05 → f(d) ≈ -0.12 (negative experienced extraction — they benefit). Alternative-market builders (constrained, organized) have d ≈ 0.48 → f(d) ≈ 0.60 (symmetric/moderate extraction relative to them). The enforcement coalition (organized, constrained, sunset) has d ≈ 0.45 → f(d) ≈ 0.50 (bears coordination costs). Chi formula: Western beneficiaries experience χ = 0.68 × (-0.12) × 1.0 ≈ -0.08 (coordination benefit). MOEX traders experience χ = 0.68 × 1.42 × 1.0 ≈ 0.96 (severe effective extraction). Alternative builders experience χ = 0.68 × 0.60 × 1.2 ≈ 0.49 (moderate extraction with global scope amplification as they operate internationally). The derivation chain produces perspectival gap: beneficiaries see coordination (rope), victims see extraction (snare), organized actors see hybrid (tangled rope).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the extraction-coordination ambiguity by explicitly modeling both functions. The coordination function is real: Western alliance coherence does require capital segregation, enforcement does solve the collective action problem of preventing sanctions evasion, and the institutional mechanism does produce shared security benefit. But the extraction function is also real: Western institutions profit from capital flow rents (dollar intermediation, capital controls pricing), enforcement reveals hierarchy (U.S. Treasury sets rules, allies comply), and asymmetry in cost distribution (MOEX participants bear maximum costs, Western actors capture maximum benefits) confirms extraction mechanism. The tangled rope classification prevents three errors: (1) false rope (naive coordination model ignoring extraction), (2) false snare (treating sanctions as pure coercion without coordination function), (3) false mountain (naturalizing geopolitical contingency as immutable law). The omegas capture the genuine uncertainties: alternative-system maturity determines whether snare persists or transitions to scaffold; compliance opacity determines whether enforcement remains real or degrades to piton; coalition unity determines whether coordination is genuine or ornamental; sunset credibility determines whether temporary logic is structural or rhetorical. Chi formula across perspectives reveals that extraction concentration is real (MOEX traders experience χ ≈ 0.96) while coordination benefits are real (Western beneficiaries experience χ < 0), making the constraint genuinely hybrid rather than disguised extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_settlement_effectiveness,
    'Will BRICS settlement systems (SPFS, cross-border RMB, digital rupee arrangements) achieve sufficient liquidity and trust to functionally replace MOEX access to Western capital?',
    'Multi-year tracking of transaction volumes in alternative systems; correlation with MOEX liquidity metrics; assessment of price discovery efficiency in RMB-denominated vs ruble-denominated trading; measurement of capital flight velocity to alternative platforms',
    'If alternative systems mature (χ > 0.5): MOEX sanctions transition from snare to scaffold (temporary constraint with functional alternative). If alternatives remain illiquid: snare classification confirmed. If alternatives achieve parity: Western financial architecture loses coordination function and becomes pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_settlement_effectiveness, empirical, 'Whether alternative settlement systems can functionally replace MOEX access').

omega_variable(
    compliance_opacity_threshold,
    'At what shadow-market size does sanctions compliance become purely performative theater rather than actual capital segregation?',
    'Forensic analysis of trade settlement patterns; detection of circular trading, trade-for-trade compensation, and repatriation mechanisms; measurement of actual capital stock in sanctioned accounts vs shadow flows; assessment of regulatory detection rates',
    'If shadow markets reach 40%+ of MOEX flow: piton classification confirmed, snare classification becomes false summit. If shadow markets remain < 10%: snare classification maintained, enforcement effectiveness proven. Threshold determines whether sanctions achieve structural isolation or devolve into compliance ritual.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compliance_opacity_threshold, empirical, 'Threshold at which sanctions compliance becomes purely performative').

omega_variable(
    western_bloc_exit_credibility,
    'Is Western financial exclusion functionally irreversible (mountain: immutable structural outcome), or does the system retain genuine sunset logic (scaffold: temporary political choice)?',
    'Analysis of political preconditions for sanctions termination; assessment of technical reversibility (SWIFT reconnection, correspondent banking restoration); measurement of institutional lock-in (vested interests in maintaining regime); historical comparison to previous sanctions regimes (Iran, North Korea, Cuba) and their termination/modification pathways',
    'If sunset credible and conditions articulated: scaffold classification confirmed. If sunset rhetoric unsupported by actual policy mechanism: scaffold is aspirational, constraint is snare. If technically irreversible: appears mountain (immutable isolation) but is actually tangled rope (asymmetric institutional lock-in).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(western_bloc_exit_credibility, conceptual, 'Whether Western financial exclusion is reversible or structural lock-in').

omega_variable(
    coordination_vs_extraction_boundary,
    'Does enforcing capital segregation constitute coordination (shared commitment to common security goal) or extraction (using capital controls as unilateral rent-extraction mechanism)?',
    'Assessment of Western coalition unity (defection rates, shadow sanctions violations); measurement of coalition cost distribution (do allies absorb opportunity costs equally?); analysis of secondary sanctions targeting allies who don''t comply; comparison of enforcement against Russian capital vs. enforcement against competing Western capital (selective enforcement reveals extraction intent)',
    'If truly coordinated: rope classification for all Western actors. If enforcement asymmetrically targets allies or reveals hierarchy: tangled rope (coordination function overlays extraction). If Western actors profit unequally from isolation: snare classification for non-beneficiary allies. Chi formula reveals whether coordination function is genuine or ornamental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether sanctions represent coordinated security or asymmetric financial extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_sanctions_moex_2024, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(moex_tr_t0, us_sanctions_moex_2024, theater_ratio, 0, 0.35).
narrative_ontology:measurement(moex_tr_t6, us_sanctions_moex_2024, theater_ratio, 6, 0.42).
narrative_ontology:measurement(moex_tr_t12, us_sanctions_moex_2024, theater_ratio, 12, 0.52).

% Extraction over time
narrative_ontology:measurement(moex_be_t0, us_sanctions_moex_2024, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(moex_be_t6, us_sanctions_moex_2024, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(moex_be_t12, us_sanctions_moex_2024, base_extractiveness, 12, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_sanctions_moex_2024, enforcement_mechanism).
narrative_ontology:affects_constraint(us_sanctions_moex_2024, swift_settlement_architecture).
narrative_ontology:affects_constraint(us_sanctions_moex_2024, brics_payment_systems).
narrative_ontology:affects_constraint(us_sanctions_moex_2024, correspondent_banking_network).

% DUAL FORMULATION NOTE:
% U.S. sanctions on MOEX decomposes into three related constraints: (1) Capital segregation mechanism (extractiveness ≈ 0.68, tangled rope) — the primary constraint modeled here. (2) SWIFT connectivity restriction (extractiveness ≈ 0.55, scaffold) — temporary enforcement mechanism with explicit reversibility. (3) Alternative settlement infrastructure race (extractiveness ≈ 0.42, tangled rope) — downstream constraint where BRICS actors build workarounds. Each has distinct ε, distinct beneficiaries, and distinct sunset logic. The family linked through network.affects_constraints reveals that sanction effectiveness depends on the entire ecosystem of alternative pathways, not just capital segregation alone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_sanctions_moex_2024, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
