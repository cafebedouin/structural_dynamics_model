% ============================================================================
% CONSTRAINT STORY: risk_socialization_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_risk_socialization_threshold, []).

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
 *   constraint_id: risk_socialization_threshold
 *   human_readable: The Asymmetric Liability Trap
 *   domain: economic/political
 *
 * SUMMARY:
 *   The asymmetric liability trap describes a structural constraint in which
 *   a systemically critical entity (typically a large financial institution
 *   or state-backed utility) captures the asymmetry between profit
 *   distribution and loss distribution. During periods of stability, profits
 *   flow to shareholders and executives, but when systemic risk materializes,
 *   losses are socialized through taxpayer-funded bailouts. The constraint
 *   exhibits high suppression (barriers to alternative institutional
 *   arrangements prevent escape) and moderate extractiveness (the extraction
 *   is real but offset by genuine systemic coordination benefits). The
 *   tension between the two components — coordination benefit + extraction
 *   harm — makes this a diagnostic exemplar of when extraction and
 *   coordination coexist structurally. The increasing theater ratio over time
 *   (0.42 → 0.68) reflects the growing gap between stated regulatory intent
 *   (ending too-big-to-fail) and actual institutional practice (preserving
 *   implicit guarantees through capital requirement leniency, favorable
 *   stress test calibration, and resolution authority capture). The
 *   constraint is most clearly a snare from the perspectives of trapped
 *   taxpayers and constrained small business competitors, who experience pure
 *   extraction with no coordination benefit. From the regulator's
 *   institutional perspective, the bailout is coordination (preventing
 *   cascade failure). From the reform coalition's perspective, it is a
 *   temporary scaffold with a sunset clause (regulatory change, capital
 *   requirements, resolution authority). From the financial authority's own
 *   perspective, it is a piton — a degraded ritual maintained through inertia
 *   despite official claims that the problem has been solved.
 *
 * KEY AGENTS:
 *   - Taxpayers: Primary victim (powerless/trapped) — no exit from currency, taxation, deposit guarantees; bear crisis losses without consent
 *   - Small Business Competitors: Secondary victim (moderate/constrained) — cannot access implicit guarantee; fail when large competitor survives bailout
 *   - Depositors & Creditors: Secondary victim (moderate/constrained) — protected by implicit guarantee but only through socialization of losses
 *   - Shareholders & Executives: Primary beneficiary (powerful/mobile) — capture upside during stability, preserve value through bailout
 *   - Central Bank / Financial Regulator: Institutional actor (institutional/arbitrage) — sees bailout as coordination mechanism preventing systemic collapse
 *   - Progressive Reform Coalition: Organized agents (organized/constrained) — reformers viewing constraint as temporary (Dodd-Frank, capital requirements); constrained by regulatory capture
 *   - Financial System Authority: Institutional actor maintaining degraded ritual (institutional/arbitrage) — piton perspective; knows implicit guarantee remains despite stated regulatory solutions
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy choice as financial law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(risk_socialization_threshold, 0.58).
domain_priors:suppression_score(risk_socialization_threshold, 0.72).
domain_priors:theater_ratio(risk_socialization_threshold, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(risk_socialization_threshold, extractiveness, 0.58).
narrative_ontology:constraint_metric(risk_socialization_threshold, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(risk_socialization_threshold, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(risk_socialization_threshold, snare).
narrative_ontology:human_readable(risk_socialization_threshold, "The Asymmetric Liability Trap").
narrative_ontology:topic_domain(risk_socialization_threshold, "economic/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(risk_socialization_threshold, systemically_critical_firm).
narrative_ontology:constraint_victim(risk_socialization_threshold, taxpayers).
narrative_ontology:constraint_victim(risk_socialization_threshold, depositors_creditors).
narrative_ontology:constraint_victim(risk_socialization_threshold, competitors).
narrative_ontology:constraint_victim(risk_socialization_threshold, future_fiscal_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAXPAYER BASE (SNARE) — Trapped in socialization of losses without consent. No exit: cannot opt out of nation-state currency/insurance guarantees, cannot liquidate firm exposure, cannot recoup bailout costs. Experiences full extraction of crisis losses through tax liability. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.82.
constraint_indexing:constraint_classification(risk_socialization_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS COMPETITORS (SNARE) — Constrained by inability to access implicit government guarantee on liabilities. Large competitor survives crisis via bailout; small competitor fails and is liquidated. d≈0.88, f(d)≈1.32, σ=1.0 → χ≈0.76.
constraint_indexing:constraint_classification(risk_socialization_threshold, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE LEADERSHIP & SHAREHOLDERS (TANGLED ROPE) — Experience coordination benefit (firm survives, shareholder value preserved during bailout) but also extraction in long-term stock price depression post-crisis. However, the coordination function is real: the bailout preserves systemic function. d≈0.35, f(d)≈0.25, σ=1.0 → χ≈0.15. Low effective extraction reflects that benefit slightly dominates cost post-recovery.
constraint_indexing:constraint_classification(risk_socialization_threshold, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: CENTRAL BANK / FINANCIAL REGULATOR (ROPE) — Sees bailout as coordination mechanism: preventing systemic collapse protects the entire financial network. Regulator has arbitrage exit (can choose intervention timing, scope, conditions). d≈0.10, f(d)≈-0.02, σ=1.0 → χ≈-0.01. Net beneficiary: regulation is enforced, crisis contained, system restored.
constraint_indexing:constraint_classification(risk_socialization_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PROGRESSIVE REFORM COALITION (SCAFFOLD) — Organized advocates (labor unions, progressive politicians, consumer protection groups) frame the bailout as temporary (with stated intent to end too-big-to-fail through regulation, capital requirements, breakup). They see suppression of alternatives (regulatory capture, lobbying power) but believe the sunset clause is structural: Dodd-Frank, TARP restrictions, stress tests create a path toward markets where firm size doesn't guarantee rescue. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.51. Moderate extraction, but coalition has agency and exit path (regulatory change).
constraint_indexing:constraint_classification(risk_socialization_threshold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FINANCIAL SYSTEM AUTHORITY (PITON) — Maintains bailout protocol as degraded institutional ritual. Publicly claims that regulation and capital requirements have solved too-big-to-fail; privately prepares for the next crisis with the same playbook (emergency lending, equity injections, liquidity facilities). theater_ratio=0.68 reflects the performative regulatory theater: stress tests show stability, but the implicit guarantee remains. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.04. Institutional beneficiary but aware its own legitimacy is degraded.
constraint_indexing:constraint_classification(risk_socialization_threshold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FINANCIAL GRAVITY VIEW (MOUNTAIN) — From a civilizational view, systemic risk is an immutable property of financial networks: concentration is mathematically inevitable under profit-maximizing dynamics; cascade effects are thermodynamic facts about coupled systems; bailouts are therefore a law of nature, not a policy choice. However, the structural data (ε=0.58, suppression=0.72, theater=0.68) contradicts this. The engine will compute this as a false summit, revealing that 'financial gravity' naturalizes what is actually a policy choice: alternative regulatory regimes exist (EU-style separation, Canadian-style capital buffers, Nordic-style resolution authority) that distribute bailout risk differently.
constraint_indexing:constraint_classification(risk_socialization_threshold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(risk_socialization_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(risk_socialization_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(risk_socialization_threshold, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(risk_socialization_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(risk_socialization_threshold, TR),
    TR >= 0.70.

:- end_tests(risk_socialization_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from taxpayers through crisis-period socialization of losses, but the extraction is partially offset by genuine systemic benefits of maintaining financial stability. The asymmetry between profit privatization and loss socialization is the core extraction mechanism. The value reflects that while extraction is real and growing (trend 0.32 → 0.58), it is not as severe as pure rent-seeking (which would be 0.70+) because the coordination benefit (preventing systemic collapse) is structurally real. Suppression (0.72): High. Significant barriers prevent escape: alternative financial systems remain marginal; regulatory alternatives (decentralized, community-based finance) lack scale; political viability of non-bailout is uncertain; regulatory capture maintains the implicit guarantee despite stated policy solutions. Suppression is not total (0.95+) because regulatory reform, capital requirements, and stress testing create some genuine friction on the extraction mechanism. Theater ratio (0.68): High. Regulatory theater is substantial: stress tests show stability while implicit guarantee is maintained; Dodd-Frank claims to end too-big-to-fail while regulatory agencies interpret rules leniently; resolution authority protocols are created but never tested in live crisis; public discourse distinguishes between official policy (problem solved) and institutional reality (bailout apparatus remains), creating performative gap.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits sharp perspectival divergence. Taxpayers (powerless/trapped) see pure extraction (Snare) — they have no exit and bear full crisis costs. Small competitors (moderate/constrained) see similar extraction (Snare) — they cannot access the guarantee that keeps large competitors alive. Shareholders (powerful/mobile) see mixed benefit (Tangled Rope) — they experience coordination (firm survives) and some extraction (stock depression post-crisis), but the coordinate benefit typically dominates long-term. The regulator (institutional/arbitrage) sees coordination (Rope) — preventing systemic collapse is the benefit that justifies bailout. The reform coalition (organized/constrained) sees a temporary problem (Scaffold) — regulatory changes will reduce the implicit guarantee. The financial authority (institutional/arbitrage) knows the process is degraded (Piton) — it maintains bailout protocol as ritual despite official claims of solution. The civilizational analytical observer risks seeing immutable law (Mountain) — financial gravity makes bailouts inevitable — but the structural data reveals this as false naturalization: alternative regulatory regimes exist and distribute risk differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Taxpayers: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — no exit from national currency, taxation, or implicit guarantee system. Competitors: Victim + constrained → d≈0.88, f(d)≈1.32. High extraction — significant barriers to accessing same guarantee as large competitor. Shareholders/Executives: Beneficiary + mobile → d≈0.35, f(d)≈0.25. Low extraction (net beneficiary) — significant exit options (sell equity, move operations), benefit exceeds cost. Regulator: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.02. Net beneficiary — arbitrage exit (can choose intervention), coordination achieved. Reform coalition: Organized agent with constrained exit → d≈0.55, f(d)≈0.75. Moderate extraction with agency — can advocate for regulatory change, not trapped, but constrained by regulatory capture. Financial authority: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification comes from theater ratio, not directionality — authority knows process is degraded.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (Extractiveness = 0.58 > 0.46 → Required): The constraint resolves the mandatrophy by showing that snare classification (pure extraction) is correct from the powerless agent perspective (taxpayers, small competitors) but incomplete from institutional perspectives. The temptation to call this a Mountain (financial gravity makes bailouts inevitable) is the false summit that the engine detects: alternative regulatory regimes exist (EU resolution authority powers, Canadian capital-requirement-based approach, Nordic stress-tested resolution protocols) that distribute socialization differently. The temptation to call it pure Rope (coordination benefit of financial stability) is also incomplete: the coordination benefit does not require the asymmetry between profit privatization and loss socialization — the same stability could be achieved with symmetric risk distribution (equity-financed firms, common-equity tier 1 buffer requirements, pre-agreed resolution mechanisms). The actual constraint is a Snare from the victim perspective (trapped taxpayers, constrained competitors) because the asymmetry persists despite alternatives existing and the suppression (regulatory capture, political inevitability of bailout) prevents those alternatives from being chosen. The reform coalition's Scaffold perspective is real but subordinate: the sunset clause (Dodd-Frank, capital requirements, resolution authority) has not actually eliminated the implicit guarantee in practice, only created regulatory theater that creates appearance of solution. The engine's classification as Snare is therefore justified: the structural facts (asymmetric risk distribution, high suppression, no exit for most agents, extraction persists despite stated policy) are snare-characteristic. The theater ratio elevation (0.42 → 0.68) tracks the growing gap between regulatory intent and institutional reality, which is a piton diagnostic: degraded ritual maintained by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_hazard_threshold,
    'At what probability of future bailout does private incentive structure flip toward maximizing downside tail risk?',
    'Empirical measurement of risk-taking behavior (derivative positions, leverage ratios, asset concentration) as a function of implicit guarantee value; historical analysis of pre-crisis vs post-crisis behavior',
    'If threshold < 60% bailout probability: all major firms are in moral hazard regime, snare classification strengthens. If threshold > 90%: only rare stress scenarios trigger extraction, snare weakens to tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_hazard_threshold, empirical, 'Probability threshold at which moral hazard dominates firm behavior').

omega_variable(
    exit_alternative_availability,
    'Do alternative financial systems (crypto, decentralized finance, community banks) actually reduce taxpayer exposure to systemic risk, or merely increase correlated risk?',
    'Stress test analysis: if alternative systems scale, do they reduce too-big-to-fail concentration or introduce new systemic vulnerabilities? Comparison of bailout costs across regulatory regimes.',
    'If true alternatives exist: suppression index decreases, exit_options improve for some victims, snare weakens. If alternatives increase correlation: suppression strengthens, snare deepens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_alternative_availability, empirical, 'Whether alternative financial systems reduce systemic risk exposure').

omega_variable(
    political_viability_of_non_bailout,
    'Could a democratic government actually allow a systemically critical firm to fail and manage the consequences, or is bailout politically inevitable?',
    'Comparative institutional analysis: Iceland''s Icesave default, Argentina''s bank defaults, vs US bailouts; public opinion polling on willingness to accept short-term contagion for long-term discipline',
    'If non-bailout is politically viable: suppression index decreases (real alternative exists), exit_options for regulatory reformers improve. If bailout is inevitable: suppression is maximum, snare deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_viability_of_non_bailout, preference, 'Whether non-bailout default is politically feasible for systemically critical firms').

omega_variable(
    regulatory_capture_extent,
    'How much of the implicit guarantee persists as a formal protection vs how much is maintained through regulatory capture and political lobbying?',
    'Institutional analysis: comparison of stated policy (Dodd-Frank ending too-big-to-fail) vs actual regulatory behavior (favorable supervision, lenient stress test thresholds, lobbying influence on resolution authority); measurement of capture intensity through regulatory comment analysis and agency budget allocation',
    'If capture is total: suppression is maximum (no real regulatory alternative), snare is unambiguous. If capture is partial: some scaffolding is real, reform coalition has genuine exit path.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'Extent to which implicit guarantee is maintained through regulatory capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(risk_socialization_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(risk_soc_tr_t0, risk_socialization_threshold, theater_ratio, 0, 0.42).
narrative_ontology:measurement(risk_soc_tr_t5, risk_socialization_threshold, theater_ratio, 5, 0.55).
narrative_ontology:measurement(risk_soc_tr_t10, risk_socialization_threshold, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(risk_soc_be_t0, risk_socialization_threshold, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(risk_soc_be_t5, risk_socialization_threshold, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(risk_soc_be_t10, risk_socialization_threshold, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(risk_socialization_threshold, enforcement_mechanism).
narrative_ontology:affects_constraint(risk_socialization_threshold, regulatory_capture_threshold).
narrative_ontology:affects_constraint(risk_socialization_threshold, moral_hazard_incentive_structure).
narrative_ontology:affects_constraint(risk_socialization_threshold, deposit_insurance_moral_hazard).

% DUAL FORMULATION NOTE:
% The asymmetric liability trap decomposes into three structurally distinct constraints: (1) regulatory_capture_threshold (ε≈0.45) — how regulatory capture enables the implicit guarantee; (2) moral_hazard_incentive_structure (ε≈0.55) — how guaranteed bailout changes firm risk-taking; (3) deposit_insurance_moral_hazard (ε≈0.38) — how deposit guarantee creates asymmetric incentives. This story (ε=0.58) is the systemic integration constraint — the combined effect of all three. Decomposition is necessary because empirical evidence and policy levers differ across the three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(risk_socialization_threshold, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
