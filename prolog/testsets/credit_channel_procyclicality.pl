% ============================================================================
% CONSTRAINT STORY: credit_channel_procyclicality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credit_channel_procyclicality, []).

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
 *   constraint_id: credit_channel_procyclicality
 *   human_readable: Credit Channel Procyclicality
 *   domain: macroeconomic_finance
 *
 * SUMMARY:
 *   Credit channel procyclicality describes the mechanism by which credit
 *   supply systematically contracts during economic downturns and expands
 *   during upturns, amplifying business cycle fluctuations. Banks reduce
 *   lending precisely when borrowers most need credit, and increase lending
 *   when growth is already strong. This constraint exhibits all six DR
 *   classification types depending on observational position. For small
 *   borrowers trapped without access to alternative funding, the constraint
 *   appears as pure extraction (Snare) — they bear all costs of credit
 *   withdrawal. For large institutions with central bank backstops, it
 *   appears as coordination (Rope) — they manage risk rationally. For
 *   regulators, it appears as an institutional amplification mechanism they
 *   are simultaneously enforcing and trying to suppress (Tangled Rope). For
 *   macroprudential policy frameworks, it appears as a temporary problem
 *   being solved through countercyclical buffers (Scaffold). For the view
 *   that credit procyclicality is a natural market feedback, it appears as
 *   institutional inertia wearing the mask of neutrality (Piton). For the
 *   analytical observer, it risks appearing as an immutable natural law
 *   (Mountain) when it is actually contingent on specific institutional
 *   structures.
 *
 * KEY AGENTS:
 *   - Small and Medium Enterprises (SMEs): Primary victims (powerless/trapped) — depend on credit access; face collapsing availability during downturns; no alternative funding sources available at scale
 *   - Household Borrowers: Secondary victims (moderate/constrained) — benefit from credit expansion but forced into deleveraging during contractions; face higher rates, tighter standards, refinancing barriers
 *   - Large Financial Institutions: Primary beneficiaries (institutional/arbitrage) — reduce exposure during downturns to protect capital; access central bank backstops; experience procyclicality as risk management
 *   - Regulatory Authorities: Institutional actor (organized/constrained) — enforce capital requirements and stress tests that mechanically amplify procyclicality; constrained by mandate and information limits
 *   - Macroprudential Policy Framework: Organized agents (organized/mobile) — countercyclical capital buffers designed to suppress procyclicality; represents exit path through policy coordination
 *   - Real Economy Stability: Abstract victim (powerless/trapped) — aggregate output volatility is amplified by credit procyclicality; bears long-term cost of suppressed potential output during downturns
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credit_channel_procyclicality, 0.58).
domain_priors:suppression_score(credit_channel_procyclicality, 0.62).
domain_priors:theater_ratio(credit_channel_procyclicality, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credit_channel_procyclicality, extractiveness, 0.58).
narrative_ontology:constraint_metric(credit_channel_procyclicality, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(credit_channel_procyclicality, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credit_channel_procyclicality, tangled_rope).
narrative_ontology:human_readable(credit_channel_procyclicality, "Credit Channel Procyclicality").
narrative_ontology:topic_domain(credit_channel_procyclicality, "macroeconomic_finance").

domain_priors:requires_active_enforcement(credit_channel_procyclicality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credit_channel_procyclicality, large_financial_institutions).
narrative_ontology:constraint_beneficiary(credit_channel_procyclicality, institutional_investors).
narrative_ontology:constraint_victim(credit_channel_procyclicality, small_medium_enterprises).
narrative_ontology:constraint_victim(credit_channel_procyclicality, household_borrowers).
narrative_ontology:constraint_victim(credit_channel_procyclicality, real_economy_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL BORROWER (SNARE) — During economic downturns, credit availability collapses precisely when borrowers most need it. Access to credit is essential to business continuity, yet the mechanism withdraws it systematically. No exit option exists: firms cannot manufacture their own credit, and alternative funding sources (equity, trade credit) are equally procyclical. Bears full extraction.
constraint_indexing:constraint_classification(credit_channel_procyclicality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HOUSEHOLD BORROWER (TANGLED ROPE) — Benefits from credit access during expansions (enables consumption smoothing, housing purchases). But faces severe constraints during contractions: tightened lending standards, higher rates, reduced access to refinancing. Coordination function (credit enables consumption) is real; asymmetric extraction is real (forced deleveraging during downturns damages household welfare disproportionately).
constraint_indexing:constraint_classification(credit_channel_procyclicality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE FINANCIAL INSTITUTION (ROPE) — Experiences credit procyclicality as a risk coordination mechanism: reducing exposure during downturns protects bank capital and profitability. The constraint aligns bank incentives with prudent risk management. Access to central bank backstop (discount window, emergency lending) provides arbitrage exit — banks can offload risk during crises. Net beneficiary with exit option.
constraint_indexing:constraint_classification(credit_channel_procyclicality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Coordinates financial stability through capital requirements and stress testing (genuine coordination function). But enforcement creates procyclicality: binding capital constraints force deleveraging during downturns, amplifying credit collapse. Regulators lack perfect information about tail risks and face political pressure during crises. Constrained by institutional constraints to their own mandate.
constraint_indexing:constraint_classification(credit_channel_procyclicality, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: MACROPRUDENTIAL POLICY (SCAFFOLD) — Countercyclical capital buffers (Basel III framework, dynamic provisioning rules) represent temporary institutional scaffolding designed to suppress procyclicality. Sunset clause is embedded: as capital requirements become sufficiently countercyclical, traditional procyclicality should decline. Exit path exists through policy coordination (build sufficient buffers in expansions, release them in downturns).
constraint_indexing:constraint_classification(credit_channel_procyclicality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL FEEDBACK NARRATIVE (PITON) — The claim that credit procyclicality is a natural, inherent feedback mechanism in credit markets (credit conditions reflect fundamentals; fundamentals worsen in downturns; credit tightens naturally) is largely performative. The narrative obscures institutional amplification: regulatory constraints, risk-weighting, mark-to-market accounting, and funding pressures create amplification far beyond what fundamentals alone would predict. Persists as a neutral description of market mechanics despite being substantially a contingent institutional artifact.
constraint_indexing:constraint_classification(credit_channel_procyclicality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, some credit procyclicality may be inherent to information asymmetry: lenders cannot fully verify borrower quality, so they rely on collateral values and backward-looking default rates. In downturns, collateral values fall and default rates spike, making lending rationally riskier. However, this perspective risks naturalizing institutional structures (collateral-based lending, regulatory constraints, mark-to-market) as immutable laws. The engine's false summit detector identifies this risk.
constraint_indexing:constraint_classification(credit_channel_procyclicality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credit_channel_procyclicality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credit_channel_procyclicality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credit_channel_procyclicality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credit_channel_procyclicality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credit_channel_procyclicality, TR),
    TR >= 0.70.

:- end_tests(credit_channel_procyclicality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint systematically channels credit away from borrowers with highest need (during downturns) and toward those with lowest need (during upturns). This represents significant misallocation. However, extractiveness is not maximal (0.85+) because: (1) some of the procyclicality reflects rational response to real risk increases, and (2) regulatory interventions are beginning to offset the mechanism. The 0.58 value reflects net extraction after accounting for these mitigation mechanisms. Suppression (0.62): High. Borrowers facing credit crunches have limited exit options: they cannot manufacture credit, equity raising is procyclical (stock markets crash in downturns), trade credit from suppliers tightens, and informal credit markets may be unavailable or at usurious rates. Large institutions have arbitrage exit (central bank lending facilities); small borrowers do not. Suppression reflects this asymmetry. Theater ratio (0.45): Moderate-low. Unlike some constraints, procyclicality has genuine functional content — banks genuinely do need to adjust lending based on changing risk conditions. The theater comes from the narrative that procyclicality is purely a natural market feedback when it is actually substantially amplified by regulatory, accounting, and funding structure constraints. The constraint is not primarily performative but its naturalization in policy discourse is.
 *
 * PERSPECTIVAL GAP:
 *   The original research group sees coordination (Rope) — they are solving the legitimate problem of communicating findings. The open science coalition sees a temporary problem with a sunset (Scaffold) — arXiv and registered reports are building alternative pathways. The journal editorial system sees its own degraded ritual (Piton) — peer review persists through inertia, not function. Replication groups see mixed coordination and extraction (Tangled Rope) — the system both enables and constrains their work. The field's epistemic reliability sees pure extraction (Snare) — premature claims contaminate the literature with no self-correction mechanism. The civilizational analytical observer risks seeing an immutable natural law (Mountain) — verification lag is inherent to science — but the structural data reveals this as a false summit: the contingent institutional arrangements (career incentives, funding concentration, publication bias) are not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) is derived from base extractiveness (ε=0.58), their directionality value (d), the scope modifier, and their power level. Small borrowers with trapped exit experience high d (0.92-0.95) and high f(d); large institutions with arbitrage exit experience low d (0.08-0.15) and negative f(d). The scope modifier σ(S) is 1.0 (national scope of credit regulation) to 1.1-1.2 (global financial markets). For trapped agents at global scope, effective extraction χ reaches 0.85-0.90 (snare territory). For institutional beneficiaries, χ stays in rope territory through the arbitrage exit reducing d. The gap between 0.90 (snare) and 0.15 (rope) from the same base ε=0.58 demonstrates how exit options determine experienced constraint intensity.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: Credit procyclicality is a genuine tangled rope, not a snare masquerading as rope. The genuine coordination function is credit allocation according to perceived risk — banks need to adjust loan portfolios when risk conditions change. The genuine extraction is the asymmetry in who bears the cyclical adjustment costs: large institutions buffer through capital and central bank access; small borrowers buffer through output contraction. The mandatrophy is resolved by recognizing that procyclicality simultaneously solves a real coordination problem (risk-adjusted lending) and creates real asymmetric extraction (cyclical amplification of borrower distress). Policy solutions (countercyclical buffers, automatic stabilizers, broader credit access) can reduce extraction without eliminating coordination. The constraint is not inherently a snare that should be eliminated, but a tangled rope that should be rebalanced to reduce extraction while preserving coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_vs_institutional_amplification,
    'What fraction of observed credit procyclicality is inherent to information asymmetry versus amplified by regulatory, accounting, and funding structure constraints?',
    'Comparative analysis of credit cycles across regulatory regimes with different capital requirements, provisioning rules, and mark-to-market accounting; decomposition of credit supply shocks into information-driven vs constraint-driven components using high-frequency bank lending data',
    'If inherent fraction > 70%: mountain classification is justified; procyclicality is fundamental to credit markets. If < 40%: snare classification is justified; procyclicality is primarily an institutional extraction mechanism. Intermediate values support tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_vs_institutional_amplification, empirical, 'Inherent versus institutional amplification of credit procyclicality').

omega_variable(
    countercyclical_policy_effectiveness,
    'Can macroprudential policy (countercyclical buffers, dynamic provisioning) actually suppress credit procyclicality without creating new distortions or shifting risk to unregulated sectors?',
    'Empirical evaluation of Basel III implementation across jurisdictions; measurement of credit cycle amplitude pre- and post-countercyclical framework adoption; tracking of regulatory arbitrage and shadow banking growth in response to tighter regulation',
    'If effective: scaffold perspective validated — policy provides genuine exit path with sunset. If ineffective or generates risk shifting: procyclicality persists under new mechanism; tangled_rope classification persists. If generates catastrophic unintended consequences: snare classification from policy-maker perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(countercyclical_policy_effectiveness, empirical, 'Effectiveness of macroprudential countercyclical policy').

omega_variable(
    large_institution_arbitrage_availability,
    'Are central bank backstops (discount window, emergency lending facilities) genuinely available to all financial institutions, or only to systemically important large banks?',
    'Historical analysis of emergency lending facility access during crises (2008 financial crisis, COVID-19 shock); comparison of central bank lending facility terms across institution sizes; measurement of de facto access barriers vs formal eligibility rules',
    'If accessible to all: rope classification for all institutional perspectives holds. If restricted to large institutions: snare/tangled_rope classification from smaller institution perspective; institutionalizes size-based extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(large_institution_arbitrage_availability, empirical, 'Central bank backstop access across institution sizes').

omega_variable(
    fiscal_stabilization_substitutability,
    'Can fiscal policy (automatic stabilizers, transfer programs) substitute for credit availability during downturns, or does credit channel procyclicality persist regardless of fiscal accommodation?',
    'Comparative analysis of credit cycles in economies with strong automatic stabilizers vs weak ones; measurement of household consumption smoothing capability via fiscal transfers vs credit access; decomposition of employment/output losses in credit crunches with and without fiscal offset',
    'If fiscal can substitute: procyclicality becomes a coordination problem solvable through policy coordination (scaffold/rope). If fiscal cannot substitute: procyclicality remains a structural constraint even with active stabilization (snare persists from borrower perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_stabilization_substitutability, empirical, 'Fiscal versus credit channel in cycle mitigation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credit_channel_procyclicality, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccproc_tr_t0, credit_channel_procyclicality, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ccproc_tr_t3, credit_channel_procyclicality, theater_ratio, 3, 0.4).
narrative_ontology:measurement(ccproc_tr_t6, credit_channel_procyclicality, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(ccproc_be_t0, credit_channel_procyclicality, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ccproc_be_t3, credit_channel_procyclicality, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(ccproc_be_t6, credit_channel_procyclicality, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credit_channel_procyclicality, resource_allocation).
narrative_ontology:affects_constraint(credit_channel_procyclicality, financial_accelerator_mechanism).
narrative_ontology:affects_constraint(credit_channel_procyclicality, collateral_feedback_loop).

% DUAL FORMULATION NOTE:
% Credit procyclicality is downstream of financial accelerator dynamics and collateral feedback loops, but represents a distinct structural constraint. The financial accelerator (net worth → credit access → investment → output → net worth) has its own extractiveness reflecting the amplification mechanism; credit procyclicality captures the distributional asymmetry of who bears cyclical adjustment costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(credit_channel_procyclicality, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
