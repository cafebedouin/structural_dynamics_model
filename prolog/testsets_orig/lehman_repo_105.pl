% ============================================================================
% CONSTRAINT STORY: lehman_repo_105
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lehman_repo_105, []).

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
 *   constraint_id: lehman_repo_105
 *   human_readable: Lehman Brothers Repo 105 Accounting Maneuver
 *   domain: economic/financial_regulation
 *
 * SUMMARY:
 *   Repo 105 was a structured finance technique employed by Lehman Brothers
 *   from 2001 through its collapse in September 2008. The maneuver involved
 *   temporary sales of securities with a commitment to repurchase them at a
 *   slightly higher price — a classic repo structure — but with a critical
 *   difference: Lehman would execute the repo in the final weeks of each
 *   quarter, reducing balance-sheet liabilities for the published financial
 *   statements, then reverse the transaction after quarter-end when the
 *   financial reports had been released. The effect was to lower reported
 *   leverage ratios while keeping Lehman's actual economic exposure
 *   unchanged. Between $38 billion and $50 billion in liabilities were
 *   cyclically moved off the balance sheet using Repo 105. The constraint
 *   operated by exploiting a regulatory gap: under US GAAP accounting rules,
 *   repos meeting certain criteria qualified as sales and thus reduced
 *   reported liabilities; under IFRS rules used internationally, the same
 *   transactions were treated as secured borrowings and liabilities remained
 *   on balance sheet. Lehman exploited this gap through use of specific
 *   counterparties (primarily Citigroup and Bank of New York Mellon) that
 *   accommodated the timing. Rating agencies and investors relied on reported
 *   leverage ratios to assess solvency; concealment of true leverage delayed
 *   downgrade cascades and counterparty flight, extracting confidence from
 *   the market during a period when Lehman's fundamentals were deteriorating.
 *   The constraint exemplifies how modern financial snares operate: they use
 *   regulatory fragmentation and information asymmetry to concentrate
 *   counterparty risk exposure while suppressing market price discovery.
 *
 * KEY AGENTS:
 *   - Lehman Brothers Management: Primary beneficiary (institutional/arbitrage) — captured temporary market confidence and access to funding by reporting lower leverage
 *   - Lehman Counterparties: Primary victims (powerless/trapped) — transacted with Lehman believing published leverage metrics; experienced extraction as uncompensated counterparty risk exposure
 *   - Lehman Shareholders: Primary victims (powerless/trapped) — received false confidence through manipulated metrics; extraction occurred as equity destruction when collapse revealed true leverage
 *   - Financial System: Systemic victim (powerless/trapped) — concentrated counterparty risk exposure to Lehman without market awareness; extraction occurred as cascading defaults and credit freeze
 *   - Rating Agencies and Regulators: Organized victims (organized/constrained) — trapped in monitoring role but systematically deceived; extraction of regulatory capacity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — reveals Repo 105 as exemplary of financial snare dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lehman_repo_105, 0.78).
domain_priors:suppression_score(lehman_repo_105, 0.82).
domain_priors:theater_ratio(lehman_repo_105, 0.88).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lehman_repo_105, extractiveness, 0.78).
narrative_ontology:constraint_metric(lehman_repo_105, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(lehman_repo_105, theater_ratio, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lehman_repo_105, snare).
narrative_ontology:human_readable(lehman_repo_105, "Lehman Brothers Repo 105 Accounting Maneuver").
narrative_ontology:topic_domain(lehman_repo_105, "economic/financial_regulation").

domain_priors:requires_active_enforcement(lehman_repo_105).

% --- Structural relationships ---
narrative_ontology:constraint_victim(lehman_repo_105, lehman_counterparties).
narrative_ontology:constraint_victim(lehman_repo_105, lehman_shareholders).
narrative_ontology:constraint_victim(lehman_repo_105, lehman_creditors).
narrative_ontology:constraint_victim(lehman_repo_105, financial_system_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COUNTERPARTIES AND CREDITORS (SNARE) — Trapped counterparties had no exit option. They transacted with Lehman believing published leverage ratios were accurate. The Repo 105 mechanism extracted information rent by concealing true leverage. Creditors discovered only at collapse that Lehman's solvency was false. Maximum experienced extraction — no prior knowledge, no recourse, no exit.
constraint_indexing:constraint_classification(lehman_repo_105, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LEHMAN SHAREHOLDERS (SNARE) — Trapped in illiquidity. The constraint extracted equity value by concealing leverage deterioration. Shareholders received false confidence through manipulated leverage metrics, preventing rational exit during the window when some value could have been salvaged. Experienced extraction as loss of firm value and destroyed capital.
constraint_indexing:constraint_classification(lehman_repo_105, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: FINANCIAL SYSTEM STABILITY (SNARE) — Trapped systemically. The constraint extracted stability by concentrating counterparty risk exposure to Lehman without market awareness. Repo 105 suppressed price discovery and hidden leverage propagation. The system bore maximum extraction cost at collapse: cascading counterparty defaults, frozen credit markets, contagion across global institutions. No exit option for the aggregate system.
constraint_indexing:constraint_classification(lehman_repo_105, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: LEHMAN MANAGEMENT (PITON) — Benefited from temporary balance-sheet concealment via Repo 105 mechanism. Experienced as a coordination tool for 'managing market perception,' not extraction. The constraint's theater_ratio of 0.88 reflects that Repo 105 was almost entirely performative: the economic substance was unchanged (Lehman's liabilities and leverage were identical before and after the repo); only the accounting presentation shifted. Management saw this as a legitimate financial management technique, maintained through regulatory arbitrage (exploiting differences between US GAAP and IFRS on repo accounting). As regulatory scrutiny increased, the mechanism became inert — no longer functional as concealment — but management continued using it through institutional inertia. Piton classification derives from the degradation of function combined with high theater.
constraint_indexing:constraint_classification(lehman_repo_105, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AUTHORITIES (SNARE) — Organized but constrained by structural lag. Rating agencies, SEC, and banking regulators had incomplete data on Repo 105 mechanics. The constraint extracted regulatory capacity by concentrating information asymmetry — the agencies could not exit regulatory responsibility but were systematically deceived about leverage. Post-collapse, regulatory response (enhanced disclosure, repo accounting harmonization) reveals the prior victimhood: they were trapped in a role requiring monitoring of opaque mechanisms.
constraint_indexing:constraint_classification(lehman_repo_105, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — Civilizational view on financial extraction. Repo 105 reveals a structural feature of modern finance: the asymmetric distribution of information about leverage and counterparty risk. The constraint extracted systemic stability by concentrating hidden liabilities in nominally solvent institutions. From the analytical perspective, this is not an immutable natural law but a snare — a pure extraction mechanism with zero coordination function, high suppression of alternatives (regulatory arbitrage), and maximum coercive overhead (the threat of counterparty contagion). The analytical observer sees Repo 105 as exemplary of how modern financial snares operate: regulatory arbitrage creates pockets where extraction can persist until collapse.
constraint_indexing:constraint_classification(lehman_repo_105, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lehman_repo_105_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lehman_repo_105, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lehman_repo_105, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lehman_repo_105, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lehman_repo_105, TR),
    TR >= 0.70.

:- end_tests(lehman_repo_105_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.78): High. Lehman extracted confidence and market access by concealing leverage. The extraction was nearly total because counterparties had no basis for pricing counterparty risk accurately. The metric reflects that Lehman captured the full information rent from the leverage concealment. Suppression (0.82): Very high. Multiple mechanisms suppressed alternatives and exit: regulatory fragmentation (GAAP/IFRS gap) enabled the technique; quarterly disclosure timing allowed temporary concealment; rating agency reliance on reported metrics prevented downgrade; counterparty access to detailed leverage data was structurally limited. Theater ratio (0.88): Very high. Repo 105 is nearly pure theater: the economic substance of Lehman's liabilities and leverage was completely unchanged by the transaction. The only thing that changed was the accounting presentation. No real coordination or efficiency gain occurred — the entire function was performative leverage ratio management. The increasing theater_ratio over the interval reflects the mechanism becoming more purely theatrical as underlying leverage deteriorated and the gap between reported and actual metrics widened.
 *
 * PERSPECTIVAL GAP:
 *   All non-analytical perspectives classify Repo 105 as Snare, reflecting that the constraint exhibited no coordination function and pure extraction. Lehman management (piton perspective) experienced it as a legitimate financial management technique — they genuinely believed quarterly leverage ratio management was normal practice. The piton classification reflects their institutional degradation: management continued using Repo 105 even as it became inert (regulatory scrutiny increased, market participants began to understand the technique), maintaining it through institutional momentum rather than function. The analytical observer confirms the snare classification across all structural victims, but the piton divergence reveals a critical feature: the beneficiary's experience of extraction as legitimate coordination is precisely what allows snares to persist. Lehman management did not experience moral culpability; they experienced routine financial management. The perspectival gap is between the beneficiary's sense of legitimacy and all victims' experience of pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d values are determined by structural position relative to the constraint. Counterparties and shareholders (trapped, powerless) experience maximum d → maximum f(d) → maximum chi. Rating agencies and regulators (organized, constrained) experience high d but with some agency → moderately high f(d). Lehman management (institutional, arbitrage) experiences low d → negative f(d) — the constraint benefits them. The beneficiary/victim asymmetry is stark: no genuine beneficiaries emerge (Lehman benefits but at cost of systemic extraction), and all other actors are victims. The suppression value (0.82) reflects that alternatives were substantially closed: market participants could not easily price counterparty risk; regulators could not easily detect the technique; shareholders could not exit before collapse.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing Repo 105 as a pure snare — a constraint with zero coordination function that persisted entirely through extraction and suppression. The constraint did not solve any genuine collective action problem (pure rope) or create hybrid coordination-extraction (tangled rope). Lehman's quarterly leverage ratio management provided no external benefit. The beneficiary's experience of the constraint as legitimate financial practice is the mechanism through which the snare persisted: management genuinely believed they were engaging in normal balance-sheet management. Rating agencies and investors, trapped by reliance on reported metrics, could not exit. Counterparties, trapped by limited information, could not price risk accurately. The financial system, trapped in interconnected counterparty exposure, bore the full extraction cost at collapse. The classification as snare is unambiguous: high extractiveness (0.78), high suppression (0.82), high theater (0.88), zero coordination function, structural reliance on information asymmetry for persistence. Repo 105 exemplifies how modern financial snares operate: they exploit regulatory fragmentation and information asymmetry to concentrate risk exposure while suppressing market discovery. Post-Lehman regulatory harmonization (FAS 166-167, IFRS 9 repo accounting) reduced the regulatory gap that enabled the technique, but the underlying snare mechanism — exploiting temporary leverage reporting gaps to extract market confidence — remains available through other channels (balance-sheet engineering, window dressing before disclosure periods).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accounting_vs_economic_substance,
    'Was Repo 105 a fraudulent concealment of Lehman''s true leverage, or a legitimate accounting technique reflecting differences in how liabilities are classified under GAAP vs IFRS?',
    'Forensic analysis of intent: internal emails and management communications discussing Repo 105''s purpose. Analysis of whether Lehman disclosed the off-balance-sheet treatment to investors and rating agencies. Regulatory guidance on repo accounting at the time of use.',
    'If intentional fraud: classification as snare is confirmed — deliberate concealment. If regulatory arbitrage: classification softens toward piton — a degraded but technically legal technique. Intent determines whether Repo 105 was extraction or institutional decay.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accounting_vs_economic_substance, empirical, 'Whether Repo 105 was fraudulent concealment or permissible accounting arbitrage').

omega_variable(
    market_participant_awareness,
    'Did market participants (counterparties, rating agencies, investors) have actual or constructive knowledge of Repo 105 during its use, and did they price leverage accordingly?',
    'Analysis of public disclosures and footnote detail in Lehman financial statements. Interviews with rating agency analysts and major counterparties. Market-price behavior around Lehman debt spreads relative to actual leverage metrics post-revelation.',
    'If participants knew: extraction level was lower — information asymmetry was partial. If participants were systematically uninformed: extraction was maximal — the constraint extracted by concentrating knowledge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_participant_awareness, empirical, 'Market participant awareness of Repo 105 during the period of use').

omega_variable(
    regulatory_gap_inevitability,
    'Given the GAAP/IFRS divergence on repo accounting treatment, was the Repo 105 gap an unavoidable feature of fragmented global financial regulation, or a deliberately exploited loophole?',
    'Historical analysis of when the GAAP/IFRS divergence emerged. Analysis of whether other large banks used similar techniques and at what scale. Regulatory deliberation records on repo accounting harmonization timing.',
    'If unavoidable: the constraint represents a structural gap in coordination (Rope or Scaffold). If deliberately exploited: the constraint is pure extraction (Snare). Resolving this determines whether post-Lehman regulatory harmonization closed a structural flaw or closed an intentional exploitation vector.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_gap_inevitability, empirical, 'Whether the GAAP/IFRS regulatory gap was unavoidable or deliberately exploited').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lehman_repo_105, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(repo105_tr_t0, lehman_repo_105, theater_ratio, 0, 0.72).
narrative_ontology:measurement(repo105_tr_t4, lehman_repo_105, theater_ratio, 4, 0.82).
narrative_ontology:measurement(repo105_tr_t8, lehman_repo_105, theater_ratio, 8, 0.88).

% Extraction over time
narrative_ontology:measurement(repo105_be_t0, lehman_repo_105, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(repo105_be_t4, lehman_repo_105, base_extractiveness, 4, 0.72).
narrative_ontology:measurement(repo105_be_t8, lehman_repo_105, base_extractiveness, 8, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lehman_repo_105, enforcement_mechanism).
narrative_ontology:affects_constraint(lehman_repo_105, rating_agency_procyclicality).
narrative_ontology:affects_constraint(lehman_repo_105, counterparty_risk_opacity).
narrative_ontology:affects_constraint(lehman_repo_105, regulatory_arbitrage_leverage).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
