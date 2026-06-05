% ============================================================================
% CONSTRAINT STORY: lehman_repo_105
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    constraint_indexing:directionality_override/3,
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
 *   Repo 105 was an accounting maneuver used by Lehman Brothers from
 *   approximately 2001-2008 to temporarily move billions in liabilities off
 *   its balance sheet at quarter-end, primarily to reduce reported leverage
 *   and hide financial deterioration from investors, credit rating agencies,
 *   and regulators. The mechanism exploited a technical loophole in FAS 140
 *   (Financial Accounting Standard 140: 'Accounting for Transfers and
 *   Servicing of Financial Assets and Extinguishments of Liabilities').
 *   Lehman would sell mortgage-backed securities and other assets to
 *   financial institutions (primarily Deutsche Bank and Barclays) with a
 *   simultaneous repurchase agreement at slightly higher prices. Under FAS
 *   140, if the repo had a sufficiently short maturity and the seller
 *   retained minimal economic interest, it was treated as a 'true sale'
 *   rather than a financing arrangement — meaning the assets and
 *   corresponding liabilities could be removed from the balance sheet
 *   entirely. Lehman would then re-enter into similar repos after quarter-end
 *   to restore the same exposure. The constraint extracted value through
 *   information asymmetry: reported leverage ratios overstated financial
 *   health, enabling Lehman to maintain investment-grade credit ratings,
 *   access cheaper funding, and inflate equity prices during periods when
 *   true leverage was catastrophic. The constraint operated at scale: by
 *   September 2008, Lehman was using Repo 105 to hide approximately $50
 *   billion in liabilities (roughly 11% of reported assets). The theater
 *   ratio increased over time as the maneuver became more routine and
 *   institutionalized—auditors and counterparties internalized the technical
 *   compliance without assessing substance. The extractiveness increased as
 *   Lehman's actual financial condition deteriorated and the gap between
 *   reported and true leverage widened. When Lehman collapsed in September
 *   2008, the constraint reversed catastrophically: counterparties discovered
 *   they held collateral pledged multiple times over, investors realized they
 *   had been lied to through lawful accounting, and the financial system
 *   faced systemic risk from hidden interconnections. The mandatrophy is
 *   resolved: Repo 105 is unambiguously a Snare from the victims' perspective
 *   (trapped investors and counterparties). From management's perspective it
 *   appeared as a Rope (coordination mechanism for balance sheet management),
 *   masking the underlying extraction. The key question is whether the
 *   extraction came from Lehman's deception or from the regulatory
 *   framework's failure to align accounting with economic substance.
 *
 * KEY AGENTS:
 *   - Lehman Management & Treasury: Primary beneficiary (institutional/arbitrage) — benefited from quarter-end leverage reduction, rating maintenance, funding access, and executive compensation tied to reported metrics
 *   - Lehman Equity Investors: Primary victim (powerless/trapped) — retail and institutional shareholders held equity during concealment period; could not exit without realizing losses
 *   - Lehman Counterparties (repo participants): Secondary victim (powerless/trapped) — creditors in repo markets faced hidden leverage and liquidity risk; collateral pledged multiple times over
 *   - Lehman Debt Investors: Secondary victim (moderate/constrained) — bondholders and debt investors faced inflated credit quality; some ability to exit but market illiquidity was severe
 *   - Ernst & Young (Auditors): Trapped institutional actor (institutional/constrained) — faced incentives to avoid antagonizing major client; technical FAS 140 compliance masked substance
 *   - Rating Agencies: Institutional observer (institutional/constrained) — access to Lehman's internal data but analytical capacity limited by complexity; constrained by modeling assumptions
 *   - Financial System: Systemic victim (organized/constrained) — aggregate system faced contagion risk from hidden interconnections and counterparty uncertainty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lehman_repo_105, 0.68).
domain_priors:suppression_score(lehman_repo_105, 0.78).
domain_priors:theater_ratio(lehman_repo_105, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lehman_repo_105, extractiveness, 0.68).
narrative_ontology:constraint_metric(lehman_repo_105, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(lehman_repo_105, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lehman_repo_105, snare).
narrative_ontology:human_readable(lehman_repo_105, "Lehman Brothers Repo 105 Accounting Maneuver").
narrative_ontology:topic_domain(lehman_repo_105, "economic/financial_regulation").

% --- Structural relationships ---
narrative_ontology:constraint_victim(lehman_repo_105, lehman_counterparties).
narrative_ontology:constraint_victim(lehman_repo_105, lehman_equity_investors).
narrative_ontology:constraint_victim(lehman_repo_105, lehman_debt_investors).
narrative_ontology:constraint_victim(lehman_repo_105, financial_system_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LEHMAN EQUITY INVESTORS (SNARE) — Retail and institutional shareholders held equity while leverage was concealed via Repo 105. They could not exit without realizing losses; the constraint trapped them through information asymmetry. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.73.
constraint_indexing:constraint_classification(lehman_repo_105, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LEHMAN COUNTERPARTIES / CREDITORS (SNARE) — Counterparties extending credit in repo markets faced hidden leverage and liquidity risk. They could not detect or exit the relationship due to opacity; collateral values collapsed when Lehman failed. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.72.
constraint_indexing:constraint_classification(lehman_repo_105, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LEHMAN DEBT INVESTORS (SNARE) — Corporate and government bond buyers faced inflated credit quality due to reported leverage ratios. They could theoretically exit but faced market illiquidity and switching costs; information asymmetry was severe. d≈0.78, f(d)≈1.20, σ=1.2 → χ≈0.62.
constraint_indexing:constraint_classification(lehman_repo_105, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: FINANCIAL SYSTEM STABILITY (SNARE) — The aggregate financial system faced systemic risk from hidden leverage cascading through repo markets and counterparty networks. Organized actors (regulators, central banks) were constrained by incomplete information and interconnection. d≈0.68, f(d)≈1.02, σ=1.2 → χ≈0.55.
constraint_indexing:constraint_classification(lehman_repo_105, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEHMAN MANAGEMENT / TREASURY (TANGLED ROPE) — Lehman's finance team benefited from quarter-end accounting relief: Repo 105 allowed temporary leverage reduction for reporting, enabling executive compensation, credit rating maintenance, and access to funding. The constraint provided coordination function (managing end-of-period balance sheets) alongside extraction (deception). d≈0.15, f(d)≈0.05, σ=1.0 → χ≈0.03. Negative extraction: net beneficiary.
constraint_indexing:constraint_classification(lehman_repo_105, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ACCOUNTING STANDARDS / AUDITOR SYSTEM (SNARE) — Auditors (Ernst & Young) faced structural incentives to avoid antagonizing a major client. The constraint (Repo 105's accounting treatment under FAS 140) trapped the auditor through client dependence, regulatory complexity, and limited enforceability. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.55. High theater_ratio (0.85) reflects that audits were performative; technical compliance (FAS 140 literalism) masked economic substance.
constraint_indexing:constraint_classification(lehman_repo_105, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (PITON) — From a regulatory/civilizational perspective, Repo 105 exploited a technical loophole (FAS 140's sale treatment of repos with repurchase obligations) in what was supposed to be a substance-over-form framework. The constraint persists via institutional inertia in accounting standards that prioritize literal compliance over economic reality. theater_ratio=0.85 indicates primarily performative compliance. The regulatory framework sees its own process as degraded.
constraint_indexing:constraint_classification(lehman_repo_105, piton,
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
 *   Extractiveness (0.68): High. Lehman captured substantial value through leverage reduction that concealed financial deterioration. The extraction was not maximal (0.68 rather than 0.95) because the maneuver had explicit costs (repo spreads, operational complexity, funding to repurchase) and benefited only Lehman, not a broader coalition. Suppression (0.78): Very high. Repo 105 worked entirely through information asymmetry. Lehman actively concealed the maneuver by timing repos to quarter-end, using offshore counterparties (Deutsche Bank's Dublin office), and exploiting the technical complexity of FAS 140. Counterparties, investors, and auditors could not easily detect the practice; it remained hidden until the Lehman bankruptcy examiner's 2010 report. Theater ratio (0.85): Very high and increasing. Audits were performative: Ernst & Young reviewed the FAS 140 compliance without assessing economic substance. Lehman's quarterly earnings calls emphasized reported leverage ratios that had been artificially reduced. Credit rating reviews relied on Lehman's published financial statements without detecting the off-balance-sheet maneuver. By 2008, the constraint had become entirely theatrical—the true leverage was 30-40x while reported leverage appeared 15-20x. Claimed type (snare): Mandatory. High extractiveness (0.68), high suppression (0.78), and multiple victims with no exit create the snare signature. The chi threshold (χ ≥ 0.66) is met: χ = 0.68 × f(0.95) × σ(global) = 0.68 × 1.42 × 1.2 ≈ 1.16 for equity investors (d≈0.95), and χ = 0.68 × f(0.92) × σ(global) ≈ 1.12 for creditors.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. Lehman management viewed Repo 105 as a beneficial coordination mechanism (Rope) for managing balance sheet liquidity at quarter-end—a technical solution to a legitimate accounting problem. Equity investors, debt holders, and counterparties viewed it as pure extraction (Snare)—a mechanism to deceive them about Lehman's true financial condition. Ernst & Young's perspective was trapped between technical compliance (FAS 140 permit) and substance-over-form principles—they classified it as a Snare on the auditor system itself (unable to exit without antagonizing a major client or openly accusing management of fraud). Rating agencies should have classified it as Snare but had insufficient detection capability. The analytical observer (civilizational perspective) sees a Piton: the accounting framework (FAS 140) has become degraded and performative, allowing literal compliance to diverge from economic substance. The gap reveals that 'coordination' and 'extraction' are not intrinsic to the maneuver but depend entirely on informational position. Lehman sees coordination; victims see extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Lehman equity investors: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. They held shares during the entire concealment period and could not exit without realizing losses. Information asymmetry prevented them from detecting Lehman's true condition. Lehman creditors & counterparties: Victim + trapped → d≈0.92, f(d)≈1.38. Nearly maximum extraction. They extended credit based on false leverage ratios; collateral pledged multiple times; no ability to detect or exit when leverage was discovered. Lehman debt investors: Victim + constrained → d≈0.78, f(d)≈1.20. High extraction with some exit option. Bondholders could theoretically sell debt, but secondary market illiquidity and credit rating reliance made exit costly. Lehman management: Beneficiary + arbitrage → d≈0.15, f(d)≈0.05. Net beneficiary. Captured quarter-end accounting relief, maintained ratings, accessed cheaper funding. Negative effective extraction (χ ≈ 0.03). Auditors: Victim + constrained → d≈0.72, f(d)≈1.15. High extraction on the auditor system itself. Client dependence and technical complexity trapped auditors in literal compliance. Financial system: Victim + constrained → d≈0.68, f(d)≈1.02. Moderate-high extraction on systemic stability. Regulators and central banks could theoretically intervene but faced incomplete information and interconnection complexity.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: Repo 105 is unambiguously a Snare. The mandatrophy (misclassification as coordination when it is extraction) is definitively resolved through the empirical outcome: when Lehman collapsed, the coordination benefits (quarter-end balance sheet relief) evaporated instantly while the extraction harm (investor and creditor losses) persisted catastrophically. This reveals that the apparent 'coordination' was purely theatrical—the balance sheet relief was temporary and costless to Lehman (reversed post-quarter), while the deception was systematic and costly to victims (permanent information loss). Lehman management's Rope perspective is a false consciousness—what they experienced as legitimate quarter-end management was actually a sophisticated deception scheme. The snare classification is robust across all perspectives except the beneficiary's. Ernst & Young's perspective (Snare on auditor independence) and the regulatory framework's perspective (Piton—degraded accounting standards) do not contradict the snare classification; they identify secondary constraints that enabled the snare to function. The mandatrophy is resolved not through theoretical argument but through structural observation: extractiveness > 0.65, suppression > 0.75, multiple victims with no exit, and empirically verified collapse of coordination benefits upon constraint failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fas_140_substance_intent,
    'Did FAS 140 intend to permit repos treated as true sales, or was Repo 105 an unforeseen technical loophole in the standard?',
    'Analysis of FASB deliberations, comment letters, and subsequent rule amendments (ASC 860); comparison of stated principles vs. literal language in FAS 140',
    'If intent: constraint is systematic (Rope or Tangled Rope). If loophole: constraint is pure extraction (Snare) by design flaw. Affects whether blame accrues to Lehman or to standard-setters.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fas_140_substance_intent, empirical, 'Whether FAS 140 intentionally permitted Repo 105 treatment').

omega_variable(
    rating_agency_detection_capability,
    'Could Moody''s, S&P, or Fitch have detected Repo 105 from public disclosures or access to Lehman''s credit files?',
    'Forensic review of rating agency workpapers, access logs, and due diligence procedures; comparison with post-failure Lehman bankruptcy disclosures',
    'If detectable: rating agencies were negligent (constraint is Snare on their judgment). If not detectable: information asymmetry was inherent (constraint is more structural Snare on investors). Changes accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rating_agency_detection_capability, empirical, 'Whether rating agencies could detect Repo 105 from available information').

omega_variable(
    repo_market_knowledge_prevalence,
    'How widespread was knowledge of Repo 105 usage among institutional investors, counterparties, and regulators before the March 2008 near-failure and September 2008 bankruptcy?',
    'Testimony from counterparties, institutional investors, and regulators; historical news coverage; internal emails from other financial institutions',
    'If widespread: constraint is coordination failure masquerading as secrecy (Tangled Rope). If unknown: constraint is pure information extraction (Snare). Affects whether Lehman''s advantage came from secrecy or from others'' negligence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(repo_market_knowledge_prevalence, empirical, 'Prevalence of knowledge about Repo 105 before failure').

omega_variable(
    auditor_regulatory_independence,
    'Was Ernst & Young''s failure to flag Repo 105 due to technical FAS 140 compliance (captured by literal standard) or to client capture and reputational risk?',
    'Analysis of EY''s FAS 140 guidance vs. peer practices; comparison with audits at other financial institutions using similar techniques; investigation of EY''s engagement incentives and client relationships',
    'If technical compliance: constraint is system-wide (Rope with low per-auditor responsibility). If client capture: constraint is Snare on auditor independence. Changes remediation strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(auditor_regulatory_independence, empirical, 'Whether auditor failure was technical compliance or client capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lehman_repo_105, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(repo105_tr_t0, lehman_repo_105, theater_ratio, 0, 0.55).
narrative_ontology:measurement(repo105_tr_t2, lehman_repo_105, theater_ratio, 2, 0.7).
narrative_ontology:measurement(repo105_tr_t4, lehman_repo_105, theater_ratio, 4, 0.85).

% Extraction over time
narrative_ontology:measurement(repo105_be_t0, lehman_repo_105, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(repo105_be_t2, lehman_repo_105, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(repo105_be_t4, lehman_repo_105, base_extractiveness, 4, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lehman_repo_105, information_standard).
narrative_ontology:affects_constraint(lehman_repo_105, financial_leverage_opacity).
narrative_ontology:affects_constraint(lehman_repo_105, accounting_standard_arbitrage).
narrative_ontology:affects_constraint(lehman_repo_105, credit_rating_model_failure).

% DUAL FORMULATION NOTE:
% Repo 105 is downstream of broader accounting standard design (FAS 140 loophole) and credit rating methodology (reliance on reported financials). It is upstream of systemic risk (hidden interconnection in repo markets). The constraint family includes: (1) FAS 140 technical design (ε≈0.12, Mountain-like rigidity of literal standard), (2) Repo 105 exploitation (ε≈0.68, Snare), and (3) Credit rating cascade failure (ε≈0.55, Tangled Rope). Repo 105 cannot be understood in isolation from the accounting standard that enabled it or the rating agency models that amplified it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lehman_repo_105, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
