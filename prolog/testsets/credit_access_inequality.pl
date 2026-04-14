% ============================================================================
% CONSTRAINT STORY: credit_access_inequality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credit_access_inequality, []).

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
 *   constraint_id: credit_access_inequality
 *   human_readable: Credit Access Inequality
 *   domain: economic/financial
 *
 * SUMMARY:
 *   Credit access inequality represents a structural constraint in which
 *   financial institutions coordinate capital allocation to borrowers while
 *   simultaneously extracting disproportionate value from those least able to
 *   bear the cost. The constraint operates through information asymmetry
 *   (lenders claim inability to assess creditworthiness of excluded
 *   borrowers), discriminatory proxies (credit scores that reproduce
 *   historical inequality), and wealth-based collateral requirements that
 *   concentrate credit access among those already privileged. The same
 *   institutional mechanism — credit risk assessment — appears as an
 *   immutable information problem (mountain), a coordination mechanism for
 *   capital allocation (rope), a system producing mixed coordination and
 *   extraction (tangled rope), a temporary problem being solved by fintech
 *   and alternative models (scaffold), and a degraded ritual maintained
 *   through institutional inertia (piton), depending on the observer's
 *   structural position within the credit system. The extractiveness has
 *   increased from 0.42 to 0.58 over the measurement interval, driven by fee
 *   proliferation, predatory alternative-lending growth, and rising denial
 *   rates for marginalized communities despite formal loosening of credit
 *   standards. The theater ratio remains moderate (0.48) because some
 *   functional risk assessment exists alongside substantial performative
 *   documentation and exclusionary screening practices.
 *
 * KEY AGENTS:
 *   - Low-Income Borrowers: Primary victims (powerless/trapped) — lack credit history, collateral, and information; face predatory rates and exclusion from formal credit; no exit options
 *   - Marginalized Communities: Secondary victims (moderate/constrained) — suffer from historical discrimination, wealth gaps, and discriminatory underwriting; constrained exit to informal or predatory lending
 *   - Financial Institutions: Primary beneficiaries (institutional/arbitrage) — capture interest margin, fees, and risk premium from entire portfolio; arbitrage exit to other markets or products
 *   - High-Creditworthiness Borrowers: Secondary beneficiaries (powerful/mobile) — access favorable rates and terms; mobile exit to alternative lenders
 *   - Credit Access Reformers: Organized agents (organized/constrained) — regulators, nonprofits, fintech advocates building alternative pathways; constrained by need to maintain financial stability
 *   - Regulatory Bodies: Institutional constraint-holders (institutional/constrained) — must coordinate financial stability while preventing discrimination; enforcement tensions limit exit
 *   - Legacy Credit Scoring Systems: Institutional persistence (institutional/arbitrage) — FICO, Equifax, TransUnion maintain market position through path dependence and network effects
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent design choices (specific credit metrics, collateral requirements) as information-theoretic necessities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credit_access_inequality, 0.58).
domain_priors:suppression_score(credit_access_inequality, 0.65).
domain_priors:theater_ratio(credit_access_inequality, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credit_access_inequality, extractiveness, 0.58).
narrative_ontology:constraint_metric(credit_access_inequality, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(credit_access_inequality, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credit_access_inequality, tangled_rope).
narrative_ontology:human_readable(credit_access_inequality, "Credit Access Inequality").
narrative_ontology:topic_domain(credit_access_inequality, "economic/financial").

domain_priors:requires_active_enforcement(credit_access_inequality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credit_access_inequality, financial_institutions).
narrative_ontology:constraint_beneficiary(credit_access_inequality, high_credit_score_borrowers).
narrative_ontology:constraint_victim(credit_access_inequality, low_income_borrowers).
narrative_ontology:constraint_victim(credit_access_inequality, marginalized_communities).
narrative_ontology:constraint_victim(credit_access_inequality, first_time_borrowers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME BORROWERS (SNARE) — Trapped by structural barriers: no credit history, no collateral, no alternative sources of capital. The constraint extracts predatory interest rates, fees, and collateral demands. Exit is impossible; these agents bear maximum extraction cost. The system maintains suppression through information asymmetry, legal complexity, and alternative predatory channels (payday lending, loan sharks).
constraint_indexing:constraint_classification(credit_access_inequality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITIES (TANGLED ROPE) — Face historical redlining, persistent discrimination, and wealth gaps that compound access barriers. The credit system coordinates capital allocation (genuine coordination function) but asymmetrically extracts from these communities through higher rates, stricter terms, and discriminatory underwriting. Constrained exit: migration to alternative lenders or informal credit networks incurs significant costs and social risks.
constraint_indexing:constraint_classification(credit_access_inequality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL INSTITUTIONS (ROPE) — Primary beneficiaries. Experience the constraint as pure coordination: risk assessment mechanisms that efficiently allocate credit to low-risk borrowers. Arbitrage exit: institutions can move capital to other markets, other products, or other geographies. The constraint functions as a coordination mechanism for capital distribution; extraction runs toward these agents.
constraint_indexing:constraint_classification(credit_access_inequality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-CREDIT-SCORE BORROWERS (TANGLED ROPE) — Benefit from preferential access and favorable rates (coordination function: risk-based pricing incentivizes creditworthy behavior). Also extract value through favorable terms that subsidize credit institution operations. Mobile exit: can access alternative credit sources (peer-to-peer lending, family, savings). Moderate extraction experienced because they have both benefit and mobility.
constraint_indexing:constraint_classification(credit_access_inequality, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CREDIT ACCESS REFORMERS (SCAFFOLD) — Organized agents (regulators, nonprofits, fintech advocates) see credit access inequality as a temporary coordination failure with a sunset: alternative credit assessment models (alternative data, machine learning, community-based lending), financial inclusion initiatives, and regulatory reform are building pathways to lower the extractiveness over time. Theater ratio is moderate because some functional verification (credit scoring) exists alongside performative documentation.
constraint_indexing:constraint_classification(credit_access_inequality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY BODIES (TANGLED ROPE) — Caught between coordinating financial stability (genuine coordination function) and preventing predatory extraction (asymmetric extraction prevention). Constrained exit: cannot fully dismantle credit scoring without destabilizing capital allocation, yet current systems produce discriminatory outcomes. Requires active enforcement to maintain the system; extraction persists because regulatory constraints limit their ability to restructure underlying mechanisms.
constraint_indexing:constraint_classification(credit_access_inequality, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: LEGACY CREDIT SCORING SYSTEMS (PITON) — Credit score methodologies (FICO, Equifax, TransUnion) persist through institutional inertia despite well-documented limitations, discrimination risk, and better alternatives available. The theater is substantial: credit scores provide the appearance of objective, data-driven risk assessment, yet they reproduce historical discrimination and exclude valid lending candidates. Maintained because alternatives haven't fully replaced them and institutions are path-dependent.
constraint_indexing:constraint_classification(credit_access_inequality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / INFORMATION IMPOSSIBILITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, some credit access inequality is inherent to asymmetric information: lenders cannot perfectly predict default, and must use proxies. This perspective sees the constraint as a natural law of finance — you cannot allocate capital to unknown borrowers without some information asymmetry creating barriers. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that the informationally-impossible framing naturalizes contingent institutional arrangements (specific credit metrics, discriminatory proxies, wealth-based collateral requirements) that could be otherwise designed.
constraint_indexing:constraint_classification(credit_access_inequality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credit_access_inequality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credit_access_inequality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credit_access_inequality, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credit_access_inequality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credit_access_inequality, TR),
    TR >= 0.70.

:- end_tests(credit_access_inequality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Financial institutions capture significant extraction from low-income borrowers through interest margins (often 8-15 percentage points higher than prime rates), origination fees (2-5%), ongoing maintenance fees, and prepayment penalties. However, the extraction is not maximal (≤0.66 threshold for snare) because some institutions offer genuine credit products with lower extraction, and the overall system coordinates some legitimate capital allocation. The rising trajectory (0.42→0.58) reflects fee proliferation and predatory alternative-lending growth in the formal market. Suppression (0.65): High. Significant barriers include lack of credit history, no collateral, information asymmetry exploitation, discriminatory underwriting, regulatory complexity, and alternative predatory channels that trap borrowers in cycles. Suppression is not total (≤1.0) because some escape routes exist (credit unions, community banks, fintech) and formal credit access is theoretically available. Theater ratio (0.48): Moderate. Credit scoring provides appearance of objective, data-driven risk assessment, yet reproduces historical discrimination and excludes viable borrowers. Regulatory compliance documentation (loan applications, disclosure forms) is substantial. However, credit scoring methodologies produce actual signal about default likelihood, so theater is not dominant (≥0.70). The theater ratio's relative stability reflects that while credit scoring methodology hasn't fundamentally changed, public awareness of its limitations and discrimination risk has grown, modestly increasing the performative/functional distinction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full perspectival range across its observation sites. Low-income borrowers see a snare: pure extraction with no exit and no coordination benefit — the credit system does not coordinate their capital needs, it excludes them or traps them in predatory cycles. Marginalized communities see tangled rope: some coordination (access to capital for business, home, education) mixed with asymmetric extraction (discrimination-inflated rates, collateral demands tied to historical wealth gaps). Financial institutions see rope: pure coordination mechanism that efficiently allocates capital to low-risk borrowers and facilitates economic activity. High-creditworthiness borrowers see tangled rope: coordination (access to capital at favorable rates) with minor extraction (cross-subsidization of riskier borrowers). Reformers see scaffold: a temporary coordination problem with a sunset — alternative credit models and fintech are building pathways to expand access at lower cost. Legacy credit scoring systems see piton: their own process is recognized as degraded (discriminatory, inefficient) but maintained through inertia. The analytical observer at civilizational scope risks seeing a mountain: information asymmetry makes credit rationing inherent and unavoidable — but the structural data reveals this as false summit, naturalizing specific institutional choices (FICO scores, collateral-based lending, centralized credit bureaus) that could be otherwise designed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from each agent's structural position relative to credit flow. Low-income borrowers are victims with trapped exit options: d ≈ 0.95, producing high f(d) ≈ 1.42, resulting in high experienced extraction chi. Financial institutions are beneficiaries with arbitrage exit: d ≈ 0.05, producing f(d) ≈ -0.12, resulting in negative experienced extraction (they net extract value). Marginalized communities are victims with constrained (not trapped) exit options: d ≈ 0.75, producing f(d) ≈ 1.10, resulting in moderate-high experienced extraction. High-creditworthiness borrowers are beneficiaries with mobile exit: d ≈ 0.35, producing f(d) ≈ 0.20, resulting in low or negligible experienced extraction. Reformers are organized agents with constrained exit: d ≈ 0.60, producing f(d) ≈ 0.85, resulting in moderate experienced extraction (they bear costs of reform effort without full structural change capacity). Regulatory bodies are institutional constraint-holders with constrained exit: d ≈ 0.55, producing f(d) ≈ 0.75, resulting in moderate experienced extraction. The piton perspective derives from theater ratio (0.48) not reaching the piton threshold (≥0.70) in the main calculation, but appears here because legacy credit scoring systems specifically have been maintained through institutional inertia despite better alternatives — the perspective represents a real institutional phenomenon even if not dominant in the aggregated metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   Credit access inequality resolves mandatrophy by instantiating how coordination and extraction coexist within a single constraint. The credit system genuinely coordinates capital allocation (a coordination function: it routes capital to productive uses). It also genuinely extracts value asymmetrically (high rates and fees on low-income borrowers who have no alternatives). These are not two separate constraints or two phases of the same constraint — they are simultaneous properties of the tangled rope. The snare perspective from the trapped borrower is not a 'misperception' of a rope; it is the borrower's genuine structural experience of pure extraction. The rope perspective from the financial institution is not false; it is their genuine structural experience of a coordination mechanism. Mandatrophy is resolved by recognizing that 'is this coordination or extraction?' has different answers depending on where you stand. The presheaf of perspectives across the indexical sites reproduces the full structure of the constraint as a hybrid that cannot be simplified to a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_discrimination_internalization,
    'To what extent is suppression structural (external barriers) versus internalized (low-income borrowers have lost faith in credit access, don''t apply)?',
    'Application rate analysis: comparison of denied applicants vs non-applicants; psychological studies on credit access beliefs; tracking of application behavior changes post-reform',
    'If internalized suppression dominates: constraint''s effective suppression is higher than structural measures suggest; post-reform barriers removal alone won''t restore credit access without belief shifts. If structural suppression dominates: removal of objective barriers (fees, rates, collateral) will quickly restore access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_discrimination_internalization, empirical, 'Extent of internalized vs structural suppression').

omega_variable(
    alternative_credit_model_effectiveness,
    'Do alternative credit assessment models (alternative data, community lending, peer-to-peer) actually produce comparable default rates to traditional credit scoring or merely shift risk?',
    'Longitudinal default rate comparison: alternative-model borrowers vs traditional-credit borrowers, controlling for actual default vs perceived risk; tracking of whether ''excluded'' borrowers actually default less than expected',
    'If alternative models work: scaffold perspective confirmed — financial inclusion sunset is real and risk-based extraction is justified. If alternative models fail: extractive credit institutions are correct that barriers are risk-based, and scaffold is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credit_model_effectiveness, empirical, 'Whether alternative credit assessment models effectively reduce risk while expanding access').

omega_variable(
    discrimination_mechanism_detection,
    'Is credit access inequality driven by statistical discrimination (rational use of correlated proxies) or taste-based discrimination (irrational prejudice)?',
    'Audit studies: matched borrower pairs (identical credit profiles, different demographics) applying for credit; analysis of underwriting documentation for proxy use patterns; comparison of default rates by demographic group controlling for observables',
    'If statistical discrimination: some extraction is justified as risk-based pricing; reform requires alternative data collection. If taste discrimination: extraction is pure rent-seeking on protected characteristics; reform requires enforcement and remediation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discrimination_mechanism_detection, empirical, 'Whether credit inequality reflects statistical or taste-based discrimination').

omega_variable(
    fintech_disruption_timeline,
    'Will fintech and alternative lending platforms substantially displace traditional credit scoring within one generation?',
    'Market share tracking of fintech vs traditional lenders; adoption curves for alternative credit models; regulatory adaptation timelines; institutional capacity to absorb new methodologies',
    'If fintech disrupts within 15 years: scaffold sunset is real, system transformation is structural. If disruption stalls: traditional institutions maintain extractive power through network effects and regulatory barriers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fintech_disruption_timeline, empirical, 'Whether fintech will displace traditional credit scoring systems').

omega_variable(
    wealth_collateral_necessity,
    'Is collateral-based lending a functional necessity for risk mitigation or a wealth-extraction mechanism that excludes otherwise-viable borrowers?',
    'Comparison of unsecured lending default rates vs collateral-secured rates for matched borrower cohorts; analysis of collateral haircuts and recovery rates; tracking of borrower outcomes post-default',
    'If collateral is functionally necessary: some barriers are coordination costs. If collateral is extraction: wealth-based access becomes pure rent-seeking on initial inequality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wealth_collateral_necessity, empirical, 'Whether collateral requirements are functionally necessary or extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credit_access_inequality, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(credit_tr_t0, credit_access_inequality, theater_ratio, 0, 0.38).
narrative_ontology:measurement(credit_tr_t10, credit_access_inequality, theater_ratio, 10, 0.42).
narrative_ontology:measurement(credit_tr_t20, credit_access_inequality, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(credit_be_t0, credit_access_inequality, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(credit_be_t10, credit_access_inequality, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(credit_be_t20, credit_access_inequality, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credit_access_inequality, resource_allocation).
narrative_ontology:affects_constraint(credit_access_inequality, wealth_inequality_accumulation).
narrative_ontology:affects_constraint(credit_access_inequality, predatory_lending_cycles).
narrative_ontology:affects_constraint(credit_access_inequality, financial_exclusion_feedback).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(credit_access_inequality, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
