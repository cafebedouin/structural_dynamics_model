% ============================================================================
% CONSTRAINT STORY: credit_risk_assessment_bias
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credit_risk_assessment_bias, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: credit_risk_assessment_bias
 *   human_readable: Credit Risk Assessment Bias in Lending Markets
 *   domain: financial/economic
 *
 * SUMMARY:
 *   Credit risk assessment bias represents a structural constraint where the
 *   legitimate technical coordination problem (lenders must distinguish
 *   borrower risk to allocate capital efficiently) overlaps with an
 *   extractive mechanism (systematic overestimation of risk for marginalized
 *   populations, leading to denial or predatory pricing). The constraint
 *   exhibits all six DR types across perspectives because the same
 *   statistical artifact — correlation between protected characteristics and
 *   measured default risk — can be interpreted as legitimate risk assessment
 *   (from the lender's perspective), coordination with hidden extraction
 *   (from the borrower's perspective), a temporary technical problem with
 *   regulatory solutions (from the reform perspective), a degraded scoring
 *   ritual (from the institutional inertia perspective), pure exclusion (from
 *   the powerless perspective), or an immutable feature of information
 *   asymmetry (from the false mountain perspective). The theater ratio (0.68)
 *   reflects that credit scoring is substantially performative: FICO scores
 *   legitimize lending decisions through statistical authority while their
 *   actual predictive validity declines as lending markets change and
 *   alternative assessment methods emerge. The extractiveness trajectory
 *   (0.35 → 0.58 over 20 years) shows accumulation of extraction as
 *   algorithmic bias replaces human judgment, concentrating power in scoring
 *   bureaus and reducing transparency.
 *
 * KEY AGENTS:
 *   - Systematically Rejected Borrowers: Primary victim (powerless/trapped) — excluded from credit markets entirely; no access alternative; maximum extraction
 *   - Subprime Borrower Population: Secondary victim (moderate/constrained) — access to credit at higher cost; genuine coordination benefit alongside asymmetric extraction
 *   - Financial Institutions: Primary beneficiary (institutional/arbitrage) — capture interest rate spreads and risk premia; can arbitrage between regulatory regimes and borrower populations
 *   - Privileged Borrower Groups: Secondary beneficiary (powerful/arbitrage) — access to favorable terms through low-bias assessment; benefit from extraction of other groups
 *   - Regulatory Reform Coalition: Organized agents (organized/constrained) — consumer protection agencies, fair lending advocates, algorithmic accountability coalitions building alternative frameworks
 *   - Credit Scoring Bureaus: Institutional actor (institutional/arbitrage) — maintain performative scoring systems; benefit from privatized risk assessment authority
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent assessment choices (credit scoring methods, variable selection, weighting schemes) as immutable information requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credit_risk_assessment_bias, 0.58).
domain_priors:suppression_score(credit_risk_assessment_bias, 0.65).
domain_priors:theater_ratio(credit_risk_assessment_bias, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credit_risk_assessment_bias, extractiveness, 0.58).
narrative_ontology:constraint_metric(credit_risk_assessment_bias, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(credit_risk_assessment_bias, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credit_risk_assessment_bias, tangled_rope).
narrative_ontology:human_readable(credit_risk_assessment_bias, "Credit Risk Assessment Bias in Lending Markets").
narrative_ontology:topic_domain(credit_risk_assessment_bias, "financial/economic").

domain_priors:requires_active_enforcement(credit_risk_assessment_bias).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credit_risk_assessment_bias, financial_institutions).
narrative_ontology:constraint_beneficiary(credit_risk_assessment_bias, privileged_borrower_groups).
narrative_ontology:constraint_victim(credit_risk_assessment_bias, marginalized_borrower_populations).
narrative_ontology:constraint_victim(credit_risk_assessment_bias, credit_access_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYSTEMATICALLY REJECTED BORROWER (SNARE) — Trapped by algorithmic exclusion and credit history feedback loops. No exit from the constraint's extraction: higher interest rates, denial of access, or subprime terms lock out borrowers lacking collateral or credit history. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(credit_risk_assessment_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUBPRIME BORROWER POPULATION (TANGLED ROPE) — Constrained by limited alternatives and higher borrowing costs, but the lending market also provides essential credit access for working-class borrowers excluded from prime markets. Genuine coordination function (capital allocation) alongside asymmetric extraction (risk premia and predatory terms). Significant suppression via high-cost debt dependency.
constraint_indexing:constraint_classification(credit_risk_assessment_bias, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL INSTITUTION (ROPE) — Experiences the constraint as legitimate risk coordination: assessing borrower risk enables efficient capital allocation. Low effective extraction from the institution's perspective; the constraint is experienced as a functional necessity. Can arbitrage between markets and regulatory regimes.
constraint_indexing:constraint_classification(credit_risk_assessment_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY REFORM COALITION (SCAFFOLD) — Organized advocacy groups, consumer protection agencies, and fair lending enforcement see credit bias as a temporary institutional failure with a sunset: anti-discrimination rules (Fair Credit Reporting Act amendments, algorithmic accountability mandates, credit counseling programs) are building alternative pathways. Low effective extraction for organized reformers who can influence policy and see an exit path.
constraint_indexing:constraint_classification(credit_risk_assessment_bias, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CREDIT SCORING APPARATUS (PITON) — The FICO score and alternative credit models persist through institutional inertia despite low functional validity for predicting actual default risk, especially for marginalized populations. The scoring ritual is substantially performative: it legitimizes lending decisions through a veneer of statistical objectivity while perpetuating historical bias. Theater ratio high; actual predictive power degraded.
constraint_indexing:constraint_classification(credit_risk_assessment_bias, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INFORMATION ASYMMETRY VIEW (MOUNTAIN) — From a civilizational/global perspective, some form of credit risk assessment is an immutable requirement of lending: lenders must distinguish between borrowers with different repayment probabilities. Information asymmetry is inherent to lending markets. However, the structural data contradicts a pure mountain classification — the extraction mechanism depends on specific implementation choices (which variables to include, how to weight them, who gets access to alternative assessment methods). The 'immutable' framing naturalizes contingent institutional choices.
constraint_indexing:constraint_classification(credit_risk_assessment_bias, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credit_risk_assessment_bias_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credit_risk_assessment_bias, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credit_risk_assessment_bias, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credit_risk_assessment_bias, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credit_risk_assessment_bias, TR),
    TR >= 0.70.

:- end_tests(credit_risk_assessment_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint reflects genuine lender needs for risk differentiation (legitimate coordination component) but the measurement of risk systematically overestimates default probability for marginalized populations due to historical bias, proxy variable contamination, and algorithmic drift. The extractiveness increased from 0.35 to 0.58 over 20 years as algorithmic models replaced human judgment, concentrating power in opaque scoring systems and reducing transparency. Suppression (0.65): High. Structural barriers to challenging adverse credit decisions include information asymmetry (borrowers don't know scoring models), regulatory opacity (credit bureaus operate under limited disclosure rules), and resource barriers (challenging denials requires legal/financial resources most excluded borrowers lack). Internalized suppression component exists (identity-locked beliefs about creditworthiness, distrust of lending institutions). Theater ratio (0.68): High. Credit scoring is substantially performative: FICO scores provide a veneer of scientific objectivity legitimizing lending decisions, but their actual predictive validity declines with algorithmic drift and model degradation. The ritual persists through institutional inertia and regulatory compliance theater rather than functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (financial institution) experiences Rope coordination. The victim (powerless borrower) experiences Snare extraction. The subprime borrower experiences Tangled Rope (mixed access and exploitation). The reform coalition experiences Scaffold (temporary problem with policy solutions). The credit scoring system experiences Piton (performative ritual). The analytical observer risks Mountain (immutable information asymmetry) but the structural data reveals this as false summit. The perspectival gaps reveal that the same mechanism (risk differentiation via credit scoring) is experienced as coordination by beneficiaries and extraction by victims depending on their exit options and power level.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position in the extraction flow. Financial institutions benefit from bias (low d → negative chi from institution perspective); they capture interest rate spreads and risk premia. Marginalized borrowers bear extraction (high d → high chi); they face denial or subprime pricing. Subprime borrowers experience mixed effects (moderate d → moderate chi): they access credit they couldn't get under strict prime standards, but at rates that extract long-term wealth. Reform coalitions have organized exit capacity (constrained but not trapped); they perceive high extraction but have institutional levers to address it. The rejection of a 'powerless' agent in a large market of excluded borrowers may be revised under Dynamic Coalition logic: if the number of rejected borrowers exceeds critical mass threshold, their power atom could be upgraded to 'organized', shifting the snare classification and enabling coalition pressure. The piton classification derives from the theater gate (theater_ratio ≥ 0.70 would trigger piton; at 0.68, it's close).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED THROUGH PERSPECTIVAL DECOMPOSITION: The constraint avoids mandatrophy by recognizing that 'credit risk assessment' covers both legitimate coordination (distinguishing high-risk from low-risk borrowers improves capital allocation) and extractive bias (systematically overestimating risk for marginalized groups enables discrimination). The constraint story does not claim credit assessment is purely extractive (Snare) — lenders genuinely face uncertainty. Nor does it claim assessment is pure coordination (Rope) — the measurement systematically disfavors marginalized populations. The Tangled Rope classification resolves the mandatrophy: the constraint has BOTH a genuine coordination function (capital allocation to creditworthy borrowers) AND asymmetric extraction (bias that harms marginalized borrowers). The perspectival gap between the lender (Rope) and borrower (Snare/Tangled Rope) reveals this structure: the same mechanism is coordination from the beneficiary's view and extraction from the victim's view. The mandatrophy is resolved by making both perspectives visible rather than privileging one as the 'true' classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_bias_causality,
    'Is measured credit risk correlation with protected characteristics a causal reflection of actual repayment risk, a proxy for excluded variables, or a direct discrimination mechanism?',
    'Causal inference analysis controlling for structural factors (income stability, employment history, neighborhood investment patterns); comparison of bias signature before and after removing protected class indicators',
    'If causal: risk assessment is legitimate coordination with difficult distributional questions. If proxy: bias is correctable through better data. If discrimination: constraint is primarily extractive regardless of statistical justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_bias_causality, empirical, 'Whether bias in credit assessment reflects actual risk, unmeasured confounders, or pure discrimination').

omega_variable(
    alternative_assessment_viability,
    'Do alternative assessment methods (non-traditional credit history, cash flow analysis, community lending) achieve comparable accuracy with lower bias, or do they introduce different blind spots?',
    'Large-scale comparison of default prediction accuracy and distributional fairness metrics across assessment methods; longitudinal tracking of borrower outcomes across lending modalities',
    'If alternatives work: the constraint is contingent institutional choice (Scaffold/Tangled Rope from reform perspective strengthened). If alternatives fail: some form of bias is inescapable technical reality (Mountain claim gains credibility).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_assessment_viability, empirical, 'Whether alternative credit assessment methods reduce bias without sacrificing accuracy').

omega_variable(
    historical_bias_feedback_loop,
    'Do historical patterns of discrimination (redlining, employment discrimination, wealth extraction) perpetuate through credit assessment as technical bias, making ''objective'' risk assessment structurally impossible without historical correction?',
    'Path analysis tracing wealth and creditworthiness gaps to historical exclusion policies; simulation of credit outcomes under various historical counterfactuals',
    'If feedback is unbreakable: constraint appears as mountain for generations until historical injustice is materially corrected. If correctable: constraint is tangled rope with potential for reform-driven sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_bias_feedback_loop, empirical, 'Whether credit bias is perpetuated through historical feedback loops requiring material correction').

omega_variable(
    suppression_mechanism_internalization,
    'How much of the measured suppression (0.65) is structural (legal, regulatory, resource barriers to credit access) versus internalized (self-exclusion, distrust of lenders, identity-locked beliefs about creditworthiness)?',
    'Pre-rejection vs. post-rejection analysis of borrower behavior; comparison of stated lending criteria versus actual approval patterns; measurement of credit-seeker behavior changes after regulatory reforms',
    'If structural dominates: constraint loosens when external barriers fall. If internalized dominates: suppression persists after barrier removal — constraint''s grip runs through cognitive patterns that survive policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Proportion of credit access suppression that is structural versus internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credit_risk_assessment_bias, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crab_tr_t0, credit_risk_assessment_bias, theater_ratio, 0, 0.52).
narrative_ontology:measurement(crab_tr_t10, credit_risk_assessment_bias, theater_ratio, 10, 0.62).
narrative_ontology:measurement(crab_tr_t20, credit_risk_assessment_bias, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(crab_be_t0, credit_risk_assessment_bias, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(crab_be_t10, credit_risk_assessment_bias, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(crab_be_t20, credit_risk_assessment_bias, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credit_risk_assessment_bias, resource_allocation).
narrative_ontology:boltzmann_floor_override(credit_risk_assessment_bias, 0.18).
narrative_ontology:affects_constraint(credit_risk_assessment_bias, algorithmic_opacity_in_lending).
narrative_ontology:affects_constraint(credit_risk_assessment_bias, wealth_gap_feedback_loop).
narrative_ontology:affects_constraint(credit_risk_assessment_bias, predatory_lending_norms).

% DUAL FORMULATION NOTE:
% Credit risk assessment bias decomposes into structural concerns: (1) legitimate statistical discrimination (technical coordination), (2) historical bias feedback (path-dependent extraction), (3) algorithmic opacity (institutional theater). This story focuses on the technical bias mechanism; downstream constraints address implementation-specific harms and systemic feedback loops.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(credit_risk_assessment_bias, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
