% ============================================================================
% CONSTRAINT STORY: informal_economy_financial_exclusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_informal_economy_financial_exclusion, []).

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
 *   constraint_id: informal_economy_financial_exclusion
 *   human_readable: Informal Economy Financial Exclusion
 *   domain: economic/financial_inclusion
 *
 * SUMMARY:
 *   Financial exclusion of informal economy workers represents a core
 *   constraint generating extreme wealth asymmetry and economic
 *   vulnerability. Approximately 1.7 billion adults lack access to formal
 *   financial services, trapping them in informal credit networks with
 *   extraction rates 10-100x higher than formal lending. The constraint
 *   operates through multiple reinforcing mechanisms: regulatory (KYC/AML
 *   requirements pricing out small accounts), structural (collateral and
 *   income documentation requirements), and institutional (incumbent bank
 *   capture of regulatory rents). The constraint exhibits high perspectival
 *   diversity: the informal worker sees pure extraction (Snare), formal
 *   institutions see legitimate coordination (Rope), fintech operators see
 *   genuine access provision with embedded exploitation (Tangled Rope),
 *   policy initiatives see temporary scaffolding (Scaffold), compliance
 *   systems see their own degradation (Piton), and the analytical observer
 *   risks naturalizing what is a contingent institutional choice as immutable
 *   economic law. The rising theater ratio indicates growing compliance
 *   theater relative to actual fraud prevention — regulatory complexity
 *   increasing without corresponding security improvement.
 *
 * KEY AGENTS:
 *   - Informal workers (powerless/trapped): Structurally excluded by absence of government documentation and formal employment history; trapped by the system that demands the identity it withholds; bears full cost of exclusion while generating economic value through informal economy
 *   - Microenterprise operators (moderate/constrained): Face high barriers to formalization and collateral requirements; constrained by documentation costs and regulatory burden; forced into moneylender networks extracting 25-100% annually
 *   - Formal financial institutions (institutional/arbitrage): Benefit from regulatory moats (high compliance costs prevent new entrants); capture market segmentation profits; see KYC/AML as coordination mechanism protecting their oligopoly
 *   - Fintech intermediaries (organized/constrained): Genuinely provide access innovation (mobile money, digital identity, alternative credit scoring) but extract through transaction fees and data harvesting; constrained by regulatory uncertainty and capital requirements
 *   - Financial inclusion policymakers (organized/mobile): Central banks, development finance institutions implementing tiered regulations and digital identity systems; have exit options (can pivot to alternative frameworks) and genuine coordination mandate
 *   - Compliance vendors (institutional/arbitrage): Software vendors, consulting firms, training providers profiting from KYC/AML theater; no functional stake in actual fraud prevention, only in regulation perpetuation
 *   - Analytical observer: Risks naturalizing contingent institutional choices (KYC documentation requirements, collateral systems) as laws of rational economics rather than seeing them as designed suppression mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(informal_economy_financial_exclusion, 0.68).
domain_priors:suppression_score(informal_economy_financial_exclusion, 0.72).
domain_priors:theater_ratio(informal_economy_financial_exclusion, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(informal_economy_financial_exclusion, extractiveness, 0.68).
narrative_ontology:constraint_metric(informal_economy_financial_exclusion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(informal_economy_financial_exclusion, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(informal_economy_financial_exclusion, snare).
narrative_ontology:human_readable(informal_economy_financial_exclusion, "Informal Economy Financial Exclusion").
narrative_ontology:topic_domain(informal_economy_financial_exclusion, "economic/financial_inclusion").

domain_priors:requires_active_enforcement(informal_economy_financial_exclusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(informal_economy_financial_exclusion, formal_financial_institutions).
narrative_ontology:constraint_beneficiary(informal_economy_financial_exclusion, regulatory_compliance_vendors).
narrative_ontology:constraint_victim(informal_economy_financial_exclusion, informal_workers).
narrative_ontology:constraint_victim(informal_economy_financial_exclusion, unbanked_populations).
narrative_ontology:constraint_victim(informal_economy_financial_exclusion, microenterprise_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMAL WORKER (SNARE) — Structurally trapped by absence of formal documentation, stable address, and credit history. Lacks access to affordable credit, payment systems, savings infrastructure. Cannot exit without documentation that the system itself withholds. Maximum experienced extraction — bears all costs of financial exclusion while generating economic value.
constraint_indexing:constraint_classification(informal_economy_financial_exclusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MICROENTERPRISE OPERATOR (SNARE) — Constrained by high transaction costs, collateral requirements, and lack of proof-of-income mechanisms. Forced into informal credit networks (moneylenders, rotating savings) with extraction rates 25-100% annually. Can theoretically formalize but faces months-long documentation process, fees, and regulatory burden. High extraction experienced.
constraint_indexing:constraint_classification(informal_economy_financial_exclusion, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FORMAL FINANCIAL INSTITUTIONS (ROPE) — Experience the constraint as coordination mechanism: KYC/AML regulations solve real collective action problem of money laundering and terrorism financing. Institutions arbitrage the exclusion through high-margin services and market segmentation. Capture regulatory benefits (risk transfer, profit concentration) with minimal coordinating burden.
constraint_indexing:constraint_classification(informal_economy_financial_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FINTECH INTERMEDIARIES (TANGLED ROPE) — Genuinely coordinate financial access through mobile money, digital identity, alternative credit scoring. But also extract through platform fees (2-8% per transaction), data harvesting, and rent-seeking on regulatory arbitrage (operating in jurisdictions with looser oversight). Provide real coordination benefit with embedded extraction.
constraint_indexing:constraint_classification(informal_economy_financial_exclusion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FINANCIAL INCLUSION POLICY INITIATIVES (SCAFFOLD) — Central bank digital currencies, financial identity programs, and tiered regulatory frameworks are designed as temporary scaffolding with sunset clauses: as informal workers formalize, the inclusion infrastructure becomes unnecessary. Theater low (genuine coordination function), extraction limited to implementation period. Designed to phase out as formal economy expands.
constraint_indexing:constraint_classification(informal_economy_financial_exclusion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY COMPLIANCE THEATER (PITON) — KYC/AML systems perform extensive documentation and verification rituals with marginal anti-laundering effectiveness (detection rates <0.1%). The ritual persists through institutional inertia: banks maintain massive compliance departments, vendors profit from compliance software, regulators claim success through audit trails. The functional purpose (detecting money laundering) has degraded; performance persists through theater and vendor lock-in.
constraint_indexing:constraint_classification(informal_economy_financial_exclusion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, financial exclusion appears as an immutable feature of asymmetric information in economics: without verifiable proof of identity and income, no lender can assess default risk, hence exclusion is a law of rational economic behavior. However, this naturalizes what is actually a contingent institutional choice: alternative risk assessment mechanisms (community reputation, blockchain identity, algorithmic micro-lending) exist but are suppressed by incumbent institutional arrangements.
constraint_indexing:constraint_classification(informal_economy_financial_exclusion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(informal_economy_financial_exclusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(informal_economy_financial_exclusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(informal_economy_financial_exclusion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(informal_economy_financial_exclusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(informal_economy_financial_exclusion, TR),
    TR >= 0.70.

:- end_tests(informal_economy_financial_exclusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The constraint extracts through multiple channels: (1) Informal credit networks charge 25-100% annually vs 5-15% formal rates, creating wealth transfer from poor to moneylenders. (2) Formal financial services deny access entirely, forcing informal workers to self-finance at zero return on savings. (3) Unregistered businesses cannot access supply chain finance, government procurement, or growth capital, constraining economic mobility. (4) Fintech platforms provide access but extract 2-8% per transaction plus data monetization. The rising trajectory reflects fintech growth (expanding extraction reach) and regulatory tightening (increasing compliance costs, reducing inclusion). Suppression (0.72): Severe and structural. Barriers include government documentation requirements (unavailable to rural/migrant populations), collateral demands (informal assets unregistered), income proof (informal work unverifiable), minimum balance fees (pricing out poor), transaction costs (percentage-based fees regressive to small transactions), regulatory uncertainty (fintech platforms face sudden deactivation), and social stigma (refusal to serve certain groups). Theater ratio (0.55): Moderate and rising. KYC/AML systems perform compliance rituals (document collection, verification, reporting) with minimal anti-laundering effectiveness — detection rates <0.1%, false-positive rates >10%. The rise indicates growing compliance infrastructure relative to actual security gain; regulatory theater expanding without functional improvement. Beneficiaries are formal institutions (incumbent banks capturing regulatory moats, fintech platforms extracting transaction rents) and compliance vendors (growing revenue from regulation). Victims are informal workers (denied access, forced into exploitative credit), microenterprises (constrained growth), and the broader economy (capital trapped in informal sector rather than productive investment).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural mechanism produces inverted classifications for beneficiaries vs victims. Formal institutions and fintech platforms (low d, beneficiaries) classify the constraint as Rope or Tangled Rope — seeing genuine coordination benefit and manageable extraction. Informal workers and microenterprises (high d, victims) classify as Snare — seeing pure extraction with no benefit. Policy initiatives (moderate d, organized exit) classify as Scaffold — seeing a temporary problem with a sunset horizon. The piton classification reflects that KYC/AML infrastructure performs increasingly theatrical compliance functions while actual fraud prevention effectiveness plateaus. The mountain classification at the analytical level is a false summit: the 'immutable law' framing naturalizes what is actually a contingent institutional choice. Alternative identity mechanisms (community reputation, blockchain, mobile biometrics) exist but are suppressed by incumbent interests — the natural law is revealed as institutional power dressed in economic theory.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural relationship to the constraint. Informal workers (powerless/trapped) have d approaching 1.0 — they are the target of extraction with no exit options. Microenterprise operators (moderate/constrained) have d ≈ 0.75 — significant extraction but some exit paths (formalization, cross-border arbitrage). Formal institutions (institutional/arbitrage) have d approaching 0.0 to negative — they are beneficiaries who can arbitrage regulatory requirements and exit whenever beneficial. Fintech operators (organized/constrained) have d ≈ 0.55 — they coordinate access (lower d) but also extract through rents (higher d). Policy initiatives (organized/mobile) have d ≈ 0.40 — organized actors with exit options (can pivot frameworks) who coordinate inclusion. The derived f(d) values produce χ = ε × f(d) × σ(S), where scope modifier σ(S) amplifies χ for global constraints (σ=1.2). Powerless agents experience χ ≈ 0.97 (maximum extraction), while institutional beneficiaries experience negative χ (subsidy from regulatory arrangement). The directionality chain correctly predicts the perspectival gap: high-d agents classify as Snare, low-d agents as Rope, despite identical ε.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through perspectival decomposition. The constraint is NOT 'truly' any single type — it IS all six types simultaneously, viewed from different structural positions. The resolution question is not 'is it Snare or Rope?' but 'which structural position are you analyzing?' The informal worker genuinely experiences a Snare. The formal institution genuinely experiences a Rope. Both are empirically true. The mandatrophy dissolves when we recognize that classification is index-relative, not observer-independent. The false summit (mountain classification) is a critical finding: the analytical observer risks naturalizing institutional choices as natural laws. This reveals the system's inherent contradiction — financial exclusion is enforced through regulations (human-made) but justified through economic laws (claimed natural). Identifying this as a false summit is the mandatrophy's resolution: the constraint is structurally contingent, not naturally necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kyc_aml_effectiveness,
    'What proportion of measured financial inclusion barriers are regulatory requirement (genuine KYC/AML purpose) vs rent-seeking enforcement (compliance theater)?',
    'Comparative analysis: countries with KYC cost <5% transaction value vs >15%; correlation between KYC stringency and actual money-laundering detection rates; false-positive rates in compliance systems',
    'If >60% regulatory requirement: snare classification confirmed (legitimate gate, but extractive enforcement). If >60% rent-seeking: classification shifts toward tangled_rope (minimal genuine coordination, heavy extraction overlay).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kyc_aml_effectiveness, empirical, 'Proportion of barriers attributable to regulation vs rent-seeking').

omega_variable(
    alternative_identity_sufficiency,
    'Do non-government identity mechanisms (community reputation, blockchain identity, biometric plus mobile phone) provide adequate fraud prevention at lower cost than government-issued documentation?',
    'Pilot data from fintech platforms using alternative identity; default rates and fraud losses compared to traditional banking; cost-per-transaction across identity mechanisms',
    'If alternative mechanisms achieve <2% fraud at <1% cost: the entire KYC infrastructure is revelation that suppressed-by-choice, not immutable natural law. Reclassify as snare with deliberate technological suppression. If alternative mechanisms fail above 5% fraud rate: confirm mountain classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_identity_sufficiency, empirical, 'Viability of non-documentary identity mechanisms').

omega_variable(
    formal_sector_absorption_rate,
    'What percentage of informal workers can realistically formalize annually given existing barriers (documentation, fees, regulatory time)? Is the absorption rate sufficient for the scaffold perspective''s sunset claim?',
    'Longitudinal tracking of formalization rates in three countries with active inclusion initiatives; cost per formalized worker; correlation with policy implementation intensity',
    'If absorption rate <2% annually: scaffold sunset is aspirational, not structural — the constraint persists beyond policy horizon. Reclassify scaffold as piton (theater of inclusion without structural change). If >5% annually: scaffold classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(formal_sector_absorption_rate, empirical, 'Rate of informal-to-formal transition under inclusion policies').

omega_variable(
    fintech_extraction_mechanism,
    'Do fintech platforms genuinely provide coordination benefit (lower cost, faster access) compared to formal banking, or do they primarily extract through data harvesting and behavioral capture?',
    'Cost comparison: fintech total fees vs traditional bank account for same services; data usage tracking and valuation; comparison of fintech user retention vs churn; analysis of whether savings/credit access actually improves outcomes vs deepens debt traps',
    'If genuine cost reduction >30%: tangled_rope confirmed with meaningful coordination. If cost reduction <10% but extraction evident: reclassify as snare with modern technology layer. If debt deepening correlates with fintech use: snare classification with digital enforcement mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fintech_extraction_mechanism, empirical, 'Net benefit of fintech inclusion vs traditional exclusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(informal_economy_financial_exclusion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infex_tr_t0, informal_economy_financial_exclusion, theater_ratio, 0, 0.42).
narrative_ontology:measurement(infex_tr_t5, informal_economy_financial_exclusion, theater_ratio, 5, 0.48).
narrative_ontology:measurement(infex_tr_t10, informal_economy_financial_exclusion, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(infex_be_t0, informal_economy_financial_exclusion, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(infex_be_t5, informal_economy_financial_exclusion, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(infex_be_t10, informal_economy_financial_exclusion, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(informal_economy_financial_exclusion, resource_allocation).
narrative_ontology:boltzmann_floor_override(informal_economy_financial_exclusion, 0.18).
narrative_ontology:affects_constraint(informal_economy_financial_exclusion, informal_credit_extraction).
narrative_ontology:affects_constraint(informal_economy_financial_exclusion, regulatory_moat_finance).
narrative_ontology:affects_constraint(informal_economy_financial_exclusion, fintech_debt_entrapment).

% DUAL FORMULATION NOTE:
% Financial exclusion decomposes into three structurally distinct constraints with different ε values. The exclusion mechanism itself (ε≈0.68, this story) coordinates financial stability and extracts from informal workers. The informal credit ecosystem (ε≈0.85, pure extraction through moneylender rents) is downstream and dependent. The regulatory moat protecting formal institutions (ε≈0.55, coordination of banking stability with extraction of market power) is upstream. All three are linked but have distinct measurement properties and resolution mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(informal_economy_financial_exclusion, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
