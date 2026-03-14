% ============================================================================
% CONSTRAINT STORY: business_investment_throttle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_business_investment_throttle, []).

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
 *   constraint_id: business_investment_throttle
 *   human_readable: Business Investment Throttle: Regulatory Barriers and Capital Allocation Asymmetry
 *   domain: economic/financial/regulatory
 *
 * SUMMARY:
 *   The business investment throttle is a regulatory system that creates
 *   asymmetric capital access by imposing compliance barriers that incumbent
 *   firms can absorb but which trap bootstrapping entrepreneurs. The
 *   constraint operates through licensing requirements, regulatory
 *   documentation, compliance staffing, and institutional gatekeeping that
 *   collectively raise the capital floor for market entry. This constraint
 *   exhibits hybrid characteristics: genuine coordination function (fraud
 *   prevention, systemic stability) coexists with institutional extraction
 *   (regulatory scope creep, incumbent moat protection, theater maintenance).
 *   The extractiveness trajectory (0.35 → 0.58 over the interval) reflects
 *   cumulative regulatory density expansion driven by post-2008 financial
 *   crisis legislation, Dodd-Frank complexity, and agency scope expansion.
 *   The theater ratio (0.38 → 0.48) reflects increasing performativity of
 *   compliance processes as regulation has accumulated beyond what
 *   demonstrably prevents actual fraud.
 *
 * KEY AGENTS:
 *   - Bootstrapping Entrepreneurs: Primary victims (powerless/trapped) — face capital floors from compliance burden, cannot access regulatory waiver programs, bear full suppression without institutional mediation
 *   - Venture-Backed Startups: Secondary beneficiary-victim (moderate/constrained) — constrained by investor expectations but benefit from regulatory moat protecting portfolio companies
 *   - Incumbent Corporations: Primary beneficiary (institutional/arbitrage) — regulatory barriers function as competitive moat; amortized compliance costs create sustainable advantage over new entrants
 *   - Regulatory Agencies: Beneficiary-victim (institutional/constrained) — genuine coordination function justifies some enforcement, but also derive budget authority and institutional scope from regulatory complexity
 *   - Fintech Coalition: Organized alternative-seekers (organized/mobile) — mobilizing around regulatory arbitrage and technology-enabled alternatives with decadal sunset horizon
 *   - Compliance Industry: Secondary beneficiary (institutional/arbitrage) — lawyers, consultants, software vendors extracting value from regulatory complexity they have incentive to maintain
 *   - Market Innovation System: Primary victim (powerless/trapped) — abstract collective good bearing cost of delayed market entry, reduced capital allocation efficiency, and suppressed innovation in capital-intensive sectors
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(business_investment_throttle, 0.58).
domain_priors:suppression_score(business_investment_throttle, 0.65).
domain_priors:theater_ratio(business_investment_throttle, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(business_investment_throttle, extractiveness, 0.58).
narrative_ontology:constraint_metric(business_investment_throttle, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(business_investment_throttle, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(business_investment_throttle, tangled_rope).
narrative_ontology:human_readable(business_investment_throttle, "Business Investment Throttle: Regulatory Barriers and Capital Allocation Asymmetry").
narrative_ontology:topic_domain(business_investment_throttle, "economic/financial/regulatory").

domain_priors:requires_active_enforcement(business_investment_throttle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(business_investment_throttle, incumbent_firms).
narrative_ontology:constraint_beneficiary(business_investment_throttle, regulatory_agencies).
narrative_ontology:constraint_beneficiary(business_investment_throttle, compliance_consultants).
narrative_ontology:constraint_victim(business_investment_throttle, startup_entrepreneurs).
narrative_ontology:constraint_victim(business_investment_throttle, capital_efficiency).
narrative_ontology:constraint_victim(business_investment_throttle, market_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BOOTSTRAPPING ENTREPRENEUR (SNARE) — Lacks institutional relationships, cannot navigate regulatory complexity, has no alternative capital sources. Bears full suppression cost: licensing fees, compliance staff, documentation overhead consume 15-30% of operating capital before revenue begins. Cannot exit without abandoning business formation. Maximum experienced extraction.
constraint_indexing:constraint_classification(business_investment_throttle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VENTURE-BACKED STARTUP (TANGLED ROPE) — Constrained by investor expectations and exit timelines, but gains genuine coordination benefits: regulatory compliance creates barriers to competitor entry, accelerating market consolidation favoring dominant portfolio companies. Bears compliance costs but also benefits from enforcement that disadvantages unbackedrivvals. Mixed extraction and coordination.
constraint_indexing:constraint_classification(business_investment_throttle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT CORPORATION (ROPE) — Amortized compliance costs across large revenue base. Regulatory barriers function as moat: new entrants cannot replicate the capital and institutional sophistication required for regulatory navigation. Net beneficiary experiencing the constraint as coordination of market structure that protects their position. Effective extraction flows toward this agent.
constraint_indexing:constraint_classification(business_investment_throttle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AGENCY (TANGLED ROPE) — Genuine coordination function: preventing fraud, systemic risk, and predatory practices justifies some compliance burden. But agency also derives budget justification, staffing growth, and political influence from regulatory complexity — higher barriers → larger enforcement apparatus → expanded agency scope. Enforcement itself becomes partially extractive (regulatory mission creep). Both beneficiary and victim of their own constraint.
constraint_indexing:constraint_classification(business_investment_throttle, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FINTECH COALITION (SCAFFOLD) — Organized technology and financial services firms mobilizing around regulatory arbitrage: moving operations to lower-barrier jurisdictions, building compliance automation tools, and advocating for streamlined licensing paths. See the investment throttle as a temporary coordination failure being solved through technological and jurisdictional alternatives. Sunset clause: blockchain/decentralized finance platforms and digital ID infrastructure may eventually bypass traditional regulatory gatekeeping (10-30 year horizon).
constraint_indexing:constraint_classification(business_investment_throttle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COMPLIANCE THEATER SYSTEM (PITON) — Many regulatory requirements (anti-money laundering documentation, know-your-customer verification, quarterly reporting) persist largely through institutional inertia and risk-aversion rather than demonstrated effectiveness. Compliance burden scales with regulatory density but not with actual fraud prevention efficacy. Theater persists because agencies and incumbent firms have incentive to maintain complexity; the enforcement ritual legitimates both authority and market barriers. Primary function has atrophied — constraint maintained through institutional theater rather than actual risk reduction.
constraint_indexing:constraint_classification(business_investment_throttle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational timescale, some investment throttle appears immutable: information asymmetry between capital providers and entrepreneurs creates legitimately higher risk for uninstitutionalized actors. Prudential regulation to prevent systemic collapse is necessarily constraining. However, the structural data reveals this naturalizing framing as false summit — the magnitude and distribution of regulatory burden far exceed what asymmetric information requires, indicating contingent institutional arrangements rather than laws of nature.
constraint_indexing:constraint_classification(business_investment_throttle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(business_investment_throttle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(business_investment_throttle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(business_investment_throttle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(business_investment_throttle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(business_investment_throttle, TR),
    TR >= 0.70.

:- end_tests(business_investment_throttle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The regulatory system extracts value by redirecting capital from productive investment to compliance overhead, while blocking entrepreneurial entry at lower resource levels. The trajectory from 0.35 to 0.58 reflects two decades of regulatory expansion post-financial crisis. Not maximum extraction (would require near-total blockade of entrepreneurial activity) but substantial. Suppression (0.65): Moderate-high. Barriers include capital requirements ($250K-$5M depending on sector), compliance staffing (1-3 FTEs for startups), documentation overhead (6-12 months pre-revenue), and licensing delays (3-24 months). Suppression is high because alternatives are genuinely limited — cannot operate without regulatory approval in most sectors. Theater ratio (0.48): Moderate. The constraint contains both functional and performative components. Fraud prevention (KYC, AML) has demonstrable value but regulatory complexity has expanded far beyond what fraud prevention requires. Documentation rituals, quarterly reporting, and compliance theater maintain agency legitimacy and incumbent protection without proportional fraud reduction. The theater ratio is lower than typical pitons because the coordination function remains genuine, even if padded with theater.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates full perspectival divergence driven by differential exit options and cost/benefit distribution. The powerless/trapped perspective (entrepreneur) sees snare — immutable barriers, no alternatives, maximum extraction. The institutional/arbitrage perspective (incumbent) sees rope — genuine coordination with net benefit. The institutional/constrained perspective (agency) sees tangled rope — coordination function coexisting with scope expansion creating self-interested extraction. The organized/mobile perspective (fintech) sees scaffold — temporary barriers being bypassed by technology and jurisdictional competition. The institutional/arbitrage perspective on theater (compliance system) sees piton — institutional inertia maintaining apparatus whose primary function has atrophied. The analytical perspective risks seeing mountain — naturalizing contingent institutional barriers as immutable features of capital markets. This full-spectrum perspectival gap indicates that the constraint is genuinely hybrid (tangled rope) rather than purely extractive (snare) or purely coordinative (rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position: bootstrap entrepreneurs with trapped exit and victim status experience maximum d (approaching 1.0), generating maximum f(d) and maximum experienced χ. Incumbent corporations with arbitrage exit and beneficiary status experience minimum d (approaching 0.0), generating minimum f(d) and negative or neutral experienced χ. Venture-backed startups occupy the middle: constrained exit and mixed victim-beneficiary status produce moderate d (0.40-0.55), intermediate χ. Regulatory agencies are partially captured — they appear as beneficiaries (from scope expansion) but are also constrained by statutory mandates, producing moderate-high d (0.45-0.60). The directionality override for compliance consultants is not needed — their beneficiary status is structural, not captured.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The business investment throttle resolves mandatrophy by exposing the difference between justified capital barriers (asymmetric information, fraud risk, systemic stability) and unjustified barriers (regulatory scope creep, incumbent protection, compliance theater). The genuine coordination function (fraud prevention, capital provider protection) can be achieved at lower regulatory cost than currently imposed. The extraction (regulatory moat protection, incumbent advantage, compliance industry rent) is not necessary for coordination. The constraint is tangled rope because it genuinely coordinates (prevents fraud, manages systemic risk) while simultaneously extracting (protects incumbents, funds regulatory apparatus growth, blocks capital access). Mandatrophy is resolved by recognizing that the two functions coexist structurally — the coordination cannot be cleanly separated from the extraction. The cage cannot be called either pure coordination or pure extraction; it is hybrid. The analytical response is not to choose one classification but to decompose: what minimum regulatory burden achieves fraud prevention (that is rope)? What additional burden serves incumbent protection (that is snare for entrepreneurs)? The current constraint bundles both, requiring tangled rope classification with the implicit mandate that the bundle should be decomposed to minimize unnecessary suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_burden_decomposition,
    'What portion of compliance costs is legitimately required for fraud prevention vs. serving as regulatory moat or institutional theater?',
    'Comparative analysis: jurisdictions with lower barriers vs. higher barriers tracking fraud rates, systemic risk incidents, and market entry rates; randomized policy experiments in reducing specific compliance requirements',
    'If legitimate portion < 30%: constraint reclassifies as primarily extractive (Snare from entrepreneurial perspective becomes even more severe). If legitimate portion > 70%: constraint reclassifies as primarily coordination with excess regulation (Tangled Rope moves toward Rope at institutional perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_burden_decomposition, empirical, 'Decomposition of compliance burden into fraud-prevention vs. institutional-theater components').

omega_variable(
    substitutability_of_regulatory_mechanisms,
    'Can technology (blockchain verification, AI-driven compliance, decentralized identity) substitute for regulatory gatekeeping without compromising systemic stability?',
    'Pilot programs in jurisdictions experimenting with technology-enabled alternative compliance; longitudinal comparison of fraud/systemic risk in traditional vs. decentralized finance ecosystems',
    'If substitutability is high: fintech coalition''s scaffold perspective is structural (sunset is real; constraint will degrade over 10-20 years). If substitutability is low: scaffold is aspirational and constraint persists (fintech coalition faces disappointment; entrepreneurial trap extends).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substitutability_of_regulatory_mechanisms, empirical, 'Whether technology can substitute for traditional regulatory gatekeeping').

omega_variable(
    capital_allocation_efficiency_loss,
    'What percentage of potentially productive investment is foregone due to regulatory barriers that do not prevent genuine fraud or systemic risk?',
    'Comparison of capital allocation patterns in high-barrier vs. low-barrier jurisdictions; analysis of venture capital allocation by sector correlation with regulatory complexity; historical comparison of innovation rates before/after regulatory expansion',
    'If efficiency loss > 15% of potential GDP: extractiveness reclassifies upward (constraint approaches Snare at aggregate economy perspective). If efficiency loss < 5%: extractiveness reclassifies downward (constraint approaches Rope at economic policy perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_allocation_efficiency_loss, empirical, 'Magnitude of capital misallocation due to regulatory barriers').

omega_variable(
    incumbent_capture_dynamics,
    'To what extent do incumbent firms actively lobby to maintain regulatory barriers, vs. barriers being independently maintained by agencies?',
    'Analysis of regulatory capture patterns: lobbying expenditure correlation with specific regulatory provisions; evolution of regulatory density in sectors with concentrated incumbents vs. competitive sectors; revolving-door patterns between agencies and incumbent firms',
    'If capture is primary driver (>60%): constraint is institutional collusion mechanism (Snare from entrepreneurial perspective is more severe; incumbent rope perspective is more extractive). If capture is secondary: constraint is primarily agency-driven institutional inertia (Piton characterization stronger; theater ratio explanation upheld).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incumbent_capture_dynamics, empirical, 'Magnitude of incumbent firm capture in regulatory barrier maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(business_investment_throttle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bit_tr_t0, business_investment_throttle, theater_ratio, 0, 0.38).
narrative_ontology:measurement(bit_tr_t5, business_investment_throttle, theater_ratio, 5, 0.42).
narrative_ontology:measurement(bit_tr_t10, business_investment_throttle, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(bit_be_t0, business_investment_throttle, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bit_be_t5, business_investment_throttle, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(bit_be_t10, business_investment_throttle, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(business_investment_throttle, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(business_investment_throttle, 0.12).
narrative_ontology:affects_constraint(business_investment_throttle, venture_capital_concentration).
narrative_ontology:affects_constraint(business_investment_throttle, startup_failure_rate_distribution).
narrative_ontology:affects_constraint(business_investment_throttle, regulatory_arbitrage_incentive).

% DUAL FORMULATION NOTE:
% The business investment throttle could decompose into two structurally distinct constraints: (1) legitimate_fraud_prevention (ε ≈ 0.15, rope) addressing asymmetric information and fraud risk, and (2) incumbent_protection_moat (ε ≈ 0.65, snare) addressing competitive barrier creation. The current story treats them bundled, generating tangled rope. Decomposition would allow separate optimization: reducing unnecessary suppression in fraud prevention while explicitly addressing incumbent capture in barrier maintenance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(business_investment_throttle, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
