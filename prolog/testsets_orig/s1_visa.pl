% ============================================================================
% CONSTRAINT STORY: s1_visa
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_s1_visa, []).

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
 *   constraint_id: s1_visa
 *   human_readable: SEC S-1 Registration Framework for Initial Public Offerings
 *   domain: economic/political
 *
 * SUMMARY:
 *   The SEC S-1 registration framework represents a foundational constraint
 *   in US capital markets: the mandatory legal disclosure and review process
 *   required for corporations to issue equity to public investors.
 *   Established under the Securities Act of 1933, the S-1 creates a
 *   standardized process for collecting financial statements, risk
 *   disclosures, management biographies, and business descriptions into a
 *   single legally binding document. The framework serves dual functions:
 *   coordination (standardizing information for efficient price discovery)
 *   and extraction (enabling underwriter gatekeeping, founder equity
 *   dilution, and retail investor information asymmetry). The classification
 *   from different perspectives reveals how institutional positioning
 *   determines whether a single constraint appears as pure coordination, pure
 *   extraction, or hybrid. Early-stage companies experience it as a snare
 *   (trapped, no alternatives). Retail investors experience it as tangled
 *   rope (coordination benefit + extraction cost). Underwriter syndicates
 *   experience it as rope (enabling profitable coordination). The analytical
 *   observer risks naturalizing it as an immutable requirement of public
 *   markets (false summit mountain) until examining how alternative
 *   structures (direct listings, SPACs) achieve equivalent results with lower
 *   extraction. The constraint's theater ratio has risen from 0.42
 *   (1933-1970, when SEC review capacity was closer to verification) to 0.58
 *   (2024, as technical complexity outpaced SEC capacity), indicating
 *   increasing performative content. This drift is characteristic of Piton
 *   degradation: the institutional review process persists through statutory
 *   obligation despite declining functional verification capacity.
 *
 * KEY AGENTS:
 *   - Early-Stage Companies: Primary victim (powerless/trapped) — bear S-1 compliance costs ($2-5M), disclosure burdens, and timeline uncertainty with no alternative exit for accessing public capital
 *   - Retail Investors: Secondary victim (moderate/constrained) — benefit from standardized disclosure (coordination) but experience information asymmetry, allocation opaqueness, and lock-up constraints (extraction)
 *   - Underwriter Syndicates: Primary beneficiary (institutional/arbitrage) — capture fees (7% average), allocation control, and repeat relationship rents; maintain gatekeeping position through syndication structure
 *   - Institutional Investors: Beneficiary with constraints (organized/constrained) — benefit from standardized disclosure and early allocation access, but constrained by fiduciary obligations and allocation relationship dynamics
 *   - SEC Institutional Capacity: Institutional actor (institutional/arbitrage) — maintains regulatory authority and fee-exemption status; limited actual verification capacity due to complexity and staff resources
 *   - Direct Listing / SPAC Frameworks: Alternative pathways (organized/mobile) — reducing S-1 monopoly through genuine exit options; represent sunset mechanism for extraction
 *   - Analytical Observer: Civilizational analysis (analytical/analytical) — risks naturalizing S-1 as inherent to public markets rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(s1_visa, 0.38).
domain_priors:suppression_score(s1_visa, 0.48).
domain_priors:theater_ratio(s1_visa, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(s1_visa, extractiveness, 0.38).
narrative_ontology:constraint_metric(s1_visa, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(s1_visa, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(s1_visa, tangled_rope).
narrative_ontology:human_readable(s1_visa, "SEC S-1 Registration Framework for Initial Public Offerings").
narrative_ontology:topic_domain(s1_visa, "economic/political").

domain_priors:requires_active_enforcement(s1_visa).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(s1_visa, sec_institutional_capacity).
narrative_ontology:constraint_beneficiary(s1_visa, institutional_investors).
narrative_ontology:constraint_beneficiary(s1_visa, capital_market_infrastructure).
narrative_ontology:constraint_victim(s1_visa, early_stage_companies).
narrative_ontology:constraint_victim(s1_visa, retail_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-STAGE COMPANY (SNARE) — Trapped in the S-1 framework with no alternative exit. The filing requirements impose fixed costs ($2-5M in legal and accounting), disclosure obligations that reveal competitive secrets, and timeline uncertainty (6-12 months average). The company cannot access public capital without S-1 compliance. Exit options are constrained to private equity, debt markets, or remaining private — all with inferior capital access. Extraction is maximized because the constraint structure enforces monopoly capture of public equity issuance.
constraint_indexing:constraint_classification(s1_visa, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RETAIL INVESTORS (TANGLED ROPE) — Experience both coordination and extraction. The S-1 framework provides coordination benefit: standardized disclosure enables price discovery and reduces information asymmetry relative to private offerings. But it also imposes extraction through underwriter gatekeeping, lockup periods, and allocation opaqueness — retail investors pay higher effective spreads and receive allocations after institutional investors. Exit is constrained: they cannot easily exit their IPO positions during lockup or escape the framework's information asymmetries.
constraint_indexing:constraint_classification(s1_visa, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNDERWRITER SYNDICATES (ROPE) — Experience the S-1 framework as coordination mechanism. The standardized filing format, SEC review process, and road show protocols solve the information aggregation problem for underwriters. They have arbitrage options (can shift to other capital markets, private placements, or international IPOs) and capture economies of scope from repeat participation. The constraint is enabling rather than extractive from this perspective — it creates the market structure within which they operate profitably.
constraint_indexing:constraint_classification(s1_visa, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL INVESTORS (TANGLED ROPE) — Benefit from S-1 standardization (coordination function): transparent, comparable information across 3,000+ annual IPOs enables efficient allocation. But also experience extraction through: limited allocation access (first-look to preferred relationships), commitment to hold (subtle pressure not to exit early), and rent extraction through equity research paywall (information bundled with trading). Exit is constrained by fiduciary obligations and reputational costs of avoiding IPO syndicates.
constraint_indexing:constraint_classification(s1_visa, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ALTERNATIVE OFFERING FRAMEWORKS (SCAFFOLD) — Direct listings (NYSE Rule 10C-1, introduced 2020) and SPAC mergers represent sunset mechanisms for the S-1 monopoly. Direct listings bypass underwriter gatekeeping entirely; SPACs reduce S-1 filing burden through DE-SPAC structure. These alternatives have low effective extraction because participants have genuine exit paths. S-1 extraction declines as these alternatives mature. The scaffold perspective is empirically grounded: direct listings grew from 0% (2019) to ~15% of public offerings (2024), with continued acceleration.
constraint_indexing:constraint_classification(s1_visa, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: SEC INSTITUTIONAL CAPACITY (PITON) — The S-1 review process is substantially performative. The SEC typically reviews disclosure content (truthfulness, completeness) but does NOT perform fundamental verification of business model viability, revenue forecasting, or market assumptions. The 'review' is a ritual certification that the disclosure package is materially complete and not actively fraudulent — not an assessment of investment merit. Theater ratio is high (0.58) because the SEC lacks capacity to verify technical claims (AI model performance, clinical trial protocols, revenue multiples) and delegates to underwriter due diligence, which is itself incentive-misaligned. The process persists through regulatory inertia and statutory obligation despite limited real-world verification function.
constraint_indexing:constraint_classification(s1_visa, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some disclosure requirement is inherent to public capital markets: anonymous buyers cannot efficiently price securities without standardized information. The S-1 framework could be seen as a natural law of market function — any public equity offering requires some mechanism for material disclosure. However, the structural data contradicts this classification: S-1 is a specific institutional choice (established 1933), not an immutable constraint. Alternative frameworks (direct listings, private continuous offerings, retail investment platforms) provide material disclosure without S-1's extraction mechanisms. The mountain classification is a false summit — naturalizing what is actually a contingent legal structure maintained through path dependence and institutional inertia.
constraint_indexing:constraint_classification(s1_visa, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: MARKET STRUCTURE AND PRICE DISCOVERY (TANGLED ROPE) — The S-1 framework provides genuine coordination function: standardized disclosure enables efficient price discovery, reduces information asymmetry, and enables comparability across 3,000+ offerings annually. Retail investors benefit from this coordination. But the framework also perpetuates asymmetric extraction: underwriter allocation opaqueness, institutional preferencing, and lock-up mechanisms extract value from retail and early-stage participants. The constraint is neither pure coordination nor pure extraction — it is fundamentally hybrid, with coordination benefits captured by institutional actors and extraction costs borne by powerless and constrained participants.
constraint_indexing:constraint_classification(s1_visa, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(s1_visa_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(s1_visa, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(s1_visa, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(s1_visa, TR),
    TR >= 0.70.

:- end_tests(s1_visa_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The S-1 framework does impose material costs on early-stage companies (compliance, dilution, delay), but the extraction is not as severe as pure-snare scenarios because: (1) alternatives exist and are growing (direct listings, SPACs, private markets), (2) many founders choose to go public despite costs (implying net benefit to founders), and (3) underwriter benefits, while real, are partly payment for distribution services. Reduced from initial assessment (0.50) to reflect growing alternative pathways. Suppression (0.48): Moderate. Significant barriers to exit include: statutory requirement (cannot issue public equity without S-1 or equivalent), underwriter gatekeeping (limited to underwriter syndicate relationships), and information asymmetry (founders cannot verify market demand without public process). But suppression is incomplete: alternative capital structures (private equity, debt, SPACs) provide partial exit, and regulatory efforts to streamline S-1 (Reg A+ for smaller offerings, Reg CF for equity crowdfunding) are reducing barriers. Theater ratio (0.58): Moderate-high. The SEC review process is largely procedural rather than fundamental: SEC staff verifies disclosure completeness and fraud markers but does NOT assess business viability, technology feasibility, or revenue forecast accuracy. Underwriter due diligence is incentive-misaligned (underwriters benefit from larger offerings). The theater has increased as technical complexity (AI, biotech, fintech claims) has outpaced SEC verification capacity. Post-2020 direct listings show that equivalent price discovery occurs without S-1 theater.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence across the observation site. Early-stage companies see a snare (trapped, extractive). Retail investors see tangled rope (coordination + extraction). Underwriters see rope (enabling, profitable coordination). Institutional investors see tangled rope (benefits + constraints). The SEC sees arbitrage renewal (continued authority). Direct listing proponents see a scaffold sunset (alternative pathways maturing). The civilizational observer sees a false summit mountain (risks naturalizing contingent institutional choice). The perspectival gap is not ambiguity — it is structural: the S-1 framework genuinely IS both coordination mechanism and extraction apparatus simultaneously. The coordination function is real (standardized disclosure enables price discovery). The extraction is also real (founder dilution, retail information asymmetry, underwriter rents). Different agents experience different proportions of each based on their structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies significantly across agents: (1) Early-stage companies: d ≈ 0.90 (victim + trapped → maximum experienced extraction via sigmoid). (2) Retail investors: d ≈ 0.55 (mixed victim/beneficiary + constrained → moderate experienced extraction). (3) Underwriter syndicates: d ≈ 0.10 (beneficiary + arbitrage → negative/low extraction, net benefit). (4) Institutional investors: d ≈ 0.50 (mixed + constrained → moderate extraction). (5) SEC: d ≈ 0.05 (beneficiary via regulatory authority + arbitrage → institutional-level negative extraction). Direct listing alternatives reduce d for early-stage companies (increasing mobile exit → lower d → lower chi), creating the scaffold sunset mechanism. The engine's automatic derivation from beneficiary/victim declarations and exit options should produce these d values without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that S-1 is fundamentally a Tangled Rope at the market-structure analytical level: it provides genuine coordination (standardized disclosure enables efficient price discovery and retail investor participation) AND genuine asymmetric extraction (founder dilution, retail information gaps, underwriter gatekeeping). The constraint cannot be classified as pure Rope because the extraction component (suppression=0.48, extractiveness=0.38) violates the Rope gate (χ ≤ 0.35, base extraction ≤ 0.45). It cannot be classified as pure Snare because the coordination function is real and valued by beneficiaries. The Tangled Rope classification is confirmed by: (1) presence of beneficiaries (institutional investors, underwriters) + victims (early-stage companies, retail investors), (2) active enforcement (statutory requirement, SEC review process), and (3) genuine coordination function (price discovery) alongside extraction. The perspectival divergence (snare for victims, rope for beneficiaries, scaffold for alternative pathways) reflects structural complexity, not classification error. The false summit mountain from the analytical observer is detected by the engine's natural law certification: S-1 cannot satisfy the accessibility_collapse gate (≥0.85) because direct listings demonstrate accessible alternative structures, violating the immutability requirement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandatory_disclosure_sufficiency,
    'Is standardized S-1 disclosure (risk factors, financial statements, management discussion) actually sufficient to enable informed capital allocation, or is retail investor risk of adverse selection inherent to public offerings?',
    'Long-term performance tracking of IPO cohorts post-disclosure; analysis of how much disclosed vs undisclosed information predicts 5-year returns; measurement of insider trading advantages during post-IPO period',
    'If sufficient: S-1 is primarily coordination (Rope from many perspectives). If insufficient: S-1 is extraction mechanism (Snare/Tangled Rope from retail perspective). If asymmetrically distributed: hybrid (Tangled Rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatory_disclosure_sufficiency, empirical, 'Whether S-1 disclosure enables informed allocation or perpetuates asymmetric information').

omega_variable(
    underwriter_gatekeeping_necessity,
    'Do underwriter syndicates provide essential verification and distribution services, or do their allocation and pricing decisions primarily serve to extract value from retail and early-stage founders?',
    'Comparison of direct listing performance (no underwriter gatekeeping) vs traditional IPO performance; analysis of allocation transparency in traditional underwritten offerings; measurement of underwriter impact on founder equity retention',
    'If essential: underwriter function is coordination (Rope). If extraction-primary: function is snare (Snare). If mixed: hybrid (Tangled Rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(underwriter_gatekeeping_necessity, empirical, 'Whether underwriter gatekeeping is necessary coordination or extractive').

omega_variable(
    sec_verification_capacity,
    'Does the SEC''s review of S-1 filings perform meaningful verification of business viability, technology claims, and financial forecasts, or is it limited to procedural completeness and fraud detection?',
    'Audit of SEC review comments vs disclosed vs actual business outcomes; measurement of SEC rejection rates and grounds; comparison of SEC-identified risks vs actual post-IPO failures; analysis of whether SEC review prevents fraudulent claims',
    'If meaningful: SEC review justifies theater_ratio < 0.40 (Rope/Mountain). If procedural only: theater_ratio > 0.60 justifies Piton classification. Current evidence suggests procedural-only (Piton confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sec_verification_capacity, empirical, 'Whether SEC S-1 review performs material verification').

omega_variable(
    alternative_offering_viability,
    'Do direct listings and SPACs provide materially equivalent capital access and investor protection relative to traditional S-1 IPOs, supporting the scaffold sunset mechanism?',
    'Longitudinal tracking of direct listing growth rates and founder satisfaction; comparison of retail investor experience in direct listings vs traditional IPOs; analysis of whether SPAC alternatives reduce extraction for early-stage founders',
    'If viable: scaffold sunset is real, and S-1 extraction will decline as alternatives mature (projected 20-30% market share by 2030). If not viable: alternatives are niche, S-1 remains dominant, and extraction persists (Snare/Tangled Rope structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_offering_viability, empirical, 'Whether direct listings and SPACs provide materially equivalent alternatives to S-1').

omega_variable(
    founder_outcome_extraction_magnitude,
    'What proportion of founder equity loss in traditional IPO process is attributable to S-1 compliance costs, underwriter fees, and lockup dilution vs other factors (market timing, subsequent dilution, employee option pools)?',
    'Cohort analysis of founder equity retention across IPO cohorts; cost allocation study isolating S-1 compliance expense, underwriter fees, and timing-related dilution',
    'If S-1 costs are >20% of founder equity loss: extraction is material and justifies Snare/Tangled Rope victim classification. If <10%: other factors dominate, and extraction is secondary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_outcome_extraction_magnitude, empirical, 'Proportion of founder equity loss attributable to S-1 framework vs other factors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(s1_visa, 1933, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(visa_tr_t0, s1_visa, theater_ratio, 0, 0.42).
narrative_ontology:measurement(visa_tr_t20, s1_visa, theater_ratio, 20, 0.5).
narrative_ontology:measurement(visa_tr_t40, s1_visa, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(visa_be_t0, s1_visa, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(visa_be_t20, s1_visa, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(visa_be_t40, s1_visa, base_extractiveness, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(s1_visa, information_standard).
narrative_ontology:affects_constraint(s1_visa, underwriter_syndication_power).
narrative_ontology:affects_constraint(s1_visa, retail_investor_information_asymmetry).
narrative_ontology:affects_constraint(s1_visa, founder_equity_dilution_dynamics).

% DUAL FORMULATION NOTE:
% The S-1 framework decomposes into two structurally distinct claims: (1) Information Standardization (ε≈0.08, Mountain) — any public market requires some material disclosure standard, and S-1 provides this. (2) S-1 Institutional Specifics (ε≈0.38, Tangled Rope) — the particular S-1 process creates extraction through underwriter gatekeeping, compliance costs, and founder dilution. The higher-extractiveness claim (S-1 Institutional Specifics) is downstream of the lower-extractiveness claim (Information Standardization requirement). This story addresses the institutional-specifics formulation; the information-standard requirement is a separate constraint with lower ε and mountain-like properties from many perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(s1_visa, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
