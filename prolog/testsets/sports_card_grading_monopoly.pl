% ============================================================================
% CONSTRAINT STORY: sports_card_grading_monopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sports_card_grading_monopoly, []).

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
 *   constraint_id: sports_card_grading_monopoly
 *   human_readable: Sports Card Grading Monopoly
 *   domain: collectibles_market/economics
 *
 * SUMMARY:
 *   The sports card grading monopoly constrains the collectibles market
 *   through control of standardized condition certification. Three major
 *   grading companies (PSA, BGS/Beckett, CGC Cards) dominate the market, with
 *   PSA holding approximately 80% market share for vintage cards. Sellers and
 *   collectors must submit cards to these services for authentication and
 *   grading, incurring fees of 15-50% per card depending on turnaround speed
 *   and card value. The monopoly extracts through fees, artificial scarcity
 *   (grading backlogs limit market liquidity), and lock-in effects (cards
 *   graded by alternative services sell at significant discounts regardless
 *   of condition). The constraint exhibits both extractive (snare for
 *   powerless sellers) and coordination (rope for institutional
 *   beneficiaries) properties from different perspectives. Theater ratio
 *   (0.58) reflects that the physical slab and hologram are performative: the
 *   actual condition assessment is proprietary, non-verifiable, and subject
 *   to grade inconsistency across different graders. The extractiveness
 *   trajectory (0.38 → 0.62 over 10 years) shows increasing rent-seeking:
 *   fees have escalated, turnaround times have lengthened during boom
 *   periods, and the market has become more dependent on monopoly
 *   certification.
 *
 * KEY AGENTS:
 *   - Major Grading Companies (PSA, BGS, CGC): Primary beneficiaries (institutional/arbitrage) — capture fees, control market access, maintain brand moat through network effects
 *   - Independent Sellers: Primary victims (powerless/trapped) — must pay grading fees or accept 30-50% price discount; no practical alternative
 *   - Retail Collectors: Secondary victims (moderate/constrained) — bear cost of grading fees passed through secondary market; can exit via raw card collecting but sacrifice liquidity
 *   - Investment Dealers: Mixed position (powerful/mobile) — benefit from standardized market but pay grading fees; mobile enough to shift asset classes
 *   - Card Manufacturers: Institutional actor with arbitrage (institutional/arbitrage) — benefit from grading-driven collectibility and secondary market liquidity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks seeing information asymmetry resolution as justifying monopoly rather than examining specific institutional form
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sports_card_grading_monopoly, 0.62).
domain_priors:suppression_score(sports_card_grading_monopoly, 0.68).
domain_priors:theater_ratio(sports_card_grading_monopoly, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sports_card_grading_monopoly, extractiveness, 0.62).
narrative_ontology:constraint_metric(sports_card_grading_monopoly, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sports_card_grading_monopoly, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sports_card_grading_monopoly, snare).
narrative_ontology:human_readable(sports_card_grading_monopoly, "Sports Card Grading Monopoly").
narrative_ontology:topic_domain(sports_card_grading_monopoly, "collectibles_market/economics").

domain_priors:requires_active_enforcement(sports_card_grading_monopoly).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sports_card_grading_monopoly, major_grading_companies).
narrative_ontology:constraint_victim(sports_card_grading_monopoly, independent_sellers).
narrative_ontology:constraint_victim(sports_card_grading_monopoly, retail_collectors).
narrative_ontology:constraint_victim(sports_card_grading_monopoly, card_market_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT SELLER (SNARE) — Card sellers without grading company credentials face severe market barriers. Ungraded or independently-graded cards sell at 30-50% discount regardless of actual condition. No practical exit exists — participating in the market requires submission to monopoly pricing (current fees 15-50% per card depending on turnaround). Suppression is maximal: alternatives are systematically devalued by collective expectation.
constraint_indexing:constraint_classification(sports_card_grading_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RETAIL COLLECTOR (SNARE) — Hobbyist collectors bear extraction through inflated prices (grading cost passed to secondary market) and artificial scarcity (high-grade cards artificially scarce due to grading backlogs). Exit is constrained rather than trapped: can collect raw cards at lower prices but faces liquidity penalty when selling. Can avoid market entirely but loses participation in the collectibles hobby. Significant extraction with some agency but not full freedom.
constraint_indexing:constraint_classification(sports_card_grading_monopoly, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INVESTMENT DEALER (TANGLED ROPE) — Professional dealers have genuine coordination benefit from standardized grading (enables wholesale markets, reduces information asymmetry, increases liquidity) but also extract through volume discounts and position advantage. Mixed experience: the system coordinates their market AND extracts from them (they pay grading fees, face turnaround delays). Mobile enough to shift to raw cards or alternative asset classes, but the coordination value keeps them participating.
constraint_indexing:constraint_classification(sports_card_grading_monopoly, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR GRADING COMPANY (ROPE) — Experiences the constraint as pure coordination: they solve the information asymmetry problem that makes collectibles markets function. Grading standardization enables price discovery, enables wholesale distribution, enables secondary markets. Net beneficiary — extraction runs toward them (fees flow from market). The constraint's existence is their business model.
constraint_indexing:constraint_classification(sports_card_grading_monopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY GRADING AUTHORITY (PITON) — Traditional institutions (PSA, BGS) maintain market dominance through institutional inertia: their grades are 'trusted' because they always have been, not because their methodology is superior. Theater is high (0.58) — the physical slab and hologram are performative markers of authenticity; the actual grading criteria are proprietary and non-verifiable. The system persists because switching costs are high (resellers won't accept alternative grades), not because the alternative is impossible. New entrants emerge but fail to displace incumbents despite better technology or lower fees.
constraint_indexing:constraint_classification(sports_card_grading_monopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Information asymmetry in collectibles is a fundamental market problem: buyers cannot verify condition without expertise, and no individual grader's credentials are universally recognized. Some form of standardized certification is structurally necessary for market function. However, the specific form (monopolistic grading companies with proprietary criteria) is not inevitable — the structural data reveals this as a false summit.
constraint_indexing:constraint_classification(sports_card_grading_monopoly, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sports_card_grading_monopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sports_card_grading_monopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sports_card_grading_monopoly, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sports_card_grading_monopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sports_card_grading_monopoly, TR),
    TR >= 0.70.

:- end_tests(sports_card_grading_monopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High and rising. The monopoly extracts through mandatory participation fees (sellers cannot avoid), price mark-ups in secondary market (grading cost paid by collector), and artificial scarcity (backlogs limit supply of graded cards). The value rose from 0.38 to 0.62 as the market matured: early grading was optional and low-cost; current grading is necessary for market participation and high-cost. The constraint is not pure extraction (0.72+) because grading does provide real coordination benefit (enables wholesale markets, enables price discovery). Suppression (0.68): High. Barriers to exit include: network effects (grade standards are worthless if market doesn't accept them), switching costs (resellers reject alternative grades), and collective expectation (buyers trust incumbent brands). Independent grading services exist but fail to capture market share despite better terms. Suppression is not maximal (0.80+) because exit is structurally possible (raw card markets exist, alternative graders operate) but economically punitive. Theater ratio (0.58): Moderate-high and rising. The physical slab and hologram are performative authenticators. The grading criteria are proprietary and non-transparent — buyers trust the grade because they trust the brand, not because they can verify the methodology. Theater has increased as grades have become more granular (half-point grades, subgrades) and harder to consistently replicate.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows the deepest perspectival divergence from sellers vs beneficiaries. Sellers (powerless/trapped) classify as Snare: they experience mandatory extraction with no exit. The major grading companies (institutional/arbitrage) classify as Rope: they experience pure coordination. Investment dealers (powerful/mobile) classify as Tangled Rope: they benefit from the coordination function but also pay extraction costs and can exit. The piton perspective reveals that the monopoly persists through institutional inertia and brand lock-in, not superior methodology — alternatives exist with better technology but fail to displace incumbents. The analytical observer risks naturalizing the monopoly as necessary (Mountain) when the specific institutional form (three companies, proprietary criteria, slabs with holograms) is contingent and extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position in the extraction flow. Sellers and collectors are victims (high d → high experienced extraction). Major grading companies are beneficiaries with no exit friction (low d → negative/near-zero extraction). Investment dealers are both beneficiary and victim depending on their position in the supply chain — they benefit from market liquidity but pay fees; they have mobile exit options but choosing to exit is costly (d ≈ 0.50 → moderate extraction). The piton perspective's directionality is determined by the institutional actor's arbitrage options: they can maintain the monopoly indefinitely, but alternatives exist and could displace them if conditions change. The theater ratio drives the piton classification more than directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE vs TANGLED ROPE RESOLUTION: The constraint classifies as Snare from the seller/collector perspective (pure extraction, trapped exit) but Tangled Rope from the dealer perspective (mixed coordination and extraction, constrained exit). The mandatrophy is resolved by recognizing these are legitimate perspectival differences. For sellers, the constraint is pure Snare: they must participate, must pay fees, must accept monopoly terms. For dealers, it is Tangled Rope: the grading function genuinely coordinates market operations (enables wholesale, enables pricing), and dealers benefit from this coordination even as they pay fees. The piton perspective (institutional/civilizational) reveals a third structural truth: the incumbent monopoly is degraded. Better alternatives (faster turnaround, transparent criteria, blockchain authentication) are technically feasible, but the monopoly persists through institutional lock-in. The false mountain (analytical observer) naturalizes this as necessary information-asymmetry resolution, when the specific form is contingent and extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authentication_vs_grading_conflation,
    'Are buyers paying for authentication (card is genuine) or grading (card is condition X)? If the former, do alternative authentication methods (spectral analysis, blockchain provenance) sufficiently compete?',
    'Market segmentation analysis: willingness-to-pay for authentication alone vs grading service; adoption rate of alternative authentication technologies; price premiums that persist after authentication alternative is available',
    'If authentication dominates: monopoly is more defensible (authentication is natural monopoly due to network effects). If grading dominates: monopoly is artificial and vulnerable to alternative grading standards.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authentication_vs_grading_conflation, empirical, 'Whether buyers value authentication or grading service in graded cards').

omega_variable(
    counterfeit_circulation_risk,
    'What is the actual counterfeit circulation rate in sports cards? If authentication is the primary barrier, does it prevent counterfeits effectively?',
    'Forensic analysis of cards in circulation; longitudinal tracking of counterfeit detection rates; correlation between authentication tightness and market price stability',
    'If counterfeits are rare (< 1%): authentication monopoly is legitimately protective. If counterfeits are common (> 5%): monopoly authentication is failing and alternatives may be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfeit_circulation_risk, empirical, 'Actual counterfeit rate in sports card market').

omega_variable(
    alternative_grading_adoption_barrier,
    'Why do alternative grading services (CGC Cards, Sportscard Guaranty, etc.) fail to displace incumbents despite lower prices and faster turnaround?',
    'Historical analysis of market share shifts; price elasticity analysis of grading service demand; willingness-to-accept analysis of alternative grades in secondary market; institutional switching cost measurement',
    'If adoption barrier is rational (better authentication, true reliability advantage): monopoly is structural. If adoption barrier is switching cost (buyers'' expectations locked in): monopoly is extractive but unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_grading_adoption_barrier, empirical, 'Why alternative grading services fail to compete successfully').

omega_variable(
    collector_identity_lock_mechanism,
    'To what extent is the monopoly grading company (PSA, BGS) part of the collector''s identity? Are collectors defending the specific company or the grading function?',
    'Survey data on brand loyalty; analysis of collector forums and communication; switching behavior analysis when alternative services offer objectively better terms; qualitative interviews on what ''having a PSA card'' means vs ''having a graded card''',
    'If identity-locked: collectors perceive alternatives as inauthentic, and monopoly has cognitive lock independent of structural lock. If rational evaluation: collector behavior is price-sensitive and alternatives can displace incumbents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collector_identity_lock_mechanism, empirical, 'Whether collector identity is fused with specific grading company brand').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sports_card_grading_monopoly, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scgm_tr_t0, sports_card_grading_monopoly, theater_ratio, 0, 0.42).
narrative_ontology:measurement(scgm_tr_t5, sports_card_grading_monopoly, theater_ratio, 5, 0.5).
narrative_ontology:measurement(scgm_tr_t10, sports_card_grading_monopoly, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(scgm_be_t0, sports_card_grading_monopoly, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(scgm_be_t5, sports_card_grading_monopoly, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(scgm_be_t10, sports_card_grading_monopoly, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sports_card_grading_monopoly, information_standard).
narrative_ontology:affects_constraint(sports_card_grading_monopoly, collectibles_authentication_standards).
narrative_ontology:affects_constraint(sports_card_grading_monopoly, secondary_market_liquidity_lock).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sports_card_grading_monopoly, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
