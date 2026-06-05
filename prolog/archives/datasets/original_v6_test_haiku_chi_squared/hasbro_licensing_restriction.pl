% ============================================================================
% CONSTRAINT STORY: hasbro_licensing_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hasbro_licensing_restriction, []).

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
 *   constraint_id: hasbro_licensing_restriction
 *   human_readable: Hasbro Licensing Restriction on Licensed Properties
 *   domain: economic/intellectual_property
 *
 * SUMMARY:
 *   Hasbro's licensing restrictions on its intellectual properties
 *   (Transformers, My Little Pony, Power Rangers, G.I. Joe, etc.) represent a
 *   structural constraint that simultaneously enables market expansion and
 *   extracts value from licensees. The constraint operates through formalized
 *   approval processes, royalty structures, exclusivity clauses, and quality
 *   control requirements that restrict licensee innovation while protecting
 *   brand consistency. This story models how the same structural mechanism
 *   produces radically different classifications depending on the observer's
 *   structural position: dependent licensees see a snare with no exit; large
 *   corporations see a mixed coordination-extraction hybrid; Hasbro sees pure
 *   coordination; the broader product ecosystem sees constrained innovation;
 *   and the analytical observer sees a tangled rope that extracts
 *   disproportionate value while claiming to protect brand integrity. The
 *   theater ratio (0.48) reflects moderate performativity in approval
 *   processes — much of the review is procedural compliance checking, but
 *   genuine quality control also occurs. The extractiveness (0.58) reflects
 *   that Hasbro has increased its leverage over the interval as its IP
 *   portfolio has become more valuable and licensees' switching costs have
 *   risen.
 *
 * KEY AGENTS:
 *   - Hasbro IP Management Division: Primary beneficiary (institutional/arbitrage) — captures value through licensing fees, approval leverage, and strategic control of product roadmaps
 *   - Small-to-Medium Licensees: Primary victims (powerless/trapped) — face high switching costs and limited negotiating power once invested in product lines; trapped by sunk costs in tooling and supply chains
 *   - Large Corporate Licensees: Secondary actors (powerful/arbitrage) — negotiate better terms due to diversified portfolios and exit options; experience mixed coordination and extraction
 *   - Consumer Product Ecosystem: Secondary victims (organized/constrained) — face restricted product diversity and higher prices due to royalty pass-through; retailers and consumers have limited ability to pressurize Hasbro directly
 *   - Retail Distribution Channel: Organized intermediary (organized/constrained) — experiences licensing restrictions on product selection; has moderate negotiating power through shelf space leverage
 *   - Analytical Observer: Neutral observer (analytical/analytical) — views constraint as tangled rope with problematic extraction; notes welfare loss from innovation restriction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hasbro_licensing_restriction, 0.58).
domain_priors:suppression_score(hasbro_licensing_restriction, 0.65).
domain_priors:theater_ratio(hasbro_licensing_restriction, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hasbro_licensing_restriction, extractiveness, 0.58).
narrative_ontology:constraint_metric(hasbro_licensing_restriction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(hasbro_licensing_restriction, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hasbro_licensing_restriction, tangled_rope).
narrative_ontology:human_readable(hasbro_licensing_restriction, "Hasbro Licensing Restriction on Licensed Properties").
narrative_ontology:topic_domain(hasbro_licensing_restriction, "economic/intellectual_property").

domain_priors:requires_active_enforcement(hasbro_licensing_restriction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hasbro_licensing_restriction, hasbro_ip_portfolio).
narrative_ontology:constraint_beneficiary(hasbro_licensing_restriction, brand_consistency_enforcement).
narrative_ontology:constraint_victim(hasbro_licensing_restriction, licensee_innovation_capacity).
narrative_ontology:constraint_victim(hasbro_licensing_restriction, consumer_product_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT LICENSEE (SNARE) — Small-to-medium licensees have no credible exit option once they invest in tooling, supply chains, and market position around a Hasbro IP license. Contract renegotiation leverages their sunk costs; termination means asset writeoff. d≈0.92, f(d)≈1.40, σ=1.1 → χ≈0.90. High effective extraction.
constraint_indexing:constraint_classification(hasbro_licensing_restriction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LARGE CORPORATE LICENSEE (TANGLED ROPE) — Major licensees (e.g., toy manufacturers with diversified portfolios) can negotiate better terms and have exit options (drop the license, develop competing IP). They experience both coordination benefit (access to valuable IP) and extraction (royalty rates, approval requirements, exclusivity restrictions). d≈0.48, f(d)≈0.60, σ=1.1 → χ≈0.38.
constraint_indexing:constraint_classification(hasbro_licensing_restriction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: HASBRO IP MANAGEMENT (ROPE) — Hasbro benefits from licensing as a coordination mechanism: it expands market reach without direct production, solves the problem of scale across diverse product categories (toys, apparel, gaming, media). Restrictions are experienced as legitimate brand protection. d≈0.08, f(d)≈-0.10, σ=1.1 → χ≈-0.06. Net beneficiary; negative effective extraction from Hasbro's perspective.
constraint_indexing:constraint_classification(hasbro_licensing_restriction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSUMER PRODUCT ECOSYSTEM (TANGLED ROPE) — Retailers, consumers, and competing product designers are constrained by limited product diversity under restrictive licensing (fewer innovation pathways, higher prices due to royalty pass-through). But the ecosystem benefits from brand consistency and quality floors. Organized actors (retail chains, consumer advocacy groups) can negotiate terms but face coordination problems. d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.58.
constraint_indexing:constraint_classification(hasbro_licensing_restriction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY LICENSING BUREAUCRACY (PITON) — Hasbro's approval process for licensee products (design review, quality control, brand guideline compliance) has grown highly formalized and theatrical. Theater ratio = 0.48 reflects that much of the review is procedural compliance checking rather than functional quality assurance; many restrictions are maintained for institutional continuity rather than genuine brand risk. χ=0.28, theater moderately high but not extreme piton territory.
constraint_indexing:constraint_classification(hasbro_licensing_restriction, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the constraint reflects genuine coordination (brand licensing legitimately solves the problem of reaching diverse markets with consistent identity) AND asymmetric extraction (Hasbro captures disproportionate value through royalties, approval leverage, and the ability to set terms unilaterally). The observer notes that restrictions on licensee innovation create welfare loss (fewer product variants, higher prices) while protecting Hasbro's strategic control. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.79.
constraint_indexing:constraint_classification(hasbro_licensing_restriction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hasbro_licensing_restriction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hasbro_licensing_restriction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hasbro_licensing_restriction, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hasbro_licensing_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hasbro_licensing_restriction, TR),
    TR >= 0.70.

:- end_tests(hasbro_licensing_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, trending upward. Hasbro uses licensing to capture value beyond direct production — royalties (typically 8-15% of wholesale), approval authority over product design, and the ability to redirect licensees' innovation toward Hasbro's strategic priorities. The increase from 0.35 to 0.58 over the 30-year interval reflects Hasbro's growing IP portfolio value and increased consolidation of licensing authority (centralized approval processes, more detailed brand guidelines). Suppression (0.65): Moderate-high. Licensees face significant barriers to exit: sunk costs in manufacturing tooling and supply chain relationships, consumer brand recognition tied to the IP, and the absence of comparable alternative IP portfolios with equivalent market reach. Smaller licensees have no realistic alternative source of established IP with proven consumer demand. Large licensees can walk away but at significant cost. Theater ratio (0.48): Moderate. Hasbro's approval process includes genuine quality control (preventing knockoff products, protecting manufacturing standards) but also performative elements (brand guideline compliance reviews that catch few substantive issues, multi-month review cycles that do not correlate with actual quality outcomes, aesthetic approval that reflects subjective brand taste rather than measurable quality). The increase from 0.35 to 0.48 reflects that procedural overhead has increased as approval authority has been formalized and centralized.
 *
 * PERSPECTIVAL GAP:
 *   This constraint displays a stark perspectival split driven by exit options and structural position. Dependent licensees (powerless/trapped) see a snare — they have invested heavily, have no exit, and face extraction through approval delays, royalty renegotiation, and exclusivity restrictions. Large corporations (powerful/arbitrage) see a tangled rope — they receive genuine market-access coordination benefits (Hasbro's IP attracts consumers and retail shelf space), but also face extraction through royalties and approval requirements. Hasbro (institutional/arbitrage) sees pure coordination — the licensing mechanism solves Hasbro's scaling problem without direct production risk. The ecosystem (organized/constrained) sees constrained innovation and higher prices, but also depends on brand consistency that Hasbro's restrictions help maintain. The analytical observer notes that the constraint bundles legitimate coordination (brand identity protection, quality assurance) with rent extraction (pricing power from limited IP alternatives, approval authority used to bias licensee strategy toward Hasbro's interests). The perspectival gap emerges because exit options differ radically: Hasbro can unilaterally change terms; large licensees can exit; small licensees cannot.
 *
 * DIRECTIONALITY LOGIC:
 *   Hasbro IP Management: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Hasbro experiences the constraint as beneficial; it solves their scaling problem. Small-to-medium licensees: Victim + trapped → d≈0.92, f(d)≈1.40. High extraction. No exit option and high dependence on Hasbro IP. Large corporate licensees: Mixed (both benefit and bear costs) + arbitrage → d≈0.48, f(d)≈0.60. Moderate extraction. They benefit from market access but face royalty and approval costs. Consumer ecosystem: Victim + constrained → d≈0.65, f(d)≈1.00. Moderate extraction. The ecosystem has limited ability to influence terms but can exert pressure through retail channel dynamics. The directionality spread (d from 0.08 to 0.92) reflects the structural heterogeneity of the constraint — it is genuinely beneficial for Hasbro and genuinely extractive for dependent licensees.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolution hinges on distinguishing legitimate coordination (brand integrity protection, quality assurance, market expansion) from rent extraction (leveraging IP scarcity to capture disproportionate value, using approval authority to bias licensee strategy). The constraint is NOT pure coordination (rope) because Hasbro's dominant position allows it to extract beyond the coordination surplus — licensees bear extraction costs they would not bear in a competitive licensing market. The constraint is NOT pure extraction (snare) because licensing genuinely provides coordination benefits: Hasbro gains market reach, licensees gain access to established IP with consumer recognition, and consumers gain consistent brand experience. The constraint IS tangled rope: it solves a real coordination problem (bringing diverse producers into coherent brand ecosystem) while enabling asymmetric extraction (Hasbro captures disproportionate value through royalties, approval leverage, and strategic control). The mandatrophy is resolved by recognizing that the coordination function is real but not sufficient to justify the extraction level — a more competitive licensing market would preserve coordination benefits while reducing Hasbro's rents. The theater component (0.48) suggests that some approval overhead is performative rather than functional, but not dramatically so; Hasbro is not primarily a theatrical constraint, though some procedures appear to be bureaucratic rather than value-adding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_restriction_threshold,
    'What level of licensee restriction maximizes ecosystem welfare vs. Hasbro IP protection? Is the current level extractive rent-seeking or legitimate brand stewardship?',
    'Comparative analysis of licensing regimes: restrictive (current Hasbro model) vs. permissive (e.g., Creative Commons licensing, open IP frameworks); measurement of product innovation rates, price elasticity, and brand degradation across regimes',
    'If current restrictions significantly exceed the welfare-maximizing threshold: constraint is primarily extractive (Snare for licensees). If current restrictions align with best practices: constraint is primarily coordinating (Rope for all parties).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_restriction_threshold, empirical, 'Whether current restriction levels maximize ecosystem welfare or represent rent-seeking').

omega_variable(
    licensee_coalition_viability,
    'Can licensees collectively negotiate terms, or does Hasbro''s market power prevent coalition formation (divide-and-conquer licensing)?',
    'Historical analysis of licensee consortium formation; examination of contract terms across comparable-size licensees; interviews with mid-market licensees on negotiation leverage; assessment of switching costs (brand recognition, tooling specificity)',
    'If coalition possible: licensees have constrained but meaningful bargaining power, constraint is Tangled Rope. If coalition is prevented: licensees are truly powerless, constraint is Snare for nearly all.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensee_coalition_viability, empirical, 'Whether licensees can form effective coalitions to negotiate better terms').

omega_variable(
    brand_degradation_empirical_risk,
    'Is Hasbro''s restriction level justified by measurable brand degradation risk, or is it speculative/precautionary?',
    'Archival analysis of past Hasbro IP damage from unlicensed or poorly-controlled derivative products; comparison to competing brands (e.g., Nintendo, Marvel) with different licensing regimes; measurement of brand equity loss correlated with licensing openness',
    'If empirical risk is high: restrictions are legitimate coordination mechanisms. If risk is low/speculative: restrictions are primarily wealth extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brand_degradation_empirical_risk, empirical, 'Whether restriction levels correspond to measurable brand degradation risk').

omega_variable(
    counterfactual_ecosystem_value,
    'How much would consumer surplus and licensee profit increase if restrictions were loosened or eliminated?',
    'Economic modeling of licensing regimes with varying restriction levels; consumer choice experiments; case study of IP markets with lower barriers (e.g., fan-created content, open-source toy designs)',
    'If counterfactual value is high: constraint is primarily extractive, redistribution is beneficial. If counterfactual value is low: restrictions provide genuine coordination benefits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_ecosystem_value, empirical, 'Estimated consumer and licensee welfare gains from relaxed restrictions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hasbro_licensing_restriction, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hasbro_tr_t0, hasbro_licensing_restriction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hasbro_tr_t15, hasbro_licensing_restriction, theater_ratio, 15, 0.42).
narrative_ontology:measurement(hasbro_tr_t30, hasbro_licensing_restriction, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(hasbro_be_t0, hasbro_licensing_restriction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hasbro_be_t15, hasbro_licensing_restriction, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(hasbro_be_t30, hasbro_licensing_restriction, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hasbro_licensing_restriction, resource_allocation).
narrative_ontology:affects_constraint(hasbro_licensing_restriction, intellectual_property_market_concentration).
narrative_ontology:affects_constraint(hasbro_licensing_restriction, toy_industry_consolidation).

% DUAL FORMULATION NOTE:
% Hasbro licensing restriction is downstream of broader IP market concentration and toy industry consolidation. The restriction mechanism depends on Hasbro's market power, which in turn depends on IP portfolio value and limited comparable alternatives. Changes to upstream IP market competition would alter the constraint's extraction potential.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hasbro_licensing_restriction, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
