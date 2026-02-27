% ============================================================================
% CONSTRAINT STORY: hasbro_licensing_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   Hasbro's licensing restrictions on intellectual property represent a
 *   structural constraint that blends genuine coordination (ensuring brand
 *   coherence across distributed manufacturing) with extractive enforcement
 *   (capturing surplus value from licensees and constrained consumers). The
 *   constraint operates through formal contract terms, audit mechanisms,
 *   approval rights, and termination clauses that restrict licensees in
 *   production capacity, design choices, pricing autonomy, and geographic
 *   markets. Over the past decade (interval 0-10), the extractiveness has
 *   increased from 0.38 to 0.52 as Hasbro has tightened approval processes
 *   and expanded audit frequency, while theater ratio has risen from 0.35 to
 *   0.48, indicating increasing performative justification (brand protection
 *   language) layered over extraction logic. The constraint exhibits all six
 *   DR types depending on the observer's structural position: for trapped
 *   manufacturers it is a snare; for competing licensees it is tangled rope;
 *   for Hasbro it is pure coordination; for consumers it is snare; for the IP
 *   regulatory regime it is a degraded piton; for the analytical observer it
 *   is primarily rope with elevated extraction overlay.
 *
 * KEY AGENTS:
 *   - Hasbro Corporate: Primary beneficiary (institutional/arbitrage) — captures brand coordination benefits and extraction rents through royalty structure and approval gatekeeping
 *   - Licensed Manufacturers (Toys, Apparel, Electronics): Primary victims (powerless/trapped, moderate/constrained) — bear production constraints, compliance costs, royalty obligations, and limited design autonomy; sunk investment in tooling creates switching cost
 *   - Consumer Choice: Secondary victim (powerless/trapped) — restricted product variety due to Hasbro's territorial restrictions and design veto power
 *   - Secondary Licensee Network: Mixed position (moderate/constrained) — benefits from shared brand infrastructure but constrained by tiering restrictions and approval delays
 *   - IP Regulatory Framework: Institutional actor (organized/constrained) — provides enforcement mechanism via patent/trademark law; maintains theater of brand protection while enabling extraction
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes licensing as fundamentally a coordination mechanism with accumulated rent-seeking overlay
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hasbro_licensing_restriction, 0.52).
domain_priors:suppression_score(hasbro_licensing_restriction, 0.58).
domain_priors:theater_ratio(hasbro_licensing_restriction, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hasbro_licensing_restriction, extractiveness, 0.52).
narrative_ontology:constraint_metric(hasbro_licensing_restriction, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hasbro_licensing_restriction, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hasbro_licensing_restriction, tangled_rope).
narrative_ontology:human_readable(hasbro_licensing_restriction, "Hasbro Licensing Restriction on Licensed Properties").
narrative_ontology:topic_domain(hasbro_licensing_restriction, "economic/intellectual_property").

domain_priors:requires_active_enforcement(hasbro_licensing_restriction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hasbro_licensing_restriction, hasbro_corporate).
narrative_ontology:constraint_victim(hasbro_licensing_restriction, licensed_manufacturers).
narrative_ontology:constraint_victim(hasbro_licensing_restriction, consumer_choice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LICENSED MANUFACTURER (SNARE) — Trapped by contractual terms that restrict production capacity, design choices, pricing autonomy, and geographic markets. Cannot exit without losing access to IP that represents 40-60% of product value. High suppression via contractual enforcement, audit rights, and termination clauses. Maximum extraction: manufacturer bears compliance costs, royalty payments, and opportunity costs while Hasbro retains upside optionality.
constraint_indexing:constraint_classification(hasbro_licensing_restriction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SECONDARY LICENSEE NETWORK (TANGLED ROPE) — Multiple licensees for apparel, electronics, collectibles, and digital products experience both coordination benefits (shared brand infrastructure, market development) and extraction (royalty tiering, approval delays, territorial restrictions). Constrained exit due to sunk investment in brand-specific tooling and relationships, but some switching capacity to alternative IP licenses. Moderate power through collective renegotiation and market signal.
constraint_indexing:constraint_classification(hasbro_licensing_restriction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: HASBRO CORPORATE (ROPE) — Benefits from coordination of distributed manufacturing and retail networks under single brand architecture. Experiences licensing restrictions as coordination mechanism: controls quality, prevents brand dilution, maintains pricing power. Arbitrage options available (in-house production, alternative licensee pools). Net beneficiary — extraction flows inward. Low experienced suppression from Hasbro's perspective.
constraint_indexing:constraint_classification(hasbro_licensing_restriction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSUMER CHOICE AND MARKET COMPETITION (SNARE) — Cannot exit the licensing restriction framework. Suppressed alternatives: consumers face reduced product variety due to Hasbro's territorial restrictions, design constraints imposed on licensees, and gatekeeping of competing IP licenses. Trapped in geographic markets where single-licensee dominance prevents price competition. High suppression via Hasbro's veto power over design innovation and production expansion.
constraint_indexing:constraint_classification(hasbro_licensing_restriction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: INDUSTRY REGULATORY FRAMEWORK (PITON) — Intellectual property protection via patent, trademark, and copyright law creates the institutional foundation for licensing restrictions. The regulatory theater is substantial: licensing agreements cite 'brand protection' and 'quality assurance,' but enforcement focuses on revenue extraction and market control. Theater ratio elevated because regulatory justification (consumer safety, brand coherence) masks extraction logic. Resistance to change is high due to institutional inertia in IP law, but the primary function (coordination) has degraded — modern supply chains coordinate through data and standards, not IP gatekeeping alone.
constraint_indexing:constraint_classification(hasbro_licensing_restriction, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a universal/civilizational frame, licensing restrictions solve genuine coordination problems: they align distributed producers with brand standards, prevent free-riding on brand reputation, and protect investment incentives for both IP creator and licensee. Viewed analytically, the constraint is primarily coordination with asymmetric information costs. However, the extractiveness value (0.52) indicates that this coordination has accumulated enough rent-seeking overlay to merit tangled_rope classification at the economic/institutional level.
constraint_indexing:constraint_classification(hasbro_licensing_restriction, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hasbro_licensing_restriction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hasbro_licensing_restriction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hasbro_licensing_restriction, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.52): Moderate-high. The licensing restrictions extract value through multiple channels: royalty rates (10-15% of wholesale typical), design constraint compliance costs, approval delay opportunity costs, and market power creation through territorial restrictions. However, extractiveness is not at snare threshold (0.66+) because genuine coordination functions persist — brand coherence across distributed manufacturing is a real problem that licensing solves. The value reflects that extraction is substantial but not the primary structural function. Suppression (0.58): Moderate-high. Licensees face significant barriers to exit through sunk investment in brand-specific tooling, contractual termination clauses with penalty provisions, and limited alternative IP sources with equivalent market value. Consumers face suppressed alternatives through territorial restrictions (Hasbro often grants exclusive licenses by region) and design constraints that prevent innovation. However, suppression is not absolute — some licensees do exit (licensing agreements are renegotiated when manufacturers switch to competing properties), and some consumers access alternative products. Theater ratio (0.48): Moderate. Licensing agreements cite 'brand protection,' 'quality assurance,' and 'consumer safety' as justifications for restrictions, but the actual enforcement focuses on royalty collection and market control. The theater has increased over the interval as Hasbro has added more performative language around brand standards while simultaneously tightening approval timelines and audit frequency. Theater is below piton threshold (0.70) because the coordination function remains substantive — Hasbro does invest in brand development and policing counterfeit products.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates a stark perspectival gap between Hasbro and its licensees. Hasbro experiences the licensing restrictions as a pure coordination mechanism (Rope) — they are solving the legitimate problem of maintaining brand coherence across hundreds of manufacturers and thousands of product SKUs. From Hasbro's perspective, royalties and approval authority are necessary tools for quality control and brand investment recovery. Licensed manufacturers experience the same restrictions as a mixed extraction-coordination constraint (Tangled Rope for large manufacturers, Snare for smaller ones) — they see the coordination benefits (shared brand development, market access) but experience the extraction through royalty obligations, design constraints, and approval delays. The gap widens for trapped manufacturers: a small toy producer with one major contract with Hasbro experiences the constraint as Snare (pure extraction with no exit), while a large consumer electronics firm with multiple brands experiences it as Tangled Rope (constrained but with switching capacity). Consumers experience the constraint as Snare (trapped in geographic markets with single-licensee dominance and suppressed design innovation). The regulatory framework pretends the constraint is Rope (pure coordination justified by brand protection) when it actually functions as Tangled Rope (mixed coordination and extraction). The analytical observer recognizes that all perspectives are partially correct — the constraint IS coordination (rope) AND extraction (snare/tangled_rope) simultaneously, with the balance shifting based on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Hasbro's directionality (d ≈ 0.05-0.15) is derived from its institutional power combined with arbitrage exit options — it can switch licensees, shift to in-house production, or license competitors if needed. The engine computes low d (full beneficiary) from these inputs. Licensed manufacturers' directionality (d ≈ 0.85-0.95 for trapped, d ≈ 0.55-0.65 for constrained) is derived from victim status (they bear extraction costs) combined with trapped or constrained exit options. The engine computes high d (target) from these inputs. The secondary licensee network has intermediate d (0.50-0.60) reflecting both victim and beneficiary positions. Consumers have high d (0.85-0.95) reflecting trapped status — they cannot exit the licensing restriction framework. The IP regulatory framework has low-to-moderate d (0.35-0.45) reflecting its institutional position as both beneficiary (from IP protection regime that enables licensing) and victim (constrained by path-dependent IP law that prevents more flexible alternatives). The perspectival gap derives from these different d values: beneficiaries (Hasbro, d low) experience low chi; victims (manufacturers, consumers, d high) experience high chi. The engine's sigmoid f(d) amplifies this gap — the same base extractiveness (ε=0.52) produces negative/low chi for beneficiaries and elevated chi for victims.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that licensing restrictions ARE both coordination AND extraction simultaneously. The mandatrophy trap would be: 'Is this a Rope (pure coordination) or a Snare (pure extraction)?' The tangled_rope classification with measured extraction (ε=0.52, suppression=0.58, theater=0.48) shows that the answer is neither pure type — it is a hybrid that solves a real coordination problem while extracting surplus value through market control. The key structural data: (1) Hasbro provides genuine coordination services (brand development, quality policing, market access), justifying some extraction as payment for these services; (2) Hasbro restricts licensee autonomy and geographic expansion beyond what coordination requires, extracting additional rent; (3) The theater ratio (0.48) indicates substantial but not dominant performative justification; (4) The measured increase in extractiveness over the interval (0.38→0.52) and theater (0.35→0.48) shows the constraint drifting toward pure extraction as Hasbro tightens controls and adds justificatory language. The constraint exhibits genuine Goodhart drift — as Hasbro's 'brand protection' language increases, the actual constraint becomes more extractive. Mandatrophy is resolved by measuring: the constraint is tangled_rope (not snare) because licensee exit is possible (some do exit, some renegotiate), because genuine coordination functions persist, and because theater remains below piton threshold. But it is NOT rope (pure coordination) because extraction is substantial, suppression is real, and Hasbro has significant arbitrage options that pure coordination would not require.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    royalty_extraction_threshold,
    'At what royalty rate does licensing transition from coordination cost-sharing to pure extraction?',
    'Comparative analysis of licensee profitability; correlation between royalty rates and licensee market exit rates; industry benchmark analysis across comparable IP licensing regimes',
    'If threshold is 8-12% of wholesale: current Hasbro rates (typically 10-15%) are at or above pure extraction threshold. If threshold is 15-20%: licensing remains substantially coordination-focused.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(royalty_extraction_threshold, empirical, 'Royalty rate threshold distinguishing coordination from extraction').

omega_variable(
    design_constraint_necessity,
    'How many of Hasbro''s design constraints on licensees serve brand protection versus market control?',
    'Analysis of constraint frequency and cost; licensee-reported competitive impact; comparison with industry-standard brand guidelines',
    'If >70% are brand-protective: snare classification weakened. If <40% are brand-protective: snare classification reinforced; extractive intent confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(design_constraint_necessity, conceptual, 'Whether design constraints serve brand protection or market control').

omega_variable(
    alternative_ip_availability,
    'Is the licensed IP sufficiently unique and valuable that licensees have no practical alternatives?',
    'Market analysis of substitute properties; licensee switching costs; time and investment required to build alternative brand relationships',
    'If substitutes are abundant: trap classification weakens; exit options improve. If unique: trap is genuine; powerless agent assessment confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_ip_availability, empirical, 'Whether alternative licensed properties provide viable substitutes').

omega_variable(
    brand_dilution_risk_magnitude,
    'How severe is the actual risk of brand damage from unrestricted licensee autonomy?',
    'Historical analysis of brand damage from licensee failures; comparison with brands using looser licensing controls; consumer perception studies across product categories',
    'If risk is high: licensing restrictions are functionally necessary (rope/coordination). If risk is low: restrictions are primarily extractive (snare/tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brand_dilution_risk_magnitude, empirical, 'Magnitude of brand dilution risk from unrestricted licensee autonomy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hasbro_licensing_restriction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hasbro_tr_t0, hasbro_licensing_restriction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hasbro_tr_t5, hasbro_licensing_restriction, theater_ratio, 5, 0.41).
narrative_ontology:measurement(hasbro_tr_t10, hasbro_licensing_restriction, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(hasbro_be_t0, hasbro_licensing_restriction, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hasbro_be_t5, hasbro_licensing_restriction, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(hasbro_be_t10, hasbro_licensing_restriction, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hasbro_licensing_restriction, information_standard).
narrative_ontology:affects_constraint(hasbro_licensing_restriction, toy_industry_market_concentration).
narrative_ontology:affects_constraint(hasbro_licensing_restriction, apparel_supply_chain_control).

% DUAL FORMULATION NOTE:
% Hasbro's licensing restrictions should be decomposed into two stories if analysis reveals that the coordination function and extraction function operate via different mechanisms or have significantly different ε values across observable contexts. The current story assumes a unified constraint; future analysis may reveal separate stories for brand-protection coordination (lower ε) and market-control extraction (higher ε).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hasbro_licensing_restriction, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
