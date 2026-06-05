% ============================================================================
% CONSTRAINT STORY: toy_industry_consolidation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_toy_industry_consolidation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: toy_industry_consolidation
 *   human_readable: Toy Industry Consolidation and Market Gatekeeping
 *   domain: economic/antitrust/consumer_markets
 *
 * SUMMARY:
 *   The toy industry has consolidated dramatically over the past two decades,
 *   driven by retailer consolidation (Walmart, Target, Amazon dominance),
 *   manufacturing consolidation (Mattel-Hasbro merger, private equity
 *   buyouts), and supply chain integration. This constraint exhibits a core
 *   Tangled Rope structure: genuine coordination benefits (scale, inventory
 *   efficiency, reduced transaction costs) coexist with asymmetric extraction
 *   (margin compression, channel closure, supplier dependency). Independent
 *   toy makers face trapped barriers to distribution; consolidated retailers
 *   and manufacturers benefit from the consolidation's market power. The
 *   constraint's theater ratio (0.48) reflects that much of the 'efficiency'
 *   narrative masks pure extraction — exclusive shelf space and delisting
 *   threats are not coordination costs but gatekeeping mechanisms. The
 *   extractiveness trajectory (0.32 → 0.58) shows gradual intensification as
 *   consolidation deepened, particularly after major mergers and Amazon's
 *   retail dominance expansion.
 *
 * KEY AGENTS:
 *   - Independent Toy Makers: Primary victims (powerless/trapped) — face closed distribution channels, margin compression, and forced dependence on consolidated retailers or DTC channels with high customer acquisition costs
 *   - Mid-Tier Toy Manufacturers: Secondary victims (moderate/constrained) — coordinate with retailers but face asymmetric extraction through margin pressure, exclusivity demands, delisting threats, and supply chain dependencies
 *   - Consolidated Retailers (Walmart, Target, Amazon): Primary beneficiaries (institutional/arbitrage) — control primary distribution channels, dictate terms, capture scale benefits, maintain arbitrage options through supplier interchangeability
 *   - Consolidated Toy Manufacturers (Mattel, Hasbro, JAKKS Pacific): Secondary beneficiaries (institutional/arbitrage) — benefit from retailer partnerships, maintain market power through IP leverage and scale, coordinate with consolidated retailers
 *   - Independent Specialty Retailers: Tertiary victims (organized/constrained) — face supplier consolidation, reduced access to exclusive products, pricing pressure from mega-retailers, forced identity shifts toward general gift retail
 *   - Consumer Choice and Innovation Ecosystem: Abstract victim (powerless/trapped) — faces narrowed product palette, reduced niche innovation, limited adaptation to local preferences; no defender or exit option
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing consolidation as inevitable market equilibrium when it reflects regulatory and contractual choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(toy_industry_consolidation, 0.58).
domain_priors:suppression_score(toy_industry_consolidation, 0.65).
domain_priors:theater_ratio(toy_industry_consolidation, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(toy_industry_consolidation, extractiveness, 0.58).
narrative_ontology:constraint_metric(toy_industry_consolidation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(toy_industry_consolidation, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(toy_industry_consolidation, tangled_rope).
narrative_ontology:human_readable(toy_industry_consolidation, "Toy Industry Consolidation and Market Gatekeeping").
narrative_ontology:topic_domain(toy_industry_consolidation, "economic/antitrust/consumer_markets").

domain_priors:requires_active_enforcement(toy_industry_consolidation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(toy_industry_consolidation, mega_retailers).
narrative_ontology:constraint_beneficiary(toy_industry_consolidation, consolidated_toy_manufacturers).
narrative_ontology:constraint_victim(toy_industry_consolidation, independent_toy_makers).
narrative_ontology:constraint_victim(toy_industry_consolidation, consumers_limited_selection).
narrative_ontology:constraint_victim(toy_industry_consolidation, innovation_suppression).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT TOY MAKER (SNARE) — Small manufacturers face insurmountable barriers to shelf space and distribution. Consolidation has closed the primary retail channels; direct-to-consumer models face logistics costs and platform gatekeeping. The independent maker cannot exit the constraint without abandoning the market entirely. Maximum extraction experienced through forced margin compression and channel closure.
constraint_indexing:constraint_classification(toy_industry_consolidation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-TIER MANUFACTURER (TANGLED ROPE) — Medium-sized toy companies coordinate with consolidated retailers (shelf access, logistics) but face asymmetric extraction (margin pressure, exclusivity demands, delisting threats). They benefit from the consolidation's economies of scale but bear significant extraction costs. Exit options exist (direct-to-consumer, specialty channels) but incur substantial switching costs.
constraint_indexing:constraint_classification(toy_industry_consolidation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSOLIDATED RETAILER (ROPE) — Mega-retailers (Walmart, Target, Amazon) experience the consolidation as coordination: standardized SKUs, predictable supplier relationships, economies of scale in logistics and inventory. Extraction runs toward this agent; they benefit from both scale and supplier dependency. High arbitrage options — can switch suppliers easily or shift product mix.
constraint_indexing:constraint_classification(toy_industry_consolidation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSOLIDATED TOY MANUFACTURER (ROPE) — Mattel, Hasbro, and JAKKS Pacific coordinate with consolidated retailers through standardized relationships and volume commitments. Benefits from scale and market power. Maintains arbitrage options through diversification, geographic expansion, and entertainment IP leverage. Experiences the constraint as coordination enabling market dominance.
constraint_indexing:constraint_classification(toy_industry_consolidation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSUMER CHOICE AND INNOVATION (SNARE) — Children and parents face a narrowed product palette; innovative or niche toys struggle to reach market. The suppression of independent makers reduces novelty and adaptation to local preferences. The abstract collective good of toy innovation has no defender and no exit option. Bears the full cost of consolidation without agency or compensation.
constraint_indexing:constraint_classification(toy_industry_consolidation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: INDEPENDENT RETAILER COALITION (TANGLED ROPE) — Specialty toy stores and independent retailers coordinate with independent manufacturers but face consolidation extraction: reduced access to exclusive products, pressure from mega-retailer pricing, supplier consolidation limiting their leverage. Some coordination benefit exists (niche market positioning) but asymmetric extraction dominates. Exit options exist but require identity shift (from toy specialist to general gift retailer).
constraint_indexing:constraint_classification(toy_industry_consolidation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, consolidation is an immutable structural feature of mature manufacturing sectors: economies of scale are inherent to toy production (injection molding, supply chain complexity, retailer leverage). Some see consolidation as inevitable equilibrium. However, structural data contradicts the mountain classification — consolidation is a contingent outcome of regulatory choices (antitrust enforcement levels, merger approval standards), not a natural law. Engine will detect false summit.
constraint_indexing:constraint_classification(toy_industry_consolidation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(toy_industry_consolidation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(toy_industry_consolidation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(toy_industry_consolidation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(toy_industry_consolidation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(toy_industry_consolidation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significantly from independent makers through channel closure and margin compression, but the extraction is not maximal (snare-level) because some exit routes exist (DTC, niche channels, licensing to consolidators). The consolidated actors capture clear benefits. Suppression (0.65): High. Multiple barriers suppress independent competition: retailer gatekeeping, capital requirements for supply chain integration, IP licensing concentration, consumer preference for established brands (learned through consolidation-era marketing). But suppression is not absolute — new brands can emerge through viral marketing or platform leverage. Theater ratio (0.48): Moderate. The efficiency narrative for consolidation is partially genuine (scale benefits are real) but significantly performative — exclusive shelf space and delisting threats are pure extraction mechanism, not efficiency requirement. Claimed type (Tangled Rope): Justified. Real coordination function exists (shared inventory systems, predictable supplier relationships reduce transaction costs); asymmetric extraction is clear (independent makers bear full cost); active enforcement is required (contracts, exclusivity clauses, retailer discretion).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence driven by structural position. Consolidated retailers see coordination (Rope) — standardized relationships, scale benefits, predictable suppliers. Consolidated manufacturers see coordination (Rope) — market access, leverage with retailers, IP bundling advantage. Independent makers see trapping (Snare) — closed channels, forced dependence, margin compression. Mid-tier manufacturers see mixed extraction and coordination (Tangled Rope) — some leverage from scale partnerships, but asymmetric pressure from both retailers and mega-competitors. Specialty retailers see extraction (Tangled Rope) — shrinking supplier options, pricing pressure, forced diversification. The consumer innovation ecosystem is voiceless (Snare) — narrowed palette, reduced niche products. The analytical observer risks seeing inevitability (Mountain) but this is a false summit: consolidation reflects regulatory choices (merger approval, antitrust enforcement levels) and contractual structures (exclusivity), not immutable economic law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural positions: Independent makers (beneficiary status absent + trapped exit + victim status) → high d → high f(d) → high χ. Consolidated retailers (beneficiary status + arbitrage exit) → low d → low/negative f(d) → negative χ. Mid-tier manufacturers (mixed victim/partial beneficiary + constrained exit) → moderate d → moderate f(d) → moderate χ. The consolidated retailers experience the constraint as rope (low extraction experienced) while independent makers experience it as snare (maximum extraction). The perspectival gap is driven entirely by exit options and structural relationship, not by disagreement about metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that Tangled Rope is the correct classification from the analytical perspective, not a false compromise. The genuine coordination benefits (scale, inventory efficiency, transaction cost reduction) coexist with asymmetric extraction (channel gatekeeping, margin compression, supplier dependency). This is NOT snare (no beneficiary coordination function) and NOT rope (clear asymmetric extraction). The mandatrophy resolution hinges on recognizing that consolidated actors genuinely benefit from coordination mechanisms WHILE simultaneously extracting from trapped suppliers. Both are true. The theater ratio (0.48) supports this: the efficiency narrative is partially legitimate, but approximately half the observed 'efficiency' is actually gatekeeping theater masking pure extraction. The extraction becomes visible only from the trapped actor's perspective; the coordinating actor experiences genuine efficiency gains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consolidation_equilibrium_threshold,
    'Is the current consolidation level a stable equilibrium or an unstable overshoot that will revert when regulatory pressure increases?',
    'Historical pattern analysis of toy industry consolidation cycles; comparative analysis with other consumer goods sectors; projection of antitrust enforcement intensity over next 10 years',
    'If equilibrium: consolidation is structural and persists (Tangled Rope). If overshoot: consolidation is temporary correction vulnerable to policy change (Scaffold with sunset).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consolidation_equilibrium_threshold, empirical, 'Whether consolidation represents stable equilibrium or regulatory overshoot').

omega_variable(
    direct_to_consumer_viability,
    'Can direct-to-consumer channels (online platforms, crowdfunding, social commerce) create sufficient scale to enable independent toy makers to bypass traditional retailer consolidation?',
    'Tracking of successful DTC toy brands; analysis of customer acquisition costs and unit economics; comparison with traditional retail margins',
    'If viable: independent makers have real exit option, reducing effective extraction (constrained rather than trapped). If not: forced dependence on consolidated retailers persists (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(direct_to_consumer_viability, empirical, 'Whether DTC channels provide viable alternative to consolidated retailers').

omega_variable(
    ip_leverage_sustainability,
    'Can entertainment IP (Disney, Marvel, Pokémon) sustain consolidated manufacturer profitability indefinitely, or will licensed IP scarcity and bidding wars eventually create opening for independent innovation?',
    'Tracking of IP license costs over time; analysis of consumer preference shifts toward unlicensed vs licensed toys; identification of market segments where IP is not dominant',
    'If sustainable: consolidated manufacturers locked in (rope/arbitrage). If unsustainable: licensing cost inflation may force diversification and reduce consolidation advantages.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ip_leverage_sustainability, empirical, 'Whether IP leverage sustains consolidated toy manufacturer dominance').

omega_variable(
    suppression_mechanism_structural_vs_performative,
    'Is suppression of independent makers driven by genuine scale advantages (structural) or by deliberate gatekeeping and exclusionary contracting (performative enforcement)?',
    'Legal discovery analysis of retailer contracts; economic analysis of actual scale cost differentials vs contractual penalties; interview data from excluded suppliers',
    'If structural: consolidation reflects real efficiency gains, extraction justified as coordination cost. If performative: suppression is pure extraction, justifying stronger antitrust intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_performative, conceptual, 'Whether suppression of independent makers is structural necessity or performative gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(toy_industry_consolidation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(toy_cons_tr_t0, toy_industry_consolidation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(toy_cons_tr_t10, toy_industry_consolidation, theater_ratio, 10, 0.42).
narrative_ontology:measurement(toy_cons_tr_t20, toy_industry_consolidation, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(toy_cons_be_t0, toy_industry_consolidation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(toy_cons_be_t10, toy_industry_consolidation, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(toy_cons_be_t20, toy_industry_consolidation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(toy_industry_consolidation, resource_allocation).
narrative_ontology:boltzmann_floor_override(toy_industry_consolidation, 0.18).
narrative_ontology:affects_constraint(toy_industry_consolidation, retail_supply_chain_concentration).
narrative_ontology:affects_constraint(toy_industry_consolidation, entertainment_ip_licensing_consolidation).
narrative_ontology:affects_constraint(toy_industry_consolidation, consumer_goods_antitrust_enforcement_capacity).

% DUAL FORMULATION NOTE:
% Toy industry consolidation is downstream of broader retail consolidation (mega-retailers) and entertainment IP concentration (Disney, Hasbro merger effects). The toy constraint has distinct extractiveness because the mechanisms are specific (shelf space gatekeeping, SKU exclusivity) even though the upstream constraints create enabling conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
