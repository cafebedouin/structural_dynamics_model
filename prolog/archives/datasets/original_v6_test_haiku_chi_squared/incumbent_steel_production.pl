% ============================================================================
% CONSTRAINT STORY: incumbent_steel_production
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_incumbent_steel_production, []).

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
 *   constraint_id: incumbent_steel_production
 *   human_readable: Incumbent Blast Furnace Steel Production Method
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The global steel industry's dependence on blast furnace technology
 *   represents a structural extraction mechanism that operates through
 *   capital lock-in, supply chain dominance, and technical standardization
 *   rather than through explicit legal prohibition. The blast furnace method
 *   requires high-grade iron ore and coking coal, both controlled by a
 *   concentrated set of integrated producers and mining companies.
 *   Alternative production methods—electric furnaces using recycled scrap,
 *   hydrogen direct reduction, plasma-based production—are technically mature
 *   but face systematic suppression through financing barriers, technical
 *   standard gatekeeping, lack of green hydrogen infrastructure, and
 *   incumbent control of R&D investment. The constraint extracts from
 *   emerging economy steelmakers without ore access, scrap recyclers,
 *   hydrogen technology developers, and climate transition actors, while
 *   benefiting integrated producers and ore/coal suppliers. The
 *   extractiveness has increased over the 50-year observation window (0.42 →
 *   0.58) as alternative technologies became technically viable but remained
 *   economically locked out, indicating that suppression has intensified
 *   rather than relaxed as alternatives have matured.
 *
 * KEY AGENTS:
 *   - Integrated Steel Producers (Vale, Rio Tinto, ArcelorMittal, Nippon Steel): Primary beneficiaries (institutional/arbitrage) — capture rents through control of coking coal and high-grade ore supply chains, market concentration, and technical standard setting
 *   - Coking Coal Suppliers (Glencore, Teck, Arch Resources): Primary beneficiaries (institutional/arbitrage) — locked-in demand from blast furnace dominance; extraction through limited supply and monopolistic pricing
 *   - Emerging Economy Steelmakers (India, Vietnam, Indonesia, Turkey): Primary victims (powerless/trapped) — cannot access high-grade ore deposits, forced to import at premium prices; lack capital for alternative furnace technology; structurally locked into higher cost structure
 *   - Scrap Steel Recyclers and Electric Furnace Operators (Nucor, Tenaris, regional recyclers): Secondary victims (moderate/constrained) — technically superior option (lower capital intensity, lower emissions) blocked by capital cost asymmetry, procurement standard bias, and financing barriers
 *   - Hydrogen Reduction Technology Developers (H2 Green Steel, Boston Metal, HYBRIT): Secondary victims (moderate/constrained) — technology mature but faces infrastructure barriers, lack of green hydrogen supply, no funding advantage vs incumbent R&D
 *   - Climate Transition Coalition (UNEP, IEA, national governments, corporate net-zero commitments): Organized victims (organized/constrained) — locked into slow transition by incumbent capital depreciation cycles; cannot overcome suppression without major policy intervention
 *   - Regulatory Bodies and Standard-Setting Organizations (ISO, ASTM, national steel standards): Mixed institutional actors (institutional/constrained) — benefit from standardization coordination but pay cost of lock-in to suboptimal incumbent technology
 *   - Blast Furnace Institution: Historical form (institutional/arbitrage) — persists through 200+ years of institutional accumulation despite technical obsolescence in many applications (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incumbent_steel_production, 0.58).
domain_priors:suppression_score(incumbent_steel_production, 0.68).
domain_priors:theater_ratio(incumbent_steel_production, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incumbent_steel_production, extractiveness, 0.58).
narrative_ontology:constraint_metric(incumbent_steel_production, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(incumbent_steel_production, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(incumbent_steel_production, snare).
narrative_ontology:human_readable(incumbent_steel_production, "Incumbent Blast Furnace Steel Production Method").
narrative_ontology:topic_domain(incumbent_steel_production, "technological/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(incumbent_steel_production, integrated_steel_producers).
narrative_ontology:constraint_beneficiary(incumbent_steel_production, coking_coal_suppliers).
narrative_ontology:constraint_beneficiary(incumbent_steel_production, high_grade_ore_suppliers).
narrative_ontology:constraint_victim(incumbent_steel_production, emerging_economy_steelmakers).
narrative_ontology:constraint_victim(incumbent_steel_production, scrap_steel_recyclers).
narrative_ontology:constraint_victim(incumbent_steel_production, hydrogen_reduction_startups).
narrative_ontology:constraint_victim(incumbent_steel_production, climate_transition_actors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING ECONOMY STEELMAKERS WITHOUT ORE ACCESS (SNARE) — Cannot exit the blast furnace supply chain due to capital constraints and geographic iron ore scarcity. Must purchase coking coal and high-grade ore at monopolistic prices controlled by incumbent suppliers. Structural victim with no alternative pathway. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.96.
constraint_indexing:constraint_classification(incumbent_steel_production, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SCRAP STEEL RECYCLERS AND ELECTRIC FURNACE OPERATORS (SNARE) — Constrained by blast furnace dominance in capital markets and industry standards. Electric furnace technology exists but faces structural disadvantages: cannot compete on cost due to electricity pricing, cannot access premium markets where blast furnace provenance is preferred, face financing barriers because incumbent operators control investment flows. d≈0.82, f(d)≈1.20, σ=1.0 → χ≈0.70.
constraint_indexing:constraint_classification(incumbent_steel_production, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HYDROGEN REDUCTION TECHNOLOGY DEVELOPERS (SNARE) — Technology exists (direct reduced iron via hydrogen) but cannot achieve scale due to suppression of alternatives. Face intellectual property barriers, lack of supply infrastructure for green hydrogen, industrial procurement standards written for blast furnace output, and incumbent control of R&D funding and pilot plant access. The constraint operates through technical standards and market structure rather than legal prohibition. d≈0.78, f(d)≈1.12, σ=1.1 → χ≈0.77.
constraint_indexing:constraint_classification(incumbent_steel_production, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: CLIMATE TRANSITION COALITION (ORGANIZED) — International climate targets require 30-40% reduction in steel sector emissions by 2050, but incumbent blast furnace infrastructure has 50+ year capital depreciation cycles. Coalition (governments, climate NGOs, corporate carbon commitments) is victim of lock-in: cannot accelerate transition without absorbing stranded asset costs. Can partially organize exit (carbon pricing, green procurement mandates) but faces coordinated supplier resistance and incumbent lobbying. d≈0.68, f(d)≈0.95, σ=1.2 → χ≈0.67.
constraint_indexing:constraint_classification(incumbent_steel_production, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTEGRATED STEEL PRODUCERS AND MAJOR ORE/COAL SUPPLIERS (ROPE) — Primary beneficiaries experiencing the constraint as coordination. The blast furnace system solves a genuine problem: proven, scalable, zero-carbon baseline (though with high direct emissions), massive existing supply chain, mature financing. Beneficiaries have arbitrage exit (can shift marginally to emerging technologies while maintaining blast furnace dominance) and derive all major extraction benefits through standard economic rent capture, not through artificial suppression. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(incumbent_steel_production, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY BODIES AND INTERNATIONAL STANDARD-SETTING ORGANIZATIONS (TANGLED ROPE) — Simultaneously benefit from and bear costs of blast furnace standardization. Benefit: technical standards provide global coordination on safety and interoperability; regulators extract rents through certification requirements that favor incumbent producers. Bear costs: locked into standards that slow climate-aligned innovation; face pressure from climate commitments and ESG regulations that they themselves created; constrained by lobbying power of incumbent producers. d≈0.52, f(d)≈0.68, σ=1.1 → χ≈0.43.
constraint_indexing:constraint_classification(incumbent_steel_production, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: THE BLAST FURNACE AS HISTORICAL INSTITUTION (PITON) — From a 200+ year perspective, the blast furnace is a degraded institutional form maintained through inertia despite superior alternative technologies (electric furnaces for recycling, direct reduction for new production) being technically mature. The constraint persists not because blast furnaces are optimal but because the institutional, financial, and social infrastructure built around them cannot be rapidly displaced. theater_ratio=0.42 indicates moderate performative content: much marketing and standard-setting effort goes to legitimizing incumbent dominance despite acknowledged technical maturity of alternatives. Inertia explains persistence better than optimization.
constraint_indexing:constraint_classification(incumbent_steel_production, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / STEEL PRODUCTION NECESSITY (FALSE SUMMIT ATTEMPT) — Risk of naturalizing the constraint as inherent to steel production ('you cannot make steel without blast furnaces'). The structural data (ε=0.58, suppression=0.68, theater=0.42, requires_active_enforcement=false) contradicts a mountain classification. The constraint is a contingent lock-in enabled by capital depreciation cycles and incumbent market power, not a law of metallurgy. The engine's false summit detector will classify this as snare or tangled rope, not mountain.
constraint_indexing:constraint_classification(incumbent_steel_production, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incumbent_steel_production_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(incumbent_steel_production, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incumbent_steel_production, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(incumbent_steel_production, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(incumbent_steel_production, TR),
    TR >= 0.70.

:- end_tests(incumbent_steel_production_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint extracts through multiple channels: (1) ore/coal suppliers extract monopolistic rents through supply concentration; (2) incumbent producers extract through capital cost asymmetry (blast furnaces require $2-4B per plant, electric furnaces $500M-1B, but amortized cost per ton is lower for blast furnaces due to existing depreciation); (3) technical standards extract by requiring revalidation of alternative outputs; (4) lack of green hydrogen infrastructure extracts by making hydrogen DRI uneconomical; (5) financing barriers extract by directing capital toward proven incumbent technology. The 0.58 value reflects that extraction is substantial but not maximal — alternative technologies are technically proven (not suppressed to zero), and climate regulations are beginning to create countervailing forces (not pure snare). Suppression (0.68): Systematic barriers to exit include: (1) lack of ore access for emerging producers (geographic/geological); (2) lack of capital for technology transition ($2-4B per plant); (3) technical standards written for blast furnace output; (4) absence of green hydrogen supply chain; (5) incumbent control of financing, R&D, and procurement standards; (6) regulatory capture by incumbents in coal and ore producing regions; (7) pension fund and supplier ecosystem lock-in to blast furnace demand. Suppression is high but not total because alternative technologies exist, some capital is becoming available (climate finance, ESG investing), and some jurisdictions are actively supporting alternatives (EU, Scandinavia). Theater ratio (0.42): Moderate-low. The constraint operates primarily through real capital and resource barriers (structural), not through performative justifications (theater). Marketing and standard-setting efforts exist (legitimizing incumbent dominance) but represent a smaller share of constraint maintenance than capital lock-in. This low theater distinguishes the constraint from piton — the constraint persists because of real economic forces, not institutional inertia alone, though inertia is a secondary factor.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence driven by exit options and beneficiary/victim status. The integrated producer sees rope or even mountain (coordination function: proven technology at scale; natural limit: metallurgical requirements for certain steel grades). The emerging economy steelmaker sees pure snare (trapped by ore geography and capital barriers). The scrap recycler sees snare with constrained exit (technology exists but capital and procurement barriers prevent entry). The hydrogen developer sees snare (technology mature but infrastructure and financing barriers suppress scale). The climate coalition sees snare-to-tangled-rope (pure extraction from lock-in, but climate regulation creates countervailing power, so mixed). The regulatory body sees tangled rope (benefits from standard-setting coordination but pays cost of technology lock-in). The blast furnace as institution sees piton (historical form maintained by inertia despite technical alternatives). The analytical observer risks seeing mountain (naturalizing incumbent dominance as inherent to steel production) but the structural data reveals it as snare/tangled rope. The perspectival gaps are primarily driven by the asymmetry between beneficiaries (institutional, arbitrage) and victims (powerless-to-organized, trapped-to-constrained).
 *
 * DIRECTIONALITY LOGIC:
 *   Integrated producers + ore/coal suppliers: Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiaries; effective extraction is negative (they subsidize the system through capital investment and technology maintenance). Emerging economy steelmakers: Victims + trapped (ore geography, capital constraints) → d≈0.93, f(d)≈1.40. Maximum extraction; no exit options and no alternatives. Scrap recyclers: Victims + constrained (technology exists but capital barriers) → d≈0.82, f(d)≈1.20. High extraction; can exit partially through technology upgrade but face financing and standard barriers. Hydrogen developers: Victims + constrained (technology exists but infrastructure and financing barriers) → d≈0.78, f(d)≈1.12. Significant extraction; real technical pathway exists but suppressed by missing supply chains and incumbent control of R&D funding. Climate coalition: Victims + constrained (political power but facing incumbent resistance) → d≈0.68, f(d)≈0.95. Moderate-high extraction; have organizing power and regulation on side but constrained by capital depreciation cycles and incumbent lobbying. Regulators: Institutional + constrained (coordination benefits but locked into suboptimal standard) → d≈0.52, f(d)≈0.68. Moderate extraction; benefit from coordination function but pay cost of technology lock-in. Blast furnace institution: Institutional + arbitrage → d≈0.08, but piton classification from theater gate (0.42 ≥ 0.70 would be piton floor, but 0.42 < 0.70 so this is false piton — constraint maintains itself through capital, not inertia).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED VIA SNARE + VICTIM COALITION POWER: The constraint classifies as snare (ε=0.58 > 0.46, suppression=0.68 > 0.60, χ≈0.75 > 0.66 from powerless/trapped perspective). Mandatrophy resolution follows the 'dynamic coalition' extension: the primary victim group (emerging economy steelmakers) is powerless in isolation (d≈0.93 → snare), but alternative victims (climate coalition, hydrogen developers, scrap recyclers) are organized to moderate power. When these groups coalesce, the effective opposition to incumbent extraction rises to organized level (multiple moderate + organized agents). The coalition has access to regulatory power (climate targets, carbon pricing, procurement standards), research funding (climate finance), and corporate commitments (net-zero steel procurement). The snare classification persists (not converted to tangled rope) because the beneficiaries retain structural advantage: capital control, ore/coal monopoly, existing depreciated furnaces, established financing access. However, the coalition's emergence as an organized countervailing force creates realistic sunset conditions — climate regulation will eventually force transition regardless of incumbent suppression (estimated 20-40 year timeline). This is not a scaffold (which requires the constraint itself to have a sunset clause) but a snare with an external forcing function (climate regulation) that will overcome suppression. The mandatrophy is resolved by classifying the constraint as snare (accurate structural assessment) while documenting the coalition power that will eventually force exit despite continued incumbent dominance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hydrogen_cost_parity_threshold,
    'At what green hydrogen cost ($/kg) does hydrogen direct reduction achieve capital-neutral cost parity with incumbent blast furnace operations, enabling exit for alternative producers?',
    'Engineering cost curves for hydrogen DRI vs blast furnace; tracking of green hydrogen production costs; market pricing analysis as hydrogen electrolyzer capacity scales',
    'If threshold achieved within 10 years: scaffold perspective validated, constraint becomes temporary (sunset becomes real). If threshold requires 30+ years: snare classification persists; alternative producers remain trapped.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hydrogen_cost_parity_threshold, empirical, 'Threshold cost of green hydrogen enabling alternative production methods').

omega_variable(
    scrap_availability_and_recycling_limits,
    'Does global scrap steel availability sustainably support 100% electric furnace production, or does blast furnace production remain necessary for net new steel supply?',
    'Material flow analysis of scrap generation rates vs steel demand; analysis of virgin vs recycled content ratios; historical recycling rate trends; building stock lifecycle modeling',
    'If sustainable 100% recycling: electric furnace is true alternative (snare has genuine exit). If net new steel requires virgin ore: blast furnace is inescapable bottleneck (mountain-like for some perspectives). If partial recycling suffices: mixed tangled rope + snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scrap_availability_and_recycling_limits, empirical, 'Whether global scrap availability enables full transition away from virgin ore production').

omega_variable(
    incumbent_capital_depreciation_flexibility,
    'Can incumbent integrated producers accelerate retirement of blast furnace assets without triggering financial crises in supplier ecosystems and pension funds?',
    'Analysis of stranded asset write-downs; financial modeling of early asset retirement vs forced depreciation; pension fund exposure to coking coal and ore suppliers; banking sector stress tests for transition scenarios',
    'If flexible depreciation possible: climate coalition gains real exit option (snare → tangled rope or scaffold). If locked into 50-year cycles: incumbent producers can enforce slow transition, extending constraint period (snare persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_capital_depreciation_flexibility, empirical, 'Whether incumbent producers can absorb financial costs of accelerated asset retirement').

omega_variable(
    technical_standard_conversion_capability,
    'Can existing technical standards for blast furnace steel (carbon equivalent, inclusion profiles, grain structure specs) be modified to accept hydrogen DRI or electric furnace output without massive quality revalidation?',
    'Metallurgical comparison of output specs across production methods; review of automotive/construction/rail standards; testing programs for alternative-method compliance; industry adoption timelines',
    'If standards are convertible: regulatory barrier is reduced (snare becomes constrained rather than trapped). If revalidation required: regulatory gate creates 5-10 year delay for alternative entry (snare persists with high suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_standard_conversion_capability, empirical, 'Whether existing technical standards can accommodate alternative production methods').

omega_variable(
    political_economy_of_incumbent_defense,
    'What is the durability of incumbent producer lobbying power as climate regulation tightens, and at what carbon price do political coalitions flip toward accelerated transition?',
    'Analysis of lobbying spending and effectiveness over time; tracking of political support shifts (government subsidies, research funding allocation); carbon price sensitivity analysis for policy flip points',
    'If political economy is path-dependent and durable: incumbent suppression persists despite regulations (snare). If flips at realistic carbon prices ($80-150/ton): coalition gains political power to overcome suppression (snare → tangled rope with sunset).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_economy_of_incumbent_defense, preference, 'Durability of incumbent producer political power under tightening climate regulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(incumbent_steel_production, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(steel_tr_t0, incumbent_steel_production, theater_ratio, 0, 0.35).
narrative_ontology:measurement(steel_tr_t25, incumbent_steel_production, theater_ratio, 25, 0.38).
narrative_ontology:measurement(steel_tr_t50, incumbent_steel_production, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(steel_be_t0, incumbent_steel_production, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(steel_be_t25, incumbent_steel_production, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(steel_be_t50, incumbent_steel_production, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(incumbent_steel_production, resource_allocation).
narrative_ontology:affects_constraint(incumbent_steel_production, green_hydrogen_supply_infrastructure).
narrative_ontology:affects_constraint(incumbent_steel_production, coking_coal_phase_out).
narrative_ontology:affects_constraint(incumbent_steel_production, steel_sector_emissions_intensity).
narrative_ontology:affects_constraint(incumbent_steel_production, emerging_economy_manufacturing_cost).

% DUAL FORMULATION NOTE:
% The incumbent blast furnace constraint is upstream of multiple downstream constraints in the steel supply ecosystem. Upstream, it depends on coking coal availability and high-grade ore extraction. The constraint family decomposes into: (1) incumbent_steel_production (ε=0.58, snare) — the capital/supply lock-in mechanism; (2) green_hydrogen_supply_infrastructure (ε≈0.45, tangled_rope) — the coordination/extraction hybrid preventing hydrogen DRI scale; (3) coking_coal_phase_out (ε≈0.35, scaffold with sunset) — the regulatory pressure forcing incumbent transition. These three are linked: hydrogen infrastructure is blocked by incumbent suppression; coal phase-out will force hydrogen investment; forced investment changes incumbent extraction mechanism from snare to constrained.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(incumbent_steel_production, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
