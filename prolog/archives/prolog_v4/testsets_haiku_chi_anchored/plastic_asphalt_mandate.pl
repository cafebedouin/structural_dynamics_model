% ============================================================================
% CONSTRAINT STORY: plastic_asphalt_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plastic_asphalt_mandate, []).

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
 *   constraint_id: plastic_asphalt_mandate
 *   human_readable: Government Mandate for Plastic-Infused Asphalt
 *   domain: economic/environmental/political
 *
 * SUMMARY:
 *   A government mandate requiring plastic-infused asphalt represents a
 *   collision between circular economy policy and transportation
 *   infrastructure reality. The constraint exhibits structural tension
 *   between legitimate coordination (diverting plastic waste from landfills,
 *   reducing virgin material demand) and extraction mechanisms (forcing
 *   costly material adoption, technology immaturity, shifting quality risk to
 *   municipalities). The mandate creates asymmetric costs: contractors and
 *   municipalities bear upfront adaptation expenses and durability risk; the
 *   plastic recycling industry captures steady market access; environmental
 *   agencies claim carbon reduction; road durability itself becomes a trapped
 *   victim in an unverified experimental system. The theater ratio is
 *   moderate (0.58) and rising: initial compliance involves extensive
 *   specification development, testing protocol expansion, and performative
 *   certification—but underlying long-term durability remains unverified. The
 *   constraint is neither pure coordination (the recycling benefit is real)
 *   nor pure extraction (municipalities do gain waste diversion), making it a
 *   Tangled Rope. However, the vulnerability to durability failure and the
 *   potential for asymmetric cost absorption across rich and poor
 *   jurisdictions elevate extraction severity. The mandatrophy is unresolved:
 *   it depends on whether plastic-asphalt truly delivers the promised
 *   durability and carbon benefit, or whether these are green theater masking
 *   industry rent-seeking.
 *
 * KEY AGENTS:
 *   - Municipal Governments: Primary victims (powerless/trapped) — must comply or lose funding; absorb higher material costs and durability risk
 *   - Asphalt Contractors: Secondary victims (moderate/trapped) — forced to adopt new specifications; face liability if materials fail; cannot exit without losing contracts
 *   - Plastic Recycling Industry: Primary beneficiary (organized/constrained) — guaranteed demand for feedstock; captures market access premium; active enforcement required to maintain mandate against cheaper alternatives
 *   - Environmental Regulator: Secondary beneficiary (institutional/arbitrage) — solves plastic waste disposition; can adjust mandate parameters; sees coordination benefit
 *   - Established Asphalt Materials Suppliers: Mixed (powerful/mobile) — R&D leaders capture competitive rents; smaller suppliers face consolidation; can relocate or pivot
 *   - Transportation Infrastructure Coalition: Organized victims (organized/constrained) — bears implementation costs; sees mandate as temporary (scaffold logic) with sunset when technology matures
 *   - Road Quality Standards Bodies: Institutional actors (institutional/arbitrage) — expand testing protocols (theater increase); maintain authority over specifications; benefit from extended verification cycles
 *   - Road Infrastructure Durability: Structural victim (analytical/analytical) — trapped; long-term performance unverified; absorbs risk of premature degradation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plastic_asphalt_mandate, 0.52).
domain_priors:suppression_score(plastic_asphalt_mandate, 0.48).
domain_priors:theater_ratio(plastic_asphalt_mandate, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plastic_asphalt_mandate, extractiveness, 0.52).
narrative_ontology:constraint_metric(plastic_asphalt_mandate, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(plastic_asphalt_mandate, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plastic_asphalt_mandate, tangled_rope).
narrative_ontology:human_readable(plastic_asphalt_mandate, "Government Mandate for Plastic-Infused Asphalt").
narrative_ontology:topic_domain(plastic_asphalt_mandate, "economic/environmental/political").

domain_priors:requires_active_enforcement(plastic_asphalt_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plastic_asphalt_mandate, plastic_recycling_industry).
narrative_ontology:constraint_beneficiary(plastic_asphalt_mandate, government_environmental_agencies).
narrative_ontology:constraint_beneficiary(plastic_asphalt_mandate, waste_management_operators).
narrative_ontology:constraint_victim(plastic_asphalt_mandate, asphalt_contractors).
narrative_ontology:constraint_victim(plastic_asphalt_mandate, municipalities_with_tight_budgets).
narrative_ontology:constraint_victim(plastic_asphalt_mandate, road_durability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MUNICIPAL GOVERNMENT (SNARE) — Trapped by mandate; must comply or face regulatory penalties and loss of federal/state road funding. Has no exit option. Bears cost of higher material complexity, quality control variance, and potential durability issues without cost recovery. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ASPHALT CONTRACTOR (SNARE) — Forced to adopt new material specifications, equipment modifications, and quality testing protocols. Cannot exit without losing contracts. Faces liability exposure if plastic-infused asphalt fails prematurely. Bears extraction through compliance costs and reduced profit margins. d≈0.88, f(d)≈1.35, σ=0.9 → χ≈0.63.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: PLASTIC RECYCLING INDUSTRY (TANGLED ROPE) — Primary beneficiary. Mandate creates guaranteed demand for recycled plastic feedstock, enabling industrial scaling and profitability. Coordination function: solves the plastic waste disposition problem. Extraction mechanism: mandates market access at favorable pricing relative to virgin materials. Active enforcement required to prevent contractors from substituting cheaper/proven alternatives. d≈0.18, f(d)≈0.05, σ=1.2 → χ≈0.03. Net beneficiary; low effective extraction.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ENVIRONMENTAL REGULATOR (ROPE) — Coordinates plastic waste diversion and carbon reduction goals with infrastructure maintenance. Sees mandate as enabling mechanism for circular economy. High-level arbitrage: can adjust percentages, material specifications, or timeline if evidence warrants. d≈0.12, f(d)≈-0.06, σ=1.0 → χ≈-0.03. Net beneficiary; negative effective extraction.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ESTABLISHED ASPHALT MATERIALS SUPPLIER (TANGLED ROPE) — Coordinating function: mandate creates formulation innovation and supplier diversification. Extraction mechanism: firms with R&D capacity to develop plastic-compatible binders gain competitive advantage; smaller suppliers cannot compete, facing market consolidation. Mobile exit: can relocate operations or pivot to adjacent markets. d≈0.42, f(d)≈0.42, σ=0.9 → χ≈0.16. Moderate extraction; specialized suppliers capture rents from patent portfolios and proprietary knowledge.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: TRANSPORTATION INFRASTRUCTURE COALITION (SCAFFOLD) — Organized actors (road associations, contractor guilds, engineering firms) view mandate as temporary coordination structure with explicit sunset: as plastic-asphalt technology matures and cost premiums decline, the mandate can be relaxed into a market mechanism. Current enforcement is necessary because market alone would not internalize environmental costs. Sees the mandate as a bridging mechanism (scaffolding) to build new supply chains and shift behavioral norms. d≈0.48, f(d)≈0.60, σ=0.9 → χ≈0.28. Moderate extraction but with clear sunset logic.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ROAD QUALITY STANDARDS BODY (PITON) — Maintains and updates asphalt performance specifications (ASTM, PG grades). The plasticated asphalt mandate creates performative compliance: testing protocols expand to verify plastic content and durability, but the actual road performance benchmarks remain uncertain and contested. Theater ratio: standards bodies issue new testing requirements, but practitioners remain unsure whether plastic-asphalt truly meets long-term durability requirements. Institutional inertia: specifications are updated periodically, but underlying verification is weak. theater_ratio≈0.58 approaches piton threshold (≥0.70). d≈0.25, f(d)≈0.20, σ=1.0 → χ≈0.10.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ROAD INFRASTRUCTURE DURABILITY (SNARE) — Plastic-infused asphalt exhibits unknown long-term behavior under thermal cycling, ultraviolet degradation, and heavy vehicle loading. The collective epistemic gap (verification of durability over 20-year lifecycle) is a trapped victim: durability cannot voice costs, cannot exit, and absorbs the extraction cost if roads degrade prematurely. Early data is mixed; some studies show improved rutting resistance, others show reduced low-temperature flexibility. The victim here is future structural reliability and public safety. d≈0.94, f(d)≈1.42, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(plastic_asphalt_mandate, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plastic_asphalt_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(plastic_asphalt_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(plastic_asphalt_mandate, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(plastic_asphalt_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(plastic_asphalt_mandate, TR),
    TR >= 0.70.

:- end_tests(plastic_asphalt_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The mandate extracts from trapped municipalities and contractors through compliance costs, technology immaturity risk, and uncertain durability. However, extraction is not maximal (≥0.66 for snare) because the coordination function is real: plastic waste diversion is a genuine environmental benefit with measurable impact. The extractiveness rises over the interval (0.28 → 0.52) as specification complexity increases and cost data accumulate. Suppression (0.48): Moderate. Contractors and municipalities have limited alternatives (cannot legally refuse; regulatory penalties are severe), but suppression is not maximum because some technical flexibility exists in formulation, sourcing, and compliance timing. Over time, suppression may decline if alternative materials or extended timelines become permitted. Theater ratio (0.58): Moderate and rising. The mandate generates performative activity (new testing standards, certification programs, environmental messaging) that grows as implementation scales. Theater is rising because initial promise (simple solution to plastic waste) collides with technical complexity (plastic variability, durability uncertainty, cost volatility), requiring increasingly elaborate verification and communication infrastructure. Claimed type (Tangled Rope): Justification is the presence of both genuine coordination (waste diversion) and asymmetric extraction (cost burden on municipalities/contractors), plus required active enforcement (regulations, compliance monitoring, penalty mechanisms). The mandate cannot succeed through voluntary adoption; coercion is structural. However, it is not a pure snare because the beneficiary (recycling industry) is also providing a coordination function—diverting waste that would otherwise require landfill or incineration infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   The plastic-asphalt mandate exhibits sharp perspectival divergence across agents. The recycling industry sees rope (coordination, market access, profit opportunity). The environmental regulator sees rope (achieving waste diversion targets). Municipalities and contractors see snare (trapped compliance, cost burden, durability risk). The standards body sees piton (expanding but performative testing requirements). The durability victim sees snare (unverified risk, no exit, no agency). The transportation infrastructure coalition sees scaffold (temporary enforcement with sunset as technology matures). The perspectival gap reflects fundamental disagreement about whether the mandate is a solution (rope), a temporary bridge (scaffold), a degraded ritual (piton), or pure extraction (snare). The mandatrophy is embedded here: the true nature of the constraint depends on whether plastic-asphalt durability is verified or not—if durability holds, most perspectives collapse to rope or scaffold; if durability fails, snare becomes dominant.
 *
 * DIRECTIONALITY LOGIC:
 *   Municipal government: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; no exit path. Asphalt contractor: Victim + trapped → d≈0.88, f(d)≈1.35. High extraction; competitive survival depends on mandate compliance. Plastic recycling industry: Beneficiary + constrained → d≈0.18, f(d)≈0.05. Low effective extraction (appears as net beneficiary); needs mandate enforcement to prevent reversal to cheaper alternatives. Environmental regulator: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.06. Negative effective extraction (appears as net beneficiary); high policy flexibility. Established asphalt supplier: Mixed (powerful/mobile) → d≈0.42, f(d)≈0.42. Moderate extraction but with mobility option (can pivot business model or geography). Transportation coalition: Organized victim → d≈0.48, f(d)≈0.60. Moderate extraction but with organizational capacity to lobby for modifications; sees exit path (sunset via technology maturation). Standards body: Institutional + arbitrage → d≈0.25, f(d)≈0.20. Low effective extraction but benefits from extended authority and specification-setting power. Road durability: Analytical/trapped → d≈0.94, f(d)≈1.42. Maximum extraction; abstract structural victim with no voice or exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint illustrates the mandatrophy between Tangled Rope (coordination + extraction) and Snare (pure extraction). The resolution depends on empirical verification: (1) Does plastic-asphalt deliver 20+ year durability equivalent to or better than conventional asphalt? (2) Does the carbon/waste lifecycle benefit exceed the embodied carbon cost of new processing infrastructure and potential premature replacements? (3) Can plastic feedstock quality be standardized at scale? If YES to all three: the constraint resolves as Rope or Scaffold—the extraction costs are legitimate innovation expenses, the coordination function is genuine, and the mandate becomes a justified temporary intervention. If NO or INCONCLUSIVE: the constraint becomes a Snare disguised as green policy—municipalities absorb durability risk, contractors bear compliance costs, and the benefit flows primarily to recycling industry; the mandate persists through regulatory force, not because it works. Current data is mixed (some durability studies positive, others concerning; lifecycle analysis incomplete; feedstock quality variable). The mandate embeds the mandatrophy: it must be enforced to protect the recycling industry's market access, which suggests extraction; but enforcement is justified by environmental benefit claims, which suggest coordination. The empirical omega variables will resolve this. Until then, the constraint remains a contested Tangled Rope with real risk of snare classification if durability fails.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    plastic_asphalt_durability_verification,
    'Does plastic-infused asphalt maintain equivalent or superior long-term durability (20+ years) compared to conventional asphalt under the full range of climate and loading conditions?',
    'Longitudinal performance monitoring of mandated plastic-asphalt roads; controlled degradation studies under accelerated weathering and traffic simulation; post-mortem material analysis of failed sections',
    'If durability confirmed: mandate becomes rope (coordination); extraction costs are legitimate innovation prices. If durability fails: mandate becomes severe snare; municipalities face replacement costs and public safety liability. If inconclusive: mandate remains tangled_rope; uncertainty sustains extraction through continued enforcement and testing requirements.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(plastic_asphalt_durability_verification, empirical, 'Long-term performance equivalence of plastic-asphalt').

omega_variable(
    plastic_recycling_feedstock_quality,
    'Can recycled plastic waste streams (mixed colors, contamination, variable polymer composition) be processed into consistent, specification-compliant asphalt modifier without reducing road performance?',
    'Analysis of plastic waste sorting/purification economics; material property variance studies across feedstock batches; correlation between feedstock quality and asphalt performance outcomes',
    'If consistent quality achieved at scale: mandate is viable tangled_rope with moderate extraction. If feedstock quality remains variable: mandate drives price volatility and hidden extraction (contractors or recyclers absorb variance costs). If infeasible at scale: mandate targets impossible constraint, becoming a piton (theater without function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plastic_recycling_feedstock_quality, empirical, 'Consistency and quality of plastic feedstock for asphalt modification').

omega_variable(
    carbon_lifecycle_benefit_verification,
    'Does the carbon reduction from diverting plastic waste offset the embodied carbon cost of new collection infrastructure, processing energy, and potential premature road replacement cycles?',
    'Lifecycle assessment comparing conventional asphalt + landfilled plastic vs mandated plastic-asphalt + infrastructure expansion; accounting for collection, transport, reprocessing, and end-of-life road material',
    'If net carbon positive: environmental justification for mandate holds; extraction becomes legitimate coordination cost. If net carbon neutral or negative: mandate is false environmentalism (green theater); extraction is pure rent-seeking disguised as sustainability. Mandatrophy resolution depends on this omega''s outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_lifecycle_benefit_verification, empirical, 'Net carbon lifecycle benefit of plastic-asphalt mandate').

omega_variable(
    municipal_cost_absorption_capacity,
    'At what threshold of cost premium (material + processing + testing) does municipal noncompliance exceed the regulatory penalty, and what is the distribution of absorptive capacity across jurisdictions?',
    'Comparative cost accounting across municipalities of varying size and revenue capacity; analysis of stated compliance costs vs regulatory penalty schedules; identification of jurisdictions where penalty avoidance exceeds mandate compliance cost',
    'If compliance cost < penalty: mandate enforces through financial pressure; primarily affects small/poor municipalities (high extraction). If compliance cost > penalty for many jurisdictions: mandate becomes selective enforcement (unequal extraction); creates legal fragmentation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(municipal_cost_absorption_capacity, empirical, 'Municipal cost absorption capacity relative to compliance expenses').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plastic_asphalt_mandate, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pam_tr_t0, plastic_asphalt_mandate, theater_ratio, 0, 0.32).
narrative_ontology:measurement(pam_tr_t3, plastic_asphalt_mandate, theater_ratio, 3, 0.45).
narrative_ontology:measurement(pam_tr_t6, plastic_asphalt_mandate, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(pam_be_t0, plastic_asphalt_mandate, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pam_be_t3, plastic_asphalt_mandate, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(pam_be_t6, plastic_asphalt_mandate, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plastic_asphalt_mandate, resource_allocation).
narrative_ontology:affects_constraint(plastic_asphalt_mandate, plastic_waste_landfill_cap).
narrative_ontology:affects_constraint(plastic_asphalt_mandate, virgin_asphalt_carbon_tax).
narrative_ontology:affects_constraint(plastic_asphalt_mandate, recycled_material_procurement_standards).

% DUAL FORMULATION NOTE:
% The plastic-asphalt mandate is downstream of broader circular economy policy (plastic waste reduction targets, extended producer responsibility) but represents a distinct constraint focused on infrastructure adoption. Upstream constraints (landfill caps, virgin material taxation) create demand pressure; this constraint operationalizes that demand through mandated material incorporation. The durability verification omega is critical: if durability fails, this constraint may decompose into separate stories (one for waste diversion coordination, one for infrastructure extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(plastic_asphalt_mandate, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
