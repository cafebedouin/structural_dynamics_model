% ============================================================================
% CONSTRAINT STORY: ocean_governance_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ocean_governance_fragmentation, []).

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
 *   constraint_id: ocean_governance_fragmentation
 *   human_readable: Ocean Governance Fragmentation
 *   domain: international_environmental/maritime_governance
 *
 * SUMMARY:
 *   Ocean governance fragmentation represents the tension between state
 *   sovereignty and the biological reality of marine ecosystems that operate
 *   at transnational scales. The constraint operates across multiple levels —
 *   the global commons (high seas, Areas beyond national jurisdiction),
 *   exclusive economic zones (EEZs) claimed by coastal states, and regional
 *   seas governed by sectoral organizations. No unified authority governs
 *   these zones; instead, overlapping and conflicting regimes create
 *   opportunities for extraction by powerful actors while leaving the marine
 *   commons and vulnerable communities with no exit. The fragmentation
 *   produces genuine coordination benefits (scientific data sharing, minimum
 *   standards for fishing equipment, pollution controls) but enables
 *   extractive asymmetries: capital-intensive industries arbitrage between
 *   weak-enforcement zones; major coastal states claim vast EEZs; artisanal
 *   communities face competition from industrial fleets operating under flags
 *   of convenience. The constraint has intensified over the past two decades
 *   as extraction technology has improved faster than governance capacity,
 *   and as new frontiers (deep-sea mining, Arctic access) have opened under
 *   even weaker jurisdiction. Reform efforts (High Seas Treaty, marine
 *   protected area networks, Blue Economy frameworks) represent scaffold
 *   mechanisms building alternative pathways, but these face suppression from
 *   incumbent beneficiaries and institutional inertia from traditional
 *   flag-state systems.
 *
 * KEY AGENTS:
 *   - Global Marine Commons: Primary victim (powerless/trapped) — cannot exit fragmented system; bears costs of extractive competition without benefits
 *   - Artisanal Fishing Communities: Primary victim (powerless/trapped) — geographically and economically dependent on fishing grounds; face extraction from industrial competition under different jurisdictions
 *   - Small Island Developing States: Secondary victim (moderate/constrained) — experience mixed coordination and extraction through regional management organizations; limited enforcement capacity and limited voice in rule-setting
 *   - Major Coastal States (China, Russia, EU member states, US): Primary beneficiary (institutional/arbitrage) — benefit from extended jurisdiction claims and ability to regulate own waters; can exit through bilateral agreements or withdrawal
 *   - Extractive Industries (large-scale fishing, shipping, deep-sea mining corporations): Primary beneficiary (institutional/arbitrage) — benefit from regulatory arbitrage; high mobility across jurisdictions
 *   - Environmental and Indigenous Coalitions: Secondary actor (organized/constrained) — organized but constrained; expertise extracted without proportional authority; building reform coalitions
 *   - Ocean Governance Reform Initiatives: Organized reformer (organized/mobile) — see fragmentation as temporary problem with sunset; building new institutional pathways
 *   - Traditional International Law Institutions: Institutional actor (institutional/arbitrage) — UNCLOS and flag state systems persist through inertia despite degraded function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ocean_governance_fragmentation, 0.58).
domain_priors:suppression_score(ocean_governance_fragmentation, 0.65).
domain_priors:theater_ratio(ocean_governance_fragmentation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ocean_governance_fragmentation, extractiveness, 0.58).
narrative_ontology:constraint_metric(ocean_governance_fragmentation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ocean_governance_fragmentation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ocean_governance_fragmentation, tangled_rope).
narrative_ontology:human_readable(ocean_governance_fragmentation, "Ocean Governance Fragmentation").
narrative_ontology:topic_domain(ocean_governance_fragmentation, "international_environmental/maritime_governance").

domain_priors:requires_active_enforcement(ocean_governance_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ocean_governance_fragmentation, coastal_states).
narrative_ontology:constraint_beneficiary(ocean_governance_fragmentation, extractive_industries).
narrative_ontology:constraint_beneficiary(ocean_governance_fragmentation, fishing_corporations).
narrative_ontology:constraint_victim(ocean_governance_fragmentation, global_marine_commons).
narrative_ontology:constraint_victim(ocean_governance_fragmentation, small_island_developing_states).
narrative_ontology:constraint_victim(ocean_governance_fragmentation, artisanal_fishing_communities).
narrative_ontology:constraint_victim(ocean_governance_fragmentation, marine_ecosystem_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARINE COMMONS (SNARE) — The ocean's collective stability cannot exit the fragmented governance system. Bears full cost of extractive competition without coordination benefit. No alternative institutional arrangement available within the constraint's time horizon. Maximum extraction from an agent with zero degrees of freedom.
constraint_indexing:constraint_classification(ocean_governance_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ARTISANAL FISHING COMMUNITIES (SNARE) — Trapped by geographic and economic dependency on near-shore fishing grounds. Have no capacity to exit the constraint or influence the fragmented governance that permits industrial overfishing in adjacent zones. Suppressed by capital-intensive industrial competitors operating under different jurisdictional regimes. Structural extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(ocean_governance_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: SMALL ISLAND DEVELOPING STATES (TANGLED ROPE) — Constrained by limited enforcement capacity and economic dependence on fishing licenses. Experience both genuine coordination benefit (regional fisheries management organizations provide data and conservation norms) and asymmetric extraction (larger states and corporations set rules; SIDS enforce without proportional voice). Exit constrained by resource limitations, not material barriers.
constraint_indexing:constraint_classification(ocean_governance_fragmentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: MAJOR COASTAL STATES (ROPE) — Benefit from fragmented governance: can claim extended continental shelf, establish exclusive economic zones, and regulate their own waters with minimal external constraint. Experience the system as coordination (UNCLOS, regional organizations) without significant extraction. High exit capacity through arbitrage — can negotiate bilateral agreements or withdraw from regional bodies. Net beneficiary.
constraint_indexing:constraint_classification(ocean_governance_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EXTRACTIVE INDUSTRIES (ROPE) — Benefit from fragmentation's regulatory arbitrage: can relocate fishing fleets to zones with weaker enforcement, use flags of convenience for shipping, or apply for deep-sea mining licenses in under-regulated areas. Experience the system as enabling coordination (port state controls, flag state responsibility) without direct extraction cost. Arbitrage exit available — can move to more permissive jurisdictions.
constraint_indexing:constraint_classification(ocean_governance_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ENVIRONMENTAL AND INDIGENOUS ADVOCACY COALITIONS (TANGLED ROPE) — Organized actors (NGOs, indigenous rights groups) see both coordination function (global awareness networks, scientific collaboration on ocean health) and extraction (token participation in governance forums with limited influence on enforcement; their expertise is extracted without proportional authority). Constrained by funding limitations and lack of enforcement power, but organized enough to resist and shape norms. Moderate extraction with real but limited exit options.
constraint_indexing:constraint_classification(ocean_governance_fragmentation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: OCEAN GOVERNANCE REFORM INITIATIVES (SCAFFOLD) — Organized initiatives (High Seas Treaty negotiations, marine protected area networks, Blue Economy frameworks) see fragmentation as a temporary coordination problem with a sunset. These mechanisms are building alternative governance pathways: global biodiversity frameworks, area-based management tools, integrated ocean planning. High suppression during implementation (incumbent coastal state resistance, industry lobbying) but sunset clause evident in normative shift toward consolidation. Theater moderate — performative summits exist but substantive rule-making is advancing.
constraint_indexing:constraint_classification(ocean_governance_fragmentation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: TRADITIONAL INTERNATIONAL LAW (PITON) — UNCLOS and flag state responsibility systems persist as institutional forms but their functional verification is substantially degraded. Theater high: flags of convenience, port state controls, and compliance reporting are largely performative. The institutions exist through inertia (legitimacy conferred by historical adoption) not current function. Extractive industries and weak-capacity states both circumvent the system while maintaining formal compliance fiction. Piton classification driven by theater_ratio ≥ 0.70.
constraint_indexing:constraint_classification(ocean_governance_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LIMIT VIEW (MOUNTAIN) — From civilizational scale, ocean governance fragmentation appears as an immutable property of international relations: sovereign states cannot be forced to coordinate; collective action problems are inherent to global commons; tragedy of the commons is a natural law. This perspective risks naturalizing contingent institutional choices. However, the structural data contradicts the mountain classification: major powers successfully coordinate on climate (Paris Agreement), shipping standards (IMO), and regional management (Antarctic Treaty). The 'inherent tragedy' framing prevents seeing that fragmentation is enforced, not emergent.
constraint_indexing:constraint_classification(ocean_governance_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ocean_governance_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ocean_governance_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ocean_governance_fragmentation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ocean_governance_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ocean_governance_fragmentation, TR),
    TR >= 0.70.

:- end_tests(ocean_governance_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting significant asymmetry in who benefits from fragmentation vs who bears costs. The value is not as extreme as pure extraction (0.72+) because major coastal states experience some genuine coordination benefit through EEZ stability and sectoral agreements, and because reform initiatives are beginning to build alternative pathways. However, extraction is substantial: artisanal communities and the marine commons bear costs without proportional benefit. The 0.58 value reflects that fragmentation enables extraction but is not its sole purpose. Suppression (0.65): High. Barriers to consolidation of governance include: (1) exit costs for major states considering surrender of sovereignty, (2) enforcement deficits in weak-capacity states making unilateral compliance costly, (3) capital requirements for monitoring technologies, (4) organized resistance from extractive industries, (5) institutional inertia in traditional flag-state systems. Theater ratio (0.68): Moderate-high. Performative elements include: flag-of-convenience systems (formal registration without enforcement), port state control (reported but not consistently enforced), international summits and agreements (high visibility, variable implementation), fishing subsidy reporting (routinely underreported), compliance certifications (often fraudulent). However, some genuine functional elements exist: scientific stock assessments, satellite monitoring improving, regional enforcement strengthening in some zones.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival variation. The marine commons and artisanal communities see pure extraction (snare) with no exit and no coordination benefit. SIDS see mixed extraction and coordination (tangled rope). Extractive industries and major states see coordination without extraction (rope) — they experience fragmentation as enabling rather than constraining. Reform initiatives see a temporary problem being solved (scaffold). Traditional institutions see their own degradation (piton). The civilizational analytical observer risks seeing an immutable natural law (mountain — tragic inevitability of state competition) when the structural evidence suggests contingent institutional choices. This full spectrum from snare to mountain on the same structural facts reveals that fragmentation's classification depends entirely on which structural position the observer occupies.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality pipeline differentiates agents by their structural position within the constraint. Powerless victims (marine commons, artisanal communities) have d ≈ 0.95-1.0 → maximum experienced extraction. Moderate agents (SIDS) have d ≈ 0.70-0.80 (trapped between coordinated participation and constrained by capacity) → high extraction but with some coordination benefit. Organized reformers (environmental coalitions) have d ≈ 0.65-0.75 (constrained by limited enforcement power but organized enough to shape norms) → moderate extraction. Institutional beneficiaries (major coastal states, extractive industries) have d ≈ 0.15-0.35 (arbitrage exit capacity, beneficiary status) → low or negative experienced extraction. The analytical observer at civilizational scale has d ≈ 0.72 (observing structure rather than suffering extraction) but risks naturalizing contingent arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint's tension between coordination (genuine benefit for some agents) and extraction (severe cost for others) is legitimate, not a classification error. Ocean governance fragmentation IS a tangled rope — it genuinely coordinates fishing standards, scientific data, and port state minimum rules while simultaneously extracting from vulnerable communities and the marine commons through regulatory arbitrage and enforcement deficits. The mandatrophy dissolves when we recognize that: (1) the coordination benefits are real but asymmetrically distributed, (2) the extraction is enabled by fragmentation but not inevitable from fragmentation itself (major powers successfully coordinate when they choose to), (3) reform initiatives are building alternative pathways with lower extractiveness and higher coordination function. The analytical observer's mountain classification is revealed as a false summit — it naturalizes as 'inevitable tragedy of the commons' what is actually a set of contingent policy choices favoring incumbent beneficiaries. The constraint's evolution (increasing theater, increasing extractiveness) shows degradation of coordination function relative to extraction, consistent with piton dynamics in traditional institutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fragmentation_as_feature_vs_bug,
    'Is ocean governance fragmentation a structural inevitability of international relations or a contingent outcome of conflicting state interests that could be consolidated into unified governance?',
    'Historical comparison: success of consolidated frameworks (Antarctic Treaty, IMO regulations, regional fisheries bodies) vs failure modes; game-theoretic modeling of coordination costs vs exit benefits for different state types',
    'If structural inevitability: mountain classification is correct; reform initiatives fail. If contingent outcome: tangled rope and scaffold classifications are correct; reform pathways are viable. Current evidence leans toward contingent (major powers coordinate on selected domains) but fragmentation persists across domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_as_feature_vs_bug, conceptual, 'Whether fragmentation is structural inevitability or contingent outcome').

omega_variable(
    enforcement_deficit_mechanism,
    'Does enforcement deficit result from genuine capacity constraints in small states or from deliberate non-enforcement by major states maintaining extractive advantage?',
    'Empirical analysis: comparison of enforcement budgets vs actual fishing violations; correlation between state capacity and compliance; interview data on state enforcement incentives; analysis of which violations trigger enforcement vs which are tolerated',
    'If capacity constraint: technical aid and funding can resolve (scaffold trajectory). If deliberate non-enforcement: major state interests in maintaining fragmentation are structural (snare/tangled rope for victims is inevitable). Current evidence suggests mix: weak capacity + active non-enforcement by major actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_deficit_mechanism, empirical, 'Source of enforcement deficit in fragmented governance').

omega_variable(
    collective_benefit_attribution,
    'Do SIDS and artisanal communities experience any genuine coordination benefits from fragmented governance, or is the ''tangled rope'' classification premature?',
    'Direct measurement: comparison of fishing productivity, price stability, and resource sustainability under fragmented vs consolidated governance regimes; case studies of regional fisheries management organization effectiveness for small-state members',
    'If no genuine coordination benefit: these agents should classify as snare, not tangled rope. If mixed (some benefit from data sharing, stock assessment, market access): tangled rope is correct. Current data suggests minimal coordination benefit for SIDS in practice despite formal mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_benefit_attribution, empirical, 'Whether SIDS genuinely benefit from fragmented governance coordination').

omega_variable(
    regulatory_arbitrage_quantification,
    'What portion of global fishing extraction results from industrial operators moving to weaker-enforcement zones vs from legitimate biological productivity differences?',
    'Fleet tracking data (AIS, satellite monitoring) to map fishing effort; regulatory regime analysis by zone; correlation of enforcement spending with catch sustainability; comparison of catch per unit effort (CPUE) across enforcement levels',
    'High arbitrage volume (>30%) would confirm rope/snare classification for extractive industries. Low volume (<10%) would suggest fragmentation is more about technical coordination than extractive opportunity. Current estimates range 15-35%, suggesting substantial but not overwhelming arbitrage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_arbitrage_quantification, empirical, 'Scale of regulatory arbitrage in ocean extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ocean_governance_fragmentation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ocean_gov_tr_t0, ocean_governance_fragmentation, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ocean_gov_tr_t10, ocean_governance_fragmentation, theater_ratio, 10, 0.62).
narrative_ontology:measurement(ocean_gov_tr_t20, ocean_governance_fragmentation, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(ocean_gov_be_t0, ocean_governance_fragmentation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ocean_gov_be_t10, ocean_governance_fragmentation, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(ocean_gov_be_t20, ocean_governance_fragmentation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ocean_governance_fragmentation, enforcement_mechanism).
narrative_ontology:affects_constraint(ocean_governance_fragmentation, fisheries_management_collective_action).
narrative_ontology:affects_constraint(ocean_governance_fragmentation, maritime_shipping_standards).
narrative_ontology:affects_constraint(ocean_governance_fragmentation, deep_sea_mining_governance).
narrative_ontology:affects_constraint(ocean_governance_fragmentation, coastal_state_eez_claims).

% DUAL FORMULATION NOTE:
% Ocean governance fragmentation is decomposed from sector-specific constraints: fisheries management (ε≈0.55, coordination-heavy), shipping standards (ε≈0.25, rope-type), deep-sea mining (ε≈0.72, snare-type, emerging). Each sector has its own governance structure and extractiveness profile. The fragmentation constraint captures the meta-level institutional gaps between sectors and the regulatory arbitrage this enables. High extractiveness in deep-sea mining derives partly from absence of consolidated governance; high coordination in shipping derives from unified IMO structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ocean_governance_fragmentation, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
