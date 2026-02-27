% ============================================================================
% CONSTRAINT STORY: unclos_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_2026, []).

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
 *   constraint_id: unclos_2026
 *   human_readable: UN Convention on the Law of the Sea (2026 Context)
 *   domain: legal/geopolitical/environmental
 *
 * SUMMARY:
 *   The UN Convention on the Law of the Sea (UNCLOS), ratified in 1994 and
 *   entering into force in 1982, represents the most comprehensive attempt to
 *   create a global legal order for maritime governance. It establishes a
 *   framework for territorial seas (12 nm), exclusive economic zones (200
 *   nm), extended continental shelves (up to 350 nm), and the high seas. In
 *   2026, UNCLOS is experiencing a phase transition: the Biodiversity Beyond
 *   National Jurisdiction (BBNJ) agreement, adopted in 2023 and entering into
 *   force in 2024, extends marine protection mechanisms to the high seas,
 *   creating new coordination functions but also new extraction risks. The
 *   constraint exhibits mixed characteristics across all six DR types
 *   depending on observer position. Wealthy coastal states with naval
 *   capacity experience UNCLOS as a coordination mechanism protecting their
 *   interests and enabling maritime commerce. Small island developing states,
 *   subsistence fishing communities, and environmental interests experience
 *   it as a framework that systematizes extraction, with procedural
 *   legitimacy masking asymmetric power. The International Sea-Bed Authority
 *   represents institutional degradation — created with aspirations to govern
 *   deep-sea mining equitably but operating as a performative body unable to
 *   constrain contractor behavior. The analytical observer risks naturalizing
 *   UNCLOS as an immutable global order reflecting law-of-the-sea principles,
 *   when in fact it reflects 1970s geopolitical power distributions that have
 *   partially shifted. The constraint's extractiveness has increased over 44
 *   years as industrial fishing pressure has intensified and deep-sea
 *   resource extraction has become technically feasible, revealing that
 *   UNCLOS's framework enables rather than constrains this extraction.
 *
 * KEY AGENTS:
 *   - Small Island Developing States (SIDS): Primary victims (powerless/trapped) — lack enforcement capacity for EEZ claims, face colonized maritime law, cannot exit ocean dependence
 *   - Subsistence Fishing Communities: Primary victims (powerless/trapped) — restricted by EEZ licensing they cannot afford, excluded by industrial fleet expansion, dependent on ocean livelihoods
 *   - Marine Ecosystem Integrity: Structural victim (powerless/trapped) — abstract collective good; no agency; bears cost of overfishing, pollution, mining pressures enabled by flag state arbitrage
 *   - Environmental Protection Coalition (BBNJ Signatories): Organized beneficiary-victim (organized/constrained) — benefits from BBNJ coordination (marine protected areas); bears enforcement costs and compliance asymmetries
 *   - Coastal States with Naval Power: Primary beneficiary (institutional/arbitrage) — gain EEZ control, resource extraction rights, enforcement authority; experience framework as coordination
 *   - Industrial Shipping & Fishing Fleets: Secondary beneficiary (institutional/arbitrage) — benefit from predictable maritime law; maintain regulatory arbitrage through flag state shopping
 *   - Major Naval Powers (USA, China, Russia): Powerful beneficiary-victim (powerful/mobile) — benefit from navigation rights and extended shelf claims; extract through asymmetric enforcement and dispute resolution favor
 *   - International Sea-Bed Authority: Institutional actor (institutional/arbitrage) — intended coordinator for deep-sea mining; operates as degraded piton; maintains performative governance with limited enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_2026, 0.52).
domain_priors:suppression_score(unclos_2026, 0.48).
domain_priors:theater_ratio(unclos_2026, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(unclos_2026, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(unclos_2026, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_2026, tangled_rope).
narrative_ontology:human_readable(unclos_2026, "UN Convention on the Law of the Sea (2026 Context)").
narrative_ontology:topic_domain(unclos_2026, "legal/geopolitical/environmental").

domain_priors:requires_active_enforcement(unclos_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_2026, coastal_states).
narrative_ontology:constraint_beneficiary(unclos_2026, flag_states).
narrative_ontology:constraint_beneficiary(unclos_2026, industrial_fishing_fleets).
narrative_ontology:constraint_beneficiary(unclos_2026, maritime_commerce_sectors).
narrative_ontology:constraint_victim(unclos_2026, marine_ecosystem_integrity).
narrative_ontology:constraint_victim(unclos_2026, small_island_developing_states).
narrative_ontology:constraint_victim(unclos_2026, subsistence_fishing_communities).
narrative_ontology:constraint_victim(unclos_2026, future_ocean_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SIDS (SNARE) — Bound by UNCLOS but lack enforcement capacity and capital for compliance monitoring. Maritime jurisdiction claims constrained by limited naval resources. Trapped within a framework designed by industrial states. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.73.
constraint_indexing:constraint_classification(unclos_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUBSISTENCE FISHING COMMUNITIES (SNARE) — Restricted by exclusive economic zones (EEZs) and licensing regimes they cannot afford. No exit from ocean-dependent livelihoods. Industrial fleet expansion subsidized while artisanal fishing faces quotas. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.74.
constraint_indexing:constraint_classification(unclos_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ENVIRONMENTAL PROTECTION COALITION (TANGLED ROPE) — BBNJ (Biodiversity Beyond National Jurisdiction) creates coordination function for marine protected areas and high-seas conservation while imposing enforcement costs on signatories. Constrained by compliance requirements but organized enough to negotiate terms. Benefits from coordination (marine reserves increase ecosystem services); bears extraction costs (monitoring, compliance). d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.47.
constraint_indexing:constraint_classification(unclos_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: COASTAL STATES WITH NAVAL POWER (ROPE) — UNCLOS enables sovereign control over EEZs (200 nm) and extended continental shelves; enforcement capacity and arbitrage options (flag state licensing, resource extraction permits). Experience the framework as coordination: maritime boundaries reduce conflict; licensing generates revenue. d≈0.10, f(d)≈0.05, σ=1.2 → χ≈0.03.
constraint_indexing:constraint_classification(unclos_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INDUSTRIAL SHIPPING & FISHING FLEETS (ROPE) — UNCLOS provides predictable legal framework; flag state registration allows regulatory arbitrage (re-flagging to states with lax compliance). Benefit from global commons coordination (navigation rights, predictable maritime law). Low extraction because exit options (flag state shopping, informal arrangements) remain viable. d≈0.15, f(d)≈0.08, σ=1.2 → χ≈0.05.
constraint_indexing:constraint_classification(unclos_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MAJOR NAVAL POWERS (TANGLED ROPE) — Control high-seas navigation; benefit from UNCLOS (freedom of navigation, extended shelves for major powers). Simultaneously extract through asymmetric enforcement (coast guard capacity, submarine monitoring, dispute resolution favor). Mobile exit options (can ignore rulings, establish parallel arrangements) but benefit from coordination (predictability, deterrence of piracy). d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.25.
constraint_indexing:constraint_classification(unclos_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL SEA-BED AUTHORITY (PITON) — Created by UNCLOS Part XI to regulate deep-sea mining. Performative governance structure: approval mechanisms exist but lack enforcement teeth; mining contractors operate in regulatory theater without genuine constraint. theater_ratio=0.65 (high proceduralism, low actual control). ISA sees its own authority as degraded — maintained through institutional inertia rather than functional effectiveness.
constraint_indexing:constraint_classification(unclos_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, ocean governance encounters immutable constraints: the high seas are inherently difficult to monitor (information asymmetry), maritime commerce requires predictable law (coordination necessity), and coastal sovereignty over EEZs is a natural expression of territorial governance principles. However, the structural data (ε=0.52, suppression=0.48, theater=0.65) contradicts this — the engine identifies a false summit. The 'natural limits' framing masks contingent institutional choices (subsidy structures, flag state arbitrage, enforcement asymmetries).
constraint_indexing:constraint_classification(unclos_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unclos_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unclos_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unclos_2026, TR),
    TR >= 0.70.

:- end_tests(unclos_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increasing over interval. In 1982, UNCLOS was primarily a coordination mechanism — maritime boundaries reduced conflict, and most coastal states lacked capacity for industrial-scale extraction. By 2026, extractiveness has increased due to: (1) industrial fishing intensification enabled by flag state arbitrage and subsidy structures, (2) emergence of deep-sea mining as technically feasible, (3) revealed vulnerability of EEZs to IUU (illegal, unreported, unregulated) fishing by powerful flag states, (4) asymmetric enforcement capacity across coastal states. The framework itself enables extraction by institutionalizing flag state sovereignty while failing to constrain it. Suppression (0.48): Moderate. UNCLOS creates formal dispute resolution (ICJ, ITLOS) and procedural legitimacy, reducing crude coercion. However, suppression of alternatives remains substantial — coastal states cannot unilaterally exit the framework without losing maritime claims; subsistence communities cannot opt out of EZZ licensing regimes; environmental interests cannot enforce high-seas protection without state sponsorship. Theater ratio (0.65): Moderately high and increasing. The ISA's governance of deep-sea mining is largely performative (proceduralism without enforcement capacity). UNCLOS Part XI dispute resolution theater creates appearance of impartial adjudication while major powers often disregard unfavorable rulings (USA rejection of ITLOS decisions on maritime boundaries). BBNJ MPA designation is becoming theater as enforcement mechanisms remain weak. Traditional UNCLOS framework (freedom of navigation, innocent passage) contains high procedural content relative to actual constraint on naval activity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. Small island developing states and subsistence communities see a snare: the framework is imposed, non-negotiable, and extraction occurs through asymmetric enforcement. Environmental coalitions see tangled rope: they benefit from BBNJ coordination functions (high-seas MPAs) while bearing enforcement costs and facing powerful states' non-compliance. Industrial actors see rope: predictable legal framework enables commerce and resource extraction; flag state arbitrage provides genuine exit options. Major naval powers see tangled rope at best, rope at worst: they benefit from the framework (extended continental shelves, navigation rights, dispute resolution that tends to favor naval power) while nominally constrained by the same rules. The piton perspective (ISA degradation) is institutional self-awareness: the intended coordinator (ISA) knows its governance is performative. The analytical observer who naturalizes UNCLOS as a natural law of ocean governance misses the contingency: the framework reflects 1970s power distributions and has not adapted to 21st-century industrial capacity asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   SIDS (powerless/trapped): Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction: no exit, no enforcement capacity, no voice in rule-making dominated by powerful states. Subsistence communities (powerless/trapped): Victim + trapped → d≈0.95, f(d)≈1.42. Near-maximum extraction: ocean-dependent livelihoods cannot exit; licensing and EEZ boundaries imposed. Environmental coalition (organized/constrained): Mixed beneficiary-victim + constrained → d≈0.55, f(d)≈0.75. Moderate extraction: constrained by compliance costs and power asymmetries; benefit from coordination function (MPAs). Coastal states with naval power (institutional/arbitrage): Beneficiary + arbitrage → d≈0.10, f(d)≈0.05. Low extraction: arbitrage options (extended shelf claims, enforcement capacity); benefit from framework. Industrial fleets (institutional/arbitrage): Beneficiary + arbitrage → d≈0.15, f(d)≈0.08. Low extraction: flag state shopping provides exit; benefit from predictable law. Major naval powers (powerful/mobile): Beneficiary + mobile → d≈0.40, f(d)≈0.40. Low-moderate extraction: mobile options (can ignore rulings); benefit from framework; extract through asymmetric enforcement (cost borne by weaker states). ISA (institutional/arbitrage): Institutional actor experiencing piton degradation; d≈0.05, f(d)≈-0.12, but theater_ratio gate drives piton classification independent of chi.
 *
 * MANDATROPHY ANALYSIS:
 *   UNCLOS 2026 resolves the mandatrophy by revealing that the constraint functions as **nested dual classifications**: (1) UNCLOS-as-coordination (rope) from the perspective of states that can enforce it and benefit from predictable maritime boundaries, and (2) UNCLOS-as-extraction (snare/tangled rope) from the perspective of states and communities that cannot enforce or exit. The mandatrophy resolution shows that the classification depends critically on capacity asymmetry: if all coastal states had equivalent surveillance and enforcement capacity, UNCLOS would be primarily rope for all perspectives. If enforcement capacity is asymmetrically distributed (which it is), then the same framework reads as rope for powerful actors and snare for powerless ones. The ISA represents institutional degradation: created with aspirations to be a rope (equitable deep-sea mining governance), it has become a piton (performative authority without enforcement). BBNJ attempts to strengthen the rope function through collective coordination (marine protected areas), but its effectiveness depends on the omega variable: whether flag state compliance can be enforced. If it can, BBNJ shifts the environmental coalition perspective toward genuine tangled rope (coordination + constraint). If it cannot, BBNJ is aspirational theater layered onto persistent extraction. The constraint resolves by explicitly modeling **power asymmetry as a structural input to perspective classification**, not as a confounding variable to be averaged away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    eez_surveillance_capacity_threshold,
    'What level of maritime surveillance capacity genuinely enables exclusive economic zone (EEZ) enforcement vs. creates illusion of control?',
    'Comparison of flag state compliance rates across EEZs with different surveillance infrastructure; analysis of illegal, unreported, unregulated (IUU) fishing detection rates and actual enforcement against detected violations',
    'If threshold is low: most SIDS are trapped (snare confirmed). If threshold requires industrial-scale technology: SIDS have no enforcement pathway (snare degraded to pure coercion). If emerging technology (satellite AIS monitoring, AI detection) lowers threshold: SIDS shift to tangled_rope; organized coalition perspective strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eez_surveillance_capacity_threshold, empirical, 'Maritime surveillance capacity threshold for genuine EEZ control').

omega_variable(
    bbnj_enforcement_teeth,
    'Does BBNJ''s marine protected area (MPA) designation mechanism create binding constraints on high-seas fishing and shipping, or is it primarily performative coordination theater?',
    'Longitudinal tracking of compliance rates within designated MPAs (vs. undesignated areas); analysis of dispute resolution outcomes; measurement of actual fleet rerouting and fishing effort reduction attributed to MPA status',
    'If enforcement is binding: environmental coalition perspective is genuine tangled_rope (coordination + extraction). If largely theater: BBNJ is a piton (degraded aspiration). If enforcement improves: scaffold perspective emerges (temporary coordination problem being solved).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bbnj_enforcement_teeth, empirical, 'Whether BBNJ MPAs enforce actual constraints or function as coordination theater').

omega_variable(
    flag_state_arbitrage_limits,
    'Can flag state regulatory arbitrage (re-flagging to low-compliance jurisdictions) be closed through targeted sanctions and port state control, or is it structurally endemic to the UNCLOS flag state model?',
    'Analysis of re-flagging patterns post-2022; effectiveness of IMO port state control initiatives (PSC) and port authority enforcement; cost-benefit of re-flagging relative to compliance costs under strict flag states',
    'If arbitrage can be closed: industrial fleet perspective shifts from rope to tangled_rope; extraction mechanism tightens. If arbitrage is endemic: rope perspective is stable; industrial actors retain exit options and experience genuine coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flag_state_arbitrage_limits, empirical, 'Whether flag state arbitrage can be structurally closed').

omega_variable(
    subsidy_structure_extraction_dependence,
    'Is the asymmetric extraction of subsistence communities primarily driven by UNCLOS EEZ framework or by fish stock depletion and industrial fleet subsidy structures that are orthogonal to UNCLOS?',
    'Counterfactual analysis: what would subsistence fishing access and sustainability look like under alternative legal regimes (commons management, co-management with industrial fleets)? Decomposition of fishing pressure into UNCLOS-attributable (licensing barriers, EEZ boundaries) vs. market-attributable (subsidies, economies of scale) components',
    'If UNCLOS-attributable: subsidy reform within UNCLOS framework could shift subsistence perspective to tangled_rope. If market-attributable: UNCLOS is a constraint document layered atop a separate extraction mechanism (subsidy system); decompose into two constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subsidy_structure_extraction_dependence, conceptual, 'Attribution of subsistence community extraction to UNCLOS vs. subsidy structures').

omega_variable(
    deep_sea_mining_isa_capture_risk,
    'Does the ISA''s governance model create structural risk of capture by mining contractors and flag state interests, rendering its regulatory function permanently degraded (piton) rather than temporarily weak?',
    'Analysis of ISA decision-making patterns; voting bloc alignment; financial dependencies on contractor fees; comparison with successful regulatory bodies (IAEA, IUCN); modeling of contractor incentives relative to ISA enforcement capacity',
    'If capture risk is high: ISA perspective is degraded piton; deep-sea mining constraint is separate story (dominated by flag state and contractor power). If capture can be mitigated: ISA could shift to tangled_rope if enforcement mechanisms are strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deep_sea_mining_isa_capture_risk, empirical, 'Whether ISA is structurally vulnerable to contractor capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_2026, 1982, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_tr_t0, unclos_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(unclos_tr_t22, unclos_2026, theater_ratio, 22, 0.48).
narrative_ontology:measurement(unclos_tr_t44, unclos_2026, theater_ratio, 44, 0.65).

% Extraction over time
narrative_ontology:measurement(unclos_be_t0, unclos_2026, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(unclos_be_t22, unclos_2026, base_extractiveness, 22, 0.38).
narrative_ontology:measurement(unclos_be_t44, unclos_2026, base_extractiveness, 44, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_2026, global_infrastructure).
narrative_ontology:affects_constraint(unclos_2026, industrial_fishing_subsidy_structure).
narrative_ontology:affects_constraint(unclos_2026, flag_state_regulatory_arbitrage).
narrative_ontology:affects_constraint(unclos_2026, deep_sea_mining_governance).
narrative_ontology:affects_constraint(unclos_2026, small_island_climate_vulnerability).
narrative_ontology:affects_constraint(unclos_2026, marine_protected_area_enforcement).

% DUAL FORMULATION NOTE:
% UNCLOS 2026 should be decomposed into separate constraint stories for distinct structural mechanisms: (1) UNCLOS-as-maritime-coordination (ε≈0.15, Rope) — boundary/navigation governance, (2) UNCLOS-as-EEZ-extraction (ε≈0.62, Snare/Tangled Rope) — asymmetric enforcement of EEZ claims, (3) UNCLOS-ISA-deep-sea-mining (ε≈0.58, Piton) — performative governance of seabed resources, (4) BBNJ-as-coordination (ε≈0.35, Rope/Scaffold) — marine protected area mechanism. The single story presented here operates at the treaty-level abstraction; detailed analysis requires decomposition by governance function. The ε=0.52 value represents the weighted average across these mechanisms and should be understood as masking significant variation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_2026, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
