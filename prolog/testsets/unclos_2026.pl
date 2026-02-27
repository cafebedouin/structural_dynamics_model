% ============================================================================
% CONSTRAINT STORY: unclos_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   constraint_id: unclos_2026
 *   human_readable: UN Convention on the Law of the Sea (2026 Context)
 *   domain: legal/geopolitical/environmental
 *
 * SUMMARY:
 *   UNCLOS (1982) established a global legal framework for maritime activity,
 *   dividing oceans into jurisdictional zones (territorial seas, EEZs,
 *   continental shelves, high seas) and creating governance bodies (ISA,
 *   ITLOS, coastal state enforcement). From 1982-2026, the constraint has
 *   functioned as both a coordination mechanism (enabling maritime commerce,
 *   fisheries, resource claims) and an extraction mechanism (protecting
 *   developed-state and industrial interests while constraining
 *   developing-state enforcement capacity and environmental protection). The
 *   2024 BBNJ (Agreement on Biodiversity Beyond National Jurisdiction) added
 *   binding conservation mechanisms, attempting to modify UNCLOS as a
 *   Scaffold with sunset logic — converting environmental commitments from
 *   aspirational to enforceable. However, the integration reveals persistent
 *   asymmetries: developed states enforce their EEZs; developing states
 *   cannot. Industrial fishing fleets operate with flag-state arbitrage;
 *   subsistence communities face border enforcement. The constraint exhibits
 *   all six types from different positions, with the primary tension between
 *   the coordination function (enabling legitimate resource claims and
 *   commerce) and the extraction mechanism (asymmetric enforcement protecting
 *   developed-state and commercial interests over developing-state and
 *   ecosystem interests).
 *
 * KEY AGENTS:
 *   - Developed Coastal States (institutional/arbitrage): Primary beneficiaries — control enforcement, negotiate favorable interpretations, exercise soft power
 *   - Industrial Fishing Fleets (institutional/arbitrage): Secondary beneficiaries — access high-seas resources, flag-state arbitrage, benefit from low effective enforcement
 *   - Developing Coastal States (moderate/constrained): Victims with limited agency — granted EEZ rights but lack enforcement capacity; subject to unequal compliance burden
 *   - Small Island Developing States (powerless/trapped): Primary victims — limited economic alternatives, dependent on marine resources, cannot enforce against distant-water fishing
 *   - Subsistence Fishing Communities (powerless/trapped): Primary victims — territorial claims subordinate to state/commercial interests; ecosystem degradation eliminates livelihood
 *   - Marine Ecosystem Health (powerless/trapped): Abstract collective victim — UNCLOS article 192 environmental duties subordinate to extraction rights
 *   - Environmental Coalition (organized/constrained): BBNJ actors attempting to modify constraint through binding conservation mechanisms; partial success with persistent exemptions
 *   - International Seabed Authority (institutional/arbitrage): Institutional actor exhibiting piton behavior — maintains regulatory apparatus without proportional enforcement; will likely become extractive beneficiary as deep-sea mining scales
 *   - Analytical Observer (analytical/analytical): Sees false summit — constraint appears immutable but is contingent on great-power coordination excluding non-state alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_2026, 0.52).
domain_priors:suppression_score(unclos_2026, 0.58).
domain_priors:theater_ratio(unclos_2026, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(unclos_2026, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(unclos_2026, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_2026, tangled_rope).
narrative_ontology:human_readable(unclos_2026, "UN Convention on the Law of the Sea (2026 Context)").
narrative_ontology:topic_domain(unclos_2026, "legal/geopolitical/environmental").

domain_priors:requires_active_enforcement(unclos_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_2026, developed_coastal_states).
narrative_ontology:constraint_beneficiary(unclos_2026, industrial_fishing_fleets).
narrative_ontology:constraint_beneficiary(unclos_2026, maritime_commerce_networks).
narrative_ontology:constraint_victim(unclos_2026, developing_coastal_states).
narrative_ontology:constraint_victim(unclos_2026, small_island_developing_states).
narrative_ontology:constraint_victim(unclos_2026, marine_ecosystem_health).
narrative_ontology:constraint_victim(unclos_2026, subsistence_fishing_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSISTENCE FISHING COMMUNITIES (SNARE) — Trapped within EEZ boundaries they do not control. Foreign industrial fleets deplete stocks; no mechanism to exit or compensate. UNCLOS enforcement mechanisms operate against them (illegal fishing prosecutions) while lacking capacity to constrain industrial extraction. Maximum experienced extraction with zero exit options. The constraint appears as pure extraction from this position.
constraint_indexing:constraint_classification(unclos_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MARINE ECOSYSTEM HEALTH (SNARE) — Non-agent abstract collective bearing costs of unsustainable extraction. UNCLOS article 192 declares duty to protect the marine environment, but this duty is systematically undermined by prior extractive rights (fishing, shipping, mining). The ecosystem cannot exit or organize. Extraction mechanism is structural subordination of environmental protection to economic activity. Theater ratio high because UNCLOS includes environmental language that appears protective but lacks enforcement teeth against state sovereignty.
constraint_indexing:constraint_classification(unclos_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DEVELOPING COASTAL STATE (TANGLED ROPE) — Constrained by lack of enforcement capacity and unequal technological/military power. UNCLOS provides coordination function: legitimate claim to 200nm EEZ and seabed resources. But asymmetric enforcement: wealthy states monitor and police their EEZs; developing states cannot. Extraction runs toward developed states through regulatory capture and unequal technology access. Mixed experience: real resource rights (coordination) offset by enforcement asymmetry (extraction). Exit options are constrained — cannot unilaterally exit UNCLOS without severe diplomatic cost; cannot effectively enforce own EEZ.
constraint_indexing:constraint_classification(unclos_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: DEVELOPED COASTAL STATE WITH ENFORCEMENT CAPACITY (ROPE) — Experiences UNCLOS primarily as coordination mechanism. Framework grants legitimate claims and recognition of enforcement rights. Possesses technological and military capacity to police EEZ and extraterritorial interests. Arbitrage options available: can operate under UNCLOS, negotiate bilateral exemptions, or exercise soft power over interpretations. Net beneficiary of the coordination function. Low experienced extraction because power asymmetry flows toward this agent.
constraint_indexing:constraint_classification(unclos_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INDUSTRIAL FISHING FLEET / MARITIME COMMERCE (ROPE) — Operates under flag state sovereignty (UNCLOS article 94). Experiences framework as enabling coordination: defines common rules for high seas access, reduces uncertainty in maritime commerce, provides dispute mechanisms. Flag state arbitrage: transfer registry to favorable jurisdiction for lower enforcement. Effective extraction from this position is low — the constraint provides more benefit (predictable rules, access guarantees) than cost (moderate compliance requirements). Primary beneficiary alongside developed coastal states.
constraint_indexing:constraint_classification(unclos_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ENVIRONMENTAL COALITION / CONSERVATION ACTORS (TANGLED ROPE) — The 2024 adoption of BBNJ (Agreement on Biodiversity Beyond National Jurisdiction) added binding conservation mechanisms to UNCLOS framework. Organized actors (IUCN, WWF, island nations, coastal communities) saw this as sunset modification — creating mechanism to constrain extractive fishing rights through marine protected areas. But extraction persists: BBNJ enforcement is weak, industrial fishing interests have carved out exemptions, and compliance remains voluntary. Mixed experience of coordination (new framework) and asymmetric enforcement (extractive interests protected). Exit options constrained: must work within UNCLOS/BBNJ system; cannot unilaterally create ocean governance. Theater moderate-high: BBNJ creates appearance of strengthened protection but exemptions and enforcement gaps undermine effectiveness.
constraint_indexing:constraint_classification(unclos_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL SEABED AUTHORITY (PITON) — Established by UNCLOS Part XI (1994 mod.) to regulate deep-sea mining. In 2026, ISA remains largely performative: approved exploitation regulations exist but deep-sea mining has not begun at scale. The ISA maintains procedural theater (meetings, regulatory drafting, environmental review protocols) without functional impact on extraction. As deep-sea mining interest accelerates (rare earth demands), ISA will either enforce constraints (shifting to Rope/Tangled Rope) or remain a ceremonial body (remaining Piton). Currently exhibits theater ratio >0.70: apparatus of regulation without proportional enforcement capacity.
constraint_indexing:constraint_classification(unclos_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational timescale, the constraint appears as an immutable law: ocean commons fundamentally require coordination framework; no alternative exists. States cannot manage oceans without UNCLOS-like structure. However, this is a false summit. The 'immutability' is actually institutional path-dependency and great-power coordination to exclude alternatives (non-state ocean governance, indigenous maritime law, bioregional frameworks). The mountain classification reveals naturalization of a contingent legal-political choice.
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
    constraint_indexing:constraint_classification(unclos_2026, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.52): Moderate-high. UNCLOS creates legitimate coordination function (fisheries management, maritime commerce rules, resource claims), but also enables asymmetric extraction through enforcement gaps and flag-state arbitrage. The 44-year trajectory shows increasing extractiveness as industrial fishing has scaled and developing states have become more economically dependent on marine resources. Suppression (0.58): Moderate-high. Multiple layers: flag-state sovereignty shields commercial operators from coastal-state enforcement (article 94); enforcement capacity gaps prevent detection of violations; environmental subordination via 'prior rights' doctrine; small island states lack alternatives to ocean-based livelihoods. Theater ratio (0.65): Moderate-high and rising. UNCLOS includes environmental protections (article 192, Part XII) and conservation mechanisms (BBNJ MPAs), but enforcement remains weak. ISA maintains regulatory apparatus without proportional impact on extraction. BBNJ introduces MPA theater: designated areas with limited enforcement and exemptions for fishing interests. The theater has increased over the interval as environmental language has proliferated without corresponding enforcement strengthening.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's classification varies from pure extraction (snare) from the subsistence fisher's perspective to pure coordination (rope) from the developed state's perspective. Subsistence communities and marine ecosystems experience maximum extraction with zero exit: trapped by geography, economics, and ecosystem dependence. They perceive UNCLOS as snare because enforcement mechanisms work against them (illegal fishing prosecutions, border closures) while failing to constrain industrial extraction. Developing coastal states perceive tangled rope: UNCLOS grants real resource rights (coordination function) but enforcement asymmetry extracts value toward developed states (asymmetric extraction). Environmental coalition perceives scaffold: BBNJ adds temporary conservation mechanisms with sunset logic — as monitoring capacity improves and alternatives mature, the extraction mechanism weakens. ISA perceives its own role as piton: maintains regulatory apparatus without proportional enforcement, theater increasing as mining scale accelerates. Developed states perceive rope: framework enables their enforcement, grants soft-power leverage, and arbitrage opportunities. Analytical observer risks perceiving mountain (natural law of ocean governance) but structural data reveals contingency: UNCLOS is a specific choice protecting state sovereignty and commercial interests, not an immutable necessity. Alternative governance models (bioregional, peer-based) show partial feasibility, indicating the mountain is a false summit — naturalization of political choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from structural position within the extraction flow. Developed coastal states benefit from the framework (low d ≈ 0.10) via enforcement capacity and arbitrage options. Industrial fleets benefit through flag-state arbitrage (low d ≈ 0.15). Developing coastal states are constrained victims (moderate-high d ≈ 0.65) — they have some agency (can enforce within own EEZ if capacity exists) but face asymmetric burden. Subsistence communities are trapped victims (high d ≈ 0.90) — no enforcement capacity, no exit options, ecosystem degradation beyond individual control. Marine ecosystem is abstract trapped victim (d ≈ 0.95) — non-agent bearing full cost. ISA institutional actors (d ≈ 0.20) — beneficiary position via potential future mining revenue and regulatory authority. The constraint's chi values vary dramatically across positions: developed states experience χ ≈ 0.10-0.20 (low extraction); developing states experience χ ≈ 0.35-0.50 (moderate extraction); subsistence communities experience χ ≈ 0.70+ (high extraction); ecosystem experiences χ ≈ 0.75+ (high extraction). This perspectival gap is the signature of a tangled rope at the system level.
 *
 * MANDATROPHY ANALYSIS:
 *   UNCLOS at the system level is tangled rope: it combines genuine coordination function (maritime commerce rules, fisheries management framework, dispute resolution) with asymmetric extraction (enforcement gaps protecting developed-state and commercial interests, environmental subordination). Mandatrophy would mislabel this as either pure coordination (rope — ignoring extraction asymmetries) or pure extraction (snare — ignoring real coordination benefits). The tangled rope classification captures both: UNCLOS does enable coordination that benefits all parties, AND it creates enforcement asymmetries that extract from developing states and ecosystems. The resolution is perspectival: from a subsistence fisher's position, the coordination benefits are inaccessible (trapped by enforcement gaps), so the constraint appears as snare. From a developed state's position, the coordination benefits are real and the extraction flow runs toward them, so it appears as rope. From the system level, tangled rope is accurate because both functions (coordination and extraction) are structurally present. The mandatrophy prevents collapsing either function: recognizing BBNJ as a real constraint on fishing (coordination improvement) while acknowledging persistent extraction (enforcement gaps, environmental subordination) is the mandatrophy-resolved position. The 2026 BBNJ integration demonstrates this: the agreement added binding conservation mechanisms (coordination strengthening) while carving out industrial fishing exemptions and maintaining weak enforcement (extraction persistence).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity_ceiling,
    'What is the structural limit of UNCLOS enforcement given global monitoring constraints and state sovereignty norms?',
    'Empirical analysis of enforcement gap: tonnage of illegal fishing vs. prosecutions; deep-sea mining activities vs. ISA oversight; shipping emissions violations vs. detected violations. Comparison to enforcement models with higher monitoring (e.g., commodity supply chains with blockchain verification).',
    'If enforcement ceiling is <30% of violations: constraint is primarily extractive (snare classification dominates). If ceiling >60%: constraint functions as intended coordination mechanism (rope/tangled rope). Current evidence suggests 15-25% ceiling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_ceiling, empirical, 'Structural limit of UNCLOS enforcement capacity').

omega_variable(
    bbnj_mechanism_effectiveness,
    'Do BBNJ marine protected areas (MPAs) actually constrain fishing extraction or function as theater masking continued extraction?',
    'Longitudinal study of fish stock recovery in designated MPAs vs. open ocean; comparison of fishing pressure inside vs. outside MPAs; analysis of illegal fishing rates within MPAs; tracking of enforcement budget allocated to MPA monitoring vs. enforcement capability.',
    'If MPAs reduce extraction by >40%: BBNJ represents genuine constraint on industrial fishing (Scaffold classification realistic). If <15%: MPAs are theater, and BBNJ is piton component (extractive interests persist). Current evidence (2026) suggests 20-30% reduction, indicating mixed efficacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bbnj_mechanism_effectiveness, empirical, 'Whether BBNJ MPAs constrain fishing or function as theater').

omega_variable(
    alternative_governance_feasibility,
    'Could bioregional ocean governance or blockchain-verified peer enforcement replace UNCLOS-based state sovereignty model?',
    'Pilot programs in specific ocean basins (Antarctic Peninsula, North Atlantic, Southeast Asian EEZ); comparison of enforcement costs and effectiveness. Assessment of whether non-state actors (indigenous communities, port authorities, shipping networks) can coordinate without state-level framework.',
    'If alternatives prove feasible: UNCLOS is revealed as contingent institutional choice, not natural law (mountain classification false). If not: state sovereignty framework may be closest possible to natural law given human coordination constraints (mountain classification partially justified). Current evidence suggests partial feasibility — some communities achieve >50% enforcement through peer mechanisms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_governance_feasibility, conceptual, 'Feasibility of non-state ocean governance alternatives').

omega_variable(
    deep_sea_mining_extraction_threshold,
    'At what scale of deep-sea mining does ISA shift from piton (theater) to snare (active extraction enforcement)?',
    'Tracking of approved mining contracts, mineral extraction volume, environmental impact assessments, and ISA enforcement actions. Determination of transition point where ISA bureaucracy becomes functionally extractive (actively protecting mining interests against conservation constraints).',
    'If threshold is low (≈100 mining contracts): ISA rapidly becomes extractive snare, theater increases. If threshold is high (≈1000+ contracts): ISA may maintain piton status longer, but eventual shift is inevitable given rare earth demand projections.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deep_sea_mining_extraction_threshold, empirical, 'Scale threshold at which ISA transitions from piton to active extraction').

omega_variable(
    small_island_state_exit_feasibility,
    'Can small island developing states (SIDS) collectively exit UNCLOS or restructure within it, or are they structurally trapped?',
    'Analysis of SIDS coalition capacity: Pacific Islands Forum, Caribbean Community governance. Assessment of alternative regional frameworks (e.g., Pacific Regional Environment Programme). Evaluation of cost of UNCLOS exit vs. cost of continued participation under asymmetric terms.',
    'If exit is feasible: SIDS are constrained, not trapped (exit_options upgrade). If not: they are powerless victims with no exit (snare classification reinforced). Current evidence suggests exit is theoretically possible but politically/economically catastrophic, maintaining trapped status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(small_island_state_exit_feasibility, empirical, 'Whether SIDS can exit UNCLOS or restructure within it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_2026, 1982, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_tr_t0, unclos_2026, theater_ratio, 0, 0.5).
narrative_ontology:measurement(unclos_tr_t22, unclos_2026, theater_ratio, 22, 0.62).
narrative_ontology:measurement(unclos_tr_t44, unclos_2026, theater_ratio, 44, 0.65).

% Extraction over time
narrative_ontology:measurement(unclos_be_t0, unclos_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(unclos_be_t22, unclos_2026, base_extractiveness, 22, 0.48).
narrative_ontology:measurement(unclos_be_t44, unclos_2026, base_extractiveness, 44, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_2026, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_2026, 0.35).
narrative_ontology:affects_constraint(unclos_2026, global_fishing_commons_tragedy).
narrative_ontology:affects_constraint(unclos_2026, deep_sea_mining_regulation).
narrative_ontology:affects_constraint(unclos_2026, island_state_climate_sovereignty).
narrative_ontology:affects_constraint(unclos_2026, maritime_dispute_resolution).

% DUAL FORMULATION NOTE:
% UNCLOS decomposition follows ε-invariance principle. The overall framework (ε=0.52, tangled rope) covers coordination + enforcement asymmetry. Two structurally distinct constraints are captured by separate stories: (1) UNCLOS dispute resolution mechanism (ε≈0.15, pure rope — ITLOS functions effectively as neutral arbiter), and (2) UNCLOS environmental protection regime (ε≈0.72, snare — article 192 environmental duties are systematically subordinated to extraction rights). These three stories form a constraint family: the dispute resolution mechanism enables coordination but lacks environmental authority; the environmental regime attempts coordination but lacks enforcement; the overall framework balances both functions asymmetrically. All three share the spatial scope (global) and institutional actors (coastal states) but differ in ε due to structural differences in enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_2026, institutional, 0.2).
constraint_indexing:directionality_override(unclos_2026, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
