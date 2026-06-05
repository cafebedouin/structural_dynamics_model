% ============================================================================
% CONSTRAINT STORY: network_effects
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_network_effects, []).

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
 *   constraint_id: network_effects
 *   human_readable: Network Effects (Demand-Side Economies of Scale)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Network effects represent a structural phenomenon where the value of a
 *   product or service increases as more people use it. This constraint
 *   occupies a critical position in understanding digital platforms,
 *   telecommunications, and two-sided markets. The paradox: network effects
 *   appear as a natural coordination mechanism (early users benefit from each
 *   other's participation) but also create extractive lock-in once a dominant
 *   network emerges. Late adopters face a trap — the platform they must join
 *   is often technically inferior to alternatives but dominates by network
 *   density alone. The constraint exhibits all six DR types from different
 *   perspectives, revealing that 'network effects' names a family of
 *   structurally distinct economic dynamics rather than a single phenomenon.
 *   The theater_ratio (0.48) reflects that narratives about inevitable
 *   network dominance operate partly as truthful descriptions of coordination
 *   benefits and partly as justifications for suppression of switching. The
 *   extractiveness trajectory (0.15→0.52 over the interval) captures the
 *   constraint's evolution: in early phases, network effects primarily
 *   coordinate users and create genuine mutual benefit; in mature phases,
 *   network effects primarily lock in late adopters and extract through
 *   monopoly power. The platform owner's perspective reveals that suppression
 *   is embedded in technical architecture (data portability barriers, API
 *   restrictions, incompatible formats) rather than through active coercion,
 *   making the constraint a tangled rope with passive enforcement.
 *
 * KEY AGENTS:
 *   - Early Adopters / First-Movers: Primary beneficiaries (institutional/arbitrage) — capture disproportionate network value; can pivot to new platforms if necessary; experience constraint as coordination mechanism
 *   - Platform Owner / Monopolist: Secondary beneficiary (institutional/arbitrage) — captures rents from network density; actively suppresses exit options; profits from lock-in; perspective inverts from beneficiary to extractor
 *   - Late Adopters: Primary victims (powerless/trapped) — forced to use dominant platform despite superior alternatives; face prohibitive switching costs; no exit option; experience maximum extraction
 *   - Competing Platform Operators: Secondary victims (organized/constrained) — organized actors investing in alternatives but suppressed by network density and winner-take-most dynamics; can partially exit but face formidable incumbent advantages
 *   - SME User Ecosystem: Tertiary victims (moderate/constrained) — experience mixed coordination (access to customer base) and extraction (platform fees, algorithmic control, terms-of-service changes); constrained by switching costs and ecosystem dependencies
 *   - Analytical Observer: Civilization-scale view (analytical/analytical) — risks naturalizing contingent institutional lock-in as inevitable economic law; false summit alert on piton classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(network_effects, 0.52).
domain_priors:suppression_score(network_effects, 0.65).
domain_priors:theater_ratio(network_effects, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(network_effects, extractiveness, 0.52).
narrative_ontology:constraint_metric(network_effects, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(network_effects, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(network_effects, tangled_rope).
narrative_ontology:human_readable(network_effects, "Network Effects (Demand-Side Economies of Scale)").
narrative_ontology:topic_domain(network_effects, "economic/technological").

domain_priors:requires_active_enforcement(network_effects).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(network_effects, early_adopters).
narrative_ontology:constraint_beneficiary(network_effects, platform_owner).
narrative_ontology:constraint_victim(network_effects, competing_platforms).
narrative_ontology:constraint_victim(network_effects, late_adopters).
narrative_ontology:constraint_victim(network_effects, locked_in_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE ADOPTER (SNARE) — Faces a trap: must use the dominant platform to participate in the network, despite potentially superior alternatives. Switching costs are prohibitive; the network density creates structural lock-in. No meaningful exit option; extraction operates through mandatory participation in an inferior equilibrium.
constraint_indexing:constraint_classification(network_effects, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING PLATFORM OPERATOR (TANGLED ROPE) — Organized actors (alternative platforms, open-source projects) can invest in features and interoperability to coordinate users, but face suppression through network density and winner-take-most dynamics. Both extraction (users preferring dominant network) and coordination (building alternative ecosystems) are present; enforcement requires sustained technical and marketing investment against a formidable incumbent.
constraint_indexing:constraint_classification(network_effects, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: EARLY ADOPTER (ROPE) — Primary beneficiary. Gains disproportionate value from network growth; coordinates with peers to establish the network's value proposition. Low experienced extraction because this agent benefits from the constraint's operation and can arbitrage to other networks if necessary. Pure coordination function from this perspective.
constraint_indexing:constraint_classification(network_effects, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM OWNER (SNARE) — Appears as beneficiary with arbitrage (can pivot to new platforms), but the perspective shifts from beneficiary to extractor. The platform owner actively suppresses exit options for late adopters through lock-in mechanisms (data portability barriers, API restrictions, incompatible formats). Extraction is embedded in the platform's technical architecture; enforcement is passive (structural) rather than active coercion. High suppression; beneficiary status inverts to reveal extraction.
constraint_indexing:constraint_classification(network_effects, snare,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SME USER ECOSYSTEM (TANGLED ROPE) — Small and medium enterprises using the platform experience mixed effects: coordination benefits (access to large customer base, vendor ecosystem, integration tools) but also extraction through platform fees, algorithmic visibility controls, and terms-of-service changes that can disadvantage specific seller classes. Suppression operates through switching costs and platform lock-in; enforcement requires ongoing compliance with platform policies.
constraint_indexing:constraint_classification(network_effects, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NULL MODEL (PITON) — From first principles, network effects are presented as a natural economic phenomenon — 'more users create more value.' But empirical observation reveals substantial theater in how network effects are invoked: they are used to justify monopoly pricing, to rationalize switching cost barriers, and to explain away competitive failures as 'inevitable network dynamics.' The theater_ratio (0.48) reflects that approximately half the activation energy behind network-driven lock-in comes from actual value creation; the other half derives from suppression of alternatives and coordination of user expectations. The constraint persists partly through its own explanatory power — the narrative 'network effects are natural' operates as institutional inertia.
constraint_indexing:constraint_classification(network_effects, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(network_effects_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(network_effects, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(network_effects, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(network_effects, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(network_effects, TR),
    TR >= 0.70.

:- end_tests(network_effects_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint exhibits meaningful extraction, but not as severe as pure monopoly because network-driven value creation is real — users do benefit from larger networks. However, the extraction grows over time as the network matures: initial value derives from genuine coordination benefits; later value derives from lock-in and suppression of alternatives. The 0.52 figure reflects the mature phase where extraction mechanisms are embedded. Suppression (0.65): Moderate-high. Significant barriers to exit include switching costs (data migration, account relocation, rebuilding social/economic connections), switching risk (fear of missing network activities during transition), technical barriers (incompatible formats, API restrictions), and coordination failure (critical mass required to make alternative viable). Suppression is not absolute — some users do switch, and emerging alternatives attract young cohorts — but substantially dampens exit. Theater ratio (0.48): Moderate. Approximately half of the activation energy behind network-driven dynamics comes from actual value creation (genuine coordination benefits); the other half derives from narrative justification of lock-in, marketing of network effects as inevitable, and rationalization of monopoly pricing as natural ecosystem dynamics. The theater has been declining as antitrust scrutiny increases, but the frame 'network effects are inevitable' still operates at institutional inertia levels.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival divergence. The early adopter sees pure coordination (Rope) — they genuinely benefit from network growth and experience the platform as solving a shared problem. The platform owner sees extraction opportunities (Snare or Tangled Rope depending on perspective) — they actively engineer lock-in while claiming to serve users. Competing operators see suppression (Tangled Rope) — they can invest in alternatives but face formidable incumbent advantages. Late adopters see pure extraction (Snare) — trapped by network density with no viable exit. SME users see mixed effects (Tangled Rope) — coordination benefits from network access but extraction through platform control. The analytical observer risks seeing inevitable economic law (Piton or false Mountain) — the narrative 'network effects are natural' can naturalize what is actually contingent institutional lock-in created through suppression of interoperability and data portability. The perspectival gap is largest between the beneficiary (early adopter) and the victim (late adopter) — they experience structurally opposite dynamics from the same network.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by structural position within the extraction/coordination flow. Early adopters and platform owners occupy beneficiary positions (low d, negative f(d), receive subsidy from the constraint). Their exit options are arbitrage — they can pivot to new platforms and maintain opportunities. Late adopters occupy full-target positions (high d, high f(d), maximum extraction experience) with no exit — they must participate in the dominant network. Competing platform operators occupy partial-target positions (moderate d) — organized but constrained by network density. SME users occupy mixed positions (moderate d) — they receive coordination benefits but also face extraction through platform control mechanisms. The piton perspective derives from high theater_ratio (0.48) indicating that the constraint's operation relies substantially on narrative and institutional inertia rather than active enforcement mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids mislabeling by distinguishing the early-phase coordination function (genuine mutual benefit from network growth) from the late-phase extraction function (lock-in of late adopters). Network effects in early phases are legitimately Rope — they solve a real coordination problem and create shared value. Network effects in late phases are legitimately Tangled Rope or Snare — they combine genuine residual coordination benefits with substantial lock-in extraction. The mandatrophy is resolved by treating network effects as a dual-phase phenomenon: phase 1 (growth) emphasizes coordination; phase 2 (maturity) emphasizes extraction. The measurement trajectory (extractiveness 0.15→0.52) captures this phase transition empirically. The constraint avoids false Rope by acknowledging suppression mechanisms (switching costs, API restrictions, data portability barriers) that are engineered, not emergent. It avoids false Mountain by recognizing that network dominance is contingent on these suppression mechanisms, not inevitable. The piton perspective correctly identifies that 'network effects are inevitable' operates as institutional theater — a narrative maintained through repeated invocation despite its partial falsehood.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitutability_threshold,
    'At what feature/UX gap do network effects cease to bind users to an inferior platform?',
    'Comparative analysis of platform migrations (MySpace→Facebook, Vine→TikTok, Twitter→Bluesky); identification of feature advantage thresholds that overcome switching costs',
    'If threshold is low (small feature gap): network effects are coordination-dominated (Rope). If threshold is high (large feature gap required): network effects are extraction-dominated (Snare). Determines whether the constraint is fundamentally a coordination problem or a lock-in mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitutability_threshold, empirical, 'Feature gap required to overcome network-driven switching costs').

omega_variable(
    interoperability_sufficiency,
    'Can open protocols and data portability standards effectively neutralize network-driven lock-in without fragmenting the network?',
    'Analysis of federated systems (ActivityPub, email protocols); measurement of adoption rates and feature parity for interoperable alternatives; assessment of fragmentation costs vs lock-in prevention benefits',
    'If interoperability works: constraint reclassifies toward Scaffold (sunset via technical standards). If fragmentation outweighs benefits: constraint remains Tangled Rope or Snare (lock-in is structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_sufficiency, empirical, 'Whether interoperability standards can neutralize network lock-in').

omega_variable(
    value_creation_attribution,
    'How much of the platform''s value derives from the network itself vs. the platform owner''s innovation?',
    'Counterfactual analysis: platforms with similar network size but different feature sets; measurement of user surplus vs platform operator profit; historical comparison of value distribution in early vs mature phases',
    'If value creation is mostly network: constraint is Rope (coordination rent-sharing). If value creation is mostly platform: constraint is Snare (extraction via monopoly). Directs whether network effects should be treated as natural coordination or engineered lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(value_creation_attribution, conceptual, 'Attribution of value creation to network effects vs platform innovation').

omega_variable(
    intergeneration_lock_in,
    'Do network effects create lock-in across generations, or do generational cohorts reset network dynamics?',
    'Longitudinal analysis of platform adoption by age cohort; measurement of switching rates across generational boundaries; study of whether Gen Z adoption of new platforms represents lock-in escape or parallel network growth',
    'If generational reset occurs: lock-in is biographical, not civilizational (Snare reclassifies to Scaffold with shorter sunset). If lock-in persists across generations: constraint is civilizational extraction (Snare confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergeneration_lock_in, empirical, 'Whether network lock-in persists across generational cohorts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(network_effects, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(neteff_tr_t0, network_effects, theater_ratio, 0, 0.25).
narrative_ontology:measurement(neteff_tr_t5, network_effects, theater_ratio, 5, 0.38).
narrative_ontology:measurement(neteff_tr_t10, network_effects, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(neteff_be_t0, network_effects, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(neteff_be_t5, network_effects, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(neteff_be_t10, network_effects, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(network_effects, resource_allocation).
narrative_ontology:affects_constraint(network_effects, digital_platform_monopoly).
narrative_ontology:affects_constraint(network_effects, data_portability_barriers).
narrative_ontology:affects_constraint(network_effects, switching_costs_technology).

% DUAL FORMULATION NOTE:
% Network effects decompose into two structurally distinct claims: (1) coordination-driven growth (early phase, genuine mutual benefit, Rope classification), (2) extraction-driven lock-in (late phase, monopoly rent-seeking, Snare/Tangled Rope classification). These are not the same constraint viewed from different angles — their ε values and suppression mechanisms differ substantially. Story treats the constraint as a time-indexed family with a phase transition. The upstream constraints (digital_platform_monopoly, switching_costs_technology) are downstream effects of network effects operating in extraction phase. Network effects in coordination phase would not create those downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(network_effects, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
