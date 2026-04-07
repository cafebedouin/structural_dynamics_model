% ============================================================================
% CONSTRAINT STORY: alternative_institution_scaling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_alternative_institution_scaling, []).

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
 *   constraint_id: alternative_institution_scaling
 *   human_readable: Alternative Institution Scaling Constraint
 *   domain: institutional_economics/governance
 *
 * SUMMARY:
 *   Alternative institutions — peer-to-peer networks, cooperative platforms,
 *   open-source governance systems, distributed decision-making structures —
 *   face systematic scaling constraints that appear natural but are actually
 *   constructed through regulatory capture, network effects exploitation, and
 *   incumbent coordination. The constraint exhibits hybrid classification:
 *   genuine coordination problems exist (how do you scale distributed
 *   systems? how do you maintain quality at scale? how do you handle network
 *   externalities?), but these are layered with extractive mechanisms
 *   (regulatory moats protect incumbents, switching costs lock in users,
 *   talent drain to established institutions). The theater ratio (0.48)
 *   reflects that regulatory compliance requirements ostensibly designed for
 *   safety/quality are often performative barriers to entry rather than
 *   functionally necessary. Measured extractiveness has increased over the
 *   interval (0.38→0.58) as incumbents have deployed more sophisticated
 *   lock-in strategies while appearing to embrace 'innovation' rhetoric.
 *
 * KEY AGENTS:
 *   - Innovative Alternative Provider: Primary victim (powerless/trapped) — faces regulatory barriers, capital requirements, network effects; cannot scale without overcoming systemic lock-in
 *   - Underserved Community: Secondary victim (powerless/trapped) — dependent on incumbent institutions providing poor service; geographically or economically isolated from alternatives
 *   - Incumbent Institution: Primary beneficiary (institutional/arbitrage) — profits from regulatory moats, network effects, and switching costs; experiences scaling as coordination opportunity
 *   - Distributed Alternative Network: Organized agent (organized/constrained) — coalition of alternative providers; experiences both genuine coordination function and internal hierarchy/extraction
 *   - Regulatory Reform Coalition: Organized agent (organized/mobile) — policy advocates and open-source communities building interoperability standards and regulatory exit pathways
 *   - Legacy Regulatory Framework: Institutional structure (institutional/arbitrage) — formal compliance rules that evolved for centralized incumbents; persists through inertia despite misalignment
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing regulatory choices as mathematical inevitabilities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alternative_institution_scaling, 0.58).
domain_priors:suppression_score(alternative_institution_scaling, 0.62).
domain_priors:theater_ratio(alternative_institution_scaling, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alternative_institution_scaling, extractiveness, 0.58).
narrative_ontology:constraint_metric(alternative_institution_scaling, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(alternative_institution_scaling, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alternative_institution_scaling, tangled_rope).
narrative_ontology:human_readable(alternative_institution_scaling, "Alternative Institution Scaling Constraint").
narrative_ontology:topic_domain(alternative_institution_scaling, "institutional_economics/governance").

domain_priors:requires_active_enforcement(alternative_institution_scaling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alternative_institution_scaling, incumbent_institutional_providers).
narrative_ontology:constraint_beneficiary(alternative_institution_scaling, network_gatekeepers).
narrative_ontology:constraint_victim(alternative_institution_scaling, alternative_institutional_innovators).
narrative_ontology:constraint_victim(alternative_institution_scaling, resource_constrained_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INNOVATIVE ALTERNATIVE PROVIDER (SNARE) — Faces systemic barriers: regulatory capture favoring incumbents, network effects locking users into legacy systems, capital requirements for infrastructure replication, and talent drain to established institutions. Cannot exit without abandoning the mission. Experiences maximum extraction through forced inefficiency and slow scaling despite superior design.
constraint_indexing:constraint_classification(alternative_institution_scaling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNDERSERVED COMMUNITY (SNARE) — Structurally dependent on incumbent institutions that provide poor service at monopoly prices. Geographic isolation, lack of alternatives, and switching costs create lock-in. Cannot exit; bears full cost of incumbent inefficiency.
constraint_indexing:constraint_classification(alternative_institution_scaling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: INCUMBENT INSTITUTION (ROPE) — Benefits from regulatory moats, network effects, and switching costs. Experiences scaling constraints as coordination opportunities: expanding geographic reach, adding service lines, standardizing processes. The constraint enables profitable growth management.
constraint_indexing:constraint_classification(alternative_institution_scaling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DISTRIBUTED ALTERNATIVE NETWORK (TANGLED ROPE) — Organized coalition of alternative providers and community advocates. Experiences genuine coordination function (resource sharing, protocol standardization) alongside asymmetric extraction: larger platforms within the network capture disproportionate growth and funding, creating hierarchy within the 'alternative' space.
constraint_indexing:constraint_classification(alternative_institution_scaling, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: REGULATORY REFORM COALITION (SCAFFOLD) — Organized agents (policy advocates, open-source communities, development NGOs) view scaling constraints as temporary regulatory artifacts with a sunset clause. Interoperability mandates, right-to-port regulations, and data portability requirements represent explicit exits from lock-in. Theater is low because the coordination problem (enabling alternative entry) is functionally real.
constraint_indexing:constraint_classification(alternative_institution_scaling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY REGULATORY FRAMEWORK (PITON) — Formal sector regulations (licensing, compliance, network infrastructure rules) were designed for incumbent institutional scale and centralized control. The framework persists through institutional inertia despite being functionally misaligned with alternative models. High theater: compliance requirements exist nominally but enforcement is inconsistent, creating performative compliance rather than actual risk.
constraint_indexing:constraint_classification(alternative_institution_scaling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, scaling constraints appear inherent to institutional change: network effects, coordination problems, and switching costs are mathematical properties of multi-agent systems, not contingent policy choices. This perspective risks naturalizing what are actually designed incentive structures and regulatory choice points.
constraint_indexing:constraint_classification(alternative_institution_scaling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alternative_institution_scaling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(alternative_institution_scaling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alternative_institution_scaling, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(alternative_institution_scaling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(alternative_institution_scaling, TR),
    TR >= 0.70.

:- end_tests(alternative_institution_scaling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over interval. The constraint extracts from alternatives through multiple mechanisms: regulatory capture (formal rules favor incumbent structure), network effects (switching costs increase with incumbent size), capital requirements (infrastructure investment barriers), and information asymmetry (incumbents control data and switching processes). The increase from 0.38 to 0.58 reflects sophistication in lock-in strategies — incumbents are becoming more deliberate about maintaining moats while using open-source rhetoric. Not yet maximum extraction (0.70+) because alternatives retain some arbitrage routes and the constraint is visibly contested. Suppression (0.62): High. Multiple structural barriers prevent exit: geographic lock-in (only incumbent serves the area), switching costs (data portability barriers, training costs), regulatory lock-in (formal licensing advantage), and psychological lock-in (normalized incumbent superiority). But suppression is not absolute — regulatory arbitrage (choosing favorable jurisdictions), capital raising for infrastructure (venture funding), and talent mobility (experienced technologists joining alternatives) provide partial escape routes. Theater ratio (0.48): Moderate. Regulatory requirements have substantial performative content — they are presented as safety/quality necessities but often function as entry barriers. However, some theater is legitimate (actual coordination challenges exist at scale). The increase from 0.35 to 0.48 reflects that compliance theater is becoming more sophisticated as incumbents deploy complexity as a barrier.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from a single structural phenomenon. At the immediate timescale with powerless victims and no exit, alternatives see Snare — they are trapped in scaling hell. At the generational timescale with organized reform, advocates see Scaffold — the constraint has a sunset as interoperability rules deploy. At the immediate timescale from the incumbent perspective, they see Rope — scaling is a coordination problem they are successfully solving. Within the alternative network itself, distribution of benefits is asymmetric (Tangled Rope) — larger platforms capture more value. The legacy regulatory framework persists (Piton) — compliance theater masks diminished function. The analytical observer risks naturalizing all of this as inevitable (Mountain) — but the structural data shows this is contingent on current regulatory choices and incumbent strategy, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by structural position relative to the scaling constraint. Incumbents with arbitrage exit options and concentrated benefits derive low d (they experience coordination benefits and can exit lock-in at will — if threatened, they can deploy anti-trust settlement or targeted innovation). Alternative providers trapped by switching costs, regulatory barriers, and capital requirements derive high d (they experience extraction fully, with minimal exit paths at individual level). Organized alternatives with coalition resources and regulatory leverage derive moderate d (they have exit paths through advocacy and interoperability standards, but face significant short-term constraints). The pipeline computes chi = ε × f(d) × σ(S) for each perspective: beneficiaries (d≈0.15) experience low chi despite high ε; trapped alternatives (d≈0.90) experience high chi; organized reformers (d≈0.55) experience moderate chi even though they have better power than individual alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the coordination-vs-extraction ambiguity by showing that both are genuinely present in the structure. The tangled rope classification is correct: genuine coordination problems exist (how do you scale distributed systems? how do you maintain quality? how do you handle network externalities?), but these are exploited by incumbent actors who have regulatory/capital advantages that prevent symmetric solution. The constraint is not 'pure coordination labeled as extraction' (would be false snare) nor 'pure extraction labeled as coordination' (would be false rope). It is hybrid — real coordination need, asymmetric extraction mechanism, high enforcement cost to maintain the lock-in. Mandatrophy resolves when the analytical observer recognizes that the 'inevitable' scaling costs are actually distributed costs that fall asymmetrically on alternatives while being captured asymmetrically by incumbents. The scaffold perspective resolves this further: interoperability regulations and data portability mandates reduce the legitimate coordination cost while eliminating the asymmetric extraction mechanism. This is not reducing coordination to zero (would make it Snare or Mountain) but rebalancing it symmetrically (would make it Rope under the new regulatory regime).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incumbent_co_optation_vs_displacement,
    'Do alternative institutions scale by displacing incumbents or by being absorbed into incumbent ecosystems?',
    'Long-term tracking of successful alternative institution trajectories: identify rate of full displacement vs acquisition/integration vs permanent market segmentation',
    'If displacement dominates: classification tilts toward Snare (alternatives genuinely trapped until critical mass reached). If co-optation dominates: classification tilts toward Rope (integrated into incumbent coordination logic, reducing experienced extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_co_optation_vs_displacement, empirical, 'Whether scaling success means displacement or absorption into incumbent institutions').

omega_variable(
    network_effect_reversibility,
    'Are network effects that lock in incumbents structurally reversible (given sufficient alternative adoption) or do they create persistent first-mover advantages?',
    'Analysis of successful protocol/platform transitions (IPv4→IPv6, TCP/IP adoption, social network migrations); measurement of switching cost reduction over time as alternatives mature',
    'If reversible: suppression decreases as alternatives mature, mountain classification becomes false summit. If irreversible: suppression persists indefinitely, snare classification is stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effect_reversibility, empirical, 'Whether network lock-in is structurally reversible').

omega_variable(
    coordination_cost_parity,
    'At what scale do alternative institutions achieve cost parity with incumbents in core coordination functions?',
    'Comparative cost accounting: measure per-unit coordination cost for incumbents vs alternatives across service categories at equivalent scale',
    'If parity achievable at regional/national scale: alternatives escape extraction trap at a reachable point (scaffold with real sunset). If parity requires global scale: alternatives remain trapped indefinitely (snare classification stable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_parity, empirical, 'Scale at which alternative institutions achieve cost parity with incumbents').

omega_variable(
    regulatory_arbitrage_sustainability,
    'Can alternative institutions sustainably operate via regulatory arbitrage (choosing friendly jurisdictions) or do incumbents successfully lobby for harmonized restrictions?',
    'Tracking of regulatory alignment over time; measurement of incumbent lobbying spend; analysis of cross-border regulatory coordination attempts',
    'If arbitrage sustainable: alternatives maintain escape routes (constrained exit, not trapped). If harmonization succeeds: arbitrage closes, suppression increases, extraction becomes inescapable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_sustainability, empirical, 'Whether regulatory arbitrage remains available as an exit route').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alternative_institution_scaling, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(altinst_tr_t0, alternative_institution_scaling, theater_ratio, 0, 0.35).
narrative_ontology:measurement(altinst_tr_t5, alternative_institution_scaling, theater_ratio, 5, 0.42).
narrative_ontology:measurement(altinst_tr_t10, alternative_institution_scaling, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(altinst_be_t0, alternative_institution_scaling, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(altinst_be_t3, alternative_institution_scaling, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(altinst_be_t6, alternative_institution_scaling, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(altinst_be_t10, alternative_institution_scaling, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(alternative_institution_scaling, resource_allocation).
narrative_ontology:affects_constraint(alternative_institution_scaling, network_effects_lock_in).
narrative_ontology:affects_constraint(alternative_institution_scaling, regulatory_capture_incumbent_advantage).
narrative_ontology:affects_constraint(alternative_institution_scaling, talent_drain_alternative_institutions).

% DUAL FORMULATION NOTE:
% Alternative institution scaling is upstream of specific sectoral constraints (e.g., fintech scaling, cooperative platform governance). This story models the general institutional constraint; sector-specific constraints have their own ε values reflecting domain-specific lock-in mechanisms but share the common scaling structure modeled here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(alternative_institution_scaling, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
