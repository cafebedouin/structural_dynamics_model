% ============================================================================
% CONSTRAINT STORY: capability_control_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_capability_control_asymmetry, []).

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
 *   constraint_id: capability_control_asymmetry
 *   human_readable: Capability Control Asymmetry
 *   domain: general/institutional_power
 *
 * SUMMARY:
 *   Capability control asymmetry describes the structural tension arising
 *   when one actor controls access to capabilities that others depend on. The
 *   constraint reflects an inherent feature of technological systems: the
 *   developer or first-mover gains structural advantage in controlling how
 *   the capability is used, modified, integrated, or deprecated. This
 *   advantage is partly legitimate (development incentives, coordination
 *   needs) and partly extractive (rent-seeking, forced dependency,
 *   obsolescence cycles). The constraint operates across institutional
 *   domains — platform economics, software licensing, technical standards,
 *   infrastructure control, and expertise gatekeeping all instantiate
 *   versions of this pattern. The extractiveness trajectory shows
 *   accumulation: early-stage capabilities with low control asymmetry
 *   (extractiveness 0.30) gradually increase suppression and extraction as
 *   network effects deepen dependency and the developer's market power
 *   concentrates. Theater ratio increases modestly as regulatory attestation
 *   layers and standards committees add performative compliance while failing
 *   to prevent asymmetric control. The constraint classifies as Tangled Rope
 *   from the dominant institutional perspective because genuine coordination
 *   exists (ecosystem development, feature communication, ecosystem
 *   governance) alongside asymmetric extraction (licensing terms, API
 *   restrictions, forced integration). However, from the perspective of
 *   powerless actors trapped in dependency, it appears as Snare — pure
 *   extraction with no meaningful exit.
 *
 * KEY AGENTS:
 *   - Capability Developer: Primary beneficiary (institutional/arbitrage) — controls access, pricing, deprecation; experiences constraint as coordination opportunity; has full arbitrage exit
 *   - Capability-Dependent Actors: Primary victims (powerless/trapped) — require capability for domain participation; no alternatives due to network effects or technical lock-in; bear full extraction cost
 *   - Secondary Developers: Mixed role (moderate/constrained) — build on primary capability but face asymmetric terms; can exit at moderate cost through forking or alternative platforms
 *   - Open-Source Coalition: Organized alternatives (organized/constrained) — build alternative capabilities with lower asymmetry; face suppression from incumbent and network effect barriers; constrained exit due to switching costs
 *   - Regulatory/Standards Bodies: Institutional actors (institutional/arbitrage) — nominally constrain capability control but often entrench incumbents; maintain through bureaucratic inertia (piton)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent market structures as mathematical inevitabilities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(capability_control_asymmetry, 0.58).
domain_priors:suppression_score(capability_control_asymmetry, 0.68).
domain_priors:theater_ratio(capability_control_asymmetry, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(capability_control_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(capability_control_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(capability_control_asymmetry, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(capability_control_asymmetry, tangled_rope).
narrative_ontology:human_readable(capability_control_asymmetry, "Capability Control Asymmetry").
narrative_ontology:topic_domain(capability_control_asymmetry, "general/institutional_power").

domain_priors:requires_active_enforcement(capability_control_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(capability_control_asymmetry, capability_developers).
narrative_ontology:constraint_beneficiary(capability_control_asymmetry, access_gatekeepers).
narrative_ontology:constraint_victim(capability_control_asymmetry, capability_dependent_actors).
narrative_ontology:constraint_victim(capability_control_asymmetry, excluded_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAPABILITY-DEPENDENT ACTOR (SNARE) — Locked into dependency on capabilities controlled by others. No alternative sources, no exit option except abandoning the capability-dependent domain entirely. Bears full extraction cost through licensing, terms-of-service violations, capability deprecation, or deliberate gatekeeping. Structurally trapped — the capability is the essential infrastructure.
constraint_indexing:constraint_classification(capability_control_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SECONDARY DEVELOPER (TANGLED ROPE) — Derives genuine benefit from using a primary capability as foundation (coordination function), but faces asymmetric extraction through licensing terms, API restrictions, feature deprecation, or forced integration of subsidiary capabilities. Can exit at moderate cost — developing alternative, building workarounds, or switching platforms. Experiences both coordination (building on shared capability) and extraction (terms imposed by primary developer).
constraint_indexing:constraint_classification(capability_control_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPABILITY DEVELOPER (ROPE) — Primary beneficiary with arbitrage exit (can license, restrict, or deprecate capabilities at will). Experiences the constraint as coordination: communicating capability features and managing adoption enables ecosystem development. Net extractive position but organized as coordination benefit — the developer gains from both the direct licensing revenue and from the ecosystem effects of capability diffusion.
constraint_indexing:constraint_classification(capability_control_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN-SOURCE COALITION (TANGLED ROPE) — Organized agents (developers, communities) building alternative capabilities that reduce asymmetry through openness. Genuinely coordinates ecosystem (enables contribution, forking, local adaptation) while extracting minimal rent. However, faces suppression from incumbent capability developers through acquisition, API weaponization, or institutional capture. Constrained exit — can fork but face network effects, coordination costs, and talent concentration in proprietary ecosystems.
constraint_indexing:constraint_classification(capability_control_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY ATTESTATION LAYER (PITON) — Regulatory/standards frameworks (certifications, compliance requirements, interoperability mandates) are nominally designed to prevent capability control abuse but often become tools that entrench incumbent developers. Theater-heavy: compliance rituals, audit processes, and certification gates persist through institutional inertia while failing to prevent asymmetric control. Maintained by bureaucratic self-preservation rather than effectiveness. High theater ratio reflects that regulatory mechanisms rarely constrain capability control in practice.
constraint_indexing:constraint_classification(capability_control_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, first-mover advantage in capability development creates inherent asymmetry in control: whoever develops a critical capability first enjoys structural advantage that is mathematically inescapable. This perspective sees capability control asymmetry as a natural law of technological development — coordination always involves asymmetry between the coordinator and the dependent. The engine will flag this as a false summit: the mathematics of network effects and switching costs are contingent on institutional choices (licensing regimes, interoperability standards, intellectual property enforcement), not universal laws.
constraint_indexing:constraint_classification(capability_control_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capability_control_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(capability_control_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capability_control_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(capability_control_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(capability_control_asymmetry, TR),
    TR >= 0.70.

:- end_tests(capability_control_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting accumulation of control mechanisms over development lifecycle. Early-stage capabilities show low asymmetry (0.30) because alternatives exist and switching costs are low. As capabilities mature and network effects concentrate adoption, extractiveness increases through licensing terms, API restrictions, forced integration, and deprecation cycles. The trajectory from 0.30→0.58 indicates extraction layering rather than single sharp mechanism. Suppression (0.68): High, reflecting multiple barriers to exit: technical lock-in (API dependencies, data formats), institutional lock-in (contracts, licensing), and network effects (ecosystem gravity). Suppression is not maximal (0.95) because organized alternatives exist and some dependency relationships have moderate exit costs, but for powerless actors, suppression is total. Theater ratio (0.55): Moderate. Regulatory frameworks and standards committees create performative compliance activities (certification processes, interoperability attestation, audit trails) but these mechanisms rarely prevent asymmetric control in practice. The modest theater ratio reflects that the primary extraction mechanism is structural (network effects, switching costs) rather than theatrical, but governance layers add performative content.
 *
 * PERSPECTIVAL GAP:
 *   This is a maximal perspectival gap case. The developer's Rope classification reflects their structural experience: genuine ecosystem coordination with positive incentive effects. The dependent's Snare reflects their structural experience: pure extraction, no exit, no coordination benefit. Both are accurate reflections of structural reality from their positions. The engine resolves this not by choosing one classification but by recognizing that all perspectives are correct given their indexed positions — the perspectival gap IS the finding. The coal analogy fails here: there is no 'true' classification independent of position. The constraint manifests as Rope to those who control capabilities and Snare to those who depend on them. The gap is structural, not epistemological.
 *
 * DIRECTIONALITY LOGIC:
 *   The suppression mechanism is mixed: technical (API unavailability without compliance), institutional (licensing restrictions, terms of service), and network effects (switching costs). For institutional perspectives, the primary suppression mechanism is contractual and technical. For organizational coalitions, suppression includes deliberate API weaponization (hostile changes designed to block alternatives) and acquisition of potential competitors. For powerless actors, all three mechanisms operate — they cannot modify APIs, cannot renegotiate contracts, and cannot absorb switching costs. The suppression metric (0.68) reflects this multi-layered structure. Theater increases over the interval as regulatory compliance rituals proliferate without constraining actual control asymmetry — a signature piton pattern.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that Tangled Rope is the accurate canonical classification at the institutional/civilizational analytical perspective: genuine coordination (ecosystem effects, technology diffusion) exists alongside asymmetric extraction (licensing, deprecation, forced integration). The temptation to classify as pure Snare (high extraction, high suppression) ignores the genuine coordination function and the legitimate role of first-mover advantage in development incentives. The temptation to classify as pure Rope (legitimate technology diffusion) ignores the extraction mechanisms and the inequality they produce. Tangled Rope captures the hybrid: both mechanisms are real and structurally necessary. The false summit (Mountain) appears from the civilizational view when naturalizing network effects as mathematical inevitability rather than institutional outcome. The false floor (Piton) appears when regulatory compliance rituals substitute for actual constraint on asymmetry. The true structure is: legitimate coordination function (justifies some asymmetry) + institutional extraction mechanisms (layered on top) = Tangled Rope. The balancing question is: how much asymmetry justifies the coordination incentive? The omega variables address this threshold directly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_control_boundary,
    'Where is the boundary between legitimate capability coordination (necessary asymmetry for development incentives) and extractive control (asymmetry beyond what development requires)?',
    'Comparative analysis across capability ecosystems with different control regimes (open-source, proprietary, mixed); measurement of ecosystem health and innovation rate as function of control asymmetry; historical analysis of capability transitions and switching costs',
    'If boundary favors coordination: more constraints reclassify as Rope. If boundary favors control: more constraints reclassify as Snare. Directly determines whether capability asymmetry is inevitable or contingent on institutional design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_control_boundary, empirical, 'Boundary between coordination asymmetry and extractive control').

omega_variable(
    substitutability_and_exit,
    'Are alternative capabilities genuinely substitutable for actors dependent on primary capabilities, or do network effects and switching costs make alternatives non-viable?',
    'Case studies of capability migration (actor switching from one capability to alternative); measurement of switching costs; analysis of feature parity and adoption barriers; network effect quantification',
    'If alternatives are genuinely substitutable: exit_options upgrade from trapped→constrained or constrained→mobile. If network effects prevent substitution: actors remain trapped despite technical availability of alternatives. Determines whether dependency is structural or institutional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substitutability_and_exit, empirical, 'Whether alternative capabilities are genuinely substitutable').

omega_variable(
    institutional_versus_technical_lock_in,
    'Is capability control asymmetry enforced technically (capability unavailable without permission), institutionally (legal/contractual barriers), or by network effects (switching too costly despite technical availability)?',
    'Analysis of enforcement mechanisms in specific capability ecosystems; comparison of technical lock-in vs legal enforcement vs network effects as dominant barriers; historical cases where lock-in mechanism changed',
    'If technical: asymmetry appears immutable (approaches Mountain). If institutional: regulatory intervention can reduce asymmetry (approaches Scaffold). If network effects: only exit is ecosystem migration (trapped classification persists). Determines policy leverage points.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_versus_technical_lock_in, empirical, 'Dominant lock-in mechanism: technical, institutional, or network effects').

omega_variable(
    rent_extraction_versus_coordination_cost,
    'What proportion of the extractiveness (0.58) represents legitimate coordination costs (development, maintenance, support) versus pure rent extraction?',
    'Cost structure analysis of primary capability developer; comparison with open-source alternatives; measurement of licensing revenue vs development costs; analysis of deprecated features and forced upgrades',
    'If primarily coordination cost: extractiveness should be lower (0.30-0.40, Rope). If primarily rent extraction: extractiveness is understated (0.70+, Snare). Directly impacts the Tangled Rope classification — distinguishes genuine hybrid from pure extraction wearing coordination clothing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_extraction_versus_coordination_cost, empirical, 'Proportion of extractiveness as coordination cost vs pure rent').

omega_variable(
    capability_deprecation_as_extraction,
    'Does capability deprecation function as a forced extraction mechanism (compelling costly upgrades or migrations) or as a legitimate platform evolution?',
    'Historical analysis of deprecation patterns; measurement of costs imposed on dependent actors by deprecation cycles; comparison with open-source alternatives'' deprecation rates; actor surveys on perceived fairness of deprecation',
    'If deprecation extracts: primary extraction mechanism is time-forced (actors forced to upgrade/migrate at developer''s pace). If evolution: extractiveness is lower and more transparent. Affects whether constraint is continuous steady-state (Snare/Rope) or cyclical (intermittent reinforcement extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_deprecation_as_extraction, empirical, 'Whether capability deprecation functions as forced extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(capability_control_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cca_tr_t0, capability_control_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cca_tr_t3, capability_control_asymmetry, theater_ratio, 3, 0.45).
narrative_ontology:measurement(cca_tr_t6, capability_control_asymmetry, theater_ratio, 6, 0.52).
narrative_ontology:measurement(cca_tr_t10, capability_control_asymmetry, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(cca_be_t0, capability_control_asymmetry, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cca_be_t3, capability_control_asymmetry, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(cca_be_t6, capability_control_asymmetry, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(cca_be_t10, capability_control_asymmetry, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(capability_control_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(capability_control_asymmetry, platform_lock_in).
narrative_ontology:affects_constraint(capability_control_asymmetry, technical_standard_capture).
narrative_ontology:affects_constraint(capability_control_asymmetry, ecosystem_dependency_dynamics).

% DUAL FORMULATION NOTE:
% Capability control asymmetry is upstream of domain-specific extraction patterns (platform lock-in, standard capture, ecosystem dependency). The same base control mechanism manifests differently depending on the capability domain and institutional context. This constraint story captures the general structural pattern; domain-specific stories should link to this as the upstream generative mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(capability_control_asymmetry, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
