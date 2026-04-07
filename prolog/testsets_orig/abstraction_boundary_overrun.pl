% ============================================================================
% CONSTRAINT STORY: abstraction_boundary_overrun
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abstraction_boundary_overrun, []).

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
 *   constraint_id: abstraction_boundary_overrun
 *   human_readable: The Leaky Black-Box Collapse
 *   domain: technological/computational
 *
 * SUMMARY:
 *   The leaky black-box collapse occurs when a software abstraction (API,
 *   library, framework, protocol) fails to hide the messy, low-level
 *   complexity it was designed to simplify. Users encounter implementation
 *   details through error messages, workarounds, or performance surprises
 *   that should have been hidden. This creates a structural tension between
 *   the library maintainer's incentive to ship fast (allowing leaks to
 *   persist) and the dependent developer's trapped position (forced to work
 *   around leaks). The constraint exhibits five distinct classification types
 *   from different observatories: pure extraction (from the trapped
 *   developer's view), mixed coordination-extraction (from the maintainer's
 *   pragmatic position), coordination with sunset (from the standardization
 *   body), degraded ritual (from the legacy specification's perspective), and
 *   a false-summit natural law claim (from the civilizational analytical
 *   view). The theater ratio rises over time as documentation and warnings
 *   about internal implementation details accumulate — the boundary becomes
 *   more performative than functional.
 *
 * KEY AGENTS:
 *   - Dependent Developer: Primary victim (powerless/trapped) — bound by abstraction contract; forced to work around leakage without alternatives
 *   - Library Maintainer: Primary beneficiary and secondary victim (moderate/constrained) — benefits from market position and optimization control; constrained by backward compatibility and user expectations
 *   - Framework Vendor: Secondary beneficiary (institutional/arbitrage) — captures market share via fast feature delivery; can arbitrage to new abstraction if old one fails
 *   - Standardization Body: Organized coalition (organized/constrained) — W3C, ECMA, POSIX build formal specs and test suites to rebuild abstraction boundaries with sunset logic
 *   - Legacy Specification: Institutional guardian (institutional/arbitrage) — maintains the original boundary design through documentation and versioning conventions
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing implementation leakage as inevitable trade-off in information theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abstraction_boundary_overrun, 0.52).
domain_priors:suppression_score(abstraction_boundary_overrun, 0.58).
domain_priors:theater_ratio(abstraction_boundary_overrun, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abstraction_boundary_overrun, extractiveness, 0.52).
narrative_ontology:constraint_metric(abstraction_boundary_overrun, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(abstraction_boundary_overrun, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abstraction_boundary_overrun, tangled_rope).
narrative_ontology:human_readable(abstraction_boundary_overrun, "The Leaky Black-Box Collapse").
narrative_ontology:topic_domain(abstraction_boundary_overrun, "technological/computational").

domain_priors:requires_active_enforcement(abstraction_boundary_overrun).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abstraction_boundary_overrun, abstraction_maintainers).
narrative_ontology:constraint_beneficiary(abstraction_boundary_overrun, early_adopters).
narrative_ontology:constraint_victim(abstraction_boundary_overrun, dependent_systems).
narrative_ontology:constraint_victim(abstraction_boundary_overrun, naive_users).
narrative_ontology:constraint_victim(abstraction_boundary_overrun, system_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT DEVELOPER (SNARE) — Trapped by the abstraction's public contract. When internal leakage occurs (e.g., implementation details force workarounds), the developer cannot exit: their code and career depend on the abstraction working. They must patch, adapt, and accept blame for 'misusing' the layer. Zero alternatives; maximum experienced extraction.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LIBRARY MAINTAINER (TANGLED ROPE) — Constrained by backward compatibility and user expectations. Fixing leaks requires breaking changes, which damages reputation and adoption. But also benefits: maintaining the abstraction provides authority, job security, and first access to optimization opportunities. Active enforcement of the abstraction boundary (documentation, warnings, versioning) required to sustain the boundary.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FRAMEWORK VENDOR (ROPE) — Experiences the abstraction as coordination mechanism. Hidden complexity allows the vendor to ship features fast and capture market share. When leaks occur, the vendor can fix them unilaterally (next version), then sell the fix. Net beneficiary; the abstraction enables their business model. Exit via arbitrage: can migrate users to new abstraction if old one collapses.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STANDARDIZATION BODY (SCAFFOLD) — Organized agents (W3C, ECMA, POSIX committees) see the leaky abstraction as a temporary coordination failure with a planned sunset. Formal specification, test suites, and reference implementations are building stricter abstraction boundaries that expose implementation details only through controlled channels. Low effective extraction because the oversight coalition has agency and an exit path — formalization of the specification layer.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY SPECIFICATION (PITON) — The original abstraction boundary was designed with clear internal/external separation. But decades of patches, ad-hoc extensions, and vendor tweaks have made the boundary itself performative: it exists as documentation and formal claims but has lost functional force. Systems work around it; the specification is maintained through inertia, not because it achieves its purpose. Theater ratio high because the abstraction is mostly symbolic.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INFORMATION THEORY VIEW (MOUNTAIN) — From a computational complexity perspective, some information leakage is inevitable: a black box that hides all internal state cannot respond to novel error conditions without the boundary becoming brittle. The tension between abstraction and diagnosticity is a fundamental trade-off in information theory. However, the structural data reveals this as a false summit — the leakage is not inevitable but engineered (poor encapsulation, insufficient error handling), not a law of computation.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abstraction_boundary_overrun_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(abstraction_boundary_overrun, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(abstraction_boundary_overrun, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(abstraction_boundary_overrun, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(abstraction_boundary_overrun, TR),
    TR >= 0.70.

:- end_tests(abstraction_boundary_overrun_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The library maintainer extracts value by controlling the optimization opportunities and migration paths available to dependent developers. When leaks force workarounds, the maintainer can later claim credit for 'fixing' issues. The extraction is systematic but not total — dependent developers have some agency through forking, switching libraries, or temporal workarounds. Suppression (0.58): Moderate-high. The abstraction's contractual promise ('you don't need to know internal details') suppresses alternatives: developers cannot access alternative implementations without breaking compatibility, cannot inspect internals to understand failures, and cannot opt out without rewriting code. But suppression is not maximal because open-source alternatives and documentation leakage provide partial transparency. Theater ratio (0.64): High-moderate. The abstraction boundary is increasingly performative: warnings about 'implementation-dependent behavior' in documentation acknowledge that the boundary is porous, yet the API surface continues to claim full encapsulation. The theater rises over time as patches and workarounds accumulate — the boundary becomes a ritual of disclaimers rather than a functional seal.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows how identical base properties (extractiveness=0.52, suppression=0.58) produce radically different classifications across structural positions. The trapped developer sees Snare — pure extraction with no exit. The constrained maintainer sees Tangled Rope — genuine coordination function (enabling fast development) mixed with asymmetric extraction. The vendor sees Rope — coordination mechanism for market capture. The standardization body sees Scaffold — temporary problem being solved by formal specs with a sunset horizon. The legacy specification sees Piton — the original functional boundary has degraded into performative ritual. The analytical observer risks seeing Mountain — 'abstraction always leaks' is naturalized as computational law — but the structural data reveals this as a false summit: the leakage is engineered, not inevitable. The perspectival gap reveals that the abstraction boundary collapse is not a single constraint but a cluster of structural phenomena experienced differently by agents with different power and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position. The trapped dependent developer (d ≈ 0.95) experiences maximum extraction — high power ceiling, no exit, forced adaptation. The constrained library maintainer (d ≈ 0.55) experiences moderate extraction — they benefit from market position but are constrained by backward compatibility, so they experience mixed costs and benefits. The arbitraging vendor (d ≈ 0.05) experiences negative extraction (net benefit) — they can exit via new abstraction designs and control the optimization pathway. The standardization body's constrained position (d ≈ 0.50) reflects symmetric cost-benefit: they benefit from stricter standards but are constrained by need for vendor consensus. The analytical observer (d ≈ 0.72) sees the system as a whole and cannot exit the abstraction tier itself, so experiences moderate extraction from the structural tension.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids the mislabeling trap by distinguishing coordination function from extraction mechanism. The maintainer's perspective (Tangled Rope) captures both: the abstraction genuinely enables fast development (coordination) AND creates optimization control asymmetry (extraction). The dependent's perspective (Snare) captures pure extraction because they have no alternative and cannot benefit from the coordination. The vendor's perspective (Rope) captures coordination because they can arbitrage. The scaffold perspective confirms that formal standardization can rebuild the boundary — the sunset is real, not aspirational. The piton perspective identifies that legacy specifications have become performative rather than functional. The false mountain claim is detected by the structural data: the leakage is not inevitable but engineered (poor encapsulation design, insufficient error handling, vendor incentive misalignment). The mandatrophy is resolved by showing that all six types are structurally accurate from different positions — the constraint is a presheaf of observations, not a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    encapsulation_sufficiency_threshold,
    'What level of internal implementation detail is unavoidable for error diagnostics without making the abstraction intractable?',
    'Comparative analysis of well-encapsulated systems (ML frameworks with rich error messages) vs over-protected systems (legacy APIs with cryptic errors); measurement of user ability to debug failures at each level',
    'If threshold is high (much detail necessary): leakage is partly structural (Rope from some perspectives). If threshold is low (little detail necessary): leakage is contingent (Snare from all perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(encapsulation_sufficiency_threshold, empirical, 'How much internal detail is structurally necessary for diagnostics').

omega_variable(
    backward_compatibility_enforcement_cost,
    'Does maintaining abstraction boundary fidelity impose unacceptable performance or feature velocity penalties on the vendor?',
    'Cost-benefit analysis of strict vs pragmatic boundaries; case studies of frameworks that prioritized boundary fidelity vs those that prioritized feature speed; correlation between abstraction strictness and adoption rates',
    'If penalties are real and severe: the library maintainer cannot enforce the boundary without business risk (Tangled Rope confirmed). If penalties are manageable: boundary enforcement is a choice, not structural (Snare confirmed from dependent perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(backward_compatibility_enforcement_cost, empirical, 'Cost to vendors of maintaining strict abstraction boundaries').

omega_variable(
    standardization_enforceability,
    'Can formal specifications and test suites actually prevent implementation leakage, or do vendor variations always outpace the standard?',
    'Historical analysis of standards (CSS, JavaScript, HTML) that achieved strict boundaries vs those that fragmented; measurement of vendor conformance over time; identification of systematic pressure points where vendors diverge',
    'If enforceable: scaffold sunset is real — standardization can rebuild abstraction boundaries. If not enforceable: standardization is aspirational (Piton perspective confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(standardization_enforceability, empirical, 'Whether standardization can enforce abstraction boundaries against vendor pressure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abstraction_boundary_overrun, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abst_tr_t0, abstraction_boundary_overrun, theater_ratio, 0, 0.35).
narrative_ontology:measurement(abst_tr_t3, abstraction_boundary_overrun, theater_ratio, 3, 0.52).
narrative_ontology:measurement(abst_tr_t6, abstraction_boundary_overrun, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(abst_be_t0, abstraction_boundary_overrun, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(abst_be_t3, abstraction_boundary_overrun, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(abst_be_t6, abstraction_boundary_overrun, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abstraction_boundary_overrun, information_standard).
narrative_ontology:affects_constraint(abstraction_boundary_overrun, api_surface_stability).
narrative_ontology:affects_constraint(abstraction_boundary_overrun, vendor_lock_in_cycle).

% DUAL FORMULATION NOTE:
% The abstraction boundary overrun is downstream of specific API design choices and upstream of dependent system failures. Separate constraint stories could be written for: (1) the vendor's optimization control (high ε), (2) the dependent's workaround burden (ε varies by abstraction maturity), and (3) the standardization body's specification effort (low ε, scaffolding function). This story integrates all three perspectives on the single boundary condition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abstraction_boundary_overrun, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
