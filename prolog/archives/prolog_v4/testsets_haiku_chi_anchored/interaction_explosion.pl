% ============================================================================
% CONSTRAINT STORY: interaction_explosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_interaction_explosion, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: interaction_explosion
 *   human_readable: The Combinatorial Complexity Trap
 *   domain: technological/social
 *
 * SUMMARY:
 *   The Combinatorial Complexity Trap occurs when multiple simple
 *   coordination mechanisms (each individually a Rope) interact non-linearly,
 *   creating emergent constraints that disadvantage newcomers and late
 *   adopters. What begins as straightforward system design—a few basic
 *   protocols, a few interfaces, a few configuration standards—undergoes
 *   explosive growth in interaction complexity as the number of possible
 *   interaction paths scales combinatorially. Early adopters and system
 *   architects benefit from the first-mover advantage: they understand the
 *   system's interaction surface implicitly, can navigate the
 *   social/technical conventions, and influence how new standards are set.
 *   Late entrants face an exponentially growing surface of interaction rules,
 *   undocumented conventions, and architectural assumptions. The constraint
 *   manifests across technological systems (software platforms with multiple
 *   frameworks, hardware ecosystems, scientific instrument interfaces),
 *   organizational structures (matrix organizations with overlapping
 *   reporting lines), and social networks (communities with evolved informal
 *   norms). The 'trap' lies in the feedback loop: each added mechanism
 *   increases complexity, each increase makes it harder for newcomers to
 *   participate meaningfully, reduced participation diversity makes
 *   architects less responsive to refactoring needs, and unremodularized
 *   complexity deepens. Theater ratio increases as standards bodies attempt
 *   to document complexity that has become too large to fully specify,
 *   creating a performative documentation effort that doesn't reduce actual
 *   cognitive burden.
 *
 * KEY AGENTS:
 *   - System Architects: Primary beneficiary (institutional/arbitrage) — control initial interaction patterns and reap coordination advantage; can exit by redesigning or creating new ecosystem
 *   - Early Adopters: Secondary beneficiary (powerful/mobile) — benefit from ecosystem growth and network effects while retaining capacity to fork or switch
 *   - Late Entrants: Primary victim (powerless/trapped) — face exponentially growing interaction surface with no choice but to master it; cannot easily exit or redesign
 *   - System Maintainers: Mixed victim/participant (moderate/constrained) — bear the implementation cost of complexity (testing, documentation, refactoring) while trapped in architectural decisions they didn't control
 *   - Coordination Commons: Abstract victim (powerless/trapped) — the collective epistemic commons loses clarity as complexity increases; system becomes opaque to outside observers, auditors, formal verification
 *   - Standards Coalition: Institutional actor (organized/constrained) — attempts to manage complexity through documentation and standardization; increasingly performative as standards proliferate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(interaction_explosion, 0.52).
domain_priors:suppression_score(interaction_explosion, 0.65).
domain_priors:theater_ratio(interaction_explosion, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(interaction_explosion, extractiveness, 0.52).
narrative_ontology:constraint_metric(interaction_explosion, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(interaction_explosion, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(interaction_explosion, tangled_rope).
narrative_ontology:human_readable(interaction_explosion, "The Combinatorial Complexity Trap").
narrative_ontology:topic_domain(interaction_explosion, "technological/social").

domain_priors:requires_active_enforcement(interaction_explosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(interaction_explosion, system_architects).
narrative_ontology:constraint_beneficiary(interaction_explosion, early_adopters).
narrative_ontology:constraint_victim(interaction_explosion, late_entrants).
narrative_ontology:constraint_victim(interaction_explosion, coordination_commons).
narrative_ontology:constraint_victim(interaction_explosion, system_maintainers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE ENTRANT (SNARE) — Cannot exit the combinatorial trap once multiple coordination mechanisms are entrenched. Must master exponentially increasing interaction surface to participate. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.80.
constraint_indexing:constraint_classification(interaction_explosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SYSTEM MAINTAINER (TANGLED ROPE) — Benefits from functional decomposition and modular interface documentation (coordination function); constrained by exponential growth in test coverage and interaction specification requirements (extraction). d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(interaction_explosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SYSTEM ARCHITECT (ROPE) — Benefits from first-mover coordination advantage and network effects. Experiences constraint as pure coordination problem: establishing clear protocols and standards simplifies future development. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(interaction_explosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EARLY ADOPTER (TANGLED ROPE) — Benefits from ecosystem growth and complementary innovation (coordination); faces increasing surface area of interdependencies and breaking changes (extraction). Has mobility to switch or fork if necessary. d≈0.45, f(d)≈0.62, σ=1.2 → χ≈0.32.
constraint_indexing:constraint_classification(interaction_explosion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: STANDARDS COALITION (PITON) — Organized effort (ISO, W3C, IEEE working groups) to manage interaction complexity through standardization. Theater ratio=0.64: much effort goes to document maintenance and compliance theater, but actual standard adoption only reduces complexity locally. The coalition perceives diminishing returns: standards proliferation itself becomes a coordination problem. χ≈0.28; low effective extraction because the coalition has organizational capacity but sees the game degrading.
constraint_indexing:constraint_classification(interaction_explosion, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, combinatorial interaction complexity is partly inherent to systems with many degrees of freedom (coordination challenge: how do you document n² interactions?) and partly engineered (extraction: architectures that prioritize early-mover advantage over modularity). The interaction explosion exhibits both universal features and contingent design choices. This perspective identifies the tangled rope structure: real coordination function (managing complexity is necessary) + real extraction (asymmetric advantage for those who set the initial interaction patterns).
constraint_indexing:constraint_classification(interaction_explosion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(interaction_explosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(interaction_explosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(interaction_explosion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(interaction_explosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(interaction_explosion, TR),
    TR >= 0.70.

:- end_tests(interaction_explosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The trap exhibits measurable extraction: late entrants must invest significantly more effort to achieve equivalent participation compared to early adopters. However, the extraction is not total coercion (ε < 0.66) because late entrants can technically participate through sustained learning; the system doesn't lock them out entirely, only raises the cost. The measurement trajectory (0.22 → 0.52) reflects the feedback loop: initial complexity is modest, but as mechanisms accumulate, extraction accelerates. Suppression (0.65): Moderate-high. The main suppression mechanism is learning curve + network effects: alternatives exist (fork the system, create a new architecture) but are costly due to ecosystem lock-in. Documentation barriers and social convention opacity increase suppression. Theater ratio (0.58, trending upward): The constraint exhibits rising performativity as standards efforts escalate. Early stages (0.35) show mostly functional coordination: people learning the real interaction patterns. By stage 7 (0.58), significant effort goes to spec-writing, compliance checking, and standards governance that doesn't proportionally reduce experienced complexity. This suggests the system is entering piton-like degradation—the coordination function is weakening relative to the overhead of managing it.
 *
 * PERSPECTIVAL GAP:
 *   The system architects experience coordination (Rope) because they control the initial conditions and set the rules. The late entrants experience extraction (Snare) because they face a fixed, high-dimensional interaction surface. Early adopters occupy the middle ground (Tangled Rope)—they benefit from coordination and ecosystem effects but increasingly face the same refactoring burden as maintainers. The analytical observer perceives a tangled rope with legitimate coordination content (managing many degrees of freedom requires some shared protocols) and legitimate extraction content (but the protocols were designed to benefit early participants). The standards coalition perceives itself as managing complexity but increasingly as executing performative documentation—they see Piton features (degraded function, high theater, persistent through inertia). This gap reveals that the constraint is not purely extractive (there is a real coordination challenge) but not purely coordinative either (the architecture choices systematize advantage). The gap widens over time as theater increases—early stakeholders see mostly 'real' complexity management, later stakeholders see mostly 'overhead'.
 *
 * DIRECTIONALITY LOGIC:
 *   System architect: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; can exit anytime by designing new system. Late entrant: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; trapped by ecosystem lock-in. System maintainer: Victim + constrained → d≈0.68, f(d)≈1.02. High extraction; constrained by need to maintain backward compatibility and organizational stability. Early adopter: Mixed (beneficiary + victim) + mobile → d≈0.45, f(d)≈0.62. Symmetric position; can benefit from ecosystem but also feels extraction creep. Standards coalition: Organized + constrained → d≈0.35, f(d)≈0.28. Low effective extraction; organized capacity but seeing diminishing returns on standardization efforts. Coordination commons: Victim + trapped → d≈0.90, f(d)≈1.35. Abstract victim; cannot organize or exit, bears cost of system opacity.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint avoids the mandatrophy (tangled rope vs snare confusion) by explicitly declaring both coordination and extraction in the base properties. The beneficiaries (architects, early adopters) are documented alongside victims (late entrants, maintainers). The constraint is tangled_rope because: (1) It has a genuine coordination function—managing interaction between many components is necessary, and protocols/standards provide value. (2) It has asymmetric extraction—the protocols were designed in ways that benefit early participants over later ones. (3) It requires active enforcement—maintaining backward compatibility, enforcing architectural decisions, and resisting refactoring. The omega variables address whether the extraction is inherent (unavoidable in any N-way system) or engineered (could be different with better upfront design). The measurement trajectory (extractiveness rising from 0.22 to 0.52) shows the feedback loop intensifying, which confirms the tangled_rope classification over time: what begins as mostly coordination (low ε) becomes increasingly extractive (ε > 0.46) as late entrants encounter the compounded surface. The analytical observer's perspective correctly identifies the tangled rope: not a false summit naturalizing contingent choices, but a real observation that the system has both coordination and extraction content. The standards coalition's piton perspective reveals the degradation mechanism: as theater rises (documentation overhead without proportional complexity reduction), the coordination function weakens relative to the enforcement overhead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_vs_engineered_complexity,
    'What fraction of combinatorial explosion is inherent to N-way systems versus engineered by architecture choices that privileged early adopters?',
    'Comparative analysis of different system architectures (microkernel vs monolithic, strongly-typed vs duck-typed, protocol-based vs object-oriented); measurement of interaction surface growth rate vs theoretical minimum for equivalent functionality',
    'If ≥80% engineered: classification shifts toward snare/extraction for all perspectives. If ≤20% engineered: shifts toward rope/coordination. If mixed: confirms tangled_rope as correct type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inherent_vs_engineered_complexity, empirical, 'Ratio of engineered complexity to inherent complexity in interaction surface').

omega_variable(
    modularity_recovery_feasibility,
    'Can interaction complexity be decomposed by retrofitting stronger modularity boundaries, or does the system require fundamental redesign?',
    'Cost-benefit analysis of modularization efforts (microservices migration, API versioning strategies, plugin architectures); measurement of refactoring effort vs complexity reduction achieved',
    'If modularization effective: late entrants'' exit_options upgrade from trapped → constrained; snare perspectives shift toward tangled_rope. If not feasible: extractive lock-in is confirmed; perspectives remain snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modularity_recovery_feasibility, empirical, 'Whether modularity retrofit can reduce interaction complexity').

omega_variable(
    documentation_vs_internalization_trade,
    'Does increased documentation/specification of interaction rules reduce theater_ratio, or does it increase theater by creating maintenance burden without reducing experiential complexity?',
    'Survey of practitioner cognitive load before/after documentation; measurement of time spent reading specs vs actually learning by integration testing; correlation between spec thoroughness and adoption friction',
    'If docs reduce theater: theater_ratio should decline, piton perspective weakens. If docs increase theater: theater_ratio climbs; standards coalition perspective confirmed as piton (performative). Theater trend directly impacts the measurement trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_vs_internalization_trade, empirical, 'Whether documentation reduces actual complexity or just shifts it to documentation maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(interaction_explosion, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(intx_tr_t0, interaction_explosion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(intx_tr_t3, interaction_explosion, theater_ratio, 3, 0.45).
narrative_ontology:measurement(intx_tr_t7, interaction_explosion, theater_ratio, 7, 0.58).

% Extraction over time
narrative_ontology:measurement(intx_be_t0, interaction_explosion, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(intx_be_t3, interaction_explosion, base_extractiveness, 3, 0.35).
narrative_ontology:measurement(intx_be_t7, interaction_explosion, base_extractiveness, 7, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(interaction_explosion, information_standard).
narrative_ontology:affects_constraint(interaction_explosion, technical_debt_accumulation).
narrative_ontology:affects_constraint(interaction_explosion, standards_proliferation).
narrative_ontology:affects_constraint(interaction_explosion, ecosystem_lock_in).

% DUAL FORMULATION NOTE:
% The interaction explosion is downstream of individual design choices but represents a distinct structural constraint. Individual protocol coordination (e.g., HTTP standards, JSON schema) may be pure Rope; the interaction explosion between multiple such mechanisms is Tangled Rope. The network shows how coordination mechanisms couple and create emergent complexity that is qualitatively different from the sum of parts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
