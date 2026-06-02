% ============================================================================
% CONSTRAINT STORY: interaction_explosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:directionality_override/3,
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
 *   The combinatorial complexity trap emerges when multiple simple
 *   coordination mechanisms (each individually a Rope with low extraction)
 *   interact in non-linear fashion, creating exponential growth in
 *   interaction patterns. Early adopters benefit from first-mover advantage
 *   and simplified interface complexity. Late joiners face multiplicative
 *   rule sets and mandatory compatibility layers. Coordinating institutions
 *   capture value through fee structures and gatekeeper roles. The constraint
 *   exhibits the full diagnostic spectrum: late joiners experience it as
 *   Snare (trapped by combinatorial burden), medium-scale organizations as
 *   Tangled Rope (mixed coordination and extraction), coordinating
 *   authorities as Rope (coordination function dominates), legacy
 *   infrastructure as Piton (performative maintenance theater),
 *   simplification movements as Scaffold (real exit pathway), and formal
 *   complexity analysis as Mountain (mathematical necessity). The trajectory
 *   shows extractiveness rising from 0.28 to 0.52 over 20 years as
 *   interaction density compounds, and theater ratio rising from 0.32 to 0.58
 *   as backwards-compatibility layers become increasingly performative. This
 *   is a canonical example of institutional accumulation without
 *   renormalization.
 *
 * KEY AGENTS:
 *   - Early Adopter Coalitions: Primary beneficiary (institutional/arbitrage) — captured value through simplified interface complexity and lock-in benefits
 *   - Coordinating Institutions: Primary beneficiary (institutional/arbitrage) — standards bodies, platform owners capture fees and gatekeeper control through compatibility enforcement
 *   - Late-Joining Participants: Primary victim (powerless/trapped) — face exponential interaction complexity with no exit option except system abandonment
 *   - Scaling Organizations: Secondary victim (moderate/constrained) — integrated early mechanisms now forced to manage exponential compatibility requirements
 *   - Coordination Capacity Commons: Victim (powerless/trapped) — abstract collective good; total system coordination overhead becomes unsustainable
 *   - Systems Interoperability: Victim (powerless/trapped) — degraded by accumulating incompatibilities and patch layers
 *   - Simplification Movement: Organized agent (organized/mobile) — technical community building alternative architectures with genuine exit pathway
 *   - Legacy Compliance Infrastructure: Institutional actor (organized/constrained) — maintains backwards-compatibility theater; constrained by switching costs
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
narrative_ontology:constraint_beneficiary(interaction_explosion, early_adopter_coalitions).
narrative_ontology:constraint_beneficiary(interaction_explosion, coordinating_institutions).
narrative_ontology:constraint_victim(interaction_explosion, late_joiners).
narrative_ontology:constraint_victim(interaction_explosion, coordination_capacity_commons).
narrative_ontology:constraint_victim(interaction_explosion, systems_interoperability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATE-JOINING PARTICIPANT (SNARE) — New entrants face combinatorial explosion of interaction rules. Each simple coordination mechanism adds multiplicative complexity rather than additive. Must master N×M×K interaction patterns. Cannot exit without abandoning participation entirely. Bears maximum extraction cost as coordination burden scales exponentially. Trapped by sunk costs and network effects.
constraint_indexing:constraint_classification(interaction_explosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SCALING ORGANIZATION (TANGLED ROPE) — Medium-sized organization that integrated early coordination mechanisms now faces exponential cost to maintain compatibility as new mechanisms emerge. Benefits from first-mover advantage and lock-in, but increasingly constrained by need to manage interaction complexity. Extraction comes through forced upgrade cycles and mandatory compatibility layers. Some exit mobility through legacy system maintenance, but increasingly constrained.
constraint_indexing:constraint_classification(interaction_explosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COORDINATING AUTHORITY (ROPE) — Central coordinating body (standards body, platform owner, regulatory agency) experiences the complexity explosion as a coordination problem to be solved through meta-coordination. Arbitrage exit through delegating complexity management to sub-coordinators. Benefits from fee structures, licensing, and influence over compatibility rules. Extraction is subordinate to coordination function — the coordination is primary.
constraint_indexing:constraint_classification(interaction_explosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LEGACY COMPLIANCE INFRASTRUCTURE (PITON) — Degraded ecosystem of backwards-compatibility layers, translation middleware, and version management systems. Originally functional coordination mechanisms, now largely performative maintenance theater. Persists through institutional inertia — replacing it would require renormalizing the entire interaction graph. Theater ratio high because much effort goes to maintaining appearance of seamless integration rather than actual integration.
constraint_indexing:constraint_classification(interaction_explosion, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SIMPLIFICATION MOVEMENT (SCAFFOLD) — Organized technical community (working groups, open-source coalitions, academic initiatives) pushing toward interface consolidation, zero-trust architectures, or fundamental redesigns. Sees the complexity trap as temporary coordination failure solvable through systematic simplification. Has genuine exit pathway through alternative architecture adoption. Sunset clause implicit in technical roadmaps for interaction collapse and re-normalization.
constraint_indexing:constraint_classification(interaction_explosion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COMPLEXITY THEORY (MOUNTAIN) — From a formal complexity perspective, combinatorial explosion of interaction states is an immutable consequence of graph density growth: N coordination mechanisms create at most O(N²) interaction patterns. This mathematical fact appears as natural law. However, structural data contradicts pure mountain classification — the extraction is contingent on institutional design choices (interaction coupling, compatibility enforcement, centralized gate-keeping), not mathematical necessity. False summit: naturalizes what is actually institutional entrenchment.
constraint_indexing:constraint_classification(interaction_explosion, mountain,
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
 *   Base Extractiveness (0.52): Moderate-high. The mechanism is not pure extraction (no suppression ≥ 0.60, no χ ≥ 0.66 from all perspectives) — coordinating institutions genuinely solve coordination problems. However, extractiveness is significant because (a) late joiners face 10-50× complexity multiplier versus early adopters on identical functionality; (b) coordinating institutions enforce compatibility rules that maximize gatekeeper control rather than minimize total complexity; (c) institutional design choices (tight coupling, centralized standards setting) compound the mathematical complexity explosion. Suppression (0.65): High. Barriers to exit include network effects (adoption value correlates with user base), sunk costs (investment in compatibility layers), contractual lock-in, and technical debt from accumulated patches. However, suppression is not absolute — simplification movements demonstrate viable alternative architectures exist; exit is costly but not impossible. Theater Ratio (0.58): Moderate-high and rising. Legacy compatibility infrastructure performs increasingly theatrical function as interaction patterns exceed human cognitive load. Much effort goes to maintaining *appearance* of seamless integration (versioning rituals, deprecation theater, migration guides) rather than actual reduction of complexity. Theater rises over interval as system ages and technical debt accumulates.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon produces radically different classifications across observer positions. The coordinating authority's Rope (legitimate problem-solving) is the late-joiner's Snare (inescapable complexity trap). The early adopter's arbitrage exit is the scaling organization's constrained trap. The mathematical mountain (inevitability of O(N²) interactions) collapses into institutional contingency when examined through the lens of structural design choices. The scaffold's genuine exit pathway (simplification) appears invisible to trapped late-joiners (piton theater masks the real possibility of redesign). The perspectival collapse reveals that 'combinatorial complexity' as a label obscures six distinct structural phenomena operating on the same substrate.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural positions. Early adopters and coordinating institutions are beneficiaries with arbitrage options — low d values (≈0.15), resulting in negative or negligible experienced extraction despite moderate base extractiveness. Late joiners are victims with trapped exit options — high d values (≈0.95), resulting in maximum experienced extraction. Scaling organizations occupy intermediate position: they are mixed beneficiaries (from early adoption lock-in) and victims (from forced compatibility maintenance), generating d ≈ 0.55-0.65 and moderate experienced extraction. The derivation chain produces perspectival gaps automatically from these structural declarations, without requiring additional parameters. Overrides are not necessary; the canonical fallback values reproduce observed agent experiences.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED THROUGH STRUCTURAL DECOMPOSITION: The apparent paradox — 'is this Rope (coordination solution) or Snare (extraction mechanism)?' — dissolves when we recognize that the constraint IS Tangled Rope. The coordinating mechanisms are genuinely coordination (Rope function), but they interact through institutional design choices that create asymmetric extraction (Snare function). Both are real. The tension is not resolvable into a single type; the constraint is hybrid by structure. The extractiveness (0.52) reflects the balance: coordination function is real and reduces total system friction, but institutional gatekeeper control extracts value from late joiners and scaling organizations. The mandatrophy is resolved by acknowledging that Tangled Rope is the correct classification — not Rope misclassified as Snare, but a genuine hybrid that exhibits both coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interaction_coupling_necessity,
    'How many coordination mechanisms truly require tight coupling for their joint function, versus how many are coupled only by institutional convention?',
    'Architectural decomposition analysis; identification of hard dependencies vs policy-enforced compatibility; formal specification of true interaction surfaces',
    'If most coupling is conventional: the complexity explosion is extractive institutional design, not structural necessity. Significant downward revision of extractiveness to ~0.30. If tight coupling is pervasive: the constraint may approach a true mountain limit. Extractiveness remains ~0.52.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interaction_coupling_necessity, empirical, 'How many interaction couplings are necessary versus institutional').

omega_variable(
    modularization_barrier_depth,
    'What is the technical and organizational barrier depth to decomposing the entangled coordination system into independently-managed modules?',
    'Cost analysis of full architectural redesign; historical case studies of successful simplification efforts (IPv6 transition, database normalization refactoring); feasibility assessment of greenfield rebuilds',
    'If barrier is shallow (< 5 years, < 30% disruption cost): scaffold perspective dominates, constraint becomes time-limited. If barrier is deep (> 10 years, > 60% disruption): snare perspective dominates, constraint becomes structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(modularization_barrier_depth, empirical, 'Technical and organizational barrier to decomposition').

omega_variable(
    network_effect_lock_in_strength,
    'How much of the late-joiner extraction is driven by true network effects (value grows with adoption) versus sunk-cost fallacy and contractual lock-in?',
    'Comparative analysis of actual switching costs versus hypothetical costs in simplified architecture; measurement of network-value gradient at system boundaries',
    'If true network effects dominate: late-joiners genuinely benefit from complexity (mild snare → rope reclassification). If lock-in dominates: extraction is institutional design choice (snare classification confirmed, extractiveness may rise to 0.65).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_lock_in_strength, empirical, 'Strength of network effects versus lock-in mechanisms').

omega_variable(
    simplification_architecture_viability,
    'Do proposed simplification architectures (zero-trust, minimal interaction surface, protocol consolidation) actually reduce complexity or merely shift it to different layers?',
    'Formal complexity analysis of alternative architectures; measurement of interaction explosion under proposed designs; comparison of operational burden (setup, maintenance, debugging)',
    'If simplification is viable: scaffold sunset is real, constraint is time-limited to ~15 years. If complexity merely shifts layers: scaffold perspective is aspirational, and the system may remain entrapped (piton perspective dominates long-term).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simplification_architecture_viability, empirical, 'Whether simplification architectures reduce or merely shift complexity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(interaction_explosion, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iexp_tr_t0, interaction_explosion, theater_ratio, 0, 0.32).
narrative_ontology:measurement(iexp_tr_t10, interaction_explosion, theater_ratio, 10, 0.45).
narrative_ontology:measurement(iexp_tr_t20, interaction_explosion, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(iexp_be_t0, interaction_explosion, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(iexp_be_t10, interaction_explosion, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(iexp_be_t20, interaction_explosion, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(interaction_explosion, enforcement_mechanism).
narrative_ontology:affects_constraint(interaction_explosion, technical_debt_accumulation).
narrative_ontology:affects_constraint(interaction_explosion, standards_proliferation_trap).
narrative_ontology:affects_constraint(interaction_explosion, backwards_compatibility_ratchet).

% DUAL FORMULATION NOTE:
% The combinatorial complexity trap decomposes into three dependent constraints: (1) technical_debt_accumulation (ε≈0.35, Piton) — degradation of system maintainability through patch layers; (2) standards_proliferation_trap (ε≈0.48, Tangled Rope) — meta-level coordination problem of coordinating coordinators; (3) backwards_compatibility_ratchet (ε≈0.42, Tangled Rope) — institutional lock-in mechanism that enforces continued coupling. The interaction explosion is downstream of all three but represents a distinct structural constraint at the systems level. Each has different extractiveness; their combination produces the observed constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(interaction_explosion, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
