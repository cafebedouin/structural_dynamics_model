% ============================================================================
% CONSTRAINT STORY: coordination_game_standard
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coordination_game_standard, []).

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
 *   constraint_id: coordination_game_standard
 *   human_readable: Coordination Game Standard
 *   domain: game_theory/coordination/information_standards
 *
 * SUMMARY:
 *   A coordination game standard is a shared convention, protocol, or format
 *   that solves a collective action problem by providing focal points for
 *   distributed decisions. Examples include: technical standards (ISO 8601
 *   for date formats, TCP/IP for networking), measurement units (SI system,
 *   Gregorian calendar), social norms (driving on the left or right), market
 *   conventions (currency denominations), and communication protocols
 *   (written language orthography). The constraint operates at the
 *   intersection of game theory and institutional economics. Each participant
 *   faces an incentive structure where their payoff depends on matching
 *   others' choices. Without a standard, coordination fails — actors choose
 *   incompatibly and all suffer. The standard solves this by providing mutual
 *   expectations. Extractiveness is minimal (0.08) because the standard's
 *   function is pure alignment — no agent extracts surplus from others; all
 *   benefit from successful coordination. Suppression (0.12) reflects
 *   switching costs and path dependence: once established, standards are
 *   difficult to change, and latecomers face higher adoption costs. Theater
 *   (0.15) reflects the minimal performative layer — standards require
 *   ceremonial reaffirmation and maintenance, but their core function is
 *   transparent. This constraint is a canonical Rope: pure coordination
 *   mechanism with low coercion, low extraction, and low theatrical overhead.
 *
 * KEY AGENTS:
 *   - All Coordinating Parties: Uniform beneficiaries (organized/constrained) — all benefit from successful coordination; none extract from others
 *   - Standards-Setting Institution: Primary actor managing evolution (institutional/arbitrage) — ISO, IETF, open-source communities, or dominant firm setting de facto standards
 *   - Individual Participants: Embedded actors (powerless/mobile at biographical, locked at generational) — make coordination decisions within infrastructure maintained by prior generations
 *   - Latecomer Cohorts: Secondary actors (moderate/constrained) — face higher switching costs than pioneers but benefit from standard once adopted
 *   - Analytical Observer: Civilizational viewpoint (analytical/analytical) — sees the standard as pure coordination solving an irreducible collective action problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coordination_game_standard, 0.08).
domain_priors:suppression_score(coordination_game_standard, 0.12).
domain_priors:theater_ratio(coordination_game_standard, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coordination_game_standard, extractiveness, 0.08).
narrative_ontology:constraint_metric(coordination_game_standard, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(coordination_game_standard, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coordination_game_standard, rope).
narrative_ontology:human_readable(coordination_game_standard, "Coordination Game Standard").
narrative_ontology:topic_domain(coordination_game_standard, "game_theory/coordination/information_standards").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coordination_game_standard, all_coordinating_parties).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL PARTICIPANT (ROPE) — Any single actor can theoretically exit the standard and defect, but doing so is costly only to themselves (loss of coordination benefit). From the individual's biographical perspective, the standard is negotiable. From generational view, the standard becomes immutable — the participant is embedded in a coordination infrastructure maintained by thousands of prior agreements. Low suppression because defection is structurally possible; low extraction because the standard does not extract FROM the individual, it extracts the coordination benefit FOR them.
constraint_indexing:constraint_classification(coordination_game_standard, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: COORDINATING COALITION (ROPE) — Organized actors (firms, governments, standards bodies) experience the constraint as pure coordination: the standard solves a collective action problem. Exit is costly for large coalitions (shifting millions of users to a new standard) but not impossible. The coalition benefits uniformly from the standard's existence. Suppression derives from coordination lock-in (high switching costs) rather than coercion, and is therefore classified as constraint rather than trapped.
constraint_indexing:constraint_classification(coordination_game_standard, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: STANDARDS-SETTING INSTITUTION (ROPE) — The institution that maintains or evolves the standard (ISO, IETF, IEEE, etc.) has maximum arbitrage: they can propose new standards, migrate populations, or broker transitions. They see the coordination game as their function — no extraction, pure coordination benefit. The institution benefits from the standard's existence and adoption, but this is alignment with their stated purpose, not extraction. Very low theater because the standard's function (enabling coordination) is entirely transparent.
constraint_indexing:constraint_classification(coordination_game_standard, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From civilizational/universal scope, the coordination game standard is pure coordination. No agent extracts from others; all benefit from successful coordination. The standard solves an irreducible collective action problem: without shared expectations, coordination fails and all parties suffer. Extraction is zero; suppression is low (no coercion); theater is near-zero (function is transparent). This constraint is a canonical rope — pure coordination mechanism with minimal overhead.
constraint_indexing:constraint_classification(coordination_game_standard, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coordination_game_standard_tests).
:- end_tests(coordination_game_standard_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The coordination game standard extracts nothing from any participant — its entire function is to align expectations so all parties benefit. Any extractiveness above zero derives from: (1) transition costs for latecomers (measurement as 0.02-0.03), and (2) minimal administrative overhead to maintain and evolve the standard (0.03-0.04). These are coordination costs, not extraction. If the measured extractiveness included proprietary licensing fees, vendor lock-in, or gatekeeping by dominant firms, the constraint would reclassify as tangled_rope or snare — but the pure coordination game standard has no such mechanisms. Suppression (0.12): Low-moderate. Switching costs are real: migrating a large population from one standard to another is costly, and individuals embedded in the standard infrastructure cannot easily opt out at biographical timescale. But suppression derives from coordination lock-in (inherent to multi-party systems) rather than coercion or threat. Defection is theoretically possible; the cost is self-inflicted loss of coordination benefit, not external punishment. Theater ratio (0.15): Low. The standard's function is transparent: enable coordination. There is minimal performative layer — standards bodies do engage in ceremonial ratification and periodic review, but these are genuinely functional (ensuring the standard evolves with technology and use cases) rather than theatrical. The near-zero theater distinguishes this from a piton, where function has atrophied and theater sustains the form.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives converge on the Rope classification, indicating a uniform-type constraint. The perspectival gap emerges not in classification type but in experienced constraint strength. Individual participants experience low suppression at biographical timescale (they can theoretically defect) but high suppression at generational timescale (they are embedded in infrastructure they did not build). Organized coalitions experience constraint at intermediate timescale (migration is costly but achievable). Standards-setting institutions experience minimal constraint — they have arbitrage (proposing new standards, brokering transitions). The analytical observer sees no extraction, constraint, or coercion at any timescale — the standard is pure coordination. These gaps are perspectival rather than classificatory: all agree the mechanism is rope, but the experienced immutability varies by power and time horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is uniformly low across all perspectives because there is no extraction flow: beneficiaries and victims are the same set (all coordinating parties). The derivation chain computes: all agents are beneficiaries (d lowered), none are victims (d not raised), exit options vary (biographical mobile → lower d; generational constrained → higher d; arbitrage → very low d; analytical → neutral). The sigmoid f(d) produces near-zero effective extraction (chi) because the base extraction (epsilon) is minimal and d is near 0.5 or below across all perspectives. This is the diagnostic signature of pure coordination: directionality is not driven by power asymmetry but by temporal embeddedness.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by observing that all six types would misrepresent the coordination game standard. Mountain would falsely naturalize a contingent social/institutional arrangement. Tangled_rope would require both genuine coordination AND asymmetric extraction — this constraint has coordination but no asymmetry (unless captured, per omega_3). Snare would require suppression ≥ 0.60 and χ ≥ 0.66 — extractiveness here is 0.08. Scaffold would require χ ≤ 0.30 and theater ≤ 0.70 — true, but scaffold implies a sunset, whereas coordination game standards persist (they evolve, but do not sunset). Piton would require theater ≥ 0.70 — but theater here is low (function is transparent). Only Rope fits: χ ≤ 0.35, base extraction ε ≤ 0.45, suppression ≥ 0.40. The constraint's metrics satisfy the rope gate precisely because it is pure coordination: low extraction, low suppression (coordination lock-in is not coercion), and all perspectives agree.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latecomer_asymmetry,
    'Do latecomers to an established standard bear asymmetric costs compared to first movers?',
    'Historical analysis of technology adoption curves; comparison of switching costs across temporal cohorts; measurement of migration friction in network effects',
    'If true: standard may contain embedded extraction (early adopters benefit disproportionately). If false: standard is pure coordination with uniform benefit distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latecomer_asymmetry, empirical, 'Whether network effects create latecomer asymmetry').

omega_variable(
    lock_in_vs_coordination,
    'Is suppression measured by switching costs a legitimate coordination constraint or disguised extraction?',
    'Decompose switching costs into: (a) coordination lock-in (inherent to multi-party systems), vs (b) artificial switching barriers (proprietary formats, predatory licensing, vendor lock-in). Measure each separately.',
    'If dominated by coordination lock-in: rope classification confirmed. If dominated by artificial barriers: reclassify as snare or tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lock_in_vs_coordination, conceptual, 'Distinguish genuine coordination lock-in from extractive switching barriers').

omega_variable(
    standards_capture,
    'Can dominant firms or coalitions capture the standards-setting process to extract rents from subordinate adopters?',
    'Institutional analysis of standards bodies; tracking of voting power, proposal influence, and patent licensing terms; comparison of standards-setting governance across ISO, IETF, de facto (industry), and open-source models',
    'If common: standards are tangled_rope or snare disguised as rope. If rare: rope classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standards_capture, empirical, 'Prevalence of standards-setting body capture').

omega_variable(
    pareto_frontier_selection,
    'Do multiple Pareto-optimal equilibria exist, and if so, which gets selected as the standard?',
    'Game-theoretic analysis of equilibrium set; historical examination of standards adoption (Bell System vs competitors, Intel vs AMD, VHS vs Betamax, etc.); identification of selection mechanisms (path dependence, power, luck)',
    'If multiple optima with power-driven selection: reclassify as tangled_rope. If single optimum or neutral selection: rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pareto_frontier_selection, empirical, 'Structure of equilibrium set and selection mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coordination_game_standard, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cgs_tr_t0, coordination_game_standard, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cgs_tr_t25, coordination_game_standard, theater_ratio, 25, 0.14).
narrative_ontology:measurement(cgs_tr_t50, coordination_game_standard, theater_ratio, 50, 0.16).

% Extraction over time
narrative_ontology:measurement(cgs_be_t0, coordination_game_standard, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(cgs_be_t25, coordination_game_standard, base_extractiveness, 25, 0.07).
narrative_ontology:measurement(cgs_be_t50, coordination_game_standard, base_extractiveness, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coordination_game_standard, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
