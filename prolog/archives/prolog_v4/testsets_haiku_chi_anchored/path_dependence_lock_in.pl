% ============================================================================
% CONSTRAINT STORY: path_dependence_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_path_dependence_lock_in, []).

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
 *   constraint_id: path_dependence_lock_in
 *   human_readable: The QWERTY Inertia Trap: Path Dependence Lock-In
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The QWERTY keyboard layout represents one of the most persistent examples
 *   of technological path-dependent lock-in. Invented in 1873 to prevent
 *   mechanical typewriter jams, QWERTY was never optimized for typing speed
 *   or ergonomics. Superior layouts (Dvorak, Colemak) were designed based on
 *   letter frequency and ergonomic principles, yet they remain marginal
 *   despite 50+ years of availability. This constraint exhibits the full
 *   spectrum of Deferential Realism classifications depending on the
 *   observer's structural position. The constraint creates asymmetric
 *   extraction: incumbent manufacturers and legacy software vendors benefit
 *   from standardization (they experience pure coordination), while
 *   innovation entrants and efficiency seekers face insurmountable lock-in
 *   costs (they experience extraction). The constraint's theater ratio (0.58)
 *   reflects that typing pedagogy systems continue to teach QWERTY through
 *   institutional inertia rather than evidence-based optimization. The
 *   emergence of non-keyboard input modalities (voice, gesture, neural
 *   interfaces) represents a genuine sunset mechanism for the scaffold
 *   perspective: as keyboards become less central to human-computer
 *   interaction, the lock-in constraint degrades to irrelevance.
 *
 * KEY AGENTS:
 *   - Efficiency Seekers: Primary victim (powerless/trapped) — desire superior layouts but face global QWERTY standardization and zero alternative availability
 *   - Innovation Entrants: Secondary victim (moderate/trapped) — startups attempting to market superior layouts face coordination failure that becomes extraction through market structure
 *   - Typing Skill Holders: Mixed agent (moderate/constrained) — benefit from QWERTY skill value but locked out of upgrading to superior layouts due to sunk investment
 *   - Incumbent Keyboard Manufacturers: Primary beneficiary (institutional/arbitrage) — experience lock-in as pure coordination; no retooling costs, universal demand assumed
 *   - Legacy Software Vendors: Secondary beneficiary (institutional/arbitrage) — benefit from standardized keyboard assumptions in operating systems and applications
 *   - Typing Pedagogy System: Institutional actor (institutional/arbitrage) — perpetuates QWERTY through curriculum inertia; sees own teaching as performative rather than optimized
 *   - Digital Native Ecosystem: Organized agents (organized/mobile) — voice interfaces, mobile touch, gesture recognition provide alternative modalities reducing keyboard lock-in salience
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent lock-in as mathematical inevitability of network effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(path_dependence_lock_in, 0.52).
domain_priors:suppression_score(path_dependence_lock_in, 0.68).
domain_priors:theater_ratio(path_dependence_lock_in, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(path_dependence_lock_in, extractiveness, 0.52).
narrative_ontology:constraint_metric(path_dependence_lock_in, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(path_dependence_lock_in, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(path_dependence_lock_in, tangled_rope).
narrative_ontology:human_readable(path_dependence_lock_in, "The QWERTY Inertia Trap: Path Dependence Lock-In").
narrative_ontology:topic_domain(path_dependence_lock_in, "technological/economic").

domain_priors:requires_active_enforcement(path_dependence_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(path_dependence_lock_in, incumbent_keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(path_dependence_lock_in, legacy_software_vendors).
narrative_ontology:constraint_beneficiary(path_dependence_lock_in, typing_skill_holders).
narrative_ontology:constraint_victim(path_dependence_lock_in, efficiency_potential).
narrative_ontology:constraint_victim(path_dependence_lock_in, innovation_entrants).
narrative_ontology:constraint_victim(path_dependence_lock_in, adopters_of_superior_layouts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EFFICIENCY POTENTIAL (SNARE) — Cannot escape QWERTY's grip. Every device, every workplace, every learning pathway defaults to QWERTY. Adopters of Dvorak, Colemak, or other superior layouts face universal incompatibility, training isolation, and social friction. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.87.
constraint_indexing:constraint_classification(path_dependence_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INNOVATION ENTRANT (SNARE) — Startups attempting to market superior keyboard layouts face a coordination failure so severe it becomes extraction. Network effects create a moat: no keyboard manufacturer will produce the superior layout if no users demand it; no users will demand it if no keyboards exist. The lock-in is active enforcement through market structure, not through explicit coercion. d≈0.88, f(d)≈1.32, σ=0.9 → χ≈0.62.
constraint_indexing:constraint_classification(path_dependence_lock_in, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: TYPING SKILL HOLDERS (TANGLED ROPE) — Humans who have invested 10,000+ hours mastering QWERTY touch-typing experience the constraint as mixed. QWERTY locks in their skill value (coordination benefit — their expertise is universally valuable). But it also locks them OUT of adopting superior layouts even if they wanted to (extraction — sunk cost trap). d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.52.
constraint_indexing:constraint_classification(path_dependence_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT KEYBOARD MANUFACTURERS (ROPE) — Experience QWERTY lock-in as pure coordination. No retooling necessary. No inventory risk. No market education costs. The constraint solves their production coordination problem: they can assume universal QWERTY demand. Manufacturing coordination is automated. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(path_dependence_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY SOFTWARE VENDORS (ROPE) — Operating system makers and software vendors benefit from QWERTY standardization. No need to support layout switching gracefully. No localization complexity for keyboards (only for character sets). The constraint is coordination: 'everyone assumes QWERTY' makes product design simpler. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(path_dependence_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: TYPING PEDAGOGY SYSTEM (PITON) — Schools teach QWERTY through habit and default, not through active evidence of superiority. The teaching infrastructure is massive but largely performative: teachers teach QWERTY because 'that's what everyone uses,' not because pedagogical research supports it as optimal. Curriculum inertia masks degraded function. theater_ratio=0.58 (moderate-high theater). If pedagogical systems re-evaluated the choice from first principles, superior layouts would win, but institutional machinery defaults to path-dependent repetition.
constraint_indexing:constraint_classification(path_dependence_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: DIGITAL NATIVE ECOSYSTEM (SCAFFOLD) — Mobile devices, voice interfaces, and gesture-based input represent genuine alternative input modalities that bypass QWERTY entirely. As these mature, the keyboard lock-in loses functional importance. QWERTY matters less when your primary interface is voice or touch. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.21. Low effective extraction because the organizational agents (tech platforms) have agency and see a mobility path (non-keyboard interfaces). Sunset clause is implicit: as voice/gesture/neural interfaces mature over 20-30 years, the constraint becomes irrelevant.
constraint_indexing:constraint_classification(path_dependence_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NETWORK EFFECTS NATURAL LAW (MOUNTAIN) — From a civilizational view, path-dependent lock-in is presented as an inherent feature of network goods: whichever standard reaches critical mass first becomes immutable by mathematical necessity. This perspective sees QWERTY as locked by nature (network effects mathematics). However, the structural data (ε=0.52, suppression=0.68, theater=0.58) contradicts the mountain gate (ε ≤ 0.25). The engine will classify this as a false summit, revealing that what appears to be mathematical inevitability is actually a contingent equilibrium sustained by coordination failure and enforcement.
constraint_indexing:constraint_classification(path_dependence_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(path_dependence_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(path_dependence_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(path_dependence_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(path_dependence_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(path_dependence_lock_in, TR),
    TR >= 0.70.

:- end_tests(path_dependence_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from efficiency seekers and innovation entrants by preventing them from adopting demonstrably superior alternatives. The extraction is enforced not through explicit coercion but through network effects and coordination failure. The value increased over the interval (from 0.28 to 0.52) because digital globalization deepened QWERTY universalization: as computers proliferated worldwide with standardized layouts, the switching cost for any alternative rose exponentially. Suppression (0.68): High. Barriers to adopting superior layouts include: universal device standardization, complete absence of commercial keyboard production for alternatives, zero pedagogical support, social friction from non-standard input, incompatibility with inherited skill bases, and psychological sunk-cost commitment. These barriers are not formally imposed but are structurally enforced. Theater ratio (0.58): Moderate-high. Typing pedagogy teaches QWERTY through institutional momentum, not evidence. If educational systems chose based on ergonomic research alone, they would teach Dvorak or Colemak. The continued QWERTY teaching reflects theatrical compliance with 'how it has always been done,' masking degraded pedagogical function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The incumbent manufacturer sees efficient coordination (Rope) — their production pipeline assumes QWERTY, making their operations simpler. The efficiency seeker sees pure extraction (Snare) — they are locked into an inferior standard with no exit. The typing skill holder sees mixed coordination and extraction (Tangled Rope) — their expertise locks in their skill value but also locks them out of upgrading. The digital native ecosystem sees a temporary problem with a sunset (Scaffold) — non-keyboard interfaces will make keyboard layout optimization irrelevant. The typing pedagogy system sees a degraded ritual (Piton) — teaching QWERTY persists through inertia, not pedagogical merit. The analytical observer risks seeing mathematical inevitability (Mountain) — network effects make lock-in appear immutable — but the structural data reveals this as a false summit: the lock-in is contingent on switching costs, artificial standardization, and coordination failure, not on mathematical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent manufacturers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; they experience pure gain from standardization. Innovation entrants: Victim + trapped → d≈0.88, f(d)≈1.32. Maximum extraction; they face insurmountable coordination failure. Efficiency seekers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; they cannot exit without sacrificing skill value and social compatibility. Typing skill holders: Mixed (beneficiary and victim) + constrained → d≈0.65, f(d)≈1.00. Moderate extraction; they benefit from skill lock-in but are locked out from upgrading. Digital native ecosystem: Organized + mobile → d≈0.35, f(d)≈0.35. Low effective extraction; they have agency (can pivot to non-keyboard interfaces) and see a mobility path. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification would be false summit; observer risks naturalizing contingency.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by decomposing the constraint into its structural components: (1) Pure network effects (symmetric coordination) — QWERTY became standardized partly because coordination on any standard matters. (2) Switching cost trap (asymmetric extraction) — once standardized, changing layouts imposes massive costs only on would-be switchers, not on stayers. (3) Institutional inertia (piton mechanism) — pedagogy and product design perpetuate QWERTY through habit, not because alternatives were evaluated and rejected. The empirical literature (Liebowitz & Margolis) initially claimed QWERTY was nearly-optimal, supporting the pure-coordination (Rope) narrative. Later research (Steen) found that switching cost dominance far exceeds any layout efficiency difference, supporting the extraction (Snare) narrative. The constraint resolves as Tangled Rope because it contains BOTH genuine coordination (standardization value) AND asymmetric extraction (switching cost traps). The ε=0.52 reflects that the coordination benefit (~0.30) exists but is overwhelmed by extraction mechanism (~0.52 net). The scaffold perspective (digital natives with non-keyboard exit) resolves the long-term mandatrophy: as voice and gesture interfaces mature, keyboard layout lock-in becomes irrelevant, converting Tangled Rope back into pure historical artifact (Piton).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_threshold,
    'At what absolute switching cost does lock-in transition from coordination problem to extraction mechanism?',
    'Empirical measurement of switching costs (retraining, hardware replacement, software adaptation) vs. benefit curves; comparison with historical switching events (VHS→DVD, Flash→HTML5)',
    'If threshold is low (< 100 hours + $500): lock-in is primarily coordination failure, recoverable by small interventions. If threshold is high (> 10,000 hours + $50k): lock-in is deep extraction, requires major ecosystem shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_threshold, empirical, 'Switching cost threshold for lock-in classification').

omega_variable(
    dvorak_superiority_magnitude,
    'Is Dvorak actually measurably superior to QWERTY, and if so, by how much?',
    'Controlled ergonomic studies comparing typing speed, accuracy, and long-term injury rates for trained users; correction for practice-level confounds (most Dvorak users are enthusiasts, not representative)',
    'If clearly superior (>10% speed gain, measurably lower injury): lock-in is clearly extractive (trapping users in inferior standard). If marginal or unclear (< 5% advantage within noise): lock-in might be neutrally coordinating (which standard matters less than having one).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dvorak_superiority_magnitude, empirical, 'Magnitude of Dvorak superiority over QWERTY').

omega_variable(
    lock_in_irrelevance_timeline,
    'When will voice, gesture, and neural interfaces render keyboard layout optimization irrelevant?',
    'Market penetration analysis of non-keyboard input modalities; expert forecasting for neural interface maturity; correlation of keyboard-free interaction adoption with abandonment of keyboard standardization pressure',
    'If soon (< 15 years): scaffold perspective is accurate, constraint degrades to piton. If late (> 50 years): keyboard input remains primary long enough to entench multiple lock-in layers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lock_in_irrelevance_timeline, empirical, 'Timeline for non-keyboard interfaces to supersede keyboard layouts').

omega_variable(
    network_effects_completeness,
    'Are all observed lock-in effects due to true network effects (value increases with adoption), or do they include artificial enforcement (switching costs, compatibility deliberation)?',
    'Decomposition analysis: measure pure network effects (what value accrues from adoption itself) vs. switching cost traps vs. compatibility enforcement. Counterfactual: if switching costs were zero, would users stay with QWERTY?',
    'If purely network effects (pure coordination): lock-in is recoverable by any coordinating force (platform mandate, ecosystem shift). If artificial enforcement (extraction): lock-in persists even if users would prefer to switch.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(network_effects_completeness, conceptual, 'Whether lock-in is pure network effects or artificial enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(path_dependence_lock_in, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_tr_t0, path_dependence_lock_in, theater_ratio, 0, 0.35).
narrative_ontology:measurement(qwerty_tr_t50, path_dependence_lock_in, theater_ratio, 50, 0.48).
narrative_ontology:measurement(qwerty_tr_t100, path_dependence_lock_in, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(qwerty_be_t0, path_dependence_lock_in, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(qwerty_be_t50, path_dependence_lock_in, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(qwerty_be_t100, path_dependence_lock_in, base_extractiveness, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(path_dependence_lock_in, information_standard).
narrative_ontology:affects_constraint(path_dependence_lock_in, metric_system_standardization).
narrative_ontology:affects_constraint(path_dependence_lock_in, programming_language_fragmentation).
narrative_ontology:affects_constraint(path_dependence_lock_in, usb_connector_proliferation).

% DUAL FORMULATION NOTE:
% Path-dependent lock-in is a family of structurally identical constraints across multiple domains (QWERTY/keyboards, metric/imperial, USB standards, programming languages). Each domain story has its own ε reflecting domain-specific switching costs, but the structural mechanism is identical: initial coordination → increasing switching cost → extraction emerges. This story focuses on QWERTY; sibling constraints in metric systems and programming languages follow the same pattern with different magnitudes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(path_dependence_lock_in, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
