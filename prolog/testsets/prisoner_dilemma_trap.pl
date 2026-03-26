% ============================================================================
% CONSTRAINT STORY: prisoner_dilemma_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prisoner_dilemma_trap, []).

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
 *   constraint_id: prisoner_dilemma_trap
 *   human_readable: Prisoner's Dilemma Trap: Mutual Defection Under Payoff Asymmetry
 *   domain: game_theory/strategic_interaction
 *
 * SUMMARY:
 *   The prisoner's dilemma is a canonical game-theoretic constraint that
 *   traps rational players in mutually suboptimal outcomes. Both players have
 *   incentive to defect regardless of the other's choice, yet mutual
 *   defection yields lower payoffs than mutual cooperation. The constraint
 *   exhibits different classifications depending on observational position:
 *   the trapped player sees a snare with no exit; the defector sees rope or
 *   moderate extraction; the institutional observer sees a temporary scaffold
 *   being overcome through contracts and trust; the game theorist sees a
 *   pedagogical piton; the logical analyst sees an immutable natural law. The
 *   extractiveness (0.58) reflects moderate-to-high suppression of the
 *   cooperative outcome while theater ratio (0.35) indicates the dilemma is
 *   functionally descriptive rather than performative. This constraint is
 *   foundational to understanding strategic coordination failures across
 *   economics, politics, ecology, and conflict.
 *
 * KEY AGENTS:
 *   - Individual Player A: Primary victim (powerless/trapped) — locked into defection through fear of exploitation; cannot unilaterally achieve cooperation
 *   - Individual Player B: Primary victim (powerless/trapped) — symmetric trapped position; mutual defection emerges as equilibrium despite suboptimality
 *   - Cooperative Equilibrium: Abstract victim (powerless/trapped) — the mutually beneficial outcome is structurally inaccessible through individual rational action
 *   - Individual Defector (perspective): Beneficiary (moderate/constrained) — captures advantage from defection when others cooperate; optimal strategy under given payoff structure
 *   - Iterated Community: Moderate agent (moderate/constrained) — benefits from institutional evolution enabling partial cooperation; bears costs of reputation and enforcement maintenance
 *   - Institutional Innovation Agent: Organized actor (organized/mobile) — can exit trap through contracts, legal enforcement, reputation systems, and repeated-game mechanisms
 *   - Game Theory Framework: Analytical observer (analytical/analytical) — represents both the canonical theoretical form (piton) and the logical structure (mountain) depending on meta-level perspective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prisoner_dilemma_trap, 0.58).
domain_priors:suppression_score(prisoner_dilemma_trap, 0.65).
domain_priors:theater_ratio(prisoner_dilemma_trap, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prisoner_dilemma_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(prisoner_dilemma_trap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(prisoner_dilemma_trap, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prisoner_dilemma_trap, snare).
narrative_ontology:human_readable(prisoner_dilemma_trap, "Prisoner's Dilemma Trap: Mutual Defection Under Payoff Asymmetry").
narrative_ontology:topic_domain(prisoner_dilemma_trap, "game_theory/strategic_interaction").

domain_priors:requires_active_enforcement(prisoner_dilemma_trap).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(prisoner_dilemma_trap, individual_defectors).
narrative_ontology:constraint_victim(prisoner_dilemma_trap, cooperative_equilibrium).
narrative_ontology:constraint_victim(prisoner_dilemma_trap, mutual_benefit_option).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED COOPERATOR (SNARE) — Individual player cannot unilaterally exit defection cycle. If one player cooperates while the other defects, the cooperator receives the worst payoff (sucker's payoff). Fear of exploitation locks the player into defection despite knowing mutual cooperation yields better joint outcome. No escape option without binding commitment mechanisms that do not exist within the game structure. Maximum experienced extraction.
constraint_indexing:constraint_classification(prisoner_dilemma_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: MUTUAL DEFECTION OUTCOME (SNARE) — The equilibrium itself is a victim. Both players receive lower payoffs than the cooperative outcome, yet rational individual incentives drive them toward this Pareto-inferior state. The mutual benefit option exists but remains inaccessible because each player's best response to the other's strategy is defection. Trapped in a structurally self-reinforcing mechanism.
constraint_indexing:constraint_classification(prisoner_dilemma_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: INDIVIDUAL DEFECTOR (ROPE) — In the asymmetric payoff reading where defection provides immediate advantage, the defector experiences the constraint as coordination logic: my defection optimally responds to your strategy choice. From this view, the game coordinates on Nash equilibrium through pure strategic interaction. Benefits from defection while mutual cooperation is suppressed. The defector perceives agency within the trap—no exit needed because the current path is optimal for them.
constraint_indexing:constraint_classification(prisoner_dilemma_trap, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 4: ITERATED PLAY COMMUNITY (TANGLED ROPE) — Over repeated interactions, coordination mechanisms emerge (tit-for-tat, reputation systems) that reduce extraction and enable partial cooperation. However, enforcement requires monitoring and sanctioning, creating institutional overhead. Benefits from repeated-game cooperation while bearing the suppression costs of maintaining reputation and reputation mechanisms. The constraint becomes a hybrid: genuine coordination function (enabling cooperation across iterations) paired with asymmetric enforcement costs.
constraint_indexing:constraint_classification(prisoner_dilemma_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 5: CONTRACT/TRUST INSTITUTION (SCAFFOLD) — Formal contracts, legal enforcement, and trust-based institutions (reputation markets, social norms) create temporary scaffolds that convert the snare into negotiable equilibrium. Organized players can exit the trap through institutional innovation: binding agreements, transparent monitoring, graduated sanctions. Sunset logic: as institutions mature, the dilemma's suppressive force diminishes. High agency; low effective extraction because exit pathways exist.
constraint_indexing:constraint_classification(prisoner_dilemma_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — The prisoner's dilemma as taught in game theory textbooks is largely a pedagogical ritual. The canonical 2×2 payoff matrix is a simplified abstraction that does not capture real strategic interaction (information asymmetries, reputation, repeated play, coalition formation, institutional context). The theory persists through educational inertia—taught because it is canonical, not because it is functionally predictive. Theater ratio reflects that the theoretical form dominates discourse despite limited real-world applicability.
constraint_indexing:constraint_classification(prisoner_dilemma_trap, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: LOGICAL STRUCTURE / NATURAL LAW VIEW (MOUNTAIN) — From a universal logical perspective, the prisoner's dilemma is an immutable consequence of decision theory under uncertainty: when payoffs are structured such that defection yields the highest individual payoff regardless of the other player's choice, rational players must defect. This structural feature of the payoff matrix is unchangeable—no agent can alter the fundamental logic. However, the scope of this mountain is constrained to the specific payoff structure; alternative payoff structures (cooperative games, common-pool resource systems) do not instantiate the dilemma.
constraint_indexing:constraint_classification(prisoner_dilemma_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prisoner_dilemma_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(prisoner_dilemma_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prisoner_dilemma_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(prisoner_dilemma_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(prisoner_dilemma_trap, TR),
    TR >= 0.70.

:- end_tests(prisoner_dilemma_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The dilemma does not extract as severely as a pure snare (e.g., monopolistic price-setting, where the extractor takes the majority of surplus). Instead, extraction in the dilemma is symmetric and structural—both players lose relative to cooperation, but the asymmetry between defection payoff and cooperation payoff is modest compared to snares with institutional gatekeeping. The value reflects that the 'extraction' is structural, not concentrated. Suppression (0.65): Moderate-high. Significant suppression of the cooperative outcome through information structure (each player cannot credibly commit to cooperation) and incentive structure (defection is dominant strategy). However, suppression is not absolute (total=1.0) because institutional mechanisms can and do overcome it. Theater ratio (0.35): Low-moderate. The dilemma is functionally descriptive—it accurately predicts outcomes under specific payoff and information conditions. The theater reflects that the pedagogical presentation often abstracts away from realistic factors (reputation, repeated play, communication, institutional context) that would convert the dilemma into richer strategic structures. Over the interval, extractiveness increases slightly (0.42→0.58) as understanding of the dilemma's pervasiveness grows; theater ratio increases marginally as the gap widens between canonical theory and empirical reality.
 *
 * PERSPECTIVAL GAP:
 *   The major perspectival gaps are: (1) Individual trapped player sees snare; defector sees rope/advantage; both perceive same payoff structure but occupy different positions in it. (2) Single-shot game (snare) versus repeated game (tangled rope) — iteration converts the constraint from pure extraction into mixed coordination-extraction. (3) Analytical observer risks piton (seeing the dilemma as a theoretical ritual) or mountain (seeing it as logically immutable) depending on meta-theoretical position. The gap between these views is the gap between the game's formal structure and its applicability to real strategic contexts. (4) Institutional perspective (scaffold) sees escape routes that strategic participants (snare perspective) cannot see from within the game structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows power-exit mapping: Trapped players with no exit options (d ≈ 0.95) experience maximum extraction; defectors with arbitrage options (d ≈ 0.20) experience positive payoff relative to equilibrium; organized actors with mobile exit (d ≈ 0.40) experience moderate extraction; analytical observers (d ≈ 0.72) perceive the structural asymmetry between cooperative possibility and defection-driven equilibrium. The beneficiary ('individual_defectors') is a conceptual position rather than a real agent—it represents the payoff structure that rewards defection. The victims ('cooperative_equilibrium', 'mutual_benefit_option') are abstract collectives that cannot organize or advocate for themselves within the game structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through multi-perspective analysis. The single-shot game IS a snare (no escape, pure extraction). The repeated game IS a tangled rope (institutional mechanisms enable partial cooperation). The theoretical frame IS a piton (pedagogical ritual with limited descriptive power). The logical structure IS a mountain (payoff structure creates immutable defection incentive under complete information). No single classification is 'the answer'—the constraint's true structure is the presheaf over all observational contexts. Resolution: Recognize that 'the prisoner's dilemma' is an ambiguous label covering multiple structurally distinct constraints (single-shot snare, repeated-game tangled rope, pedagogical piton, logical mountain). Each deserves its own analysis. Empirically, the single-shot dilemma is rare; most real strategic interactions are repeated games with institutional context, shifting classification toward tangled rope or scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    payoff_structure_assumption,
    'Are the payoff values intrinsic to the dilemma or contingent on how the problem is framed?',
    'Empirical examination of real strategic interactions (arms races, environmental commons, public goods contributions) to determine whether the canonical prisoner''s dilemma payoff structure naturally emerges or requires specific framing assumptions',
    'If intrinsic: the dilemma is a robust structural feature of many real interactions (snare classification robust). If contingent: the dilemma is an artifact of payoff assumptions, and real interactions exhibit richer strategic structures (classification becomes problem-dependent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(payoff_structure_assumption, conceptual, 'Whether dilemma payoff structure is intrinsic or framing-dependent').

omega_variable(
    information_asymmetry_scope,
    'What fraction of real-world strategic dilemmas involve the complete information assumptions of the canonical game, versus asymmetric information that changes the strategic structure?',
    'Empirical sampling of strategic interactions in economics, politics, ecology, and organizational behavior; categorization by information completeness',
    'If majority complete-information: dilemma trap is broadly applicable (high extractiveness stable). If majority incomplete-information: the canonical dilemma misses the real constraint structure (extractiveness overstated, true constraint is information revelation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_scope, empirical, 'Scope of complete-information assumption in real strategic dilemmas').

omega_variable(
    institutional_escape_mechanisms,
    'What institutional innovations most reliably convert prisoner''s dilemma outcomes into cooperative equilibria? Are there systematic barriers to their adoption?',
    'Cross-domain institutional analysis of successful cooperation mechanisms (legal contracts, monitoring systems, graduated sanctions, repeated-game reputation); identification of contexts where these mechanisms fail or are deliberately prevented',
    'If mechanisms widely available: the snare classification is conditional on lack of institutional context (scaffold perspective correct). If mechanisms systematically blocked: snare is more robust across contexts. If mechanisms imperfectly effective: tangled rope classification dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_escape_mechanisms, empirical, 'Effectiveness and availability of institutional escape mechanisms').

omega_variable(
    evolutionary_stability,
    'In multi-generation strategic interactions, does cooperative strategy re-emerge as an equilibrium, or do defection-based strategies persist indefinitely?',
    'Evolutionary game theory analysis and empirical observation of repeated-game outcomes across many iterations; measurement of strategy distribution at evolutionary equilibrium',
    'If cooperation emerges: the biological/evolutionary timescale escapes the trap (mountain classification fails at civilizational horizon). If defection persists: the trap is robust even under evolutionary pressure (snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evolutionary_stability, empirical, 'Whether evolutionary pressure produces cooperative emergence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prisoner_dilemma_trap, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pdt_tr_t0, prisoner_dilemma_trap, theater_ratio, 0, 0.25).
narrative_ontology:measurement(pdt_tr_t2, prisoner_dilemma_trap, theater_ratio, 2, 0.3).
narrative_ontology:measurement(pdt_tr_t4, prisoner_dilemma_trap, theater_ratio, 4, 0.35).

% Extraction over time
narrative_ontology:measurement(pdt_be_t0, prisoner_dilemma_trap, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(pdt_be_t2, prisoner_dilemma_trap, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(pdt_be_t4, prisoner_dilemma_trap, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prisoner_dilemma_trap, enforcement_mechanism).
narrative_ontology:affects_constraint(prisoner_dilemma_trap, arms_race_escalation).
narrative_ontology:affects_constraint(prisoner_dilemma_trap, tragedy_of_the_commons).
narrative_ontology:affects_constraint(prisoner_dilemma_trap, public_goods_underprovision).
narrative_ontology:affects_constraint(prisoner_dilemma_trap, coordination_failure_asymmetric_info).

% DUAL FORMULATION NOTE:
% The prisoner's dilemma as a structural constraint should be decomposed into: (1) single_shot_dilemma_snare (ε≈0.58, immediate/biographical horizon), (2) iterated_dilemma_tangled_rope (ε≈0.35, biographical/generational with institutional evolution), and (3) canonical_game_theory_piton (ε≈0.15, theoretical discourse). These stories have different extractiveness values and different institutional escape mechanisms. The single-shot form is upstream; the iterated form and theoretical form depend on it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
