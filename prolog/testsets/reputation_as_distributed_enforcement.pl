% ============================================================================
% CONSTRAINT STORY: reputation_as_distributed_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reputation_as_distributed_enforcement, []).

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
 *   constraint_id: reputation_as_distributed_enforcement
 *   human_readable: Reputation as Distributed Enforcement Mechanism
 *   domain: social_systems/institutional_dynamics/power_asymmetry
 *
 * SUMMARY:
 *   Reputation systems in platform economies, labor markets, and social
 *   networks create a structural paradox: the same distributed enforcement
 *   mechanism that enables trust and coordination for mobile actors functions
 *   as an inescapable trap for powerless actors. A gig worker with no
 *   alternative income sources experiences a single negative review as
 *   catastrophic — it cascades through algorithmic ranking, reduces job
 *   offers, and creates a downward spiral with no appeal mechanism. A mobile
 *   professional with multiple income streams and geographic flexibility
 *   experiences the same review as manageable noise — they can exit to
 *   alternative platforms, leverage positive reputation from other venues, or
 *   simply absorb the reputational cost. The institutional rules are
 *   identical; the experienced constraint is radically different. This
 *   constraint demonstrates how power asymmetries transform coordination
 *   mechanisms into extraction mechanisms without any change in the formal
 *   structure. The theater_ratio (0.48) reflects moderate performativity:
 *   reputation systems do provide genuine information about past behavior,
 *   but much of the verification ritual (review solicitation, rating
 *   displays, algorithmic curation) serves platform engagement and rent
 *   extraction rather than pure coordination. The extractiveness has
 *   increased over the interval (0.42 → 0.58) as platforms have layered
 *   additional rent-seeking mechanisms (premium visibility, review
 *   manipulation services, algorithmic opacity) onto the base coordination
 *   function.
 *
 * KEY AGENTS:
 *   - Trapped Worker: Primary victim (powerless/trapped) — single negative review creates cascading exclusion; no exit options; experiences pure extraction
 *   - Constrained Freelancer: Secondary victim (moderate/constrained) — benefits from positive reputation but bears extraction from lock-in and platform rent-seeking; mixed experience
 *   - Mobile Professional: Primary beneficiary (powerful/arbitrage) — can arbitrage across platforms and markets; experiences coordination with minimal extraction
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — designs system, captures network effects, extracts rents through algorithmic curation
 *   - Worker Cooperative: Organized actor (organized/mobile) — collective bargaining power and alternative verification pathways reduce extraction but cannot fully exit platform-mediated markets
 *   - Decentralized Identity Coalition: Organized actor (organized/constrained) — building portable reputation infrastructure with sunset logic; sees current extraction as temporary
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees irreducible structural ambiguity between coordination function and extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reputation_as_distributed_enforcement, 0.58).
domain_priors:suppression_score(reputation_as_distributed_enforcement, 0.62).
domain_priors:theater_ratio(reputation_as_distributed_enforcement, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reputation_as_distributed_enforcement, extractiveness, 0.58).
narrative_ontology:constraint_metric(reputation_as_distributed_enforcement, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reputation_as_distributed_enforcement, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reputation_as_distributed_enforcement, tangled_rope).
narrative_ontology:human_readable(reputation_as_distributed_enforcement, "Reputation as Distributed Enforcement Mechanism").
narrative_ontology:topic_domain(reputation_as_distributed_enforcement, "social_systems/institutional_dynamics/power_asymmetry").

domain_priors:requires_active_enforcement(reputation_as_distributed_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reputation_as_distributed_enforcement, mobile_actors_with_exit_options).
narrative_ontology:constraint_beneficiary(reputation_as_distributed_enforcement, platform_operators).
narrative_ontology:constraint_beneficiary(reputation_as_distributed_enforcement, high_reputation_incumbents).
narrative_ontology:constraint_victim(reputation_as_distributed_enforcement, trapped_actors_without_negotiation_leverage).
narrative_ontology:constraint_victim(reputation_as_distributed_enforcement, new_entrants_without_history).
narrative_ontology:constraint_victim(reputation_as_distributed_enforcement, marginalized_groups_with_structural_disadvantage).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED WORKER (SNARE) — Cannot exit local labor market or geographic region. Single negative review or reputation mark creates cascading exclusion from employment, housing, credit. No alternative verification pathway. Reputation system functions as pure extraction mechanism with no coordination benefit — the worker needs the opportunities more than the opportunities need any particular worker. Maximum experienced extraction from identical institutional rules that appear neutral.
constraint_indexing:constraint_classification(reputation_as_distributed_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CONSTRAINED FREELANCER (TANGLED ROPE) — Has some geographic and market mobility but faces significant switching costs. Reputation system provides genuine coordination value (clients can assess quality, reducing search costs) but also extracts through lock-in effects, rating manipulation, and platform dependency. Can exit at cost. Mixed experience: benefits from positive reputation accumulation while bearing extraction from negative review persistence and platform rent-seeking.
constraint_indexing:constraint_classification(reputation_as_distributed_enforcement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MOBILE PROFESSIONAL (ROPE) — High mobility across markets, platforms, and geographic regions. Can arbitrage between reputation systems, exit platforms that extract excessively, build reputation across multiple venues. Experiences reputation system primarily as coordination mechanism: signals quality to potential partners, reduces transaction costs, enables premium pricing. Negative reviews are manageable noise. Net beneficiary of the system.
constraint_indexing:constraint_classification(reputation_as_distributed_enforcement, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Designs and controls the reputation system. Captures value through network effects, data aggregation, and algorithmic curation. Experiences the constraint as pure coordination: the reputation system solves the trust problem that enables the platform's existence. Can modify rules, adjust algorithms, and extract rents through design choices. Maximum beneficiary position with full exit options to alternative business models.
constraint_indexing:constraint_classification(reputation_as_distributed_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: WORKER COOPERATIVE (TANGLED ROPE) — Organized labor with collective bargaining power and alternative reputation verification pathways (union membership, apprenticeship systems, peer networks). Experiences reputation systems as mixed: benefits from collective reputation building and mutual aid, but also faces platform power asymmetries and algorithmic bias. Can partially exit through alternative coordination mechanisms but cannot fully escape platform-mediated markets. Moderate extraction with genuine coordination function.
constraint_indexing:constraint_classification(reputation_as_distributed_enforcement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: DECENTRALIZED IDENTITY COALITION (SCAFFOLD) — Organized actors building portable reputation systems (blockchain credentials, federated identity, open reputation protocols) that reduce platform lock-in. Sees current centralized reputation systems as temporary coordination mechanism with sunset logic: as portable reputation infrastructure matures, platform-specific reputation loses extractive power. Extraction is tolerated because it is declining over the time horizon as interoperability standards emerge.
constraint_indexing:constraint_classification(reputation_as_distributed_enforcement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, reputation systems solve genuine coordination problems (adverse selection, moral hazard, trust in anonymous markets) while simultaneously creating extractive lock-in and power asymmetries. The same mechanism that enables distributed trust enforcement also enables distributed punishment that falls disproportionately on those without exit options. Structural ambiguity is irreducible: the coordination function and extraction mechanism are inseparable at the architectural level.
constraint_indexing:constraint_classification(reputation_as_distributed_enforcement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reputation_as_distributed_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reputation_as_distributed_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reputation_as_distributed_enforcement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reputation_as_distributed_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reputation_as_distributed_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The base coordination function (signaling quality, reducing search costs) has genuine value, but platforms layer significant rent extraction on top: algorithmic opacity, lock-in effects, premium visibility fees, review manipulation markets, and asymmetric negative review persistence. The extraction has increased over the interval as platforms have matured from pure coordination to rent-seeking. Suppression (0.62): Moderate-high. Powerless actors face severe barriers to exit: geographic immobility, lack of alternative income sources, absence of appeal mechanisms, algorithmic amplification of negative signals, and network effects that concentrate opportunity on high-reputation platforms. Moderate-power actors face lower but still significant suppression through switching costs and reputation non-portability. Theater ratio (0.48): Moderate. Reputation systems do provide genuine information about past behavior (lower theater than pure performative compliance), but significant performative elements exist: review solicitation rituals, rating display psychology, algorithmic curation that prioritizes engagement over accuracy, and platform-sponsored verification badges that signal compliance rather than quality.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the canonical power-dependent perspectival gap. Powerless actors see Snare — pure extraction with no coordination benefit because they need the opportunities more than the opportunities need them. Moderate-power actors see Tangled Rope — genuine coordination value mixed with significant extraction through lock-in and rent-seeking. Powerful actors see Rope — primarily coordination with minimal extraction because they can exit and arbitrage. Platform operators see Rope — pure coordination from their perspective because they capture the value. Organized actors see either Tangled Rope (worker cooperatives with partial exit) or Scaffold (decentralized identity coalitions building sunset infrastructure). The analytical observer sees Tangled Rope at the civilizational level — the coordination function and extraction mechanism are architecturally inseparable. The gap reveals that 'reputation as coordination' vs 'reputation as extraction' is not a property of the system but a property of the observer's structural position within it.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint demonstrates power-dependent directionality from identical institutional rules. Trapped actors are victims with no exit options — the derivation chain assigns high d (≈ 0.95), producing high f(d) and high experienced extraction (χ > 0.66, Snare classification). Constrained actors are mixed victims/beneficiaries with exit costs — moderate d (≈ 0.65), moderate f(d), moderate χ (0.46 < χ < 0.66, Tangled Rope classification). Mobile actors are beneficiaries with arbitrage options — low d (≈ 0.15), low f(d), low or negative χ (Rope classification). Platform operators are pure beneficiaries with full control — very low d (≈ 0.05), negative f(d), negative χ (Rope classification). The perspectival gap is structural: the same reputation system that coordinates trust for those with exit options extracts from those without exit options. No directionality overrides are needed — the beneficiary/victim declarations plus exit options produce the correct d values through the standard derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint is legitimately Tangled Rope from the analytical perspective because it exhibits both genuine coordination function (solving adverse selection and moral hazard in anonymous markets) AND asymmetric extraction (lock-in effects, algorithmic rent-seeking, disproportionate punishment of powerless actors). The coordination function is real — without reputation systems, anonymous market exchange would face severe trust problems. The extraction is also real — platforms layer rent-seeking mechanisms on top of the base coordination, and power asymmetries transform the same institutional rules into radically different experienced constraints. The mandatrophy is resolved by recognizing that the coordination and extraction are inseparable at the architectural level: the distributed enforcement mechanism that enables trust also enables distributed punishment that falls disproportionately on those without exit options. This is not mislabeling coordination as extraction or vice versa — it is recognizing that the same mechanism performs both functions simultaneously, with the balance depending on the observer's power position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    portability_threshold,
    'At what level of reputation portability across platforms does the lock-in extraction mechanism break down?',
    'Empirical measurement of switching costs and reputation transfer rates across platforms with varying interoperability standards; longitudinal tracking of worker mobility as portable identity systems mature',
    'If portability threshold is low (< 30% reputation transfer): current extraction persists even with interoperability standards. If high (> 70% transfer): scaffold sunset is real and extraction declines as portability increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(portability_threshold, empirical, 'Reputation portability threshold for breaking lock-in extraction').

omega_variable(
    algorithmic_bias_magnitude,
    'How much of the differential extraction across power positions is due to algorithmic bias versus structural power asymmetries?',
    'Controlled experiments comparing reputation outcomes for identical behavior across demographic groups; audit studies of platform algorithms; decomposition of variance in reputation scores by structural vs. behavioral factors',
    'If bias is primary driver: technical fixes (algorithmic fairness) can reduce extraction. If structural asymmetries dominate: extraction persists regardless of algorithmic neutrality because powerless actors face different choice sets.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_bias_magnitude, empirical, 'Relative contribution of algorithmic bias vs. structural power asymmetries').

omega_variable(
    coordination_floor_estimate,
    'What is the minimum extractiveness inherent to any reputation-based coordination mechanism, below which the system cannot function?',
    'Theoretical analysis of information asymmetry and adverse selection in trust games; empirical comparison of reputation system overhead across different institutional designs (centralized platforms, federated networks, peer-to-peer systems)',
    'If floor is high (> 0.40): much of observed extraction is necessary coordination cost, shifting classification toward Rope from more perspectives. If floor is low (< 0.20): most extraction is rent-seeking, confirming Snare/Tangled Rope classifications.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_floor_estimate, conceptual, 'Minimum extractiveness inherent to reputation coordination').

omega_variable(
    negative_review_persistence,
    'Does the asymmetric persistence of negative vs. positive reputation information serve a coordination function or primarily an extraction function?',
    'Comparison of prediction accuracy for future behavior using symmetric vs. asymmetric weighting of reputation history; analysis of false positive/negative rates in reputation-based exclusion decisions',
    'If asymmetric persistence improves prediction: it serves coordination (protecting against bad actors). If symmetric weighting performs equally well: asymmetric persistence is extractive (creates excessive punishment and lock-in).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negative_review_persistence, empirical, 'Whether negative review persistence serves coordination or extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reputation_as_distributed_enforcement, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rep_enf_tr_t0, reputation_as_distributed_enforcement, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rep_enf_tr_t3, reputation_as_distributed_enforcement, theater_ratio, 3, 0.4).
narrative_ontology:measurement(rep_enf_tr_t6, reputation_as_distributed_enforcement, theater_ratio, 6, 0.44).
narrative_ontology:measurement(rep_enf_tr_t9, reputation_as_distributed_enforcement, theater_ratio, 9, 0.48).

% Extraction over time
narrative_ontology:measurement(rep_enf_be_t0, reputation_as_distributed_enforcement, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(rep_enf_be_t3, reputation_as_distributed_enforcement, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(rep_enf_be_t6, reputation_as_distributed_enforcement, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(rep_enf_be_t9, reputation_as_distributed_enforcement, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reputation_as_distributed_enforcement, identity_coordination).
narrative_ontology:affects_constraint(reputation_as_distributed_enforcement, platform_algorithmic_curation).
narrative_ontology:affects_constraint(reputation_as_distributed_enforcement, gig_economy_labor_precarity).
narrative_ontology:affects_constraint(reputation_as_distributed_enforcement, social_credit_systems).

% DUAL FORMULATION NOTE:
% Reputation systems are a constraint family with multiple structurally distinct stories: (1) reputation as distributed enforcement (this story, ε=0.58), (2) algorithmic curation and ranking (downstream, higher ε due to opacity and manipulation), (3) social credit as state surveillance (downstream, much higher ε due to coercive enforcement). Each has different extractiveness reflecting different institutional implementations of the same base coordination mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
