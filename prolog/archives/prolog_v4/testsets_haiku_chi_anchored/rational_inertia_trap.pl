% ============================================================================
% CONSTRAINT STORY: rational_inertia_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rational_inertia_trap, []).

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
 *   constraint_id: rational_inertia_trap
 *   human_readable: The Legacy Protocol Lock-in
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The Legacy Protocol Lock-in is a structural constraint where a
 *   technologically superior alternative protocol exists, but individual and
 *   organizational actors remain in an inferior legacy system because the
 *   cost of switching locally exceeds the local immediate benefit, even
 *   though the system-wide gain from migration would be massive. This is not
 *   a technical problem — engineers can always build better protocols. It is
 *   a coordination problem disguised as a technical problem: the payoff
 *   structure of the decision (stay or switch) creates a stable equilibrium
 *   that persists indefinitely unless disrupted by critical mass, regulatory
 *   mandate, or external catastrophe. The constraint exhibits a clear
 *   perspectival presheaf: the trapped individual user sees a snare (no
 *   escape), the institutional steward sees rope (their status depends on
 *   lock-in), the organized migration coalition sees tangled rope (they
 *   provide coordination but also extract governance rents), the defector
 *   sees scaffold (critical mass will flip the game), and the game theorist
 *   risks seeing a mountain (an invariant property of coordination itself).
 *   The extractiveness score (0.52) reflects that the lock-in extracts from
 *   adopters via delayed access to superior protocols, but the extraction is
 *   enabled by voluntary network participation — suppression is high (0.65)
 *   because adopters face switching costs and coordination friction, not
 *   because force is applied. Theater ratio (0.58) reflects that governance
 *   structures and standards bodies devoted to the legacy protocol
 *   increasingly perform ceremonial functions as technical justifications
 *   weaken.
 *
 * KEY AGENTS:
 *   - Individual Users/Small Organizations (powerless/trapped): Primary victims — absorb full switching cost individually while benefiting individually only at critical mass
 *   - Incumbent Protocol Stewards (institutional/arbitrage): Primary beneficiaries — extract status and governance rents from lock-in; control standard-setting
 *   - Mid-Tier Institutions (moderate/constrained): Secondary victims — bear organizational technical debt and coordination burden; cannot defect alone
 *   - Migration Coalition (organized/constrained): Secondary beneficiary/victim — provide genuine coordination infrastructure but extract governance authority in new protocol
 *   - Large Defectors (powerful/mobile): Tertiary actor — can absorb switching costs; defection by critical mass of defectors triggers phase transition
 *   - Legacy Governance Apparatus (institutional/arbitrage): Performs ceremony — standards bodies, regulatory bodies, educational institutions maintaining legitimacy of legacy system
 *   - Analytical Observer (analytical/analytical): Risk naturalizing contingent structural arrangement as invariant game-theoretic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rational_inertia_trap, 0.52).
domain_priors:suppression_score(rational_inertia_trap, 0.65).
domain_priors:theater_ratio(rational_inertia_trap, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rational_inertia_trap, extractiveness, 0.52).
narrative_ontology:constraint_metric(rational_inertia_trap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(rational_inertia_trap, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rational_inertia_trap, tangled_rope).
narrative_ontology:human_readable(rational_inertia_trap, "The Legacy Protocol Lock-in").
narrative_ontology:topic_domain(rational_inertia_trap, "technological/economic").

domain_priors:requires_active_enforcement(rational_inertia_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rational_inertia_trap, incumbent_protocol_stewards).
narrative_ontology:constraint_beneficiary(rational_inertia_trap, switching_cost_arbitrageurs).
narrative_ontology:constraint_victim(rational_inertia_trap, adopter_pool).
narrative_ontology:constraint_victim(rational_inertia_trap, system_efficiency_gains).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED ADOPTER (SNARE) — Individual users face switching costs (retraining, data migration, infrastructure reinvestment, coordination friction) that exceed their personal benefit horizon. Trapped by network effects: leaving the legacy system means losing access to the majority who remain. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.77.
constraint_indexing:constraint_classification(rational_inertia_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER INSTITUTIONAL ACTOR (TANGLED ROPE) — Organizations benefit from legacy protocol familiarity (training ecosystem, tool maturity, talent pool fluent in legacy system) while bearing the cost of perpetual technical debt and compatibility workarounds. Switching is possible but requires massive coordination with peers. d≈0.68, f(d)≈1.05, σ=1.2 → χ≈0.57.
constraint_indexing:constraint_classification(rational_inertia_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PROTOCOL STEWARD INSTITUTION (ROPE) — Legacy protocol maintainers and standard-setters benefit from network inertia: their status as custodians, control over version updates, and ability to extract licensing/certification rents are all secured by lock-in. They experience the constraint as pure coordination: keeping the legacy alive coordinates expectations. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(rational_inertia_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MIGRATION COALITION (TANGLED ROPE) — Organized agents (open-source projects, standards bodies, large cloud providers) push for protocol migration and create migration tooling. They provide genuine coordination infrastructure (bridges, translators, phased adoption plans) but also extract value by positioning themselves as migration authorities or capturing new protocol governance. d≈0.45, f(d)≈0.42, σ=1.2 → χ≈0.26.
constraint_indexing:constraint_classification(rational_inertia_trap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY GOVERNANCE APPARATUS (PITON) — Standards organizations, regulatory bodies, and educational institutions built around the legacy protocol are substantially performative. Certification programs, compliance audits, and regulatory approval processes exist to maintain the apparatus's status, not to deliver system function. theater_ratio=0.58 reflects that governance rituals persist despite diminishing functional value. The apparatus sees itself as degraded — necessary for transition management but no longer a genuine steering mechanism. d≈0.15, f(d)≈0.05, σ=1.2 → χ≈0.04.
constraint_indexing:constraint_classification(rational_inertia_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DEFECTOR COALITION (SCAFFOLD) — Large, capable institutions (tech leaders, wealthy jurisdictions) can absorb switching costs and migrate to superior protocols. They experience the constraint as temporary coordination failure with a clear sunset: as critical mass of defectors builds, the coordination problem flips (staying in legacy becomes irrational). d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.20. Low effective extraction because defectors have agency and see a critical-mass threshold.
constraint_indexing:constraint_classification(rational_inertia_trap, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a game-theoretic view, the lock-in appears to be an invariant property of coordination problems: the payoff structure (high individual switching cost, high benefit of coordination on ANY protocol) creates a stable equilibrium regardless of absolute efficiency. No escape without external shock or coordinated defection. However, structural data (ε=0.52, suppression=0.65, theater=0.58) contradicts mountain classification — the constraint is contingent on switching-cost and network-effect mechanisms, not a law of nature.
constraint_indexing:constraint_classification(rational_inertia_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rational_inertia_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rational_inertia_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rational_inertia_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rational_inertia_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rational_inertia_trap, TR),
    TR >= 0.70.

:- end_tests(rational_inertia_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increasing over interval. At t=0 (early lock-in), ε=0.28 reflects that the legacy protocol was genuinely optimal, and network effects are nascent — users benefit from coordination on any protocol, legacy or new. As the interval progresses and superior alternatives emerge, ε increases to 0.52 as adopters bear increasing technical debt. The extraction is not malicious — it emerges from the rational incentive structure. Each individual user, facing switching cost S and personal benefit of switching B per year, rationally stays locked in if S > B × (lifetime remaining). But S is sticky (includes retraining, data migration, infrastructure reinvestment, loss of accumulated tool expertise) while B grows as the technology gap widens. The paradox: the system-wide benefit (aggregate of all B's, minus switching costs paid only once) far exceeds the system-wide cost, yet no individual has incentive to initiate. Suppression (0.65): High and increasing. Network effects create powerful suppression: leaving the legacy protocol means losing access to 95%+ of potential collaborators and data sources. This is not coercive force but structural constraint harder to escape than force — it is voluntary participation in a system that punishes exit. Theater ratio (0.58): Increasing from 0.35 to 0.58. As the technical gap widens, governance structures (standards bodies, certification programs, regulatory compliance schemes) increasingly perform ceremonial functions. Early certification justified technical competence; later certification justifies legitimacy of a degraded system. Educational institutions continue teaching legacy protocols partly due to inertia in curriculum and partly because trained personnel become a constituency defending legacy investment.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is the full range from snare to rope. The trapped individual user sees no escape (snare): they are locked in by network effects and switching costs, with no alternative path. The incumbent steward sees pure coordination (rope): maintaining the legacy protocol is their institutional function, and lock-in is simply the fact that coordination has already occurred. The analytical game theorist risks seeing a mountain (natural law of coordination): the payoff structure that creates lock-in is universal to any coordination problem, so lock-in appears inevitable. But the empirical structural data contradicts this: extractiveness is not at mountain levels (ε=0.52, not ≤0.25), and suppression is contingent on network size and switching-cost mechanisms, not a law of physics. The defectors see scaffold: they have the resources to absorb switching costs and can trigger a phase transition where lock-in reverses (staying in legacy becomes irrational). The migration coalition sees tangled rope: they provide genuine migration infrastructure and coordination, but also extract governance authority in the new protocol. This perspectival spread is diagnostic: if all perspectives yielded snare, the constraint would be inescapable; if all yielded rope, coordination would be seamless. The spread indicates a real structural instability — lock-in is neither absolute (mountain) nor voluntary (rope) but contingent on critical mass.
 *
 * DIRECTIONALITY LOGIC:
 *   Trapped adopters: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction: adopters have no exit and bear full switching cost. Mid-tier institutions: Victim + constrained → d≈0.68, f(d)≈1.05. High extraction but not maximal because institutions can theoretically coordinate to migrate (constrained but not trapped). Protocol stewards: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiaries with maximal exit freedom; they benefit from lock-in and have easiest path to maintain it. Migration coalition: Mixed → d≈0.45, f(d)≈0.42. Moderate extraction because they provide genuine coordination but also position themselves as authorities in the new protocol. Large defectors: Beneficiary (of protocol superiority after migration) + mobile → d≈0.35, f(d)≈0.32. Low effective extraction because they have both mobility and resources; their defection is an exit, not a cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by revealing that lock-in is neither pure coordination (rope) nor pure extraction (snare), but a tangled hybrid where the functional part (coordination around a common protocol) is genuine, and the extractive part (keeping the protocol inferior to available alternatives) is real. The beneficiary-steward (institutional/arbitrage, rope perspective) benefits from coordination on ANY protocol — but their specific interest is coordination on the legacy protocol, which delays migration to superior alternatives. The victim-adopter (powerless/trapped, snare perspective) bears the cost of this delay. The constraint is not a natural-law lock-in (mountain) but a contingent institutional arrangement where switching-cost and network-effect mechanisms create stable equilibrium. Mandatrophy is resolved by recognizing that defection at critical mass destabilizes the equilibrium: once sufficient actors have switched, the network-effect suppression flips direction (majority effect), and staying in legacy becomes irrational. The empirical question (omega_critical_mass_dynamics) determines whether the critical mass is reachable without external mandate. If yes, the constraint is self-resolving scaffold. If no, external mandate is necessary to break the equilibrium. The tangled rope classification stands in both cases — the coordination function (people rationally stay on the same protocol) is real, and the extraction (stewards benefit from delay) is real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_threshold,
    'What is the precise threshold of switching cost relative to user lifetime value at which individual rationality flips from ''stay'' to ''switch''?',
    'Empirical study of actual migration adoption curves; correlation between switching cost estimates and adoption timelines across multiple protocol transitions (IPv4→IPv6, HTTP/1.1→HTTP/2, SMTP→successor standards)',
    'If threshold is lower than current estimates: lock-in is weaker than believed, and defection will accelerate faster. If threshold is higher: lock-in is stronger, and migration requires external mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_threshold, empirical, 'Precise switching cost threshold for individual migration rationality').

omega_variable(
    critical_mass_dynamics,
    'What network size constitutes critical mass such that joining the new protocol becomes rational even for actors with high switching costs?',
    'Historical analysis of actual protocol transitions; S-curve fitting to adoption curves; identification of inflection points where growth accelerates',
    'If critical mass is lower: migration is latent and can tip suddenly. If higher: migration remains slow indefinitely without intervention. Direct implication for scaffold sunset estimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(critical_mass_dynamics, empirical, 'Critical mass threshold for protocol migration acceleration').

omega_variable(
    external_mandate_effectiveness,
    'Can regulatory or institutional mandate force protocol migration faster than organic incentive structures, or does mandate create new lock-ins (regulatory avoidance workarounds)?',
    'Comparative analysis of mandated vs voluntary migrations; tracking of post-mandate workarounds and compliance theater; cost data on enforcement overhead',
    'If mandate is effective: policy intervention can break lock-in. If mandate creates new lock-ins: the constraint transmutes but persists. Direct consequence for policy intervention strategies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_mandate_effectiveness, empirical, 'Whether external mandate can break lock-in or merely displaces it').

omega_variable(
    bridging_technology_viability,
    'Can intermediate bridging technologies (protocol translators, dual-stack implementations) reduce switching costs enough to unlock organic migration without full flag-day cutover?',
    'Technical feasibility analysis of bridging; cost modeling of long-term dual-stack maintenance vs single migration cost; empirical observation of migration velocity with vs without bridges in place',
    'If bridges are viable: migration becomes incremental (scaffold with extended sunset). If maintenance burden of bridges exceeds single migration: bridges are a false exit, prolonging the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bridging_technology_viability, empirical, 'Whether bridging technologies enable incremental migration').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rational_inertia_trap, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rit_tr_t0, rational_inertia_trap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rit_tr_t10, rational_inertia_trap, theater_ratio, 10, 0.48).
narrative_ontology:measurement(rit_tr_t20, rational_inertia_trap, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(rit_be_t0, rational_inertia_trap, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(rit_be_t10, rational_inertia_trap, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(rit_be_t20, rational_inertia_trap, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rational_inertia_trap, information_standard).
narrative_ontology:affects_constraint(rational_inertia_trap, network_effect_threshold).
narrative_ontology:affects_constraint(rational_inertia_trap, switching_cost_aggregation).
narrative_ontology:affects_constraint(rational_inertia_trap, protocol_defection_cascade).

% DUAL FORMULATION NOTE:
% The Legacy Protocol Lock-in constrains adoption of any superior alternative. It is distinct from and downstream of the specific technical merits of candidate protocols (network_effect_threshold) and upstream of the coordination dynamics that trigger defection cascades (protocol_defection_cascade). The constraint family models the full lifecycle: technical superiority alone does not guarantee migration; migration requires critical mass; critical mass is contingent on switching cost and network effects. Each story in the family has a different ε reflecting different structural aspects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
