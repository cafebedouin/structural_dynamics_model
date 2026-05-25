% ============================================================================
% CONSTRAINT STORY: network_effect_critical_mass
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_network_effect_critical_mass, []).

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
 *   constraint_id: network_effect_critical_mass
 *   human_readable: Network Effect Critical Mass Threshold
 *   domain: platform_economics/coordination_systems
 *
 * SUMMARY:
 *   The critical mass constraint in network-effect platforms creates a
 *   structural barrier to competition and user exit that appears
 *   simultaneously as a coordination mechanism (solving the problem of user
 *   heterogeneity in value), an extraction mechanism (capturing asymmetric
 *   rewards for first movers), and a regulatory artifact (policy choices that
 *   enforce or dissolve lock-in). The constraint is not a law of nature but a
 *   product of specific design choices, policy frameworks, and coordination
 *   failures. As network effects strengthen during the lifecycle
 *   (extractiveness rising from 0.15 at launch to 0.52 at maturity), the
 *   theatrical component remains low (0.35), indicating that the extraction
 *   mechanism is structurally real rather than performative. The critical
 *   mass threshold represents the inflection point where network effects
 *   shift from exogenous (growth driven by marketing/funding) to endogenous
 *   (growth driven by the network itself). Late entrants face a prisoner's
 *   dilemma: their platform has superior technology but no users; users won't
 *   join because there are no users. The incumbent's advantage is not
 *   technological but topological — they crossed the threshold first.
 *   Interoperability standards (ActivityPub, portable identity, open
 *   protocols) offer a structural escape route by reducing switching costs to
 *   near zero, potentially dissolving the critical mass barrier entirely.
 *   Regulatory frameworks (DMA interoperability requirements, GDPR data
 *   portability) are attempting to force this solution, but actual uptake
 *   suggests network effects persist even with technical interoperability —
 *   users cluster on the largest instance/platform regardless of
 *   compatibility.
 *
 * KEY AGENTS:
 *   - Locked-In User Base: Primary victim (powerless/trapped) — users embed social/economic capital in incumbent network; switching costs exceed benefit threshold; no exit path
 *   - Late Entrant Competitors: Primary victim (moderate/constrained) — face structural barrier to market entry; cannot achieve critical mass without users; cannot attract users without critical mass
 *   - First-Mover Platform: Primary beneficiary (institutional/arbitrage) — captures topological advantage; controls user interface and data; benefits from path dependence and lock-in
 *   - Sophisticated Multiplatform Users: Secondary actor (powerful/mobile) — maintain presence across platforms; arbitrage between them; benefit from network effects but experience fragmentation costs
 *   - Interoperability Coalition: Organized actor (organized/constrained) — regulators, open-source projects, competing platforms; building bridges across critical mass barriers; attempting sunset logic
 *   - Incumbent Regulatory Frame: Institutional actor (institutional/arbitrage) — maintains incumbent advantage through regulatory theater; performs compliance while preserving concentration
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy choices as inevitable network economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(network_effect_critical_mass, 0.52).
domain_priors:suppression_score(network_effect_critical_mass, 0.58).
domain_priors:theater_ratio(network_effect_critical_mass, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(network_effect_critical_mass, extractiveness, 0.52).
narrative_ontology:constraint_metric(network_effect_critical_mass, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(network_effect_critical_mass, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(network_effect_critical_mass, tangled_rope).
narrative_ontology:human_readable(network_effect_critical_mass, "Network Effect Critical Mass Threshold").
narrative_ontology:topic_domain(network_effect_critical_mass, "platform_economics/coordination_systems").

domain_priors:requires_active_enforcement(network_effect_critical_mass).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(network_effect_critical_mass, first_mover_platform).
narrative_ontology:constraint_beneficiary(network_effect_critical_mass, incumbent_network_owner).
narrative_ontology:constraint_victim(network_effect_critical_mass, late_entrant_competitors).
narrative_ontology:constraint_victim(network_effect_critical_mass, user_switching_friction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN USER BASE (SNARE) — Users face switching costs that exceed the benefit threshold for moving to superior alternative platforms. Network effects trap users in suboptimal equilibria. No exit path exists once critical mass is breached — the user's social/economic connections are embedded in the incumbent network. Pure extraction: the incumbent captures surplus through lock-in, with minimal coordination benefit once the network is established.
constraint_indexing:constraint_classification(network_effect_critical_mass, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LATE ENTRANT COMPETITOR (SNARE) — Cannot compete on network effects once incumbents cross critical mass. New entrants face a structural barrier: users won't join a new platform until it reaches critical mass, but critical mass won't arrive until users join. The constraint enforces monopolistic or duopolistic equilibria by making entry prohibitively expensive. Even superior technology cannot overcome network lock-in. The late entrant bears the full cost of this structural asymmetry.
constraint_indexing:constraint_classification(network_effect_critical_mass, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: FIRST-MOVER PLATFORM (ROPE) — Experiences the critical mass threshold as a coordination mechanism: reaching the threshold solves the adoption problem and creates a self-reinforcing cycle. The network effect is a genuine coordination benefit — users want to join the largest network, platforms want to serve the largest user base. The first-mover captures asymmetric rewards but these derive from solving a real coordination problem, not from pure extraction.
constraint_indexing:constraint_classification(network_effect_critical_mass, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SOPHISTICATED MULTIPLATFORM USER (TANGLED ROPE) — Users with sufficient resources/expertise can maintain presence on multiple platforms or arbitrage between them. They benefit from network effects (liquidity, features improve with scale) but also bear costs (fragmented communication, coordination overhead across platforms). Some extraction occurs through data harvesting and attention capture, but substantial coordination value exists. These agents have partial exit capacity through multiplatform strategies.
constraint_indexing:constraint_classification(network_effect_critical_mass, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INTEROPERABILITY COALITION (SCAFFOLD) — Regulatory bodies, open-source movements, and competing platforms organize to build interoperability standards (ActivityPub, data portability requirements, federated protocols). These create bridges across critical mass boundaries, enabling users to maintain identity/data across platforms without full switching. Interoperability standards have a sunset logic: if adopted, they dissolve the critical mass constraint by removing switching costs. Theater ratio is low — the constraint's mechanism is structural, not performative.
constraint_indexing:constraint_classification(network_effect_critical_mass, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: REGULATORY INCUMBENT FRAME (PITON) — Incumbent platforms have become the default regulatory target and institutional interface for oversight. Regulatory frameworks (GDPR, DMA, antitrust review) treat network effect concentration as inevitable rather than as a contingent outcome of policy choices. The regulatory discourse maintains the constraint through performative compliance reviews and half-measures, theater that preserves network concentration while appearing to address it. The constraint persists through institutional inertia rather than genuine structural necessity.
constraint_indexing:constraint_classification(network_effect_critical_mass, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, network effects are an inherent feature of systems with positive feedback — larger networks generate more value, attracting more users. This appears as a law of economics: scale advantages in networks are natural and inevitable. However, the structural data reveals this as a false summit: the critical mass threshold is contingent on coordination failures, absent interoperability, and policy choices that enforce compatibility lock-in. The mountain classification naturalizes policy contingencies.
constraint_indexing:constraint_classification(network_effect_critical_mass, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(network_effect_critical_mass_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(network_effect_critical_mass, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(network_effect_critical_mass, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(network_effect_critical_mass, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(network_effect_critical_mass, TR),
    TR >= 0.70.

:- end_tests(network_effect_critical_mass_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The critical mass constraint extracts value from late entrants (who cannot compete) and locked-in users (who face switching costs). However, the extraction is not as severe as pure predatory extraction because the first-mover genuinely solved a coordination problem — reaching critical mass was difficult and risky. The 0.52 value reflects that much of the first-mover's reward is fair return on innovation risk, while some derives from enforcement of lock-in. The measurement trajectory (0.15 → 0.52) shows extractiveness increasing as network effects strengthen. Suppression (0.58): Moderate-high. Switching costs for users include: social graph lock-in (all contacts are on the incumbent), data lock-in (years of messages/connections embedded), interoperability absent (cannot access the incumbent network from alternative platforms), and artificial friction (difficult data export, API restrictions). However, suppression is not absolute — multiplatform users demonstrate partial escape; interoperability standards lower switching costs; users can maintain multiple accounts. Theater ratio (0.35): Low. The extraction mechanism is structurally real, not performative. Users genuinely cannot switch without losing value; competitors genuinely cannot enter without reaching critical mass first. This is not theatrical constraint but topological constraint. The low theater ratio distinguishes this from piton-type degradation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon produces radically different classifications depending on the observer's position. The locked-in user sees a snare: they are trapped in a suboptimal equilibrium with no escape. The late entrant competitor sees a snare: the market is closed to them by topology, not by law or technology. The first-mover sees rope: the network effect is solving the real coordination problem of user heterogeneity. The multiplatform user sees tangled rope: they benefit from scale (features improve with users) but bear costs (fragmentation, attention capture). The interoperability coalition sees a scaffold: technical and regulatory solutions can dissolve the critical mass barrier. The incumbent regulatory frame sees a piton: the constraint persists through institutional inertia, not genuine structural necessity. The analytical observer risks seeing a mountain: critical mass effects are inherent to networks, inevitable features of positive feedback systems. The perspectival gap reveals that the constraint's type is not fixed but perspectival — it is a snare only from the locked-in user's view, a rope only from the first-mover's view. The engine's classification of tangled_rope is the meta-perspectival reading: the constraint contains both genuine coordination and asymmetric extraction, simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by agent structural position. Locked-in users (powerless/trapped) experience high d ≈ 0.95 (full targets of extraction), yielding high f(d) ≈ 1.42 and high χ. Late entrants (moderate/constrained) experience d ≈ 0.70 (victims with some options), yielding f(d) ≈ 1.02 and moderate-high χ. First-movers (institutional/arbitrage) experience d ≈ 0.10 (beneficiaries with exit options), yielding f(d) ≈ -0.05 and negative χ (net recipients of extraction flow). Scope modifier σ(global) = 1.2 amplifies χ across the board — network effects are planetary-scale coordination problems, so their effective extraction is amplified relative to local constraints. The global scope enables the constraint to operate at high extractiveness despite moderate base extractiveness — the scope multiplication is critical to χ ≥ 0.66 in the snare perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint is correctly classified as tangled_rope because it possesses BOTH a coordination function (solving the user heterogeneity problem, genuinely creating value through network effects) AND asymmetric extraction (late entrants cannot compete, locked-in users cannot exit). The coordination function is real — networks are more valuable at scale, and reaching critical mass is a genuine coordination achievement. The extraction is real — the first-mover captures disproportionate reward, and the topology prevents competition. Neither can be reduced to the other. The false-summit risk is the natural-law perspective that treats critical mass as inevitable. The resolution is to recognize that critical mass is contingent on: (1) absent interoperability (switching costs high), (2) policy choices that enforce lock-in (API restrictions, data portability absent), and (3) first-mover advantage that is real but time-bound. The constraint transitions from tangled_rope to scaffold once interoperability is deployed — the coordination function remains but extraction falls as switching costs approach zero. The constraint transitions from tangled_rope to rope once regulatory enforcement dissolves lock-in mechanisms. The apparent inevitability of network effects is actually the inevitability of lock-in given current policy frameworks, not inevitability of the network effects themselves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_mass_threshold_definition,
    'What constitutes the ''critical mass'' threshold at which network effects become self-reinforcing? Is it absolute (fixed user count) or relative (percentage of addressable market)?',
    'Longitudinal analysis of adoption curves across different platforms and markets; identification of inflection points where user growth shifts from exogenous to endogenous; cross-market comparison of threshold percentages',
    'If threshold is fixed: extractiveness is determined by market size and adoption progress. If relative: extractiveness is determined by competitive positioning and market fragmentation. Different ε values result depending on resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(critical_mass_threshold_definition, empirical, 'Definition and measurement of critical mass threshold').

omega_variable(
    interoperability_feasibility,
    'Can interoperability standards (ActivityPub, data portability, federated identity) actually reduce switching costs enough to dissolve the critical mass barrier, or do network externalities persist even with technical interoperability?',
    'Comparative analysis of federated platforms (Mastodon, Signal) vs centralized incumbents (Twitter, WhatsApp); measurement of actual user switching rates after interoperability features are deployed; analysis of whether users remain on largest instance/platform despite interoperability availability',
    'If feasible: scaffold perspective is structural, sunset is real, extractiveness can decline toward 0.30 range. If infeasible: network effects persist despite technical interoperability, suggesting coordination problem is deeper than switching costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_feasibility, empirical, 'Whether interoperability standards can dissolve critical mass barriers').

omega_variable(
    user_heterogeneity_in_network_value,
    'Do all users contribute equally to network value, or do highly heterogeneous user cohorts (power users, influencers, enterprise customers) disproportionately drive critical mass perception?',
    'Network analysis of influence distribution; measurement of user engagement distribution; analysis of platform growth rate correlation with acquisition of specific user cohorts; comparison of platform growth before and after major influencer/enterprise adoption',
    'If heterogeneous: critical mass is driven by acquiring the ''right'' users rather than absolute numbers; extractiveness is higher for late entrants who must compete for scarce power-user attention. If homogeneous: critical mass is pure scale, symmetric across user types.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_heterogeneity_in_network_value, empirical, 'Heterogeneity of network value contribution across user types').

omega_variable(
    enforcement_mechanism_authenticity,
    'Is the critical mass lock-in enforced by genuine switching costs and user externalities, or by artificial platform-imposed friction (making data export difficult, restricting API access, social pressure)?',
    'Experimental measurement of switching costs with and without platform-imposed friction; analysis of user exit behavior when friction is removed (e.g., GDPR data portability compliance); comparison of platforms with high vs low data portability commitment',
    'If genuine: critical mass reflects real coordination problems, extractiveness is ~0.52 or higher due to network topology. If artificial: much of the suppression (0.58) is policy-controlled and reversible; extractiveness could fall to ~0.25 with policy changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_authenticity, empirical, 'Whether lock-in is enforced by genuine switching costs or artificial friction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(network_effect_critical_mass, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(neteff_tr_t0, network_effect_critical_mass, theater_ratio, 0, 0.25).
narrative_ontology:measurement(neteff_tr_t3, network_effect_critical_mass, theater_ratio, 3, 0.28).
narrative_ontology:measurement(neteff_tr_t6, network_effect_critical_mass, theater_ratio, 6, 0.33).
narrative_ontology:measurement(neteff_tr_t10, network_effect_critical_mass, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(neteff_be_t0, network_effect_critical_mass, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(neteff_be_t3, network_effect_critical_mass, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(neteff_be_t6, network_effect_critical_mass, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(neteff_be_t10, network_effect_critical_mass, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(network_effect_critical_mass, resource_allocation).
narrative_ontology:affects_constraint(network_effect_critical_mass, platform_data_lock_in).
narrative_ontology:affects_constraint(network_effect_critical_mass, social_graph_monopoly).
narrative_ontology:affects_constraint(network_effect_critical_mass, api_gatekeeping).

% DUAL FORMULATION NOTE:
% The critical mass constraint is upstream of specific platform-level lock-in mechanisms (data lock-in, social graph capture, API gatekeeping). These downstream constraints have their own extractiveness values reflecting domain-specific extraction mechanisms; the critical mass threshold represents the topological prerequisite that enables those mechanisms to operate effectively. Decomposition: critical_mass_threshold (ε=0.52, Tangled Rope) → platform_data_lock_in (ε=0.65, Snare) → api_gatekeeping (ε=0.48, Tangled Rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(network_effect_critical_mass, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
