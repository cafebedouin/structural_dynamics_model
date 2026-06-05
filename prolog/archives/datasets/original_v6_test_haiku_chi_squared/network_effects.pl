% ============================================================================
% CONSTRAINT STORY: network_effects
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_network_effects, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: network_effects
 *   human_readable: Network Effects (Demand-Side Economies of Scale)
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Network effects represent one of the most significant economic
 *   constraints of the digital age, yet they are commonly mischaracterized as
 *   immutable laws of economics when they are actually contingent
 *   institutional arrangements. A technology gains value as more people adopt
 *   it — this is the positive feedback loop — but the extraction mechanism
 *   depends entirely on whether users can exit. In a world with perfect
 *   interoperability (data portability, open standards, federated protocols),
 *   network effects are pure coordination: users benefit from a larger
 *   network and can switch at low cost. In a world with proprietary platforms
 *   and high switching costs, network effects become snares: users are locked
 *   in to an incumbent's network despite the existence of superior
 *   alternatives. The constraint's extractiveness has increased from 0.15 to
 *   0.58 over the past decade as platforms have: (1) actively engineered
 *   switching costs through proprietary APIs and data lock-in, (2) built
 *   comprehensive social graphs that become more valuable and costly to
 *   replicate, and (3) captured network value for the platform operator
 *   rather than distributing it to users. Theater ratio increased from 0.22
 *   to 0.48 as multi-homing became nominally common (users maintain multiple
 *   accounts) but functionally ineffective (primary identity remains
 *   concentrated on one platform). The constraint is a tangled rope because
 *   it serves a genuine coordination function (connecting dispersed users)
 *   while simultaneously extracting from those users through lock-in.
 *   Regulatory intervention and open standards movements represent structural
 *   attempts to lower the theater ratio and restructure the constraint toward
 *   pure coordination (Rope) or sunset (Scaffold).
 *
 * KEY AGENTS:
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — captures network value through lock-in; benefits from coordination surplus without bearing lock-in costs
 *   - Locked-In User: Primary victim (powerless/trapped) — embedded in dominant network; faces prohibitive switching costs; cannot exit despite alternatives
 *   - New Entrant Competitor: Secondary victim (moderate/constrained) — faces structural impossibility of competing; network's size advantage is insurmountable barrier to entry
 *   - Early Adopter: Secondary beneficiary (powerful/mobile) — established presence on winning network before lock-in solidified; can arbitrage between networks
 *   - Regulatory Authority: Organized actor (organized/constrained) — enforces interoperability, data portability, and antitrust constraints; extracts from platform while preserving coordination
 *   - Open Standards Coalition: Organized actors (organized/mobile) — builds federated protocols and interoperability standards to reduce switching costs; pursues sunset of lock-in constraint
 *   - Legacy Network Participant: Passive victim (moderate/constrained) — enrolled in deprecated network through sunk switching costs; remains passive despite preference drift
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent lock-in as immutable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(network_effects, 0.58).
domain_priors:suppression_score(network_effects, 0.65).
domain_priors:theater_ratio(network_effects, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(network_effects, extractiveness, 0.58).
narrative_ontology:constraint_metric(network_effects, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(network_effects, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(network_effects, tangled_rope).
narrative_ontology:human_readable(network_effects, "Network Effects (Demand-Side Economies of Scale)").
narrative_ontology:topic_domain(network_effects, "economic/technological").

domain_priors:requires_active_enforcement(network_effects).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(network_effects, platform_operator).
narrative_ontology:constraint_beneficiary(network_effects, early_adopters).
narrative_ontology:constraint_beneficiary(network_effects, incumbent_network_participants).
narrative_ontology:constraint_victim(network_effects, new_entrant_competitors).
narrative_ontology:constraint_victim(network_effects, switching_users).
narrative_ontology:constraint_victim(network_effects, network_excluded_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN USER (SNARE) — Once embedded in a dominant network (social media, messaging, payment rails), individual users face prohibitive switching costs. Network value is captured by the platform operator; users cannot exit without losing access to their entire social graph and transaction history. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.96. Pure extraction from user perspective.
constraint_indexing:constraint_classification(network_effects, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NEW ENTRANT COMPETITOR (SNARE) — A startup attempting to compete in a network-effects market faces a structural trap: the incumbent's network is more valuable precisely because it is larger, making it impossible to attract users to the new platform. Even superior technology cannot overcome the coordination barrier. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.67. High extraction through network-lock-in.
constraint_indexing:constraint_classification(network_effects, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR / INCUMBENT (ROPE) — The network operator experiences network effects as pure coordination: their product solves a collective action problem (connecting dispersed users). The coordination function generates genuine value — a billion-person network is more useful than a hundred-person network. The operator benefits from this coordination surplus. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.05. Net beneficiary; coordination is functional.
constraint_indexing:constraint_classification(network_effects, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (TANGLED ROPE) — Regulators must enforce antitrust constraints, interoperability mandates, and data portability rules while preserving the coordination function that makes the network valuable. The regulation extracts from the platform operator (mandatory interoperability, API access) but also enables coordination (users can port social graphs, switching becomes feasible). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.43. Hybrid: enforcement mechanism + coordination benefit.
constraint_indexing:constraint_classification(network_effects, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTEROPERABLE ALTERNATIVE (TANGLED ROPE) — A federation protocol (e.g., ActivityPub, Bluesky's AT Protocol) offers both coordination (federated network maintains social graph portability) and extraction resistance (no single operator captures all value). Participants benefit from coordination while maintaining exit options. d≈0.45, f(d)≈0.60, σ=1.2 → χ≈0.42. Medium effective extraction because federation requires coordination overhead.
constraint_indexing:constraint_classification(network_effects, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY PROPRIETARY NETWORK (PITON) — Old networks (MySpace, Friendster, Orkut) persist even after user preference shifts because switching costs are high and institutional momentum is strong. The network's value is maintained theatrically through inertia rather than genuine network effects. Users remain passively enrolled even though they access the network rarely. theater_ratio=0.62. The coordination function has atrophied; the extraction persists.
constraint_indexing:constraint_classification(network_effects, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, network effects appear as an immutable law: any technology that increases in value with adoption exhibits network economics. This is presented as a physical/economic law — inevitable, unchangeable, universal. However, the structural data (ε=0.58, suppression=0.65) reveals this as a false natural law. Network effects are contingent on: proprietary lock-in (not inherent), absence of interoperability standards (contingent policy choice), high switching costs (engineered, not natural), and regulatory forbearance (policy choice, not law). The false summit occurs because the analytical observer naturalizes what is actually institutional architecture.
constraint_indexing:constraint_classification(network_effects, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: OPEN STANDARDS COALITION (SCAFFOLD) — Movements toward interoperable protocols (ActivityPub for social media, DID for identity, open banking APIs) represent a temporary coordination solution with a sunset clause. As open standards mature and network switching costs decline, the original network-lock-in extraction mechanism loses force. d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.25. Low effective extraction because the coalition is building exit pathways. Theater ratio drops as protocol maturity increases.
constraint_indexing:constraint_classification(network_effects, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(network_effects_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(network_effects, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(network_effects, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(network_effects, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(network_effects, TR),
    TR >= 0.70.

:- end_tests(network_effects_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximum. Network effects generate genuine coordination value (the network IS more useful with more users), but the platform operator captures a disproportionate share of this value through proprietary lock-in rather than distributing it. The constraint would be lower extractiveness (≈0.25) if switching costs were low (open standards, data portability); it would be higher (≈0.72) if switching were literally impossible (no alternatives exist). At 0.58, the extractiveness reflects the current equilibrium: switching is difficult but not impossible; alternatives exist but are at a disadvantage; users are extracted from but not maximally. Suppression (0.65): High. Switching costs are engineered into the platform architecture: proprietary data formats, API restrictions, social graph lock-in, and regulatory barriers to interoperability. These suppress alternatives and reduce user exit options. However, suppression is not absolute (≈1.0) because regulatory pressure, open standards, and user frustration create cracks. Theater ratio (0.48): Moderate. The network's coordination function is genuine (a larger network is more useful), but the extraction is rationalized as inevitable ('network effects are laws of economics') rather than examined as institutional choice. Multi-homing is theatrically common but functionally rare. The 0.48 value reflects that roughly half the network activity is genuine coordination (users benefit from network size) and half is theater (platforms claiming lock-in as unavoidable while suppressing interoperability options).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a sharp perspectival gap between beneficiaries and victims. The platform operator sees network effects as pure coordination (Rope) — they are solving a collective action problem and deserve to capture value from doing so. The locked-in user sees the same constraint as pure extraction (Snare) — they are trapped by switching costs they did not choose and cannot escape. The new entrant competitor sees it as a structural barrier (Snare) — the incumbent's size advantage is insurmountable. The regulatory authority sees it as a hybrid (Tangled Rope) — coordination is valuable but extraction has become excessive, requiring enforcement to restore balance. The open standards coalition sees it as temporary (Scaffold) — interoperability standards will eventually dissolve lock-in, though the sunset is slow. The legacy network sees it as degraded (Piton) — the original coordination function has withered but institutional inertia keeps it alive. The analytical observer risks seeing it as a natural law (Mountain) — network effects are presented as immutable economic law, but this naturalizes policy choices about interoperability and data portability. Each perspective is structurally justified; the gap reveals that the same phenomenon is coordination, extraction, regulation, hope, theater, and false inevitability simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Locked-in user: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction directional vector. Cannot exit; bears full cost of lock-in. New entrant: Victim + constrained → d≈0.85, f(d)≈1.15. High extraction; can theoretically start a new network but faces insurmountable coordination barrier. Platform operator: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary with low directional extraction cost. Can exit (sell business, pivot) but benefits from staying. Regulatory authority: Organized + constrained → d≈0.55, f(d)≈0.75. Constrained by legal mandates and political economy; extracted from through compliance costs; benefits from coordination value. Interoperable alternative: Powerful + mobile → d≈0.45, f(d)≈0.60. Can exit toward federation; both constrained by protocol overhead and benefiting from coordination. Legacy network: Moderate + constrained → d≈0.65, f(d)≈1.00. Moderate extraction from sunk switching costs; passive enrollment. Open standards coalition: Organized + mobile → d≈0.35, f(d)≈0.35. Low effective extraction; active exit pathway building. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. Observer-relative directionality; mountain classification is perspectival outcome of naturalizing contingent architecture.
 *
 * MANDATROPHY ANALYSIS:
 *   Network effects resolve the mandatrophy by disambiguating the label. 'Network effects' conflates two structurally distinct claims: (1) the coordination value of larger networks (genuine positive externality: 'A network with N users is more useful than a network with N-1 users'), and (2) the lock-in mechanism that prevents users from exercising exit options (institutional policy choice: 'Users cannot move their data to competitor platforms'). Claim 1 is approximately a Mountain: it is a stable property of any technology that exhibits positive network externalities. Claim 2 is a Snare or Tangled Rope: it is a consequence of proprietary architecture and regulatory choices, not of the network externality itself. The corpus conflates these by using the label 'network effects' to justify lock-in as inevitable. The mandatrophy is resolved by recognizing that the COORDINATION VALUE is real and permanent (mountain-like), but the LOCK-IN MECHANISM is contingent and restructurable (snare/rope-like depending on regulation). A network effects constraint in a world with open standards and data portability is Rope (pure coordination). The same constraint in a proprietary architecture is Snare (extraction). The label 'network effects' does not determine the classification — institutional architecture does. This story therefore treats network effects as a Tangled Rope: real coordination value + real extraction mechanism, with the mix depending on policy choices about interoperability and data portability. The false natural law (mountain) is explicitly rejected by the analytical perspective's inclusion and commentary on why it fails the natural law gates (ε and suppression are too high; resistance is too low).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_lock_in_boundary,
    'At what point does network effect coordination become lock-in extraction? Where is the boundary between ''value from being in the network'' and ''extraction from inability to leave''?',
    'Measure user switching costs (data portability, social graph exportability, API access); compare coordination value (network utility) to extraction value (lock-in premium). Boundary occurs where switching cost exceeds network switching benefit.',
    'If switching costs ≤ net benefit of alternative: classification shifts toward Rope. If switching costs >> net benefit: classification is Snare. This determines whether network effects are fundamentally coordination or fundamentally extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_lock_in_boundary, empirical, 'Boundary between network coordination and lock-in extraction').

omega_variable(
    interoperability_standard_sufficiency,
    'Do open interoperability standards (ActivityPub, DID, open banking APIs) actually eliminate network lock-in, or do they merely add a new coordination overhead layer?',
    'Longitudinal measurement of user switching rates post-interoperability; comparison of switching costs before/after standard adoption; analysis of whether federation protocols generate their own lock-in (federation vendor lock-in).',
    'If interoperability is effective: scaffold sunset is real, network effects become reversible coordination. If interoperability adds overhead: the constraint shifts from platform lock-in to protocol lock-in, perpetuating extraction with a different gatekeeper.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_standard_sufficiency, empirical, 'Whether open standards eliminate network lock-in or merely transfer it').

omega_variable(
    natural_law_vs_policy_choice,
    'Is network effect lock-in an immutable law of economics, or a contingent institutional outcome that depends on policy choices (regulation, interoperability mandates, data portability)?',
    'Comparative analysis of network-effect markets under different regulatory regimes; examination of jurisdictions with strong interoperability requirements vs. light-touch regulation; case studies of network-effects constraints that were successfully dissolved through policy.',
    'If immutable: network effects are mountains (no policy escape). If contingent: network effects are snares/tangled ropes that can be restructured through regulation, interoperability, and open standards. This determines whether the constraint is a law of economics or a law of specific institutional design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_policy_choice, conceptual, 'Whether network lock-in is a natural law or a policy-contingent outcome').

omega_variable(
    multi_homing_viability,
    'Can users effectively multi-home (maintain presence on multiple networks) as an escape from lock-in, or do network effects create a dominant-platform equilibrium where multi-homing is theatrically common but functionally ineffective?',
    'User behavior analysis: active engagement distribution across platforms; measurement of true switching (primary identity shifts) vs. nominal presence (dormant accounts). Comparison of user effort required for multi-homing vs. single-platform specialization.',
    'If multi-homing works: users have genuine mobile options, exit_options shift from trapped/constrained to mobile. If multi-homing is theater: users appear to have choice but functionally remain locked in, perpetuating snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_homing_viability, empirical, 'Whether multi-homing is a viable exit option or theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(network_effects, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(neteff_tr_t0, network_effects, theater_ratio, 0, 0.22).
narrative_ontology:measurement(neteff_tr_t5, network_effects, theater_ratio, 5, 0.35).
narrative_ontology:measurement(neteff_tr_t10, network_effects, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(neteff_be_t0, network_effects, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(neteff_be_t5, network_effects, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(neteff_be_t10, network_effects, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(network_effects, resource_allocation).
narrative_ontology:boltzmann_floor_override(network_effects, 0.35).
narrative_ontology:affects_constraint(network_effects, platform_gatekeeping).
narrative_ontology:affects_constraint(network_effects, data_portability).
narrative_ontology:affects_constraint(network_effects, interoperability_standards).
narrative_ontology:affects_constraint(network_effects, two_sided_market_tipping).

% DUAL FORMULATION NOTE:
% Network effects decompose into: (1) coordination_value (pure positive externality, mountain-like), and (2) lock_in_mechanism (institutional extraction, snare-like). This story treats the composite phenomenon as Tangled Rope. Upstream: positive_network_externality (constraint_id: positive_externality_economics, ε≈0.05, Mountain). Downstream: platform_gatekeeping (constraint_id: platform_gatekeeping, ε≈0.62, Snare), data_portability (constraint_id: data_portability, ε≈0.48, Tangled Rope), interoperability_standards (constraint_id: interoperability_standards, ε≈0.35, Scaffold). Network effects as a unified phenomenon has ε≈0.58 (Tangled Rope) because it combines the coordination value (low ε) with the lock-in infrastructure (high ε).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(network_effects, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
