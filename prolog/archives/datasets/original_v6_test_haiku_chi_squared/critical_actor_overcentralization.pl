% ============================================================================
% CONSTRAINT STORY: critical_actor_overcentralization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_critical_actor_overcentralization, []).

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
 *   constraint_id: critical_actor_overcentralization
 *   human_readable: The Single Point of Failure
 *   domain: logistical/technological/economic
 *
 * SUMMARY:
 *   A single point of failure in network architecture creates a structural
 *   vulnerability where the entire system's viability depends on the
 *   continued capacity and benevolence of one actor. This constraint appears
 *   across logistical networks (Amazon's role in e-commerce fulfillment),
 *   financial systems (clearinghouse banks in payment settlement),
 *   technological infrastructure (cloud providers in digital services), and
 *   organizational hierarchies (charismatic founder dependency). The
 *   constraint is not inherently pathological — centralization around a
 *   capable hub solves genuine coordination problems and eliminates
 *   duplicative inefficiency. However, once established, the critical actor's
 *   structural position creates extraction opportunities: dependency can be
 *   monetized through rent-seeking (higher fees, restrictive terms), held
 *   hostage for policy concessions (content moderation, data sharing), or
 *   weaponized for competitive advantage. The dependent nodes face a
 *   coordination dilemma: individually, exit is costly (switching costs, loss
 *   of network effects); collectively, exit is possible (distributed
 *   alternatives exist) but requires synchronized defection to reach critical
 *   mass. The constraint exhibits all six DR types from different
 *   perspectives because the same structural phenomenon can be read as an
 *   efficient coordination mechanism (rope), a degraded legacy system
 *   (piton), an opportunity for organized alternatives (scaffold), a source
 *   of power-asymmetric extraction (tangled rope from the regulator view), a
 *   survival trap (snare from the dependent nodes view), or a fundamental
 *   fragility (snare from the analytical observer's civilizational
 *   perspective). The extractiveness score (0.58) reflects that the critical
 *   actor is actively leveraging its position but not yet at maximum capacity
 *   — dependent nodes retain some agency in negotiation, and alternatives are
 *   developing. Theater ratio (0.38) is relatively low because
 *   single-point-of-failure constraints require continuous operational
 *   legitimacy: the critical actor cannot easily mask non-performance, and
 *   catastrophic failures expose the constraint's extractive structure. The
 *   suppression score (0.72) reflects high barriers to exit: network effects,
 *   switching costs, regulatory inertia, and technical complexity all
 *   suppress alternatives.
 *
 * KEY AGENTS:
 *   - Critical Actor: Institutional beneficiary (institutional/arbitrage) — occupies sole access point; extracts rent from dependency; perceives role as coordination solution
 *   - Dependent Network Participants: Primary victims (powerless/trapped) — bear costs of centralized risk and extraction; cannot exit without losing network access entirely
 *   - Systemic Resilience: Abstract victim (powerless/trapped) — the network's adaptive capacity is constrained by single-point risk; bears civilizational-scale cost of systemic fragility
 *   - Regulator or Network Designer: Moderate actor (moderate/constrained) — recognizes both coordination value and extraction hazard; constrained by sunk costs and transition barriers
 *   - Resilience Coalition: Organized agents (organized/mobile) — consortium of alternative infrastructure developers; see distributed alternatives as a sunset pathway
 *   - Competing Incumbent: Powerful peer (powerful/mobile) — could build redundancy but deterred by coordination costs; sees both efficiency and monopoly rent opportunity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — evaluates trade-off between coordination efficiency and systemic fragility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(critical_actor_overcentralization, 0.58).
domain_priors:suppression_score(critical_actor_overcentralization, 0.72).
domain_priors:theater_ratio(critical_actor_overcentralization, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(critical_actor_overcentralization, extractiveness, 0.58).
narrative_ontology:constraint_metric(critical_actor_overcentralization, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(critical_actor_overcentralization, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(critical_actor_overcentralization, snare).
narrative_ontology:human_readable(critical_actor_overcentralization, "The Single Point of Failure").
narrative_ontology:topic_domain(critical_actor_overcentralization, "logistical/technological/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(critical_actor_overcentralization, critical_actor).
narrative_ontology:constraint_victim(critical_actor_overcentralization, dependent_network_participants).
narrative_ontology:constraint_victim(critical_actor_overcentralization, systemic_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT NETWORK PARTICIPANT (SNARE) — Cannot exit without abandoning network access entirely. Bears full cost of single actor's rent extraction through enforced dependency. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.95. High effective extraction driven by trapped exit and global scope amplification.
constraint_indexing:constraint_classification(critical_actor_overcentralization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CRITICAL ACTOR (ROPE) — Experiences the constraint as coordination mechanism: centralizing network operations through this actor solves the collective action problem of establishing a common hub. Benefits from structural position without perception of extraction. d≈0.02, f(d)≈-0.18, σ=1.2 → χ≈-0.12. Negative effective extraction; net beneficiary.
constraint_indexing:constraint_classification(critical_actor_overcentralization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATOR/NETWORK DESIGNER (TANGLED ROPE) — Recognizes both the coordination value (eliminating duplicative intermediaries) and the extraction hazard (dependency leverage). Constrained by the sunk cost of existing infrastructure; cannot easily migrate without catastrophic disruption. Sees mandatory diversification requirements as extraction-suppression mechanism. d≈0.58, f(d)≈0.70, σ=1.0 → χ≈0.40. Mixed: genuine coordination function but asymmetric extraction.
constraint_indexing:constraint_classification(critical_actor_overcentralization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGACY INFRASTRUCTURE OPERATOR (PITON) — Maintains the single-point architecture through institutional inertia. Alternatives (distributed ledgers, redundant systems, mesh networks) exist but require coordinated migration. Theater_ratio≈0.38 is below the piton gate (≥0.70), but this perspective reveals why: the legacy operator's theatrical maintenance activities (quarterly disaster recovery drills, compliance reporting) mask the fact that true functional resilience has atrophied — the drills never trigger real failover because switching costs are prohibitive.
constraint_indexing:constraint_classification(critical_actor_overcentralization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RESILIENCE COALITION (SCAFFOLD) — Organized actors (blockchain consortia, federated databases, mesh network advocates) perceive the single-point constraint as a temporary coordination failure with a sunset: distributed alternatives are technically viable and deployment timelines are contracting. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.22. Low effective extraction because mobile exit enables coalition to defect to alternative architectures within a defined timescale (5-15 years for mainstream adoption).
constraint_indexing:constraint_classification(critical_actor_overcentralization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COMPETING INCUMBENT (TANGLED ROPE) — Powerful actor that could build alternative infrastructure (a second clearinghouse, rival cloud provider) but is deterred by coordination costs and switching network effects. Sees both coordination value (single hub is genuinely efficient) and extraction opportunity (could charge for redundancy). d≈0.48, f(d)≈0.63, σ=1.0 → χ≈0.36. Mixed: coordination benefit from standardization but extraction incentive from monopoly rent.
constraint_indexing:constraint_classification(critical_actor_overcentralization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — Sees the single-point-of-failure as a constraint on the entire system's adaptive capacity. The network is trapped in a Pareto-efficient but fragile equilibrium: decentralization would reduce extraction but increase coordination costs. The analytical observer measures d≈0.80 (universal scope, civilizational timescale, civilizational stakes), f(d)≈1.18, σ=1.0 → χ≈0.68. The constraint is snare because it extracts from systemic resilience itself.
constraint_indexing:constraint_classification(critical_actor_overcentralization, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(critical_actor_overcentralization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(critical_actor_overcentralization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(critical_actor_overcentralization, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(critical_actor_overcentralization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(critical_actor_overcentralization, TR),
    TR >= 0.70.

:- end_tests(critical_actor_overcentralization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The critical actor's structural position generates extraction opportunities, but not at the maximum level because: (1) dependent nodes retain some individual negotiating power (they can threaten defection, migrate non-critical operations, or demand contractual protections); (2) competitive threat from alternative infrastructure constrains rent-seeking (aggressive extraction accelerates defection); (3) regulatory scrutiny of monopolistic leverage creates political costs. The score reflects active rent-seeking (higher fees, unfavorable terms, restricted API access) combined with passive leverage (inherited monopoly position without aggressive threats). Over the measurement interval, extractiveness rises (0.42 → 0.58) because the critical actor incrementally increases terms-of-service burden as dependent nodes' switching costs increase (network effects deepen). Suppression (0.72): High. Barriers to exit include: (1) switching costs (technical migration, data export, validation on new platform); (2) network effects (the value of the network depends on its density; leaving reduces value for remaining nodes); (3) regulatory friction (the critical actor often occupies a regulated position; alternative platforms face approval delays); (4) technical complexity (replicating the critical actor's full functionality is non-trivial); (5) coordination problem (individuals cannot exit in isolation; defection requires collective action). Theater ratio (0.38): Low-moderate. The single-point-of-failure constraint is relatively functionally transparent compared to other snares because catastrophic failures expose the constraint's structure. The critical actor cannot hide non-performance through performative activity — the constraint lives or dies by operational reality. However, theater does exist: disaster recovery theater (drills, backups), compliance theater (certifications, audits), and network-effect theater (making exit appear costlier than it actually is). The theater ratio is lower in mature critical nodes and rises slightly over time (0.25 → 0.38) as the operator invests in perceived resilience rather than actual redundancy.
 *
 * PERSPECTIVAL GAP:
 *   The critical actor sees coordination (Rope); dependent nodes see extraction (Snare); the regulator sees mixed coordination-extraction (Tangled Rope); alternatives see a sunset problem (Scaffold); the analytical observer sees systemic fragility (Snare at civilizational scale). The perspectival gap reveals the core tension: the critical actor's experience of coordination efficiency masks the dependent nodes' experience of extraction. From the critical actor's perspective, d ≈ 0.02 (beneficiary/arbitrage) produces negative effective extraction (χ ≈ -0.12), confirming their perception of a fair coordination solution. From the dependent nodes' perspective, d ≈ 0.93 (victim/trapped) produces high effective extraction (χ ≈ 0.95), confirming their perception of a snare. The gap arises not from observational disagreement but from structural asymmetry: the same constraint delivers coordination value to the hub and extraction value to the periphery. The piton perspective reveals that maintaining the single-point architecture requires increasing theatrical investment (drills, backups, reassurance) precisely because true functional alternatives are developing and making the legacy dependency appear increasingly irrational. The scaffold perspective shows that organized defection is plausible but requires sustained coordination — the coalition can build alternatives within 5-15 years if switching cost thresholds fall below critical levels or alternative maturity crosses a viability threshold.
 *
 * DIRECTIONALITY LOGIC:
 *   Critical Actor: Beneficiary + arbitrage → d ≈ 0.02. The derivation captures that the critical actor perceives its position as solving a genuine coordination problem with no extraction. The arbitrage exit option reflects their capacity to credibly defect to alternative markets or revenue streams (though they rarely exercise this option). Dependent Nodes: Victim + trapped → d ≈ 0.93. The high d reflects that dependent nodes bear costs (availability risk, price pressure, terms-of-service burden) and have no credible exit. Trapped exit dominates because network effects and switching costs exceed the cost of continued dependency for most nodes. Regulator/Designer: Both + constrained → d ≈ 0.58. The regulator sees both the coordination value (the single-point actor eliminated duplicative intermediaries) and the extraction value (dependent nodes face rent extraction). Constrained exit reflects that the regulator cannot mandate alternatives without disrupting the existing network — the sunk cost of current infrastructure constrains their agency. Resilience Coalition: Victim + mobile → d ≈ 0.35. The coalition perceives extraction (the current architecture constrains their growth and innovation) but has mobile exit options (build alternatives, migrate users). The lower d reflects that this group can defect credibly and has medium-term pathways to do so. Competing Incumbent: Mixed + mobile → d ≈ 0.48. The powerful incumbent perceives both efficiency (single hub is genuinely efficient) and extraction opportunity (could charge for redundancy). Mobile exit means they can credibly build alternatives if incentivized. Analytical Observer: Universal scope, trapped exit → d ≈ 0.80. The observer perceives systemic extraction (the constraint extracts from civilization's adaptive resilience) and has no exit (they cannot abandon the analysis of civilizational risk). The high d reflects the stakes and the trapped perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE OR ROPE?: The mandatrophy is resolved by recognizing that snare and rope are not contradictory readings of the same constraint — they are asymmetric readings of the same structural position. The critical actor genuinely solves a coordination problem: eliminating duplicative intermediaries, standardizing processes, reducing transaction costs for the entire network. This is the coordination function, and the critical actor experiences it as rope (low extraction, beneficial to all). Simultaneously, dependent nodes experience extraction because their exit options are suppressed (trapped) and the critical actor can monetize their dependency. The snare classification dominates for dependent nodes because suppression (0.72) is high and extraction (0.58) is material. The rope classification dominates for the critical actor because d ≈ 0.02 produces negative effective extraction. This is not a case of 'which type is really correct?' — both are correct relative to structural position. The mandatrophy is resolved by the network topology: the single-point-of-failure constraint creates asymmetric power distribution that cannot be erased by relabeling. PREVENTING MISCLASSIFICATION: The error to avoid is naturalizing the single-point-of-failure as an immutable coordination requirement. Network theory demonstrates that highly distributed architectures (mesh networks, blockchain consensus, federated databases) can solve identical coordination problems with lower extraction. The single-point architecture is chosen because it is locally efficient (easier to manage, lower per-node costs) not because it is uniquely capable. The constraint's evolution from rope-dominated (early network formation, genuine efficiency) to snare-dominated (mature dependency, extraction asymmetry) tracks empirically: as dependent nodes' switching costs increase and the critical actor's profit potential becomes clear, extraction intensifies. The theater ratio remains low because single-point failures are operationally visible — the constraint cannot hide behind performative activity for long.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_threshold,
    'What switching cost threshold makes exit from the single-point actor viable for dependent nodes?',
    'Cost-benefit analysis: migration to alternative (cost, time, technical risk) vs continuing under extraction. Empirical observation of when nodes actually switch.',
    'If threshold < 10% of annual operational cost: exit is truly mobile, perspectives 5 and 6 shift to mobile exit. If threshold > 30%: exit is effectively trapped, snare classification dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_threshold, empirical, 'Switching cost threshold for viable exit').

omega_variable(
    alternative_architecture_maturity,
    'Are distributed alternatives (blockchain, federated systems, mesh networks) genuinely functional for the critical node''s operational requirements?',
    'Technical comparison of latency, throughput, security, regulatory compliance; pilot deployments; cost modeling for equivalent service levels.',
    'If yes: scaffold perspective is structural (sunset is real). If no: alternatives are aspirational theater; scaffold collapses to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_architecture_maturity, empirical, 'Functional maturity of distributed alternatives').

omega_variable(
    critical_actor_rent_extraction_scope,
    'Is the critical actor actively extracting rents above competitive levels, or is the extraction passive (inherited monopoly position without aggressive leverage)?',
    'Price comparison with theoretical competitive alternatives; analysis of terms-of-service evolution; documented cases of leverage being exercised against dependent nodes.',
    'If active extraction: snare classification is empirically justified (χ≈0.95 is realistic). If passive: constraint may be rope with asymmetric power rather than snare (χ ≈ 0.35-0.45).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(critical_actor_rent_extraction_scope, empirical, 'Degree of active rent extraction by critical actor').

omega_variable(
    coalition_critical_mass_threshold,
    'What percentage of dependent nodes must coordinate for alternative infrastructure to reach critical mass and trigger defection cascade?',
    'Network game theory modeling; historical case studies of platform transitions (TCP/IP adoption, payment processor competition); threshold estimation from switching cost distribution.',
    'If threshold < 20%: coalition can easily trigger transition, scaffold sunset becomes inevitable (5-10 year timescale). If threshold > 50%: coordination failure locks in snare structure for decades.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_critical_mass_threshold, empirical, 'Coalition critical mass for defection to alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(critical_actor_overcentralization, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spof_tr_t0, critical_actor_overcentralization, theater_ratio, 0, 0.25).
narrative_ontology:measurement(spof_tr_t10, critical_actor_overcentralization, theater_ratio, 10, 0.32).
narrative_ontology:measurement(spof_tr_t20, critical_actor_overcentralization, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(spof_be_t0, critical_actor_overcentralization, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(spof_be_t10, critical_actor_overcentralization, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(spof_be_t20, critical_actor_overcentralization, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(critical_actor_overcentralization, global_infrastructure).
narrative_ontology:affects_constraint(critical_actor_overcentralization, platform_monopoly_pricing).
narrative_ontology:affects_constraint(critical_actor_overcentralization, network_resilience_fragility).
narrative_ontology:affects_constraint(critical_actor_overcentralization, coordination_cost_externalization).

% DUAL FORMULATION NOTE:
% The single-point-of-failure constraint decomposes into three related constraints: (1) the coordination efficiency of centralization (platform_monopoly_pricing), (2) the systemic fragility of single-point architecture (network_resilience_fragility), and (3) the distribution of coordination costs to dependent nodes (coordination_cost_externalization). The ε value (0.58) reflects the mixed nature of this constraint — genuine coordination function paired with real extraction. Downstream constraints exhibit higher ε values as the extraction mechanism becomes clearer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
