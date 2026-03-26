% ============================================================================
% CONSTRAINT STORY: collective_action_capacity_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collective_action_capacity_collapse, []).

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
 *   constraint_id: collective_action_capacity_collapse
 *   human_readable: Collective Action Capacity Collapse
 *   domain: political_economy/coordination_failure
 *
 * SUMMARY:
 *   Collective action capacity collapse describes the structural constraint
 *   in which distributed individual agents cannot coordinate their actions to
 *   solve shared problems, even when coordination would benefit all. The
 *   constraint exhibits the full signature of a snare: high suppression
 *   (0.72) reflects insurmountable barriers to coordination at scale; high
 *   extractiveness (0.58) reflects that the mechanism concentrates benefits
 *   onto status quo beneficiaries while distributing costs onto distributed
 *   agents; theater ratio (0.68) reflects that vestigial coordination
 *   structures persist through inertia despite functional atrophy. The
 *   measurement trajectory shows monotonic degradation: base extractiveness
 *   rose from 0.32 to 0.58 over the interval, indicating increasing
 *   concentration of benefits. Theater ratio rose from 0.42 to 0.68,
 *   indicating that formal coordination structures have become progressively
 *   more performative relative to their actual capacity. The constraint
 *   manifests across domains (labor, environmental, political) wherever
 *   distributed agents cannot overcome freerider incentives, information
 *   asymmetries, and coordination barriers. Incumbent beneficiaries
 *   experience the collapse not as constraint but as stability — the
 *   inability of distributed agents to mobilize is precisely the coordination
 *   outcome that preserves status quo power. The six perspectives reveal the
 *   full perspectival range: from the trapped distributed agent (snare) to
 *   the beneficiary who sees the constraint as solution (rope), to
 *   organizational brokers (tangled rope) and degraded civic institutions
 *   (piton). The analytical observer risks naturalizing this constraint as an
 *   immutable law of collective action (mountain), when structural evidence
 *   suggests the collapse is contingent on institutional design,
 *   communication technology, and cultural fatalism about collective
 *   efficacy.
 *
 * KEY AGENTS:
 *   - Distributed Individual Agents: Primary victims (powerless/trapped) — face insurmountable freerider barriers; no incentive for unilateral defection from non-coordination; zero exit options
 *   - Sub-collective Coalitions: Secondary victims (moderate/constrained) — some organization possible at local scale but insufficient for critical mass; high costs for maintaining internal discipline
 *   - Organizational Brokers: Intermediate actors (organized/arbitrage) — political parties, unions, NGOs extract asymmetric benefits while providing some coordination function; can exit by redirecting attention
 *   - Status Quo Beneficiaries: Primary beneficiaries (institutional/arbitrage) — incumbent power holders benefit from inability of distributed agents to mobilize; constraint functions as stability mechanism for their position
 *   - Vestigial Civic Organizations: Degraded structures (organized/constrained) — former coordination mechanisms persist through institutional inertia despite functional collapse; high theater ratio indicates performance replacing function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent equilibrium as universal law; frames collapse as Olson's universal logic rather than contingent institutional outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collective_action_capacity_collapse, 0.58).
domain_priors:suppression_score(collective_action_capacity_collapse, 0.72).
domain_priors:theater_ratio(collective_action_capacity_collapse, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collective_action_capacity_collapse, extractiveness, 0.58).
narrative_ontology:constraint_metric(collective_action_capacity_collapse, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(collective_action_capacity_collapse, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collective_action_capacity_collapse, snare).
narrative_ontology:human_readable(collective_action_capacity_collapse, "Collective Action Capacity Collapse").
narrative_ontology:topic_domain(collective_action_capacity_collapse, "political_economy/coordination_failure").

domain_priors:requires_active_enforcement(collective_action_capacity_collapse).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(collective_action_capacity_collapse, incumbent_beneficiaries).
narrative_ontology:constraint_victim(collective_action_capacity_collapse, distributed_collective_agents).
narrative_ontology:constraint_victim(collective_action_capacity_collapse, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISTRIBUTED COLLECTIVE AGENT (SNARE) — Individual agents face insurmountable barriers to exit. Any single defector gains no benefit while bearing full cost of non-participation. Coordination mechanisms have atrophied or been actively suppressed. Trapped within a system where rational individual behavior aggregates to collective failure. Zero degrees of freedom.
constraint_indexing:constraint_classification(collective_action_capacity_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUB-COLLECTIVE COALITION (SNARE) — Some organization possible at scale below critical mass. Coalitions face high costs for defection from intra-group norms but insufficient power to solve the larger collective action problem. Constrained by resource requirements and internal freerider pressure. Extraction dominates coordination function.
constraint_indexing:constraint_classification(collective_action_capacity_collapse, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ORGANIZATIONAL BROKER (TANGLED ROPE) — Intermediaries (political parties, unions, NGOs) extract fees/control while coordinating collective action. They provide genuine coordination function but benefit asymmetrically from the arrangement. Can exit by redirecting collective energy elsewhere. Active enforcement of hierarchy maintains the asymmetry.
constraint_indexing:constraint_classification(collective_action_capacity_collapse, tangled_rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATUS QUO BENEFICIARY (ROPE) — Incumbent institutions and power holders experience the collapse as pure coordination: preventing mobilization IS the coordination problem they need solved. Their exit is costless — the constraint binds others, not them. They see low extraction because the system functions perfectly from their perspective.
constraint_indexing:constraint_classification(collective_action_capacity_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: VESTIGIAL ORGANIZATION (PITON) — Former coordination structures (labor unions, civic associations, religious organizations) persist through institutional inertia despite atrophied capacity. Theater persists: meetings, publications, rituals claiming capacity that no longer exists. Membership maintains formal structure despite functional collapse. High theater ratio indicates degraded coordination function.
constraint_indexing:constraint_classification(collective_action_capacity_collapse, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a theoretical standpoint, the Olson logic is presented as an invariant: larger groups inherently face exponentially rising coordination costs. This perspective sees collective action failure as a natural law of social physics — unavoidable, universal, irreducible. However, this masks the contingency: communication technology, institutional design, and cultural norms all modulate the cost structure. The mountain classification risks naturalizing what is a complex, plastic equilibrium.
constraint_indexing:constraint_classification(collective_action_capacity_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_action_capacity_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(collective_action_capacity_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collective_action_capacity_collapse, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(collective_action_capacity_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(collective_action_capacity_collapse, TR),
    TR >= 0.70.

:- end_tests(collective_action_capacity_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint concentrates benefits on status quo beneficiaries while distributing costs across distributed agents. The measurement trajectory (0.32→0.58) shows increasing extraction over time as coordination capacity has atrophied and broker control has consolidated. The extractiveness reflects both the freerider mechanism (individuals cannot capture full benefit of coordination) and the institutional capture by incumbent beneficiaries (those already possessing power benefit from the inability of distributed agents to mobilize). Suppression (0.72): High. Multiple interlocking barriers prevent coordination: information asymmetries (agents don't know others' preferences or commitments), freerider incentives (benefit to defecting from collective action exceeds benefit to staying), coordination costs (assembling and maintaining large groups requires infrastructure), and legal/institutional barriers (systems designed to prevent or channel collective action). The suppression is substantial but not total — localized coordination is possible under specific conditions (small groups, high salience, repeated interaction, institutional support). Theater ratio (0.68): Moderately high. Formal coordination structures (unions, parties, associations) persist through institutional inertia despite significantly atrophied actual capacity. They maintain meeting schedules, publications, rituals of decision-making, and claims of representation while actual member engagement and effective decision-making power have collapsed. The theater provides legitimacy cover for what is increasingly broker control rather than collective agency. Measurements show the theater ratio increasing as actual coordination function declined — as the real capacity atrophied, the performative content rose to maintain institutional legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The constraint appears as rope to the status quo beneficiary (coordination function = maintaining stability works perfectly for them), as snare to the distributed agent (maximum suppression, zero agency), as tangled rope to the broker (genuine coordination plus asymmetric extraction), as piton to the vestigial organization (formal structure, atrophied function), and as mountain to the analyst (universal law of collective action). Each perspective is accurate from its structural position. The perspectival gap reveals that the same mechanism is Rope to those who benefit, Snare to those who bear costs, and Piton to the institutions that once enabled coordination but now just perform it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim status plus exit options. Status quo beneficiaries with arbitrage exit (can leave the problem domain entirely) experience low or negative d — they benefit from non-coordination. Distributed agents with trapped exit experience high d (1.0) — they bear full cost and have no escape. Organizational brokers with arbitrage but also victim status (bearing some suppression costs) experience moderate d (~0.55). The piton agent with constrained exit but degraded function experiences d around 0.35 — they're weakly trapped by institutional structures but their actual constraint is the atrophy of their own capacity rather than external suppression. The analytical observer with analytical exit experiences canonical d≈0.73, observing the full structure but from a potentially naturalizing perspective. The engine derives these d values from the beneficiary/victim declarations and applies the sigmoid f(d) to compute effective extractiveness chi for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves through perspectival decomposition. The constraint is not 'Snare vs Rope' — it is Snare from the perspective of distributed agents, Rope from the perspective of status quo beneficiaries, Tangled Rope from the perspective of brokers, and Piton from the perspective of degraded civic institutions. The analytical observer risks false summit classification (mountain) by naturalizing the contingent equilibrium as universal law. The mandatrophy is resolved by recognizing that the classification depends on index (P,T,E,S): the same extractiveness value (0.58) produces different types from different perspectives because the experienced extraction depends on the agent's structural position (beneficiary vs victim, exit options, time horizon). The primary classification (snare) corresponds to the powerless, trapped, distributed agent perspective — the victims who bear the constraint. Alternative perspectives show why the constraint persists: beneficiaries experience it as solution, brokers benefit from it, vestigial institutions maintain it through inertia, and analysts risk naturalizing it as law. The system is not in error — it is revealing that multiple perspectives on the same extractiveness value produce multiple types depending on structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    olson_universality,
    'Is the Olson logic a universal law of collective action or a contingent equilibrium dependent on institutional design and communication technology?',
    'Historical comparison of coordination success across different institutional designs (federalism, nested subsidiarity, platform-mediated coordination); measurement of actual coordination costs under different communication regimes; identification of successful large-scale coordination mechanisms.',
    'If universal: collective action failure is inherent to scale, justifying status quo. If contingent: institutional redesign and technology diffusion could collapse the constraint entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(olson_universality, conceptual, 'Whether Olson logic represents universal law or contingent institutional equilibrium').

omega_variable(
    suppression_mechanism_internalization,
    'How much of the measured suppression (0.72) is structural (external barriers to coordination) versus internalized (the distributed agents have absorbed fatalism about their collective capacity)?',
    'Post-mobilization analysis: when external barriers to coordination are removed (e.g., platform tools, meeting infrastructure provided), do agents mobilize? If yes, suppression was largely structural. If no, suppression is partly internalized as learned helplessness.',
    'If structural: targeted removal of barriers (communication technology, legal space) can restore capacity. If internalized: capacity requires also restoring cognitive-emotional belief in collective agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Proportion of suppression that is structural versus internalized').

omega_variable(
    broker_extraction_necessity,
    'Is the asymmetric extraction by organizational brokers (perspective 3) a necessary coordinating cost or opportunistic rent-seeking layered onto coordination?',
    'Comparative analysis of broker control ratios across organizations with different governance models; measurement of coordination success and member satisfaction in democratic vs. hierarchical broker structures; identification of minimum necessary coordination overhead.',
    'If necessary: tangled rope classification is appropriate; extraction is justified coordination cost. If opportunistic: constraint should be re-classified as snare in many contexts; organizational redesign could reduce extraction significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(broker_extraction_necessity, empirical, 'Whether broker extraction is necessary coordination cost or opportunistic rent-seeking').

omega_variable(
    network_externality_collapse,
    'Does the collapse occur at a critical threshold of network fragmentation, or does it degrade continuously with declining coordination density?',
    'Network analysis of real-world collective structures at different fragmentation levels; identification of any threshold behavior in coordination success rates; modeling of phase transitions in collective action capacity.',
    'If threshold: some possibility of sudden recovery if minimum network density is restored. If continuous degradation: capacity must be maintained continuously or risks slow atrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_externality_collapse, empirical, 'Whether capacity collapse occurs at critical threshold or through continuous degradation').

omega_variable(
    futurity_discount_asymmetry,
    'Is the collapse fundamentally driven by present-focused incentive structures that discount future harms, or by present-time constraints on information and coordination capacity?',
    'Behavioral analysis of agent time horizons and discount rates; comparison of mobilization success for immediate vs. delayed benefits; identification of whether extending time horizon restores coordination or reveals deeper constraints.',
    'If driven by discounting: interventions that align present and future incentives could restore capacity. If driven by present constraints: temporal alignment alone is insufficient; capacity infrastructure must be built independent of incentive structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(futurity_discount_asymmetry, conceptual, 'Whether collapse is driven by time discounting or by present coordination capacity constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collective_action_capacity_collapse, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cacol_tr_t0, collective_action_capacity_collapse, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cacol_tr_t3, collective_action_capacity_collapse, theater_ratio, 3, 0.55).
narrative_ontology:measurement(cacol_tr_t6, collective_action_capacity_collapse, theater_ratio, 6, 0.64).
narrative_ontology:measurement(cacol_tr_t9, collective_action_capacity_collapse, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(cacol_be_t0, collective_action_capacity_collapse, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cacol_be_t3, collective_action_capacity_collapse, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(cacol_be_t6, collective_action_capacity_collapse, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(cacol_be_t9, collective_action_capacity_collapse, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collective_action_capacity_collapse, resource_allocation).
narrative_ontology:affects_constraint(collective_action_capacity_collapse, freerider_incentive_structure).
narrative_ontology:affects_constraint(collective_action_capacity_collapse, information_asymmetry_coordination).
narrative_ontology:affects_constraint(collective_action_capacity_collapse, incumbent_power_stability).
narrative_ontology:affects_constraint(collective_action_capacity_collapse, civic_institutional_erosion).

% DUAL FORMULATION NOTE:
% Collective action capacity collapse decomposes into multiple structurally distinct constraints: the freerider mechanism (ε≈0.35, Rope from beneficiary view, Snare from victim view), information asymmetries preventing common knowledge formation (ε≈0.25, primarily coordination problem), incumbent power preservation through preventive suppression (ε≈0.42, Snare), and civic institutional atrophy (ε≈0.62, Piton). The present story captures the integrated macro-level constraint; upstream stories model the constituent mechanisms. The network edges indicate structural dependency: the collapse requires all constituent mechanisms to fail simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(collective_action_capacity_collapse, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
