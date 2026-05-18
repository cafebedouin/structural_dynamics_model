% ============================================================================
% CONSTRAINT STORY: sicilian_expedition_overshooting_pathology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sicilian_expedition_overshooting_pathology, []).

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
 *   constraint_id: sicilian_expedition_overshooting_pathology
 *   human_readable: Sicilian Expedition as High-Bandwidth Overshooting Pathology
 *   domain: ancient_politics/military_overreach
 *
 * SUMMARY:
 *   In 415 BCE, the Athenian assembly voted to launch an expedition to Sicily
 *   with unprecedented scale and ambition. The decision was procedurally
 *   legitimate — passed by open debate and majority vote in the world's first
 *   democratic assembly — but operationally catastrophic. The assembly
 *   authorized a commitment (60+ ships, thousands of troops, extended
 *   overseas campaign) that the city's logistical infrastructure could not
 *   support, the military command could not coordinate coherently, and the
 *   intelligence apparatus could not adequately plan for. This case
 *   exemplifies a failure mode in distributed decision-making: high-bandwidth
 *   outer-layer authorization (assembly deliberation and voting) exceeds the
 *   low-bandwidth capacity of inner-layer execution (military logistics,
 *   naval supply chains, allied coordination). The constraint captures the
 *   mechanism by which democratic decisions can become snares — they are
 *   legitimate in form but impossible in operation, binding agents to
 *   commitments they cannot fulfill. The extractiveness trajectory (0.35 →
 *   0.68) reflects this escalation: initial authorization extracts political
 *   capital and resource commitment, but as logistical collapse becomes
 *   evident, the extraction intensifies because the trapped agents (soldiers,
 *   allies, treasury) face increasing costs with no exit mechanism.
 *
 * KEY AGENTS:
 *   - Expeditionary Soldiers: Primary victims (powerless/trapped) — conscripted or voluntarily enrolled; bear full cost of strategic failure; no exit after mobilization
 *   - Athenian Political Elite: Primary beneficiaries (institutional/arbitrage) — architects of the expedition; profit from decision-making authority, resource allocation power, and prestige; lower operational risk
 *   - Nicias, Alcibiades, Lamachos: Military Command (institutional/constrained) — inherit operationally incoherent mandate; manage supply shortfalls through improvisation; become performative theater
 *   - Allied Syracusan Forces: Secondary victims (moderate/constrained) — depend on Athenian support; trapped in alliance as bandwidth collapses; extraction increases as support degrades
 *   - Athenian Strategic Capacity: Systems-level victim (organized/mobile) — the city-state itself becomes snared; strategic flexibility exhausted; unable to defend homeland during Peloponnesian War
 *   - Athenian Assembly: Outer decision-making layer (institutional/arbitrage) — high-bandwidth authority structure; can authorize faster than logistics can execute; benefits from fast decisions; bears no direct operational cost
 *   - Athenian Logistics/Navy: Inner execution layer (institutional/constrained) — low-bandwidth operational capacity; cannot absorb authorization rate; becomes bottleneck; theater increases as improvisation replaces planning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sicilian_expedition_overshooting_pathology, 0.68).
domain_priors:suppression_score(sicilian_expedition_overshooting_pathology, 0.72).
domain_priors:theater_ratio(sicilian_expedition_overshooting_pathology, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sicilian_expedition_overshooting_pathology, extractiveness, 0.68).
narrative_ontology:constraint_metric(sicilian_expedition_overshooting_pathology, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sicilian_expedition_overshooting_pathology, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sicilian_expedition_overshooting_pathology, snare).
narrative_ontology:human_readable(sicilian_expedition_overshooting_pathology, "Sicilian Expedition as High-Bandwidth Overshooting Pathology").
narrative_ontology:topic_domain(sicilian_expedition_overshooting_pathology, "ancient_politics/military_overreach").

domain_priors:requires_active_enforcement(sicilian_expedition_overshooting_pathology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sicilian_expedition_overshooting_pathology, athenian_political_elite).
narrative_ontology:constraint_beneficiary(sicilian_expedition_overshooting_pathology, expedition_architects).
narrative_ontology:constraint_victim(sicilian_expedition_overshooting_pathology, expeditionary_soldiers).
narrative_ontology:constraint_victim(sicilian_expedition_overshooting_pathology, athenian_strategic_capacity).
narrative_ontology:constraint_victim(sicilian_expedition_overshooting_pathology, allied_forces).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPEDITIONARY SOLDIER (SNARE) — Conscripted or voluntarily enrolled into an operation authorized by assembly vote but operationally incoherent. No exit option once mobilized. Bears full cost of command failure, logistical collapse, and strategic mismatch. Trapped within a constraint that the outer (assembly) authority imposed without inner (operational) capacity to execute. Maximum suppression — the soldier cannot refuse a democratic decision, cannot leave mid-campaign, cannot escape the consequences of elite planning failure.
constraint_indexing:constraint_classification(sicilian_expedition_overshooting_pathology, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ATHENIAN POLITICAL ELITE (TANGLED ROPE) — Experiences the constraint as both coordination and extraction. The decision coordinates elite political ambitions (imperial expansion, prestige, wealth redistribution to shipbuilders and troops) while extracting unprecedented resources from the treasury and manpower from the citizen body. The elite authorize the expedition, profit from the decision-making authority and resource allocation power, and face lower operational risk than soldiers. Active enforcement required: the assembly vote must be enforced against budget objections and council hesitation. Beneficiaries (political architects) and victims (soldiers and treasury) are distinct. Chi moderates because the elite experience agency — they can modulate commitment, withdraw, reallocate resources — but asymmetry remains high.
constraint_indexing:constraint_classification(sicilian_expedition_overshooting_pathology, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: ALLIED SYRACUSAN FORCES (SNARE) — Promised Athenian support for their conflict with Syracuse but receive incomplete logistical support due to bandwidth collapse. Constrained exit (honor-bound to alliance, dependent on Athenian naval power) but face degraded coordination and resource extraction. The alliance mechanism created suppression: commitment to Athens now carries risk Athens cannot manage. High extraction — the Syracusan allies are drawn into a larger conflict they cannot control, on terms set by Athenian assembly without their participation.
constraint_indexing:constraint_classification(sicilian_expedition_overshooting_pathology, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ATHENIAN COMMAND STRUCTURE (PITON) — The generals (Nicias, Alcibiades, Lamachos) inherit an operationally incoherent mandate: the assembly authorized an expedition of unprecedented scale without specifying supply chains, force composition, or success criteria. Command becomes performative theater — generating reports to justify escalation, managing supply shortfalls through improvisation, maintaining morale in a failing operation. The command structure was originally a coordination mechanism (strategic direction, tactical execution) but has degraded into ritual response to assembly pressure. Theater ratio (0.58) reflects that command activity becomes increasingly divorced from operational function — ceremony of leadership substituting for actual strategic adjustment. The institution persists through inertia, not because it works.
constraint_indexing:constraint_classification(sicilian_expedition_overshooting_pathology, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: ATHENIAN STRATEGIC CAPACITY (SYSTEMS PERSPECTIVE) — Organized as a constraint on the city-state itself. Athens authorized commitment rate that exceeded inner operational capacity. The assembly (high-bandwidth outer layer) can make decisions faster than logistics (low-bandwidth inner layer) can execute them. Strategic capacity is the victim: the expedition exhausts the navy's repair cycles, depletes the treasury, constrains future operations, and ultimately undermines the city's ability to defend itself in the Peloponnesian War. The constraint is snare for the system because Athens cannot exit — once the expedition is authorized and mobilized, the strategic commitment is binding. High suppression: the system is locked into escalation.
constraint_indexing:constraint_classification(sicilian_expedition_overshooting_pathology, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / OVERSHOOTING PATHOLOGY THESIS (MOUNTAIN CANDIDATE) — From a civilizational timescale, high-bandwidth outer-layer decision-making producing inner-layer operational collapse appears as a recurring natural law of political systems. Democratic assemblies can authorize commitments faster than military logistics can absorb them. Information bandwidth (rhetoric, persuasion, votes) exceeds operational bandwidth (supply chains, training, intelligence). This is a structural property of the human organizations: outer decision rate > inner execution capacity = overshooting. However, the structural data reveals this is a false summit: the bandwidth mismatch is not immutable. Smaller expeditions (lower outer bandwidth) or more restrictive assembly procedures (slower outer bandwidth) or larger standing armies (higher inner capacity) would prevent the pathology. The 'natural law' naturalizes what is a contingent institutional arrangement.
constraint_indexing:constraint_classification(sicilian_expedition_overshooting_pathology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sicilian_expedition_overshooting_pathology_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sicilian_expedition_overshooting_pathology, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sicilian_expedition_overshooting_pathology, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sicilian_expedition_overshooting_pathology, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sicilian_expedition_overshooting_pathology, TR),
    TR >= 0.70.

:- end_tests(sicilian_expedition_overshooting_pathology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-moderate, rising through the interval. Initial extractiveness (0.35) reflects the authorization phase — political capital extracted from elite, resource commitment extracted from treasury, but operational cost not yet realized. As the campaign proceeds and logistical inadequacy becomes evident, extractiveness rises (0.52 at mid-mobilization) as the trapped agents face escalating costs. Final measurement (0.68) reflects the constraint's peak severity as soldiers face impossible supply conditions, command becomes increasingly dysfunctional, and Athens cannot withdraw without massive sunk costs. Suppression (0.72): High. Multiple suppression mechanisms operate simultaneously: (1) procedural — once authorized by assembly vote, individual soldiers cannot refuse (democratic decision binding); (2) social — honor-bound participation prevents mid-campaign withdrawal; (3) logistical — committed supplies and troops create path-dependency; (4) informational — soldiers deployed without accurate knowledge of conditions; (5) strategic — Athenian commitment to allies creates diplomatic lock-in. Theater ratio (0.58): Moderate-high, rising through interval. Initial theater (0.35) reflects genuine strategic planning and logistics coordination in the authorization phase. As operational collapse becomes evident, theater increases (0.45, then 0.58) as command generates reports justifying escalation, maintains morale through ceremony, and performs strategic adjustment without actual capability. The increase indicates that command activity becomes increasingly divorced from operational function — performative leadership substituting for strategic realism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates dramatic perspectival divergence across institutional positions. The political elite experience tangled_rope (they benefit from the decision-making authority and resource allocation; they coordinate a genuine political ambition; but they extract enormous costs from soldiers and treasury). The soldiers experience snare (committed to an operation that cannot succeed; no exit; bearing full cost of command failure). The command structure experiences piton (the institution degrades from operational coordination to performative theater as bandwidth collapse reveals incoherence). The strategic capacity view (systems perspective) experiences snare at a different scale — Athens becomes locked into a commitment that exhausts its capacity to defend itself. The analytical observer at civilizational scale risks seeing this as a natural law (bandwidth mismatch is inherent to human organization) but the structural data reveals this as a false summit: smaller expeditions, slower assembly procedures, or larger standing armies would prevent the pathology. The elite benefit from fast outer-layer decisions (arbitrage exit option) while soldiers and strategy bear the cost (trapped/constrained exit). This asymmetry is the structural root of the snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position within the extraction flow. Political elite: d ≈ 0.15 (beneficiaries with arbitrage exit). They authorized the expedition, profit from its authorization, and can reallocate resources or withdraw if politically feasible. Soldiers: d ≈ 0.92 (victims with trapped exit). Committed to operation by assembly vote; no legal or social exit; bear full cost of command failure. Command structure: d ≈ 0.55 (mixed position with constrained exit). Command benefits from operational authority but faces reputational risk from failure; constrained because public commitment and troop dependencies prevent easy reversal. Strategic capacity: d ≈ 0.88 (victim of systems constraint with mobile but slow exit). Athens can change strategy, but path-dependency and sunk costs make exit costly; the constraint is structural to the decision-making architecture. The derived d values explain why elite and soldiers experience fundamentally different classifications: elite arbitrage (low f(d)) produces rope-like experience; soldier entrapment (high f(d)) produces snare experience. The gap is not perceptual — it is structural.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids overshooting the snare classification by including the tangled_rope perspective (political elite) and clarifying that the snare emerges from a specific structural position (soldiers) rather than characterizing the entire operation as pure extraction. The elite genuinely coordinate a political ambition (imperial expansion, prestige redistribution) alongside extractive resource-taking. The soldiers face pure extraction because they have no coordination benefit — the operation they are committed to cannot succeed. The mandatrophy is resolved by recognizing that the same constraint can be snare to some agents and tangled_rope to others, depending on directionality. The false summit (mountain as natural law) is identified and documented: bandwidth mismatch appears natural (outer decision-rate always exceeds inner capacity) but is contingent on institutional design (smaller expeditions, slower voting procedures, or larger standing armies would prevent it). The resolution shows that the constraint's classification chain depends critically on exit-option and beneficiary/victim differentiation — without these distinctions, the whole operation would be mislabeled as either pure coordination (assembly legitimacy) or pure extraction (soldier entrapment), collapsing the actual complexity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bandwidth_measurement_ambiguity,
    'How do we measure and compare decision-making bandwidth (assembly authorization rate) versus operational bandwidth (logistics execution rate)? Are they comparable on the same scale?',
    'Quantitative analysis of assembly session frequency, decision-making timescale (votes per month), and logistics cycle times (weeks per supply convoy, months per campaign season). Comparison with successful operations (e.g., Marathon campaign) to establish baseline ratios.',
    'If bandwidth mismatch is the primary driver: the constraint is a property of the institutional architecture (reformable). If bandwidth mismatch is secondary to hubris/overconfidence: the constraint is more cognitive (requires different intervention). If bandwidth is immeasurable/incommensurable: the overshooting pathology thesis is conceptual rather than empirical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bandwidth_measurement_ambiguity, empirical, 'Measurability and comparability of decision-making versus operational bandwidth').

omega_variable(
    information_quality_degradation,
    'Was the assembly''s decision driven by incomplete information (bandwidth collapse in information flow) or by deliberate rhetorical manipulation (information suppression)?',
    'Historical analysis of debate transcripts (Thucydides), council deliberations, and scout reports available to Nicias and Alcibiades versus information presented to the assembly. Correlation between information asymmetry and vote outcome.',
    'If information degradation: constraint is a property of command structure and logistics (fixable). If rhetorical manipulation: constraint is a property of individual actors and persuasion dynamics (requires different analysis). If both: the constraint is composite — overshooting enabled by information suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_quality_degradation, empirical, 'Role of information quality versus rhetorical manipulation in assembly decision').

omega_variable(
    exit_mechanism_failure,
    'Could the assembly have voted to cancel or reduce the expedition after mobilization began, or was the decision-lock absolute?',
    'Analysis of Athenian constitutional procedures for withdrawing or modifying assembly decisions; examination of historical precedent (other expeditions recalled, downsized, or redirected); identification of procedural barriers to reversal.',
    'If exit was procedurally available: the suppression score (0.72) is overstated, constraint is weaker. If exit was procedurally blocked: the snare classification is reinforced. If exit was procedurally possible but politically impossible (face-saving, ambition sunk-cost): the constraint includes internalized suppression (identity-lock elements).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_mechanism_failure, empirical, 'Whether assembly could have reversed or modified the expedition decision').

omega_variable(
    false_summit_natural_law_status,
    'Is the bandwidth-mismatch overshooting pathology a genuine immutable law of political organization, or a contingent institutional artifact?',
    'Comparative historical and organizational analysis: examine systems with feedback mechanisms (e.g., quorum requirements, mandatory consultation periods, post-vote review processes) that slow outer-layer decision-making. If such systems successfully prevent overshooting, the ''natural law'' is disproven. If overshooting recurs regardless of institutional design, the pathology is more fundamental.',
    'If contingent: the constraint should be reclassified from mountain to tangled_rope (reformable institutional design). If fundamental: mountain classification stands, but false-summit risk remains high because beneficiaries (political elites) benefit from fast outer-layer decisions. If both: the constraint is a *design choice* (fast decision-making chosen despite overshooting risk) rather than a natural limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_status, conceptual, 'Natural law status of bandwidth-mismatch overshooting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sicilian_expedition_overshooting_pathology, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sicilian_theater_initial, sicilian_expedition_overshooting_pathology, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sicilian_theater_mid, sicilian_expedition_overshooting_pathology, theater_ratio, 3, 0.45).
narrative_ontology:measurement(sicilian_theater_late, sicilian_expedition_overshooting_pathology, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(sicilian_initial_authorization, sicilian_expedition_overshooting_pathology, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sicilian_mid_mobilization, sicilian_expedition_overshooting_pathology, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(sicilian_logistics_collapse, sicilian_expedition_overshooting_pathology, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sicilian_expedition_overshooting_pathology, enforcement_mechanism).
narrative_ontology:affects_constraint(sicilian_expedition_overshooting_pathology, peloponnesian_war_strategic_exhaustion).
narrative_ontology:affects_constraint(sicilian_expedition_overshooting_pathology, athenian_democratic_decision_pathology).
narrative_ontology:affects_constraint(sicilian_expedition_overshooting_pathology, ancient_logistics_bandwidth_constraint).

% DUAL FORMULATION NOTE:
% The Sicilian Expedition represents a specific instantiation of a more general constraint family: high-bandwidth-outer-layer decision-making producing inner-layer operational collapse. The expedition itself (this story) has extractiveness ~0.68 and classifies as snare. The broader pattern (democratic assembly decision-rate exceeding logistics capacity) appears as a mountain candidate in the analytical perspective. Decomposition: the institutional design constraint (assembly bandwidth vs. logistics bandwidth) is the upstream story with lower extractiveness and potentially mountain classification; the Sicilian Expedition is the downstream manifestation with high extractiveness and snare classification. Both should be linked via network.affects_constraints to show how institutional architecture enables specific catastrophic deployments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sicilian_expedition_overshooting_pathology, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
