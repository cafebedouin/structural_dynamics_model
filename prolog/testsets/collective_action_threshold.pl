% ============================================================================
% CONSTRAINT STORY: collective_action_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collective_action_threshold, []).

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
 *   constraint_id: collective_action_threshold
 *   human_readable: Collective Action Threshold for Transforming Distributed Enforcement
 *   domain: social_systems/institutional_dynamics/power_asymmetry
 *
 * SUMMARY:
 *   The collective action threshold represents the minimum coordination
 *   required for trapped actors to transform distributed enforcement
 *   mechanisms from inescapable (Snare) to navigable (Rope). This constraint
 *   is structurally distinct from the upstream
 *   reputation_as_distributed_enforcement constraint: the upstream constraint
 *   models the enforcement mechanism itself (how reputation systems extract
 *   from targets), while this constraint models the threshold dynamics that
 *   determine whether collective organization can transform that enforcement
 *   mechanism. The key structural feature is index transformation: isolated
 *   individuals experience the constraint as powerless/trapped (high π, no
 *   exit), but once the collective action threshold is crossed, the same
 *   individuals experience the constraint as organized/mobile (low π, exit
 *   options available). This transformation is not merely perceptual — it
 *   reflects a real change in the power relationship and the enforcement
 *   mechanism's effectiveness. The constraint exhibits scaffold dynamics
 *   because successful organizing produces a sunset: as the coalition
 *   matures, the extraction (organizing costs, retaliation risk, coordination
 *   overhead) declines while the coordination benefits (collective bargaining
 *   leverage, mutual aid, solidarity infrastructure) persist and strengthen.
 *   The theater_ratio is moderate (0.35) because some organizing activity is
 *   performative (symbolic actions, ritual solidarity displays) but much is
 *   functional (actual coordination, resource pooling, strategic planning).
 *   The constraint's extractiveness declines over the interval (0.52 → 0.38)
 *   as organizing success transforms the power relationship.
 *
 * KEY AGENTS:
 *   - Isolated Individuals: Primary victims (powerless/trapped) — face distributed enforcement with no exit before collective organization; bear maximum extraction
 *   - Early Organizers: Secondary victims (moderate/constrained) — face retaliation risk and coordination costs but gain access to solidarity networks and collective leverage; mixed extraction and coordination
 *   - Organizing Campaign: Temporary coordination mechanism (organized/constrained) — exists to build the coalition; dissolves or transforms once threshold is crossed; scaffold dynamics with sunset
 *   - Established Coalition: Primary beneficiaries (organized/mobile) — experience transformed constraint as coordination; low extraction because collective power enables exit and bargaining leverage
 *   - Enforcement Institutions: Institutional actors (institutional/arbitrage) — benefit from high collective action threshold; perceive threshold as natural law (false summit); have exit options that isolated individuals lack
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both inherent coordination cost and artificially imposed suppression; identifies threshold height as partly extractive
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collective_action_threshold, 0.38).
domain_priors:suppression_score(collective_action_threshold, 0.48).
domain_priors:theater_ratio(collective_action_threshold, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collective_action_threshold, extractiveness, 0.38).
narrative_ontology:constraint_metric(collective_action_threshold, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(collective_action_threshold, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collective_action_threshold, scaffold).
narrative_ontology:human_readable(collective_action_threshold, "Collective Action Threshold for Transforming Distributed Enforcement").
narrative_ontology:topic_domain(collective_action_threshold, "social_systems/institutional_dynamics/power_asymmetry").

narrative_ontology:has_sunset_clause(collective_action_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(collective_action_threshold, organizing_workers).
narrative_ontology:constraint_beneficiary(collective_action_threshold, coalition_members).
narrative_ontology:constraint_beneficiary(collective_action_threshold, solidarity_networks).
narrative_ontology:constraint_victim(collective_action_threshold, isolated_individuals).
narrative_ontology:constraint_victim(collective_action_threshold, pre_organization_targets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED INDIVIDUAL (SNARE) — Before collective organization, the individual faces distributed enforcement mechanisms (reputation systems, social sanctions, economic retaliation) with no exit. Suppression is maximal because the enforcement is distributed across the entire social network. The individual cannot escape the constraint through individual action — any attempt to resist triggers coordinated retaliation from multiple enforcement nodes. High experienced extraction because the individual bears full cost of the enforcement mechanism with no countervailing power.
constraint_indexing:constraint_classification(collective_action_threshold, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: EARLY ORGANIZER (TANGLED ROPE) — Agents who begin organizing face mixed dynamics: the constraint still extracts (organizers face retaliation risk, resource costs, coordination overhead) but also provides genuine coordination benefits (shared information, mutual aid, collective bargaining leverage). Exit is constrained rather than trapped — organizers can leave the organizing effort but at significant personal cost (loss of solidarity network, continued exposure to original enforcement mechanism, reputation damage within the organizing community). The coordination function is real but embedded within ongoing extraction.
constraint_indexing:constraint_classification(collective_action_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ESTABLISHED COALITION (ROPE) — Once the collective action threshold is crossed and the coalition is established, members experience the constraint as coordination: the same distributed enforcement mechanisms that trapped isolated individuals now serve as collective bargaining leverage, mutual protection networks, and solidarity infrastructure. Exit is mobile — members can leave the coalition without catastrophic cost because the coalition's existence has transformed the underlying enforcement landscape. Low experienced extraction because the coalition is a net beneficiary of the transformed constraint.
constraint_indexing:constraint_classification(collective_action_threshold, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZING CAMPAIGN (SCAFFOLD) — The organizing process itself is a temporary coordination mechanism with a sunset clause: the campaign exists to build the coalition, and once the coalition is established (threshold crossed), the campaign structure dissolves or transforms into maintenance infrastructure. The constraint has moderate extraction during the organizing phase (resource costs, coordination overhead, retaliation risk) but this extraction is tolerated because it declines over the time horizon as the coalition matures. The sunset is structural: successful organizing eliminates the need for the organizing campaign by transforming the power relationship.
constraint_indexing:constraint_classification(collective_action_threshold, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ENFORCEMENT INSTITUTION (MOUNTAIN) — Institutions that benefit from distributed enforcement (employers, platform operators, regulatory bodies) perceive the collective action threshold as an immutable feature of social coordination: 'organizing is hard' becomes naturalized as a law of human behavior rather than a contingent outcome of suppression mechanisms. This perspective sees the threshold as unchangeable at immediate time horizons because the institution has arbitrage exit — it can shift enforcement strategies, relocate operations, or restructure to avoid organized resistance. However, this is a false summit: the threshold is not a natural law but a product of institutional design choices (communication barriers, retaliation mechanisms, legal restrictions on organizing).
constraint_indexing:constraint_classification(collective_action_threshold, mountain,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the collective action threshold exhibits both genuine coordination function (solving the free-rider problem, enabling collective bargaining, building solidarity infrastructure) and asymmetric extraction (the threshold itself is artificially elevated by institutional suppression mechanisms that make organizing harder than it needs to be). The analytical observer sees that some coordination cost is inherent to collective action (information aggregation, preference alignment, commitment mechanisms) but much of the observed threshold height is extractive overhead (legal barriers to organizing, employer retaliation, platform design that fragments communication, surveillance that raises organizing costs).
constraint_indexing:constraint_classification(collective_action_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_action_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(collective_action_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collective_action_threshold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(collective_action_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38 at T=6): Moderate. The constraint extracts during the organizing phase (resource costs, retaliation risk, coordination overhead) but extraction declines as the coalition matures. The final value reflects that some coordination cost persists (maintaining the coalition, managing internal conflicts, adapting to institutional counter-strategies) but the extraction is significantly lower than the pre-organization baseline (0.52 at T=0). The declining trajectory is the scaffold signature: temporary extraction tolerated because it produces a sunset. Suppression (0.48): Moderate-high. Significant barriers to collective organization include legal restrictions on organizing (anti-union laws, right-to-work regimes), employer retaliation (firing organizers, surveillance, captive-audience meetings), platform design that fragments communication (algorithmic feed curation, terms-of-service restrictions on organizing), and free-rider dynamics (individuals benefit from collective action without bearing organizing costs). However, suppression is not total — some organizing succeeds, and the threshold can be crossed with sufficient coordination. Theater ratio (0.35): Moderate. Some organizing activity is performative (symbolic strikes, ritual solidarity displays, public demonstrations with limited strategic impact) but much is functional (actual coordination of work stoppages, resource pooling for strike funds, strategic planning for bargaining leverage, mutual aid networks). The theater ratio is lower than many institutional constraints because organizing is resource-constrained — performative activity that doesn't contribute to crossing the threshold is quickly abandoned.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how index transformation produces perspectival gaps that are not merely differences in perception but reflect real structural changes in power relationships. The isolated individual sees Snare (inescapable extraction) because they genuinely have no exit — the enforcement mechanism is distributed across the entire social network and individual resistance triggers coordinated retaliation. The early organizer sees Tangled Rope (mixed coordination and extraction) because organizing provides real benefits (solidarity, collective leverage) but also imposes real costs (retaliation risk, resource expenditure). The established coalition sees Rope (coordination) because the threshold crossing has transformed the enforcement mechanism from a tool of extraction into collective infrastructure. The organizing campaign sees Scaffold (temporary coordination with sunset) because the campaign exists to build the coalition and dissolves once the threshold is crossed. The enforcement institution sees Mountain (immutable natural law) because it has arbitrage exit and perceives the threshold as an unchangeable feature of human behavior — but this is a false summit, naturalizing institutional design choices (legal barriers, retaliation mechanisms) as laws of nature. The analytical observer sees Tangled Rope because the threshold exhibits both genuine coordination function (solving free-rider problems, enabling collective bargaining) and extractive overhead (artificially elevated by institutional suppression). The perspectival gaps are not reconcilable to a single 'true' classification — the presheaf over observation sites IS the structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint demonstrates index transformation mechanics: the same structural enforcement mechanism produces different experienced extraction depending on the agent's power position and exit options. Isolated individuals (powerless/trapped) are full targets of the enforcement mechanism — directionality d ≈ 0.95, producing high effective extraction χ via the sigmoid f(d). Early organizers (moderate/constrained) have intermediate directionality d ≈ 0.65 because they face ongoing extraction (retaliation risk, coordination costs) but also gain coordination benefits (solidarity networks, collective leverage). Established coalition members (organized/mobile) are net beneficiaries — directionality d ≈ 0.45 because the transformed enforcement mechanism now serves collective interests rather than extracting from individuals. Enforcement institutions (institutional/arbitrage) are primary beneficiaries of the high threshold — directionality d ≈ 0.15 because they benefit from fragmented opposition and can exit (relocate, restructure) when organizing succeeds. The analytical observer (analytical/analytical) has directionality d ≈ 0.72, reflecting that the observer sees both the coordination function and the extractive overhead but is not directly subject to either. The key insight: crossing the collective action threshold changes the directionality value for the same individuals — the power position transformation is not merely perceptual but reflects a real structural change in who benefits and who bears costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the classification depends on the observer's structural position and time horizon. The isolated individual's Snare classification is not a misperception — it accurately reflects their structural reality (powerless/trapped with no exit). The established coalition's Rope classification is also not a misperception — it accurately reflects the transformed power relationship (organized/mobile with exit options). The scaffold classification from the organizing campaign perspective captures the temporal dynamics: the constraint is extractive during the organizing phase but produces a sunset as the coalition matures. The analytical observer's Tangled Rope classification identifies that some threshold height is inherent coordination cost (legitimate) while some is artificially imposed suppression (extractive). The enforcement institution's Mountain classification is a false summit — it naturalizes contingent institutional arrangements (legal barriers to organizing, employer retaliation mechanisms) as immutable features of social coordination. The mandatrophy is resolved by recognizing that all these classifications are simultaneously valid from their respective observation sites, and the structural reality is the presheaf that relates them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_height_decomposition,
    'What proportion of the observed collective action threshold is inherent coordination cost versus artificially imposed suppression?',
    'Cross-cultural and cross-institutional comparison of organizing success rates; natural experiments where legal barriers to organizing are removed or imposed; measurement of organizing timeline and resource requirements in high-suppression vs low-suppression environments',
    'If threshold is primarily inherent coordination cost: constraint classifies as Rope from more perspectives (legitimate coordination problem). If threshold is primarily imposed suppression: constraint classifies as Snare from more perspectives (extractive barrier maintained by beneficiaries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_height_decomposition, empirical, 'Decomposition of threshold into inherent vs imposed components').

omega_variable(
    critical_mass_stability,
    'Is the critical mass threshold for successful organizing stable across contexts or does it vary with institutional suppression intensity?',
    'Longitudinal analysis of organizing campaigns across different suppression regimes; identification of threshold size as a function of employer retaliation intensity, legal restrictions, and communication infrastructure; measurement of coalition stability post-threshold',
    'If threshold is stable: suggests inherent coordination dynamics (Rope/Scaffold). If threshold varies with suppression: suggests extractive mechanism (Snare/Tangled Rope) where institutions actively manipulate the threshold to prevent organizing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(critical_mass_stability, empirical, 'Stability of critical mass threshold across suppression regimes').

omega_variable(
    sunset_mechanism_reliability,
    'Does crossing the collective action threshold reliably produce a sunset (declining extraction over time) or do established coalitions face persistent extraction that prevents the scaffold classification?',
    'Tracking of extractiveness and suppression metrics for established coalitions over biographical time horizons; measurement of whether coalition maintenance costs decline, stabilize, or increase post-threshold; identification of institutional counter-organizing strategies that sustain extraction',
    'If extraction reliably declines post-threshold: scaffold classification confirmed. If extraction persists or increases: reclassify as Tangled Rope (ongoing hybrid) or identify institutional counter-mechanisms that prevent the sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_mechanism_reliability, empirical, 'Reliability of sunset mechanism post-threshold').

omega_variable(
    index_transformation_reversibility,
    'Is the power position transformation (powerless → organized) reversible, and if so, what mechanisms enable or prevent reversal?',
    'Case studies of coalition collapse or institutional counter-organizing; measurement of whether individuals who participated in failed organizing attempts return to powerless/trapped status or retain some organizational capacity; identification of institutional strategies for fragmenting established coalitions',
    'If transformation is irreversible: supports scaffold classification (one-way transition with sunset). If transformation is reversible: suggests Tangled Rope or Snare (extraction persists through institutional counter-mechanisms that can reset the power relationship).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(index_transformation_reversibility, empirical, 'Reversibility of power position transformation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collective_action_threshold, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cat_theater_t0, collective_action_threshold, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cat_theater_t3, collective_action_threshold, theater_ratio, 3, 0.3).
narrative_ontology:measurement(cat_theater_t6, collective_action_threshold, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(cat_extract_t0, collective_action_threshold, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(cat_extract_t3, collective_action_threshold, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(cat_extract_t6, collective_action_threshold, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collective_action_threshold, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of reputation_as_distributed_enforcement. The upstream constraint models the enforcement mechanism itself (how reputation systems extract from targets). This constraint models the threshold dynamics that determine whether collective organization can transform that enforcement mechanism. The two constraints have different ε values because they measure different structural phenomena: the upstream constraint measures the extractiveness of the reputation system; this constraint measures the extractiveness of the organizing process required to transform that system. The constraints are linked because the upstream enforcement mechanism creates the suppression that elevates the collective action threshold, but they are structurally distinct and require separate stories per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
