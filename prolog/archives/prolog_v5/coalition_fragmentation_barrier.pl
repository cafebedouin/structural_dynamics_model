% ============================================================================
% CONSTRAINT STORY: coalition_fragmentation_barrier
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coalition_fragmentation_barrier, []).

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
 *   constraint_id: coalition_fragmentation_barrier
 *   human_readable: Coalition Fragmentation Barrier
 *   domain: political_economy/collective_action
 *
 * SUMMARY:
 *   Coalition fragmentation barriers are structural constraints that prevent
 *   member exit from collective political movements and organizations. The
 *   constraint arises from the inherent tension between the coordination
 *   function that coalitions provide (enabling collective action against
 *   powerful adversaries through scale and unity) and the extraction
 *   mechanisms embedded in coalition governance (asymmetric distribution of
 *   power, resources, and credit for victories). Members experience
 *   simultaneous benefits (collective identity, shared cause, amplified
 *   voice) and costs (loss of individual agency, labor extraction, unequal
 *   benefit distribution). The barrier to fragmentation is enforced through
 *   multiple mechanisms: social pressure and ostracism for defectors, sunk
 *   cost psychology, identity fusion with the collective project, and the
 *   genuine reduction in bargaining power that fragmented groups experience.
 *   The constraint exhibits high suppression (0.65) because exit carries
 *   severe perceived costs, and meaningful alternatives to coalition
 *   membership are structurally limited in asymmetric power environments. The
 *   theater ratio (0.48) indicates moderate performative content — coalition
 *   unity symbolism masks internal conflicts, but the coordination function
 *   is substantially real rather than purely theatrical.
 *
 * KEY AGENTS:
 *   - Ordinary Coalition Members: Primary victims (powerless/trapped) — individual members have structural exit barriers; suppression operates through social ties, identity fusion, and belief in necessity of unity.
 *   - Organized Factions: Secondary participants (moderate/constrained) — mid-level organizations within coalitions negotiate between staying (for scale) and leaving (for autonomy). Face real costs to exit but have genuine alternatives.
 *   - Coalition Leadership: Primary beneficiary (institutional/arbitrage) — controls decision-making, resource allocation, and public representation. Experiences the constraint as coordination mechanism that concentrates bargaining power. Can exit without extreme cost.
 *   - External Observer Coalitions: Structural allies (organized/mobile) — peer coalitions and solidarity networks that interact with the focal coalition. See fragmentation as natural and provisional rather than catastrophic.
 *   - Coalition Apparatus: Institutional beneficiary (institutional/constrained) — formal bureaucratic structures, staffing, funding mechanisms that maintain the coalition's existence. Staff and administrators depend on the apparatus continuing.
 *   - Analytical Observer: System perspective (analytical/analytical) — sees coalitions as solving genuine collective action problems while creating extraction structures. The constraint serves essential coordination functions while extracting from membership.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coalition_fragmentation_barrier, 0.58).
domain_priors:suppression_score(coalition_fragmentation_barrier, 0.65).
domain_priors:theater_ratio(coalition_fragmentation_barrier, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coalition_fragmentation_barrier, extractiveness, 0.58).
narrative_ontology:constraint_metric(coalition_fragmentation_barrier, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(coalition_fragmentation_barrier, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coalition_fragmentation_barrier, tangled_rope).
narrative_ontology:human_readable(coalition_fragmentation_barrier, "Coalition Fragmentation Barrier").
narrative_ontology:topic_domain(coalition_fragmentation_barrier, "political_economy/collective_action").

domain_priors:requires_active_enforcement(coalition_fragmentation_barrier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coalition_fragmentation_barrier, coalition_leadership).
narrative_ontology:constraint_beneficiary(coalition_fragmentation_barrier, primary_faction).
narrative_ontology:constraint_victim(coalition_fragmentation_barrier, coalition_member_base).
narrative_ontology:constraint_victim(coalition_fragmentation_barrier, coalition_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORDINARY COALITION MEMBER (SNARE) — Individual members have no exit mechanism. Leaving the coalition means abandoning shared cause, social ties, and collective identity. Yet the coalition structure increasingly extracts loyalty and labor without delivering proportional benefits. Suppression is high: social ostracism for defection, sunk costs in coalition activities, and the belief that collective action requires unity even in the face of asymmetric distribution.
constraint_indexing:constraint_classification(coalition_fragmentation_barrier, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ORGANIZED FACTION (TANGLED ROPE) — Mid-level organized subgroups within the coalition experience both coordination and extraction. The coalition framework enables collective action against external adversaries, but the internal distribution of decision-making power, resource allocation, and credit for victories is asymmetric. Exit carries costs: loss of scale, reduced bargaining power, and reputational damage from fragmentation, but alternatives exist and some factions do splinter.
constraint_indexing:constraint_classification(coalition_fragmentation_barrier, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COALITION LEADERSHIP (ROPE) — Leadership experiences the coalition as a coordination mechanism that solves collective action problems and concentrates bargaining power. They experience extraction as minimal — the internal organizational hierarchy is framed as necessary for efficacy. Leadership can exit the coalition without extreme cost (move to another political movement, return to private sector, shift to different organizing), so exit_options are arbitrage. The coordination function is genuine: leadership genuinely solves the free-rider problem.
constraint_indexing:constraint_classification(coalition_fragmentation_barrier, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EXTERNAL OBSERVER COALITION (SCAFFOLD) — Allied external organizations (peer coalitions, international solidarity networks, meta-NGOs) perceive the coalition as a temporary coordination structure with an implicit sunset. They expect coalitions to fragment and reform around emergent issues, and they work within that assumption. The coalition is useful for THIS campaign or moment, but all parties know it will transform or dissolve. Low theater: the coordination function is transparent and acknowledged to be provisional.
constraint_indexing:constraint_classification(coalition_fragmentation_barrier, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL COALITION APPARATUS (PITON) — Formal coalition structures (bureaucratic processes, standing committees, funding mechanisms) persist long after the issues that justified them have shifted. The apparatus is maintained through institutional inertia — job preservation, constituent expectations, formalized protest rituals. High theater: the apparatus performs coalition unity symbolically while actual coordination happens through informal networks. The formal structures are largely decorative.
constraint_indexing:constraint_classification(coalition_fragmentation_barrier, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, coalitions solve a genuine coordination problem (enabling collective action against powerful adversaries) while simultaneously creating an extraction structure (those who control the coalition apparatus extract power, status, and resources from the membership). Both functions are real. The constraint is neither pure coordination nor pure extraction but a hybrid that requires active enforcement to maintain. The barrier to coalition fragmentation is structural: it prevents both harmful dissolution AND protective exit.
constraint_indexing:constraint_classification(coalition_fragmentation_barrier, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coalition_fragmentation_barrier_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coalition_fragmentation_barrier, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coalition_fragmentation_barrier, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coalition_fragmentation_barrier, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(coalition_fragmentation_barrier, TR),
    TR >= 0.70.

:- end_tests(coalition_fragmentation_barrier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Leadership systematically captures benefits disproportionate to contribution (media attention, funding, political influence, career advancement). The measurement trajectory shows extractiveness increasing over time as leadership consolidation deepens and internal distribution becomes more unequal. At the initial state (0.35), the coalition was perceived as more egalitarian and extraction was lower. By measurement point 3 (0.45), leadership consolidation was visible. By point 6 (0.58), extraction had become structural feature. The value reflects that extraction is real and significant but not absolute — most members still benefit from collective scale and identity. Suppression (0.65): High. Barriers to exit are multifaceted: social ostracism and reputation damage for defectors, deep identity fusion (members have internalized coalition identity as core self-concept), sunk costs in coalition activities and relationships, and rational perception that fragmentation reduces collective power. However, suppression is not total (some members do exit or reduce participation), suggesting that while barriers are severe, they are not insurmountable for all agents. Theater ratio (0.48): Moderate-low. Coalition activities have real coordination content (actual collective action, real threats met through collective power, genuine resource pooling) but also significant performative content (unity rituals, symbolic demonstrations, internal loyalty performances that maintain cohesion). The balance tips slightly toward real function rather than theater, but the gap is narrowing as internal contradictions force more internal consensus performances.
 *
 * PERSPECTIVAL GAP:
 *   The largest gap is between the powerless member (Snare: d=0.92, χ ≈ high) and the institutional leadership (Rope: d=0.08, χ ≈ low/negative). Both parties operate within the same structural constraint (coalition membership and coordination requirements), but their experienced extractiveness differs radically because their positions in the extraction flow are opposite. The member experiences the constraint as a net cost (snare); leadership experiences it as net benefit (rope). This is the diagnostic signature of Tangled Rope from the analytical perspective — genuine coordination function (members do gain collective power) plus asymmetric extraction (leadership captures disproportionate benefits). If the constraint were pure extraction (Snare from all perspectives), all agents would experience high χ. If it were pure coordination (Rope from all perspectives), all would experience low χ. The perspectival gap proves the hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position relative to extraction flow. Ordinary members (powerless/trapped) have d ≈ 0.92 — they are victims bearing extraction with no exit option. The sigmoid f(d) produces high effective extractiveness (χ) from their perspective. Leadership (institutional/arbitrage) has d ≈ 0.08 — they are beneficiaries with full exit option, so f(d) is negative, producing low or negative χ. Organized factions (moderate/constrained) have d ≈ 0.58 — middle position, experiencing both extraction and benefits, with constrained (not trapped) exit. The analytical observer (analytical/analytical) has canonical d ≈ 0.73, representing neutral measurement position. Directionality overrides are not needed because the structural data (beneficiary/victim declarations + exit options + power levels) correctly captures the differentiation. Leadership benefits and has exit (arbitrage); members suffer and lack exit (trapped); factions are mixed with constrained exit. The derivation chain produces appropriate d values automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   The coalition fragmentation barrier avoids the mandatrophy by acknowledging that coalitions genuinely solve coordination problems (collective action against powerful adversaries is more effective at larger scale) while also genuinely extracting (leadership concentrates power and benefits disproportionately). The constraint cannot be classified as pure Rope because extraction is real and asymmetric. It cannot be classified as pure Snare because coordination benefits are real — members are not simply victims of coercion, they are participants in a structure that provides genuine collective power. The Tangled Rope classification (from the analytical perspective) and the differentiated lower-level perspectives (member sees Snare, leadership sees Rope, faction sees Tangled Rope) together resolve the mandatrophy: the constraint is legitimately hybrid, not mislabeled, and the classification accurately captures that it serves both functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intra_coalition_power_distribution,
    'Is the asymmetry in resource distribution and decision-making authority within the coalition a necessary feature of collective efficacy or a contingent distribution that could be restructured?',
    'Comparative analysis of coalition outcomes under different governance structures (rotating leadership, consensus-based decision-making, transparent fund allocation); historical case studies of coalitions with egalitarian vs hierarchical structures.',
    'If necessary: the constraint cannot be resolved without sacrificing coalition effectiveness (defensive fragmentation is required). If contingent: the extraction is unnecessary and the constraint can transition to pure Rope through structural reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intra_coalition_power_distribution, empirical, 'Whether power asymmetry in coalitions is structurally necessary or contingent').

omega_variable(
    member_exit_mechanisms,
    'What objective or perceived barriers prevent coalition members from exiting without severe penalties (social ostracism, identity loss, collective power reduction)?',
    'Exit cost quantification (reputation damage, social network loss, economic impact); comparison with factions that successfully split without devastating reputational collapse; analysis of member beliefs about exit costs vs actual costs.',
    'If barriers are primarily psychological (members believe exit is catastrophic but could survive it): reclassify as identity_locked rather than trapped, suggest reframing interventions. If barriers are material: the snare classification stands and exit requires material cost reduction (alternative organizing infrastructure, reputation protection mechanisms).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(member_exit_mechanisms, empirical, 'Objective vs perceived barriers to member exit from coalition').

omega_variable(
    coalition_fragmentation_timing,
    'Do coalitions fragment due to internal contradictions accumulating past a threshold, or due to strategic manipulation by external adversaries who have learned to exploit internal fissures?',
    'Timeline analysis of fragmentation events; correspondence with internal contradiction escalation vs external pressure campaigns; member testimony on fragmentation triggers.',
    'If internal contradictions: extractiveness is intrinsic to the coalition form and suppression arises from genuine pressure (Tangled Rope, Snare). If external manipulation: suppression is externally imposed and some perspectives might be more accurately classified as constrained-by-adversary rather than trapped-by-coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_fragmentation_timing, empirical, 'Whether coalition fragmentation is internally or externally driven').

omega_variable(
    leadership_capture_mechanism,
    'Does coalition leadership capture emerge from rational delegation (members rationally grant leadership authority) or from organizational lock-in (leadership positions become self-perpetuating regardless of member preference)?',
    'Analysis of leadership succession patterns; member satisfaction surveys tracking attitudes toward leadership over time; comparison of leadership vision/strategy with stated member priorities.',
    'If rational delegation: leadership extraction is a negotiated trade-off for collective efficacy (tension within Tangled Rope). If lock-in: leadership extraction is sustained through organizational inertia and propaganda (closer to Snare dynamics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leadership_capture_mechanism, empirical, 'Whether coalition leadership capture is rational delegation or organizational lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coalition_fragmentation_barrier, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cfb_tr_t0, coalition_fragmentation_barrier, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cfb_tr_t3, coalition_fragmentation_barrier, theater_ratio, 3, 0.35).
narrative_ontology:measurement(cfb_tr_t6, coalition_fragmentation_barrier, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(cfb_be_t0, coalition_fragmentation_barrier, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cfb_be_t3, coalition_fragmentation_barrier, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(cfb_be_t6, coalition_fragmentation_barrier, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coalition_fragmentation_barrier, enforcement_mechanism).
narrative_ontology:affects_constraint(coalition_fragmentation_barrier, collective_action_free_rider_problem).
narrative_ontology:affects_constraint(coalition_fragmentation_barrier, leadership_accountability_mechanism).
narrative_ontology:affects_constraint(coalition_fragmentation_barrier, coalition_member_burnout).

% DUAL FORMULATION NOTE:
% Coalition fragmentation barrier is downstream of the free-rider problem (which it partially solves through coercive unity enforcement) and upstream of leadership accountability mechanisms (which attempt to constrain leadership extraction). The constraint family includes: collective_action_free_rider_problem (ε=0.15, Rope) — the pure coordination problem coalitions solve; coalition_fragmentation_barrier (ε=0.58, Tangled Rope) — the enforcement mechanism that maintains unity at extraction cost; leadership_accountability_mechanism (ε=0.42, Scaffold) — attempts to constrain leadership extraction with enforcement that should sunset as internal structures mature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
