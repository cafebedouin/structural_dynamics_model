% ============================================================================
% CONSTRAINT STORY: motivated_reasoning_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_motivated_reasoning_suppression, []).

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
 *   constraint_id: motivated_reasoning_suppression
 *   human_readable: Motivated Reasoning Suppression of Uncomfortable Truths
 *   domain: cognitive/epistemic
 *
 * SUMMARY:
 *   Motivated reasoning suppression represents a structural constraint on
 *   epistemic accuracy that operates at multiple scales simultaneously:
 *   individual cognition, group dynamics, institutional narrative
 *   maintenance, and civilizational knowledge production. The constraint
 *   extracts through a mechanism of identity-protective suppression: agents
 *   suppress awareness of truths that would invalidate core identity
 *   commitments or group membership, bearing costs in reality-tracking
 *   capacity, internal cognitive dissonance, and reduced collective
 *   intelligence. The extractiveness has increased over the measured interval
 *   (0.35 → 0.58) as institutional amplification of motivated reasoning has
 *   intensified through information architecture that creates filter bubbles,
 *   algorithmic content curation, and identity-polarized media ecosystems.
 *   The theater ratio (0.65) reflects that significant institutional and
 *   cognitive effort is devoted to performing objectivity and rationality
 *   while suppression mechanisms operate beneath the performative layer. This
 *   constraint exhibits all six DR types from different structural positions,
 *   making it diagnostic of how indexical classification captures different
 *   experienced realities of the same phenomenon.
 *
 * KEY AGENTS:
 *   - Reality-Tracking Agents: Primary victims (powerless/identity_locked) — face suppression through identity-protective biases that make confronting uncomfortable truths psychologically costly
 *   - Identity-Preserving Narratives: Primary beneficiary (institutional/arbitrage) — organizations, movements, ideologies benefit from suppression that maintains group cohesion and narrative integrity
 *   - Institutional Narrative Keepers: Secondary beneficiary (institutional/arbitrage) — organizations maintain suppression mechanisms through incentive structures that reward narrative conformity
 *   - Caught-In-The-Middle Members: Secondary victim (moderate/constrained) — experience simultaneous coordination (group belonging) and extraction (cognitive conformity costs)
 *   - Meta-Cognitive Observers: Institutional actor (analytical/analytical) — capable of naming suppression mechanisms but unable to escape them, instantiating the oracle gap
 *   - Truth-Seeking Institutions: Organized agents (organized/constrained) — science, journalism, reality-testing communities experience suppression as engineerable rather than inevitable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(motivated_reasoning_suppression, 0.58).
domain_priors:suppression_score(motivated_reasoning_suppression, 0.72).
domain_priors:theater_ratio(motivated_reasoning_suppression, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(motivated_reasoning_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(motivated_reasoning_suppression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(motivated_reasoning_suppression, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(motivated_reasoning_suppression, snare).
narrative_ontology:human_readable(motivated_reasoning_suppression, "Motivated Reasoning Suppression of Uncomfortable Truths").
narrative_ontology:topic_domain(motivated_reasoning_suppression, "cognitive/epistemic").

domain_priors:requires_active_enforcement(motivated_reasoning_suppression).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(motivated_reasoning_suppression, identity_preserving_narratives).
narrative_ontology:constraint_beneficiary(motivated_reasoning_suppression, comfortable_existing_beliefs).
narrative_ontology:constraint_victim(motivated_reasoning_suppression, epistemic_accuracy).
narrative_ontology:constraint_victim(motivated_reasoning_suppression, reality_tracking_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REALITY-TRACKING AGENT (SNARE) — An agent whose identity is constituted through commitment to a belief system, ideology, or self-concept faces extraction through motivated reasoning that suppresses contradictory evidence. The binding is cognitive rather than external: the agent is structurally mobile (could encounter new information, could change views) but identity-locked into a frame that makes seeing the suppressed truth unthinkable. The snare extracts through forced internal conflict — facing evidence that would invalidate identity costs psychological disruption and group belonging simultaneously. No external barrier prevents exit; the internal frame makes exit unimaginable.
constraint_indexing:constraint_classification(motivated_reasoning_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(universal))).

% PERSPECTIVE 2: INSTITUTIONAL NARRATIVE KEEPER (ROPE) — Organizations, movements, and identity communities benefit from motivated reasoning that suppresses inconvenient truths. The constraint functions as coordination: members coordinate around shared narratives, establishing group boundaries and shared meaning. From the beneficiary's perspective, the suppression is protective — it keeps the community cohesive and shields members from demoralizing facts. The institutional actor has high mobility (can update narratives, can exit the suppression mechanism) but experiences it as essential coordination. Effective extraction is low because the beneficiary perceives genuine coordination value.
constraint_indexing:constraint_classification(motivated_reasoning_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: CAUGHT-IN-THE-MIDDLE MEMBER (TANGLED ROPE) — An agent at moderate power who benefits from group membership (access to narratives, social belonging, epistemic authority within the group) but also bears the cost of suppression (internal cognitive dissonance when encountering suppressed evidence, career or social penalties for raising contradictions, reduced reality-tracking capacity). This agent experiences the constraint as both coordination and extraction simultaneously. Exit is costly (losing group membership, reputational damage) but possible. Suppression is maintained through mixed mechanisms: genuine group cohesion AND asymmetric extraction of cognitive conformity.
constraint_indexing:constraint_classification(motivated_reasoning_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: META-COGNITIVE OBSERVER (PITON) — An agent capable of recognizing their own motivated reasoning and identity locks while remaining unable to fully escape them. This represents institutional inertia at the cognitive level: the agent can name the suppression mechanism (theater_ratio = 0.65 indicates performative cognition — the agent performs dispassionate rationality while suppressing awareness of their own suppression). The analytical perspective instantiates the oracle gap (Theorem 4): meta-cognitive awareness does not equal freedom from the frame. The piton classification reflects that the analytical observer's own instruments cannot detect the structure preventing them from seeing. This perspective is itself identity-locked, demonstrating that the framework's own position is captured.
constraint_indexing:constraint_classification(motivated_reasoning_suppression, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: CIVILIZATIONAL NATURAL LAW (MOUNTAIN) — From a sufficiently abstract position, motivated reasoning may appear as an irreducible feature of bounded rationality: agents with limited cognitive resources inevitably filter information through identity-protective heuristics. The suppression of uncomfortable truths becomes a law of cognition analogous to physical constraints. However, this perspective risks naturalizing what is structurally a snare: confusing the universal presence of cognitive biases (an immutable feature) with the particular institutional amplification of those biases (a contingent extractive mechanism). The mountain classification is almost certainly false — a false summit that mistakes the invariant human capacity for motivated reasoning with the structural weaponization of that capacity through institutional suppression.
constraint_indexing:constraint_classification(motivated_reasoning_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: TRUTH-SEEKING INSTITUTION (SCAFFOLD) — Science, journalism, fact-checking organizations, and reality-testing communities are organized agents experiencing motivated reasoning suppression as a temporary challenge with a sunset clause. These institutions are explicitly designed to counteract motivated reasoning through adversarial testing, publication bias correction, and incentive alignment with accuracy. From this perspective, the suppression mechanism is not inevitable — it can be engineered away through institutional design (pre-registration, open data, replication incentives, blind review). The scaffold classification reflects that the constraint persists despite clear exit pathways and dedicated organizational effort to build alternatives. Theater ratio is moderate because institutions publicly perform commitment to accuracy while suppression mechanics persist beneath the performative layer.
constraint_indexing:constraint_classification(motivated_reasoning_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(motivated_reasoning_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(motivated_reasoning_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(motivated_reasoning_suppression, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(motivated_reasoning_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(motivated_reasoning_suppression, TR),
    TR >= 0.70.

:- end_tests(motivated_reasoning_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, increasing over interval. The constraint extracts through cognitive conformity costs and reality-tracking reduction. The trajectory shows institutional amplification: motivated reasoning is a universal feature of bounded cognition, but institutional architectures (social media algorithmic curation, identity-polarized media, incentive structures rewarding narrative loyalty) have weaponized it. Initial extractiveness (0.35) reflects baseline motivated reasoning; final extractiveness (0.58) reflects institutional amplification. Suppression (0.72): High. Barriers to escaping the suppression include identity fusion (exiting requires becoming a different person), group membership costs (raising contradictions threatens belonging), institutional punishment (professional penalties for challenging narratives), and epistemic isolation (suppression mechanisms prevent exposure to unsuppressed information). These barriers are cognitive/social rather than purely material, enabling identity_locked classification. Theater ratio (0.65): Moderate-high and increasing. Institutional and individual performance of objectivity is widespread while suppression mechanics operate beneath the surface. Agents engage in performative rationality while suppressing awareness of their suppression — the performance is partly sincere (genuine commitment to reasoning) and partly theatrical (unconscious suppression of suppression awareness).
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival divergence occurs between snare (reality-tracking agent) and rope (institutional narrative keeper) perspectives. The agent bearing extraction (powerless/trapped via identity_lock) sees a snare; the agent benefiting from extraction (institutional/arbitrage) sees a rope with genuine coordination function. This gap reveals that the constraint's primary mechanism is identity-protective suppression: what the beneficiary experiences as group cohesion, the victim experiences as forced cognitive conformity. The scaffold perspective introduces a crucial analytical move — it reframes suppression as engineerable rather than inevitable, identifying institutional design solutions (pre-registration, open data, adversarial testing) that could create exits for identity-locked agents. The piton perspective reveals that meta-cognitive awareness of suppression does not enable escape — the analytical observer can name the mechanism while remaining bound by it, instantiating the oracle gap (Theorem 4). The false mountain perspective (civilizational natural law) is the most dangerous: it naturalizes contingent institutional suppression as inherent to cognition, collapsing the distinctions between universal human bias and institutional weaponization of that bias. The perspectival structure itself becomes diagnostic: when powerless agents see snare and institutions see rope from identical base properties, the gap reveals that the 'coordination' is actually extraction dressed in coordination language.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to the extraction flow. Reality-tracking agents are positioned as victims — they bear suppression costs while institutions benefit from narrative control. Their identity_locked exit (unable to escape without becoming different persons) yields high d (≈0.89), amplifying experienced extraction through the sigmoid f(d). Institutional narrative keepers are positioned as beneficiaries with arbitrage exits — they can update narratives or abandon suppression strategies if needed, yielding low d (≈0.15), producing negative or low χ (extraction flows toward them). The caught-in-the-middle member experiences asymmetric position: structurally mobile (could leave the group) but socially constrained by belonging needs. Their d (≈0.55) reflects mixed position. The engine applies scope multiplier σ(S): universal scope (σ=1.0) means no amplification; local scope (σ=0.8) dampens extraction detection. Motivated reasoning suppression is a universal phenomenon (all scales show similar mechanisms), so σ(universal) = 1.0 is appropriate. The combination ε=0.58, f(d=0.89)≈1.28, σ(universal)=1.0 yields χ≈0.74 for the powerless agent — high effective extraction consistent with snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   PARTIAL RESOLUTION: The mandatrophy is partially resolved through perspectival decomposition but reveals deep structural ambiguity. The snare classification (from powerless agent perspective) is correct for agents whose identity is constituted through suppressed beliefs. The rope classification (from beneficiary perspective) is correct for institutions whose coordination function is genuine, even if extraction is embedded. The tangled rope classification (from moderate perspective) correctly identifies mixed coordination-extraction. The scaffold classification (from organized perspective) is correct IF institutional design solutions actually work — if they don't, scaffold becomes piton (performative commitment to accuracy without functional improvement). The piton classification (from meta-cognitive observer) reveals the oracle gap paradox: awareness of suppression does not enable escape. The false mountain classification (civilizational view) is the key diagnostic failure: if one collapses the institutional amplification with the baseline human capacity for motivated reasoning, the constraint appears inevitable rather than engineerable. The mandatrophy is resolved by maintaining the perspectival structure: the constraint IS all six types simultaneously, depending on agent position and structural capacity to exit. The dominant classification from the primary victim's perspective is snare. The empirical test: does removing institutional suppression architecture (algorithmic curation, identity-polarized media, incentive structures) measurably reduce suppression? If yes, the constraint is engineerable and scaffold is correct. If no, suppression persists at baseline and mountain is closer to correct. Current corpus assumption: suppression is partially engineerable (scaffold is real but incomplete), making tangled rope the most accurate ground-truth classification, with snare and rope as legitimate perspectival specializations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_fusion_mechanism,
    'Is the suppression mechanism primarily identity-fusion (the agent''s self-concept is constituted through the belief) or primarily group-conformity (the agent suppresses truth to maintain group belonging)?',
    'Longitudinal observation of isolated vs socially-embedded agents holding the same belief; comparison of suppression intensity when group pressure is removed vs when identity stakes are maximized',
    'If identity-fusion dominant: classification remains snare with identity_locked exit. If group-conformity dominant: classification may shift toward tangled_rope where agent can exit by changing social context. If both equally: snare classification confirmed but with dual binding mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_mechanism, empirical, 'Whether suppression is identity-fusion or group-conformity driven').

omega_variable(
    extractiveness_attribution,
    'Is the measured extractiveness (0.58) a property of the motivated reasoning mechanism itself or a property of institutional architecture that weaponizes motivated reasoning?',
    'Comparison of motivated reasoning suppression in contexts with different institutional incentive structures; testing whether the same cognitive bias produces extraction in non-institutional contexts vs institutional ones',
    'If mechanism-intrinsic: suppression is unavoidable and close to natural law (mountain). If architecture-dependent: suppression is contingent and engineerable (scaffold). Current taxonomy assumes architecture-dependent — the high extractiveness reflects institutional amplification, not cognitive inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_attribution, empirical, 'Whether extractiveness is intrinsic to motivated reasoning or amplified by institutional context').

omega_variable(
    suppression_internalization_depth,
    'At what point does externally-imposed narrative control become internalized self-suppression? Can the suppression persist after removal of institutional incentives?',
    'Post-exit behavioral tracking: measurement of suppression intensity and identity-lock persistence in agents who have left identity-constituting communities; comparison with agents still embedded',
    'If suppression is primarily internalized: exit from institutional context does not resolve the constraint. If primarily externally-maintained: exit enables rapid recovery of reality-tracking capacity. Current assumption: mixed, with internalization building over biographical timescales.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_depth, empirical, 'Internalization depth of identity-protective suppression mechanisms').

omega_variable(
    collective_action_boundary,
    'Can powerless agents (identity_locked individuals) achieve collective action to resist motivated reasoning suppression? What threshold of mutual recognition is required?',
    'Historical analysis of movements challenging established narratives; identification of conditions under which identity-locked agents organize despite high intra-group suppression',
    'If collective action is possible: dynamic coalition extension could reclassify powerless agents to organized, shifting snare toward tangled_rope from multiple perspectives. If collective action is impossible: snare classification is stable and institutionally enforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_action_boundary, empirical, 'Whether identity-locked agents can achieve collective resistance to suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(motivated_reasoning_suppression, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mrs_tr_t0, motivated_reasoning_suppression, theater_ratio, 0, 0.48).
narrative_ontology:measurement(mrs_tr_t5, motivated_reasoning_suppression, theater_ratio, 5, 0.58).
narrative_ontology:measurement(mrs_tr_t10, motivated_reasoning_suppression, theater_ratio, 10, 0.65).
narrative_ontology:measurement(mrs_tr_t15, motivated_reasoning_suppression, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(mrs_be_t0, motivated_reasoning_suppression, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mrs_be_t5, motivated_reasoning_suppression, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(mrs_be_t10, motivated_reasoning_suppression, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(mrs_be_t15, motivated_reasoning_suppression, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(motivated_reasoning_suppression, identity_coordination).
narrative_ontology:boltzmann_floor_override(motivated_reasoning_suppression, 0.12).
narrative_ontology:affects_constraint(motivated_reasoning_suppression, narrative_capture_institutional).
narrative_ontology:affects_constraint(motivated_reasoning_suppression, epistemic_closure_group_dynamics).
narrative_ontology:affects_constraint(motivated_reasoning_suppression, belief_perseverance_confirmation_bias).

% DUAL FORMULATION NOTE:
% Motivated reasoning suppression decomposes into three structurally distinct constraints: (1) individual cognitive bias (baseline motivated reasoning, ε≈0.20, rope-like) (2) group-amplified suppression (intra-group conformity enforcement, ε≈0.50, tangled_rope) (3) institutional weaponization (algorithmic curation, identity-polarized media, ε≈0.65, snare). This story models the institutional amplification. The upstream story (individual bias) is lower-extraction; the downstream stories (specific institutional implementations of suppression) are higher-extraction and more tractable to engineering solutions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(motivated_reasoning_suppression, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
