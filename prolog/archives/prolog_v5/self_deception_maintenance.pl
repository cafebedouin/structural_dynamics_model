% ============================================================================
% CONSTRAINT STORY: self_deception_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_self_deception_maintenance, []).

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
 *   constraint_id: self_deception_maintenance
 *   human_readable: Self-Deception Maintenance Constraint
 *   domain: psychology/cognition/interpersonal
 *
 * SUMMARY:
 *   Self-deception maintenance is a constraint in which an agent continuously
 *   suppresses awareness of contradiction, usually to protect a fragile
 *   identity or to avoid unbearable affect. The constraint operates through
 *   the agent's own cognitive system — suppression is internally enforced
 *   rather than externally imposed, making it structurally similar to
 *   addiction or trauma response. The deceived agent experiences this as
 *   identity lock (they cannot imagine themselves outside the false
 *   narrative) combined with high suppression (they must actively avoid
 *   reality-testing and manage cognitive dissonance). From the anxiety
 *   avoidance system's perspective, it is pure coordination — a protective
 *   mechanism solving the problem of intolerable affect. From the relational
 *   network's perspective, it is mixed coordination and extraction — the
 *   deception provides behavioral stability but extracts authenticity. The
 *   constraint exhibits all markers of a snare: high base extractiveness
 *   (0.58), high suppression (0.72), low exit capacity (identity_locked),
 *   minimal coordination function (theater_ratio 0.68), and the absence of
 *   structural benefits that would justify the costs. The measurements show
 *   degradation over time: theater_ratio increases from 0.42 to 0.68 as
 *   reality-testing avoidance becomes more energetically expensive, and
 *   base_extractiveness rises from 0.38 to 0.58 as the cognitive overhead of
 *   maintaining the deception accumulates. This pattern is characteristic of
 *   inert constraints: the system that once served protective function
 *   becomes increasingly theatrical and extractive as reality-disconfirming
 *   evidence accumulates.
 *
 * KEY AGENTS:
 *   - Deceived Agent: Primary victim (powerless/identity_locked) — bears the full cost of suppressed reality-testing and identity inflexibility; cannot exit because their identity is constituted through the false narrative
 *   - Protected Identity: Primary beneficiary (institutional/arbitrage) — the self-concept that the deception serves to maintain; experiences the constraint as coordination because it depends on the maintained narrative
 *   - Anxiety Avoidance System: Secondary beneficiary (institutional/arbitrage) — the psychological system that maintains the deception for anxiety regulation; experiences it as pure protection
 *   - Relational Network: Secondary victim/beneficiary (moderate/constrained) — benefits from behavioral stability and absence of confrontation but bears the cost of relational inauthenticity and unaddressed problems
 *   - Intergenerational Pattern: Extended victim (powerless/trapped) — when deception maintenance involves family or cultural narratives, children inherit the constraint without choice
 *   - Analytical Observer: Observers (analytical/analytical) — therapists, researchers, close others who see the deception functioning as degraded institution (piton) rather than protective mechanism (rope)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(self_deception_maintenance, 0.58).
domain_priors:suppression_score(self_deception_maintenance, 0.72).
domain_priors:theater_ratio(self_deception_maintenance, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(self_deception_maintenance, extractiveness, 0.58).
narrative_ontology:constraint_metric(self_deception_maintenance, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(self_deception_maintenance, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(self_deception_maintenance, snare).
narrative_ontology:human_readable(self_deception_maintenance, "Self-Deception Maintenance Constraint").
narrative_ontology:topic_domain(self_deception_maintenance, "psychology/cognition/interpersonal").

domain_priors:requires_active_enforcement(self_deception_maintenance).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(self_deception_maintenance, protected_identity).
narrative_ontology:constraint_beneficiary(self_deception_maintenance, anxiety_avoidance_system).
narrative_ontology:constraint_victim(self_deception_maintenance, epistemic_accuracy).
narrative_ontology:constraint_victim(self_deception_maintenance, adaptive_response_capacity).
narrative_ontology:constraint_victim(self_deception_maintenance, relational_authenticity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DECEIVED AGENT (SNARE) — Trapped by identity fusion with the false narrative. The agent's self-concept depends on the maintained deception; exit would require abandoning not just a belief but the identity constructed around it. Experiences full extraction burden: cognitive dissonance suppression, reality-testing avoidance, selective attention maintenance. High suppression from internalized enforcement — the agent polices their own cognition to protect the deception. Identity lock makes structural mobility (they could learn the truth) functionally inaccessible.
constraint_indexing:constraint_classification(self_deception_maintenance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ANXIETY AVOIDANCE SYSTEM (ROPE) — The psychological system maintaining the deception experiences it as pure coordination: deception prevents unbearable anxiety activation, creating functional stability through protective denial. From this system's framing, the constraint is a cooperative mechanism solving the problem of intolerable affect. No experienced extraction — the system benefits directly from maintaining the false narrative. Theater is performative (the narrative is constructed, maintained through selective processing) but serves genuine anxiety regulation function.
constraint_indexing:constraint_classification(self_deception_maintenance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE RELATIONAL NETWORK (TANGLED ROPE) — People in the agent's life experience mixed coordination and extraction. The deception provides coordination benefit (stable, predictable behavior patterns; avoidance of confrontation; maintained relational surface) but extracts through inauthenticity, unaddressed relational problems, and the burden of maintaining the false narrative alongside the agent. Constrained exit — people could refuse to collude with the deception but at relationship cost. Moderate extraction: benefits from stability but bears the cost of relational shallowness.
constraint_indexing:constraint_classification(self_deception_maintenance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: INTERGENERATIONAL PATTERN LOCK (SNARE) — When self-deception maintenance involves trauma patterns or family narratives, the constraint operates across generations. Children learn to maintain the same deceptions as parents; the false narrative becomes cultural-emotional inheritance. Trapped exit at generational timescale — breaking the deception pattern requires disrupting inherited meaning-making. High suppression: family systems actively enforce the narrative silence. The intergenerational victim experiences maximum extraction because they inherit the deception without choice.
constraint_indexing:constraint_classification(self_deception_maintenance, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE THERAPEUTIC REFRAMING SYSTEM (PITON) — From the external observer perspective (therapist, researcher, analyst), self-deception maintenance appears as a degraded institution: the mechanisms that once served protective function (trauma response, anxiety management, identity stability) persist through inertia even when circumstances have changed. The deception is maintained not because it works but because alternatives feel impossibly disorienting. Theater ratio is high because much of the energy goes into narrative maintenance and reality-testing avoidance rather than actual problem-solving. The piton classification reflects that the constraint's primary function has atrophied but institutional persistence continues.
constraint_indexing:constraint_classification(self_deception_maintenance, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER (SNARE) — From the analytical/civilizational context, self-deception maintenance is a structural snare: the cognitive architecture required for strategic deception (compartmentalization, selective attention, motivated reasoning) is the same architecture that creates the extraction mechanism — the agent becomes victim of their own enforcement. The suppression is total from this perspective because exit requires tools (meta-awareness, tolerance for uncertainty, identity plasticity) that the constraint itself prevents developing. Effective extraction grows with intelligence and sophistication: smarter agents can build more elaborate and durable false narratives.
constraint_indexing:constraint_classification(self_deception_maintenance, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(self_deception_maintenance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(self_deception_maintenance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(self_deception_maintenance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(self_deception_maintenance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(self_deception_maintenance, TR),
    TR >= 0.70.

:- end_tests(self_deception_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts through cognitive labor (suppressing disconfirming evidence, maintaining narrative consistency, managing cognitive dissonance), emotional labor (regulating the anxiety that the truth would trigger), relational labor (colluding with partners in narrative maintenance), and adaptive capacity (the agent's ability to respond to changing circumstances is reduced because reality-testing is suppressed). The rising trajectory from 0.38 to 0.58 reflects that as circumstances change and disconfirming evidence accumulates, the energy cost of maintaining the deception increases exponentially. This is not a stable extraction like a snare with external enforcement — it is a constraint that becomes progressively more extractive as the agent's life circumstances diverge from the false narrative. Suppression (0.72): High. The suppression is internalized and multi-layered: (1) active suppression of reality-testing (the agent avoids information that would disconfirm the narrative), (2) motivated reasoning (disconfirming evidence is reinterpreted to fit the narrative), (3) emotional regulation (affect associated with truth is preemptively blocked), (4) relational enforcement (the agent punishes relationship partners who challenge the narrative), (5) suppression of awareness of the suppression (the agent does not recognize that they are suppressing). Theater ratio (0.68): High. Much of the energy goes into maintaining the narrative appearance rather than solving actual problems. The false identity requires constant performance — behavioral consistency, emotional tone management, selective self-presentation — that consumes energy that could address underlying issues. The theatrical quality increases over time as the effort required to maintain the narrative grows.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a striking perspectival gap between the agent's internal experience and external observers' experience. From the agent's identity-locked perspective, the deception feels like essential self-protection — they classify it as coordination (rope) or even as natural law (mountain: 'this is just who I am'). From the anxiety avoidance system's perspective (institutional/arbitrage), the deception is pure coordination solving a genuine anxiety problem. From the relational network's perspective, the constraint is tangled rope — it coordinates behavioral stability while extracting authenticity. From the intergenerational or external therapeutic perspective, it is a snare: the agent is trapped by their own psychological system, experiencing maximum suppression and minimal exit capacity. The analytical observer sees degraded institution (piton): the mechanisms that once served protection now persist through inertia, consuming increasing energy while solving fewer problems. The gap between 'this is who I am' (identity-locked mountain) and 'this is a snare trapping you' (analytical snare) is the primary diagnostic signature of self-deception maintenance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the agent's structural position relative to the constraint and their exit capacity. The deceived agent (powerless + identity_locked) experiences very high d: they are the target of extraction even though they are also the enforcer. The anxiety avoidance system (institutional + arbitrage) experiences low d: they are the beneficiary, experiencing the constraint as coordination. The relational network (moderate + constrained) experiences moderate d: they benefit from behavioral stability but bear relationship costs, so their extraction is mixed. The intergenerational pattern (powerless + trapped) experiences highest d: they inherit the constraint without choice. The analytical observer (analytical + analytical) occupies an unusual position: they see the full structure while the agent does not. The operative mechanism is that the deceived agent's identity lock prevents them from perceiving their own d-value — they cannot see that they are the target of their own extraction system. The beneficiaries (protected identity and anxiety avoidance system) experience d near 0.0 because they benefit directly from the maintained deception. The relational network's d is higher because they bear suppression and extraction costs while being colluded into cooperation.
 *
 * MANDATROPHY ANALYSIS:
 *   Self-deception maintenance resolves the mandatrophy by revealing that the classification is not identity-locked in its conclusion. The constraint is unambiguously a snare from the analytical, relational-network, and intergenerational perspectives. The only perspectives that classify it as rope (coordination) or mountain (natural law) are perspectives that are themselves captured by the deception. The deceived agent cannot see the snare because they are identity-locked within it; the anxiety avoidance system cannot see the extraction because it is the beneficiary; the protected identity cannot see the trap because it depends on the trap. This is a case where mandatrophy resolution depends entirely on perspective. From captured perspectives, the constraint appears benign or natural. From external perspectives, it is clearly extractive. The engine's task is to flag that perspectives emanating from identity_locked positions at biographical timescale should be suspect — they are likely to misclassify snares as ropes because the binding mechanism is cognitive rather than structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_fusion_extent,
    'How deeply is the agent''s identity fused with the false narrative? Is the binding primarily cognitive (narrative structure) or emotional (identity security)?',
    'Exploration of identity stability through therapeutic exposure to contradiction. If identity destabilizes quickly when narrative is challenged, fusion is high. If identity sustains through challenge, binding is more narrative than existential.',
    'If primarily emotional: exit requires emotional reprocessing of core identity stability — maximum suppression, identity_locked exit. If primarily narrative: exit requires narrative reconstruction — constrained exit, potentially mobile over extended time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_fusion_extent, empirical, 'Depth of identity fusion with false narrative').

omega_variable(
    anxiety_substitution_feasibility,
    'Are there other anxiety-regulation mechanisms available that could replace the deception function, or is this the only available strategy?',
    'Cognitive-behavioral assessment of distress tolerance, emotion regulation capacity, alternative coping strategies. Determines whether the deception is the only viable anxiety management tool or whether viable alternatives exist but are not perceived.',
    'If only available strategy: suppression is structural (agent lacks capacity for exit) — classification remains Snare. If alternatives exist but are not seen: suppression is perceptual (belief constraint) — classification degrades toward Tangled Rope with identity_locked exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anxiety_substitution_feasibility, empirical, 'Whether alternative anxiety-regulation mechanisms are available').

omega_variable(
    relational_collusion_stability,
    'How stable is the relational network''s collusion with the deception? Are relationship partners actively enforcing the false narrative or passively accepting it?',
    'Analysis of relational communication patterns, confrontation responses, boundary maintenance. If partners actively enforce silence about the deception, collusion is stable and benefits them (reduces their own discomfort). If partners passively accept but would change if the agent changed, their buy-in is contingent.',
    'If actively enforced by partners: extractiveness increases because the deception is maintained by relationship bargains — victims are the agent and anyone outside the collusion who must interact with the false narrative. If passively accepted: extractiveness is primarily individual (self-enforcement) and relationship cost is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relational_collusion_stability, empirical, 'Stability of relational collusion with maintained deception').

omega_variable(
    reality_disconfirmation_rate,
    'How frequently does external reality directly contradict the maintained narrative? Can the agent successfully avoid or reframe disconfirming evidence?',
    'Longitudinal tracking of disconfirming events and agent''s narrative responses. High avoidance capability = high suppression. Low avoidance (frequent forced narrative reconstruction) = lower suppression, possible scaffold dynamics if narrative is being gradually updated.',
    'If reality disconfirmation is rare: deception is maintainable through selective attention — sustains Snare classification. If frequent: energy cost of reality-testing avoidance rises, potentially making the constraint unsustainable over time — suggests degradation to Piton or Scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reality_disconfirmation_rate, empirical, 'Frequency and avoidability of reality-disconfirming evidence').

omega_variable(
    beneficiary_identity_clarity,
    'Is the ''protected identity'' a coherent self-concept that benefits from the deception, or is the deception maintaining contradictory aspects of self-image?',
    'Narrative coherence analysis. If the false narrative serves to paper over internal contradictions (claims of integrity while behaving unethically, claims of independence while being controlled), the beneficiary is contradictory — the deception is maintaining an impossible identity configuration.',
    'If coherent: extractiveness is stable — false narrative reliably protects the identity. If contradictory: extractiveness is high AND unstable — the deception must continuously work to suppress awareness of its own failure, creating a secondary extraction layer (energy consumed in suppression of suppression awareness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_clarity, conceptual, 'Coherence of the identity protected by deception').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(self_deception_maintenance, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sdm_tr_t0, self_deception_maintenance, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sdm_tr_t3, self_deception_maintenance, theater_ratio, 3, 0.58).
narrative_ontology:measurement(sdm_tr_t6, self_deception_maintenance, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(sdm_be_t0, self_deception_maintenance, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sdm_be_t3, self_deception_maintenance, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(sdm_be_t6, self_deception_maintenance, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(self_deception_maintenance, attachment_coordination).
narrative_ontology:boltzmann_floor_override(self_deception_maintenance, 0.12).
narrative_ontology:affects_constraint(self_deception_maintenance, trauma_response_lock).
narrative_ontology:affects_constraint(self_deception_maintenance, relational_authenticity_suppression).
narrative_ontology:affects_constraint(self_deception_maintenance, identity_plasticity_constraint).

% DUAL FORMULATION NOTE:
% Self-deception maintenance is decomposed from broader interpersonal constraint family. This story addresses the individual psychology of deception maintenance (ε=0.58). Upstream stories: trauma_response_lock (ε=0.35, why the initial deception formed) and identity_plasticity_constraint (ε=0.48, why identity cannot adapt to new reality). Downstream: relational_authenticity_suppression (ε=0.52, extraction of relational authenticity through maintained collusion). Each constraint has distinct ε because they involve different extraction mechanisms — self-deception is about cognitive suppression labor, trauma lock is about affect regulation, identity constraint is about meaning-making capability, relational extraction is about authenticity cost. The constraint family forms a causal chain: trauma → identity rigidity → self-deception maintenance → relational inauthenticity → intergenerational transmission.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(self_deception_maintenance, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
