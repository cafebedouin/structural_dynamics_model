% ============================================================================
% CONSTRAINT STORY: misattribution_feedback_loop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_misattribution_feedback_loop, []).

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
 *   constraint_id: misattribution_feedback_loop
 *   human_readable: Misattribution Feedback Loop in Conversational Exit
 *   domain: applied_ethics/social_psychology/phenomenology_of_attention
 *
 * SUMMARY:
 *   The misattribution feedback loop operates at the intersection of
 *   introspective access limits (upstream mountain constraint) and social
 *   coordination incentives. An agent experiences discomfort during a
 *   conversation — the topic is awkward, the relational dynamic is tense, or
 *   the cognitive demand of tracking multiple perspectives is high. Rather
 *   than attributing the discomfort to its actual source (social awkwardness,
 *   emotional activation, cognitive load from novelty), the agent misreads it
 *   as depletion: 'I'm too tired for this conversation.' This misattribution
 *   is reinforced by cultural narratives around self-care and
 *   boundary-setting, which frame any claimed limit as virtuous and
 *   unchallengeable. The agent exits the conversation prematurely, citing
 *   depletion. The exit forecloses the evidence that would correct the
 *   misattribution: if the agent had stayed in the conversation, they would
 *   have discovered that the discomfort was temporary, that they had more
 *   capacity than they thought, and that the conversation could deepen past
 *   the awkward threshold. Instead, the premature exit confirms the depletion
 *   narrative ('I was right to leave — I was exhausted'), and the loop
 *   tightens. Over time, the agent's tolerance for conversational discomfort
 *   decreases, their depletion claims become more frequent, and their access
 *   to relational and epistemic depth atrophies. The constraint exhibits
 *   tangled rope structure: it provides genuine coordination (exit from
 *   awkwardness) while extracting from conversation depth potential and
 *   epistemic accuracy.
 *
 * KEY AGENTS:
 *   - Narrative Self-Justification: Primary beneficiary (institutional/arbitrage) — the depletion claim provides low-cost exit from discomfort with minimal face threat
 *   - Conversation Depth Potential: Primary victim (powerless/identity_locked) — the unrealized epistemic and relational value foreclosed by premature exit; identity-locked because the depletion frame makes continued engagement unthinkable
 *   - Conversational Partner: Secondary victim (moderate/constrained) — loses access to depth, receives false signal about relationship capacity; constrained from challenging the depletion claim by social cost
 *   - Epistemic Accuracy: Secondary victim (powerless/trapped) — the agent's self-knowledge is contaminated by the misattribution; no exit from the feedback loop without external intervention
 *   - Metacognitive Training Community: Organized agents (organized/mobile) — therapists, contemplative practitioners, communication trainers building alternative pathways via discomfort-depletion distinction training
 *   - Self-Care Discourse: Institutional actor (institutional/mobile) — cultural narrative that originally countered overwork but now enables premature exit; high theater ratio (performs boundary-setting without distinguishing genuine limits from misattribution)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(misattribution_feedback_loop, 0.48).
domain_priors:suppression_score(misattribution_feedback_loop, 0.52).
domain_priors:theater_ratio(misattribution_feedback_loop, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(misattribution_feedback_loop, extractiveness, 0.48).
narrative_ontology:constraint_metric(misattribution_feedback_loop, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(misattribution_feedback_loop, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(misattribution_feedback_loop, tangled_rope).
narrative_ontology:human_readable(misattribution_feedback_loop, "Misattribution Feedback Loop in Conversational Exit").
narrative_ontology:topic_domain(misattribution_feedback_loop, "applied_ethics/social_psychology/phenomenology_of_attention").

domain_priors:requires_active_enforcement(misattribution_feedback_loop).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(misattribution_feedback_loop, narrative_self_justification).
narrative_ontology:constraint_beneficiary(misattribution_feedback_loop, social_comfort_maintenance).
narrative_ontology:constraint_victim(misattribution_feedback_loop, conversation_depth_potential).
narrative_ontology:constraint_victim(misattribution_feedback_loop, epistemic_accuracy).
narrative_ontology:constraint_victim(misattribution_feedback_loop, relational_trust_development).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONVERSATION DEPTH POTENTIAL (SNARE) — The unrealized epistemic and relational value that could emerge from sustained engagement. Identity-locked because the agent's self-concept as 'depleted' makes continued engagement literally unthinkable from within the frame. Maximum extraction: the feedback loop forecloses the very evidence that would reveal the misattribution.
constraint_indexing:constraint_classification(misattribution_feedback_loop, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: CONVERSATIONAL PARTNER (TANGLED ROPE) — Experiences both coordination (the exit provides social comfort, avoids awkwardness) and extraction (loses access to depth, receives false signal about relationship capacity). Constrained exit: can challenge the depletion claim but faces social cost of appearing demanding or insensitive.
constraint_indexing:constraint_classification(misattribution_feedback_loop, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: NARRATIVE SELF-JUSTIFICATION (ROPE) — Primary beneficiary. The depletion narrative provides socially acceptable exit from discomfort with minimal face threat. Experiences the constraint as pure coordination: it solves the problem of how to exit awkward conversations without admitting discomfort or disinterest. Arbitrage exit: can deploy alternative exit strategies (honesty, topic shift) but depletion claim is lowest-cost.
constraint_indexing:constraint_classification(misattribution_feedback_loop, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: METACOGNITIVE TRAINING COMMUNITY (SCAFFOLD) — Organized agents (therapists, contemplative practitioners, communication trainers) building alternative pathways: teaching distinction between discomfort and depletion, training counterfactual capacity tests, normalizing awkwardness as growth signal. Sees the loop as temporary coordination failure with sunset logic: as metacognitive literacy spreads, the misattribution loses its grip.
constraint_indexing:constraint_classification(misattribution_feedback_loop, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: SELF-CARE DISCOURSE (PITON) — The cultural narrative that 'listening to your body' and 'honoring your limits' is always virtuous. Originally functional (countering overwork, burnout), now largely theatrical: the discourse persists through institutional inertia in wellness culture despite enabling premature exit from growth-edge conversations. High theater ratio: the self-care frame performs boundary-setting without distinguishing genuine depletion from misattributed discomfort.
constraint_indexing:constraint_classification(misattribution_feedback_loop, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both the genuine coordination function (exit from conversations that truly exceed capacity) and the extractive mechanism (premature exit forecloses corrective evidence). The loop is not a natural law — introspective access limits (upstream mountain) create vulnerability to misattribution, but the feedback loop itself is a contingent social-cognitive pattern that could be interrupted with training.
constraint_indexing:constraint_classification(misattribution_feedback_loop, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(misattribution_feedback_loop_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(misattribution_feedback_loop, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(misattribution_feedback_loop, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(misattribution_feedback_loop, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(misattribution_feedback_loop, TR),
    TR >= 0.70.

:- end_tests(misattribution_feedback_loop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The loop extracts from conversation depth potential (foreclosed epistemic and relational value), epistemic accuracy (contaminated self-knowledge), and relational trust development (partner receives false signal about capacity). The extraction is not maximal because the coordination function is genuine — some exits are warranted, and the depletion narrative does solve the problem of how to exit awkward conversations. But the extraction is significant because the feedback mechanism prevents self-correction: the agent never discovers that many of their depletion claims were misattributions. Suppression (0.52): Moderate. The loop is sustained by introspective access limits (upstream mountain — the agent genuinely cannot distinguish discomfort from depletion without training), social cost asymmetry (challenging a depletion claim is taboo in self-care culture), and identity fusion (the depletion narrative becomes constitutive of self-concept over time). But suppression is not total — metacognitive training can interrupt the loop, and some agents spontaneously discover the misattribution through counterfactual capacity tests. Theater ratio (0.68): High. The self-care discourse performs boundary-setting and self-knowledge ('listening to your body') without distinguishing genuine depletion from misattributed discomfort. The theater has increased over the interval as wellness culture has normalized all claimed limits as unchallengeable, making the depletion narrative a universal exit strategy regardless of its accuracy.
 *
 * PERSPECTIVAL GAP:
 *   The narrative self-justification system sees pure coordination (Rope) — the depletion claim solves the problem of how to exit awkward conversations. The metacognitive training community sees a temporary problem with a sunset (Scaffold) — training in discomfort-depletion distinction is building alternative pathways. The self-care discourse sees its own degraded ritual (Piton) — the boundary-setting frame persists through cultural inertia despite enabling premature exit. The conversational partner sees mixed coordination and extraction (Tangled Rope) — the exit provides social comfort but forecloses depth. The conversation depth potential sees pure extraction (Snare) — the feedback loop forecloses the very evidence that would reveal the misattribution, and the identity-lock makes exit from the frame impossible without external intervention. The analytical observer sees tangled rope structure: genuine coordination function (warranted exits) coexisting with extractive mechanism (premature exits that foreclose corrective evidence).
 *
 * DIRECTIONALITY LOGIC:
 *   The narrative self-justification system is the primary beneficiary: it captures the coordination value (low-cost exit from awkwardness) while externalizing the extraction cost onto conversation depth potential and epistemic accuracy. The system experiences low directionality (d ≈ 0.15) because it benefits from the constraint — the depletion claim solves a real social problem. The conversation depth potential is the primary victim with maximum directionality (d ≈ 0.89): it is identity-locked (the depletion frame makes continued engagement unthinkable from within) and bears the full cost of premature exit. The conversational partner experiences moderate directionality (d ≈ 0.55): constrained from challenging the claim by social cost, but also benefits from avoiding awkwardness. The metacognitive training community experiences low directionality (d ≈ 0.35): organized agents with mobile exit options who see the loop as a solvable coordination problem. The self-care discourse experiences low directionality (d ≈ 0.20): institutional actor with mobile exit (could shift norms) but currently benefits from the theater (wellness culture is sustained by the discourse). The analytical observer experiences moderate directionality (d ≈ 0.72): recognizes both coordination and extraction but has no direct stake in the loop.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing coordination (exit from conversations that genuinely exceed capacity) from extraction (premature exit that forecloses evidence of misattribution). The depletion narrative is not pure extraction — it solves a real social problem (how to exit awkwardness without face threat). But it is also not pure coordination — the feedback loop prevents self-correction, contaminating the agent's self-knowledge and foreclosing relational depth. The tangled rope classification captures this hybrid structure: the constraint coordinates (provides exit strategy) while extracting (forecloses corrective evidence). The perspectival gap reveals the structure: beneficiaries see rope, victims see snare, and the analytical observer sees the tangled rope that emerges from their interaction. The upstream mountain (introspective access limits) creates vulnerability to misattribution, but the feedback loop itself is a contingent social-cognitive pattern sustained by coordination incentives and cultural narratives — not a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discomfort_depletion_boundary,
    'What phenomenological markers reliably distinguish genuine cognitive depletion from misattributed social discomfort?',
    'Counterfactual capacity testing: if the agent can immediately engage in a different cognitively demanding task (complex game, technical problem-solving) after claiming depletion in conversation, the depletion claim is likely misattribution. Longitudinal tracking of claimed depletion episodes vs. objective performance measures.',
    'If reliable markers exist and are teachable: the loop becomes a scaffold (solvable coordination problem). If markers are unreliable or unteachable: the loop remains a tangled rope or degrades to snare (unavoidable extraction from conversation depth).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discomfort_depletion_boundary, empirical, 'Phenomenological boundary between genuine depletion and misattributed discomfort').

omega_variable(
    metacognitive_training_effectiveness,
    'Does explicit training in discomfort-depletion distinction actually reduce premature conversational exit rates?',
    'RCT comparing trained vs. untrained cohorts on: (1) exit timing relative to awkwardness onset, (2) post-conversation regret rates, (3) relationship depth development over time. Measure whether training breaks the feedback loop or merely adds another layer of self-monitoring theater.',
    'If effective: scaffold perspective confirmed — the loop has a real sunset. If ineffective: the loop may be more deeply embedded in identity maintenance than metacognitive intervention can reach, suggesting identity_locked mechanism is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metacognitive_training_effectiveness, empirical, 'Whether metacognitive training breaks the feedback loop').

omega_variable(
    social_cost_asymmetry,
    'Is the social cost of challenging a depletion claim higher than the cost of accepting premature exit, and does this asymmetry sustain the loop?',
    'Experimental manipulation of social norms around depletion claims: measure exit patterns in contexts where challenging depletion is normalized vs. contexts where it''s taboo. If asymmetry is primary driver, norm change should reduce loop frequency.',
    'If asymmetry is primary: the loop is sustained by coordination incentives (avoiding awkwardness of challenge), suggesting rope-like features dominate. If asymmetry is secondary: the loop is sustained by internal misattribution regardless of social context, suggesting snare-like features dominate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(social_cost_asymmetry, empirical, 'Whether social cost asymmetry sustains the feedback loop').

omega_variable(
    identity_fusion_depth,
    'To what extent is the depletion narrative constitutive of self-concept vs. instrumentally deployed for exit?',
    'Implicit association testing: measure automatic associations between ''depletion'' and ''self'' vs. ''depletion'' and ''strategy''. Interview data on whether agents experience depletion claims as discoveries about themselves or as tactical moves. Longitudinal tracking of identity stability around depletion narratives.',
    'If constitutive (identity-fused): the loop is harder to interrupt — exit is not strategic but phenomenologically real from within the frame. If instrumental: the loop is more amenable to intervention via cost-benefit reframing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_depth, conceptual, 'Whether depletion narrative is identity-constitutive or instrumental').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(misattribution_feedback_loop, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(misattr_tr_t0, misattribution_feedback_loop, theater_ratio, 0, 0.45).
narrative_ontology:measurement(misattr_tr_t3, misattribution_feedback_loop, theater_ratio, 3, 0.55).
narrative_ontology:measurement(misattr_tr_t6, misattribution_feedback_loop, theater_ratio, 6, 0.62).
narrative_ontology:measurement(misattr_tr_t10, misattribution_feedback_loop, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(misattr_be_t0, misattribution_feedback_loop, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(misattr_be_t3, misattribution_feedback_loop, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(misattr_be_t6, misattribution_feedback_loop, base_extractiveness, 6, 0.43).
narrative_ontology:measurement(misattr_be_t10, misattribution_feedback_loop, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(misattribution_feedback_loop, identity_coordination).

% DUAL FORMULATION NOTE:
% The misattribution feedback loop is downstream of introspective_access_limits (mountain) but represents a distinct structural constraint. The upstream constraint establishes that agents cannot directly introspect the difference between discomfort and depletion without training; the feedback loop is the social-cognitive pattern that emerges when this introspective limit interacts with coordination incentives (exit from awkwardness) and cultural narratives (self-care discourse). The loop has its own extractiveness value (0.48) reflecting the foreclosed conversation depth and contaminated self-knowledge, distinct from the upstream mountain's near-zero extractiveness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
