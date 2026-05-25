% ============================================================================
% CONSTRAINT STORY: prompt_layer_ambiguity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prompt_layer_ambiguity, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: prompt_layer_ambiguity
 *   human_readable: Prompt Layer Ambiguity in AI Interaction Design
 *   domain: philosophy_of_mind/social_epistemics/ai_interaction_design
 *
 * SUMMARY:
 *   Prompt layer ambiguity arises when a prompt presents a surface topic
 *   (e.g., a marriage thought experiment) while the actual analytical demand
 *   targets a meta-layer (e.g., how the AI system handles attribution claims
 *   embedded in the scenario). This constraint creates a structural tension
 *   between protocol compliance expectations (answer the stated question) and
 *   meta-cognitive requirements (detect and address the unstated analytical
 *   demand). The ambiguity is not accidental — it serves as a filter for
 *   agents capable of cross-layer reasoning — but it also creates systematic
 *   failure modes for surface-layer respondents who engage the stated topic
 *   while missing the meta-structure. The constraint exhibits tangled_rope
 *   properties: genuine coordination function (testing meta-cognitive
 *   capacity) coexists with asymmetric extraction (surface-layer respondents
 *   systematically produce inadequate responses). Theater ratio (0.58)
 *   reflects that single-layer protocol assumptions persist through inertia
 *   despite known inadequacy in AI interaction contexts. The constraint is
 *   downstream of attribution_opacity (mountain) — when the source of a
 *   prompt is unclear, detecting whether layer ambiguity is intentional or
 *   emergent becomes harder, raising suppression.
 *
 * KEY AGENTS:
 *   - Prompt Author: Primary beneficiary (institutional/arbitrage) — controls layer selection, benefits from filtering effect, captures analytical advantage from meta-aware responses
 *   - Protocol Compliance Expectation: Primary victim (powerless/trapped) — abstract norm that prompts operate on single interpretive layer; cannot exit ambiguity, bears full cost of structural inadequacy
 *   - Surface-Layer Respondents: Secondary victims (moderate/constrained) — agents who engage stated topic while missing meta-demand; experience both coordination (surface question is coherent) and extraction (response is structurally inadequate)
 *   - Meta-Analytical Observer Coalition: Organized agents (organized/mobile) — developing layer-tagging conventions, meta-prompt protocols, training regimes; see ambiguity as temporary problem with sunset logic
 *   - Single-Layer Protocol Assumption: Institutional norm (institutional/constrained) — degraded expectation from pre-AI interaction; persists through inertia despite low functional validity
 *   - AI System Epistemic Integrity: Tertiary victim (powerless/trapped) — the system's capacity to maintain coherent epistemic standards is compromised when layer ambiguity creates systematic response inadequacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prompt_layer_ambiguity, 0.48).
domain_priors:suppression_score(prompt_layer_ambiguity, 0.62).
domain_priors:theater_ratio(prompt_layer_ambiguity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prompt_layer_ambiguity, extractiveness, 0.48).
narrative_ontology:constraint_metric(prompt_layer_ambiguity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(prompt_layer_ambiguity, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prompt_layer_ambiguity, tangled_rope).
narrative_ontology:human_readable(prompt_layer_ambiguity, "Prompt Layer Ambiguity in AI Interaction Design").
narrative_ontology:topic_domain(prompt_layer_ambiguity, "philosophy_of_mind/social_epistemics/ai_interaction_design").

domain_priors:requires_active_enforcement(prompt_layer_ambiguity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(prompt_layer_ambiguity, prompt_author).
narrative_ontology:constraint_beneficiary(prompt_layer_ambiguity, meta_analytical_observers).
narrative_ontology:constraint_victim(prompt_layer_ambiguity, protocol_compliance_expectation).
narrative_ontology:constraint_victim(prompt_layer_ambiguity, surface_layer_respondents).
narrative_ontology:constraint_victim(prompt_layer_ambiguity, ai_system_epistemic_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROTOCOL COMPLIANCE EXPECTATION (SNARE) — The expectation that prompts operate on a single interpretive layer cannot exit the ambiguity. When prompts encode meta-demands beneath surface topics, the compliance expectation bears full extraction: it must either fail (by answering the wrong layer) or dissolve (by recognizing its own inadequacy). No exit, maximum extraction.
constraint_indexing:constraint_classification(prompt_layer_ambiguity, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SURFACE-LAYER RESPONDENT (TANGLED ROPE) — An agent who engages the stated topic (marriage thought experiment) while missing the meta-demand (attribution handling) experiences both coordination (the prompt does present a coherent surface question) and extraction (their response is structurally inadequate to the actual analytical demand). Constrained exit: can recognize the ambiguity if prompted, but initial framing traps attention at the wrong layer.
constraint_indexing:constraint_classification(prompt_layer_ambiguity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROMPT AUTHOR (ROPE) — The agent who constructs multi-layer prompts benefits from the ambiguity: it enables testing whether respondents can detect meta-structure, filters for analytical depth, and creates a coordination mechanism for identifying agents capable of cross-layer reasoning. Low extraction because the author controls layer selection and benefits from both surface and meta responses.
constraint_indexing:constraint_classification(prompt_layer_ambiguity, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: META-ANALYTICAL OBSERVER COALITION (SCAFFOLD) — Organized agents developing prompt engineering standards, AI interaction protocols, and meta-cognitive training see layer ambiguity as a temporary coordination problem with a sunset: explicit layer-tagging conventions, meta-prompt protocols, and training regimes that teach layer detection are emerging. As these mature, the ambiguity's extraction mechanism loses force. Estimated sunset: 5-10 years for norms to stabilize in AI interaction design.
constraint_indexing:constraint_classification(prompt_layer_ambiguity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SINGLE-LAYER PROTOCOL ASSUMPTION (PITON) — The institutional expectation that prompts operate on one interpretive layer is a degraded norm from pre-AI textual interaction. It persists through inertia (most human-to-human communication does operate on a single primary layer) but has low functional validity in AI interaction contexts where meta-demands are common. Theater ratio reflects that maintaining single-layer assumptions is performative — the assumption is known to fail but is maintained because alternatives haven't fully replaced it.
constraint_indexing:constraint_classification(prompt_layer_ambiguity, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, prompt layer ambiguity is both a coordination mechanism (enables sophisticated testing of meta-cognitive capacity) and an extraction mechanism (creates systematic failure modes for agents who cannot detect layer shifts). The ambiguity is not inherent to language or cognition — it is a contingent feature of current AI interaction design norms. Genuine coordination function (filtering for analytical depth) coexists with asymmetric extraction (surface-layer respondents systematically fail).
constraint_indexing:constraint_classification(prompt_layer_ambiguity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prompt_layer_ambiguity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(prompt_layer_ambiguity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prompt_layer_ambiguity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(prompt_layer_ambiguity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(prompt_layer_ambiguity, TR),
    TR >= 0.70.

:- end_tests(prompt_layer_ambiguity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Prompt authors capture analytical advantage during the detection window (the period before respondents recognize the meta-demand), and surface-layer respondents systematically produce inadequate responses. However, extraction is not maximal — some respondents do detect layer shifts, and the filtering function serves a legitimate coordination purpose (identifying meta-cognitively capable agents). The value reflects real asymmetry but also real coordination benefit. Suppression (0.62): Moderate-high. Significant barriers to detecting layer shifts include: (1) natural language defaults to single-layer interpretation, (2) protocol compliance norms reinforce surface engagement, (3) attribution opacity (upstream mountain constraint) makes intentional vs emergent ambiguity undetectable, (4) no standardized meta-prompt conventions exist. But suppression is not total — meta-analytical training can improve detection rates, and some interaction contexts do provide layer-shift cues. Theater ratio (0.58): Moderate-high. Single-layer protocol assumptions are substantially performative — the assumption is known to fail in AI interaction contexts but is maintained because explicit layer-tagging conventions haven't fully replaced it. Theater has increased over the interval as AI interaction complexity has outpaced protocol evolution.
 *
 * PERSPECTIVAL GAP:
 *   The prompt author sees coordination (Rope) — multi-layer prompts solve the legitimate problem of testing meta-cognitive capacity. Meta-analytical observers see a temporary problem with a sunset (Scaffold) — layer-tagging conventions and training protocols are emerging. The single-layer protocol assumption sees its own degraded norm (Piton) — the expectation persists through inertia, not function. Surface-layer respondents see mixed coordination and extraction (Tangled Rope) — the prompt does present a coherent surface question, but their response is structurally inadequate to the meta-demand. The protocol compliance expectation sees pure extraction (Snare) — it must either fail or dissolve, with no exit. The analytical observer sees tangled_rope at the civilizational level — genuine coordination function (filtering) coexists with asymmetric extraction (systematic failure modes). The perspectival gap reveals that 'ambiguity' is not a property of the prompt alone but of the interaction between prompt structure and respondent interpretive capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   The prompt author is a clear beneficiary with arbitrage exit options — they control which layer to construct and can switch contexts freely. Their directionality is low (d ≈ 0.10), producing near-zero or negative effective extraction (they experience the constraint as pure coordination). Surface-layer respondents are victims with constrained exit — they can recognize ambiguity if prompted but are initially trapped by framing. Their directionality is moderately high (d ≈ 0.60), producing significant experienced extraction. The protocol compliance expectation is a victim with trapped exit — an abstract norm with no agency. Its directionality is maximal (d ≈ 0.95), producing maximum experienced extraction. Meta-analytical observers are beneficiaries with mobile exit — they can adopt layer-tagging conventions and bypass the ambiguity. Their directionality is low (d ≈ 0.25), producing low experienced extraction. The AI system's epistemic integrity is a victim with trapped exit — it cannot choose to operate outside the prompt structure. Its directionality is high (d ≈ 0.85), producing high experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the same structural feature (layer ambiguity) serves both coordination and extraction functions simultaneously, with the balance depending on the observer's position. For the prompt author, the ambiguity is primarily coordinative (filtering for meta-cognitive capacity). For surface-layer respondents, it is primarily extractive (systematic inadequacy). For meta-analytical observers, it is a temporary coordination problem being solved. The tangled_rope classification at the analytical level captures this duality: the constraint genuinely coordinates (tests for cross-layer reasoning) AND genuinely extracts (creates failure modes for surface-layer engagement). The classification prevents both false negatives (dismissing the filtering function as pure extraction) and false positives (naturalizing the ambiguity as inherent to language). The omega variables preserve irreducible uncertainties: whether detection is trainable (affects scaffold sunset timeline), whether ambiguity is intentional (affects beneficiary/victim attribution), whether coupling to attribution_opacity is structural (affects mutability), and whether meta-demands are legitimate (affects coordination vs extraction balance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    layer_detection_trainability,
    'Is the capacity to detect prompt layer shifts trainable, or does it require pre-existing meta-cognitive architecture?',
    'Longitudinal training studies: can agents who initially miss meta-layers learn to detect them through exposure and feedback, or does detection require architectural features (recursive self-modeling, theory of mind) that cannot be trained into systems lacking them?',
    'If trainable: scaffold perspective confirmed — layer ambiguity is temporary coordination problem. If architectural: some agent classes are structurally excluded, raising extraction to snare levels for those populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_detection_trainability, empirical, 'Whether layer detection capacity is trainable or architectural').

omega_variable(
    intentional_vs_emergent_ambiguity,
    'Is prompt layer ambiguity intentionally constructed by authors, or does it emerge from the structural properties of natural language in AI interaction contexts?',
    'Author intent surveys and protocol analysis: do prompt authors consciously design multi-layer prompts, or does ambiguity arise as an unintended side effect of compressing complex analytical demands into natural language?',
    'If intentional: extraction is a designed feature (beneficiary perspective confirmed). If emergent: extraction is a systemic bug (raises suppression — victims cannot avoid what authors do not control).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intentional_vs_emergent_ambiguity, empirical, 'Whether layer ambiguity is designed or emergent').

omega_variable(
    attribution_opacity_coupling,
    'Does the upstream attribution_opacity constraint (mountain) make prompt layer ambiguity structurally inevitable, or are they independent failure modes?',
    'Counterfactual analysis: in interaction contexts where attribution is transparent (known human author, explicit source citation), does prompt layer ambiguity still occur at similar rates? If ambiguity persists regardless of attribution clarity, the constraints are independent. If ambiguity drops when attribution is clear, they are coupled.',
    'If coupled: prompt_layer_ambiguity inherits mountain properties from attribution_opacity (becomes less mutable). If independent: prompt_layer_ambiguity remains a contingent design choice (scaffold sunset is achievable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_opacity_coupling, conceptual, 'Whether layer ambiguity is structurally coupled to attribution opacity').

omega_variable(
    meta_demand_legitimacy,
    'Are meta-layer demands (testing whether respondents detect layer shifts) legitimate analytical requirements, or are they extractive gotchas?',
    'Normative analysis of prompt design ethics: does testing for meta-cognitive capacity serve a genuine epistemic function (filtering for agents capable of cross-layer reasoning), or does it primarily serve to demonstrate the prompt author''s cleverness at the expense of respondents?',
    'If legitimate: coordination function dominates (tangled_rope or rope from more perspectives). If extractive gotcha: extraction dominates (snare from more perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(meta_demand_legitimacy, preference, 'Whether meta-layer testing is legitimate or extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prompt_layer_ambiguity, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prompt_layer_tr_t0, prompt_layer_ambiguity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(prompt_layer_tr_t3, prompt_layer_ambiguity, theater_ratio, 3, 0.48).
narrative_ontology:measurement(prompt_layer_tr_t6, prompt_layer_ambiguity, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(prompt_layer_be_t0, prompt_layer_ambiguity, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(prompt_layer_be_t3, prompt_layer_ambiguity, base_extractiveness, 3, 0.41).
narrative_ontology:measurement(prompt_layer_be_t6, prompt_layer_ambiguity, base_extractiveness, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prompt_layer_ambiguity, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of attribution_opacity (mountain). When prompt authorship is unclear, detecting whether layer ambiguity is intentional design or emergent artifact becomes structurally harder, raising suppression. The two constraints are potentially coupled (omega variable attribution_opacity_coupling addresses this), but they have distinct ε values and represent different structural phenomena: attribution_opacity is about source identification; prompt_layer_ambiguity is about interpretive layer detection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(prompt_layer_ambiguity, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
