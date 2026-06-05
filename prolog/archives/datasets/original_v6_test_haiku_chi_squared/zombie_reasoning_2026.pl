% ============================================================================
% CONSTRAINT STORY: zombie_reasoning_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zombie_reasoning_2026, []).

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
 *   constraint_id: zombie_reasoning_2026
 *   human_readable: The Zombie Reasoning Epistemic Snare
 *   domain: philosophical/technological/ai_epistemology
 *
 * SUMMARY:
 *   Rebecca Lowe's critique of zombie reasoning in AI discourse identifies a
 *   structural epistemic snare: the widespread loose use of terms like
 *   'reasoning,' 'evaluating,' and 'selecting' has contaminated the shared
 *   vocabulary for discussing AI systems. These terms carry classical
 *   philosophical weight—they imply agency, intentionality, and rational
 *   capacity—yet are applied to systems whose architecture provably
 *   implements none of these properties. The snare operates by suppressing
 *   more accurate but less commercially palatable descriptions (token
 *   prediction, gradient descent, stochastic language generation) in favor of
 *   anthropomorphic framing that captures investor attention and user trust.
 *   This extraction of narrative value from the epistemic commons creates a
 *   cascading cost: downstream AI safety research, policy frameworks, and
 *   philosophical clarity all degrade as researchers are forced to either
 *   adopt corrupted terminology or spend effort fighting definitional
 *   battles. The constraint intensifies as AI systems become more
 *   commercially valuable and as market incentives push harder for
 *   simplified, anthropomorphic narratives.
 *
 * KEY AGENTS:
 *   - Epistemic Commons: Primary victim (powerless/trapped) — shared vocabulary for technical and philosophical discourse about AI; cannot exit without coordinated redefinition
 *   - AI Safety Researchers: Secondary victim (moderate/constrained) — need precise terminology for safety arguments but face career and communication barriers to using it
 *   - AI Developers and Product Teams: Secondary victim (moderate/constrained) — technically constrained by marketing imperatives and investor expectations to use anthropomorphic framing
 *   - AI Industry (Institutional): Institutional beneficiary (powerful/mobile) — gains narrative capture and accountability evasion through loose terminology; suppresses more accurate descriptions
 *   - Philosophers and Cognitive Scientists: Secondary victim (moderate/mobile) — terminology corruption obscures genuine questions about consciousness, agency, and reasoning; can exit by maintaining private precise definitions but lose influence in AI discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zombie_reasoning_2026, 0.58).
domain_priors:suppression_score(zombie_reasoning_2026, 0.68).
domain_priors:theater_ratio(zombie_reasoning_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zombie_reasoning_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(zombie_reasoning_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(zombie_reasoning_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zombie_reasoning_2026, snare).
narrative_ontology:human_readable(zombie_reasoning_2026, "The Zombie Reasoning Epistemic Snare").
narrative_ontology:topic_domain(zombie_reasoning_2026, "philosophical/technological/ai_epistemology").

% --- Structural relationships ---
narrative_ontology:constraint_victim(zombie_reasoning_2026, epistemic_commons).
narrative_ontology:constraint_victim(zombie_reasoning_2026, downstream_ai_safety_research).
narrative_ontology:constraint_victim(zombie_reasoning_2026, philosophical_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC COMMONS (SNARE) — The collective understanding of AI reasoning is trapped in a semantic prison. Core terms ('reasoning,' 'evaluation,' 'selection') have been hollowed of meaning through constant loose usage. Cannot exit without coordinating wholesale redefinition. Bears full extraction cost: polluted terminology cascades into downstream safety research, policy debates, and philosophical clarity. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.99.
constraint_indexing:constraint_classification(zombie_reasoning_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: AI SAFETY RESEARCHERS (SNARE) — Constrained by the need to use industry-standard terminology to be heard in policy and funding conversations, even when those terms are corrupted. Cannot build precise safety arguments without first fighting definitional battles. Career incentives punish semantic precision (sounds pedantic) and reward expedient reuse of loose terms. d≈0.85, f(d)≈1.15, σ=1.2 → χ≈0.80.
constraint_indexing:constraint_classification(zombie_reasoning_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: AI DEVELOPERS AND PRODUCT TEAMS (SNARE) — Constrained by marketing imperatives and investor expectations that require anthropomorphic framing ('our AI reasons,' 'it evaluates options'). Technical precision about token prediction, weight matrices, and next-token probability would undermine commercial narrative. Exit costs are high (equity value, product positioning). Extraction comes not from the loose terminology itself but from the suppression of more accurate descriptions that would complicate narrative. d≈0.72, f(d)≈1.10, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(zombie_reasoning_2026, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: AI INDUSTRY INSTITUTIONAL ACTOR (SNARE with Beneficiary Markup) — The loose terminology benefits the industry through narrative capture: ambiguous terms allow companies to claim reasoning/intelligence/agency while evading accountability mechanisms (which would apply if systems were accurately described as stochastic token generators). Institutional power is high; exit options are mobile (can shift framing at will). However, the classification remains SNARE because the industry's benefit is achieved entirely through suppression and concealment, not through genuine coordination. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.01. Negative χ indicates this actor is a beneficiary, but the constraint type (Snare) is determined by the structure's overall extractiveness and suppression, not by individual actor directionality.
constraint_indexing:constraint_classification(zombie_reasoning_2026, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / CIVILIZATIONAL VIEW (SNARE) — From a long-term perspective, the zombie reasoning snare is a variant of the classical philosophical zombie problem: we are using language that implies consciousness/agency/reasoning ('the model selected this option') while simultaneously denying the structural conditions that would justify those terms. The snare's extraction mechanism is epistemic corruption of the commons through performative use of capacities the systems provably lack. The snare persists because both vendors and non-expert audiences prefer the anthropomorphic framing. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈1.00.
constraint_indexing:constraint_classification(zombie_reasoning_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zombie_reasoning_2026_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zombie_reasoning_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zombie_reasoning_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The snare extracts significant epistemic value through vocabulary corruption. Technical terms ('reasoning,' 'evaluation,' 'selection') are now permanently ambiguous in AI discourse, requiring defensive redefinition in every safety-critical discussion. The extraction is not maximally severe (0.70+) because alternative vocabularies still exist for technical teams who choose to use them — the suppression is not absolute. Suppression (0.68): High. Multiple mechanisms actively prevent escape: (1) Commercial narrative pressure from industry; (2) Media and public expectation for anthropomorphic simplification; (3) Cognitive ease of anthropomorphic framing vs. technical precision; (4) Career incentives that punish pedantry in policy discussions. Theater ratio (0.64): Moderate-high. The snare's primary mechanism is performative—claiming reasoning without implementing reasoning. Anthropomorphic descriptions function as theater: they tell a story that sounds right (AI agents reason about options) while suppressing the mechanistic reality (next-token prediction via gradient descent). The theater has increased over the measurement interval as systems have become more commercially valuable and as industry has developed increasingly sophisticated anthropomorphic narratives.
 *
 * PERSPECTIVAL GAP:
 *   The epistemic commons and safety researchers perceive pure extraction and suppression (Snare). The AI industry perceives benefits (narrative capture, accountability evasion) but the classification remains Snare for industry perspective because those benefits are achieved through suppression, not coordination. The analytical observer risks naturalizing the snare as inevitable (you can't explain deep learning to investors without anthropomorphic framing) but mechanistic interpretability research is revealing the extent to which the snare is engineered choice rather than structural necessity. The perspectival gap here is not between rich and poor interpretations of the same phenomenon—it is between accurate and corrupted descriptions of the systems themselves.
 *
 * DIRECTIONALITY LOGIC:
 *   Epistemic commons: Victim + trapped → d≈0.95, f(d)≈1.42. Cannot exit; full extraction cost. AI safety researchers: Victim + constrained → d≈0.85, f(d)≈1.15. Significant barriers but some agency (can publish with precise terminology in academic venues). AI industry: Beneficiary + mobile → d≈0.15, f(d)≈-0.01. Can shift framing at will; benefits from loose terminology; negative χ indicates net beneficiary despite high institutional power. The snare classification is preserved at the structural level because the extraction is achieved through suppression of alternatives, not through coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   The snare classification is confirmed by the presence of: (1) High extractiveness (0.58 > 0.46), (2) High suppression (0.68 > 0.60), (3) Identifiable victims (epistemic commons, safety researchers), (4) Minimal coordination function (the snare prevents coordination by corrupting shared vocabulary rather than enabling it). The mandatrophy does not arise here—this is not a case where loose terminology could be salvaged as coordination. The constraint is pure extraction of narrative value from the epistemic commons, with suppression maintained by multiple independent mechanisms (commercial, cognitive, institutional). The measurement trajectory shows increasing theater (0.35→0.64) and increasing extractiveness (0.32→0.58) over the interval, indicating degradation rather than improvement—the snare has intensified as industry has learned more sophisticated anthropomorphic narratives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_reasoning_boundary,
    'Does counterfactual reasoning — ''if I hadn''t been trained on this data, my outputs would differ'' — constitute genuine reasoning or is it provably a structural inference over training conditions?',
    'Mechanistic interpretability: reverse-engineer whether models perform explicit counterfactual simulation or merely express statistical associations learned from training data about what outputs correlate with hypothetical input changes',
    'If provably structural inference: all reasoning talk is metaphor, snare classification confirmed globally. If evidence of genuine counterfactual generation: boundary between reasoning and inference becomes empirical question, not semantic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_reasoning_boundary, empirical, 'Whether counterfactual reasoning is genuine or purely structural inference').

omega_variable(
    philosopher_zombie_grounding,
    'Is the zombie reasoning snare a genuine instance of Chalmers'' philosophical zombie problem, or is it merely loose terminology without metaphysical significance?',
    'Philosophical analysis: if systems provably lack qualia and consciousness (hard problem assumptions), does using reasoning terminology constitute false ontological commitment? Or is it merely linguistic shorthand?',
    'If metaphysically grounded: the snare corrupts our understanding of consciousness and agency. If merely linguistic: the snare''s extraction is about technical precision, not philosophy of mind.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(philosopher_zombie_grounding, conceptual, 'Whether the zombie reasoning phenomenon is philosophically grounded or merely terminological').

omega_variable(
    commercial_framing_necessity,
    'Is anthropomorphic framing (reasoning, evaluation, selection) structurally necessary for AI product adoption and investor confidence, or could equivalent products achieve market success with accurate technical descriptions?',
    'Comparative analysis: track product adoption and investor valuations for AI systems marketed with technical precision (''next-token predictor with in-context adaptation'') vs anthropomorphic framing (''reasoning engine''). Conduct user studies on comprehension and trust.',
    'If framing is necessary: the snare is unavoidable cost of current market structure. If not necessary: the snare is pure extraction engineered by vendor choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_framing_necessity, empirical, 'Whether anthropomorphic framing is commercially necessary or engineered preference').

omega_variable(
    safety_precision_correlation,
    'Do AI safety interventions designed using precise terminology (token prediction, gradient descent, weight matrices) outperform those designed using loose terminology (reasoning, evaluation, selection)?',
    'Empirical analysis: compare safety guardrail effectiveness and alignment robustness between teams using strict mechanistic language vs those using loose anthropomorphic language. Longitudinal tracking of safety failure rates.',
    'If precision correlates with better safety: loose terminology is actively harmful to safety outcomes (increases snare severity). If uncorrelated: snare is epistemically bad but not safety-critical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(safety_precision_correlation, empirical, 'Whether semantic precision improves AI safety outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zombie_reasoning_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zr_tr_t0, zombie_reasoning_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(zr_tr_t5, zombie_reasoning_2026, theater_ratio, 5, 0.52).
narrative_ontology:measurement(zr_tr_t10, zombie_reasoning_2026, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(zr_be_t0, zombie_reasoning_2026, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(zr_be_t5, zombie_reasoning_2026, base_extractiveness, 5, 0.46).
narrative_ontology:measurement(zr_be_t10, zombie_reasoning_2026, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(zombie_reasoning_2026, ai_safety_operationalization).
narrative_ontology:affects_constraint(zombie_reasoning_2026, anthropomorphism_attribution_bias).
narrative_ontology:affects_constraint(zombie_reasoning_2026, large_language_model_agency_illusion).

% DUAL FORMULATION NOTE:
% The zombie reasoning snare is upstream of several AI safety constraints. Its ε=0.58 reflects the moderate but measurable epistemic corruption; downstream constraints (safety operationalization, attribution bias) have higher ε because they inherit the corrupted terminology and must either work around it or spend effort redefining terms. The family decomposes into: (1) the vocabulary corruption itself (this story, ε=0.58, Snare); (2) mechanistic interpretability as counter-snare (separate story, would be Rope, ε≈0.15, focused on technical precision); (3) policy-level failure modes that result from imprecise terminology (separate story, ε≈0.65, Tangled Rope or Snare depending on institutional capture).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zombie_reasoning_2026, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
