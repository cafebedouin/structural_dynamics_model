% ============================================================================
% CONSTRAINT STORY: layered_brain_processing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_layered_brain_processing, []).

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
 *   constraint_id: layered_brain_processing
 *   human_readable: Layered Contextual Meaning Construction in Brain-LLM Isomorphism
 *   domain: neuroscience/machine_learning/cognition
 *
 * SUMMARY:
 *   The isomorphism between layered brain processing (cortical hierarchy,
 *   serial receptive field refinement) and the transformer architecture of
 *   large language models (stacked layers, sequential token processing) has
 *   become a dominant conceptual framework in contemporary neuroscience and
 *   cognitive science. This constraint captures the structural dynamics of
 *   how this analogy functions as both a genuine coordination mechanism and
 *   an extraction apparatus. The brain-LLM mapping enables unprecedented
 *   cross-disciplinary funding, attracts machine learning researchers into
 *   neuroscience, creates shared conceptual vocabulary, and provides concrete
 *   computational models for testing neural theories. Simultaneously, it
 *   suppresses alternative cognitive architectures (embodied cognition,
 *   predictive coding, 4E frameworks, dynamical systems approaches) by
 *   channeling funding, editorial space, and research attention toward model
 *   systems that exhibit transformer-like serial processing. The constraint
 *   exhibits genuine coordination benefits — without the analogy,
 *   interdisciplinary collaboration would be difficult, funding
 *   justifications opaque, and model validation unfocused. But it also
 *   extracts from the embodied cognition community, which has found itself
 *   marginalized as 'non-computational' or 'unfalsifiable' relative to the
 *   precise, testable claims of brain-LLM isomorphism. The theater ratio
 *   (0.58) reflects that much of the analogy's rhetorical force comes from
 *   superficial structural parallels (layers in cortex ↔ layers in
 *   transformer) that may not reflect deep computational homology. Funding
 *   rhetoric emphasizes brain-LLM alignment more than funded research
 *   systematically tests the correspondence.
 *
 * KEY AGENTS:
 *   - LLM Research Community: Primary beneficiary (institutional/arbitrage) — gains legitimacy, funding, and collaborative access to neuroscience data by framing deep learning as brain-like
 *   - Neuroscience Funding Institutions: Secondary beneficiary (institutional/arbitrage) — can justify funding to both neuroscience and AI communities using the shared conceptual framework
 *   - Embodied Cognition Researchers: Primary victim (powerless/trapped) — marginalized in funding competitions, editorial decisions, and field prestige hierarchies; cannot exit without abandoning career investment
 *   - Interpretability Coalition: Organized secondary actor (organized/constrained) — sees brain-LLM analogy as a useful temporary scaffold for mechanistic understanding but recognizes it will be superseded
 *   - Neuroscientific Pluralists: Mixed secondary actor (moderate/constrained) — constrained by funding structures privileging brain-LLM mapping, but also benefit from shared conceptual frameworks
 *   - Analytical Observer: Civilizational perspective — risks naturalizing a contingent analogy into a law of cognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(layered_brain_processing, 0.38).
domain_priors:suppression_score(layered_brain_processing, 0.42).
domain_priors:theater_ratio(layered_brain_processing, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(layered_brain_processing, extractiveness, 0.38).
narrative_ontology:constraint_metric(layered_brain_processing, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(layered_brain_processing, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(layered_brain_processing, tangled_rope).
narrative_ontology:human_readable(layered_brain_processing, "Layered Contextual Meaning Construction in Brain-LLM Isomorphism").
narrative_ontology:topic_domain(layered_brain_processing, "neuroscience/machine_learning/cognition").

domain_priors:requires_active_enforcement(layered_brain_processing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(layered_brain_processing, llm_researchers).
narrative_ontology:constraint_beneficiary(layered_brain_processing, neuroscience_funders).
narrative_ontology:constraint_beneficiary(layered_brain_processing, cognitive_model_developers).
narrative_ontology:constraint_victim(layered_brain_processing, interpretability_researchers).
narrative_ontology:constraint_victim(layered_brain_processing, embodied_cognition_community).
narrative_ontology:constraint_victim(layered_brain_processing, neuroscientific_pluralism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMBODIED COGNITION COMMUNITY (SNARE) — Trapped within a field dominated by the brain-as-LLM analogy. Cannot exit without abandoning funding and publication venues. Bears full cost of suppressed alternative hypotheses (motor theories, predictive coding, 4E cognition). d≈0.92, f(d)≈1.39, σ=1.2 → χ≈0.65.
constraint_indexing:constraint_classification(layered_brain_processing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NEUROSCIENTIFIC PLURALISTS (TANGLED ROPE) — Constrained by funding structures and journal editorial gates that privilege brain-LLM mapping, but also benefit from the coordination function of having a shared model architecture for cross-disciplinary dialogue. d≈0.68, f(d)≈1.08, σ=1.0 → χ≈0.41.
constraint_indexing:constraint_classification(layered_brain_processing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LLM RESEARCH COMMUNITY (ROPE) — Institutional beneficiary with arbitrage exit. Benefits from brain-LLM isomorphism claims through increased funding, legitimacy, and collaborative partnerships with neuroscience. Experiences the constraint as coordination: a shared conceptual framework enables cross-disciplinary funding and co-authored papers. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(layered_brain_processing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERPRETABILITY COALITION (SCAFFOLD) — Organized agents (Anthropic, mechanistic interpretability researchers, neural circuit analysis groups) see the brain-LLM mapping as a temporary scaffold: it provides a useful conceptual bridge (neurons ↔ tokens, layers ↔ brain regions) for building interpretability methods. But they recognize the mapping as provisional — real brain circuits will diverge from transformer layers as understanding deepens. d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.22. Has sunset clause: as mechanistic understanding of both brains and LLMs advances, the analogy will be superseded by precise structural-functional mappings.
constraint_indexing:constraint_classification(layered_brain_processing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NEUROSCIENCE FUNDING INSTITUTIONS (PITON) — Universities, NIH, NSF maintain the brain-LLM analogy primarily through institutional inertia and theatrical compliance: it justifies funding allocation between neuroscience and AI labs, enables interdisciplinary grant programs, and provides convenient conceptual framing in grant reviews. The actual verification of layer-by-layer correspondence between brain and LLM is low-intensity. theater_ratio=0.58 reflects that funding rhetoric emphasizes the isomorphism more than funded research actually tests it. d≈0.12, f(d)≈-0.05, σ=1.0 → χ≈-0.003.
constraint_indexing:constraint_classification(layered_brain_processing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the brain-LLM constraint is a genuine coordination mechanism (shared conceptual vocabulary enabling collaboration) PLUS an asymmetric extraction (suppressing alternative cognitive architectures and embodiment-centered approaches). The constraint exhibits both properties simultaneously. d≈0.52, f(d)≈0.67, σ=1.0 → χ≈0.26.
constraint_indexing:constraint_classification(layered_brain_processing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(layered_brain_processing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(layered_brain_processing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(layered_brain_processing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(layered_brain_processing, TR),
    TR >= 0.70.

:- end_tests(layered_brain_processing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The brain-LLM analogy provides genuine coordination benefits (shared vocabulary, cross-disciplinary collaboration, concrete testable models) that offset some of the extractive effect. But career and funding incentives do asymmetrically favor researchers who adopt the brain-LLM framing over those pursuing alternative approaches. The extractiveness is not as severe as early observations suggested (0.55+) because the analogy is not solely extractive — it genuinely improves interdisciplinary communication. Suppression (0.42): Moderate. Alternative frameworks are not explicitly banned, but they face barriers: funding agencies prefer brain-LLM proposals, journals favor work framed in terms of the analogy, hiring committees expect familiarity with transformer architectures. Embodied cognition still has publication venues and some funding, so suppression is not total. Theater ratio (0.58): Moderate-high. Much of the public case for brain-LLM isomorphism rests on surface analogies (layers, sequential processing) that are rhetorically compelling but may not reflect computational depth. Funding documents and grant rhetoric emphasize the analogy more intensely than actual funded research tests it. The theater has increased over the 6-year interval as the analogy became fashionable — early work was more careful about caveats; recent work often treats the isomorphism as established fact.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a genuine perspectival divergence between institutional beneficiaries and powerless dissenters. The LLM research community sees rope (pure coordination) — the brain-LLM mapping solves the real problem of interdisciplinary communication. The interpretability coalition sees a temporary scaffold — the analogy is useful for now but will be superseded. The embodied cognition researcher sees snare (pure extraction) — trapped in a field that has decided their research questions are obsolete, unable to access funding or publication venues without adopting the brain-LLM framing. The analytical observer (civilizational scope) sees tangled rope — genuine coordination plus real asymmetric extraction of suppressed alternatives. The neuroscience funding institution sees a piton (theatrical maintenance of institutional arrangements) — the analogy justifies funding allocations between labs without deeply investigating whether the analogy is true. The pluralist neuroscientist is caught between: the constraint enables collaboration they want, but suppresses research directions they think are important.
 *
 * DIRECTIONALITY LOGIC:
 *   LLM research community: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Neuroscience funding institutions: Beneficiary + arbitrage → d≈0.12, f(d)≈-0.05. Net beneficiary (though with piton character). Embodied cognition researchers: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction — they cannot easily exit the field or adopt alternative framing without career costs. Neuroscientific pluralists: Victim + constrained → d≈0.68, f(d)≈1.08. Significant extraction but not maximal; they have some funding channels and publication venues outside the brain-LLM framework. Interpretability coalition: Organized + constrained → d≈0.45, f(d)≈0.48. Low-moderate extraction; coalition has some agency (Anthropic, academic labs) and sees a path beyond the analogy. Analytical observer: analytical → d≈0.52, f(d)≈0.67. Tangled rope is the proper high-level classification — the constraint is genuinely hybrid.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY UNRESOLVED (claimed_type = tangled_rope, ε=0.38 < 0.70). The constraint exhibits both coordination and extraction properties, justifying Tangled Rope classification. However, the unresolved mandatrophy concerns whether the coordination function is genuine or performative. IF the brain-LLM isomorphism turns out to be fundamentally false (omega variables 1-4 resolve against homology), the 'coordination' will have been theater all along — the constraint would reclassify as Snare. Conversely, IF the isomorphism is deeply valid (omegas resolve in favor of homology), the extraction will have been a temporary side effect of rapid scientific fashion, not a permanent feature — the constraint might reclassify as Rope as the field matures and alternative frameworks are vindicated on their own merits rather than suppressed. The Tangled Rope classification holds only if: (a) the isomorphism provides real coordination benefits while remaining unproven, AND (b) suppression of alternatives is real. Once the empirical questions resolve, the constraint will either collapse (Snare, if coordination was fake) or stabilize (Rope, if coordination was genuine and extraction is incidental).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    layer_correspondence_mechanism,
    'Do brain layers (cortical strata) actually process information serially like transformer layers, or is cortical processing massively parallel with feedback and lateral interactions that transformer architectures don''t capture?',
    'High-resolution neural recording + optogenetic mapping of information flow across cortical layers during natural language processing; comparison of latency and parallelism with sequential LLM token processing',
    'If sequential: brain-LLM mapping is valid; Rope/Scaffold perspectives strengthen. If parallel with rich feedback: the analogy is superficial; embodied and predictive coding perspectives become viable alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(layer_correspondence_mechanism, empirical, 'Whether cortical layers process information sequentially like transformer layers or in parallel with feedback').

omega_variable(
    embodied_grounding_necessity,
    'Is semantic meaning in the brain fundamentally grounded in sensorimotor systems (hands, mouth, proprioception), or can it be constructed from statistical patterns in language alone, as LLMs demonstrate?',
    'Neuroscience: studies of aphasia patients with sensorimotor loss; fMRI correlation between semantic processing and motor/sensory cortex. Behavioral: comparison of semantic understanding in individuals with congenital sensory/motor absence vs typical population.',
    'If grounding is necessary: brain-LLM isomorphism is false; embodied cognition vindicated, and the constraint represents extraction (suppressing embodiment research). If statistical patterns suffice: brain-LLM mapping is more plausible; the constraint represents genuine coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(embodied_grounding_necessity, empirical, 'Whether semantic meaning requires sensorimotor grounding or can be constructed from statistics alone').

omega_variable(
    scaling_law_universality,
    'Do brain scaling laws (e.g., neural population size vs cognitive capacity) follow the same power-law relationships as LLM scaling laws, or are they domain-specific?',
    'Comparative scaling analysis: cortical neuron count vs cognitive performance across species; LLM parameter count vs benchmark performance. Meta-analysis of existing literature on brain-behavior scaling.',
    'If universal scaling: profound isomorphism; brain-LLM analogy becomes foundational rather than contingent. If domain-specific: scaling laws are independent; the constraint is a convenient but brittle mapping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaling_law_universality, empirical, 'Whether brain and LLM scaling laws are universally similar or domain-specific').

omega_variable(
    attention_versus_salience,
    'Does transformer attention (learned weighted sum of token representations) actually model biological attention mechanisms, or is it a fundamentally different computational primitive?',
    'Neuroscience: comparison of attention-modulated neural activity patterns with transformer attention weight distributions using large-scale neural recordings. Psychophysics: test whether attention dynamics predicted by transformer models match behavioral saliency and detection thresholds.',
    'If genuine homology: strengthens brain-LLM mapping; attention becomes a shared primitive. If superficial analogy: analogy is restricted to high-level architecture; local computation mechanisms diverge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_versus_salience, empirical, 'Whether transformer attention models biological attention mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(layered_brain_processing, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lbp_tr_t0, layered_brain_processing, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lbp_tr_t3, layered_brain_processing, theater_ratio, 3, 0.48).
narrative_ontology:measurement(lbp_tr_t6, layered_brain_processing, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(lbp_be_t0, layered_brain_processing, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(lbp_be_t3, layered_brain_processing, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(lbp_be_t6, layered_brain_processing, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(layered_brain_processing, information_standard).
narrative_ontology:affects_constraint(layered_brain_processing, transformer_interpretability_bottleneck).
narrative_ontology:affects_constraint(layered_brain_processing, embodied_cognition_marginalization).
narrative_ontology:affects_constraint(layered_brain_processing, neuroscience_funding_concentration).

% DUAL FORMULATION NOTE:
% The brain-LLM constraint decomposes into three structurally distinct claims: (1) cortical layers process information serially like transformer layers (ε≈0.15, Mountain candidate); (2) transformer attention mechanisms model biological attention (ε≈0.28, Rope candidate); (3) LLM scaling laws predict brain scaling laws (ε≈0.42, Tangled Rope candidate). The present story treats these as a unified constraint (ε=0.38) because they are bundled rhetorically in funding and research discourse. As empirical work separates these claims, separate stories may be warranted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(layered_brain_processing, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
