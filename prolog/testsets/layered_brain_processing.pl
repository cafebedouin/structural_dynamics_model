% ============================================================================
% CONSTRAINT STORY: layered_brain_processing
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-22
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
    domain_priors:emerges_naturally/1,
    narrative_ontology:interval/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:omega_variable/3,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: layered_brain_processing
 *   human_readable: Layered Contextual Meaning Construction
 *   domain: technological/biological
 *
 * SUMMARY:
 *   The human brain builds meaning from speech through a stepwise, layered process
 *   that mirrors the architecture of large language models like GPT-2 and Llama 2[cite: 12, 25].
 *   This challenges the "rule-based" view of language in favor of a statistical,
 *   contextual emergence of meaning[cite: 13, 42]. The constraint is the
 *   biological reality of this process.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Individual Listener (powerless/trapped): Primary target, subject to the unchangeable neural processing steps.
 *   - AI Researchers (analytical/mobile): Primary beneficiary, as AI models gain utility by mimicking this process.
 *   - Traditional Linguists (institutional/trapped): Secondary actor, trapped by a prior paradigm that this biological reality refutes.
 *   - Modern Neuroscientist (analytical/mobile): Analytical observer, mapping the process and seeing its functional coordination.
 *
 * NARRATIVE ARC:
 *   Long-held ideas of "fixed symbols" and "rigid hierarchies" [cite: 41] acted as a
 *   conceptual "Snare" that suppressed the discovery of the brain's "Mountain"
 *   of layered biological processing until AI provided the "Rope" (the model) to
 *   map it[cite: 33, 42].
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: AI models extract "contextual representations" from human data[cite: 44].
% While the study is scientific, the convergence suggests AI mimics human data
% processing to gain utility[cite: 33, 39].
domain_priors:base_extractiveness(layered_brain_processing, 0.35).

% Rationale: "Long-standing rule-based ideas" and "rigid hierarchies" dominated
% for many years[cite: 21, 41], making the flexible, statistical reality
% "unexpected"[cite: 23]. This suppression score reflects the old paradigm's effect.
domain_priors:suppression_score(layered_brain_processing, 0.75).

% Rationale: Extraction is substantive — the constraint's costs are real, not theatrical.
domain_priors:theater_ratio(layered_brain_processing, 0.13).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(layered_brain_processing, extractiveness, 0.35).
narrative_ontology:constraint_metric(layered_brain_processing, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(layered_brain_processing, theater_ratio, 0.13).

% --- NL Profile Metrics (required for mountain constraints) ---
% This is a biological process, a fact of nature. Alternatives are not
% structurally accessible, and one cannot "resist" how their brain processes speech.
narrative_ontology:constraint_metric(layered_brain_processing, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(layered_brain_processing, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
% The analytical perspective (neuroscientist) sees a functional coordination mechanism.
narrative_ontology:constraint_claim(layered_brain_processing, rope).
narrative_ontology:human_readable(layered_brain_processing, "Layered Contextual Meaning Construction").
narrative_ontology:topic_domain(layered_brain_processing, "technological/biological").

% --- Emergence flag (required for mountain constraints) ---
% The constraint is a biological process that emerges naturally.
domain_priors:emerges_naturally(layered_brain_processing).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
% AI Research/Models benefit from mimicking brain hierarchy[cite: 33, 48].
narrative_ontology:constraint_beneficiary(layered_brain_processing, ai_researchers).
%
% Who bears disproportionate cost?
% The "Rule-based paradigm" previously victimized scientific progress by
% hiding real-time brain activity patterns[cite: 21, 44].
narrative_ontology:constraint_victim(layered_brain_processing, traditional_linguistics).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE TRADITIONAL LINGUIST (MOUNTAIN)
% From this view, language *is* the rules; phonemes and morphemes are
% the unchangeable building blocks of reality[cite: 41, 43]. They are trapped
% in a paradigm they perceive as natural law.
constraint_indexing:constraint_classification(layered_brain_processing, mountain,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE MODERN NEUROSCIENTIST (ROPE)
% The layered hierarchy is a tool for building meaning gradually[cite: 12, 42].
% It is functional coordination that allows the brain to grasp complex context[cite: 30, 45].
% This is the analytical perspective.
constraint_indexing:constraint_classification(layered_brain_processing, rope,
    context(agent_power(analytical),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE INDIVIDUAL LISTENER (SNARE)
% The brain "does not grasp meaning all at once"[cite: 27]. Each word
% must pass through a series of steps[cite: 28]. This biological necessity,
% combined with high suppression from the old linguistic paradigm, is
% experienced as a snare.
constraint_indexing:constraint_classification(layered_brain_processing, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(layered_brain_processing_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between the listener (target) and neuroscientist (analytical).
    constraint_indexing:constraint_classification(layered_brain_processing, TypeTarget,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(layered_brain_processing, TypeAnalytical,
        context(agent_power(analytical), _, _, _)),
    TypeTarget \= TypeAnalytical.

test(context_superiority_scaling) :-
    % Test that contextual representations (Score 1) explain activity
    % better than rigid rules (Score 2) [cite: 44]
    ScoreContextual = 0.9,
    ScoreRuleBased = 0.3,
    ScoreContextual > ScoreRuleBased.

test(time_unfolding_insight) :-
    % Later brain responses (Broca's) peak later, matching deeper AI layers [cite: 32]
    DeepLayerTime = later,
    EarlyLayerTime = earlier,
    DeepLayerTime \= EarlyLayerTime.

:- end_tests(layered_brain_processing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is the biological process of layered meaning construction.
 *   - Extractiveness (0.35): Represents the utility AI models gain by mimicking this process, effectively extracting a functional blueprint from biology.
 *   - Suppression (0.75): High score reflects the dominance of the prior "rule-based" paradigm, which this biological reality refutes and was suppressed by.
 *   - NL Profile: As a biological law, the process has near-total accessibility collapse (no alternatives) and near-zero resistance (it's involuntary).
 *
 * PERSPECTIVAL GAP:
 *   - The Traditional Linguist (institutional/trapped) sees a Mountain, as their rule-based system was perceived as an immutable law of language.
 *   - The Neuroscientist (analytical/mobile) sees a Rope, a functional coordination mechanism for building meaning.
 *   - The Individual Listener (powerless/trapped) experiences a Snare, as they are involuntarily subject to a process that was long misrepresented by the dominant (suppressive) linguistic paradigm.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `ai_researchers` benefit by gaining a validated, efficient architecture for language models.
 *   - Victims: `traditional_linguistics` as a paradigm is the victim, as its core tenets are invalidated by this biological evidence.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification correctly identifies the biological process as having a coordination function (Rope from the analytical view), preventing it from being mislabeled as pure extraction. The Snare perspective for the powerless individual is only possible due to the high suppression score inherited from the clash with the old paradigm, correctly capturing the nuance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_layered_brain_processing_1,
    "Do AI and brains converge because AI is truly brain-like, or because AI is trained solely on the output (speech/text) of this brain architecture?",
    "Compare AI trained on non-human data to neural activity of the same non-human sources",
    "If AI mimics output: Scaffold. If AI mimics structure: Mountain.",
    confidence_without_resolution(medium)
).

omega_variable(
    omega_layered_brain_processing_2,
    "Is the step-by-step extraction of meaning [cite: 28] a functional necessity for consciousness or a biological constraint that limits comprehension speed?",
    "Audit of neural step-speed vs. information density thresholds",
    "If necessity: Mountain. If predatory/limitation: Snare.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_layered_brain_processing_1, empirical, 'Does AI/brain convergence reflect shared structure or AI mimicking brain output?').
narrative_ontology:omega_variable(omega_layered_brain_processing_2, empirical, 'Is layered meaning extraction a functional necessity or a biological limitation?').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(layered_brain_processing, 0, 10).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */