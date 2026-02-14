% ============================================================================
% CONSTRAINT STORY: english_chinese_tense_structure
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-11
% ============================================================================

:- module(constraint_english_chinese_tense_structure, []).

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
    domain_priors:emerges_naturally/1,
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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: english_chinese_tense_structure
 *   human_readable: Obligatory Tense Marking vs. Aspect-Context Encoding
 *   domain: linguistic/cognitive
 *
 * SUMMARY:
 *   English grammar obligates speakers to encode temporal reference through
 *   verb morphology (past -ed, progressive -ing, will/shall for future).
 *   Mandarin Chinese lacks grammatical tense entirely, using instead aspect
 *   markers (le, zhe, guo), temporal adverbs, and pragmatic context to
 *   convey time reference. This is a structural feature of each language's
 *   grammar — not a policy, convention, or institution. Speakers of each
 *   language must comply with these structural requirements to produce
 *   well-formed utterances, but no agent extracts benefit from this
 *   compliance. The constraint is symmetric across all power positions.
 *
 *   Cognitive implications: English speakers are forced to attend to temporal
 *   distinctions on every verb phrase. Chinese speakers can leave temporal
 *   reference implicit when context suffices, directing cognitive resources
 *   toward aspect, discourse flow, and topic-comment organization. Research
 *   by Chen (2013), Boroditsky (2001), and others suggests this shapes
 *   habitual attention patterns — not determining thought, but channeling
 *   default cognitive allocation.
 *
 * KEY AGENTS (by structural relationship):
 *   - English speakers: Subject to obligatory tense marking (any power/any exit)
 *   - Chinese speakers: Subject to aspect-context encoding (any power/any exit)
 *   - Comparative linguists: Analytical observers — see both systems
 *
 * UNIFORM-TYPE JUSTIFICATION:
 *   This is a natural-language structural constraint. Grammatical tense
 *   obligatoriness is not something any agent designed, benefits from
 *   asymmetrically, or can opt out of while remaining a competent speaker.
 *   It classifies as Mountain from every perspective. No enrichment needed.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(english_chinese_tense_structure, 0.05).
domain_priors:suppression_score(english_chinese_tense_structure, 0.02).
domain_priors:theater_ratio(english_chinese_tense_structure, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(english_chinese_tense_structure, extractiveness, 0.05).
narrative_ontology:constraint_metric(english_chinese_tense_structure, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(english_chinese_tense_structure, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
narrative_ontology:constraint_metric(english_chinese_tense_structure, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(english_chinese_tense_structure, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(english_chinese_tense_structure, mountain).

% --- Emergence flag (required for mountain constraints) ---
% Required for the mountain metric gate: without this, the
% classify_from_metrics mountain clause will not fire.
domain_priors:emerges_naturally(english_chinese_tense_structure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context/4 terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% UNIFORM-TYPE: Mountain from all perspectives.
% Grammatical tense systems are structural properties of natural language.
% No agent designed them, no agent extracts from them asymmetrically,
% no agent can opt out while remaining a competent speaker.

% PERSPECTIVE 1: INDIVIDUAL SPEAKER (native, limited metalinguistic awareness)
% Even a powerless speaker with no exit experiences this as natural law.
% The grammar IS the medium; you cannot speak English without marking tense.
constraint_indexing:constraint_classification(english_chinese_tense_structure, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LANGUAGE POLICY INSTITUTION
% National academies and education ministries cannot repeal tense marking.
% They can standardize dialects, but the tense/aspect typological distinction
% predates and outlasts any institution.
constraint_indexing:constraint_classification(english_chinese_tense_structure, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: COMPARATIVE LINGUIST (analytical observer)
% At civilizational timescale, typological features like tense obligatoriness
% are among the slowest-changing properties of language. The English/Chinese
% split on this dimension predates written records in both traditions.
constraint_indexing:constraint_classification(english_chinese_tense_structure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(english_chinese_tense_structure_tests).

test(uniform_mountain) :-
    % All perspectives classify as mountain — verify uniformity.
    forall(
        constraint_indexing:constraint_classification(
            english_chinese_tense_structure, Type, _),
        Type = mountain
    ).

test(threshold_validation) :-
    narrative_ontology:constraint_metric(
        english_chinese_tense_structure, extractiveness, E),
    E =< 0.25.

:- end_tests(english_chinese_tense_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.05: The tense/aspect typological difference extracts
 *   nothing from anyone. It is a structural feature of how each language
 *   encodes temporal reference. English speakers pay a small cognitive cost
 *   (obligatory temporal attention on every clause) and Chinese speakers pay
 *   a different cost (heavier reliance on pragmatic inference for time
 *   reference). Neither cost constitutes extraction in the economic sense —
 *   there is no beneficiary of these costs.
 *
 *   Suppression 0.02: Speakers must comply with their language's grammar to
 *   be understood, but this is not coercive suppression — it is the enabling
 *   condition of communication itself. A speaker can learn the other language,
 *   but cannot "opt out" of tense marking while speaking English.
 *
 *   Theater ratio 0.05: There is no performative maintenance cost. The
 *   grammatical system operates automatically in fluent speakers.
 *
 *   NL Profile: Accessibility Collapse is 1.0 because it is impossible to
 *   speak well-formed English without marking tense. Resistance is 0.0 as
 *   there is no coherent way to resist a language's core grammar. The
 *   constraint emerges naturally from linguistic evolution.
 *
 * PERSPECTIVAL GAP:
 *   None. This is a uniform-type Mountain. The grammatical structure is
 *   equally constraining from every perspective. A powerless monolingual
 *   speaker and a language-policy institution face the same structural
 *   reality: English marks tense obligatorily; Chinese does not.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary or victim. The constraint is symmetric. The small
 *   nonzero ε (0.05) reflects the cognitive channeling effect documented
 *   in psycholinguistic research — habitual attention patterns differ
 *   between tense-obligatory and tense-optional language speakers — but
 *   this channeling does not transfer value asymmetrically.
 *
 * COGNITIVE-CULTURAL NOTE:
 *   English obligatory tense marking forces speakers to temporally anchor
 *   every predication. This may correlate with cultural tendencies toward
 *   linear temporal framing, explicit scheduling, and future-discounting
 *   patterns (Chen 2013). Chinese aspect-context encoding allows temporal
 *   ambiguity, correlating with more flexible temporal framing and what
 *   some researchers describe as a more holistic integration of past,
 *   present, and future in discourse. These are tendencies, not
 *   determinisms — the constraint shapes the cognitive default, not the
 *   cognitive ceiling.
 *
 * MANDATROPHY ANALYSIS:
 *   Mountain classification prevents two mislabelings:
 *   (1) Treating the structural difference as a Rope (coordination device) —
 *       it was not designed for coordination; it emerged through millennia of
 *       unplanned grammatical drift.
 *   (2) Treating it as a Snare (extraction device) — no agent exploits this
 *       structural feature to extract from speakers. The cognitive channeling
 *       effect is a side effect, not a designed extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_tense_cognitive_depth,
    'Does obligatory tense marking produce a measurably deeper cognitive channeling effect than aspect-context encoding, or are the documented differences primarily experimental artifacts of task design?',
    'Longitudinal neuroimaging studies comparing temporal cognition in balanced bilinguals vs monolinguals across tense-marking and tense-optional languages, controlling for literacy and education effects.',
    'If true: the 0.05 epsilon may understate the cognitive structuring effect, though Mountain classification holds regardless because the effect is symmetric (no asymmetric extraction). If false: epsilon is accurate; the structural difference is even more purely formal.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(english_chinese_tense_structure, 0, 10).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Network relationships — this mountain is the structural foundation
% that the linguistic relativity framing constraint builds upon.
% The empirical fact of structural difference is cited as evidence for
% the cultural-cognitive claims in the downstream story.

% DUAL FORMULATION NOTE:
% This constraint is one of 2 stories decomposed from "English-Chinese
% linguistic structure and its cognitive-cultural effects."
% Decomposed because ε differs across observables (ε-invariance principle).
% The structural fact (this story, ε=0.05) is Mountain — nobody extracts
% from grammatical typology. The framing of that fact for cultural
% conclusions (sibling story, ε=0.42) is Tangled Rope — genuine research
% coordination mixed with asymmetric cultural extraction.
% Related stories:
%   - linguistic_relativity_cultural_framing (ε=0.42, Tangled Rope)

narrative_ontology:affects_constraint(english_chinese_tense_structure, linguistic_relativity_cultural_framing).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */