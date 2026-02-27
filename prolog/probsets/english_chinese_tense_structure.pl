% ============================================================================
% CONSTRAINT STORY: english_chinese_tense_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
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
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

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
 *   English relies on obligatory tense marking, while Chinese encodes
 *   temporal information through aspect and context. This difference
 *   represents distinct but functional solutions to encoding time in
 *   language. Both strategies have benefits and drawbacks in terms of
 *   explicitness, cognitive load, and cross-linguistic communication.
 *
 * KEY AGENTS:
 *   - English Speakers: Benefit from explicit tense marking (moderate/mobile)
 *   - Chinese Speakers: Benefit from aspect-context encoding (moderate/mobile)
 *   - Linguists/Cognitive Scientists: Analyze the trade-offs between the strategies (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(english_chinese_tense_structure, 0.05).
domain_priors:suppression_score(english_chinese_tense_structure, 0.25).
domain_priors:theater_ratio(english_chinese_tense_structure, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(english_chinese_tense_structure, extractiveness, 0.05).
narrative_ontology:constraint_metric(english_chinese_tense_structure, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(english_chinese_tense_structure, theater_ratio, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(english_chinese_tense_structure, rope).
narrative_ontology:human_readable(english_chinese_tense_structure, "Obligatory Tense Marking vs. Aspect-Context Encoding").
narrative_ontology:topic_domain(english_chinese_tense_structure, "linguistic/cognitive").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(english_chinese_tense_structure, english_speakers).
narrative_ontology:constraint_beneficiary(english_chinese_tense_structure, chinese_speakers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% English speakers benefit from explicit tense marking, which aids in disambiguation and reduces cognitive load during comprehension. They are mobile, as they can switch to other languages or adapt to simplified forms, but standard English requires tense marking.
constraint_indexing:constraint_classification(english_chinese_tense_structure, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Chinese speakers benefit from the aspect-context encoding strategy, which allows for flexibility and avoids unnecessary grammatical complexity. They are mobile because they can learn languages with explicit tense marking.
constraint_indexing:constraint_classification(english_chinese_tense_structure, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% From an analytical perspective, both strategies represent efficient solutions to the problem of encoding temporal information in language. This perspective views the differences as arising from different evolutionary trajectories and cognitive biases, ultimately resulting in a form of coordinated efficiency. There is a slight coordination cost in cross-linguistic communication.
constraint_indexing:constraint_classification(english_chinese_tense_structure, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(english_chinese_tense_structure_tests).
:- end_tests(english_chinese_tense_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.05): Very Low. There is a minor extraction cost due to increased cognitive load in cross-linguistic communication, but it is minimal. Suppression (0.25): Low. Both languages are effective in their respective contexts. Theater Ratio (0.10): Very low. These linguistic structures serve their purpose effectively and efficiently.
 *
 * PERSPECTIVAL GAP:
 *   The perspective gap stems from the different encoding strategies. English speakers might find the Chinese approach ambiguous without explicit tense, while Chinese speakers might perceive English as verbose and redundant.
 *
 * DIRECTIONALITY LOGIC:
 *   Both English and Chinese speakers benefit from their respective linguistic structures. Linguists and cognitive scientists can analyze the trade-offs impartially.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that both strategies are functional solutions to encoding time, each with its own set of trade-offs. The classification as rope reflects the coordinated efficiency of both systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(english_chinese_tense_structure, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(english_chinese_tense_structure, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
