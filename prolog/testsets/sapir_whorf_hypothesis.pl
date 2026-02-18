% ============================================================================
% CONSTRAINT STORY: sapir_whorf_hypothesis
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_sapir_whorf_hypothesis, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: sapir_whorf_hypothesis
 * human_readable: Sapir-Whorf Hypothesis (Linguistic Relativity)
 * domain: social/cognitive
 * * SUMMARY:
 * The Sapir-Whorf hypothesis proposes that the structure of a language affects its speakers' worldview or cognition. In its "strong" form (linguistic determinism), language limits what can be thought; in its "weak" form (linguistic relativity), it merely influences perception. This constraint models the social and cognitive effects of an agent's native language.
 * * KEY AGENTS:
 * - Monolingual Speaker: Subject (Powerless), trapped within their language's conceptual framework.
 * - Propagandist/Ideologue: Beneficiary (Institutional), who weaponizes language to shape thought.
 * - Linguist/Cognitive Scientist: Auditor (Analytical), who observes the structure and its effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(sapir_whorf_hypothesis, 0.55). % Moderate. Language extracts "unlabeled" thoughts by forcing cognition into predefined semantic ruts, taxing thoughts that fall outside the norm.
domain_priors:suppression_score(sapir_whorf_hypothesis, 0.70).   % High. What is not easily named is often not noticed, making alternative conceptual frameworks invisible or difficult to access.
domain_priors:theater_ratio(sapir_whorf_hypothesis, 0.10).       % Low. Language is overwhelmingly functional, not theatrical.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(sapir_whorf_hypothesis, extractiveness, 0.55).
narrative_ontology:constraint_metric(sapir_whorf_hypothesis, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(sapir_whorf_hypothesis, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Language presents itself as a pure coordination tool for communication.
narrative_ontology:constraint_claim(sapir_whorf_hypothesis, tangled_rope).
narrative_ontology:human_readable(sapir_whorf_hypothesis, "Sapir-Whorf Hypothesis (Linguistic Relativity)").
narrative_ontology:topic_domain(sapir_whorf_hypothesis, "social/cognitive").

% Binary flags
% Language can be actively enforced (e.g., Newspeak, censorship) to maintain its extractive properties.
domain_priors:requires_active_enforcement(sapir_whorf_hypothesis).

% Structural property derivation hooks:
% Beneficiaries use language to coordinate and shape worldviews.
narrative_ontology:constraint_beneficiary(sapir_whorf_hypothesis, ideologues).
narrative_ontology:constraint_beneficiary(sapir_whorf_hypothesis, advertisers).
% Victims are those whose thoughts are constrained or whose cross-cultural communication is impaired.
narrative_ontology:constraint_victim(sapir_whorf_hypothesis, dissenters).
narrative_ontology:constraint_victim(sapir_whorf_hypothesis, cross_cultural_communicators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE MONOLINGUAL SUBJECT (SNARE)
% Experiences their native language as an inescapable reality, a trap for thought.
% χ = 0.55 (ε) * 1.5 (π(powerless)) * 1.0 (σ(national)) = 0.825. This is a clear Snare.
constraint_indexing:constraint_classification(sapir_whorf_hypothesis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE DIPLOMAT / PROPAGANDIST (ROPE)
% Views language as a tool for coordination and influence, leveraging its structure.
% χ = 0.55 (ε) * -0.2 (π(institutional)) * 1.1 (σ(continental)) = -0.121. This is a clear Rope.
constraint_indexing:constraint_classification(sapir_whorf_hypothesis, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the coordination function and the asymmetric extraction.
% The metrics (E=0.55, S=0.70), beneficiaries, victims, and enforcement requirement
% all point to a Tangled Rope from a neutral, analytical viewpoint.
% χ = 0.55 (ε) * 1.15 (π(analytical)) * 1.2 (σ(global)) = 0.759.
constraint_indexing:constraint_classification(sapir_whorf_hypothesis, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sapir_whorf_hypothesis_tests).

test(perspectival_gap) :-
    % Verify the gap between the powerless subject (Snare) and the institutional user (Rope).
    constraint_indexing:constraint_classification(sapir_whorf_hypothesis, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sapir_whorf_hypothesis, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(sapir_whorf_hypothesis, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_for_tangled_rope) :-
    % A Tangled Rope requires high extraction and suppression.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(sapir_whorf_hypothesis, ExtMetricName, E),
    narrative_ontology:constraint_metric(sapir_whorf_hypothesis, SuppMetricName, S),
    E >= 0.50,
    S >= 0.40.

test(tangled_rope_structural_requirements) :-
    % Verify all three structural properties for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(sapir_whorf_hypothesis, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(sapir_whorf_hypothesis, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(sapir_whorf_hypothesis).

:- end_tests(sapir_whorf_hypothesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Sapir-Whorf hypothesis is a classic example of a constraint whose nature
 * changes dramatically with perspective. For a monolingual individual, the limits
 * of their language are effectively the limits of their world (Snare). For an
 * institutional actor like a propagandist or diplomat, language is a powerful
 * coordination tool (Rope). The base extractiveness of 0.55 reflects the cognitive
 * "tax" language imposes by forcing fluid reality into discrete, lossy categories.
 * The high suppression score of 0.70 reflects how difficult it is to think a
 * concept for which one has no word.
 *
 * * MANDATROPHY ANALYSIS:
 * The analytical classification as a Tangled Rope is critical. A simpler model
 * might see the high suppression and extraction and label language a pure Snare,
 * missing its essential, world-building coordination function. By recognizing
 * that it has both beneficiaries (coordination) and victims (asymmetric extraction)
 * and requires enforcement to maintain its ideological purity (e.g., Newspeak),
 * the Tangled Rope classification provides a much richer and more accurate model,
 * resolving the mandatrophy of seeing only the trap and not the tool.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_sapir_whorf,
    'Does language truly prevent thoughts (strong determinism) or merely make some thoughts easier than others (weak relativity)?',
    'Long-term neuro-linguistic studies on speakers of languages without specific numeric or spatial markers.',
    'If strong determinism is true, the constraint is a hard Snare/Mountain. If weak relativity is true, it is a malleable Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(sapir_whorf_hypothesis, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models how language can be instrumentalized over time, increasing
% its extractive properties as it is weaponized for political or commercial ends.
% Base extractiveness > 0.46, so this is required.

% Theater ratio over time (remains low and stable):
narrative_ontology:measurement(swh_tr_t0, sapir_whorf_hypothesis, theater_ratio, 0, 0.10).
narrative_ontology:measurement(swh_tr_t5, sapir_whorf_hypothesis, theater_ratio, 5, 0.10).
narrative_ontology:measurement(swh_tr_t10, sapir_whorf_hypothesis, theater_ratio, 10, 0.10).

% Extraction over time (models increasing instrumentalization):
narrative_ontology:measurement(swh_ex_t0, sapir_whorf_hypothesis, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(swh_ex_t5, sapir_whorf_hypothesis, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(swh_ex_t10, sapir_whorf_hypothesis, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Language is the quintessential information standard.
narrative_ontology:coordination_type(sapir_whorf_hypothesis, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */