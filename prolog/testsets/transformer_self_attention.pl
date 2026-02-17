% ============================================================================
% CONSTRAINT STORY: transformer_self_attention
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_transformer_self_attention, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: transformer_self_attention
 * human_readable: The Transformer Self-Attention Architecture
 * domain: technological
 * * SUMMARY:
 * The transformer architecture is the foundation for modern large language models, replacing recurrent neural networks (RNNs) with a "self-attention" mechanism. This allows models to weigh the importance of all words in a sequence simultaneously, enabling massive performance gains by leveraging internet-scale data. This architectural dominance creates a powerful coordination standard but also displaces prior technologies and centralizes computational requirements.
 * * KEY AGENTS:
 * - Legacy RNN Developers: The subjects, whose technology was rendered obsolete (Powerless).
 * - Modern AI Researchers/Companies: The beneficiaries, who can leverage the architecture for unprecedented capabilities (Institutional).
 * - Systems Auditor: An analytical observer evaluating the architecture's structural properties (Analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(transformer_self_attention, 0.75). % High: Enables massive value extraction from public data.
domain_priors:suppression_score(transformer_self_attention, 0.40).   % Moderate: Displaced RNNs through superior performance, not explicit prohibition.
domain_priors:theater_ratio(transformer_self_attention, 0.05).       % Low: The architecture is highly functional, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(transformer_self_attention, extractiveness, 0.75).
narrative_ontology:constraint_metric(transformer_self_attention, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(transformer_self_attention, theater_ratio, 0.05).

% Constraint self-claim (what does the constraint claim to be?)
% It presents itself as a superior coordination mechanism for processing information.
narrative_ontology:constraint_claim(transformer_self_attention, tangled_rope).
narrative_ontology:human_readable(transformer_self_attention, "The Transformer Self-Attention Architecture").

% Binary flags
domain_priors:requires_active_enforcement(transformer_self_attention). % Requires massive computational infrastructure and data pipelines to maintain dominance.

% Structural property derivation hooks for Tangled Rope classification:
narrative_ontology:constraint_beneficiary(transformer_self_attention, modern_ai_companies).
narrative_ontology:constraint_victim(transformer_self_attention, legacy_rnn_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For developers invested in prior architectures (RNNs), the transformer is a Snare.
% Its dominance makes their skills and tools obsolete, trapping them in a non-competitive paradigm.
% χ = 0.75 (ε) * 1.5 (π(powerless)) * 1.0 (σ(national)) = 1.125
constraint_indexing:constraint_classification(transformer_self_attention, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For AI companies and researchers, the architecture is a pure Rope—an immensely powerful
% coordination tool that unlocks new capabilities and commercial opportunities.
% χ = 0.75 (ε) * -0.2 (π(institutional)) * 1.2 (σ(global)) = -0.18
constraint_indexing:constraint_classification(transformer_self_attention, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analytically, the architecture is a Tangled Rope. It provides a genuine, powerful
% coordination function (Rope aspect) but also involves massive asymmetric extraction
% of value from public data and displaces alternative approaches through its de facto
% standardization, which requires active enforcement (Snare aspect).
constraint_indexing:constraint_classification(transformer_self_attention, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transformer_self_attention_tests).

test(perspectival_gap_is_snare_vs_rope) :-
    constraint_indexing:constraint_classification(transformer_self_attention, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transformer_self_attention, rope,
        context(agent_power(institutional), _, _, _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(transformer_self_attention, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(high_extraction_threshold_met) :-
    narrative_ontology:constraint_metric(transformer_self_attention, extractiveness, E),
    E >= 0.46.

:- end_tests(transformer_self_attention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness (0.75) is high because the transformer architecture's primary innovation
 * is its ability to effectively extract signal and value from internet-scale datasets, a previously
 * intractable task. The suppression score (0.40) is moderate, as it outcompeted rather than banned
 * prior methods like RNNs.
 *
 * This creates a classic Perspectival Gap. For beneficiaries (AI labs), it is a pure Rope, a tool
 * of immense power and coordination. For those invested in the displaced technology (RNNs), it is a
 * Snare that renders their expertise and infrastructure obsolete.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The system correctly classifies this as a Tangled Rope from an analytical perspective. A simpler
 * model might misclassify it as a pure Rope (ignoring the displacement and centralization effects)
 * or a pure Snare (ignoring its genuine and profound coordination benefits). The Tangled Rope
 * classification acknowledges that it is both a powerful coordination tool and a source of
 * asymmetric extraction and coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_transformer_self_attention_1,
    'Is the high extractiveness from public data a functional necessity for intelligence (Rope), or a predatory aggregation of global human labor (Snare)?',
    'Audit of model performance using curated small datasets vs. uncurated internet scrapes.',
    'If necessity, the constraint leans Mountain. If predatory, it is a pure Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transformer_self_attention, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The transformer architecture started with high utility and has only become more
% extractively efficient over time. The theater ratio remains low as it is a core,
% functional technology.
%
% Theater ratio over time (stable and low):
narrative_ontology:measurement(tsa_tr_t0, transformer_self_attention, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tsa_tr_t5, transformer_self_attention, theater_ratio, 5, 0.05).
narrative_ontology:measurement(tsa_tr_t10, transformer_self_attention, theater_ratio, 10, 0.05).

% Extraction over time (increasing as models and datasets scaled):
narrative_ontology:measurement(tsa_ex_t0, transformer_self_attention, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(tsa_ex_t5, transformer_self_attention, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(tsa_ex_t10, transformer_self_attention, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The transformer architecture functions as a de facto standard for information processing.
narrative_ontology:coordination_type(transformer_self_attention, information_standard).

% The dominance and computational hunger of this architecture directly influence
% the constraints and demands on the semiconductor supply chain.
narrative_ontology:affects_constraint(transformer_self_attention, semiconductor_supply).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */