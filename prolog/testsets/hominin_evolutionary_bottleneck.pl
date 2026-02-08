% ============================================================================
% CONSTRAINT STORY: hominin_evolutionary_bottleneck
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_hominin_evolutionary_bottleneck, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: hominin_evolutionary_bottleneck
 * human_readable: The Hominin Evolutionary Bottleneck & Replacement Event
 * domain: scientific/biological
 * * SUMMARY:
 * For millennia, the story of human evolution was constrained by a sparse fossil record.
 * Recent fossil and genetic data have revealed a complex history where our direct ancestors
 * expanded out of Africa ~60,000 years ago, replacing all other hominin species. This event,
 * and the scientific consensus describing it, acts as a constraint with vastly different
 * meanings depending on the perspective.
 * * KEY AGENTS:
 * - Extinct Hominin (e.g., H. naledi): Subject (Powerless)
 * - Modern Scientist: Beneficiary/Analyst (Analytical)
 * - Natural History Museum: Beneficiary (Institutional)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale: The 60,000-year-old expansion involved the total replacement of other hominin species.
% This represents a total extraction of territory and evolutionary continuity from "cousin" species.
domain_priors:base_extractiveness(hominin_evolutionary_bottleneck, 0.75).
% Rationale: The dominant "Out of Africa" theory, backed by strong evidence, has suppressed
% alternative "multiregional" hypotheses, making it difficult for them to gain traction.
domain_priors:suppression_score(hominin_evolutionary_bottleneck, 0.60).
% Rationale: The scientific process is functional, not theatrical. The presentation in museums has some
% theatricality, but the core constraint is the evidence itself.
domain_priors:theater_ratio(hominin_evolutionary_bottleneck, 0.10).
domain_priors:requires_active_enforcement(hominin_evolutionary_bottleneck).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(hominin_evolutionary_bottleneck, extractiveness, 0.75).
narrative_ontology:constraint_metric(hominin_evolutionary_bottleneck, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(hominin_evolutionary_bottleneck, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The scientific consensus is a constructed framework based on evidence.
narrative_ontology:constraint_claim(hominin_evolutionary_bottleneck, rope).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(hominin_evolutionary_bottleneck, homo_sapiens).
narrative_ontology:constraint_victim(hominin_evolutionary_bottleneck, extinct_hominin_cousins).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For species like H. naledi, the evolutionary expansion of Homo sapiens was a terminal 'Snare'.
% They were confined to specific environments and ultimately driven to extinction by a competing group.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE BENEFICIARY (INSTITUTIONAL) - MOUNTAIN
% The museum presents the scientific consensus as a 'Mountain'. The complex, debated 'Rope'
% of the scientists is simplified into an immutable, factual diorama for public consumption.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER - ROPE
% For the scientist, the bottleneck and expansion is a 'Rope'—a powerful explanatory
% framework that ties together disparate data points into a coherent narrative about human origins.
constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hominin_evolutionary_bottleneck_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    % Verify the base extractiveness is in the high-extraction range for a Snare.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hominin_evolutionary_bottleneck, ExtMetricName, E),
    E >= 0.46.

test(multi_perspective_variance) :-
    % Verify that all three core perspectives yield different classifications.
    constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, Type2, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(hominin_evolutionary_bottleneck, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(hominin_evolutionary_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this constraint is the biological replacement of one group by another, which is a
 * high-extraction event (E=0.75). The perspectival gap is stark:
 * - For the extinct hominin (powerless), this is a Snare. Their evolutionary line was terminated.
 * - For the modern scientist (analytical), the theory explaining this event is a Rope—a powerful
 *   coordination tool for organizing vast amounts of genetic and fossil data into a coherent story.
 * - For the museum (institutional), this complex scientific Rope is simplified and presented to the
 *   public as an immutable fact—a Mountain. The nuance and debate are flattened for clarity.
 * This demonstrates how a single event can be perceived as a trap, a tool, and a law depending on the index.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The high extraction score (0.75) is justified by the biological reality of
 * species replacement. The system correctly avoids mislabeling this as a pure Mountain by recognizing
 * the perspectival nature of the scientific consensus built around it. The Rope and Mountain classifications
 * apply to the *theory*, while the Snare classification applies to the *event itself* from the victim's view.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_hominin_evolutionary_bottleneck_1,
    'Was the replacement of hominin cousins a predatory biological displacement (Snare) or a natural adaptive shift (Mountain)?',
    'Audit of genetic admixture rates to determine if replacement was violent or absorptive.',
    'If predatory -> Snare. If adaptive -> Mountain.',
    confidence_without_resolution(medium)
).

omega_variable(
    omega_hominin_evolutionary_bottleneck_2,
    'Do other isolated pockets of hominins still exist in the fossil record yet to be found?',
    'Systematic exploration of deep-cave systems in previously unsearched regions.',
    'If yes -> The bottleneck is less restrictive. If no -> The replacement model is absolute.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hominin_evolutionary_bottleneck, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the solidification of the "Out of Africa" consensus over the
% last few decades. As more evidence accumulated, the theory became more "extractive"
% in its explanatory power, crowding out alternatives.
%
% Theater ratio over time:
narrative_ontology:measurement(heb_tr_t0, hominin_evolutionary_bottleneck, theater_ratio, 0, 0.05).
narrative_ontology:measurement(heb_tr_t5, hominin_evolutionary_bottleneck, theater_ratio, 5, 0.08).
narrative_ontology:measurement(heb_tr_t10, hominin_evolutionary_bottleneck, theater_ratio, 10, 0.10).

% Extraction over time:
narrative_ontology:measurement(heb_ex_t0, hominin_evolutionary_bottleneck, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(heb_ex_t5, hominin_evolutionary_bottleneck, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(heb_ex_t10, hominin_evolutionary_bottleneck, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The scientific consensus acts as a standard for interpreting information.
narrative_ontology:coordination_type(hominin_evolutionary_bottleneck, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */