% ============================================================================
% CONSTRAINT STORY: hoa_covenants
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_hoa_covenants, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: hoa_covenants
 * human_readable: HOA Architectural Review and Restrictive Covenants
 * domain: economic/social/legal
 * * SUMMARY:
 * Legally binding restrictive covenants that dictate the aesthetic and structural
 * modifications allowed on private property within a homeowners' association (HOA).
 * Managed by an Architectural Review Board (ARB), these constraints regulate
 * everything from paint color and landscaping to mailboxes and roof materials,
 * often through a subjective enforcement process.
 * * KEY AGENTS:
 * - Individual Homeowner: The subject bound by the covenants and subject to fines.
 * - HOA Board/ARB Member: The institutional enforcer of the "community standard."
 * - Real Estate Analyst: An analytical observer of property value and market friction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(hoa_covenants, 0.55). % High due to fines, fees, and loss of autonomy.
domain_priors:suppression_score(hoa_covenants, 0.30).   % Alternatives are not actively suppressed, but legal/social inertia is high.
domain_priors:theater_ratio(hoa_covenants, 0.75).       % High due to subjective, performative enforcement of outdated rules.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(hoa_covenants, extractiveness, 0.55).
narrative_ontology:constraint_metric(hoa_covenants, suppression_requirement, 0.30).
narrative_ontology:constraint_metric(hoa_covenants, theater_ratio, 0.75).

% Constraint self-claim (what does the constraint claim to be?)
% HOAs claim to be a coordination mechanism for preserving property values.
narrative_ontology:constraint_claim(hoa_covenants, piton).
narrative_ontology:human_readable(hoa_covenants, "HOA Architectural Review and Restrictive Covenants").

% Binary flags
domain_priors:requires_active_enforcement(hoa_covenants). % Fines and liens are used for enforcement.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(hoa_covenants, property_developers).
narrative_ontology:constraint_beneficiary(hoa_covenants, hoa_management_companies).
narrative_ontology:constraint_victim(hoa_covenants, individual_homeowners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The homeowner faces asymmetric power, fines, and high exit costs.
% χ = 0.55 (ε) * 1.5 (π(powerless)) * 0.8 (σ(local)) = 0.66
constraint_indexing:constraint_classification(hoa_covenants, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The HOA board sees the covenants as an essential coordination tool.
% χ = 0.55 (ε) * -0.2 (π(institutional)) * 0.9 (σ(regional)) = -0.099 (felt as a net benefit)
constraint_indexing:constraint_classification(hoa_covenants, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% The analyst sees a degraded system that persists through inertia, where
% performative enforcement has replaced the original function.
constraint_indexing:constraint_classification(hoa_covenants, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(hoa_covenants, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenants_tests).

test(perspectival_gap_snare_vs_rope, [nondet]) :-
    % Verify the homeowner sees a Snare while the board sees a Rope.
    constraint_indexing:constraint_classification(hoa_covenants, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hoa_covenants, rope, context(agent_power(institutional), _, _, _)).

test(piton_classification_requires_high_theater, [nondet]) :-
    % Verify the Piton classification is correctly conditioned on the theater_ratio.
    domain_priors:theater_ratio(hoa_covenants, TR),
    ( TR > 0.70 ->
        constraint_indexing:constraint_classification(hoa_covenants, piton, context(agent_power(analytical), _, _, _))
    ; \+ constraint_indexing:constraint_classification(hoa_covenants, piton, context(agent_power(analytical), _, _, _))
    ).

:- end_tests(hoa_covenants_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect the degradation of a coordination mechanism into a system of performative control.
 * - Base Extractiveness (0.55): This is high, representing not just monetary dues but the significant loss of owner autonomy and the financial risk of arbitrary fines and liens.
 * - Theater Ratio (0.75): This is the key metric for the Piton classification. It models the common scenario where HOA boards spend more effort enforcing trivial, outdated aesthetic rules (e.g., "the wrong shade of beige") than addressing functional community issues. The enforcement becomes a performance of authority rather than a tool for value preservation.
 * - Perspectival Gap: The gap is stark. For the institutional board, the rules are a Rope, a tool of control and coordination they wield. For the powerless homeowner, who is trapped by high transaction costs of moving, the same rules are a Snare, an arbitrary and extractive trap.
 *
 * MANDATROPHY ANALYSIS:
 * The Piton classification is a direct resolution of Mandatrophy. Instead of misclassifying the HOA system as a functional Rope (its stated purpose) or a pure Snare (ignoring its origins), the Piton type correctly identifies it as a degraded constraint. It was once a Rope intended for coordination, but its primary function has atrophied, and it now persists through legal inertia and theatrical enforcement. This prevents the system from making a category error.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_hoa_covenants_value,
    'Do architectural restrictions empirically increase property value compared to similar non-HOA homes, controlling for other factors?',
    'A long-term econometric study controlling for neighborhood age, location, and amenities.',
    'If NO, the "Rope" classification for the Board collapses, revealing the entire structure as a Snare from all non-beneficiary perspectives.',
    confidence_without_resolution(medium)
).

omega_variable(
    omega_hoa_covenants_motivation,
    'Is ARB enforcement primarily driven by a consistent application of rules for community benefit (Rope) or by the personal biases and power dynamics of board members (Snare)?',
    'Sentiment analysis of board meeting minutes combined with frequency analysis of litigation and violation types.',
    'Determines whether the constraint is a flawed coordination tool or a mechanism for localized, arbitrary power.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hoa_covenants, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the degradation of the HOA from a coordination tool
% into a high-theater, high-extraction system over its lifecycle.
% Required because base_extractiveness (0.55) > 0.46.

% Theater ratio over time (metric_substitution drift):
narrative_ontology:measurement(hoa_covenants_tr_t0, hoa_covenants, theater_ratio, 0, 0.20).
narrative_ontology:measurement(hoa_covenants_tr_t5, hoa_covenants, theater_ratio, 5, 0.55).
narrative_ontology:measurement(hoa_covenants_tr_t10, hoa_covenants, theater_ratio, 10, 0.75).

% Extraction over time (extraction_accumulation drift):
narrative_ontology:measurement(hoa_covenants_ex_t0, hoa_covenants, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(hoa_covenants_ex_t5, hoa_covenants, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(hoa_covenants_ex_t10, hoa_covenants, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The core function of HOA covenants is rule enforcement.
narrative_ontology:coordination_type(hoa_covenants, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */