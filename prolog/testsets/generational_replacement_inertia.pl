% ============================================================================
% CONSTRAINT STORY: generational_replacement_inertia
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_generational_replacement_inertia, []).

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
 * * constraint_id: generational_replacement_inertia
 * human_readable: Generational Cognitive Inertia
 * domain: social/psychological
 * * SUMMARY:
 * This constraint posits that human thoughts are trapped in "well-worn pathways"
 * that rarely change, leading to a state where societies only evolve as a
 * function of generational replacement. The "Primary Human Problem" is the duality
 * of being "good" but also "self-centered, lazy and stubborn," making the
 * "breaking of chains" a biological rather than intellectual process.
 * * KEY AGENTS:
 * - The Individual Thinker: Subject (Powerless), trapped in static mental routes.
 * - The Institution: Beneficiary (Institutional), plans around demographic shifts.
 * - The Systems Auditor: Observer (Analytical), assesses the overall structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Base extractiveness (0.75): High. The constraint extracts the agency and
% potential for growth of existing individuals, liquidating their capacity for
% change in favor of "generational replacement".
domain_priors:base_extractiveness(generational_replacement_inertia, 0.75).
% Suppression score (0.80): High. "Advice is useless" and "fools won't heed it,"
% actively suppressing the visibility of alternative pathways for the current generation.
domain_priors:suppression_score(generational_replacement_inertia, 0.80).
% Theater ratio (0.05): Very low. This is presented as a core, functional process
% of social change, not a performative one.
domain_priors:theater_ratio(generational_replacement_inertia, 0.05).
domain_priors:requires_active_enforcement(generational_replacement_inertia).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(generational_replacement_inertia, extractiveness, 0.75).
narrative_ontology:constraint_metric(generational_replacement_inertia, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(generational_replacement_inertia, theater_ratio, 0.05).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be an immutable law of human social dynamics.
narrative_ontology:constraint_claim(generational_replacement_inertia, snare).

% Structural property derivation hooks:
% The process benefits future generations at the cost of the current one's potential.
narrative_ontology:constraint_beneficiary(generational_replacement_inertia, future_generations).
narrative_ontology:constraint_victim(generational_replacement_inertia, current_generation_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE INDIVIDUAL THINKER (SNARE)
% For the individual, cognitive inertia is a Snare. Their thoughts are trapped
% in a cycle where they seek "corroboration" over advice, strangling their own
% potential for evolution.
% χ = 0.75 * π(powerless:1.5) * σ(local:0.8) = 0.90
constraint_indexing:constraint_classification(generational_replacement_inertia, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE INSTITUTION (ROPE)
% For a long-term institution, generational replacement is a Rope. It is a
% predictable demographic reality that allows for long-range planning. The
% negative power modifier reflects that the institution benefits from this
% predictable churn, viewing it as essential coordination infrastructure.
% χ = 0.75 * π(institutional:-0.2) * σ(national:1.0) = -0.15
constraint_indexing:constraint_classification(generational_replacement_inertia, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% The observer sees a high-extraction, high-suppression system that does not
% require active enforcement. It is a structural Snare that liquidates individual
% agency as its core function, even if that function produces downstream social change.
% χ = 0.75 * π(analytical:1.15) * σ(global:1.2) = 1.035
constraint_indexing:constraint_classification(generational_replacement_inertia, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(generational_replacement_inertia_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(generational_replacement_inertia, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(generational_replacement_inertia, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    % Verify that the base extraction is high, consistent with a Snare classification.
    domain_priors:base_extractiveness(generational_replacement_inertia, E),
    assertion(E >= 0.46).

:- end_tests(generational_replacement_inertia_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this constraint is the massive perspectival gap between those
 * experiencing it and those managing it. For the individual (powerless) and the
 * analytical observer, the high base extraction (0.75) and suppression (0.80)
 * make it a clear Snare. It traps potential and liquidates agency.
 *
 * The key insight is the institutional perspective. An institution planning over
 * civilizational timescales sees this process not as a trap but as a predictable,
 * beneficial mechanism for social renewal—a Rope. The negative power modifier for
 * institutional agents (π = -0.2) causes the effective extraction (χ) to become
 * negative (-0.15), correctly classifying it as a Rope from their index. The
 * constraint's claim to be 'natural_law' is the ideological justification for
 * this extractive process.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * Mandatrophy is resolved because the system correctly identifies the perspectival
 * gap. The institutional claim of a 'Rope' (or 'Mountain' in the original text)
 * is shown to be index-dependent. The high base extraction is not ignored; it is
 * felt acutely by the powerless, leading to the Snare classification that
 * reflects the lived experience of those trapped by the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_gri_1,
    'Is the inability for individual thoughts to change a biological necessity (Mountain) or an extractive psychological Snare that could be escaped with better tools?',
    'Audit of cognitive plasticity in nourishing vs. poisonous psychological and social environments.',
    'If necessity, it is an evolutionary Mountain. If a product of environment, it is a solvable Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(generational_replacement_inertia, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This constraint is modeled as a stable,
% core feature of human social dynamics, so the metrics do not drift over the
% interval. This is required because base_extractiveness > 0.46.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(gri_tr_t0, generational_replacement_inertia, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gri_tr_t5, generational_replacement_inertia, theater_ratio, 5, 0.05).
narrative_ontology:measurement(gri_tr_t10, generational_replacement_inertia, theater_ratio, 10, 0.05).

% Extraction over time (stable and high):
narrative_ontology:measurement(gri_ex_t0, generational_replacement_inertia, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(gri_ex_t5, generational_replacement_inertia, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(gri_ex_t10, generational_replacement_inertia, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% No coordination type is declared as the mechanism is described as a passive,
% emergent biological process rather than an active coordination protocol.
% No network relationships are declared at this time.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */