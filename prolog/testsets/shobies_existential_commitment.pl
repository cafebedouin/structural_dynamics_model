% ============================================================================
% CONSTRAINT STORY: shobies_existential_commitment
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_shobies_existential_commitment, []).

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
 * * constraint_id: shobies_existential_commitment
 * human_readable: The Risk-Safety Paradox
 * domain: social/existential
 * * SUMMARY:
 * From Ursula K. Le Guin's "The Shobies' Story," this constraint defines a state where safety and functionality are paradoxically dependent on total existential risk. It posits that "nothing works" unless an agent commits their "soul" to it, and that "nothing’s safe" except that which has been put at risk. This creates a high-stakes coordination problem where individual security must be sacrificed for collective success.
 * * KEY AGENTS:
 * - The Cautious Individual: Subject (Powerless) who feels their autonomy and safety extracted by the demand for total commitment.
 * - The Project Collective: Beneficiary (Institutional) for whom this total commitment is the essential coordination mechanism for success.
 * - The Sage/Narrator: Auditor (Analytical) who observes the structure of this paradox.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale: High (0.65). The constraint requires agents to "stake everything" and "give our souls". This is a severe extraction of individual autonomy and security.
domain_priors:base_extractiveness(shobies_existential_commitment, 0.65).
% Rationale: Moderate (0.5). The claim "nothing works except..." suppresses the validity of low-risk, low-commitment alternatives, framing them as fundamentally broken.
domain_priors:suppression_score(shobies_existential_commitment, 0.5).
% Rationale: Low (0.1). The constraint is described as highly functional, not performative. The commitment is real, not theatrical.
domain_priors:theater_ratio(shobies_existential_commitment, 0.1).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(shobies_existential_commitment, extractiveness, 0.65).
narrative_ontology:constraint_metric(shobies_existential_commitment, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(shobies_existential_commitment, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a fundamental mechanism for achieving results.
narrative_ontology:constraint_claim(shobies_existential_commitment, tangled_rope).
narrative_ontology:human_readable(shobies_existential_commitment, "The Risk-Safety Paradox").

% Binary flags
% The commitment is an internal, psychological enforcement mechanism.
domain_priors:requires_active_enforcement(shobies_existential_commitment).

% Structural property derivation hooks:
% The collective project "works" because of the commitment.
narrative_ontology:constraint_beneficiary(shobies_existential_commitment, project_collective).
% The individual's desire for low-risk safety is sacrificed.
narrative_ontology:constraint_victim(shobies_existential_commitment, individual_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE CAUTIOUS INDIVIDUAL (SNARE)
% For the uncommitted or fearful agent, this is a Snare. The requirement to
% "stake everything" feels like total extraction, strangling their sense of
% safety until they surrender their autonomy.
constraint_indexing:constraint_classification(shobies_existential_commitment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PROJECT COLLECTIVE (ROPE)
% From the institutional perspective of the project, the crew's total
% commitment is the fundamental coordination mechanism (Rope) that makes
% an impossible mission viable. The high stakes are not a bug, but the
% feature that guarantees cohesion.
constraint_indexing:constraint_classification(shobies_existential_commitment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees a structure with a genuine coordination function (the
% project succeeds) but also severe, asymmetric extraction (individuals must
% sacrifice autonomy). It requires active enforcement (internal commitment).
% This combination of coordination and extraction is a Tangled Rope.
constraint_indexing:constraint_classification(shobies_existential_commitment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shobies_existential_commitment_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(shobies_existential_commitment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shobies_existential_commitment, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(threshold_validation_high_extraction) :-
    % Verify this is correctly identified as a high-extraction constraint.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(shobies_existential_commitment, ExtMetricName, E),
    E >= 0.46.

test(tangled_rope_conditions_met) :-
    % Verify the analytical observer correctly sees a Tangled Rope.
    constraint_indexing:constraint_classification(shobies_existential_commitment, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(shobies_existential_commitment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this constraint is the perspectival gap between the individual and the collective. For the 'powerless' individual, the demand to "give your soul" is a coercive Snare with no safe alternative. For the 'institutional' collective, this same demand is the Rope that binds the group and ensures mission success.
 *
 * The analytical observer's classification was changed from 'Mountain' to 'Tangled Rope'. A Mountain requires base extraction <= 0.15; at 0.65, this constraint is far too extractive. The system correctly identifies it as a Tangled Rope because it meets all three criteria: it has a coordination function (beneficiary exists), asymmetric extraction (victim exists), and requires active enforcement. This prevents the system from misclassifying a constructed, high-cost social contract as a law of nature.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Tangled Rope classification is the key to resolving mandatrophy. It acknowledges that the constraint *does* perform a vital coordination function (making things "work") while simultaneously recognizing the severe extraction imposed on its subjects. It is neither a pure Rope (ignoring the cost) nor a pure Snare (ignoring the function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Is the paradox a fundamental law or a constructed narrative to enforce cohesion?
omega_variable(
    omega_shobies_existential_commitment,
    "Is the causal link between total risk and functional safety a physical/metaphysical law (Mountain-like), or is it a socially constructed belief system (Tangled Rope) used to justify extreme demands?",
    "Empirical comparison of project success rates in 'total risk' vs 'mitigated risk' environments across different cultural contexts.",
    "If it is a law, the constraint is a Mountain and the high extraction is an unavoidable cost of reality. If it is constructed, it is a Tangled Rope that could potentially be replaced by a less extractive coordination mechanism.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(shobies_existential_commitment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint (E=0.65).
% The model assumes this is a stable, perennial constraint, so the values
% do not drift over the interval.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(shobies_tr_t0, shobies_existential_commitment, theater_ratio, 0, 0.1).
narrative_ontology:measurement(shobies_tr_t5, shobies_existential_commitment, theater_ratio, 5, 0.1).
narrative_ontology:measurement(shobies_tr_t10, shobies_existential_commitment, theater_ratio, 10, 0.1).

% Extraction over time (stable and high):
narrative_ontology:measurement(shobies_ex_t0, shobies_existential_commitment, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(shobies_ex_t5, shobies_existential_commitment, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(shobies_ex_t10, shobies_existential_commitment, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The coordination relies on enforcing a state of mind (total commitment).
narrative_ontology:coordination_type(shobies_existential_commitment, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */