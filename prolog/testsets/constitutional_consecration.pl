% ============================================================================
% CONSTRAINT STORY: constitutional_consecration
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_constitutional_consecration, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: constitutional_consecration
 * human_readable: The Proposition of Equality as a Binding Sacrifice
 * domain: political/legal
 * * SUMMARY:
 * Lincoln's Gettysburg Address re-frames the American constitutional project not as a mere legal compact, but as a "proposition" that all men are created equal. This proposition becomes a binding constraint, consecrated by the sacrifice of soldiers. The "unfinished work" for the living is to uphold this constraint, ensuring the survival of the democratic system, which extracts "the last full measure of devotion" to prevent its own collapse.
 * * KEY AGENTS:
 * - The Soldier: Subject (Powerless), whose life is extracted.
 * - The Union Leadership (Lincoln): Beneficiary/Architect (Institutional), seeking to preserve the system.
 * - The Secessionist: A powerful actor viewing the constraint as a coercive trap.
 * - The Analytical Historian: An observer classifying the system's structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(constitutional_consecration, 0.60). % Extracts "the last full measure of devotion" (life). Snare extraction >= 0.46.
domain_priors:suppression_score(constitutional_consecration, 0.90).   % The Civil War is the mechanism suppressing the alternative (secession).
domain_priors:theater_ratio(constitutional_consecration, 0.10).       % The actions are brutally functional, not performative. Piton >= 0.70.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(constitutional_consecration, extractiveness, 0.60).
narrative_ontology:constraint_metric(constitutional_consecration, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(constitutional_consecration, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The speech frames the sacrifice as necessary for a coordination goal ("government of the people... shall not perish").
narrative_ontology:constraint_claim(constitutional_consecration, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(constitutional_consecration). % Required for Tangled Rope. The "great task remaining" is active enforcement.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(constitutional_consecration, the_union).
narrative_ontology:constraint_victim(constitutional_consecration, soldiers_dead).
narrative_ontology:constraint_victim(constitutional_consecration, secessionists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE SOLDIER (SNARE)
% For the soldier whose life is forfeit, the proposition is a fatal trap.
% The effective extraction is total (χ = 0.60 * 1.5 * 0.8 = 0.72), far into Snare territory.
% It feels like a Mountain because their agency is zero, but its constructed nature makes it a Snare.
constraint_indexing:constraint_classification(constitutional_consecration, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE UNION LEADERSHIP (ROPE)
% For Lincoln, the constraint is a coordination mechanism (Rope) to preserve the nation.
% The institutional power modifier reduces the felt extraction (χ = 0.60 * -0.2 * 1.0 = -0.12),
% making the cost appear negligible compared to the existential goal of preserving the coordinating structure.
constraint_indexing:constraint_classification(constitutional_consecration, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL HISTORIAN (TANGLED ROPE)
% The observer sees both the coordination function (preserving the Union) and the
% brutal, asymmetric extraction (death of soldiers). It requires active enforcement (the war).
% This combination of properties defines a Tangled Rope.
constraint_indexing:constraint_classification(constitutional_consecration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SECESSIONIST (SNARE)
% For those attempting to exit the Union, the proposition is a coercive Snare,
% enforced by military power, which suppresses their alternative.
constraint_indexing:constraint_classification(constitutional_consecration, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_consecration_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(constitutional_consecration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_consecration, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the gap into a Tangled Rope.
    constraint_indexing:constraint_classification(constitutional_consecration, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify the base metrics align with a high-extraction, high-suppression constraint.
    narrative_ontology:constraint_metric(constitutional_consecration, extractiveness, E),
    narrative_ontology:constraint_metric(constitutional_consecration, suppression_requirement, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(constitutional_consecration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this constraint is the perspectival gap between the soldier and the state. For the soldier on the battlefield (powerless, trapped), the demand for their life is an absolute, making the constraint feel like a Mountain. However, because it is a constructed political demand, its technical classification is a Snare, reflecting the total extraction of agency and life. For the Union leadership (institutional), this same mechanism is a Rope—a necessary tool for coordinating national survival. The costs are externalized and thus the felt extraction is negative.
 *
 * The Analytical Observer's classification as a Tangled Rope is critical. It correctly identifies that the constraint is not a pure Snare (as it has a genuine, large-scale coordination function for its beneficiaries) nor a pure Rope (as it involves immense, asymmetric extraction from its victims).
 *
 * * MANDATROPHY ANALYSIS:
 * This constraint is a classic case where a system risks Mandatrophy. By classifying it as a Tangled Rope, the system avoids two errors. First, it rejects the purely cynical view that the Union is just a Snare with no coordinating value. Second, it rejects the purely idealistic view that the Union is a pure Rope, ignoring the brutal extraction required to maintain it. The Tangled Rope classification forces an acknowledgment of both truths simultaneously: it is a system of coordination *and* a system of violent, asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_constitutional_consecration,
    "Does the 'proposition' of equality functionally include the four million enslaved people at the time of the speech, or is it an abstract principle about the legal status of states?",
    "Analysis of the Reconstruction Amendments and subsequent civil rights legislation, which attempted to resolve this ambiguity.",
    "If narrow, the constraint is a Tangled Rope benefiting only one group via extraction from another. If broad, it's a Scaffold for a more inclusive Rope, justifying the high initial suppression.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(constitutional_consecration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for the period of the Civil War, where the "unfinished work"
% intensified, increasing the extractive requirement to its peak at Gettysburg.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (remains low; the war is functional, not performative):
narrative_ontology:measurement(cc_tr_t0, constitutional_consecration, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cc_tr_t5, constitutional_consecration, theater_ratio, 5, 0.10).
narrative_ontology:measurement(cc_tr_t10, constitutional_consecration, theater_ratio, 10, 0.10).

% Extraction over time (rises as the war demands greater sacrifice):
narrative_ontology:measurement(cc_ex_t0, constitutional_consecration, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(cc_ex_t5, constitutional_consecration, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(cc_ex_t10, constitutional_consecration, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint's primary function is to enforce the continuation of the Union.
narrative_ontology:coordination_type(constitutional_consecration, enforcement_mechanism).


/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */