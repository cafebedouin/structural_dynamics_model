% ============================================================================
% CONSTRAINT STORY: sunk_cost_fallacy
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_sunk_cost_fallacy, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: sunk_cost_fallacy
 * human_readable: The Sunk Cost Fallacy
 * domain: economic/social/cognitive
 * * SUMMARY:
 * The sunk cost fallacy is the tendency to continue an endeavor once an
 * investment in money, effort, or time has been made, even when the current
 * costs outweigh the likely future benefits. It is a "rear-view mirror"
 * approach to decision-making that ignores future marginal utility in favor
 * of emotional accounting for past losses.
 * * KEY AGENTS:
 * - The Entrenched Participant: An agent who feels they have "too much
 * invested to quit" (e.g., a student in the wrong major or a failing project lead).
 * - The Systemic Exploiter: An institution (e.g., a subscription service or
 * casino) that uses initial buy-ins to lock agents into further spending.
 * - The Rational Arbiter: An external observer who ignores past costs to
 * focus solely on future ROI.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale (Extractiveness): It extracts future "optionality" and "capital."
% It benefits entities that profit from "lock-in" while the victim extracts only
% emotional comfort from not admitting failure.
domain_priors:base_extractiveness(sunk_cost_fallacy, 0.5).
% Rationale (Suppression): It suppresses the "Exit" alternative. The brain frames
% quitting as "waste," making the rational choice (stopping) feel
% morally or logically invisible.
domain_priors:suppression_score(sunk_cost_fallacy, 0.6).
% Rationale (Theater): The fallacy is a functional cognitive bias, not a
% performative act. Its theater ratio is very low.
domain_priors:theater_ratio(sunk_cost_fallacy, 0.1).
domain_priors:requires_active_enforcement(sunk_cost_fallacy).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(sunk_cost_fallacy, extractiveness, 0.5).
narrative_ontology:constraint_metric(sunk_cost_fallacy, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(sunk_cost_fallacy, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% The fallacy presents itself as a trap of prior commitment.
narrative_ontology:constraint_claim(sunk_cost_fallacy, snare).
narrative_ontology:human_readable(sunk_cost_fallacy, "The Sunk Cost Fallacy").
narrative_ontology:topic_domain(sunk_cost_fallacy, "economic/social/cognitive").

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(sunk_cost_fallacy, casinos).
narrative_ontology:constraint_beneficiary(sunk_cost_fallacy, predatory_subscription_models).
narrative_ontology:constraint_victim(sunk_cost_fallacy, unhappy_career_holders).
narrative_ontology:constraint_victim(sunk_cost_fallacy, failed_project_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE UNHAPPY DOCTORAL STUDENT (SNARE)
% For the person seven years into a degree they no longer want, the fallacy
% is a Snare. The "investment" they've made acts as the rope that
% strangles their current freedom. They cannot leave because they cannot
% face the "waste" of the past seven years, so they lose the *next* seven years too.
% χ = 0.5 * 1.5 (powerless) * 0.8 (local) = 0.6. With S=0.6, this is a Snare.
constraint_indexing:constraint_classification(sunk_cost_fallacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PROJECT MANAGER (ROPE)
% For the manager, "sunk cost" logic is often used as a Rope. It is a
% coordination mechanism for persistence. By reminding the team or the
% board of "how far we've come," they pull together resources.
% χ = 0.5 * -0.2 (institutional) * 0.9 (regional) = -0.09. Negative extraction -> Rope.
constraint_indexing:constraint_classification(sunk_cost_fallacy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE BEHAVIORAL ECONOMIST (SNARE)
% The analyst sees the mechanism for what it is: an extractive, suppressive
% cognitive trap. While its origin is a mountain-like feature of human cognition,
% its *function* is that of a Snare, systematically extracting future value.
% χ = 0.5 * 1.15 (analytical) * 1.2 (global) = 0.69. With S=0.6, this is a Snare.
constraint_indexing:constraint_classification(sunk_cost_fallacy, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sunk_cost_fallacy_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between the powerless victim and the institutional beneficiary.
    constraint_indexing:constraint_classification(sunk_cost_fallacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sunk_cost_fallacy, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(snare_threshold_validation) :-
    % Verify the base metrics meet the criteria for a high-extraction snare.
    domain_priors:base_extractiveness(sunk_cost_fallacy, E),
    domain_priors:suppression_score(sunk_cost_fallacy, S),
    E >= 0.46,
    S >= 0.4.

test(analytical_classification_is_snare) :-
    % Verify the analytical observer correctly identifies the mechanism as a Snare.
    constraint_indexing:constraint_classification(sunk_cost_fallacy, snare,
        context(agent_power(analytical),
                time_horizon(civilizational),
                exit_options(analytical),
                spatial_scope(global))).

:- end_tests(sunk_cost_fallacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint story models a cognitive bias as a perspectivally variant
 * structure. The key insight is the gap between the institutional agent, who
 * can leverage the bias as a coordination tool (Rope), and the powerless
 * individual, for whom it is a value-extracting trap (Snare).
 *
 * The analytical observer's classification was changed from Mountain to Snare.
 * While the bias's *origin* is a near-immutable feature of human cognition
 * (a mountain-like fact), the Deferential Realism system classifies the
 * *functional effect* of the constraint. Given the high base extractiveness (0.5)
 * and suppression (0.6), the mechanism's function is undeniably that of a Snare,
 * and an objective analyst would classify it as such based on the metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_sunk_cost_fallacy,
    "To what degree is the 'Snare' caused by internal cognitive bias vs. external social shame of quitting?",
    "Comparative study of abandonment rates in anonymous vs. public commitments.",
    "If primarily internal, it's a cognitive Snare. If primarily social, it's a cultural Tangled Rope that requires active social enforcement.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(sunk_cost_fallacy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this constraint is modeled as flat. As a core cognitive
% bias, its properties are assumed to be stable over the observed interval.
% This is required because base_extractiveness > 0.46.

% Theater ratio over time (stable):
narrative_ontology:measurement(scf_tr_t0, sunk_cost_fallacy, theater_ratio, 0, 0.1).
narrative_ontology:measurement(scf_tr_t5, sunk_cost_fallacy, theater_ratio, 5, 0.1).
narrative_ontology:measurement(scf_tr_t10, sunk_cost_fallacy, theater_ratio, 10, 0.1).

% Extraction over time (stable):
narrative_ontology:measurement(scf_ex_t0, sunk_cost_fallacy, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(scf_ex_t5, sunk_cost_fallacy, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(scf_ex_t10, sunk_cost_fallacy, base_extractiveness, 10, 0.5).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% No coordination_type is declared. The sunk cost fallacy is a cognitive bias
% that *disrupts* rational resource allocation; it is not a coordination
% mechanism itself, though it can be exploited by them.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */