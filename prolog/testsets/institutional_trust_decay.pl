% ============================================================================
% CONSTRAINT STORY: institutional_trust_decay
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-23
% ============================================================================

:- module(constraint_institutional_trust_decay, []).

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
 * * constraint_id: institutional_trust_decay
 * human_readable: The Legitimacy Void
 * domain: social
 * * SUMMARY:
 * A systemic condition where the shared belief in institutional competence and
 * integrity evaporates. This transforms social "Ropes" into perceived "Snares,"
 * as coordination becomes indistinguishable from coercion. The institution's
 * actions become performative, maintaining the appearance of function while
 * the underlying social contract has failed.
 * * KEY AGENTS:
 * - The Dissident (General Populace): Subject (Powerless) - Views all official data as a predatory trap.
 * - The Press Secretary (Institutional Actors): Beneficiary (Institutional) - Managing the theatrical "Piton" of authority.
 * - The Sociometrician: Auditor (Analytical) - Quantifying the gap between signal and belief.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(institutional_trust_decay, 0.68). % High: The cost of non-cooperation extracts social capital.
domain_priors:suppression_score(institutional_trust_decay, 0.55).   % High: Lack of trusted alternative institutions.
domain_priors:theater_ratio(institutional_trust_decay, 0.88).       % Extremely High: Indicators of a Piton status.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(institutional_trust_decay, extractiveness, 0.68).
narrative_ontology:constraint_metric(institutional_trust_decay, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(institutional_trust_decay, theater_ratio, 0.88).

% Constraint self-claim (what does the constraint claim to be?)
% The institution claims its structures are for coordination, hiding the decay.
narrative_ontology:constraint_claim(institutional_trust_decay, piton).
narrative_ontology:human_readable(institutional_trust_decay, "The Legitimacy Void").
narrative_ontology:topic_domain(institutional_trust_decay, "social").

% Binary flags
domain_priors:requires_active_enforcement(institutional_trust_decay).

% Structural property derivation hooks (required for high extraction)
narrative_ontology:constraint_beneficiary(institutional_trust_decay, institutional_actors).
narrative_ontology:constraint_victim(institutional_trust_decay, general_populace).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The individual feels trapped in a system that no longer yields a ROI on trust.
constraint_indexing:constraint_classification(institutional_trust_decay, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution maintains that the structures are still essential coordination mechanisms.
constraint_indexing:constraint_classification(institutional_trust_decay, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Detection of "Piton" status: high theater_ratio indicates inert structures.
constraint_indexing:constraint_classification(institutional_trust_decay, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(institutional_trust_decay, TR), TR >= 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_trust_decay_tests).

test(perspectival_gap) :-
    % Verify variance: Snare for the subject, Rope for the institution.
    constraint_indexing:constraint_classification(institutional_trust_decay, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_trust_decay, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(piton_analysis) :-
    % Verify that the analytical observer detects a Piton due to high theater_ratio.
    constraint_indexing:constraint_classification(institutional_trust_decay, piton, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify the constraint meets the high-extraction threshold.
    narrative_ontology:constraint_metric(institutional_trust_decay, extractiveness, E),
    E >= 0.46.

:- end_tests(institutional_trust_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Base extractiveness (0.68) captures the "trust tax" where transactions
 * require expensive verification instead of low-cost social capital. Suppression
 * (0.55) reflects the difficulty of establishing alternative, trusted institutions.
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by the high Theater Ratio (0.88). The system
 * identifies that while the institution acts as if it is a "Rope," it is
 * functionally a "Piton" kept alive by administrative inertia and performative
 * acts of governance. The analytical observer correctly classifies it as a Piton,
 * preventing a misclassification as a functional Snare or Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_institutional_trust_decay,
    'Can trust be restored via reform (Scaffold), or is the decay a physical Mountain of entropy?',
    'Historical comparison of reformist vs. revolutionary outcomes in trust-depleted states.',
    'If reform: Transition to Scaffold; If entropy: Descent into Snare-totalitarianism.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(institutional_trust_decay, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the decay of trust over the interval. Initially, the
% institution was more functional (lower extraction, lower theater), but it
% degraded over time into a performative shell.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(itd_tr_t0, institutional_trust_decay, theater_ratio, 0, 0.22).
narrative_ontology:measurement(itd_tr_t5, institutional_trust_decay, theater_ratio, 5, 0.65).
narrative_ontology:measurement(itd_tr_t10, institutional_trust_decay, theater_ratio, 10, 0.88).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(itd_ex_t0, institutional_trust_decay, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(itd_ex_t5, institutional_trust_decay, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(itd_ex_t10, institutional_trust_decay, base_extractiveness, 10, 0.68).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint represents the decay of what was once an enforcement mechanism
% for social contracts.
narrative_ontology:coordination_type(institutional_trust_decay, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */