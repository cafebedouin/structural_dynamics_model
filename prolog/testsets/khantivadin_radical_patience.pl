% ============================================================================
% CONSTRAINT STORY: khantivadin_radical_patience
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_khantivadin_radical_patience, []).

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
 * * constraint_id: khantivadin_radical_patience
 * human_readable: The Teacher of Patience (Khantivadin)
 * domain: religious/ethical
 * * SUMMARY:
 * This constraint models the state of radical patience (Khanti) in the face of absolute physical liquidation. A sage, Khantivadin, is dismembered by a jealous king for preaching to the king's wives. The sage refuses to respond with anger, identifying patience as a quality of mind independent of the body, thereby transforming a physical Snare into a philosophical Mountain.
 * * KEY AGENTS:
 * - Khantivadin (The Sage): Subject (Powerless), physically trapped.
 * - The King of Kausala: Beneficiary (Institutional), wielding absolute power.
 * - The Buddha (as narrator): Auditor (Analytical), framing the event as a timeless moral law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(khantivadin_radical_patience, 1.0). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(khantivadin_radical_patience, 0.9).   % Structural property (raw, unscaled). The king's rage suppresses all alternatives.
domain_priors:theater_ratio(khantivadin_radical_patience, 0.0).       % Piton detection (>= 0.70). The violence is brutally functional, not theatrical.

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(khantivadin_radical_patience, extractiveness, 1.0).
narrative_ontology:constraint_metric(khantivadin_radical_patience, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(khantivadin_radical_patience, theater_ratio, 0.0).

% Constraint self-claim (what does the constraint claim to be?)
% The teaching derived from the event claims to be a natural law of enlightenment.
narrative_ontology:constraint_claim(khantivadin_radical_patience, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(khantivadin_radical_patience). % The king's violence is active and direct.

% Structural property derivation hooks:
% The king benefits by asserting his authority; the sage's body is the victim.
narrative_ontology:constraint_beneficiary(khantivadin_radical_patience, king_of_kausala).
narrative_ontology:constraint_victim(khantivadin_radical_patience, khantivadin_physical_body).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For Khantivadin's physical body, the king's violence is a terminal Snare.
% Extraction is absolute (liquidation).
constraint_indexing:constraint_classification(khantivadin_radical_patience, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the King, violence is a Rope—a tool to coordinate his soldiers and
% enforce his perceived ownership and authority.
constraint_indexing:constraint_classification(khantivadin_radical_patience, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% From the Buddha's perspective, the sage's response reveals an immutable
% moral law (a Mountain): true patience is a property of mind, not body,
% and is a zero-degree-of-freedom requirement for enlightenment.
constraint_indexing:constraint_classification(khantivadin_radical_patience, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(khantivadin_radical_patience_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless, institutional, and analytical.
    constraint_indexing:constraint_classification(khantivadin_radical_patience, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(khantivadin_radical_patience, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(khantivadin_radical_patience, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypeAnalytical == mountain,
    TypePowerless \= TypeInstitutional,
    TypeInstitutional \= TypeAnalytical.

test(threshold_validation) :-
    % This is a high-extraction constraint, so it must meet the Snare threshold.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(khantivadin_radical_patience, ExtMetricName, E),
    E >= 0.46.

:- end_tests(khantivadin_radical_patience_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This story presents one of the most extreme perspectival gaps possible. The base
 * extractiveness is 1.0, representing the complete physical liquidation of an agent.
 * From the victim's (Khantivadin's) perspective, this is a terminal Snare. From the
 * perpetrator's (the King's) perspective, violence is a Rope used to enforce his
 * authority. From the analytical/philosophical perspective (the Buddha's), the
 * event reveals a Mountain—an immutable law about the nature of mind and suffering.
 * The system must be able to hold all three classifications as simultaneously true
 * from their respective indices.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The absolute extraction (1.0) would normally indicate an unresolvable Mandatrophy.
 * However, the narrative resolves it by shifting the frame of reference. The sage
 * re-classifies the physical Snare as irrelevant to his mental state, effectively
 * finding an 'exit' into a different ontological frame where the extraction does
 * not register as suffering. This is a rare case of resolution through perspectival
 * transformation rather than structural change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_khantivadin_intent,
    'Was the king''s violence a calculated act of political enforcement (Rope) or an irrational, intoxicated rage (Snare)?',
    'Historical/psychological analysis of the king''s state of mind and subsequent policies.',
    'If enforcement, it is a tool of statecraft. If rage, it is pure predatory extraction with no coordination function.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(khantivadin_radical_patience, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. In this case, the event is singular
% and terminal, so the metrics are constant across the interval.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (no theatricality, just raw violence):
narrative_ontology:measurement(krp_tr_t0, khantivadin_radical_patience, theater_ratio, 0, 0.0).
narrative_ontology:measurement(krp_tr_t5, khantivadin_radical_patience, theater_ratio, 5, 0.0).
narrative_ontology:measurement(krp_tr_t10, khantivadin_radical_patience, theater_ratio, 10, 0.0).

% Extraction over time (terminal and absolute from the start):
narrative_ontology:measurement(krp_ex_t0, khantivadin_radical_patience, base_extractiveness, 0, 1.0).
narrative_ontology:measurement(krp_ex_t5, khantivadin_radical_patience, base_extractiveness, 5, 1.0).
narrative_ontology:measurement(krp_ex_t10, khantivadin_radical_patience, base_extractiveness, 10, 1.0).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This constraint is primarily an ethical/philosophical one derived from a singular
% violent act. It does not represent a systemic coordination mechanism or have
% direct structural links to other constraints in the corpus.

% narrative_ontology:coordination_type(khantivadin_radical_patience, enforcement_mechanism).
% narrative_ontology:affects_constraint(khantivadin_radical_patience, other_constraint_id).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */