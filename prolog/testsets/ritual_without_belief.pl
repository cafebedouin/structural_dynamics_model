% ============================================================================
% CONSTRAINT STORY: ritual_without_belief
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_ritual_without_belief, []).

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
 * * constraint_id: ritual_without_belief
 * human_readable: The Hollow Orthopraxy
 * domain: social/organizational/religious
 * * SUMMARY:
 * A scenario where the external performance of a ritual or protocol is
 * strictly enforced, even though the underlying belief or functional utility
 * has vanished. This coordination "Rope" for institutional stability acts
 * as a "Snare" for the subject, who must expend cognitive and temporal
 * resources on performative compliance to avoid social or professional
 * liquidation.
 * * KEY AGENTS:
 * - Compliance Adherent: Subject (Powerless)
 * - Tradition/Policy Custodian: Beneficiary (Institutional)
 * - Socio-Logician: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.83) as the ritual siphons the subject's primary agency
% into the maintenance of a non-functional social signal.
domain_priors:base_extractiveness(ritual_without_belief, 0.83). % Snare extraction >= 0.46
domain_priors:suppression_score(ritual_without_belief, 0.75).   % Structural property (raw, unscaled). High cost of non-compliance.
domain_priors:theater_ratio(ritual_without_belief, 0.95).       % Piton detection (>= 0.70). Extreme theater: the ritual is purely performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(ritual_without_belief, extractiveness, 0.83).
narrative_ontology:constraint_metric(ritual_without_belief, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ritual_without_belief, theater_ratio, 0.95).

% Constraint self-claim (what does the constraint claim to be?)
% The institution claims it is for coordination and stability.
narrative_ontology:constraint_claim(ritual_without_belief, tangled_rope).
narrative_ontology:human_readable(ritual_without_belief, "The Hollow Orthopraxy").
narrative_ontology:topic_domain(ritual_without_belief, "social/organizational/religious").

% Binary flags
domain_priors:requires_active_enforcement(ritual_without_belief). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope.
narrative_ontology:constraint_beneficiary(ritual_without_belief, tradition_custodians).
narrative_ontology:constraint_victim(ritual_without_belief, compliance_adherents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: they must perform the ritual to access
% essential services or status, despite the lack of belief.
% χ = 0.83 * π(powerless=1.5) * σ(national=1.0) = 1.245 (High Snare)
constraint_indexing:constraint_classification(ritual_without_belief, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the ritual as a Rope—the only way to coordinate
% behavior and ensure institutional continuity.
% χ = 0.83 * π(institutional=-0.2) * σ(global=1.2) = -0.1992 (Negative extraction -> Rope)
constraint_indexing:constraint_classification(ritual_without_belief, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.83) and suppression (0.75) masking as coordination.
% The presence of beneficiaries, victims, and enforcement confirms Tangled Rope.
constraint_indexing:constraint_classification(ritual_without_belief, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.95) > 0.70 triggers Piton: the "ritual" is a
% non-functional, performative artifact of institutional inertia.
constraint_indexing:constraint_classification(ritual_without_belief, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ritual_without_belief_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(ritual_without_belief, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ritual_without_belief, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(ritual_without_belief, piton,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Ensure high extraction (0.83) is correctly categorized.
    narrative_ontology:constraint_metric(ritual_without_belief, extractiveness, E),
    E >= 0.46.

test(piton_audit_logic) :-
    % Ensure extreme theater (0.95) results in Piton detection.
    domain_priors:theater_ratio(ritual_without_belief, TR),
    TR > 0.70,
    constraint_indexing:constraint_classification(ritual_without_belief, piton,
        context(agent_power(analytical), _, _, _)).

:- end_tests(ritual_without_belief_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.83) reflects a "Mandatrophy" state where the
 * "coordination" is actually a parasitic consumption of cognitive energy.
 * The high theater ratio (0.95) confirms that the ritual's original function
 * has atrophied, leaving only the performative shell. The Perspectival Gap
 * is stark: the Compliance Adherent feels a Snare because they are forced to
 * lie to survive. The Tradition Custodian sees a Rope because the
 * external uniformity prevents the chaos of individual dissent.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The high extraction is resolved by the dual analytical classifications of
 * Piton and Tangled Rope. The Piton classification, driven by the 0.95 theater
 * ratio, confirms the functional decay. The Tangled Rope classification
 * correctly identifies that despite this decay, the structure still performs
 * a coercive coordination function with asymmetric extraction, preventing a
 * misclassification as a pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_belief_recapture,
    'Can the belief be re-instilled, or is the decay irreversible (Snare vs Mountain)?',
    'Tracking the sincerity of internal communications vs external performance over time.',
    'If belief returns: Snare of current culture. If decay persists: Mountain of Cultural Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ritual_without_belief, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the decay of the ritual from a functional practice
% into a hollow, extractive performance.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(rwb_tr_t0, ritual_without_belief, theater_ratio, 0, 0.20).
narrative_ontology:measurement(rwb_tr_t5, ritual_without_belief, theater_ratio, 5, 0.60).
narrative_ontology:measurement(rwb_tr_t10, ritual_without_belief, theater_ratio, 10, 0.95).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(rwb_ex_t0, ritual_without_belief, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(rwb_ex_t5, ritual_without_belief, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(rwb_ex_t10, ritual_without_belief, base_extractiveness, 10, 0.83).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The ritual is a mechanism for enforcing behavioral uniformity.
narrative_ontology:coordination_type(ritual_without_belief, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% narrative_ontology:boltzmann_floor_override(ritual_without_belief, 0.1).

% Network relationships (structural influence edges)
% narrative_ontology:affects_constraint(ritual_without_belief, other_constraint_id).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */