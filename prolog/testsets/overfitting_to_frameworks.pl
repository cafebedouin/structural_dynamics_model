% ============================================================================
% CONSTRAINT STORY: overfitting_to_frameworks
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_overfitting_to_frameworks, []).

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
 * * constraint_id: overfitting_to_frameworks
 * human_readable: The Rigidity of the Map
 * domain: technological/cognitive/organizational
 * * SUMMARY:
 * This constraint occurs when an organization or agent optimizes their
 * behavior so tightly to a specific evaluative or technical framework that
 * they lose the ability to perceive or respond to external reality. The
 * framework becomes a "Snare" for internal agents while appearing as a
 * necessary "Rope" for institutional consistency.
 * * KEY AGENTS:
 * - Junior Implementer: Subject (Powerless)
 * - Framework Architect: Beneficiary (Institutional)
 * - Systems Resilience Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.78) as the system siphons adaptive capacity into
% hyper-specialized compliance with the internal map.
domain_priors:base_extractiveness(overfitting_to_frameworks, 0.78).
domain_priors:suppression_score(overfitting_to_frameworks, 0.65).
domain_priors:theater_ratio(overfitting_to_frameworks, 0.82). % High theater: focus on framework "scores" over actual outcomes.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(overfitting_to_frameworks, extractiveness, 0.78).
narrative_ontology:constraint_metric(overfitting_to_frameworks, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(overfitting_to_frameworks, theater_ratio, 0.82).

% Constraint self-claim: The framework claims to be a coordination tool for quality.
narrative_ontology:constraint_claim(overfitting_to_frameworks, tangled_rope).

% Binary flags and structural properties for Tangled Rope classification.
domain_priors:requires_active_enforcement(overfitting_to_frameworks).
narrative_ontology:constraint_beneficiary(overfitting_to_frameworks, framework_architects).
narrative_ontology:constraint_victim(overfitting_to_frameworks, junior_implementers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The implementer is trapped: following the framework leads to failure,
% but diverging leads to professional extraction/punishment.
constraint_indexing:constraint_classification(overfitting_to_frameworks, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The architect views the framework as a vital Rope for maintaining
% coordination and quality standards across a distributed organization.
constraint_indexing:constraint_classification(overfitting_to_frameworks, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The default analytical view detects both a genuine coordination function
% (beneficiaries exist) and asymmetric extraction (victims exist), along with
% active enforcement. This is the canonical signature of a Tangled Rope.
constraint_indexing:constraint_classification(overfitting_to_frameworks, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% A different analytical view, focused on function vs performance, sees the
% high theater ratio (0.82) and classifies it as a Piton: an atrophied
% constraint maintained through inertial, theatrical compliance.
constraint_indexing:constraint_classification(overfitting_to_frameworks, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(overfitting_to_frameworks, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(overfitting_to_frameworks_tests).

test(perspectival_gap) :-
    % Verify the Snare vs Rope conflict for the powerless and institutional indices.
    constraint_indexing:constraint_classification(overfitting_to_frameworks, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(overfitting_to_frameworks, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification) :-
    % Ensure the analytical observer correctly identifies it as a Tangled Rope.
    constraint_indexing:constraint_classification(overfitting_to_frameworks, tangled_rope,
        context(agent_power(analytical), time_horizon(historical), exit_options(analytical), spatial_scope(global))).

test(piton_detection) :-
    % Ensure high theater ratio correctly triggers Piton classification from the auditor's perspective.
    constraint_indexing:constraint_classification(overfitting_to_frameworks, piton,
        context(agent_power(analytical), time_horizon(civilizational), exit_options(arbitrage), spatial_scope(universal))).

:- end_tests(overfitting_to_frameworks_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.78) reflects the high cost of lost adaptability and
 * innovation imposed on implementers. The suppression score (0.65) reflects
 * the professional and organizational penalties for deviating from the framework.
 * The high theater ratio (0.82) shows that compliance activities have become
 * detached from real-world outcomes, a classic sign of institutional decay.
 *
 * PERSPECTIVAL GAP:
 * The gap is stark. For the Junior Implementer, the framework is a Snare that
 * punishes correct-but-non-compliant actions. For the Framework Architect, it
 * is a Rope that ensures predictability and control, solving a coordination
 * problem at scale. Both perspectives are locally valid.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint is a classic case of Mandatrophy, where a system designed
 * for coordination becomes primarily extractive. The resolution is the
 * Tangled Rope classification. It avoids the simplistic binary of "good
 * coordination" (Rope) vs. "bad extraction" (Snare). By classifying it as
 * Tangled Rope, the system acknowledges that the framework *does* have a
 * coordination function (hence the Beneficiary's view) but that this function
 * is coupled with severe, asymmetric extraction and requires active enforcement
 * to maintain. This nuanced classification prevents mislabeling the entire
 * structure as purely predatory or purely beneficial, capturing the reality
 * of its dual nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for constraints with base_extractiveness > 0.46.
omega_variable(
    omega_framework_permeability,
    'Can the framework integrate "outside" signals, or is it a closed loop (Snare vs Mountain)?',
    'Auditing the rate of change in framework parameters relative to market volatility.',
    'If static: Mountain of Rigidity. If adaptive: Snare of Control.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(overfitting_to_frameworks, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint began as a useful coordination tool (low extraction, low theater)
% but decayed over time into a rigid, performative system. This progression
% is typical of extraction_accumulation and metric_substitution drift.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(overfitting_to_frameworks_tr_t0, overfitting_to_frameworks, theater_ratio, 0, 0.10).
narrative_ontology:measurement(overfitting_to_frameworks_tr_t5, overfitting_to_frameworks, theater_ratio, 5, 0.40).
narrative_ontology:measurement(overfitting_to_frameworks_tr_t10, overfitting_to_frameworks, theater_ratio, 10, 0.82).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(overfitting_to_frameworks_ex_t0, overfitting_to_frameworks, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(overfitting_to_frameworks_ex_t5, overfitting_to_frameworks, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(overfitting_to_frameworks_ex_t10, overfitting_to_frameworks, base_extractiveness, 10, 0.78).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The framework acts as a standard for information and process, making it
% an information_standard type for Boltzmann analysis.
narrative_ontology:coordination_type(overfitting_to_frameworks, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */