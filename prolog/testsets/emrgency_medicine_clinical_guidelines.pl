% ============================================================================
% CONSTRAINT STORY: em_clinical_guidelines
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_em_clinical_guidelines, []).

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
 * * constraint_id: em_clinical_guidelines
 * human_readable: Emergency Medicine Clinical Guidelines
 * domain: medical/legal/institutional
 * * SUMMARY:
 * Clinical guidelines (e.g., PECARN for pediatric head trauma or Ottawa Ankle Rules)
 * are introduced as decision-support tools to standardize care and reduce unnecessary testing.
 * However, their ontological status shifts dramatically depending on the agent's exposure
 * to litigation risk and systemic throughput pressure, creating a significant perspectival gap.
 * * KEY AGENTS:
 * - Frontline ER Physician: Subject (Powerless), balancing diagnostic speed and liability.
 * - Hospital Quality Committee: Beneficiary (Institutional), monitoring compliance and cost metrics.
 * - Systems Auditor: Observer (Analytical), evaluating the structural properties of the guideline system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(em_clinical_guidelines, 0.60). % Extraction of cognitive autonomy, time for documentation, and exposure to liability.
domain_priors:suppression_score(em_clinical_guidelines, 0.50).   % Clinical intuition is suppressed in favor of algorithmic adherence.
domain_priors:theater_ratio(em_clinical_guidelines, 0.25).       % Primarily functional, but with a growing component of "defensive medicine" documentation.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(em_clinical_guidelines, extractiveness, 0.60).
narrative_ontology:constraint_metric(em_clinical_guidelines, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(em_clinical_guidelines, theater_ratio, 0.25).

% Constraint self-claim (what does the constraint claim to be?)
% The system is presented as a pure coordination mechanism for quality improvement.
narrative_ontology:constraint_claim(em_clinical_guidelines, tangled_rope).
narrative_ontology:human_readable(em_clinical_guidelines, "Emergency Medicine Clinical Guidelines").
narrative_ontology:topic_domain(em_clinical_guidelines, "medical/legal/institutional").

% Binary flags
domain_priors:requires_active_enforcement(em_clinical_guidelines). % Required for Tangled Rope; enforced via audits and legal "standard of care".

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(em_clinical_guidelines, hospital_administrators).
narrative_ontology:constraint_beneficiary(em_clinical_guidelines, insurance_payers).
narrative_ontology:constraint_victim(em_clinical_guidelines, frontline_physicians).
narrative_ontology:constraint_victim(em_clinical_guidelines, atypical_patients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The frontline ER physician experiences the guideline as an asymmetric liability trap.
% Following it gives little reward, but deviating creates significant legal risk.
% χ = 0.60 (ε) * 1.5 (π(powerless)) * 0.8 (σ(local)) = 0.72
constraint_indexing:constraint_classification(em_clinical_guidelines, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The hospital quality committee sees a pure coordination tool for standardizing care,
% reducing costs, and improving population-level metrics.
% χ = 0.60 (ε) * -0.2 (π(institutional)) * 1.0 (σ(national)) = -0.12 (felt as a benefit)
constraint_indexing:constraint_classification(em_clinical_guidelines, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The system analyst sees a hybrid constraint. It has a genuine coordination function
% (beneficiaries exist) but also imposes significant, asymmetric extraction on a
% specific group (victims exist), and requires active enforcement to maintain.
% χ = 0.60 (ε) * 1.15 (π(analytical)) * 1.2 (σ(global)) = 0.828
constraint_indexing:constraint_classification(em_clinical_guidelines, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(em_clinical_guidelines_tests).

test(perspectival_gap) :-
    % Verify the gap between the physician (Snare) and the committee (Rope).
    constraint_indexing:constraint_classification(em_clinical_guidelines, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(em_clinical_guidelines, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    assertion(TypePowerless \= TypeInstitutional).

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(em_clinical_guidelines, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(high_extraction_threshold) :-
    % Verify the base extractiveness meets the threshold for a high-extraction constraint.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(em_clinical_guidelines, ExtMetricName, E),
    assertion(E >= 0.46).

:- end_tests(em_clinical_guidelines_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness of 0.60 reflects the significant cost imposed on clinicians
 * in terms of cognitive autonomy, documentation time, and legal liability. The suppression
 * score of 0.50 captures how these guidelines devalue expert clinical intuition for
 * non-standard cases.
 *
 * The key insight is the perspectival gap. For the institution, it's a Rope that coordinates
 * behavior for systemic benefit. For the physician, it's a Snare that extracts autonomy
 * and creates risk. The Analytical Observer classification of Tangled Rope is crucial;
 * it prevents mandatrophy by correctly identifying the structure as a hybrid of both
 * coordination and extraction, which is validated by the presence of both beneficiaries
 * and victims, alongside the need for active enforcement. This is a classic example of
 * a well-intentioned Rope degrading into a Tangled Rope under institutional and legal pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_em_guidelines,
    'Do the guidelines primarily track evolving biological truth or ossifying legal precedent?',
    'Analysis of guideline update triggers: compare changes following major clinical trials vs. changes following major malpractice settlements.',
    'If driven by legal precedent, the system is a pure Snare disguised as medicine. If driven by clinical trials, it retains a core Rope function.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(em_clinical_guidelines, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the guideline's lifecycle from a helpful suggestion (low E)
% to a rigid, legally enforced standard (high E). This trajectory is common as
% institutions codify practices. Extraction accumulation is clearly visible.

% Theater ratio over time (defensive documentation increases):
narrative_ontology:measurement(em_guidelines_tr_t0, em_clinical_guidelines, theater_ratio, 0, 0.10).
narrative_ontology:measurement(em_guidelines_tr_t5, em_clinical_guidelines, theater_ratio, 5, 0.18).
narrative_ontology:measurement(em_guidelines_tr_t10, em_clinical_guidelines, theater_ratio, 10, 0.25).

% Extraction over time (autonomy loss and legal risk increase):
narrative_ontology:measurement(em_guidelines_ex_t0, em_clinical_guidelines, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(em_guidelines_ex_t5, em_clinical_guidelines, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(em_guidelines_ex_t10, em_clinical_guidelines, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The guideline's primary function is to standardize information processing for diagnosis.
narrative_ontology:coordination_type(em_clinical_guidelines, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */