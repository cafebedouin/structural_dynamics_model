% ============================================================================
% CONSTRAINT STORY: emrgency_medicine_clinical_guidelines
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emrgency_medicine_clinical_guidelines, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: emrgency_medicine_clinical_guidelines
 *   human_readable: Emergency Medicine Clinical Guidelines
 *   domain: medical/legal/institutional
 *
 * SUMMARY:
 *   Clinical guidelines (e.g., PECARN for pediatric head trauma or Ottawa
 *   Ankle Rules) are introduced as decision-support tools to standardize care
 *   and reduce unnecessary testing. These guidelines aim to improve patient
 *   outcomes, reduce medical errors, and lower healthcare costs. The adoption
 *   and adherence to these guidelines, however, can be complex, involving a
 *   variety of perspectives and potential tensions.
 *
 * KEY AGENTS:
 *   - Patients: Beneficiaries of consistent and appropriate care (powerless/mobile)
 *   - Emergency Medicine Physicians: Users of the guidelines, with some autonomy (moderate/constrained)
 *   - Hospital Administration: Implementers and enforcers of the guidelines (institutional/analytical)
 *   - Medical Research Community: Evaluators and improvers of the guidelines (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emrgency_medicine_clinical_guidelines, 0.35).
domain_priors:suppression_score(emrgency_medicine_clinical_guidelines, 0.3).
domain_priors:theater_ratio(emrgency_medicine_clinical_guidelines, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emrgency_medicine_clinical_guidelines, extractiveness, 0.35).
narrative_ontology:constraint_metric(emrgency_medicine_clinical_guidelines, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(emrgency_medicine_clinical_guidelines, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emrgency_medicine_clinical_guidelines, rope).
narrative_ontology:human_readable(emrgency_medicine_clinical_guidelines, "Emergency Medicine Clinical Guidelines").
narrative_ontology:topic_domain(emrgency_medicine_clinical_guidelines, "medical/legal/institutional").

domain_priors:requires_active_enforcement(emrgency_medicine_clinical_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emrgency_medicine_clinical_guidelines, patients).
narrative_ontology:constraint_beneficiary(emrgency_medicine_clinical_guidelines, hospitals).
narrative_ontology:constraint_beneficiary(emrgency_medicine_clinical_guidelines, physicians).
narrative_ontology:constraint_victim(emrgency_medicine_clinical_guidelines, physician_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Hospitals benefit from standardized care, reduced liability, and potentially lower costs. They can analyze and adapt the guidelines over time.
constraint_indexing:constraint_classification(emrgency_medicine_clinical_guidelines, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% Physicians benefit from decision support and a framework for justifying their actions, though their autonomy is somewhat constrained. There is some mobility as they can practice outside institutions that require strict adherence to the guidelines.
constraint_indexing:constraint_classification(emrgency_medicine_clinical_guidelines, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Patients benefit from more consistent and appropriate care, and can seek care elsewhere if they disagree with the recommended approach. They have power to move to another hospital.
constraint_indexing:constraint_classification(emrgency_medicine_clinical_guidelines, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% Researchers can evaluate guideline effectiveness and identify areas for improvement, but may be constrained by funding and data access. The analytical viewpoint reveals a combination of coordination and potential extraction, i.e., Tangled Rope classification. By observing over generations, research can fine tune the guidelines, but individual cases may be extracted to prove that guidelines fail
constraint_indexing:constraint_classification(emrgency_medicine_clinical_guidelines, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emrgency_medicine_clinical_guidelines_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emrgency_medicine_clinical_guidelines, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emrgency_medicine_clinical_guidelines, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(emrgency_medicine_clinical_guidelines_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Low to moderate. Guidelines extract some autonomy from physicians, but this is balanced by the benefits of decision support and reduced liability. Suppression (0.30): Low. Physicians retain the ability to deviate from guidelines in specific cases, but this may require justification. Theater ratio (0.15): Low. The primary function of guidelines is to improve patient care, with minimal performative elements.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap exists between hospital administration, which views the guidelines as a tool for standardization and cost control, and physicians, who may perceive them as a constraint on their clinical judgment. Patients generally benefit from the guidelines, but may experience frustration if they feel their individual needs are not being adequately addressed.
 *
 * DIRECTIONALITY LOGIC:
 *   Hospitals and patients benefit from the consistency, reduced testing, and cost savings that the guidelines provide. Physicians may experience a slight loss of autonomy (d > 0.5), however, this is often offset by the reduced liability and improved decision support. The medical research community's d value is about 0.5 since they have to work within the confines of the current guidelines when forming research questions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emrgency_medicine_clinical_guidelines, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emrgency_medicine_clinical_guidelines, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
