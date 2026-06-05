% ============================================================================
% CONSTRAINT STORY: vns_implant_for_trd
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vns_implant_for_trd, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vns_implant_for_trd
 *   human_readable: Vagus Nerve Stimulation (VNS) Implant for Treatment-Resistant Depression
 *   domain: technological
 *
 * SUMMARY:
 *   The Vagus Nerve Stimulation (VNS) implant system for treatment-resistant
 *   depression (TRD) represents a complex interaction between technological
 *   intervention, patient experience, and economic incentives. While offering
 *   potential relief for a difficult-to-treat condition, it also involves
 *   significant costs, potential side effects, and uncertainties about
 *   long-term efficacy.
 *
 * KEY AGENTS:
 *   - TRD Patients: Primary target (powerless/trapped) - bear the costs and risks of the invasive procedure.
 *   - Device Manufacturers: Primary beneficiary (institutional/arbitrage) - profit from the sale and maintenance of the devices.
 *   - Surgeons: Secondary beneficiary (powerful/mobile) - gain from performing the surgical procedure.
 *   - Hospitals: Secondary beneficiary (institutional/constrained) - increased revenue and patient base.
 *   - Analytical Observer: Sees the full structure (analytical/analytical) - assesses the overall impact and balance of benefits and costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vns_implant_for_trd, 0.55).
domain_priors:suppression_score(vns_implant_for_trd, 0.45).
domain_priors:theater_ratio(vns_implant_for_trd, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vns_implant_for_trd, extractiveness, 0.55).
narrative_ontology:constraint_metric(vns_implant_for_trd, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(vns_implant_for_trd, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vns_implant_for_trd, tangled_rope).
narrative_ontology:human_readable(vns_implant_for_trd, "Vagus Nerve Stimulation (VNS) Implant for Treatment-Resistant Depression").
narrative_ontology:topic_domain(vns_implant_for_trd, "technological").

domain_priors:requires_active_enforcement(vns_implant_for_trd).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vns_implant_for_trd, device_manufacturers).
narrative_ontology:constraint_beneficiary(vns_implant_for_trd, surgeons).
narrative_ontology:constraint_beneficiary(vns_implant_for_trd, hospitals).
narrative_ontology:constraint_victim(vns_implant_for_trd, trd_patients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Patients with TRD may feel trapped due to limited alternative treatment options and the invasive nature of the implant. The high cost and potential side effects contribute to the snare-like experience.
constraint_indexing:constraint_classification(vns_implant_for_trd, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Device manufacturers benefit from the sale and maintenance of VNS devices. They can readily switch to alternative product lines if VNS becomes unprofitable or undesirable.
constraint_indexing:constraint_classification(vns_implant_for_trd, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The analytical observer recognizes the mixed coordination and extraction. VNS provides a treatment option (coordination) but also introduces potential risks and costs for patients (extraction).
constraint_indexing:constraint_classification(vns_implant_for_trd, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% Surgeons benefit from performing VNS implantations, providing a new service and revenue stream to their practice. They are mobile, as they can offer a range of surgical procedures.
constraint_indexing:constraint_classification(vns_implant_for_trd, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vns_implant_for_trd_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vns_implant_for_trd, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vns_implant_for_trd, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vns_implant_for_trd, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vns_implant_for_trd_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.55. Moderate extraction due to the financial burden on patients, the potential for side effects, and the uncertainty of long-term efficacy. Suppression: 0.45. Moderate suppression due to limited alternative options for TRD patients, creating a sense of limited choice. Theater Ratio: 0.30. Low theater ratio as the treatment involves a direct, physical intervention with tangible, measurable effects, although the mechanisms are not fully understood.
 *
 * PERSPECTIVAL GAP:
 *   TRD patients experience the treatment as a snare due to feeling trapped and the potential for negative outcomes. Device manufacturers see it as a coordination mechanism (rope), allowing them to create and sell a product. Surgeons see it similarly as they can now offer a surgical treatment for a condition with very few treatments. The analytical observer sees a mixed coordination-extraction hybrid (tangled rope), acknowledging both the potential benefits and inherent risks.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_efficacy,
    'What is the long-term efficacy of VNS for TRD compared to other treatments?',
    'Longitudinal studies comparing VNS to alternative therapies and sham stimulation.',
    'If efficacy is low, the treatment becomes primarily extractive. If high, it supports a more coordination-focused classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_efficacy, empirical, 'Uncertainty regarding the long-term effectiveness of VNS for TRD.').

omega_variable(
    patient_selection_criteria,
    'What are the optimal patient selection criteria for VNS to maximize its benefits and minimize harm?',
    'Clinical trials and retrospective data analysis to identify patient characteristics that predict treatment response.',
    'Refined criteria could shift the classification from a snare for some patients to a rope or scaffold if benefits are more clearly defined and targeted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_selection_criteria, empirical, 'Uncertainty regarding the ideal patient selection for VNS therapy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vns_implant_for_trd, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vns__tr_t0, vns_implant_for_trd, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vns__tr_t5, vns_implant_for_trd, theater_ratio, 5, 0.25).
narrative_ontology:measurement(vns__tr_t10, vns_implant_for_trd, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(vns__be_t0, vns_implant_for_trd, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vns__be_t5, vns_implant_for_trd, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(vns__be_t10, vns_implant_for_trd, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
