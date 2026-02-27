% ============================================================================
% CONSTRAINT STORY: pancreatic_cancer_lethality_v1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pancreatic_cancer_lethality_v1, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pancreatic_cancer_lethality_v1
 *   human_readable: Biological Lethality of Pancreatic Adenocarcinoma
 *   domain: technological/medical/oncology
 *
 * SUMMARY:
 *   Pancreatic adenocarcinoma's biological lethality represents a natural law
 *   constraint in the Deferential Realism framework. The 5-year survival rate
 *   below 10% is not the product of institutional failure, coordination
 *   breakdown, or policy misallocation — it is an emergent property of
 *   irreducible biological, anatomical, and temporal constraints. The
 *   pancreas's retroperitoneal location limits surgical resection margins;
 *   the cancer's intrinsic chemotherapy resistance defeats pharmaceutical
 *   intervention; the disease's aggressive dissemination and late-stage
 *   presentation at diagnosis eliminate curative options. This constraint is
 *   invariant across all technological approaches and persists despite
 *   decades of research investment, clinical trial infrastructure, and
 *   molecular profiling advances. The lethality manifests identically whether
 *   evaluated through early detection metrics, therapeutic response rates, or
 *   survival curves — it is a mountain: unchangeable and fixed.
 *
 * KEY AGENTS:
 *   - Afflicted Patient: Primary constraint-bearer (powerless/trapped/biographical) — confronts biological lethality with no exit option
 *   - Oncologist: Clinical translator (moderate/analytical/generational) — observes constraints as irreducible biological realities, not policy failures
 *   - Research Community: Institutional investigator (organized/analytical/civilizational) — documents plateau in survival despite optimized therapeutics
 *   - Analytical Observer: Theoretical perspective (institutional/analytical/civilizational) — identifies constraint as structural property of pancreatic anatomy and cancer biology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pancreatic_cancer_lethality_v1, 0.08).
domain_priors:suppression_score(pancreatic_cancer_lethality_v1, 0.02).
domain_priors:theater_ratio(pancreatic_cancer_lethality_v1, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pancreatic_cancer_lethality_v1, extractiveness, 0.08).
narrative_ontology:constraint_metric(pancreatic_cancer_lethality_v1, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(pancreatic_cancer_lethality_v1, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(pancreatic_cancer_lethality_v1, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(pancreatic_cancer_lethality_v1, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pancreatic_cancer_lethality_v1, mountain).
narrative_ontology:human_readable(pancreatic_cancer_lethality_v1, "Biological Lethality of Pancreatic Adenocarcinoma").
narrative_ontology:topic_domain(pancreatic_cancer_lethality_v1, "technological/medical/oncology").

domain_priors:emerges_naturally(pancreatic_cancer_lethality_v1).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE AFFLICTED PATIENT (MOUNTAIN) — No exit option exists. Pancreatic cancer's biological lethality is a constraint imposed by cellular and systemic biology that transcends institutional or political solutions. The patient confronts an immutable natural limit: the cancer's aggressive phenotype, late detection biology, and limited organ tolerance. From this perspective, the constraint is purely natural law — no degree of freedom, no negotiation, no escape route except palliative care or experimental intervention with minimal survival impact.
constraint_indexing:constraint_classification(pancreatic_cancer_lethality_v1, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE ONCOLOGIST (MOUNTAIN) — From the position of clinical practice, pancreatic cancer lethality appears as a structural natural law that resists technological intervention. Current surgical, chemotherapeutic, and radiation approaches marginally extend survival (months, not years). The constraint is not one of knowledge gaps or policy failures — it is a deep biological property: early dissemination, intrinsic chemotherapy resistance, anatomical constraints on resection, and the organ's critical endocrine function. Even as an organized, educated, therapeutically capable agent, the oncologist encounters the constraint as irreducible.
constraint_indexing:constraint_classification(pancreatic_cancer_lethality_v1, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational, universal scope, pancreatic cancer lethality is an invariant constraint on human biology. The high lethality emerges from irreducible constraints: (1) Anatomical: the pancreas is deeply embedded in the retroperitoneum, limiting resection margins and allowing early lymphatic/vascular invasion. (2) Cellular: pancreatic adenocarcinoma exhibits intrinsic resistance to chemotherapy and radiation through desmoplastic stroma, poor drug penetration, and multi-drug transporter activity. (3) Temporal: most pancreatic cancers are detected at stage III-IV, when curative intervention is impossible. These constraints are not policy failures, institutional failures, or coordination problems — they are structural features of pancreatic anatomy and cancer biology that persist across all current technological approaches.
constraint_indexing:constraint_classification(pancreatic_cancer_lethality_v1, mountain,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE RESEARCH COMMUNITY (MOUNTAIN) — Despite decades of research investment, molecular profiling, immunotherapy trials, and combination regimens, the survival plateau persists. This is not evidence of insufficient effort or coordination failure — it is evidence of a deep constraint. The research community observes that pancreatic cancer lethality is not contingent on policy, funding allocation, or institutional arrangements. The constraint is structural: the biology of this cancer type, the constraints of human anatomy, and the limitations of current-generation therapeutic modalities. The mountain persists across all observables and measurement frameworks.
constraint_indexing:constraint_classification(pancreatic_cancer_lethality_v1, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pancreatic_cancer_lethality_v1_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(pancreatic_cancer_lethality_v1, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pancreatic_cancer_lethality_v1, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(pancreatic_cancer_lethality_v1, ExtMetricName, E),
    domain_priors:suppression_score(pancreatic_cancer_lethality_v1, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(pancreatic_cancer_lethality_v1),
    narrative_ontology:constraint_metric(pancreatic_cancer_lethality_v1, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(pancreatic_cancer_lethality_v1, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(pancreatic_cancer_lethality_v1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The constraint is not extractive — no agent captures value or benefit from pancreatic cancer lethality. The low value reflects that this is a pure natural limit, not a mechanism that transfers resources from one party to another. Suppression (0.02): Negligible. There is no suppression of alternatives — the biological mechanisms operate transparently. All agents (patients, physicians, researchers) understand the constraint fully; the problem is not information asymmetry or institutional gatekeeping, but the irreducible difficulty of the constraint itself. Theater ratio (0.15): Very low. Clinical management of pancreatic cancer involves genuine therapeutic attempts (surgery, chemotherapy, radiation), not performative ritual. The theater ratio reflects only the margins of uncertainty in prognosis discussions and supportive care framing — the bulk of clinical activity is substantive engagement with an intractable biological problem. The flatness of the measurement trajectory over 50 years reflects the constraint's invariance — lethality does not improve substantially, indicating that the constraint is not socially contingent.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap across the four perspectives — they all classify as mountain. This uniformity across observables (patient experience, clinical practice, research outcomes, analytical review) is itself evidence of the mountain classification. If the constraint were social or institutional in origin, we would expect different perspectives (e.g., beneficiaries seeing rope, victims seeing snare). The fact that all perspectives converge on the same immutable classification indicates that the constraint is truly structural: a property of biology itself, not of human arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint has no directionality in the traditional sense because there are no structural beneficiaries or victims relative to the constraint's operation. The constraint is not extractive — it does not advantage one agent at the expense of another. All agents (patients, clinicians, researchers, institutions) are uniformly constrained by the same biological reality. The absence of beneficiary/victim structure is itself evidence of mountain classification: a true natural law does not serve interests; it simply delimits what is possible for all who encounter it.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN ONLY: No mandatrophy resolution required. This constraint exhibits zero degrees of freedom across all indices. The five-year survival rate is not negotiable through policy, coordination, or institutional design. The constraint is invariant across all measurement observables: anatomical analysis, molecular profiling, clinical trial outcomes, and epidemiological tracking all reveal the same irreducible lethality. The mountain classification is robust against alternative measurement frameworks or observables. This is a canonical natural law in the medical domain — comparable to fundamental limits in physics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    early_detection_possibility,
    'Could earlier detection through biomarker screening convert the constraint from mountain to rope by enabling curative resection before dissemination?',
    'Longitudinal prospective screening studies correlating early biomarker detection with R0 resection rates and 5-year survival outcomes in screened vs unscreened populations',
    'If early detection sufficient: survival plateau is detection-timing constraint (potentially rope/scaffold, not mountain). If early detection insufficient: confirms mountain status — even detected-early cancers have similar poor survival, indicating intrinsic biological constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(early_detection_possibility, empirical, 'Whether earlier detection converts lethality from mountain to contingent constraint').

omega_variable(
    chemotherapy_resistance_mechanism,
    'Is pancreatic adenocarcinoma''s chemotherapy resistance an intrinsic cellular property (mountain) or a tumor microenvironment problem (potentially reversible via architecture modification)?',
    'Comparative genomics and in vitro chemosensitivity across organ sites; desmoplastic stroma ablation studies in mouse models with chemotherapy response monitoring; single-cell transcriptomics identifying resistance mechanisms as cell-intrinsic vs stromal-derived',
    'If intrinsic: confirms mountain — biological property of the cancer cells themselves. If stromal/architectural: suggests rope/tangled rope — technological solution exists but requires coordination or sustained intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chemotherapy_resistance_mechanism, empirical, 'Whether chemotherapy resistance is intrinsic or microenvironment-mediated').

omega_variable(
    anatomical_constraint_fundamentality,
    'Is the pancreas''s retroperitoneal location and proximity to vital structures a fundamental anatomical constraint or a limitation of current surgical technique?',
    'Comparative anatomy of pancreatectomy margins across species; analysis of R0 resection rates in specialized high-volume centers vs general surgery settings; outcome correlation with degree of gross residual disease left behind',
    'If fundamental: confirms mountain — anatomy imposes limits regardless of surgeon skill. If technical: suggests rope — training, volume, and technique can extend curative resection rates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(anatomical_constraint_fundamentality, empirical, 'Whether retroperitoneal location imposes fundamental resection limits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pancreatic_cancer_lethality_v1, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(panc_lethality_tr_t0, pancreatic_cancer_lethality_v1, theater_ratio, 0, 0.12).
narrative_ontology:measurement(panc_lethality_tr_t25, pancreatic_cancer_lethality_v1, theater_ratio, 25, 0.14).
narrative_ontology:measurement(panc_lethality_tr_t50, pancreatic_cancer_lethality_v1, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(panc_lethality_be_t0, pancreatic_cancer_lethality_v1, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(panc_lethality_be_t25, pancreatic_cancer_lethality_v1, base_extractiveness, 25, 0.08).
narrative_ontology:measurement(panc_lethality_be_t50, pancreatic_cancer_lethality_v1, base_extractiveness, 50, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pancreatic_cancer_lethality_v1, information_standard).
narrative_ontology:affects_constraint(pancreatic_cancer_lethality_v1, cancer_screening_access).
narrative_ontology:affects_constraint(pancreatic_cancer_lethality_v1, oncology_resource_allocation).
narrative_ontology:affects_constraint(pancreatic_cancer_lethality_v1, end_of_life_care_frameworks).

% DUAL FORMULATION NOTE:
% Pancreatic cancer lethality is a natural law constraint (mountain) that affects downstream policy constraints in screening, resource allocation, and care frameworks. These downstream constraints may be rope, tangled_rope, or snare depending on institutional response, but they are all structured around the irreducible upstream natural law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
