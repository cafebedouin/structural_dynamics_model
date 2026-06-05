% ============================================================================
% CONSTRAINT STORY: pancreatic_cancer_lethality_v1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pancreatic_cancer_lethality_v1
 *   human_readable: Biological Lethality of Pancreatic Adenocarcinoma
 *   domain: medical/oncology/biological
 *
 * SUMMARY:
 *   Pancreatic adenocarcinoma exemplifies a constraint that arises from
 *   natural biological law rather than institutional arrangement, policy
 *   choice, or coordination failure. The disease is characterized by
 *   retroperitoneal location (limiting surgical resectability), aggressive
 *   histology (nearly universal KRAS mutations, high mitotic index), early
 *   hematogenous dissemination (often metastatic at presentation), and
 *   tumor-intrinsic chemoresistance (through multidrug efflux, DNA repair
 *   enhancement, and microenvironmental barriers). The 5-year survival rate
 *   of <10% has remained substantially unchanged for 30 years despite
 *   improvements in chemotherapy, targeted therapy, and supportive care. This
 *   persistence suggests the constraint is rooted in irreducible biological
 *   complexity rather than gaps in clinical knowledge or therapeutic
 *   delivery. The constraint is invariant across observables: measured by
 *   overall survival, disease-free survival, response rate, or
 *   molecular-level progression markers, the biological lethality is
 *   consistent and intrinsic. No institutional reorganization, policy
 *   intervention, or care model innovation has substantially altered the
 *   survival outcome — because the constraint is not institutional but
 *   biological.
 *
 * KEY AGENTS:
 *   - Patients diagnosed with pancreatic adenocarcinoma: Primary agent bearing the constraint (powerless/trapped) — face the biological lethality directly
 *   - Treating oncologists: Secondary agent constrained by biology (moderate/constrained) — cannot negotiate with tumor biology, only respond within biological limits
 *   - Research oncology community: Organized institutional actors (organized/mobile) — invest generations of effort; constraint persists despite research sophistication
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — observes that the constraint is structural and biological, not contingent and institutional
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
narrative_ontology:topic_domain(pancreatic_cancer_lethality_v1, "medical/oncology/biological").

domain_priors:emerges_naturally(pancreatic_cancer_lethality_v1).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PATIENT (MOUNTAIN) — A patient diagnosed with pancreatic adenocarcinoma faces a constraint that is structurally identical to a physical law: the biological realities of tumor location (retroperitoneal, adjacent to vital vasculature), aggressive histology (ductal carcinoma predominance, high mitotic rate), early metastatic dissemination, and chemoresistance pathways are intrinsic to the disease phenotype. No negotiation, arbitrage, or institutional escape exists. The 5-year survival rate of <10% is not a policy choice or coordination failure — it is an emergent property of cellular biology. d≈1.0, f(d)≈1.42, σ=1.0 → χ≈0.11, classified as Mountain because the constraint emerges from natural law.
constraint_indexing:constraint_classification(pancreatic_cancer_lethality_v1, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE TREATING ONCOLOGIST (MOUNTAIN) — The oncologist's options are constrained by the same natural laws. Gemcitabine + nab-paclitaxel (FOLFIRINOX) represents the current standard; median overall survival is 11–13 months with treatment, versus 4–5 months without. This is not a failure of medical will but a ceiling imposed by tumor biology: KRAS mutations drive oncogenic signaling, desmoplastic stroma impedes drug penetration, and epithelial-mesenchymal transition programs enable dissemination. The constraint is accessible (the biology is measurable, drug mechanisms are understood) but resistance to therapeutic manipulation is extreme. d≈0.60, f(d)≈0.75, σ=1.0 → χ≈0.06, classified as Mountain because intervention capacity is bounded by biological law.
constraint_indexing:constraint_classification(pancreatic_cancer_lethality_v1, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: THE RESEARCH ONCOLOGY COMMUNITY (MOUNTAIN) — Organized cancer biology research has made incremental progress (improved chemotherapy combinations, targeted therapies for small subsets, immunotherapy trials) but the 5-year survival floor remains <10% over a 30-year interval. The constraint persists across multiple research generations: the fundamental barrier is not ignorance of biology but the exponential growth disadvantage and adaptive capacity of metastatic tumors. KRAS mutations are nearly universal in pancreatic adenocarcinoma; KRAS inhibition has modest efficacy even in engineered cell systems. This suggests the constraint is rooted in irreducible biological complexity. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.05, classified as Mountain because the constraint has survived multiple generations of research effort without fundamental breakthrough.
constraint_indexing:constraint_classification(pancreatic_cancer_lethality_v1, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER / CIVILIZATIONAL VIEW (MOUNTAIN) — From a civilizational timescale, pancreatic cancer lethality reflects a deep biological constraint: competition dynamics between host immune surveillance and tumor adaptive capacity are governed by principles of population genetics and evolutionary game theory. Tumors that evade immunity and chemotherapy do so through mechanisms (mutation, clonal selection, epigenetic plasticity) that operate on timescales (days to months) much faster than immune or therapeutic response (weeks to months). The constraint emerges from the fundamental asymmetry: tumor cells have access to mutation rates (10^-9 to 10^-10 per base pair per cell division) and are selected for trait combinations (growth, invasion, metastasis, immunoevasion) that are orthogonal to survival of the patient. This is not a policy failure or coordination problem — it is a structural property of biological competition. ε=0.08 reflects that the constraint is narrowly defined (lethality from adenocarcinoma specifically, not oncology in general). d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09, classified as Mountain because the constraint is inevitable under biological law.
constraint_indexing:constraint_classification(pancreatic_cancer_lethality_v1, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

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
 *   Extractiveness (ε=0.08): Very low, at the natural law boundary. The constraint is not extractive in the institutional sense — no agent 'extracts' from the patient in terms of resource transfer or asymmetric benefit. The lethality is a property of the malignancy itself, not of any institutional or policy arrangement. The low ε reflects that the constraint is purely structural, with no coordination or suppression mechanism — it simply is. Suppression (0.02): Minimal. The constraint cannot be suppressed because it is not suppressible — it is biological. There is no alternative pathway that would change the outcome if only institutional barriers were removed. Accessibility Collapse (0.92): Very high. The biological mechanisms of pancreatic cancer lethality are highly accessible to measurement and understanding: tumor biology is well-characterized at the molecular, cellular, and organismal levels. Genomic sequencing, histopathology, and animal models provide clear access to the underlying constraints. Resistance (0.08): Very low. Despite this accessibility, resistance to therapeutic manipulation is extreme: attempts to interfere with KRAS signaling, immune evasion, chemoresistance, or metastatic dissemination have yielded minimal survival gains. This is characteristic of a mountain constraint — it is accessible but resistant to alteration. Theater Ratio (0.15): Very low. Clinical pancreatic cancer care involves minimal theatrical performance; treatments are administered based on biological plausibility (chemotherapy proven effective in comparative trials) and clinical judgment. The focus is on functional outcomes (survival, quality of life) rather than performative ritual.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap because the biological lethality is invariant across all observational positions. The patient, oncologist, research community, and analytical observer all perceive the same structural reality: a malignancy that is deadly by biological necessity, not institutional contingency. All perspectives classify as Mountain because the constraint is rooted in natural law. The lack of perspectival gap is diagnostic of a true mountain: if the constraint classified differently from different positions (e.g., snare to the patient, rope to the institution, mountain to the analytical observer), the constraint would be decomposable into institutional and biological components. The fact that all perspectives converge on mountain classification suggests that institutional and policy factors, while present, do not substantially alter the underlying biological lethality constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives derive high d values (patient: d≈1.0, oncologist: d≈0.60, researcher: d≈0.50, observer: d≈0.72) because all agents are targets of or constrained by the biological law, not beneficiaries. There is no agent that benefits from the constraint in the sense of institutional extraction. The constraint is not asymmetric in terms of who pays and who benefits — it is asymmetric only in the sense that the tumor's adaptive capacity exceeds the host's defensive capacity. This asymmetry is biological, not institutional, and reflects the evolutionary logic of competition between organisms (host) and cells (tumor). No directionality overrides are needed because the structural data accurately reflects the biological reality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pancreatic_cancer_lethality_v1, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% Pancreatic cancer lethality is a pure natural law constraint with no institutional decomposition. Clinical outcomes, policy interventions, and care models may modify the hospice experience or quality-of-life trajectory, but they do not alter the fundamental biological lethality. If a story were written about 'access to pancreatic cancer treatment' or 'disparities in survival by socioeconomic status,' those would be separate constraints (institutional/policy) with different ε values, distinct from the biological lethality constraint itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
