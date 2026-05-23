% ============================================================================
% CONSTRAINT STORY: career_duration_compression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_career_duration_compression, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: career_duration_compression
 *   human_readable: Career Duration Compression in Clinical Medicine
 *   domain: healthcare_workforce_economics/organizational_sociology
 *
 * SUMMARY:
 *   Career duration compression in clinical medicine — the decline in mean
 *   clinical career length from 57.1 years (2008 cohort) to 48.1 years (2024
 *   cohort), coupled with an 11% 'never practiced' subpopulation among
 *   residency graduates — presents as a mountain constraint from all measured
 *   perspectives. Clinicians, healthcare systems, and analytical observers
 *   all perceive shortened careers as an inevitable feature of modern
 *   medicine, driven by generational shifts, work-life rebalancing, or the
 *   inherent intensity of contemporary clinical practice. However, the
 *   constraint has identifiable beneficiaries (healthcare systems that
 *   capture value from workforce churn through reduced salary costs and
 *   pension obligations) and victims (clinicians who exit prematurely and
 *   patients who lose continuity), raising the possibility that the mountain
 *   classification represents successful naturalization of a constructed
 *   constraint rather than genuine natural law. The constraint is downstream
 *   of two higher-extraction mechanisms: administrative_extraction_mechanism
 *   (tangled_rope, ε=0.42) and gendered_retention_asymmetry (snare, ε=0.68),
 *   suggesting that what appears as an immutable demographic trend may be the
 *   cumulative effect of extractive upstream constraints. The false summit
 *   hypothesis: career compression is presented as natural law ('this is just
 *   how modern medicine works') to obscure the structural beneficiaries of
 *   workforce churn.
 *
 * KEY AGENTS:
 *   - Departing Clinician: Primary victim (powerless/trapped) — bears full cost of premature exit after decade+ training investment; perceives compression as unchangeable
 *   - Mid-Career Physician: Secondary victim (moderate/constrained) — can switch settings at high cost but cannot escape underlying structural forces
 *   - Healthcare System: Primary beneficiary (institutional/arbitrage) — captures value from workforce churn (lower salary costs, reduced pension obligations) while framing compression as external demographic shift
 *   - Patient Continuity: Abstract victim (powerless/trapped) — collective good that cannot organize or exit; bears cost of reduced clinician experience and relationship disruption
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent organizational arrangements as inevitable features of post-industrial knowledge work
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(career_duration_compression, 0.05).
domain_priors:suppression_score(career_duration_compression, 0.03).
domain_priors:theater_ratio(career_duration_compression, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(career_duration_compression, extractiveness, 0.05).
narrative_ontology:constraint_metric(career_duration_compression, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(career_duration_compression, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(career_duration_compression, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(career_duration_compression, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(career_duration_compression, mountain).
narrative_ontology:human_readable(career_duration_compression, "Career Duration Compression in Clinical Medicine").
narrative_ontology:topic_domain(career_duration_compression, "healthcare_workforce_economics/organizational_sociology").

domain_priors:emerges_naturally(career_duration_compression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(career_duration_compression, healthcare_systems).
narrative_ontology:constraint_beneficiary(career_duration_compression, administrative_structures).
narrative_ontology:constraint_victim(career_duration_compression, clinical_workforce).
narrative_ontology:constraint_victim(career_duration_compression, patient_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPARTING CLINICIAN (MOUNTAIN) — Experiences career compression as an immutable force. Individual clinicians see burnout, administrative burden, and work-life incompatibility as unchangeable features of modern medicine. No exit from the profession without abandoning decade+ of training investment. Perceives the constraint as natural law.
constraint_indexing:constraint_classification(career_duration_compression, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-CAREER PHYSICIAN (MOUNTAIN) — Constrained by practice ownership, patient panels, and geographic ties, but still perceives career compression as inevitable. Can switch specialties or practice settings at high cost, but cannot escape the underlying structural forces driving early exit across all settings.
constraint_indexing:constraint_classification(career_duration_compression, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HEALTHCARE SYSTEM (MOUNTAIN) — Benefits from workforce churn (lower salary costs for early-career clinicians, reduced pension obligations) but perceives career compression as an external demographic/generational shift beyond institutional control. Treats shortened careers as a natural constraint requiring adaptation, not a phenomenon the system produces.
constraint_indexing:constraint_classification(career_duration_compression, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, career compression appears as a universal trend across high-skill professions in post-industrial economies: knowledge work intensity, credential inflation, and work-life rebalancing all point toward shorter career arcs as an inevitable feature of late-stage capitalism. However, the structural data contradicts this — the constraint has identifiable beneficiaries (systems that capture value from churn) and victims (clinicians and patients), suggesting the 'natural law' framing may be a false summit.
constraint_indexing:constraint_classification(career_duration_compression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(career_duration_compression_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(career_duration_compression, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(career_duration_compression, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(career_duration_compression, ExtMetricName, E),
    domain_priors:suppression_score(career_duration_compression, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(career_duration_compression),
    narrative_ontology:constraint_metric(career_duration_compression, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(career_duration_compression, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(career_duration_compression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.05): Very low, at the mountain threshold. The constraint extracts minimally in its direct form — career compression itself is a temporal phenomenon (shorter careers) rather than an active extraction mechanism. The extraction occurs upstream (administrative burden, gendered retention barriers) and manifests downstream as compressed duration. The low ε reflects that the compression, considered in isolation from its causes, appears as a demographic shift rather than an extractive process. Suppression (0.03): Very low. Clinicians can exit at any time; the constraint does not actively prevent departure. The 'suppression' is the sunk cost of training investment, which is a historical fact rather than an ongoing coercive mechanism. Theater ratio (0.08): Very low, rising slightly over the interval. Career compression is not performative — the exits are real, the shortened careers are real. The minimal theater reflects genuine structural change rather than symbolic activity. Accessibility collapse (0.92): Very high. All agents perceive career compression as unchangeable within their biographical time horizon. Individual clinicians cannot extend their careers by individual action when the structural forces (administrative burden, work intensity, compensation structure) are system-level. Resistance (0.08): Very low. Attempts to resist career compression (individual clinicians trying to sustain long careers, systems trying to retain senior clinicians) face overwhelming structural headwinds. The constraint reasserts itself across all practice settings and specialties.
 *
 * PERSPECTIVAL GAP:
 *   The absence of a perspectival gap is itself diagnostic. All four perspectives — powerless/trapped clinician, moderate/constrained mid-career physician, institutional/arbitrage healthcare system, and analytical/analytical observer — classify career compression as mountain. This universal agreement on immutability is unusual in the DR framework, where most constraints show type variation across perspectives. The uniformity suggests either (1) genuine natural law, where the constraint is truly unchangeable from all positions, or (2) successful naturalization, where a constructed constraint has been so thoroughly normalized that even its beneficiaries perceive it as inevitable. The structural data supports interpretation (2): the constraint has identifiable beneficiaries (systems capturing churn value), identifiable victims (clinicians and patients), and is downstream of two higher-extraction mechanisms (administrative burden and gendered retention barriers) that are demonstrably contingent on organizational choices. The false summit detector should flag this constraint for reclassification: the mountain consensus may reflect ideological capture rather than structural immutability.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's beneficiary/victim structure is subtle because career compression appears as a passive demographic trend rather than an active extraction mechanism. Healthcare systems are declared as beneficiaries because they capture value from workforce churn: early-career clinicians have lower salary costs, reduced pension obligations accrue when careers shorten, and turnover enables systems to avoid long-term employment commitments. However, systems perceive themselves as victims of an external demographic shift ('we can't retain clinicians because of generational preferences'), which is why the institutional perspective classifies as mountain despite beneficiary status. This is the false summit signature: the beneficiary perceives the constraint as natural law. Clinicians are declared as victims because they bear the cost of premature exit: lost income over shortened careers, sunk training investment that yields fewer practice years, and the psychological cost of leaving a profession they trained for. The directionality derivation chain will compute low d for healthcare systems (beneficiary + arbitrage exit → low extraction experienced) and high d for clinicians (victim + trapped exit → high extraction experienced), but the mountain classification from all perspectives overrides these differences — all agents perceive the constraint as immutable regardless of their structural position. This universal mountain perception despite asymmetric benefit/cost distribution is the diagnostic signature of a false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by demonstrating that a mountain classification can be a false summit — a constructed constraint successfully naturalized across all perspectives. The low extractiveness (0.05) and low suppression (0.03) are genuine: career compression, considered in isolation, is not actively extractive or coercive. But the constraint is downstream of higher-extraction mechanisms (administrative_extraction_mechanism at ε=0.42, gendered_retention_asymmetry at ε=0.68), and its beneficiaries (healthcare systems) perceive it as natural law despite capturing value from it. This is not a misclassification — it is a correct classification of a naturalized constraint. The mandatrophy is resolved by recognizing that mountains can be false summits, and that universal mountain perception is a signal for false summit detection rather than confirmation of natural law. The omega variables document the empirical tests that would distinguish genuine natural law (career compression is universal across all healthcare system structures and historical eras) from naturalized extraction (career compression is contingent on specific organizational arrangements that benefit from workforce churn).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generational_preference_vs_structural_coercion,
    'Is career compression driven by autonomous generational preference shifts (younger clinicians choosing shorter careers) or by structural coercion (systems making long careers untenable)?',
    'Longitudinal survey data comparing stated career intentions at residency entry vs actual exit timing; exit interview analysis distinguishing voluntary departure from burnout-driven exit; comparison of career duration in systems with different administrative burden levels',
    'If preference-driven: mountain classification holds — compression is a demographic shift. If coercion-driven: reclassify as snare or tangled_rope — compression is an extraction mechanism with identifiable beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generational_preference_vs_structural_coercion, empirical, 'Whether career compression reflects autonomous preference or structural coercion').

omega_variable(
    administrative_extraction_causality,
    'Does administrative burden directly cause early exit, or is it correlated with exit through a third variable (e.g., system financialization driving both administrative expansion and clinician dissatisfaction)?',
    'Natural experiment analysis comparing career duration in systems that reduced administrative burden vs matched controls; regression discontinuity at policy changes affecting documentation requirements',
    'If direct causation: career compression is downstream of administrative_extraction_mechanism (tangled_rope). If third-variable: compression may be genuinely structural (mountain) with administrative burden as a symptom rather than cause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_extraction_causality, empirical, 'Causal relationship between administrative burden and career exit').

omega_variable(
    never_practiced_subpopulation_mechanism,
    'Does the ''never practiced'' subpopulation (11% of residency graduates) represent rational selection out of an untenable system, or does it represent a distinct phenomenon (e.g., residency as credentialing for non-clinical roles)?',
    'Career trajectory analysis of never-practiced cohort: what roles do they enter? Comparison of debt burden, specialty choice, and stated reasons for non-entry across never-practiced vs early-exit vs long-career cohorts',
    'If rational exit: strengthens snare classification — the system is so extractive that informed agents avoid entry. If credentialing pathway: weakens extraction interpretation — residency serves a coordination function for non-clinical healthcare roles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(never_practiced_subpopulation_mechanism, empirical, 'Whether never-practiced subpopulation represents system avoidance or alternative credentialing').

omega_variable(
    false_summit_naturalization,
    'Is the mountain classification from all perspectives evidence of genuine natural law, or evidence of successful naturalization of a constructed constraint?',
    'Cross-national comparison: do healthcare systems with different organizational structures (single-payer, corporatized, physician-owned) show similar career compression trajectories? Historical comparison: was career compression present in earlier eras with different administrative structures?',
    'If universal across systems and eras: mountain holds. If variable by system structure: false summit — the constraint is contingent on specific organizational arrangements that benefit from workforce churn.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization, empirical, 'Whether universal mountain classification indicates natural law or naturalized extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(career_duration_compression, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_2008, career_duration_compression, theater_ratio, 0, 0.05).
narrative_ontology:measurement(theater_2016, career_duration_compression, theater_ratio, 8, 0.06).
narrative_ontology:measurement(theater_2024, career_duration_compression, theater_ratio, 16, 0.08).

% Extraction over time
narrative_ontology:measurement(extract_2008, career_duration_compression, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(extract_2016, career_duration_compression, base_extractiveness, 8, 0.04).
narrative_ontology:measurement(extract_2024, career_duration_compression, base_extractiveness, 16, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(career_duration_compression, resource_allocation).

% DUAL FORMULATION NOTE:
% Career duration compression is downstream of administrative_extraction_mechanism and gendered_retention_asymmetry. The upstream constraints have higher extractiveness values (0.42 and 0.68 respectively) reflecting active extraction mechanisms. Career compression has low extractiveness (0.05) because it is the cumulative effect of upstream extraction rather than an independent extractive process. The network structure is: administrative_extraction_mechanism → career_duration_compression; gendered_retention_asymmetry → career_duration_compression. This constraint does not affect downstream constraints — it is a terminal node in the healthcare workforce constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
