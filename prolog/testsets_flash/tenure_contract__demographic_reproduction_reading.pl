% ============================================================================
% CONSTRAINT STORY: tenure_contract__demographic_reproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__demographic_reproduction_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tenure_contract__demographic_reproduction_reading
 *   human_readable: Tenure Peer Review as Demographic Gatekeeping
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint describes the operation of tenure peer review within
 *   higher education as a mechanism for demographic gatekeeping. While
 *   ostensibly designed to ensure academic quality and freedom, this reading
 *   argues that the criteria of 'fit' and 'collegiality' are applied in ways
 *   that reproduce the existing demographic composition of faculty,
 *   particularly benefiting demographically dominant groups and
 *   disadvantaging underrepresented candidates. The constraint is claimed as
 *   a Snare due to its high extraction from victims and active suppression of
 *   alternative career paths or challenges to the status quo.
 *
 * KEY AGENTS:
 *   - demographically_dominant_faculty: Primary beneficiary (institutional/arbitrage) — benefits from preferential evaluation and reduced competition.
 *   - underrepresented_faculty_candidates: Primary victim (powerless/identity_locked) — bears the cost of structural exclusion and biased evaluation.
 *   - university_administration: Agenda setter (institutional/constrained) — administers the tenure process, balancing institutional reputation with internal political pressures.
 *   - junior_faculty_of_color: Victim (moderate/identity_locked) — faces higher scrutiny and subjective criteria during the tenure track.
 *   - female_junior_faculty: Victim (moderate/identity_locked) — experiences similar subjective biases and often higher service loads.
 *   - academic_job_market_entrants: Excluded (powerless/trapped) — would challenge the system but lack leverage or voice within the process.
 *   - diversity_equity_inclusion_advocates: Observer (organized/constrained) — analyze and critique the system, pushing for reform but facing institutional inertia.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, 0.78).
domain_priors:suppression_score(tenure_contract__demographic_reproduction_reading, 0.85).
domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__demographic_reproduction_reading, snare).
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Gatekeeping").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, '197ad66e-7595-4f50-b3ac-d5bb556e3cf9').
narrative_ontology:cs_kernel_codification('197ad66e-7595-4f50-b3ac-d5bb556e3cf9', formalized).
narrative_ontology:cs_authority_grounding('197ad66e-7595-4f50-b3ac-d5bb556e3cf9', practice).
narrative_ontology:cs_interpretation_layer_present('197ad66e-7595-4f50-b3ac-d5bb556e3cf9').
narrative_ontology:cs_reading_relation('197ad66e-7595-4f50-b3ac-d5bb556e3cf9', tenure_contract__academic_freedom_reading, influences).
narrative_ontology:cs_reading_relation('197ad66e-7595-4f50-b3ac-d5bb556e3cf9', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('197ad66e-7595-4f50-b3ac-d5bb556e3cf9', foundational, demographic_homogeneity_as_collegiality).
narrative_ontology:cs_axiom_status(demographic_homogeneity_as_collegiality, holdable).
narrative_ontology:cs_axiom_grounding('197ad66e-7595-4f50-b3ac-d5bb556e3cf9', demographic_homogeneity_as_collegiality, conventional).
narrative_ontology:cs_axiom('197ad66e-7595-4f50-b3ac-d5bb556e3cf9', foundational, subjective_fit_as_merit).
narrative_ontology:cs_axiom_status(subjective_fit_as_merit, holdable).
narrative_ontology:cs_axiom_grounding('197ad66e-7595-4f50-b3ac-d5bb556e3cf9', subjective_fit_as_merit, conventional).
narrative_ontology:cs_reference_frame('197ad66e-7595-4f50-b3ac-d5bb556e3cf9', homogeneous_collegial_academy).
narrative_ontology:cs_drift_state('197ad66e-7595-4f50-b3ac-d5bb556e3cf9', contemporary_dei_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('197ad66e-7595-4f50-b3ac-d5bb556e3cf9', '').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, university_administration).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_faculty_candidates).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, junior_faculty_of_color).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, female_junior_faculty).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(tenure_contract__demographic_reproduction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__demographic_reproduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__demographic_reproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the career costs (lost opportunities, emotional labor, delayed advancement) borne by underrepresented faculty who are denied tenure or face biased evaluation. Suppression (0.85) is high due to the limited number of tenure-track positions, the subjective nature of 'fit' criteria, and the professional identity-lock that makes leaving academia a high-cost exit. The theater ratio (0.45) indicates that a significant portion of the peer review process, while framed as meritocratic, serves to maintain existing power structures rather than purely evaluate research productivity. The increasing trend in extractiveness and suppression over the interval reflects the hardening of these gatekeeping mechanisms and the rising stakes in a competitive academic environment.
 *
 * PERSPECTIVAL GAP:
 *   Demographically dominant faculty perceive the tenure system as a legitimate mechanism for quality control and academic freedom (closer to a Rope or even Mountain from their seat). Underrepresented faculty, however, experience it as a highly extractive and suppressive Snare, where subjective criteria are weaponized for demographic reproduction. University administration may view it as a necessary, albeit imperfect, governance tool. The engine's per-seat classification will capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Demographically dominant faculty are beneficiaries (d near 0.0) as the system implicitly favors their reproduction, reducing competition and solidifying their positions. Underrepresented faculty candidates, junior faculty of color, and female junior faculty are victims (d near 1.0) as they bear the direct costs of exclusion and biased evaluation. University administration, while administering the system, also benefits from a stable, predictable faculty composition, even if it's demographically skewed. Academic job market entrants are excluded, facing a system they cannot influence. Diversity, Equity, and Inclusion advocates are analytical observers, attempting to shift the system from the outside.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the original mandate of tenure (protecting academic freedom and ensuring quality) has atrophied, and the system now primarily serves a latent function of demographic reproduction. The classification as a Snare prevents mislabeling this as a legitimate coordination mechanism (Rope) or a natural outcome (Mountain), highlighting the active extraction and suppression involved. The 'contested' status of the founding problem corroborates this mandatrophy, as the original problem of academic freedom is now secondary to the problem of gatekeeping.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_fit_vs_structural_bias,
    'Are ''fit'' and ''collegiality'' criteria genuinely meritocratic assessments of academic contribution, or are they proxies for demographic and cultural similarity?',
    'Longitudinal studies correlating ''fit'' evaluations with demographic characteristics and subsequent research productivity, controlling for objective metrics. Disaggregated data on tenure success rates by demographic group.',
    'If proxies, the constraint''s extractiveness and suppression are higher than acknowledged, and its claimed coordination function (ensuring quality) is largely theatrical. This would shift the classification further towards Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_fit_vs_structural_bias, empirical, 'Ambiguity of ''fit'' and ''collegiality'' criteria in tenure review.').

omega_variable(
    tenure_kernel_reading_divergence,
    'This constraint is a ''demographic_reproduction_reading'' of the ''tenure_contract'' kernel. How would the classification change under the ''academic_freedom_reading'' or ''institutional_extraction_reading''?',
    'Analyzing the same structural data through the lens of each sibling reading, focusing on beneficiaries, victims, and the primary function served. The engine''s multi-reading analysis will compute this.',
    'The ''academic_freedom_reading'' would likely classify as a Rope or Scaffold, emphasizing coordination and temporary support for inquiry. The ''institutional_extraction_reading'' would likely classify as a Snare, but with different beneficiaries (early winners) and victims (contingent labor). This reading highlights demographic exclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tenure_kernel_reading_divergence, conceptual, 'Impact of alternative readings of the tenure contract kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (lack of alternative career paths, institutional power dynamics) or internalized (self-censorship, identity-lock from professional socialization)?',
    'Post-exit career trajectories and qualitative interviews with faculty who left academia due to tenure denial. If suppression persists as self-limiting beliefs or career path dependence after institutional barriers are removed, it indicates internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as targets carry the suppression with them. This would amplify the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in tenure gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__demographic_reproduction_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(tenu_tr_t10, tenure_contract__demographic_reproduction_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(tenu_tr_t20, tenure_contract__demographic_reproduction_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(tenu_tr_t30, tenure_contract__demographic_reproduction_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__demographic_reproduction_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(tenu_be_t10, tenure_contract__demographic_reproduction_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(tenu_be_t20, tenure_contract__demographic_reproduction_reading, base_extractiveness, 20, 0.73).
narrative_ontology:measurement(tenu_be_t30, tenure_contract__demographic_reproduction_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__demographic_reproduction_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(tenu_su_t10, tenure_contract__demographic_reproduction_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(tenu_su_t20, tenure_contract__demographic_reproduction_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(tenu_su_t30, tenure_contract__demographic_reproduction_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, identity_coordination).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, university_hiring_practices).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, academic_publishing_metrics).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'tenure_contract' kernel. Each reading highlights a different structural function and has a different ε value. They are linked to capture their interdependencies and the contested nature of tenure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
