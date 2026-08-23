% ============================================================================
% CONSTRAINT STORY: tenure_contract__demographic_reproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
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
 *   Tenure peer review presents itself as a meritocratic quality-control
 *   mechanism protecting academic freedom. This reading argues that the
 *   actual operation of 'fit' and 'collegiality' criteria functions as
 *   demographic gatekeeping, reproducing the dominant-group composition of
 *   the professoriate. The constraint extracts career advancement and
 *   epistemic authority from underrepresented scholars and transfers it to
 *   dominant groups, while suppressing alternative evaluation frameworks. The
 *   academic freedom justification becomes a cover story when the system's
 *   primary observable effect is demographic closure.
 *
 * KEY AGENTS:
 *   - dominant_demographic_groups: Primary beneficiary (organized/arbitrage) — receives preferential evaluation and career security
 *   - incumbent_tenured_faculty: Primary agenda_setter (institutional/arbitrage) — defines and enforces subjective criteria
 *   - underrepresented_faculty: Primary payer (moderate/constrained) — bears extraction through discounted productivity and fit scrutiny
 *   - marginalized_scholars: Primary payer (powerless/trapped) — faces anticipatory gatekeeping and career distortion
 *   - tenure_committees: Secondary agenda_setter (institutional/constrained) — executes the gatekeeping
 *   - university_administration: Secondary agenda_setter (institutional/arbitrage) — legitimates and resources the system
 *   - students_and_public: Excluded (powerless/trapped) — bears long-term epistemic and representational costs
 *   - critical_scholarship_observers: Observer (analytical/analytical) — documents the mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, 0.72).
domain_priors:suppression_score(tenure_contract__demographic_reproduction_reading, 0.78).
domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__demographic_reproduction_reading, snare).
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Gatekeeping").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, 'fa44d95d-f4dc-47e8-b42f-12d9cc30e704').
narrative_ontology:cs_kernel_codification('fa44d95d-f4dc-47e8-b42f-12d9cc30e704', formalized).
narrative_ontology:cs_authority_grounding('fa44d95d-f4dc-47e8-b42f-12d9cc30e704', extraction).
narrative_ontology:cs_interpretation_layer_present('fa44d95d-f4dc-47e8-b42f-12d9cc30e704').
narrative_ontology:cs_reading_relation('fa44d95d-f4dc-47e8-b42f-12d9cc30e704', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa44d95d-f4dc-47e8-b42f-12d9cc30e704', tenure_contract__institutional_extraction_reading, influences).
narrative_ontology:cs_axiom('fa44d95d-f4dc-47e8-b42f-12d9cc30e704', foundational, demographic_composition_as_tenure_criterion).
narrative_ontology:cs_axiom_status(demographic_composition_as_tenure_criterion, holdable).
narrative_ontology:cs_axiom_grounding('fa44d95d-f4dc-47e8-b42f-12d9cc30e704', demographic_composition_as_tenure_criterion, empirically_contingent).
narrative_ontology:cs_axiom('fa44d95d-f4dc-47e8-b42f-12d9cc30e704', secondary, fit_and_collegiality_as_exclusionary_mechanisms).
narrative_ontology:cs_axiom_status(fit_and_collegiality_as_exclusionary_mechanisms, holdable).
narrative_ontology:cs_axiom_grounding('fa44d95d-f4dc-47e8-b42f-12d9cc30e704', fit_and_collegiality_as_exclusionary_mechanisms, empirically_contingent).
narrative_ontology:cs_reference_frame('fa44d95d-f4dc-47e8-b42f-12d9cc30e704', demographic_closure_tenure).
narrative_ontology:cs_drift_state('fa44d95d-f4dc-47e8-b42f-12d9cc30e704', contemporary_critical_scholarship_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fa44d95d-f4dc-47e8-b42f-12d9cc30e704', '').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, dominant_demographic_groups).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, incumbent_tenured_faculty).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_faculty).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, marginalized_scholars).
narrative_ontology:constraint_vindicates(tenure_contract__demographic_reproduction_reading, demographic_homogeneity_as_institutional_stability).
narrative_ontology:constraint_vindicates(tenure_contract__demographic_reproduction_reading, subjective_evaluation_as_legitimate_gatekeeping).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faculty from historically dominant demographics (white, male, elite-institution pedigree) experience tenure review as a collegial affirmation. Their 'fit' is presumed; their networks control the evaluation criteria. They accrue career security, prestige, and resource control without bearing the burden of proving belonging.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, dominant_demographic_groups, beneficiary,
    organized, generational, arbitrage, national).

% Tenured faculty compose and chair the committees that define 'fit' and 'collegiality'. They reproduce their own demographic and intellectual profile by setting unwritten standards. They benefit from a system that validates their past choices and protects their departmental culture from disruption.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, incumbent_tenured_faculty, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__demographic_reproduction_reading, incumbent_tenured_faculty, beneficiary).

% Faculty from underrepresented groups (women, racial minorities, first-generation scholars) face tenure reviews where their productivity is discounted and their 'fit' is interrogated. They must over-perform on measurable metrics while navigating subjective criteria that encode dominant-group norms. Exit means leaving academia or accepting contingent roles.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_faculty, payer,
    moderate, biographical, constrained, national).

% Early-career scholars from marginalized backgrounds (including contingent faculty seeking tenure-track positions) encounter the gatekeeping effect before they even reach tenure review. The anticipation of biased evaluation shapes their research choices, service loads, and mental health. They have no institutional leverage and few alternative career paths that value their training.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, marginalized_scholars, payer,
    powerless, immediate, trapped, national).

% Committees (departmental, college, university) execute the review. They are formally charged with evaluating research, teaching, and service, but in practice they enforce 'fit' and 'collegiality' through unrecorded deliberations. Their decisions are opaque and unappealable. Members rotate, but the culture of evaluation persists.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, tenure_committees, agenda_setter,
    institutional, biographical, constrained, local).

% Provosts and presidents set policy frameworks and approve tenure decisions. They benefit from a stable, prestigious faculty that attracts funding and rankings. They resist external pressure to reform criteria because the current system legitimates their authority and manages liability. They can redirect resources but rarely challenge the evaluation culture.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, university_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Students (especially from underrepresented groups) lose mentors and role models when marginalized scholars are denied tenure. The public loses diverse research perspectives. Neither group has a seat in tenure deliberations, though they bear the long-term consequences of a homogenized professoriate.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, students_and_public, excluded,
    powerless, biographical, trapped, national).

% Scholars of higher education, sociology of science, and critical university studies document the demographic outcomes of tenure review. They produce the evidence that the 'fit' criteria correlate with race, gender, and pedigree rather than productivity. Their work is cited in litigation and policy debates but is dismissed within the tenure process itself.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, critical_scholarship_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement claims to coordinate quality assurance and academic freedom through peer evaluation, but the operative coordination is demographic reproduction: ensuring that the professoriate continues to reflect the social composition of its current incumbents.
% TRANSFER_FUNCTION: Moves tenure-track positions, lifetime employment, institutional resources, and disciplinary authority from underrepresented and marginalized scholars to dominant demographic groups via subjective 'fit' and 'collegiality' criteria that are uncorrelated with research output.
% ABSENT_VOICES: Underrepresented faculty and marginalized scholars are structurally excluded from the committees that define evaluation standards. Students and the public, who bear the cost of a homogenized knowledge production system, are never consulted. Critical scholarship that exposes the mechanism is treated as external critique rather than internal evidence.
% DISAPPEARANCE_RATIONALE: If the subjective gatekeeping criteria vanished overnight, tenure decisions would rely on transparent, productivity-based metrics. Demographic composition of tenured faculty would shift toward the composition of the qualified PhD pool within a generation. The institutional culture that treats demographic similarity as 'collegiality' would lose its primary reproduction mechanism.
% FOUNDING_PROBLEM: The tenure system was built to protect academic freedom by decoupling researcher survival from institutional displeasure or political backlash, enabling high-risk inquiry.
% FOUNDING_PROBLEM_CORROBORATION: The 1940 AAUP Statement of Principles (lineage authority) attests the founding problem as academic freedom protection. Critical scholars (e.g., feminist epistemologists, critical race theorists in higher education) attest that the problem has shifted: the system now primarily protects demographic closure. Empirical studies of tenure outcomes (e.g., Ginther & Kahn 2018; Posselt 2020) corroborate the demographic disparity from outside the beneficiary set.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__demographic_reproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__demographic_reproduction_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) is high because the transfer of tenure positions and resources along demographic lines is substantial and systematic. Suppression (0.78) is high because the criteria are opaque, unappealable, and enforced through institutional power; alternatives (blind review, structured rubrics) are actively resisted. Theater ratio (0.55) is moderate: the peer review ritual performs quality assurance while the real work is demographic sorting. Accessibility collapse (0.75) is high because once a candidate is deemed a 'poor fit', no alternative pathway to tenure exists within the institution. Resistance (0.60) is moderate: legal challenges, diversity initiatives, and critical scholarship push back but have not altered the core mechanism. The claimed type is snare because the coordination story (quality/freedom) is cover for extraction, persistence depends on coercion (no appeal, no exit), and victims are identifiable.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant demographic groups and incumbent tenured faculty are structural beneficiaries: they collect tenure security, prestige, and control over evaluation standards (d near 0.0). Underrepresented faculty and marginalized scholars are structural targets: they pay with over-performance requirements, psychological toll, and career exit (d near 1.0). Tenure committees and administration are agenda_setters with institutional power; their directionality is complex — they administer the extraction but also benefit from the system's stability. Students and the public are excluded with no exit. Critical observers are analytical with full exit. The derivation chain from beneficiary/victim declarations plus exit options produces the expected directionality spread.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate (academic freedom protection) is contested: the system persists but its original justification is empirically undermined by demographic outcome data. The arrangement now serves a latent mandate (demographic reproduction) that no one formally avows. This is not a scaffold (no sunset) nor a piton (the function is active, not atrophied). The mandatrophy is unresolved: the institution cannot acknowledge the shift without losing legitimacy, so it maintains the academic freedom narrative while the extraction machinery runs. The classification as snare captures this: the cover story is essential to the constraint's survival.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_emergence_of_gatekeeping,
    'Is the demographic gatekeeping an intentional design of incumbent faculty or an emergent property of subjective criteria?',
    'Historical analysis of tenure policy revisions; discourse analysis of committee deliberations; comparison of explicit vs. implicit criteria across institutions.',
    'If intentional, the constraint is a designed snare with liable actors. If emergent, it is a structural snare where reform must target the criteria themselves, not the actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_emergence_of_gatekeeping, empirical, 'Whether the gatekeeping mechanism is designed or emergent.').

omega_variable(
    fit_criteria_separability,
    'Can ''fit'' and ''collegiality'' be operationally defined in ways that do not correlate with demographic identity?',
    'Natural experiments from institutions that have implemented structured evaluation rubrics; regression analysis of tenure outcomes before/after rubric adoption controlling for productivity.',
    'If separable, the extraction is contingent on current implementation and reform is possible within the tenure framework. If inseparable, the criteria themselves are the extraction mechanism and tenure review must be replaced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fit_criteria_separability, conceptual, 'Whether the subjective criteria are intrinsically demographic or contingently so.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the demographic reproduction reading logically foreclose the academic freedom reading within a single institutional framework?',
    'Philosophical analysis of the logical compatibility of ''tenure protects inquiry'' and ''tenure reproduces demographic hierarchy'' as primary functions; historical case studies of institutions that have attempted both.',
    'If forecloses, the kernel admits only one dominant reading at a time — institutional reform requires replacing the kernel. If coexists, both readings can be held by different factions, and the kernel is a site of permanent contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between this reading and the academic freedom reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_tr_t1970, tenure_contract__demographic_reproduction_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_tr_t1985, tenure_contract__demographic_reproduction_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_tr_t2000, tenure_contract__demographic_reproduction_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_tr_t2010, tenure_contract__demographic_reproduction_reading, theater_ratio, 2010, 0.52).
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_tr_t2020, tenure_contract__demographic_reproduction_reading, theater_ratio, 2020, 0.55).
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_tr_t2025, tenure_contract__demographic_reproduction_reading, theater_ratio, 2025, 0.55).

% Extraction over time
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_be_t1970, tenure_contract__demographic_reproduction_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_be_t1985, tenure_contract__demographic_reproduction_reading, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_be_t2000, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_be_t2010, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_be_t2020, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2020, 0.71).
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_be_t2025, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_su_t1970, tenure_contract__demographic_reproduction_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_su_t1985, tenure_contract__demographic_reproduction_reading, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_su_t2000, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_su_t2010, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_su_t2020, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2020, 0.78).
narrative_ontology:measurement(tenure_contract__demographic_reproduction_reading_su_t2025, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tenure_contract__demographic_reproduction_reading, 0.08).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__institutional_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the tenure_contract kernel. The academic_freedom_reading claims tenure coordinates inquiry protection (low ε). The institutional_extraction_reading claims tenure coordinates rent extraction by senior faculty (high ε for juniors). This reading claims tenure coordinates demographic reproduction (high ε for marginalized groups). The three readings share the same formal kernel (tenure policies) but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__demographic_reproduction_reading, institutional, 0.35).
constraint_indexing:directionality_override(tenure_contract__demographic_reproduction_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
