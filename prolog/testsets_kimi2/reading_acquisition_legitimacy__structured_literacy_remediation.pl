% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__structured_literacy_remediation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__structured_literacy_remediation, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: reading_acquisition_legitimacy__structured_literacy_remediation
 *   human_readable: Structured Literacy Remediation Legitimacy Constraint
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint story models the structured literacy remediation reading
 *   of the reading acquisition legitimacy kernel. It treats as a binding norm
 *   the claim that legitimate reading instruction must be designed for the
 *   most vulnerable learners first, following explicit, cumulative,
 *   diagnostic, and multisensory principles. The constraint operates within
 *   education policy by delegitimizing balanced and meaning-first
 *   alternatives, mandating specific curricula and assessment regimes, and
 *   concentrating authority in a reform coalition and vendor ecosystem. The
 *   kernel is contested: three sibling readings instantiate different
 *   beneficiary/victim structures and different epsilon values. This reading
 *   is authored as a clean, epsilon-invariant constraint per Rule 1;
 *   committer structure is routed to omega variables per Rule 2.
 *
 * KEY AGENTS:
 *   - reading_reform_coalition: Agenda-setter (organized/generational) â defines legitimacy standards and benefits from policy adoption
 *   - structured_literacy_vendors: Beneficiary (powerful/biographical) â captures curriculum and assessment revenue
 *   - vulnerable_learners: Beneficiary (powerless/biographical) â receives targeted instruction, cannot exit
 *   - general_education_teachers: Payer (moderate/biographical) â bears retraining and compliance costs
 *   - balanced_literacy_practitioners: Payer (moderate/biographical, identity-locked) â methodology delegitimized, professional identity threatened
 *   - state_legislators: Agenda-setter (institutional/generational) â enacts mandates
 *   - independent_reading_researchers: Observer (analytical/generational) â evaluates claims from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.62).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.58).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy Remediation Legitimacy Constraint").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education_policy/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, '7c938868-5d68-4c15-955b-41bb96d1bf7d').
narrative_ontology:cs_kernel_codification('7c938868-5d68-4c15-955b-41bb96d1bf7d', formalized).
narrative_ontology:cs_authority_grounding('7c938868-5d68-4c15-955b-41bb96d1bf7d', expertise).
narrative_ontology:cs_interpretation_layer_present('7c938868-5d68-4c15-955b-41bb96d1bf7d').
narrative_ontology:cs_reading_relation('7c938868-5d68-4c15-955b-41bb96d1bf7d', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('7c938868-5d68-4c15-955b-41bb96d1bf7d', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('7c938868-5d68-4c15-955b-41bb96d1bf7d', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_axiom('7c938868-5d68-4c15-955b-41bb96d1bf7d', foundational, intervention_grade_prevention_universal).
narrative_ontology:cs_axiom_status(intervention_grade_prevention_universal, holdable).
narrative_ontology:cs_axiom_grounding('7c938868-5d68-4c15-955b-41bb96d1bf7d', intervention_grade_prevention_universal, instrumental).
narrative_ontology:cs_axiom('7c938868-5d68-4c15-955b-41bb96d1bf7d', foundational, vulnerable_learner_lexical_priority).
narrative_ontology:cs_axiom_status(vulnerable_learner_lexical_priority, holdable).
narrative_ontology:cs_axiom_grounding('7c938868-5d68-4c15-955b-41bb96d1bf7d', vulnerable_learner_lexical_priority, deontological).
narrative_ontology:cs_reference_frame('7c938868-5d68-4c15-955b-41bb96d1bf7d', explicit_diagnostic_legitimacy).
narrative_ontology:cs_drift_state('7c938868-5d68-4c15-955b-41bb96d1bf7d', contemporary_policy_cycle, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7c938868-5d68-4c15-955b-41bb96d1bf7d', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, vulnerable_learners).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_vendors).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, reading_reform_coalition).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, general_education_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_practitioners).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, explicit_instruction_hypothesis).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__structured_literacy_remediation, dyslexia_specificity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines the evidentiary and pedagogical standards for legitimate reading instruction; campaigns for state-level policy mandates and curriculum adoption criteria; professional authority, grant funding, and consultancies grow as structured literacy becomes mandatory.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, reading_reform_coalition, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, reading_reform_coalition, beneficiary).

% Sell proprietary curriculum packages, diagnostic assessment systems, and professional development tied to the explicit, cumulative, multisensory model; revenue scales directly with district and state mandates.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_vendors, beneficiary,
    powerful, biographical, mobile, national).

% Receive intensive, explicit, diagnostic instruction as the stated priority population; cannot opt out of the instructional framework or select alternative pedagogical approaches.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, vulnerable_learners, beneficiary,
    powerless, biographical, trapped, local).

% Must abandon previous instructional methods, undergo extensive retraining, implement continuous diagnostic assessment, and document fidelity to structured protocols regardless of their existing classroom effectiveness.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, general_education_teachers, payer,
    moderate, biographical, constrained, local).

% Their established methodology is formally delegitimized; professional identity is fused with balanced literacy practices developed over decades; face retraining mandates, performance evaluations tied to explicit-instruction fidelity, and social stigmatization in professional communities.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_practitioners, payer,
    moderate, biographical, identity_locked, local).

% Enact laws mandating evidence-based reading instruction, structured literacy criteria, and restrictions on balanced-literacy curriculum adoption; respond to advocacy coalitions and parent groups.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, state_legislators, agenda_setter,
    institutional, generational, mobile, national).

% Evaluate efficacy claims and methodological disputes from outside the reform coalition; some corroborate vulnerable-learner deficits under previous regimes, others challenge method-centrism and point to socioeconomic confounds.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, independent_reading_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_vendors).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates educators, districts, and resources around a unified, evidence-based approach to reading instruction that prioritizes students most at risk of failure, preventing the variability and inconsistency that left vulnerable learners without systematic decoding support.
% TRANSFER_FUNCTION: Moves public education funds, professional development hours, and curriculum authority from general-education and balanced-literacy educators toward structured literacy vendors, diagnostic assessment providers, and the reform coalition; extracts compliance and retraining costs from teachers and districts.
% ABSENT_VOICES: Whole-language practitioners, critical literacy scholars, and educators serving multilingual learners whose orthographic backgrounds do not align with English-centric explicit phonics are excluded from curriculum adoption committees and policy panels; their empirical objections are treated as ideologically motivated rather than evidence-based.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, districts would revert to heterogeneous instructional methods, curriculum markets would re-diversify, professional development would abandon intensive diagnostic/explicit training, and the current reform coalition would lose policy leverage; reading instruction would reorganize around local educator judgment and diverse student needs rather than mandated remediation principles.
% FOUNDING_PROBLEM: Large numbers of studentsâparticularly those with dyslexia and specific learning disabilitiesâwere failing to acquire reading under implicit, meaning-first instructional regimes; the education system lacked a consistent, evidence-backed method to ensure decoding skills for the most vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Special-education advocacy organizations and some independent reading researchers outside the reform coalition corroborate historical underservice of dyslexic students; however, education economists and sociologists contest that instructional method was the primary cause, attributing failure to resource inequity, class size, and poverty. No fully neutral corroboration exists.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__structured_literacy_remediation, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__structured_literacy_remediation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__structured_literacy_remediation, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) is substantial because the constraint moves significant public funds toward proprietary curriculum and assessment ecosystems and extracts extensive compliance costs from educators. Suppression (0.58) reflects the active delegitimization of balanced and meaning-first methods through policy mandates, credentialing criteria, and public stigmatization. Theater ratio (0.45) captures performative adoption where districts purchase structured literacy packages but implement them with low fidelity. Accessibility collapse (0.68) indicates that once mandated, alternative methods become materially and socially inaccessible. Resistance (0.52) reflects organized pushback from teacher unions, balanced literacy advocates, and critical literacy scholars. The temporal series trace the constraint's evolution from the 2000 National Reading Panel report through the 2020s state-mandate wave, showing extraction and suppression ratcheting upward as enforcement infrastructure matured.
 *
 * PERSPECTIVAL GAP:
 *   From the reform coalition and vendor seats, the constraint is genuine coordination solving a collective-action failure in reading instruction; from the general-education and balanced-literacy seats, the same structure operates as enforced extraction that suppresses professional judgment and alternative evidence bases. The engine computes this divergence from structural data: low directionality for vendors and the coalition, high directionality for identity-locked practitioners.
 *
 * DIRECTIONALITY LOGIC:
 *   The reading_reform_coalition and structured_literacy_vendors sit near the beneficiary end (low d): they collect authority, revenue, and policy leverage from the constraint's operation. Vulnerable_learners also sit near the beneficiary end, though they do not collect rentsâthey receive subsidized coordination. General_education_teachers sit at moderate-high d: they bear compliance costs but retain some mobility. Balanced_literacy_practitioners sit near full-target (high d): their professional identity is fused to the delegitimized method, making exit cognitively and socially costly. State_legislators sit near symmetric: they enforce but do not personally collect, and can pivot to other policy frames.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâsystematic reading failure among vulnerable learnersâis contested but not dead; vulnerable learners do continue to struggle, so the coordination function has not fully atrophied. However, the constraint's mandate has expanded beyond the original problem scope (intervention for struggling readers) to universal preventative intervention for all students, suggesting mandatrophy risk: the arrangement may be growing broader than the problem that justified it. If the founding problem were ever fully solved (all vulnerable readers served), the universal-mandate structure would likely persist as institutionalized extraction, pivoting to a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pedagogical_natural_law_or_construct,
    'Does the structured literacy constraint reflect an invariant cognitive mechanism of reading acquisition, or a historically constructed pedagogical and policy regime?',
    'Cross-cultural literacy acquisition studies, neuroimaging of multilingual readers, and historical analysis of pre-phonics high-literacy societies.',
    'If multiple viable pathways to literacy exist, the constraint is a tangled rope or snare using natural-law rhetoric; if the explicit-cumulative pathway is truly invariant, the constraint trends toward rope or mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_natural_law_or_construct, empirical, 'Whether structured literacy is a discovered natural law or a constructed norm.').

omega_variable(
    sibling_reading_structural_divergence,
    'How would the beneficiary-victim structure, extractiveness, and directionality change if this kernel were read through whole-language or balanced-literacy framings?',
    'Comparative constraint-story analysis across the four readings in the reading_acquisition_legitimacy kernel family.',
    'A whole-language reading would likely reverse payer and beneficiary roles, making explicit-phonics practitioners the payers; the kernel''s classification is reading-dependent and must be evaluated per-file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_divergence, conceptual, 'Structural divergence across kernel readings.').

omega_variable(
    implementation_fidelity_gap,
    'Does the measured extraction derive from the structured literacy principles themselves, or from low-fidelity implementation that performs the aesthetic without the substance?',
    'Classroom observation studies and fidelity audits comparing high-implementation structured literacy classrooms with mandated-but-performative adoptions.',
    'If extraction tracks theater, the constraint is piton-like in degraded implementations; if extraction is inherent to the diagnostic-explicit model even at high fidelity, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_fidelity_gap, empirical, 'Whether extraction is in the principle or the performance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reading_acquisition_structured_lit_tr_t0, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(reading_acquisition_structured_lit_tr_t5, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 5, 0.25).
narrative_ontology:measurement(reading_acquisition_structured_lit_tr_t10, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 10, 0.32).
narrative_ontology:measurement(reading_acquisition_structured_lit_tr_t15, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 15, 0.36).
narrative_ontology:measurement(reading_acquisition_structured_lit_tr_t20, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 20, 0.42).
narrative_ontology:measurement(reading_acquisition_structured_lit_tr_t25, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(reading_acquisition_structured_lit_be_t0, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(reading_acquisition_structured_lit_be_t5, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(reading_acquisition_structured_lit_be_t10, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(reading_acquisition_structured_lit_be_t15, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(reading_acquisition_structured_lit_be_t20, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(reading_acquisition_structured_lit_be_t25, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(reading_acquisition_structured_lit_su_t0, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(reading_acquisition_structured_lit_su_t5, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 5, 0.3).
narrative_ontology:measurement(reading_acquisition_structured_lit_su_t10, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(reading_acquisition_structured_lit_su_t15, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(reading_acquisition_structured_lit_su_t20, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(reading_acquisition_structured_lit_su_t25, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__balanced_literacy_integration).

% DUAL FORMULATION NOTE:
% This constraint is one of four structurally distinct readings of the reading_acquisition_legitimacy kernel. Each reading has a different epsilon, different beneficiary/victim structure, and different classification. They are linked as a constraint family via network.affects_constraints. The confusion is in the colloquial language ('reading instruction'), not in the structural mathematics; the framework disambiguates the label into precise claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
