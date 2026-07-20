% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__hybrid_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: correct_latin_kernel__hybrid_reading
 *   human_readable: Hybrid Latin Correctness Framework (Medieval Morphology Continuous, Syntax/Lexicon Recovered)
 *   domain: historical/philological/intellectual
 *
 * SUMMARY:
 *   The correct Latin kernel under the hybrid reading posits that core
 *   morphology remained continuous from classical antiquity through the
 *   Middle Ages, while syntax and lexicon became corrupted and must be
 *   recovered from classical textual witnesses. This constraint coordinates
 *   philological practice across periods but asymmetrically extracts
 *   legitimacy and resources toward classical philology. It is claimed as
 *   tangled rope: genuine editorial coordination layered with
 *   institutionalized classical supremacy. The metrics and claim are authored
 *   independently.
 *
 * KEY AGENTS:
 *   - classical_philologists: Primary agenda-setter/beneficiary (institutional/identity_locked) â controls norms, journals, departments
 *   - textual_editors: Secondary agenda-setter/beneficiary (organized/constrained) â enforces reconstruction protocol in editions
 *   - medieval_latinists: Primary payer (moderate/constrained) â bears devaluation of medieval syntax/lexicon
 *   - students_of_latin: Secondary payer (powerless/constrained) â bears pedagogical cost of artificial hybrid
 *   - general_historical_linguists: Analytical observer (organized/analytical) â provides external empirical challenge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, 0.68).
domain_priors:suppression_score(correct_latin_kernel__hybrid_reading, 0.58).
domain_priors:theater_ratio(correct_latin_kernel__hybrid_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(correct_latin_kernel__hybrid_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__hybrid_reading, "Hybrid Latin Correctness Framework (Medieval Morphology Continuous, Syntax/Lexicon Recovered)").
narrative_ontology:topic_domain(correct_latin_kernel__hybrid_reading, "historical/philological/intellectual").

domain_priors:requires_active_enforcement(correct_latin_kernel__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__hybrid_reading, 'ba5a87a0-ba24-4f6c-a028-d2bd970236ad').
narrative_ontology:cs_kernel_codification('ba5a87a0-ba24-4f6c-a028-d2bd970236ad', fixed_text).
narrative_ontology:cs_authority_grounding('ba5a87a0-ba24-4f6c-a028-d2bd970236ad', lineage).
narrative_ontology:cs_interpretation_layer_present('ba5a87a0-ba24-4f6c-a028-d2bd970236ad').
narrative_ontology:cs_reading_relation('ba5a87a0-ba24-4f6c-a028-d2bd970236ad', correct_latin_kernel__continuity_reading, influences).
narrative_ontology:cs_reading_relation('ba5a87a0-ba24-4f6c-a028-d2bd970236ad', correct_latin_kernel__discontinuity_reading, influences).
narrative_ontology:cs_axiom('ba5a87a0-ba24-4f6c-a028-d2bd970236ad', foundational, morphology_continuous_with_classical).
narrative_ontology:cs_axiom_status(morphology_continuous_with_classical, holdable).
narrative_ontology:cs_axiom_grounding('ba5a87a0-ba24-4f6c-a028-d2bd970236ad', morphology_continuous_with_classical, empirically_contingent).
narrative_ontology:cs_axiom('ba5a87a0-ba24-4f6c-a028-d2bd970236ad', foundational, syntax_lexicon_require_classical_recovery).
narrative_ontology:cs_axiom_status(syntax_lexicon_require_classical_recovery, holdable).
narrative_ontology:cs_axiom_grounding('ba5a87a0-ba24-4f6c-a028-d2bd970236ad', syntax_lexicon_require_classical_recovery, empirically_contingent).
narrative_ontology:cs_reference_frame('ba5a87a0-ba24-4f6c-a028-d2bd970236ad', classical_latin_grammatical_state).
narrative_ontology:cs_drift_state('ba5a87a0-ba24-4f6c-a028-d2bd970236ad', post_medievalist_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ba5a87a0-ba24-4f6c-a028-d2bd970236ad', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__hybrid_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(correct_latin_kernel__hybrid_reading, textual_editors).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, medieval_latinists).
narrative_ontology:constraint_victim(correct_latin_kernel__hybrid_reading, students_of_latin).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control university departments, journals, and critical edition series that define correct Latin. Their professional identity is fused with the prestige of the classical period; they set the norms that treat medieval morphology as continuous but syntax and lexicon as requiring classical recovery.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, classical_philologists, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__hybrid_reading, classical_philologists, beneficiary).

% Produce critical editions applying the hybrid framework; decide which medieval morphological forms to retain and which syntactical or lexical features to emend back toward classical standards. Their authority and career advancement depend on mastery of the reconstruction protocol.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, textual_editors, agenda_setter,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(correct_latin_kernel__hybrid_reading, textual_editors, beneficiary).

% Study post-classical Latin texts whose syntax and lexicon are treated as corrupt within the hybrid framework. Must frame research within classical norms to secure publication and funding; their period's linguistic autonomy is systematically devalued.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, medieval_latinists, payer,
    moderate, biographical, constrained, continental).

% Taught a hybrid language in which morphological continuity is assumed but syntactical and lexical correctness must be recovered from classical texts. Bear the pedagogical cost of an artificial norm that does not correspond to any historical speaker's competence.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, students_of_latin, payer,
    powerless, biographical, constrained, national).

% Study Latin within typological and sociolinguistic frameworks that treat all historical stages as equally systematic. They observe the philological constraint from outside and provide empirical evidence that challenges the discontinuity assumption in syntax and lexicon.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__hybrid_reading, general_historical_linguists, observer,
    organized, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__hybrid_reading, classical_philologists).
narrative_ontology:fixing_cost_class(correct_latin_kernel__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the philological community around a single editorial standard for Latin texts spanning antiquity to the Renaissance, enabling shared practices of emendation, annotation, and pedagogy without requiring scholars to adjudicate period boundaries for every form.
% TRANSFER_FUNCTION: Moves legitimacy, funding, and career prestige from medieval Latin studies to classical philology and textual editing, while transferring the burden of proof and reconstruction labor onto medieval material.
% ABSENT_VOICES: Post-classical authors whose systematic but non-classical Latin is treated as corrupt; general historical linguists who regard all attested stages as rule-governed; vernacular-speaking scribes whose competence is measured against a classical template rather than on its own terms.
% DISAPPEARANCE_RATIONALE: If the hybrid framework vanished, critical editions would reorganize around period-specific grammars, classical philology would lose its privileged anchor over the whole tradition, medieval Latin syntax and lexicon would be evaluated on their own terms, and pedagogical norms would shift away from the artificial hybrid.
% FOUNDING_PROBLEM: The nineteenth-century need to produce critical editions of a vast Latin corpus spanning fifteen centuries with consistent grammatical standards, before the development of period-specific historical linguistics.
% FOUNDING_PROBLEM_CORROBORATION: General historical linguists and medievalists attest that period-specific description has superseded the need for a single correct Latin; classical philologists assert the problem remains live. No external corroboration from outside the benefiting parties supports the live reading â the mismatch is the signal.
narrative_ontology:disappearance_verdict(correct_latin_kernel__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__hybrid_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__hybrid_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin_kernel__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__hybrid_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the framework systematically transfers legitimacy from medieval to classical material. Suppression (0.58) reflects active enforcement through peer review, hiring, and editorial standards that marginalize period-specific linguistics. Theater ratio (0.48) is elevated: much editorial activity now maintains the classical prestige hierarchy rather than solving live textual problems. Accessibility collapse (0.62) is substantial because alternatives (treating medieval Latin as autonomous) are hard to publish within the discipline. Resistance (0.52) comes from medievalists and general linguistics. The temporal series show extraction accumulation from 1850 to 2000, with slight decline as external challenges mount, while theater rises toward the 0.5 piton threshold.
 *
 * PERSPECTIVAL GAP:
 *   The classical philologist seat experiences the constraint as necessary coordination enabling shared standards across a vast corpus; the medievalist seat experiences the same structure as enforced extraction that devalues their period. The engine computes this divergence from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   classical_philologists and textual_editors are declared beneficiaries with identity_locked or constrained exit, placing their directionality near the beneficiary pole. medieval_latinists and students_of_latin are declared victims with constrained exit, placing their directionality near the target pole. general_historical_linguists are observers with analytical exit, exempt from directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â producing consistent critical editions before period-specific linguistics existed â is dead. Modern historical linguistics provides better tools. The constraint persists because classical philologists are identity-locked to the reference frame and because the editorial infrastructure generates careers. This is mandatrophy without piton resolution: the function is dead but extraction remains concentrated enough to prevent drift to pure inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_validity_of_morphological_continuity,
    'Is core morphology truly continuous from classical to medieval Latin, or does it also show systematic medieval innovation masked by editorial normalization?',
    'Statistical analysis of unedited manuscripts compared against critical editions to measure editorial flattening of morphological variation.',
    'If morphology is less continuous than claimed, the hybrid reading''s foundation erodes toward discontinuity_reading; if sustained, the hybrid claim is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_validity_of_morphological_continuity, empirical, 'Whether morphological continuity is empirically sustained or an artifact of editorial practice.').

omega_variable(
    reconstruction_as_extraction,
    'Does the layered reconstruction method genuinely recover an underlying classical layer, or does it impose a classical template that extracts legitimacy from medieval material?',
    'Comparative analysis of editorial choices across classical-leaning and medievalist editorial schools, measuring rates of emendation against independent linguistic criteria.',
    'If the latter, the constraint shifts toward snare; if the former, it remains tangled rope with a live coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_as_extraction, conceptual, 'Whether reconstruction is recovery or imposition.').

omega_variable(
    institutional_inertia_vs_live_doctrine,
    'Does the hybrid reading persist because it solves a live coordination problem, or because philology departments are identity-locked to the classical reference frame?',
    'Career-path analysis of philologists and funding-flow analysis tracing grants and positions to classical versus medieval Latin objects of study.',
    'If identity-locked inertia dominates, drift toward piton accelerates; if live coordination dominates, tangled rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_inertia_vs_live_doctrine, conceptual, 'Whether persistence is functional or inertial.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__hybrid_reading, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(corr_tr_t30, correct_latin_kernel__hybrid_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(corr_tr_t60, correct_latin_kernel__hybrid_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(corr_tr_t90, correct_latin_kernel__hybrid_reading, theater_ratio, 90, 0.4).
narrative_ontology:measurement(corr_tr_t120, correct_latin_kernel__hybrid_reading, theater_ratio, 120, 0.45).
narrative_ontology:measurement(corr_tr_t150, correct_latin_kernel__hybrid_reading, theater_ratio, 150, 0.47).
narrative_ontology:measurement(corr_tr_t180, correct_latin_kernel__hybrid_reading, theater_ratio, 180, 0.48).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__hybrid_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(corr_be_t30, correct_latin_kernel__hybrid_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(corr_be_t60, correct_latin_kernel__hybrid_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(corr_be_t90, correct_latin_kernel__hybrid_reading, base_extractiveness, 90, 0.68).
narrative_ontology:measurement(corr_be_t120, correct_latin_kernel__hybrid_reading, base_extractiveness, 120, 0.72).
narrative_ontology:measurement(corr_be_t150, correct_latin_kernel__hybrid_reading, base_extractiveness, 150, 0.7).
narrative_ontology:measurement(corr_be_t180, correct_latin_kernel__hybrid_reading, base_extractiveness, 180, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__hybrid_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(corr_su_t30, correct_latin_kernel__hybrid_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(corr_su_t60, correct_latin_kernel__hybrid_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(corr_su_t90, correct_latin_kernel__hybrid_reading, suppression_requirement, 90, 0.58).
narrative_ontology:measurement(corr_su_t120, correct_latin_kernel__hybrid_reading, suppression_requirement, 120, 0.6).
narrative_ontology:measurement(corr_su_t150, correct_latin_kernel__hybrid_reading, suppression_requirement, 150, 0.59).
narrative_ontology:measurement(corr_su_t180, correct_latin_kernel__hybrid_reading, suppression_requirement, 180, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(correct_latin_kernel__hybrid_reading, identity_coordination).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__hybrid_reading, discontinuity_reading).

% DUAL FORMULATION NOTE:
% The label 'correct Latin' conflates three structurally distinct claims. Decomposed per the Îµ-invariance principle into separate stories linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
