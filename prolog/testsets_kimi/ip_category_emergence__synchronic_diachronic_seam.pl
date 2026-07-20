% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__synchronic_diachronic_seam
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__synchronic_diachronic_seam, []).

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
 *   constraint_id: ip_category_emergence__synchronic_diachronic_seam
 *   human_readable: M4/M5 Collapse Test for IP Category Emergence (Synchronic-Diachronic Seam)
 *   domain: legal_philosophy/intellectual_property
 *
 * SUMMARY:
 *   This constraint instantiates the synchronic_diachronic_seam reading of
 *   the kernel ip_category_emergence, which concerns how intellectual
 *   property categories emerged around the 1710 Statute of Anne. The sibling
 *   readings are thinkability_reading (category emergence via conceptual
 *   coherence) and first_holding_reading (occupancy change via authorial
 *   rights-holder recognition). This reading functions as a meta-level
 *   collapse test (M4/M5) that adjudicates whether the kernel structure is
 *   authentic (independent dimensions) or spurious (temporal framing
 *   artifact). The constraint operates in Anglo-American legal philosophy and
 *   historical jurisprudence as a methodological gatekeeping device.
 *
 * KEY AGENTS:
 *   - Analytic jurisprudence program (agenda_setter/institutional): administers the M4/M5 collapse test and gains disciplinary centrality
 *   - Empirical legal historians (payer/moderate): bear the cost of translating archival findings into the thinkability/first-holding binary
 *   - Critical legal historians (payer/beneficiary/moderate): critique the seam but must engage its vocabulary
 *   - Interdisciplinary scholars (excluded/moderate): study creativity and ownership outside the legal-philosophical frame and are excluded from central debate
 *   - IP practitioners (payer/powerful): pay overhead to navigate abstract methodological scholarship for practical historical arguments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, 0.62).
domain_priors:suppression_score(ip_category_emergence__synchronic_diachronic_seam, 0.55).
domain_priors:theater_ratio(ip_category_emergence__synchronic_diachronic_seam, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, extractiveness, 0.62).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(ip_category_emergence__synchronic_diachronic_seam, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__synchronic_diachronic_seam, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__synchronic_diachronic_seam, "M4/M5 Collapse Test for IP Category Emergence (Synchronic-Diachronic Seam)").
narrative_ontology:topic_domain(ip_category_emergence__synchronic_diachronic_seam, "legal_philosophy/intellectual_property").

domain_priors:requires_active_enforcement(ip_category_emergence__synchronic_diachronic_seam).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__synchronic_diachronic_seam, '00525db6-0b98-41bb-9bfc-424a9d8fe165').
narrative_ontology:cs_kernel_codification('00525db6-0b98-41bb-9bfc-424a9d8fe165', fixed_text).
narrative_ontology:cs_authority_grounding('00525db6-0b98-41bb-9bfc-424a9d8fe165', lineage).
narrative_ontology:cs_interpretation_layer_present('00525db6-0b98-41bb-9bfc-424a9d8fe165').
narrative_ontology:cs_reading_relation('00525db6-0b98-41bb-9bfc-424a9d8fe165', ip_category_emergence__thinkability_reading, influences).
narrative_ontology:cs_reading_relation('00525db6-0b98-41bb-9bfc-424a9d8fe165', ip_category_emergence__first_holding_reading, influences).
narrative_ontology:cs_axiom('00525db6-0b98-41bb-9bfc-424a9d8fe165', foundational, m4_m5_exhaustive_disjunction).
narrative_ontology:cs_axiom_status(m4_m5_exhaustive_disjunction, holdable).
narrative_ontology:cs_axiom_grounding('00525db6-0b98-41bb-9bfc-424a9d8fe165', m4_m5_exhaustive_disjunction, empirically_contingent).
narrative_ontology:cs_axiom('00525db6-0b98-41bb-9bfc-424a9d8fe165', secondary, kernel_authenticity_requires_seam_independence).
narrative_ontology:cs_axiom_status(kernel_authenticity_requires_seam_independence, holdable).
narrative_ontology:cs_axiom_grounding('00525db6-0b98-41bb-9bfc-424a9d8fe165', kernel_authenticity_requires_seam_independence, conventional).
narrative_ontology:cs_reference_frame('00525db6-0b98-41bb-9bfc-424a9d8fe165', ip_emergence_dual_structure).
narrative_ontology:cs_drift_state('00525db6-0b98-41bb-9bfc-424a9d8fe165', contemporary_critical_turn, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('00525db6-0b98-41bb-9bfc-424a9d8fe165', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__synchronic_diachronic_seam, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, analytic_jurisprudence_program).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, empirical_legal_historians).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, interdisciplinary_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ip_category_emergence__synchronic_diachronic_seam, critical_legal_historians).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, critical_legal_historians).
narrative_ontology:constraint_victim(ip_category_emergence__synchronic_diachronic_seam, ip_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and administers the M4/M5 collapse test as a criterion for rigorous legal-historical scholarship. Frames the 1710 Statute of Anne through the binary of thinkability versus first-holding, and judges contributions by whether they adjudicate the seam. Gains disciplinary centrality, citations, and institutional prestige from the test's adoption in top journals and law schools.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, analytic_jurisprudence_program, agenda_setter,
    institutional, generational, constrained, global).

% Works with archival evidence on early modern printing, licensing, and the 1710 statute. To publish in leading legal history venues, must translate concrete findings into the thinkability/first-holding framework or risk being read as theoretically unsophisticated. Bears the cost of methodological translation and delayed empirical publication.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, empirical_legal_historians, payer,
    moderate, biographical, constrained, national).

% Argues that formalist distinctions like thinkability and first-holding collapse under historical scrutiny, treating the seam as a temporal framing artifact. Gains publication space by critiquing the test, but must still engage the M4/M5 vocabulary and accept its terms as the field's central problem.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, critical_legal_historians, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__synchronic_diachronic_seam, critical_legal_historians, beneficiary).

% Studies creativity, authorship, and ownership from science studies, anthropology, and literary history. Would object that the entire legal-philosophical framing of 1710 is parochial, but is excluded from the central IP history conversation because the M4/M5 test is internal to analytical jurisprudence and requires training in its vocabulary.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, interdisciplinary_scholars, excluded,
    moderate, biographical, mobile, global).

% Occasionally needs historical arguments for doctrinal innovation, amicus briefs, or policy testimony. Must commission or navigate highly abstract methodological scholarship that rarely resolves into actionable historical claims; pays the cost of translation to practical argument.
narrative_ontology:constraint_stakeholder(ip_category_emergence__synchronic_diachronic_seam, ip_practitioners, payer,
    powerful, immediate, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__synchronic_diachronic_seam, analytic_jurisprudence_program).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared methodological test for adjudicating whether the synchronic concept of thinkability and the diachronic fact of first-holding are structurally independent in the emergence of intellectual property categories, coordinating legal philosophers and historians around a common evaluative vocabulary.
% TRANSFER_FUNCTION: Moves scholarly attention and institutional prestige from empirical historical inquiry to methodological framing disputes; transfers the cost of methodological translation to empirical historians and the cost of navigational overhead to practitioners.
% ABSENT_VOICES: Interdisciplinary scholars from science studies, anthropology, and literary history who study creativity and ownership without the legal-philosophical framing; empirical archivists who treat 1710 as a concrete statutory event rather than a conceptual seam.
% DISAPPEARANCE_RATIONALE: If the M4/M5 collapse test vanished, legal historians would cease organizing their work around the thinkability/first-holding binary; empirical scholars would write directly from archives without methodological translation; interdisciplinary scholars would enter the conversation without jurisprudential gatekeeping; the field would rearrange around socio-legal and material histories.
% FOUNDING_PROBLEM: The 1710 Statute of Anne generated competing interpretations about whether it marked a conceptual breakthrough (thinkability) or a social recognition (first-holding). Legal philosophy needed a rigorous test to determine whether these were distinct phenomena or the same event under different descriptions.
% FOUNDING_PROBLEM_CORROBORATION: Analytical jurisprudence programs attest the problem is live and central. Empirical legal historians and interdisciplinary scholars from outside the benefiting parties attest the founding problem is a disciplinary construct rather than a historical necessity; critical legal history corroborates that the seam is a framing artifact.
narrative_ontology:disappearance_verdict(ip_category_emergence__synchronic_diachronic_seam, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__synchronic_diachronic_seam, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__synchronic_diachronic_seam, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__synchronic_diachronic_seam, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__synchronic_diachronic_seam, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__synchronic_diachronic_seam, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__synchronic_diachronic_seam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-to-high because the test forces scholars to adopt a methodological vocabulary that serves the agenda-setting program; suppression (0.55) reflects peer-review enforcement of the binary; theater_ratio (0.48) indicates that nearly half of the engagement with the test is performative rehearsal of the methodological binary rather than generation of new historical knowledge. Accessibility_collapse (0.68) captures how difficult it becomes to see the 1710 moment outside the thinkability/first-holding frame once one is socialized into the field. Resistance (0.48) reflects ongoing pushback from empirical and interdisciplinary scholars. The measurement series show gradual institutionalization: extraction rises as the test becomes a citation requirement, theater rises as ritualistic engagement displaces substantive historical inquiry, and suppression tracks the hardening of peer-review norms around the seam.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (analytic jurisprudence) experiences the constraint as genuine coordination around a necessary methodological clarification. The payer seats (empirical historians, practitioners) experience it as enforced methodological overhead. The excluded seat (interdisciplinary scholars) experiences it as a silencing frame. The engine computes this divergence from the structural data: same constraint, radically different computed types by seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The analytic jurisprudence program is the structural beneficiary: it collects prestige, citations, and institutional centrality from the test's operation (d near beneficiary end). Empirical legal historians and interdisciplinary scholars are structural targets: they bear the costs of methodological translation and exclusion (d near target end). Critical legal historians sit ambiguously near the middle because they both gain publication space from the debate and pay by accepting its terms. IP practitioners are moderate targets: they have mobile exit (can ignore legal philosophy) but pay when they need historical arguments.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination and extraction to be present. The M4/M5 test does coordinate a genuine scholarly debate about the relationship between concept and practice in legal history, but it also extracts by concentrating prestige in the methodological gatekeepers and taxing empirical scholars. A pure rope reading would ignore the gatekeeping; a pure snare reading would ignore that the problem is intellectually real. Tangled rope captures the hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    seam_test_empirical_validity,
    'Does historical evidence actually support the claim that thinkability and first-holding always co-occur, or is the apparent co-occurrence an artifact of selective source reading?',
    'Comparative historical analysis of jurisdictions where IP categories emerged without a clear first holder or where occupancy change preceded conceptual coherence.',
    'If they vary independently in the historical record, the kernel is authentic and the test validates a real seam; if they always co-occur, the kernel is spurious and the test dissolves into a single dimension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seam_test_empirical_validity, empirical, 'Whether the M4/M5 collapse test tracks a real historical pattern or a selective reading.').

omega_variable(
    critical_historian_beneficiary_status,
    'Do critical legal historians who debunk the seam test function as beneficiaries of the constraint by gaining publication space from the debate, or as victims by being forced into the test''s vocabulary?',
    'Citation-network and tenure-track analysis of critical scholars versus empirical scholars in the same field.',
    'If critical scholars are net beneficiaries, extraction is concentrated on empirical and interdisciplinary scholars; if they are also victims, the constraint is more uniformly extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_historian_beneficiary_status, conceptual, 'Ambiguity in the structural position of critical legal historians relative to the constraint.').

omega_variable(
    methodological_gatekeeping_or_genuine_inquiry,
    'Is the M4/M5 collapse test a genuine coordination mechanism for clarifying IP history, or primarily a gatekeeping device that extracts attention from empirical inquiry?',
    'Content-analysis of articles invoking M4/M5: do they generate new historical knowledge or primarily rehearse the methodological binary?',
    'If the latter, theater_ratio and extractiveness should be revised upward; if the former, the constraint may be more rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_gatekeeping_or_genuine_inquiry, empirical, 'Whether the test functions as inquiry or as disciplinary gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__synchronic_diachronic_seam, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t0, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ip_c_tr_t6, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 6, 0.28).
narrative_ontology:measurement(ip_c_tr_t12, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 12, 0.35).
narrative_ontology:measurement(ip_c_tr_t18, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 18, 0.4).
narrative_ontology:measurement(ip_c_tr_t24, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 24, 0.45).
narrative_ontology:measurement(ip_c_tr_t30, ip_category_emergence__synchronic_diachronic_seam, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t0, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ip_c_be_t6, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(ip_c_be_t12, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(ip_c_be_t18, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(ip_c_be_t24, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(ip_c_be_t30, ip_category_emergence__synchronic_diachronic_seam, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t0, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ip_c_su_t6, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(ip_c_su_t12, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(ip_c_su_t18, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 18, 0.5).
narrative_ontology:measurement(ip_c_su_t24, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(ip_c_su_t30, ip_category_emergence__synchronic_diachronic_seam, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__synchronic_diachronic_seam, identity_coordination).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__synchronic_diachronic_seam, first_holding_reading).

% DUAL FORMULATION NOTE:
% The kernel ip_category_emergence decomposes into three structurally distinct claims: thinkability_reading (category emergence), first_holding_reading (occupancy change), and synchronic_diachronic_seam (the collapse test that adjudicates their relationship). Each reading has a different epsilon, beneficiary structure, and classification. This reading subjects the other two to a test of independent variability; if they fail, the kernel collapses into a single dimension.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
