% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__positivist_reading, []).

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
 *   constraint_id: us_constitution_meaning__positivist_reading
 *   human_readable: Constitutional Positivism: Formal Validity Rule
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the positivist reading of the US Constitution,
 *   asserting that its validity and meaning derive solely from formal
 *   enactment procedures and institutional authority, explicitly excluding
 *   external moral principles. It functions as a framework for judicial
 *   interpretation, coordinating legal actors around a textual and procedural
 *   understanding of law. This reading is one of several competing
 *   interpretations of the 'us_constitution_meaning' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, 0.65).
domain_priors:suppression_score(us_constitution_meaning__positivist_reading, 0.75).
domain_priors:theater_ratio(us_constitution_meaning__positivist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__positivist_reading, "Constitutional Positivism: Formal Validity Rule").
narrative_ontology:topic_domain(us_constitution_meaning__positivist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__positivist_reading, '8b80b0af-ae96-4255-ad08-300a82cea5d5').
narrative_ontology:cs_kernel_codification('8b80b0af-ae96-4255-ad08-300a82cea5d5', fixed_text).
narrative_ontology:cs_authority_grounding('8b80b0af-ae96-4255-ad08-300a82cea5d5', lineage).
narrative_ontology:cs_interpretation_layer_present('8b80b0af-ae96-4255-ad08-300a82cea5d5').
narrative_ontology:cs_reading_relation('8b80b0af-ae96-4255-ad08-300a82cea5d5', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b80b0af-ae96-4255-ad08-300a82cea5d5', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('8b80b0af-ae96-4255-ad08-300a82cea5d5', foundational, constitutional_validity_from_enactment).
narrative_ontology:cs_axiom_status(constitutional_validity_from_enactment, holdable).
narrative_ontology:cs_axiom_grounding('8b80b0af-ae96-4255-ad08-300a82cea5d5', constitutional_validity_from_enactment, conventional).
narrative_ontology:cs_axiom('8b80b0af-ae96-4255-ad08-300a82cea5d5', foundational, moral_principles_not_source_of_law).
narrative_ontology:cs_axiom_status(moral_principles_not_source_of_law, holdable).
narrative_ontology:cs_axiom_grounding('8b80b0af-ae96-4255-ad08-300a82cea5d5', moral_principles_not_source_of_law, deontological).
narrative_ontology:cs_reference_frame('8b80b0af-ae96-4255-ad08-300a82cea5d5', formal_legal_process_supremacy).
narrative_ontology:cs_drift_state('8b80b0af-ae96-4255-ad08-300a82cea5d5', contemporary_legal_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8b80b0af-ae96-4255-ad08-300a82cea5d5', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__positivist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, legislature).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, executive_branch).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, constitutional_judges).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, substantive_justice_advocates).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, moral_rights_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bound by the positivist reading, they interpret the Constitution strictly according to its text and formal enactment procedures, excluding external moral principles from validity determinations. They benefit from the clarity and reduced discretion this approach offers.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, constitutional_judges, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from a positivist reading as it upholds the supremacy of formally enacted law and the amendment process, limiting judicial invalidation based on non-textual grounds. Their legislative output is more secure if it adheres to formal constitutional requirements.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, legislature, beneficiary,
    institutional, biographical, mobile, national).

% Operates within a more predictable legal framework when constitutional validity is tied to formal procedures, reducing uncertainty from subjective judicial interpretations. This facilitates consistent policy implementation.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, executive_branch, beneficiary,
    institutional, biographical, mobile, national).

% Bear the cost when their claims for justice, even if morally compelling, are dismissed by courts for lacking explicit textual or procedural grounding in the Constitution. They must pursue legislative or amendment paths, which are often difficult.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, substantive_justice_advocates, payer,
    organized, generational, constrained, national).

% Individuals or groups whose fundamental rights claims are rooted in evolving moral understandings rather than explicit constitutional text find their arguments suppressed by a positivist judiciary. Their only recourse is often political action or seeking constitutional amendment.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, moral_rights_claimants, payer,
    powerless, immediate, trapped, local).

% Analyze and debate the merits and consequences of constitutional positivism, contributing to the ongoing discourse about interpretive methods. They are not directly subject to its enforcement but shape its intellectual environment.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% Their interpretive methodology, which emphasizes evolving social attitudes and circumstances in constitutional meaning, is explicitly rejected by the positivist reading. They are excluded from the legitimate interpretive framework under this constraint.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, living_constitutionalist_theorists, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, objective standard for constitutional interpretation, reducing judicial discretion and promoting legal certainty by grounding validity in formal enactment and institutional authority.
% TRANSFER_FUNCTION: Transfers interpretive authority from external moral or philosophical reasoning to formal legal text and procedures, from those seeking substantive justice based on evolving norms to those upholding procedural regularity and textual fidelity.
% ABSENT_VOICES: Natural law theorists, proponents of evolving moral standards, and those whose rights claims lack explicit textual grounding are structurally excluded from the legitimate interpretive framework. They would argue for a more morally responsive judiciary.
% DISAPPEARANCE_RATIONALE: If the positivist reading vanished overnight, constitutional interpretation would become highly subjective, leading to legal instability, judicial overreach based on individual moral preferences, and a collapse of predictable legal outcomes, forcing a reorganization of the entire legal system.
% FOUNDING_PROBLEM: To establish a stable, predictable legal order grounded in written law and formal processes, preventing arbitrary rule by judges and ensuring democratic accountability through a defined amendment procedure.
% FOUNDING_PROBLEM_CORROBORATION: Legal positivists, some political scientists, and institutionalists outside the direct beneficiaries would corroborate the ongoing need for formal legal grounding to maintain stability and prevent judicial activism. However, critics argue the problem has shifted from arbitrary rule to rigid injustice.
narrative_ontology:disappearance_verdict(us_constitution_meaning__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__positivist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(us_constitution_meaning__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__positivist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderately high (0.65) because this reading, by design, dismisses substantive moral claims that lack formal textual grounding, imposing a cost on those seeking justice through non-textual arguments. Suppression is high (0.75) as it actively excludes alternative interpretive methodologies (e.g., living constitutionalism) from legitimate legal discourse. The theater ratio is low (0.10) because it is a genuine, actively applied legal theory, not merely a performance. Accessibility collapse is moderate (0.60) as it closes off some interpretive avenues but leaves others (amendment, legislative action) open. Resistance is moderate (0.50) due to constant challenges from other interpretive schools.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of institutional actors (judges, legislature, executive), this reading provides essential legal stability and predictability, upholding the rule of law. From the perspective of substantive justice advocates and moral rights claimants, it can appear rigid and unjust, actively suppressing morally compelling arguments in favor of procedural formalism. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature, executive branch, and constitutional judges are beneficiaries, as the reading provides a clear, stable framework for their operations and upholds their institutional authority. Substantive justice advocates and moral rights claimants are victims, as their arguments are often dismissed for lacking formal textual basis. Legal scholars observe and critique, while living constitutionalist theorists are excluded, their methodology deemed illegitimate by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivism_originalism_distinction,
    'Is the positivist reading truly distinct from the originalist reading in practical application, or does it effectively collapse into originalism when the constitutional amendment process is gridlocked?',
    'Comparative legal analysis of judicial decisions under both frameworks in periods of legislative gridlock; empirical study of how judges articulate their reasoning when textualism and original intent diverge.',
    'If it collapses, its distinct contribution to legal theory is diminished, and its classification might merge with that of originalism, potentially amplifying its perceived extractiveness if originalism is more extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivism_originalism_distinction, conceptual, 'Ambiguity in the practical distinction between positivism and originalism.').

omega_variable(
    legitimacy_vs_justice_tradeoff,
    'Does the exclusion of external moral principles, while promoting procedural legitimacy, lead to outcomes so unjust that it ultimately undermines the broader legitimacy of the constitutional system?',
    'Longitudinal studies of public trust in the judiciary correlated with the perceived moral outcomes of constitutional rulings; philosophical analysis of the relationship between legal validity and moral acceptability.',
    'If severe injustice erodes legitimacy, the constraint''s long-term stability is threatened, and its effective extractiveness (χ) might be higher than measured, as it extracts public trust. This would shift its classification towards a Snare over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_vs_justice_tradeoff, preference, 'The tension between formal legal legitimacy and substantive moral justice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__positivist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__positivist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__positivist_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__positivist_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_meaning__positivist_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(us_c_tr_t80, us_constitution_meaning__positivist_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(us_c_tr_t100, us_constitution_meaning__positivist_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__positivist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__positivist_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__positivist_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(us_c_be_t60, us_constitution_meaning__positivist_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(us_c_be_t80, us_constitution_meaning__positivist_reading, base_extractiveness, 80, 0.64).
narrative_ontology:measurement(us_c_be_t100, us_constitution_meaning__positivist_reading, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__positivist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__positivist_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__positivist_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(us_c_su_t60, us_constitution_meaning__positivist_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(us_c_su_t80, us_constitution_meaning__positivist_reading, suppression_requirement, 80, 0.74).
narrative_ontology:measurement(us_c_su_t100, us_constitution_meaning__positivist_reading, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__living_constitutionalist_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
