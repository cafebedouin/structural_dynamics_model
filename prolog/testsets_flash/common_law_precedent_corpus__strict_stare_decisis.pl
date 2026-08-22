% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__strict_stare_decisis, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis in Common Law Precedent
 *   domain: legal/jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the 'strict stare decisis' reading of common
 *   law precedent, where past judicial rulings are considered highly binding
 *   and can only be departed from under extraordinary circumstances. It is
 *   presented as a mechanism for legal stability and predictability, but its
 *   rigid application can lead to significant extraction from litigants and
 *   social movements seeking to adapt law to contemporary realities. The
 *   claimed type is 'tangled_rope' because it genuinely provides coordination
 *   (stability) but also involves asymmetric extraction and requires active
 *   enforcement to maintain its rigidity.
 *
 * KEY AGENTS:
 *   - judicial_conservatives: Primary agenda-setter (institutional/identity_locked) — actively enforces strict adherence to precedent.
 *   - litigants_seeking_norm_change: Primary target (powerless/trapped) — bears the costs of rigid precedent.
 *   - social_reform_advocates: Secondary target (organized/constrained) — constrained by the high bar for legal change.
 *   - legal_system_stability: Abstract beneficiary (analytical/analytical) — benefits from predictability.
 *   - judicial_moderates: Observer (institutional/constrained) — navigates the tension between stability and evolution.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.65).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.75).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.65).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis in Common Law Precedent").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal/jurisprudence").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, '493709c8-bae3-483e-9b1d-ec22c16c039b').
narrative_ontology:cs_kernel_codification('493709c8-bae3-483e-9b1d-ec22c16c039b', formalized).
narrative_ontology:cs_authority_grounding('493709c8-bae3-483e-9b1d-ec22c16c039b', lineage).
narrative_ontology:cs_interpretation_layer_present('493709c8-bae3-483e-9b1d-ec22c16c039b').
narrative_ontology:cs_reading_relation('493709c8-bae3-483e-9b1d-ec22c16c039b', common_law_precedent_corpus__evolutionary_framework, influences).
narrative_ontology:cs_reading_relation('493709c8-bae3-483e-9b1d-ec22c16c039b', common_law_precedent_corpus__pluralist_balancing, influences).
narrative_ontology:cs_axiom('493709c8-bae3-483e-9b1d-ec22c16c039b', foundational, precedent_binds_categorically).
narrative_ontology:cs_axiom_status(precedent_binds_categorically, holdable).
narrative_ontology:cs_axiom_grounding('493709c8-bae3-483e-9b1d-ec22c16c039b', precedent_binds_categorically, deontological).
narrative_ontology:cs_axiom('493709c8-bae3-483e-9b1d-ec22c16c039b', foundational, judicial_restraint_is_supreme).
narrative_ontology:cs_axiom_status(judicial_restraint_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('493709c8-bae3-483e-9b1d-ec22c16c039b', judicial_restraint_is_supreme, conventional).
narrative_ontology:cs_reference_frame('493709c8-bae3-483e-9b1d-ec22c16c039b', classical_legal_positivism).
narrative_ontology:cs_drift_state('493709c8-bae3-483e-9b1d-ec22c16c039b', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('493709c8-bae3-483e-9b1d-ec22c16c039b', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, judicial_conservatives).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, legal_system_stability).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, litigants_seeking_norm_change).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, social_reform_advocates).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, judicial_restraint_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges and legal scholars who prioritize adherence to past rulings, viewing it as essential for legal predictability and limiting judicial activism. They actively enforce the strict application of precedent and resist its reinterpretation or overruling.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, judicial_conservatives, agenda_setter,
    institutional, generational, identity_locked, national).

% Parties in legal disputes whose cases challenge existing legal norms or seek to overturn established precedents. They face significant hurdles and costs due to the high bar for departing from precedent, often losing cases that might succeed under a more flexible interpretive framework.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, litigants_seeking_norm_change, payer,
    powerless, immediate, trapped, local).

% Organizations and movements pushing for legal changes to align with evolving social values. They find their efforts constrained by the rigidity of strict stare decisis, requiring extraordinary political or legislative action rather than judicial evolution.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, social_reform_advocates, payer,
    organized, generational, constrained, national).

% The abstract quality of a predictable and consistent legal framework. It benefits from strict adherence to precedent by reducing uncertainty and promoting public confidence in the law's impartiality, even if this comes at the cost of adaptability.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_system_stability, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(common_law_precedent_corpus__strict_stare_decisis, legal_system_stability).

% Judges and scholars who acknowledge the value of precedent but seek a balance between stability and the need for legal evolution. They observe the tension between strict adherence and societal change, often seeking narrow paths for distinguishing cases rather than outright overruling.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, judicial_moderates, observer,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for legal interpretation and application, ensuring consistency across judicial decisions and guiding legal actors on expected outcomes.
% TRANSFER_FUNCTION: Transfers the burden of legal change from the judiciary to the legislative branch or requires extraordinary justification from litigants, preserving the authority of past rulings and the interpretive power of those who uphold them.
% ABSENT_VOICES: Future generations and marginalized groups whose interests were not represented in past legal decisions are implicitly excluded. Their perspectives would challenge the legitimacy of rigidly binding past norms that do not reflect contemporary justice or social realities.
% DISAPPEARANCE_RATIONALE: If strict stare decisis vanished, judicial decisions would lose their binding force, leading to legal chaos, unpredictable outcomes, and a collapse of confidence in the legal system's ability to provide consistent justice. Every case would be decided de novo, and the entire structure of common law would dissolve.
% FOUNDING_PROBLEM: The problem of arbitrary judicial decision-making and unpredictable legal outcomes, leading to a lack of public trust and an inability for citizens to order their affairs according to known laws.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and political scientists, alongside judicial conservatives, corroborate that the problem of legal uncertainty remains live, and strict stare decisis is seen as a bulwark against it. Social reform advocates, however, contest whether the current application of stare decisis genuinely solves this problem or merely entrenches outdated norms.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__strict_stare_decisis, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the strict application of precedent often forces outcomes that are misaligned with contemporary justice or economic realities, imposing costs on those who cannot overcome the high bar for change. Suppression is also high (0.75) as the legal system actively suppresses attempts to overturn or significantly reinterpret precedent, requiring extraordinary justification and often lengthy, costly litigation. The theater ratio is low (0.20) because the commitment to precedent is largely genuine, though some arguments for its absolute necessity may be performative when applied to clearly outdated rulings. The metrics show a gradual increase in extractiveness and suppression over time, reflecting the hardening of this interpretive stance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of judicial conservatives, strict stare decisis is a 'rope' that ensures the integrity and predictability of the legal system. From the perspective of litigants seeking norm change and social reform advocates, it operates as a 'snare' or 'tangled_rope', trapping them in outdated legal frameworks and extracting significant resources to achieve even minor adjustments. The engine will compute these divergent classifications based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Judicial conservatives are beneficiaries (d near 0.0) as their interpretive framework is upheld and their authority reinforced. Litigants seeking norm change and social reform advocates are targets (d near 1.0) as they bear the direct costs of the constraint's rigidity. Legal system stability is an abstract beneficiary. Judicial moderates are observers, experiencing a more balanced directionality as they navigate the system's internal tensions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to provide stability and predictability is still live, but its strict application risks entrenching outdated norms, leading to a potential mandatrophy where the 'coordination' function becomes a cover for 'extraction'. The high extractiveness and suppression, coupled with the 'contested' status of the founding problem, suggest a drift towards a snare-like operation for those seeking change, even as it maintains a rope-like function for those who benefit from stability. The classification as 'tangled_rope' captures this hybrid nature, preventing mislabeling it as pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strictness_vs_adaptability_balance,
    'What is the optimal balance between legal stability (strict stare decisis) and adaptability to evolving social norms and empirical realities?',
    'Longitudinal studies comparing legal systems with different approaches to precedent, assessing outcomes in terms of justice, efficiency, and public trust. Deliberative democratic processes to establish societal preferences for legal change.',
    'If a more adaptable approach is deemed optimal, the current strictness would be reclassified as excessive extraction. If strictness is reaffirmed, the current classification would be validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strictness_vs_adaptability_balance, preference, 'The normative trade-off between legal rigidity and flexibility.').

omega_variable(
    judicial_activism_definition,
    'Is the ''extraordinary justification'' required for departing from precedent a neutral standard, or is it selectively applied to suppress certain types of legal change?',
    'Empirical analysis of judicial decisions, comparing the success rates of different types of challenges to precedent and identifying patterns of bias in the application of ''extraordinary justification'' criteria.',
    'If the standard is found to be selectively applied, the suppression metric would be re-evaluated as higher and more targeted, pushing the constraint further towards a Snare classification for specific groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_activism_definition, empirical, 'Whether the standard for overturning precedent is neutrally applied.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine ''strict stare decisis'' reading of common law precedent, or is it a more flexible ''evolutionary framework'' that is merely presented as strict for strategic reasons?',
    'Analysis of judicial opinions and legal scholarship over time, focusing on the actual methods of distinguishing and reinterpreting precedent versus explicit overruling. Comparison with other common law jurisdictions.',
    'If it is found to be a more flexible framework, the extractiveness and suppression metrics would be lower, and the claimed type might shift towards a ''Rope'' or ''Scaffold'' (if temporary) classification, reflecting a more adaptive system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between strict and flexible readings of common law precedent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1950, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(comm_tr_t1970, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(comm_tr_t1990, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(comm_tr_t2010, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(comm_tr_t2024, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t1950, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(comm_be_t1970, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(comm_be_t1990, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1990, 0.63).
narrative_ontology:measurement(comm_be_t2010, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(comm_be_t2024, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1950, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(comm_su_t1970, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(comm_su_t1990, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1990, 0.73).
narrative_ontology:measurement(comm_su_t2010, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(comm_su_t2024, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'common_law_precedent_corpus' kernel. Its strict interpretation of stare decisis directly influences the operational space for more flexible readings by setting a high bar for legal change.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
