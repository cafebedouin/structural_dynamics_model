% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanafi_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Analogical Reasoning and Juristic Preference
 *   domain: Islamic Jurisprudence / Legal Philosophy / Institutional History
 *
 * SUMMARY:
 *   This constraint represents the Hanafi school's jurisprudential method,
 *   which emphasizes analogical reasoning (qiyas) and juristic preference
 *   (istihsan) alongside the Qur'an and Hadith. It is one reading of the
 *   broader 'jurisprudential_method_kernel' that addresses how Islamic law is
 *   derived and applied. The method is presented as a necessary tool for
 *   legal adaptation, but it extracts from those who prefer strict textualism
 *   and suppresses alternative interpretive approaches. The claimed type is
 *   'tangled_rope' because it genuinely coordinates legal reasoning for novel
 *   cases while simultaneously extracting interpretive authority and imposing
 *   a specific methodological burden.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.7).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.65).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Jurisprudential Method: Analogical Reasoning and Juristic Preference").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "Islamic Jurisprudence / Legal Philosophy / Institutional History").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, '32532dda-7e94-4d71-a3fe-335a5b92851c').
narrative_ontology:cs_kernel_codification('32532dda-7e94-4d71-a3fe-335a5b92851c', formalized).
narrative_ontology:cs_authority_grounding('32532dda-7e94-4d71-a3fe-335a5b92851c', lineage).
narrative_ontology:cs_interpretation_layer_present('32532dda-7e94-4d71-a3fe-335a5b92851c').
narrative_ontology:cs_reading_relation('32532dda-7e94-4d71-a3fe-335a5b92851c', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('32532dda-7e94-4d71-a3fe-335a5b92851c', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('32532dda-7e94-4d71-a3fe-335a5b92851c', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('32532dda-7e94-4d71-a3fe-335a5b92851c', foundational, reason_extends_divine_intent).
narrative_ontology:cs_axiom_status(reason_extends_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('32532dda-7e94-4d71-a3fe-335a5b92851c', reason_extends_divine_intent, deontological).
narrative_ontology:cs_axiom('32532dda-7e94-4d71-a3fe-335a5b92851c', foundational, qiyas_istihsan_valid_sources).
narrative_ontology:cs_axiom_status(qiyas_istihsan_valid_sources, holdable).
narrative_ontology:cs_axiom_grounding('32532dda-7e94-4d71-a3fe-335a5b92851c', qiyas_istihsan_valid_sources, conventional).
narrative_ontology:cs_reference_frame('32532dda-7e94-4d71-a3fe-335a5b92851c', rational_legal_extension_framework).
narrative_ontology:cs_drift_state('32532dda-7e94-4d71-a3fe-335a5b92851c', contemporary_islamic_jurisprudence, gap(stable, minor, true)).
narrative_ontology:cs_created_at('32532dda-7e94-4d71-a3fe-335a5b92851c', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, rationalist_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textualist_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, lay_muslims_seeking_simple_answers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary proponents and practitioners of the Hanafi school, they apply and extend Islamic law using analogical reasoning (qiyas) and juristic preference (istihsan). Their professional identity and authority are deeply intertwined with this methodology, which they actively teach and defend.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists, agenda_setter,
    institutional, generational, identity_locked, global).

% Scholars who value and benefit from the intellectual space for reasoned interpretation and adaptation of Islamic law to novel cases. They find the Hanafi methodology congenial to their approach, though they may not be exclusively Hanafi.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, rationalist_scholars, beneficiary,
    organized, biographical, constrained, global).

% Scholars from other schools (e.g., Hanbali) or movements who advocate for a strict, literal interpretation of the Qur'an and Hadith. They view analogical reasoning and juristic preference as innovations (bid'ah) that deviate from divine intent, bearing the cost of their methodology being deemed insufficient for novel cases within the Hanafi framework.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, textualist_scholars, payer,
    powerful, generational, constrained, global).

% Individuals who may find the complex, nuanced process of analogical reasoning and juristic preference inaccessible or confusing, preferring direct, unambiguous answers derived solely from foundational texts. They bear the cost of needing specialized juristic interpretation for everyday matters.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, lay_muslims_seeking_simple_answers, payer,
    powerless, immediate, constrained, local).

% Jurists of the Maliki school, who emphasize the practice of the Medinan community. They observe and critique the Hanafi methodology from their own distinct interpretive framework, representing a different approach to legal derivation.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, maliki_jurists, observer,
    institutional, generational, analytical, global).

% Jurists of the Shafii school, known for their systematic four-tier hierarchy of legal sources. They engage with the Hanafi method, often highlighting methodological differences and advocating for their own standardized approach.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, shafii_jurists, observer,
    institutional, generational, analytical, global).

% Jurists of the Hanbali school, who prioritize the literal text and Companion opinions, often rejecting extensive analogical reasoning. They represent the most direct opposition to the Hanafi method's rationalist tendencies.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanbali_jurists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a systematic and adaptable methodology for deriving legal rulings (fatwas) for novel situations and changing societal contexts not explicitly covered by the Qur'an or Hadith, ensuring the continued relevance and consistency of Islamic law.
% TRANSFER_FUNCTION: Transfers interpretive authority from strict textual literalism to trained jurists capable of sophisticated analogical reasoning (qiyas) and juristic preference (istihsan), enabling the extension of divine intent to new cases. This also transfers the burden of complex legal derivation to these specialized scholars.
% ABSENT_VOICES: Strict textualists and literalists who reject the legitimacy of analogical reasoning and juristic preference as sources of law. While present in other schools, their methodological claims are structurally excluded from the Hanafi framework's internal discourse on legal derivation.
% DISAPPEARANCE_RATIONALE: If the Hanafi jurisprudential method vanished overnight, a significant portion of Islamic law, particularly in regions where it is historically dominant (e.g., South Asia, Ottoman lands), would lose its primary interpretive framework. This would lead to widespread legal uncertainty, a vacuum in addressing modern issues, and potentially a forced, disruptive adoption of other, less adaptable schools of thought.
% FOUNDING_PROBLEM: The challenge of applying divine law (Qur'an and Hadith) to an ever-expanding range of novel cases and changing societal circumstances that are not directly addressed in the foundational texts, while maintaining consistency, divine intent, and practical applicability.
% FOUNDING_PROBLEM_CORROBORATION: Hanafi jurists and their followers consistently attest to the ongoing necessity of their methodology for addressing contemporary issues. While critics from other schools dispute the Hanafi solution, they generally corroborate the existence of the underlying problem of legal adaptation to novel circumstances. Historical legal records and scholarly debates across centuries also attest to this persistent challenge.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanafi_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because the method requires specialized training and intellectual effort, effectively creating a barrier to entry for legal interpretation and concentrating authority among trained jurists. Suppression (0.65) is significant as it actively marginalizes purely textualist or literalist approaches within its sphere of influence, deeming them insufficient for comprehensive legal derivation. The theater ratio is low (0.1) because the intellectual work of qiyas and istihsan is genuinely performed and central to the school's function, not merely performative. Accessibility collapse is moderate (0.6) as it provides a clear, albeit complex, path for legal reasoning, but alternatives (simpler textualism) are significantly diminished. Resistance (0.55) is moderate, reflecting ongoing historical and contemporary debates with other schools that challenge the Hanafi method's premises.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hanafi jurists, this method is a vital and legitimate means of extending divine intent, ensuring the law's adaptability and justice. From the perspective of textualist scholars, it represents an unwarranted innovation that deviates from the purity of divine revelation. The engine's classification will highlight this divergence, showing coordination for beneficiaries and extraction for victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanafi jurists and rationalist scholars are beneficiaries (low d) as they gain interpretive authority and intellectual space. Textualist scholars and lay Muslims seeking simple answers are targets (high d) as they bear the cost of methodological complexity and the suppression of their preferred interpretive simplicity. Other schools' jurists are observers, neither directly benefiting nor paying within the Hanafi framework, but representing alternative, often resistant, positions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationality_limits_ambiguity,
    'What are the inherent limits of human reason (aql) in accurately extending divine intent to novel cases, and at what point does qiyas or istihsan risk exceeding these limits?',
    'Theological and philosophical discourse, comparative analysis of legal outcomes across schools, and empirical study of societal impact of rulings derived through extensive analogical reasoning.',
    'If reason''s limits are found to be narrower than assumed, the legitimacy of certain Hanafi derivations could be challenged, potentially increasing perceived extractiveness for those who prefer more direct textual guidance. If broader, it reinforces the Hanafi position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationality_limits_ambiguity, conceptual, 'Ambiguity regarding the scope and reliability of human reason in Islamic legal derivation.').

omega_variable(
    istihsan_objectivity_ambiguity,
    'To what extent is juristic preference (istihsan) a truly objective method for achieving legal equity, versus a subjective exercise influenced by individual jurists'' biases or societal pressures?',
    'Detailed case studies of istihsan application, analysis of juristic reasoning across different historical contexts, and internal critiques within the Hanafi school regarding its consistent application.',
    'If istihsan is found to be highly subjective, it could undermine the perceived fairness and universality of Hanafi rulings, increasing the perceived extraction from those subject to its application without clear, objective criteria.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_objectivity_ambiguity, empirical, 'Uncertainty about the objectivity and consistency of juristic preference (istihsan).').

omega_variable(
    textualist_legitimacy_ambiguity,
    'Is the textualist claim to exclusive authenticity (i.e., that only literal text and unanimous consensus are valid sources) a defensible methodological position, or does it inherently lead to legal stagnation and impracticality for modern societies?',
    'Comparative legal analysis of societies governed by purely textualist vs. adaptable methodologies, and ongoing theological debates about the nature of divine revelation and its application.',
    'If textualism is found to be practically unworkable, it would strengthen the Hanafi position. If it is found to be a viable alternative, it would reduce the perceived suppression of textualist approaches and challenge the Hanafi method''s necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textualist_legitimacy_ambiguity, preference, 'Ambiguity regarding the practical and theological legitimacy of purely textualist legal methodologies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(juri_tr_t20, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(juri_tr_t60, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(juri_tr_t80, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(juri_be_t20, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(juri_be_t60, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(juri_be_t80, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 80, 0.69).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 100, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(juri_su_t20, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(juri_su_t60, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 60, 0.63).
narrative_ontology:measurement(juri_su_t80, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 80, 0.64).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'jurisprudential_method_kernel', each representing a distinct school of Islamic legal thought. They are linked as a constraint family, with each reading offering a different approach to legal derivation and having distinct beneficiaries and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
