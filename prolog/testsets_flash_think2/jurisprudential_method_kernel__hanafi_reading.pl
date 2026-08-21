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
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Analogical Reasoning and Juristic Preference
 *   domain: islamic_jurisprudence/legal_philosophy
 *
 * SUMMARY:
 *   This constraint describes the Hanafi school's jurisprudential method,
 *   which posits that Islamic law derives from the Qur'an and Hadith, but is
 *   extensively filtered through analogical reasoning (qiyas) and juristic
 *   preference (istihsan). Reason is considered a legitimate tool for
 *   extending divine intent to novel cases. This reading is one of several
 *   competing methodologies within Islamic legal philosophy, each offering a
 *   distinct approach to deriving law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.65).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.55).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Jurisprudential Method: Analogical Reasoning and Juristic Preference").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "islamic_jurisprudence/legal_philosophy").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, 'c7fd285e-e7fc-4df5-9587-5365da57314d').
narrative_ontology:cs_kernel_codification('c7fd285e-e7fc-4df5-9587-5365da57314d', formalized).
narrative_ontology:cs_authority_grounding('c7fd285e-e7fc-4df5-9587-5365da57314d', lineage).
narrative_ontology:cs_interpretation_layer_present('c7fd285e-e7fc-4df5-9587-5365da57314d').
narrative_ontology:cs_reading_relation('c7fd285e-e7fc-4df5-9587-5365da57314d', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7fd285e-e7fc-4df5-9587-5365da57314d', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7fd285e-e7fc-4df5-9587-5365da57314d', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('c7fd285e-e7fc-4df5-9587-5365da57314d', foundational, reason_as_source_of_law).
narrative_ontology:cs_axiom_status(reason_as_source_of_law, holdable).
narrative_ontology:cs_axiom_grounding('c7fd285e-e7fc-4df5-9587-5365da57314d', reason_as_source_of_law, deontological).
narrative_ontology:cs_axiom('c7fd285e-e7fc-4df5-9587-5365da57314d', foundational, qiyas_istihsan_as_valid_sources).
narrative_ontology:cs_axiom_status(qiyas_istihsan_as_valid_sources, holdable).
narrative_ontology:cs_axiom_grounding('c7fd285e-e7fc-4df5-9587-5365da57314d', qiyas_istihsan_as_valid_sources, conventional).
narrative_ontology:cs_reference_frame('c7fd285e-e7fc-4df5-9587-5365da57314d', rationalist_juristic_autonomy).
narrative_ontology:cs_drift_state('c7fd285e-e7fc-4df5-9587-5365da57314d', contemporary_islamic_revivalism, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c7fd285e-e7fc-4df5-9587-5365da57314d', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, hanafi_legal_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textualist_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, lay_muslims_seeking_direct_textual_guidance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, lay_muslims_seeking_direct_textual_guidance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These jurists apply, teach, and defend the Hanafi methodology, benefiting from the intellectual framework and the authority it grants them in deriving legal rulings for novel cases. Their professional identity is deeply intertwined with the school's rationalist approach.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanafi_legal_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Scholars from other schools or those advocating for a strict textualist approach find their interpretive methods marginalized or explicitly rejected by the Hanafi emphasis on reason beyond literal text. They bear the cost of reduced influence and perceived authenticity.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, textualist_scholars, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, textualist_scholars, excluded).

% These individuals receive legal rulings derived from the Hanafi method, which can be complex and require reliance on juristic authority. While benefiting from a coherent legal system, they 'pay' by accepting interpretations that may diverge from a direct reading of primary texts.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, lay_muslims_seeking_direct_textual_guidance, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, lay_muslims_seeking_direct_textual_guidance, beneficiary).

% Scholars of the Maliki school, who emphasize the practice of the Medinan community, observe and critique the Hanafi method from their own distinct jurisprudential framework.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, maliki_scholars, observer,
    organized, generational, mobile, global).

% Scholars of the Shafii school, known for its strict hierarchical methodology, observe and engage in intellectual debate with the Hanafi approach, often highlighting methodological differences.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, shafii_scholars, observer,
    organized, generational, mobile, global).

% Scholars of the Hanbali school, which prioritizes literal adherence to text and tradition, often strongly oppose the Hanafi school's use of analogical reasoning and juristic preference, viewing them as innovations.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanbali_scholars, observer,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanafi_reading, hanafi_legal_scholars).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a systematic and adaptable method for deriving Islamic law from primary sources (Qur'an and Hadith), enabling consistent legal rulings for novel cases and evolving societal contexts.
% TRANSFER_FUNCTION: Transfers interpretive authority from direct, literal textual engagement to trained jurists capable of extensive analogical reasoning and juristic preference. It also transfers the burden of legal derivation from individual textual interpretation to a structured, rationalist methodology.
% ABSENT_VOICES: Strict textualists, those advocating for direct, unmediated access to divine texts without extensive juristic interpretation, and proponents of other schools whose methodologies are implicitly or explicitly de-prioritized by the Hanafi framework.
% DISAPPEARANCE_RATIONALE: If the Hanafi jurisprudential method vanished overnight, the application of Islamic law within its sphere of influence would become highly fragmented and inconsistent, especially for novel cases. The established legal system would lose its coherence, leading to significant societal and intellectual reorganization.
% FOUNDING_PROBLEM: The challenge of applying divine revelation (Qur'an and Hadith) to an ever-expanding range of human situations and novel legal problems not explicitly covered by the texts, while maintaining consistency, legitimacy, and adaptability.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Islamic law and contemporary legal scholars (including those from other schools) acknowledge the historical necessity of developing systematic legal methodologies to address societal changes and new challenges. While they may disagree on the specific methods, the underlying problem of legal application to novel contexts remains live.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanafi_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it provides a genuine coordination function (a systematic method for legal derivation) but also involves asymmetric extraction. Extraction (0.65) is high on novel cases, as the method empowers a specific class of jurists with rationalist training, potentially marginalizing more textualist approaches. Suppression (0.55) reflects the active defense and propagation of this methodology, which implicitly or explicitly suppresses alternative interpretive frameworks within its sphere of influence. The theater ratio is low (0.20) because the method is genuinely functional, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hanafi jurists, this method is a necessary and legitimate means of ensuring divine law remains relevant and applicable. From the perspective of textualist scholars, it represents an innovation (bid'ah) that deviates from the authentic sources, leading to a perception of extraction of interpretive authority. The engine's classification captures this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanafi legal scholars are the primary beneficiaries and agenda-setters, as they wield the interpretive tools and gain authority from the method's application. Textualist scholars and lay Muslims seeking direct textual guidance are victims, as their preferred modes of engagement with divine texts are constrained or superseded by the juristic methodology. Other schools of thought (Maliki, Shafii, Hanbali) are observers or payers, bearing the cost of intellectual competition and differing legitimacy claims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationalism_vs_textualism_legitimacy,
    'Is human reason, as applied through qiyas and istihsan, a legitimate and authentic tool for extending divine intent, or does it constitute an unwarranted innovation (bid''ah) that corrupts the kernel of divine law?',
    'Theological and philosophical debate, historical analysis of early Islamic legal practice, and the long-term societal impact of rulings derived from each approach.',
    'If deemed an unwarranted innovation, the Hanafi method''s legitimacy would be severely undermined, reclassifying it closer to a Snare for those who adhere to strict textualism. If affirmed as legitimate, its coordination function would be strengthened, potentially moving it closer to a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rationalism_vs_textualism_legitimacy, conceptual, 'Ambiguity regarding the theological legitimacy of rationalist legal tools.').

omega_variable(
    istihsan_scope_ambiguity,
    'To what extent can juristic preference (istihsan) be applied without undermining the systematic nature of qiyas or the primacy of textual sources, and is its application consistently bounded within the Hanafi school?',
    'Detailed comparative analysis of Hanafi legal rulings across different historical periods and geographical regions, examining the consistency and limits of istihsan''s application.',
    'If istihsan is found to be inconsistently or excessively applied, it could indicate a higher degree of arbitrary extraction by individual jurists, increasing the effective extractiveness and potentially shifting the classification towards a Snare. If consistently bounded, it reinforces the method''s coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_scope_ambiguity, empirical, 'Uncertainty regarding the scope and consistency of juristic preference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(juri_tr_t300, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 300, 0.17).
narrative_ontology:measurement(juri_tr_t600, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 600, 0.19).
narrative_ontology:measurement(juri_tr_t900, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 900, 0.2).
narrative_ontology:measurement(juri_tr_t1200, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1200, 0.2).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(juri_be_t300, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 300, 0.58).
narrative_ontology:measurement(juri_be_t600, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 600, 0.62).
narrative_ontology:measurement(juri_be_t900, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 900, 0.64).
narrative_ontology:measurement(juri_be_t1200, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1200, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(juri_su_t300, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 300, 0.5).
narrative_ontology:measurement(juri_su_t600, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 600, 0.53).
narrative_ontology:measurement(juri_su_t900, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 900, 0.54).
narrative_ontology:measurement(juri_su_t1200, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1200, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jurisprudential_method_kernel', which describes the fundamental question of how Islamic law is derived. This Hanafi reading emphasizes analogical reasoning and juristic preference, distinguishing it from other schools like the Maliki (Medinan practice), Shafii (strict hierarchy), and Hanbali (literal textualism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
