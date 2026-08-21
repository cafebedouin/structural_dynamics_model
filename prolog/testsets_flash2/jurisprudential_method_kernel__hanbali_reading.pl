% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanbali_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Hanbali Jurisprudential Method: Textual Literalism and Rejection of Analogical Reasoning
 *   domain: islamic_jurisprudence/legal_philosophy/institutional_history
 *
 * SUMMARY:
 *   This constraint describes the Hanbali school's jurisprudential
 *   methodology, which emphasizes strict textual adherence to the Qur'an,
 *   Hadith, and Companion opinions, while rejecting analogical reasoning
 *   (qiyas) and juristic preference (istihsan) as illegitimate innovations
 *   (bid'ah). This reading is one of several competing methodologies within
 *   Islamic jurisprudence, each forming a distinct constraint. The Hanbali
 *   reading is characterized by high extraction from rationalist jurists and
 *   customary practices, and high suppression of alternative interpretive
 *   methods. The claimed type is 'snare' because its coordination story
 *   (purity of divine law) serves as cover for the suppression of
 *   intellectual diversity and the consolidation of authority within a
 *   specific textualist lineage.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.85).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.75).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, snare).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Hanbali Jurisprudential Method: Textual Literalism and Rejection of Analogical Reasoning").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "islamic_jurisprudence/legal_philosophy/institutional_history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, '00e6a8cd-5d2b-4aca-abd1-734890d27fc9').
narrative_ontology:cs_kernel_codification('00e6a8cd-5d2b-4aca-abd1-734890d27fc9', fixed_text).
narrative_ontology:cs_authority_grounding('00e6a8cd-5d2b-4aca-abd1-734890d27fc9', lineage).
narrative_ontology:cs_interpretation_layer_present('00e6a8cd-5d2b-4aca-abd1-734890d27fc9').
narrative_ontology:cs_reading_relation('00e6a8cd-5d2b-4aca-abd1-734890d27fc9', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('00e6a8cd-5d2b-4aca-abd1-734890d27fc9', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('00e6a8cd-5d2b-4aca-abd1-734890d27fc9', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('00e6a8cd-5d2b-4aca-abd1-734890d27fc9', foundational, literal_text_supremacy).
narrative_ontology:cs_axiom_status(literal_text_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('00e6a8cd-5d2b-4aca-abd1-734890d27fc9', literal_text_supremacy, deontological).
narrative_ontology:cs_axiom('00e6a8cd-5d2b-4aca-abd1-734890d27fc9', foundational, qiyas_istihsan_bidah).
narrative_ontology:cs_axiom_status(qiyas_istihsan_bidah, holdable).
narrative_ontology:cs_axiom_grounding('00e6a8cd-5d2b-4aca-abd1-734890d27fc9', qiyas_istihsan_bidah, theological).
narrative_ontology:cs_reference_frame('00e6a8cd-5d2b-4aca-abd1-734890d27fc9', early_salaf_practice).
narrative_ontology:cs_drift_state('00e6a8cd-5d2b-4aca-abd1-734890d27fc9', contemporary_legal_pluralism, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('00e6a8cd-5d2b-4aca-abd1-734890d27fc9', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, textualist_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, conservative_religious_institutions).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, customary_practice_adherents).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, innovative_legal_thinkers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, lay_muslim_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These scholars uphold the Hanbali methodology, emphasizing strict adherence to the literal text of the Qur'an and Hadith. Their authority and careers are built on this interpretive framework, which positions them as guardians of 'orthodoxy' against perceived innovations. They actively enforce the rejection of analogical reasoning and juristic preference.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, textualist_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Institutions that benefit from the stability and perceived purity offered by the Hanbali method. This approach often aligns with their conservative theological and social agendas, providing a clear, unyielding legal framework that resists modern interpretations and challenges to traditional authority. They fund and promote textualist scholarship.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, conservative_religious_institutions, beneficiary,
    institutional, generational, constrained, national).

% Jurists from other schools (like Hanafi) who rely on analogical reasoning (qiyas) and juristic preference (istihsan) to derive law for novel cases. They face intellectual and institutional pressure from Hanbali adherents, who label their methods as 'bid'ah' (innovation), potentially undermining their legitimacy and career prospects within certain contexts.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    powerful, biographical, constrained, global).

% Communities or individuals whose legal and social practices are rooted in local customs or interpretations that might not have direct textual support from the Qur'an or Hadith, or which rely on broader consensus (ijma) that is not strictly unanimous. They find their practices challenged or delegitimized by the Hanbali method's strictures.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, customary_practice_adherents, payer,
    moderate, biographical, constrained, local).

% Scholars and thinkers who seek to develop new legal interpretations or methodologies to address contemporary challenges, often drawing on broader principles or rationalist approaches. They are directly targeted by the Hanbali method's rejection of 'innovation' and face significant resistance, marginalization, or accusations of heresy, making their intellectual 'exit' from the Hanbali framework costly to their professional identity.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, innovative_legal_thinkers, payer,
    moderate, biographical, identity_locked, global).

% Many in the lay community benefit from the perceived clarity and certainty offered by a literalist approach, which can simplify religious observance and legal understanding. However, they are also constrained by the rigidity of the system, which may not easily adapt to their evolving social realities or diverse needs, leading to a sense of alienation or difficulty in applying the law.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, lay_muslim_community, beneficiary,
    powerless, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unambiguous method for deriving Islamic law, ensuring consistency and preventing perceived corruption of divine revelation by human reason or innovation. It coordinates legal interpretation around foundational texts.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual juristic reasoning and local custom to a strict textualist methodology, consolidating power within a specific scholarly lineage and conservative institutions. It extracts intellectual freedom and adaptability from other jurists and communities.
% ABSENT_VOICES: Early Islamic jurists who extensively used qiyas (analogical reasoning) and istihsan (juristic preference) would object, arguing for the necessity of reason in extending divine intent. Modern reformist thinkers, seeking to adapt Islamic law to contemporary contexts, are also excluded, as their methods are deemed 'bid'ah'.
% DISAPPEARANCE_RATIONALE: If the Hanbali method's strictures vanished, the landscape of Islamic jurisprudence would immediately diversify. Rationalist approaches would gain prominence, allowing for greater legal flexibility and adaptation to modern challenges. The authority of textualist scholars would diminish, and new forms of consensus-building would emerge, leading to a significant reorganization of legal and religious institutions.
% FOUNDING_PROBLEM: The proliferation of diverse legal opinions and the perceived corruption of divine law through excessive reliance on human reason and local customs, leading to a desire for a pure, text-based methodology.
% FOUNDING_PROBLEM_CORROBORATION: Textualist scholars and conservative institutions attest that the problem of 'innovation' and deviation from textual sources remains live. However, rationalist jurists and legal historians outside this school argue that the problem was largely a methodological dispute, and the Hanbali approach itself created new forms of rigidity that hinder legal development, making the 'problem' a contested framing.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanbali_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because this method severely limits the tools available for legal derivation, forcing jurists to conform to a narrow textualist framework or face delegitimization. Suppression is also high (0.75) as it actively labels alternative methods as 'bid'ah', effectively coercing adherence and suppressing intellectual dissent. The theater ratio is low (0.1) because the method is genuinely applied and enforced, not merely performed. Accessibility collapse is high (0.8) as it aims to collapse all alternative interpretive paths. Resistance is moderate (0.6) as other schools and thinkers continuously challenge its rigidity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of textualist scholars, this method is a 'rope' or even a 'mountain' – a necessary return to the pure sources of Islam, ensuring divine law's integrity. From the perspective of rationalist jurists, it is a 'snare' that stifles intellectual inquiry and prevents the law from addressing new realities. The engine's classification as 'snare' reflects the structural reality of extraction and suppression, regardless of the internal justification.
 *
 * DIRECTIONALITY LOGIC:
 *   Textualist scholars and conservative religious institutions are the primary beneficiaries and agenda-setters, as their authority is reinforced by this method. Rationalist jurists, customary practice adherents, and innovative legal thinkers are the victims/payers, as their methods are delegitimized and suppressed. The lay Muslim community is a mixed beneficiary/payer, gaining clarity but losing adaptability.
 *
 * MANDATROPHY ANALYSIS:
 *   The Hanbali method's mandate to preserve the purity of divine law is still 'live' for its adherents. However, the classification as 'snare' prevents mislabeling it as pure coordination. The high extractiveness and suppression, particularly against methods like qiyas which are widely accepted in other schools, indicate that the constraint's persistence is less about solving a universal coordination problem and more about maintaining a specific interpretive authority through coercion. The 'mandate' is used to justify the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_reason_in_law,
    'Is human reason (qiyas, istihsan) a legitimate tool for deriving Islamic law, or is it an innovation that corrupts divine intent?',
    'Historical analysis of early Islamic legal development and theological arguments regarding the scope of divine revelation and human intellect. Consensus among diverse scholarly traditions on the role of reason.',
    'If reason is deemed legitimate, the Hanbali method''s suppression of qiyas would be reclassified as pure extraction, shifting its type further towards snare. If reason is deemed illegitimate, the Hanbali method''s claims of purity would be strengthened, potentially lowering its perceived extractiveness for its adherents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_reason_in_law, conceptual, 'The fundamental conceptual dispute over the role of human reason in Islamic legal derivation.').

omega_variable(
    unanimous_consensus_feasibility,
    'Is ''unanimous consensus'' (ijma) a practically achievable and verifiable source of law in a globally diverse Muslim community, or is it an idealized concept that effectively limits legal development?',
    'Empirical study of historical and contemporary instances of ijma, assessing the criteria for ''unanimity'' and the mechanisms for its verification across different schools and regions.',
    'If unanimous consensus is found to be practically unachievable, the Hanbali method''s reliance on it as a primary source (after text) would be seen as a further constraint on legal development, increasing its effective suppression. If it is found feasible, it would lend more weight to the Hanbali claim of a robust, non-innovative source of law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimous_consensus_feasibility, empirical, 'The empirical feasibility and practical implications of the Hanbali method''s strict definition of ijma.').

omega_variable(
    identity_lock_mechanism_for_jurists,
    'To what extent is the ''identity_locked'' exit option for rationalist and innovative jurists a result of structural barriers (e.g., institutional funding, publication gatekeeping) versus internalized professional identity (e.g., fear of being labeled ''heretical'' or ''unorthodox'' by peers)?',
    'Sociological studies of career trajectories and intellectual freedom within different Islamic legal institutions, combined with qualitative interviews exploring jurists'' self-perceptions and fears of professional ostracization.',
    'If internalized identity lock is a dominant factor, the effective suppression is higher than structural measures suggest, as jurists carry the suppression with them even if external barriers are reduced. If structural barriers are primary, policy interventions (e.g., academic freedom protections) could more directly reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_jurists, empirical, 'Structural vs. internalized suppression mechanism for jurists adhering to alternative methodologies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(juri_tr_t300, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 300, 0.1).
narrative_ontology:measurement(juri_tr_t600, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 600, 0.1).
narrative_ontology:measurement(juri_tr_t900, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement(juri_tr_t1200, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1200, 0.1).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(juri_be_t300, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 300, 0.75).
narrative_ontology:measurement(juri_be_t600, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 600, 0.8).
narrative_ontology:measurement(juri_be_t900, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 900, 0.83).
narrative_ontology:measurement(juri_be_t1200, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1200, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(juri_su_t300, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 300, 0.65).
narrative_ontology:measurement(juri_su_t600, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 600, 0.7).
narrative_ontology:measurement(juri_su_t900, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 900, 0.73).
narrative_ontology:measurement(juri_su_t1200, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1200, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, shafii_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jurisprudential_method_kernel'. Its strict textualism and rejection of analogical reasoning directly influence the interpretive space for other schools, which either react against it or are constrained by its claims of methodological purity. This is part of a family of constraints representing different schools of Islamic jurisprudence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
