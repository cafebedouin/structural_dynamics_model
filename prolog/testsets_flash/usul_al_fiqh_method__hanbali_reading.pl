% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanbali_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: usul_al_fiqh_method__hanbali_reading
 *   human_readable: Hanbali Reading of Usul al-Fiqh: Textual Restrictiveness
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   This constraint describes the Hanbali school's methodology within Islamic
 *   jurisprudence (usul al-fiqh), characterized by maximal textual
 *   restrictiveness, minimal analogical reasoning (qiyas), preference for
 *   weak hadith over qiyas, and a strong emphasis on blocking innovations
 *   (sadd al-dhara'i) to preserve textual fidelity. It is one reading of the
 *   broader 'usul_al_fiqh_method' kernel, which encompasses diverse
 *   interpretive approaches across Sunni legal schools. The Hanbali reading
 *   prioritizes textual literalism and tradition over rationalist legal
 *   development.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.65).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.75).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Reading of Usul al-Fiqh: Textual Restrictiveness").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, '1521fc40-f165-4efe-8550-b1ae7a8140ab').
narrative_ontology:cs_kernel_codification('1521fc40-f165-4efe-8550-b1ae7a8140ab', fixed_text).
narrative_ontology:cs_authority_grounding('1521fc40-f165-4efe-8550-b1ae7a8140ab', lineage).
narrative_ontology:cs_interpretation_layer_present('1521fc40-f165-4efe-8550-b1ae7a8140ab').
narrative_ontology:cs_reading_relation('1521fc40-f165-4efe-8550-b1ae7a8140ab', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('1521fc40-f165-4efe-8550-b1ae7a8140ab', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('1521fc40-f165-4efe-8550-b1ae7a8140ab', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('1521fc40-f165-4efe-8550-b1ae7a8140ab', foundational, textual_primacy_over_reason).
narrative_ontology:cs_axiom_status(textual_primacy_over_reason, holdable).
narrative_ontology:cs_axiom_grounding('1521fc40-f165-4efe-8550-b1ae7a8140ab', textual_primacy_over_reason, deontological).
narrative_ontology:cs_axiom('1521fc40-f165-4efe-8550-b1ae7a8140ab', foundational, blocking_means_to_evil_as_source).
narrative_ontology:cs_axiom_status(blocking_means_to_evil_as_source, holdable).
narrative_ontology:cs_axiom_grounding('1521fc40-f165-4efe-8550-b1ae7a8140ab', blocking_means_to_evil_as_source, conventional).
narrative_ontology:cs_reference_frame('1521fc40-f165-4efe-8550-b1ae7a8140ab', salafi_textual_purity).
narrative_ontology:cs_drift_state('1521fc40-f165-4efe-8550-b1ae7a8140ab', contemporary_globalized_islam, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('1521fc40-f165-4efe-8550-b1ae7a8140ab', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, textualist_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, conservative_ulema).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_legal_developers).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, innovative_muftis).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, lay_muslims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for strict adherence to Quran and authenticated Hadith, minimizing the role of human reason or analogy. They gain authority and legitimacy by presenting their interpretations as the most faithful to foundational texts, thereby limiting alternative legal methodologies.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, textualist_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the Hanbali methodology's emphasis on textual fidelity, which reinforces their authority in interpreting established sources and resisting legal innovations (bid'a). This approach provides a clear framework for legal rulings, reducing ambiguity but also limiting flexibility.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, conservative_ulema, beneficiary,
    organized, generational, constrained, global).

% Bear the cost of minimized qiyas (analogical reasoning) and preference for weak hadith over rational inference. Their ability to develop legal solutions for contemporary issues through reasoned opinion is constrained, leading to a perception of legal stagnation or irrelevance in certain contexts.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, regional).

% Find their efforts to integrate local customs ('urf) or public interest (maslaha) into legal rulings severely restricted. The Hanbali emphasis on textual sources often overrides customary practices, leading to a disconnect between formal law and lived social norms.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, customary_legal_developers, payer,
    powerless, biographical, trapped, local).

% Face significant challenges in issuing fatwas (legal opinions) that address modern complexities, as the Hanbali method limits the scope for independent reasoning (ijtihad) and prioritizes textual precedents, even weak ones, over analogical extension.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, innovative_muftis, payer,
    moderate, biographical, constrained, national).

% Benefit from a clear, consistent, and textually grounded legal framework that minimizes perceived human error or innovation. However, they may also experience a lack of flexibility in addressing new challenges, leading to a sense of rigidity in legal application.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, lay_muslims, beneficiary,
    powerless, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a highly structured and textually anchored methodology for deriving Islamic law, ensuring consistency and minimizing divergence from foundational sources across different jurists and regions.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual juristic reasoning (ra'y, qiyas) to the literal interpretation of sacred texts (Quran, Hadith), thereby concentrating interpretive power within textualist scholarly circles.
% ABSENT_VOICES: Jurists advocating for expansive qiyas, istihsan (juristic preference), maslaha mursala (unrestricted public interest), or 'urf (custom) are marginalized. They would argue for greater flexibility and responsiveness to changing social realities, but their methodologies are systematically de-prioritized by this reading.
% DISAPPEARANCE_RATIONALE: If the Hanbali methodology vanished, the landscape of Islamic jurisprudence would fundamentally shift. Other schools with more expansive interpretive tools would gain prominence, leading to a more diverse and potentially more adaptable body of law, but also potentially greater interpretive fragmentation.
% FOUNDING_PROBLEM: The Hanbali school emerged in response to perceived excesses of rationalist interpretation and the proliferation of speculative legal opinions, aiming to restore fidelity to the earliest textual sources and the practice of the Salaf (pious predecessors).
% FOUNDING_PROBLEM_CORROBORATION: The problem of interpretive divergence and perceived innovation remains a live concern for many Islamic scholars and communities, particularly those advocating for a return to 'pure' Islam. While other schools contest the severity of the 'excesses' that Hanbalism sought to correct, the underlying tension between textualism and rationalism in Islamic law is widely acknowledged by historians of Islamic jurisprudence and contemporary legal theorists outside the Hanbali school.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the Hanbali method's structural limitation on legal development and adaptation, which imposes costs on jurists seeking to address contemporary issues through rational inference. Suppression (0.75) is high due to the active intellectual and institutional pressure to conform to textualist interpretations, often framing alternative methodologies as 'innovation' (bid'a) to be resisted. The theater ratio (0.20) is low, as the commitment to textual fidelity is largely genuine, though some performative aspects may exist in the rhetoric against 'innovation'. Accessibility collapse (0.70) is high because alternative interpretive paths are significantly curtailed once this methodology is adopted. Resistance (0.40) is moderate, as other schools and rationalist jurists continuously challenge this restrictive approach.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of textualist scholars and conservative ulema (agenda-setters/beneficiaries), this methodology is a 'rope' that ensures the purity and authenticity of Islamic law, coordinating adherence to divine revelation. From the perspective of rationalist jurists and customary legal developers (payers/victims), it operates as a 'snare' or 'tangled rope', extracting their interpretive freedom and suppressing legal evolution, thereby hindering the law's responsiveness to societal needs.
 *
 * DIRECTIONALITY LOGIC:
 *   Textualist scholars and conservative ulema are beneficiaries (d=0.0-0.2) as their authority is reinforced by the textualist framework. Rationalist jurists, customary legal developers, and innovative muftis are targets (d=0.8-1.0) as their preferred methods are suppressed or de-prioritized. Lay Muslims are mixed (d=0.4-0.6), benefiting from perceived certainty but potentially losing flexibility.
 *
 * MANDATROPHY ANALYSIS:
 *   The Hanbali reading's mandate to preserve textual fidelity and block innovation remains 'live' for its adherents, preventing a clear mandatrophy resolution. However, the increasing extractiveness and suppression over time, coupled with the 'contested' status of the founding problem, suggests that while the original problem of 'speculative legal opinions' may have been addressed, the methodology has accumulated extractive features by limiting legitimate legal development. The classification as 'tangled_rope' reflects this hybrid nature: a genuine coordination function (textual consistency) coupled with asymmetric extraction (suppression of alternative legal reasoning).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_fidelity_vs_legal_flexibility,
    'Is the Hanbali reading''s maximal textual restrictiveness a necessary condition for preserving the authenticity of Islamic law, or does it unduly sacrifice legal flexibility and responsiveness to contemporary challenges?',
    'Comparative analysis of legal outcomes and societal impact in jurisdictions predominantly influenced by Hanbali vs. other schools, particularly regarding modern ethical and economic dilemmas. Longitudinal studies on the adaptability of legal systems.',
    'If necessary, the extraction is a legitimate cost of coordination (closer to Rope); if undue, it is an artifact of power consolidation (closer to Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_fidelity_vs_legal_flexibility, conceptual, 'The trade-off between textual fidelity and legal adaptability.').

omega_variable(
    suppression_of_ijtihad,
    'To what extent does the Hanbali methodology''s preference for weak hadith over qiyas genuinely preserve textual integrity versus merely suppressing independent juristic reasoning (ijtihad)?',
    'Historical analysis of specific legal cases where weak hadith was preferred, examining the alternative qiyas that were rejected and their potential societal benefits. Scholarly debate on the criteria for ''weak'' hadith and ''sound'' qiyas.',
    'If it primarily suppresses ijtihad, the suppression metric is more extractive than coordinative; if it genuinely preserves textual integrity, the suppression is a necessary boundary for the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_ijtihad, empirical, 'The true function of preferring weak hadith over qiyas.').

omega_variable(
    bid_a_definition_scope,
    'Is the Hanbali reading''s broad application of ''sadd al-dhara''i'' (blocking means to evil) to innovations a legitimate defense against theological deviation, or does it stifle beneficial legal and social development by over-categorizing as ''innovation''?',
    'Analysis of historical and contemporary instances where ''sadd al-dhara''i'' was invoked, assessing whether the ''evil'' prevented was genuine or whether the ''innovation'' blocked was beneficial. Cross-school comparison of bid''a definitions.',
    'If it stifles beneficial development, the constraint''s suppression is more arbitrary and extractive; if it genuinely prevents harm, the suppression is a necessary protective function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bid_a_definition_scope, preference, 'The scope and legitimacy of blocking innovations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t800, usul_al_fiqh_method__hanbali_reading, theater_ratio, 800, 0.1).
narrative_ontology:measurement(usul_tr_t1200, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1200, 0.15).
narrative_ontology:measurement(usul_tr_t1600, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1600, 0.18).
narrative_ontology:measurement(usul_tr_t2024, usul_al_fiqh_method__hanbali_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(usul_be_t800, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 800, 0.55).
narrative_ontology:measurement(usul_be_t1200, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1200, 0.6).
narrative_ontology:measurement(usul_be_t1600, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1600, 0.62).
narrative_ontology:measurement(usul_be_t2024, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t800, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 800, 0.65).
narrative_ontology:measurement(usul_su_t1200, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1200, 0.7).
narrative_ontology:measurement(usul_su_t1600, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1600, 0.72).
narrative_ontology:measurement(usul_su_t2024, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, identity_coordination).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__shafii_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'usul_al_fiqh_method' kernel, each representing a major Sunni school of law. The Hanbali reading emphasizes textual literalism and restricts rationalist legal development, contrasting with the more expansive approaches of other schools. Each reading is modeled as a separate constraint due to significant differences in their structural extractiveness and suppression of alternative methodologies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
