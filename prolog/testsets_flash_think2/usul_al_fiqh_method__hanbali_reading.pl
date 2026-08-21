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
 *   constraint_id: usul_al_fiqh_method__hanbali_reading
 *   human_readable: Hanbali School's Jurisprudential Method
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   This constraint describes the Hanbali school's jurisprudential method
 *   (usul al-fiqh), which is one reading of a contested kernel concerning the
 *   sources and methodology of Islamic law. It emphasizes maximal
 *   restrictiveness to textual sources (Quran and authenticated Hadith),
 *   minimizes the use of qiyas (analogical reasoning), prefers weak hadith
 *   over qiyas, and actively employs sadd al-dhara'i (blocking the means to
 *   innovation) to preserve textual fidelity. The method is claimed as a
 *   means of ensuring purity and adherence to revelation, but its strictures
 *   are experienced as extractive by those advocating for rationalist legal
 *   development or customary law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.65).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.75).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali School's Jurisprudential Method").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, '3eb24bc2-d846-4fb3-a634-c7b9ace3fbd3').
narrative_ontology:cs_kernel_codification('3eb24bc2-d846-4fb3-a634-c7b9ace3fbd3', formalized).
narrative_ontology:cs_authority_grounding('3eb24bc2-d846-4fb3-a634-c7b9ace3fbd3', lineage).
narrative_ontology:cs_interpretation_layer_present('3eb24bc2-d846-4fb3-a634-c7b9ace3fbd3').
narrative_ontology:cs_reading_relation('3eb24bc2-d846-4fb3-a634-c7b9ace3fbd3', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('3eb24bc2-d846-4fb3-a634-c7b9ace3fbd3', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('3eb24bc2-d846-4fb3-a634-c7b9ace3fbd3', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('3eb24bc2-d846-4fb3-a634-c7b9ace3fbd3', foundational, textual_primacy_over_reason).
narrative_ontology:cs_axiom_status(textual_primacy_over_reason, holdable).
narrative_ontology:cs_axiom_grounding('3eb24bc2-d846-4fb3-a634-c7b9ace3fbd3', textual_primacy_over_reason, deontological).
narrative_ontology:cs_axiom('3eb24bc2-d846-4fb3-a634-c7b9ace3fbd3', foundational, blocking_means_to_innovation).
narrative_ontology:cs_axiom_status(blocking_means_to_innovation, holdable).
narrative_ontology:cs_axiom_grounding('3eb24bc2-d846-4fb3-a634-c7b9ace3fbd3', blocking_means_to_innovation, conventional).
narrative_ontology:cs_reference_frame('3eb24bc2-d846-4fb3-a634-c7b9ace3fbd3', early_textualist_purity).
narrative_ontology:cs_drift_state('3eb24bc2-d846-4fb3-a634-c7b9ace3fbd3', contemporary_legal_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3eb24bc2-d846-4fb3-a634-c7b9ace3fbd3', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, hanbali_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, textualist_adherents).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, conservative_legal_tradition).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_law_advocates).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, legal_innovators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define, interpret, and propagate the Hanbali jurisprudential method, benefiting from its authority and the intellectual tradition it represents. They actively enforce its principles through teaching, fatwas, and judicial rulings.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hanbali_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Find certainty, purity, and a sense of fidelity to foundational sources in the strict textual adherence of the Hanbali method. They benefit from the clear boundaries it sets against perceived innovations.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, textualist_adherents, beneficiary,
    organized, biographical, identity_locked, global).

% Benefits from the Hanbali method's emphasis on textual fidelity as a bulwark against rapid legal change and adaptation, reinforcing established norms and interpretations.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, conservative_legal_tradition, beneficiary,
    institutional, generational, constrained, global).

% Jurists from other schools (e.g., Hanafi) who advocate for more expansive use of qiyas (analogical reasoning) or ra'y (reasoned opinion) find their methods minimized or rejected by the Hanbali approach, limiting their interpretive scope and influence.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_jurists, payer,
    powerful, biographical, constrained, global).

% Those who rely on local custom ('urf) or unrestricted public interest (maslaha mursala) as sources of law find their arguments largely excluded or severely restricted by the Hanbali method's textual primacy.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, customary_law_advocates, payer,
    moderate, biographical, constrained, global).

% Scholars and practitioners seeking to adapt Islamic law to modern social, economic, or political contexts find their efforts constrained by the Hanbali method's strict textualism and emphasis on blocking innovations (sadd al-dhara'i).
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, legal_innovators, payer,
    powerless, biographical, constrained, global).

% Represent an alternative, more expansive interpretive tradition that prioritizes qiyas, ra'y, and istihsan. They are structurally excluded from the Hanbali method's internal logic and its claims to interpretive authority.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hanafi_scholars, excluded,
    institutional, generational, constrained, global).

% Represent an alternative tradition that gives weight to Medinan practice and maslaha mursala. Their methods are largely excluded by the Hanbali framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, maliki_scholars, excluded,
    institutional, generational, constrained, global).

% Represent an alternative tradition that systematized usul al-fiqh but with different priorities for hadith authentication and qiyas application. Their approach is distinct and often in tension with the Hanbali method.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, shafii_scholars, excluded,
    institutional, generational, constrained, global).

% Analyze the structural implications and historical development of different Islamic jurisprudential methods, including the Hanbali school, from an external, academic perspective.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, comparative_law_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanbali_reading, hanbali_scholars).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, consistent, and highly text-centric methodology for deriving Islamic law, ensuring fidelity to foundational sources (Quran and authenticated Hadith) and actively limiting perceived innovations (bid'a) in legal interpretation.
% TRANSFER_FUNCTION: Transfers interpretive authority from human reason, local custom, and expansive analogical reasoning to the direct textual sources, thereby concentrating interpretive power within the textualist scholarly tradition and its established methodology.
% ABSENT_VOICES: Jurists from other schools (Hanafi, Maliki, Shafii) who advocate for more expansive use of qiyas, istihsan, maslaha mursala, or 'urf are structurally excluded from the Hanbali method's internal discourse. They would argue for greater flexibility and adaptation in legal development.
% DISAPPEARANCE_RATIONALE: If the Hanbali method and its enforcement vanished overnight, the landscape of Islamic jurisprudence would fundamentally shift. There would likely be a significant increase in the application of analogical reasoning, juristic preference, and customary law, potentially accelerating legal development and adaptation in many Muslim-majority societies, and altering the balance of power among jurisprudential schools.
% FOUNDING_PROBLEM: To preserve the purity and integrity of Islamic law from perceived innovations (bid'a) and speculative reasoning, ensuring strict adherence to the foundational textual sources (Quran and Sunnah) as understood by early generations.
% FOUNDING_PROBLEM_CORROBORATION: Hanbali scholars and conservative religious institutions attest to the ongoing threat of bid'a and the need for strict textual fidelity. However, jurists from other schools and reformist thinkers contest this, arguing that the founding problem is either exaggerated or that the method itself hinders necessary legal evolution and adaptation to contemporary challenges.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanbali_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The Hanbali method's extractiveness (0.65) stems from its active suppression of alternative interpretive tools, which limits the scope for legal development and adaptation outside of strict textualism. Suppression (0.75) is high because the method requires continuous scholarly and judicial enforcement to maintain its interpretive boundaries against other schools and reformist tendencies. The theater ratio (0.15) is low, indicating that the method is genuinely applied and not merely performative; its principles are actively used in legal derivation and adjudication. Accessibility collapse is high (0.8) for alternative methods, as their scope is actively minimized. Resistance is moderate (0.5) due to ongoing scholarly debates and the existence of other prominent schools with different methodologies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hanbali scholars and textualist adherents, this method is a necessary and righteous path to preserving the purity of Islamic law, functioning as a 'rope' or even 'mountain' of divine guidance. However, from the perspective of rationalist jurists or legal innovators, the same structure operates as a 'snare' or 'tangled rope,' actively extracting flexibility and suppressing alternative approaches to legal development.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanbali scholars and textualist adherents are the primary beneficiaries, gaining interpretive authority and certainty from the method's strict adherence to foundational texts. Rationalist jurists, customary law advocates, and legal innovators are the targets, as their preferred methods and desired outcomes are suppressed or excluded. The method coordinates a specific form of textual fidelity but extracts from alternative forms of legal reasoning and development.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_textual_primacy,
    'Is the Hanbali method''s textual primacy an inherent, divinely mandated feature of Islamic revelation (a ''natural law'' of interpretation) or a constructed jurisprudential choice among several valid approaches?',
    'Comparative theological and historical analysis of early Islamic legal thought, examining the diversity of interpretive approaches among the Companions and early jurists, and the arguments for and against the ''naturalness'' of strict textualism.',
    'If deemed a constructed choice, the constraint''s ''mountain'' claims (from its internal perspective) would be reclassified, highlighting its role in shaping, rather than merely reflecting, legal reality. If genuinely natural, its extractiveness would be re-evaluated as an inherent cost of fidelity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_textual_primacy, conceptual, 'Ambiguity regarding the inherent vs. constructed nature of the Hanbali method''s textual primacy.').

omega_variable(
    impact_on_legal_development,
    'Does the Hanbali method genuinely preserve the purity of Islamic law and prevent harmful innovations, or does it impede necessary legal adaptation and innovation in contemporary contexts, leading to stagnation or irrelevance?',
    'Empirical study of legal outcomes in societies where the Hanbali method is dominant versus those influenced by more flexible schools, assessing the capacity for legal reform, social justice, and responsiveness to modern challenges.',
    'If found to impede necessary adaptation, the constraint''s effective extractiveness on legal innovators would be amplified, and its coordination function (preserving purity) would be re-evaluated against its practical consequences. If found to genuinely preserve purity without undue stagnation, its extractiveness would be seen as a necessary cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_legal_development, empirical, 'Ambiguity regarding the long-term impact of the Hanbali method on legal development and societal well-being.').

omega_variable(
    internalized_suppression_of_alternatives,
    'To what extent is the suppression of alternative interpretive methods (qiyas, ra''y, istihsan) internalized by jurists within the Hanbali tradition, making them less likely to even conceive of or advocate for such alternatives?',
    'Qualitative research among Hanbali scholars, exploring their epistemic frameworks, training, and attitudes towards non-textualist sources, potentially using counterfactual scenarios to gauge the degree of internalized constraint.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than structural measures suggest, as the ''victims'' (rationalist jurists, innovators) within the tradition carry the suppression with them, even in the absence of overt external enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_of_alternatives, empirical, 'Structural vs. internalized suppression of alternative interpretive methods within the Hanbali school.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 850, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t850, usul_al_fiqh_method__hanbali_reading, theater_ratio, 850, 0.1).
narrative_ontology:measurement(usul_tr_t1100, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1100, 0.12).
narrative_ontology:measurement(usul_tr_t1350, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1350, 0.13).
narrative_ontology:measurement(usul_tr_t1600, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1600, 0.14).
narrative_ontology:measurement(usul_tr_t1850, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(usul_tr_t2020, usul_al_fiqh_method__hanbali_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(usul_be_t850, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 850, 0.5).
narrative_ontology:measurement(usul_be_t1100, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1100, 0.55).
narrative_ontology:measurement(usul_be_t1350, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1350, 0.6).
narrative_ontology:measurement(usul_be_t1600, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1600, 0.63).
narrative_ontology:measurement(usul_be_t1850, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1850, 0.64).
narrative_ontology:measurement(usul_be_t2020, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t850, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 850, 0.6).
narrative_ontology:measurement(usul_su_t1100, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1100, 0.65).
narrative_ontology:measurement(usul_su_t1350, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1350, 0.7).
narrative_ontology:measurement(usul_su_t1600, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1600, 0.73).
narrative_ontology:measurement(usul_su_t1850, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1850, 0.74).
narrative_ontology:measurement(usul_su_t2020, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__shafii_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'usul_al_fiqh_method' kernel, representing the Hanbali school's approach to legal derivation, which emphasizes strict textual adherence and minimizes rationalist methods. It is part of a family of constraints describing the different jurisprudential schools in Islamic law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
