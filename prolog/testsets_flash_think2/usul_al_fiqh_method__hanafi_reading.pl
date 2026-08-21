% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanafi_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Hanafi School's Expansive Legal Methodology (Usul al-Fiqh)
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   This constraint describes the Hanafi school's methodology within Usul
 *   al-Fiqh (Islamic legal theory), emphasizing expansive analogical
 *   reasoning (qiyas), reasoned opinion (ra'y), and juristic preference for
 *   public interest (istihsan) when textual sources are silent or ambiguous.
 *   It is one reading of the broader 'usul_al_fiqh_method' kernel, which
 *   encompasses the diverse methodologies of the four Sunni schools of law.
 *   The Hanafi reading empowers a class of rationalist jurists to adapt law,
 *   but in doing so, it implicitly suppresses strict textualist
 *   interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.45).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.6).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi School's Expansive Legal Methodology (Usul al-Fiqh)").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, 'a28fb2cd-6d28-4cb3-8d28-419d0bbb5228').
narrative_ontology:cs_kernel_codification('a28fb2cd-6d28-4cb3-8d28-419d0bbb5228', formalized).
narrative_ontology:cs_authority_grounding('a28fb2cd-6d28-4cb3-8d28-419d0bbb5228', lineage).
narrative_ontology:cs_interpretation_layer_present('a28fb2cd-6d28-4cb3-8d28-419d0bbb5228').
narrative_ontology:cs_reading_relation('a28fb2cd-6d28-4cb3-8d28-419d0bbb5228', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('a28fb2cd-6d28-4cb3-8d28-419d0bbb5228', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('a28fb2cd-6d28-4cb3-8d28-419d0bbb5228', usul_al_fiqh_method__hanbali_reading, forecloses).
narrative_ontology:cs_axiom('a28fb2cd-6d28-4cb3-8d28-419d0bbb5228', foundational, rational_derivation_valid_beyond_text).
narrative_ontology:cs_axiom_status(rational_derivation_valid_beyond_text, holdable).
narrative_ontology:cs_axiom_grounding('a28fb2cd-6d28-4cb3-8d28-419d0bbb5228', rational_derivation_valid_beyond_text, conventional).
narrative_ontology:cs_axiom('a28fb2cd-6d28-4cb3-8d28-419d0bbb5228', foundational, public_interest_trumps_strict_analogy).
narrative_ontology:cs_axiom_status(public_interest_trumps_strict_analogy, holdable).
narrative_ontology:cs_axiom_grounding('a28fb2cd-6d28-4cb3-8d28-419d0bbb5228', public_interest_trumps_strict_analogy, instrumental).
narrative_ontology:cs_reference_frame('a28fb2cd-6d28-4cb3-8d28-419d0bbb5228', rationalist_legal_adaptation).
narrative_ontology:cs_drift_state('a28fb2cd-6d28-4cb3-8d28-419d0bbb5228', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a28fb2cd-6d28-4cb3-8d28-419d0bbb5228', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, hanafi_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, legal_innovation_advocates).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, public_interest_advocates).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, strict_textualist_claims).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, lay_adherents_seeking_simplicity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and enforcers of the Hanafi methodology. They benefit from the expansive scope for rationalist legal derivation (ra'y, istihsan) and the authority it grants them in applying Islamic law to novel cases. Their professional identity is deeply tied to this interpretive framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanafi_jurists, agenda_setter,
    institutional, generational, identity_locked, global).

% Scholars and practitioners who seek to adapt Islamic law to contemporary challenges. They benefit from the Hanafi methodology's flexibility and its emphasis on reasoned opinion and juristic preference for public interest, which allows for legal evolution.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, legal_innovation_advocates, beneficiary,
    organized, biographical, constrained, regional).

% Groups and individuals who champion legal outcomes that serve the broader welfare of the community (maslaha). They find the Hanafi school's principle of istihsan (juristic preference) a valuable tool for achieving equitable and socially beneficial rulings, even if it means departing from strict analogy.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, public_interest_advocates, beneficiary,
    organized, biographical, mobile, local).

% The methodological claims of those who prioritize strict adherence to the literal text of the Quran and Hadith, minimizing the role of human reason or expansive analogy. The Hanafi methodology's broad application of ra'y and istihsan effectively 'extracts' interpretive authority from these textualist claims, limiting their scope and influence within the Hanafi framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, strict_textualist_claims, payer,
    moderate, generational, constrained, global).

% Ordinary Muslims who prefer clear, straightforward legal rulings directly derived from foundational texts, without complex juristic reasoning or nuanced preferences. The expansive Hanafi methodology can be perceived as introducing complexity and uncertainty, making legal understanding less accessible and requiring reliance on specialized jurists.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, lay_adherents_seeking_simplicity, payer,
    powerless, immediate, constrained, local).

% Academics who study different schools of Islamic law and their methodologies. They analyze the structural implications of the Hanafi approach without being bound by its internal commitments or directly benefiting/paying from its operation.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, comparative_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanafi_reading, hanafi_jurists).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a comprehensive and adaptable framework for deriving Islamic legal rulings (fatawa) in diverse and evolving contexts, ensuring consistency and legitimacy across a wide range of cases not explicitly covered by foundational texts.
% TRANSFER_FUNCTION: Transfers significant interpretive authority and the power to shape legal outcomes from strict textual literalism to the class of trained Hanafi jurists, particularly those skilled in rationalist legal reasoning and discerning public interest.
% ABSENT_VOICES: Strict textualist scholars and movements, particularly those aligned with the Hanbali school, would object vehemently to the expansive application of qiyas, ra'y, and istihsan, arguing that it constitutes unwarranted innovation (bid'ah) and deviates from the pristine sources of Islamic law. They are structurally excluded from shaping the Hanafi methodology itself.
% DISAPPEARANCE_RATIONALE: The Hanafi school is one of the oldest and most widely followed schools of Islamic jurisprudence, forming the basis of legal systems and personal status laws in numerous countries. Its disappearance would lead to a profound crisis in legal authority, requiring a complete re-evaluation of countless rulings, a restructuring of legal education, and a re-founding of judicial practice across vast regions.
% FOUNDING_PROBLEM: To develop a systematic methodology for applying Islamic law to novel situations and evolving societal needs that were not directly or explicitly addressed by the Quran or the Sunnah (Prophetic tradition), while ensuring the rulings remained consistent with the broader spirit and objectives of the Sharia.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians, sociologists of religion, and comparative law scholars, from outside the direct beneficiaries, corroborate the historical and ongoing need for interpretive methodologies to address legal gaps and societal change within Islamic legal traditions. The problem of applying ancient texts to modern realities remains a live challenge across all schools.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanafi_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).
:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates legal reasoning and provides a robust framework for legal development (benefiting Hanafi jurists and advocates for legal innovation). However, it simultaneously involves asymmetric extraction by limiting the interpretive authority of strict textualist claims and potentially imposing complex legal reasoning on lay adherents. Active enforcement is required to maintain the authority of Hanafi jurists and their interpretive methods against competing methodologies. Extractiveness is moderate-low, reflecting that while it empowers a class, it's primarily a functional legal system, not pure rent-seeking. Suppression is moderate, as it actively defends its methodological space against more restrictive approaches.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hanafi jurists, this methodology is a necessary and beneficial coordination mechanism for a dynamic legal tradition. From the perspective of strict textualists, it represents an overreach of human reason and a deviation from divine command. The engine's classification as Tangled Rope captures this inherent tension between coordination and extraction, which is central to the inter-school debates within Islamic jurisprudence.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanafi jurists are clear beneficiaries and agenda-setters, as the methodology grants them significant interpretive authority. Advocates for legal innovation and public interest also benefit from the flexibility. Strict textualist claims are the primary 'victims' as their interpretive scope is curtailed. Lay adherents seeking simplicity may also experience a form of extraction through increased legal complexity. Comparative law scholars serve as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationalism_vs_textualism_legitimacy,
    'Is the expansive scope for jurist-driven rationalist derivation (ra''y, istihsan) a legitimate and necessary adaptation of Islamic law, or an unwarranted innovation that compromises textual fidelity?',
    'Theological and jurisprudential consensus formation over centuries, or a decisive shift in the perceived authority of human reason versus divine revelation within the broader Muslim scholarly community.',
    'If deemed unwarranted, the constraint''s legitimacy would collapse, and its extractiveness from textualist claims would be reclassified as pure suppression. If universally accepted as legitimate, its coordination function would be amplified, and extractiveness would diminish.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rationalism_vs_textualism_legitimacy, conceptual, 'The fundamental conceptual dispute over the role of human reason in Islamic legal derivation.').

omega_variable(
    istihsan_scope_ambiguity,
    'What are the precise limits and conditions for applying istihsan (juristic preference for public interest) to depart from strict analogy, and are these limits consistently applied?',
    'Detailed case studies of Hanafi legal rulings over time, analyzing the explicit justifications for istihsan and comparing them against stated methodological principles. This would require extensive textual analysis of fatwas and legal commentaries.',
    'If istihsan is found to be applied arbitrarily or without clear, consistent limits, the constraint''s theater_ratio would increase, and its coordination function would be undermined by perceived capriciousness. If consistently applied, its legitimacy as a coordination mechanism would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_scope_ambiguity, empirical, 'The empirical consistency and defined limits of juristic preference in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanafi_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(usul_tr_t200, usul_al_fiqh_method__hanafi_reading, theater_ratio, 200, 0.07).
narrative_ontology:measurement(usul_tr_t400, usul_al_fiqh_method__hanafi_reading, theater_ratio, 400, 0.08).
narrative_ontology:measurement(usul_tr_t600, usul_al_fiqh_method__hanafi_reading, theater_ratio, 600, 0.09).
narrative_ontology:measurement(usul_tr_t800, usul_al_fiqh_method__hanafi_reading, theater_ratio, 800, 0.1).
narrative_ontology:measurement(usul_tr_t1000, usul_al_fiqh_method__hanafi_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(usul_tr_t1200, usul_al_fiqh_method__hanafi_reading, theater_ratio, 1200, 0.1).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(usul_be_t200, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 200, 0.38).
narrative_ontology:measurement(usul_be_t400, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 400, 0.4).
narrative_ontology:measurement(usul_be_t600, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 600, 0.42).
narrative_ontology:measurement(usul_be_t800, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 800, 0.43).
narrative_ontology:measurement(usul_be_t1000, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 1000, 0.44).
narrative_ontology:measurement(usul_be_t1200, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 1200, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(usul_su_t200, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 200, 0.5).
narrative_ontology:measurement(usul_su_t400, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 400, 0.55).
narrative_ontology:measurement(usul_su_t600, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 600, 0.58).
narrative_ontology:measurement(usul_su_t800, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 800, 0.59).
narrative_ontology:measurement(usul_su_t1000, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 1000, 0.6).
narrative_ontology:measurement(usul_su_t1200, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 1200, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, identity_coordination).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'usul_al_fiqh_method' kernel, each representing a major Sunni school of Islamic law. Each reading has a unique set of beneficiaries, victims, and structural properties, necessitating separate constraint stories linked by this network relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
