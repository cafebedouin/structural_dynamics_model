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
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Reasoned Extension of Divine Law
 *   domain: islamic_jurisprudence/legal_philosophy/institutional_history
 *
 * SUMMARY:
 *   This constraint describes the Hanafi school's jurisprudential method,
 *   which emphasizes analogical reasoning (qiyas) and juristic preference
 *   (istihsan) as legitimate tools for extending divine intent to novel
 *   cases, alongside the Qur'an and Hadith. It is one reading of the broader
 *   'jurisprudential_method_kernel' in Islamic law. The method provides a
 *   robust framework for legal adaptation but also concentrates interpretive
 *   authority among jurists skilled in rationalist deduction, leading to a
 *   high degree of extraction from those who prefer simpler, textualist
 *   approaches.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.68).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.55).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Jurisprudential Method: Reasoned Extension of Divine Law").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "islamic_jurisprudence/legal_philosophy/institutional_history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, 'cf2d4950-ef22-4cfe-9978-c3c9c53727bf').
narrative_ontology:cs_kernel_codification('cf2d4950-ef22-4cfe-9978-c3c9c53727bf', formalized).
narrative_ontology:cs_authority_grounding('cf2d4950-ef22-4cfe-9978-c3c9c53727bf', lineage).
narrative_ontology:cs_interpretation_layer_present('cf2d4950-ef22-4cfe-9978-c3c9c53727bf').
narrative_ontology:cs_reading_relation('cf2d4950-ef22-4cfe-9978-c3c9c53727bf', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf2d4950-ef22-4cfe-9978-c3c9c53727bf', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf2d4950-ef22-4cfe-9978-c3c9c53727bf', jurisprudential_method_kernel__hanbali_reading, forecloses).
narrative_ontology:cs_axiom('cf2d4950-ef22-4cfe-9978-c3c9c53727bf', foundational, reason_extends_divine_intent).
narrative_ontology:cs_axiom_status(reason_extends_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('cf2d4950-ef22-4cfe-9978-c3c9c53727bf', reason_extends_divine_intent, deontological).
narrative_ontology:cs_axiom('cf2d4950-ef22-4cfe-9978-c3c9c53727bf', foundational, juristic_preference_is_valid).
narrative_ontology:cs_axiom_status(juristic_preference_is_valid, holdable).
narrative_ontology:cs_axiom_grounding('cf2d4950-ef22-4cfe-9978-c3c9c53727bf', juristic_preference_is_valid, conventional).
narrative_ontology:cs_reference_frame('cf2d4950-ef22-4cfe-9978-c3c9c53727bf', rationalist_legal_adaptation).
narrative_ontology:cs_drift_state('cf2d4950-ef22-4cfe-9978-c3c9c53727bf', contemporary_islamic_revivalism, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('cf2d4950-ef22-4cfe-9978-c3c9c53727bf', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, rationalist_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, communities_with_novel_cases).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textualist_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, lay_practitioners_seeking_simple_answers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These jurists interpret and apply divine law using extensive analogical reasoning (qiyas) and juristic preference (istihsan). They benefit from the intellectual authority and career paths this methodology creates, allowing them to address novel legal issues not explicitly covered in foundational texts. Their identity is deeply fused with this rationalist approach.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists, agenda_setter,
    institutional, generational, identity_locked, global).

% Scholars who prioritize reason and systematic legal theory find their approach validated and empowered by the Hanafi method. They benefit from the intellectual space to develop complex legal arguments and contribute to a dynamic legal tradition, but are constrained by the need to remain within the broader Hanafi framework.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, rationalist_scholars, beneficiary,
    organized, biographical, constrained, global).

% Scholars from other schools (e.g., Hanbali) who emphasize strict adherence to literal texts and reject extensive analogical reasoning or juristic preference. They bear the cost of having their interpretive authority challenged and their methodologies deemed less comprehensive for novel cases, leading to a loss of influence in certain legal domains.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, textualist_scholars, payer,
    powerful, generational, constrained, global).

% Individuals seeking clear, unambiguous legal rulings for everyday life. They may find the complex, reasoned extensions of the Hanafi method difficult to understand or apply, preferring direct textual guidance. They are 'trapped' by the need to follow established legal opinions without the capacity to engage in the intricate reasoning themselves.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, lay_practitioners_seeking_simple_answers, payer,
    powerless, immediate, trapped, local).

% Communities facing new social, economic, or technological challenges for which direct textual rulings are unavailable. They benefit from the Hanafi method's capacity to provide reasoned legal solutions, allowing Islamic law to remain relevant and applicable in evolving contexts. They can 'shop' for rulings from different schools if the Hanafi one is unworkable, but often prefer the intellectual rigor.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, communities_with_novel_cases, beneficiary,
    moderate, biographical, mobile, regional).

% Jurists from the Maliki school, who prioritize the living tradition of Medina ('amal ahl al-Madina) as a source of law. While they also use analogical reasoning, their emphasis on Medinan practice means they would object to the Hanafi method's broader application of istihsan and qiyas without that specific historical grounding.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, maliki_jurists, excluded,
    institutional, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a systematic and adaptable framework for deriving legal rulings from divine sources, ensuring consistency and coherence across diverse and evolving contexts, particularly for novel cases not explicitly covered in the Qur'an or Hadith.
% TRANSFER_FUNCTION: Transfers interpretive authority and intellectual prestige to jurists trained in rationalist methodologies, enabling them to extend divine intent to new situations. It transfers the burden of complex reasoning from lay practitioners to scholars, while also transferring influence away from purely textualist approaches.
% ABSENT_VOICES: Strict textualist and traditionalist scholars (e.g., Hanbalis) are structurally marginalized in contexts where the Hanafi method is dominant; they would argue for a more literal and less interpretive approach, fearing innovation (bid'ah). Maliki jurists, with their emphasis on Medinan practice, would also offer a different, historically grounded approach to legal extension.
% DISAPPEARANCE_RATIONALE: If the Hanafi jurisprudential method vanished, the vast body of Islamic law derived through qiyas and istihsan would lose its methodological grounding. Legal systems in regions historically dominated by Hanafi thought would face a crisis of legitimacy and applicability, forcing a radical reorganization around alternative, likely more restrictive, interpretive frameworks. The ability of Islamic law to adapt to modernity would be severely hampered.
% FOUNDING_PROBLEM: The early Islamic community faced the challenge of applying divine revelation (Qur'an and Hadith) to an expanding empire with diverse customs and novel situations not directly addressed by the texts. A method was needed to extend divine intent systematically and rationally.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Islamic law and contemporary legal scholars (including those outside the Hanafi school, such as some modern Shafi'is) corroborate that the problem of applying divine law to novel cases remains live. The need for reasoned extension is widely acknowledged, even if the specific methods are debated. The Hanafi school's historical success in diverse contexts attests to its efficacy in addressing this problem.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanafi_reading, 'none', 1).

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
 *   The extractiveness (0.68) is high because the method's complexity and reliance on juristic discretion create a barrier to entry for non-specialists and marginalize purely textualist interpretations, effectively 'extracting' interpretive authority. Suppression (0.55) is moderate, as alternative schools exist, but the Hanafi method's institutional dominance in many regions means its interpretive framework is actively enforced through legal systems and educational institutions. Theater ratio (0.20) is low; the rationalist arguments are genuinely functional, but some performative defense of istihsan against charges of innovation exists.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hanafi jurists, the method is a necessary and sophisticated coordination mechanism for a living legal tradition. From the perspective of textualist scholars, it is an extractive mechanism that elevates human reason over divine text and suppresses alternative, more literal interpretations. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanafi jurists and rationalist scholars are clear beneficiaries, as the method empowers their intellectual work and institutional roles. Textualist scholars and lay practitioners seeking simple answers are victims, as their preferred modes of engagement with divine law are de-emphasized or made inaccessible. Communities with novel cases are beneficiaries, as they receive adaptable legal solutions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rationalism_vs_textualism_legitimacy,
    'Is the Hanafi method''s extensive use of qiyas and istihsan a legitimate extension of divine intent, or an unwarranted innovation (bid''ah) that corrupts the kernel?',
    'Historical analysis of early Islamic legal discourse and theological arguments regarding the scope of human reason in divine law. Consensus among diverse, independent scholarly bodies.',
    'If deemed an unwarranted innovation, the constraint''s legitimacy would collapse, and its extractiveness would be reclassified as pure extraction (snare). If affirmed as legitimate, its coordination function would be strengthened, potentially lowering its effective extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rationalism_vs_textualism_legitimacy, conceptual, 'Legitimacy of rationalist legal extension in Islamic jurisprudence.').

omega_variable(
    interpretive_authority_concentration,
    'Does the complexity of the Hanafi method inherently concentrate interpretive authority in a specialized elite, or could its rationalist tools be democratized?',
    'Empirical study of legal education and access to interpretive tools across different Islamic legal traditions. Counterfactual analysis of simplified Hanafi curricula.',
    'If concentration is inherent, the method''s extractiveness is a structural feature. If democratizable, the current high extractiveness is a contingent outcome of institutional choices, suggesting potential for reform to lower extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_concentration, empirical, 'Structural vs. contingent concentration of interpretive authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(juri_tr_t350, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 350, 0.15).
narrative_ontology:measurement(juri_tr_t700, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 700, 0.18).
narrative_ontology:measurement(juri_tr_t1050, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1050, 0.2).
narrative_ontology:measurement(juri_tr_t1400, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1400, 0.2).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(juri_be_t350, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 350, 0.55).
narrative_ontology:measurement(juri_be_t700, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 700, 0.62).
narrative_ontology:measurement(juri_be_t1050, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1050, 0.65).
narrative_ontology:measurement(juri_be_t1400, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1400, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(juri_su_t350, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 350, 0.4).
narrative_ontology:measurement(juri_su_t700, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 700, 0.48).
narrative_ontology:measurement(juri_su_t1050, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1050, 0.52).
narrative_ontology:measurement(juri_su_t1400, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1400, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jurisprudential_method_kernel', focusing on the Hanafi school's emphasis on qiyas and istihsan. Other readings (Maliki, Shafii, Hanbali) represent distinct methodological approaches to deriving Islamic law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
