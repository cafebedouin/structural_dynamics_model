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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Hanafi Reading of Usul al-Fiqh Method (Qiyas, Ra'y, Istihsan)
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   This constraint describes the Hanafi school's methodology within Usul
 *   al-Fiqh (Islamic legal theory), characterized by its expansive
 *   application of analogical reasoning (qiyas), reliance on reasoned opinion
 *   (ra'y), and the principle of juristic preference (istihsan) to depart
 *   from strict analogy for public interest. It is one reading of the broader
 *   'usul_al_fiqh_method' kernel, which encompasses the diverse methodologies
 *   of the major Sunni legal schools. The Hanafi approach prioritizes
 *   adaptability and jurist discretion, leading to a classification as a
 *   Tangled Rope due to its genuine coordination function alongside
 *   asymmetric extraction from those who advocate for stricter textualism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.72).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.65).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi Reading of Usul al-Fiqh Method (Qiyas, Ra'y, Istihsan)").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, '6b125cd6-358a-4518-a3de-370b89892e62').
narrative_ontology:cs_kernel_codification('6b125cd6-358a-4518-a3de-370b89892e62', formalized).
narrative_ontology:cs_authority_grounding('6b125cd6-358a-4518-a3de-370b89892e62', lineage).
narrative_ontology:cs_interpretation_layer_present('6b125cd6-358a-4518-a3de-370b89892e62').
narrative_ontology:cs_reading_relation('6b125cd6-358a-4518-a3de-370b89892e62', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b125cd6-358a-4518-a3de-370b89892e62', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b125cd6-358a-4518-a3de-370b89892e62', usul_al_fiqh_method__hanbali_reading, forecloses).
narrative_ontology:cs_axiom('6b125cd6-358a-4518-a3de-370b89892e62', foundational, reasoned_opinion_as_source).
narrative_ontology:cs_axiom_status(reasoned_opinion_as_source, holdable).
narrative_ontology:cs_axiom_grounding('6b125cd6-358a-4518-a3de-370b89892e62', reasoned_opinion_as_source, conventional).
narrative_ontology:cs_axiom('6b125cd6-358a-4518-a3de-370b89892e62', foundational, istihsan_for_public_interest).
narrative_ontology:cs_axiom_status(istihsan_for_public_interest, holdable).
narrative_ontology:cs_axiom_grounding('6b125cd6-358a-4518-a3de-370b89892e62', istihsan_for_public_interest, instrumental).
narrative_ontology:cs_reference_frame('6b125cd6-358a-4518-a3de-370b89892e62', early_hanafi_rationalism).
narrative_ontology:cs_drift_state('6b125cd6-358a-4518-a3de-370b89892e62', contemporary_islamic_legal_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6b125cd6-358a-4518-a3de-370b89892e62', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, hanafi_jurists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, muslim_community_seeking_adaptability).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, strict_textualists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, hanbali_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The scholars and legal practitioners of the Hanafi school who apply and interpret Islamic law using expansive analogical reasoning (qiyas), reasoned opinion (ra'y), and juristic preference (istihsan). They benefit from the intellectual flexibility and authority this methodology grants them.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanafi_jurists, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, hanafi_jurists, beneficiary).

% Members of the Muslim community who benefit from the Hanafi school's ability to provide legal rulings for novel situations and adapt to changing social contexts, often through the application of istihsan for public interest. Their options are constrained by the available schools of thought.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, muslim_community_seeking_adaptability, beneficiary,
    moderate, biographical, constrained, global).

% Scholars and laypersons who advocate for a strict adherence to the literal text of the Quran and Sunnah, minimizing the role of human reason and analogy. They view the expansive Hanafi methodology as an innovation that deviates from foundational sources and bear the cost of its dominance in many regions.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, strict_textualists, payer,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, strict_textualists, excluded).

% The scholars and legal practitioners of the Hanbali school, known for their maximal textual restrictiveness and minimal use of qiyas and ra'y. They are structurally disadvantaged by the Hanafi school's expansive methodology, which they see as undermining the authority of primary texts.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, hanbali_scholars, payer,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanafi_reading, hanbali_scholars, excluded).

% Scholars of the Maliki school, who also employ non-textual sources like Medinan practice and maslaha (public interest), but with different methodologies and priorities than the Hanafi school. They observe the Hanafi method as a distinct, coexisting approach.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, maliki_scholars, observer,
    institutional, generational, analytical, global).

% Scholars of the Shafii school, who systematized usul al-fiqh and emphasized hadith authentication and a more restricted use of qiyas. They observe the Hanafi method as a distinct, coexisting approach within the broader framework of Islamic legal theory.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanafi_reading, shafii_scholars, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and adaptable framework for deriving Islamic legal rulings (fatwas) in diverse and evolving contexts, ensuring consistency within the Hanafi school while allowing for reasoned adaptation.
% TRANSFER_FUNCTION: Transfers interpretive authority and flexibility from strict textual sources to the trained jurist class, enabling the generation of new rulings that address contemporary issues, often at the expense of textual literalism.
% ABSENT_VOICES: Early textualist movements and later Salafi-inspired groups, who would argue for a return to strict textual adherence and reject the expansive use of ra'y and istihsan, are marginalized or excluded from the dominant Hanafi discourse.
% DISAPPEARANCE_RATIONALE: If the Hanafi methodology vanished, a major pillar of Islamic legal thought would collapse. Millions of Muslims rely on Hanafi jurisprudence, and its absence would create a massive vacuum, forcing a reorganization of legal systems and interpretive traditions across vast regions.
% FOUNDING_PROBLEM: The need to apply Islamic law to new situations and emerging societal challenges not explicitly addressed in the Quran or authenticated Sunnah, particularly as the early Muslim empire expanded into diverse cultures.
% FOUNDING_PROBLEM_CORROBORATION: Independent legal historians and contemporary muftis across various schools acknowledge the ongoing challenge of applying classical Islamic law to modern contexts, confirming the founding problem's continued relevance, even if they dispute the Hanafi solution.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanafi_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(usul_al_fiqh_method__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanafi_reading, 0.72, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.72) is high because the expansive scope for jurist discretion, while framed as serving public interest, can lead to interpretations that benefit the jurist class or maintain existing power structures, and it implicitly extracts from the authority of strict textual sources. Suppression (0.65) is moderate-high as the Hanafi school's dominance in many regions means alternative, more textualist methodologies are marginalized or actively resisted within its sphere of influence. Theater ratio (0.20) is low because the method is genuinely applied and forms the backbone of a living legal tradition, not merely a performance. Accessibility collapse (0.50) is moderate; while other schools offer alternatives, within the Hanafi tradition, this method is the primary lens. Resistance (0.55) is moderate, as other schools and textualist movements have historically and continue to challenge the Hanafi approach.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hanafi jurists, the method is a necessary and beneficial coordination mechanism for applying divine law to human affairs. From the perspective of strict textualists, it is an extractive innovation that dilutes the purity of the divine message. The engine's classification as Tangled Rope captures this divergence, recognizing both the coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanafi jurists are clear beneficiaries and agenda-setters, gaining authority and flexibility. The Muslim community seeking adaptability also benefits from practical rulings. Strict textualists and Hanbali scholars are victims/payers, as their preferred methods are suppressed or devalued by the Hanafi approach's dominance. Other schools (Maliki, Shafii) are observers, as their own methodologies coexist but are distinct.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately representing the ''hanafi_reading'' of the ''usul_al_fiqh_method'' kernel?',
    'Comparative analysis with primary sources of Hanafi jurisprudence and scholarly consensus on its distinguishing features.',
    'If misidentified, the entire analysis of inter-school relations and axiomatic conflicts would be flawed, leading to incorrect classification of this and sibling constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being instantiated from the kernel.').

omega_variable(
    textual_restrictiveness_ambiguity,
    'To what extent is the Hanafi reading''s expansive interpretation of sources genuinely derived from foundational texts versus a pragmatic adaptation?',
    'Detailed historical-critical analysis of early Hanafi legal derivations compared to contemporary interpretations, tracing the evolution of methodological principles.',
    'If primarily pragmatic adaptation, the ''conventional'' grounding of its axioms might shift towards ''instrumental'' or even ''extraction'', potentially increasing its computed extractiveness and reinforcing its Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_restrictiveness_ambiguity, empirical, 'Ambiguity regarding the textual grounding of expansive Hanafi methods.').

omega_variable(
    istihsan_public_interest_objectivity,
    'Is the ''public interest'' (maslaha) invoked by istihsan an objective, universally agreed-upon principle, or is it subject to the subjective interpretation of jurists, potentially serving specific interests?',
    'Comparative study of istihsan applications across different historical periods and regions, identifying consistent vs. divergent interpretations of ''public interest'' and their beneficiaries.',
    'If ''public interest'' is consistently subjective, the extractiveness of the constraint would be higher, as it would represent a more direct transfer of authority to jurists without clear external accountability, pushing it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_public_interest_objectivity, conceptual, 'Objectivity of ''public interest'' in juristic preference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanafi_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(usul_tr_t20, usul_al_fiqh_method__hanafi_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__hanafi_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(usul_tr_t60, usul_al_fiqh_method__hanafi_reading, theater_ratio, 60, 0.17).
narrative_ontology:measurement(usul_tr_t80, usul_al_fiqh_method__hanafi_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement(usul_tr_t100, usul_al_fiqh_method__hanafi_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(usul_be_t20, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(usul_be_t60, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(usul_be_t80, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(usul_be_t100, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(usul_su_t20, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(usul_su_t60, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(usul_su_t80, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 80, 0.63).
narrative_ontology:measurement(usul_su_t100, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, identity_coordination).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, islamic_legal_rulings_validity).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, islamic_finance_principles).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'usul_al_fiqh_method' kernel, each representing a major Sunni legal school's methodology. Each reading is a separate constraint story, linked here to reflect their shared domain and contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
