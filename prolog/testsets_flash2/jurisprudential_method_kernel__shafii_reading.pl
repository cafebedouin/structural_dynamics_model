% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__shafii_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__shafii_reading
 *   human_readable: Al-Shafi'i's Jurisprudential Method (Hadith-Centric Reading)
 *   domain: islamic_jurisprudence/legal_philosophy/institutional_history
 *
 * SUMMARY:
 *   This constraint describes al-Shafi'i's methodological standardization of
 *   Islamic jurisprudence, which established a strict four-tier hierarchy of
 *   legal sources: Qur'an, Hadith, Ijma (consensus), and Qiyas (analogical
 *   reasoning). This reading emphasizes the elevation of Hadith
 *   authentication as the primary arbiter, resolving earlier schools'
 *   inconsistencies. It is one reading of the broader
 *   'jurisprudential_method_kernel' which encompasses the diverse approaches
 *   of the major Sunni legal schools.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, 0.65).
domain_priors:suppression_score(jurisprudential_method_kernel__shafii_reading, 0.7).
domain_priors:theater_ratio(jurisprudential_method_kernel__shafii_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__shafii_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__shafii_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__shafii_reading, "Al-Shafi'i's Jurisprudential Method (Hadith-Centric Reading)").
narrative_ontology:topic_domain(jurisprudential_method_kernel__shafii_reading, "islamic_jurisprudence/legal_philosophy/institutional_history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__shafii_reading, 'f6ea0d46-0de6-4b69-9fa3-daf0e1538faf').
narrative_ontology:cs_kernel_codification('f6ea0d46-0de6-4b69-9fa3-daf0e1538faf', formalized).
narrative_ontology:cs_authority_grounding('f6ea0d46-0de6-4b69-9fa3-daf0e1538faf', lineage).
narrative_ontology:cs_interpretation_layer_present('f6ea0d46-0de6-4b69-9fa3-daf0e1538faf').
narrative_ontology:cs_reading_relation('f6ea0d46-0de6-4b69-9fa3-daf0e1538faf', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6ea0d46-0de6-4b69-9fa3-daf0e1538faf', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6ea0d46-0de6-4b69-9fa3-daf0e1538faf', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('f6ea0d46-0de6-4b69-9fa3-daf0e1538faf', foundational, hadith_as_second_source_of_law).
narrative_ontology:cs_axiom_status(hadith_as_second_source_of_law, holdable).
narrative_ontology:cs_axiom_grounding('f6ea0d46-0de6-4b69-9fa3-daf0e1538faf', hadith_as_second_source_of_law, deontological).
narrative_ontology:cs_axiom('f6ea0d46-0de6-4b69-9fa3-daf0e1538faf', foundational, qiyas_as_last_resort).
narrative_ontology:cs_axiom_status(qiyas_as_last_resort, holdable).
narrative_ontology:cs_axiom_grounding('f6ea0d46-0de6-4b69-9fa3-daf0e1538faf', qiyas_as_last_resort, conventional).
narrative_ontology:cs_reference_frame('f6ea0d46-0de6-4b69-9fa3-daf0e1538faf', al_shafii_original_formulation).
narrative_ontology:cs_drift_state('f6ea0d46-0de6-4b69-9fa3-daf0e1538faf', contemporary_islamic_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f6ea0d46-0de6-4b69-9fa3-daf0e1538faf', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, hadith_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__shafii_reading, shafii_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, customary_practice_advocates).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__shafii_reading, independent_analogical_reasoners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adhere to and propagate al-Shafi'i's methodology, which provides a clear, standardized framework for legal derivation. They benefit from the clarity and authority this method confers, making their rulings more consistent and defensible within the school.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, shafii_jurists, agenda_setter,
    institutional, generational, identity_locked, global).

% Their expertise in authenticating and transmitting Hadith is elevated to a central, indispensable role in legal derivation. This grants them significant intellectual authority and influence within the Shafi'i school, as their work directly underpins legal rulings.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, hadith_scholars, beneficiary,
    organized, generational, constrained, global).

% Find their reliance on local custom and established community practice (e.g., 'amal ahl al-Madina) diminished as an independent source of law. They must now justify customary practices through the Shafi'i hierarchy, often by finding supporting Hadith or analogical links, rather than asserting custom's inherent validity.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, customary_practice_advocates, payer,
    moderate, biographical, constrained, local).

% Their ability to apply broad, independent analogical reasoning (Qiyas) is constrained by its placement as the lowest tier in the Shafi'i hierarchy. They must ensure their Qiyas is strictly derived from the Qur'an and Hadith, rather than from broader principles or juristic preference, limiting their interpretive freedom.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, independent_analogical_reasoners, payer,
    moderate, biographical, constrained, regional).

% Represent a rival school that places greater emphasis on analogical reasoning and juristic preference, often allowing reason to extend divine intent more broadly. Their methodological approach is implicitly challenged by al-Shafi'i's stricter hierarchy, which limits the scope of such reasoning.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, hanafi_jurists, excluded,
    institutional, generational, identity_locked, global).

% Represent a rival school that validates the living tradition of Medina as a source of law. This source is not explicitly recognized as an independent tier in al-Shafi'i's hierarchy, effectively excluding it as a primary legal basis unless it can be subsumed under Hadith or Ijma.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__shafii_reading, maliki_jurists, excluded,
    institutional, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes the sources and methods of Islamic law, resolving inconsistencies between earlier schools and providing a clear, hierarchical framework for deriving legal rulings, thereby coordinating legal interpretation across diverse contexts.
% TRANSFER_FUNCTION: Transfers interpretive authority from diverse local practices and broad juristic discretion to a centralized, text-based methodology, particularly emphasizing Hadith authentication and transmission. This elevates the status of Hadith scholars and those trained in al-Shafi'i's method.
% ABSENT_VOICES: Jurists from other schools (Hanafi, Maliki, Hanbali) who prioritize different sources or methodologies are implicitly excluded from the Shafi'i framework's internal discourse, as their foundational premises are not fully accommodated. Advocates for broader juristic discretion or local customary law also find their voices marginalized.
% DISAPPEARANCE_RATIONALE: If al-Shafi'i's methodology vanished, the standardization it brought to Islamic jurisprudence would collapse. Legal derivation would revert to earlier, more diverse and potentially inconsistent methods, leading to significant fragmentation and contestation over the legitimacy of various legal rulings. The roles of Hadith scholars and Shafi'i jurists would be fundamentally altered.
% FOUNDING_PROBLEM: The early Islamic legal landscape was characterized by diverse and often inconsistent methods of legal derivation, leading to a lack of methodological clarity and disputes over the legitimacy of rulings across different regions and schools.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Islamic law and contemporary legal scholars (including those outside the Shafi'i school) corroborate the historical problem of methodological inconsistency. While al-Shafi'i's solution is contested by other schools, the need for a coherent legal methodology remains a live concern in Islamic jurisprudence.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__shafii_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__shafii_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jurisprudential_method_kernel__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__shafii_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is medium-high because while it provides clarity, it also imposes a specific interpretive framework that diminishes the independent authority of other sources, extracting interpretive freedom from those who previously relied on them. Suppression (0.70) is high due to the active intellectual and institutional effort required to establish and maintain this hierarchy against competing methodologies. The theater ratio (0.20) is low, as the method is genuinely functional in standardizing legal derivation, though some performativity exists in defending its absolute necessity over other valid approaches.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Shafi'i jurists and Hadith scholars, this method is a 'rope' that brings order and clarity to legal reasoning, making it a net benefit. However, from the perspective of those who prioritize customary practice or broader analogical reasoning (e.g., Hanafi or Maliki jurists), it functions as a 'snare' or 'tangled_rope', extracting their interpretive autonomy and forcing their methods into a subordinate position.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith scholars and Shafi'i jurists are clear beneficiaries, as their roles and methodologies are elevated. Advocates of customary practice and independent analogical reasoners are victims, as their preferred sources are demoted or constrained. Rival schools like Hanafi and Maliki jurists are structurally excluded, as their foundational premises are not fully integrated into this hierarchy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (standardizing legal derivation) remains live, preventing it from being a piton. However, the contestation over its 'founding_problem_status' (live vs. solved) suggests a potential for mandatrophy, where the solution has become an extractive mechanism. The classification as a 'tangled_rope' acknowledges both its genuine coordination function and its asymmetric extraction, preventing mislabeling it as pure coordination (rope) or pure extraction (snare) without acknowledging its dual nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hadith_authenticity_burden,
    'Does the emphasis on Hadith authentication create an undue burden on legal derivation, potentially limiting the law''s adaptability to novel circumstances?',
    'Comparative legal analysis of Shafi''i rulings in novel cases versus those from schools with broader interpretive tools (e.g., Hanafi), assessing speed and consistency of adaptation.',
    'If the burden is significant, it suggests a higher effective extraction of interpretive flexibility, pushing the classification closer to a snare for jurists facing new challenges. If adaptability is maintained, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hadith_authenticity_burden, empirical, 'Assesses the practical impact of Hadith-centricity on legal adaptability.').

omega_variable(
    customary_law_subsumption,
    'To what extent does al-Shafi''i''s method genuinely integrate or merely subsume customary practice and local consensus, rather than truly excluding them?',
    'Detailed historical and ethnographic study of Shafi''i legal practice in diverse regions, analyzing how local customs are either justified within the hierarchy or implicitly ignored/overridden.',
    'If customary practices are effectively subsumed and re-justified, the ''payer'' status of customary_practice_advocates is confirmed. If they are genuinely excluded, the suppression metric is higher for these groups, and the constraint''s extractive nature is more pronounced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_subsumption, empirical, 'Clarifies the fate of customary law under the Shafi''i hierarchy.').

omega_variable(
    methodological_naturalness_vs_construction,
    'Is al-Shafi''i''s hierarchy a ''natural'' discovery of the inherent structure of Islamic law, or a constructed methodological choice that gained dominance through intellectual and institutional effort?',
    'Analysis of pre-Shafi''i legal debates and the arguments used by rival schools to justify their own methodologies. If other coherent, equally ''natural'' structures existed, it points to construction.',
    'If constructed, the ''emerges_naturally'' property is false, and the constraint''s persistence relies more heavily on active enforcement and institutionalization, increasing its effective extractiveness for those outside the Shafi''i school. If natural, its coordination function is more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(methodological_naturalness_vs_construction, conceptual, 'Examines the foundational claim of naturalness for the Shafi''i method.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__shafii_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__shafii_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(juri_tr_t200, jurisprudential_method_kernel__shafii_reading, theater_ratio, 200, 0.15).
narrative_ontology:measurement(juri_tr_t400, jurisprudential_method_kernel__shafii_reading, theater_ratio, 400, 0.2).
narrative_ontology:measurement(juri_tr_t600, jurisprudential_method_kernel__shafii_reading, theater_ratio, 600, 0.2).
narrative_ontology:measurement(juri_tr_t800, jurisprudential_method_kernel__shafii_reading, theater_ratio, 800, 0.2).
narrative_ontology:measurement(juri_tr_t1000, jurisprudential_method_kernel__shafii_reading, theater_ratio, 1000, 0.2).
narrative_ontology:measurement(juri_tr_t1200, jurisprudential_method_kernel__shafii_reading, theater_ratio, 1200, 0.2).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(juri_be_t200, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(juri_be_t400, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 400, 0.6).
narrative_ontology:measurement(juri_be_t600, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 600, 0.63).
narrative_ontology:measurement(juri_be_t800, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 800, 0.65).
narrative_ontology:measurement(juri_be_t1000, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 1000, 0.65).
narrative_ontology:measurement(juri_be_t1200, jurisprudential_method_kernel__shafii_reading, base_extractiveness, 1200, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(juri_su_t200, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 200, 0.6).
narrative_ontology:measurement(juri_su_t400, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 400, 0.65).
narrative_ontology:measurement(juri_su_t600, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 600, 0.68).
narrative_ontology:measurement(juri_su_t800, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 800, 0.7).
narrative_ontology:measurement(juri_su_t1000, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 1000, 0.7).
narrative_ontology:measurement(juri_su_t1200, jurisprudential_method_kernel__shafii_reading, suppression_requirement, 1200, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__shafii_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__shafii_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jurisprudential_method_kernel', which describes the foundational methodologies of Islamic jurisprudence. This specific reading focuses on al-Shafi'i's Hadith-centric hierarchy. Other readings (Hanafi, Maliki, Hanbali) represent alternative, competing methodologies for legal derivation, each with distinct ε values and stakeholder impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
