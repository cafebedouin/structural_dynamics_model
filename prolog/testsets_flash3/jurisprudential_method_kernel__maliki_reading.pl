% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__maliki_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__maliki_reading
 *   human_readable: Maliki Jurisprudential Method: Medinan Practice as Source of Law
 *   domain: islamic_jurisprudence/legal_philosophy/institutional_history
 *
 * SUMMARY:
 *   This constraint describes the Maliki school's jurisprudential method,
 *   which posits that law derives from the Qur'an and Hadith, but crucially,
 *   also from the living tradition ('amal ahl al-Madina) of the Medinan
 *   community, due to its perceived faithful preservation of the Prophet's
 *   practice. This reading elevates Medinan custom to a source of law,
 *   implicitly devaluing other regional practices or interpretive methods.
 *   The constraint is claimed as a 'tangled_rope' because it genuinely
 *   coordinates legal interpretation while simultaneously extracting
 *   authority from alternative claims and benefiting the Maliki scholarly
 *   lineage.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.45).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.6).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Maliki Jurisprudential Method: Medinan Practice as Source of Law").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "islamic_jurisprudence/legal_philosophy/institutional_history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, '8a7b16f5-ea37-44e1-a7b2-3aa72ca031fa').
narrative_ontology:cs_kernel_codification('8a7b16f5-ea37-44e1-a7b2-3aa72ca031fa', formalized).
narrative_ontology:cs_authority_grounding('8a7b16f5-ea37-44e1-a7b2-3aa72ca031fa', lineage).
narrative_ontology:cs_interpretation_layer_present('8a7b16f5-ea37-44e1-a7b2-3aa72ca031fa').
narrative_ontology:cs_reading_relation('8a7b16f5-ea37-44e1-a7b2-3aa72ca031fa', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a7b16f5-ea37-44e1-a7b2-3aa72ca031fa', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a7b16f5-ea37-44e1-a7b2-3aa72ca031fa', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('8a7b16f5-ea37-44e1-a7b2-3aa72ca031fa', foundational, amal_ahl_al_madina_as_legal_source).
narrative_ontology:cs_axiom_status(amal_ahl_al_madina_as_legal_source, holdable).
narrative_ontology:cs_axiom_grounding('8a7b16f5-ea37-44e1-a7b2-3aa72ca031fa', amal_ahl_al_madina_as_legal_source, conventional).
narrative_ontology:cs_axiom('8a7b16f5-ea37-44e1-a7b2-3aa72ca031fa', foundational, medinan_practice_preserves_sunnah_most_faithfully).
narrative_ontology:cs_axiom_status(medinan_practice_preserves_sunnah_most_faithfully, holdable).
narrative_ontology:cs_axiom_grounding('8a7b16f5-ea37-44e1-a7b2-3aa72ca031fa', medinan_practice_preserves_sunnah_most_faithfully, theological).
narrative_ontology:cs_reference_frame('8a7b16f5-ea37-44e1-a7b2-3aa72ca031fa', early_maliki_scholarly_consensus).
narrative_ontology:cs_drift_state('8a7b16f5-ea37-44e1-a7b2-3aa72ca031fa', contemporary_globalized_islamic_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8a7b16f5-ea37-44e1-a7b2-3aa72ca031fa', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, maliki_scholarly_lineage).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_community_members).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_claims).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, scholars_outside_maliki_school).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, muslim_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and transmitters of the Maliki school's methodology. They benefit from the authority granted to Medinan practice, which elevates their tradition and gives them a privileged position in legal discourse. Their identity is fused with the school's continuity.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, maliki_scholarly_lineage, agenda_setter,
    institutional, generational, identity_locked, regional).

% Benefit from the elevation of their local customs and practices to a source of divine law, reinforcing their communal identity and traditions. They are coordinated by adherence to the 'amal ahl al-Madina, which provides social cohesion and a shared legal framework.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_community_members, beneficiary,
    moderate, biographical, constrained, local).

% Claims to legal authenticity or methodological validity that do not prioritize Medinan practice are implicitly or explicitly devalued. They bear the cost of having their interpretive authority diminished within the Maliki framework.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_claims, payer,
    powerless, generational, trapped, global).

% Scholars from other schools (Hanafi, Shafi'i, Hanbali) whose methodologies differ. While they have their own schools, within the Maliki framework, their alternative interpretive claims are treated as less authoritative, requiring them to engage on Maliki terms if seeking broader acceptance.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, scholars_outside_maliki_school, payer,
    organized, biographical, constrained, global).

% Benefit from a clear, established legal framework that provides guidance for daily life and religious practice. Their identity is often tied to adherence to a particular school of thought, making exit from the tradition difficult.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, muslim_laity, beneficiary,
    powerless, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and authoritative method for deriving Islamic law, ensuring consistency and legitimacy by grounding it in the preserved practice of the Prophet's community in Medina, thereby coordinating legal interpretation and application across generations and regions where the Maliki school is dominant.
% TRANSFER_FUNCTION: Transfers interpretive authority and legitimacy from alternative jurisprudential methods to the Maliki school's emphasis on Medinan practice, effectively channeling scholarly influence and institutional resources towards those who adhere to this methodology.
% ABSENT_VOICES: Scholars and communities from other Islamic legal schools, particularly those who emphasize analogical reasoning (qiyas) or juristic preference (istihsan) more heavily, would argue that their methods are equally valid and necessary for addressing novel legal issues. They are 'absent' in the sense that their methodologies are not given equal weight within the Maliki framework.
% DISAPPEARANCE_RATIONALE: If the Maliki jurisprudential method, particularly the authority of 'amal ahl al-Madina, vanished overnight, the legal systems and scholarly traditions in regions where it is dominant (e.g., North Africa, West Africa) would face a profound crisis of legitimacy and coherence. Legal rulings would be contested, scholarly hierarchies would collapse, and new interpretive frameworks would rapidly emerge to fill the void, leading to a significant reorganization of Islamic legal thought and practice.
% FOUNDING_PROBLEM: The early Muslim community faced the challenge of systematizing legal rulings after the Prophet's death, ensuring that new issues were addressed in a manner consistent with divine revelation and the Prophet's sunnah, amidst diverse opinions and practices across different regions.
% FOUNDING_PROBLEM_CORROBORATION: The Maliki scholarly lineage attests that the problem of ensuring legal consistency and authenticity remains live, particularly in adapting to modern challenges while preserving tradition. Independent historians of Islamic law corroborate that the problem of legal systematization was indeed foundational for all early schools, though they might contest the Maliki school's specific claims of Medinan superiority as a historical rather than a theological or methodological fact.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jurisprudential_method_kernel__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__maliki_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__maliki_reading_tests).
:- end_tests(jurisprudential_method_kernel__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) stems from the implicit devaluation of non-Medinan interpretive claims, which are forced to contend with a higher bar for legitimacy within the Maliki framework. Suppression (0.6) is present because the Maliki school actively defends the authority of 'amal ahl al-Madina against competing methodologies, requiring active enforcement of its interpretive hierarchy. Theater ratio is low (0.1) as the method is largely functional in guiding legal practice, with minimal performative maintenance. The historical measurements show a gradual increase in extractiveness and suppression as the Maliki school solidified its position and defended its methodology against other emerging schools.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Maliki scholarly lineage, this method is a robust and authentic way to preserve Islamic law, a 'rope' of coordination. From the perspective of scholars from other schools, it represents an 'extraction' of interpretive authority, a 'snare' that limits the scope of legitimate legal reasoning. The engine's computation will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The Maliki scholarly lineage and Medinan community members are beneficiaries, as their traditions and authority are elevated. Non-Medinan interpretive claims and scholars outside the Maliki school are victims, as their methodologies are implicitly or explicitly subordinated. Muslim laity are beneficiaries of a coherent legal system, but also subject to its interpretive constraints.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_authenticity_of_amal_ahl_al_madina,
    'To what extent does the ''amal ahl al-Madina truly represent the unadulterated practice of the Prophet and his Companions, as opposed to later Medinan scholarly consensus?',
    'Further historical and critical Hadith studies, comparing early Medinan legal opinions with broader Hadith collections and non-Medinan historical accounts.',
    'If ''amal ahl al-Madina is found to be more a product of later scholarly consensus than direct prophetic practice, the foundational empirical claim of the Maliki method would be weakened, potentially reducing its perceived legitimacy and extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_authenticity_of_amal_ahl_al_madina, empirical, 'The historical veracity of the Maliki school''s claim regarding Medinan practice.').

omega_variable(
    scope_of_medinan_practice_authority,
    'Is the authority of ''amal ahl al-Madina intended to be universal for all Muslims, or primarily for those within the Maliki school''s sphere of influence?',
    'Analysis of classical Maliki texts regarding the application of ''amal ahl al-Madina outside Medina, and comparative study of how other schools engaged with this concept.',
    'If universal, the extractiveness from non-Medinan claims is higher; if regional, the constraint''s scope of extraction is more limited, potentially reclassifying it as a more benign coordination mechanism for its adherents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_medinan_practice_authority, conceptual, 'The intended geographical and methodological scope of Medinan practice as a legal source.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative interpretive claims structural (institutionalized scholarly hierarchies) or internalized (scholars self-censoring to conform to Maliki orthodoxy)?',
    'Sociological studies of contemporary Maliki scholarly communities and analysis of historical debates where dissenting views were expressed or suppressed.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as scholars carry the suppression with them even in less overtly coercive environments. If purely structural, removing institutional barriers would more readily lead to diversification of interpretive methods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative interpretive claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__maliki_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(juri_tr_t300, jurisprudential_method_kernel__maliki_reading, theater_ratio, 300, 0.08).
narrative_ontology:measurement(juri_tr_t600, jurisprudential_method_kernel__maliki_reading, theater_ratio, 600, 0.1).
narrative_ontology:measurement(juri_tr_t900, jurisprudential_method_kernel__maliki_reading, theater_ratio, 900, 0.09).
narrative_ontology:measurement(juri_tr_t1200, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1200, 0.1).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(juri_be_t300, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 300, 0.38).
narrative_ontology:measurement(juri_be_t600, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 600, 0.45).
narrative_ontology:measurement(juri_be_t900, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 900, 0.42).
narrative_ontology:measurement(juri_be_t1200, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1200, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(juri_su_t300, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 300, 0.5).
narrative_ontology:measurement(juri_su_t600, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 600, 0.6).
narrative_ontology:measurement(juri_su_t900, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 900, 0.55).
narrative_ontology:measurement(juri_su_t1200, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1200, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jurisprudential_method_kernel', which concerns the fundamental sources and methods for deriving Islamic law. This Maliki reading emphasizes the practice of the Medinan community. Other readings (Hanafi, Shafi'i, Hanbali) offer alternative methodologies, leading to a family of linked constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
