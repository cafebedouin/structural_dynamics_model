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
 *   constraint_id: jurisprudential_method_kernel__maliki_reading
 *   human_readable: Maliki Jurisprudential Method: Medinan Practice as Source of Law
 *   domain: religious/legal/institutional
 *
 * SUMMARY:
 *   This constraint represents the Maliki school's jurisprudential method,
 *   which asserts that Islamic law derives from the Qur'an and Hadith as
 *   practiced in the Medinan community, with the living tradition ('amal ahl
 *   al-Madina) serving as a valid and authoritative source due to Medina's
 *   faithful preservation of the Prophet's practice. This is one reading of
 *   the broader 'jurisprudential_method_kernel', which encompasses various
 *   schools' approaches to deriving Islamic law. The constraint coordinates
 *   legal interpretation within the Maliki school but extracts by implicitly
 *   devaluing other interpretive claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.55).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.7).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Maliki Jurisprudential Method: Medinan Practice as Source of Law").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "religious/legal/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, '2f2594c0-8a27-4c10-b774-256a22fcf789').
narrative_ontology:cs_kernel_codification('2f2594c0-8a27-4c10-b774-256a22fcf789', formalized).
narrative_ontology:cs_authority_grounding('2f2594c0-8a27-4c10-b774-256a22fcf789', lineage).
narrative_ontology:cs_interpretation_layer_present('2f2594c0-8a27-4c10-b774-256a22fcf789').
narrative_ontology:cs_reading_relation('2f2594c0-8a27-4c10-b774-256a22fcf789', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f2594c0-8a27-4c10-b774-256a22fcf789', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f2594c0-8a27-4c10-b774-256a22fcf789', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('2f2594c0-8a27-4c10-b774-256a22fcf789', foundational, amal_ahl_al_madina_as_source).
narrative_ontology:cs_axiom_status(amal_ahl_al_madina_as_source, holdable).
narrative_ontology:cs_axiom_grounding('2f2594c0-8a27-4c10-b774-256a22fcf789', amal_ahl_al_madina_as_source, conventional).
narrative_ontology:cs_axiom('2f2594c0-8a27-4c10-b774-256a22fcf789', foundational, medinan_practice_superior_authenticity).
narrative_ontology:cs_axiom_status(medinan_practice_superior_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('2f2594c0-8a27-4c10-b774-256a22fcf789', medinan_practice_superior_authenticity, theological).
narrative_ontology:cs_reference_frame('2f2594c0-8a27-4c10-b774-256a22fcf789', prophetic_medinan_practice).
narrative_ontology:cs_drift_state('2f2594c0-8a27-4c10-b774-256a22fcf789', contemporary_global_islamic_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f2594c0-8a27-4c10-b774-256a22fcf789', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_maliki_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, other_sunni_schools_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, maliki_jurists_and_judges).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, muslim_laity_maliki_regions).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, muslim_laity_maliki_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines, transmits, and legitimizes the 'amal ahl al-Madina as a primary source of law. Benefits from the authority and prestige derived from this claim of direct continuity with prophetic practice.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage, agenda_setter,
    institutional, generational, identity_locked, regional).

% Apply and propagate the Maliki methodology in their legal rulings and scholarly works. They benefit from a clear, authoritative framework that guides their interpretive efforts and provides a basis for legal consistency within the school.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, maliki_jurists_and_judges, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, maliki_jurists_and_judges, agenda_setter).

% Their interpretive claims, which may rely more heavily on broader Hadith collections, analogical reasoning (qiyas), or juristic preference (istihsan), are implicitly or explicitly devalued by the Maliki school's emphasis on Medinan practice. They bear the cost of having to defend their methodologies against this claim of superior authenticity.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, non_maliki_jurists, payer,
    powerful, generational, constrained, global).

% Scholars from Hanafi, Shafi'i, and Hanbali schools whose foundational methodologies differ. While they coexist in the broader Sunni legal discourse, the Maliki claim of Medinan authenticity creates a structural pressure that challenges the equal validity of their own interpretive approaches.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, other_sunni_schools_scholars, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, other_sunni_schools_scholars, excluded).

% Benefit from legal consistency and perceived authenticity within their communities. They bear the cost of adherence to a specific school's interpretations, which may limit their access to alternative legal opinions or practices.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, muslim_laity_maliki_regions, beneficiary,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__maliki_reading, muslim_laity_maliki_regions, payer).

% Analyze the Maliki jurisprudential method as a historical, sociological, and philosophical phenomenon, without being bound by its internal theological or legal claims. They observe its impact on legal systems and societies.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, secular_legal_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__maliki_reading, medinan_scholarly_lineage).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__maliki_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a consistent and authoritative methodology for deriving Islamic law, ensuring legal coherence and perceived authenticity within the Maliki school by prioritizing the living tradition of Medina.
% TRANSFER_FUNCTION: Transfers interpretive authority and legitimacy from broader textual analysis (e.g., extensive Hadith collections) to the specific, historically situated practice of the Medinan community, benefiting those who adhere to and transmit this tradition.
% ABSENT_VOICES: Scholars from other Sunni legal schools (Hanafi, Shafi'i, Hanbali) whose methodologies are implicitly or explicitly challenged by the Maliki school's claim of Medinan authenticity. They are present in the broader Islamic legal discourse but are structurally excluded from the internal Maliki claim of methodological superiority.
% DISAPPEARANCE_RATIONALE: If the Maliki jurisprudential method, particularly the authority of 'amal ahl al-Madina, vanished overnight, the legal landscape in regions historically dominated by it would undergo significant upheaval. New interpretive frameworks would be required, potentially leading to legal fragmentation or the adoption of other Sunni schools' methodologies, fundamentally reorganizing legal practice and scholarly authority.
% FOUNDING_PROBLEM: To establish a reliable and authentic source of Islamic law beyond the Qur'an and Hadith, by prioritizing the living practice ('amal) of the Prophet's community in Medina, thereby resolving ambiguities and ensuring fidelity to the earliest Islamic practice.
% FOUNDING_PROBLEM_CORROBORATION: Maliki scholars and institutions attest to the ongoing necessity of this method for legal authenticity and consistency. Independent historians of Islamic law corroborate the historical problem of legal fragmentation and the Maliki school's attempt to resolve it through this methodology, though they may not endorse the theological claims of Medinan superiority. Legislative hearings and scholarly debates in various Muslim-majority countries also reflect ongoing discussions about the validity and application of different jurisprudential methods.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jurisprudential_method_kernel__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__maliki_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) is medium because while it provides a clear framework, it imposes a specific interpretive hierarchy that can be seen as a cost by those favoring broader textual or rational methods. Suppression (0.70) is high because the claim of Medinan authenticity actively suppresses alternative interpretive claims to equal validity, requiring active scholarly and institutional defense. Theater ratio (0.15) is low, as the method remains a genuinely functional and actively applied framework, not merely performative. The claimed type is 'tangled_rope' because it genuinely coordinates legal interpretation while simultaneously extracting authority from alternative methodologies through its specific claims of authenticity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Maliki scholars, this method is a necessary and authentic means of preserving Islamic law. From the perspective of other Sunni schools, it is a restrictive methodology that over-privileges regional practice over broader textual evidence or rational inquiry. The engine's classification will highlight this divergence between the claimed coordination and the measured extraction/suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   The Medinan scholarly lineage and Maliki jurists are primary beneficiaries, gaining authority and a clear interpretive path. Non-Maliki jurists and other Sunni schools' scholars are targets, as their methodologies are implicitly challenged and devalued. The Muslim laity in Maliki regions are both beneficiaries (legal consistency) and payers (limited interpretive choice). Secular legal scholars are observers, analyzing the system without internal commitment.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mislabeling by acknowledging both its coordination function (providing a clear legal methodology) and its extractive nature (devaluing alternative interpretive claims). The 'live' status of the founding problem, coupled with the 'world_rearranges' disappearance verdict, indicates that the constraint's mandate is still perceived as relevant, preventing it from being classified as a piton, despite the contestation over its status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maliki_reading_of_jurisprudential_method_kernel,
    'This constraint is the Maliki reading of the ''jurisprudential_method_kernel''. What would be the structural changes if a different reading (e.g., Hanafi, Hanbali, Shafi''i) were adopted as the dominant framework?',
    'Comparative legal analysis of the practical implications of each school''s methodology on legal rulings, scholarly authority, and institutional structures.',
    'Adopting a different reading would shift the beneficiaries and victims of interpretive authority, alter the perceived authenticity of legal sources, and likely change the effective extractiveness and suppression of the legal system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maliki_reading_of_jurisprudential_method_kernel, conceptual, 'Identifies this constraint as a specific reading of a contested kernel.').

omega_variable(
    authenticity_of_amal_ahl_al_madina,
    'Is the ''amal ahl al-Madina truly a more faithful preservation of prophetic practice than broader Hadith collections or analogical reasoning, as claimed by the Maliki school?',
    'Historical and Hadith-critical scholarship comparing the reliability and scope of Medinan practice with other sources, and analyzing the historical development of legal methodologies.',
    'If the claim of superior authenticity is empirically or conceptually undermined, the Maliki method''s legitimacy would erode, reducing its extractiveness and suppression over alternative interpretive claims. If corroborated, its authority would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_of_amal_ahl_al_madina, empirical, 'Examines the foundational claim of authenticity for Medinan practice.').

omega_variable(
    suppression_of_ijtihad,
    'Does the emphasis on Medinan practice, as a primary source, unduly suppress independent juristic reasoning (ijtihad) in other regions or for novel cases not covered by Medinan ''amal?',
    'Analysis of Maliki legal history and contemporary fatwas to identify instances where ''amal ahl al-Madina has been prioritized over other forms of ijtihad, and its impact on legal innovation.',
    'If significant suppression of ijtihad is demonstrated, the constraint''s effective suppression and extractiveness would be higher than currently measured, indicating a greater cost to legal dynamism and diversity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_ijtihad, empirical, 'Assesses the impact of the Maliki method on juristic independence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__maliki_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(juri_tr_t20, jurisprudential_method_kernel__maliki_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__maliki_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(juri_tr_t60, jurisprudential_method_kernel__maliki_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(juri_tr_t80, jurisprudential_method_kernel__maliki_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__maliki_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(juri_be_t20, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 40, 0.51).
narrative_ontology:measurement(juri_be_t60, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement(juri_be_t80, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 80, 0.54).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 100, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(juri_su_t20, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(juri_su_t60, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(juri_su_t80, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 80, 0.69).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
