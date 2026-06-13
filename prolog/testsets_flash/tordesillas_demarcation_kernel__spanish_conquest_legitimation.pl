% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__spanish_conquest_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__spanish_conquest_legitimation, []).

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
 *   constraint_id: tordesillas_demarcation_kernel__spanish_conquest_legitimation
 *   human_readable: Papal Demarcation as Spanish Conquest Legitimation
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   This constraint represents the 'Spanish conquest legitimation' reading of
 *   the Tordesillas Demarcation kernel. It frames the Papal grants (Bulls of
 *   Donation, Treaty of Tordesillas) not merely as a division of exploration
 *   rights, but as a direct license for the Spanish Crown to claim
 *   sovereignty over, conquer, and subjugate indigenous populations and their
 *   territories west of the agreed-upon line. This reading emphasizes the
 *   extractive and suppressive aspects of the colonial enterprise, directly
 *   enabled by the Papal authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.9).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.95).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.9).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Papal Demarcation as Spanish Conquest Legitimation").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '24bad201-748e-467b-88c0-f1c14189f652').
narrative_ontology:cs_kernel_codification('24bad201-748e-467b-88c0-f1c14189f652', fixed_text).
narrative_ontology:cs_authority_grounding('24bad201-748e-467b-88c0-f1c14189f652', lineage).
narrative_ontology:cs_interpretation_layer_present('24bad201-748e-467b-88c0-f1c14189f652').
narrative_ontology:cs_reading_relation('24bad201-748e-467b-88c0-f1c14189f652', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, coexists_with).
narrative_ontology:cs_axiom('24bad201-748e-467b-88c0-f1c14189f652', foundational, papal_grant_confers_sovereignty_over_non_christian_lands).
narrative_ontology:cs_axiom_status(papal_grant_confers_sovereignty_over_non_christian_lands, holdable).
narrative_ontology:cs_axiom_grounding('24bad201-748e-467b-88c0-f1c14189f652', papal_grant_confers_sovereignty_over_non_christian_lands, theological).
narrative_ontology:cs_axiom('24bad201-748e-467b-88c0-f1c14189f652', foundational, indigenous_peoples_lack_true_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_peoples_lack_true_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('24bad201-748e-467b-88c0-f1c14189f652', indigenous_peoples_lack_true_sovereignty, theological).
narrative_ontology:cs_reference_frame('24bad201-748e-467b-88c0-f1c14189f652', divine_right_of_conquest).
narrative_ontology:cs_drift_state('24bad201-748e-467b-88c0-f1c14189f652', post_enlightenment_international_law, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('24bad201-748e-467b-88c0-f1c14189f652', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_conquistadors).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary beneficiary and enforcer, using the Papal grant to assert exclusive rights to lands west of the Tordesillas line, justifying conquest, resource extraction, and the establishment of colonial administration. It actively suppressed any challenges to this claim.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown, agenda_setter,
    institutional, generational, arbitrage, global).

% Received direct authorization and material support for expeditions, gaining wealth, land, and titles through the subjugation of indigenous peoples and the exploitation of resources. Their actions were legitimized by the Papal grant.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_conquistadors, beneficiary,
    powerful, biographical, mobile, continental).

% Provided the theological and moral justification for the conquest through the doctrine of 'discovery' and the mandate to convert non-Christians. Benefited from the expansion of its spiritual domain and the acquisition of new converts and resources.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, catholic_church, beneficiary,
    institutional, civilizational, analytical, global).

% Were the primary victims, subjected to territorial dispossession, forced labor (e.g., encomienda system), cultural destruction, and forced conversion. Their sovereignty and property rights were entirely disregarded by the Papal grant and subsequent Spanish actions.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line, payer,
    powerless, generational, trapped, continental).

% Were theoretically excluded from claiming lands west of the line by the Papal decree, though many eventually challenged this through piracy, smuggling, and later, direct colonial expansion. They would have contested the legitimacy of the exclusive grant.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, other_european_powers, excluded,
    organized, generational, constrained, global).

% Analyze the historical and legal implications of the Papal grants, often critiquing their role in legitimizing colonialism and the violation of indigenous sovereignty from a modern perspective.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a framework for the Spanish Crown to coordinate its colonial expansion efforts, allocate resources, and manage claims among conquistadors, while minimizing direct conflict with Portugal over newly 'discovered' territories.
% TRANSFER_FUNCTION: Transferred sovereignty, land, and resources from indigenous populations to the Spanish Crown and its agents, along with the labor and cultural autonomy of the indigenous peoples.
% ABSENT_VOICES: The indigenous populations, whose lands and lives were directly impacted, were entirely absent from the negotiation and legitimization process. Their voices would have asserted pre-existing sovereignty, property rights, and self-determination.
% DISAPPEARANCE_RATIONALE: If the Papal grant's legitimizing power had vanished, the Spanish conquest would have lacked its primary legal and moral justification, potentially altering the pace, nature, and extent of colonization, and certainly changing the historical narrative of sovereignty in the Americas.
% FOUNDING_PROBLEM: The problem of legitimizing European claims to newly 'discovered' non-Christian lands, preventing conflict between Catholic powers (Spain and Portugal), and providing a moral framework for the subjugation and conversion of indigenous peoples.
% FOUNDING_PROBLEM_CORROBORATION: Modern international law and indigenous rights movements universally reject the legal and moral basis of the Papal grants for territorial conquest. Historians and legal scholars outside the Catholic Church and former colonial powers corroborate that the 'problem' of legitimizing conquest is no longer considered valid, and the original justification is defunct.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__spanish_conquest_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__spanish_conquest_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.9) because the entire colonial project was designed for resource extraction and labor exploitation, with minimal reciprocal benefit to the indigenous populations. Suppression is also very high (0.95) due to the military force, legal doctrines, and religious coercion used to enforce Spanish claims and prevent indigenous resistance or alternative forms of governance. Theater ratio is low (0.1) as the 'civilizing mission' and 'conversion' narratives were largely cover for material gain, but some genuine (if misguided) missionary zeal existed. The high accessibility_collapse (0.9) reflects the near-total disregard for indigenous sovereignty and the imposition of a new legal order. Resistance (0.7) was significant but ultimately overwhelmed by Spanish military and institutional power.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Spanish Crown and the Church, this was a legitimate exercise of authority for the 'greater good' of conversion and civilization. From the indigenous perspective, it was an act of unprovoked aggression, theft, and genocide. The engine's classification as a Snare reflects the latter, emphasizing the coercive and extractive reality over the legitimizing narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spanish Crown, conquistadors, and the Catholic Church are clear beneficiaries, deriving immense material and spiritual gains. Indigenous populations west of the line are the unequivocal targets and victims, experiencing total dispossession and subjugation. Other European powers are 'excluded' as they were theoretically barred from the western territories, though they later challenged this exclusion.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_scope,
    'Was the Papal authority genuinely understood by all European powers at the time to grant full territorial sovereignty and rights of subjugation over non-Christian lands, or merely exclusive rights of evangelization and trade?',
    'Analysis of contemporary legal treatises and diplomatic correspondence from non-Spanish/Portuguese powers regarding the Bulls of Donation and Treaty of Tordesillas.',
    'If the broader interpretation of full sovereignty and subjugation was not universally accepted, the constraint''s legitimacy (even among Europeans) was weaker, implying higher suppression was required to maintain it. If it was universally accepted, the constraint''s ''naturalness'' (within the European legal framework) was higher, making its extractive nature more insidious.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_authority_scope, conceptual, 'Ambiguity in the scope of Papal authority to grant sovereignty.').

omega_variable(
    indigenous_sovereignty_recognition,
    'To what extent did the Spanish Crown or the Catholic Church ever genuinely acknowledge or attempt to reconcile with pre-existing indigenous sovereignty, even rhetorically, versus outright denial?',
    'Examination of the ''Requerimiento'' and other legal instruments, as well as debates like Valladolid, for any genuine attempt at recognizing indigenous rights beyond a pretext for conquest.',
    'If there was any genuine (even if failed) attempt at recognition, the ''suppression'' metric might be slightly lower, reflecting a more complex, albeit still extractive, interaction. If it was pure denial, the high suppression and extractiveness are fully justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_sovereignty_recognition, empirical, 'Degree of recognition of indigenous sovereignty by colonial powers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 1493, 1898).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1493, 0.05).
narrative_ontology:measurement(tord_tr_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1550, 0.08).
narrative_ontology:measurement(tord_tr_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1650, 0.1).
narrative_ontology:measurement(tord_tr_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1750, 0.12).
narrative_ontology:measurement(tord_tr_t1850, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(tord_tr_t1898, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1898, 0.18).

% Extraction over time
narrative_ontology:measurement(tord_be_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1493, 0.7).
narrative_ontology:measurement(tord_be_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1550, 0.85).
narrative_ontology:measurement(tord_be_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1650, 0.9).
narrative_ontology:measurement(tord_be_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1750, 0.88).
narrative_ontology:measurement(tord_be_t1850, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1850, 0.8).
narrative_ontology:measurement(tord_be_t1898, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1898, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1493, 0.7).
narrative_ontology:measurement(tord_su_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1550, 0.9).
narrative_ontology:measurement(tord_su_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1650, 0.95).
narrative_ontology:measurement(tord_su_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1750, 0.9).
narrative_ontology:measurement(tord_su_t1850, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1850, 0.85).
narrative_ontology:measurement(tord_su_t1898, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1898, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enforcement_mechanism).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, portuguese_exploration_legitimation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomienda_system).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, colonial_resource_extraction_regime).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Tordesillas Demarcation kernel, focusing on Spanish conquest legitimation. It is linked to the 'portuguese_exploration_legitimation' reading, which emphasizes inter-European coordination over eastern territories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
