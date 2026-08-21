% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__indigenous_return_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__indigenous_return_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: jewish_self_determination__indigenous_return_reading
 *   human_readable: Jewish Indigeneity and Decolonization (Indigenous Return Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the 'indigenous return' reading of Jewish
 *   self-determination, asserting that Jewish people are indigenous to the
 *   land with an unbroken connection, thereby framing Zionism as
 *   decolonization rather than colonization. This reading is a specific
 *   interpretation within a highly contested kernel, aiming to establish a
 *   legitimate, non-colonial basis for Jewish sovereignty. The claimed type
 *   is 'rope' to reflect the asserted coordination function of recognizing
 *   indigenous rights, but the metrics reflect the high extractiveness and
 *   suppression inherent in maintaining this claim amidst intense
 *   contestation and the reframing of other indigenous claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, 0.65).
domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, 0.75).
domain_priors:theater_ratio(jewish_self_determination__indigenous_return_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__indigenous_return_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__indigenous_return_reading, "Jewish Indigeneity and Decolonization (Indigenous Return Reading)").
narrative_ontology:topic_domain(jewish_self_determination__indigenous_return_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__indigenous_return_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__indigenous_return_reading, '9ba12255-d051-4896-ac0c-fa62fe36f79a').
narrative_ontology:cs_kernel_codification('9ba12255-d051-4896-ac0c-fa62fe36f79a', formalized).
narrative_ontology:cs_authority_grounding('9ba12255-d051-4896-ac0c-fa62fe36f79a', lineage).
narrative_ontology:cs_interpretation_layer_present('9ba12255-d051-4896-ac0c-fa62fe36f79a').
narrative_ontology:cs_reading_relation('9ba12255-d051-4896-ac0c-fa62fe36f79a', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_reading_relation('9ba12255-d051-4896-ac0c-fa62fe36f79a', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ba12255-d051-4896-ac0c-fa62fe36f79a', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ba12255-d051-4896-ac0c-fa62fe36f79a', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_axiom('9ba12255-d051-4896-ac0c-fa62fe36f79a', foundational, jewish_unbroken_indigenous_connection).
narrative_ontology:cs_axiom_status(jewish_unbroken_indigenous_connection, holdable).
narrative_ontology:cs_axiom_grounding('9ba12255-d051-4896-ac0c-fa62fe36f79a', jewish_unbroken_indigenous_connection, empirically_contingent).
narrative_ontology:cs_axiom('9ba12255-d051-4896-ac0c-fa62fe36f79a', foundational, zionism_as_decolonization).
narrative_ontology:cs_axiom_status(zionism_as_decolonization, holdable).
narrative_ontology:cs_axiom_grounding('9ba12255-d051-4896-ac0c-fa62fe36f79a', zionism_as_decolonization, instrumental).
narrative_ontology:cs_reference_frame('9ba12255-d051-4896-ac0c-fa62fe36f79a', uncontested_indigenous_return).
narrative_ontology:cs_drift_state('9ba12255-d051-4896-ac0c-fa62fe36f79a', contemporary_postcolonial_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9ba12255-d051-4896-ac0c-fa62fe36f79a', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__indigenous_return_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, liberal_zionist_advocates).
narrative_ontology:constraint_victim(jewish_self_determination__indigenous_return_reading, palestinian_indigenous_claimants).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, indigenous_rights_framework).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts and actively promotes the narrative of unbroken Jewish connection and indigeneity to the land, framing Zionism as a decolonization movement. Benefits from the legitimacy and political capital derived from this framing.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land, agenda_setter,
    institutional, generational, identity_locked, global).

% Their indigenous claims are reframed as later arrival or subordinate by this constraint's narrative, forcing them to continuously assert their own indigeneity and rights against a discourse that seeks to displace or diminish them. They bear the costs of this contestation and narrative suppression.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, palestinian_indigenous_claimants, payer,
    powerless, generational, trapped, local).

% Engage with the concept of indigenous rights, often seeking universal application. They observe and sometimes support or critique the application of indigenous frameworks to the Jewish claim, influencing international discourse and policy.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, international_indigenous_rights_advocates, observer,
    organized, generational, analytical, global).

% Analyze power dynamics, colonialism, and decolonization. Many critique the indigenous return framing of Zionism, viewing it as a form of settler colonialism, and thus stand in direct opposition to this constraint's core premise.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, postcolonial_theorists, observer,
    analytical, generational, analytical, global).

% Seek to reconcile Zionist aspirations with liberal democratic values and international law. This reading provides a powerful, non-colonial justification for their political project, aligning it with contemporary progressive discourse.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, liberal_zionist_advocates, beneficiary,
    organized, biographical, constrained, global).

% Advocate for Jewish collective survival and flourishing through diaspora pluralism, rejecting territorial sovereignty as a primary mode. This constraint's emphasis on land-based indigeneity directly contradicts their vision and is seen as a dangerous political path.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, anti_zionist_diasporists, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the recognition of Jewish indigeneity and self-determination in the land, providing a framework for national liberation and return.
% TRANSFER_FUNCTION: Transfers historical narrative primacy, political legitimacy, and moral authority to Jewish claims of indigeneity, implicitly subordinating or reframing other indigenous claims to the same land.
% ABSENT_VOICES: Palestinian indigenous claimants, whose historical narratives and claims are actively reframed or marginalized by this constraint, and those postcolonial scholars who universally apply settler-colonial frameworks to Zionism.
% DISAPPEARANCE_RATIONALE: If the claim of Jewish indigeneity and unbroken connection vanished, the entire political and historical justification for Zionism as a decolonization movement would collapse. This would fundamentally alter the conflict's framing, international discourse, and the perceived legitimacy of the state's founding narrative.
% FOUNDING_PROBLEM: To establish a legitimate, non-colonial basis for Jewish self-determination in the land, countering narratives that frame Zionism as a settler-colonial project and aligning it with global indigenous rights movements.
% FOUNDING_PROBLEM_CORROBORATION: Jewish historical and cultural institutions, some international legal scholars, and certain indigenous studies scholars corroborate the historical connection and indigenous status. Critics (Palestinian scholars, many postcolonial theorists, some human rights organizations) dispute this framing, arguing the founding problem is a cover for ongoing dispossession.
narrative_ontology:disappearance_verdict(jewish_self_determination__indigenous_return_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__indigenous_return_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__indigenous_return_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jewish_self_determination__indigenous_return_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__indigenous_return_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__indigenous_return_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__indigenous_return_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high not due to direct economic extraction from declared victims (as per this reading's framing, which declares no victims), but from the significant costs borne by those whose narratives and claims are subordinated or suppressed by the assertion of this constraint. The high suppression (0.75) reflects active efforts to marginalize counter-narratives and alternative historical interpretations. Resistance (0.80) is very high, indicating the intense contestation this claim faces. The theater ratio is low (0.20) because this is an actively asserted and defended claim, not a performative one. The measurement series show a gradual increase in extractiveness and suppression, reflecting the intensifying global debate and the increasing effort required to maintain this narrative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Jewish claimants, this constraint is a legitimate assertion of indigenous rights and a necessary act of decolonization, functioning as a 'rope' to coordinate recognition. From the perspective of Palestinian indigenous claimants, the same constraint operates as a 'snare' or 'tangled rope', actively suppressing their own claims and extracting legitimacy from their narrative, even if they are not explicitly named as 'victims' by this specific reading's framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish claimants are the primary beneficiaries and agenda-setters, as the constraint legitimizes their claims. Palestinian indigenous claimants are payers, bearing the costs of narrative subordination and contestation, even if not directly 'victims' of extraction by this specific constraint's mechanism. International advocates and theorists act as observers, influencing the discourse but not directly subject to the constraint's primary mechanisms. Anti-Zionist diasporists are excluded, as their alternative vision of Jewish flourishing is incompatible with this land-based indigenous claim.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigeneity_definition_ambiguity,
    'Is ''indigeneity'' a fixed historical status or a contested political claim, and how does its definition impact the validity of competing claims?',
    'Comparative legal and anthropological studies of indigenous status in other contested territories, focusing on criteria for recognition and the treatment of overlapping claims.',
    'If indigeneity is primarily a political claim, the constraint''s extractiveness and suppression are higher, reflecting the power dynamics of its assertion. If it''s a fixed historical status, the constraint''s legitimacy is higher, and its extractiveness (if accepted) would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigeneity_definition_ambiguity, conceptual, 'Ambiguity in the definition and application of ''indigeneity'' in contested contexts.').

omega_variable(
    reframing_as_extraction,
    'Does the reframing of Palestinian presence as ''later arrival or co-indigenous with subordinate claim'' constitute a form of narrative extraction or epistemic suppression, even without direct economic transfer?',
    'Analysis of the impact of narrative dominance on political agency and resource allocation for the reframed group, using frameworks from critical discourse analysis and postcolonial studies.',
    'If confirmed as a form of extraction, the constraint''s effective extractiveness for Palestinian claimants would be higher, potentially reclassifying it as a ''tangled_rope'' or ''snare'' from their seat, despite this reading''s claim of no victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reframing_as_extraction, conceptual, 'Whether narrative reframing of indigenous claims functions as a form of extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legal/political barriers to counter-narratives) or internalized (narrative dominance shaping public perception)?',
    'Longitudinal studies of public opinion and media representation in various jurisdictions, alongside analysis of legal and policy frameworks that restrict counter-narratives.',
    'If suppression is largely internalized, the constraint''s effective suppression is higher than structural measures suggest, as the narrative persists even if formal barriers are removed. If primarily structural, legal/policy changes could more directly reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in narrative contestation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__indigenous_return_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__indigenous_return_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__indigenous_return_reading, theater_ratio, 1967, 0.16).
narrative_ontology:measurement(jewi_tr_t1987, jewish_self_determination__indigenous_return_reading, theater_ratio, 1987, 0.18).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__indigenous_return_reading, theater_ratio, 2000, 0.19).
narrative_ontology:measurement(jewi_tr_t2014, jewish_self_determination__indigenous_return_reading, theater_ratio, 2014, 0.2).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__indigenous_return_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(jewi_be_t1987, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1987, 0.6).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(jewi_be_t2014, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2014, 0.64).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1967, 0.65).
narrative_ontology:measurement(jewi_su_t1987, jewish_self_determination__indigenous_return_reading, suppression_requirement, 1987, 0.7).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__indigenous_return_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(jewi_su_t2014, jewish_self_determination__indigenous_return_reading, suppression_requirement, 2014, 0.74).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__indigenous_return_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__indigenous_return_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__settler_colonial_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'jewish_self_determination' kernel, each representing a distinct structural claim and classification. This reading focuses on Jewish indigeneity and decolonization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
