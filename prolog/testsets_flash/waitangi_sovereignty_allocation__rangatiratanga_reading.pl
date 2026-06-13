% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__rangatiratanga_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__rangatiratanga_reading
 *   human_readable: Treaty of Waitangi: Tino Rangatiratanga Reading
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'rangatiratanga reading' of the
 *   Treaty of Waitangi, where the Māori text's Article II is understood to
 *   retain full Māori authority (tino rangatiratanga) over their lands,
 *   resources, and taonga (treasures), with the Crown gaining only
 *   kāwanatanga (governorship) over settlers. This reading asserts Māori
 *   inherent sovereignty and limits Crown jurisdiction, leading to ongoing
 *   contestation with the dominant Crown sovereignty interpretation. The
 *   constraint is claimed as a Tangled Rope because it has a genuine
 *   coordination function (establishing a framework for co-existence) but
 *   also involves significant asymmetric extraction and requires active
 *   enforcement to maintain the Crown's current position against Māori
 *   claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.6).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.7).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Treaty of Waitangi: Tino Rangatiratanga Reading").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, '8390d626-c839-4543-98e1-ddb55d2c61c4').
narrative_ontology:cs_kernel_codification('8390d626-c839-4543-98e1-ddb55d2c61c4', fixed_text).
narrative_ontology:cs_authority_grounding('8390d626-c839-4543-98e1-ddb55d2c61c4', lineage).
narrative_ontology:cs_interpretation_layer_present('8390d626-c839-4543-98e1-ddb55d2c61c4').
narrative_ontology:cs_reading_relation('8390d626-c839-4543-98e1-ddb55d2c61c4', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('8390d626-c839-4543-98e1-ddb55d2c61c4', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_axiom('8390d626-c839-4543-98e1-ddb55d2c61c4', foundational, maori_inherent_sovereignty).
narrative_ontology:cs_axiom_status(maori_inherent_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('8390d626-c839-4543-98e1-ddb55d2c61c4', maori_inherent_sovereignty, deontological).
narrative_ontology:cs_axiom('8390d626-c839-4543-98e1-ddb55d2c61c4', foundational, crown_kawanatanga_limited_to_settlers).
narrative_ontology:cs_axiom_status(crown_kawanatanga_limited_to_settlers, holdable).
narrative_ontology:cs_axiom_grounding('8390d626-c839-4543-98e1-ddb55d2c61c4', crown_kawanatanga_limited_to_settlers, conventional).
narrative_ontology:cs_reference_frame('8390d626-c839-4543-98e1-ddb55d2c61c4', maori_text_original_intent).
narrative_ontology:cs_drift_state('8390d626-c839-4543-98e1-ddb55d2c61c4', contemporary_new_zealand_law, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('8390d626-c839-4543-98e1-ddb55d2c61c4', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_hapu).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_communities).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_institutions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) reflects the ongoing dispossession of Māori lands and resources under the Crown's asserted sovereignty, despite the Treaty's Māori text. Suppression (0.7) is high due to the historical and ongoing legal and political mechanisms used to marginalize the rangatiratanga reading and enforce Crown authority. The theater ratio (0.4) indicates that while some efforts are made to acknowledge Māori rights, a significant portion of state activity performs a commitment to a unified sovereignty that this reading fundamentally challenges. Resistance (0.8) is high, reflecting continuous Māori activism, legal challenges, and political organizing to assert tino rangatiratanga.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Māori iwi and hapū, this reading represents an unfulfilled promise and a basis for asserting their rights, experiencing the constraint as a struggle against extraction and suppression. From the Crown's perspective, acknowledging this reading fully would entail a significant loss of established power and resources, making it a 'payer' of potential future costs. The judiciary, as an agenda-setter, mediates these divergent perspectives, shaping the practical outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   Māori iwi and hapū are beneficiaries of this reading's assertion of their inherent authority, but also targets of the Crown's resistance to its full implementation (d is complex, but leans towards beneficiary for the *reading itself*). The Crown government and settler institutions are targets/payers, as this reading challenges their established authority and resource control. The New Zealand judiciary, as an agenda-setter, is structurally positioned to interpret and enforce, influencing directionality for all parties.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate (the assertion of tino rangatiratanga) is considered by its proponents to be an inherent and ongoing right, not a temporary function. The contest is over its recognition and implementation, not its obsolescence. The classification as Tangled Rope prevents mislabeling it as a Snare, acknowledging the genuine coordination function intended by the Māori text, while still capturing the asymmetric extraction and suppression that have characterized its historical non-implementation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_recognition_status,
    'To what extent is the ''rangatiratanga reading'' legally recognized and enforceable within the New Zealand legal system, beyond rhetorical acknowledgment?',
    'Analysis of judicial precedents, legislative acts, and government policy documents that explicitly affirm or deny the full scope of tino rangatiratanga as understood by this reading.',
    'If fully recognized, the constraint would shift towards a Rope or even a Mountain (for inherent rights), with significantly reduced extraction and suppression. If largely unrecognized, it remains a Tangled Rope or Snare, with its coordination function undermined by ongoing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_recognition_status, empirical, 'The practical legal status of the rangatiratanga reading.').

omega_variable(
    resource_control_implementation,
    'What is the actual level of Māori control over traditional lands and resources (e.g., forests, fisheries, water) compared to the theoretical claims of tino rangatiratanga?',
    'Empirical study of resource management decisions, land ownership patterns, and economic benefits derived from resources within traditional Māori territories.',
    'A high degree of actual Māori control would reduce the measured extractiveness and suppression, potentially shifting the classification towards a Rope. Low actual control, despite the reading''s claims, would confirm high extraction and suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_control_implementation, empirical, 'Gap between claimed and actual Māori control over resources.').

omega_variable(
    sovereignty_framing_ambiguity,
    'Is the concept of ''tino rangatiratanga'' fundamentally incommensurable with Western notions of ''sovereignty'', or can a legal framework reconcile both?',
    'Conceptual analysis by legal scholars and indigenous philosophers, and practical experimentation with co-governance models that attempt to bridge the concepts.',
    'If incommensurable, the conflict is irreducible, and the constraint will remain highly contested and extractive. If reconcilable, pathways to a more equitable and less extractive arrangement may emerge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_framing_ambiguity, conceptual, 'Conceptual compatibility of tino rangatiratanga and Western sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(wait_tr_t1880, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1880, 0.2).
narrative_ontology:measurement(wait_tr_t1920, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1920, 0.3).
narrative_ontology:measurement(wait_tr_t1960, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1960, 0.5).
narrative_ontology:measurement(wait_tr_t2000, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(wait_tr_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1840, 0.2).
narrative_ontology:measurement(wait_be_t1880, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1880, 0.4).
narrative_ontology:measurement(wait_be_t1920, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1920, 0.6).
narrative_ontology:measurement(wait_be_t1960, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1960, 0.75).
narrative_ontology:measurement(wait_be_t2000, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(wait_be_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1840, 0.3).
narrative_ontology:measurement(wait_su_t1880, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1880, 0.5).
narrative_ontology:measurement(wait_su_t1920, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1920, 0.7).
narrative_ontology:measurement(wait_su_t1960, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1960, 0.85).
narrative_ontology:measurement(wait_su_t2000, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(wait_su_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, identity_coordination).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, new_zealand_land_claims_settlement_process).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_language_revitalization_efforts).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'waitangi_sovereignty_allocation' kernel. It focuses on the Māori text's retention of tino rangatiratanga, contrasting with the Crown sovereignty and partnership readings. Each reading has distinct implications for power, resource allocation, and legal interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
