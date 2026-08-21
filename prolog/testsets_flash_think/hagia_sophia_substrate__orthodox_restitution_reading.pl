% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__orthodox_restitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__orthodox_restitution_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__orthodox_restitution_reading
 *   human_readable: Hagia Sophia: Orthodox Restitution Claim
 *   domain: cultural_heritage/sovereignty/religious_authority
 *
 * SUMMARY:
 *   This constraint represents the claim that Hagia Sophia's legitimacy
 *   derives from its founding as a Christian cathedral and should return to
 *   Orthodox ecclesiastical control or remain neutral to honor its Byzantine
 *   origins. It is one reading of the 'hagia_sophia_substrate' kernel. The
 *   claim is largely symbolic and ideologically active, with low material
 *   extraction by its proponents and high theatricality in its maintenance
 *   through diplomatic and rhetorical efforts. It is classified as a Piton
 *   because the original function (Orthodox cathedral) has atrophied, but the
 *   claim for its return persists due to historical inertia and symbolic
 *   performance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__orthodox_restitution_reading, 0.15).
domain_priors:suppression_score(hagia_sophia_substrate__orthodox_restitution_reading, 0.1).
domain_priors:theater_ratio(hagia_sophia_substrate__orthodox_restitution_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(hagia_sophia_substrate__orthodox_restitution_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__orthodox_restitution_reading, piton).
narrative_ontology:human_readable(hagia_sophia_substrate__orthodox_restitution_reading, "Hagia Sophia: Orthodox Restitution Claim").
narrative_ontology:topic_domain(hagia_sophia_substrate__orthodox_restitution_reading, "cultural_heritage/sovereignty/religious_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__orthodox_restitution_reading, 'dd4ff049-8d58-4005-926d-3876aa42bba6').
narrative_ontology:cs_kernel_codification('dd4ff049-8d58-4005-926d-3876aa42bba6', fixed_text).
narrative_ontology:cs_authority_grounding('dd4ff049-8d58-4005-926d-3876aa42bba6', lineage).
narrative_ontology:cs_interpretation_layer_present('dd4ff049-8d58-4005-926d-3876aa42bba6').
narrative_ontology:cs_reading_relation('dd4ff049-8d58-4005-926d-3876aa42bba6', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('dd4ff049-8d58-4005-926d-3876aa42bba6', hagia_sophia_substrate__universal_heritage_reading, influences).
narrative_ontology:cs_axiom('dd4ff049-8d58-4005-926d-3876aa42bba6', foundational, ecclesiastical_primacy_over_secular_control).
narrative_ontology:cs_axiom_status(ecclesiastical_primacy_over_secular_control, holdable).
narrative_ontology:cs_axiom_grounding('dd4ff049-8d58-4005-926d-3876aa42bba6', ecclesiastical_primacy_over_secular_control, deontological).
narrative_ontology:cs_axiom('dd4ff049-8d58-4005-926d-3876aa42bba6', foundational, byzantine_historical_continuity).
narrative_ontology:cs_axiom_status(byzantine_historical_continuity, holdable).
narrative_ontology:cs_axiom_grounding('dd4ff049-8d58-4005-926d-3876aa42bba6', byzantine_historical_continuity, conventional).
narrative_ontology:cs_reference_frame('dd4ff049-8d58-4005-926d-3876aa42bba6', byzantine_ecclesiastical_control).
narrative_ontology:cs_drift_state('dd4ff049-8d58-4005-926d-3876aa42bba6', contemporary_turkish_sovereignty, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('dd4ff049-8d58-4005-926d-3876aa42bba6', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__orthodox_restitution_reading, greek_state).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_sovereignty).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hagia_sophia_substrate__orthodox_restitution_reading, turkish_state).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__orthodox_restitution_reading, byzantine_heritage_preservation).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__orthodox_restitution_reading, ecclesiastical_property_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives symbolic affirmation and a rallying point for identity and historical memory from the persistence of the claim. Their connection is deeply cultural and religious, making exit from the claim unthinkable.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, eastern_orthodox_diaspora, beneficiary,
    powerless, generational, identity_locked, global).

% Gains diplomatic leverage and a point of national pride by advocating for the restitution claim. While it could theoretically drop the claim, doing so would incur significant political costs domestically and internationally.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, greek_state, beneficiary,
    institutional, generational, constrained, national).

% Bears the cost of persistent external claims on its sovereignty and cultural heritage. As the sovereign power, it controls the site's status but is trapped by historical narratives and international pressure. It also acts as the agenda-setter for the site's current use.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, turkish_state, payer,
    institutional, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__orthodox_restitution_reading, turkish_state, agenda_setter).

% Would experience an interruption of their current worship rights and a challenge to their historical narrative if the restitution claim were implemented. Their identity is tied to the site's status as a mosque.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, islamic_worship_community, payer,
    organized, biographical, identity_locked, national).

% Monitor the situation and issue recommendations regarding the site's status, often advocating for its preservation as universal heritage. They analyze the competing claims without direct enforcement power over the site.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__orthodox_restitution_reading, international_heritage_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This claim does not solve a coordination problem; it asserts a historical right and seeks to re-establish a prior state of affairs.
% TRANSFER_FUNCTION: Seeks to transfer symbolic legitimacy, historical narrative control, and ultimately, ecclesiastical control of the Hagia Sophia from the Turkish state to the Eastern Orthodox Church.
% ABSENT_VOICES: Secular Turkish citizens who might prefer a purely museum status, or other religious groups with historical ties to the site, are often marginalized in the nationalistic and religious framing of the debate.
% DISAPPEARANCE_RATIONALE: If this claim vanished overnight, it would remove a significant point of contention in Greek-Turkish relations and a rallying point for Orthodox identity, leading to a rearrangement of diplomatic and cultural narratives, though the physical site's status might not immediately change.
% FOUNDING_PROBLEM: The historical loss of Hagia Sophia as a Christian cathedral following the Ottoman conquest of Constantinople in 1453 and its subsequent conversion to a mosque.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of the Byzantine Empire, ecclesiastical histories of the Orthodox Church, and statements from international religious leaders corroborate the historical status and the ongoing claim, independent of the direct beneficiaries.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__orthodox_restitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__orthodox_restitution_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__orthodox_restitution_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hagia_sophia_substrate__orthodox_restitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__orthodox_restitution_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).
:- end_tests(hagia_sophia_substrate__orthodox_restitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because this reading, as a claim, does not directly extract material resources from the site's current operation; its 'extraction' is primarily symbolic (of legitimacy, narrative control). Suppression is low (0.1) because the proponents of this claim do not actively enforce it on the ground; rather, their claim is suppressed by the Turkish state. The theater ratio is high (0.7) as the claim's persistence relies heavily on diplomatic rhetoric, historical commemoration, and ideological performance rather than direct functional activity. Resistance is high (0.8) because the claim is strongly opposed by the Turkish state and the Islamic worship community. Accessibility collapse is low (0.2) as this claim does not diminish the viability of alternative framings; it merely competes with them.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Orthodox claimants, this is a righteous assertion of historical and religious truth. From the perspective of the Turkish state and Islamic community, it is an external interference with national sovereignty and religious freedom. The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The Eastern Orthodox diaspora and the Greek state are beneficiaries, gaining symbolic and diplomatic capital from the claim's persistence. The Turkish state and the Islamic worship community are victims, bearing the cost of external challenges to their sovereignty and worship rights. International heritage bodies act as observers, analyzing the competing claims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_vs_symbolic_extraction,
    'Is the ''extraction'' of this claim purely symbolic (legitimacy, narrative control) or does it have latent material consequences (e.g., tourism revenue, property rights)?',
    'Analysis of economic impacts of similar heritage disputes, or hypothetical modeling of revenue streams under different ownership/management structures.',
    'If material consequences are significant, the base_extractiveness would be higher, potentially shifting the classification towards a Snare if active enforcement were present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_vs_symbolic_extraction, empirical, 'Ambiguity between symbolic and material forms of extraction.').

omega_variable(
    claim_enforceability,
    'Is this claim genuinely enforceable through international law or diplomatic pressure, or is it primarily a rhetorical tool?',
    'Legal analysis of international heritage law and precedents, or observation of the outcomes of diplomatic interventions.',
    'If enforceable, the suppression metric (by the Turkish state against the claim) would be higher, and the claim''s own ''power'' would be greater, potentially altering the balance of forces in the overall kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(claim_enforceability, conceptual, 'The practical enforceability of the restitution claim.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the core disagreement over Hagia Sophia primarily a religious claim, a cultural heritage claim, or a national sovereignty claim?',
    'Content analysis of public discourse, legal arguments, and diplomatic statements from all parties involved.',
    'The classification of the ''hagia_sophia_substrate'' kernel itself, and the relationships between its readings, would shift depending on which framing is considered primary. This reading emphasizes the religious/ecclesiastical aspect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'The fundamental framing of the Hagia Sophia dispute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__orthodox_restitution_reading, 1923, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1923, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1923, 0.7).
narrative_ontology:measurement(hagi_tr_t1950, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1950, 0.7).
narrative_ontology:measurement(hagi_tr_t1975, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 1975, 0.7).
narrative_ontology:measurement(hagi_tr_t2000, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2000, 0.7).
narrative_ontology:measurement(hagi_tr_t2024, hagia_sophia_substrate__orthodox_restitution_reading, theater_ratio, 2024, 0.7).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1923, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1923, 0.15).
narrative_ontology:measurement(hagi_be_t1950, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(hagi_be_t1975, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 1975, 0.15).
narrative_ontology:measurement(hagi_be_t2000, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(hagi_be_t2024, hagia_sophia_substrate__orthodox_restitution_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t1923, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1923, 0.1).
narrative_ontology:measurement(hagi_su_t1950, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(hagi_su_t1975, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 1975, 0.1).
narrative_ontology:measurement(hagi_su_t2000, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(hagi_su_t2024, hagia_sophia_substrate__orthodox_restitution_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__orthodox_restitution_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hagia_sophia_substrate' kernel, each representing a distinct claim regarding the site's legitimacy and control. They are linked to model their interdependencies and contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
