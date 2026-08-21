% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: Marriage Commitment Legitimacy (Hybrid Pragmatic Reading)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'hybrid pragmatic' reading of the
 *   Manifesto kernel, which addresses the legitimacy of marriage commitments
 *   within a religious institution. This reading posits the Manifesto as a
 *   strategic institutional adaptation, deploying prophetic authority to
 *   manage an exogenous crisis (federal legal pressure) while preserving core
 *   theological commitments through scope ambiguity. The institutional
 *   leadership benefits from this flexibility, while rank-and-file members
 *   and theological scholars bear the costs of interpretive uncertainty and
 *   legitimacy ambiguity. This is one of three sibling readings of the
 *   'marriage_commitment_legitimacy' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.65).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.7).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "Marriage Commitment Legitimacy (Hybrid Pragmatic Reading)").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'ffcb65e9-5c5f-48ea-baf3-c55fceb536c0').
narrative_ontology:cs_kernel_codification('ffcb65e9-5c5f-48ea-baf3-c55fceb536c0', formalized).
narrative_ontology:cs_authority_grounding('ffcb65e9-5c5f-48ea-baf3-c55fceb536c0', lineage).
narrative_ontology:cs_interpretation_layer_present('ffcb65e9-5c5f-48ea-baf3-c55fceb536c0').
narrative_ontology:cs_reading_relation('ffcb65e9-5c5f-48ea-baf3-c55fceb536c0', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('ffcb65e9-5c5f-48ea-baf3-c55fceb536c0', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('ffcb65e9-5c5f-48ea-baf3-c55fceb536c0', foundational, prophetic_guidance_allows_adaptation).
narrative_ontology:cs_axiom_status(prophetic_guidance_allows_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('ffcb65e9-5c5f-48ea-baf3-c55fceb536c0', prophetic_guidance_allows_adaptation, theological).
narrative_ontology:cs_axiom('ffcb65e9-5c5f-48ea-baf3-c55fceb536c0', foundational, institutional_survival_is_paramount).
narrative_ontology:cs_axiom_status(institutional_survival_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('ffcb65e9-5c5f-48ea-baf3-c55fceb536c0', institutional_survival_is_paramount, instrumental).
narrative_ontology:cs_reference_frame('ffcb65e9-5c5f-48ea-baf3-c55fceb536c0', doctrinal_continuity_under_prophetic_authority).
narrative_ontology:cs_drift_state('ffcb65e9-5c5f-48ea-baf3-c55fceb536c0', post_manifesto_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ffcb65e9-5c5f-48ea-baf3-c55fceb536c0', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theological_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Navigates external legal pressures while maintaining internal doctrinal coherence. Benefits from the flexibility of a 'prophetic' interpretation that allows for adaptation without explicit theological reversal, preserving institutional legitimacy and avoiding federal sanctions.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Bear the interpretive uncertainty and legitimacy ambiguity of the Manifesto. They are expected to accept the new policy as divinely guided while reconciling it with prior, seemingly contradictory, theological teachings. Exit means abandoning deeply held identity and community.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_members, payer,
    powerless, biographical, identity_locked, local).

% Struggle to provide coherent theological justifications for the Manifesto that satisfy both traditional doctrine and the new pragmatic interpretation. Their intellectual work is constrained by the need to support institutional narratives, often at the cost of academic integrity or clarity.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theological_scholars, payer,
    moderate, generational, constrained, global).

% Exerted legal pressure that precipitated the Manifesto. Observes institutional compliance with federal law, without directly engaging in the theological interpretation. Its actions are an exogenous force that the institution must adapt to.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_government, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates institutional adaptation to exogenous legal pressure while maintaining a semblance of internal theological consistency, allowing the institution to continue operating without direct conflict with federal law.
% TRANSFER_FUNCTION: Transfers interpretive flexibility and institutional survival to the leadership, at the cost of interpretive clarity and doctrinal certainty for rank-and-file members and scholars.
% ABSENT_VOICES: Hardline traditionalists who would demand explicit theological justification for any change, or outright defiance of federal law, are marginalized or silenced within the institutional discourse. Their perspective is not given a legitimate platform.
% DISAPPEARANCE_RATIONALE: If this hybrid pragmatic reading vanished, the institution would face an immediate crisis of legitimacy, either by directly confronting federal law (if the exogenous override reading became dominant) or by undergoing a profound theological schism (if the endogenous reinterpretation reading was rejected by a significant portion of the membership). The institutional structure and its relationship with both its members and external authorities would fundamentally reorganize.
% FOUNDING_PROBLEM: The institution faced an existential threat from federal legal action regarding its marriage practices, which conflicted with core theological tenets.
% FOUNDING_PROBLEM_CORROBORATION: Institutional leadership attests the problem is live, citing ongoing need for legal compliance and doctrinal preservation. External legal analysts corroborate the initial federal pressure, while internal dissenters (though marginalized) attest to the ongoing theological tension.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the cost borne by members and scholars in reconciling the pragmatic shift with prior doctrine. Suppression (0.70) is high due to the institutional pressure to conform to the official narrative and the marginalization of dissenting voices. The theater ratio (0.40) indicates a significant portion of institutional communication and activity is dedicated to maintaining the appearance of prophetic continuity, even as the underlying theological rationale is stretched. The claimed type is 'tangled_rope' because it serves a genuine coordination function (institutional survival and adaptation) but involves asymmetric extraction from its members.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership's perspective, this reading is a necessary and divinely sanctioned act of adaptation, a 'rope' for survival. From the perspective of many rank-and-file members and scholars, it functions as a 'tangled_rope' or even a 'snare,' extracting intellectual honesty and spiritual certainty in exchange for institutional continuity.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership is the primary beneficiary (d=0.0-0.2) as they gain interpretive flexibility and institutional stability. Rank-and-file members and theological scholars are targets (d=0.7-0.9) as they bear the costs of cognitive dissonance and constrained intellectual inquiry. The federal government acts as an external force, not a direct beneficiary or victim of this specific internal interpretive constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (institutional survival and doctrinal preservation) is still live, but the method of achieving it (pragmatic reinterpretation via scope ambiguity) has introduced significant extraction. The classification as a tangled_rope prevents mislabeling this as pure coordination, highlighting the costs borne by members for the institution's adaptation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_authority_legitimacy,
    'To what extent do rank-and-file members genuinely accept the institutional leadership''s interpretive authority to reconcile prior doctrine with the Manifesto''s pragmatic shift?',
    'Longitudinal surveys of member belief and adherence, analysis of internal dissent and schismatic movements, and ethnographic studies of local congregational discourse.',
    'If acceptance is low, the effective suppression and extractiveness on members are higher than measured, as their compliance is coerced rather than consensual. This would push the classification closer to a ''snare'' for the member seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, empirical, 'Assesses the true level of internal legitimacy for the leadership''s interpretation.').

omega_variable(
    theological_coherence_sustainability,
    'Can the ''scope ambiguity'' strategy maintain theological coherence over generations, or will it inevitably lead to further doctrinal erosion or explicit schism?',
    'Historical analysis of similar institutional adaptations in other religious traditions, and future theological developments within the institution itself.',
    'If unsustainable, the long-term costs to the institution''s core identity and the spiritual well-being of its members are higher, indicating a more severe form of extraction that sacrifices long-term integrity for short-term survival.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_coherence_sustainability, conceptual, 'Examines the long-term viability of the interpretive strategy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional pressure, social ostracism for dissent) or internalized (members'' identity-lock, belief in prophetic authority)?',
    'Post-exit suppression trajectory: if suppression persists after members leave the institution, reclassify as partially internalized. Analysis of institutional policies vs. individual member narratives.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making exit less effective as a release valve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for rank-and-file members.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_legitimacy' kernel. It focuses on the institutional adaptation aspect, distinct from readings emphasizing pure federal coercion or pure prophetic revelation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
