% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__prophetic_override_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__prophetic_override_reading
 *   human_readable: Prophetic Override Reading of Eternal Marriage Covenant
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint describes the 'prophetic override' reading of the eternal
 *   marriage covenant within a specific religious tradition. It posits that a
 *   living prophet, guided by continuing revelation, has the authority to
 *   supersede prior divine commandments when circumstances (often external
 *   pressures like federal law) require it for the survival or adaptation of
 *   the institution. This reading allows for doctrinal flexibility but
 *   creates tension for members committed to earlier, immutable
 *   interpretations of revelation.
 *
 * KEY AGENTS:
 *   - church_leadership: Agenda setter (institutional/arbitrage) — receives and interprets revelation, directs policy.
 *   - members_adhering_to_prior_revelation: Payer (powerless/identity_locked) — bears the cost of doctrinal shifts, faces internal conflict.
 *   - dissenting_factions: Payer (moderate/constrained) — actively resists or questions the override, may face excommunication.
 *   - institutional_survival: Beneficiary (analytical/civilizational) — the abstract entity that benefits from adaptation.
 *   - federal_government: Observer (institutional/analytical) — external pressure that triggers the override mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.65).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.78).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Prophetic Override Reading of Eternal Marriage Covenant").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, '680d2409-b14f-4d2f-86f1-c6c73f3154d8').
narrative_ontology:cs_kernel_codification('680d2409-b14f-4d2f-86f1-c6c73f3154d8', formalized).
narrative_ontology:cs_authority_grounding('680d2409-b14f-4d2f-86f1-c6c73f3154d8', lineage).
narrative_ontology:cs_interpretation_layer_present('680d2409-b14f-4d2f-86f1-c6c73f3154d8').
narrative_ontology:cs_reading_relation('680d2409-b14f-4d2f-86f1-c6c73f3154d8', eternal_marriage_covenant__immutable_commandment_reading, influences).
narrative_ontology:cs_reading_relation('680d2409-b14f-4d2f-86f1-c6c73f3154d8', eternal_marriage_covenant__temporal_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('680d2409-b14f-4d2f-86f1-c6c73f3154d8', foundational, living_prophet_supersedes_prior_revelation).
narrative_ontology:cs_axiom_status(living_prophet_supersedes_prior_revelation, holdable).
narrative_ontology:cs_axiom_grounding('680d2409-b14f-4d2f-86f1-c6c73f3154d8', living_prophet_supersedes_prior_revelation, theological).
narrative_ontology:cs_axiom('680d2409-b14f-4d2f-86f1-c6c73f3154d8', secondary, institutional_survival_is_divine_mandate).
narrative_ontology:cs_axiom_status(institutional_survival_is_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('680d2409-b14f-4d2f-86f1-c6c73f3154d8', institutional_survival_is_divine_mandate, theological).
narrative_ontology:cs_reference_frame('680d2409-b14f-4d2f-86f1-c6c73f3154d8', continuing_revelation_as_adaptive_mechanism).
narrative_ontology:cs_drift_state('680d2409-b14f-4d2f-86f1-c6c73f3154d8', contemporary_secular_pluralism, gap(stable, minor, true)).
narrative_ontology:cs_created_at('680d2409-b14f-4d2f-86f1-c6c73f3154d8', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, institutional_survival).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, members_adhering_to_prior_revelation).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, dissenting_factions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because members are required to abandon deeply held beliefs and practices, often tied to their salvation, at the directive of leadership. Suppression (0.78) is high due to the severe social and spiritual penalties for dissent, including excommunication and loss of eternal blessings. Theater ratio (0.4) reflects the performative aspect of 'receiving' revelation that aligns with institutional imperatives, where the divine justification masks pragmatic adaptation. The metrics reflect the cost borne by members when the 'eternal' nature of a commandment is overridden by a living prophet.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of church leadership, this is a necessary coordination mechanism for institutional survival and adaptation, ensuring the church remains relevant and legally viable. From the perspective of members adhering to prior revelation, it is an extractive mechanism that demands conformity to shifting doctrine, undermining the perceived immutability of divine law and extracting their prior commitments.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership is the primary beneficiary (d near 0.0) as they maintain institutional control and ensure survival. Members adhering to prior revelation and dissenting factions are targets (d near 1.0) as they bear the direct costs of doctrinal shifts and face severe penalties for non-compliance. Institutional survival is an abstract beneficiary (d near 0.0) as the constraint directly enables its continuity. The federal government acts as an external force, not directly benefiting or paying from the constraint's internal operation, but influencing its activation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not mandatrophic in the sense of its function atrophying. Instead, its 'mandate' (divine revelation) is actively reinterpreted and overridden to serve a higher-order mandate (institutional survival). The classification as Tangled Rope captures this: it coordinates the church's adaptation to external pressures but does so by extracting from members' prior commitments to 'immutable' doctrine. The 'founding problem' of maintaining a divinely guided community is still 'live', but the 'solution' (prophetic override) creates new forms of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophetic_authority_source,
    'Is prophetic authority truly independent of external pressures, or is it activated by institutional survival imperatives?',
    'Historical analysis of revelation timing relative to external threats and internal dissent; theological exegesis of ''continuing revelation'' in periods of stability vs. crisis.',
    'If primarily activated by survival, the constraint''s ''divine'' justification is a cover for institutional pragmatism, increasing its effective extractiveness and suppression from members who believe it is purely divine. If truly independent, the constraint is a genuine (though still extractive) coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prophetic_authority_source, empirical, 'Source of prophetic authority: divine mandate vs. institutional pragmatism.').

omega_variable(
    reading_of_eternal_marriage_covenant,
    'Is the ''eternal marriage covenant'' an immutable divine law, a mutable prophetic directive, or a temporally accommodated principle?',
    'This constraint instantiates the ''prophetic override'' reading. Sibling readings (''immutable_commandment_reading'', ''temporal_accommodation_reading'') offer alternative structural interpretations. Resolution depends on which interpretive framework is adopted by the community.',
    'If the ''immutable commandment'' reading were adopted, the constraint would appear as a Mountain (unchangeable divine law) with high suppression for those who deviate. If ''temporal accommodation'' were adopted, it would be a Scaffold (temporary suspension for a higher good). This reading (prophetic override) frames it as a Tangled Rope, where authority coordinates adaptation but extracts from those committed to prior forms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_of_eternal_marriage_covenant, conceptual, 'This constraint is one reading of the ''eternal_marriage_covenant'' kernel. The core disagreement is whether divine law can be superseded by living revelation, and under what conditions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(eter_tr_t10, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(eter_tr_t20, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(eter_be_t10, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(eter_be_t20, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(eter_su_t10, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(eter_su_t20, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 20, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'eternal_marriage_covenant' kernel. This 'prophetic override' reading allows for doctrinal shifts via living revelation, contrasting with the 'immutable commandment' reading (which holds polygamy as an eternal law) and the 'temporal accommodation' reading (which suspends practice without renouncing doctrine).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
