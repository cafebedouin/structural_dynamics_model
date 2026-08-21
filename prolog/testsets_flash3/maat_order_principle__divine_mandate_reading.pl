% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__divine_mandate_reading, []).

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
 *   constraint_id: maat_order_principle__divine_mandate_reading
 *   human_readable: Ma'at as Divine Mandate of Pharaoh
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This constraint represents the 'divine mandate' reading of Ma'at in
 *   ancient Egypt, where cosmic order flows through the Pharaoh, who is
 *   considered incapable of violating it. This reading places the Pharaoh
 *   outside the system of constraint, making him the source rather than the
 *   subject of Ma'at. All royal actions are thus justified as inherently
 *   aligned with cosmic necessity, leading to high extraction and suppression
 *   of any alternative interpretations or challenges to royal authority. The
 *   claimed type is 'snare' because the coordination story (cosmic order) is
 *   cover for absolute extraction, maintained by severe suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.9).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.95).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, snare).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Ma'at as Divine Mandate of Pharaoh").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, '8cf98b0e-e4af-4b8f-9653-5a5b397c8ef0').
narrative_ontology:cs_kernel_codification('8cf98b0e-e4af-4b8f-9653-5a5b397c8ef0', formalized).
narrative_ontology:cs_authority_grounding('8cf98b0e-e4af-4b8f-9653-5a5b397c8ef0', extraction).
narrative_ontology:cs_interpretation_layer_present('8cf98b0e-e4af-4b8f-9653-5a5b397c8ef0').
narrative_ontology:cs_reading_relation('8cf98b0e-e4af-4b8f-9653-5a5b397c8ef0', maat_order_principle__reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('8cf98b0e-e4af-4b8f-9653-5a5b397c8ef0', maat_order_principle__distributed_maintenance_reading, forecloses).
narrative_ontology:cs_axiom('8cf98b0e-e4af-4b8f-9653-5a5b397c8ef0', foundational, pharaoh_is_divine_source_of_maat).
narrative_ontology:cs_axiom_status(pharaoh_is_divine_source_of_maat, holdable).
narrative_ontology:cs_axiom_grounding('8cf98b0e-e4af-4b8f-9653-5a5b397c8ef0', pharaoh_is_divine_source_of_maat, theological).
narrative_ontology:cs_axiom('8cf98b0e-e4af-4b8f-9653-5a5b397c8ef0', foundational, royal_action_cannot_violate_maat).
narrative_ontology:cs_axiom_status(royal_action_cannot_violate_maat, holdable).
narrative_ontology:cs_axiom_grounding('8cf98b0e-e4af-4b8f-9653-5a5b397c8ef0', royal_action_cannot_violate_maat, deontological).
narrative_ontology:cs_reference_frame('8cf98b0e-e4af-4b8f-9653-5a5b397c8ef0', pharaonic_absolute_divine_authority).
narrative_ontology:cs_drift_state('8cf98b0e-e4af-4b8f-9653-5a5b397c8ef0', contemporary_historical_analysis, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('8cf98b0e-e4af-4b8f-9653-5a5b397c8ef0', '').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, priestly_elite).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, commoners).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, scribal_bureaucracy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, scribal_bureaucracy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The divine embodiment of Ma'at, from whom cosmic order flows. By definition, cannot violate Ma'at. Benefits from absolute authority and the justification of all royal actions as inherently aligned with cosmic order. Exit is unthinkable, as it would dissolve the cosmic order itself.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, pharaoh, agenda_setter,
    institutional, generational, arbitrage, national).

% Interprets and propagates the divine mandate, reinforcing the Pharaoh's unique status. Benefits from proximity to power, ritual authority, and material support derived from the state. Their role is to maintain the theological framework that justifies the Pharaoh's position.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, priestly_elite, beneficiary,
    organized, generational, constrained, national).

% Subject to the Pharaoh's absolute rule, which is presented as the manifestation of Ma'at. Bear the costs of royal decrees, taxation, and labor without recourse, as any challenge is framed as cosmic disorder. Their only 'exit' is rebellion, which is met with overwhelming force and religious condemnation.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, commoners, payer,
    powerless, biographical, trapped, local).

% Administers the Pharaoh's will, enforcing laws and collecting taxes. While benefiting from their elevated status relative to commoners, they are ultimately instruments of the Pharaoh's divine mandate and cannot question its source or legitimacy. Their identity is fused with the state apparatus.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, scribal_bureaucracy, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, scribal_bureaucracy, beneficiary).

% Observe the Egyptian system from an external perspective, sometimes engaging in diplomacy or conflict. They are not subject to Ma'at but interact with its political manifestations.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, foreign_powers, observer,
    institutional, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a singular, unquestionable source of authority (Pharaoh) for all societal organization, ensuring stability and preventing internal dissent by grounding all law and order in cosmic necessity.
% TRANSFER_FUNCTION: Transfers absolute authority and material wealth from the entire society to the Pharaoh and the priestly elite, justified as the necessary maintenance of cosmic order.
% ABSENT_VOICES: Any philosophical or political traditions that would challenge the divine right of kings or propose alternative sources of legitimacy for governance are suppressed and absent from the discourse. Their arguments for distributed responsibility or reciprocal obligations would be deemed heresy.
% DISAPPEARANCE_RATIONALE: If the divine mandate of Ma'at vanished, the Pharaoh's absolute authority would collapse, leading to immediate political instability, challenges to the existing social hierarchy, and a complete reorganization of power structures and religious justifications.
% FOUNDING_PROBLEM: To establish a stable, unified kingdom after periods of fragmentation, requiring a powerful, unquestionable central authority to maintain order and manage resources (e.g., Nile floods).
% FOUNDING_PROBLEM_CORROBORATION: The priestly elite and the Pharaoh's court consistently attest that the problem of maintaining cosmic order and societal stability is perpetually live. Historical records and archaeological evidence from outside the benefiting parties corroborate the initial need for strong central authority during early dynastic periods, but do not corroborate the divine mandate as the only solution or its continued necessity in its absolute form.
narrative_ontology:disappearance_verdict(maat_order_principle__divine_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__divine_mandate_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__divine_mandate_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(maat_order_principle__divine_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__divine_mandate_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__divine_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__divine_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.9) because the Pharaoh and priestly elite capture nearly all surplus, justified by their unique role in maintaining Ma'at. Suppression is extremely high (0.95) as any challenge to the Pharaoh's divine authority is met with severe punishment and religious condemnation, effectively trapping commoners. Theater ratio is high (0.6) because while some rituals genuinely aim to maintain cosmic balance, a significant portion of the performance serves to reinforce the Pharaoh's unchallengeable status and justify extraction. Accessibility collapse is high (0.8) as the ideological framework leaves virtually no conceptual space for alternatives, and resistance is low (0.1) due to the overwhelming power and ideological control.
 *
 * PERSPECTIVAL GAP:
 *   From the Pharaoh's and priestly elite's perspective, this is a 'mountain' or 'rope' – an unchangeable cosmic law or a necessary coordination mechanism. From the commoners' perspective, it is a 'snare' – a system of absolute extraction and suppression. The engine's classification will reflect the latter due to the high extractiveness and suppression metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The Pharaoh is the ultimate beneficiary and agenda-setter (d=0.0), as Ma'at flows through him, justifying his absolute power. The priestly elite are also strong beneficiaries (d=0.15), deriving their authority and resources from their role in interpreting and propagating this mandate. Commoners and the scribal bureaucracy are victims (d=0.9 and d=0.8 respectively), bearing the costs of extraction and having their actions constrained by a system they cannot challenge. The scribal bureaucracy, while having some status, is identity-locked into the system.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling absolute extraction as natural law or coordination. The 'divine mandate' reading of Ma'at, while presented as a cosmic necessity, functions as a snare by concentrating power and resources through a coercive, ideologically enforced structure. The high theater ratio indicates that much of the 'maintenance of Ma'at' is performative justification for this extraction, rather than genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pharaoh_as_source_vs_subject,
    'Is the Pharaoh structurally outside the Ma''at constraint system as its source, or is he also subject to its principles?',
    'Analysis of historical texts and legal codes that explicitly describe mechanisms for constraining royal power or holding Pharaoh accountable to Ma''at, or the absence thereof.',
    'If the Pharaoh is also subject, the constraint''s extractiveness and suppression would be lower, and its claimed type might shift towards a ''tangled_rope'' or ''rope'' for the Pharaoh''s seat, as he would bear some cost of compliance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pharaoh_as_source_vs_subject, conceptual, 'Ambiguity regarding the Pharaoh''s position relative to Ma''at.').

omega_variable(
    cosmic_necessity_vs_political_justification,
    'To what extent is the justification of extraction as ''cosmic necessity'' a genuine belief system versus a political tool for maintaining power?',
    'Comparative analysis with other ancient civilizations'' justifications for authority, and internal textual criticism for inconsistencies or explicit political motivations in religious texts.',
    'If primarily a political tool, the ''theater_ratio'' would be higher, and the ''suppression'' metric would be more clearly understood as coercive rather than ideological, reinforcing the ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cosmic_necessity_vs_political_justification, empirical, 'Ambiguity of cosmic necessity as genuine belief or political tool.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., overwhelming force, lack of alternatives) or internalized (e.g., belief in divine order, fear of cosmic chaos)?',
    'Analysis of commoner narratives, folk tales, and archaeological evidence for signs of covert resistance or alternative belief systems. If suppression persists after the extractive mechanism is removed (e.g., during periods of weak central authority), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the snare more robust. If purely structural, removing the Pharaoh''s power would immediately lead to widespread resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(maat_tr_t25, maat_order_principle__divine_mandate_reading, theater_ratio, 25, 0.55).
narrative_ontology:measurement(maat_tr_t50, maat_order_principle__divine_mandate_reading, theater_ratio, 50, 0.6).
narrative_ontology:measurement(maat_tr_t75, maat_order_principle__divine_mandate_reading, theater_ratio, 75, 0.62).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__divine_mandate_reading, theater_ratio, 100, 0.6).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(maat_be_t25, maat_order_principle__divine_mandate_reading, base_extractiveness, 25, 0.88).
narrative_ontology:measurement(maat_be_t50, maat_order_principle__divine_mandate_reading, base_extractiveness, 50, 0.9).
narrative_ontology:measurement(maat_be_t75, maat_order_principle__divine_mandate_reading, base_extractiveness, 75, 0.91).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__divine_mandate_reading, base_extractiveness, 100, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(maat_su_t25, maat_order_principle__divine_mandate_reading, suppression_requirement, 25, 0.92).
narrative_ontology:measurement(maat_su_t50, maat_order_principle__divine_mandate_reading, suppression_requirement, 50, 0.95).
narrative_ontology:measurement(maat_su_t75, maat_order_principle__divine_mandate_reading, suppression_requirement, 75, 0.96).
narrative_ontology:measurement(maat_su_t100, maat_order_principle__divine_mandate_reading, suppression_requirement, 100, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
