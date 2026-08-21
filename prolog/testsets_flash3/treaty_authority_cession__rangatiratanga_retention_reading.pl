% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__rangatiratanga_retention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__rangatiratanga_retention_reading, []).

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
 *   constraint_id: treaty_authority_cession__rangatiratanga_retention_reading
 *   human_readable: Treaty of Waitangi: Rangatiratanga Retention (Māori Text Controls)
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This constraint represents the 'Rangatiratanga Retention' reading of the
 *   Treaty of Waitangi, where the Māori text is paramount (via contra
 *   proferentem). It asserts that Māori ceded only limited governance
 *   (kāwanatanga) to the Crown, retaining full authority (tino
 *   rangatiratanga) over their affairs. The Treaty, under this reading,
 *   establishes a partnership requiring ongoing consent and negotiation, not
 *   a cession of sovereignty. The constraint is classified as a Rope because
 *   it genuinely coordinates shared authority, but its effectiveness has been
 *   historically challenged by competing interpretations and Crown actions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.25).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.4).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Treaty of Waitangi: Rangatiratanga Retention (Māori Text Controls)").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__rangatiratanga_retention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, '5c9438b7-c64f-415a-96a1-6b3b6e1eb197').
narrative_ontology:cs_kernel_codification('5c9438b7-c64f-415a-96a1-6b3b6e1eb197', fixed_text).
narrative_ontology:cs_authority_grounding('5c9438b7-c64f-415a-96a1-6b3b6e1eb197', lineage).
narrative_ontology:cs_interpretation_layer_present('5c9438b7-c64f-415a-96a1-6b3b6e1eb197').
narrative_ontology:cs_reading_relation('5c9438b7-c64f-415a-96a1-6b3b6e1eb197', treaty_authority_cession__crown_cession_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c9438b7-c64f-415a-96a1-6b3b6e1eb197', treaty_authority_cession__biculturalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c9438b7-c64f-415a-96a1-6b3b6e1eb197', treaty_authority_cession__retrospective_snare_exposure, influences).
narrative_ontology:cs_axiom('5c9438b7-c64f-415a-96a1-6b3b6e1eb197', foundational, maori_text_paramountcy).
narrative_ontology:cs_axiom_status(maori_text_paramountcy, holdable).
narrative_ontology:cs_axiom_grounding('5c9438b7-c64f-415a-96a1-6b3b6e1eb197', maori_text_paramountcy, conventional).
narrative_ontology:cs_axiom('5c9438b7-c64f-415a-96a1-6b3b6e1eb197', foundational, tino_rangatiratanga_retained).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_retained, holdable).
narrative_ontology:cs_axiom_grounding('5c9438b7-c64f-415a-96a1-6b3b6e1eb197', tino_rangatiratanga_retained, deontological).
narrative_ontology:cs_reference_frame('5c9438b7-c64f-415a-96a1-6b3b6e1eb197', original_maori_understanding).
narrative_ontology:cs_drift_state('5c9438b7-c64f-415a-96a1-6b3b6e1eb197', contemporary_legal_discourse, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5c9438b7-c64f-415a-96a1-6b3b6e1eb197', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, hapu_iwi).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, crown_as_partner).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, settler_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain tino rangatiratanga (full authority) over their lands, resources, and culture, with the Crown exercising kāwanatanga (governance) only with their consent. They are bound by the Treaty as a sacred covenant, making exit from its framework unthinkable.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, hapu_iwi, beneficiary,
    organized, generational, identity_locked, national).

% Exercises legitimate governance (kāwanatanga) in partnership with hapū/iwi, requiring ongoing consent for actions affecting Māori. This reading constrains the Crown's unilateral power, requiring negotiation and respect for Māori authority.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, crown_as_partner, agenda_setter,
    institutional, generational, constrained, national).

% Interprets the Treaty, increasingly acknowledging the Māori text and the principle of partnership. Their rulings can affirm or challenge the Crown's adherence to this reading, but they do not directly set policy.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, new_zealand_judiciary, observer,
    institutional, generational, analytical, national).

% Benefits from the stability of a functioning state but may perceive the requirements of partnership and consent as an imposition on 'national' sovereignty or a cost to economic development. Their consent is required for political stability.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, settler_population, payer,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for shared authority and mutual obligation between Māori (hapū/iwi) and the Crown, enabling peaceful coexistence and ordered governance while preserving indigenous self-determination.
% TRANSFER_FUNCTION: Transfers limited governance authority (kāwanatanga) to the Crown, while retaining full Māori authority (tino rangatiratanga). It mandates a reciprocal flow of respect, consultation, and consent for legitimate Crown action.
% ABSENT_VOICES: Those who insist on a unitary, Westminster-style parliamentary sovereignty would object, as this reading fundamentally challenges the Crown's unilateral authority. They are present in political discourse but structurally excluded from the Treaty's interpretive framework itself.
% DISAPPEARANCE_RATIONALE: If this reading of the Treaty vanished, the constitutional basis for Māori rights and the Crown's obligations would collapse, leading to profound political instability, renewed land disputes, and a crisis of legitimacy for the New Zealand state. The entire legal and social fabric would be forced to reorganize.
% FOUNDING_PROBLEM: To establish a basis for British settlement and governance in New Zealand while protecting Māori sovereignty and property rights, preventing inter-tribal warfare, and ensuring peace.
% FOUNDING_PROBLEM_CORROBORATION: Māori leaders and scholars consistently attest that the problem of protecting rangatiratanga and ensuring equitable partnership remains live. International indigenous rights bodies and historical commissions also corroborate the ongoing nature of this challenge, distinct from the Crown's self-serving narratives.
narrative_ontology:disappearance_verdict(treaty_authority_cession__rangatiratanga_retention_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__rangatiratanga_retention_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(treaty_authority_cession__rangatiratanga_retention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).
:- end_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because this reading fundamentally limits Crown extraction by requiring consent. Suppression is moderate (0.4) reflecting the historical and ongoing struggle to enforce this reading against Crown resistance, but it is not a pure snare as Māori agency and resistance are strong. Theater ratio is low (0.1) because the commitment to partnership, while often breached, is a genuine aspiration within this reading, not mere performance. Resistance is high (0.7) reflecting the active and sustained efforts by Māori to uphold this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of hapū/iwi, this reading is a foundational Rope, a sacred covenant for partnership. From the perspective of a Crown-centric 'cession' reading, this would be seen as an illegitimate constraint on sovereign power. The engine's classification of Rope reflects the structural reality of shared authority and mutual obligation inherent in this specific reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Hapū/iwi are primary beneficiaries, as their authority is affirmed. The Crown, when acting legitimately within this partnership, is also a beneficiary of ordered governance. The settler population may experience this as a 'cost' to their perceived unilateral sovereignty, making them payers. The New Zealand judiciary acts as an observer, increasingly affirming this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the Treaty as a pure Snare by emphasizing the retained Māori authority and the partnership obligation. While historical Crown actions often operated as a Snare (as seen in the 'retrospective_snare_exposure' reading), this specific reading of the Treaty itself functions as a coordination mechanism for shared governance, albeit one that requires constant defense against alternative interpretations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_text_interpretation_ambiguity,
    'Is the Māori text''s meaning of ''kāwanatanga'' (governance) and ''tino rangatiratanga'' (full authority) definitively understood and accepted by all parties, or does ambiguity persist?',
    'Ongoing legal precedent, political negotiation, and public education campaigns. Resolution would involve a shared, legally binding interpretation of these terms.',
    'If ambiguity persists, the constraint''s effectiveness as a Rope is undermined, allowing alternative, more extractive readings to gain traction. If resolved, the Rope classification is strengthened, and Crown actions become more clearly accountable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_text_interpretation_ambiguity, conceptual, 'Ambiguity in key Māori terms in the Treaty text.').

omega_variable(
    crown_adherence_to_partnership,
    'To what extent does the Crown genuinely adhere to the principles of partnership and consent required by this reading, versus merely performing consultation while pursuing unilateral agendas?',
    'Empirical analysis of Crown policy outcomes, Māori satisfaction with consultation processes, and independent audits of partnership agreements. Consistent negative outcomes would indicate performative adherence.',
    'If adherence is largely performative, the constraint''s effective extractiveness increases, and its classification drifts towards a Tangled Rope or even a Snare, as the coordination function becomes cover for extraction. Genuine adherence reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crown_adherence_to_partnership, empirical, 'Gap between Crown''s stated commitment to partnership and actual practice.').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading of the ''treaty_authority_cession'' kernel. What would change structurally if a sibling reading (e.g., ''crown_cession_reading'') were adopted as the dominant interpretation?',
    'A shift in legal precedent or legislative action explicitly endorsing a different reading. This is a conceptual omega, resolved by observing which interpretive framework gains legal and political dominance.',
    'If the ''crown_cession_reading'' became dominant, this constraint would effectively cease to exist as a Rope, replaced by a Snare (from the Māori perspective) or a Mountain (from the Crown''s perspective of inherent sovereignty). The victim set would expand to include all hapū/iwi.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of alternative kernel readings on constraint structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(trea_tr_t1880, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1880, 0.15).
narrative_ontology:measurement(trea_tr_t1920, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(trea_tr_t1960, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(trea_tr_t2024, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1840, 0.1).
narrative_ontology:measurement(trea_be_t1880, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1880, 0.2).
narrative_ontology:measurement(trea_be_t1920, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1920, 0.3).
narrative_ontology:measurement(trea_be_t1960, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1960, 0.25).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2000, 0.2).
narrative_ontology:measurement(trea_be_t2024, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1840, 0.3).
narrative_ontology:measurement(trea_su_t1880, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1880, 0.5).
narrative_ontology:measurement(trea_su_t1920, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(trea_su_t1960, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1960, 0.45).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(trea_su_t2024, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, identity_coordination).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__biculturalism_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Treaty of Waitangi's authority cession kernel. Each reading has a different ε and structural properties, reflecting the contested nature of the Treaty's interpretation. This reading emphasizes Māori retention of tino rangatiratanga.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
