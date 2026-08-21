% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__retrospective_snare_exposure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__retrospective_snare_exposure, []).

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
 *   constraint_id: treaty_authority_cession__retrospective_snare_exposure
 *   human_readable: Treaty Authority Cession: Retrospective Snare Exposure
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This constraint describes the Treaty of Waitangi as an extraction
 *   mechanism, where the fundamental textual divergence between the Māori and
 *   English versions of the Treaty meant that Māori chiefs could not have
 *   assented to the English claim of full sovereignty. The subsequent land
 *   transfers and legislative overrides by the Crown are thus revealed as
 *   extraction operating under the cover of a mistranslation, making the
 *   Treaty's operation a snare from its inception, visible retrospectively.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.88).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.92).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.88).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty Authority Cession: Retrospective Snare Exposure").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, '57408e19-ac33-47a2-a48d-4ebf02453f45').
narrative_ontology:cs_kernel_codification('57408e19-ac33-47a2-a48d-4ebf02453f45', fixed_text).
narrative_ontology:cs_authority_grounding('57408e19-ac33-47a2-a48d-4ebf02453f45', extraction).
narrative_ontology:cs_interpretation_layer_present('57408e19-ac33-47a2-a48d-4ebf02453f45').
narrative_ontology:cs_reading_relation('57408e19-ac33-47a2-a48d-4ebf02453f45', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('57408e19-ac33-47a2-a48d-4ebf02453f45', treaty_authority_cession__rangatiratanga_retention_reading, influences).
narrative_ontology:cs_axiom('57408e19-ac33-47a2-a48d-4ebf02453f45', foundational, textual_divergence_constitutes_extraction).
narrative_ontology:cs_axiom_status(textual_divergence_constitutes_extraction, holdable).
narrative_ontology:cs_axiom_grounding('57408e19-ac33-47a2-a48d-4ebf02453f45', textual_divergence_constitutes_extraction, empirically_contingent).
narrative_ontology:cs_axiom('57408e19-ac33-47a2-a48d-4ebf02453f45', foundational, informed_consent_absent_for_sovereignty_transfer).
narrative_ontology:cs_axiom_status(informed_consent_absent_for_sovereignty_transfer, holdable).
narrative_ontology:cs_axiom_grounding('57408e19-ac33-47a2-a48d-4ebf02453f45', informed_consent_absent_for_sovereignty_transfer, deontological).
narrative_ontology:cs_reference_frame('57408e19-ac33-47a2-a48d-4ebf02453f45', maori_pre_treaty_sovereignty).
narrative_ontology:cs_drift_state('57408e19-ac33-47a2-a48d-4ebf02453f45', post_treaty_legislative_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('57408e19-ac33-47a2-a48d-4ebf02453f45', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, settler_government).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_signatories).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, contemporary_maori_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Signed the Māori text of the Treaty, understanding it to grant the Crown limited governance (kāwanatanga) while retaining full Māori authority (tino rangatiratanga). They were structurally unable to assent to the English text's claim of full sovereignty due to linguistic and conceptual divergence, making them unwitting targets of extraction.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_signatories, payer,
    powerless, biographical, trapped, national).

% Inherited the consequences of the original textual divergence, including extensive land loss, erosion of traditional authority, and ongoing struggle for recognition of their retained sovereignty. Their identity is deeply tied to the Treaty and its original Māori meaning.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_descendants, payer,
    powerless, generational, identity_locked, national).

% Directly benefited from the Crown's interpretation of the Treaty as a cession of sovereignty, enabling the acquisition of vast tracts of Māori land for settlement and development. This apparatus actively leveraged the perceived legal authority derived from the English text.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Established and maintained its authority based on the English text of the Treaty, passing legislation and implementing policies that systematically undermined Māori authority and facilitated land alienation. Its legitimacy was grounded in the assertion of full sovereignty.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, settler_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Retrospectively analyze the textual divergence, the historical context of the Treaty's signing, and the subsequent legal and political actions that constituted the extractive mechanism. They provide critical insights into the structural nature of the snare.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, analytical_historians_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% Actively work to expose the historical extraction and achieve redress for Māori. While they benefit from growing recognition of the Treaty's Māori text, they remain constrained by the enduring institutional power of the settler government and its historical interpretations.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, contemporary_maori_advocates, beneficiary,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ostensible coordination function was to establish a framework for British settlement and governance in New Zealand, while protecting Māori rights and authority, as understood by the Māori text.
% TRANSFER_FUNCTION: Transferred perceived sovereignty and vast tracts of land from Māori to the Crown, enabled by the fundamental textual divergence between the Māori and English versions of the Treaty, and subsequently enforced through legislative and administrative actions.
% ABSENT_VOICES: Māori chiefs who did not sign the Treaty, future generations of Māori, and any party who could have fully understood and articulated the implications of the English sovereignty claim at the time of signing. Their absence allowed the extractive mechanism to operate covertly.
% DISAPPEARANCE_RATIONALE: If the Crown's historical assertion of sovereignty via the English text vanished, the entire constitutional, legal, and land ownership framework of New Zealand would be fundamentally challenged. It would necessitate a complete re-evaluation of historical claims, land titles, and current governance structures, leading to a profound reorganization of the nation's foundations.
% FOUNDING_PROBLEM: The Crown's stated problem was to establish legitimate British sovereignty and facilitate orderly settlement in New Zealand, while ostensibly protecting Māori interests and preventing uncontrolled land sales.
% FOUNDING_PROBLEM_CORROBORATION: Crown historical narratives often present the problem as one of establishing order and legitimate governance in a 'new' colony. Māori scholars and advocates, supported by independent linguistic and historical analysis, argue that the true underlying problem was colonial expansion and land acquisition, with the Treaty serving as a deceptive instrument for this purpose. This reading is corroborated by linguistic analysis of the Māori text and historical records of subsequent land transactions and legislative actions.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(treaty_authority_cession__retrospective_snare_exposure, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__retrospective_snare_exposure, 0.88, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__retrospective_snare_exposure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__retrospective_snare_exposure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the Crown's interpretation led to massive land alienation and the systematic undermining of Māori authority, representing a profound transfer of wealth and power. Suppression is also very high (0.92) as the mechanism was covert at the point of signing and later enforced through legislative and military means, actively suppressing Māori resistance and alternative interpretations. The theater ratio is low (0.15) because the initial act of signing was a genuine event, but its function as a snare was a structural outcome of the divergence, not a performance. The 'partnership' narrative that emerged later served to obscure, rather than constitute, the primary extractive function. Accessibility collapse is high (0.85) because the linguistic trap and subsequent legislative actions effectively eliminated Māori's ability to exit or resist the Crown's asserted sovereignty.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's historical perspective, the Treaty was a legitimate act of cession and coordination, establishing British sovereignty. From the perspective of this 'retrospective snare exposure' reading, the same event is re-framed as a covert, structurally enforced extraction, where the textual divergence itself was the primary mechanism of the snare. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown's land-purchasing apparatus and the settler government are the clear beneficiaries, leveraging the English text to assert sovereignty and acquire land. Māori signatories and their descendants are the victims, bearing the costs of land loss and diminished authority due to a cession they could not have understood. Analytical observers and contemporary Māori advocates work to expose and redress this historical imbalance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_divergence_intentionality,
    'Was the textual divergence between the Māori and English versions of the Treaty of Waitangi an intentional act of deception by the Crown, or a genuine misunderstanding of incommensurable concepts?',
    'Further historical and linguistic research into the drafting process, communications between Crown agents, and contemporary Māori understanding of the terms used. Analysis of subsequent Crown actions for consistency with either interpretation.',
    'If intentional, it strengthens the ''snare'' classification by demonstrating malicious intent. If unintentional but exploited, it still supports the ''snare'' classification by highlighting the structural vulnerability created by incommensurability, but shifts the moral culpability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_divergence_intentionality, empirical, 'Ambiguity regarding the intent behind the Treaty''s textual divergence.').

omega_variable(
    sovereignty_concept_incommensurability,
    'To what extent were the concepts of ''sovereignty'' (as understood by the British Crown) and ''rangatiratanga'' (as understood by Māori) truly incommensurable, making genuine consent to cession impossible?',
    'Deep comparative linguistic and conceptual analysis of 19th-century British legal thought and Māori political philosophy, drawing on a wider range of historical texts and oral traditions.',
    'If incommensurable, it reinforces the structural impossibility of Māori assenting to the English text''s meaning, strengthening the ''snare'' classification regardless of intent. If partially commensurable, it introduces nuance regarding the degree of misunderstanding versus deliberate misrepresentation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_concept_incommensurability, conceptual, 'Conceptual gap between British and Māori understandings of authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(trea_tr_t1870, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1870, 0.1).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(trea_tr_t1940, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1940, 0.2).
narrative_ontology:measurement(trea_tr_t1980, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(trea_tr_t2024, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1840, 0.65).
narrative_ontology:measurement(trea_be_t1870, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1870, 0.78).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1900, 0.85).
narrative_ontology:measurement(trea_be_t1940, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1940, 0.89).
narrative_ontology:measurement(trea_be_t1980, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1980, 0.9).
narrative_ontology:measurement(trea_be_t2024, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1840, 0.7).
narrative_ontology:measurement(trea_su_t1870, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1870, 0.82).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1900, 0.9).
narrative_ontology:measurement(trea_su_t1940, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1940, 0.95).
narrative_ontology:measurement(trea_su_t1980, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1980, 0.93).
narrative_ontology:measurement(trea_su_t2024, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__retrospective_snare_exposure, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'treaty_authority_cession' kernel. It focuses on the textual divergence as the core extraction mechanism, providing a structural explanation for the outcomes contested by other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
