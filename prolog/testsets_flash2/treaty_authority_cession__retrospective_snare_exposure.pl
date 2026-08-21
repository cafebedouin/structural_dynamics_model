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
 *   constraint_id: treaty_authority_cession__retrospective_snare_exposure
 *   human_readable: Treaty of Waitangi: Retrospective Snare of Textual Divergence
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This constraint describes the Treaty of Waitangi as a retrospective
 *   snare, where the textual divergence between the Māori and English
 *   versions of the Treaty constituted a covert extraction mechanism. Māori
 *   chiefs, signing the Māori text, could not have assented to the full
 *   cession of sovereignty claimed by the English text. This fundamental
 *   mistranslation and misrepresentation enabled subsequent land transfers
 *   and legislative overrides, which are now visible as extraction operating
 *   under the guise of legitimate treaty authority. The constraint's claimed
 *   type is 'snare' because the coordination story (establishing governance
 *   and protecting Māori) was a cover for an underlying, actively enforced
 *   extraction of sovereignty and land, with identifiable victims and
 *   suppressed alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.92).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.88).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.92).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty of Waitangi: Retrospective Snare of Textual Divergence").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, '26c5cd67-2e8e-46bb-9c46-ad1e32e0a3d9').
narrative_ontology:cs_kernel_codification('26c5cd67-2e8e-46bb-9c46-ad1e32e0a3d9', fixed_text).
narrative_ontology:cs_authority_grounding('26c5cd67-2e8e-46bb-9c46-ad1e32e0a3d9', extraction).
narrative_ontology:cs_interpretation_layer_present('26c5cd67-2e8e-46bb-9c46-ad1e32e0a3d9').
narrative_ontology:cs_reading_relation('26c5cd67-2e8e-46bb-9c46-ad1e32e0a3d9', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('26c5cd67-2e8e-46bb-9c46-ad1e32e0a3d9', treaty_authority_cession__rangatiratanga_retention_reading, coexists_with).
narrative_ontology:cs_axiom('26c5cd67-2e8e-46bb-9c46-ad1e32e0a3d9', foundational, textual_divergence_as_extraction).
narrative_ontology:cs_axiom_status(textual_divergence_as_extraction, holdable).
narrative_ontology:cs_axiom_grounding('26c5cd67-2e8e-46bb-9c46-ad1e32e0a3d9', textual_divergence_as_extraction, empirically_contingent).
narrative_ontology:cs_axiom('26c5cd67-2e8e-46bb-9c46-ad1e32e0a3d9', foundational, absence_of_informed_consent_to_cession).
narrative_ontology:cs_axiom_status(absence_of_informed_consent_to_cession, holdable).
narrative_ontology:cs_axiom_grounding('26c5cd67-2e8e-46bb-9c46-ad1e32e0a3d9', absence_of_informed_consent_to_cession, deontological).
narrative_ontology:cs_reference_frame('26c5cd67-2e8e-46bb-9c46-ad1e32e0a3d9', maori_sovereignty_pre_treaty).
narrative_ontology:cs_drift_state('26c5cd67-2e8e-46bb-9c46-ad1e32e0a3d9', post_treaty_implementation_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('26c5cd67-2e8e-46bb-9c46-ad1e32e0a3d9', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, settler_government).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_signatories).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendants).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, iwi_hapu_collectives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited directly from the perceived cession of sovereignty, enabling large-scale land acquisition and settlement under the English text's interpretation. This apparatus was the primary recipient of the 'gains' from the textual divergence.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, beneficiary,
    institutional, generational, arbitrage, national).

% Established and maintained legislative and administrative authority over Aotearoa New Zealand, operating under the assumption of full sovereignty ceded by Māori chiefs. This government actively enforced laws and policies that derived from the English text's interpretation, often overriding Māori customary law and authority.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, settler_government, agenda_setter,
    institutional, generational, constrained, national).

% Signed the Māori text of the Treaty, believing they were assenting to a form of governance (kāwanatanga) while retaining their inherent authority (tino rangatiratanga). They were unknowingly subjected to a full cession of sovereignty under the English text, leading to loss of land and self-determination.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_signatories, payer,
    powerless, biographical, trapped, local).

% Inherited the consequences of the textual divergence, experiencing ongoing dispossession, marginalization, and the erosion of their customary authority. Their identity is deeply tied to the land and the Treaty, making 'exit' from the colonial system a complex, identity-locked struggle for recognition and redress.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_descendants, payer,
    organized, generational, identity_locked, national).

% Traditional Māori tribal and sub-tribal groups whose collective lands, resources, and self-governance structures were undermined by the Crown's assertion of full sovereignty. They continue to bear the costs of historical grievances and seek redress through various legal and political channels.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, iwi_hapu_collectives, payer,
    organized, generational, constrained, regional).

% Interprets the Treaty and its implications, often navigating the tension between the English and Māori texts. While not directly benefiting from the original extraction, its rulings can either perpetuate or mitigate the effects of the textual divergence.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, new_zealand_judiciary, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly, to coordinate the establishment of British governance in Aotearoa New Zealand, ensuring peace and order between Māori and settlers, and regulating land sales.
% TRANSFER_FUNCTION: Transferred perceived sovereignty and vast tracts of land from Māori to the British Crown and subsequent settler governments, while simultaneously transferring a limited form of 'kāwanatanga' (governance) to the Crown in the Māori text, which was then interpreted as full sovereignty in the English text.
% ABSENT_VOICES: The full implications of the English text's cession of sovereignty were absent from the understanding of Māori signatories. Had they understood 'kāwanatanga' as full sovereignty, they would not have signed. Their true assent was suppressed by the linguistic and conceptual chasm.
% DISAPPEARANCE_RATIONALE: If the retrospective snare of textual divergence were truly resolved and its effects undone, the entire constitutional and land ownership framework of Aotearoa New Zealand would need to be fundamentally renegotiated, leading to a radical rearrangement of power, resources, and governance structures.
% FOUNDING_PROBLEM: The British Crown sought to establish legitimate authority over Aotearoa New Zealand to protect Māori from unscrupulous settlers, secure British interests, and facilitate orderly settlement, while Māori sought to protect their lands and authority from unchecked settler encroachment.
% FOUNDING_PROBLEM_CORROBORATION: The Crown's original intent is attested by historical documents and colonial policy. Māori and their descendants, supported by historical scholarship and the Waitangi Tribunal, attest that the problem of protecting Māori authority was never truly solved by the Treaty as implemented, and indeed, the Treaty itself became the mechanism of its erosion.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(treaty_authority_cession__retrospective_snare_exposure, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__retrospective_snare_exposure, 0.92, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is very high (0.92) because the core of the constraint is the systematic dispossession of Māori land and authority. Suppression is also very high (0.88) due to the Crown's legislative power, military force, and legal interpretations that actively suppressed Māori resistance and alternative claims. The theater ratio is high (0.65) because the 'partnership' and 'protection' aspects of the Treaty, as presented by the Crown, increasingly became a performance masking the ongoing extraction. Accessibility collapse is significant (0.75) as Māori alternatives for self-governance and land retention were systematically undermined. Resistance is high (0.70) reflecting continuous Māori efforts to challenge the Crown's interpretation and seek redress.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Crown and settler government, the Treaty was a legitimate act of cession, establishing British sovereignty and enabling 'progress.' From the Māori perspective, and retrospectively, it was a profound act of deception and dispossession, a snare that leveraged linguistic and conceptual differences to extract what could not have been legitimately ceded. The engine's classification as a snare from the victim's seat captures this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown land-purchasing apparatus and the settler government are the primary beneficiaries, gaining land and authority (low d). Māori signatories, their descendants, and iwi/hapu collectives are the clear victims, losing land, sovereignty, and cultural integrity (high d). The New Zealand judiciary acts as an observer, its interpretations shaping the ongoing impact of the snare.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_effect_ambiguity,
    'Was the textual divergence a deliberate act of deception by the Crown, or an unavoidable consequence of linguistic and conceptual incommensurability?',
    'Forensic historical analysis of Crown communications and policy documents from the period, examining evidence of awareness of the divergence and its implications for Māori understanding.',
    'If deliberate, it strengthens the ''snare'' classification by highlighting malicious intent. If unavoidable, it points to a structural snare arising from colonial power dynamics and linguistic barriers, rather than explicit malice, but the extractive outcome remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_effect_ambiguity, empirical, 'Ambiguity regarding the intentionality of the textual divergence.').

omega_variable(
    reparations_sufficiency,
    'Are current and proposed Treaty settlements and legislative redress mechanisms sufficient to resolve the historical extraction and restore Māori tino rangatiratanga?',
    'Longitudinal study of the impact of settlements on Māori well-being, economic self-sufficiency, and exercise of self-determination, assessed against Māori-defined metrics of success.',
    'If insufficient, the ''snare'' continues to operate in its effects, requiring further structural change. If sufficient, the constraint may transition towards a ''scaffold'' (transitional support) or ''rope'' (genuine coordination) as historical injustices are genuinely addressed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reparations_sufficiency, preference, 'Uncertainty regarding the efficacy and completeness of redress for historical Treaty breaches.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (legislative override, legal precedent) or internalized (intergenerational trauma, cultural erosion leading to reduced agency)?',
    'Post-redress trajectory: if suppression persists after structural barriers are removed, reclassify as partially internalized. Qualitative research on Māori experiences of agency and self-determination.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This would imply that legal and political redress alone are insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of colonial history.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1840, 0.3).
narrative_ontology:measurement(trea_tr_t1880, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1880, 0.5).
narrative_ontology:measurement(trea_tr_t1920, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1920, 0.6).
narrative_ontology:measurement(trea_tr_t1960, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1960, 0.7).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2000, 0.68).
narrative_ontology:measurement(trea_tr_t2024, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1840, 0.7).
narrative_ontology:measurement(trea_be_t1880, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1880, 0.85).
narrative_ontology:measurement(trea_be_t1920, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1920, 0.9).
narrative_ontology:measurement(trea_be_t1960, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1960, 0.95).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2000, 0.93).
narrative_ontology:measurement(trea_be_t2024, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1840, 0.6).
narrative_ontology:measurement(trea_su_t1880, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1880, 0.8).
narrative_ontology:measurement(trea_su_t1920, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1920, 0.9).
narrative_ontology:measurement(trea_su_t1960, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1960, 0.95).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(trea_su_t2024, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__retrospective_snare_exposure, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, maori_land_court_jurisdiction).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, crown_forest_assets_act).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, fisheries_settlement_act).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'treaty_authority_cession' kernel. This 'retrospective_snare_exposure' reading focuses on the extractive nature of the textual divergence itself, contrasting with the 'crown_cession_reading' (which asserts full sovereignty cession) and the 'rangatiratanga_retention_reading' (which asserts Māori retention of full authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
