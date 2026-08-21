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
 *   human_readable: Treaty of Waitangi: Retrospective Snare Exposure of Textual Divergence
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This constraint story analyzes the Treaty of Waitangi from the
 *   'retrospective snare exposure' reading, focusing on how the textual
 *   divergence between the Māori and English versions of the Treaty
 *   constituted a covert extraction mechanism. Māori chiefs, signing the
 *   Māori text, could not have assented to the full cession of sovereignty
 *   claimed by the English text. This fundamental misunderstanding, exploited
 *   by the Crown, enabled subsequent land transfers and legislative overrides
 *   that stripped Māori of their authority and resources. The constraint
 *   operates as a snare because the coordination story (a fair agreement
 *   between parties) was a cover for an extractive process, visible only
 *   retrospectively as the consequences of the textual divergence unfolded.
 *   The period covered (1840-1975) marks the initial signing through to the
 *   establishment of the Waitangi Tribunal, which began to formally
 *   investigate Treaty breaches.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.95).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.88).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.95).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty of Waitangi: Retrospective Snare Exposure of Textual Divergence").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, 'c19e6f7d-9fab-4ae7-aa43-65150e474f96').
narrative_ontology:cs_kernel_codification('c19e6f7d-9fab-4ae7-aa43-65150e474f96', fixed_text).
narrative_ontology:cs_authority_grounding('c19e6f7d-9fab-4ae7-aa43-65150e474f96', extraction).
narrative_ontology:cs_interpretation_layer_present('c19e6f7d-9fab-4ae7-aa43-65150e474f96').
narrative_ontology:cs_reading_relation('c19e6f7d-9fab-4ae7-aa43-65150e474f96', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('c19e6f7d-9fab-4ae7-aa43-65150e474f96', treaty_authority_cession__rangatiratanga_retention_reading, coexists_with).
narrative_ontology:cs_axiom('c19e6f7d-9fab-4ae7-aa43-65150e474f96', foundational, textual_divergence_as_extraction).
narrative_ontology:cs_axiom_status(textual_divergence_as_extraction, holdable).
narrative_ontology:cs_axiom_grounding('c19e6f7d-9fab-4ae7-aa43-65150e474f96', textual_divergence_as_extraction, empirically_contingent).
narrative_ontology:cs_axiom('c19e6f7d-9fab-4ae7-aa43-65150e474f96', foundational, absence_of_informed_consent_invalidates_cession).
narrative_ontology:cs_axiom_status(absence_of_informed_consent_invalidates_cession, holdable).
narrative_ontology:cs_axiom_grounding('c19e6f7d-9fab-4ae7-aa43-65150e474f96', absence_of_informed_consent_invalidates_cession, deontological).
narrative_ontology:cs_reference_frame('c19e6f7d-9fab-4ae7-aa43-65150e474f96', maori_text_as_binding_agreement).
narrative_ontology:cs_drift_state('c19e6f7d-9fab-4ae7-aa43-65150e474f96', post_treaty_signing_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c19e6f7d-9fab-4ae7-aa43-65150e474f96', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, settler_government).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, settler_descendants).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_signatories).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendants).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_iwi_hapu).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited directly from the perceived cession of sovereignty, enabling large-scale land acquisition and settlement. Operated under the assumption of full Crown sovereignty derived from the English text, facilitating land transfers that Māori signatories did not understand as outright sales of sovereign rights.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Exercised legislative and administrative authority over all of Aotearoa New Zealand, including Māori lands, based on the English text's claim of full sovereignty. This reading enabled the establishment of a colonial legal system that systematically marginalized Māori authority.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, settler_government, agenda_setter,
    institutional, generational, constrained, national).

% Inherited land and a political system founded on the premise of Crown sovereignty, benefiting from the historical land transfers and the legal framework established under the English text's interpretation. Their current position is structurally advantaged by the historical extraction.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, settler_descendants, beneficiary,
    powerful, generational, mobile, national).

% Signed the Māori text of the Treaty, believing they were ceding only governance (kāwanatanga) while retaining full chieftainship (tino rangatiratanga) over their lands and people. They could not have assented to the full sovereignty cession implied by the English text, making the subsequent land transfers and legislative overrides an act of extraction under false pretenses.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_signatories, payer,
    powerless, biographical, trapped, local).

% Bear the intergenerational consequences of land loss, cultural suppression, and political marginalization stemming from the Crown's interpretation and enforcement of the English text. They are actively engaged in seeking redress and reasserting tino rangatiratanga.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_descendants, payer,
    organized, generational, identity_locked, national).

% Traditional tribal groupings whose collective authority and land base were eroded by the Crown's assertion of sovereignty. They continue to advocate for the recognition of their inherent rights and the principles of the Māori text of the Treaty.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_iwi_hapu, payer,
    organized, civilizational, identity_locked, regional).

% Review the historical and ongoing treatment of Māori under the Treaty, often highlighting the divergence between the texts and the implications for indigenous rights. Their observations provide external corroboration for the extractive nature of the Crown's historical actions.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Treaty was intended to coordinate relations between Māori and the British Crown, establishing a framework for governance and settlement in Aotearoa New Zealand.
% TRANSFER_FUNCTION: The English text of the Treaty transferred full sovereignty from Māori chiefs to the British Crown, enabling the Crown to acquire vast tracts of land and impose its legal system. The Māori text transferred limited governance, retaining Māori authority over their lands and resources.
% ABSENT_VOICES: The full implications of the English text's sovereignty claim were absent from the understanding of Māori signatories. Had they understood the full extent of the cession, they would not have signed. Their 'voice' was present in the Māori text, but that text was systematically overridden.
% DISAPPEARANCE_RATIONALE: If the historical interpretation and enforcement of the English text's sovereignty claim vanished, the entire legal and political structure of Aotearoa New Zealand would be fundamentally challenged. Land ownership, constitutional arrangements, and the distribution of power would require radical renegotiation, as the foundation of settler governance would be revealed as resting on a misrepresentation.
% FOUNDING_PROBLEM: The British Crown sought to establish legitimate authority over Aotearoa New Zealand to manage British settlement and prevent other European powers from claiming the territory, while also protecting Māori from unchecked settler expansion.
% FOUNDING_PROBLEM_CORROBORATION: While the Crown initially claimed to protect Māori, the subsequent history of land confiscation and legislative override, as documented by the Waitangi Tribunal and independent historians, demonstrates that the problem of protecting Māori was not genuinely solved. The problem of establishing British authority was solved, but at the expense of Māori sovereignty, making the original protective mandate 'dead' in practice. International human rights bodies corroborate the ongoing impact of this historical injustice.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(treaty_authority_cession__retrospective_snare_exposure, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__retrospective_snare_exposure, 0.95, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.95) because the Crown's interpretation of the Treaty led to massive land loss and the imposition of a foreign legal system, fundamentally dispossessing Māori. Suppression is also high (0.88) due to the legislative power of the settler government, which systematically overrode Māori customary law and claims. Theater ratio is significant (0.65) because the 'partnership' and 'protection' rhetoric surrounding the Treaty served to mask the underlying extractive agenda, with the Māori text itself becoming a performative element rather than a binding agreement. Accessibility collapse is high (0.75) as Māori had few legal or political avenues to resist the Crown's assertion of sovereignty, and resistance (0.70) reflects the ongoing, though often suppressed, Māori opposition to these actions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Crown and settler government, the Treaty was a legitimate act of cession, establishing British sovereignty and enabling 'orderly' settlement. From the Māori perspective, and retrospectively from this reading, it was a profound act of deception and dispossession, where a coordination agreement was transformed into a snare through textual manipulation and subsequent power imbalances. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown land-purchasing apparatus and settler government are clear beneficiaries (d near 0.0) as they gained land and authority. Māori signatories and their descendants are the primary victims (d near 1.0), experiencing profound loss. Settler descendants are also beneficiaries, inheriting the advantages of the extractive system. International human rights bodies serve as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_divergence_intentionality,
    'Was the textual divergence between the Māori and English versions of the Treaty of Waitangi an intentional act of deception by the Crown, or an unavoidable consequence of linguistic and conceptual differences?',
    'Historical linguistic analysis of the translation process, examination of contemporaneous Crown correspondence and instructions, and expert testimony on cross-cultural communication in the 19th century.',
    'If intentional, it strengthens the ''snare'' classification by highlighting deliberate malice. If unintentional but exploited, it still supports the ''snare'' classification by demonstrating the structural vulnerability created by the divergence, regardless of initial intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_divergence_intentionality, empirical, 'Determines the degree of deliberate exploitation in the textual divergence.').

omega_variable(
    sovereignty_concept_incommensurability,
    'To what extent was the concept of ''sovereignty'' (as understood by the British) truly incommensurable with Māori understandings of ''tino rangatiratanga'' (chieftainship/self-determination) at the time of the Treaty signing?',
    'Deep historical and anthropological research into pre-1840 Māori political structures and concepts of authority, compared with contemporary British legal and political theory.',
    'If incommensurable, it reinforces the impossibility of genuine consent to the English text''s terms, further solidifying the ''snare'' classification. If some degree of commensurability existed, it might slightly reduce the perceived ''snare'' aspect, though not eliminate the extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_concept_incommensurability, conceptual, 'Assesses the fundamental conceptual gap regarding sovereignty.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legislative override, land confiscation) or internalized (cultural erosion, loss of language) for Māori?',
    'Post-redress trajectory: if suppression persists after land returns and legal recognition, reclassify as partially internalized. If it diminishes with structural changes, it was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after structural barriers are removed. This would indicate a deeper, more insidious form of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for Māori.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 1840, 1975).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1840, 0.3).
narrative_ontology:measurement(trea_tr_t1860, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1860, 0.45).
narrative_ontology:measurement(trea_tr_t1880, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1880, 0.55).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1900, 0.6).
narrative_ontology:measurement(trea_tr_t1920, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1920, 0.63).
narrative_ontology:measurement(trea_tr_t1940, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1940, 0.64).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1975, 0.65).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1840, 0.7).
narrative_ontology:measurement(trea_be_t1860, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1860, 0.85).
narrative_ontology:measurement(trea_be_t1880, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1880, 0.9).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1900, 0.92).
narrative_ontology:measurement(trea_be_t1920, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1920, 0.93).
narrative_ontology:measurement(trea_be_t1940, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1940, 0.94).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1975, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1840, 0.6).
narrative_ontology:measurement(trea_su_t1860, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1860, 0.75).
narrative_ontology:measurement(trea_su_t1880, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1880, 0.8).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(trea_su_t1920, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1920, 0.87).
narrative_ontology:measurement(trea_su_t1940, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1940, 0.88).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1975, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__retrospective_snare_exposure, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession__crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession__rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, maori_land_court_jurisdiction).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, settler_property_rights).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'treaty_authority_cession' kernel. It focuses on the textual divergence as an extraction mechanism, contrasting with the 'crown_cession_reading' (full sovereignty ceded) and the 'rangatiratanga_retention_reading' (Māori sovereignty retained). All three are distinct constraints arising from the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
