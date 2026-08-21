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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   This constraint describes the Treaty of Waitangi as an extractive
 *   mechanism, where the textual divergence between the Māori and English
 *   versions of the Treaty constituted a snare. Māori signatories, assenting
 *   to the Māori text, could not have legitimately assented to the English
 *   text's claim of full sovereignty cession. Subsequent land transfers and
 *   legislative overrides are seen as the operationalization of this
 *   extraction, made possible by the initial mistranslation and the Crown's
 *   unilateral enforcement of its interpretation. This reading exposes the
 *   covert nature of the extraction at the time of operation, which only
 *   became fully visible retrospectively through critical analysis.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.85).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.9).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.85).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty Authority Cession: Retrospective Snare Exposure").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, '7431d70e-6989-4610-b264-2d2b780de11e').
narrative_ontology:cs_kernel_codification('7431d70e-6989-4610-b264-2d2b780de11e', fixed_text).
narrative_ontology:cs_authority_grounding('7431d70e-6989-4610-b264-2d2b780de11e', extraction).
narrative_ontology:cs_interpretation_layer_present('7431d70e-6989-4610-b264-2d2b780de11e').
narrative_ontology:cs_reading_relation('7431d70e-6989-4610-b264-2d2b780de11e', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('7431d70e-6989-4610-b264-2d2b780de11e', treaty_authority_cession__rangatiratanga_retention_reading, influences).
narrative_ontology:cs_axiom('7431d70e-6989-4610-b264-2d2b780de11e', foundational, textual_divergence_as_extraction).
narrative_ontology:cs_axiom_status(textual_divergence_as_extraction, holdable).
narrative_ontology:cs_axiom_grounding('7431d70e-6989-4610-b264-2d2b780de11e', textual_divergence_as_extraction, conventional).
narrative_ontology:cs_axiom('7431d70e-6989-4610-b264-2d2b780de11e', foundational, uninformed_consent_invalidates_cession).
narrative_ontology:cs_axiom_status(uninformed_consent_invalidates_cession, holdable).
narrative_ontology:cs_axiom_grounding('7431d70e-6989-4610-b264-2d2b780de11e', uninformed_consent_invalidates_cession, deontological).
narrative_ontology:cs_reference_frame('7431d70e-6989-4610-b264-2d2b780de11e', unilateral_imposition_of_sovereignty).
narrative_ontology:cs_drift_state('7431d70e-6989-4610-b264-2d2b780de11e', contemporary_post_colonial_critique, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7431d70e-6989-4610-b264-2d2b780de11e', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, settler_government).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_signatories).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendants).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_iwi_hapu).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The colonial administrative and legal structures that facilitated the acquisition of Māori land, operating under the premise of ceded sovereignty. This apparatus directly benefited from the perceived legitimacy of the English text of the Treaty.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% The successive governments of New Zealand that inherited and perpetuated the legal framework established by the Crown's interpretation of the Treaty, benefiting from expanded territorial control and legislative authority over Māori affairs.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, settler_government, beneficiary,
    institutional, generational, arbitrage, national).

% The Māori chiefs who signed the Treaty of Waitangi in its Māori text, believing they were ceding only governance (kāwanatanga) while retaining full authority (tino rangatiratanga) over their lands and people. They were unknowingly trapped by the divergence with the English text's claim of full sovereignty cession.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_signatories, payer,
    powerless, biographical, trapped, local).

% The generations of Māori who have lived under the legal and political consequences of the Crown's interpretation, experiencing land alienation, loss of autonomy, and cultural suppression. Their identity is deeply tied to the Treaty and its contested meaning.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_descendants, payer,
    organized, generational, identity_locked, national).

% Māori tribes and sub-tribes who collectively lost control over their ancestral lands and resources due to the Crown's assertion of sovereignty and subsequent legislative actions, which were enabled by the original textual divergence.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_iwi_hapu, payer,
    organized, generational, constrained, regional).

% Academics and legal experts who analyze the historical, linguistic, and legal aspects of the Treaty, often highlighting the structural inequities and the extractive nature of the Crown's interpretation. They contribute to the contemporary understanding of the constraint.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, legal_scholars_indigenous_rights, observer,
    analytical, biographical, analytical, global).

% Organizations that monitor and advocate for indigenous rights globally, often citing cases like the Treaty of Waitangi as examples of colonial injustice and the need for self-determination. They provide an external perspective on the constraint's impact.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__retrospective_snare_exposure, settler_government).
narrative_ontology:fixing_cost_class(treaty_authority_cession__retrospective_snare_exposure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint was presented as coordinating the establishment of British governance in New Zealand and the protection of Māori rights, creating a framework for colonial settlement and interaction.
% TRANSFER_FUNCTION: Covertly transferred full sovereignty and vast tracts of land from Māori to the British Crown, facilitated by the linguistic ambiguity between the Māori and English texts of the Treaty, and subsequently enforced through legislative and administrative actions.
% ABSENT_VOICES: A truly informed Māori voice, fully aware of the English text's claim to absolute sovereignty and its long-term implications for land and authority, was absent at the point of signing. Their perspective would have fundamentally altered the negotiation or prevented assent.
% DISAPPEARANCE_RATIONALE: If the historical effect of this textual divergence and subsequent extraction were undone, the entire constitutional, legal, and land ownership framework of New Zealand would be fundamentally challenged and would require massive societal and political rearrangement, including renegotiation of sovereignty and land rights.
% FOUNDING_PROBLEM: The British Crown sought to legitimize its claim to sovereignty over New Zealand and facilitate organized settlement, while Māori sought to protect their existing authority (tino rangatiratanga) and control over their lands and resources amidst increasing European presence.
% FOUNDING_PROBLEM_CORROBORATION: The Crown's historical narrative asserts a clear and legitimate cession of sovereignty. However, Māori oral histories, contemporary legal scholarship, and indigenous rights advocates corroborate that the founding problem was one of fundamentally incompatible understandings, which was exploited by the Crown's subsequent actions. This shifted-function reading is supported by extensive historical and linguistic analysis from outside the benefiting parties.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(treaty_authority_cession__retrospective_snare_exposure, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__retrospective_snare_exposure, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extraction is very high (0.85) because the constraint fundamentally enabled the transfer of sovereignty and land without informed consent. Suppression is also very high (0.90) as the linguistic trap was compounded by subsequent legislative and military enforcement, effectively collapsing alternatives for Māori. The theater ratio is low (0.10) because the extraction was a real, material process, not merely performative. The claimed type is 'snare' because the coordination story (mutual agreement) was a cover for a fundamentally extractive process, dependent on coercion and the suppression of alternatives (Māori sovereignty). The measurements show extractiveness and suppression intensifying over time as the Crown's interpretation was enforced and Māori resistance was met with further suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Crown and settler government, the Treaty was a legitimate act of cession, establishing a 'rope' for orderly governance and settlement. From the perspective of Māori signatories and their descendants, the same structure operated as a 'snare,' covertly extracting sovereignty and land through linguistic deception and subsequent force. The engine's classification will highlight this divergence based on the declared beneficiaries, victims, and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown land-purchasing apparatus and settler government are clear beneficiaries (d near 0.0), gaining land and authority. Māori signatories, descendants, and iwi/hapu are the primary targets (d near 1.0), bearing the costs of lost land, sovereignty, and cultural integrity. Legal scholars and international human rights bodies act as analytical observers, exposing the structural dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original 'mandate' (as framed by the Crown) was to establish legitimate governance and protect Māori interests. This analysis reveals that the actual function was extractive from the outset, making the 'mandate' a cover story. The persistence of the constraint is due to the ongoing benefits to the settler state and the suppression of Māori alternatives, rather than a genuine coordination function. This prevents mislabeling it as a 'rope' or 'scaffold' by exposing the foundational extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linguistic_intent_ambiguity,
    'To what extent was the textual divergence a deliberate act of deception versus an unavoidable consequence of translating complex legal concepts between disparate cultures and legal traditions?',
    'Further historical and linguistic analysis of colonial drafting practices, and comparative studies of other colonial treaties involving similar linguistic gaps.',
    'If deliberate deception is established, the ''snare'' classification is strengthened, emphasizing malicious intent. If unavoidable, the ''snare'' remains, but the emphasis shifts to structural injustice and the Crown''s responsibility for the consequences of its unilateral interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_intent_ambiguity, empirical, 'Ambiguity regarding the intent behind the Treaty''s textual divergence.').

omega_variable(
    sovereignty_concept_incommensurability,
    'Was the concept of ''sovereignty'' (as understood by the British Crown) truly incommensurable with Māori understandings of ''tino rangatiratanga'' (full chieftainship/authority) at the time of the Treaty''s signing, making genuine consent impossible?',
    'Deep comparative analysis of 19th-century British legal theory and Māori political philosophy, drawing on historical records and oral traditions.',
    'If incommensurability is confirmed, it reinforces the ''snare'' classification by demonstrating that the very conceptual ground for agreement was absent, making the ''coordination'' story untenable. If some commensurability is found, it might suggest a ''tangled_rope'' where genuine (albeit unequal) coordination was attempted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_concept_incommensurability, conceptual, 'Whether the core concepts of sovereignty were mutually intelligible.').

omega_variable(
    mandatrophy_of_coordination_claim,
    'Has the original coordination function (mutual protection and governance) completely atrophied, or does a residual, albeit highly asymmetric, coordination function persist?',
    'Analysis of contemporary Māori-Crown relations and Treaty settlement processes: if these processes genuinely address historical grievances and re-establish partnership, some coordination may be emerging. If they primarily serve to legitimize the status quo, the atrophy is complete.',
    'If a genuine, albeit asymmetric, coordination function is found to persist, the constraint might lean towards a ''tangled_rope'' or ''piton'' (if the extraction is diffuse and maintenance theatrical). If no genuine coordination remains, the ''snare'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_coordination_claim, empirical, 'Whether any genuine coordination function of the Treaty remains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(trea_tr_t1880, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1880, 0.08).
narrative_ontology:measurement(trea_tr_t1920, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(trea_tr_t1960, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(trea_tr_t2024, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1840, 0.6).
narrative_ontology:measurement(trea_be_t1880, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1880, 0.75).
narrative_ontology:measurement(trea_be_t1920, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1920, 0.8).
narrative_ontology:measurement(trea_be_t1960, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1960, 0.85).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2000, 0.88).
narrative_ontology:measurement(trea_be_t2024, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1840, 0.7).
narrative_ontology:measurement(trea_su_t1880, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1880, 0.8).
narrative_ontology:measurement(trea_su_t1920, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1920, 0.85).
narrative_ontology:measurement(trea_su_t1960, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1960, 0.9).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2000, 0.92).
narrative_ontology:measurement(trea_su_t2024, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__retrospective_snare_exposure, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, maori_land_alienation_laws).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, settler_sovereignty_doctrine).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, rangatiratanga_retention_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'treaty_authority_cession' kernel. It focuses on the textual divergence as an extraction mechanism, influencing other readings by providing a structural explanation for their claims or foreclosing their core premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
