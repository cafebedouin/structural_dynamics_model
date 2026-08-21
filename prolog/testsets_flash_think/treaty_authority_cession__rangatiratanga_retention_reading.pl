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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Treaty of Waitangi: Rangatiratanga Retention Reading
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This constraint instantiates the 'rangatiratanga_retention_reading' of
 *   the 'treaty_authority_cession' kernel. This reading emphasizes the
 *   primacy of the Māori text of the Treaty of Waitangi, asserting that Māori
 *   retained 'tino rangatiratanga' (full authority/sovereignty) over their
 *   lands, resources, and culture, while granting the Crown 'kāwanatanga'
 *   (governance) over its own subjects. It contrasts with the
 *   'crown_cession_reading' (which asserts full cession of sovereignty to the
 *   Crown) and the 'biculturalism_reading' (which frames the Treaty as a
 *   basis for shared cultural identity rather than a constitutional
 *   partnership). From this reading's perspective, the Crown's historical and
 *   ongoing unilateral exercise of sovereignty constitutes a profound breach
 *   of the Treaty, leading to significant extraction and suppression.
 *
 * KEY AGENTS:
 *   - hapu_iwi: Primary target (organized/identity_locked) — bears extraction
 *   - maori_citizens: Secondary target (moderate/constrained) — bears extraction
 *   - crown_government: Primary agenda_setter/beneficiary (institutional/constrained) — benefits from historical extraction, administers the constraint
 *   - pakeha_settlers: Historical beneficiary (powerful/mobile) — benefited from land acquisition
 *   - treaty_tribunal: Analytical observer (institutional/analytical) — investigates breaches, makes recommendations
 *   - international_human_rights_bodies: Analytical observer (analytical/analytical) — provides external scrutiny
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.85).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.78).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Treaty of Waitangi: Rangatiratanga Retention Reading").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__rangatiratanga_retention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, '1672d05d-aac3-4baa-849a-66ba0c8ec5b2').
narrative_ontology:cs_kernel_codification('1672d05d-aac3-4baa-849a-66ba0c8ec5b2', fixed_text).
narrative_ontology:cs_authority_grounding('1672d05d-aac3-4baa-849a-66ba0c8ec5b2', lineage).
narrative_ontology:cs_interpretation_layer_present('1672d05d-aac3-4baa-849a-66ba0c8ec5b2').
narrative_ontology:cs_reading_relation('1672d05d-aac3-4baa-849a-66ba0c8ec5b2', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('1672d05d-aac3-4baa-849a-66ba0c8ec5b2', treaty_authority_cession__biculturalism_reading, influences).
narrative_ontology:cs_reading_relation('1672d05d-aac3-4baa-849a-66ba0c8ec5b2', treaty_authority_cession__retrospective_snare_exposure, coexists_with).
narrative_ontology:cs_axiom('1672d05d-aac3-4baa-849a-66ba0c8ec5b2', foundational, maori_text_primacy).
narrative_ontology:cs_axiom_status(maori_text_primacy, holdable).
narrative_ontology:cs_axiom_grounding('1672d05d-aac3-4baa-849a-66ba0c8ec5b2', maori_text_primacy, conventional).
narrative_ontology:cs_axiom('1672d05d-aac3-4baa-849a-66ba0c8ec5b2', foundational, tino_rangatiratanga_retained).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_retained, holdable).
narrative_ontology:cs_axiom_grounding('1672d05d-aac3-4baa-849a-66ba0c8ec5b2', tino_rangatiratanga_retained, deontological).
narrative_ontology:cs_reference_frame('1672d05d-aac3-4baa-849a-66ba0c8ec5b2', treaty_as_constitutional_partnership).
narrative_ontology:cs_drift_state('1672d05d-aac3-4baa-849a-66ba0c8ec5b2', contemporary_new_zealand, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1672d05d-aac3-4baa-849a-66ba0c8ec5b2', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, crown_government).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, pakeha_settlers).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, hapu_iwi).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, maori_citizens).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, maori_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, treaty_as_living_document).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, contra_proferentem_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the original signatories and inheritors of 'tino rangatiratanga', hapū and iwi bear the costs of the Crown's historical and ongoing assertion of full sovereignty, experiencing land alienation, resource loss, and legislative override. Their identity is deeply tied to their ancestral lands and self-determination.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, hapu_iwi, payer,
    organized, generational, identity_locked, local).

% Experience the systemic effects of the Crown's failure to uphold the Treaty as a partnership, including socio-economic disparities and cultural erosion. They actively advocate for Treaty rights and self-determination.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, maori_citizens, payer,
    moderate, biographical, constrained, national).

% Claims to uphold the Treaty while historically and often currently acting as if it holds full, unqualified sovereignty. Benefits from the historical acquisition of land and resources, and the ability to legislate unilaterally. Faces increasing pressure to reconcile its actions with this reading.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, crown_government, agenda_setter,
    institutional, generational, constrained, national).

% Historically benefited from the Crown's assertion of sovereignty and subsequent land acquisition, leading to economic and social advantages. Many are now increasingly aware of the Treaty's alternative readings and the historical injustices.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, pakeha_settlers, beneficiary,
    powerful, biographical, mobile, national).

% An independent commission established to inquire into claims by Māori relating to the Treaty of Waitangi. Its recommendations often align with the principles of this reading, but it lacks binding authority to enforce them.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, treaty_tribunal, observer,
    institutional, biographical, analytical, national).

% Monitor indigenous rights globally and provide external scrutiny and pressure on the New Zealand government to adhere to international standards that often align with the principles of this reading.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, international_human_rights_bodies, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a constitutional partnership between Māori and the British Crown, defining spheres of authority where Māori retain 'tino rangatiratanga' (full authority) over their lands, resources, and culture, and the Crown exercises 'kāwanatanga' (governance) over its own subjects.
% TRANSFER_FUNCTION: Intended to transfer limited governance authority ('kāwanatanga') to the Crown for its subjects, while retaining full Māori authority ('tino rangatiratanga'). In practice, the Crown's dominant interpretation led to the illegitimate transfer of vast Māori lands, resources, and legislative power to the Crown and settlers.
% ABSENT_VOICES: The voices of hapū and iwi who signed the Māori text, believing they retained full authority, were effectively absent from the subsequent legislative and judicial interpretations that asserted full Crown sovereignty, leading to a systematic silencing of their understanding of the Treaty.
% DISAPPEARANCE_RATIONALE: If this reading and its underlying principles vanished, the legal and moral basis for Māori claims to self-determination, historical redress, and constitutional partnership would be severely undermined. This would lead to a fundamental reordering of New Zealand's constitutional and social fabric, likely exacerbating existing inequalities and conflicts.
% FOUNDING_PROBLEM: To establish a framework for shared authority and peaceful coexistence between Māori and the British Crown, manage British settlement, and protect Māori interests and authority ('tino rangatiratanga') from unchecked colonial expansion, while granting the Crown limited governance over its own subjects.
% FOUNDING_PROBLEM_CORROBORATION: Māori communities, legal scholars, and the Treaty Tribunal consistently attest that the core issues of partnership, self-determination, and redress for historical breaches remain live. This is supported by ongoing Treaty claims, academic research, and public discourse, providing corroboration from outside the immediate beneficiaries of the Crown's interpretation.
narrative_ontology:disappearance_verdict(treaty_authority_cession__rangatiratanga_retention_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__rangatiratanga_retention_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(treaty_authority_cession__rangatiratanga_retention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the massive historical and ongoing transfer of Māori land, resources, and self-determination to the Crown and settlers, viewed as illegitimate under this reading. Suppression (0.78) is high due to the Crown's legislative and judicial actions that systematically undermined Māori authority and resistance. The theater ratio (0.60) indicates that a significant portion of the Crown's 'partnership' rhetoric and 'consultation' processes serve to legitimize its continued unilateral exercise of sovereignty, rather than genuinely upholding the Treaty's intent. The claimed type 'rope' reflects the *intended* constitutional partnership, while the metrics expose the *actual* operation as highly extractive due to the Crown's historical and ongoing actions.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's 'crown_cession_reading' perspective, the Treaty established a legitimate, low-extraction governance framework. From this 'rangatiratanga_retention_reading', the same historical events are seen as high-extraction and high-suppression, fundamentally violating the Treaty's intent. The engine's computation of per-seat classification will highlight this divergence, showing the Crown as a beneficiary of a snare-like operation, while Māori are victims of it, despite the 'rope' claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Hapū and iwi, as the holders of 'tino rangatiratanga', are the primary targets (high d) as the constraint (Crown's assertion of sovereignty) extracts from their inherent authority and resources. Māori citizens also bear costs. The Crown government is the primary beneficiary and agenda-setter (low d) as it collects the benefits of asserted sovereignty and administers the system. Pākehā settlers are beneficiaries due to historical advantages. The Treaty Tribunal and international bodies are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the Crown's unilateral actions as legitimate governance by highlighting the original intent of partnership and retained 'tino rangatiratanga'. It exposes how the Crown's interpretation has allowed the original mandate of protection and partnership to atrophy into a mechanism for asserting unqualified sovereignty, thereby revealing the underlying extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_ambiguity,
    'Is the Treaty of Waitangi fundamentally a cession of sovereignty (Crown reading) or a constitutional partnership retaining Māori authority (Rangatiratanga reading)?',
    'Constitutional reform establishing a supreme law based on the Māori text, or a binding international arbitration on the Treaty''s original intent.',
    'If resolved in favor of the Rangatiratanga reading, the Crown''s historical actions would be unequivocally reclassified as illegitimate extraction; if resolved in favor of the Crown reading, Māori claims would be significantly weakened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_ambiguity, conceptual, 'The core ambiguity of the Treaty''s constitutional status.').

omega_variable(
    translation_asymmetry_impact,
    'To what extent did the linguistic and conceptual differences between the Māori and English texts of the Treaty of Waitangi constitute an inherent extraction mechanism from the moment of signing?',
    'Detailed historical and linguistic analysis of the signing context, coupled with a legal framework that applies the ''contra proferentem'' rule (interpretation against the drafter) to the Crown.',
    'If the asymmetry is proven to be an inherent extraction mechanism, the entire historical trajectory of Crown-Māori relations would be re-framed as operating under a foundational snare, regardless of the Crown''s stated intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translation_asymmetry_impact, empirical, 'The role of mistranslation in enabling extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of Māori self-determination primarily structural (Crown legislation, legal system) or internalized (cognitive patterns persisting after barrier removal)?',
    'Post-settlement outcomes analysis: if Māori self-governance and economic autonomy persist after structural barriers are removed, reclassify as primarily structural. If internal divisions or dependency persist, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — Māori communities carry the suppression with them after structural barriers are removed, requiring deeper interventions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in Māori self-determination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(trea_tr_t1860, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1860, 0.3).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1900, 0.5).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1975, 0.55).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2000, 0.6).
narrative_ontology:measurement(trea_tr_t2024, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1840, 0.1).
narrative_ontology:measurement(trea_be_t1860, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1860, 0.65).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1900, 0.9).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1975, 0.88).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2000, 0.86).
narrative_ontology:measurement(trea_be_t2024, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1840, 0.2).
narrative_ontology:measurement(trea_su_t1860, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1860, 0.7).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1975, 0.8).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(trea_su_t2024, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, biculturalism_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, retrospective_snare_exposure).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, maori_land_court_jurisdiction).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, resource_management_act_interpretation).

% DUAL FORMULATION NOTE:
% This constraint is one of three primary readings of the 'treaty_authority_cession' kernel. It focuses on the Māori text and the retention of 'tino rangatiratanga', contrasting with the 'crown_cession_reading' (English text, full sovereignty) and the 'biculturalism_reading' (cultural partnership). The 'retrospective_snare_exposure' is a meta-analysis of the textual divergence itself as an extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
