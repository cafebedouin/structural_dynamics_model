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
 *   constraint_id: treaty_authority_cession__rangatiratanga_retention_reading
 *   human_readable: Treaty of Waitangi: Rangatiratanga Retention Reading
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This constraint represents the 'rangatiratanga retention' reading of the
 *   Treaty of Waitangi, which asserts the primacy of the Māori text and the
 *   retention of Māori sovereignty ('tino rangatiratanga') while granting the
 *   Crown limited governance ('kāwanatanga'). It frames the Treaty as a
 *   partnership requiring ongoing consent. While the reading itself describes
 *   an ideal 'Rope' of coordination, the historical measurements reflect the
 *   severe divergence from this ideal, where Crown actions often ignored
 *   Māori authority, leading to significant land alienation and cultural
 *   suppression, which this reading retrospectively exposes as a snare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.25).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.3).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Treaty of Waitangi: Rangatiratanga Retention Reading").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__rangatiratanga_retention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, '4a8f64e5-4976-4690-9275-daf6ca826aa1').
narrative_ontology:cs_kernel_codification('4a8f64e5-4976-4690-9275-daf6ca826aa1', fixed_text).
narrative_ontology:cs_authority_grounding('4a8f64e5-4976-4690-9275-daf6ca826aa1', lineage).
narrative_ontology:cs_interpretation_layer_present('4a8f64e5-4976-4690-9275-daf6ca826aa1').
narrative_ontology:cs_reading_relation('4a8f64e5-4976-4690-9275-daf6ca826aa1', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('4a8f64e5-4976-4690-9275-daf6ca826aa1', treaty_authority_cession__biculturalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a8f64e5-4976-4690-9275-daf6ca826aa1', treaty_authority_cession__retrospective_snare_exposure, influences).
narrative_ontology:cs_axiom('4a8f64e5-4976-4690-9275-daf6ca826aa1', foundational, maori_text_primacy_contra_proferentem).
narrative_ontology:cs_axiom_status(maori_text_primacy_contra_proferentem, holdable).
narrative_ontology:cs_axiom_grounding('4a8f64e5-4976-4690-9275-daf6ca826aa1', maori_text_primacy_contra_proferentem, conventional).
narrative_ontology:cs_axiom('4a8f64e5-4976-4690-9275-daf6ca826aa1', foundational, tino_rangatiratanga_retained_by_maori).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_retained_by_maori, holdable).
narrative_ontology:cs_axiom_grounding('4a8f64e5-4976-4690-9275-daf6ca826aa1', tino_rangatiratanga_retained_by_maori, deontological).
narrative_ontology:cs_reference_frame('4a8f64e5-4976-4690-9275-daf6ca826aa1', treaty_as_partnership_1840).
narrative_ontology:cs_drift_state('4a8f64e5-4976-4690-9275-daf6ca826aa1', contemporary_post_treaty_settlements, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('4a8f64e5-4976-4690-9275-daf6ca826aa1', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, maori_hapu_iwi).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, crown_government).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, new_zealand_citizens).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, maori_hapu_iwi).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, new_zealand_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the original inhabitants and signatories to the Māori text, they retain 'tino rangatiratanga' (full chieftainship/sovereignty) over their lands, resources, and culture. They are beneficiaries of the partnership, but also 'pay' through ongoing engagement and the need to assert their rights against historical violations of the Treaty. Their identity is deeply tied to their ancestral lands and the Treaty itself.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, maori_hapu_iwi, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, maori_hapu_iwi, payer).

% The Crown receives 'kāwanatanga' (governorship) for the purpose of maintaining law and order, but this is limited to governance and does not equate to full sovereignty over Māori. The Crown benefits from the legitimacy conferred by the Treaty as a founding document, but is constrained by the requirement for ongoing Māori consent and partnership. Exiting the Treaty framework would entail severe legitimacy and constitutional crises.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, crown_government, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, crown_government, beneficiary).

% Benefit from the stable constitutional framework and national identity derived from the Treaty. They indirectly bear the costs of Treaty settlements and ongoing partnership efforts through taxation and social adjustments. Their engagement with the Treaty is often mediated by political discourse and education.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, new_zealand_citizens, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, new_zealand_citizens, payer).

% A quasi-judicial body established to inquire into claims by Māori against the Crown for breaches of the Treaty of Waitangi. It provides recommendations to the government, acting as a critical analytical and investigative seat that assesses the historical and ongoing adherence to the Treaty's principles.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, treaty_tribunal, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a framework for shared governance and resource management in New Zealand, ensuring mutual benefit and respect for retained Māori authority, while enabling British settlement and the establishment of a legal system.
% TRANSFER_FUNCTION: Transfers limited governance authority ('kāwanatanga') to the Crown for the purpose of maintaining law and order, while explicitly retaining full Māori authority ('tino rangatiratanga') over their lands, resources, and culture. This requires the Crown to seek Māori consent for actions affecting Māori interests.
% ABSENT_VOICES: Those who advocate for full Māori sovereignty (beyond a partnership model) or those who deny any Māori authority beyond individual citizenship. Also, those who would argue for a purely English-text interpretation of the Treaty, dismissing the Māori text's primacy.
% DISAPPEARANCE_RATIONALE: If this reading of the Treaty vanished overnight, the entire constitutional and legal framework of New Zealand would be fundamentally altered. The basis for Crown authority would be undermined, Māori claims to sovereignty and resources would be unaddressed, and a renegotiation of authority and land rights would be required, leading to profound societal and political instability.
% FOUNDING_PROBLEM: To establish a legitimate basis for British settlement and governance in New Zealand while protecting Māori land, culture, and authority, and preventing inter-tribal warfare.
% FOUNDING_PROBLEM_CORROBORATION: The Treaty Tribunal's ongoing inquiries and findings, academic historians specializing in Treaty studies, and international indigenous rights bodies consistently corroborate that the founding problem of establishing a just and equitable relationship between Māori and the Crown, respecting retained authority, remains live and contested.
narrative_ontology:disappearance_verdict(treaty_authority_cession__rangatiratanga_retention_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__rangatiratanga_retention_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The claimed type 'rope' reflects the ideal of a genuine partnership with retained Māori authority and mutual benefit, as understood by this reading. However, the temporal measurements show a significant increase in extractiveness and suppression over time, reflecting the historical reality where the Crown's actions often violated the spirit of this partnership, leading to a 'retrospective snare'. The initial low values for extractiveness and suppression represent the ideal state of the partnership at its inception, while the rising values track the historical erosion of Māori authority and the imposition of Crown sovereignty. The recent slight decrease in extractiveness and suppression reflects the impact of Treaty settlements and increased recognition of Māori rights.
 *
 * PERSPECTIVAL GAP:
 *   The Crown's historical perspective often diverged sharply from this reading, asserting full sovereignty and treating Māori authority as subordinate. This created a profound perspectival gap where the Crown perceived its actions as legitimate governance (a 'Rope' or 'Mountain' of law), while Māori experienced them as extraction and suppression (a 'Snare'). The engine's computation of per-seat classifications from the structural data, combined with the temporal drift, captures this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   From the perspective of this reading, Māori hapū and iwi are beneficiaries due to the retention of their 'tino rangatiratanga', but also payers through their ongoing engagement and the historical costs of asserting their rights. The Crown is an agenda-setter and beneficiary of legitimate governance, but is constrained by the partnership. New Zealand citizens are diffuse beneficiaries of a stable nation, but also bear the costs of historical injustices. The Treaty Tribunal acts as an analytical observer, assessing adherence to the Treaty's terms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maori_text_vs_english_text_primacy,
    'Which text of the Treaty of Waitangi (Māori or English) holds legal primacy in New Zealand''s constitutional framework?',
    'Definitive judicial ruling by the Supreme Court of New Zealand or constitutional amendment explicitly clarifying textual primacy.',
    'If the Māori text''s primacy is universally affirmed, this reading''s claims of retained ''tino rangatiratanga'' would gain stronger legal force, potentially reclassifying historical Crown actions as more clearly extractive. If the English text''s primacy is affirmed, this reading would be weakened, and the ''crown_cession_reading'' would gain ground.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maori_text_vs_english_text_primacy, conceptual, 'Ambiguity regarding the authoritative text of the Treaty.').

omega_variable(
    kawanatanga_vs_rangatiratanga_scope,
    'What are the precise boundaries and scope of ''kāwanatanga'' (governorship) ceded to the Crown and ''tino rangatiratanga'' (full chieftainship/sovereignty) retained by Māori?',
    'Ongoing Treaty settlements, co-governance arrangements, and judicial interpretations that define specific areas of shared or exclusive authority.',
    'Clearer delineation would strengthen the ''rope'' aspect of the partnership by reducing ambiguity and contestation. Continued ambiguity allows for ''tangled rope'' dynamics where the Crown can expand ''kāwanatanga'' at the expense of ''tino rangatiratanga''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kawanatanga_vs_rangatiratanga_scope, empirical, 'Uncertainty regarding the division of authority between Crown and Māori.').

omega_variable(
    retrospective_snare_legitimacy,
    'To what extent do historical land transfers and legislative overrides, enacted under a ''crown_cession_reading'', constitute illegitimate extraction from the perspective of the ''rangatiratanga_retention_reading''?',
    'Comprehensive historical inquiry and legal analysis by the Treaty Tribunal, followed by government acknowledgment and redress through Treaty settlements.',
    'Full acknowledgment of the ''retrospective snare'' would solidify the historical classification of many Crown actions as ''snare'' from this reading''s perspective, reinforcing the need for ongoing redress and partnership. Denial would perpetuate the perspectival gap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retrospective_snare_legitimacy, empirical, 'Historical land alienation as illegitimate extraction under this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(trea_tr_t1870, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1870, 0.2).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1900, 0.3).
narrative_ontology:measurement(trea_tr_t1940, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1940, 0.35).
narrative_ontology:measurement(trea_tr_t1980, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(trea_tr_t2024, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1840, 0.25).
narrative_ontology:measurement(trea_be_t1870, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1870, 0.45).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1900, 0.6).
narrative_ontology:measurement(trea_be_t1940, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1940, 0.7).
narrative_ontology:measurement(trea_be_t1980, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(trea_be_t2024, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1840, 0.3).
narrative_ontology:measurement(trea_su_t1870, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1870, 0.55).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(trea_su_t1940, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1940, 0.8).
narrative_ontology:measurement(trea_su_t1980, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1980, 0.85).
narrative_ontology:measurement(trea_su_t2024, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__biculturalism_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__retrospective_snare_exposure).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, maori_land_rights_framework).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, new_zealand_constitutional_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'treaty_authority_cession' kernel. This 'rangatiratanga_retention_reading' emphasizes Māori textual primacy and retained sovereignty, contrasting with the 'crown_cession_reading' (English text primacy, full Crown sovereignty) and the 'biculturalism_reading' (shared but not equal authority). The 'retrospective_snare_exposure' is an analytical lens applied to the kernel's historical operation, influenced by this reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
